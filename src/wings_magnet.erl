%%
%%  wings_magnet.erl --
%%
%%     This module implements the Magnet command.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_magnet.erl,v 1.19 2001/12/28 11:34:03 bjorng Exp $
%%

-module(wings_magnet).
-export([sub_menu/1,command/2]).

%% Shell interface.
-export([add_user_expr/2,replace_user_expr/2,
	 list_user_exprs/0,delete_user_expr/1]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,sort/1,concat/1]).

sub_menu(St) ->
    Dirs = directions(),
    M = [{"Gaussian",{gaussian,Dirs}},
	 {"Linear",{linear,Dirs}}|user_defined(Dirs)],
    {"Magnet",{magnet,list_to_tuple(M)}}.

user_defined(Dirs) ->
    case wings_pref:browse(magnet_uexpr) of
	[] -> [];
	Uexprs ->
	    [separator|[{Name,{Key,Dirs}} || {Key,{Name,_}} <- Uexprs]]
    end.

command({Type,Dir}, #st{selmode=vertex}=St) ->
    Vec = wings_util:make_vector(Dir),
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 setup_1(Vs, We, Vec, Type, Acc)
			 end, [], St),
    wings_drag:init_drag(Tvs, {magnet,constraint(Dir)}, St#st{inf_r=1.0}).

setup_1(Vs, #we{id=Id}=We, Vec, Type, Acc) ->
    Tv = vertices_to_vertices(gb_sets:to_list(Vs), We, Type, Vec),
    [{Id,Tv}|Acc].

constraint(free) -> view_dependent;
constraint(Other) -> none.

directions() ->
    {{"Normal",normal},
     {"Free",free},
     {"X",x},
     {"Y",y},
     {"Z",z}}.

add_user_expr(Name, Body) ->
    Uexprs = wings_pref:browse(magnet_uexpr),
    Key = list_to_atom("user" ++ integer_to_list(length(Uexprs))),
    wings_pref:set_value({magnet_uexpr,Key}, {Name,Body}),
    unload(),
    ok.

replace_user_expr(Num, Body) ->
    Key = num_to_key(Num),
    {Name,_} = wings_pref:get_value({magnet_uexpr,Key}),
    wings_pref:set_value({magnet_uexpr,Key}, {Name,Body}),
    unload(),
    ok.

unload() ->
    code:delete(wings__magnet),
    code:purge(wings__magnet),
    code:delete(wings__magnet),
    code:purge(wings__magnet).

list_user_exprs() ->
    list_user_exprs(wings_pref:browse(magnet_uexpr), 1).

list_user_exprs([{_,{Name,Body}}|Us], I) ->
    Arity = func_arity(Body),
    Func = {function,1,'fun',Arity,Body},
    io:format("~w) ~p\n~s\n", [I,Name,erl_pp:function(Func)]),
    list_user_exprs(Us, I+1);
list_user_exprs([], I) -> ok.

func_arity([{clause,_,Args,_,_}|_]) ->
    length(Args).

delete_user_expr(N) ->
    case num_to_key(N) of
	none -> not_found;
	Key -> wings_pref:delete_value({magnet_uexpr,Key})
    end.

num_to_key(Num) ->
    num_to_key(wings_pref:browse(magnet_uexpr), 1, Num).

num_to_key([{Key,{Name,Body}}|Us], Num, Num) -> Key;
num_to_key([_|Us], I, Num) -> num_to_key(Us, I+1, Num);
num_to_key([], I, Num) -> none.

load_user_module() ->
    Uexprs = wings_pref:browse(magnet_uexpr),
    Funs = [wrap_body(Func, Body) || {Func,{_,Body}} <- Uexprs],
    Module = make_module(wings__magnet, Funs),
    compile(Module).

wrap_body(Name, Body0) ->
    Body = [{'fun',1,{clauses,Body0}}],
    make_function(Name, [], Body).

%%
%% Conversion of vertice selections to vertices. :-)
%% Not entirely pointless, as we'll need to add vectors for
%% the points (vertices).
%%

vertices_to_vertices(Vs, We, Type, normal) ->
    Vec = e3d_vec:norm(e3d_vec:add(vertex_normal(Vs, We))),
    make_tvs(Type, Vec, Vs, We);
vertices_to_vertices(Vs, We, Type, Vec) ->
    make_tvs(Type, Vec, Vs, We).

vertex_normal(Vs, We) ->
    foldl(fun(V, Acc) ->
		  Vec = wings_vertex:normal(V, We),
		  [Vec|Acc]
	  end, [], Vs).

make_tvs(Type, Vec, Vs, #we{vs=Vtab}=We) ->
    DF = distance_fun(Type),
    All = gb_trees:keys(Vtab),
    VsPos = wings_util:add_vpos(All, We),
    Center = wings_vertex:center(Vs, Vtab),
    {All,magnet_fun(DF, Vec, Center, VsPos)}.

magnet_fun(DF, Vec, Center, VsPos) ->
    fun(view_changed, NewWe) ->
	    magnet_fun(DF, Center, Vec,
		       wings_util:update_vpos(VsPos, NewWe));
       ({Dx,Falloff}, A) ->
	    move(Dx, Falloff, DF, Vec, Center, VsPos, A);
       ({Dx,Dy,Falloff}, A) ->
	    free_move(Dx, Dy, Falloff, DF, Center, VsPos, A)
    end.

move(Dx, Falloff, DF, Vec0, Center, VsPos, A) ->
    wings_drag:message([Dx], distance),
    Vec = e3d_vec:mul(Vec0, Dx),
    magnet_move(Falloff, DF, Vec, Center, VsPos, A).

free_move(Dx, Dy, Falloff, DF, Center, VsPos, A) ->
    wings_drag:message([Dx,Dy], distance),
    #view{azimuth=Az,elevation=El} = wings_view:current(),
    M0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    M = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    Vec = e3d_mat:mul_point(M, {Dx,Dy,0.0}),
    magnet_move(Falloff, DF, Vec, Center, VsPos, A).

magnet_move(Falloff, DF, Vec, Center, VsPos, A0) ->
    foldl(fun({V,#vtx{pos=Pos0}=Rec}=Vtx, A) ->
		  case e3d_vec:dist(Pos0, Center) of
%   		      Dist0 when Dist0 > Falloff ->
%   			  [Vtx|A];
		      Dist0 ->
			  case DF(Dist0, Falloff) of
			      Dist when Dist < 1.0E-5 ->
				  [Vtx|A];
			      Dist ->
				  Offset = e3d_vec:mul(Vec, Dist),
				  Pos = e3d_vec:add(Pos0, Offset),
				  [{V,Rec#vtx{pos=Pos}}|A]
			  end
		  end
	  end, A0, VsPos).

%%%
%%% Pre-defined distance functions.
%%%

distance_fun(linear) ->
    fun(Dist0, Radius) ->
	    Dist = Dist0/Radius,
	    case 1-Dist of
		Negative when Negative =< 0 -> 0.0;
		Positive -> Positive
	    end
    end;
distance_fun(gaussian) ->
    fun(Dist0, Radius) ->
	    Dist = Dist0/Radius,
	    math:exp(-(Dist*Dist)/2.0)
    end;
distance_fun(User) ->
    case catch wings__magnet:User() of
	{'EXIT',{undef,_}} ->
	    load_user_module(),
	    wings__magnet:User();
	Fun when is_function(Fun) -> Fun
    end.

%%%
%%% Compilation support.
%%%

compile(Module) ->
    case compile:forms(Module, [report_errors]) of
	{ok,Name,Code} ->
	    code:delete(Name),
	    code:purge(Name),
	    {module,Name} = code:load_binary(Name, Name, Code),
	    ok;
	error ->
	    throw({command_error,
		   "Compilation error: See Erlang shell window."})
    end.

make_module(Name, Funcs) ->
    [{attribute,1,module,Name},
     {attribute,0,compile,export_all}|Funcs ++ [{eof,0}]].

make_function(Name, Args, Body) ->
    {function,1,Name,length(Args),[{clause,1,Args,[],Body}]}.
