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
%%     $Id: wings_magnet.erl,v 1.13 2001/12/13 12:01:40 bjorng Exp $
%%

-module(wings_magnet).
-export([sub_menu/1,command/2]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3]).

sub_menu(St) ->
    Dirs = directions(),
    M = [{"Gaussian",{gaussian,Dirs}},
	 {"Linear",{linear,Dirs}},
	 separator|user_defined(Dirs)],
    {"Magnet",{magnet,list_to_tuple(M)}}.

user_defined(Dirs) ->
    New = {"New",new},
    case wings_pref:get_value(magnet_user_exprs, []) of
	[] -> [New];
	L -> [{Name,{Key,Dirs}} || {Name,Key,_} <- L] ++ [separator,New]
    end.

command(new, St) ->
    new(),
    St;
command({Type,Dir}, #st{selmode=vertex}=St) ->
    Vec = wings_util:make_vector(Dir),
    Tvs = wings_sel:fold_shape(fun(Sh, Items, Acc) ->
				       setup_1(Sh, Items, Vec, Type, Acc)
			       end, [], St),
    wings_drag:init_drag(Tvs, constraint(Type), St#st{inf_r=1.0}).

setup_1(#shape{id=Id,sh=We}=Sh, Items, Vec, Type, Acc) ->
    Tv = vertices_to_vertices(gb_sets:to_list(Items), We, Type, Vec),
    [{Id,Tv}|Acc].

constraint(free) -> view_dependent;
constraint(Other) -> none.

directions() ->
    {{"Normal",normal},
     {"Free",free},
     {"X",x},
     {"Y",y},
     {"Z",z}}.

new() ->
    case wings_getline:string("Name: ") of
	aborted -> aborted;
	Name ->
	    Funs = wings_pref:get_value(magnet_user_exprs, []),
	    Key = list_to_atom("user" ++ integer_to_list(length(Funs))),
	    Def = "Dist=D/R, math:exp(-(Dist*Dist)/2)",
	    case wings_getline:string("Expr: ", Def) of
		aborted -> aborted;
		Str -> new(Name, Key, Funs, Str)
	    end
    end.
		    
new(Name, Key, Funs0, Str) ->
    Args = [make_var('D'),make_var('R')],
    Body0 = parse_str(Str),
    Body = [{'fun',1,{clauses,[{clause,1,Args,[],Body0}]}}],
    F = make_function(Key, [], Body),
    Funs = [{Name,Key,F}|Funs0],
    FunList = [Fun || {_,_,Fun} <- Funs],
    Module = make_module(wings__magnet, FunList),
    compile(Module),
    wings_pref:set_value(magnet_user_exprs, Funs).

load_user_module() ->
    Funs = wings_pref:get_value(magnet_user_exprs, []),
    FunList = [Fun || {_,_,Fun} <- Funs],
    Module = make_module(wings__magnet, FunList),
    compile(Module).

%%
%% Conversion of vertice selections to vertices. :-)
%% Not entirely pointless, as we'll need to add vectors for
%% the points (vertices).
%%

vertices_to_vertices(Vs, We, Type, normal) ->
    make_tvs(Type, vertex_normals(We, Vs));
vertices_to_vertices(Vs, We, Type, Vec) -> make_tvs(Type, Vs, Vec).

vertex_normals(We, Vs) ->
    foldl(fun(V, Acc) ->
		  Vec = wings_vertex:normal(V, We),
		  [{Vec,[V]}|Acc]
	  end, [], Vs).

make_tvs(Type, Vs, Vec) ->
    make_tvs(Type, [{Vec,Vs}]).

make_tvs(Type, Tvs) ->
    DF = distance_fun(Type),
    fun(#shape{sh=#we{vs=Vtab0}=We0}=Sh, Dx, Dy, #st{inf_r=IR}=St) ->
	    Vtab = magnet_move(Tvs, Dx, Dy, {DF,IR}, St, Vtab0, Vtab0),
	    We = We0#we{vs=Vtab},
	    {shape,Sh#shape{sh=We}}
    end.

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
	    math:exp(-(Dist*Dist)/2)
    end;
distance_fun(User) ->
    case catch wings__magnet:User() of
	{'EXIT',{undef,_}} ->
	    load_user_module(),
	    wings__magnet:User();
	Fun when is_function(Fun) -> Fun
    end.

magnet_move([{free,Vs}|Tvs], Dx, Dy, IR, St, OVtab, Vtab0) ->
    wings_drag:message([Dx,Dy], distance),
    #view{azimuth=Az,elevation=El} = wings_view:current(),
    M0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    M = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    {Xt,Yt,Zt} = e3d_mat:mul_point(M, {Dx,Dy,0.0}),
    Vtab = magnet_move_1({Xt,Yt,Zt}, Vs, IR, OVtab, Vtab0),
    magnet_move(Tvs, Dx, Dy, IR, St, OVtab, Vtab);
magnet_move([{{Xt0,Yt0,Zt0},Vs}|Tvs], Dx, Dy, IR, St, OVtab, Vtab0) ->
    wings_drag:message([Dx], distance),
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    Vtab = magnet_move_1({Xt,Yt,Zt}, Vs, IR, OVtab, Vtab0),
    magnet_move(Tvs, Dx, Dy, IR, St, OVtab, Vtab);
magnet_move([], Dx, Dy, IR, St, OVtab, Vtab) -> Vtab.

magnet_move_1(VtVec, Vs, IR, OVtab, Vtab) ->
    foldl(fun(V, Tab) -> 
		  Center = wings_vertex:pos(V, OVtab),
		  magnet_move_2(VtVec, Center, IR, OVtab, Tab)
	  end, Vtab, Vs).

magnet_move_2(TrVec, Center, {DF,IR}, OVtab, Vtab) ->
    wings_util:fold_vertex(
      fun (V, #vtx{pos=Pos0}=Vtx, Tab) ->
	      Dist0 = e3d_vec:dist(Pos0, Center),
	      case DF(Dist0, IR) of
		  Dist when Dist < 1.0E-5 -> Tab;
		  Dist ->
		      #vtx{pos=Pos1} = gb_trees:get(V, Vtab),
		      Offset = e3d_vec:mul(TrVec, Dist),
		      Pos = e3d_vec:add(Pos1, Offset),
		      gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab)
	      end
      end, Vtab, OVtab).

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

parse_str(Str) ->
    case erl_scan:string(Str) of
	{ok,Tokens,_} ->
	    case erl_parse:parse_exprs(Tokens ++ [{dot, 1}]) of
		{ok,Term} -> Term;
		{error,{_,_,Reason}} -> throw({command_error,Reason})
	    end;
	{error, {_,_,Reason}, _} -> throw({command_error,Reason})
    end.

make_module(Name, Funcs) ->
    [{attribute,1,module,Name},
     {attribute,0,compile,export_all}|Funcs ++ [{eof,0}]].

make_function(Name, Args, Body) ->
    {function,1,Name,length(Args),[{clause,1,Args,[],Body}]}.

make_var(Var) ->
    {var,1,Var}.
