%%
%%  wings_body.erl --
%%
%%     This module contains most of the command for entire Wings objects.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_body.erl,v 1.30 2002/04/11 08:20:39 bjorng Exp $
%%

-module(wings_body).
-export([menu/3,command/2,convert_selection/1,auto_smooth/1]).

-include("wings.hrl").
-import(wings_draw, [model_changed/1]).
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,seq/2]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    XYZ = wings_menu_util:xyz(),
    Menu = [{"Object operations",ignore},
	    separator,
	    {"Move",{move,Dir}},
	    wings_menu_util:rotate(),
	    wings_menu_util:scale(),
	    separator,
	    {"Flip",{flip,XYZ}},
	    separator,
	    {"Invert",invert},
	    separator,
	    {"Tighten",tighten},
	    {"Smooth",smooth},
	    {"Combine",combine},
	    {"Separate",separate},
	    separator,
	    {"Cleanup",cleanup,[option]},
	    {"Auto-Smooth",auto_smooth,[option]},
	    separator,
	    {"Duplicate",{duplicate,Dir}},
	    {"Delete",delete}|wings_vec:menu(St)],
    wings_menu:popup_menu(X, Y, body, Menu, St).

command(invert, St) ->
    {save_state,model_changed(invert_normals(St))};
command({duplicate,Dir}, St) ->
    duplicate(Dir, St);
command(delete, St) ->
    {save_state,model_changed(delete(St))};
command(tighten, St) ->
    tighten(St);
command(smooth, St) ->
    ?SLOW({save_state,model_changed(smooth(St))});
command(combine, St) ->
    {save_state,model_changed(combine(St))};
command(separate, St) ->
    {save_state,model_changed(separate(St))};
command(auto_smooth, St) ->
    auto_smooth(St);
command({auto_smooth,Ask}, St) ->
    auto_smooth(Ask, St);
command({flip,Plane}, St) ->
    {save_state,model_changed(flip(Plane, St))};
command(cleanup, St) ->
    cleanup(false, St);
command({cleanup,Ask}, St) ->
    cleanup(Ask, St);
command(collapse, St) ->
    {save_state,model_changed(wings_collapse:collapse(St))};
command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St).

%%
%% Convert the current selection to a body selection.
%%
convert_selection(#st{sel=Sel0}=St) ->
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || {Id,_} <- Sel0],
    wings_sel:set(body, Sel, St).

%%%
%%% The Cleanup command.
%%%

cleanup(Ask, St) when is_atom(Ask) ->
    Qs = [{"Short edges",true,[{key,short_edges}]},
	  {hframe,
	   [{label,"Length tolerance"},{text,1.0E-3,[{range,{1.0E-5,10.0}}]}]},
	  {"Isolated Vertices",true,[{key,isolated_vs}]},
	  {hframe,
	   [{label,"Maximum Angle"},{text,1.0,[{range,{1.0E-5,180.0}}]}]}],
    wings_ask:dialog(Ask,
		  [{vframe, Qs, [{title,"Remove"}]}], St,
		  fun(Res) -> {body,{cleanup,Res}} end);
cleanup(Opts, St0) ->
    St = wings_sel:map(fun(_, We) -> cleanup_1(Opts, We) end, St0),
    {save_state,model_changed(St)}.

cleanup_1([{short_edges,Flag},Tolerance|Opts], We0) ->
    We = case Flag of
	     true -> clean_short_edges(Tolerance, We0);
	     false -> We0
	 end,
    cleanup_1(Opts, We);
cleanup_1([{isolated_vs,true},Angle|Opts], We) ->
    Cos = cos_degrees(Angle),
    cleanup_1(Opts, clean_isolated_vertices(Cos, We));
cleanup_1([_|Opts], We) ->
    cleanup_1(Opts, We);
cleanup_1([], We) -> We.

clean_isolated_vertices(Cos, #we{vs=Vtab}=We) ->
    foldl(fun({V,#vtx{pos=Pos}}, W) ->
		  clean_isolated(Cos, V, Pos, W)
	  end, We, gb_trees:to_list(Vtab)).

clean_isolated(Cos, V, Pos, We) ->
    Es = wings_vertex:fold(
	   fun(_, _, Rec, A) ->
		   OtherPos = wings_vertex:other_pos(V, Rec, We),
		   [e3d_vec:norm(e3d_vec:sub(Pos, OtherPos))|A]
	   end, [], V, We),
    case Es of
	[E1,E2] ->
	    case abs(e3d_vec:dot(E1, E2)) of
		Dot when Dot > Cos ->
		    case wings_vertex:dissolve(V, We) of
			error -> We;
			Other -> Other
		    end;
		_ -> We
	    end;
	_ -> We
    end.

clean_short_edges(Tolerance, #we{es=Etab,vs=Vtab}=We) ->
    Short = foldl(
	      fun({Edge,#edge{vs=Va,ve=Vb}}, A) ->
		      VaPos = wings_vertex:pos(Va, Vtab),
		      VbPos = wings_vertex:pos(Vb, Vtab),
		      case abs(e3d_vec:dist(VaPos, VbPos)) of
			  Dist when Dist < Tolerance -> [Edge|A];
			  Dist -> A
		      end
	      end, [], gb_trees:to_list(Etab)),
    foldl(fun(Edge, #we{es=Et}=W) ->
		  case gb_trees:is_defined(Edge, Et) of
		      true -> wings_collapse:collapse_edge(Edge, W);
		      false -> W
		  end
	  end, We, Short).

%%%
%%% The Invert command.
%%%

invert_normals(St) ->
    wings_sel:map(fun(_, We) -> wings_we:invert_normals(We) end, St).

%%%
%%% The Duplicate command.
%%%

duplicate(Dir, #st{onext=Oid0}=St0) ->
    Copy = "copy",
    St = wings_sel:fold(fun(_, We, St) ->
				wings_shape:insert(We, Copy, St)
			end, St0, St0),
    %% Select the duplicate items, not the original items.
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || Id <- seq(Oid0, St#st.onext-1)],
    wings_move:setup(Dir, wings_sel:set(Sel, St)).

%%%
%%% The Delete command.
%%%

delete(#st{shapes=Shapes0}=St) ->
    Shapes = wings_sel:fold(fun(_, #we{id=Id}, Shs) ->
				    gb_trees:delete(Id, Shs)
			    end, Shapes0, St),
    St#st{shapes=Shapes,sel=[]}.

%%%
%%% The Flip command
%%%

flip(Plane0, St) ->
    Plane = flip_scale(Plane0),
    wings_sel:map(fun(_, We) -> flip_body(Plane, We) end, St).

flip_body(Plane, We0) ->
    {Cx,Cy,Cz} = e3d_vec:average(wings_vertex:bounding_box(We0)),
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, Plane),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    We = wings_we:transform_vs(M, We0),
    wings_we:invert_normals(We).

flip_scale(x) -> e3d_mat:scale(-1.0, 1.0, 1.0);
flip_scale(y) -> e3d_mat:scale(1.0, -1.0, 1.0);
flip_scale(z) -> e3d_mat:scale(1.0, 1.0, -1.0).

%%%
%%% The Tighten command.
%%%

tighten(St) ->
    Tvs = wings_sel:fold(fun tighten/3, [], St),
    wings_drag:setup(Tvs, [percent], St).

tighten(_, #we{vs=Vtab}=We, A) ->
    Vs = gb_trees:keys(Vtab),
    wings_vertex_cmd:tighten(Vs, We, A).
    
%%%
%%% The Smooth command.
%%%
%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.
%%% 

smooth(St) ->
    wings_sel:map(
      fun(_, #we{name=Name}=We) ->
	      wings_io:progress("Smoothing \"" ++ Name ++ "\""),
	      wings_subdiv:smooth(We)
      end, St).

%%%
%%% The Combine command.
%%%

combine(#st{sel=[]}=St) -> St;
combine(#st{shapes=Shs0,sel=[{Id,_}=S|_]=Sel0}=St) ->
    Shs1 = sofs:from_external(gb_trees:to_list(Shs0), [{id,object}]),
    Sel1 = sofs:from_external(Sel0, [{id,dummy}]),
    Sel2 = sofs:domain(Sel1),
    Wes = sofs:to_external(sofs:range(sofs:restriction(Shs1, Sel2))),
    Mode = unify_modes(Wes),
    We0 = wings_we:merge(Wes),
    We = We0#we{id=Id,mode=Mode},
    Shs2 = sofs:drestriction(Shs1, Sel2),
    Shs = gb_trees:from_orddict(sort([{Id,We}|sofs:to_external(Shs2)])),
    St#st{shapes=Shs,sel=[S]}.

unify_modes([#we{mode=Mode}|Wes]) ->
    unify_modes(Wes, Mode).

unify_modes([#we{mode=Mode}|Wes], Mode) ->
    unify_modes(Wes, Mode);
unify_modes([#we{mode=Mode}|Wes], OldMode) ->
    NewMode = unify_modes_1(sort([OldMode,Mode])),
    unify_modes(Wes, NewMode);
unify_modes([], Mode) -> Mode.

unify_modes_1([_,vertex]) ->
    throw({command_error,"Objects with vertex colors cannot be combined "
	   "with objects with materials and/or textures."});
unify_modes_1([material,uv]) -> uv.

%%%
%%% The Separate command.
%%%

separate(St) ->
    Sep = "sep",
    wings_sel:fold(
      fun(_, #we{id=Id}=We0, St0) ->
	      case wings_we:separate(We0) of
		  [_] -> St0;
		  [We|Wes] ->
		      St1 = foldl(fun(W, A) ->
					  wings_shape:insert(W, Sep, A)
				  end, St0, Wes),
		      wings_shape:replace(Id, We, St1)
	      end
      end, St, St).

%%%
%%% The Auto-Smooth command.
%%%

auto_smooth(St) ->
    do_auto_smooth(60, St).

auto_smooth(Ask, St) when is_atom(Ask) ->
    wings_ask:ask(Ask, [{"Crease Angle",60,[{range,{0,180}}]}], St,
		  fun(Res) -> {body,{auto_smooth,Res}} end);
auto_smooth([Angle], St) ->
    {save_state,model_changed(do_auto_smooth(Angle, St))}.

do_auto_smooth(Angle, St) ->
    Cos = cos_degrees(Angle),
    wings_sel:map(fun(_, We) -> auto_smooth_1(Cos, We) end, St).

auto_smooth_1(Cos, #we{es=Etab,he=Htab0}=We) ->
    Htab = foldl(fun({E,R}, A) ->
			 auto_smooth(E, R, Cos, A, We)
		 end, Htab0, gb_trees:to_list(Etab)),
    We#we{he=Htab}.

auto_smooth(Edge, #edge{lf=Lf,rf=Rf}, Cos, H0, We) ->
    Ln = wings_face:normal(Lf, We),
    Lr = wings_face:normal(Rf, We),
    case e3d_vec:is_zero(Ln) orelse e3d_vec:is_zero(Lr) of
	true -> H0;				%Ignore this edge.
	false ->
	    case e3d_vec:dot(Ln, Lr) of
		P when P < Cos ->
		    wings_edge:hardness(Edge, hard, H0);
		P ->				%angle =< 60
		    wings_edge:hardness(Edge, soft, H0)
	    end
    end.

cos_degrees(Angle) ->
    math:cos(Angle*math:pi()/180.0).
