%%
%%  wings_magnet.erl --
%%
%%     This module implements the Magnet command.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_magnet.erl,v 1.31 2002/03/25 09:54:15 bjorng Exp $
%%

-module(wings_magnet).
-export([setup/3,transform/2,recalc/3,flags/2,
	 menu_help/0,drag_help/1,hotkey/1]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,sort/1,concat/1,reverse/1]).
-define(HUGE, 9.9E307).

setup({magnet,Type,Route,Point}, VsSel, We) ->
    {VsDist,R} = case Route of
		     shortest -> inf_shortest(Point, VsSel, We);
		     surface -> inf_surface(Point, VsSel, We)
		 end,
    Magnet = {Type,R},
    VsInf = recalc(1.0, VsDist, Magnet),
    Affected = foldl(fun({V,_,_,_}, A) -> [V|A] end, [], VsInf),
    {VsInf,Magnet,Affected}.

transform(Trans, VsInf) ->
    transform_1(Trans, VsInf, []).

transform_1(Trans, [{V,#vtx{pos=Pos0}=Vtx,Dist,Inf}|T], Acc) ->
    Pos = Trans(Pos0),
    transform_1(Trans, T, [{V,Vtx#vtx{pos=Pos},Dist,Inf}|Acc]);
transform_1(_Trans, [], Acc) -> Acc.

recalc(Sc, VsInf, {Type,R0}) ->
    R = R0*Sc,
    foldl(fun({V,Vtx,Dist,_}, A) when Dist =< R ->
		  [{V,Vtx,Dist,mf(Type, Dist, R)}|A];
	     ({V,Vtx,Dist,_}, A) ->
		  [{V,Vtx,Dist,mf(Type, R, R)}|A]
	  end, [], VsInf).

flags(none, Flags) -> Flags;
flags({magnet,Type,_,_}, Flags) -> [{magnet,Type}|Flags].

menu_help() ->
    "Magnet: " ++
	[lmb] ++ " Pick influence radius  " ++
	[mmb] ++ " Use last radius".
    
drag_help(Type) ->
    "[+] or [-] Tweak R  " ++
	help_1(Type, [{1,dome},{2,bell},{3,straight},{4,spike}]).

help_1(Type, [{Digit,Type}|T]) ->
    "[" ++ [$0+Digit] ++ "] <<" ++
	wings_util:cap(atom_to_list(Type)) ++ ">> " ++ help_1(Type, T);
help_1(Type, [{Digit,ThisType}|T]) ->
    "[" ++ [$0+Digit] ++ "] " ++
	wings_util:cap(atom_to_list(ThisType)) ++ " " ++ help_1(Type, T);
help_1(_, []) -> [].

hotkey($1) -> dome;
hotkey($2) -> bell;
hotkey($3) -> straight;
hotkey($4) -> spike;
hotkey(_) -> none.
    
%%%
%%% Local functions.
%%%

mf(bell, D, R) -> math:sin((R-D)/R*math:pi());
mf(dome, D, R) -> math:sin((R-D)/R*math:pi()/2);
mf(straight, D, R) -> (R-D)/R;
mf(spike, D0, R) when is_float(D0), is_float(R) ->
    D = R-D0,
    D*D/R*R.

%%%
%%% Calculation of influence radius: Shortest distance route.
%%%

inf_shortest({_,_,_}=Point, VsSel, #we{vs=Vtab}=We) ->
    R = radius(Point, VsSel, Vtab),
    if
	R < 1.0E-6 ->
	    wings_util:error("Too short influence radius.");
	true ->
	    ok
    end,
    inf_shortest(R, VsSel, We);
inf_shortest(R, VsSel0, #we{vs=Vtab}) when is_number(R) ->
    VsSel = foldl(fun(V, A) ->
			  Pos = wings_vertex:pos(V, Vtab),
			  [{V,Pos}|A]
		  end, [], VsSel0),
    inf_1(gb_trees:to_list(Vtab), VsSel, R, []).

inf_1([{V,#vtx{pos=Pos}=Vtx}|T], VsSel, R, Acc) ->
    case inf_2(VsSel, V, Pos, none, R) of
	none -> inf_1(T, VsSel, R, Acc);
	Dist -> inf_1(T, VsSel, R, [{V,Vtx,Dist,0}|Acc])
    end;
inf_1([], _VsSel, R, Acc) -> {Acc,R}.

inf_2([{V,_}|_], V, _, _, _) -> 0.0;
inf_2([{_,Pos}|T], V, VPos, Prev, R) ->
    case e3d_vec:dist(Pos, VPos) of
	Dist when Dist =< R -> inf_2(T, V, VPos, V, Dist);
	_Dist -> inf_2(T, V, VPos, Prev, R)
    end;
inf_2([], _, _, none, _Dist) -> none;
inf_2([], _, _, _, Dist) -> Dist.

radius(Outer, [V0|Vs], Vtab) ->
    foldl(fun(V, R0) ->
		  Pos = wings_vertex:pos(V, Vtab),
		  case e3d_vec:dist(Pos, Outer) of
		      R when R < R0 -> R;
		      _ -> R0
		  end
	  end, e3d_vec:dist(Outer, wings_vertex:pos(V0, Vtab)), Vs).

%%%
%%% Calculation of influence radius: Surface distance route.
%%%
-define(FUDGE_MARGIN, 2.0).

inf_surface({_,_,_}=Point, VsSel, We) ->
    %% Step 1: Rough filtering. Calculate the center and radius
    %% of a bounding sphere which will enclose any vertices that
    %% potentially would be inside the influence radius.
    ?SLOW(ok),
    Middle = e3d_vec:average(wings_vertex:bounding_box(VsSel, We)),
    case e3d_vec:dist(Point, Middle) of
	R when R < 1.0E-6 ->
	    wings_util:error("Too short influence radius.");
	R ->
	    inf_surface_1(Middle, R*?FUDGE_MARGIN, Point, VsSel, We)
    end;
inf_surface(InfRadius, VsSel, We) ->
    Middle = e3d_vec:average(wings_vertex:bounding_box(VsSel, We)),
    inf_surface_1(Middle, InfRadius*?FUDGE_MARGIN, InfRadius, VsSel, We).

inf_surface_1(Middle, R, Point, VsSel0, #we{vs=Vtab}=We) ->
    %% Step 2: Build the list of all vertices inside the bounding sphere.
    VsSel = gb_sets:from_list(VsSel0),
    NearVs0 = foldl(fun({V,#vtx{pos=Pos}=Vtx}, A) ->
			   case e3d_vec:dist(Pos, Middle) of
			       D when D < R ->
				   Rdist = init_route_dist(V, VsSel),
				   [{V,{Vtx,Rdist}}|A];
			       _ -> A
			   end
		   end, [], gb_trees:to_list(Vtab)),
    NearVs = gb_trees:from_orddict(reverse(NearVs0)),
    inf_surface_2(VsSel, NearVs, Point, We).
    
inf_surface_2(Ws0, Vs0, Point, We) ->
    %% Step 3: Starting from the working set (the selected vertices),
    %% calculate the shortest surface path for each vertex for all
    %% vertices inside the bounding sphere (Vs).
    case gb_trees:is_empty(Ws0) of
	true ->
	    inf_surface_3(gb_trees:to_list(Vs0), Point);
	false ->
	    {V,Ws1} = gb_sets:take_smallest(Ws0),
	    {Ws,Vs} = update_ws(V, We, Ws1, Vs0),
	    inf_surface_2(Ws, Vs, Point, We)
    end.

inf_surface_3(Vs, {_,_,_}=Point) ->
    %% Step 4: We now want to calculate surface distance of the original
    %% falloff point given (Point) to the nearest of the selected vertices.
    %% As an approximation, we use the distance of the vertex that is nearest
    %% to Point.
    {_,MaxDist} =
	foldl(fun({_,{#vtx{pos=P},Dist}}, {EuclD,_SurfD}=A) ->
		      case e3d_vec:dist(P, Point) of
			  D when D < EuclD, Dist < ?HUGE -> {D,Dist};
			  _ -> A
		      end
	      end, {?HUGE,none}, Vs),
    wings_pref:set_value(magnet_radius, MaxDist),
    inf_surface_3(Vs, MaxDist);
inf_surface_3(Vs, MaxDist) ->
    %% Step 5: Throw away vertices that are too far away.
    {foldl(fun({V,{Vtx,Dist}}, A) when Dist =< MaxDist ->
		   [{V,Vtx,Dist,0}|A];
	      (_, A) -> A
	   end, [], Vs),MaxDist}.

init_route_dist(V, VsSel) ->
    case gb_sets:is_member(V, VsSel) of
	true -> 0.0;
	false -> ?HUGE
    end.

update_ws(V, #we{vs=Vtab}=We, Ws, VsInfo) ->
    BorderVs = all_bordering(V, We),
    {_,Dist} = gb_trees:get(V, VsInfo),
    Pos = wings_vertex:pos(V, Vtab),
    update_ws_1(BorderVs, Pos, Dist, We, Ws, VsInfo).

update_ws_1([V|BorderVs], Vpos, Dist0, Vtab, Ws0, VsInfo0) ->
    case gb_trees:lookup(V, VsInfo0) of
	none ->
	    update_ws_1(BorderVs, Vpos, Dist0, Vtab, Ws0, VsInfo0);
	{value,{Vtx,OldDist}} ->
	    case Dist0+e3d_vec:dist(Vpos, wings_vertex:pos(V, Vtab)) of
		Dist when Dist < OldDist ->
		    Ws = gb_sets:add(V, Ws0),
		    VsInfo = gb_trees:enter(V, {Vtx,Dist}, VsInfo0),
		    update_ws_1(BorderVs, Vpos, Dist0, Vtab, Ws, VsInfo);
		_ ->
		    update_ws_1(BorderVs, Vpos, Dist0, Vtab, Ws0, VsInfo0)
	    end
    end;
update_ws_1([], _Vpos, _Dist, _Vtab, Ws, VsInfo) -> {Ws,VsInfo}.

all_bordering(V, We) ->
    Faces = wings_vertex:fold(fun(_, Face, _, A) -> [Face|A] end, [], V, We),
    wings_face:to_vertices(Faces, We).
