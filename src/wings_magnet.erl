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
%%     $Id: wings_magnet.erl,v 1.41 2002/12/26 09:47:08 bjorng Exp $
%%

-module(wings_magnet).
-export([setup/3,transform/2,recalc/3,flags/2,
	 dialog/1,dialog/2,menu_help/0,drag_help/1,hotkey/1]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,sort/1,concat/1,reverse/1]).
-define(HUGE, 9.9E307).

setup({magnet,Type,Route,Point}, VsSel, We) ->
    {VsDist,R} = case Route of
		     shortest -> inf_shortest(Point, VsSel, We);
		     midpoint -> inf_midpoint(Point, VsSel, We);
		     surface -> inf_surface(Point, VsSel, We)
		 end,
    Magnet = {Type,R},
    VsInf = recalc(1.0, VsDist, Magnet),
    Affected = foldl(fun({V,_,_,_}, A) -> [V|A] end, [], VsInf),
    {VsInf,Magnet,Affected}.

transform(Trans, VsInf) ->
    transform_1(Trans, VsInf, []).

transform_1(Trans, [{V,Pos,Dist,Inf}|T], Acc) ->
    transform_1(Trans, T, [{V,Trans(Pos),Dist,Inf}|Acc]);
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

dialog(Fun) ->
    R0 = wings_pref:get_value(magnet_radius),
    wings_ask:dialog(
      [{hframe,[{text,R0}],[{title,"Influence Radius"}]}|common_dialog()],
      fun([R,Route]) ->
	      wings_pref:set_value(magnet_distance_route, Route),
	      Type = wings_pref:get_value(magnet_type),
	      Mag = {magnet,Type,Route,R},
	      Fun(Mag)
      end).

dialog(Point, Fun) ->
    wings_ask:dialog(
      common_dialog(),
      fun([Route]) ->
	      wings_pref:set_value(magnet_distance_route, Route),
	      Type = wings_pref:get_value(magnet_type),
	      Mag = {magnet,Type,Route,Point},
	      Fun(Mag)
      end).

common_dialog() ->
    DefRoute = {route,wings_pref:get_value(magnet_distance_route)},
    [{hframe,
      [{alt,DefRoute,"Shortest",shortest},
       {alt,DefRoute,"Midpoint",midpoint},
       {alt,DefRoute,"Surface",surface}],
      [{title,"Distance Route"}]}].
			  
menu_help() ->
    Msg = "Magnet: [L] Pick influence radius  "
	"[M] Specify radius numerically  "
	"[R] Use last radius",
    Route0 = wings_pref:get_value(magnet_distance_route),
    Route = wings_util:cap(atom_to_list(Route0)),
    wings_wm:message(Msg, ["Route: "|Route]).
    
drag_help(Type) ->
    "[+] or [-] Tweak R  " ++
	help_1(Type, [{1,bell},{2,dome},{3,straight},{4,spike}]).

help_1(Type, [{Digit,Type}|T]) ->
    "[" ++ [$0+Digit] ++ "] <<" ++
	wings_util:cap(atom_to_list(Type)) ++ ">> " ++ help_1(Type, T);
help_1(Type, [{Digit,ThisType}|T]) ->
    "[" ++ [$0+Digit] ++ "] " ++
	wings_util:cap(atom_to_list(ThisType)) ++ " " ++ help_1(Type, T);
help_1(_, []) -> [].

hotkey($1) -> bell;
hotkey($2) -> dome;
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
    D = (R-D0)/R,
    D*D.

check_radius(R) when R < 1.0E-6 ->
    wings_util:error("Too short influence radius.");
check_radius(_) -> ok.

%%%
%%% Calculation of influence radius: Shortest distance route.
%%%

inf_shortest({_,_,_}=Point, VsSel, #we{vp=Vtab}=We) ->
    R = radius(Point, VsSel, Vtab),
    check_radius(R),
    inf_shortest(R, VsSel, We);
inf_shortest(R, VsSel0, #we{vp=Vtab}) when is_number(R) ->
    check_radius(R),
    VsSel = foldl(fun(V, A) ->
			  Pos = gb_trees:get(V, Vtab),
			  [{V,Pos}|A]
		  end, [], VsSel0),
    inf_1(gb_trees:to_list(Vtab), VsSel, R, []).

inf_1([{V,Pos}|T], VsSel, R, Acc) ->
    case inf_2(VsSel, V, Pos, none, R) of
	none -> inf_1(T, VsSel, R, Acc);
	Dist -> inf_1(T, VsSel, R, [{V,Pos,Dist,0}|Acc])
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
		  Pos = gb_trees:get(V, Vtab),
		  case e3d_vec:dist(Pos, Outer) of
		      R when R < R0 -> R;
		      _ -> R0
		  end
	  end, e3d_vec:dist(Outer, gb_trees:get(V0, Vtab)), Vs).

%%%
%%% Calculation of influence radius: Midpoint distance route.
%%%

inf_midpoint({_,_,_}=Point, VsSel, We) ->
    Midpoint = wings_vertex:center(VsSel, We),
    R = e3d_vec:dist(Point, Midpoint),
    inf_midpoint_1(R, Midpoint, VsSel, We);
inf_midpoint(R, VsSel, We) when is_number(R) ->
    check_radius(R),
    Midpoint = wings_vertex:center(VsSel, We),
    inf_midpoint_1(R, Midpoint, VsSel, We).

inf_midpoint_1(R, Midpoint, Vs0, #we{vp=Vtab}) ->
    Vs = gb_sets:from_list(Vs0),
    {foldl(fun({V,Pos}, A) ->
		   case e3d_vec:dist(Pos, Midpoint) of
		       Dist when Dist =< R ->
			   [{V,Pos,Dist,0}|A];
		       _Dist ->
			   case gb_sets:is_member(V, Vs) of
			       false -> A;
			       true -> [{V,Pos,R,0}|A]
			   end
		   end
	   end, [], gb_trees:to_list(Vtab)),R}.

%%%
%%% Calculation of influence radius: Surface distance route.
%%%

inf_surface({_,_,_}=Point, VsSel, We) ->
    %% Find the reachable vertex nearest to the given point.
    Vs = wings_vertex:reachable(VsSel, We),
    Limit = inf_surface_nearest(Vs, Point, We),
    inf_surface_0(Limit, VsSel, We);
inf_surface(Limit, VsSel, We) ->
    inf_surface_0(Limit, VsSel, We).

inf_surface_0(Limit, VsSel, We) ->
    ?SLOW(ok),
    Ws0 = foldl(fun(V, A) -> [{0.0,V}|A] end, [], VsSel),
    Ws = gb_sets:from_list(Ws0),
    inf_surface_1(Ws, Limit, We, gb_trees:empty()).
    
inf_surface_1(Ws0, Limit, We, Vs0) ->
    %% Starting from the working set (the selected vertices),
    %% calculate the shortest surface path for each vertex.
    case gb_sets:is_empty(Ws0) of
	true ->
	    inf_surface_3(Vs0, Limit, We);
	false ->
	    {{Dist,V},Ws} = gb_sets:take_smallest(Ws0),
	    inf_surface_2(Ws, Dist, V, Limit, We, Vs0)
    end.

inf_surface_2(Ws, Dist0, V, {V,NearDist}, We, Vs) ->
    %% We have found the surface distance to the vertex
    %% nearest to the limit point for the influcence radius.
    Dist = Dist0 + NearDist,
    inf_surface_2(Ws, Dist0, V, Dist, We, Vs);
inf_surface_2(_Ws, Dist, _V, Limit, We, Vs)
  when is_number(Limit), Dist > Limit ->
    %% The current vertex in the workset is outside the Limit distance.
    %% As the workset is sorted in distance order, all others in the
    %% workset are outside too.
    inf_surface_3(Vs, Limit, We);
inf_surface_2(Ws0, Dist, V, Limit, We, VsDist0) ->
    case gb_trees:lookup(V, VsDist0) of
	none ->
	    %% There is no previous surface distance calculated
	    %% for this vertex.
	    VsDist = gb_trees:insert(V, Dist, VsDist0),
	    Ws = inf_surface_uws(Ws0, Dist, V, We, VsDist),
	    inf_surface_1(Ws, Limit, We, VsDist);
	{value,{V,OldDist}} when Dist < OldDist ->
	    %% We have found a shorter surface route to this
	    %% vertex than the previously stored one.
	    VsDist = gb_trees:update(V, Dist, VsDist0),
	    Ws = inf_surface_uws(Ws0, Dist, V, We, VsDist),
	    inf_surface_1(Ws, Limit, We, VsDist);
	{value,_} ->
	    %% The previously calculated distance to this vertex
	    %% is shorter. Ignore this distance.
	    inf_surface_1(Ws0, Limit, We, VsDist0)
    end.

inf_surface_uws(Ws, Dist, V, #we{vp=Vtab}=We, VsDist) ->        
    %% We now have the shortest surface distance found so far
    %% to this vertex. Calculate the surface distance to all
    %% bordering vertices and update the work set.
    BorderVs = all_bordering(V, We),
    Pos = gb_trees:get(V, Vtab),
    inf_surface_uws_1(BorderVs, Pos, Dist, Vtab, VsDist, Ws).

inf_surface_uws_1([V|Vs], Vpos, Dist0, Vtab, VsDist, Ws0) ->
    Dist = Dist0 + e3d_vec:dist(Vpos, gb_trees:get(V, Vtab)),
    case gb_trees:lookup(V, VsDist) of
	none ->
	    Ws = gb_sets:add({Dist,V}, Ws0),
	    inf_surface_uws_1(Vs, Vpos, Dist0, Vtab, VsDist, Ws);
	{value,{_,OldDist}} when Dist < OldDist ->
	    Ws = gb_sets:add({Dist,V}, Ws0),
	    inf_surface_uws_1(Vs, Vpos, Dist0, Vtab, VsDist, Ws);
	{value,_} ->
	    inf_surface_uws_1(Vs, Vpos, Dist0, Vtab, VsDist, Ws0)
    end;
inf_surface_uws_1([], _Vpos, _Dist, _Vtab, _VsDist, Ws) -> Ws.

inf_surface_3(VsDist, Limit, #we{vp=Vtab}) ->
    check_radius(Limit),
    {foldl(fun({V,Dist}, A) when Dist =< Limit ->
		   [{V,gb_trees:get(V, Vtab),Dist,0}|A];
	      (_, A) -> A
	   end, [], gb_trees:to_list(VsDist)),Limit}.

inf_surface_nearest([V|Vs], Point, #we{vp=Vtab}) ->
    Dist = e3d_vec:dist(gb_trees:get(V, Vtab), Point),
    inf_surface_nearest_1(Vs, Point, Vtab, Dist, V).

inf_surface_nearest_1([V|Vs], Point, Vtab, OldDist, OldV) ->
    case e3d_vec:dist(gb_trees:get(V, Vtab), Point) of
	Dist when Dist < OldDist ->
	    inf_surface_nearest_1(Vs, Point, Vtab, Dist, V);
	_Dist ->
	    inf_surface_nearest_1(Vs, Point, Vtab, OldDist, OldV)
    end;
inf_surface_nearest_1([], _, _, Dist, V) -> {V,Dist}.

all_bordering(V, We) ->
    Faces = wings_vertex:fold(fun(_, Face, _, A) -> [Face|A] end, [], V, We),
    wings_face:to_vertices(Faces, We).
