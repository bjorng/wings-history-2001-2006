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
%%     $Id: wings_magnet.erl,v 1.26 2002/03/18 06:15:00 bjorng Exp $
%%

-module(wings_magnet).
-export([setup/3,transform/2,update_vpos/2,influences/2,cleanup/0]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,sort/1,concat/1]).

setup({magnet,Type,Magnet}, VsSel0, #we{vs=Vtab}) ->
    R = radius(Magnet, VsSel0, Vtab),
    if
	R < 1.0E-6 ->
	    wings_util:error("Too short influence radius.");
	true ->
	    ok
    end,
    VsSel = foldl(fun(V, A) ->
			  Pos = wings_vertex:pos(V, Vtab),
			  [{V,Pos}|A]
		  end, [], VsSel0),
    VsDist = inf_1(gb_trees:to_list(Vtab), VsSel, R, []),
    Affected = foldl(fun({V,_,_}, A) -> [V|A] end, [], VsDist),
    put(wings_magnet_cache, {none,none}),
    {{magnet,R,Type,VsDist},Affected}.

transform(Trans, {magnet,R,Type,VsDist}) ->
    {magnet,R,Type,transform_1(Trans, VsDist, [])}.

transform_1(Trans, [{V,#vtx{pos=Pos0}=Vtx,Inf}|T], Acc) ->
    Pos = Trans(Pos0),
    transform_1(Trans, T, [{V,Vtx#vtx{pos=Pos},Inf}|Acc]);
transform_1(_Trans, [], Acc) -> Acc.

update_vpos({magnet,R,Type,VsDist}, We) ->
    put(wings_magnet_cache, {none,none}),
    {magnet,R,Type,wings_util:update_vpos(VsDist, We)}.

influences(Sc, {magnet,R0,Type,VsDist}) ->
    case get(wings_magnet_cache) of
	{Sc,VsInf} -> VsInf;
	{_,_} ->
	    R = R0*Sc,
	    VsInf = foldl(fun({V,Vtx,Dist}, A) when Dist =< R ->
				  [{V,Vtx,mf(Type, Dist, R)}|A];
			     ({V,Vtx,_Dist}, A) ->
				  [{V,Vtx,mf(Type, R, R)}|A]
			  end, [], VsDist),
	    put(wings_magnet_cache, {Sc,VsInf}),
	    VsInf
    end.

cleanup() ->
    erase(wings_magnet_cache).

inf_1([{V,#vtx{pos=Pos}=Vtx}|T], VsSel, R, Acc) ->
    case inf_2(VsSel, V, Pos, none, R) of
	none -> inf_1(T, VsSel, R, Acc);
	Dist -> inf_1(T, VsSel, R, [{V,Vtx,Dist}|Acc])
    end;
inf_1([], _VsSel, _OldR, Acc) -> Acc.

inf_2([{V,_}|_], V, _, _, _) -> 0.0;
inf_2([{_,Pos}|T], V, VPos, Prev, R) ->
    case e3d_vec:dist(Pos, VPos) of
	Dist when Dist =< R -> inf_2(T, V, VPos, V, Dist);
	_Dist -> inf_2(T, V, VPos, Prev, R)
    end;
inf_2([], _, _, none, _Dist) -> none;
inf_2([], _, _, _, Dist) -> Dist.

mf(bell, D, R) -> math:sin((R-D)/R*math:pi());
mf(dome, D, R) -> math:sin((R-D)/R*math:pi()/2);
mf(straight, D, R) -> (R-D)/R;
mf(spike, D0, R) when is_float(D0), is_float(R) ->
    D = R-D0,
    D*D/R*R.

radius(Outer, [V0|Vs], Vtab) ->
    foldl(fun(V, R0) ->
		  Pos = wings_vertex:pos(V, Vtab),
		  case e3d_vec:dist(Pos, Outer) of
		      R when R < R0 -> R;
		      _ -> R0
		  end
	  end, e3d_vec:dist(Outer, wings_vertex:pos(V0, Vtab)), Vs).
