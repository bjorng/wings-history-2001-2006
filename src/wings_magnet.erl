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
%%     $Id: wings_magnet.erl,v 1.27 2002/03/20 20:35:04 bjorng Exp $
%%

-module(wings_magnet).
-export([setup/3,transform/2,recalc/3]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,sort/1,concat/1]).

setup({magnet,Type,Point}, VsSel0, #we{vs=Vtab}) ->
    R = radius(Point, VsSel0, Vtab),
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

inf_1([{V,#vtx{pos=Pos}=Vtx}|T], VsSel, R, Acc) ->
    case inf_2(VsSel, V, Pos, none, R) of
	none -> inf_1(T, VsSel, R, Acc);
	Dist -> inf_1(T, VsSel, R, [{V,Vtx,Dist,0}|Acc])
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
