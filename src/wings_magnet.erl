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
%%     $Id: wings_magnet.erl,v 1.24 2002/03/13 11:53:40 bjorng Exp $
%%

-module(wings_magnet).
-export([influences/3]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,sort/1,concat/1]).

influences({magnet,Magnet}, VsSel0, #we{vs=Vtab}=We) ->
    Center = e3d_vec:average(wings_vertex:bounding_box(VsSel0, We)),
    R = e3d_vec:dist(Center, Magnet)*1.10,
    VsSel = foldl(fun(V, A) ->
			  Pos = wings_vertex:pos(V, Vtab),
			  [{V,Pos}|A]
		  end, [], VsSel0),
    inf_1(gb_trees:to_list(Vtab), VsSel, R, []).

inf_1([{V,#vtx{pos=Pos}=Vtx}|T], VsSel, R, Acc) ->
    case inf_2(VsSel, V, Pos, none, R) of
	none -> inf_1(T, VsSel, R, Acc);
	Dist -> inf_1(T, VsSel, R, [{V,Vtx,Dist}|Acc])
    end;
inf_1([], _VsSel, _OldR, Acc) ->
    R = foldl(fun({_,_,Dist}, D) when Dist > D -> Dist;
		 (_, D) -> D
	      end, 0, Acc),
    foldl(fun({V,Vtx,Dist}, A) ->
		  [{V,Vtx,mf(dome, Dist, R)}|A]
	  end, [], Acc).
    
inf_2([{V,_}|_], V, _, _, _) -> none;
inf_2([{_,Pos}|T], V, VPos, Prev, R) ->
    case e3d_vec:dist(Pos, VPos) of
	Dist when Dist < R -> inf_2(T, V, VPos, V, Dist);
	_Dist -> inf_2(T, V, VPos, Prev, R)
    end;
inf_2([], _, _, none, _Dist) -> none;
inf_2([], _, _, _, Dist) -> Dist.

mf(bell, D, R) -> math:sin((R-D)/R*3.1416);
mf(dome, D, R) -> math:sin((R-D)/R*3.1416/2).
