%%
%%  wpc_region.erl --
%%
%%     Plug-in with region and edge-loop commands.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_region.erl,v 1.5 2002/08/09 06:19:59 bjorng Exp $
%%

-module(wpc_region).

-export([init/0,menu/2,command/2]).

-include_lib("wings.hrl").

-import(lists, [map/2,foldr/3,foldl/3,reverse/1,append/1]).
-define(HUGE, 1.0E300).

init() ->
    true.

menu({face,move}, Menu0) ->
    Menu0 ++ [separator,
	      {"Region",region,
	       "Move region along the normal of its edge loop."}];
menu({face,scale}, Menu0) ->
    Menu0 ++ [separator,
	      {"Region",region,
	       "Scale region from the plane of its edge loop."}];
menu({face,rotate}, Menu0) ->
    Menu0 ++ [separator,
	      {"Region",region,
	       "Rotate region around the normal of its edge loop."}];
menu({face,flatten}, Menu0) ->
    Menu0 ++ [separator,
	      {"Region",region,
	       "Flatten region to the normal of its edge loop."}];
menu(_, Menu) -> Menu.

command({face,{move,region}}, St) ->
    Tvs = wpa:sel_fold(
	    fun(Faces, We, Acc) ->
		    Id = wpa:obj_id(We),
		    [{Id,move_region(Faces, We)}|Acc]
	    end, [], St),
    wpa:drag(Tvs, [distance], St);
command({face,{scale,region}}, St) ->
    Tvs = wpa:sel_fold(
	    fun(Faces, We, Acc) ->
		    Id = wpa:obj_id(We),
		    [{Id,scale_region(Faces, We)}|Acc]
	    end, [], St),
    wpa:drag(Tvs, [{percent,{-1.0,?HUGE}}], St);
command({face,{rotate,region}}, St) ->
    Tvs = wpa:sel_fold(
	    fun(Faces, We, Acc) ->
		    rotate_region(Faces, We, Acc)
	    end, [], St),
    wpa:drag(Tvs, [angle], St);
command({face,{flatten,region}}, St) ->
    wpa:sel_map(
      fun(Faces, We) ->
	      flatten_region(Faces, We)
      end, St);
command(_, _) -> next.

%%%
%%% Move Region.
%%%

move_region(Faces, We) ->
    move_region(wpa:sel_face_regions(Faces, We), We, []).

move_region([Fs|Regs], We, Acc0) ->
    Acc = case wpa:face_outer_vertices(Fs, We) of
	      [OuterVs] -> move_region(OuterVs, Fs, We, Acc0);
	      _ -> region_error()
	  end,
    move_region(Regs, We, Acc);
move_region([], _, Acc) -> Acc.

move_region(OuterVs0, Faces, We, Acc) ->
    OuterVs = reverse(OuterVs0),
    PlaneNormal = wings_face:face_normal(OuterVs, We),
    [{PlaneNormal,wings_face:to_vertices(Faces, We)}|Acc].

%%%
%%% Scale Region.
%%%

scale_region(Faces, We) ->
    scale_region(wpa:sel_face_regions(Faces, We), We, []).

scale_region([Fs|Regs], We, Acc0) ->
    Acc = case wpa:face_outer_vertices(Fs, We) of
	      [OuterVs] -> scale_region(OuterVs, Fs, We, Acc0);
	      _ -> region_error()
	  end,
    scale_region(Regs, We, Acc);
scale_region([], _, Acc) -> Acc.

scale_region(OuterVs0, Faces, We, Acc) ->
    OuterVs = reverse(OuterVs0),
    PlaneNormal = wings_face:face_normal(OuterVs, We),
    WeTemp = wpa:vertex_flatten(OuterVs, PlaneNormal, We),
    Center = wings_vertex:center(OuterVs, WeTemp),
    foldl(fun(V, A) ->
		  Pos = wpa:vertex_pos(V, We),
		  Vec = e3d_vec:sub(Pos, Center),
		  [{Vec,[V]}|A]
	  end, Acc, wings_face:to_vertices(Faces, We)).

%%%
%%% Rotate Region.
%%%

rotate_region(Faces, We, Acc) ->
    rotate_region_1(wpa:sel_face_regions(Faces, We), We, Acc).

rotate_region_1([Fs|Regs], We, Acc0) ->
    Acc = case wpa:face_outer_vertices(Fs, We) of
	      [OuterVs] -> rotate_region(OuterVs, Fs, We, Acc0);
	      _ -> region_error()
	  end,
    rotate_region_1(Regs, We, Acc);
rotate_region_1([], _, Acc) -> Acc.

rotate_region(OuterVs0, Faces, We, Acc) ->
    OuterVs = reverse(OuterVs0),
    PlaneNormal = wings_face:face_normal(OuterVs, We),
    Vs = wings_face:to_vertices(Faces, We),
    Center = wings_vertex:center(OuterVs, We),
    VsPos = wings_util:add_vpos(Vs, We),
    Id = wpa:obj_id(We),
    [{Id,{Vs,rotate_fun(Center, VsPos, PlaneNormal)}}|Acc].

rotate_fun(Center, VsPos, Axis) ->
    fun([Angle], A) ->
	    rotate(Center, Axis, Angle, VsPos, A)
    end.

rotate({Cx,Cy,Cz}, Axis, Angle, VsPos, Acc0) ->
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Axis)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    foldl(fun({V,#vtx{pos=Pos0}=Vtx}, Acc) ->
		  Pos = e3d_mat:mul_point(M, Pos0),
		  [{V,Vtx#vtx{pos=Pos}}|Acc]
	  end, Acc0, VsPos).

%%%
%%% Flatten Region.
%%%

flatten_region(Faces, We) ->
    flatten_region_1(wpa:sel_face_regions(Faces, We), We).

flatten_region_1([Fs|Regs], We0) ->
    We = case wpa:face_outer_vertices(Fs, We0) of
	     [OuterVs] -> flatten_region_2(OuterVs, We0);
	     _ -> region_error()
	 end,
    flatten_region_1(Regs, We);
flatten_region_1([], We) -> We.

flatten_region_2(Vs, We) ->
    PlaneNormal = wings_face:face_normal(Vs, We),
    wpa:vertex_flatten(Vs, PlaneNormal, We).

%%%
%%% Utilities.
%%%

region_error() ->
    wpa:error("Each region must have exactly one edge loop.").
