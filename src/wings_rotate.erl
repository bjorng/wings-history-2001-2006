%%
%%  wings_rotate.erl --
%%
%%     This module implements the Rotate command.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_rotate.erl,v 1.23 2002/03/13 11:57:39 bjorng Exp $
%%

-module(wings_rotate).
-export([setup/2,rotate/5]).
-include("wings.hrl").

-import(lists, [foldl/3]).

setup({Vec,Center,Magnet}, St) ->
    setup(wings_util:make_vector(Vec), Center, Magnet, St);
setup({Vec,Center}, St) ->
    setup(wings_util:make_vector(Vec), Center, none, St).

setup(Vec, Center, Magnet, #st{selmode=vertex}=St) ->
    Tvs = wings_sel:fold(
	    fun(Vs, We, Acc) ->
		    rotate(Vec, Center, Magnet, gb_sets:to_list(Vs), We, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Vec, Center, Magnet, #st{selmode=edge}=St) ->
    Tvs = wings_sel:fold(
	    fun(Edges, We, Acc) ->
		    edges_to_vertices(Vec, Center, Magnet, Edges, We, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Vec, Center, Magnet, #st{selmode=face}=St) ->
    Tvs = wings_sel:fold(
	    fun(Faces, We, Acc) ->
		    faces_to_vertices(Vec, Center, Magnet, Faces, We, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Vec, Center, _Magnet, #st{selmode=body}=St) ->
    Tvs = wings_sel:fold(
	    fun(_, #we{id=Id}=We, Acc) ->
		    [{Id,body_rotate(Vec, Center, We)}|Acc]
	    end, [], St),
    init_drag({matrix,Tvs}, Vec, St).

init_drag(Tvs, Vec, St) ->
    wings_drag:setup(Tvs, [angle], flags(Vec), St).

flags(free) -> [screen_relative];
flags(_) -> [].

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Vec, Center, Magnet, Edges0, We, Acc) ->
    foldl(fun(Edges, A) ->
		  edges_to_vertices_1(Vec, Center, Magnet, Edges, We, A)
	  end, Acc, wings_sel:edge_regions(Edges0, We)).

edges_to_vertices_1(normal, Center, Magnet, Es, #we{es=Etab}=We, Acc) ->
    Ns = foldl(fun(Edge, A) ->
		       #edge{lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
		       [wings_face:normal(Lf, We),
			wings_face:normal(Rf, We)|A]
	       end, [], gb_sets:to_list(Es)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    edges_to_vertices_1(Vec, Center, Magnet, Es, We, Acc);
edges_to_vertices_1(Vec, Center, Magnet, Es, We, Acc) ->
    Vs = wings_edge:to_vertices(Es, We),
    rotate(Vec, Center, Magnet, Vs, We, Acc).
 
%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Vec, Center, Magnet, Faces0, We, Acc) ->
    foldl(fun(Faces, A) ->
		  faces_to_vertices_1(Vec, Center, Magnet, Faces, We, A)
	  end, Acc, wings_sel:face_regions(Faces0, We)).

faces_to_vertices_1(normal, Center, Magnet, Faces, We, Acc) ->
    Ns = foldl(fun(Face, N0) ->
		       [wings_face:normal(Face, We)|N0]
	       end, [], gb_sets:to_list(Faces)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    Vs = wings_face:to_vertices(Faces, We),
    rotate(Vec, Center, Magnet, Vs, We, Acc);
faces_to_vertices_1(Vec, Center, Magnet, Faces, We, Acc) ->
    rotate(Vec, Center, Magnet, wings_face:to_vertices(Faces, We), We, Acc).

%%
%% Conversion of body selections (entire objects) to vertices.
%%

body_rotate(Vec, center, We) ->
    body_rotate(Vec, wings_vertex:center(We), Vec);
body_rotate(free, {Cx,Cy,Cz}, _We) ->
    fun(Matrix0, [Angle]) when is_float(Angle) ->
	    Vec = view_vector(),
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:translate(Cx, Cy, Cz)),
	    M = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Vec)),
	    e3d_mat:mul(M, e3d_mat:translate(-Cx, -Cy, -Cz))
    end;
body_rotate(Vec, {Cx,Cy,Cz}, _We) ->
    fun(Matrix0, [Angle]) when is_float(Angle) ->
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:translate(Cx, Cy, Cz)),
	    M = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Vec)),
	    e3d_mat:mul(M, e3d_mat:translate(-Cx, -Cy, -Cz))
    end.

%%%
%%% Setup rotation.
%%%

rotate(Vec, Center, Vs, We, Acc) ->
    rotate(Vec, Center, none, Vs, We, Acc).

rotate(Vec, center, Magnet, Vs, We, Acc) ->
    Center = wings_vertex:center(Vs, We),
    rotate(Vec, Center, Magnet, Vs, We, Acc);
rotate(free, Center, Magnet, Vs, We, Acc) ->
    rotate(view_vector(), Center, Magnet, Vs, We, Acc);
rotate(Vec, Center, Magnet, Vs, #we{id=Id}=We, Acc) ->
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = rotate_fun(Center, VsPos, Vec),
    [{Id,{Vs,Fun}}|magnet(Vec, Center, Magnet, Vs, We, Acc)].

rotate_fun(Center, VsPos, Axis) ->
    fun(view_changed, NewWe) ->
	    NewAxis = view_vector(),
	    NewVsPos = wings_util:update_vpos(VsPos, NewWe),
	    rotate_fun(Center, NewVsPos, NewAxis);
       ([Dx], A) ->
	    do_rotate(Center, Axis, Dx, VsPos, A)
    end.

do_rotate({Cx,Cy,Cz}, Axis, Angle, VsPos, Acc0) ->
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Axis)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    foldl(fun({V,#vtx{pos=Pos0}=Vtx}, Acc) ->
		  Pos = e3d_mat:mul_point(M, Pos0),
		  [{V,Vtx#vtx{pos=Pos}}|Acc]
	  end, Acc0, VsPos).

view_vector() ->
    e3d_vec:norm(wings_view:eye_point()).

magnet(_Vec, _Center, none, _Vs, _We, Acc) -> Acc;
magnet(Vec, Center, Magnet, Vs0, #we{id=Id}=We, Acc) ->
    VsInf = wings_magnet:influences(Magnet, Vs0, We),
    Affected = [V || {V,_,_} <- VsInf],
    [{Id,{Affected,magnet_rotate_fun(Vec, Center, VsInf)}}|Acc].

magnet_rotate_fun(Axis0, Center, VsInf0) ->
    fun(view_changed, NewWe) ->
	    Axis = view_vector(),
	    VsInf = wings_util:update_vpos(VsInf0, NewWe),
	    magnet_rotate_fun(Axis, Center, VsInf);
       ([Dx], A) -> magnet_rotate(Axis0, Center, Dx, VsInf0, A)
    end.
    
magnet_rotate(Axis, {Cx,Cy,Cz}, Angle, VsPos, Acc0) ->
    foldl(fun({V,#vtx{pos=Pos0}=Vtx,Inf}, Acc) ->
		  M0 = e3d_mat:translate(Cx, Cy, Cz),
		  M1 = e3d_mat:mul(M0, e3d_mat:rotate(Inf*Angle, Axis)),
		  M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
		  Pos = e3d_mat:mul_point(M, Pos0),
		  [{V,Vtx#vtx{pos=Pos}}|Acc]
	  end, Acc0, VsPos).
