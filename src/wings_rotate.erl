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
%%     $Id: wings_rotate.erl,v 1.22 2002/03/11 11:04:02 bjorng Exp $
%%

-module(wings_rotate).
-export([setup/2,rotate/4]).
-include("wings.hrl").

-import(lists, [foldl/3]).

setup({Vec,Center}, St) ->
    setup(wings_util:make_vector(Vec), Center, St);
setup(Vec, St) ->
    setup(wings_util:make_vector(Vec), center, St).

setup(Vec, Center, #st{selmode=vertex}=St) ->
    Tvs = wings_sel:fold(
	    fun(Vs, #we{id=Id}=We, Acc) ->
		    [{Id,rotate(Vec, Center, gb_sets:to_list(Vs), We)}|Acc]
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Vec, Center, #st{selmode=edge}=St) ->
    Tvs = wings_sel:fold(
	    fun(Edges, We, Acc) ->
		    edges_to_vertices(Vec, Center, Edges, We, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Vec, Center, #st{selmode=face}=St) ->
    Tvs = wings_sel:fold(
	    fun(Faces, We, Acc) ->
		    faces_to_vertices(Vec, Center, Faces, We, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Vec, Center, #st{selmode=body}=St) ->
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

edges_to_vertices(Vec, Center, Edges0, #we{id=Id}=We, Acc) ->
    foldl(fun(Edges, A) ->
		  [{Id,edges_to_vertices(Vec, Center, Edges, We)}|A]
	  end, Acc, wings_sel:edge_regions(Edges0, We)).

edges_to_vertices(normal, Center, Es, #we{es=Etab}=We) ->
    Ns = foldl(fun(Edge, Acc) ->
		       #edge{lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
		       [wings_face:normal(Lf, We),
			wings_face:normal(Rf, We)|Acc]
	       end, [], gb_sets:to_list(Es)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    edges_to_vertices(Vec, Center, Es, We);
edges_to_vertices(Vec, Center, Es, #we{es=Etab}=We) ->
    Vs = wings_edge:to_vertices(Es, We),
    rotate(Vec, Center, Vs, We).
 
%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Vec, Center, Faces0, #we{id=Id}=We, Acc) ->
    foldl(fun(Faces, A) ->
		  [{Id,faces_to_vertices(Vec, Center, Faces, We)}|A]
	  end, Acc, wings_sel:face_regions(Faces0, We)).

faces_to_vertices(normal, Center, Faces, We) ->
    Ns = foldl(fun(Face, N0) ->
		       [wings_face:normal(Face, We)|N0]
	       end, [], gb_sets:to_list(Faces)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    Vs = wings_face:to_vertices(Faces, We),
    rotate(Vec, Center, Vs, We);
faces_to_vertices(Vec, Center, Faces, We) ->
    rotate(Vec, Center, wings_face:to_vertices(Faces, We), We).

%%
%% Conversion of body selections (entire objects) to vertices.
%%

body_rotate(Vec, center, We) ->
    body_rotate(Vec, wings_vertex:center(We), Vec);
body_rotate(free, {Cx,Cy,Cz}, Vec) ->
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

%% Setup rotation.

rotate(Vec, center, Vs, We) ->
    Center = wings_vertex:center(Vs, We),
    rotate(Vec, Center, Vs, We);
rotate(free, Center, Vs, We) ->
    rotate(view_vector(), Center, Vs, We);
rotate(Vec, Center, Vs, We) ->
    VsPos = wings_util:add_vpos(Vs, We),
    {Vs,rotate_fun(Center, VsPos, Vec)}.

rotate_fun(Center, VsPos, Axis) ->
    fun(view_changed, NewWe) ->
	    NewAxis = view_vector(),
	    NewVsPos = wings_util:update_vpos(VsPos, NewWe),
	    rotate_fun(Center, NewVsPos, NewAxis);
       ([Dx], A) ->
	    rotate(Center, Axis, Dx, VsPos, A)
    end.

rotate({Cx,Cy,Cz}, Axis, Angle, VsPos, Acc0) ->
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Axis)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    foldl(fun({V,#vtx{pos=Pos0}=Vtx}, Acc) ->
		  Pos = e3d_mat:mul_point(M, Pos0),
		  [{V,Vtx#vtx{pos=Pos}}|Acc]
	  end, Acc0, VsPos).

view_vector() ->
    e3d_vec:norm(wings_view:eye_point()).
