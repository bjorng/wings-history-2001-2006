%%
%%  wings_rotate.erl --
%%
%%     This module implements the Rotate command.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_rotate.erl,v 1.18 2001/12/26 14:46:26 bjorng Exp $
%%

-module(wings_rotate).
-export([setup/2]).
-include("wings.hrl").

-import(lists, [foldl/3]).

setup(Type, #st{selmode=vertex}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs = wings_sel:fold(
	    fun(Vs, #we{id=Id}=We, Acc) ->
		    [{Id,vertices_to_vertices(Vs, We, Vec)}|Acc]
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Type, #st{selmode=edge}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs = wings_sel:fold(
	    fun(Edges, We, Acc) ->
		    edges_to_vertices(Edges, We, Vec, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Type, #st{selmode=face}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs = wings_sel:fold(
	    fun(Faces, We, Acc) ->
		    faces_to_vertices(Faces, We, Vec, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, St);
setup(Type, #st{selmode=body}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs = wings_sel:fold(
	    fun(_, #we{id=Id}=We, Acc) ->
		    [{Id,body_to_vertices(We, Vec)}|Acc]
	    end, [], St),
    init_drag({matrix,Tvs}, Vec, St).

init_drag(Tvs, Vec, St) ->
    wings_drag:init_drag(Tvs, constraint(Vec), angle, St).

constraint(free) -> view_dependent;
constraint(Other) -> none.

%%
%% Conversion of vertex selection to vertices. :-)
%% Not entirely pointless, as we'll need to add vectors for
%% the points (vertices).
%%

vertices_to_vertices(Vs, We, Vec) ->
    rotate(gb_sets:to_list(Vs), We, Vec).

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Edges0, #we{id=Id}=We, Vec, Acc) ->
    foldl(fun(Edges, A) ->
		  [{Id,edges_to_vertices(Edges, We, Vec)}|A]
	  end, Acc, wings_sel:edge_regions(Edges0, We)).

edges_to_vertices(Es, #we{es=Etab}=We, normal) ->
    Ns = foldl(fun(Edge, Acc) ->
		       #edge{lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
		       [wings_face:normal(Lf, We),
			wings_face:normal(Rf, We)|Acc]
	       end, [], gb_sets:to_list(Es)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    edges_to_vertices(Es, We, Vec);
edges_to_vertices(Es, #we{es=Etab}=We, Vec) ->
    Vs = wings_edge:to_vertices(Es, We),
    rotate(Vs, We, Vec).
 
%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Faces0, #we{id=Id}=We, Vec, Acc) ->
    foldl(fun(Faces, A) ->
		  [{Id,faces_to_vertices(Faces, We, Vec)}|A]
	  end, Acc, wings_sel:face_regions(Faces0, We)).

faces_to_vertices(Faces, We, normal) ->
    #we{fs=Ftab,es=Etab,vs=Vtab} = We,
    Ns = foldl(fun(Face, N0) ->
		       [wings_face:normal(Face, We)|N0]
	       end, [], gb_sets:to_list(Faces)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    Vs = wings_face:to_vertices(Faces, We),
    rotate(Vs, We, Vec);
faces_to_vertices(Faces, We, Vec) ->
    rotate(wings_face:to_vertices(Faces, We), We, Vec).

%%
%% Conversion of body selections (entire objects) to vertices.
%%

body_to_vertices(We, Vec) ->
    body_rotate_fun(We, Vec).

body_rotate_fun(We, free) ->
    {Cx,Cy,Cz} = wings_vertex:center(We),
    fun(Matrix0, {Dx,Dy}) when float(Dx) ->
	    wings_drag:message([Dx], angle),
	    A = 15*Dx,
	    Vec = view_vector(),
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:translate(Cx, Cy, Cz)),
	    M = e3d_mat:mul(M0, e3d_mat:rotate(A, Vec)),
	    e3d_mat:mul(M, e3d_mat:translate(-Cx, -Cy, -Cz))
    end;
body_rotate_fun(We, Vec) ->
    {Cx,Cy,Cz} = wings_vertex:center(We),
    fun(Matrix0, Dx) when float(Dx) ->
	    A = 15*Dx,
	    wings_drag:message([A], angle),
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:translate(Cx, Cy, Cz)),
	    M = e3d_mat:mul(M0, e3d_mat:rotate(A, Vec)),
	    e3d_mat:mul(M, e3d_mat:translate(-Cx, -Cy, -Cz))
    end.

%% Setup rotation.

rotate(Vs, We, free) ->
    Center = wings_vertex:center(Vs, We),
    VsPos = wings_util:add_vpos(Vs, We),
    Vec = view_vector(),
    {Vs,rotate_fun(Center, VsPos, Vec)};
rotate(Vs, We, Vec) ->
    Center = wings_vertex:center(Vs, We),
    VsPos = wings_util:add_vpos(Vs, We),
    {Vs,rotate_fun(Center, VsPos, Vec)}.

rotate_fun(Center, VsPos, Axis) ->
    fun(view_changed, NewWe) ->
	    NewAxis = view_vector(),
	    NewVsPos = wings_util:update_vpos(VsPos, NewWe),
	    rotate_fun(Center, NewVsPos, NewAxis);
       ({Dx,Dy}, A) ->
	    rotate(Center, Axis, Dx, VsPos, A);
       (Dx, A) ->
	    rotate(Center, Axis, Dx, VsPos, A)
    end.

rotate({Cx,Cy,Cz}, Axis, Dx, VsPos, Acc0) ->
    Angle = 15*Dx,
    wings_drag:message([Angle], angle),
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Axis)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    foldl(fun({V,#vtx{pos=Pos0}=Vtx}, Acc) ->
		  Pos = e3d_mat:mul_point(M, Pos0),
		  [{V,Vtx#vtx{pos=Pos}}|Acc]
	  end, Acc0, VsPos).

view_vector() ->
    e3d_vec:norm(wings_view:eye_point()).
