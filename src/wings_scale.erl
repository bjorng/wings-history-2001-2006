%%
%%  wings_scale.erl --
%%
%%     This module implements the Scale command plus
%%     the interactive part of the Bevel (face) and Inset commands.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_scale.erl,v 1.30 2002/03/11 11:04:02 bjorng Exp $
%%

-module(wings_scale).
-export([setup/2,inset/1]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3]).
-define(HUGE, 1.0E307).

setup({radial,Vec,Point}, St) ->
    setup({radial,Vec}, Point, St);
setup({Vec,Point}, St) ->
    setup(Vec, Point, St);
setup(Vec, St) ->
    setup(Vec, center, St).

setup(Vec, Point, #st{selmode=vertex}=St) ->
    Tvs = scale_vertices(Vec, Point, St),
    init_drag(Tvs, St);
setup(Vec, Point, #st{selmode=edge}=St) ->
    Tvs = wings_sel:fold(
	    fun(Edges, We, Acc) ->
		    edges_to_vertices(Vec, Point, Edges, We, Acc)
	    end, [], St),
    init_drag(Tvs, St);
setup(Vec, Point, #st{selmode=face}=St) ->
    Tvs = wings_sel:fold(
	    fun(Faces, We, Acc) ->
		    faces_to_vertices(Vec, Point, Faces, We, Acc)
	    end, [], St),
    init_drag(Tvs, St);
setup(Vec, Point, #st{selmode=body}=St) ->
    Tvs = wings_sel:fold(
	    fun(_, #we{id=Id}=We, Acc) ->
		    [{Id,body_to_vertices(Vec, Point, We)}|Acc]
	    end, [], St),
    init_drag({matrix,Tvs}, St).

init_drag(Tvs, St) ->
    wings_drag:setup(Tvs, [{percent,{-1.0,?HUGE}}], [], St).

inset(St) ->
    Tvs = wings_sel:fold(
	    fun(Faces, #we{id=Id}=We, Acc) ->
		    [{Id,inset(Faces, We)}|Acc]
	    end, [], St),
    wings_drag:setup(Tvs, [{percent,{-?HUGE,1}}], [], St).

inset(Faces, We) ->
    inset(gb_sets:to_list(Faces), We, {[],?HUGE}).
    
inset([Face|Faces], We, Acc) ->
    inset(Faces, We, inset_face(Face, We, Acc));
inset([], _We, {Vs0,Min}) ->
    Vs = map(fun({V,Vec,Dist}) when Dist > Min ->
		     {V,e3d_vec:mul(Vec, Min/Dist)};
		({V,Vec,_Dist}) ->
		     {V,Vec}
	     end, Vs0),
    R = sofs:relation(Vs),
    F = sofs:relation_to_family(R),
    foldl(fun average_vectors/2, [], sofs:to_external(F)).

inset_face(Face, #we{vs=Vtab}=We, Acc) ->
    Vs0 = wings_face:surrounding_vertices(Face, We),
    Center = wings_vertex:center(Vs0, We),
    wings_face:fold(
      fun(_, _, #edge{vs=Va,ve=Vb}, {A0,Min}) ->
	      Pos = wings_vertex:pos(Va, Vtab),
	      Dir = e3d_vec:sub(wings_vertex:pos(Vb, Vtab), Pos),
	      DirSqr = e3d_vec:dot(Dir, Dir),
	      ToCenter = e3d_vec:sub(Center, Pos),
	      case catch e3d_vec:dot(Dir, ToCenter) / DirSqr of
		  {'EXIT',_} ->
		      wings_util:error("There are too short edges in one "
				       "or more selected faces. "
				       "(Use Cleanup.)");
		  T0 ->
		      PerpPos = e3d_vec:add(Pos, e3d_vec:mul(Dir, T0)),
		      Vec = e3d_vec:sub(Center, PerpPos),
		      Dist = e3d_vec:len(Vec),
		      A = [{Va,Vec,Dist},{Vb,Vec,Dist}|A0],
		      if 
			  Dist < Min -> {A,Dist};
			  true -> {A,Min}
		      end
	      end
      end, Acc, Face, We).

average_vectors({V,[Vec]}, Acc) ->
    %% Yes, it can happen.
    [{Vec,[V]}|Acc];
average_vectors({V,[VecA,VecB]}, Acc) ->
    Dot = e3d_vec:dot(VecA, VecB) /
  	e3d_vec:len(VecA) / e3d_vec:len(VecB),
    Vec = e3d_vec:divide(e3d_vec:add(VecA, VecB), (1+Dot)),
    [{Vec,[V]}|Acc].

%%
%% Scaling of vertices.
%%

scale_vertices(Vec, center, St) ->
    Tvs = wings_sel:fold(fun(Vs, #we{id=Id}=We, Acc) ->
				 [{Id,gb_sets:to_list(Vs),We}|Acc]
			 end, [], St),
    BB = foldl(fun({_,Vs,We}, BB) ->
		       wings_vertex:bounding_box(Vs, We, BB)
	       end, none, Tvs),
    Center = e3d_vec:average(BB),
    foldl(fun({Id,Vs,We}, Acc) ->
		  [{Id,scale(Vec, Center, Vs, We)}|Acc]
	  end, [], Tvs);
scale_vertices(Vec, Center, St) ->
    wings_sel:fold(
      fun(Vs0, #we{id=Id}=We, Acc) ->
	      Vs = gb_sets:to_list(Vs0),
	      [{Id,scale(Vec, Center, Vs, We)}|Acc]
      end, [], St).

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Vec, Point, Edges0, #we{id=Id}=We, Acc) ->
    foldl(fun(Edges, A) ->
		  [{Id,edges_to_vertices_1(Vec, Point, Edges, We)}|A]
	  end, Acc, wings_sel:edge_regions(Edges0, We)).

edges_to_vertices_1(Vec, Point, Edges, We) ->
    Vs = wings_edge:to_vertices(Edges, We),
    scale(Vec, Point, Vs, We).

%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Vec, Point, Faces0, #we{id=Id}=We, Acc) ->
    foldl(fun(Faces, A) ->
		  [{Id,faces_to_vertices_1(Vec, Point, Faces, We)}|A]
	  end, Acc, wings_sel:face_regions(Faces0, We)).

faces_to_vertices_1(Vec, Point, Faces, We) ->
    Vs = wings_face:to_vertices(Faces, We),
    scale(Vec, Point, Vs, We).

%%
%% Conversion of body selection to vertices.
%%

body_to_vertices(Vec, center, We) ->
    Center = e3d_vec:average(wings_vertex:bounding_box(We)),
    body_to_vertices_1(Vec, Center);
body_to_vertices(Vec, Point, _We) ->
    body_to_vertices_1(Vec, Point).

body_to_vertices_1(Vec0, Center) ->
    Vec = make_vector(Vec0),
    fun(_Matrix0, [Dx]) when is_float(Dx) ->
	    make_matrix(Dx+1.0, Vec, Center)
    end.

%%%
%%% Utilities.
%%%

scale(Vec, center, Vs, We) ->
    Center = e3d_vec:average(wings_vertex:bounding_box(Vs, We)),
    scale(Vec, Center, Vs, We);
scale(Vec0, Center, Vs, We) ->
    Vec = make_vector(Vec0),
    VsPos = wings_util:add_vpos(Vs, We),
    {Vs,fun([Dx], A0) ->
		Matrix = make_matrix(Dx+1.0, Vec, Center),
		foldl(fun({V,#vtx{pos=Pos0}=Vtx}, A) ->
			      Pos = e3d_mat:mul_point(Matrix, Pos0),
			      [{V,Vtx#vtx{pos=Pos}}|A]
		      end, A0, VsPos)
	end}.

make_vector({radial,{_,_,_}}=Vec) -> Vec;
make_vector({_,_,_}=Vec) -> Vec;
make_vector(x) -> {1.0,0.0,0.0};
make_vector(y) -> {0.0,1.0,0.0};
make_vector(z) -> {0.0,0.0,1.0};
make_vector(radial_x) -> {radial,{1.0,0.0,0.0}};
make_vector(radial_y) -> {radial,{0.0,1.0,0.0}};
make_vector(radial_z) -> {radial,{0.0,0.0,1.0}};
make_vector(uniform) -> uniform.

make_matrix(Dx, uniform, Center) ->
    Mat0 = e3d_mat:translate(Center),
    Mat = e3d_mat:mul(Mat0, e3d_mat:scale(Dx, Dx, Dx)),
    e3d_mat:mul(Mat, e3d_mat:translate(e3d_vec:neg(Center)));
make_matrix(Dx, {radial,Vec}, Center) ->
    RotBack = e3d_mat:rotate_to_z(Vec),
    Rot = e3d_mat:transpose(RotBack),
    Mat0 = e3d_mat:translate(Center),
    Mat1 = e3d_mat:mul(Mat0, Rot),
    Mat2 = e3d_mat:mul(Mat1, e3d_mat:scale(Dx, Dx, 1.0)),
    Mat = e3d_mat:mul(Mat2, RotBack),
    e3d_mat:mul(Mat, e3d_mat:translate(e3d_vec:neg(Center)));
make_matrix(Dx, Vec, Center) ->
    RotBack = e3d_mat:rotate_to_z(Vec),
    Rot = e3d_mat:transpose(RotBack),
    Mat0 = e3d_mat:translate(Center),
    Mat1 = e3d_mat:mul(Mat0, Rot),
    Mat2 = e3d_mat:mul(Mat1, e3d_mat:scale(1.0, 1.0, Dx)),
    Mat = e3d_mat:mul(Mat2, RotBack),
    e3d_mat:mul(Mat, e3d_mat:translate(e3d_vec:neg(Center))).
