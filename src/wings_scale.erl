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
%%     $Id: wings_scale.erl,v 1.34 2002/03/18 08:54:01 bjorng Exp $
%%

-module(wings_scale).
-export([setup/2,inset/1]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3]).
-define(HUGE, 1.0E307).

setup({Vec,Point,Magnet}, St) ->
    setup(Vec, Point, Magnet, St);
setup({Vec,Point}, St) ->
    setup(Vec, Point, none, St).

setup(Vec, Point, Magnet, #st{selmode=vertex}=St) ->
    Tvs = scale_vertices(Vec, Point, Magnet, St),
    init_drag(Tvs, Magnet, St);
setup(Vec, Point, Magnet, #st{selmode=edge}=St) ->
    Tvs = wings_sel:fold(
	    fun(Edges, We, Acc) ->
		    edges_to_vertices(Vec, Point, Magnet, Edges, We, Acc)
	    end, [], St),
    init_drag(Tvs, Magnet, St);
setup(Vec, Point, Magnet, #st{selmode=face}=St) ->
    Tvs = wings_sel:fold(
	    fun(Faces, We, Acc) ->
		    faces_to_vertices(Vec, Point, Magnet, Faces, We, Acc)
	    end, [], St),
    init_drag(Tvs, Magnet, St);
setup(Vec, Point, _Magnet, #st{selmode=body}=St) ->
    Tvs = wings_sel:fold(
	    fun(_, #we{id=Id}=We, Acc) ->
		    [{Id,body_to_vertices(Vec, Point, We)}|Acc]
	    end, [], St),
    init_drag({matrix,Tvs}, none, St).

init_drag(Tvs, Magnet, St) ->
    wings_drag:setup(Tvs, [{percent,{-1.0,?HUGE}}|magnet_unit(Magnet)],
		     [], St).

magnet_unit(none) -> [];
magnet_unit(_) -> [falloff].

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

scale_vertices(Vec, center, Magnet, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 [{gb_sets:to_list(Vs),We}|Acc]
			 end, [], St),
    BB = foldl(fun({Vs,We}, BB) ->
		       wings_vertex:bounding_box(Vs, We, BB)
	       end, none, Tvs),
    Center = e3d_vec:average(BB),
    foldl(fun({Vs,We}, Acc) ->
		  scale(Vec, Center, Magnet, Vs, We, Acc)
	  end, [], Tvs);
scale_vertices(Vec, Center, Magnet, St) ->
    wings_sel:fold(
      fun(Vs0, We, Acc) ->
	      Vs = gb_sets:to_list(Vs0),
	      scale(Vec, Center, Magnet, Vs, We, Acc)
      end, [], St).

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Vec, center, Magnet, Edges0, We, Acc) ->
    foldl(fun(Edges, A) ->
		  edges_to_vertices_1(Vec, center, Magnet, Edges, We, A)
	  end, Acc, wings_sel:edge_regions(Edges0, We));
edges_to_vertices(Vec, Point, Magnet, Edges, We, Acc) ->
    edges_to_vertices_1(Vec, Point, Magnet, Edges, We, Acc).

edges_to_vertices_1(Vec, Point, Magnet, Edges, We, Acc) ->
    if
	Magnet == none; Acc == [] ->
	    Vs = wings_edge:to_vertices(Edges, We),
	    scale(Vec, Point, Magnet, Vs, We, Acc);
	true ->
	    wings_util:error("Magnet scale on multiple edge regions requires "
			     "an explicit scale origin.")
    end.


%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Vec, center, Magnet, Faces0, We, Acc) ->
    foldl(fun(Faces, A) ->
		  faces_to_vertices_1(Vec, center, Magnet, Faces, We, A)
	  end, Acc, wings_sel:face_regions(Faces0, We));
faces_to_vertices(Vec, Point, Magnet, Faces, We, Acc) ->
    faces_to_vertices_1(Vec, Point, Magnet, Faces, We, Acc).

faces_to_vertices_1(Vec, Point, Magnet, Faces, We, Acc) ->
    if
	Magnet == none; Acc == [] ->
	    Vs = wings_face:to_vertices(Faces, We),
	    scale(Vec, Point, Magnet, Vs, We, Acc);
	true ->
	    wings_util:error("Magnet scale on multiple face regions requires "
			     "an explicit scale origin.")
    end.

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

scale(Vec, center, Magnet, Vs, We, A) ->
    Center = e3d_vec:average(wings_vertex:bounding_box(Vs, We)),
    scale(Vec, Center, Magnet, Vs, We, A);
scale(Vec0, Center, Magnet, Vs, #we{id=Id}=We, Acc) ->
    Vec = make_vector(Vec0),
    {Pre,Post} = make_matrices(Vec, Center),
    VsPos = mul(Vs, Post, We),
    Fun = fun([Dx], A0) ->
		  {Sx,Sy,Sz} = make_scale(Dx+1.0, Vec),
		  Matrix = e3d_mat:mul(Pre, e3d_mat:scale(Sx, Sy, Sz)),
		  foldl(fun({V,#vtx{pos=Pos0}=Vtx}, A) ->
				Pos = e3d_mat:mul_point(Matrix, Pos0),
				[{V,Vtx#vtx{pos=Pos}}|A]
			end, A0, VsPos)
	  end,
    magnet(Vec, Magnet, Pre, Post, Vs, We, {Id,{Vs,Fun}}, Acc).

magnet(_Vec, none, _Pre, _Post, _Vs, _We, Tv, Acc) -> [Tv|Acc];
magnet(Vec, Magnet0, Pre, Post, Vs0, #we{id=Id}=We, _, Acc) ->
    {Magnet1,Affected} = wings_magnet:setup(Magnet0, Vs0, We),
    Magnet = pre_transform(Post, Magnet1),
    [{Id,{Affected,magnet_scale_fun(Vec, Pre, Magnet)}}|Acc].

pre_transform(Matrix, Magnet) ->
      wings_magnet:transform(fun(Pos) ->
				     e3d_mat:mul_point(Matrix, Pos)
			     end, Magnet).
    
magnet_scale_fun(Vec, Pre, Magnet) ->
    fun([Dx,R], A0) ->
	    VsInf = wings_magnet:influences(R, Magnet),
	    foldl(fun({V,#vtx{pos={Px,Py,Pz}}=Vtx,Inf}, A) ->
			  {Sx,Sy,Sz} = make_scale(Dx*Inf+1.0, Vec),
			  Pos0 = {Px*Sx,Py*Sy,Pz*Sz},
			  Pos = e3d_mat:mul_point(Pre, Pos0),
			  [{V,Vtx#vtx{pos=Pos}}|A]
		  end, A0, VsInf)
    end.

make_vector({radial,{_,_,_}}=Vec) -> Vec;
make_vector({radial,x}) -> {radial,{1.0,0.0,0.0}};
make_vector({radial,y}) -> {radial,{0.0,1.0,0.0}};
make_vector({radial,z}) -> {radial,{0.0,0.0,1.0}};
make_vector({_,_,_}=Vec) -> Vec;
make_vector(x) -> {1.0,0.0,0.0};
make_vector(y) -> {0.0,1.0,0.0};
make_vector(z) -> {0.0,0.0,1.0};
make_vector(uniform) -> uniform.

make_matrix(Dx, Vec, Center) ->
    {Pre,Post} = make_matrices(Vec, Center),
    {Sx,Sy,Sz} = make_scale(Dx, Vec),
    e3d_mat:mul(e3d_mat:mul(Pre, e3d_mat:scale(Sx, Sy, Sz)), Post).

make_scale(Dx, uniform) -> {Dx,Dx,Dx};
make_scale(Dx, {radial,_}) -> {Dx,Dx,1.0};
make_scale(Dx, _) -> {1.0,1.0,Dx}.

make_matrices(uniform, Center) ->
    Pre = e3d_mat:translate(Center),
    Post = e3d_mat:translate(e3d_vec:neg(Center)),
    {Pre,Post};
make_matrices({radial,Vec}, Center) ->
    RotBack = e3d_mat:rotate_to_z(Vec),
    Rot = e3d_mat:transpose(RotBack),
    Pre0 = e3d_mat:translate(Center),
    Pre = e3d_mat:mul(Pre0, Rot),
    Post = e3d_mat:mul(RotBack, e3d_mat:translate(e3d_vec:neg(Center))),
    {Pre,Post};
make_matrices(Vec, Center) ->
    RotBack = e3d_mat:rotate_to_z(Vec),
    Rot = e3d_mat:transpose(RotBack),
    Pre0 = e3d_mat:translate(Center),
    Pre = e3d_mat:mul(Pre0, Rot),
    Post = e3d_mat:mul(RotBack, e3d_mat:translate(e3d_vec:neg(Center))),
    {Pre,Post}.

mul(Vs, Matrix, #we{vs=Vtab}) ->
    foldl(fun(V, A) ->
		  #vtx{pos=Pos0} = Vtx = gb_trees:get(V, Vtab),
		  Pos = e3d_mat:mul_point(Matrix, Pos0),
		  [{V,Vtx#vtx{pos=Pos}}|A]
	  end, [], Vs).
