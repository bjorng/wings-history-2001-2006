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
%%     $Id: wings_scale.erl,v 1.22 2002/02/07 11:49:08 bjorng Exp $
%%

-module(wings_scale).
-export([setup/2,inset/1]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3]).
-define(HUGE, 1.0E200).

setup(Type, #st{selmode=vertex}=St) ->
    Tvs = wings_sel:fold(
	    fun(Vs, #we{id=Id}=We, Acc) ->
		    [{Id,scale_vertices(Type, gb_sets:to_list(Vs), We)}|Acc]
	    end, [], St),
    init_drag(Tvs, St);
setup(Type, #st{selmode=edge}=St) ->
    Tvs = wings_sel:fold(
	    fun(Edges, #we{id=Id}=We, Acc) ->
		    [{Id,edges_to_vertices(Edges, We, Type)}|Acc]
	    end, [], St),
    init_drag(Tvs, St);
setup(Type, #st{selmode=face}=St) ->
    Tvs = wings_sel:fold(
	    fun(Faces, #we{id=Id}=We, Acc) ->
		    [{Id,faces_to_vertices(Faces, We, Type)}|Acc]
	    end, [], St),
    init_drag(Tvs, St);
setup(Type, #st{selmode=body}=St) ->
    Tvs = wings_sel:fold(
	    fun(_, #we{id=Id}=We, Acc) ->
		    [{Id,body_to_vertices(We, Type)}|Acc]
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
inset([], We, {Vs0,Min}=Acc) ->
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
      fun(_, Edge, #edge{vs=Va,ve=Vb}, {A0,Min}) ->
	      Pos = wings_vertex:pos(Va, Vtab),
	      Dir = e3d_vec:sub(wings_vertex:pos(Vb, Vtab), Pos),
	      T0 = e3d_vec:dot(Dir, e3d_vec:sub(Center, Pos)) /
		  e3d_vec:dot(Dir, Dir),
	      Vec = e3d_vec:sub(Center,
				e3d_vec:add(Pos, e3d_vec:mul(Dir, T0))),
	      Dist = e3d_vec:len(Vec),
	      A = [{Va,Vec,Dist},{Vb,Vec,Dist}|A0],
	      if 
		  Dist < Min -> {A,Dist};
		  true -> {A,Min}
	      end
      end, Acc, Face, We).

average_vectors({V,[Vec]}, Acc) ->
    %% Yes, it can happen.
    [{Vec,[V]}|Acc];
average_vectors({V,[VecA,VecB]=Vecs}, Acc) ->
    Dot = e3d_vec:dot(VecA, VecB) /
  	e3d_vec:len(VecA) / e3d_vec:len(VecB),
    Vec = e3d_vec:divide(e3d_vec:add(VecA, VecB), (1+Dot)),
    [{Vec,[V]}|Acc].

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Es0, #we{es=Etab}=We, Type) ->
    foldl(fun(Es, A) ->
		  Vs = wings_edge:to_vertices(Es, We),
		  scale_vertices(Type, Vs, We, A)
	  end, [], wings_sel:edge_regions(Es0, We)).

%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Faces0, We, Type) ->
    foldl(fun(Faces, A) ->
		  Vs = wings_face:to_vertices(Faces, We),
		  scale_vertices(Type, Vs, We, A)
	  end, [], wings_sel:face_regions(Faces0, We)).

%%
%% Conversion of body selection to vertices.
%%

body_to_vertices(We, Type) ->
    Center = e3d_vec:average(wings_vertex:bounding_box(We)),
    {Xt0,Yt0,Zt0} = filter_vec(Type, {1.0,1.0,1.0}),
    fun(Matrix0, [Dx]) when float(Dx) ->
	    Xt = 1.01+Xt0*Dx,
	    Yt = 1.01+Yt0*Dx,
	    Zt = 1.01+Zt0*Dx,
	    Mat0 = e3d_mat:translate(Center),
	    Mat = e3d_mat:mul(Mat0, e3d_mat:scale(Xt, Yt, Zt)),
	    e3d_mat:mul(Mat, e3d_mat:translate(e3d_vec:neg(Center)))
    end.

%%%
%%% Utilities.
%%%

scale_vertices(Type, Vs, We) ->
    scale_vertices(Type, Vs, We, []).

scale_vertices({Type,{Center,Vec}}, Vs, We, Acc) ->
    scale_vertices(Type, Center, Vs, We, Acc);
scale_vertices(Type, Vs, We, Acc) ->
    Center = e3d_vec:average(wings_vertex:bounding_box(Vs, We)),
    scale_vertices(Type, Center, Vs, We, Acc).

scale_vertices(Type, Center, Vs, #we{vs=Vtab}=We, Acc0) ->
    foldl(fun(V, Acc) ->
		  Pos = wings_vertex:pos(V, Vtab),
		  Vec0 = e3d_vec:sub(Pos, Center),
		  Vec = filter_vec(Type, Vec0),
		  [{Vec,[V]}|Acc]
	  end, Acc0, Vs).

filter_vec(uniform, Vec) -> Vec;
filter_vec(x, {X,_,_}) -> {X,0,0};
filter_vec(y, {_,Y,_}) -> {0,Y,0};
filter_vec(z, {_,_,Z}) -> {0,0,Z};
filter_vec(radial_x, {_,Y,Z}) -> {0,Y,Z};
filter_vec(radial_y, {X,_,Z}) -> {X,0,Z};
filter_vec(radial_z, {X,Y,_}) -> {X,Y,0}.
