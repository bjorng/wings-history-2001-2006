%%
%%  wings_scale.erl --
%%
%%     This module implements the Scale command plus
%%     the interactive part of the Bevel (face) and Inset commands.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_scale.erl,v 1.1.1.1 2001/08/14 18:16:39 bjorng Exp $
%%

-module(wings_scale).
-export([setup/2,bevel_face/2,inset/1]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3]).
-define(HUGE, 1.0E200).

setup(Type, #st{selmode=vertex}=St) ->
    Tvs = wings_sel:fold_shape(
	    fun(#shape{id=Id,sh=We}, Vs, Acc) ->
		    [{Id,scale_vertices(Type, gb_sets:to_list(Vs), We)}|Acc]
	    end, [], St),
    init_drag(Tvs, St);
setup(Type, #st{selmode=edge}=St) ->
    Tvs = wings_sel:fold_region(
	    fun(Id, Edges, We, Acc) ->
		    [{Id,edges_to_vertices(Edges, We, Type)}|Acc]
	    end, [], St),
    init_drag(Tvs, St);
setup(Type, #st{selmode=face}=St) ->
    Tvs = wings_sel:fold_region(
	    fun(Id, Faces, We, Acc) ->
		    [{Id,faces_to_vertices(Faces, We, Type)}|Acc]
	    end, [], St),
    init_drag(Tvs, St);
setup(Type, #st{selmode=body}=St) ->
    Tvs = wings_sel:fold(
	    fun(#shape{id=Id}=Sh, Acc) ->
		    [{Id,body_to_vertices(Sh, Type)}|Acc]
	    end, [], St),
    init_drag(Tvs, St).

init_drag(Tvs, St) ->
    wings_drag:init_drag(Tvs, {-1.0,?HUGE}, St).

bevel_face(MoveEdges, St) ->
    Tvs0 = wings_sel:fold_shape(
	     fun(#shape{id=Id,sh=We}, Faces, Acc) ->
		     [{Id,inset(Faces, We)}|Acc]
	     end, [], St),
    Tvs = wings_sel:fold_shape(
	    fun(#shape{id=Id,sh=We}, Mes, Acc) ->
		    [{Id,bevel_move(Mes, We)}|Acc]
	    end, Tvs0, St#st{sel=MoveEdges}),
    wings_drag:init_drag(Tvs, {0,1}, St).

inset(St) ->
    Tvs = wings_sel:fold_shape(
	    fun(#shape{id=Id,sh=We}, Faces, Acc) ->
		    [{Id,inset(Faces, We)}|Acc]
	    end, [], St),
    wings_drag:init_drag(Tvs, {0,1}, St).

inset(Faces, We) ->
    inset(gb_sets:iterator(Faces), We, {[],?HUGE}).
    
inset(Iter0, We, {Vs0,Min}=Acc) ->
    case gb_sets:next(Iter0) of
	none ->
	    Vs = map(fun({V,Vec,Dist}) ->
			     if
				 Dist > Min ->
				     {V,wings_mat:mul(Vec, Min/Dist)};
				 true -> {V,Vec}
			     end
		     end, Vs0),
	    R = sofs:relation(Vs),
	    F = sofs:relation_to_family(R),
	    foldl(fun average_vectors/2, [], sofs:to_external(F));
	{Face,Iter} ->
	    inset(Iter, We, inset_face(Face, We, Acc))
    end.

inset_face(Face, #we{vs=Vtab}=We, Acc) ->
    Vs0 = wings_face:surrounding_vertices(Face, We),
    Center = wings_vertex:center(Vs0, We),
    wings_face:fold(
      fun(_, Edge, #edge{vs=Va,ve=Vb}, {A0,Min}) ->
	      Pos = wings_vertex:pos(Va, Vtab),
	      Dir = wings_mat:subtract(wings_vertex:pos(Vb, Vtab), Pos),
	      T0 = wings_mat:dot_product(
		     Dir, wings_mat:subtract(Center, Pos)) /
		  wings_mat:dot_product(Dir, Dir),
	      Vec = wings_mat:subtract(
		      Center,
		      wings_mat:add(Pos, wings_mat:mul(Dir, T0))),
	      Dist = wings_mat:len(Vec),
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
    Dot = wings_mat:dot_product(VecA, VecB) /
  	wings_mat:len(VecA) / wings_mat:len(VecB),
    Vec = wings_mat:divide(wings_mat:add(VecA, VecB), (1+Dot)),
    [{Vec,[V]}|Acc].

bevel_move(MoveEdges, We) ->
    Vs = dict:fold(fun(Face, Vs, A) ->
			   bevel_move_1(Face, Vs, We, A)
		   end, [], MoveEdges),
    wings_util:average_normals(Vs).

bevel_move_1(Face, Es, #we{es=Etab}=We, Acc) ->
    Normal0 = wings_face:normal(Face, We),
    Normal = wings_mat:negate(Normal0),
    foldl(fun(Edge, A) ->
		  #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		  [{Va,Normal},{Vb,Normal}|A]
	  end, Acc, Es).

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Es, #we{es=Etab}=We, Type) ->
    Vs = foldl(fun(Edge, Acc) ->
		       #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		       gb_sets:add(Va, gb_sets:add(Vb, Acc))
	       end, gb_sets:empty(), gb_sets:to_list(Es)),
    scale_vertices(Type, gb_sets:to_list(Vs), We).

%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Faces, We, Type) ->
    scale_vertices(Type, gb_sets:to_list(wings_face:to_vertices(Faces, We)), We).

%%
%% Conversion of body selection to vertices.
%%

body_to_vertices(Sh, Type) ->
    scale_fun(Sh, Type).

scale_fun(Sh, Type) ->
    {Cx,Cy,Cz} = wings_util:center(Sh),
    {Xt0,Yt0,Zt0} = filter_vec(Type, {1.0,1.0,1.0}),
    fun(Sh, Dx, Dy, St) when float(Dx) ->
	    wings_io:message(lists:flatten(io_lib:format("X:~10p", [Dx]))),
	    Xt = 1.01+Xt0*Dx,
	    Yt = 1.01+Yt0*Dx,
	    Zt = 1.01+Zt0*Dx,
	    Mat0 = wings_mat:translate(Cx, Cy, Cz),
	    Mat1 = wings_mat:mult(Mat0, wings_mat:scale(Xt, Yt, Zt)),
	    Mat = wings_mat:mult(Mat1, wings_mat:translate(-Cx, -Cy, -Cz)),
	    {shape_matrix,wings_mat:transpose(Mat)}
    end.

%%%
%%% Utilities.
%%%

scale_vertices(Type, Vs0, #we{vs=Vtab}) ->
    Vs = [{V,wings_vertex:pos(V, Vtab)} || V <- Vs0],
    Center = find_center(Vs),
    foldl(fun({V,Pos}, Acc) ->
		  Vec0 = wings_mat:subtract(Pos, Center),
		  Vec = filter_vec(Type, Vec0),
		  [{Vec,[V]}|Acc]
	  end, [], Vs).
    
find_center(Vs) ->
    Zero = 0.0,
    {Xsum,Ysum,Zsum} =
	foldl(fun({V,{X,Y,Z}}, {Xacc,Yacc,Zacc}) ->
		      {Xacc+X,Yacc+Y,Zacc+Z}
	      end, {Zero,Zero,Zero}, Vs),
    N = length(Vs),
    {Xsum/N,Ysum/N,Zsum/N}.

filter_vec(uniform, Vec) -> Vec;
filter_vec(x, {X,_,_}) -> {X,0,0};
filter_vec(y, {_,Y,_}) -> {0,Y,0};
filter_vec(z, {_,_,Z}) -> {0,0,Z};
filter_vec(radial_x, {_,Y,Z}) -> {0,Y,Z};
filter_vec(radial_y, {X,_,Z}) -> {X,0,Z};
filter_vec(radial_z, {X,Y,_}) -> {X,Y,0}.
