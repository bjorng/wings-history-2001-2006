%%
%%  wings_face.erl --
%%
%%     This module contains help routines for faces, such as fold functions
%%     face iterators.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_face.erl,v 1.34 2003/04/23 17:49:03 bjorng Exp $
%%

-module(wings_face).
-export([convert_selection/1,select_more/1,select_less/1,
	 from_edges/2,
	 other/2,vertices/2,
	 to_vertices/2,
	 normal/2,face_normal/2,good_normal/2,
	 center/2,
	 vinfo/2,vinfo/3,
	 vertices_cw/2,vertices_cw/3,
	 vertices_ccw/2,vertices_ccw/3,
	 extend_border/2,bordering_faces/2,
	 inner_edges/2,outer_edges/2,
	 fold/4,fold/5,fold_vinfo/4,fold_faces/4,
	 iterator/2,skip_to_edge/2,skip_to_cw/2,skip_to_ccw/2,
	 next_cw/1,next_ccw/1,
	 iter2etab/1,
	 patch_face/3,patch_face/4,
	 are_neighbors/3]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keymember/3,member/2]).

%%
%% Convert the current selection to a face selection.
%%
convert_selection(#st{selmode=body}=St) ->
    wings_sel:convert_shape(
      fun(_, We) ->
	      wings_sel:get_all_items(face, We)
      end, face, St);
convert_selection(#st{selmode=face}=St) ->
    wings_sel:convert_shape(
      fun(Sel0, We) ->
	      extend_border(Sel0, We)
      end, face, St);
convert_selection(#st{selmode=edge}=St) ->
    wings_sel:convert_shape(fun(Es, We) -> from_edges(Es, We) end, face, St);
convert_selection(#st{selmode=vertex}=St) ->
    wings_sel:convert_shape(fun(Vs, We) -> from_vs(Vs, We) end, face, St).

from_edges(Es, #we{es=Etab}=We) ->
    Fs = from_edges(gb_sets:to_list(Es), Etab, []),
    wings_sel:subtract_mirror_face(Fs, We).
    
from_edges([E|Es], Etab, Acc) ->
    #edge{lf=Lf,rf=Rf} = gb_trees:get(E, Etab),
    from_edges(Es, Etab, [Lf,Rf|Acc]);
from_edges([], _, Acc) -> gb_sets:from_list(Acc).

from_vs(Vs, We) ->
    from_vs(gb_sets:to_list(Vs), We, []).

from_vs([V|Vs], We, Acc0) ->
    Acc = wings_vertex:fold(fun(_, Face, _, A) -> [Face|A] end, Acc0, V, We),
    from_vs(Vs, We, Acc);
from_vs([], We, Acc) ->
    wings_sel:subtract_mirror_face(gb_sets:from_list(Acc), We).

%%% Select more.
select_more(St) ->
    wings_sel:convert_shape(fun select_more/2, face, St).

select_more(Fs0, We) ->
    Fs = foldl(fun(Face, A) ->
		       do_select_more(Face, We, A)
	       end, Fs0, gb_sets:to_list(Fs0)),
    wings_sel:subtract_mirror_face(Fs, We).

do_select_more(Face, We, Acc) ->
    foldl(fun(V, A0) ->
		  wings_vertex:fold(
		    fun(_, AFace, _, A1) ->
			    gb_sets:add(AFace, A1)
		    end, A0, V, We)
	  end, Acc, vertices_ccw(Face, We)).

select_less(St) ->
    wings_sel:convert_shape(
      fun(Faces0, We) ->
	      Faces1 = gb_sets:to_list(Faces0),
	      Faces = sofs:from_external(Faces1, [face]),
	      VsFs0 = vs_faces(Faces1, We),
	      VsFs = sofs:relation(VsFs0, [{vertex,face}]),
	      Vs = sofs:to_external(sofs:domain(VsFs)),
	      BorderVs0 = vs_bordering(Vs, Faces0, We),
	      BorderVs = sofs:from_external(BorderVs0, [vertex]),
	      BorderFs = sofs:restriction(VsFs, BorderVs),
	      Border = sofs:range(BorderFs),
	      Sel = sofs:difference(Faces, Border),
	      gb_sets:from_ordset(sofs:to_external(Sel))
      end, face, St).

vs_faces(Faces, We) ->
    fold_faces(fun(Face, V, _, _, A) -> [{V,Face}|A] end, [], Faces, We).

vs_bordering(Vs, FaceSet, We) ->
    B = foldl(fun(V, A) ->
		      case vtx_bordering(V, FaceSet, We) of
			  true -> [V|A];
			  false -> A
		      end
	      end, [], Vs),
    ordsets:from_list(B).

vtx_bordering(V, FaceSet, We) ->
    wings_vertex:fold(
      fun(_, _, _, true) -> true;
	 (_, Face, _, false) ->
	      not gb_sets:is_member(Face, FaceSet)
      end, false, V, We).

%% other(Face, EdgeRecord) -> OtherFace
%%  Pick up the "other face" from an edge record.
other(Face, #edge{lf=Face,rf=Other}) -> Other;
other(Face, #edge{rf=Face,lf=Other}) -> Other.

%% to_vertices(FaceGbSet, We) -> VertexList
%%  Convert a set of faces to a list of vertices.
to_vertices(Faces, We) when is_list(Faces) ->
    to_vertices(Faces, We, []);
to_vertices(Faces, We) ->
    to_vertices(gb_sets:to_list(Faces), We, []).

to_vertices([Face|Faces], We, Acc0) ->
    Acc = fold(fun(V, _, _, A) -> [V|A] end, Acc0, Face, We),
    to_vertices(Faces, We, Acc);
to_vertices([], _, Acc) -> ordsets:from_list(Acc).

%% vertices(Face, We) -> NumberOfVertices
%%  Calculate number of vertices building up a face.
vertices(Face, We) ->
    fold(fun(_, _, _, N) -> N+1 end, 0, Face, We).

%% Return the normal for a face.

normal(Face, #we{vp=Vtab}=We) ->
    Vpos = fold(fun(V, _, _, A) ->
			[gb_trees:get(V, Vtab)|A]
		end, [], Face, We),
    e3d_vec:normal(Vpos).

%% Return the normal for a face, with the face given explicitly
%% as its vertices.

face_normal(Vs, #we{vp=Vtab}) ->
    face_normal(Vs, Vtab, []);
face_normal(Vs, Vtab) ->
    face_normal(Vs, Vtab, []).

face_normal([V|Vs], Vtab, Acc) ->
    Pos = gb_trees:get(V, Vtab),
    face_normal(Vs, Vtab, [Pos|Acc]);
face_normal([], _Vtab, Acc) -> e3d_vec:normal(reverse(Acc)).

%% Tests if the face has a good normal.
good_normal(Face, #we{vp=Vtab}=We) ->
    [Va,Vb|_] = Vpos =
	fold(fun(V, _, _, A) ->
		     [gb_trees:get(V, Vtab)|A]
	     end, [], Face, We),
    D = e3d_vec:sub(Va, Vb),
    good_normal(D, Vpos, Vpos).

good_normal(D1, [_Va|[Vb,Vc|_]=Vs], More) ->
    ?ASSERT(D1 == e3d_vec:sub(_Va, Vb)),
    D2 = e3d_vec:sub(Vb, Vc),
    Cross = e3d_vec:cross(D1, D2),
    case e3d_vec:len(Cross) of
	Zero when abs(Zero) < 1.0e-5 ->
	    good_normal(D2, Vs, More);
	_Len -> true
    end;
good_normal(D1, Vs, [Va,Vb|_]) ->
    good_normal(D1, Vs++[Va,Vb], []);
good_normal(_, _, _) -> false.

%% center(Face, We)
%%  Return the center of the face.
center(Face, We) ->
    wings_vertex:center(vertices_ccw(Face, We), We).

%% Vertex info for drawing.

vinfo(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    vinfo(Face, Edge, We).

vinfo(Face, Edge, #we{es=Etab}) ->
    vinfo(Edge, Etab, Face, Edge, []).

vinfo(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vinfo(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,a=Col,lf=Face,ltsu=NextEdge} ->
	    vinfo(NextEdge, Etab, Face, LastEdge, [[V|Col]|Acc]);
	#edge{ve=V,b=Col,rtsu=NextEdge} ->
	    vinfo(NextEdge, Etab, Face, LastEdge, [[V|Col]|Acc])
    end.

vertices_cw(Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    vertices_cw_1(Edge, Etab, Face, Edge, []).

vertices_cw(Face, Edge, #we{es=Etab}) ->
    vertices_cw_1(Edge, Etab, Face, Edge, []).

vertices_cw_1(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vertices_cw_1(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=NextEdge} ->
	    vertices_cw_1(NextEdge, Etab, Face, LastEdge, [V|Acc]);
	#edge{vs=V,rf=Face,rtpr=NextEdge} ->
	    vertices_cw_1(NextEdge, Etab, Face, LastEdge, [V|Acc])
    end.

vertices_ccw(Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    vertices_ccw_1(Edge, Etab, Face, Edge, []).

vertices_ccw(Face, Edge, #we{es=Etab}) ->
    vertices_ccw_1(Edge, Etab, Face, Edge, []).

vertices_ccw_1(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vertices_ccw_1(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge} ->
	    vertices_ccw_1(NextEdge, Etab, Face, LastEdge, [V|Acc]);
	#edge{vs=V,rf=Face,rtsu=NextEdge} ->
	    vertices_ccw_1(NextEdge, Etab, Face, LastEdge, [V|Acc])
    end.

%% extend_border(FacesGbSet, We) -> FacesGbSet'
%%  Extend the the given set of faces to include all faces not in the
%%  set that share at least one edge with a face in the set.
extend_border(Fs0, We) ->
    Fs = foldl(fun(Face, S0) ->
		       fold(fun(_, _, #edge{lf=Lf,rf=Rf}, S1) ->
				    if
					Lf =/= Face -> gb_sets:add(Lf, S1);
					true -> gb_sets:add(Rf, S1)
				    end
			    end, S0, Face, We)
	       end, Fs0, gb_sets:to_list(Fs0)),
    wings_sel:subtract_mirror_face(Fs, We).

%% bordering_faces(FacesGbSet, We) -> FacesGbSet'
%%  Given a set of faces, return all faces that are adjacent to
%%  a least one face outside the set.
bordering_faces(Faces, We) ->
    fold_faces(fun(Face, _, _, Rec, A) ->
		       OtherFace = other(Face, Rec),
		       case gb_sets:is_member(OtherFace, Faces) of
			   true -> A;
			   false -> gb_sets:add(Face, A)
		       end
	       end, gb_sets:empty(), Faces, We).

%% inner_edges(Faces, We) -> [Edge]
%%  Given a set of faces, return all inner edges.
inner_edges(Faces, We) ->
    S = fold_faces(fun(_, _, E, _, A) -> [E|A] end, [], Faces, We),
    inner_edges_1(sort(S), []).

inner_edges_1([E,E|T], In) ->
    inner_edges_1(T, [E|In]);
inner_edges_1([_|T], In) ->
    inner_edges_1(T, In);
inner_edges_1([], In) -> reverse(In).

%% outer_edges(Faces, We) -> [Edge]
%%  Given a set of faces, return all outer edges.
outer_edges(Faces, We) ->
    S = fold_faces(fun(_, _, E, _, A) -> [E|A] end, [], Faces, We),
    outer_edges_1(sort(S), []).

outer_edges_1([E,E|T], Out) ->
    outer_edges_1(T, Out);
outer_edges_1([E|T], Out) ->
    outer_edges_1(T, [E|Out]);
outer_edges_1([], Out) -> reverse(Out).

%% Fold over all edges surrounding a face.

fold(F, Acc, Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    fold(Edge, Etab, F, Acc, Face, Edge, not_done).

fold(F, Acc, Face, Edge, #we{es=Etab}) ->
    fold(Edge, Etab, F, Acc, Face, Edge, not_done).

fold(LastEdge, _, _, Acc, _, LastEdge, done) -> Acc;
fold(Edge, Etab, F, Acc0, Face, LastEdge, _) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge}=E ->
	    Acc = F(V, Edge, E, Acc0),
	    fold(NextEdge, Etab, F, Acc, Face, LastEdge, done);
	#edge{vs=V,rf=Face,rtsu=NextEdge}=E ->
	    Acc = F(V, Edge, E, Acc0),
	    fold(NextEdge, Etab, F, Acc, Face, LastEdge, done)
    end.

%% Fold over all edges surrounding a face.

fold_vinfo(F, Acc, Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    fold_vinfo(F, Acc, Face, Edge, Edge, Etab, not_done).

fold_vinfo(_F, Acc, _Face, LastEdge, LastEdge, _Etab, done) -> Acc;
fold_vinfo(F, Acc0, Face, Edge, LastEdge, Etab, _) ->
    Acc = case gb_trees:get(Edge, Etab) of
	      #edge{vs=V,a=VInfo,lf=Face,ltsu=NextEdge} ->
		  F(V, VInfo, Acc0);
	      #edge{ve=V,b=VInfo,rf=Face,rtsu=NextEdge} ->
		  F(V, VInfo, Acc0)
	  end,
    fold_vinfo(F, Acc, Face, NextEdge, LastEdge, Etab, done).

%% Fold over a set of faces.

fold_faces(F, Acc0, [Face|Faces], We) ->
    Acc = fold(fun(V, Edge, Rec, A) ->
		       F(Face, V, Edge, Rec, A)
	       end, Acc0, Face, We),
    fold_faces(F, Acc, Faces, We);
fold_faces(_F, Acc, [], _We) -> Acc;
fold_faces(F, Acc, Faces, We) ->
    fold_faces(F, Acc, gb_sets:to_list(Faces), We).

%% Return an iterator which can be used to traverse the face.

iterator(Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    {face_iterator,Edge,Face,Etab}.

skip_to_edge(Edge, {face_iterator,_,_,_}=Iter0) ->
    case next_cw(Iter0) of
	{_,Edge,_,_} -> Iter0;
	{_,_,_,Iter} -> skip_to_edge(Edge, Iter)
    end.

skip_to_cw(V, {face_iterator,_,Face,_}=Iter0) ->
    case next_cw(Iter0) of
	{_,_,#edge{vs=V,lf=Face},Iter} -> Iter;
	{_,_,#edge{ve=V,rf=Face},Iter} -> Iter;
	{_,_,_,Iter} -> skip_to_cw(V, Iter)
    end.

skip_to_ccw(V, {face_iterator,_,Face,_}=Iter0) ->
    case next_ccw(Iter0) of
	{_,_,#edge{ve=V,lf=Face},Iter} -> Iter;
	{_,_,#edge{vs=V,rf=Face},Iter} -> Iter;
	{_,_,_,Iter} -> skip_to_ccw(V, Iter)
    end.

iter2etab({face_iterator,_,_,Etab}) -> Etab.

%% Return next edge clockwise.

next_cw({face_iterator,Edge,Face,Etab}) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}};
	#edge{vs=V,rf=Face,rtsu=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}}
    end.

%% Return next edge counter-clockwise.

next_ccw({face_iterator,Edge,Face,Etab}) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}};
	#edge{vs=V,rf=Face,rtpr=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}}
    end.

patch_face(Face, NewEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	NewEdge -> Ftab;
	_ -> gb_trees:update(Face, NewEdge, Ftab)
    end.

patch_face(Face, Edge, NewEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	Edge -> gb_trees:update(Face, NewEdge, Ftab);
	_ -> Ftab
    end.

%% Test whether two faces are neighbors or not. (In the sense that
%% they share at least one vertex.)
are_neighbors(FaceA, FaceB, We) ->
    VsA = wings_face:vertices_ccw(FaceA, We),
    VsB = wings_face:vertices_ccw(FaceB, We),
    ordsets:intersection(ordsets:from_list(VsA),
			 ordsets:from_list(VsB)) =/= [].
