%%
%%  wings_face.erl --
%%
%%     This module contains help routines for faces, such as fold functions
%%     face iterators.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_face.erl,v 1.9 2001/09/25 09:39:18 bjorng Exp $
%%

-module(wings_face).
-export([convert_selection/1,select_more/1,select_less/1,
	 other/2,vertices/2,
	 normal/2,face_normal/2,good_normal/2,
	 to_vertices/2,surrounding_vertices/2,
	 inner_edges/2,outer_edges/2,
	 fold/4,fold_faces/4,
	 bordering_faces/2,
	 iterator/2,skip_to_edge/2,skip_to_cw/2,skip_to_ccw/2,
	 next_cw/1,next_ccw/1,
	 patch_face/3,patch_face/4]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keymember/3,member/2]).

%%
%% Convert the current selection to a face selection.
%%
convert_selection(#st{selmode=body}=St) ->
    wings_sel:convert_shape(
      fun(_, #we{fs=Ftab}) ->
	      gb_sets:from_list(gb_trees:keys(Ftab))
      end, face, St);
convert_selection(#st{selmode=face}=St) ->
    wings_sel:convert_shape(
      fun(Sel0, We) ->
	      gb_sets:fold(
		fun(Face, S0) ->
			fold(fun(_, _, #edge{lf=Lf,rf=Rf}, S1) ->
				     if Lf =/= Face ->
					     gb_sets:add(Lf, S1);
					true ->
					     gb_sets:add(Rf, S1)
				     end
			     end, S0, Face, We)
		end, Sel0, Sel0)
      end, face, St);
convert_selection(#st{selmode=edge}=St) ->
    wings_sel:convert(
      fun(Edge, #we{es=Etab}, Sel0) ->
	      #edge{lf=FaceL,rf=FaceR} = gb_trees:get(Edge, Etab),
	      gb_sets:add(FaceL, gb_sets:add(FaceR, Sel0))
      end, face, St);
convert_selection(#st{selmode=vertex}=St) ->
    wings_sel:convert(
      fun(V, We, Sel0) ->
	      wings_vertex:fold(
		fun(_, Face, _, Sel1) ->
			gb_sets:add(Face, Sel1)
		end, Sel0, V, We)
      end, face, St).

%%% Select more.
select_more(St) ->
    wings_sel:convert(
      fun(Face, We, A) ->
	      fold(
		fun(V, _, _, AA) ->
			wings_vertex:fold(
			  fun(_, AFace, _, AAA) ->
				  gb_sets:add(AFace, AAA)
			  end, AA, V, We)
		end, gb_sets:add(Face, A), Face, We)
      end, face, St).

select_less(St) ->
    wings_sel:convert_shape(
      fun(Faces, We) ->
	      Border = bordering_faces(Faces, We),
	      gb_sets:difference(Faces, Border)
      end, face, St).

%% other(Face, EdgeRecord) -> OtherFace
%%  Pick up the "other face" from an edge record.
other(Face, #edge{lf=Face,rf=Other}) -> Other;
other(Face, #edge{rf=Face,lf=Other}) -> Other.

%% to_vertices(FaceGbSet, We) -> VertexGbSet
%%  Convert a set of faces to a set of vertices.
to_vertices(Faces, We) ->
    to_vertices(gb_sets:to_list(Faces), We, []).

to_vertices([Face|Faces], We, Acc0) ->
    Acc = fold(fun(V, _, _, A) -> [V|A] end, Acc0, Face, We),
    to_vertices(Faces, We, Acc);
to_vertices([], We, Acc) -> ordsets:from_list(Acc).

%% vertices(Face, We) -> NumberOfVertices
%%  Calculate number of vertices building up a face.
vertices(Face, We) ->
    fold(fun(_, _, _, N) -> N+1 end, 0, Face, We).

%% Return the normal for a face.

normal(Face, #we{vs=Vtab}=We) ->
    Vpos = fold(fun(V, _, _, A) ->
			[wings_vertex:pos(V, Vtab)|A]
		end, [], Face, We),
    face_normal_1(Vpos).

%% Return the normal for a face, with the face given explicitly
%% as its vertices.

face_normal(Vs, Vtab) ->
    Vpos = [wings_vertex:pos(P, Vtab) || P <- Vs],
    face_normal_1(Vpos).

face_normal_1([Va,Vb|_]=Vpos) ->
    D = e3d_vec:sub(Va, Vb),
    Nsum = face_normal_2(D, Vpos, Vpos, []),
    case e3d_vec:len(Nsum) of
	Zero when abs(Zero) < 1.0e-5 ->
	    e3d_vec:zero();
	Len ->
	    e3d_vec:divide(Nsum, Len)
    end.

face_normal_2(D1, [Va|[Vb,Vc|_]=Vs], More, Acc) ->
    ?ASSERT(D1 == e3d_vec:sub(Va, Vb)),
    D2 = e3d_vec:sub(Vb, Vc),
    Cross = e3d_vec:cross(D1, D2),
    face_normal_2(D2, Vs, More, [Cross|Acc]);
face_normal_2(D1, Vs, [Va,Vb|_], Acc) ->
    face_normal_2(D1, Vs++[Va,Vb], [], Acc);
face_normal_2(D1, Other, More, Acc) -> e3d_vec:add(Acc).

%% Tests if the face has a good normal.
good_normal(Face, #we{vs=Vtab}=We) ->
    [Va,Vb|_] = Vpos =
	fold(fun(V, _, _, A) ->
		     [wings_vertex:pos(V, Vtab)|A]
	     end, [], Face, We),
    D = e3d_vec:sub(Va, Vb),
    good_normal(D, Vpos, Vpos).

good_normal(D1, [Va|[Vb,Vc|_]=Vs], More) ->
    ?ASSERT(D1 == e3d_vec:sub(Va, Vb)),
    D2 = e3d_vec:sub(Vb, Vc),
    Cross = e3d_vec:cross(D1, D2),
    case e3d_vec:len(Cross) of
	Zero when abs(Zero) < 1.0e-5 ->
	    good_normal(D2, Vs, More);
	Len -> true
    end;
good_normal(D1, Vs, [Va,Vb|_]) ->
    good_normal(D1, Vs++[Va,Vb], []);
good_normal(D1, Other, More) -> false.

%% Return the vertices surrounding a face.

surrounding_vertices(Face, #we{es=Etab,fs=Ftab}) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    face_traverse(Face, Edge, Edge, Etab, [], not_done).

face_traverse(Face, LastEdge, LastEdge, Es, Acc, done)-> Acc;
face_traverse(Face, Edge, LastEdge, Es, Acc, _) ->
    case gb_trees:get(Edge, Es) of
	#edge{ve=V,lf=Face,ltsu=NextEdge} ->
	    face_traverse(Face, NextEdge, LastEdge, Es, [V|Acc], done);
	#edge{vs=V,rf=Face,rtsu=NextEdge} ->
	    face_traverse(Face, NextEdge, LastEdge, Es, [V|Acc], done)
    end.

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
inner_edges_1([E|T], In) ->
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
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    fold(F, Acc, Face, Edge, Edge, Etab, not_done).

fold(F, Acc, Face, LastEdge, LastEdge, Etab, done)-> Acc;
fold(F, Acc0, Face, Edge, LastEdge, Etab, _) ->
    Acc = case gb_trees:get(Edge, Etab) of
	      #edge{ve=V,lf=Face,ltsu=NextEdge}=E ->
		  F(V, Edge, E, Acc0);
	      #edge{vs=V,rf=Face,rtsu=NextEdge}=E ->
		  F(V, Edge, E, Acc0)
    end,
    fold(F, Acc, Face, NextEdge, LastEdge, Etab, done).

%% fold over a set of faces.

fold_faces(F, Acc0, [Face|Faces], We) ->
    Acc = fold(fun(V, Edge, Rec, A) ->
		       F(Face, V, Edge, Rec, A)
	       end, Acc0, Face, We),
    fold_faces(F, Acc, Faces, We);
fold_faces(F, Acc, [], We) -> Acc;
fold_faces(F, Acc, Faces, We) ->
    fold_faces(F, Acc, gb_sets:to_list(Faces), We).

%% Return an iterator which can be used to traverse the face.

iterator(Face, #we{es=Etab,fs=Ftab}) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
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

%% Return next edge clockwise.

next_cw({face_iterator,Edge,Face,Etab}) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}};
	#edge{vs=V,rf=Face,rtsu=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}}
    end.

%% Return next edge clockwise.

next_ccw({face_iterator,Edge,Face,Etab}) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}};
	#edge{vs=V,rf=Face,rtpr=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}}
    end.

patch_face(Face, NewEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	#face{edge=NewEdge} -> Ftab;
	Rec -> gb_trees:update(Face, Rec#face{edge=NewEdge}, Ftab)
    end.

patch_face(Face, Edge, NewEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	#face{edge=Edge}=Rec ->
	    gb_trees:update(Face, Rec#face{edge=NewEdge}, Ftab);
	Other -> Ftab
    end.
