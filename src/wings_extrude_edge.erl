%%
%%  wings_extrude_edge.erl --
%%
%%     This module contains the Extrude and Bevel commands for edges.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_extrude_edge.erl,v 1.1 2001/08/14 18:16:40 bjorng Exp $
%%

-module(wings_extrude_edge).
-export([bevel/1,extrude/2]).

-include("wings.hrl").
-import(lists, [foldl/3,keydelete/3,member/2,reverse/2,last/1]).

-define(EXTRUDE_DIST, 2.0).

%%
%% The Bevel command (for edges).
%%

bevel(St0) ->
    {St,{Tvs,Sel}} = wings_sel:mapfold_shape(fun bevel_edges/4, {[],[]}, St0),
    wings_drag:init_drag(Tvs, {0.0,1.0E200}, St#st{selmode=face,sel=Sel}).

bevel_edges(Id, Edges, #we{es=Etab,next_id=Next}=We0, {Tvs,Ss}) ->
    {We1,OrigVs} = extrude_edges(Edges, We0),
    {We2,FaceSel0} = bevel_dissolve(Edges, We1),
    Tv = bevel_tv(OrigVs, We2),
    #we{fs=Ftab,vs=Vtab0} = We3 =
	foldl(fun(V, W0) ->
		      wings_collapse:collapse_vertex(V, W0)
	      end, We2, OrigVs),
    Vtab = bevel_reset_pos(OrigVs, We2, Vtab0),
    We = We3#we{vs=Vtab},
    FaceSel1 = [Face || Face <- FaceSel0, gb_trees:is_defined(Face, Ftab)],
    FaceSel = gb_sets:from_ordset(FaceSel1),
    {We,{[{Id,Tv}|Tvs],[{Id,FaceSel}|Ss]}}.

bevel_tv(Vs, We) ->
    foldl(fun(V, A) -> bevel_tv_1(V, We, A) end, [], Vs).

bevel_tv_1(V, We, Acc) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Face, Edge, Rec, Tv0) ->
	      OtherV = wings_vertex:other(V, Rec),
	      Pos = wings_vertex:pos(OtherV, We),
	      Vec = wings_mat:divide(wings_mat:subtract(Pos, Center),
				     ?EXTRUDE_DIST),
	      [{Vec,[OtherV]}|Tv0]
      end, Acc, V, We).

bevel_reset_pos(Vs, We, Vtab) ->
    foldl(fun(V, A) -> bevel_reset_pos_1(V, We, A) end, Vtab, Vs).

bevel_reset_pos_1(V, We, Vtab) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Face, Edge, Rec, Vt) ->
	      OtherV = wings_vertex:other(V, Rec),
	      Vtx = gb_trees:get(OtherV, Vt),
	      gb_trees:update(OtherV, Vtx#vtx{pos=Center}, Vt)
      end, Vtab, V, We).

bevel_dissolve(Edges, #we{es=Etab}=We0) ->
    Faces = foldl(fun(E, A) ->
			  #edge{lf=Lf,rf=Rf} = gb_trees:get(E, Etab),
			  [Lf,Rf|A]
		  end, [], gb_sets:to_list(Edges)),
    #we{fs=Ftab} = We = wings_edge:dissolve_edges(Edges, We0),
    {We,Faces}.

%%
%% The Extrude command (for edges).
%%

extrude(Type, St0) ->
    St = wings_sel:map_shape(
	   fun(Edges, We0) ->
		   {We,_} = extrude_edges(Edges, We0),
		   We
	   end, St0),
    wings_move:setup(Type, St).


extrude_edges(Edges, #we{es=Etab}=We0) ->
    G = digraph:new(),
    foldl(fun(Edge, A) ->
		  #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		  Faces = [Lf,Rf],
		  digraph:add_vertex(G, Va),
		  digraph:add_vertex(G, Vb),
		  digraph:add_edge(G, Va, Vb, Faces),
		  digraph:add_edge(G, Vb, Va, Faces),
		  A
	  end, [], gb_sets:to_list(Edges)),
    Vs0 = digraph:vertices(G),
    Vs1 = ordsets:from_list(Vs0),
    {We2,Vs} = foldl(fun(V, A) ->
			     new_vertices(V, G, Edges, A)
		     end, {We0,[]}, Vs1),
    We = connect(Vs, G, We2),
    digraph:delete(G),
    {enclosed_faces(Vs1, Edges, We),Vs1}.

new_vertices(V, G, Edges, {We0,_}=Acc) ->
    Center = wings_vertex:pos(V, We0),
    wings_vertex:fold(
      fun(Edge, _, _, {W0,Vs}=A) ->
	      case gb_sets:is_member(Edge, Edges) of
		  true -> A;
		  false ->
		      {W1,NewV,_} = wings_edge:cut(Edge, 2, W0),
		      digraph:add_vertex(G, NewV),
		      digraph:add_edge(G, V, NewV),
		      digraph:add_edge(G, NewV, V),
		      {move_vertex(NewV, Center, W1),[NewV|Vs]}
	      end
      end, Acc, V, We0).
		      
move_vertex(V, Center, #we{vs=Vtab0}=We) ->
    #vtx{pos=Pos0} = Rec = gb_trees:get(V, Vtab0),
    Dir = wings_mat:subtract(Pos0, Center),
    case wings_mat:len(Dir) of
	D when D < ?EXTRUDE_DIST -> We;
	D ->
	    Pos = wings_mat:add(Center, wings_mat:mul(wings_mat:norm(Dir),
						      ?EXTRUDE_DIST)),
	    Vtab = gb_trees:update(V, Rec#vtx{pos=Pos}, Vtab0),
	    We#we{vs=Vtab}
    end.
    
connect(Vs, G, We) ->
    FaceVs = wings_vertex:per_face(Vs, We),
    foldl(fun({Face,Vs}, A) -> connect(Face, Vs, G, A) end, We, FaceVs).

connect(Face, Vs, G, We) ->
    R0 = foldl(fun(V, A) ->
		       [Neighbour] = digraph:out_neighbours(G, V),
		       [{Neighbour,V}|A]
	       end, [], Vs),
    R = sofs:relation(R0),
    F0 = sofs:relation_to_family(R),
    F = sofs:to_external(F0),
    Paths = find_paths(F, Face, G, []),
    do_connect(Paths, We).

do_connect([{First,Last,LongPath}|T], We0) ->
    We = connect_inner(First, Last, LongPath, We0),
    do_connect(T, We);
do_connect([[_,_]=Pair|T], We0) ->
    We = wings_vertex_cmd:connect(gb_sets:from_list(Pair), We0),
    do_connect(T, We);
do_connect([], We) -> We.

find_paths([{_,[_,_]=Pair}|T], Face, G, Paths) ->
    find_paths(T, Face, G, [Pair|Paths]);
find_paths([{CornerA,[V]}|T0], Face, G, Paths) ->
    case find_path(CornerA, T0, Face, G) of
	none -> find_paths(T0, Face, G, Paths);
	{CornerB,DestV,[CornerA,CornerB]} ->
	    T = keydelete(CornerB, 1, T0),
	    find_paths(T, Face, G, [[V,DestV]|Paths]);
	{CornerB,DestV,LongPath} ->
	    T = keydelete(CornerB, 1, T0),
	    find_paths(T, Face, G, [{V,DestV,LongPath}|Paths])
    end;
find_paths([], Face, G, Paths) -> Paths.

find_path(CornerA, [{_,[_,_]}|T], Face, G) ->
    find_path(CornerA, T, Face, G);
find_path(CornerA, [{CornerB,[V]}|T], Face, G) ->
    case get_face_path(CornerA, CornerB, Face, G) of
	false -> find_path(CornerA, T, Face, G);
	Path  -> {CornerB,V,Path}
    end;
find_path(Corner, [], Face, G) -> none.

get_face_path(From, To, Face, G) ->
    get_face_path(From, To, Face, G, [From]).

get_face_path(From, To, Face, G, Acc) ->
    Next = foldl(fun(E, A) ->
			 case digraph:edge(G, E) of
			     {_,Va,Vb,[Lf,Rf]} when Face =:= Lf; Face =:= Rf ->
				 case member(Va, Acc) of
				     false -> Va;
				     true ->
					 case member(Vb, Acc) of
					     false -> Vb;
					     true -> A
					 end
				 end;
			     Other -> A
			 end
		 end, none, digraph:out_edges(G, From)),
    case Next of
	none -> false;
	To -> reverse(Acc, [To]);
	Other -> get_face_path(Next, To, Face, G, [Next|Acc])
    end.

connect_inner(Current, Last, [A|[B,C|_]=Next], We0) ->
    {We,V} = connect_one_inner(Current, A, B, C, We0),
    connect_inner(V, Last, Next, We);
connect_inner(Current, Last, Other, We) ->
    wings_vertex_cmd:connect([Current,Last], We).

enclosed_faces(Vs, Edges, We) ->
    Fs0 = wings_vertex:per_face(Vs, We),
    fully_enclosed(Fs0, Edges, We).
    
fully_enclosed([{Face,_}|Fs], Edges, We) ->
    Encl = wings_face:fold(fun(_, _, _, false) -> false;
			      (_, Edge, _, true) ->
				   gb_sets:is_member(Edge, Edges)
			   end, true, Face, We),
    case Encl of
	true ->
	    fully_enclosed(Fs, Edges, connect_fully_enclosed(Face, We));
	false ->
	    fully_enclosed(Fs, Edges, We)
    end;
fully_enclosed([], Edges, We) -> We.

connect_fully_enclosed(Face, We0) ->
    case wings_face:surrounding_vertices(Face, We0) of
	[Va,Vb,Vc|Vs]=Path ->
 	    {We1,NewFace} = wings_vertex:force_connect(Va, Vc, Face, We0),
 	    Edge = NewFace + 1,
 	    {We2,NewVa,DelEdge} = wings_edge:cut(Edge, 2, We1),
 	    Last = last(Path),
	    We3 = move_vertex(NewVa, Last, Va, Vb, We2),
	    {We4,NewVb} = connect_one_inner(NewVa, Va, Vb, Vc, We3),
	    We = wings_edge:dissolve_edge(DelEdge, We4),
 	    connect_inner(NewVb, NewVa, tl(Path)++[hd(Path)], We);
	Other -> We0				%Degenerated face.
    end.

connect_one_inner(Current, A, B, C, We0) ->
    Pair = [Current,B],
    [Face] = [Face || {Face,[_,_]} <- wings_vertex:per_face(Pair, We0)],
    {We1,NewFace} = wings_vertex:force_connect(Current, B, Face, We0),
    Edge = NewFace + 1,
    {We,NewV,_} = wings_edge:cut(Edge, 2, We1),
    {move_vertex(NewV, A, B, C, We),NewV}.

move_vertex(V, A, B, C, #we{vs=Vtab}=We) ->
    VRec0 = gb_trees:get(V, Vtab),
    APos = wings_vertex:pos(A, Vtab),
    BPos = wings_vertex:pos(B, Vtab),
    CPos = wings_vertex:pos(C, Vtab),
    VecA = wings_mat:norm(wings_mat:subtract(APos, BPos)),
    VecB = wings_mat:norm(wings_mat:subtract(CPos, BPos)),
    Vec = wings_mat:norm(wings_mat:add(VecA, VecB)),
    NewPos =
	case wings_mat:len(Vec) of
	    Short when Short < 1.0E-6 ->
		%% A "winged vertex" - the edges have the same direction.
		%% Now we must get tricky.
		VPos = wings_vertex:pos(V, Vtab),
		Dir = wings_mat:subtract(VPos, BPos),
		DPos = wings_mat:add([BPos,Dir,Dir]),
		wings_mat:add(BPos, wings_mat:subtract(DPos, APos));
	    Other ->
		Sin = wings_mat:len(wings_mat:cross_product(VecA, Vec)),
		wings_mat:add(BPos, wings_mat:mul(Vec, ?EXTRUDE_DIST/Sin))
	end,
    VRec = VRec0#vtx{pos=NewPos},
    We#we{vs=gb_trees:update(V, VRec, Vtab)}.
