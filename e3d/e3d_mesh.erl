%%
%%  e3d_mesh.erl --
%%
%%     Utility functions for E3D meshes, such as cleanup and triangulation.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_mesh.erl,v 1.4 2001/08/27 07:34:51 bjorng Exp $
%%

-module(e3d_mesh).
-export([clean/1,triangulate/1,make_quads/1,vertex_normals/1]).

-include("e3d.hrl").
-import(lists, [foreach/2,sort/1,reverse/1,reverse/2,seq/2,
		foldl/3,filter/2,mapfoldl/3,mapfoldr/3,last/1]).

clean(#e3d_mesh{vs=Vs0,fs=Fs0,he=He0}=Mesh) ->
    {Vs,Fs,He} = clean(Vs0, Fs0, He0),
    Mesh#e3d_mesh{vs=Vs,fs=Fs,he=He}.

clean(Vs0, Fs0, He0) ->
    Vtab0 = make_vtab(Vs0),
    {Fs1,Vtab} = merge_vertices(Fs0, Vtab0),
    Fs2 = number_faces(sort(Fs1)),
    Es0 = collect_edges(Fs2),
    Part = partition_faces(Es0),
    {Vs,Fs3} = renumber_part(Part, Fs2, gb_trees:to_list(Vtab)),
    Fs = [Data || {_,Data} <- Fs3],
    {Vs,Fs,He0}.

%% Merge vertices for bad faces.
merge_vertices(Fs0, Vtab0) ->
    {Fs1,Map} = merge_vertices_1(Fs0, Vtab0, [], gb_trees:empty()),
    Vtab = foldl(fun({V,_}, A) -> gb_trees:delete(V, A) end,
		 Vtab0, gb_trees:to_list(Map)),
    Fs = [merge_renumber_face(Face, Map) || Face <- Fs1],
    {Fs,Vtab}.

merge_vertices_1([#e3d_face{vs=[A0,B0,C0|_]=Vs}=F|Fs], Vtab, FsAcc, Map0) ->
    case good_vs(Vs) of
	false ->
	    merge_vertices_1(Fs, Vtab, Map0, FsAcc);
	true ->
	    A = gb_trees:get(A0, Vtab),
	    B = gb_trees:get(B0, Vtab),
	    C = gb_trees:get(C0, Vtab),
	    D1 = e3d_vec:sub(A, B),
	    D2 = e3d_vec:sub(B, C),
	    case e3d_vec:norm_cross(D1, D2) of
		zero_vector ->
		    Map1 = almost_same(A0, A, B0, B, Map0),
		    Map2 = almost_same(A0, A, C0, C, Map1),
		    Map = almost_same(B0, B, C0, C, Map2),
		    merge_vertices_1(Fs, Vtab, FsAcc, Map);
		Normal -> merge_vertices_1(Fs, Vtab, [F|FsAcc], Map0)
	    end
    end;
merge_vertices_1([], Vtab, FsAcc, Map) -> {FsAcc,Map}.

almost_same(A0, A, B0, B, Map) ->
    case e3d_vec:dist(A, B) of
	Dist when Dist < 1.0E-4 ->
	    case gb_trees:lookup(A0, Map) of
		{value,V} -> gb_trees:enter(B0, V, Map);
		none -> gb_trees:enter(B0, A0, Map)
	    end;
	Dist -> Map
    end.

merge_renumber_face(#e3d_face{vs=Vs}=Face, Map) ->
    Face#e3d_face{vs=[renumber_vtx(V, Map) || V <- Vs]}.

renumber_vtx(V, Map) ->
    case gb_trees:lookup(V, Map) of
	{value,NewV} -> renumber_vtx(NewV, Map);
	none -> V
    end.

make_vtab(Vs) ->
    make_vtab(Vs, 0, []).
make_vtab([V|Vs], N, Acc) ->
    make_vtab(Vs, N+1, [{N,V}|Acc]);
make_vtab([], N, Acc) ->
    gb_trees:from_orddict(reverse(Acc)).

number_faces(Fs) ->
    number_faces(Fs, 0, []).
number_faces([F|Fs], Face, Acc) ->
    number_faces(Fs, Face+1, [{Face,F}|Acc]);
number_faces([], Face, Acc) -> reverse(Acc).

collect_edges(Fs) ->
    collect_edges(Fs, []).

collect_edges([{Face,#e3d_face{vs=Vs,vis=Flag}}|Fs], Acc0) ->
    case good_vs(Vs) of
	false -> collect_edges(Fs, Acc0);
	true ->
	    Pairs = pairs(Vs, Flag),
	    Acc = edges(Pairs, Face, Acc0),
	    collect_edges(Fs, Acc)
    end;
collect_edges([], Es0) ->
    Es1 = sofs:relation(Es0),
    Es = sofs:relation_to_family(Es1),
    sofs:to_external(Es).

good_vs([V,V,_]) -> false;
good_vs([_,V,V]) -> false;
good_vs([V,_,V]) -> false;
good_vs(Other) -> true.

edges([{Va,Vb,Vis}=Vs|Ps], Face, Acc) when Va < Vb ->
    Name = {Va,Vb},
    edges(Ps, Face, [{Name,Face}|Acc]);
edges([{Va,Vb,Vis}=Vs|Ps], Face, Acc) ->
    Name = {Vb,Va},
    edges(Ps, Face, [{Name,Face}|Acc]);
edges([], Face, Acc) -> Acc.

pairs(Vs, Vis) ->
    pairs(Vs, Vs, Vis, []).

pairs([V1|[V2|_]=Vs], More, Vis, Acc) ->
    State = visible(Vis),
    pairs(Vs, More, Vis bsl 1, [{V1,V2,State}|Acc]);
pairs([V1], [V2|_], Vis, Acc) ->
    State = visible(Vis),
    [{V1,V2,State}|Acc].

visible(F) when F band 4 =/= 0 -> visible;
visible(F) -> invisible.

    
debug_dump(Es, Ftab, Vs) ->
    {ok,F} = file:open("debug.dump", [write]),
    foreach(fun({E,Data}) ->
		    io:format(F, "~w: ~w\n", [E,Data])
	    end, Es),
    io:format(F, "\n\nFaces\n\n", []),
    foreach(fun({Face,Data}) ->
		    io:format(F, "~w: ~w\n", [Face,Data])
	    end, Ftab),
    io:format(F, "\n\nVertices\n\n", []),
    print_vs(Vs, F, 0),
    ok = file:close(F).

print_vs([Pos|Fs], F, V) ->
    io:format(F, "~w: ~w\n", [V,Pos]),
    print_vs(Fs, F, V+1);
print_vs([], F, Face) -> ok.

%%%

partition_faces(Es0) ->
    F0 = sofs:family_of_subsets(Es0),
    F1 = sofs:specification(fun({_,L}) -> length(L) =< 2 end, F0),
    R0 = sofs:family_to_relation(F0),
    R = sofs:inverse(R0),
    F2 = sofs:relation_to_family(R),
    F = sofs:to_external(F2),
    Etab0 = sofs:to_external(F1),
    Ftab = gb_trees:from_orddict(F),
    Etab = gb_trees:from_orddict(Etab0),
    part_faces_1(Ftab, Etab, []).

part_faces_1(Ftab0, Etab, Acc) ->
    case gb_trees:is_empty(Ftab0) of
	true -> Acc;
	false ->
	    {Face,_,_} = gb_trees:take_smallest(Ftab0),
	    {Part,Ftab} = part_faces_2(gb_sets:singleton(Face),
				      Ftab0, Etab, gb_sets:empty()),
	    part_faces_1(Ftab, Etab, [gb_sets:to_list(Part)|Acc])
    end.

part_faces_2(Ws0, Ftab0, Etab, Acc0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {Acc0,Ftab0};
	false ->
	    {Face,Ws1} = gb_sets:take_smallest(Ws0),
	    Edges = gb_trees:get(Face, Ftab0),
	    Ftab = gb_trees:delete(Face, Ftab0),
	    Acc = gb_sets:insert(Face, Acc0),
	    Ws = part_faces_3(Edges, Etab, Acc, Ws1),
	    part_faces_2(Ws, Ftab, Etab, Acc)
    end.

part_faces_3([E|Es], Etab, Seen, Acc0) ->
    case gb_trees:lookup(E, Etab) of
	none -> part_faces_3(Es, Etab, Seen, Acc0);
	{value,Fs} ->
	    Acc = foldl(
		    fun(Face, A) ->
			    case gb_sets:is_member(Face, Seen) of
				true -> A;
				false -> gb_sets:add(Face, A)
			    end
		    end, Acc0, Fs),
	    part_faces_3(Es, Etab, Seen, Acc)
    end;
part_faces_3([], Etab, Seen, Acc) -> Acc.

%%%

renumber_part(Ps, Fs, Vtab0) ->
    Ftab = sofs:relation(Fs),
    Vtab = sofs:relation(Vtab0),
    renumber_part(Ps, Ftab, Vtab, [], []).
    
renumber_part([P|Ps], Ftab, Vtab, FsAcc0, VsAcc0) ->
    Fs0 = sofs:restriction(Ftab, sofs:from_list(P)),
    Fs1 = sofs:range(Fs0),
    Fs = sofs:to_external(Fs1),
    Vs0 = sofs:from_term([Vs || #e3d_face{vs=Vs} <- Fs]),
    Vs1 = sofs:union(Vs0),
    Vs2 = sofs:restriction(Vtab, Vs1),
    Vs = sofs:to_external(Vs2),
    {Map0,_} = mapfoldl(fun({V,Pos}, N) ->
				{{V,N},N+1}
			end, length(VsAcc0), Vs),
    Map = gb_trees:from_orddict(Map0),
    FsAcc = [renumber_face(Face, Map) || Face <- Fs] ++ FsAcc0,
    VsAcc = reverse([Pos || {_,Pos} <- Vs], VsAcc0),
    renumber_part(Ps, Ftab, Vtab, FsAcc, VsAcc);
renumber_part([], Ftab, Vtab, FsAcc, VsAcc) ->
    {reverse(VsAcc),number_faces(sort(FsAcc))}.

renumber_face(#e3d_face{vs=Vs}=Face, Map) ->
    Face#e3d_face{vs=[gb_trees:get(V, Map) || V <- Vs]}.

%%%
%%% If two adjacent triangles share a hidden edge, combine the
%%% triangles to a quad. Triangles with more than one hidden
%%% will never be combined to avoid isolating vertices and/or
%%% creating concave polygons.
%%%

make_quads(#e3d_mesh{type=triangle,fs=Fs0}=Mesh) ->
    Ftab0 = number_faces(Fs0),
    Es = rhe_collect_edges(Ftab0),
    case edges_ok(Es) of
	false -> Mesh;
	true ->
	    Ftab1 = gb_trees:from_orddict(Ftab0),
	    Ftab = merge_faces(Es, Ftab1),
	    Fs = wings_util:gb_trees_values(Ftab),
	    Mesh#e3d_mesh{type=polygon,fs=Fs}
    end;
make_quads(Mesh) -> Mesh.

edges_ok([{_,[{_,Va,Vb,_},{_,Vb,Va,_}]}|T]) -> edges_ok(T);
edges_ok([_|_]=Other) -> false;
edges_ok([]) -> true.
    
merge_faces([{Name,L}|Es], Ftab0) ->
    Ftab = case L of
	       [{Fa,Va,Vb,invisible},{Fb,_,_,invisible}] ->
		   merge_faces_1(Fa, Fb, Va, Vb, Ftab0);
	       Other -> Ftab0
	   end,
    merge_faces(Es, Ftab);
merge_faces([], Ftab) -> Ftab.

merge_faces_1(Fa, Fb, Va, Vb, Ftab0) ->
    case {gb_trees:lookup(Fa, Ftab0),gb_trees:lookup(Fb, Ftab0)} of
	{{value,#e3d_face{vs=Vs1,mat=Mat}=Rec0},{value,#e3d_face{vs=Vs2,mat=Mat}}} ->
	    case merge_faces_2(Va, Vb, Vs1, Vs2) of
		error -> Ftab0;
		Vs when list(Vs) ->
		    Rec = Rec0#e3d_face{vs=Vs,vis=-1},
		    Ftab = gb_trees:update(Fa, Rec, Ftab0),
		    gb_trees:delete(Fb, Ftab)
	    end;
	{_,_} -> Ftab0
    end.

merge_faces_2(Va, Vb, VsA0, VsB0) ->
    VsA = rot_face(Va, Vb, VsA0),
    VsB = rot_face(Va, Vb, VsB0),
    merge_faces_3(Va, Vb, VsA, VsB).

merge_faces_3(Va, Vb, [Va,Vb,Vx], [Vb,Va,Vy]) -> [Vx,Va,Vy,Vb];
merge_faces_3(Va, Vb, [Va,Vb,Vx], [Va,Vb,Vy]) -> [Vx,Va,Vy,Vb];
merge_faces_3(Va, Vb, [Va,Vb|Vs1], [Vb,Va|Vs2]) ->
    [Vb|Vs1]++[Va|Vs2];
merge_faces_3(Va, Vb, [Va,Vb|Vs1], [Va,Vb|Vs2]) ->
    [Vb|Vs1]++[Va|Vs2];
merge_faces_3(Va, Vb, Vs1, Vs2) -> error.

rot_face(Va, Vb, [Va,Vb|_]=Face) -> Face;
rot_face(Va, Vb, [Vb,Va|_]=Face) -> Face;
rot_face(Va, Vb, [Va,Vx,Vb]) -> [Vb,Va,Vx];
rot_face(Va, Vb, [Vb,Vx,Va]) -> [Va,Vb,Vx];
rot_face(Va, Vb, [Vx,Va,Vb]) -> [Va,Vb,Vx];
rot_face(Va, Vb, [Vx,Vb,Va]) -> [Va,Vb,Vx];
rot_face(Va, Vb, Vs) ->
    rot_face(Va, Vb, Vs, []).

rot_face(Va, Vb, [Va,Vb|_]=Vs, Acc) -> Vs ++ reverse(Acc);
rot_face(Va, Vb, [Vb,Va|_]=Vs, Acc) -> Vs ++ reverse(Acc);
rot_face(Va, Vb, [Va|_]=Vs0, Acc) ->
    [Vb|Vs] = reverse(Vs0),
    [Vb|reverse(Vs)];
rot_face(Va, Vb, [Vb|_]=Vs0, Acc) ->
    [Va|Vs] = reverse(Vs0),
    [Va|reverse(Vs)];
rot_face(Va, Vb, [V|Vs], Acc) -> rot_face(Va, Vb, Vs, [V|Acc]).

rhe_collect_edges(Fs) ->
    rhe_collect_edges(Fs, []).

rhe_collect_edges([{Face,#e3d_face{vs=Vs,vis=Vis0}}|Fs], Acc0) ->
    Vis1 = Vis0 band 7,
    Vis = case (Vis1 band 1) + ((Vis1 bsr 1) band 1) + ((Vis1 bsr 2) band 1) of
	      0 -> 7;
	      1 -> 7;
	      _ -> Vis1
	  end,
    Pairs = pairs(Vs, Vis),
    Acc = rhe_edges(Pairs, Face, Acc0),
    rhe_collect_edges(Fs, Acc);
rhe_collect_edges([], Es0) ->
    Es1 = sofs:relation(Es0),
    Es = sofs:relation_to_family(Es1),
    sofs:to_external(Es).

rhe_edges([{Va,Vb,Vis}=Vs|Ps], Face, Acc) when Va < Vb ->
    Name = {Va,Vb},
    rhe_edges(Ps, Face, [{Name,{Face,Va,Vb,Vis}}|Acc]);
rhe_edges([{Va,Vb,Vis}=Vs|Ps], Face, Acc) ->
    Name = {Vb,Va},
    rhe_edges(Ps, Face, [{Name,{Face,Va,Vb,Vis}}|Acc]);
rhe_edges([], Face, Acc) -> Acc.

%%%
%%% Mesh triangulation.
%%%

triangulate(#e3d_mesh{type=triangle}=Mesh) -> Mesh;
triangulate(#e3d_mesh{type=polygon,fs=Fs0,vs=Vs}=Mesh) ->
    case triangulate(Fs0, list_to_tuple(Vs), []) of
	error -> error;
	Fs -> Mesh#e3d_mesh{type=triangle,fs=Fs}
    end.

triangulate([#e3d_face{vs=[_,_,_]}=Tri|Ps], Vtab, Acc) ->
    triangulate(Ps, Vtab, [Tri|Acc]);
triangulate([#e3d_face{vs=[A,B,C,D]}=Quad|Ps], Vtab, Acc) ->
    TriA = Quad#e3d_face{vs=[A,B,C],vis=6},
    TriB = Quad#e3d_face{vs=[C,D,A],vis=6},
    triangulate(Ps, Vtab, [TriA,TriB|Acc]);
triangulate([#e3d_face{vs=Vs}=FaceRec|Ps], Vtab, Acc0) ->
    Plane = polygon_plane(Vs, Vtab),
    Acc = case is_convex(Vs, Plane, Vtab) of
 	      true ->
 		  tri_convex_poly(Vs, FaceRec, -1, false, Acc0);
  	      false ->
		  tri_concave_poly(Vs, Vtab, Plane, FaceRec, Acc0)
	  end,
    if
	Acc =:= error -> error;
	true -> triangulate(Ps, Vtab, Acc)
    end;
triangulate([], Vtab, Acc) -> reverse(Acc).

tri_convex_poly([A,B,C|More0], FaceRec, Vis0, Dir, Acc0) ->
    Tri = FaceRec#e3d_face{vs=[A,B,C],vis=(Vis0 bsl 1) band 7},
    Acc = [Tri|Acc0],
    case Dir of
	false ->
	    case reverse(More0) of
		[] -> Acc;
		[Last|More1] ->
		    More = [Last,A,C|reverse(More1)],
		    Vis = ((Vis0 bsr 3) bsl 3) bor
			((Vis0 band 1) bsl 1) bor 1,
		    tri_convex_poly(More, FaceRec, Vis, true, Acc)
	    end;
	true ->
	    case More0 of
		[] -> Acc;
		Other ->
		    More = [A,C|More0],
		    Vis = ((Vis0 bsr 3) bsl 2) bor (Vis0 band 1),
		    tri_convex_poly(More, FaceRec, Vis, false, Acc)
	    end
    end.

tri_concave_poly(Vs, Vtab, Plane, FaceRec, Acc0) ->
    case tri_concave_poly(Vs, [], Vtab, Plane, FaceRec, false, Acc0) of
	error ->
	    NegPlane = e3d_vec:neg(Plane),
	    tri_concave_poly(Vs, [], Vtab, NegPlane, FaceRec, false, Acc0);
	Acc -> Acc
    end.

tri_concave_poly([A,B], [_|_]=More0, Vtab, Plane, FaceRec, _, Acc) ->
    case reverse(More0) of
	[error|_] -> error;
	More1 ->
	    More = [A,B|More1],
	    tri_concave_poly(More, [error], Vtab, Plane, FaceRec, false, Acc)
    end;
tri_concave_poly([A,B], [], Vtab, Plane, FaceRec, Convex, Acc) -> error;
tri_concave_poly([A,B,C], [], Vtab, Plane, FaceRec, {Rest,Acc}, _) ->
    Tri = FaceRec#e3d_face{vs=[A,B,C],vis=7},
    [Tri|Acc];
%%    tri_convex_poly(Rest, FaceRec, 7, false, Acc);
tri_concave_poly([A|[B,C|Vs]=Vs0]=X, More0, Vtab, Plane, FaceRec,
		 Convex0, Acc0) ->
    case is_convex([A,B,C], Plane, Vtab) of
	true ->
	    Tri = FaceRec#e3d_face{vs=[A,B,C],vis=7},
	    Acc = [Tri|Acc0],
	    More = case reverse(More0) of
		       [error|M] -> Vs++M;
		       M -> Vs++M
		   end,
	    Rest = [A,C|More],
	    Convex = if
			 Convex0 =:= false -> {Rest,Acc};
			 true -> Convex0
		     end,
	    tri_concave_poly(Rest, [], Vtab, Plane, FaceRec, Convex, Acc);
	false ->
	    tri_concave_poly(Vs0, [A|More0], Vtab, Plane, FaceRec, false, Acc0)
    end.

polygon_plane([A,B,C|_], Vtab) ->
    e3d_vec:normal(element(A+1, Vtab), element(B+1, Vtab), element(C+1, Vtab)).
	    
is_convex(Vs, Plane, Vtab) ->
    is_convex(Vs, Vs, Plane, Vtab).

is_convex([A0|[B0,C0|_]=Vs], More, Plane, Vtab) ->
    A = element(A0+1, Vtab),
    B = element(B0+1, Vtab),
    C = element(C0+1, Vtab),
    U = e3d_vec:sub(B, A),
    N = e3d_vec:cross(e3d_vec:norm(e3d_vec:sub(C, B)), Plane),
    Dot = e3d_vec:dot(U, N),
    if
 	Dot > 0 -> is_convex(Vs, More, Plane, Vtab);
 	true -> false
    end;
is_convex([A,B], [C,D,E|_], Plane, Vtab) ->
    is_convex([A,B,C,D,E], [], Plane, Vtab);
is_convex([A,B], [], Plane, Vtab) -> true.

%%%
%%% Calculate normals for each vertex in a mesh.
%%%

vertex_normals(#e3d_mesh{fs=Ftab,vs=Vtab0,he=He}=Mesh) ->
    Vtab = list_to_tuple(Vtab0),
    FaceNormals = face_normals(Ftab, Vtab),

    %% Calculate normals for vertices with no hard edges.
    HardVs = sofs:field(sofs:relation(He)),
    VtxFace0 = sofs:relation(vtx_to_face_tab(Ftab)),
    HardVtxFace0 = sofs:restriction(VtxFace0, HardVs),
    VtxFace1 = sofs:difference(VtxFace0, HardVtxFace0),
    VtxFace2 = sofs:relation_to_family(VtxFace1),
    VtxFace = sofs:to_external(VtxFace2),
    VtxNormals0 = vertex_normals(VtxFace, 0, FaceNormals),

    %% Calculate normals for vertices surrounded by one or more hard edges.
    HardVtxFace = sofs:to_external(HardVtxFace0),
    VtxNormals1 = vn_hard_normals(He, HardVtxFace, Ftab,
				  FaceNormals, VtxNormals0),

    %% Generate face data.
    VtxNormals = gb_trees:from_orddict(sort(VtxNormals1)),
    Faces = vn_faces(Ftab, VtxNormals, 0, []),
    Normals0 = wings_util:gb_trees_values(VtxNormals),
    Normals1 = sort(Normals0),
    Normals = [N || {Vn,N} <- Normals1],
    {Faces,Normals}.

vn_faces([#e3d_face{mat=Mat,vs=Vs0}|Fs], VtxNormals, Face, Acc) ->
    Vs1 = foldl(fun(V, A) ->
			vn_face(V, VtxNormals, Face, A)
		end, [], Vs0),
    Vs = reverse(Vs1),
    vn_faces(Fs, VtxNormals, Face+1, [{Mat,Vs}|Acc]);
vn_faces([], VtxNormals, Face, Acc) -> reverse(Acc).

vn_face(V, VtxNormals, Face, Acc) ->
    [{V,vn_lookup(V, Face, VtxNormals)}|Acc].
    
vn_lookup(V, Face, VtxNormals) ->
    case gb_trees:lookup(V, VtxNormals) of
	{value,{Vn,_}} -> Vn;
	none ->
	    {Vn,_} = gb_trees:get({V,Face}, VtxNormals),
	    Vn
    end.
	    
face_normals(Ftab, Vtab) ->
    {Ns,_} = mapfoldl(fun(#e3d_face{vs=[A0,B0,C0|_]}, Face) ->
			      A = element(A0+1, Vtab),
			      B = element(B0+1, Vtab),
			      C = element(C0+1, Vtab),
			      {{Face,e3d_vec:normal(A, B, C)},Face+1}
		      end, 0, Ftab),
    gb_trees:from_orddict(Ns).

vtx_to_face_tab(Fs) ->
    vtx_to_face_tab(Fs, 0, []).

vtx_to_face_tab([#e3d_face{vs=Vs}|Fs], Face, Acc0) ->
    Acc = [{V,Face} || V <- Vs] ++ Acc0,
    vtx_to_face_tab(Fs, Face+1, Acc);
vtx_to_face_tab([], Face, Acc) -> Acc.

vertex_normals(Vfs, Vn, FaceNormals) ->
    vertex_normals(Vfs, Vn, FaceNormals, []).

vertex_normals([{V,Fs}|Vfs], Vn, FaceNormals, Acc) ->
    Ns = [gb_trees:get(F, FaceNormals) || F <- Fs],
    N = e3d_vec:norm(e3d_vec:add(Ns)),
    vertex_normals(Vfs, Vn+1, FaceNormals, [{V,{Vn,N}}|Acc]);
vertex_normals([], Vn, FaceNormals, Acc) -> Acc.

vn_hard_normals([], HardVtxFace, Fs, FaceNormals, VtxNormals) -> VtxNormals;
vn_hard_normals(He, HardVtxFace, Fs, FaceNormals, VtxNormals0) ->
    Hard = sofs:from_list(He),
    Edges = sofs:relation(vn_face_edges(Fs, 0, [])),
    Soft0 = sofs:drestriction(Edges, Hard),
    Soft = sofs:relation_to_family(Soft0),
    G = digraph:new(),
    make_digraph_1(G, sofs:to_external(Soft)),

    VtxNormals = vn_hard_normals_1(G, HardVtxFace, FaceNormals,
				   length(VtxNormals0), VtxNormals0),
    digraph:delete(G),
    VtxNormals.

vn_hard_normals_1(G, [VF|VFs], FaceNormals, Vn, Acc) ->
    Reachable = digraph_utils:reachable([VF], G),
    Ns0 = [gb_trees:get(Face, FaceNormals) || {_,Face} <- Reachable],
    N = case Ns0 of
	    [N0] -> N0;
	    Ns -> e3d_vec:mul(e3d_vec:add(Ns), 1/length(Ns))
	end,
    vn_hard_normals_1(G, VFs, FaceNormals, Vn+1, [{VF,{Vn,N}}|Acc]);
vn_hard_normals_1(G, [], FaceNormals, Vn, Acc) -> Acc.
    
make_digraph_1(G, [{{Va,Vb},[Fx,Fy]}|T]) ->
    digraph_add_edge(G, {Va,Fx}, {Va,Fy}),
    digraph_add_edge(G, {Vb,Fx}, {Vb,Fy}),
    make_digraph_1(G, T);
make_digraph_1(G, [_|T]) ->
    make_digraph_1(G, T);
make_digraph_1(G, []) -> ok.

digraph_add_edge(G, Va, Vb) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb),
    digraph:add_edge(G, Vb, Va).

vn_face_edges([#e3d_face{vs=Vs}|Fs], Face, Acc) ->
    vn_face_edges(Fs, Face+1, vn_pairs(Vs, Vs, Face, Acc));
vn_face_edges([], Face, Acc) -> Acc.

vn_pairs([V1|[V2|_]=Vs], More, Face, Acc) ->
    vn_pairs(Vs, More, Face, [{vn_edge_name(V1, V2),Face}|Acc]);
vn_pairs([V1], [V2|_], Face, Acc) ->
    [{vn_edge_name(V1, V2),Face}|Acc].

vn_edge_name(Va, Vb) when Va < Vb -> {Va,Vb};
vn_edge_name(Va, Vb) -> {Vb,Va}.
