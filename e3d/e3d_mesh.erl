%%
%%  e3d_mesh.erl --
%%
%%     Utility functions for E3D meshes, such as cleanup and triangulation.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_mesh.erl,v 1.25 2002/11/05 18:47:37 bjorng Exp $
%%

-module(e3d_mesh).
-export([clean_faces/1,orient_normals/1,transform/1,transform/2,
	 triangulate/1,quadrangulate/1,
	 make_quads/1,vertex_normals/1,renumber/1,partition/1]).
-export([triangulate_face/2,triangulate_face_with_holes/3]).
-export([quadrangulate_face/2,quadrangulate_face_with_holes/3]).

-include("e3d.hrl").
-import(lists, [foreach/2,sort/1,reverse/1,reverse/2,seq/2,
		foldl/3,filter/2,mapfoldl/3,mapfoldr/3,last/1]).

%% orient_normals(Mesh0) -> Mesh
%%  Orient the face normals consistently.
orient_normals(Mesh) ->
    e3d__meshclean:orient_normals(Mesh).

%% clean_faces(Mesh0) -> Mesh
%%  Remove duplicate vertices and faces with fewer than three edges.
clean_faces(Mesh) ->
    e3d__meshclean:clean_faces(Mesh).

%% transform(Mesh0) -> Mesh
%%  Transform all vertices in the mesh by the matrix in the e3d_mesh
%%  record.
%%

transform(#e3d_mesh{matrix=none}=Mesh) -> Mesh;
transform(#e3d_mesh{matrix=Matrix}=Mesh) -> transform(Mesh, Matrix).
    
%% transform(Mesh0, Matrix) -> Mesh
%%  Transform all vertices in the mesh by the matrix.
transform(#e3d_mesh{vs=Vs0}=Mesh, Matrix) ->
    case e3d_mat:is_identity(Matrix) of
	true -> Mesh;
	false ->
	    Vs1 = foldl(fun(P, A) ->
				[e3d_mat:mul_point(Matrix, P)|A]
			end, [], Vs0),
	    Vs = reverse(Vs1),
	    Mesh#e3d_mesh{vs=Vs}
    end.

%% make_quads(Mesh0) -> Mesh
%%  If two adjacent triangles share a hidden edge, combine the
%%  triangles to a quad. Triangles with more than one hidden edge
%%  will never be combined to avoid isolating vertices and/or
%%  creating concave polygons.
make_quads(#e3d_mesh{type=triangle,fs=Fs0}=Mesh) ->
    Ftab0 = number_faces(Fs0),
    Es = rhe_collect_edges(Ftab0),
    Ftab1 = gb_trees:from_orddict(Ftab0),
    Ftab = merge_faces(Es, Ftab1),
    Fs = gb_trees:values(Ftab),
    Mesh#e3d_mesh{type=polygon,fs=Fs};
make_quads(Mesh) -> Mesh.

%%%
%%% Mesh triangulation.
%%%

triangulate(#e3d_mesh{}=Mesh) ->
    e3d__tri_quad:triangulate(Mesh).

triangulate_face(Face, Vcoords) ->
    e3d__tri_quad:triangulate_face(Face, Vcoords).

triangulate_face_with_holes(Face, Holes, Vcoords) ->
    e3d__tri_quad:triangulate_face_with_holes(Face, Holes, Vcoords).

%%%
%%% Mesh quadrangulation.
%%%

quadrangulate(#e3d_mesh{}=Mesh) ->
    e3d__tri_quad:quadrangulate(Mesh).

quadrangulate_face(Face, Vcoords) ->
    e3d__tri_quad:quadrangulate_face(Face, Vcoords).

quadrangulate_face_with_holes(Face, Holes, Vcoords) ->
    e3d__tri_quad:quadrangulate_face_with_holes(Face, Holes, Vcoords).

%% vertex_normals(Mesh0) -> Mesh
%%  Calculate vertex normals for each face.
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
    Normals0 = gb_trees:values(VtxNormals),
    Normals1 = sort(Normals0),
    Normals = [N || {_Vn,N} <- Normals1],
    Mesh#e3d_mesh{fs=Faces,ns=Normals}.

%% renumber(Mesh0) -> Mesh
%%  Removes vertices and UV coordinates that are not referenced
%%  from any faces and renumbers vertices and UV coordinates to
%%  remove the gaps.
renumber(#e3d_mesh{tx=Tx,vs=Vtab,ns=Ns}=Mesh) ->
    {UsedVs,UsedUv,UsedNs} = rn_used_vs(Mesh),
    if
	length(Vtab) =/= length(UsedVs);
	length(Tx) =/= length(UsedUv);
	length(Ns) =/= length(UsedNs) ->
	    renumber_1(Mesh, UsedVs, UsedUv, UsedNs);
	true -> Mesh
    end.

%% partition(Mesh0) -> [Mesh]
%%  Partitions a mesh in disjoint sub-meshes.
partition(#e3d_mesh{fs=Faces0}=Template) ->
    Faces1 = number_faces(Faces0),
    Faces = sofs:relation(Faces1, [{face,data}]),
    FacePart = partition_1(Faces),
    Res = foldl(fun(Fs0, A) ->
			Fs = strip_index(sofs:to_external(Fs0)),
			Mesh = renumber(Template#e3d_mesh{fs=Fs}),
			[Mesh|A]
		end, [], sort(FacePart)),
    reverse(Res).

%%%
%%% End of exported functions. Local functions follow.
%%%

%%%
%%% Help functions for make_quads/1.
%%%

merge_faces([{_Name,L}|Es], Ftab0) ->
    Ftab = case L of
	       [{Fa,Va,Vb,invisible},{Fb,_,_,invisible}] ->
		   merge_faces_1(Fa, Fb, Va, Vb, Ftab0);
	       _Other -> Ftab0
	   end,
    merge_faces(Es, Ftab);
merge_faces([], Ftab) -> Ftab.

merge_faces_1(Fa, Fb, Va, Vb, Ftab0) ->
    case {gb_trees:lookup(Fa, Ftab0),gb_trees:lookup(Fb, Ftab0)} of
	{{value,#e3d_face{vs=Vs1,tx=Tx1,mat=Mat}=Rec0},
	 {value,#e3d_face{vs=Vs2,tx=Tx2,mat=Mat}}} ->
	    case merge_faces_2(Va, Vb, Vs1, Vs2) of
		error -> Ftab0;
		Vs when is_list(Vs) ->
		    Tx = merge_uvs(Vs, Vs1, Vs2, Tx1, Tx2),
		    Rec = Rec0#e3d_face{vs=Vs,tx=Tx,vis=-1},
		    Ftab = gb_trees:update(Fa, Rec, Ftab0),
		    gb_trees:delete(Fb, Ftab)
	    end;
	{_,_} -> Ftab0
    end.

merge_uvs(_, _, _, [], []) -> [];
merge_uvs(Vs, Vs1, Vs2, Tx1, Tx2) ->
    R0 = [zip(Vs1, Tx1),zip(Vs2, Tx2)],
    R1 = sofs:set(R0, [[{v,uv}]]),
    R = sofs:union(R1),
    F0 = sofs:relation_to_family(R),
    F = gb_trees:from_orddict(sofs:to_external(F0)),
    merge_uvs_1(Vs, F).

merge_uvs_1([V|T], V2UV) ->
    [UV|_] = gb_trees:get(V, V2UV),
    [UV|merge_uvs_1(T, V2UV)];
merge_uvs_1([], _) -> [].

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
merge_faces_3(_Va, _Vb, _Vs1, _Vs2) -> error.

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
rot_face(Va, Vb, [Va|_]=Vs0, _Acc) ->
    [Vb|Vs] = reverse(Vs0),
    [Vb|reverse(Vs)];
rot_face(Va, Vb, [Vb|_]=Vs0, _Acc) ->
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

rhe_edges([{Va,Vb,Vis}|Ps], Face, Acc) when Va < Vb ->
    Name = {Va,Vb},
    rhe_edges(Ps, Face, [{Name,{Face,Va,Vb,Vis}}|Acc]);
rhe_edges([{Va,Vb,Vis}|Ps], Face, Acc) ->
    Name = {Vb,Va},
    rhe_edges(Ps, Face, [{Name,{Face,Va,Vb,Vis}}|Acc]);
rhe_edges([], _Face, Acc) -> Acc.

pairs(Vs, Vis) ->
    pairs(Vs, Vs, Vis, []).

pairs([V1|[V2|_]=Vs], More, Vis, Acc) ->
    State = visible(Vis),
    pairs(Vs, More, Vis bsl 1, [{V1,V2,State}|Acc]);
pairs([V1], [V2|_], Vis, Acc) ->
    State = visible(Vis),
    [{V1,V2,State}|Acc].

visible(F) when F band 4 =/= 0 -> visible;
visible(_) -> invisible.

%%%
%%% Help functions for vertex_normals/1.
%%%

vn_faces([#e3d_face{vs=Vs}=E3DFace|Fs], VtxNormals, Face, Acc) ->
    Ns0 = foldl(fun(V, A) ->
			[vn_lookup(V, Face, VtxNormals)|A]
		end, [], Vs),
    Ns = reverse(Ns0),
    vn_faces(Fs, VtxNormals, Face+1, [E3DFace#e3d_face{ns=Ns}|Acc]);
vn_faces([], _VtxNormals, _Face, Acc) -> reverse(Acc).
    
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
			      {{Face,e3d_vec:normal(A, B, C)},Face+1};
			 (_, Face) ->
			      {{Face,e3d_vec:zero()},Face+1}
		      end, 0, Ftab),
    gb_trees:from_orddict(Ns).

vtx_to_face_tab(Fs) ->
    vtx_to_face_tab(Fs, 0, []).

vtx_to_face_tab([#e3d_face{vs=Vs}|Fs], Face, Acc0) ->
    Acc = [{V,Face} || V <- Vs] ++ Acc0,
    vtx_to_face_tab(Fs, Face+1, Acc);
vtx_to_face_tab([], _Face, Acc) -> Acc.

vertex_normals(Vfs, Vn, FaceNormals) ->
    vertex_normals(Vfs, Vn, FaceNormals, []).

vertex_normals([{V,Fs}|Vfs], Vn, FaceNormals, Acc) ->
    Ns = [gb_trees:get(F, FaceNormals) || F <- Fs],
    N = e3d_vec:norm(e3d_vec:add(Ns)),
    vertex_normals(Vfs, Vn+1, FaceNormals, [{V,{Vn,N}}|Acc]);
vertex_normals([], _Vn, _FaceNormals, Acc) -> Acc.

vn_hard_normals([], _HardVtxFace, _Fs, _FaceNormals, VtxNormals) ->
    VtxNormals;
vn_hard_normals(He, HardVtxFace, Fs, FaceNormals, VtxNormals0) ->
    Hard = sofs:set(He),
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
	    Ns -> e3d_vec:norm(e3d_vec:add(Ns))
	end,
    vn_hard_normals_1(G, VFs, FaceNormals, Vn+1, [{VF,{Vn,N}}|Acc]);
vn_hard_normals_1(_G, [], _FaceNormals, _Vn, Acc) -> Acc.
    
make_digraph_1(G, [{{Va,Vb},[Fx,Fy]}|T]) ->
    digraph_add_edge(G, {Va,Fx}, {Va,Fy}),
    digraph_add_edge(G, {Vb,Fx}, {Vb,Fy}),
    make_digraph_1(G, T);
make_digraph_1(G, [_|T]) ->
    make_digraph_1(G, T);
make_digraph_1(_G, []) -> ok.

digraph_add_edge(G, Va, Vb) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb),
    digraph:add_edge(G, Vb, Va).

vn_face_edges([#e3d_face{vs=Vs}|Fs], Face, Acc) ->
    vn_face_edges(Fs, Face+1, vn_pairs(Vs, Vs, Face, Acc));
vn_face_edges([], _Face, Acc) -> Acc.

vn_pairs([V1|[V2|_]=Vs], More, Face, Acc) ->
    vn_pairs(Vs, More, Face, [{vn_edge_name(V1, V2),Face}|Acc]);
vn_pairs([V1], [V2|_], Face, Acc) ->
    [{vn_edge_name(V1, V2),Face}|Acc].

vn_edge_name(Va, Vb) when Va < Vb -> {Va,Vb};
vn_edge_name(Va, Vb) -> {Vb,Va}.

%%%
%%% Help functions for renumber/1.
%%%

renumber_1(#e3d_mesh{fs=Ftab0,vs=Vs0,tx=Tx0,ns=Ns0,he=He0}=Mesh,
	   UsedVs, UsedUV, UsedNs) ->
    VsMap = rn_make_map(UsedVs, 0, []),
    UVMap = rn_make_map(UsedUV, 0, []),
    NsMap = rn_make_map(UsedNs, 0, []),
    Ftab = renumber_ftab(Ftab0, VsMap, UVMap, NsMap, []),
    He = renumber_hard_edges(He0, VsMap, []),
    Vs = rn_remove_unused(Vs0, VsMap),
    Tx = rn_remove_unused(Tx0, UVMap),
    Ns = rn_remove_unused(Ns0, NsMap),
    Mesh#e3d_mesh{fs=Ftab,vs=Vs,tx=Tx,ns=Ns,he=He}.

renumber_ftab([#e3d_face{vs=Vs0,tx=Tx0,ns=Ns0}=Rec|Fs],
	      VsMap, UVMap, NsMap, Acc) ->
    Vs = [map_vtx(V, VsMap) || V <- Vs0],
    Tx = [map_vtx(V, UVMap) || V <- Tx0],
    Ns = [map_vtx(V, NsMap) || V <- Ns0],
    renumber_ftab(Fs, VsMap, UVMap, NsMap,
		  [Rec#e3d_face{vs=Vs,tx=Tx,ns=Ns}|Acc]);
renumber_ftab([], _, _, _, Acc) -> reverse(Acc).

renumber_hard_edges([{Va0,Vb0}|T], VsMap, Acc) ->
    Va = map_vtx(Va0, VsMap),
    case map_vtx(Vb0, VsMap) of
	Vb when Va < Vb ->
	    renumber_hard_edges(T, VsMap, [{Va,Vb}|Acc]);
	Vb ->
	    renumber_hard_edges(T, VsMap, [{Vb,Va}|Acc])
    end;
renumber_hard_edges([], _, Acc) -> reverse(Acc).

map_vtx(V, {map,Low,_}) -> V-Low;
map_vtx(V, Map) -> gb_trees:get(V, Map).

rn_remove_unused(Vs, {map,Low,N}) ->
    lists:sublist(Vs, Low+1, N);
rn_remove_unused(Vs, Map) ->
    rn_remove_unused(Vs, Map, 0, []).

rn_remove_unused([V|Vs], Map, I, Acc) ->
    case gb_trees:is_defined(I, Map) of
	true -> rn_remove_unused(Vs, Map, I+1, [V|Acc]);
	false -> rn_remove_unused(Vs, Map, I+1, Acc)
    end;
rn_remove_unused([], _, _, Acc) -> reverse(Acc).

rn_used_vs(#e3d_mesh{fs=Ftab}) ->
    Vs = foldl(fun(#e3d_face{vs=Vs}, A) -> Vs++A end, [], Ftab),
    UV = foldl(fun(#e3d_face{tx=Tx}, A) -> Tx++A end, [], Ftab),
    Ns = foldl(fun(#e3d_face{ns=Ns}, A) -> Ns++A end, [], Ftab),
    {ordsets:from_list(Vs),ordsets:from_list(UV),ordsets:from_list(Ns)}.

rn_make_map([V], I, Acc0) ->
    [{Low,_}|_] = Acc = reverse(Acc0, [{V,I}]),
    High = V+1,
    case High-Low of
	Range when Range =:= length(Acc) -> {map,Low,Range};
	_Range -> gb_trees:from_orddict(Acc)
    end;
rn_make_map([V|Vs], I, Acc) ->
    rn_make_map(Vs, I+1, [{V,I}|Acc]);
rn_make_map([], _, []) -> gb_trees:empty().

%%%
%%% Help functions for partition/1.
%%%

partition_1(Faces) ->
    E2F = par_pairs(sofs:to_external(Faces), []),
    R = sofs:relation(E2F, [{edge,face}]),
    F0 = sofs:relation_to_family(R),
    CR = sofs:canonical_relation(sofs:range(F0)),
    F1 = sofs:relation_to_family(CR),
    F = sofs:family_union(F1),
    G = sofs:family_to_digraph(F),
    Cs = digraph_utils:strong_components(G),
    digraph:delete(G),
    foldl(fun(C, A) ->
		  Part = sofs:set(C, [face]),
		  [sofs:restriction(Faces, Part)|A]
	  end, [], Cs).

par_pairs([{Face,#e3d_face{vs=Vs}}|Fs], Acc) ->
    par_pairs(Fs, par_pairs_1(Vs, Vs, Face, Acc));
par_pairs([], Acc) -> Acc.

par_pairs_1([V1|[V2|_]=Vs], More, Face, Acc) ->
    par_pairs_1(Vs, More, Face, [{par_edge_name(V1, V2),Face}|Acc]);
par_pairs_1([V1], [V2|_], Face, Acc) ->
    [{par_edge_name(V1, V2),Face}|Acc].

par_edge_name(Va, Vb) when Va < Vb -> [Va|Vb];
par_edge_name(Va, Vb) -> [Vb|Va].

strip_index(Fs) ->
    strip_index(Fs, []).
strip_index([{_,Data}|T], Acc) ->
    strip_index(T, [Data|Acc]);
strip_index([], Acc) -> reverse(Acc).

%%%
%%% Common help functions.
%%%

number_faces(Fs) ->
    number_faces(Fs, 0, []).
number_faces([F|Fs], Face, Acc) ->
    number_faces(Fs, Face+1, [{Face,F}|Acc]);
number_faces([], _Face, Acc) -> reverse(Acc).

zip([V|Vs], [UV|UVs]) ->
    [{V,UV}|zip(Vs, UVs)];
zip([], []) -> [].
