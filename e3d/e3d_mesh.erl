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
%%     $Id: e3d_mesh.erl,v 1.15 2002/02/02 07:11:08 bjorng Exp $
%%

-module(e3d_mesh).
-export([clean/1,triangulate/1,quadrangulate/1,
	 make_quads/1,vertex_normals/1]).
-export([triangulate_face/2,triangulate_face_with_holes/3]).
-export([quadrangulate_face/2,quadrangulate_face_with_holes/3]).

-include("e3d.hrl").
-import(lists, [foreach/2,sort/1,reverse/1,reverse/2,seq/2,
		foldl/3,filter/2,mapfoldl/3,mapfoldr/3,last/1]).

clean(Mesh) ->
    e3d__meshclean:clean(Mesh).

number_faces(Fs) ->
    number_faces(Fs, 0, []).
number_faces([F|Fs], Face, Acc) ->
    number_faces(Fs, Face+1, [{Face,F}|Acc]);
number_faces([], Face, Acc) -> reverse(Acc).

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

%%%
%%% If two adjacent triangles share a hidden edge, combine the
%%% triangles to a quad. Triangles with more than one hidden edge
%%% will never be combined to avoid isolating vertices and/or
%%% creating concave polygons.
%%%

make_quads(#e3d_mesh{type=triangle,fs=Fs0}=Mesh) ->
    Ftab0 = number_faces(Fs0),
    Es = rhe_collect_edges(Ftab0),
    Ftab1 = gb_trees:from_orddict(Ftab0),
    Ftab = merge_faces(Es, Ftab1),
    Fs = gb_trees:values(Ftab),
    Mesh#e3d_mesh{type=polygon,fs=Fs};
make_quads(Mesh) -> Mesh.

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
    Normals0 = gb_trees:values(VtxNormals),
    Normals1 = sort(Normals0),
    Normals = [N || {Vn,N} <- Normals1],
    {Faces,Normals}.

vn_faces([#e3d_face{mat=Mat,vs=Vs0,tx=Tx}|Fs], VtxNormals, Face, Acc) ->
    Vs1 = foldl(fun(V, A) ->
			vn_face(V, VtxNormals, Face, A)
		end, [], Vs0),
    Vs2 = reverse(Vs1),
    Vs = add_uv(Vs2, Tx),
    vn_faces(Fs, VtxNormals, Face+1, [{Mat,Vs}|Acc]);
vn_faces([], VtxNormals, Face, Acc) -> reverse(Acc).

add_uv(Vs, []) -> Vs;
add_uv([{V,N}|Vs], [UV|UVs]) ->
    [{V,N,UV}|add_uv(Vs, UVs)];
add_uv([], []) -> [].

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
