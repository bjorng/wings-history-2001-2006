%%
%%  wings__du.erl --
%%
%%     Low-level drawing utilities.
%%
%%  Copyright (c) 2003-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings__du.erl,v 1.13 2004/04/12 11:08:52 bjorng Exp $
%%

-module(wings__du).
-export([plain_face/1,plain_face/2,uv_face/2,uv_face/3,vcol_face/2,vcol_face/3,
	 smooth_plain_face/2,smooth_plain_face/3,
	 smooth_uv_face/2,smooth_uv_face/3,
	 smooth_vcol_face/2,smooth_vcol_face/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d.hrl").

-import(lists, [reverse/1,seq/2]).

%% plain_face([Position]) -> ok
%%  Draw a tri or quad face with neither UV coordinates nor vertex colors.
plain_face([A,B,C]) ->
    gl:vertex3fv(A),
    gl:vertex3fv(B),
    gl:vertex3fv(C);
plain_face([A,B,C,D]) ->
    gl:vertex3fv(A),
    gl:vertex3fv(B),
    gl:vertex3fv(C),
    gl:vertex3fv(A),
    gl:vertex3fv(C),
    gl:vertex3fv(D).

%% plain_face(FaceNormal, [Position]) -> ok
%%  Draw a face with neither UV coordinates nor vertex colors.
plain_face(_, [A,B,C,D]) ->
    gl:vertex3fv(A),
    gl:vertex3fv(B),
    gl:vertex3fv(D),
    gl:vertex3fv(B),
    gl:vertex3fv(C),
    gl:vertex3fv(D);
plain_face(N, VsPos) ->
    Vs = seq(0, length(VsPos)-1),
    Fs = e3d_mesh:triangulate_face(#e3d_face{vs=Vs}, N, VsPos),
    plain_face_1(Fs, list_to_tuple(VsPos)).

plain_face_1([#e3d_face{vs=[A0,B0,C0]}|Fs], Vtab) ->
    A = A0+1, B = B0+1, C = C0+1,
    gl:vertex3fv(element(A, Vtab)),
    gl:vertex3fv(element(B, Vtab)),
    gl:vertex3fv(element(C, Vtab)),
    plain_face_1(Fs, Vtab);
plain_face_1([_|Fs], Vtab) ->
    plain_face_1(Fs, Vtab);
plain_face_1([], _) -> ok.

%% uv_face([Position], [UV]) -> ok
%%  Draw a tri or quad with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face([A,B,C], [UVa,UVb,UVc]) ->
    uv_face_vtx(A, UVa),
    uv_face_vtx(B, UVb),
    uv_face_vtx(C, UVc);
uv_face([A,B,C,D], [UVa,UVb,UVc,UVd]) ->
    uv_face_vtx(A, UVa),
    uv_face_vtx(B, UVb),
    uv_face_vtx(C, UVc),
    gl:vertex3fv(C),
    uv_face_vtx(D, UVd),
    uv_face_vtx(A, UVa).

%% uv_face(FaceNormal, [Position|UV]) -> ok
%%  Draw a face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face(_, [A,B,C,D], [UVa,UVb,UVc,UVd]) ->
    uv_face_vtx(A, UVa),
    uv_face_vtx(B, UVb),
    uv_face_vtx(D, UVd),
    gl:vertex3fv(D),
    uv_face_vtx(B, UVb),
    uv_face_vtx(C, UVc);
uv_face(N, VsPos, UVs) ->
    Vs = seq(0, length(VsPos)-1),
    Fs = e3d_mesh:triangulate_face(#e3d_face{vs=Vs}, N, VsPos),
    uv_face_1(Fs, list_to_tuple(VsPos), list_to_tuple(UVs)).

uv_face_1([#e3d_face{vs=[A0,B0,C0]}|Fs], Vtab, UVtab) ->
    A = A0+1, B = B0+1, C = C0+1,
    uv_face_vtx(element(A, Vtab), element(A, UVtab)),
    uv_face_vtx(element(B, Vtab), element(B, UVtab)),
    uv_face_vtx(element(C, Vtab), element(C, UVtab)),
    uv_face_1(Fs, Vtab, UVtab);
uv_face_1([_|Fs], Vtab, UVtab) ->
    uv_face_1(Fs, Vtab, UVtab);
uv_face_1([], _, _) -> ok.

uv_face_vtx(Pos, {U,V}) ->
    gl:texCoord2f(U, V),
    gl:vertex3fv(Pos);
uv_face_vtx(Pos, _) ->
    gl:texCoord2i(0, 0),
    gl:vertex3fv(Pos).

%% vcol_face([Position], [Color]) -> ok
%%  Draw a tri or quad with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face([A,B,C], [Ca,Cb,Cc]) ->
    vcol_face_vtx(A, Ca),
    vcol_face_vtx(B, Cb),
    vcol_face_vtx(C, Cc);
vcol_face([A,B,C,D], [Ca,Cb,Cc,Cd]) ->
    vcol_face_vtx(A, Ca),
    vcol_face_vtx(B, Cb),
    vcol_face_vtx(C, Cc),
    gl:vertex3fv(C),
    vcol_face_vtx(D, Cd),
    vcol_face_vtx(A, Ca).

%% vcol_face(FaceNormal, [Position|Color]) -> ok
%%  Draw a face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face(_, [A,B,C,D], [Ca,Cb,Cc,Cd]) ->
    vcol_face_vtx(A, Ca),
    vcol_face_vtx(B, Cb),
    vcol_face_vtx(D, Cd),
    gl:vertex3fv(D),
    vcol_face_vtx(B, Cb),
    vcol_face_vtx(C, Cc);
vcol_face(N, VsPos, Cols) ->
    Vs = seq(0, length(VsPos)-1),
    Fs = e3d_mesh:triangulate_face(#e3d_face{vs=Vs}, N, VsPos),
    vcol_face_1(Fs, list_to_tuple(Cols), list_to_tuple(VsPos)).

vcol_face_1([#e3d_face{vs=[A0,B0,C0]}|Fs], Ctab, Vtab) ->
    A = A0+1, B = B0+1, C = C0+1,
    vcol_face_vtx(element(A, Vtab), element(A, Ctab)),
    vcol_face_vtx(element(B, Vtab), element(B, Ctab)),
    vcol_face_vtx(element(C, Vtab), element(C, Ctab)),
    vcol_face_1(Fs, Ctab, Vtab);
vcol_face_1([_|Fs], Ctab, Vtab) ->
    vcol_face_1(Fs, Ctab, Vtab);
vcol_face_1([], _, _) -> ok.

vcol_face_vtx(Pos, {R,G,B}) ->
    gl:color3f(R, G, B),
    gl:vertex3fv(Pos);
vcol_face_vtx(Pos, _) ->
    gl:color3f(1, 1, 1),
    gl:vertex3fv(Pos).

%%%
%%% Drawing of faces with smooth normals.
%%%

%% smooth_plain_face([{Position,_,VertexNormal}]) -> ok
%%  Draw a smooth tri or quad with neither UV coordinates nor vertex colors.
smooth_plain_face([A,B,C], [Na,Nb,Nc]) ->
    smooth_plain_face_vtx(A, Na),
    smooth_plain_face_vtx(B, Nb),
    smooth_plain_face_vtx(C, Nc);
smooth_plain_face([A,B,C,D], [Na,Nb,Nc,Nd]) ->
    smooth_plain_face_vtx(A, Na),
    smooth_plain_face_vtx(B, Nb),
    smooth_plain_face_vtx(C, Nc),
    gl:vertex3fv(C),
    smooth_plain_face_vtx(D, Nd),
    smooth_plain_face_vtx(A, Na).

%% smooth_plain_face(FaceNormal, [Position], [[_|VertexNormal]]) -> ok
%%  Draw a smooth face with neither UV coordinates nor vertex colors.
smooth_plain_face(_, [A,B,C,D], [Na,Nb,Nc,Nd]) ->
    smooth_plain_face_vtx(A, Na),
    smooth_plain_face_vtx(B, Nb),
    smooth_plain_face_vtx(D, Nd),
    gl:vertex3fv(D),
    smooth_plain_face_vtx(B, Nb),
    smooth_plain_face_vtx(C, Nc);
smooth_plain_face(N, VsPos, Ns) ->
    Vs = seq(0, length(VsPos)-1),
    Fs = e3d_mesh:triangulate_face(#e3d_face{vs=Vs}, N, VsPos),
    smooth_plain_face_1(Fs, list_to_tuple(VsPos), list_to_tuple(Ns)).

smooth_plain_face_1([#e3d_face{vs=[A0,B0,C0]}|Fs], Vtab, Ntab) ->
    A = A0+1, B = B0+1, C = C0+1,
    smooth_plain_face_vtx(element(A, Vtab), element(A, Ntab)),
    smooth_plain_face_vtx(element(B, Vtab), element(B, Ntab)),
    smooth_plain_face_vtx(element(C, Vtab), element(C, Ntab)),
    smooth_plain_face_1(Fs, Vtab, Ntab);
smooth_plain_face_1([_|Fs], Vtab, Ntab) ->
    smooth_plain_face_1(Fs, Vtab, Ntab);
smooth_plain_face_1([], _, _) -> ok.

smooth_plain_face_vtx(P, [_|N]) ->
    gl:normal3fv(N),
    gl:vertex3fv(P).

%% smooth_uv_face([{Position,UV,VertexNormal}]) -> ok
%%  Draw a smoth tri or quad with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face([A,B,C], [Ai,Bi,Ci]) ->
    smooth_uv_face_vtx(A, Ai),
    smooth_uv_face_vtx(B, Bi),
    smooth_uv_face_vtx(C, Ci);
smooth_uv_face([A,B,C,D], [Ai,Bi,Ci,Di]) ->
    smooth_uv_face_vtx(A, Ai),
    smooth_uv_face_vtx(B, Bi),
    smooth_uv_face_vtx(C, Ci),
    gl:vertex3fv(C),
    smooth_uv_face_vtx(D, Di),
    smooth_uv_face_vtx(A, Ai).

%% smooth_uv_face(FaceNormal, [Position], [[UV|VertexNormal]]) -> ok
%%  Draw a smoth face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face(_, [A,B,C,D], [UVa,UVb,UVc,UVd]) ->
    smooth_uv_face_vtx(A, UVa),
    smooth_uv_face_vtx(B, UVb),
    smooth_uv_face_vtx(D, UVd),
    gl:vertex3fv(D),
    smooth_uv_face_vtx(B, UVb),
    smooth_uv_face_vtx(C, UVc);
smooth_uv_face(N, VsPos, UVs) ->
    Vs = seq(0, length(VsPos)-1),
    Fs = e3d_mesh:triangulate_face(#e3d_face{vs=Vs}, N, VsPos),
    smooth_uv_face_1(Fs, list_to_tuple(VsPos), list_to_tuple(UVs)).

smooth_uv_face_1([#e3d_face{vs=[A0,B0,C0]}|Fs], Vtab, UVtab) ->
    A = A0+1, B = B0+1, C = C0+1,
    smooth_uv_face_vtx(element(A, Vtab), element(A, UVtab)),
    smooth_uv_face_vtx(element(B, Vtab), element(B, UVtab)),
    smooth_uv_face_vtx(element(C, Vtab), element(C, UVtab)),
    smooth_uv_face_1(Fs, Vtab, UVtab);
smooth_uv_face_1([_|Fs], Vtab, UVtab) ->
    smooth_uv_face_1(Fs, Vtab, UVtab);
smooth_uv_face_1([], _, _) -> ok.

smooth_uv_face_vtx(P, [{U,V}|N]) ->
    gl:texCoord2f(U, V),
    gl:normal3fv(N),
    gl:vertex3fv(P);
smooth_uv_face_vtx(P, [_|N]) ->
    gl:texCoord2i(0, 0),
    gl:normal3fv(N),
    gl:vertex3fv(P).

%% smooth_vcol_face([Position], [[UV|VertexNormal]]) -> ok
%%  Draw a smooth tri or quad with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face([A,B,C], [Ai,Bi,Ci]) ->
    smooth_vcol_face_vtx(A, Ai),
    smooth_vcol_face_vtx(B, Bi),
    smooth_vcol_face_vtx(C, Ci);
smooth_vcol_face([A,B,C,D], [Ai,Bi,Ci,Di]) ->
    smooth_vcol_face_vtx(A, Ai),
    smooth_vcol_face_vtx(B, Bi),
    smooth_vcol_face_vtx(C, Ci),
    gl:vertex3fv(C),
    smooth_vcol_face_vtx(D, Di),
    smooth_vcol_face_vtx(A, Ai).

%% smooth_vcol_face(FaceNormal, [Position], [[Color|VertexNormal]]) -> ok
%%  Draw a smooth face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face(_, [A,B,C,D], [Ca,Cb,Cc,Cd]) ->
    smooth_vcol_face_vtx(A, Ca),
    smooth_vcol_face_vtx(B, Cb),
    smooth_vcol_face_vtx(D, Cd),
    gl:vertex3fv(D),
    smooth_vcol_face_vtx(B, Cb),
    smooth_vcol_face_vtx(C, Cc);
smooth_vcol_face(N, VsPos, Cols) ->
    Vs = seq(0, length(VsPos)-1),
    Fs = e3d_mesh:triangulate_face(#e3d_face{vs=Vs}, N, VsPos),
    smooth_vcol_face_1(Fs, list_to_tuple(VsPos), list_to_tuple(Cols)).

smooth_vcol_face_1([#e3d_face{vs=[A0,B0,C0]}|Fs], Vtab, Ctab) ->
    A = A0+1, B = B0+1, C = C0+1,
    smooth_vcol_face_vtx(element(A, Vtab), element(A, Ctab)),
    smooth_vcol_face_vtx(element(B, Vtab), element(B, Ctab)),
    smooth_vcol_face_vtx(element(C, Vtab), element(C, Ctab)),
    smooth_vcol_face_1(Fs, Vtab, Ctab);
smooth_vcol_face_1([_|Fs], Vtab, Ctab) ->
    smooth_vcol_face_1(Fs, Vtab, Ctab);
smooth_vcol_face_1([], _, _) -> ok.

smooth_vcol_face_vtx(P, [{R,G,B}|N]) ->
    gl:color3f(R, G, B),
    gl:normal3fv(N),
    gl:vertex3fv(P);
smooth_vcol_face_vtx(P, [_|N]) ->
    gl:color3f(1, 1, 1),
    gl:normal3fv(N),
    gl:vertex3fv(P).
