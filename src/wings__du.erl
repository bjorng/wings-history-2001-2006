%%
%%  wings__du.erl --
%%
%%     Low-level drawing utilities.
%%
%%  Copyright (c) 2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings__du.erl,v 1.8 2003/12/08 19:15:45 bjorng Exp $
%%

-module(wings__du).
-export([init_cb/1,begin_end/2,
	 plain_face/1,plain_face/2,uv_face/2,uv_face/3,vcol_face/2,vcol_face/3,
	 smooth_plain_face/2,smooth_plain_face/3,
	 smooth_uv_face/2,smooth_uv_face/3,
	 smooth_vcol_face/2,smooth_vcol_face/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(GL__BEGIN(Type), ok).
-define(GL__END(Op), Op).

init_cb(Tess) ->
    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_NONE),
    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_NONE).

begin_end(Type, Body) ->
    gl:'begin'(Type),
    Res = Body(),
    gl:'end'(),
    gl:edgeFlag(?GL_TRUE),
    Res.

%% plain_face([Position]) -> ok
%%  Draw a tri or quad face with neither UV coordinates nor vertex colors.
plain_face([A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    gl:vertex3dv(A),
    gl:vertex3dv(B),
    ?GL__END(gl:vertex3dv(C));
plain_face([A,B,C,D]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    gl:vertex3dv(A),
    gl:vertex3dv(B),
    ?GL__END(gl:vertex3dv(C)),
    ?GL__BEGIN(?GL_TRIANGLES),
    gl:vertex3dv(A),
    gl:vertex3dv(C),
    ?GL__END(gl:vertex3dv(D)).

%% plain_face(FaceNormal, [Position]) -> ok
%%  Draw a face with neither UV coordinates nor vertex colors.
plain_face(N, VsPos) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    plain_face_1(Tess, VsPos).

plain_face_1(Tess, [P|T]) ->
    glu:tessVertex(Tess, P),
    plain_face_1(Tess, T);
plain_face_1(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%% uv_face([Position], [UV]) -> ok
%%  Draw a tri or quad with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face([A,B,C], [UVa,UVb,UVc]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    uv_face_vtx(A, UVa),
    uv_face_vtx(B, UVb),
    ?GL__END(uv_face_vtx(C, UVc));
uv_face([A,B,C,D], [UVa,UVb,UVc,UVd]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    uv_face_vtx(A, UVa),
    uv_face_vtx(B, UVb),
    ?GL__END(uv_face_vtx(C, UVc)),
    ?GL__BEGIN(?GL_TRIANGLES),
    uv_face_vtx(A, UVa),
    uv_face_vtx(C, UVc),
    ?GL__END(uv_face_vtx(D, UVd)).

uv_face_vtx(Pos, {U,V}) ->
    gl:texCoord2f(U, V),
    gl:vertex3dv(Pos);
uv_face_vtx(Pos, _) ->
    gl:texCoord2i(0, 0),
    gl:vertex3dv(Pos).

%% uv_face(FaceNormal, [Position|UV]) -> ok
%%  Draw a face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face(N, VsPos, UVs) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    uv_face_1(Tess, VsPos, UVs).

uv_face_1(Tess, [P|Ps], [{_,_}=UV|UVs]) ->
    glu:tessVertex(Tess, P, [{texcoord2,UV}]),
    uv_face_1(Tess, Ps, UVs);
uv_face_1(Tess, [P|Ps], [_|UVs]) ->
    glu:tessVertex(Tess, P, [{texcoord2,{0.0,0.0}}]),
    uv_face_1(Tess, Ps, UVs);
uv_face_1(Tess, [], []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%% vcol_face([Position], [Color]) -> ok
%%  Draw a tri or quad with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face([A,B,C], [Ca,Cb,Cc]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    vcol_face_vtx(A, Ca),
    vcol_face_vtx(B, Cb),
    ?GL__END(vcol_face_vtx(C, Cc));
vcol_face([A,B,C,D], [Ca,Cb,Cc,Cd]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    vcol_face_vtx(A, Ca),
    vcol_face_vtx(B, Cb),
    ?GL__END(vcol_face_vtx(C, Cc)),
    ?GL__BEGIN(?GL_TRIANGLES),
    vcol_face_vtx(A, Ca),
    vcol_face_vtx(C, Cc),
    ?GL__END(vcol_face_vtx(D, Cd)).

vcol_face_vtx(Pos, {R,G,B}) ->
    gl:color3f(R, G, B),
    gl:vertex3dv(Pos);
vcol_face_vtx(Pos, _) ->
    gl:color3f(1, 1, 1),
    gl:vertex3dv(Pos).

%% vcol_face(FaceNormal, [Position|Color]) -> ok
%%  Draw a face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face(N, VsPos, Cols) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    vcol_face_1(Tess, VsPos, Cols).

vcol_face_1(Tess, [P|Ps], [{_,_,_}=Col|Cols]) ->
    glu:tessVertex(Tess, P, [{color,Col}]),
    vcol_face_1(Tess, Ps, Cols);
vcol_face_1(Tess, [P|Ps], [_|Cols]) ->
    glu:tessVertex(Tess, P, [{color,{1.0,1.0,1.0}}]),
    vcol_face_1(Tess, Ps, Cols);
vcol_face_1(Tess, [], []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%%%
%%% Drawing of faces with smooth normals.
%%%

%% smooth_plain_face([{Position,_,VertexNormal}]) -> ok
%%  Draw a smooth tri or quad with neither UV coordinates nor vertex colors.
smooth_plain_face([A,B,C], [Na,Nb,Nc]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_plain_face_vtx(A, Na),
    smooth_plain_face_vtx(B, Nb),
    ?GL__END(smooth_plain_face_vtx(C, Nc));
smooth_plain_face([A,B,C,D], [Na,Nb,Nc,Nd]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_plain_face_vtx(A, Na),
    smooth_plain_face_vtx(B, Nb),
    ?GL__END(smooth_plain_face_vtx(C, Nc)),
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_plain_face_vtx(A, Na),
    smooth_plain_face_vtx(C, Nc),
    ?GL__END(smooth_plain_face_vtx(D, Nd)).

smooth_plain_face_vtx(P, [_|N]) ->
    gl:normal3fv(N),
    gl:vertex3dv(P).

%% smooth_plain_face(FaceNormal, [Position], [[_|VertexNormal]]) -> ok
%%  Draw a smooth face with neither UV coordinates nor vertex colors.
smooth_plain_face(N, VsPos, Ns) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_plain_face_1(Tess, VsPos, Ns).

smooth_plain_face_1(Tess, [P|Ps], [[_|N]|Ns]) ->
    glu:tessVertex(Tess, P, [{normal,N}]),
    smooth_plain_face_1(Tess, Ps, Ns);
smooth_plain_face_1(Tess, [], []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%% smooth_uv_face([{Position,UV,VertexNormal}]) -> ok
%%  Draw a smoth tri or quad with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face([A,B,C], [Ai,Bi,Ci]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_uv_face_vtx(A, Ai),
    smooth_uv_face_vtx(B, Bi),
    ?GL__END(smooth_uv_face_vtx(C, Ci));
smooth_uv_face([A,B,C,D], [Ai,Bi,Ci,Di]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_uv_face_vtx(A, Ai),
    smooth_uv_face_vtx(B, Bi),
    ?GL__END(smooth_uv_face_vtx(C, Ci)),
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_uv_face_vtx(A, Ai),
    smooth_uv_face_vtx(C, Ci),
    ?GL__END(smooth_uv_face_vtx(D, Di)).

smooth_uv_face_vtx(P, [{U,V}|N]) ->
    gl:texCoord2f(U, V),
    gl:normal3fv(N),
    gl:vertex3dv(P);
smooth_uv_face_vtx(P, [_|N]) ->
    gl:texCoord2i(0, 0),
    gl:normal3fv(N),
    gl:vertex3dv(P).

%% smooth_uv_face(FaceNormal, [Position], [[UV|VertexNormal]]) -> ok
%%  Draw a smoth face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face(N, VsPos, Info) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_uv_face_1(Tess, VsPos, Info).

smooth_uv_face_1(Tess, [P|Ps], [[{_,_}=UV|N]|Info]) ->
    glu:tessVertex(Tess, P, [{normal,N},{texcoord2,UV}]),
    smooth_uv_face_1(Tess, Ps, Info);
smooth_uv_face_1(Tess, [P|Ps], [[_|N]|Info]) ->
    glu:tessVertex(Tess, P, [{normal,N},{texcoord2,{0.0,0.0}}]),
    smooth_uv_face_1(Tess, Ps, Info);
smooth_uv_face_1(Tess, [], []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%% smooth_vcol_face([Position], [[UV|VertexNormal]]) -> ok
%%  Draw a smooth tri or quad with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face([A,B,C], [Ai,Bi,Ci]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_vcol_face_vtx(A, Ai),
    smooth_vcol_face_vtx(B, Bi),
    ?GL__END(smooth_vcol_face_vtx(C, Ci));
smooth_vcol_face([A,B,C,D], [Ai,Bi,Ci,Di]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_vcol_face_vtx(A, Ai),
    smooth_vcol_face_vtx(B, Bi),
    ?GL__END(smooth_vcol_face_vtx(C, Ci)),
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_vcol_face_vtx(A, Ai),
    smooth_vcol_face_vtx(C, Ci),
    ?GL__END(smooth_vcol_face_vtx(D, Di)).

smooth_vcol_face_vtx(P, [{R,G,B}|N]) ->
    gl:color3f(R, G, B),
    gl:normal3fv(N),
    gl:vertex3dv(P);
smooth_vcol_face_vtx(P, [_|N]) ->
    gl:color3f(1, 1, 1),
    gl:normal3fv(N),
    gl:vertex3dv(P).

%% smooth_vcol_face(FaceNormal, [Position], [[Color|VertexNormal]]) -> ok
%%  Draw a smooth face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face(N, VsPos, Cols) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_vcol_face_1(Tess, VsPos, Cols).

smooth_vcol_face_1(Tess, [P|Ps], [[{_,_,_}=C|N]|Cols]) ->
    glu:tessVertex(Tess, P, [{color,C},{normal,N}]),
    smooth_vcol_face_1(Tess, Ps, Cols);
smooth_vcol_face_1(Tess, [P|Ps], [[_|N]|Cols]) ->
    glu:tessVertex(Tess, P, [{color,{1.0,1.0,1.0}},{normal,N}]),
    smooth_vcol_face_1(Tess, Ps, Cols);
smooth_vcol_face_1(Tess, [], []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).
