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
%%     $Id: wings__du.erl,v 1.5 2003/08/27 09:23:58 bjorng Exp $
%%

-module(wings__du).
-export([is_quirky_loaded/0,init_cb/1,begin_end/2,
	 plain_face/1,plain_face/2,uv_face/1,uv_face/2,vcol_face/1,vcol_face/2,
	 smooth_plain_face/1,smooth_plain_face/2,
	 smooth_uv_face/1,smooth_uv_face/2,
	 smooth_vcol_face/1,smooth_vcol_face/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

%%%
%%% Some graphics cards such as the Nvdia's Riva TNT have quirks
%%% when one gl:begin/gl:end pair is used for drawing many triangles
%%% (when edge flags are used). Enclosing each triangle in its own
%%% gl:begin/gl:end pair avoids the problem.
%%%
%%% This module will be compiled in two versions to avoid run-time
%%% tests for whether the workaround should be used or not.
%%%

-ifdef(QUIRKY_OPENGL).
-define(GL__BEGIN(Type), gl:'begin'(Type)).
-define(GL__END(Op), Op, gl:'end'()).
-else.
-define(GL__BEGIN(Type), ok).
-define(GL__END(Op), Op).
-endif.

-ifdef(QUIRKY_OPENGL).
is_quirky_loaded() -> true.
-else.
is_quirky_loaded() -> false.
-endif.

-ifdef(QUIRKY_OPENGL).
init_cb(Tess) ->
    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_GLBEGIN),
    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_GLEND).
-else.
init_cb(Tess) ->
    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_NONE),
    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_NONE).
-endif.

-ifdef(QUIRKY_OPENGL).
begin_end(_, Body) ->
    Res = Body(),
    gl:edgeFlag(?GL_TRUE),
    Res.
-else.
begin_end(Type, Body) ->
    gl:'begin'(Type),
    Res = Body(),
    gl:'end'(),
    gl:edgeFlag(?GL_TRUE),
    Res.
-endif.

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
    gl:edgeFlag(?GL_FALSE),
    ?GL__END(gl:vertex3dv(C)),
    ?GL__BEGIN(?GL_TRIANGLES),
    gl:vertex3dv(A),
    gl:edgeFlag(?GL_TRUE),
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

%% uv_face([Position|UV]) -> ok
%%  Draw a tri or quad with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face([A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    uv_face_vtx(A),
    uv_face_vtx(B),
    ?GL__END(uv_face_vtx(C));
uv_face([A,B,C,D]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    uv_face_vtx(A),
    uv_face_vtx(B),
    gl:edgeFlag(?GL_FALSE),
    ?GL__END(uv_face_vtx(C)),
    ?GL__BEGIN(?GL_TRIANGLES),
    uv_face_vtx(A),
    gl:edgeFlag(?GL_TRUE),
    uv_face_vtx(C),
    ?GL__END(uv_face_vtx(D)).

uv_face_vtx([Pos|{U,V}]) ->
    gl:texCoord2f(U, V),
    gl:vertex3dv(Pos);
uv_face_vtx([Pos|_]) ->
    gl:texCoord2i(0, 0),
    gl:vertex3dv(Pos).

%% uv_face(FaceNormal, [Position|UV]) -> ok
%%  Draw a face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face(N, Vs) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    uv_face_1(Tess, Vs).

uv_face_1(Tess, [[Pos|{_,_}=UV]|T]) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,UV}]),
    uv_face_1(Tess, T);
uv_face_1(Tess, [[Pos|_]|T]) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,{0.0,0.0}}]),
    uv_face_1(Tess, T);
uv_face_1(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%% vcol_face([Position|Color]) -> ok
%%  Draw a tri or quad with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face([A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    vcol_face_vtx(A),
    vcol_face_vtx(B),
    ?GL__END(vcol_face_vtx(C));
vcol_face([A,B,C,D]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    vcol_face_vtx(A),
    vcol_face_vtx(B),
    gl:edgeFlag(?GL_FALSE),
    ?GL__END(vcol_face_vtx(C)),
    ?GL__BEGIN(?GL_TRIANGLES),
    vcol_face_vtx(A),
    gl:edgeFlag(?GL_TRUE),
    vcol_face_vtx(C),
    ?GL__END(vcol_face_vtx(D)).

vcol_face_vtx([Pos|{R,G,B}]) ->
    gl:color3f(R, G, B),
    gl:vertex3dv(Pos);
vcol_face_vtx([Pos|_]) ->
    gl:color3f(1, 1, 1),
    gl:vertex3dv(Pos).

%% vcol_face(FaceNormal, [Position|Color]) -> ok
%%  Draw a face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face(N, Vs) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    vcol_face_1(Tess, Vs).

vcol_face_1(Tess, [[Pos|{_,_,_}=Col]|T]) ->
    glu:tessVertex(Tess, Pos, [{color,Col}]),
    vcol_face_1(Tess, T);
vcol_face_1(Tess, [[Pos|_]|T]) ->
    glu:tessVertex(Tess, Pos, [{color,{1.0,1.0,1.0}}]),
    vcol_face_1(Tess, T);
vcol_face_1(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%%%
%%% Drawing of faces with smooth normals.
%%%

%% smooth_plain_face([{Position,_,VertexNormal}]) -> ok
%%  Draw a smooth tri or quad with neither UV coordinates nor vertex colors.
smooth_plain_face([A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_plain_face_vtx(A),
    smooth_plain_face_vtx(B),
    ?GL__END(smooth_plain_face_vtx(C));
smooth_plain_face([A,B,C,D]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_plain_face_vtx(A),
    smooth_plain_face_vtx(B),
    ?GL__END(smooth_plain_face_vtx(C)),
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_plain_face_vtx(A),
    smooth_plain_face_vtx(C),
    ?GL__END(smooth_plain_face_vtx(D)).

smooth_plain_face_vtx({P,_,N}) ->
    gl:normal3fv(N),
    gl:vertex3dv(P).

%% smooth_plain_face(FaceNormal, [{Position,_,VertexNormal}]) -> ok
%%  Draw a smooth face with neither UV coordinates nor vertex colors.
smooth_plain_face(N, Vs) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_plain_face_1(Tess, Vs).

smooth_plain_face_1(Tess, [{P,_,N}|Vs]) ->
    glu:tessVertex(Tess, P, [{normal,N}]),
    smooth_plain_face_1(Tess, Vs);
smooth_plain_face_1(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%% smooth_uv_face([{Position,UV,VertexNormal}]) -> ok
%%  Draw a smoth tri or quad with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face([A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_uv_face_vtx(A),
    smooth_uv_face_vtx(B),
    ?GL__END(smooth_uv_face_vtx(C));
smooth_uv_face([A,B,C,D]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_uv_face_vtx(A),
    smooth_uv_face_vtx(B),
    ?GL__END(smooth_uv_face_vtx(C)),
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_uv_face_vtx(A),
    smooth_uv_face_vtx(C),
    ?GL__END(smooth_uv_face_vtx(D)).

smooth_uv_face_vtx({P,{U,V},N}) ->
    gl:texCoord2f(U, V),
    gl:normal3fv(N),
    gl:vertex3dv(P);
smooth_uv_face_vtx({P,_,N}) ->
    gl:texCoord2i(0, 0),
    gl:normal3fv(N),
    gl:vertex3dv(P).

%% smooth_uv_face(FaceNormal, [{Position,UV,VertexNormal}]) -> ok
%%  Draw a smoth face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face(N, Vs) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_uv_face_1(Tess, Vs).

smooth_uv_face_1(Tess, [{P,{_,_}=UV,N}|Vs]) ->
    glu:tessVertex(Tess, P, [{normal,N},{texcoord2,UV}]),
    smooth_uv_face_1(Tess, Vs);
smooth_uv_face_1(Tess, [{P,_,N}|Vs]) ->
    glu:tessVertex(Tess, P, [{normal,N},{texcoord2,{0.0,0.0}}]),
    smooth_uv_face_1(Tess, Vs);
smooth_uv_face_1(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%% smooth_vcol_face([{Position,UV,VertexNormal}]) -> ok
%%  Draw a smooth tri or quad with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face([A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_vcol_face_vtx(A),
    smooth_vcol_face_vtx(B),
    ?GL__END(smooth_vcol_face_vtx(C));
smooth_vcol_face([A,B,C,D]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_vcol_face_vtx(A),
    smooth_vcol_face_vtx(B),
    ?GL__END(smooth_vcol_face_vtx(C)),
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_vcol_face_vtx(A),
    smooth_vcol_face_vtx(C),
    ?GL__END(smooth_vcol_face_vtx(D)).

smooth_vcol_face_vtx({P,{R,G,B},N}) ->
    gl:color3f(R, G, B),
    gl:normal3fv(N),
    gl:vertex3dv(P);
smooth_vcol_face_vtx({P,_,N}) ->
    gl:color3f(1, 1, 1),
    gl:normal3fv(N),
    gl:vertex3dv(P).

%% smooth_vcol_face(FaceNormal, [{Position,UV,VertexNormal}]) -> ok
%%  Draw a smooth face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face(N, Vs) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_vcol_face_1(Tess, Vs).

smooth_vcol_face_1(Tess, [{P,{_,_,_}=Color,N}|Vs]) ->
    glu:tessVertex(Tess, P, [{color,Color},{normal,N}]),
    smooth_vcol_face_1(Tess, Vs);
smooth_vcol_face_1(Tess, [{P,_,N}|Vs]) ->
    glu:tessVertex(Tess, P, [{color,{1.0,1.0,1.0}},{normal,N}]),
    smooth_vcol_face_1(Tess, Vs);
smooth_vcol_face_1(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).
