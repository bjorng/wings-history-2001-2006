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
%%     $Id: wings__du.erl,v 1.2 2003/08/24 09:20:22 bjorng Exp $
%%

-module(wings__du).
-export([is_quirky_loaded/0,init_cb/1,begin_end/2,
	 mat_face/2,uv_face/2,vcol_face/2,
	 smooth_mat_face/2,smooth_uv_face/2,smooth_vcol_face/2]).

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

%% mat_face(FaceNormal, [Position]) -> ok
%%  Draw a face with neither UV coordinates nor vertex colors.
mat_face(_, [A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    gl:vertex3dv(A),
    gl:vertex3dv(B),
    ?GL__END(gl:vertex3dv(C));
mat_face(N, [A,B,C,D]=VsPos) ->
    case wings_draw_util:good_triangulation(N, A, B, C, D) of
	false ->
	    mat_face_1(N, VsPos);
	true ->
	    ?GL__BEGIN(?GL_TRIANGLES),
	    gl:vertex3dv(A),
	    gl:vertex3dv(B),
	    gl:edgeFlag(?GL_FALSE),
	    ?GL__END(gl:vertex3dv(C)),
	    ?GL__BEGIN(?GL_TRIANGLES),
	    gl:vertex3dv(A),
	    gl:edgeFlag(?GL_TRUE),
	    gl:vertex3dv(C),
 	    ?GL__END(gl:vertex3dv(D))
    end;
mat_face(N, VsPos) -> mat_face_1(N, VsPos).

mat_face_1(N, VsPos) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    mat_face_2(Tess, VsPos).

mat_face_2(Tess, [P|T]) ->
    glu:tessVertex(Tess, P),
    mat_face_2(Tess, T);
mat_face_2(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%% uv_face(FaceNormal, [Position|UV]) -> ok
%%  Draw a face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face(_, [A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    uv_face_vtx(A),
    uv_face_vtx(B),
    ?GL__END(uv_face_vtx(C));
uv_face(N, [[A0|_]=A,[B0|_]=B,[C0|_]=C,[D0|_]=D]=VsPos) ->
    case wings_draw_util:good_triangulation(N, A0, B0, C0, D0) of
	false ->
	    uv_face_1(N, VsPos);
	true ->
	    ?GL__BEGIN(?GL_TRIANGLES),
	    uv_face_vtx(A),
	    uv_face_vtx(B),
	    gl:edgeFlag(?GL_FALSE),
	    ?GL__END(uv_face_vtx(C)),
	    ?GL__BEGIN(?GL_TRIANGLES),
	    uv_face_vtx(A),
	    gl:edgeFlag(?GL_TRUE),
	    uv_face_vtx(C),
	    ?GL__END(uv_face_vtx(D))
    end;
uv_face(N, Vs) -> uv_face_1(N, Vs).

uv_face_1(N, Vs) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    uv_face_2(Tess, Vs).

uv_face_2(Tess, [[Pos|{_,_}=UV]|T]) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,UV}]),
    uv_face_2(Tess, T);
uv_face_2(Tess, [[Pos|_]|T]) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,{0.0,0.0}}]),
    uv_face_2(Tess, T);
uv_face_2(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

uv_face_vtx([Pos|{U,V}]) ->
    gl:texCoord2f(U, V),
    gl:vertex3dv(Pos);
uv_face_vtx([Pos|_]) ->
    gl:texCoord2i(0, 0),
    gl:vertex3dv(Pos).

%% vcol_face(FaceNormal, [Position|Color]) -> ok
%%  Draw a face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face(_, [A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    vcol_face_vtx(A),
    vcol_face_vtx(B),
    ?GL__END(vcol_face_vtx(C));
vcol_face(N, [[A0|_]=A,[B0|_]=B,[C0|_]=C,[D0|_]=D]=VsPos) ->
    case wings_draw_util:good_triangulation(N, A0, B0, C0, D0) of
	false ->
	    vcol_face_1(N, VsPos);
	true ->
	    ?GL__BEGIN(?GL_TRIANGLES),
	    vcol_face_vtx(A),
	    vcol_face_vtx(B),
	    gl:edgeFlag(?GL_FALSE),
	    ?GL__END(vcol_face_vtx(C)),
	    ?GL__BEGIN(?GL_TRIANGLES),
	    vcol_face_vtx(A),
	    gl:edgeFlag(?GL_TRUE),
	    vcol_face_vtx(C),
	    ?GL__END(vcol_face_vtx(D))
    end;
vcol_face(N, Vs) -> vcol_face_1(N, Vs).

vcol_face_1(N, Vs) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    vcol_face_2(Tess, Vs).

vcol_face_2(Tess, [[Pos|{_,_,_}=Col]|T]) ->
    glu:tessVertex(Tess, Pos, [{color,Col}]),
    vcol_face_2(Tess, T);
vcol_face_2(Tess, [[Pos|_]|T]) ->
    glu:tessVertex(Tess, Pos, [{color,{1.0,1.0,1.0}}]),
    vcol_face_2(Tess, T);
vcol_face_2(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

vcol_face_vtx([Pos|{R,G,B}]) ->
    gl:color3f(R, G, B),
    gl:vertex3dv(Pos);
vcol_face_vtx([Pos|_]) ->
    gl:color3f(1, 1, 1),
    gl:vertex3dv(Pos).

%%%
%%% Drawing of faces with smooth normals.
%%%

%% smooth_mat_face(FaceNormal, [Position,_,VertexNormal]) -> ok
%%  Draw a smooth face with neither UV coordinates nor vertex colors.
smooth_mat_face(_, [A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_mat_face_vtx(A),
    smooth_mat_face_vtx(B),
    ?GL__END(smooth_mat_face_vtx(C));
smooth_mat_face(N, [A,B,C,D]=Vs) ->
    Ap = element(1, A),
    Bp = element(1, B),
    Cp = element(1, C),
    Dp = element(1, D),
    case wings_draw_util:good_triangulation(N, Ap, Bp, Cp, Dp) of
	true ->
	    ?GL__BEGIN(?GL_TRIANGLES),
	    smooth_mat_face_vtx(A),
	    smooth_mat_face_vtx(B),
	    ?GL__END(smooth_mat_face_vtx(C)),
	    ?GL__BEGIN(?GL_TRIANGLES),
	    smooth_mat_face_vtx(A),
	    smooth_mat_face_vtx(C),
	    ?GL__END(smooth_mat_face_vtx(D));
	false ->
	    smooth_mat_face_1(N, Vs)
    end;
smooth_mat_face(N, Vs) -> smooth_mat_face_1(N, Vs).
	    
smooth_mat_face_1({X,Y,Z}, Vs) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_mat_face_2(Vs, Tess),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

smooth_mat_face_2([{P,_,N}|Vs], Tess) ->
    glu:tessVertex(Tess, P, [{normal,N}]),
    smooth_mat_face_2(Vs, Tess);
smooth_mat_face_2([], _) -> ok.

smooth_mat_face_vtx({P,_,N}) ->
    gl:normal3fv(N),
    gl:vertex3dv(P).

%% smooth_uv_face(FaceNormal, [Position,UV,VertexNormal]) -> ok
%%  Draw a smoth face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face(_, [A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_uv_face_vtx(A),
    smooth_uv_face_vtx(B),
    ?GL__END(smooth_uv_face_vtx(C));
smooth_uv_face(N, [A,B,C,D]=Vs) ->
    Ap = element(1, A),
    Bp = element(1, B),
    Cp = element(1, C),
    Dp = element(1, D),
    case wings_draw_util:good_triangulation(N, Ap, Bp, Cp, Dp) of
	true ->
	    ?GL__BEGIN(?GL_TRIANGLES),
	    smooth_uv_face_vtx(A),
	    smooth_uv_face_vtx(B),
	    ?GL__END(smooth_uv_face_vtx(C)),
	    ?GL__BEGIN(?GL_TRIANGLES),
	    smooth_uv_face_vtx(A),
	    smooth_uv_face_vtx(C),
	    ?GL__END(smooth_uv_face_vtx(D));
	false ->
	    smooth_uv_face_1(N, Vs)
    end;
smooth_uv_face(N, Vs) -> smooth_uv_face_1(N, Vs).

smooth_uv_face_1({X,Y,Z}, Vs) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_uv_face_2(Vs, Tess),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

smooth_uv_face_2([{P,{_,_}=UV,N}|Vs], Tess) ->
    glu:tessVertex(Tess, P, [{normal,N},{texcoord2,UV}]),
    smooth_uv_face_2(Vs, Tess);
smooth_uv_face_2([{P,_,N}|Vs], Tess) ->
    glu:tessVertex(Tess, P, [{normal,N},{texcoord2,{0.0,0.0}}]),
    smooth_uv_face_2(Vs, Tess);
smooth_uv_face_2([], _) -> ok.

smooth_uv_face_vtx({P,{U,V},N}) ->
    gl:texCoord2f(U, V),
    gl:normal3fv(N),
    gl:vertex3dv(P);
smooth_uv_face_vtx({P,_,N}) ->
    gl:texCoord2i(0, 0),
    gl:normal3fv(N),
    gl:vertex3dv(P).
	    
%% smooth_vcol_face(FaceNormal, [Position,UV,VertexNormal]) -> ok
%%  Draw a smooth face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face(_, [A,B,C]) ->
    ?GL__BEGIN(?GL_TRIANGLES),
    smooth_vcol_face_vtx(A),
    smooth_vcol_face_vtx(B),
    ?GL__END(smooth_vcol_face_vtx(C));
smooth_vcol_face(N, [A,B,C,D]=Vs) ->
    Ap = element(1, A),
    Bp = element(1, B),
    Cp = element(1, C),
    Dp = element(1, D),
    case wings_draw_util:good_triangulation(N, Ap, Bp, Cp, Dp) of
	true ->
	    ?GL__BEGIN(?GL_TRIANGLES),
	    smooth_vcol_face_vtx(A),
	    smooth_vcol_face_vtx(B),
	    ?GL__END(smooth_vcol_face_vtx(C)),
	    ?GL__BEGIN(?GL_TRIANGLES),
	    smooth_vcol_face_vtx(A),
	    smooth_vcol_face_vtx(C),
	    ?GL__END(smooth_vcol_face_vtx(D));
	false ->
	    smooth_vcol_faces_1(N, Vs)
    end;
smooth_vcol_face(N, Vs) -> smooth_vcol_faces_1(N, Vs).

smooth_vcol_faces_1({X,Y,Z}, Vs) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    smooth_vcol_face_2(Vs, Tess),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

smooth_vcol_face_2([{P,{_,_,_}=Color,N}|Vs], Tess) ->
    glu:tessVertex(Tess, P, [{color,Color},{normal,N}]),
    smooth_vcol_face_2(Vs, Tess);
smooth_vcol_face_2([{P,_,N}|Vs], Tess) ->
    glu:tessVertex(Tess, P, [{color,{1.0,1.0,1.0}},{normal,N}]),
    smooth_vcol_face_2(Vs, Tess);
smooth_vcol_face_2([], _) -> ok.

smooth_vcol_face_vtx({P,{R,G,B},N}) ->
    gl:color3f(R, G, B),
    gl:normal3fv(N),
    gl:vertex3dv(P);
smooth_vcol_face_vtx({P,_,N}) ->
    gl:color3f(1, 1, 1),
    gl:normal3fv(N),
    gl:vertex3dv(P).
