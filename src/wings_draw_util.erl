%%
%%  wings_draw_util.erl --
%%
%%     Utilities for drawing objects.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw_util.erl,v 1.1 2001/11/28 20:49:36 bjorng Exp $
%%

-module(wings_draw_util).
-export([init/0,tess/0,sel_face/2,face/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

init() ->
    Tess = glu:newTess(),
    put(wings_gnu_tess, Tess), 
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_GLBEGIN),
    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_GLEND),
    glu:tessCallback(Tess, ?GLU_TESS_EDGE_FLAG, ?ESDL_TESSCB_GLEDGEFLAG),
    glu:tessCallback(Tess, ?GLU_TESS_COMBINE, ?ESDL_TESSCB_COMBINE).

tess() ->
    get(wings_gnu_tess).

%%
%% Draw a face for selection and tesselate it if necessary.
%%

sel_face(Face, #we{vs=Vtab}=We) ->
    case wings_face:surrounding_vertices(Face, We) of
	[_,_,_,_,_|_]=Vs ->
	    {X,Y,Z} = wings_face:face_normal(Vs, We),
	    Tess = tess(),
	    glu:tessNormal(Tess, X, Y, Z),
	    glu:tessBeginPolygon(Tess),
	    glu:tessBeginContour(Tess),
	    tess_sel_face(Tess, Vs, Vtab),
	    glu:tessEndContour(Tess),
	    glu:tessEndPolygon(Tess),
	    gl:edgeFlag(?GL_TRUE);
	Vs ->
	    gl:'begin'(?GL_POLYGON),
	    sel_face_1(Vs, Vtab),
	    gl:'end'()
    end.

tess_sel_face(Tess, [V|T], Vtab) ->
    glu:tessVertex(Tess, pos(V, Vtab)),
    tess_sel_face(Tess, T, Vtab);
tess_sel_face(Tess, [], Vtab) -> ok.

sel_face_1([Pos|T], Vtab) ->
    gl:vertex3fv(pos(Pos, Vtab)),
    sel_face_1(T, Vtab);
sel_face_1([], Vtab) -> ok.

%%
%% Draw a face. Tesselate polygons (>4 edges).
%%

face(Face, Edge, #we{mode=vertex}=We) ->
    case wings_face:draw_info(Face, Edge, We) of
	[_,_,_,_,_|_]=Vs ->
	    {X,Y,Z} = N = wings_face:draw_normal(Vs),
	    Tess = tess(),
	    glu:tessNormal(Tess, X, Y, Z),
	    glu:tessBeginPolygon(Tess),
	    glu:tessBeginContour(Tess),
	    tess_face_vtxcol(Tess, Vs, [{normal,N}]),
	    glu:tessEndContour(Tess),
	    glu:tessEndPolygon(Tess),
	    gl:edgeFlag(?GL_TRUE);
	Vs ->
	    gl:'begin'(?GL_POLYGON),
	    gl:normal3fv(wings_face:draw_normal(Vs)),
	    face_vtxcol_1(Vs),
	    gl:'end'()
    end;
face(Face, Edge, #we{vs=Vtab}=We) ->
    case wings_face:surrounding_vertices(Face, Edge, We) of
	[_,_,_,_,_|_]=Vs ->
	    {X,Y,Z} = N = wings_face:face_normal(Vs, We),
	    Tess = tess(),
	    glu:tessNormal(Tess, X, Y, Z),
	    glu:tessBeginPolygon(Tess),
	    glu:tessBeginContour(Tess),
	    Info = [{material,?GL_FRONT,?GL_DIFFUSE,{1.0,1.0,1.0}},
		    {normal,N}],
	    tess_face(Tess, Vs, Info, Vtab),
	    glu:tessEndContour(Tess),
	    glu:tessEndPolygon(Tess),
	    gl:edgeFlag(?GL_TRUE);
	Vs ->
	    gl:'begin'(?GL_POLYGON),
	    gl:normal3fv(wings_face:face_normal(Vs, We)),
	    gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, {1.0,1.0,1.0}),
	    face_1(Vs, Vtab),
	    gl:'end'()
    end.

tess_face_vtxcol(Tess, [{Pos,Col}|T], Normal) ->
    glu:tessVertex(Tess, Pos, [{material,?GL_FRONT,?GL_DIFFUSE,Col}|Normal]),
    tess_face_vtxcol(Tess, T, Normal);
tess_face_vtxcol(Tess, [], Normal) -> ok.

tess_face(Tess, [V|T], N, Vtab) ->
    glu:tessVertex(Tess, pos(V, Vtab), N),
    tess_face(Tess, T, N, Vtab);
tess_face(Tess, [], N, Vtab) -> ok.

face_vtxcol_1([{Pos,Col}|T]) ->
    gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, Col),
    gl:vertex3fv(Pos),
    face_vtxcol_1(T);
face_vtxcol_1([]) -> ok.

face_1([Pos|T], Vtab) ->
    gl:vertex3fv(pos(Pos, Vtab)),
    face_1(T, Vtab);
face_1([], Vtab) -> ok.


%%
%% Utilities.
%%

pos(Key, Tab) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tab),
    Pos.
