%%
%%  wings_draw_util.erl --
%%
%%     Utilities for drawing objects.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw_util.erl,v 1.13 2002/02/24 22:36:52 bjorng Exp $
%%

-module(wings_draw_util).
-export([init/0,tess/0,begin_end/1,face/2,face/3,flat_face/2,flat_face/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

init() ->
    case tess() of
	undefined -> ok;
	OldTess -> glu:deleteTess(OldTess)
    end,
    Tess = glu:newTess(),
    put(wings_glu_tess, Tess), 
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
    glu:tessCallback(Tess, ?GLU_TESS_EDGE_FLAG, ?ESDL_TESSCB_GLEDGEFLAG),
    glu:tessCallback(Tess, ?GLU_TESS_COMBINE, ?ESDL_TESSCB_COMBINE),
    case wings_pref:get_value(display_list_opt) of
	false ->
	    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_GLBEGIN),
	    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_GLEND);
	true ->ok
    end.

tess() ->
    get(wings_glu_tess).

begin_end(Body) ->
    case wings_pref:get_value(display_list_opt) of
	false ->
	    Body();
	true ->
	    gl:'begin'(?GL_TRIANGLES),
	    Body(),
	    gl:'end'()
    end,
    gl:edgeFlag(?GL_TRUE).

%%
%% Draw a face. Tesselate polygons (4 edges or more).
%%

face(Face, #we{fs=Ftab}=We) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    face(Face, Edge, We).

face(Face, Edge, #we{mode=material,vs=Vtab}=We) ->
    Vs = wings_face:surrounding_vertices(Face, Edge, We),
    {X,Y,Z} = N = wings_face:face_normal(Vs, We),
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    Info = [{normal,N}],
    tess_face(Tess, Vs, Info, Vtab);
face(Face, Edge, We) ->
    Vs = wings_face:draw_info(Face, Edge, We),
    {X,Y,Z} = N = wings_face:draw_normal(Vs),
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_face_vtxcol(Tess, Vs, [{normal,N}]).

tess_face_vtxcol(Tess, [{Pos,{_,_}=UV}|T], Normal) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,UV}|Normal]),
    tess_face_vtxcol(Tess, T, Normal);
tess_face_vtxcol(Tess, [{Pos,{_,_,_}=Col}|T], Normal) ->
    glu:tessVertex(Tess, Pos, [{material,?GL_FRONT,
				?GL_AMBIENT_AND_DIFFUSE,Col}|Normal]),
    tess_face_vtxcol(Tess, T, Normal);
tess_face_vtxcol(Tess, [], _Normal) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

tess_face(Tess, [V|T], N, Vtab) ->
    glu:tessVertex(Tess, pos(V, Vtab), N),
    tess_face(Tess, T, N, Vtab);
tess_face(Tess, [], _N, _Vtab) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%%
%% Draw a face. Tesselate polygons (4 edges or more).
%%

flat_face(Face, #we{vs=Vtab}=We) ->
    Vs = wings_face:surrounding_vertices(Face, We),
    {X,Y,Z} = wings_face:face_normal(Vs, We),
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_flat_face(Tess, Vs, Vtab).

flat_face(Face, Edge, #we{vs=Vtab}=We) ->
    Vs = wings_face:surrounding_vertices(Face, Edge, We),
    {X,Y,Z} = wings_face:face_normal(Vs, We),
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_flat_face(Tess, Vs, Vtab).

tess_flat_face(Tess, [V|T], Vtab) ->
    glu:tessVertex(Tess, pos(V, Vtab)),
    tess_flat_face(Tess, T, Vtab);
tess_flat_face(Tess, [], _Vtab) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%%
%% Utilities.
%%

pos(Key, Tab) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tab),
    Pos.
