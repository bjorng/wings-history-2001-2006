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
%%     $Id: wings_draw_util.erl,v 1.17 2002/04/12 15:09:13 bjorng Exp $
%%

-module(wings_draw_util).
-export([init/0,tess/0,begin_end/1,face/2,face/3,flat_face/2,flat_face/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [reverse/1]).

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
    end,
        P = <<16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55>>,
    gl:polygonStipple(P).

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
    VsPos = vspos(Vs, Vtab, []),
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    {X,Y,Z} = N,
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_face(Tess, VsPos);
face(Face, Edge, We) ->
    Vs = wings_face:draw_info(Face, Edge, We),
    {X,Y,Z} = N = wings_face:draw_normal(Vs),
    gl:normal3fv(N),
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_face_vtxcol(Tess, Vs).

tess_face_vtxcol(Tess, [{Pos,{_,_}=UV}|T]) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,UV}]),
    tess_face_vtxcol(Tess, T);
tess_face_vtxcol(Tess, [{Pos,{_,_,_}=Col}|T]) ->
    glu:tessVertex(Tess, Pos, [{material,?GL_FRONT,
				?GL_AMBIENT_AND_DIFFUSE,Col}]),
    tess_face_vtxcol(Tess, T);
tess_face_vtxcol(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

tess_face(Tess, [P|T]) ->
    glu_tessVertex(Tess, P),
    tess_face(Tess, T);
tess_face(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

-record(tessPtr, {ptr}).

%% Dirty hack until Dan includes it in ESDL.
glu_tessVertex(#tessPtr{ptr=Tobj}, {X,Y,Z}) ->
    sdl:cast(<<(941):16, Tobj:32/unsigned,
	      X:64/float,Y:64/float,Z:64/float,
	      3:16,0>>).

%%
%% Draw a face. Tesselate polygons (4 edges or more).
%%

flat_face(Face, #we{fs=Ftab}=We) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    flat_face(Face, Edge, We).

flat_face(Face, Edge, #we{vs=Vtab}=We) ->
    Vs = wings_face:surrounding_vertices(Face, Edge, We),
    VsPos = vspos(Vs, Vtab, []),
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    {X,Y,Z} = N,
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_flat_face(Tess, VsPos).

tess_flat_face(Tess, [P|T]) ->
    glu:tessVertex(Tess, P),
    tess_flat_face(Tess, T);
tess_flat_face(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%%
%% Utilities.
%%

vspos([V|Vs], Vtab, Acc) ->
    #vtx{pos=P} = gb_trees:get(V, Vtab),
    vspos(Vs, Vtab, [P|Acc]);
vspos([], _, Acc) -> reverse(Acc).
