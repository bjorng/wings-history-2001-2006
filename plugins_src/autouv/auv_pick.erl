%%
%%  auv_pick.erl --
%%
%%     This module handles picking in AutoUV using OpenGL.
%%
%%  Copyright (c) 2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: auv_pick.erl,v 1.1 2003/08/13 05:19:53 bjorng Exp $
%%

-module(auv_pick).
-export([get_hits/1,draw/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("auv.hrl").

-import(lists, [foreach/2]).

%%%
%%% Pick up raw hits.
%%%

get_hits(HitBuf) ->
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> none;
	NumHits ->
 	    HitData = sdl_util:read(HitBuf, 5*NumHits),
 	    get_hits_1(NumHits, HitData, [])
    end.

get_hits_1(0, _, Acc) -> Acc;
get_hits_1(N, [2,_,_,A,B|T], Acc) ->
    get_hits_1(N-1, T, [{A,B}|Acc]).

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

draw() ->
    wings_draw_util:map(fun draw_fun/2, []).

draw_fun(#dlo{pick=none,src_we=We}=D, _) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    draw_1(We),
    gl:endList(),
    draw_dlist(D#dlo{pick=List});
draw_fun(D, _) -> draw_dlist(D).

draw_dlist(#dlo{pick=Pick,src_we=#we{id=Id}}=D) ->
    gl:pushName(Id),
    wings_draw_util:call(Pick),
    gl:popName(),
    D.

draw_1(We) ->
    Tess = wings_draw_util:tess(),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
    glu:tessCallback(Tess, ?GLU_TESS_EDGE_FLAG, ?ESDL_TESSCB_NONE),
    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_GLBEGIN),
    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_GLEND),
    draw_2(We),
    wings_draw_util:init_cb(Tess),
    gl:edgeFlag(?GL_TRUE).
    
draw_2(#we{name=Ch}=We) ->
    #ch{fs=Fs,center={CX,CY},scale=Scale,rotate=Rot} = Ch,
    gl:pushMatrix(),
    gl:translatef(CX, CY, 0),
    gl:scalef(Scale, Scale, 1),
    gl:rotatef(Rot, 0, 0, 1),
    gl:pushName(0),
    foreach(fun(Face) ->
		    gl:loadName(Face),
		    face(Face, We)
	    end, Fs),
    gl:popName(),
    gl:popMatrix().

face(Face, #we{vp=Vtab}=We) ->
    Vs = wings_face:vertices_cw(Face, We),
    face_1(Vs, Vtab, []).

face_1([V|Vs], Vtab, Acc) ->
    face_1(Vs, Vtab, [gb_trees:get(V, Vtab)|Acc]);
face_1([], _, VsPos) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    face_2(N, VsPos).

face_2(_, [A,B,C]) ->
    gl:'begin'(?GL_TRIANGLES),
    gl:vertex3dv(A),
    gl:vertex3dv(B),
    gl:vertex3dv(C),
    gl:'end'();
face_2(N, [A,B,C,D]=VsPos) ->
    case wings_draw_util:consistent_normal(A, B, C, N) andalso
	wings_draw_util:consistent_normal(A, C, D, N) of
	false ->
	    face_3(N, VsPos);
	true ->
 	    gl:'begin'(?GL_QUADS),
	    gl:vertex3dv(A),
	    gl:vertex3dv(B),
	    gl:vertex3dv(C),
	    gl:vertex3dv(D),
	    gl:'end'()
    end;
face_2(N, VsPos) -> face_3(N, VsPos).

face_3(N, VsPos) ->
    Tess = wings_draw_util:tess(),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_face(Tess, VsPos).

tess_face(Tess, [P|T]) ->
    glu:tessVertex(Tess, P),
    tess_face(Tess, T);
tess_face(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).
