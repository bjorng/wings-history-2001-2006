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
%%     $Id: auv_pick.erl,v 1.4 2003/08/17 08:41:42 bjorng Exp $
%%

-module(auv_pick).
-export([event/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("auv.hrl").

-import(lists, [foreach/2,map/2,reverse/2]).

%% For ordinary picking.
-record(pick,
	{st,					%Saved state.
	 op					%Operation: add/delete
	}).

% event(#mousemotion{}=Mm, Uvs) ->
%     {seq,push,handle_hilite_event(Mm, #hl{st=St,redraw=Redraw};
event(#mousebutton{button=1,x=X,y=Y,mod=Mod,state=?SDL_PRESSED}, Uvs) ->
    pick(X, Y, Mod, Uvs);
event(_, _) -> next.

% pick(X, Y, Mod, Uvs) when Mod band (?SHIFT_BITS bor ?CTRL_BITS) =/= 0 ->
%     Pick = #marquee{ox=X,oy=Y,st=Uvs},
%     clear_hilite_marquee_mode(Pick);
pick(X, Y, _, Uvs0) ->
    case do_pick(X, Y, Uvs0) of
	none ->
	    next;
% 	    Pick = #marquee{ox=X,oy=Y,st=Uvs},
% 	    clear_hilite_marquee_mode(Pick);
	{PickOp,Uvs} ->
	    wings_wm:dirty(),
	    Pick = #pick{st=Uvs,op=PickOp},
	    {seq,push,get_pick_event(Pick)}
    end.

%%
%% Drag picking.
%%

get_pick_event(Pick) ->
    {replace,fun(Ev) -> pick_event(Ev, Pick) end}.

pick_event(redraw, #pick{st=Uvs}) ->
    wpc_autouv:redraw(Uvs),
    keep;
pick_event(#mousemotion{x=X,y=Y}, #pick{st=Uvs0,op=Op}=Pick) ->
    case do_pick(X, Y, Uvs0) of
	none -> keep;
	{Op,Uvs} ->
	    wings_wm:dirty(),
	    wpc_autouv:update_dlists(Uvs),
	    get_pick_event(Pick#pick{st=Uvs});
	{_,_} -> keep
    end;
pick_event(#mousebutton{button=1,state=?SDL_RELEASED}, #pick{st=Uvs}) ->
    wings_wm:later({new_state,Uvs}),
    pop;
pick_event(_, _) -> keep.

do_pick(X, Y, #uvstate{sel=Sel0}=Uvs) ->
    case raw_pick(X, Y, Uvs) of
	none -> none;
	Hit ->
	    St = #st{sel=Sel0},
	    {Type,#st{sel=Sel}} = update_selection({body,Hit}, St),
	    {Type,Uvs#uvstate{sel=Sel}}
    end.

update_selection({Mode,{Id,Item}}, #st{sel=Sel0}=St) ->
    {Type,Sel} = update_selection(Id, Item, Sel0, []),
    {Type,St#st{selmode=Mode,sel=Sel,sh=false}}.

update_selection(Id, Item, [{I,_}=H|T], Acc) when Id > I ->
    update_selection(Id, Item, T, [H|Acc]);
update_selection(Id, Item, [{I,_}|_]=T, Acc) when Id < I ->
    {add,reverse(Acc, [{Id,gb_sets:singleton(Item)}|T])};
update_selection(Id, Item, [{_,Items0}|T0], Acc) -> %Id == I
    case gb_sets:is_member(Item, Items0) of
	true ->
	    Items = gb_sets:delete(Item, Items0),
	    T = case gb_sets:is_empty(Items) of
		    true -> T0;
		    false -> [{Id,Items}|T0]
		end,
	    {delete,reverse(Acc, T)};
	false ->
	    Items = gb_sets:insert(Item, Items0),
	    {add,reverse(Acc, [{Id,Items}|T0])}
    end;
update_selection(Id, Item, [], Acc) ->
    {add,reverse(Acc, [{Id,gb_sets:singleton(Item)}])}.

raw_pick(X0, Y0, #uvstate{geom={Vx,Vy,Vw,Vh}}) ->
    HitBuf = get(wings_hitbuf),
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    {W,H} = wings_wm:win_size(),
    X = float(X0),
    Y = H-float(Y0),
    S = 5,
    glu:pickMatrix(X, Y, S, S, {0,0,W,H}),
    glu:ortho2D(Vx, Vy, Vw, Vh),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    draw(),
    case get_hits(HitBuf) of
	none -> none;
	[Hit|_] -> Hit
    end.

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
    #ch{fs=Fs} = Ch,
    gl:pushMatrix(),
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
