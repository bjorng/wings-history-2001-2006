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
%%     $Id: auv_pick.erl,v 1.5 2003/08/23 13:43:09 bjorng Exp $
%%

-module(auv_pick).
-export([event/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("auv.hrl").

-import(lists, [foreach/2,map/2,reverse/2,keysearch/3]).

%% For ordinary picking.
-record(pick,
	{st,					%Saved state.
	 op					%Operation: add/delete
	}).

%% For highlighting.
-record(hl,
	{st,					%Saved state.
	 prev=none				%Previous hit ({Id,Item}).
	}).

% event(#mousemotion{}=Mm, #st{selmode=Mode}=St) ->
%     case hilite_enabled(Mode) of
% 	false -> next;
% 	true -> {seq,push,handle_hilite_event(Mm, #hl{st=St})}
%     end;
event(#mousebutton{button=1,x=X,y=Y,mod=Mod,state=?SDL_PRESSED}, St) ->
    pick(X, Y, Mod, St);
event(_, _) -> next.

hilite_enabled(_) -> true.

% pick(X, Y, Mod, St) when Mod band (?SHIFT_BITS bor ?CTRL_BITS) =/= 0 ->
%     Pick = #marquee{ox=X,oy=Y,st=Uvs},
%     clear_hilite_marquee_mode(Pick);
pick(X, Y, _, St0) ->
    case do_pick(X, Y, St0) of
	none ->
	    next;
% 	    Pick = #marquee{ox=X,oy=Y,st=Uvs},
% 	    clear_hilite_marquee_mode(Pick);
	{PickOp,St} ->
	    wings_wm:dirty(),
	    Pick = #pick{st=St,op=PickOp},
	    {seq,push,get_pick_event(Pick)}
    end.

%%
%% Highlighting on mouse move.
%%

get_hilite_event(HL) ->
    fun(Ev) -> handle_hilite_event(Ev, HL) end.

handle_hilite_event(redraw, #hl{st=St}) ->
    wpc_autouv:redraw(St),
    keep;
handle_hilite_event(#mousemotion{x=X,y=Y}, #hl{prev=PrevHit,st=St}=HL) ->
    case raw_pick(X, Y, St) of
	PrevHit ->
	    get_hilite_event(HL);
	none ->
	    wings_wm:dirty(),
	    insert_hilite_dl(none, none),
	    wpc_autouv:update_dlists(St),
	    get_hilite_event(HL#hl{prev=none});
	Hit ->
	    wings_wm:dirty(),
	    DL = hilite_draw_sel_dl(Hit, St),
	    insert_hilite_dl(Hit, DL),
	    wpc_autouv:update_dlists(St),
	    get_hilite_event(HL#hl{prev=Hit})
    end;
handle_hilite_event(init_opengl, #hl{st=St}) ->
    wings:init_opengl(St);
handle_hilite_event(_, _) ->
    insert_hilite_dl(none, none),
    next.

insert_hilite_dl(Hit, DL) ->
    wings_draw_util:map(fun(D, _) ->
				insert_hilite_dl_1(D, Hit, DL)
			end, []).

insert_hilite_dl_1(#dlo{src_we=#we{id=Id}}=D, {_,{Id,_}}, DL) ->
    {D#dlo{hilite=DL},[]};
insert_hilite_dl_1(D, _, _) -> {D#dlo{hilite=none},[]}.

hilite_draw_sel_dl({Mode,{Id,Item}=Hit}, #st{shapes=Shs}=St) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    hilite_color(Hit, St),
    We = gb_trees:get(Id, Shs),
    hilit_draw_sel(Mode, Item, We),
    gl:endList(),
    List.

hilite_color({Id,Item}, #st{sel=Sel}) ->
    Key = case keysearch(Id, 1, Sel) of
	      false -> unselected_hlite;
	      {value,{Id,Items}} ->
		  case gb_sets:is_member(Item, Items) of
		      false -> unselected_hlite;
		      true -> selected_hlite
		  end
	  end,
    gl:color3fv(wings_pref:get_value(Key)).

hilit_draw_sel(vertex, V, #we{vp=Vtab}) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:'begin'(?GL_POINTS),
    gl:vertex3fv(gb_trees:get(V, Vtab)),
    gl:'end'();
hilit_draw_sel(edge, Edge, #we{es=Etab,vp=Vtab}) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:'begin'(?GL_LINES),
    gl:vertex3fv(gb_trees:get(Va, Vtab)),
    gl:vertex3fv(gb_trees:get(Vb, Vtab)),
    gl:'end'();
hilit_draw_sel(face, Face, We) ->
    case wings_pref:get_value(selection_style) of
	stippled -> gl:enable(?GL_POLYGON_STIPPLE);
	solid -> ok
    end,
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_draw_util:begin_end(fun() ->
				      wings_draw_util:flat_face(Face, We)
			      end),
    gl:disable(?GL_POLYGON_STIPPLE);
hilit_draw_sel(body, _, #we{name=#ch{fs=Fs}}=We) ->
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_draw_util:begin_end(
      fun() ->
	      foreach(fun(Face) ->
			      wings_draw_util:flat_face(Face, We)
		      end, Fs)
      end).

%%
%% Drag picking.
%%

get_pick_event(Pick) ->
    {replace,fun(Ev) -> pick_event(Ev, Pick) end}.

pick_event(redraw, #pick{st=St}) ->
    wpc_autouv:redraw(St),
    keep;
pick_event(#mousemotion{x=X,y=Y}, #pick{st=St0,op=Op}=Pick) ->
    case do_pick(X, Y, St0) of
	none -> keep;
	{Op,St} ->
	    wings_wm:dirty(),
	    wpc_autouv:update_dlists(St),
	    get_pick_event(Pick#pick{st=St});
	{_,_} -> keep
    end;
pick_event(#mousebutton{button=1,state=?SDL_RELEASED}, #pick{st=St}) ->
    wings_wm:later({new_state,St}),
    pop;
pick_event(_, _) -> keep.

do_pick(X, Y, St) ->
    case raw_pick(X, Y, St) of
	none -> none;
	Hit -> update_selection(Hit, St)
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

raw_pick(X0, Y0, #st{selmode=Mode,bb=#uvstate{geom={Vx,Vy,Vw,Vh}}}) ->
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
	[Hit|_] -> {Mode,Hit}
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
    gl:pushName(0),
    foreach(fun(Face) ->
		    gl:loadName(Face),
		    face(Face, We)
	    end, Fs),
    gl:popName().

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
