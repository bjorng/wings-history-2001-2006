%%
%%  wings_pick.erl --
%%
%%     This module handles picking using OpenGL.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pick.erl,v 1.102 2003/07/25 12:24:08 bjorng Exp $
%%

-module(wings_pick).
-export([event/2,event/3,hilite_event/3]).
-export([do_pick/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,reverse/2,
		sort/1,foldl/3,map/2,min/1,
		keysearch/3,member/2,delete/2]).

%% For ordinary picking.
-record(pick,
	{st,					%Saved state.
	 op					%Operation: add/delete
	}).

%% For marquee picking.
-record(marquee,
	{ox,oy,					%Original X,Y.
	 cx,cy,					%Current X,Y.
	 st
	}).

%% For highlighting.
-record(hl,
	{st,					%Saved state.
	 redraw,				%Redraw function.
	 prev=none				%Previous hit ({Id,Item}).
	}).

event(Ev, St) ->
    event(Ev, St, St).

event(#mousemotion{}=Mm, #st{selmode=Mode}=St, Redraw) ->
    case hilite_enabled(Mode) of
	false -> next;
	true -> {seq,push,handle_hilite_event(Mm, #hl{st=St,redraw=Redraw})}
    end;
event(#mousebutton{button=1,x=X,y=Y,mod=Mod,state=?SDL_PRESSED}, St, _) ->
    pick(X, Y, Mod, St);
event(_, _, _) -> next.

hilite_event(#mousemotion{}=Mm, #st{selmode=Mode}=St, Redraw) ->
    case hilite_enabled(Mode) of
	false -> next;
	true ->
	    {seq,{push,dummy},
	     handle_hilite_event(Mm, #hl{st=St,redraw=Redraw})}
    end;
hilite_event(_, _, _) -> next.

hilite_enabled(vertex) -> wings_pref:get_value(vertex_hilite);
hilite_enabled(edge) -> wings_pref:get_value(edge_hilite);
hilite_enabled(face) -> wings_pref:get_value(face_hilite);
hilite_enabled(body) -> wings_pref:get_value(body_hilite).

pick(X, Y, Mod, St) when Mod band (?SHIFT_BITS bor ?CTRL_BITS) =/= 0 ->
    Pick = #marquee{ox=X,oy=Y,st=St},
    clear_hilite_marquee_mode(Pick);
pick(X, Y, _, St0) ->
    case do_pick(X, Y, St0) of
	none ->
	    Pick = #marquee{ox=X,oy=Y,st=St0},
	    clear_hilite_marquee_mode(Pick);
	{PickOp,_,St} ->
	    wings_wm:dirty(),
	    wings_draw:update_dlists(St),
	    Pick = #pick{st=St,op=PickOp},
	    {seq,push,get_pick_event(Pick)}
    end.

%%
%% Highlighting on mouse move.
%%

get_hilite_event(HL) ->
    fun(Ev) -> handle_hilite_event(Ev, HL) end.

handle_hilite_event(redraw, #hl{redraw=#st{sel=[]}=St,prev={_,Where,{_,Elem}}}) ->
    Info = case Where of
	       original -> io_lib:format("#~p", [Elem]);
	       mirror -> io_lib:format("#~p (in mirror)", [Elem])
	   end,
    wings:redraw(Info, St),
    keep;
handle_hilite_event(redraw, #hl{redraw=#st{}=St}) ->
    wings:redraw(St),
    keep;
handle_hilite_event(redraw, #hl{redraw=Redraw}) ->
    Redraw(),
    keep;
handle_hilite_event(#mousemotion{x=X,y=Y}, #hl{prev=PrevHit,st=St}=HL) ->
    case raw_pick(X, Y, St) of
	PrevHit ->
	    get_hilite_event(HL);
	none ->
	    wings_wm:dirty(),
	    insert_hilite_dl(none, none),
	    wings_draw:update_dlists(St),
	    get_hilite_event(HL#hl{prev=none});
	Hit ->
	    wings_wm:dirty(),
	    DL = hilite_draw_sel_dl(Hit, St),
	    insert_hilite_dl(Hit, DL),
	    wings_draw:update_dlists(St),
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

insert_hilite_dl_1(#dlo{src_we=#we{id=Id}}=D, {_,_,{Id,_}}, DL) ->
    {D#dlo{hilite=DL},[]};
insert_hilite_dl_1(D, _, _) -> {D#dlo{hilite=none},[]}.

hilite_draw_sel_dl({_,_,{_}}, _) -> none;
hilite_draw_sel_dl({Mode,_,{Id,Item}=Hit}, #st{shapes=Shs}=St) ->
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
hilit_draw_sel(body, _, #we{fs=Ftab}=We) ->
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_draw_util:begin_end(
      fun() ->
	      foreach(fun({Face,Edge}) ->
			      wings_draw_util:flat_face(Face, Edge, We)
		      end, gb_trees:to_list(Ftab))
      end).

%%
%% Marquee picking.
%%
clear_hilite_marquee_mode(#marquee{st=St}=Pick) ->
    Message = "[Ctrl] Deselect  "
	"[Shift] (De)select only elements wholly inside marquee",
    wings_wm:message(Message),
    {seq,push,
     fun(redraw) ->
	     wings:redraw(St),
	     wings_wm:later(now_enter_marquee_mode),
	     keep;
	(now_enter_marquee_mode) ->
	     wings_wm:grab_focus(wings_wm:this()),
	     wings_io:ortho_setup(),
	     gl:flush(),
	     gl:drawBuffer(?GL_FRONT),
	     get_marquee_event(Pick);
	(Ev) ->
	     wings_io:putback_event(Ev),
	     keep
     end}.

get_marquee_event(Pick) ->
    {replace,fun(Ev) -> marquee_event(Ev, Pick) end}.

marquee_event(redraw, #marquee{cx=Cx,cy=Cy,st=St}=M) ->
    gl:drawBuffer(?GL_BACK),
    wings:redraw(St),
    gl:drawBuffer(?GL_FRONT),
    wings_io:ortho_setup(),
    draw_marquee(Cx, Cy, M),
    keep;
marquee_event(init_opengl, #marquee{st=St}) ->
    wings:init_opengl(St);
marquee_event(#mousemotion{x=X,y=Y}, #marquee{cx=Cx,cy=Cy}=M) ->
    draw_marquee(Cx, Cy, M),
    draw_marquee(X, Y, M),
    get_marquee_event(M#marquee{cx=X,cy=Y});
marquee_event(#mousebutton{x=X0,y=Y0,mod=Mod,button=1,state=?SDL_RELEASED}, M) ->
    {Inside,Op} =
	if
	    Mod band ?SHIFT_BITS =/= 0, Mod band ?CTRL_BITS =/= 0 ->
		{true,delete};
	    Mod band ?CTRL_BITS =/= 0 ->
		{false,delete};
	    Mod band ?SHIFT_BITS =/= 0 ->
		{true,add};
	    true ->
		{false,add}
	end,
    #marquee{ox=Ox,oy=Oy,st=St0} = M,
    gl:drawBuffer(?GL_BACK),
    X = (Ox+X0)/2.0,
    Y = (Oy+Y0)/2.0,
    W = abs(Ox-X)*2.0,
    H = abs(Oy-Y)*2.0,
    case marquee_pick(Inside, X, Y, W, H, St0) of
	{none,_} -> ok;
	{Hits,_} ->
	    St = marquee_update_sel(Op, Hits, St0),
	    wings_wm:later({new_state,St})
    end,
    wings_wm:release_focus(),
    wings_wm:later(revert_state),
    pop;
marquee_event(_, _) -> keep.

marquee_pick(false, X, Y, W, H, St0) ->
    case pick_all(false, X, Y, W, H, St0) of
	{none,_}=None -> None;
	{Hits,St} -> {[{abs(Id),Face} || {Id,Face} <- Hits],St}
    end;
marquee_pick(true, X, Y0, W, H, St0) ->
    case pick_all(true, X, Y0, W, H, St0) of
	{none,_}=R -> R;
	{Hits0,St} ->
	    Hits1 = wings_util:rel2fam(Hits0),
	    HitsOrig = [Hit || {Id,_}=Hit <- Hits1, Id > 0],
	    HitsMirror = [Hit || {Id,_}=Hit <- Hits1, Id < 0],
	    {MM,PM,ViewPort} = wings_util:get_matrices(0, original),
	    {_,_,_,Wh} = ViewPort,
	    Y = Wh - Y0,
	    RectData = {MM,PM,ViewPort,X-W/2,Y-H/2,X+W/2,Y+H/2},
	    Hits2 = marquee_convert(HitsOrig, RectData, St, []),
	    Hits3 = marquee_convert(HitsMirror, RectData, St, []),
	    Hits = sofs:to_external(sofs:union(Hits2, Hits3)),
	    {Hits,St}
    end.

marquee_convert([{Id,Faces}|Hits], RectData0,
	       #st{selmode=Mode,shapes=Shs}=St, Acc) ->
    We = gb_trees:get(abs(Id), Shs),
    RectData = if
		   Id < 0 ->
		       {MM,PM,_} = wings_util:get_matrices(-Id, mirror),
		       RectData1 = setelement(2, RectData0, PM),
		       setelement(1, RectData1, MM);
		   true -> RectData0
	       end,
    case marquee_convert_1(Faces, Mode, RectData, We) of
	[] ->
	    marquee_convert(Hits, RectData, St, Acc);
	Items ->
	    marquee_convert(Hits, RectData, St, [{abs(Id),Items}|Acc])
    end;
marquee_convert([], _, _, Hits) ->
    sofs:family_to_relation(sofs:family(Hits)).

marquee_convert_1(Faces0, face, Rect, #we{vp=Vtab}=We) ->
    Vfs0 = wings_face:fold_faces(
	     fun(Face, V, _, _, A) ->
		     [{V,Face}|A]
	     end, [], Faces0, We),
    Vfs = wings_util:rel2fam(Vfs0),
    Kill0 = [Fs || {V,Fs} <- Vfs, not is_inside_rect(gb_trees:get(V, Vtab), Rect)],
    Kill1 = sofs:set(Kill0, [[face]]),
    Kill = sofs:union(Kill1),
    Faces1 = sofs:from_external(Faces0, [face]),
    Faces = sofs:difference(Faces1, Kill),
    sofs:to_external(Faces);
marquee_convert_1(Faces, vertex, Rect, #we{vp=Vtab}=We) ->
    Vs0 = wings_face:fold_faces(fun(_, V, _, _, A) ->
					[V|A]
				end, [], Faces, We),
    Vs = ordsets:from_list(Vs0),
    [V || V <- Vs, is_inside_rect(gb_trees:get(V, Vtab), Rect)];
marquee_convert_1(Faces, edge, Rect, #we{vp=Vtab}=We) ->
    Es0 = wings_face:fold_faces(fun(_, _, E, Rec, A) ->
					[{E,Rec}|A]
				end, [], Faces, We),
    Es = ordsets:from_list(Es0),
    [E || {E,#edge{vs=Va,ve=Vb}} <- Es,
	  is_all_inside_rect([gb_trees:get(Va, Vtab),gb_trees:get(Vb, Vtab)], Rect)];
marquee_convert_1(_Faces, body, Rect, #we{vp=Vtab}) ->
    case is_all_inside_rect(gb_trees:values(Vtab), Rect) of
	true -> [0];
	false -> []
    end.

is_all_inside_rect([P|Ps], Rect) ->
    is_inside_rect(P, Rect) andalso is_all_inside_rect(Ps, Rect);
is_all_inside_rect([], _Rect) -> true.

is_inside_rect({Px,Py,Pz}, {MM,PM,ViewPort,X1,Y1,X2,Y2}) ->
    {Sx,Sy,_} = glu:project(Px, Py, Pz, MM, PM, ViewPort),
    X1 < Sx andalso Sx < X2 andalso
	Y1 < Sy andalso Sy < Y2.

draw_marquee(undefined, undefined, _) -> ok;
draw_marquee(X, Y, #marquee{ox=Ox,oy=Oy}) ->
    gl:color3f(1, 1, 1),
    gl:enable(?GL_COLOR_LOGIC_OP),
    gl:logicOp(?GL_XOR),
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2i(X, Oy),
    gl:vertex2i(X, Y),
    gl:vertex2i(Ox, Y),
    gl:vertex2i(Ox, Oy),
    gl:'end'(),
    gl:flush(),
    gl:disable(?GL_COLOR_LOGIC_OP).

marquee_update_sel(Op, Hits0, #st{selmode=body}=St) ->
    Hits1 = sofs:relation(Hits0, [{id,data}]),
    Hits2 = sofs:domain(Hits1),
    Zero = sofs:from_term([0], [data]),
    Hits = sofs:constant_function(Hits2, Zero),
    marquee_update_sel_1(Op, Hits, St);
marquee_update_sel(Op, Hits0, St) ->
    Hits1 = sofs:relation(Hits0, [{id,data}]),
    Hits = sofs:relation_to_family(Hits1),
    marquee_update_sel_1(Op, Hits, St).

marquee_update_sel_1(add, Hits0, #st{sel=Sel0}=St) ->
    Hits = marquee_filter_hits(Hits0, St),
    Sel1 = [{Id,gb_sets:to_list(Items)} || {Id,Items} <- Sel0],
    Sel2 = sofs:from_external(Sel1, [{id,[data]}]),
    Sel3 = sofs:family_union(Sel2, Hits),
    Sel4 = sofs:to_external(Sel3),
    Sel = [{Id,gb_sets:from_list(Items)} || {Id,Items} <- Sel4],
    St#st{sel=Sel};
marquee_update_sel_1(delete, Hits, #st{sel=Sel0}=St) ->
    Sel1 = [{Id,gb_sets:to_list(Items)} || {Id,Items} <- Sel0],
    Sel2 = sofs:from_external(Sel1, [{id,[data]}]),
    Sel3 = sofs:family_difference(Sel2, Hits),
    Sel4 = sofs:to_external(Sel3),
    Sel = [{Id,gb_sets:from_list(Items)} || {Id,Items} <- Sel4, Items =/= []],
    St#st{sel=Sel}.

%% Filter out any mirror face from the hits.
marquee_filter_hits(Hits0, #st{selmode=face,shapes=Shs}) ->
    Type = sofs:type(Hits0),
    Hits = map(fun({Id,Faces}=Hit) ->
		case gb_trees:get(Id, Shs) of
		    #we{mirror=none} -> Hit;
		    #we{mirror=Face} -> {Id,delete(Face, Faces)}
		end
	       end, sofs:to_external(Hits0)),
    sofs:from_external(Hits, Type);
marquee_filter_hits(Hits, _) -> Hits.

%%
%% Drag picking.
%%

get_pick_event(Pick) ->
    {replace,fun(Ev) -> pick_event(Ev, Pick) end}.

pick_event(redraw, #pick{st=St}) ->
    wings:redraw(St),
    keep;
pick_event(#mousemotion{x=X,y=Y}, #pick{op=Op,st=St0}=Pick) ->
    case do_pick(X, Y, St0) of
	none -> keep;
	{Op,_,St} ->
	    wings_wm:dirty(),
	    wings_draw:update_dlists(St),
	    get_pick_event(Pick#pick{st=St});
	{_,_,_} -> keep
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

raw_pick(X0, Y0, St) ->
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
    wings_view:projection(),
    wings_view:modelview(),
    gl:enable(?GL_CULL_FACE),
    select_draw(St),
    gl:disable(?GL_CULL_FACE),
    case get_hits(HitBuf) of
	none -> none;
	Hits -> filter_hits(Hits, X, Y, St)
    end.

update_selection({Mode,MM,{Id,Item}}, #st{sel=Sel0}=St) ->
    {Type,Sel} = update_selection(Id, Item, Sel0, []),
    {Type,MM,St#st{selmode=Mode,sel=Sel,sh=false}}.

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

%%%
%%% Filter hits to obtain just one hit.
%%%

filter_hits(Hits, X, Y, #st{selmode=Mode0,shapes=Shs,sel=Sel,sh=Sh}) ->
    Mode = if
	       Sh, Mode0 =/= body, Sel == [] ->
		   {auto,Mode0};
	       true -> Mode0
	   end,
    EyePoint = wings_view:eye_point(),
    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint, none).

filter_hits_1([{Id,Face}|Hits], Shs, Mode, X, Y, EyePoint, Hit0) ->
    Mtx = if 
	      Id < 0 -> wings_util:mirror_matrix(-Id);
	      true -> identity
	  end,
    We = gb_trees:get(abs(Id), Shs),
    Vs = wings_face:vertices_cw(Face, We),
    Hit = best_hit(Id, Face, Vs, We, EyePoint, Mtx, Hit0),
    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint, Hit);
filter_hits_1([], _Shs, _Mode, _X, _Y, _EyePoint, none) -> none;
filter_hits_1([], _Shs, Mode, X, Y, _EyePoint, {_,{Id,Face,We}}) ->
    if
	Id < 0 -> convert_hit(Mode, X, Y, -Id, Face, mirror, We);
	true -> convert_hit(Mode, X, Y, Id, Face, original, We)
    end.

mul_point(identity, P) -> P;
mul_point(Mtx, P) -> e3d_mat:mul_point(Mtx, P).

best_hit(Id, Face, Vs, We, EyePoint, Matrix, Hit0) ->
    Center = mul_point(Matrix, wings_vertex:center(Vs, We)),
    D = e3d_vec:sub(Center, EyePoint),
    DistSqr = e3d_vec:dot(D, D),
    case Hit0 of
	none ->
	    {DistSqr,{Id,Face,We}};
	{DistSqr0,_} when DistSqr < DistSqr0 ->
	    {DistSqr,{Id,Face,We}};
	_Other -> Hit0
    end.

%%
%% Given a selection hit, return the correct vertex/edge/face/body.
%%

convert_hit(body, _X, _Y, Id, _Face, MM, _We) ->
    {body,MM,{Id,0}};
convert_hit(face, _X, _Y, Id, Face, MM, _We) ->
    {face,MM,{Id,Face}};
convert_hit({auto,_}, X, Y, Id, Face, MM, We) ->
    Trans = wings_util:get_matrices(Id, MM),
    Vs = sort(find_vertex(Face, We, X, Y, Trans)),
    [{Vdist0,{Xva,Yva},V},{_,{Xvb,Yvb},_}|_] = Vs,
    Vdist = math:sqrt(Vdist0),
    Es = find_edge(Face, We, X, Y, Trans),
    {Edist0,_,Edge} = min(Es),
    Edist = math:sqrt(Edist0),
    Xd = Xva-Xvb,
    Yd = Yva-Yvb,
    Lim0 = math:sqrt(Xd*Xd+Yd*Yd) / 4,
    Lim1 = min([math:sqrt(L) || {_,L,_} <- Es]) / 4,
    Lim = min([20.0,Lim0,Lim1]),
    Hilite = if
		 Vdist < Lim -> {vertex,MM,{Id,V}};
		 Edist < Lim -> {edge,MM,{Id,Edge}};
		 true -> {face,MM,{Id,Face}}
	     end,
    check_restriction(Hilite, Id, V, Edge, Face);
convert_hit(Mode, X, Y, Id, Face, MM, We) ->
    Trans = wings_util:get_matrices(Id, MM),
    case Mode of
	vertex ->
	    {_,_,V} = min(find_vertex(Face, We, X, Y, Trans)),
	    {vertex,MM,{Id,V}};
	edge ->
	    {_,_,E} = min(find_edge(Face, We, X, Y, Trans)),
	    {edge,MM,{Id,E}}
    end.

find_vertex(Face, We, X, Y, Trans) ->
    Vs0 = wings_face:vertices_ccw(Face, We),
    map(fun(V) ->
		{Xs,Ys} = Pos = project_vertex(V, We, Trans),
		Dx = X-Xs,
		Dy = Y-Ys,
		{Dx*Dx+Dy*Dy,Pos,V}
	end, Vs0).

find_edge(Face, We, Cx, Cy, Trans) ->
    wings_face:fold(
      fun(_, Edge, #edge{vs=Va,ve=Vb}, A) ->
	      {Ax,Ay} = project_vertex(Va, We, Trans),
	      {Bx,By} = project_vertex(Vb, We, Trans),
	      if
		  is_float(Ax), is_float(Ay),
		  is_float(Bx), is_float(By) ->
		      Xdist = Bx-Ax,
		      Ydist = By-Ay,
		      L = Xdist*Xdist+Ydist*Ydist,
		      {Px,Py} =
			  case catch ((Cx-Ax)*Xdist+(Cy-Ay)*Ydist)/L of
			      {'EXIT',_} -> {Ax,Ay};
			      R when R =< 0 -> {Ax,Ay};
			      R when R >= 1 -> {Bx,By};
			      R -> {Ax+R*Xdist,Ay+R*Ydist}
			  end,
		      Xdiff = Px-Cx,
		      Ydiff = Py-Cy,
		      DistSqr = Xdiff*Xdiff + Ydiff*Ydiff,
		      [{DistSqr,L,Edge}|A]
	      end
      end, [], Face, We).

project_vertex(V, We, {ModelMatrix,ProjMatrix,ViewPort}) ->
    {Px,Py,Pz} = wings_vertex:pos(V, We),
    {Xs,Ys,_} = glu:project(Px, Py, Pz, ModelMatrix,
			    ProjMatrix, ViewPort),
    {Xs,Ys}.

check_restriction({Mode,MM,_}=Hilite, Id, V, Edge, Face) ->
    case wings:get_mode_restriction() of
	all -> Hilite;
	Modes ->
	    case member(Mode, Modes) of
		true -> Hilite;
		false -> restrict_hilite(Mode, Modes, Id, V, Edge, Face, MM)
	    end
    end.

restrict_hilite(vertex, Modes, Id, _V, Edge, Face, MM) ->
    case member(edge, Modes) of
	true -> {edge,MM,{Id,Edge}};
	false ->
	    true = member(face, Modes),
	    {face,MM,{Id,Face}}
    end;
restrict_hilite(edge, Modes, Id, V, _Edge, Face, MM) ->
    case member(vertex, Modes) of
	true -> {vertex,MM,{Id,V}};
	false ->
	    true = member(face, Modes),
	    {face,MM,{Id,Face}}
    end;
restrict_hilite(face, Modes, Id, V, Edge, _Face, MM) ->
    case member(edge, Modes) of
	true -> {edge,MM,{Id,Edge}};
	false ->
	    true = member(vertex, Modes),
	    {vertex,MM,{Id,V}}
    end.
	    
%%
%% Pick all in the given rectangle (with center at X,Y).
%%

pick_all(_DrawFaces, _X, _Y, W, H, St) when W < 1.0; H < 1.0 ->
    {none,St};
pick_all(DrawFaces, X, Y0, W, H, St) ->
    HitBuf = get(wings_hitbuf),
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    {Ww,Wh} = wings_wm:win_size(),
    Y = Wh-Y0,
    glu:pickMatrix(X, Y, W, H, [0,0,Ww,Wh]),
    wings_view:projection(),
    wings_view:modelview(),
    case DrawFaces of
	true ->
	    gl:enable(?GL_CULL_FACE),
	    select_draw(St),
	    gl:disable(?GL_CULL_FACE);
	false -> marquee_draw(St)
    end,
    {get_hits(HitBuf),St}.

marquee_draw(#st{selmode=edge}) ->
      Draw = fun(#we{es=Etab,vp=Vtab}) ->
		     foreach(fun({Edge,#edge{vs=Va,ve=Vb}}) ->
				     gl:loadName(Edge),
				     gl:'begin'(?GL_LINES),
				     gl:vertex3fv(gb_trees:get(Va, Vtab)),
				     gl:vertex3fv(gb_trees:get(Vb, Vtab)),
				     gl:'end'()
			     end, gb_trees:to_list(Etab))
	     end,
    marquee_draw_1(Draw);
marquee_draw(#st{selmode=vertex}) ->
    Draw = fun(#we{vp=Vtab}) ->
		   foreach(fun({V,Pos}) ->
				   gl:loadName(V),
				   gl:'begin'(?GL_POINTS),
				   gl:vertex3fv(Pos),
				   gl:'end'()
			   end, gb_trees:to_list(Vtab))
	   end,
    marquee_draw_1(Draw);
marquee_draw(St) -> select_draw(St).

marquee_draw_1(Draw) ->
    wings_draw_util:fold(fun(D, _) -> marquee_draw_fun(D, Draw) end, []).

marquee_draw_fun(#dlo{src_we=#we{perm=Perm}}, _) when not ?IS_SELECTABLE(Perm) -> ok;
marquee_draw_fun(#dlo{mirror=Mirror,src_we=#we{id=Id}=We}, Draw) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:pushName(0),
    Draw(We),
    gl:popName(),
    gl:endList(),
    gl:pushName(Id),
    case Mirror of
	none ->
	    wings_draw_util:call(List);
	Matrix ->
	    wings_draw_util:call(List),
	    gl:pushMatrix(),
	    gl:multMatrixf(Matrix),
	    wings_draw_util:call(List),
	    gl:popMatrix()
    end,
    gl:popName(),
    gl:deleteLists(List, 1).

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

select_draw(_) ->
    wings_draw_util:map(fun select_draw_fun/2, []).

select_draw_fun(#dlo{work=Work,src_we=#we{id=Id,perm=Perm}=We}=D, _)
  when ?IS_LIGHT(We), ?IS_SELECTABLE(Perm) ->
    gl:pushName(Id),
    gl:pushName(1),
    wings_draw_util:call(Work),
    gl:popName(),
    gl:popName(),
    D;
select_draw_fun(#dlo{pick=none,src_we=We}=D, _) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    select_draw_1(We),
    gl:endList(),
    draw_dlist(D#dlo{pick=List});
select_draw_fun(D, _) -> draw_dlist(D).

draw_dlist(#dlo{mirror=none,pick=Pick,src_we=#we{id=Id}}=D) ->
    gl:pushName(Id),
    wings_draw_util:call(Pick),
    gl:popName(),
    D;
draw_dlist(#dlo{mirror=Matrix,pick=Pick,src_we=#we{id=Id}}=D) ->
    gl:pushName(Id),
    wings_draw_util:call(Pick),
    gl:loadName(-Id),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    wings_draw_util:call(Pick),
    gl:popMatrix(),
    gl:popName(),
    gl:frontFace(?GL_CCW),
    D.

select_draw_1(#we{perm=Perm}=We) when ?IS_SELECTABLE(Perm) ->
    Tess = wings_draw_util:tess(),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
    case wings_pref:get_value(display_list_opt) of
	false ->
	    select_draw_2(We);
	true ->
	    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_GLBEGIN),
	    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_GLEND),
	    select_draw_2(We),
	    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_NONE),
	    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_NONE)
    end,
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
    gl:edgeFlag(?GL_TRUE);
select_draw_1(_) -> ok.
    
select_draw_2(#we{fs=Ftab}=We) ->
    gl:pushName(0),
    foreach(fun({Face,Edge}) ->
		    gl:loadName(Face),
		    wings_draw_util:flat_face(Face, Edge, We)
	    end, gb_trees:to_list(Ftab)),
    gl:popName().
