%%
%%  wings_pick.erl --
%%
%%     This module handles picking using OpenGL.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pick.erl,v 1.46 2002/05/12 05:00:53 bjorng Exp $
%%

-module(wings_pick).
-export([event/2]).
-export([do_pick/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,reverse/2,
		sort/1,foldl/3,map/2,min/1,keysearch/3,member/2]).

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
	 prev=none				%Previous hit ({Id,Item}).
	}).

event(#mousemotion{}=Mm, #st{selmode=Mode}=St) ->
    case hilite_enabled(Mode) of
	false -> next;
	true -> {seq,{push,dummy},handle_hilite_event(Mm, #hl{st=St})}
    end;
event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, St) ->
    pick(X, Y, St);
event(_, _) -> next.

hilite_enabled(vertex) -> wings_pref:get_value(vertex_hilite);
hilite_enabled(edge) -> wings_pref:get_value(edge_hilite);
hilite_enabled(face) -> wings_pref:get_value(face_hilite);
hilite_enabled(body) -> wings_pref:get_value(body_hilite).

pick(X, Y, St0) ->
    Mod = sdl_keyboard:getModState(),
    if 
	Mod band (?SHIFT_BITS bor ?CTRL_BITS) =/= 0 ->
	    Pick = #marquee{ox=X,oy=Y,st=St0},
	    marquee_mode(Pick);
	true ->
	    case do_pick(X, Y, St0) of
		none ->
		    Pick = #marquee{ox=X,oy=Y,st=St0},
		    marquee_mode(Pick);
		{PickOp,St} ->
		    wings_wm:dirty(),
		    Pick = #pick{st=St,op=PickOp},
		    {seq,{push,dummy},get_pick_event(Pick)}
	    end
    end.

%%
%% Highlighting on mouse move.
%%

get_hilite_event(HL) ->
    fun(Ev) -> handle_hilite_event(Ev, HL) end.

handle_hilite_event(redraw, #hl{st=St}) ->
    wings:redraw(St),
    keep;
handle_hilite_event(#mousemotion{x=X,y=Y}, #hl{prev=PrevHit,st=St}=HL) ->
    case do_pick_1(X, Y, St) of
	PrevHit ->
	    get_hilite_event(HL);
	none ->
	    insert_hilite_fun(none, none),
	    wings_wm:dirty(),
	    get_hilite_event(HL#hl{prev=none});
	Hit ->
	    wings_wm:dirty(),
	    DrawFun = hilite_draw_sel_fun(Hit, St),
	    insert_hilite_fun(Hit, DrawFun),
	    get_hilite_event(HL#hl{prev=Hit})
    end;
handle_hilite_event(_, _) ->
    insert_hilite_fun(none, none),
    next.

insert_hilite_fun(Hit, DrawFun) ->
    wings_draw_util:update(fun(D, _) ->
				   insert_hilite(D, Hit, DrawFun)
			   end, []).

insert_hilite(eol, _, _) -> eol;
insert_hilite(#dlo{src_we=#we{id=Id}}=D, {_,{Id,_}}, DrawFun) ->
    {D#dlo{hilite=DrawFun},[]};
insert_hilite(D, _, _) -> {D#dlo{hilite=none},[]}.

hilite_draw_sel_fun({Mode,{Id,Item}=Hit}, St) ->
    fun() ->
	    hilite_color(Hit, St),
	    #st{shapes=Shs} = St,
	    We = gb_trees:get(Id, Shs),
	    hilit_draw_sel(Mode, Item, We)
    end.

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

hilit_draw_sel(vertex, V, #we{vs=Vtab}) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:'begin'(?GL_POINTS),
    #vtx{pos=Pos} = gb_trees:get(V, Vtab),
    gl:vertex3fv(Pos),
    gl:'end'();
hilit_draw_sel(edge, Edge, #we{es=Etab,vs=Vtab}) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:'begin'(?GL_LINES),
    gl:vertex3fv(pos(Va, Vtab)),
    gl:vertex3fv(pos(Vb, Vtab)),
    gl:'end'();
hilit_draw_sel(face, Face, We) ->
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_draw_util:begin_end(fun() ->
				      wings_draw_util:flat_face(Face, We)
			      end);
hilit_draw_sel(body, _, #we{fs=Ftab}=We) ->
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_draw_util:begin_end(
      fun() ->
	      foreach(fun({Face,#face{edge=Edge}}) ->
			      wings_draw_util:face(Face, Edge, We)
		      end, gb_trees:to_list(Ftab))
      end).

%%
%% Marquee picking.
%%

marquee_mode(Pick) ->
    wings_io:setup_for_drawing(),
    wings_io:draw_message(
      fun() ->
	      Message = "[Ctrl] Deselect  "
		  "[Shift] (De)select only elements wholly inside marquee",
	      wings_io:text_at(0, Message)
      end),
    {seq,{push,dummy},get_marquee_event(Pick)}.

get_marquee_event(Pick) ->
    {replace,fun(Ev) -> marquee_event(Ev, Pick) end}.

marquee_event(#mousemotion{x=X,y=Y}, #marquee{cx=Cx,cy=Cy}=M) ->
    draw_marquee(Cx, Cy, M),
    draw_marquee(X, Y, M),
    get_marquee_event(M#marquee{cx=X,cy=Y});
marquee_event(#mousebutton{x=X0,y=Y0,button=1,state=?SDL_RELEASED}, M) ->
    {Inside,Op} =
	case sdl_keyboard:getModState() of
	    Mod when Mod band ?SHIFT_BITS =/= 0, Mod band ?CTRL_BITS =/= 0 ->
		{true,delete};
	    Mod when Mod band ?CTRL_BITS =/= 0 ->
		{false,delete};
	    Mod when Mod band ?SHIFT_BITS =/= 0 ->
		{true,add};
	    _Mod -> {false,add}
	end,
    #marquee{ox=Ox,oy=Oy,st=St0} = M,
    wings_io:cleanup_after_drawing(),
    X = (Ox+X0)/2.0,
    Y = (Oy+Y0)/2.0,
    W = abs(Ox-X)*2.0,
    H = abs(Oy-Y)*2.0,
    case marquee_pick(Inside, X, Y, W, H, St0) of
	{none,_} -> ok;
	{Hits,_} ->
	    St = marquee_update_sel(Op, Hits, St0),
	    wings_io:putback_event({new_state,St})
    end,
    wings_wm:dirty(),
    pop;
marquee_event(_, _) -> keep.

marquee_pick(false, X, Y, W, H, St) ->
    pick_all(false, X, Y, W, H, St);
marquee_pick(true, X, Y0, W, H, St0) ->
    case pick_all(true, X, Y0, W, H, St0) of
	{none,_}=R -> R;
	{Hits0,St} ->
	    Hits1 = marquee_filter(Hits0, St),
	    Hits2 = sofs:relation(Hits1),
	    Hits3 = sofs:relation_to_family(Hits2),
	    Hits4 = sofs:to_external(Hits3),
	    wings_view:projection(),
	    wings_view:model_transformations(),
	    MM = gl:getDoublev(?GL_MODELVIEW_MATRIX),
	    PM = gl:getDoublev(?GL_PROJECTION_MATRIX),
	    [_,_,_,Wh] = ViewPort = gl:getIntegerv(?GL_VIEWPORT),
	    Y = Wh - Y0,
	    RectData = {MM,PM,ViewPort,X-W/2,Y-H/2,
			X+W/2,Y+H/2},
	    Hits = marquee_convert(Hits4, RectData, St, []),
	    {Hits,St}
    end.

marquee_filter(Hits, #st{selmode=Mode,shapes=Shs}) ->
    EyePoint = wings_view:eye_point(),
    marquee_filter_1(Hits, Shs, Mode, EyePoint, []).

marquee_filter_1([{Id,Face}|Hits], Shs, Mode, EyePoint, Acc) ->
    We = gb_trees:get(Id, Shs),
    Vs = [V|_] = wings_face:surrounding_vertices(Face, We),
    N = wings_face:face_normal(Vs, We),
    D = e3d_vec:dot(wings_vertex:pos(V, We), N),
    case e3d_vec:dot(EyePoint, N) of
	S when S < D ->				%Ignore back-facing face.
	    marquee_filter_1(Hits, Shs, Mode, EyePoint, Acc);
	_S ->					%Front-facing face.
	    marquee_filter_1(Hits, Shs, Mode, EyePoint, [{Id,Face}|Acc])
    end;
marquee_filter_1([], _St, _Mode, _EyePoint, Acc) -> Acc.

marquee_convert([{Id,Faces}|Hits], RectData,
	       #st{selmode=Mode,shapes=Shs}=St, Acc) ->
    We = gb_trees:get(Id, Shs),
    case marquee_convert_1(Faces, Mode, RectData, We) of
	[] ->
	    marquee_convert(Hits, RectData, St, Acc);
	Items ->
	    marquee_convert(Hits, RectData, St, [{Id,Items}|Acc])
    end;
marquee_convert([], _RectData, _St, Hits) ->
    sofs:to_external(sofs:family_to_relation(sofs:family(Hits))).

marquee_convert_1(Faces0, face, Rect, #we{vs=Vtab}=We) ->
    Vfs0 = wings_face:fold_faces(
	     fun(Face, V, _, _, A) ->
		     [{V,Face}|A]
	     end, [], Faces0, We),
    Vfs1 = sofs:relation(Vfs0, [{vertex,face}]),
    Vfs2 = sofs:relation_to_family(Vfs1),
    Vfs = sofs:to_external(Vfs2),
    Kill0 = [Fs || {V,Fs} <- Vfs, not is_inside_rect(pos(V, Vtab), Rect)],
    Kill1 = sofs:set(Kill0, [[face]]),
    Kill = sofs:union(Kill1),
    Faces1 = sofs:from_external(Faces0, [face]),
    Faces = sofs:difference(Faces1, Kill),
    sofs:to_external(Faces);
marquee_convert_1(Faces, vertex, Rect, #we{vs=Vtab}=We) ->
    Vs0 = wings_face:fold_faces(fun(_, V, _, _, A) ->
					[V|A]
				end, [], Faces, We),
    Vs = ordsets:from_list(Vs0),
    [V || V <- Vs, is_inside_rect(pos(V, Vtab), Rect)];
marquee_convert_1(Faces, edge, Rect, #we{vs=Vtab}=We) ->
    Es0 = wings_face:fold_faces(fun(_, _, E, Rec, A) ->
					[{E,Rec}|A]
				end, [], Faces, We),
    Es = ordsets:from_list(Es0),
    [E || {E,#edge{vs=Va,ve=Vb}} <- Es,
	  is_all_inside_rect([pos(Va, Vtab),pos(Vb, Vtab)], Rect)];
marquee_convert_1(_Faces, body, Rect, #we{vs=Vtab}) ->
    case is_all_inside_rect(gb_trees:values(Vtab), Rect) of
	true -> [0];
	false -> []
    end.

is_all_inside_rect([#vtx{pos=P}|Ps], Rect) ->
    is_inside_rect(P, Rect) andalso is_all_inside_rect(Ps, Rect);
is_all_inside_rect([P|Ps], Rect) ->
    is_inside_rect(P, Rect) andalso is_all_inside_rect(Ps, Rect);
is_all_inside_rect([], _Rect) -> true.

is_inside_rect({Px,Py,Pz}, {MM,PM,ViewPort,X1,Y1,X2,Y2}) ->
    {true,Sx,Sy,_} = glu:project(Px, Py, Pz, MM, PM, ViewPort),
    X1 < Sx andalso Sx < X2 andalso
	Y1 < Sy andalso Sy < Y2.

draw_marquee(undefined, undefined, _) -> ok;
draw_marquee(X, Y, #marquee{ox=Ox,oy=Oy}) ->
    gl:color3f(1.0, 1.0, 1.0),
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

marquee_update_sel_1(add, Hits, #st{sel=Sel0}=St) ->
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
	{Op,St} ->
	    wings_wm:dirty(),
	    get_pick_event(Pick#pick{st=St});
	{_,_} -> keep
    end;
pick_event(#mousebutton{button=1,state=?SDL_RELEASED}, #pick{st=St}) ->
    wings_io:putback_event({new_state,St}),
    pop;
pick_event(_, _) -> keep.

do_pick(X, Y, St) ->
    case do_pick_1(X, Y, St) of
	none -> none;
	Hit -> update_selection(Hit, St)
    end.

do_pick_1(X0, Y0, St) ->
    HitBuf = get(wings_hitbuf),
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    X = float(X0),
    Y = H-float(Y0),
    S = 5.0,
    glu:pickMatrix(X, Y, S, S, [0,0,W,H]),
    wings_view:perspective(),
    wings_view:model_transformations(),
    select_draw(St),
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> none;
	NumHits ->
	    HitData = sdl_util:readBin(HitBuf, 5*NumHits),
	    Hits = get_hits(NumHits, HitData, []),
	    case filter_hits(Hits, X, Y, St) of
		none -> none;
		Hit -> Hit
	    end
    end.

update_selection({Mode,{Id,Item}}, #st{sel=Sel0}=St) ->
    {Type,Sel} = update_selection(Id, Item, Sel0, []),
    {Type,St#st{selmode=Mode,sel=Sel}}.

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

get_hits(0, _, Acc) -> Acc;
get_hits(N, <<NumNames:32,_:32,_:32,Tail0/binary>>, Acc) ->
    <<Names:NumNames/binary-unit:32,Tail/binary>> = Tail0,
    Name = get_name(NumNames, Names, []),
    get_hits(N-1, Tail, [Name|Acc]).

get_name(0, _Tail, Acc) -> list_to_tuple(reverse(Acc));
get_name(N, <<Name:32/signed,Names/binary>>, Acc) ->
    get_name(N-1, Names, [Name|Acc]).

%%%
%%% Filter hits to obtain just one hit.
%%%

filter_hits(Hits, X, Y, #st{selmode=Mode0,shapes=Shs,sel=Sel}) ->
    Mode = case Sel of
	       [] when Mode0 =/= body ->
		   case wings_pref:get_value(smart_highlighting) of
		       true -> auto;
		       false -> Mode0
		   end;
	       _ -> Mode0
	   end,
    EyePoint = wings_view:eye_point(),
    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint, none).

filter_hits_1([{Id,Face}|Hits], Shs, Mode, X, Y, EyePoint, Hit0) ->
    Mtx = if 
	      Id < 0 ->
		  e3d_mat:compress(mirror_matrix(Id));
	      true -> identity
	  end,
    We = gb_trees:get(abs(Id), Shs),
    Vs = wings_face:surrounding_vertices(Face, We),
    Ps0 = [wings_vertex:pos(V, We) || V <- Vs],
    [P|_] = Ps = mul_points(Mtx, Ps0),
    N = if
	    Id < 0 -> e3d_vec:neg(e3d_vec:normal(Ps));
	    true -> e3d_vec:normal(Ps)
	end,
    D = e3d_vec:dot(P, N),
    case e3d_vec:dot(EyePoint, N) of
 	S when S < D ->				%Ignore back-facing face.
 	    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint, Hit0);
	_S ->					%Candidate.
	    Hit = best_hit(Id, Face, Vs, We, EyePoint, Mtx, Hit0),
	    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint, Hit)
    end;
filter_hits_1([], _Shs, _Mode, _X, _Y, _EyePoint, none) -> none;
filter_hits_1([], _Shs, Mode, X, Y, _EyePoint, {_,{Id,Face,We}}) ->
    convert_hit(Mode, X, Y, Id, Face, We).

mul_point(identity, P) -> P;
mul_point(Mtx, P) -> e3d_mat:mul_point(Mtx, P).

mul_points(identity, Ps) -> Ps;
mul_points(Mtx, Ps) -> [e3d_mat:mul_point(Mtx, P) || P <- Ps].
    
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

convert_hit(body, _X, _Y, Id, _Face, _We) -> {body,{abs(Id),0}};
convert_hit(face, _X, _Y, Id, Face, _We) -> {face,{abs(Id),Face}};
convert_hit(auto, X, Y, Id0, Face, We) ->
    Id = abs(Id0),
    Trans = get_matrices(Id0),
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
		 Vdist < Lim -> {vertex,{Id,V}};
		 Edist < Lim -> {edge,{Id,Edge}};
		 true -> {face,{Id,Face}}
	     end,
    check_restriction(Hilite, Id, V, Edge, Face);
convert_hit(Mode, X, Y, Id0, Face, We) ->
    Id = abs(Id0),
    Trans = get_matrices(Id0),
    case Mode of
	vertex ->
	    {_,_,V} = min(find_vertex(Face, We, X, Y, Trans)),
	    {vertex,{Id,V}};
	edge ->
	    {_,_,E} = min(find_edge(Face, We, X, Y, Trans)),
	    {edge,{Id,E}}
    end.

get_matrices(Id) ->
    wings_view:projection(),
    wings_view:model_transformations(),
    if
	Id < 0 ->
	    Matrix = mirror_matrix(Id),
	    gl:multMatrixf(Matrix);
	true -> ok
    end,
    ViewPort = gl:getIntegerv(?GL_VIEWPORT),
    ModelMatrix = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    ProjMatrix = gl:getDoublev(?GL_PROJECTION_MATRIX),
    {ModelMatrix,ProjMatrix,ViewPort}.

mirror_matrix(Id) ->
    wings_draw_util:fold(fun mirror_matrix/2, abs(Id)).

mirror_matrix(#dlo{mirror=Matrix,src_we=#we{id=Id}}, Id) -> Matrix;
mirror_matrix(_, Acc) -> Acc.
    
find_vertex(Face, We, X, Y, Trans) ->
    Vs0 = wings_face:surrounding_vertices(Face, We),
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
    {true,Xs,Ys,_} = glu:project(Px, Py, Pz, ModelMatrix,
				 ProjMatrix, ViewPort),
    {Xs,Ys}.

check_restriction({Mode,_}=Hilite, Id, V, Edge, Face) ->
    case wings_io:get_icon_restriction() of
	all -> Hilite;
	Modes ->
	    case member(Mode, Modes) of
		true -> Hilite;
		false -> restrict_hilite(Mode, Modes, Id, V, Edge, Face)
	    end
    end.

restrict_hilite(vertex, Modes, Id, _V, Edge, Face) ->
    case member(edge, Modes) of
	true -> {edge,{Id,Edge}};
	false ->
	    true = member(face, Modes),
	    {face,{Id,Face}}
    end;
restrict_hilite(edge, Modes, Id, V, _Edge, Face) ->
    case member(vertex, Modes) of
	true -> {vertex,{Id,V}};
	false ->
	    true = member(face, Modes),
	    {face,{Id,Face}}
    end;
restrict_hilite(face, Modes, Id, V, Edge, _Face) ->
    case member(edge, Modes) of
	true -> {edge,{Id,Edge}};
	false ->
	    true = member(vertex, Modes),
	    {vertex,{Id,V}}
    end.
	    
%%
%% Pick all in the given rectangle (with center at X,Y).
%%

pick_all(_DrawFaces, _X, _Y, W, H, St) when W < 1.0; H < 1.0 ->
    {none,St};
pick_all(DrawFaces, X0, Y0, W, H, St0) ->
    HitBuf = get(wings_hitbuf),
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    [_,_,_,Wh] = ViewPort = gl:getIntegerv(?GL_VIEWPORT),
    X = float(X0),
    Y = Wh-float(Y0),
    glu:pickMatrix(X, Y, W, H, ViewPort),
    wings_view:perspective(),
    wings_view:model_transformations(),
    case DrawFaces of
	true -> select_draw(St0);
	false -> marquee_draw(St0)
    end,
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> {none,St0};
	NumHits ->
 	    HitData = sdl_util:readBin(HitBuf, 5*NumHits),
 	    {get_hits(NumHits, HitData, []),St0}
    end.

marquee_draw(#st{selmode=edge}=St) ->
    foreach_we(
      fun(#we{perm=Perm}) when ?IS_NOT_SELECTABLE(Perm) -> ok;
	 (#we{es=Etab,vs=Vtab}) ->
	      gl:pushName(0),
	      foreach(fun({Edge,#edge{vs=Va,ve=Vb}}) ->
			      gl:loadName(Edge),
			      gl:'begin'(?GL_LINES),
			      gl:vertex3fv(pos(Va, Vtab)),
			      gl:vertex3fv(pos(Vb, Vtab)),
			      gl:'end'()
		      end, gb_trees:to_list(Etab)),
	      gl:popName()
      end, St);
marquee_draw(#st{selmode=vertex}=St) ->
    foreach_we(fun(#we{perm=Perm}) when ?IS_NOT_SELECTABLE(Perm) -> ok;
		  (#we{vs=Vtab}) ->
		       gl:pushName(0),
		       foreach(fun({V,#vtx{pos=Pos}}) ->
				       gl:loadName(V),
				       gl:'begin'(?GL_POINTS),
				       gl:vertex3fv(Pos),
				       gl:'end'()
			       end, gb_trees:to_list(Vtab)),
		       gl:popName()
	       end, St);
marquee_draw(St) -> select_draw(St).

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

select_draw(_) ->
    wings_draw_util:update(fun select_draw/2, []).

select_draw(eol, _) -> eol;
select_draw(#dlo{pick=none,src_we=We}=D, _) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    select_draw_1(We),
    gl:endList(),
    draw_dlist(D#dlo{pick=List});
select_draw(D, _) -> draw_dlist(D).

draw_dlist(#dlo{mirror=none,pick=Pick,src_we=#we{id=Id}}=D) ->
    gl:pushName(Id),
    gl:callList(Pick),
    gl:popName(),
    D;
draw_dlist(#dlo{mirror=Matrix,pick=Pick,src_we=#we{id=Id}}=D) ->
    gl:pushName(Id),
    gl:callList(Pick),
    gl:loadName(-Id),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    gl:callList(Pick),
    gl:popMatrix(),
    gl:popName(),
    D.

select_draw_1(#we{perm=Perm}=We) when ?IS_SELECTABLE(Perm) ->
    case wings_pref:get_value(display_list_opt) of
 	false -> select_draw_nonopt(We);
	true ->  select_draw_opt(We)
    end,
    gl:edgeFlag(?GL_TRUE);
select_draw_1(_) -> ok.
    
select_draw_opt(#we{fs=Ftab}=We) ->
    gl:pushName(0),
    foreach(fun({Face,#face{edge=Edge}}) ->
		    gl:loadName(Face),
		    gl:'begin'(?GL_TRIANGLES),
		    draw_face(Face, Edge, We),
		    gl:'end'()
	    end, gb_trees:to_list(Ftab)),
    gl:popName().

select_draw_nonopt(#we{fs=Ftab}=We) ->
    gl:pushName(0),
    foreach(fun({Face,#face{edge=Edge}}) ->
		    gl:loadName(Face),
		    draw_face(Face, Edge, We)
	    end, gb_trees:to_list(Ftab)),
    gl:popName().

%%
%% Utilities.
%%

pos(Key, Tree) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tree),
    Pos.

foreach_we(F, #st{shapes=Shapes}) ->
    Iter = gb_trees:iterator(Shapes),
    foreach_we_1(F, Iter).

foreach_we_1(F, Iter0) ->
    case gb_trees:next(Iter0) of
	none -> gl:edgeFlag(?GL_TRUE);
	{Id,We,Iter} ->
	    gl:pushName(Id),
	    F(We),
	    gl:popName(),
	    foreach_we_1(F, Iter)
    end.

draw_face(Face, Edge, We) ->
    Vs = wings_face:draw_info(Face, Edge, We),
    {X,Y,Z} = wings_face:draw_normal(Vs),
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({Pos,_}) ->
		    glu:tessVertex(Tess, Pos)
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).
