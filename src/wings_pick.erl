%%
%%  wings_pick.erl --
%%
%%     This module handles picking using OpenGL.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pick.erl,v 1.29 2002/01/14 08:22:49 bjorng Exp $
%%

-module(wings_pick).
-export([event/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,reverse/2,
		sort/1,foldl/3,map/2,min/1,keysearch/3]).

%% For ordinary picking.
-record(pick,
	{st,					%Saved state.
	 op					%Operation: add/delete
	}).

%% For marque picking.
-record(marque,
	{ox,oy,					%Original X,Y.
	 cx,cy,					%Current X,Y.
	 inside,				%All items must be
						% exactly inside.
	 op=add,				%add/delete
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
event(Ev, St) -> next.

hilite_enabled(vertex) -> wings_pref:get_value(vertex_hilite);
hilite_enabled(edge) -> wings_pref:get_value(edge_hilite);
hilite_enabled(face) -> wings_pref:get_value(face_hilite);
hilite_enabled(body) -> wings_pref:get_value(body_hilite).

pick(X, Y, St0) ->
    {Inside,Marque,MarqueOp} =
	case sdl_keyboard:getModState() of
	    Mod when Mod band ?SHIFT_BITS =/= 0, Mod band ?CTRL_BITS =/= 0 ->
		{true,true,delete};
	    Mod when Mod band ?CTRL_BITS =/= 0 ->
		{false,true,delete};
	    Mod when Mod band ?SHIFT_BITS =/= 0 ->
		{true,true,add};
	    Mod ->
		{false,maybe,add}
	end,
    case Marque of
	true ->
	    wings_io:setup_for_drawing(),
	    Pick = #marque{inside=Inside,op=MarqueOp,ox=X,oy=Y,st=St0},
	    {seq,{push,dummy},get_marque_event(Pick)};
	maybe ->
	    case do_pick(X, Y, St0) of
		none ->
		    wings_io:setup_for_drawing(),
		    Pick = #marque{inside=Inside,op=MarqueOp,ox=X,oy=Y,st=St0},
		    {seq,{push,dummy},get_marque_event(Pick)};
		{PickOp,St} ->
		    wings:redraw(St),
		    Pick = #pick{st=St,op=PickOp},
		    {seq,{push,dummy},get_pick_event(Pick)}
	    end
    end.

%%
%% Highlighting on mouse move.
%%

get_hilite_event(HL) ->
    fun(Ev) -> handle_hilite_event(Ev, HL) end.

handle_hilite_event(#mousemotion{x=X,y=Y}=Mm, #hl{prev=PrevHit,st=St}=HL) ->
    case do_pick_1(X, Y, St) of
	PrevHit ->
	    get_hilite_event(HL);
	none ->
	    wings:redraw(St),
	    get_hilite_event(HL#hl{prev=none});
	Hit ->
	    DrawFun = hilite_draw_sel_fun(Hit, St),
	    wings:redraw(St#st{hilite=DrawFun}),
	    get_hilite_event(HL#hl{prev=Hit})
    end;
handle_hilite_event(Ev, St) -> next.

hilite_draw_sel_fun(Hit, St) ->
    fun() ->
	    hilite_color(Hit, St),
	    #st{selmode=Mode,shapes=Shs} = St,
	    {Id,Item} = Hit,
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
    wings_draw_util:face(Face, We);
hilit_draw_sel(body, _, #we{fs=Ftab}=We) ->
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    foreach(fun({Face,#face{edge=Edge}}) ->
		    wings_draw_util:face(Face, Edge, We)
	    end, gb_trees:to_list(Ftab)).

%%
%% Marque picking.
%%
get_marque_event(Pick) ->
    {replace,fun(Ev) -> marque_event(Ev, Pick) end}.

marque_event(#mousemotion{x=X,y=Y}, #marque{cx=Cx,cy=Cy}=M) ->
    draw_marque(Cx, Cy, M),
    draw_marque(X, Y, M),
    get_marque_event(M#marque{cx=X,cy=Y});
marque_event(#mousebutton{x=X0,y=Y0,button=1,state=?SDL_RELEASED}, M) ->
    #marque{op=Op,inside=Inside,ox=Ox,oy=Oy,st=St0} = M,
    wings_io:cleanup_after_drawing(),
    X = (Ox+X0)/2.0,
    Y = (Oy+Y0)/2.0,
    W = abs(Ox-X)*2.0,
    H = abs(Oy-Y)*2.0,
    St = case marque_pick(Inside, X, Y, W, H, St0) of
	     {none,St1} -> St1;
	     {Hits,St1} ->
		 St2 = marque_update_sel(Op, Hits, St0),
		 wings_io:putback_event({new_selection,St2}),
		 St2
	 end,
    wings:redraw(St),
    pop;
marque_event(Event, Pick) -> keep.

marque_pick(false, X, Y, W, H, St) ->
    pick_all(false, X, Y, W, H, St);
marque_pick(true, X, Y0, W, H, St0) ->
    case pick_all(true, X, Y0, W, H, St0) of
	{none,_}=R -> R;
	{Hits0,St} ->
	    Hits1 = marque_filter(Hits0, St),
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
	    Hits = marque_convert(Hits4, RectData, St, []),
	    {Hits,St}
    end.

marque_filter(Hits, #st{selmode=Mode,shapes=Shs}) ->
    EyePoint = wings_view:eye_point(),
    marque_filter_1(Hits, Shs, Mode, EyePoint, []).

marque_filter_1([{Id,Face}|Hits], Shs, Mode, EyePoint, Acc) ->
    We = gb_trees:get(Id, Shs),
    Vs = [V|_] = wings_face:surrounding_vertices(Face, We),
    N = wings_face:face_normal(Vs, We),
    D = e3d_vec:dot(wings_vertex:pos(V, We), N),
    case e3d_vec:dot(EyePoint, N) of
	S when S < D ->				%Ignore back-facing face.
	    marque_filter_1(Hits, Shs, Mode, EyePoint, Acc);
	S ->					%Front-facing face.
	    marque_filter_1(Hits, Shs, Mode, EyePoint, [{Id,Face}|Acc])
    end;
marque_filter_1([], St, Mode, EyePoint, Acc) -> Acc.

marque_convert([{Id,Faces}|Hits], RectData,
	       #st{selmode=Mode,shapes=Shs}=St, Acc) ->
    We = gb_trees:get(Id, Shs),
    case marque_convert_1(Faces, Mode, RectData, We) of
	[] ->
	    marque_convert(Hits, RectData, St, Acc);
	Items ->
	    marque_convert(Hits, RectData, St, [{Id,Items}|Acc])
    end;
marque_convert([], RectData, St, Hits) ->
    sofs:to_external(sofs:family_to_relation(sofs:family(Hits))).

marque_convert_1(Faces0, face, Rect, #we{vs=Vtab}=We) ->
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
marque_convert_1(Faces, vertex, Rect, #we{vs=Vtab}=We) ->
    Vs0 = wings_face:fold_faces(fun(_, V, E, _, A) ->
					[V|A]
				end, [], Faces, We),
    Vs = ordsets:from_list(Vs0),
    [V || V <- Vs, is_inside_rect(pos(V, Vtab), Rect)];
marque_convert_1(Faces, edge, Rect, #we{vs=Vtab}=We) ->
    Es0 = wings_face:fold_faces(fun(_, _, E, Rec, A) ->
					[{E,Rec}|A]
				end, [], Faces, We),
    Es = ordsets:from_list(Es0),
    [E || {E,#edge{vs=Va,ve=Vb}} <- Es,
	  is_all_inside_rect([pos(Va, Vtab),pos(Vb, Vtab)], Rect)];
marque_convert_1(Faces, body, Rect, #we{vs=Vtab}=We) ->
    case is_all_inside_rect(gb_trees:values(Vtab), Rect) of
	true -> [0];
	false -> []
    end.

is_all_inside_rect([#vtx{pos=P}|Ps], Rect) ->
    is_inside_rect(P, Rect) andalso is_all_inside_rect(Ps, Rect);
is_all_inside_rect([P|Ps], Rect) ->
    is_inside_rect(P, Rect) andalso is_all_inside_rect(Ps, Rect);
is_all_inside_rect([], Rect) -> true.

is_inside_rect({Px,Py,Pz}, {MM,PM,ViewPort,X1,Y1,X2,Y2}) ->
    R = {true,Sx,Sy,_} = glu:project(Px, Py, Pz, MM, PM, ViewPort),
    X1 < Sx andalso Sx < X2 andalso
	Y1 < Sy andalso Sy < Y2.

draw_marque(undefined, undefined, _) -> ok;
draw_marque(X, Y, #marque{ox=Ox,oy=Oy}) ->
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

marque_update_sel(Op, Hits0, #st{selmode=body}=St) ->
    Hits1 = sofs:relation(Hits0, [{id,data}]),
    Hits2 = sofs:domain(Hits1),
    Zero = sofs:from_term([0], [data]),
    Hits = sofs:constant_function(Hits2, Zero),
    marque_update_sel_1(Op, Hits, St);
marque_update_sel(Op, Hits0, #st{sel=Sel}=St) ->
    Hits1 = sofs:relation(Hits0, [{id,data}]),
    Hits = sofs:relation_to_family(Hits1),
    marque_update_sel_1(Op, Hits, St).

marque_update_sel_1(add, Hits, #st{sel=Sel0}=St) ->
    Sel1 = [{Id,gb_sets:to_list(Items)} || {Id,Items} <- Sel0],
    Sel2 = sofs:from_external(Sel1, [{id,[data]}]),
    Sel3 = sofs:family_union(Sel2, Hits),
    Sel4 = sofs:to_external(Sel3),
    Sel = [{Id,gb_sets:from_list(Items)} || {Id,Items} <- Sel4],
    St#st{sel=Sel};
marque_update_sel_1(delete, Hits, #st{sel=Sel0}=St) ->
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

pick_event(#mousemotion{x=X,y=Y}, #pick{op=Op,st=St0}=Pick) ->
    case do_pick(X, Y, St0) of
	none -> keep;
	{Op,St} ->
	    wings:redraw(St),
	    get_pick_event(Pick#pick{st=St});
	{_,_} -> keep
    end;
pick_event(#mousebutton{button=1,state=?SDL_RELEASED}, #pick{st=St}) ->
    wings_io:putback_event({new_selection,St}),
    pop;
pick_event(Event, Pick) -> keep.

do_pick(X, Y, St) ->
    case do_pick_1(X, Y, St) of
	none -> none;
	Hit -> update_selection(Hit, St)
    end.

do_pick_1(X0, Y0, #st{shapes=Shapes,selmode=Mode}=St) ->
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

update_selection({Id,Item}, #st{sel=Sel0}=St) ->
    {Type,Sel} = update_selection(Id, Item, Sel0, []),
    {Type,St#st{sel=Sel}}.

update_selection(Id, Item, [{I,_}=H|T], Acc) when Id > I ->
    update_selection(Id, Item, T, [H|Acc]);
update_selection(Id, Item, [{I,_}|_]=T, Acc) when Id < I ->
    {add,reverse(Acc, [{Id,gb_sets:singleton(Item)}|T])};
update_selection(Id, Item, [{I,Items0}|T0], Acc) -> %Id == I
    ?ASSERT(Id == I),
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

get_name(0, Tail, Acc) -> list_to_tuple(reverse(Acc));
get_name(N, <<Name:32,Names/binary>>, Acc) ->
    get_name(N-1, Names, [Name|Acc]).

%%%
%%% Filter hits to obtain just one hit.
%%%

filter_hits(Hits, X, Y, #st{selmode=Mode,shapes=Shs}) ->
    EyePoint = wings_view:eye_point(),
    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint, none).

filter_hits_1([{Id,Face}|Hits], Shs, Mode, X, Y, EyePoint, Hit0) ->
    We = gb_trees:get(Id, Shs),
    Vs = [V|_] = wings_face:surrounding_vertices(Face, We),
    N = wings_face:face_normal(Vs, We),
    D = e3d_vec:dot(wings_vertex:pos(V, We), N),
    case e3d_vec:dot(EyePoint, N) of
	S when S < D ->				%Ignore back-facing face.
	    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint, Hit0);
	S ->					%Candidate.
	    Hit = best_hit(Id, Face, Vs, We, EyePoint, Hit0),
	    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint, Hit)
    end;
filter_hits_1([], St, Mode, X, Y, EyePoint, none) -> none;
filter_hits_1([], St, Mode, X, Y, EyePoint, {_,{Id,Face,We}}) ->
    convert_hit(Mode, X, Y, Id, Face, We).

best_hit(Id, Face, Vs, We, EyePoint, Hit0) ->
    Center = wings_vertex:center(Vs, We),
    D = e3d_vec:sub(Center, EyePoint),
    DistSqr = e3d_vec:dot(D, D),
    case Hit0 of
	none ->
	    {DistSqr,{Id,Face,We}};
	{DistSqr0,_} when DistSqr < DistSqr0 ->
	    {DistSqr,{Id,Face,We}};
	Other ->
	    Hit0
    end.

%%
%% Given a selection hit, return the correct vertex/edge/face/body.
%%

convert_hit(body, X, Y, Id, Face, We) -> {Id,0};
convert_hit(face, X, Y, Id, Face, We) -> {Id,Face};
convert_hit(Mode, X, Y, Id, Face, We) ->
    wings_view:projection(),
    wings_view:model_transformations(),
    ViewPort = gl:getIntegerv(?GL_VIEWPORT),
    ModelMatrix = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    ProjMatrix = gl:getDoublev(?GL_PROJECTION_MATRIX),
    Trans = {ModelMatrix,ProjMatrix,ViewPort},
    case Mode of
	vertex -> {Id,find_vertex(Face, We, X, Y, Trans)};
	edge ->   {Id,find_edge(Face, We, X, Y, Trans)}
    end.

find_vertex(Face, We, X, Y, Trans) ->
    Vs0 = wings_face:surrounding_vertices(Face, We),
    Vs = map(fun(V) ->
		     {Xs,Ys} = project_vertex(V, We, Trans),
		     Dx = X-Xs,
		     Dy = Y-Ys,
		     {Dx*Dx+Dy*Dy,V}
	     end, Vs0),
    {_,V} = min(Vs),
    V.

find_edge(Face, We, Cx, Cy, Trans) ->
    Es = wings_face:fold(
	   fun(_, Edge, #edge{vs=Va,ve=Vb}, A) ->
		   {Ax,Ay} = project_vertex(Va, We, Trans),
		   {Bx,By} = project_vertex(Vb, We, Trans),
		   if
		       is_float(Ax), is_float(Ay),
		       is_float(Bx), is_float(By) ->
			   Xdist = Bx-Ax,
			   Ydist = By-Ay,
			   L = Xdist*Xdist+Ydist*Ydist,
			   {Px,Py} = case ((Cx-Ax)*Xdist+(Cy-Ay)*Ydist)/L of
					 R when R =< 0 -> {Ax,Ay};
					 R when R >= 1 -> {Bx,By};
					 R -> {Ax+R*Xdist,Ay+R*Ydist}
				     end,
			   Xdiff = Px-Cx,
			   Ydiff = Py-Cy,
			   DistSqr = Xdiff*Xdiff + Ydiff*Ydiff,
			   [{DistSqr,Edge}|A]
		   end
	   end, [], Face, We),
    {_,Edge} = min(Es),
    Edge.

project_vertex(V, We, {ModelMatrix,ProjMatrix,ViewPort}) ->
    {Px,Py,Pz} = wings_vertex:pos(V, We),
    {true,Xs,Ys,_} = glu:project(Px, Py, Pz, ModelMatrix,
				 ProjMatrix, ViewPort),
    {Xs,Ys}.

%%
%% Pick all in the given rectangle (with center at X,Y).
%%

pick_all(DrawFaces, X, Y, W, H, St) when W < 1.0; H < 1.0 -> {none,St};
pick_all(DrawFaces, X0, Y0, W, H, #st{shapes=Shapes,selmode=Mode}=St0) ->
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
    St = case DrawFaces of
	     true ->
		 select_draw(St0);
	     false ->
		 marque_draw(St0),
		 St0
	 end,
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> {none,St};
	NumHits ->
 	    HitData = sdl_util:readBin(HitBuf, 5*NumHits),
 	    {get_hits(NumHits, HitData, []),St}
    end.

marque_draw(#st{selmode=edge}=St) ->
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
marque_draw(#st{selmode=vertex}=St) ->
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
marque_draw(St) ->
    foreach_we(fun(#we{perm=Perm}) when ?IS_NOT_SELECTABLE(Perm) -> ok;
		  (#we{fs=Ftab}=We) ->
		       gl:pushName(0),
		       foreach(fun({Face,#face{edge=Edge}}) ->
				       gl:loadName(Face),
				       draw_face(Face, Edge, We)
			       end, gb_trees:to_list(Ftab)),
		       gl:popName()
	       end, St).

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

select_draw(St) ->
    Dlist = select_draw_0(St),
    gl:callList(Dlist),
    St.

select_draw_0(St) ->
    case wings_draw:get_dlist() of
	#dl{pick=none}=DL ->
	    Dlist = ?DL_PICK,
	    gl:newList(Dlist, ?GL_COMPILE),
	    select_draw_1(St),
	    gl:endList(),
	    wings_draw:put_dlist(DL#dl{pick=Dlist}),
	    Dlist;
	#dl{pick=Dlist} -> Dlist
    end.

select_draw_1(St) ->
    foreach_we(fun(#we{fs=Ftab,perm=Perm}=We) when ?IS_SELECTABLE(Perm) ->
		       gl:pushName(0),
		       foreach(fun({Face,#face{edge=Edge}}) ->
				       gl:loadName(Face),
				       draw_face(Face, Edge, We)
			       end, gb_trees:to_list(Ftab)),
		       gl:popName();
		  (#we{}) -> ok
	       end, St).

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
	none -> ok;
	{Id,We,Iter} ->
	    gl:pushName(Id),
	    F(We),
	    gl:popName(),
	    foreach_we_1(F, Iter)
    end.

draw_face(Face, Edge, We) ->
    case wings_face:draw_info(Face, Edge, We) of
	[_,_,_,_,_|_]=Vs ->
	    {X,Y,Z} = N = wings_face:draw_normal(Vs),
	    Tess = wings_draw_util:tess(),
	    glu:tessNormal(Tess, X, Y, Z),
	    glu:tessBeginPolygon(Tess),
	    glu:tessBeginContour(Tess),
	    foreach(fun({Pos,Col}) ->
			    glu:tessVertex(Tess, Pos)
		    end, Vs),
	    glu:tessEndContour(Tess),
	    glu:tessEndPolygon(Tess),
	    gl:edgeFlag(?GL_TRUE);
	Vs ->
	    gl:'begin'(?GL_POLYGON),
	    foreach(fun({Pos,Col}) -> gl:vertex3fv(Pos) end, Vs),
	    gl:'end'()
    end.
