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
%%     $Id: wings_pick.erl,v 1.9 2001/11/23 14:37:53 bjorng Exp $
%%

-module(wings_pick).
-export([pick/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,reverse/2,
		sort/1,foldl/3,map/2,min/1]).

-record(pick,
	{st,					%Saved state.
	 op					%add/delete
	}).

pick(X, Y, St0) ->
    case do_pick(X, Y, St0) of
	none ->
	    Pick = #pick{st=St0},
	    {seq,{push,dummy},get_marque_event(Pick)};
	{Op,St} ->
	    wings:redraw(St),
	    Pick = #pick{st=St,op=Op},
	    {seq,{push,dummy},get_pick_event(Pick)}
    end.

%%
%% Marque picking.
%%
get_marque_event(Pick) ->
    {replace,fun(Ev) -> marque_event(Ev, Pick) end}.

marque_event(#mousebutton{button=1,state=?SDL_RELEASED}, Pick) -> pop;
marque_event(Event, Pick) -> keep.

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

do_pick(X0, Y0, #st{hit_buf=HitBuf,shapes=Shapes,selmode=Mode}=St0) ->
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),

    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    X = float(X0),
    Y = H-float(Y0),
    S = 1.0,
    glu:pickMatrix(X, Y, S, S, [0,0,W,H]),
    wings_view:perspective(),
    St = select_draw(St0),

    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> none;
	NumHits ->
	    HitData = sdl_util:readBin(HitBuf, ?HIT_BUF_SIZE),
	    Hits = get_hits(NumHits, HitData, []),
	    case filter_hits(Hits, X, Y, St) of
		none -> none;
		Hit -> update_selection(Hit, St)
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
%%% Filter hits to obtain just one hit.
%%%

filter_hits(Hits, X, Y, #st{selmode=Mode,shapes=Shs}) ->
    EyePoint = wings_view:eye_point(),
    filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint).

filter_hits_1([{_,[Id,Face]}|Hits], Shs, Mode, X, Y, EyePoint) ->
    #shape{sh=We} = gb_trees:get(Id, Shs),
    Vs = [V|_] = wings_face:surrounding_vertices(Face, We),
    N = wings_face:face_normal(Vs, We),
    D = e3d_vec:dot(wings_vertex:pos(V, We), N),
    case e3d_vec:dot(EyePoint, N) of
	S when S < D -> filter_hits_1(Hits, Shs, Mode, X, Y, EyePoint);
	S -> convert_hit(Mode, X, Y, Id, Face, We)
    end;
filter_hits_1([], St, Mode, X, Y, EyePoint) -> none.
    
get_hits(0, _, Acc) -> sort(Acc);
get_hits(N, <<NumNames:32,Z0:32,_:32,Tail0/binary>>, Acc) ->
    <<Names:NumNames/binary-unit:32,Tail/binary>> = Tail0,
    Name = get_name(NumNames, Names, []),
    get_hits(N-1, Tail, [{Z0,Name}|Acc]).

get_name(0, Tail, Acc) -> reverse(Acc);
get_name(N, <<Name:32,Names/binary>>, Acc) ->
    get_name(N-1, Names, [Name|Acc]).

%%
%% Given a selection hit, return the correct vertex/edge/face/body.
%%

convert_hit(body, X, Y, Id, Face, We) -> {Id,0};
convert_hit(face, X, Y, Id, Face, We) -> {Id,Face};
convert_hit(Mode, X, Y, Id, Face, We) ->
    wings_view:projection(),
    wings_view:model_transformations(dummy),
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
			   L = math:sqrt(Xdist*Xdist+Ydist*Ydist),
			   S = ((Ay-Cy)*Xdist-(Ax-Cx)*Ydist)/L/L,
			   Dist = abs(S)*L,
			   [{Dist,Edge}|A]
		   end
	   end, [], Face, We),
    {_,Edge} = min(Es),
    Edge.

project_vertex(V, We, {ModelMatrix,ProjMatrix,ViewPort}) ->
    {Px,Py,Pz} = wings_vertex:pos(V, We),
    {true,Xs,Ys,_} = project(Px, Py, Pz, ModelMatrix, ProjMatrix, ViewPort),
    {Xs,Ys}.

project(Objx, Objy, ObjZ, ModelMatrix0, ProjMatrix0, [Vx,Vy,Vw,Vh]) ->
    ModelMatrix = list_to_tuple(ModelMatrix0),
    ProjMatrix = list_to_tuple(ProjMatrix0),
    Pos0 = {Objx,Objy,ObjZ,1.0},
    Pos1 = e3d_mat:mul(ModelMatrix, Pos0),
    Pos2 = {X0,Y0,Z0,W} = e3d_mat:mul(ProjMatrix, Pos1),
    X1 = X0 / W,
    Y1 = Y0 / W,
    Z1 = Z0 / W,

    %% Map x, y, and z to range 0-1.
    X2 = X1 * 0.5 + 0.5,
    Y2 = Y1 * 0.5 + 0.5,
    Z = Z1 * 0.5 + 0.5,

    %% Map x and y to viewport.
    X = X2*Vw + Vx,
    Y = Y2*Vh + Vy,

    {true,X,Y,Z}.

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

select_draw(St0) ->
    wings_view:model_transformations(St0),
    #st{dl=#dl{pick=Dlist}=DL} = St = select_draw_0(St0),
    gl:callList(Dlist),
    St.

select_draw_0(#st{dl=#dl{pick=none}=DL}=St) ->
    Dlist = ?DL_PICK,
    gl:newList(Dlist, ?GL_COMPILE),
    gl:pushAttrib(?GL_LINE_BIT),
    select_draw_1(St),
    gl:popAttrib(),
    gl:endList(),
    St#st{dl=DL#dl{pick=Dlist}};
select_draw_0(St) -> St.

select_draw_1(St) ->
    foreach_we(fun(We) ->
		       gl:pushName(0),
		       wings_util:fold_face(
			 fun(Face, #face{edge=Edge}, _) ->
				 gl:loadName(Face),
				 draw_face(Face, Edge, We)
			 end, [], We),
		       gl:popName()
	       end, St).

% select_draw_1(#st{selmode=edge}=St) ->
%     foreach_we(
%       fun(#we{vs=Vtab}=We) ->
% 	      gl:pushName(0),
% 	      wings_util:foreach_edge(
% 		fun(Edge, #edge{vs=Vstart,ve=Vend}, _Sh) ->
% 			gl:loadName(Edge),
% 			gl:'begin'(?GL_LINES),
% 			gl:vertex3fv(lookup_pos(Vstart, Vtab)),
% 			gl:vertex3fv(lookup_pos(Vend, Vtab)),
% 			gl:'end'()
% 		end, We),
% 	      gl:popName()
%       end, St);
% select_draw_1(#st{selmode=vertex}=St) ->
%     foreach_we(fun(#we{}=We) ->
% 		       gl:pushName(0),
% 		       wings_util:fold_vertex(
% 			 fun(V, #vtx{pos=Pos}, _) ->
% 				 gl:loadName(V),
% 				 gl:'begin'(?GL_POINTS),
% 				 gl:vertex3fv(Pos),
% 				 gl:'end'()
% 			 end, [], We),
% 		       gl:popName()
% 	       end, St).

lookup_pos(Key, Tree) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tree),
    Pos.

foreach_we(F, #st{shapes=Shapes}) ->
    Iter = gb_trees:iterator(Shapes),
    foreach_we_1(F, Iter).

foreach_we_1(F, Iter0) ->
    case gb_trees:next(Iter0) of
	none -> ok;
	{Id,#shape{sh=#we{}=We},Iter} ->
	    gl:pushName(Id),
	    F(We),
	    gl:popName(),
	    foreach_we_1(F, Iter);
	{Id,_,Iter} ->
	    foreach_we_1(F, Iter)
    end.

draw_face(Face, Edge, #we{es=Etab,vs=Vtab}) ->
    gl:'begin'(?GL_POLYGON),
    draw_face_1(Face, Edge, Edge, Etab, Vtab, not_done),
    gl:'end'().

draw_face_1(Face, LastEdge, LastEdge, Etab, Vtab, done) -> ok;
draw_face_1(Face, Edge, LastEdge, Etab, Vtab, Acc) ->
    {Next,V} = case gb_trees:get(Edge, Etab) of
		   #edge{ve=V0,lf=Face,ltpr=Next0}=Rec -> {Next0,V0};
		   #edge{vs=V0,rf=Face,rtpr=Next0}=Rec -> {Next0,V0}
	       end,
    gl:vertex3fv(lookup_pos(V, Vtab)),
    draw_face_1(Face, Next, LastEdge, Etab, Vtab, done).
