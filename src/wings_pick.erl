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
%%     $Id: wings_pick.erl,v 1.7 2001/11/22 09:08:29 bjorng Exp $
%%

-module(wings_pick).
-export([pick/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,sort/1,foldl/3,map/2]).

pick(#st{hit_buf=HitBuf,shapes=Shapes,selmode=Mode}=St0, X0, Y0) ->
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),

    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    X = float(X0),
    Y = H-float(Y0),
    S = case Mode of
	    edge -> 10.0;
	    Other -> 1.0
	end,
    glu:pickMatrix(X, Y, S, S, [0,0,W,H]),
    wings_view:perspective(),
    St = select_draw(St0),

    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> St;
	NumHits ->
	    HitData = sdl_util:readBin(HitBuf, ?HIT_BUF_SIZE),
	    Hits = get_hits(NumHits, HitData, []),
	    case Hits of
		[] -> St;
		[{_,Hit}|_] -> update_selection(Hit, X, Y, St)
	    end
    end.

update_selection([Id,Item], X, Y, #st{sel=Sel0}=St) ->
    Sel = update_selection(Id, Item, Sel0, X, Y, St),
    St#st{sel=Sel}.

update_selection(Id, Item, [{I,_}=H|T], X, Y, St) when Id > I ->
    [H|update_selection(Id, Item, T, X, Y, St)];
update_selection(Id, Item0, [{I,_}|_]=T, X, Y, St) when Id < I ->
    Item = find_item(Id, Item0, X, Y, St),
    [{Id,gb_sets:singleton(Item)}|T];
update_selection(Id, Item0, [{I,Items0}|T], X, Y, St) -> %Id == I
    ?ASSERT(Id == I),
    Item = find_item(Id, Item0, X, Y, St),
    case gb_sets:is_member(Item, Items0) of
	true ->
	    Items = gb_sets:delete(Item, Items0),
	    case gb_sets:is_empty(Items) of
		true -> T;
		false -> [{Id,Items}|T]
	    end;
	false ->
	    Items = gb_sets:insert(Item, Items0),
	    [{Id,Items}|T]
    end;
update_selection(Id, Item0, [], X, Y, St) ->
    Item = find_item(Id, Item0, X, Y, St),
    [{Id,gb_sets:singleton(Item)}].

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

find_item(Id, Item, X, Y, #st{selmode=body}) -> Item;
find_item(Id, Face, X, Y, #st{selmode=face}) -> Face;
find_item(Id, Edge, X, Y, #st{selmode=edge}) -> Edge; %XXX Temporary.
find_item(Id, Face, X, Y, #st{selmode=Mode,shapes=Shapes}=St) ->
    #shape{sh=#we{}=We} = gb_trees:get(Id, Shapes),
    wings_view:projection(),
    wings_view:model_transformations(St),
    ViewPort = gl:getIntegerv(?GL_VIEWPORT),
    ModelMatrix = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    ProjMatrix = gl:getDoublev(?GL_PROJECTION_MATRIX),
    case Mode of
	vertex ->
	    find_vertex(Face, We, X, Y, ModelMatrix, ProjMatrix, ViewPort);
	edge ->
	    find_edge(Face, We, X, Y, ModelMatrix, ProjMatrix, ViewPort)
    end.

find_vertex(Face, We, X, Y, ModelMatrix, ProjMatrix, ViewPort) ->
    Vs0 = wings_face:surrounding_vertices(Face, We),
    Vs = map(fun(V) ->
		     {Px,Py,Pz} = wings_vertex:pos(V, We),
		     {true,Xs,Ys,_} = project(Px, Py, Pz, ModelMatrix,
					      ProjMatrix, ViewPort),
		     Dx = X-Xs,
		     Dy = Y-Ys,
		     {Dx*Dx+Dy*Dy,V}
	     end, Vs0),
    {_,V} = lists:min(Vs),
    V.
find_edge(Face, We, X, Y, ModelMatrix, ProjMatrix, ViewPort) ->
    1.

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
    make_dlist(St);
select_draw_0(#st{selmode=Mode,dl=#dl{pick_mode=Mode}=DL}=St) ->
    St;
select_draw_0(St) ->
    make_dlist(St).

make_dlist(#st{selmode=Mode,dl=DL}=St) ->
    Dlist = ?DL_PICK,
    gl:newList(Dlist, ?GL_COMPILE),
    gl:pushAttrib(?GL_LINE_BIT),
    select_draw_1(St),
    gl:popAttrib(),
    gl:endList(),
    St#st{dl=DL#dl{pick=Dlist,pick_mode=Mode}}.

select_draw_1(#st{selmode=body}=St) ->
    wings_util:foreach_shape(
      fun(Id, #shape{sh=Data}=Sh) ->
	      gl:pushName(Id),
	      gl:pushName(0),
	      draw_faces(Data, false, St),
	      gl:popName(),
	      gl:popName()
      end, St);
select_draw_1(#st{selmode=edge}=St) ->
    foreach_we(
      fun(#we{vs=Vtab}=We) ->
	      gl:pushName(0),
	      wings_util:foreach_edge(
		fun(Edge, #edge{vs=Vstart,ve=Vend}, _Sh) ->
			gl:loadName(Edge),
			gl:'begin'(?GL_LINES),
			gl:vertex3fv(lookup_pos(Vstart, Vtab)),
			gl:vertex3fv(lookup_pos(Vend, Vtab)),
			gl:'end'()
		end, We),
	      gl:popName()
      end, St);
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

draw_faces(#we{}=We, _, St) ->
    wings_util:fold_face(
      fun(Face, #face{edge=Edge}, _) ->
	      draw_face(Face, Edge, We)
      end, [], We).

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
