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
%%     $Id: wings_pick.erl,v 1.2 2001/11/04 20:11:03 bjorng Exp $
%%

-module(wings_pick).
-export([pick/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,sort/1]).

pick(#st{hit_buf=HitBuf,shapes=Shapes}=St0, X, Y) ->
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    pick_matrix(St0, X, Y),
    wings_view:perspective(St0),
    St = select_draw(St0),

    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> St;
	NumHits ->
	    HitData = sdl_util:readBin(HitBuf, ?HIT_BUF_SIZE),
	    Hits = get_hits(NumHits, HitData, []),
	    %%io:format("~w\n", [Hits]),
	    case Hits of
		[] -> St;
		[{_,Hit}|_] -> update_selection(Hit, St)
	    end
    end.

pick_matrix(#st{selmode=face}, X, Y) ->
    pick_matrix_1(1.0, X, Y);
pick_matrix(_, X, Y) ->
    pick_matrix_1(10.0, X, Y).

pick_matrix_1(S, X, Y) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    glu:pickMatrix(float(X), H-float(Y), S, S, [0,0,W,H]).

update_selection([Id,Item], #st{sel=Sel0}=St) ->
    Sel = update_selection(Id, Item, Sel0),
    St#st{sel=Sel}.

update_selection(Id, Item, [{I,_}=H|T]) when Id > I ->
    [H|update_selection(Id, Item, T)];
update_selection(Id, Item, [{I,_}|_]=T) when Id < I ->
    [{Id,gb_sets:singleton(Item)}|T];
update_selection(Id, Item, [{I,Items0}|T]) ->	%Id == I
    ?ASSERT(Id == I),
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
update_selection(Id, Item, []) ->
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
    Dlist = 100,
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
select_draw_1(#st{selmode=face}=St) ->
    foreach_we(fun(We) ->
		       gl:pushName(0),
		       wings_util:fold_face(
			 fun(Face, #face{edge=Edge}, _) ->
				 gl:loadName(Face),
				 draw_face(Face, Edge, We)
			 end, [], We),
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
%     gl:pushName(0),
%     foreach_we(fun(We) ->
% 		       gl:pushName(0),
% 		       wings_util:fold_face(
% 			 fun(Face, #face{edge=Edge}, _) ->
% 				 gl:loadName(Face),
% 				 draw_face(Face, Edge, We)
% 			 end, [], We),
% 		       gl:popName()
% 	       end, St),
%     gl:popName();
select_draw_1(#st{selmode=vertex}=St) ->
    foreach_we(fun(#we{}=We) ->
		       gl:pushName(0),
		       wings_util:fold_vertex(
			 fun(V, #vtx{pos=Pos}, _) ->
				 gl:loadName(V),
				 gl:'begin'(?GL_POINTS),
				 gl:vertex3fv(Pos),
				 gl:'end'()
			 end, [], We),
		       gl:popName()
	       end, St).

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
