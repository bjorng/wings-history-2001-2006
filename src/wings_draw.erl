%%
%%  wings_draw.erl --
%%
%%     This module draws objects using OpenGL and handles picking.
%%
%%  Copyright (c) 2001 Jakob Cederlund, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw.erl,v 1.1 2001/08/14 18:16:35 bjorng Exp $
%%

-module(wings_draw).
-export([model_changed/1,render/1,select/3]).

-include("gl.hrl").
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1]).

-define(EDGE_MODE_LINEWIDTH, 4.0).
-define(NORMAL_LINEWIDTH, 0.1).
-define(VERTEX_MODE_POINTSIZE, 10.0).
-define(SEL_COLOR, {0.5,0.0,0.0}).

model_changed(St) ->
    St#st{dl=none}.

%%
%% Renders all shapes, including selections.
%%

render(#st{shapes=Shapes,distance=Dist,azimuth=Az,elevation=El}=St0) ->
    ?CHECK_ERROR(),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    wings_view:model_transformations(St0),
    ground_and_axes(St0),
    St = update_display_lists(St0),
    draw_shapes(St),
    gl:popAttrib(),
    ?CHECK_ERROR(),
    St.

draw_shapes(#st{opts=#opt{smooth=true},dl=#dl{drag_faces=none}=DL}=St) ->
    #dl{we=DlistWe} = DL,
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),
    gl:shadeModel(?GL_SMOOTH),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_LIGHT0),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    ?CHECK_ERROR(),
    gl:enable(?GL_BLEND),
    ?CHECK_ERROR(),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    ?CHECK_ERROR(),
    gl:polygonOffset(2.0, 2.0),
    draw_we(St),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_CULL_FACE),
    draw_selection(St);
draw_shapes(#st{selmode=SelMode,opts=#opt{wire=Wire}}=St) ->
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),

    %% Draw faces for winged-edge-objects.
    case Wire of
	true -> ok;
	false ->
	    gl:color3f(0.5, 0.5, 0.5),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    gl:polygonOffset(2.0, 2.0),
	    draw_we(St)
    end,

    %% Draw edges.
    case {Wire,SelMode} of
	{true,_} -> gl:color3f(1.0, 1.0, 1.0);
	{_,body} -> gl:color3f(0.3, 0.3, 0.3);
	{_,_} -> gl:color3f(0.0, 0.0, 0.0)
    end,
    gl:lineWidth(case SelMode of
		     edge -> ?EDGE_MODE_LINEWIDTH;
		     _ -> ?NORMAL_LINEWIDTH end),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:enable(?GL_POLYGON_OFFSET_LINE),
    gl:polygonOffset(1.0, 1.0),
    gl:disable(?GL_CULL_FACE),
    draw_we(St),

    %% If vertex selection mode, draw vertices.
    case SelMode of
	vertex ->
	    gl:color3f(0.0, 0.0, 0.0), 
	    gl:pointSize(?VERTEX_MODE_POINTSIZE),
	    gl:enable(?GL_POLYGON_OFFSET_POINT),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_POINT),
	    draw_we(St);
	NotVertex -> ok
    end,

    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_POINT),
    gl:disable(?GL_POLYGON_OFFSET_FILL),

    %% Selection.
    draw_selection(St),

    %% Draw hard edges.
    draw_hard_edges(St).

draw_we(#st{dl=#dl{we=DlistWe,dragging=WeDrag,matrix=Matrix}}) ->
    gl:callList(DlistWe),
    case WeDrag of
	none -> ok;
	Other ->
	    gl:pushMatrix(),
	    gl:multMatrixf(Matrix),
	    gl:callList(WeDrag),
	    gl:popMatrix()
    end.

update_display_lists(#st{shapes=Shapes,dl=none,opts=#opt{smooth=Smooth}}=St) ->
    DlistWe = 98,
    gl:newList(DlistWe, ?GL_COMPILE),
    foreach(fun({_,Sh}) ->
		    shape(Sh, Smooth, St)
	    end, gb_trees:to_list(Shapes)),
    gl:endList(),
    St#st{dl=#dl{we=DlistWe}};
update_display_lists(#st{dl=#dl{drag_faces=none}}=St) -> St;
update_display_lists(#st{dl=#dl{we=none,drag_faces=Faces}=DL}=St) ->
    %% Collect the static display list - faces that will not be moved.
    DlistId = make_dlist(98, Faces, false, St),
    update_display_lists(St#st{dl=DL#dl{we=DlistId}});
update_display_lists(#st{dl=#dl{dragging=none,drag_faces=Faces}=DL}=St) ->
    %% Collect the dynamic display list - everything that will be moved.
    DlistId = make_dlist(97, Faces, true, St),
    update_display_lists(St#st{dl=DL#dl{dragging=DlistId}});
update_display_lists(St) -> St.

make_dlist(DlistId, Faces, DrawMembers, #st{shapes=Shapes0}=St) ->
    gl:newList(DlistId, ?GL_COMPILE),
    make_dlist_1(gb_trees:to_list(Shapes0), Faces, DrawMembers),
    gl:endList(),
    DlistId.

make_dlist_1([{Id,Shape}|Shs], [{Id,Faces}|Fs], DrawMembers) ->
    Draw = fun(F, Fs) -> DrawMembers =:= gb_sets:is_member(F, Fs) end,
    mkdl_draw_faces(Shape, Faces, Draw),
    make_dlist_1(Shs, Fs, DrawMembers);
make_dlist_1([{Id,Shape}|Shs], Fs, DrawMembers) ->
    Draw = not DrawMembers,
    mkdl_draw_faces(Shape, dummy, fun(_, _) -> Draw end),
    make_dlist_1(Shs, Fs, DrawMembers);
make_dlist_1([], Fs, Draw) -> ok.

mkdl_draw_faces(#shape{sh=#we{}=We}, Faces, Draw) ->
    wings_util:fold_face(
      fun(Face, #face{edge=Edge}, _) ->
	      case Draw(Face, Faces) of
		  true -> draw_face(Face, Edge, We);
		  false -> ok
	      end
      end, [], We);
mkdl_draw_faces(_, _, _) -> ok.

shape(#shape{sh=Data}, Smooth, St) ->
    draw_faces(Data, Smooth, St).

draw_faces(#we{}=We, true, #st{mat=Mtab}) ->
    draw_smooth_faces(Mtab, We);
draw_faces(#we{}=We, false, St) ->
    wings_util:fold_face(
      fun(Face, #face{edge=Edge}, _) ->
	      draw_face(Face, Edge, We)
      end, [], We).

draw_smooth_faces(Mtab, We) ->
    Faces0 = wings_we:normals(We),
    Faces1 = sofs:relation(Faces0),
    Faces2 = sofs:relation_to_family(Faces1),
    Faces = sofs:to_external(Faces2),
    draw_smooth_1(Faces, Mtab).

draw_smooth_1([{Mat,Faces}|T], Mtab) ->
    material(Mat, Mtab),
    draw_smooth_2(Faces),
    draw_smooth_1(T, Mtab);
draw_smooth_1([], Mtab) -> ok.

draw_smooth_2([[{P1,N1},{P2,N2},{P3,N3}]|Fs]) ->
    gl:'begin'(?GL_POLYGON),
    gl:normal3fv(N1),
    gl:vertex3fv(P1),
    gl:normal3fv(N2),
    gl:vertex3fv(P2),
    gl:normal3fv(N3),
    gl:vertex3fv(P3),
    gl:'end'(),
    draw_smooth_2(Fs);
draw_smooth_2([[{P1,N1},{P2,N2},{P3,N3},{P4,N4}]|Fs]) ->
    gl:'begin'(?GL_POLYGON),
    gl:normal3fv(N1),
    gl:vertex3fv(P1),
    gl:normal3fv(N2),
    gl:vertex3fv(P2),
    gl:normal3fv(N3),
    gl:vertex3fv(P3),
    gl:normal3fv(N4),
    gl:vertex3fv(P4),
    gl:'end'(),
    draw_smooth_2(Fs);
draw_smooth_2([Vs|Fs]) ->
    gl:'begin'(?GL_POLYGON),
    foreach(fun({P,N}) ->
 		    gl:normal3fv(N),
 		    gl:vertex3fv(P)
 	    end, Vs),
    gl:'end'(),
    draw_smooth_2(Fs);
draw_smooth_2([]) -> ok.

material(Mat, Mtab) when atom(Mat) ->
    #mat{setup=Setup} = gb_trees:get(Mat, Mtab),
    Setup();
material([Mat|_], Mtab) ->
    material(Mat, Mtab).

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

draw_hard_edges(#st{shapes=Shapes}) ->
    gl:color3f(0.0, 0.5, 0.0),
    foreach(
      fun({Edge,#shape{sh=#we{he=Htab}=We}}) ->
	      case gb_sets:is_empty(Htab) of
		  true -> ok;
		  false -> draw_hard_edges_1(We)
	      end;
	 (_) -> ok
      end, gb_trees:to_list(Shapes)),
    ?CHECK_ERROR(),
    gl:depthFunc(?GL_LESS).

draw_hard_edges_1(#we{es=Etab,he=Htab,vs=Vtab}) ->
    foreach(fun(Edge) ->
		    #edge{vs=Vstart,ve=Vend} = gb_trees:get(Edge, Etab),
		    gl:'begin'(?GL_LINES),
		    gl:vertex3fv(lookup_pos(Vstart, Vtab)),
		    gl:vertex3fv(lookup_pos(Vend, Vtab)),
		    gl:'end'()
	    end, gb_sets:to_list(Htab)).

%%
%% Draw the currently selected items.
%% 

draw_selection(#st{selmode=body}=St) ->
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    gl:color3fv(?SEL_COLOR),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_sel:foreach(
      fun(_, #shape{matrix=Matrix,sh=Data}) ->
	      gl:pushMatrix(),
	      gl:multMatrixf(Matrix),
	      draw_faces(Data, false, St),
	      gl:popMatrix()
      end, St),
    St;
draw_selection(#st{selmode=face}=St) ->
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    gl:color3fv(?SEL_COLOR),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_sel:foreach(
      fun(Face, #shape{sh=#we{fs=Ftab}=We}) ->
	      #face{edge=Edge} = gb_trees:get(Face, Ftab),
	      draw_face(Face, Edge, We)
      end, St),
    St;
draw_selection(#st{selmode=edge}=St) ->
    gl:color3fv(?SEL_COLOR),
    gl:lineWidth(?EDGE_MODE_LINEWIDTH),
    wings_sel:foreach(
      fun(Edge, #shape{sh=#we{es=Etab,vs=Vtab}}=Sh) ->
	      #edge{vs=Vstart,ve=Vend} = gb_trees:get(Edge, Etab),
	      gl:'begin'(?GL_LINES),
	      gl:vertex3fv(lookup_pos(Vstart, Vtab)),
	      gl:vertex3fv(lookup_pos(Vend, Vtab)),
	      gl:'end'()
      end, St),
    St;
draw_selection(#st{selmode=vertex}=St) ->
    gl:color3fv(?SEL_COLOR),
    gl:pointSize(?VERTEX_MODE_POINTSIZE),
    wings_sel:foreach(
      fun(V, #shape{sh=#we{vs=Vtab}}) ->
	      gl:'begin'(?GL_POINTS),
	      gl:vertex3fv(lookup_pos(V, Vtab)),
	      gl:'end'()
      end, St),
    St.

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

select_draw(#st{dl=#dl{pick=Dlist0}=DL}=St) ->
    wings_view:model_transformations(St),
    case Dlist0 of
	none ->
	    Dlist = 100,
	    gl:newList(Dlist, ?GL_COMPILE),
	    gl:pushAttrib(?GL_LINE_BIT),
	    select_draw_1(St),
	    gl:popAttrib(),
	    gl:endList(),
	    gl:callList(Dlist),
	    St#st{dl=DL#dl{pick=Dlist}};
	Dlist ->
	    gl:callList(Dlist),
	    St
    end.

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
    gl:lineWidth(2*?EDGE_MODE_LINEWIDTH),
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
select_draw_1(#st{selmode=vertex}=St) ->
    gl:pointSize(2*?VERTEX_MODE_POINTSIZE),
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

%%
%% Selections.
%%

select(#st{hit_buf=HitBuf,shapes=Shapes}=St0, X, Y) ->
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    glu:pickMatrix(float(X), H-float(Y), 8.0, 8.0, [0,0,W,H]),
    wings_view:perspective(St0),
    St = select_draw(St0),

    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> St;
	Hits ->
	    HitData = sdl_util:readBin(HitBuf, ?HIT_BUF_SIZE),
	    Hit = process_hits(Hits, HitData, none, 1.0e200),
	    update_selection(Hit, St)
    end.

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

process_hits(0, _, Name, ZMin) -> Name;
process_hits(Hits, <<NumNames:32,Z0:32,_:32,Tail0/binary>>, OldName, OldZMin) ->
    <<Names:NumNames/binary-unit:32,Tail/binary>> = Tail0,
    case Z0/16#7FFFFFFF of
	ZMin when ZMin < OldZMin ->
	    Name = get_name(NumNames, Names, []),
	    process_hits(Hits-1, Tail, Name, ZMin);
	ZMin ->
	    process_hits(Hits-1, Tail, OldName, OldZMin)
    end.

get_name(0, Tail, Acc) -> reverse(Acc);
get_name(N, <<Name:32,Names/binary>>, Acc) ->
    get_name(N-1, Names, [Name|Acc]).

%%
%% Miscellanous.
%%

ground_and_axes(#st{opts=#opt{ground=Ground,axes=Axes}}) ->
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    case Ground of
	true -> groundplane(Axes);
	false -> ok
    end,
    ?CHECK_ERROR(),
    case Axes of
	true ->
	    axis(1),
	    axis(2),
	    axis(3);
	false -> ok
    end,
    ?CHECK_ERROR(),
    gl:popAttrib().

axis(I) ->
    A0 = {0.0,0.0,0.0},
    A = setelement(I, A0, 1000.0),
    B = setelement(I, A0, -1000.0),
    C = setelement(I, A0, 1.0),
    C2 = setelement(I, {0.5,0.5,0.5}, 0.0),
    gl:'begin'(?GL_LINES),
    gl:color3fv(C),
    gl:vertex3fv(A0),
    gl:vertex3fv(A),
    gl:color3fv(C2),
    gl:vertex3fv(A0),
    gl:vertex3fv(B),
    gl:'end'().

groundplane(Axes) ->
    gl:color3f(0.0, 0.0, 0.0),
    ?CHECK_ERROR(),
    gl:lineWidth(0.1),
    ?CHECK_ERROR(),
    gl:'begin'(?GL_LINES),
    Sz = ?GROUND_GRID_SIZE * 10,
    groundplane(-Sz, Sz, Sz, Axes),
    gl:'end'(),
    ?CHECK_ERROR().

groundplane(X, Last, Sz, Axes) when X > Last -> ok;
groundplane(0.0, Last, Sz, true) ->
    groundplane(?GROUND_GRID_SIZE, Last, Sz, true);
groundplane(X, Last, Sz, Axes) ->
    gl:vertex3f(float(X), 0.0, -Sz),
    gl:vertex3f(float(X), 0.0, Sz),
    gl:vertex3f(-Sz, 0.0, float(X)),
    gl:vertex3f(Sz, 0.0, float(X)),
    groundplane(X+?GROUND_GRID_SIZE, Last, Sz, Axes).
