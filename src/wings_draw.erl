%%
%%  wings_draw.erl --
%%
%%     This module draws objects using OpenGL and handles picking.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw.erl,v 1.15 2001/11/06 07:06:13 bjorng Exp $
%%

-module(wings_draw).
-export([model_changed/1,render/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1]).

-define(EDGE_MODE_LINEWIDTH, 2.0).
-define(NORMAL_LINEWIDTH, 0.1).
-define(VERTEX_MODE_POINTSIZE, 5.0).
-define(SEL_COLOR, {0.65,0.0,0.0}).

model_changed(St) -> St#st{dl=none}.

%%
%% Renders all shapes, including selections.
%%

render(#st{shapes=Shapes}=St0) ->
    ?CHECK_ERROR(),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    wings_view:model_transformations(St0),
    ground_and_axes(St0),
    St1 = update_display_lists(St0),
    St = make_sel_dlist(St1),
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
    draw_sel(St);
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
    draw_sel(St),

    %% Draw hard edges.
    draw_hard_edges(St).

draw_sel(#st{sel=[],dl=#dl{sel=none}}=St) -> ok;
draw_sel(#st{selmode=edge,dl=#dl{sel=DlistSel}}) ->
    gl:color3fv(?SEL_COLOR),
    gl:lineWidth(?EDGE_MODE_LINEWIDTH),
    gl:callList(DlistSel);
draw_sel(#st{selmode=vertex,dl=#dl{sel=DlistSel}}) ->
    gl:color3fv(?SEL_COLOR),
    gl:pointSize(?VERTEX_MODE_POINTSIZE),
    gl:callList(DlistSel);
draw_sel(#st{dl=#dl{sel=DlistSel}}) ->
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    gl:color3fv(?SEL_COLOR),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    case catch gl:callList(DlistSel) of
	{'EXIT',_} -> exit({bad_call_list,DlistSel});
	_ -> ok
    end.
    
draw_we(#st{dl=#dl{we=DlistWe,dragging=WeDrag,matrix=Matrix}}) ->
    gl:callList(DlistWe),
    case WeDrag of
	none -> ok;
	Other ->
	    gl:pushMatrix(),
	    gl:multMatrixf(e3d_mat:expand(Matrix)),
	    gl:callList(WeDrag),
	    gl:popMatrix()
    end.

update_display_lists(#st{shapes=Shapes,dl=none,opts=#opt{smooth=Smooth}}=St) ->
    DlistWe = 98,
    gl:newList(DlistWe, ?GL_COMPILE),
    foreach(fun(Sh) ->
		    shape(Sh, Smooth, St)
	    end, gb_trees:values(Shapes)),
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

make_sel_dlist(#st{sel=[],dl=DL}=St) ->
    St#st{dl=DL#dl{sel=none}};
make_sel_dlist(#st{dl=#dl{sel=none}}=St) ->
    do_make_sel_dlist(St);
make_sel_dlist(#st{sel=Sel,dl=#dl{old_sel=Sel}}=St) ->
    St;
make_sel_dlist(#st{dl=DL}=St) ->
    do_make_sel_dlist(St);
make_sel_dlist(St) -> St.

do_make_sel_dlist(#st{sel=Sel,dl=DL}=St) ->
    DlistSel = 95,
    gl:newList(DlistSel, ?GL_COMPILE),
    draw_selection(St),
    gl:endList(),
    St#st{dl=DL#dl{old_sel=Sel,sel=DlistSel}}.

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
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_material:apply_material(Mat, Mtab),
    draw_smooth_2(Faces),
    gl:popAttrib(),
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
      fun(#shape{sh=#we{he=Htab}=We}) ->
	      case gb_sets:is_empty(Htab) of
		  true -> ok;
		  false -> draw_hard_edges_1(We)
	      end;
	 (_) -> ok
      end, gb_trees:values(Shapes)),
    ?CHECK_ERROR().

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
    wings_sel:foreach(
      fun(_, #shape{matrix=Matrix,sh=Data}) ->
	      gl:pushMatrix(),
	      gl:multMatrixf(e3d_mat:expand(Matrix)),
	      draw_faces(Data, false, St),
	      gl:popMatrix()
      end, St),
    St;
draw_selection(#st{selmode=face}=St) ->
    wings_sel:foreach(
      fun(Face, #shape{sh=#we{fs=Ftab}=We}) ->
	      #face{edge=Edge} = gb_trees:get(Face, Ftab),
	      draw_face(Face, Edge, We)
      end, St),
    St;
draw_selection(#st{selmode=edge}=St) ->
    gl:'begin'(?GL_LINES),
    wings_sel:foreach(
      fun(Edge, #shape{sh=#we{es=Etab,vs=Vtab}}=Sh) ->
	      #edge{vs=Vstart,ve=Vend} = gb_trees:get(Edge, Etab),
	      gl:vertex3fv(lookup_pos(Vstart, Vtab)),
	      gl:vertex3fv(lookup_pos(Vend, Vtab))
      end, St),
    gl:'end'(),
    St;
draw_selection(#st{selmode=vertex}=St) ->
    gl:'begin'(?GL_POINTS),
    wings_sel:foreach(
      fun(V, #shape{sh=#we{vs=Vtab}}) ->
	      gl:vertex3fv(lookup_pos(V, Vtab))
      end, St),
    gl:'end'(),
    St.

lookup_pos(Key, Tree) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tree),
    Pos.

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
    C2 = setelement(I, {0.8,0.8,0.8}, 0.0),
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
