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
%%     $Id: wings_draw.erl,v 1.27 2001/11/27 20:58:59 bjorng Exp $
%%

-module(wings_draw).
-export([init/0,tess/0,model_changed/1,render/1,ground_and_axes/0,
	 draw_face_normal/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1]).

model_changed(St) -> St#st{dl=none}.

-define(DL_FACES, (?DL_DRAW_BASE)).
-define(DL_EDGES, (?DL_DRAW_BASE+1)).
-define(DL_SEL, (?DL_DRAW_BASE+2)).

init() ->
    Tess = glu:newTess(),
    put(wings_gnu_tess, Tess), 
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_GLBEGIN),
    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_GLEND),
    glu:tessCallback(Tess, ?GLU_TESS_EDGE_FLAG, ?ESDL_TESSCB_GLEDGEFLAG),
    glu:tessCallback(Tess, ?GLU_TESS_COMBINE, ?ESDL_TESSCB_SIMPLE_COMBINE),
    glu:tessCallback(Tess, ?GLU_TESS_ERROR, ?ESDL_TESSCB_ERROR_PRINT).

tess() ->
    get(wings_gnu_tess).

%%
%% Renders all shapes, including selections.
%%

render(#st{shapes=Shapes}=St0) ->
    ?CHECK_ERROR(),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:enable(?GL_DEPTH_TEST),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    St1 = update_display_lists(St0),
    St = make_sel_dlist(St1),
    wings_view:projection(),
    wings_view:model_transformations(),
    ground_and_axes(),
    draw_shapes(St),
    gl:popAttrib(),
    ?CHECK_ERROR(),
    St.

draw_shapes(St) ->
    case wings_pref:get_value(smooth_preview) of
	true -> draw_smooth_shapes(St);
	false -> draw_plain_shapes(St)
    end.

draw_smooth_shapes(#st{dl=DL}=St) ->
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),
    gl:shadeModel(?GL_SMOOTH),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:polygonOffset(2.0, 2.0),
    draw_faces(St),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_CULL_FACE),
    ?CHECK_ERROR(),
    draw_sel(St).

draw_plain_shapes(#st{selmode=SelMode}=St) ->
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),

    gl:enable(?GL_LIGHT0),
    gl:enable(?GL_LIGHT1),

    %% Draw faces for winged-edge-objects.
    Wire = wings_pref:get_value(wire_mode),
    case Wire of
	true -> ok;
	false ->
% 	    FaceColor = wings_pref:get_value(face_color),
% 	    gl:color3fv(FaceColor),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    gl:polygonOffset(2.0, 2.0),
	    gl:shadeModel(?GL_SMOOTH),
	    gl:enable(?GL_LIGHTING),
	    draw_faces(St),
	    gl:disable(?GL_LIGHTING),
	    gl:shadeModel(?GL_FLAT)
    end,

    %% Draw edges.
    case {Wire,SelMode} of
	{true,_} -> gl:color3f(1.0, 1.0, 1.0);
	{_,body} -> gl:color3f(0.3, 0.3, 0.3);
	{_,_} -> gl:color3f(0.0, 0.0, 0.0)
    end,
    gl:lineWidth(case SelMode of
		     edge -> wings_pref:get_value(edge_width);
		     _ -> ?NORMAL_LINEWIDTH end),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:enable(?GL_POLYGON_OFFSET_LINE),
    gl:polygonOffset(1.0, 1.0),
    draw_faces(St),

    %% If vertex selection mode, draw vertices.
    case SelMode of
	vertex ->
	    gl:color3f(0.0, 0.0, 0.0), 
	    gl:pointSize(wings_pref:get_value(vertex_size)),
	    gl:enable(?GL_POLYGON_OFFSET_POINT),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_POINT),
	    draw_faces(St);
	NotVertex -> ok
    end,

    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_POINT),
    gl:disable(?GL_POLYGON_OFFSET_FILL),

    draw_sel(St),
    draw_hard_edges(St).

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_sel(#st{sel=[],dl=#dl{sel=none}}=St) -> ok;
draw_sel(#st{selmode=edge,dl=#dl{sel=DlistSel}}) ->
    sel_color(),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:callList(DlistSel);
draw_sel(#st{selmode=vertex,dl=#dl{sel=DlistSel}}) ->
    sel_color(),
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:callList(DlistSel);
draw_sel(#st{dl=#dl{sel=DlistSel}}) ->
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    sel_color(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    case catch gl:callList(DlistSel) of
	{'EXIT',_} -> exit({bad_call_list,DlistSel});
	_ -> ok
    end.
    
draw_faces(#st{dl=#dl{faces=DlistFaces}}) ->
    gl:callList(DlistFaces).

draw_edges(#st{dl=#dl{edges=DlistEdges}}) ->
    gl:callList(DlistEdges).

update_display_lists(#st{shapes=Shapes,dl=none}=St) ->
    Smooth = wings_pref:get_value(smooth_preview),
    gl:newList(?DL_FACES, ?GL_COMPILE),
    foreach(fun(Sh) ->
		    shape(Sh, Smooth, St)
	    end, gb_trees:values(Shapes)),
    gl:endList(),
    Dl = #dl{faces=?DL_FACES},
    case Smooth of
	true -> St#st{dl=Dl};
	false -> make_edge_dlist(Dl, St)
    end;
update_display_lists(St) -> St.

make_edge_dlist(Dl, #st{shapes=Shapes}=St) ->
    gl:newList(?DL_EDGES, ?GL_COMPILE),
    foreach(fun(Sh) ->
		    edges(Sh)
	    end, gb_trees:values(Shapes)),
    gl:endList(),
    St#st{dl=Dl#dl{edges=?DL_EDGES}}.

edges(#shape{sh=#we{es=Etab,vs=Vtab,he=Htab}}) ->
    gl:'begin'(?GL_LINES),
    draw_edges(gb_trees:values(Etab), Vtab),
    gl:'end'().

draw_edges([#edge{vs=Va,ve=Vb}|Es], Vtab) ->
    gl:vertex3fv(lookup_pos(Va, Vtab)),
    gl:vertex3fv(lookup_pos(Vb, Vtab)),
    draw_edges(Es, Vtab);
draw_edges([], Vtab) -> ok.

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
    DlistSel = ?DL_SEL,
    gl:newList(DlistSel, ?GL_COMPILE),
    draw_selection(St),
    gl:endList(),
    St#st{dl=DL#dl{old_sel=Sel,sel=DlistSel}}.

shape(#shape{sh=Data}, Smooth, St) ->
    draw_faces(Data, Smooth, St).

draw_faces(#we{}=We, true, #st{mat=Mtab}) ->
    draw_smooth_faces(Mtab, We);
draw_faces(#we{}=We, false, St) ->
    wings_util:fold_face(
      fun(Face, #face{edge=Edge}, _) ->
	      draw_face_normal(Face, Edge, We)
      end, [], We).

draw_smooth_faces(Mtab, #we{mode=vertex}=We) ->
    Faces = wings_we:normals(We),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT, ?GL_DIFFUSE),
    draw_smooth_vcolor(Faces),
    gl:disable(?GL_COLOR_MATERIAL);
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

draw_smooth_2([[_,_,_,_,_|_]=Vs|Fs]) ->
    %% This face needs tesselation.
    Tess = tess(),
    glu:tessNormal(Tess, 0, 0, 0),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{Diff,N}}) ->
		    glu:tessVertex(Tess, P, [{normal,N}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_2(Fs);
draw_smooth_2([Vs|Fs]) ->
    gl:'begin'(?GL_POLYGON),
    foreach(fun({P,{Diff,N}}) ->
 		    gl:normal3fv(N),
 		    gl:vertex3fv(P)
 	    end, Vs),
    gl:'end'(),
    draw_smooth_2(Fs);
draw_smooth_2([]) -> ok.

%% Smooth drawing for vertex colors.
draw_smooth_vcolor([{_,[_,_,_,_,_|_]=Vs}|T]) ->
    Tess = tess(),
    glu:tessNormal(Tess, 0, 0, 0),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{{R,G,B}=Diff,N}}) ->
		    glu:tessVertex(Tess, P,
				   [{normal,N},
				    {color,Diff},
				    {material,?GL_FRONT,?GL_DIFFUSE,Diff}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_vcolor(T);
draw_smooth_vcolor([{_,Vs}|T]) ->
    gl:'begin'(?GL_POLYGON),
    foreach(fun({P,{{R,G,B}=Diff,N}}) ->
 		    gl:normal3fv(N),
		    gl:color3f(R, G, B),
		    gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, Diff),
 		    gl:vertex3fv(P)
 	    end, Vs),
    gl:'end'(),
    draw_smooth_vcolor(T);
draw_smooth_vcolor([]) -> ok.

draw_hard_edges(#st{shapes=Shapes}) ->
    gl:color3fv(wings_pref:get_value(hard_edge_color)),
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
%% Draw a face and tesselate it if necessary.
%%

draw_face_normal(Face, Edge, We) ->
    case wings_face:draw_info(Face, Edge, We) of
	[_,_,_,_,_|_]=Vs ->
	    {X,Y,Z} = N = wings_face:draw_normal(Vs),
	    Tess = wings_draw:tess(),
	    glu:tessNormal(Tess, X, Y, Z),
	    glu:tessBeginPolygon(Tess),
	    glu:tessBeginContour(Tess),
	    draw_tess_face(Tess, Vs, N),
	    glu:tessEndContour(Tess),
	    glu:tessEndPolygon(Tess),
	    gl:edgeFlag(?GL_TRUE);
	Vs ->
	    gl:'begin'(?GL_POLYGON),
	    gl:normal3fv(wings_face:draw_normal(Vs)),
	    draw_face_normal_1(Vs),
	    gl:'end'()
    end.

draw_tess_face(Tess, [{Pos,Col}|T], Normal) ->
    glu:tessVertex(Tess, Pos, [{material,?GL_FRONT,?GL_DIFFUSE,Col},
			       {normal,Normal}]),
    draw_tess_face(Tess, T, Normal);
draw_tess_face(Tess, [], Normal) -> ok.

draw_face_normal_1([{Pos,Col}|T]) ->
    gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, Col),
    gl:vertex3fv(Pos),
    draw_face_normal_1(T);
draw_face_normal_1([]) -> ok.

%%
%% Draw the currently selected items.
%%

draw_selection(#st{selmode=body}=St) ->
    wings_sel:foreach(
      fun(_, #shape{sh=Data}) ->
	      draw_faces(Data, false, St)
      end, St),
    St;
draw_selection(#st{selmode=face}=St) ->
    wings_sel:foreach(
      fun(Face, #shape{sh=#we{fs=Ftab}=We}) ->
	      #face{edge=Edge} = gb_trees:get(Face, Ftab),
	      draw_face_normal(Face, Edge, We)
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

ground_and_axes() ->
    Ground = wings_pref:get_value(show_groundplane),
    Axes = wings_pref:get_value(show_axes),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    case Ground of
	true -> groundplane(Axes);
	false -> ok
    end,
    ?CHECK_ERROR(),
    case Axes of
	true ->
	    axis(1, get_pref(x_color), get_pref(neg_x_color)),
	    axis(2, get_pref(y_color), get_pref(neg_y_color)),
	    axis(3, get_pref(z_color), get_pref(neg_z_color));
	false -> ok
    end,
    ?CHECK_ERROR(),
    gl:popAttrib().

get_pref(Key) ->
    wings_pref:get_value(Key).

axis(I, Pos, Neg) ->
    A0 = {0.0,0.0,0.0},
    A = setelement(I, A0, 1000.0),
    B = setelement(I, A0, -1000.0),
    gl:'begin'(?GL_LINES),
    gl:color3fv(Pos),
    gl:vertex3fv(A0),
    gl:vertex3fv(A),
    gl:color3fv(Neg),
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
