%%
%%  wings_draw.erl --
%%
%%     This module draws objects using OpenGL.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw.erl,v 1.49 2002/01/12 19:24:25 bjorng Exp $
%%

-module(wings_draw).
-export([model_changed/1,sel_changed/1,
	 get_dlist/0,put_dlist/1,render/1,ground_and_axes/0,
	 axis_letters/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1]).

model_changed(St) ->
    erase(wings_dlist),
    St.

get_dlist() ->
    get(wings_dlist).

put_dlist(DL) ->
    put(wings_dlist, DL).

sel_changed(St) ->
    Dl = get(wings_dlist),
    put(wings_dlist, Dl#dl{old_sel=none,sel=none}),
    St.

-define(DL_FACES, (?DL_DRAW_BASE)).
-define(DL_EDGES, (?DL_DRAW_BASE+1)).
-define(DL_SEL, (?DL_DRAW_BASE+2)).
-define(DL_NORMALS, (?DL_DRAW_BASE+3)).

%%
%% Renders all shapes, including selections.
%%

render(St) ->
    ?CHECK_ERROR(),
    gl:enable(?GL_DEPTH_TEST),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    Smooth = wings_pref:get_value(smooth_preview),
    update_display_lists(St),
    make_sel_dlist(Smooth, St),
    make_normals_dlist(St),
    wings_view:projection(),
    wings_view:model_transformations(),
    ground_and_axes(),
    show_saved_bb(St),
    case Smooth of
	true -> draw_smooth_shapes(St);
	false -> draw_plain_shapes(St)
    end,
    draw_normals(),
    axis_letters(),
    gl:popAttrib(),
    ?CHECK_ERROR(),
    St.

draw_smooth_shapes(St) ->
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),
    gl:shadeModel(?GL_SMOOTH),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:polygonOffset(2.0, 2.0),
    draw_faces(),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_CULL_FACE),
    ?CHECK_ERROR(),
    draw_sel(St).

draw_plain_shapes(#st{selmode=SelMode}=St) ->
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),

    %% Draw faces for winged-edge-objects.
    Wire = wings_pref:get_value(wire_mode),
    case Wire of
	true -> ok;
	false ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    gl:polygonOffset(2.0, 2.0),
	    gl:shadeModel(?GL_SMOOTH),
	    gl:enable(?GL_LIGHTING),
	    draw_faces(),
	    gl:disable(?GL_LIGHTING),
	    gl:shadeModel(?GL_FLAT)
    end,

    %% Draw edges.
    case Wire orelse wings_pref:get_value(show_edges) of
	false -> ok;
	true ->
	    case {Wire,SelMode} of
		{true,_} -> gl:color3f(1.0, 1.0, 1.0);
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
	    draw_faces()
    end,

    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_POINT),
    gl:disable(?GL_POLYGON_OFFSET_FILL),

    ?CHECK_ERROR(),
    draw_hilite(St),
    ?CHECK_ERROR(),
    draw_sel(St),
    ?CHECK_ERROR(),
    draw_hard_edges(St).

draw_hilite(#st{hilite=none}) -> ok;
draw_hilite(#st{hilite=Hilite}) -> Hilite().

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_sel(#st{selmode=edge}) ->
    #dl{sel=DlistSel} = get_dlist(),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:callList(DlistSel);
draw_sel(#st{selmode=vertex}) ->
    #dl{sel=DlistSel} = get_dlist(),
    gl:callList(DlistSel);
draw_sel(St) ->
    #dl{sel=DlistSel} = get_dlist(),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:callList(DlistSel).
    
draw_faces() ->
    #dl{faces=DlistFaces} = get_dlist(),
    gl:callList(DlistFaces).

update_display_lists(#st{shapes=Shapes}=St) ->
    case get_dlist() of
	undefined ->
	    Smooth = wings_pref:get_value(smooth_preview),
	    gl:newList(?DL_FACES, ?GL_COMPILE),
	    foreach(fun(#we{perm=Perm}=We) when ?IS_VISIBLE(Perm) ->
			    draw_faces(We, Smooth, St);
		       (#we{}) -> ok
		    end, gb_trees:values(Shapes)),
	    gl:endList(),
	    put_dlist(#dl{faces=?DL_FACES});
	DL -> ok
    end.

make_sel_dlist(Smooth, St) ->
    case {get_dlist(),St} of
	{#dl{sel=none},_} -> do_make_sel_dlist(Smooth, St);
	{#dl{old_sel=Sel},#st{sel=Sel}} -> St;
	{_,_} -> do_make_sel_dlist(Smooth, St)
    end.

do_make_sel_dlist(false, #st{selmode=body,sel=Sel,shapes=Shs}=St) ->
    DL = get_dlist(),
    DlistSel = ?DL_SEL,
    gl:newList(DlistSel, ?GL_COMPILE),
    case {gb_trees:size(Shs),length(Sel)} of
	{Sz,Sz} ->
	    sel_color(),
	    draw_faces();
	{_,_} ->
	    draw_selection(false, St)
    end,
    gl:endList(),
    put_dlist(DL#dl{old_sel=Sel,sel=DlistSel});
do_make_sel_dlist(Smooth, St) -> do_make_sel_dlist_1(Smooth, St).

do_make_sel_dlist_1(Smooth, #st{sel=Sel}=St) ->
    DlistSel = ?DL_SEL,
    gl:newList(DlistSel, ?GL_COMPILE),
    draw_selection(Smooth, St),
    gl:endList(),
    DL = get_dlist(),
    put_dlist(DL#dl{old_sel=Sel,sel=DlistSel}).
    
draw_faces(We, true, #st{mat=Mtab}) ->
    draw_smooth_faces(Mtab, We);
draw_faces(#we{fs=Ftab}=We, false, St) ->
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {1.0,1.0,1.0}),
    foreach(fun({Face,#face{edge=Edge}}) ->
		    wings_draw_util:face(Face, Edge, We)
	    end, gb_trees:to_list(Ftab)).

draw_smooth_faces(Mtab, #we{mode=vertex}=We) ->
    Faces = wings_we:normals(We),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE),
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

draw_smooth_2([[_,_,_,_|_]=Vs|Fs]) ->
    %% This face needs tesselation.
    Tess = wings_draw_util:tess(),
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
    foreach(fun({P,{{U,V},N}}) ->
		    gl:texCoord2f(U, V),
 		    gl:normal3fv(N),
 		    gl:vertex3fv(P);
	       ({P,{Diff,N}}) ->
 		    gl:normal3fv(N),
 		    gl:vertex3fv(P)
 	    end, Vs),
    gl:'end'(),
    draw_smooth_2(Fs);
draw_smooth_2([]) -> ok.

%% Smooth drawing for vertex colors.
draw_smooth_vcolor([{_,[_,_,_,_|_]=Vs}|T]) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, 0, 0, 0),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{{R,G,B}=Diff,N}}) ->
		    glu:tessVertex(Tess, P,
				   [{normal,N},
				    {color,Diff}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_vcolor(T);
draw_smooth_vcolor([{_,Vs}|T]) ->
    gl:'begin'(?GL_POLYGON),
    foreach(fun({P,{{R,G,B}=Diff,N}}) ->
 		    gl:normal3fv(N),
		    gl:color3f(R, G, B),
 		    gl:vertex3fv(P)
 	    end, Vs),
    gl:'end'(),
    draw_smooth_vcolor(T);
draw_smooth_vcolor([]) -> ok.

draw_hard_edges(#st{shapes=Shapes}) ->
    gl:color3fv(wings_pref:get_value(hard_edge_color)),
    foreach(
      fun(#we{perm=Perm}) when ?IS_NOT_VISIBLE(Perm) -> ok;
	 (#we{he=Htab}=We) ->
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

draw_selection(Smooth, #st{selmode=body}=St) ->
    sel_color(),
    wings_sel:foreach(
      fun(_, We) ->
	      draw_faces(We, false, St)
      end, St);
draw_selection(Smooth, #st{selmode=face}=St) ->
    sel_color(),
    wings_sel:foreach(
      fun(Face, #we{fs=Ftab}=We) ->
	      #face{edge=Edge} = gb_trees:get(Face, Ftab),
	      wings_draw_util:face(Face, Edge, We)
      end, St);
draw_selection(Smooth, #st{selmode=edge}=St) ->
    sel_color(),
    gl:'begin'(?GL_LINES),
    wings_sel:foreach(
      fun(Edge, #we{es=Etab,vs=Vtab}) ->
	      #edge{vs=Vstart,ve=Vend} = gb_trees:get(Edge, Etab),
	      gl:vertex3fv(lookup_pos(Vstart, Vtab)),
	      gl:vertex3fv(lookup_pos(Vend, Vtab))
      end, St),
    gl:'end'();
draw_selection(Smooth, #st{selmode=vertex}=St) ->
    draw_vtx_sel(Smooth, St).

draw_vtx_sel(Smooth, #st{shapes=Shapes,sel=Sel}) ->
    draw_vtx_sel(gb_trees:to_list(Shapes), Sel, Smooth).

draw_vtx_sel([{Id1,#we{vs=Vtab}}|Shs], [{Id2,_}|_]=Sel, true)
  when Id1 < Id2 ->
    draw_vtx_sel(Shs, Sel, true);
draw_vtx_sel([{Id1,#we{vs=Vtab}=We}|Shs], [{Id2,_}|_]=Sel, false)
  when Id1 < Id2 ->
    draw_unsel_vtx(fun() ->
			   foreach(fun(#vtx{pos=Pos}) ->
					   gl:vertex3fv(Pos) end,
				   gb_trees:values(Vtab))
		   end, We),
    draw_vtx_sel(Shs, Sel, false);
draw_vtx_sel([{_,#we{vs=Vtab}}|Shs], [], true) -> ok;
draw_vtx_sel([{_,#we{vs=Vtab}=We}|Shs], [], false) ->
    draw_unsel_vtx(fun() ->
			   foreach(fun(#vtx{pos=Pos}) ->
					   gl:vertex3fv(Pos) end,
				   gb_trees:values(Vtab))
		   end, We),
    draw_vtx_sel(Shs, [], false);
draw_vtx_sel([{Id,#we{vs=Vtab0}=We}|Shs], [{Id,Vs}|Sel], Smooth) ->
    Vtab = sofs:from_external(gb_trees:to_list(Vtab0), [{vertex,data}]),
    R = sofs:from_external(gb_sets:to_list(Vs), [vertex]),
    SelVs = sofs:restriction(Vtab, R),
    DrawFun = fun({_,#vtx{pos=Pos}}) -> gl:vertex3fv(Pos) end,
    sel_color(),
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:'begin'(?GL_POINTS),
    foreach(DrawFun, sofs:to_external(SelVs)),
    gl:'end'(),
    case Smooth of
	true -> ok;
	false ->
	    NonSelVs = sofs:drestriction(Vtab, R),
	    draw_unsel_vtx(
	      fun() ->
		      foreach(DrawFun, sofs:to_external(NonSelVs))
	      end, We)
    end,
    draw_vtx_sel(Shs, Sel, Smooth);
draw_vtx_sel([], [], Smooth) -> ok.

draw_unsel_vtx(Draw, #we{perm=Perm}) when ?IS_NOT_VISIBLE(Perm)->
    ok;
draw_unsel_vtx(Draw, We) ->
    case wings_pref:get_value(vertex_size) of
	0.0 -> ok;
	PtSize -> 
	    gl:pointSize(PtSize),
	    gl:color3f(0, 0, 0),
	    gl:'begin'(?GL_POINTS),
	    Draw(),
	    gl:'end'()
    end.

lookup_pos(Key, Tree) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tree),
    Pos.

%%
%% Draw face normals.
%%

draw_normals() ->
    case get_dlist() of
	#dl{normals=none} -> ok;
	#dl{normals=DL} -> gl:callList(DL)
    end.

make_normals_dlist(St) ->
    #dl{normals=OldDl} = DL = get_dlist(),
    case {wings_pref:get_value(show_normals),OldDl} of
	{false,none} -> ok;
	{false,_} -> put_dlist(DL#dl{normals=none});
	{true,none} ->
	    gl:newList(?DL_NORMALS, ?GL_COMPILE),
	    make_normals_dlist_1(St),
	    gl:endList(),
	    put_dlist(DL#dl{normals=?DL_NORMALS});
	{true,_} -> ok
    end.

make_normals_dlist_1(#st{selmode=Mode,shapes=Shs}) ->
    gl:color3f(0, 0, 1),
    gl:lineWidth(2.0),
    foreach(fun(#we{perm=Perm}=We) when ?IS_VISIBLE(Perm) ->
		    make_normals_dlist_2(Mode, We);
	       (#we{}) -> ok
	    end, gb_trees:values(Shs)).

make_normals_dlist_2(vertex, #we{vs=Vtab}=We) ->
    gl:'begin'(?GL_LINES),
    foreach(fun(V) ->
		    Pos = lookup_pos(V, Vtab),
		    gl:vertex3fv(Pos),
		    N = wings_vertex:normal(V, We),
		    gl:vertex3fv(e3d_vec:add(Pos, e3d_vec:mul(N, 0.3)))
	    end, gb_trees:keys(Vtab)),
    gl:'end'();
make_normals_dlist_2(edge, #we{es=Etab,vs=Vtab}=We) ->
    gl:'begin'(?GL_LINES),
    foreach(fun(#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}) ->
		    PosA = lookup_pos(Va, Vtab),
		    PosB = lookup_pos(Vb, Vtab),
		    Mid = e3d_vec:average([PosA,PosB]),
		    gl:vertex3fv(Mid),
		    N = e3d_vec:average([wings_face:normal(Lf, We),
					 wings_face:normal(Rf, We)]),
		    gl:vertex3fv(e3d_vec:add(Mid, e3d_vec:mul(N, 0.3)))
	    end, gb_trees:values(Etab)),
    gl:'end'();

make_normals_dlist_2(Other, #we{fs=Ftab}=We) ->
    gl:'begin'(?GL_LINES),
    foreach(fun(Face) ->
		    Vs = wings_face:surrounding_vertices(Face, We),
		    C = wings_vertex:center(Vs, We),
		    gl:vertex3fv(C),
		    N = wings_face:face_normal(Vs, We),
		    gl:vertex3fv(e3d_vec:add(C, e3d_vec:mul(N, 0.3)))
	    end, gb_trees:keys(Ftab)),
    gl:'end'().
	    
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

axis_letters() ->
    case wings_pref:get_value(show_axis_letters) andalso
	wings_pref:get_value(show_axes) of
	false -> ok;
	true ->
	    axis_letter(1, axisx, wings_pref:get_value(x_color)),
	    axis_letter(2, axisy, wings_pref:get_value(y_color)),
	    axis_letter(3, axisz, wings_pref:get_value(z_color))
    end.

axis_letter(I, Char, Color) ->
    Start = {0.0,0.0,0.0},
    End = setelement(I, Start, 1000.0),
    FeedBuf = (get(wings_hitbuf))#sdlmem{type=?GL_FLOAT},
    gl:feedbackBuffer(10, ?GL_2D, FeedBuf),
    gl:renderMode(?GL_FEEDBACK),
    gl:'begin'(?GL_LINES),
    gl:vertex3fv(Start),
    gl:vertex3fv(End),
    gl:'end'(),
    case gl:renderMode(?GL_RENDER) of
	NumItems when NumItems >= 5 ->
	    <<_:?GL_FLOAT_SIZE/float,
	     _:?GL_FLOAT_SIZE/float,
	     _:?GL_FLOAT_SIZE/float,
	     Xclip:?GL_FLOAT_SIZE/float,
	     Yclip:?GL_FLOAT_SIZE/float,
	     _/binary>> = sdl_util:readBin(FeedBuf, NumItems);
	Other ->
	    Xclip = Yclip = 0.0
    end,
    wings_io:axis_text(Xclip, Yclip, Char, Color).

groundplane(Axes) ->
    gl:color3fv(wings_pref:get_value(grid_color)),
    ?CHECK_ERROR(),
    gl:lineWidth(0.1),
    gl:'begin'(?GL_LINES),
    Sz = ?GROUND_GRID_SIZE * 10,
    groundplane(-Sz, Sz, Sz, Axes),
    gl:'end'(),
    ?CHECK_ERROR().

groundplane(X, Last, Sz, Axes) when X > Last -> ok;
groundplane(0.0, Last, Sz, true) ->
    groundplane(?GROUND_GRID_SIZE, Last, Sz, true);
groundplane(X, Last, Sz, Axes) ->
    case (wings_view:current())#view.along_axis of
        x ->
            gl:vertex3f(0, X, -Sz),
            gl:vertex3f(0, X, Sz),
            gl:vertex3f(0, -Sz, X),
            gl:vertex3f(0, Sz, X);
        y ->
            gl:vertex3f(X, 0, -Sz),
            gl:vertex3f(X, 0, Sz),
            gl:vertex3f(-Sz, 0, X),
            gl:vertex3f(Sz, 0, X);
        z ->
            gl:vertex3f(X, -Sz, 0),
            gl:vertex3f(X, Sz, 0),
            gl:vertex3f(-Sz, X, 0),
            gl:vertex3f(Sz, X, 0)
    end,
    groundplane(X+?GROUND_GRID_SIZE, Last, Sz, Axes).

show_saved_bb(#st{bb=none}) -> ok;
show_saved_bb(#st{bb=[{X1,Y1,Z1},{X2,Y2,Z2}]}) ->
    case wings_pref:get_value(show_bb) of
	false -> ok;
	true ->
	    gl:enable(?GL_LINE_STIPPLE),
	    gl:lineStipple(4, 2#1110111011101110),
	    gl:color3f(0, 0, 1),
	    gl:'begin'(?GL_LINE_STRIP),
	    gl:vertex3f(X1, Y1, Z1),
	    gl:vertex3f(X2, Y1, Z1),
	    gl:vertex3f(X2, Y2, Z1),
	    gl:vertex3f(X1, Y2, Z1),
	    gl:vertex3f(X1, Y1, Z1),
	    gl:vertex3f(X1, Y1, Z2),
	    gl:vertex3f(X2, Y1, Z2),
	    gl:vertex3f(X2, Y2, Z2),
	    gl:vertex3f(X1, Y2, Z2),
	    gl:vertex3f(X1, Y1, Z2),
	    gl:'end'(),
	    gl:'begin'(?GL_LINES),
	    gl:vertex3f(X1, Y2, Z1),
	    gl:vertex3f(X1, Y2, Z2),
	    gl:vertex3f(X2, Y2, Z1),
	    gl:vertex3f(X2, Y2, Z2),
	    gl:vertex3f(X2, Y1, Z1),
	    gl:vertex3f(X2, Y1, Z2),
	    gl:'end'(),
	    gl:disable(?GL_LINE_STIPPLE)
    end.
