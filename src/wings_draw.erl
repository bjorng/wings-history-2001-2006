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
%%     $Id: wings_draw.erl,v 1.74 2002/05/07 06:22:09 bjorng Exp $
%%

-module(wings_draw).
-export([model_changed/0,model_changed/1,sel_changed/1,
	 clear_orig_sel/0,make_vec_dlist/1,
	 update_display_lists/2,
	 draw_faces/2,
	 smooth_faces/2,
	 get_dlist/0,put_dlist/1,render/1,ground_and_axes/0,
	 axis_letters/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1]).

model_changed(St) ->
    model_changed(),
    St.

model_changed() ->
    case erase(wings_dlist) of
	undefined -> ok;
	#dl{faces=D1,smooth=D2,vs=D3,pick=D4,normals=D5,smoothed=D6} ->
	    del_list(D1), del_list(D2), del_list(D3),
	    del_list(D4), del_list(D5), del_list(D6)
    end.

del_list(none) -> ok;
del_list(DL) -> gl:deleteLists(DL, 1).

get_dlist() ->
    get(wings_dlist).

put_dlist(DL) ->
    put(wings_dlist, DL).

sel_changed(St) ->
    Dl = get(wings_dlist),
    put(wings_dlist, Dl#dl{old_sel=none,sel=none}),
    St.

clear_orig_sel() ->
    case wings_draw:get_dlist() of
	#dl{orig_sel={_,DlistSel}}=DL ->
	    gl:deleteLists(DlistSel, 1),
	    wings_draw:put_dlist(DL#dl{orig_sel=none});
	_Other -> ok
    end.

-define(DL_FACES, (?DL_DRAW_BASE)).
-define(DL_VERTICES, (?DL_DRAW_BASE+1)).
-define(DL_SEL1, (?DL_DRAW_BASE+2)).
-define(DL_SEL2, (?DL_DRAW_BASE+3)).
-define(DL_NORMALS, (?DL_DRAW_BASE+4)).
-define(DL_SMOOTH, (?DL_DRAW_BASE+5)).

%%
%% Renders all shapes, including selections.
%%

render(St) ->
    ?CHECK_ERROR(),
    gl:enable(?GL_DEPTH_TEST),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    Workmode = wings_pref:get_value(workmode),
    update_display_lists(Workmode, St),
    make_sel_dlist(not Workmode, St),
    make_normals_dlist(St),
    make_vec_dlist(St),
    wings_view:projection(),
    wings_view:model_transformations(),
    ground_and_axes(),
    show_saved_bb(St),
    case Workmode of
	false -> render_smooth(St);
	true -> render_plain(St)
    end,
    draw_normals(),
    gl:callList(?DL_UTIL),
    axis_letters(),
    gl:popAttrib(),
    ?CHECK_ERROR(),
    St.

render_smooth(St) ->
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),
    gl:shadeModel(?GL_SMOOTH),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:enable(?GL_BLEND),
    ?CHECK_ERROR(),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:polygonOffset(2.0, 2.0),
    #dl{smooth=DlistSmooth} = get_dlist(),
    gl:callList(DlistSmooth),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_CULL_FACE),
    case wings_pref:get_value(wire_mode) of
	false -> ok;
	true ->
	    gl:color3f(1.0, 1.0, 1.0),
	    gl:lineWidth(?NORMAL_LINEWIDTH),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:enable(?GL_POLYGON_OFFSET_LINE),
	    gl:polygonOffset(1.0, 1.0),
	    draw_faces()
    end,

    ?CHECK_ERROR(),
    draw_orig_sel(),
    draw_sel(St).

render_plain(#st{selmode=SelMode}=St) ->
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
		{_,body} -> gl:color3f(0.3, 0.3, 0.3);
		{_,_} -> gl:color3f(0.0, 0.0, 0.0)
	    end,
	    gl:lineWidth(case SelMode of
			     edge -> wings_pref:get_value(edge_width);
			     _ -> ?NORMAL_LINEWIDTH end),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:enable(?GL_POLYGON_OFFSET_LINE),
	    gl:polygonOffset(1.0, 1.0),
	    case wings_pref:get_value(show_wire_backfaces) of
		true ->
		    gl:disable(?GL_CULL_FACE),
		    draw_faces(),
		    gl:enable(?GL_CULL_FACE);
		false ->
		    draw_faces()
	    end
    end,

    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_POINT),
    gl:disable(?GL_POLYGON_OFFSET_FILL),

    ?CHECK_ERROR(),
    draw_hilite(St),
    ?CHECK_ERROR(),
    draw_orig_sel(),
    draw_sel(St),
    draw_vertices(St),
    ?CHECK_ERROR(),
    draw_hard_edges(St).

draw_hilite(#st{hilite=none}) -> ok;
draw_hilite(#st{hilite=Hilite}) -> Hilite().

draw_vertices(#st{selmode=vertex}=St) ->
    case get_dlist() of
	#dl{vs=none}=DL ->
    	    gl:newList(?DL_VERTICES, ?GL_COMPILE),
	    draw_vertices_1(St),
	    gl:endList(),
	    put_dlist(DL#dl{vs=?DL_VERTICES}),
	    gl:callList(?DL_VERTICES);
	#dl{vs=DlistVs} ->
	    gl:callList(DlistVs)
    end;
draw_vertices(_) -> ok.

draw_vertices_1(#st{shapes=Shs}) ->
    case wings_pref:get_value(vertex_size) of
	0.0 -> ok;
	PtSize -> 
	    gl:pointSize(PtSize),
	    gl:color3f(0, 0, 0),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun(#we{vs=Vtab,perm=Perm}) when ?IS_VISIBLE(Perm) ->
			    draw_vertices_2(gb_trees:values(Vtab));
		       (#we{}) -> ok
		    end, gb_trees:values(Shs)),
	    gl:'end'()
    end.

draw_vertices_2(Vs) ->
    foreach(fun(#vtx{pos=Pos}) -> gl:vertex3fv(Pos) end, Vs).

draw_orig_sel() ->
    case get_dlist() of
	#dl{orig_sel=none} -> ok;
	#dl{orig_sel={Mode,DL}}-> draw_orig_sel_1(Mode, DL)
    end.

draw_orig_sel_1(vertex, DlistSel) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)*2),
    gl:depthMask(?GL_FALSE),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    {R0,G0,B0} = wings_pref:get_value(selected_color),
    gl:color4f(R0, G0, B0, 0.5),
    gl:callList(DlistSel),
    gl:disable(?GL_BLEND),
    gl:depthMask(?GL_TRUE);
draw_orig_sel_1(edge, DlistSel) ->
    gl:enable(?GL_LINE_STIPPLE),
    gl:lineStipple(2, 16#AAAA),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:depthMask(?GL_FALSE),
    gl:callList(DlistSel),
    gl:depthMask(?GL_TRUE),
    gl:disable(?GL_LINE_STIPPLE);
draw_orig_sel_1(_, DlistSel) ->
    gl:enable(?GL_POLYGON_STIPPLE),
    gl:depthMask(?GL_FALSE),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:callList(DlistSel),
    gl:depthMask(?GL_TRUE),
    gl:disable(?GL_POLYGON_STIPPLE).

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_sel(#st{selmode=edge}) ->
    #dl{sel=DlistSel} = get_dlist(),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:callList(DlistSel);
draw_sel(#st{selmode=vertex}) ->
    sel_color(),
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    #dl{sel=DlistSel} = get_dlist(),
    gl:callList(DlistSel);
draw_sel(_St) ->
    #dl{sel=DlistSel} = get_dlist(),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:callList(DlistSel).
    
draw_faces() ->
    #dl{faces=DlistFaces} = get_dlist(),
    gl:callList(DlistFaces).

update_display_lists(Workmode, St) ->
    case get_dlist() of
	undefined ->
	    put_dlist(#dl{}),
	    update_display_lists(Workmode, St);
	#dl{faces=none}=DL when Workmode == true ->
	    update_display_lists_plain(DL, St);
	#dl{smooth=none}=DL when Workmode == false ->
	    update_display_lists_smooth(DL, St);
	_ -> ok
    end,
    case {wings_pref:get_value(wire_mode),get_dlist()} of
	{true,#dl{faces=none}} ->
	    update_display_lists(true, St);
	{_,_} -> ok
    end.

update_display_lists_smooth(DL0, #st{shapes=Shs}=St) ->
    gl:newList(?DL_SMOOTH, ?GL_COMPILE),
    foreach(fun(#we{perm=Perm}=We) when ?IS_VISIBLE(Perm) ->
		    smooth_faces(We, St);
	       (#we{}) -> ok
	    end, gb_trees:values(Shs)),
    gl:endList(),
    put_dlist(DL0#dl{smooth=?DL_SMOOTH}).

update_display_lists_plain(DL0, #st{shapes=Shs}=St) ->
    gl:newList(?DL_FACES, ?GL_COMPILE),
    foreach(fun(#we{perm=Perm}=We) when ?IS_VISIBLE(Perm) ->
		    draw_faces(We, St);
	       (#we{}) -> ok
	    end, gb_trees:values(Shs)),
    gl:endList(),
    put_dlist(DL0#dl{faces=?DL_FACES}).
    
make_sel_dlist(Smooth, St) ->
    case {get_dlist(),St} of
	{#dl{sel=none},_} -> do_make_sel_dlist(Smooth, St);
	{#dl{old_sel=Sel},#st{sel=Sel}} -> St;
	{_,_} -> do_make_sel_dlist(Smooth, St)
    end.

do_make_sel_dlist(false, #st{selmode=body,sel=Sel,shapes=Shs}=St) ->
    DL = get_dlist(),
    DlistSel = get_sel_dlist(DL),
    gl:newList(DlistSel, ?GL_COMPILE),
    case {gb_trees:size(Shs),length(Sel)} of
	{Sz,Sz} ->
	    sel_color(),
	    draw_faces();
	{_,_} -> draw_selection(St)
    end,
    gl:endList(),
    put_dlist(DL#dl{old_sel=Sel,sel=DlistSel});
do_make_sel_dlist(_Smooth, #st{sel=Sel}=St) ->
    DL = get_dlist(),
    DlistSel = get_sel_dlist(DL),
    gl:newList(DlistSel, ?GL_COMPILE),
    draw_selection(St),
    gl:endList(),
    put_dlist(DL#dl{old_sel=Sel,sel=DlistSel}).

get_sel_dlist(#dl{orig_sel=none}) -> ?DL_SEL1;
get_sel_dlist(_) -> ?DL_SEL2.
    
draw_faces(#we{mode=uv}=We, #st{mat=Mtab}=St) ->
    case wings_pref:get_value(show_textures) of
	true ->
	    MatFaces = mat_faces(We),
	    draw_uv_faces(MatFaces, We, Mtab);
	false ->
	    draw_faces(We#we{mode=none}, St)
    end;
draw_faces(#we{fs=Ftab}=We, _St) ->
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {1,1,1,1}),
    wings_draw_util:begin_end(
      fun() ->
	      draw_faces_1(gb_trees:to_list(Ftab), We)
      end).

draw_faces_1([{Face,#face{edge=Edge}}|Fs], We) ->
    wings_draw_util:face(Face, Edge, We),
    draw_faces_1(Fs, We);
draw_faces_1([], _) -> ok.

draw_uv_faces([{Mat,Faces}|T], We, Mtab) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_material:apply_material(Mat, Mtab),
    wings_draw_util:begin_end(
      fun() ->
	      draw_uv_faces_1(Faces, We)
      end),
    gl:popAttrib(),
    draw_uv_faces(T, We, Mtab);
draw_uv_faces([], _We, _Mtab) -> ok.

draw_uv_faces_1([{Face,Edge}|Fs], We) ->
    wings_draw_util:face(Face, Edge, We),
    draw_uv_faces_1(Fs, We);
draw_uv_faces_1([], _We) -> ok.

mat_faces(#we{fs=Ftab}) ->
    mat_faces(gb_trees:to_list(Ftab), []).

mat_faces([{Face,#face{mat=Mat,edge=Edge}}|Fs], Acc) ->
    mat_faces(Fs, [{Mat,{Face,Edge}}|Acc]);
mat_faces([], Faces0) ->
    Faces1 = sofs:relation(Faces0),
    Faces = sofs:relation_to_family(Faces1),
    sofs:to_external(Faces).

smooth_faces(#we{mode=vertex}=We, _St) ->
    Faces = wings_we:normals(We),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE),
    wings_draw_util:begin_end(fun() -> draw_smooth_vcolor(Faces) end),
    gl:disable(?GL_COLOR_MATERIAL);
smooth_faces(#we{mode=Mode}=We, #st{mat=Mtab}) ->
    Faces0 = wings_we:normals(We),
    Faces1 = sofs:relation(Faces0),
    Faces2 = sofs:relation_to_family(Faces1),
    Faces = sofs:to_external(Faces2),
    case wings_pref:get_value(show_textures) of
	false when Mode == uv ->
	    draw_smooth_plain(Faces0);
	_Other ->
	    draw_smooth_1(Faces, Mtab)
    end.
    
draw_smooth_1([{Mat,Faces}|T], Mtab) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_material:apply_material(Mat, Mtab),
    wings_draw_util:begin_end(
      fun() ->
	      draw_smooth_2(Faces)
      end),
    gl:popAttrib(),
    draw_smooth_1(T, Mtab);
draw_smooth_1([], _Mtab) -> ok.

draw_smooth_2([Vs|Fs]) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, 0, 0, 0),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{{_,_}=UV,N}}) ->
		    glu:tessVertex(Tess, P, [{texcoord2,UV},{normal,N}]);
	       ({P,{_,N}}) ->
		    glu:tessVertex(Tess, P, [{normal,N}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_2(Fs);
draw_smooth_2([]) -> ok.

%% Smooth drawing for vertex colors.
draw_smooth_vcolor([{_,Vs}|T]) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, 0, 0, 0),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{{_,_,_}=Diff,N}}) ->
		    glu:tessVertex(Tess, P,
				   [{normal,N},
				    {color,Diff}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_vcolor(T);
draw_smooth_vcolor([]) -> ok.

%% Smooth drawing without any materials.
draw_smooth_plain(Faces) ->
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {1,1,1,1}),
    wings_draw_util:begin_end(
      fun() ->
	      draw_smooth_plain_1(Faces)
      end).

draw_smooth_plain_1([{_,Vs}|T]) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, 0, 0, 0),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{_,N}}) ->
		    glu:tessVertex(Tess, P, [{normal,N}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_plain_1(T);
draw_smooth_plain_1([]) -> ok.

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

draw_selection(#st{selmode=body}=St) ->
    sel_color(),
    wings_sel:foreach(
      fun(_, We) ->
	      draw_faces(We, St)
      end, St);
draw_selection(#st{selmode=face}=St) ->
    sel_color(),
    wings_draw_util:begin_end(
      fun() ->
	      wings_sel:foreach(
		fun(Face, #we{fs=Ftab}=We) ->
			#face{edge=Edge} = gb_trees:get(Face, Ftab),
			wings_draw_util:face(Face, Edge, We)
		end, St)
      end);
draw_selection(#st{selmode=edge}=St) ->
    sel_color(),
    gl:'begin'(?GL_LINES),
    wings_sel:foreach(
      fun(Edge, #we{es=Etab,vs=Vtab}) ->
	      #edge{vs=Vstart,ve=Vend} = gb_trees:get(Edge, Etab),
	      gl:vertex3fv(lookup_pos(Vstart, Vtab)),
	      gl:vertex3fv(lookup_pos(Vend, Vtab))
      end, St),
    gl:'end'();
draw_selection(#st{selmode=vertex,shapes=Shapes,sel=Sel}) ->
    draw_vtx_sel(gb_trees:to_list(Shapes), Sel).

draw_vtx_sel([{Id1,_}|Shs], [{Id2,_}|_]=Sel) when Id1 < Id2 ->
    draw_vtx_sel(Shs, Sel);
draw_vtx_sel([{Id,#we{vs=Vtab0}}|Shs], [{Id,Vs}|Sel]) ->
    Vtab = sofs:from_external(gb_trees:to_list(Vtab0), [{vertex,data}]),
    R = sofs:from_external(gb_sets:to_list(Vs), [vertex]),
    SelVs = sofs:restriction(Vtab, R),
    DrawFun = fun({_,#vtx{pos=Pos}}) -> gl:vertex3fv(Pos) end,
    gl:'begin'(?GL_POINTS),
    foreach(DrawFun, sofs:to_external(SelVs)),
    gl:'end'(),
    draw_vtx_sel(Shs, Sel);
draw_vtx_sel([_|_], []) -> ok;
draw_vtx_sel([], []) -> ok.

lookup_pos(Key, Tree) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tree),
    Pos.

%%
%% Draw face normals.
%%

draw_normals() ->
    case get_dlist() of
	#dl{normals=none} -> ok;
	#dl{normals={_,DL}} -> gl:callList(DL)
    end.

make_normals_dlist(#st{selmode=Mode}=St) ->
    #dl{normals=OldDl} = DL = get_dlist(),
    case {wings_pref:get_value(show_normals),OldDl} of
	{false,none} -> ok;
	{false,_} -> put_dlist(DL#dl{normals=none});
	{true,{Mode,DL}} -> ok;
	{true,_} ->
	    gl:newList(?DL_NORMALS, ?GL_COMPILE),
	    make_normals_dlist_1(St),
	    gl:endList(),
	    put_dlist(DL#dl{normals={Mode,?DL_NORMALS}})
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
make_normals_dlist_2(_, #we{fs=Ftab}=We) ->
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
    Axes = wings_pref:get_value(show_axes),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    groundplane(Axes),
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
	    MM = list_to_tuple(gl:getDoublev(?GL_MODELVIEW_MATRIX)),
	    PM = list_to_tuple(gl:getDoublev(?GL_PROJECTION_MATRIX)),
	    ViewPort = gl:getIntegerv(?GL_VIEWPORT),
	    Info = {MM,PM,ViewPort},
	    axis_letter(1, axisx, wings_pref:get_value(x_color), Info),
	    axis_letter(2, axisy, wings_pref:get_value(y_color), Info),
 	    axis_letter(3, axisz, wings_pref:get_value(z_color), Info)
    end.

axis_letter(I, Char, Color, {MM,PM,Viewport}) ->
    Start = {0.0,0.0,0.0},
    End = setelement(I, Start, 1000.0),
    {Ox,Oy,_,Ow} = proj(Start, MM, PM),
    {Px,Py,_,Pw0} = proj(End, MM, PM),
    Pw = abs(Pw0),
    if
	-Pw < Px, Px < Pw, -Pw < Py, Py < Pw ->
	    show_letter(Px, Py, Pw, Char, Color, Viewport);
	true ->
	    clip(Ox, Oy, Ow, Px, Py, Pw, Char, Color, Viewport)
    end.

clip(Ox, Oy, Ow, Px, Py, Pw, Char, Color, Viewport) ->
    AxisRay = line(Ox, Oy, Px, Py),
    Lines = [line(-Ow, -Ow, Ow, -Ow),line(-Ow, Ow, Ow, Ow),
	     line(-Ow, -Ow, -Ow, Ow),line(Ow, -Ow, Ow, Ow)],
    case clip_1(AxisRay, Lines, {Ow,Pw}) of
	none -> ok;
	{X,Y,W} -> show_letter(X, Y, W, Char, Color, Viewport)
    end.

clip_1({O1,D1}=Axis, [{O2,D2}|Lines], {Ow,_}=W) ->
    E = 1.0E-6,
    case {pdot(D1, D2),pdot(D2, D1)} of
	{Z1,Z2} when abs(Z1) < E; abs(Z2) < E ->
	    clip_1(Axis, Lines, W);
	{Div1,Div2} ->
	    S = pdot(sub(O2, O1), D2)/Div1,
	    T = pdot(sub(O1, O2), D1)/Div2,
	    if
		S < 0.0; T < 0.0; T > 1.0 ->
		    clip_1(Axis, Lines, W);
		true ->
		    {X,Y} = add(O1, mul(D1, S)),
		    {X,Y,Ow}
	    end
    end;
clip_1(_, [], _W) -> none.

show_letter(X0, Y0, W, Char, Color, [Vx,Vy,Vw,Vh]) ->
    X = (0.5*X0/W + 0.5)*Vw + Vx,
    Y = (0.5*Y0/W + 0.5)*Vh + Vy,
    wings_io:axis_text(X, Y, Char, Color).
	    
proj({X0,Y0,Z0}, MM, PM) ->
    Vec = e3d_mat:mul(MM, {X0,Y0,Z0,1.0}),
    e3d_mat:mul(PM, Vec).

line(Ox, Oy, Px, Py) -> {{Ox,Oy},{Px-Ox,Py-Oy}}.

pdot({X1,Y1}, {X2,Y2}) -> Y1*X2-X1*Y2.
add({X1,Y1}, {X2,Y2}) -> {X1+X2,Y1+Y2}.
sub({X1,Y1}, {X2,Y2}) -> {X1-X2,Y1-Y2}.
mul({X,Y}, S) -> {X*S,Y*S}.

groundplane(Axes) ->
    case (wings_pref:get_value(show_groundplane) orelse
	  (wings_pref:get_value(force_show_along_grid) andalso
	   (wings_view:current())#view.along_axis =/= none)) of
	true -> groundplane_1(Axes);
	false -> ok
    end.

groundplane_1(Axes) ->
    #view{along_axis=Along} = wings_view:current(),
    gl:color3fv(wings_pref:get_value(grid_color)),
    ?CHECK_ERROR(),
    gl:lineWidth(?NORMAL_LINEWIDTH),
    gl:'begin'(?GL_LINES),
    Sz = ?GROUND_GRID_SIZE * 10,
    groundplane(Along, -Sz, Sz, Sz, Axes),
    gl:'end'(),
    ?CHECK_ERROR().

groundplane(_Along, X, Last, _Sz, _Axes) when X > Last -> ok;
groundplane(Along, 0.0, Last, Sz, true) ->
    groundplane(Along, ?GROUND_GRID_SIZE, Last, Sz, true);
groundplane(Along, X, Last, Sz, Axes) ->
    case Along of
	x ->
            gl:vertex3f(0, X, -Sz),
            gl:vertex3f(0, X, Sz),
            gl:vertex3f(0, -Sz, X),
            gl:vertex3f(0, Sz, X);
        z ->
            gl:vertex3f(X, -Sz, 0),
            gl:vertex3f(X, Sz, 0),
            gl:vertex3f(-Sz, X, 0),
            gl:vertex3f(Sz, X, 0);
	_Other ->
            gl:vertex3f(X, 0, -Sz),
            gl:vertex3f(X, 0, Sz),
            gl:vertex3f(-Sz, 0, X),
            gl:vertex3f(Sz, 0, X)
    end,
    groundplane(Along, X+?GROUND_GRID_SIZE, Last, Sz, Axes).

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

%%%
%%% Show active vector.
%%%

make_vec_dlist(#st{vec=none}) ->
    gl:newList(?DL_UTIL, ?GL_COMPILE),
    gl:endList();
make_vec_dlist(#st{vec={Center,Vec}}) ->
    gl:newList(?DL_UTIL, ?GL_COMPILE),
    draw_vec(Center, Vec),
    gl:endList();
make_vec_dlist(#st{vec=Center}) ->
    gl:newList(?DL_UTIL, ?GL_COMPILE),
    Width = wings_pref:get_value(active_vector_width),
    gl:color3fv(wings_pref:get_value(active_vector_color)),
    gl:pointSize(Width*3.5),
    gl:'begin'(?GL_POINTS),
    gl:vertex3fv(Center),
    gl:'end'(),
    gl:endList().

draw_vec(Center, Vec0) ->
    Vec = e3d_vec:mul(Vec0, wings_pref:get_value(active_vector_size)),
    End = e3d_vec:add(Center,Vec),
    HeadVec = e3d_vec:mul(Vec, -0.2),
    HeadPt = e3d_vec:add(End, HeadVec),
    case HeadVec of
	{Same,Same,Same} ->
	    PosHead0 = e3d_vec:cross(HeadVec, {0.25,-0.25,0.25}),
	    PosHead1 = e3d_vec:cross(HeadVec, {-0.25,0.25,-0.25});
	_Other ->
	    PosHead0 = e3d_vec:cross(HeadVec, {0.25,0.25,0.25}),
	    PosHead1 = e3d_vec:cross(HeadVec, {-0.25,-0.25,-0.25})
    end,
    Width = wings_pref:get_value(active_vector_width),
    gl:color3fv(wings_pref:get_value(active_vector_color)),
    gl:pointSize(Width*3.5),
    gl:lineWidth(Width),
    gl:'begin'(?GL_LINES),
    gl:vertex3fv(Center),
    gl:vertex3fv(End),
    gl:vertex3fv(End),
    gl:vertex3fv(e3d_vec:sub(HeadPt,PosHead0)),
    gl:vertex3fv(End),
    gl:vertex3fv(e3d_vec:sub(HeadPt,PosHead1)),
    gl:'end'().
