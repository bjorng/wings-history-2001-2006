%%
%%  wings_draw_util.erl --
%%
%%     Utilities for drawing objects.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw_util.erl,v 1.119 2004/01/25 17:55:47 bjorng Exp $
%%

-module(wings_draw_util).
-export([init/0,delete_dlists/0,tess/0,begin_end/1,begin_end/2,
	 update/2,map/2,fold/2,changed_materials/1,
	 render/1,call/1,
	 prepare/3,
	 plain_face/2,plain_face/3,uv_face/3,vcol_face/2,vcol_face/3,
	 smooth_plain_faces/2,smooth_uv_faces/2,smooth_vcol_faces/2,
	 unlit_face/2,unlit_face/3,
	 force_flat_color/2,force_flat_color/3,good_triangulation/5]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foreach/2,foldl/3]).

-record(du,
	{dl=[],					%Display list records.
	 mat=gb_trees:empty(),			%Materials.
	 used=[]				%Display lists in use.
	 }).

init() ->
    case get(wings_tesselator) of
	undefined -> ok;
	OldTess -> glu:deleteTess(OldTess)
    end,
    Tess = glu:newTess(),
    put(wings_tesselator, Tess),

    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_NONE),
    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_NONE),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
    glu:tessCallback(Tess, ?GLU_TESS_EDGE_FLAG, ?ESDL_TESSCB_NOP),
    glu:tessCallback(Tess, ?GLU_TESS_COMBINE, ?ESDL_TESSCB_COMBINE),

    Dl = case get_dl_data() of
	     undefined -> [];
	     #du{dl=Dl0,used=Used} ->
		 ?CHECK_ERROR(),
		 foreach(fun(DL) -> gl:deleteLists(DL, 1) end, Used),
		 gl:getError(),			%Clear error.
		 clear_old_dl(Dl0)
	 end,
    put_dl_data(#du{dl=Dl}),
    P = <<16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55>>,
    gl:polygonStipple(P).

delete_dlists() ->
    case erase(wings_wm:get_prop(display_lists)) of
	#du{used=Used} ->
	    foreach(fun(DL) -> gl:deleteLists(DL, 1) end, Used),
	    gl:getError();			%Clear error.
	_ ->
	    ok
    end.

clear_old_dl([#dlo{src_we=We,proxy_data=Pd0,ns=Ns}|T]) ->
    Pd = wings_subdiv:clean(Pd0),
    [#dlo{src_we=We,mirror=none,proxy_data=Pd,ns=Ns}|clear_old_dl(T)];
clear_old_dl([]) -> [].

tess() ->
    get(wings_tesselator).

begin_end(Body) ->
    wings__du:begin_end(?GL_TRIANGLES, Body).

begin_end(Type, Body) ->
    wings__du:begin_end(Type, Body).

get_dl_data() ->
    get(wings_wm:get_prop(display_lists)).

put_dl_data(Data) ->
    put(wings_wm:get_prop(display_lists), Data).

%%
%% Get a list of all materials that were changed since the last time
%% the display lists were updated. (The list does not include materials
%% that were deleted or added.)
%%

changed_materials(#st{mat=NewMat}) ->
    case get_dl_data() of
	#du{mat=NewMat} -> [];
	#du{mat=OldMat}=Du ->
	    put_dl_data(Du#du{mat=NewMat}),
	    changed_materials_1(gb_trees:to_list(OldMat), NewMat, [])
    end.

changed_materials_1([{Name,Val}|T], New, Acc) ->
    case gb_trees:lookup(Name, New) of
	none -> changed_materials_1(T, New, Acc);
	{value,Val} -> changed_materials_1(T, New, Acc);
	{value,_} -> changed_materials_1(T, New, [Name|Acc])
    end;
changed_materials_1([], _, Acc) -> Acc.

%%
%% Update allows addition of new objects at the end.
%%

update(Fun, Data) ->
    #du{dl=Dlists} = get_dl_data(),
    update_1(Fun, Dlists, Data, [], []).

update_1(Fun, [D0|Dlists], Data0, Seen0, Acc) ->
    case Fun(D0, Data0) of
	#dlo{}=D ->
	    Seen = update_seen(D, Seen0),
	    update_1(Fun, Dlists, Data0, Seen, [D|Acc]);
	deleted ->
	    update_1(Fun, Dlists, Data0, Seen0, Acc);
	{deleted,Data} ->
	    update_1(Fun, Dlists, Data, Seen0, Acc);
	{D,Data} ->
	    Seen = update_seen(D, Seen0),
	    update_1(Fun, Dlists, Data, Seen, [D|Acc])
    end;
update_1(Fun, [], Data0, Seen0, Acc) ->
    case Fun(eol, Data0) of
	{D,Data} ->
	    Seen = update_seen(D, Seen0),
	    update_1(Fun, [], Data, Seen, [D|Acc]);
	eol ->
	    update_last(Data0, Seen0, Acc)
    end.

%%
%% Only allows updating of existing entries.
%%

map(Fun, Data) ->
    #du{dl=Dlists} = get_dl_data(),
    map_1(Fun, Dlists, Data, [], []).

map_1(Fun, [D0|Dlists], Data0, Seen0, Acc) ->
    case Fun(D0, Data0) of
	#dlo{}=D ->
	    Seen = update_seen(D, Seen0),
	    map_1(Fun, Dlists, Data0, Seen, [D|Acc]);
	{D,Data} ->
	    Seen = update_seen(D, Seen0),
	    map_1(Fun, Dlists, Data, Seen, [D|Acc])
    end;
map_1(_Fun, [], Data, Seen, Acc) ->
    update_last(Data, Seen, Acc).

update_last(Data, Seen, Acc) ->
    #du{used=Used0} = Du = get_dl_data(),
    Used = ordsets:from_list(Seen),
    put_dl_data(Du#du{used=Used,dl=reverse(Acc)}),
    NotUsed = ordsets:subtract(Used0, Used),
    foreach(fun(DL) -> gl:deleteLists(DL, 1) end, NotUsed),
    Data.

update_seen(D, Seen) ->
    update_seen_0(size(D), D, Seen).

update_seen_0(0, _, Seen) -> Seen;
update_seen_0(I, D, Seen0) ->
    Seen = update_seen_1(element(I, D), Seen0),
    update_seen_0(I-1, D, Seen).
    
update_seen_1([H|T], Seen) ->
    update_seen_1(T, update_seen_1(H, Seen));
update_seen_1([], Seen) -> Seen;
update_seen_1(none, Seen) -> Seen;
update_seen_1({call,Dl1,Dl2}, Seen) ->
    update_seen_1(Dl1, update_seen_1(Dl2, Seen));
update_seen_1({matrix,_,Dl}, Seen) ->
    update_seen_1(Dl, Seen);
update_seen_1(Dl, Seen) when is_integer(Dl) ->
    [Dl|Seen];
update_seen_1(_, Seen) -> Seen.

%%
%% Fold over dlo list.
%%

fold(Fun, Acc) ->
    #du{dl=Dlists} = get_dl_data(),
    foldl(Fun, Acc, Dlists).

%%
%% Render from the saved display lists.
%%

render(#st{selmode=Mode}=St) ->
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_CURRENT_BIT bor ?GL_ENABLE_BIT bor
		  ?GL_TEXTURE_BIT bor ?GL_POLYGON_BIT bor
		  ?GL_LINE_BIT bor ?GL_COLOR_BUFFER_BIT bor
		  ?GL_LIGHTING_BIT),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_CULL_FACE),
    wings_view:load_matrices(true),
    ground_and_axes(),
    show_saved_bb(St),
    #du{dl=Dl} = get_dl_data(),
    Work = wings_wm:get_prop(workmode),
    render_scene(Dl, Mode, Work, false),
    render_scene(Dl, Mode, Work, true),
    axis_letters(),
    gl:disable(?GL_CULL_FACE),
    gl:lineWidth(1),
    wings_io:ortho_setup(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    {W,H} = wings_wm:win_size(),
    gl:rectf(0.5, 0.5, W-0.5, H-0.5),
    gl:popAttrib().

render_scene(_, _, true, true) -> ok;
render_scene(Dls, Mode, Work, RenderTrans) ->
    render_scene_1(Dls, Mode, Work, RenderTrans).
    
render_scene_1([D|Dls], Mode, Work, RenderTrans) ->
    gl:frontFace(?GL_CCW),
    render_object_0(D, Mode, Work, RenderTrans),
    render_scene_1(Dls, Mode, Work, RenderTrans);
render_scene_1([], _, _, _) -> ok.

render_object_0(#dlo{drag={matrix,_,_,Matrix}}=D, Mode, Work, RT) ->
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_object(D, Mode, Work, RT),
    gl:popMatrix();
render_object_0(D, Mode, Work, RT) ->
    render_object(D, Mode, Work, RT).

render_object(#dlo{mirror=none}=D, Mode, Work, RenderTrans) ->
    render_object_1(D, Mode, Work, RenderTrans);
render_object(#dlo{mirror=Matrix}=D, Mode, Work, RenderTrans) ->
    render_object_1(D, Mode, Work, RenderTrans),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_object_1(D, Mode, Work, RenderTrans),
    gl:popMatrix().

render_object_1(#dlo{src_we=We}=D, _, _, false) when ?IS_LIGHT(We) ->
    wings_light:render(D);
render_object_1(#dlo{src_we=#we{light=L}}, _, _, _) when L =/= none ->
    ok;
render_object_1(D, Mode, true, _) ->
    render_plain(D, Mode);
render_object_1(#dlo{transparent=true}=D, _, false, false) ->
    gl:disable(?GL_CULL_FACE),
    render_smooth(D, false),
    gl:enable(?GL_CULL_FACE);
render_object_1(#dlo{transparent=true}=D, _, false, true) ->
    render_smooth(D, true);
render_object_1(#dlo{transparent=false}=D, _, false, RenderTrans) ->
    render_smooth(D, RenderTrans).

render_plain(#dlo{work=Faces,edges=Edges,src_we=We,proxy_data=none}=D, SelMode) ->
    %% Draw faces for winged-edge-objects.
    Wire = wire(We),
    case Wire of
	false ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    gl:polygonOffset(2, 2),
	    gl:shadeModel(?GL_SMOOTH),
	    gl:enable(?GL_LIGHTING),
	    call(Faces),
	    gl:disable(?GL_LIGHTING),
	    gl:shadeModel(?GL_FLAT);
	true -> ok
    end,

    %% Draw edges.
    case Wire orelse wings_pref:get_value(show_edges) of
	false -> ok;
	true ->
	    case {SelMode,wings_pref:get_value(edge_color)} of
		{body,{0.0,0.0,0.0}} ->
		    gl:color3f(0.3, 0.3, 0.3);
		{_,EdgeColor} ->
		    gl:color3fv(EdgeColor)
	    end,
	    gl:lineWidth(edge_width(SelMode)),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:enable(?GL_POLYGON_OFFSET_LINE),
	    gl:polygonOffset(1, 1),
	    case Wire andalso wings_pref:get_value(show_wire_backfaces) =:= true of
		true ->
		    gl:disable(?GL_CULL_FACE),
		    call(Edges),
		    gl:enable(?GL_CULL_FACE);
		false ->
		    call(Edges)
	    end
    end,
    render_plain_rest(D, Wire, SelMode);
render_plain(#dlo{src_we=We}=D, SelMode) ->
    Wire = wire(We),
    wings_subdiv:draw(D, Wire),
    render_plain_rest(D, Wire, SelMode).

render_plain_rest(D, Wire, SelMode) ->
    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_FILL),

    draw_hilite(D),
    case Wire of
	true ->
	    gl:disable(?GL_CULL_FACE),
	    draw_orig_sel(D),
	    draw_sel(D),
	    gl:enable(?GL_CULL_FACE);
	false ->
	    draw_orig_sel(D),
	    draw_sel(D)
    end,
    draw_vertices(D, SelMode),
    draw_hard_edges(D, SelMode),
    draw_normals(D).

wire(#we{id=Id}) ->
    W = wings_wm:get_prop(wireframed_objects),
    gb_sets:is_member(Id, W).

render_smooth(#dlo{work=Work,edges=Edges,smooth=Smooth,transparent=Trans,
		   src_we=We,proxy_data=Pd}=D,
	      RenderTrans) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(2, 2),

    case Trans of
	false -> gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_FALSE);
	true -> gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_TRUE)
    end,

    case RenderTrans of
	true ->
	    gl:enable(?GL_BLEND),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:depthMask(?GL_FALSE);
	false ->
	    gl:disable(?GL_BLEND),
	    gl:depthMask(?GL_TRUE)
    end,

    case {Smooth,RenderTrans} of
	{none,false} ->
	    if
		Pd =:= none ->
		    call(Work);
		true ->
		    wings_subdiv:draw(D, wire(We))
	    end;
	{[Op,_],false} -> call(Op);
	{[_,Tr],true} -> call(Tr);
	{_,_} -> ok
    end,

    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:depthMask(?GL_TRUE),
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    case wire(We) of
	true when Pd =:= none ->
	    gl:color3fv(wings_pref:get_value(edge_color)),
	    gl:lineWidth(1),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:enable(?GL_POLYGON_OFFSET_LINE),
	    gl:polygonOffset(1, 1),
	    call(Edges);
	true ->
	    wings_subdiv:draw_smooth_edges(D);
	false -> ok
    end,
    draw_hilite(D),
    draw_orig_sel(D),
    draw_sel(D).

draw_sel(#dlo{sel=SelDlist,src_sel={edge,_}}) ->
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    sel_color(),
    call(SelDlist);
draw_sel(#dlo{sel=SelDlist,src_sel={vertex,_}}) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    sel_color(),
    call(SelDlist);
draw_sel(#dlo{orig_sel=OrigSel,sel=SelDlist}) ->
    sel_color(),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1, 1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    case OrigSel =/= none orelse wings_pref:get_value(selection_style) =:= solid of
	true ->					%Solid selection style.
	    call(SelDlist);
	false ->				%Stippled selection style.
	    gl:enable(?GL_POLYGON_STIPPLE),
	    call(SelDlist),
	    gl:disable(?GL_POLYGON_STIPPLE)
    end.

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_vertices(#dlo{src_we=#we{perm=P},vs=VsDlist}, vertex) when ?IS_SELECTABLE(P) ->
    call(VsDlist);
draw_vertices(_, _) -> ok.

draw_hilite(#dlo{hilite=DL}) -> call(DL).

draw_orig_sel(#dlo{orig_sel=none}) -> ok;
draw_orig_sel(#dlo{orig_sel=Dlist,orig_mode=Mode}) ->
    sel_color(),
    draw_orig_sel_1(Mode, Dlist).

draw_orig_sel_1(vertex, DlistSel) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)*2),
    gl:depthMask(?GL_FALSE),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    {R0,G0,B0} = wings_pref:get_value(selected_color),
    gl:color4f(R0, G0, B0, 0.5),
    call(DlistSel),
    gl:disable(?GL_BLEND),
    gl:depthMask(?GL_TRUE);
draw_orig_sel_1(edge, DlistSel) ->
    gl:lineWidth(wings_pref:get_value(selected_edge_width)*2),
    gl:depthMask(?GL_FALSE),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    {R0,G0,B0} = wings_pref:get_value(selected_color),
    gl:color4f(R0, G0, B0, 0.5),
    call(DlistSel),
    gl:disable(?GL_BLEND),
    gl:depthMask(?GL_TRUE);
draw_orig_sel_1(_, DlistSel) ->
    gl:enable(?GL_POLYGON_STIPPLE),
    gl:depthMask(?GL_FALSE),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1, 1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    call(DlistSel),
    gl:depthMask(?GL_TRUE),
    gl:disable(?GL_POLYGON_STIPPLE).

draw_hard_edges(#dlo{hard=none}, _) -> ok;
draw_hard_edges(#dlo{hard=Hard}, SelMode) ->
    gl:lineWidth(edge_width(SelMode)),
    gl:color3fv(wings_pref:get_value(hard_edge_color)),
    call(Hard).

draw_normals(#dlo{normals=none}) -> ok;
draw_normals(#dlo{normals=Ns}) ->
    gl:color3f(0, 0, 1),
    gl:lineWidth(2),
    call(Ns).

edge_width(edge) -> wings_pref:get_value(edge_width);
edge_width(_) -> 1.

%%%
%%% Set material and draw faces.
%%%

prepare(Ftab, #dlo{src_we=We}, St) ->
    prepare(Ftab, We, St);
prepare(Ftab, #we{mode=vertex}=We, St) ->
    case {wings_pref:get_value(show_colors),Ftab} of
	{false,[{_,Edge}|_]} when is_integer(Edge) ->
	    Fs0 = sofs:from_external(Ftab, [{face,edge}]),
	    Fs1 = sofs:domain(Fs0),
	    Fs = sofs:to_external(Fs1),
	    {color,{[{wings_color:white(),Fs}],[]},St};
	{false,_} ->
	    {color,{[{wings_color:white(),Ftab}],[]},St};
	{true,_} ->
	    {color,vtx_color_split(Ftab, We),St}
    end;
prepare(Ftab, #we{mode=material}=We, St) ->
    MatFaces = wings_material:mat_faces(Ftab, We),
    {material,MatFaces,St}.

vtx_color_split([{_,Edge}|_]=Ftab0, We) when is_integer(Edge) ->
    vtx_color_split_1(Ftab0, We, [], []);
vtx_color_split(Ftab, _) -> vtx_smooth_color_split(Ftab).

vtx_color_split_1([{Face,Edge}|Fs], We, SameAcc, DiffAcc) ->
    Cols = wings_face:vertex_info(Face, Edge, We),
    case vtx_color_split_2(Cols) of
	different -> vtx_color_split_1(Fs, We, SameAcc, [[Face|Cols]|DiffAcc]);
	Col -> vtx_color_split_1(Fs, We, [{Col,Face}|SameAcc], DiffAcc)
    end;
vtx_color_split_1([], _, SameAcc, DiffAcc) ->
    {wings_util:rel2fam(SameAcc),DiffAcc}.

vtx_color_split_2([C,C|Cols]) -> vtx_color_split_3(Cols, C);
vtx_color_split_2(_) -> different.

vtx_color_split_3([C|Cols], C) -> vtx_color_split_3(Cols, C);
vtx_color_split_3([_|_], _) -> different;
vtx_color_split_3([], C) -> C.

vtx_smooth_color_split(Ftab) ->
    vtx_smooth_color_split_1(Ftab, [], []).

vtx_smooth_color_split_1([{_,Vs}=Face|Fs], SameAcc, DiffAcc) ->
    case vtx_smooth_face_color(Vs) of
	different -> vtx_smooth_color_split_1(Fs, SameAcc, [Face|DiffAcc]);
	Col -> vtx_smooth_color_split_1(Fs, [{Col,Face}|SameAcc], DiffAcc)
    end;
vtx_smooth_color_split_1([], SameAcc, DiffAcc) ->
    {wings_util:rel2fam(SameAcc),DiffAcc}.

vtx_smooth_face_color([[Col|_]|T]) ->
    vtx_smooth_face_color_1(T, Col).

vtx_smooth_face_color_1([[Col|_]|T], Col) ->
    vtx_smooth_face_color_1(T, Col);
vtx_smooth_face_color_1([_|_], _) -> different;
vtx_smooth_face_color_1([], Col) -> Col.

%%
%% Triangulate and draw a face.
%%

plain_face(Face, #dlo{ns=Ns}) ->
    case gb_trees:get(Face, Ns) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:plain_face(VsPos);
	{N,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:plain_face(N, VsPos)
    end;
plain_face(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    plain_face(Face, Edge, We).

plain_face(Face, _, #dlo{}=D) ->
    plain_face(Face, D);
plain_face(Face, Edge, #we{vp=Vtab}=We) ->
    Vs = wings_face:vertices_cw(Face, Edge, We),
    plain_face_1(Vs, Vtab, []).

plain_face_1([V|Vs], Vtab, Acc) ->
    plain_face_1(Vs, Vtab, [gb_trees:get(V, Vtab)|Acc]);
plain_face_1([], _, [_,_,_]=VsPos) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    wings__du:plain_face(VsPos);
plain_face_1([], _, [A,B,C,D]=VsPos) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    case good_triangulation(N, A, B, C, D) of
	false -> wings__du:plain_face(N, VsPos);
	true -> wings__du:plain_face(VsPos)
    end;
plain_face_1([], _, VsPos) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    wings__du:plain_face(N, VsPos).

%%
%% Tesselate and draw face. Include UV coordinates.
%%

uv_face(Face, Edge, #dlo{src_we=We,ns=Ns}) ->
    UVs = wings_face:vertex_info(Face, Edge, We),
    case gb_trees:get(Face, Ns) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:uv_face(VsPos, UVs);
	{N,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:uv_face(N, VsPos, UVs)
    end;
uv_face(Face, Edge, #we{vp=Vtab}=We) ->
    Vs0 = wings_face:vinfo_cw(Face, Edge, We),
    uv_face_1(Vs0, Vtab, [], []).

uv_face_1([[V|Col]|Vs], Vtab, Nacc, VsAcc) ->
    Pos = gb_trees:get(V, Vtab),
    uv_face_1(Vs, Vtab, [Pos|Nacc], [Col|VsAcc]);
uv_face_1([], _, [_,_,_]=VsPos, UVs) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    wings__du:uv_face(VsPos, UVs);
uv_face_1([], _, [A,B,C,D]=VsPos, UVs) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    case good_triangulation(N, A, B, C, D) of
	false -> wings__du:uv_face(N, VsPos, UVs);
	true -> wings__du:uv_face(VsPos, UVs)
    end;
uv_face_1([], _, VsPos, UVs) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    wings__du:uv_face(N, VsPos, UVs).

%%
%% Tesselate and draw face. Include vertex colors.
%%

vcol_face(Face, #dlo{src_we=We,ns=Ns}) ->
    Cols = wings_face:vertex_info(Face, We),
    case gb_trees:get(Face, Ns) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:vcol_face(VsPos, Cols);
	{N,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:vcol_face(N, VsPos, Cols)
    end;
vcol_face(Face, #we{fs=Ftab,vp=Vtab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    Vs0 = wings_face:vinfo_cw(Face, Edge, We),
    vcol_face_1(Vs0, Vtab, [], []).

vcol_face(Face, #dlo{ns=Ns}, Cols) ->
    case gb_trees:get(Face, Ns) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:vcol_face(VsPos, Cols);
	{N,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:vcol_face(N, VsPos, Cols)
    end.

vcol_face_1([[V|Col]|Vs], Vtab, Nacc, VsAcc) ->
    Pos = gb_trees:get(V, Vtab),
    vcol_face_1(Vs, Vtab, [Pos|Nacc], [Col|VsAcc]);
vcol_face_1([], _, [_,_,_]=VsPos, Cols) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    wings__du:vcol_face(VsPos, Cols);
vcol_face_1([], _, [A,B,C,D]=VsPos, Cols) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    case good_triangulation(N, A, B, C, D) of
	false -> wings__du:vcol_face(N, VsPos, Cols);
	true -> wings__du:vcol_face(VsPos, Cols)
    end;
vcol_face_1([], _, VsPos, Cols) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    wings__du:vcol_face(N, VsPos, Cols).

%% good_triangulation(Normal, Point1, Point2, Point3, Point4) -> true|false
%%  Return true if triangulation by connecting Point1 to Point3 is OK.
%%  The normal Normal should be averaged normal for the quad.
good_triangulation({Nx,Ny,Nz}, {Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}, {Dx,Dy,Dz})
  when is_float(Ax), is_float(Ay), is_float(Az) ->
    CAx = Cx-Ax, CAy = Cy-Ay, CAz = Cz-Az,
    ABx = Ax-Bx, ABy = Ay-By, ABz = Az-Bz,
    DAx = Dx-Ax, DAy = Dy-Ay, DAz = Dz-Az,
    D1 = Nx*(CAy*ABz-CAz*ABy) + Ny*(CAz*ABx-CAx*ABz) + Nz*(CAx*ABy-CAy*ABx),
    D2 = Nx*(DAz*CAy-DAy*CAz) + Ny*(DAx*CAz-DAz*CAx) + Nz*(DAy*CAx-DAx*CAy),
    good_triangulation_1(D1, D2).

good_triangulation_1(D1, D2) when D1 > 0, D2 > 0 -> true;
good_triangulation_1(_, _) -> false.

%% force_flat_color(OriginalDlist, Color) -> NewDlist.
%%  Wrap a previous display list (that includes gl:color*() calls)
%%  into a new display lists that forces the flat color Color
%%  on all elements.
force_flat_color(Dl, RGB) ->
    force_flat_color(Dl, RGB, fun() -> ok end).

force_flat_color(OriginalDlist, {R,G,B}, DrawExtra) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    gl:pushAttrib(?GL_CURRENT_BIT bor ?GL_ENABLE_BIT bor
		  ?GL_POLYGON_BIT bor ?GL_LINE_BIT bor
		  ?GL_COLOR_BUFFER_BIT bor
		  ?GL_LIGHTING_BIT),
    DrawExtra(),
    gl:enable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHT0),
    gl:disable(?GL_LIGHT1),
    gl:disable(?GL_LIGHT2),
    gl:disable(?GL_LIGHT3),
    gl:disable(?GL_LIGHT4),
    gl:disable(?GL_LIGHT5),
    gl:disable(?GL_LIGHT6),
    gl:disable(?GL_LIGHT7),
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0,0,0,0}),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, {R,G,B,1}),
    call(OriginalDlist),
    gl:popAttrib(),
    gl:endList(),
    {call,Dl,OriginalDlist}.

smooth_plain_faces([{Face,VsInfo}|Fs], Ns) ->
    case gb_trees:get(Face, Ns) of
	[_|Pos] -> wings__du:smooth_plain_face(Pos, VsInfo);
	{N,Pos} -> wings__du:smooth_plain_face(N, Pos, VsInfo)
    end,
    smooth_plain_faces(Fs, Ns);
smooth_plain_faces([], _) -> ok.

smooth_uv_faces([{Face,VsInfo}|Fs], Ns) ->
    case gb_trees:get(Face, Ns) of
	[_|Pos] -> wings__du:smooth_uv_face(Pos, VsInfo);
	{N,Pos} -> wings__du:smooth_uv_face(N, Pos, VsInfo)
    end,
    smooth_uv_faces(Fs, Ns);
smooth_uv_faces([], _) -> ok.

smooth_vcol_faces([{Face,VsInfo}|Fs], Ns) ->
    case gb_trees:get(Face, Ns) of
	[_|Pos] -> wings__du:smooth_vcol_face(Pos, VsInfo);
	{N,Pos} -> wings__du:smooth_vcol_face(N, Pos, VsInfo)
    end,
    smooth_vcol_faces(Fs, Ns);
smooth_vcol_faces([], _) -> ok.

%% Draw a face without any lighting.
unlit_face(Face, #dlo{ns=Ns}) ->
    case gb_trees:get(Face, Ns) of
	[_|VsPos] -> wings__du:plain_face(VsPos);
	{N,VsPos} -> wings__du:plain_face(N, VsPos)
    end;
unlit_face(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    unlit_face(Face, Edge, We).

unlit_face(Face, Edge, #we{vp=Vtab}=We) ->
    Vs = wings_face:vertices_cw(Face, Edge, We),
    unlit_face_1(Vs, Vtab, []).

unlit_face_1([V|Vs], Vtab, Acc) ->
    unlit_face_1(Vs, Vtab, [gb_trees:get(V, Vtab)|Acc]);
unlit_face_1([], _, [_,_,_]=VsPos) ->
    wings__du:plain_face(VsPos);
unlit_face_1([], _, [A,B,C,D]=VsPos) ->
    N = e3d_vec:normal(VsPos),
    case good_triangulation(N, A, B, C, D) of
	false -> wings__du:plain_face(N, VsPos);
	true -> wings__du:plain_face(VsPos)
    end;
unlit_face_1([], _, VsPos) ->
    N = e3d_vec:normal(VsPos),
    wings__du:plain_face(N, VsPos).

%%
%% Utilities.
%%

call(none) -> none;
call({call,Dl,_}) -> call(Dl);
call({call_in_this_win,Win,Dl}) ->
    case wings_wm:this() of
	Win -> call(Dl);
	_ -> ok
    end;
call([H|T]) -> call(H), call(T);
call([]) -> ok;
call(Dl) when is_integer(Dl) -> gl:callList(Dl).

%%
%% Miscellanous.
%%

ground_and_axes() ->
    Axes = wings_wm:get_prop(show_axes),
    ?CHECK_ERROR(),
    groundplane(Axes),
    ?CHECK_ERROR(),
    #view{yon=Yon} = wings_view:current(),
    case Axes of
	true ->
	    axis(1, Yon, get_pref(x_color), get_pref(neg_x_color)),
	    axis(2, Yon, get_pref(y_color), get_pref(neg_y_color)),
	    axis(3, Yon, get_pref(z_color), get_pref(neg_z_color));
	false -> ok
    end.

get_pref(Key) ->
    wings_pref:get_value(Key).

axis(I, Yon, Pos, Neg) ->
    A0 = {0.0,0.0,0.0},
    A = setelement(I, A0, Yon),
    B = setelement(I, A0, -Yon),
    gl:'begin'(?GL_LINES),
    gl:color3fv(Pos),
    gl:vertex3fv(A0),
    gl:vertex3fv(A),
    gl:color3fv(Neg),
    gl:vertex3fv(A0),
    gl:vertex3fv(B),
    gl:'end'().

dummy_axis_letter() ->
    %% Attempt to work around a crash occurring with Matrox cards.
    MM = list_to_tuple(gl:getDoublev(?GL_MODELVIEW_MATRIX)),
    PM = list_to_tuple(gl:getDoublev(?GL_PROJECTION_MATRIX)),
    %% Since this is a workaround, we will do a real fetching
    %% of the viewport (rather than wings_wm:viewport/0).
    [X,Y,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Viewport = {X,Y,W,H},
    dummy_axis_letter(MM, PM, Viewport).

dummy_axis_letter(_, _, {_,_,W,H}) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    wings_io:set_color(wings_pref:get_value(background_color)),
    axis_text(10, 90, axisx),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW).

axis_letters() ->
    case wings_pref:get_value(show_axis_letters) andalso
	wings_wm:get_prop(show_axes) of
	false ->
	    case wings_pref:get_value(dummy_axis_letter) of
		false -> ok;
		true -> dummy_axis_letter()
	    end;
	true ->
	    MM = list_to_tuple(gl:getDoublev(?GL_MODELVIEW_MATRIX)),
	    PM = list_to_tuple(gl:getDoublev(?GL_PROJECTION_MATRIX)),
	    ViewPort = wings_wm:viewport(),
	    Start = {0.0,0.0,0.0},
	    Origin = proj(Start, MM, PM),
	    Info = {Start,Origin,MM,PM,ViewPort},

	    gl:matrixMode(?GL_PROJECTION),
	    gl:pushMatrix(),
	    gl:loadIdentity(),
	    {_,_,W,H} = ViewPort,
	    glu:ortho2D(0, W, 0, H),
	    gl:matrixMode(?GL_MODELVIEW),
	    gl:pushMatrix(),
	    gl:loadIdentity(),

	    #view{yon=Yon} = wings_view:current(),
	    axis_letter(1, Yon, axisx, x_color, Info),
	    axis_letter(2, Yon, axisy, y_color, Info),
 	    axis_letter(3, Yon, axisz, z_color, Info),

	    gl:popMatrix(),
	    gl:matrixMode(?GL_PROJECTION),
	    gl:popMatrix(),
	    gl:matrixMode(?GL_MODELVIEW)
    end.

axis_letter(I, Yon, Char, Color0, {Start,{Ox,Oy,_,Ow},MM,PM,Viewport}) ->
    Color = wings_pref:get_value(Color0),
    wings_io:set_color(Color),
    End = setelement(I, Start, Yon),
    {Px,Py,_,Pw} = proj(End, MM, PM),
    if
	-Pw < Px, Px < Pw, -Pw < Py, Py < Pw ->
	    show_letter(Px, Py, Pw, Char, Viewport);
	true ->
	    clip(Ox, Oy, Ow, Px, Py, Pw, Char, Viewport)
    end.

clip(Ox, Oy, Ow, Px, Py, Pw, Char, Viewport) ->
    AxisRay = line(Ox, Oy, Px, Py),
    Lines = [line(-Ow, -Ow, Ow, -Ow),line(-Ow, Ow, Ow, Ow),
	     line(-Ow, -Ow, -Ow, Ow),line(Ow, -Ow, Ow, Ow)],
    case clip_1(AxisRay, Lines, {Ow,Pw}) of
	none -> ok;
	{X,Y,W} -> show_letter(X, Y, W, Char, Viewport)
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

show_letter(X0, Y0, W, Char, {_,_,Vw,Vh}) ->
    X = trunc((0.5*X0/W+0.5)*(Vw-20) + 10),
    Y = trunc((0.5*Y0/W+0.5)*(Vh-16) + 7),
    axis_text(X, Y, Char).

axis_text(X, Y, C) ->
    gl:rasterPos2i(X, Y),
    wings_text:char(C).

proj({X0,Y0,Z0}, MM, PM) ->
    e3d_mat:mul(PM, e3d_mat:mul(MM, {X0,Y0,Z0,1.0})).

line(Ox, Oy, Px, Py) -> {{Ox,Oy},{Px-Ox,Py-Oy}}.

pdot({X1,Y1}, {X2,Y2}) -> Y1*X2-X1*Y2.
add({X1,Y1}, {X2,Y2}) -> {X1+X2,Y1+Y2}.
sub({X1,Y1}, {X2,Y2}) -> {X1-X2,Y1-Y2}.
mul({X,Y}, S) -> {X*S,Y*S}.

groundplane(Axes) ->
    case (wings_wm:get_prop(show_groundplane) orelse
	  (wings_pref:get_value(force_show_along_grid) andalso
	   (wings_view:current())#view.along_axis =/= none)) of
	true -> groundplane_1(Axes);
	false -> ok
    end.

groundplane_1(Axes) ->
    #view{along_axis=Along} = wings_view:current(),
    gl:color3fv(wings_pref:get_value(grid_color)),
    gl:lineWidth(1),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    case Along of
	x -> gl:rotatef(90, 0, 1, 0);
	z -> ok;
	_ -> gl:rotatef(90, 1, 0, 0)
    end,
    gl:'begin'(?GL_LINES),
    Sz = ?GROUND_GRID_SIZE * 10,
    groundplane_2(-Sz, Sz, Sz, Axes),
    gl:'end'(),
    gl:popMatrix(),
    ?CHECK_ERROR().

groundplane_2(X, Last, _Sz, _Axes) when X > Last -> ok;
groundplane_2(X, Last, Sz, true) when X == 0 ->
    groundplane_2(?GROUND_GRID_SIZE, Last, Sz, true);
groundplane_2(X, Last, Sz, Axes) ->
    gl:vertex2f(X, -Sz),
    gl:vertex2f(X, Sz),
    gl:vertex2f(-Sz, X),
    gl:vertex2f(Sz, X),
    groundplane_2(X+?GROUND_GRID_SIZE, Last, Sz, Axes).

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
