%%
%%  wings_draw_util.erl --
%%
%%     Utilities for drawing objects.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw_util.erl,v 1.36 2002/08/01 06:50:15 bjorng Exp $
%%

-module(wings_draw_util).
-export([init/0,tess/0,begin_end/1,update/2,map/2,fold/2,render/1,
	 call/1,face/2,face/3,flat_face/2,flat_face/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foreach/2,foldl/3]).

-record(du,
	{dl=[],					%Display lists
	 used=gb_sets:empty(),			%Display lists in use.
	 vec,					%Display list for vector.
	 src_vec=undefined,			%Source for vector.
	 tess					%Tesselator.
	 }).

init() ->
    case get(?MODULE) of
	undefined -> ok;
	#du{tess=OldTess,used=Used,vec=Vec} ->
	    ?CHECK_ERROR(),
	    glu:deleteTess(OldTess),
	    foreach(fun(DL) -> gl:deleteLists(DL, 1) end,
		    gb_sets:to_list(Used)),
	    catch gl:deleteLists(Vec, 1),
	    gl:getError()			%Clear error.
    end,
    Tess = glu:newTess(),
    put(?MODULE, #du{tess=Tess,vec=gl:genLists(1)}),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
    glu:tessCallback(Tess, ?GLU_TESS_EDGE_FLAG, ?ESDL_TESSCB_GLEDGEFLAG),
    glu:tessCallback(Tess, ?GLU_TESS_COMBINE, ?ESDL_TESSCB_COMBINE),
    case wings_pref:get_value(display_list_opt) of
	false ->
	    glu:tessCallback(Tess, ?GLU_TESS_BEGIN, ?ESDL_TESSCB_GLBEGIN),
	    glu:tessCallback(Tess, ?GLU_TESS_END, ?ESDL_TESSCB_GLEND);
	true ->ok
    end,
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

tess() ->
    #du{tess=Tess} = get(?MODULE),
    Tess.

begin_end(Body) ->
    case wings_pref:get_value(display_list_opt) of
	false ->
	    Res = Body();
	true ->
	    gl:'begin'(?GL_TRIANGLES),
	    Res = Body(),
	    gl:'end'()
    end,
    gl:edgeFlag(?GL_TRUE),
    Res.

%%
%% Update allows addition of new objects at the end.
%%

update(Fun, Data) ->
    #du{dl=Dlists} = get(?MODULE),
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
    #du{dl=Dlists} = get(?MODULE),
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
    #du{used=Used0} = Du = get(?MODULE),
    Used = gb_sets:from_list(Seen),
    NotUsed = gb_sets:difference(Used0, Used),
    foreach(fun(DL) -> gl:deleteLists(DL, 1) end,
	    gb_sets:to_list(NotUsed)),
    put(?MODULE, Du#du{used=Used,dl=reverse(Acc)}),
    Data.

update_seen(D, Seen) ->
    #dlo{work=F,smooth=Sm1,smoothed=Sm2,hard=Hard,vs=Vs,
	 sel=Sel,orig_sel=OrigSel,normals=Ns,pick=Pick} = D,
    Lists = [F,Sm1,Sm2,Hard,Vs,Sel,OrigSel,Ns,Pick],
    update_seen_1(Lists, Seen).

update_seen_1([H|T], Seen) ->
    update_seen_1(T, update_seen_1(H, Seen));
update_seen_1([], Seen) -> Seen;
update_seen_1(none, Seen) -> Seen;
update_seen_1({matrix,_,Dl}, Seen) ->
    update_seen_1(Dl, Seen);
update_seen_1(Dl, Seen) when is_integer(Dl) ->
    [Dl|Seen].

%%
%% Fold over dlo list.
%%

fold(Fun, Acc) ->
    #du{dl=Dlists} = get(?MODULE),
    foldl(Fun, Acc, Dlists).

%%
%% Render from the saved display lists.
%%

render(#st{selmode=Mode}=St) ->
    gl:pushAttrib(?GL_CURRENT_BIT bor ?GL_ENABLE_BIT bor
		  ?GL_TEXTURE_BIT bor ?GL_POLYGON_BIT bor
		  ?GL_LINE_BIT bor ?GL_COLOR_BUFFER_BIT bor
		  ?GL_LIGHTING_BIT),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_CULL_FACE),
    wings_view:projection(),
    wings_view:model_transformations(),
    ground_and_axes(),
    show_saved_bb(St),
    #du{dl=Dl} = get(?MODULE),
    Work = wings_pref:get_value(workmode),
    render_scene(Dl, Mode, Work, false),
    render_scene(Dl, Mode, Work, true),
    axis_letters(),
    dummy_axis_letter(),
    gl:disable(?GL_DEPTH_TEST),
    draw_vec(St),
    gl:popAttrib().

render_scene(_, _, true, true) -> ok;
render_scene(Dls, Mode, Work, RenderTrans) ->
    render_scene_1(Dls, Mode, Work, RenderTrans).
    
render_scene_1([D|Dls], Mode, Work, RenderTrans) ->
    gl:cullFace(?GL_BACK),
    render_object(D, Mode, Work, RenderTrans),
    render_scene_1(Dls, Mode, Work, RenderTrans);
render_scene_1([], _, _, _) -> ok.

render_object(#dlo{mirror=none}=D, Mode, Work, RenderTrans) ->
    render_object_1(D, Mode, Work, RenderTrans);
render_object(#dlo{mirror=Matrix}=D, Mode, Work, RenderTrans) ->
    render_object_1(D, Mode, Work, RenderTrans),
    gl:cullFace(?GL_FRONT),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_object_1(D, Mode, Work, RenderTrans),
    gl:popMatrix().

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

render_plain(#dlo{work=Faces,wire=Wire}=D, SelMode) ->

    %% Draw faces for winged-edge-objects.
    case Wire of
	false ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    gl:polygonOffset(2.0, 2.0),
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
	    case Wire andalso wings_pref:get_value(show_wire_backfaces) of
		true ->
		    gl:disable(?GL_CULL_FACE),
		    call(Faces),
		    gl:enable(?GL_CULL_FACE);
		false ->
		    call(Faces)
	    end
    end,

    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    draw_hilite(D),
    case Wire andalso wings_pref:get_value(show_wire_backfaces) of
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
    draw_hard_edges(D),
    draw_normals(D).

render_smooth(#dlo{work=Work,smooth=Smooth,transparent=Trans}=D, RenderTrans) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(2.0, 2.0),

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
	{none,false} -> call(Work);
	{[Op,_],false} -> call(Op);
	{[_,Tr],true} -> call(Tr);
	{_,_} -> ok
    end,

    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:depthMask(?GL_TRUE),
    draw_edges(D).

draw_edges(#dlo{work=Work,wire=Wire}=D) ->
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    case Wire of
	false -> ok;
	true ->
	    gl:color3f(1.0, 1.0, 1.0),
	    gl:lineWidth(?NORMAL_LINEWIDTH),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:enable(?GL_POLYGON_OFFSET_LINE),
	    gl:polygonOffset(1.0, 1.0),
	    call(Work)
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
draw_sel(#dlo{sel=SelDlist}) ->
    sel_color(),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    call(SelDlist).

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_vertices(#dlo{src_we=#we{perm=P},vs=VsDlist}, vertex) when ?IS_SELECTABLE(P) ->
    call(VsDlist);
draw_vertices(_, _) -> ok.

draw_hilite(#dlo{hilite=none}) -> ok;
draw_hilite(#dlo{hilite=Hilite}) -> Hilite().

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
    gl:enable(?GL_LINE_STIPPLE),
    gl:lineStipple(2, 16#AAAA),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:depthMask(?GL_FALSE),
    call(DlistSel),
    gl:depthMask(?GL_TRUE),
    gl:disable(?GL_LINE_STIPPLE);
draw_orig_sel_1(_, DlistSel) ->
    gl:enable(?GL_POLYGON_STIPPLE),
    gl:depthMask(?GL_FALSE),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    call(DlistSel),
    gl:depthMask(?GL_TRUE),
    gl:disable(?GL_POLYGON_STIPPLE).

draw_hard_edges(#dlo{hard=none}) -> ok;
draw_hard_edges(#dlo{hard=Hard}) ->
    gl:color3fv(wings_pref:get_value(hard_edge_color)),
    call(Hard).

draw_normals(#dlo{normals=none}) -> ok;
draw_normals(#dlo{normals=Ns}) ->
    gl:color3f(0, 0, 1),
    gl:lineWidth(2.0),
    call(Ns).

%%
%% Tesselate and draw face. Include vertex colors or UV coordinates.
%%

face(Face, #we{fs=Ftab}=We) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    face(Face, Edge, We).

face(Face, Edge, We) ->
    Vs = wings_face:draw_info(Face, Edge, We),
    {X,Y,Z} = N = wings_face:draw_normal(Vs),
    gl:normal3fv(N),
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_face_vtxcol(Tess, Vs).

tess_face_vtxcol(Tess, [{Pos,{_,_}=UV}|T]) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,UV}]),
    tess_face_vtxcol(Tess, T);
tess_face_vtxcol(Tess, [{Pos,{_,_,_}=Col}|T]) ->
    glu:tessVertex(Tess, Pos, [{material,?GL_FRONT,
				?GL_AMBIENT_AND_DIFFUSE,Col}]),
    tess_face_vtxcol(Tess, T);
tess_face_vtxcol(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%%
%% Draw a face. Tesselate polygons (4 edges or more).
%%

flat_face(Face, #we{fs=Ftab}=We) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    flat_face(Face, Edge, We).

flat_face(Face, Edge, #we{vs=Vtab}=We) ->
    Vs = wings_face:surrounding_vertices(Face, Edge, We),
    VsPos = vspos(Vs, Vtab, []),
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    {X,Y,Z} = N,
    Tess = tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_flat_face(Tess, VsPos).

tess_flat_face(Tess, [P|T]) ->
    glu:tessVertex(Tess, P),
    tess_flat_face(Tess, T);
tess_flat_face(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

%%
%% Utilities.
%%

vspos([V|Vs], Vtab, Acc) ->
    #vtx{pos=P} = gb_trees:get(V, Vtab),
    vspos(Vs, Vtab, [P|Acc]);
vspos([], _, Acc) -> reverse(Acc).

call(none) -> none;
call([H|T]) -> call(H), call(T);
call([]) -> ok;
call({matrix,Matrix,Dl}) ->
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    call(Dl),
    gl:popMatrix();
call(Dl) when is_integer(Dl) -> gl:callList(Dl).

%%
%% Miscellanous.
%%

ground_and_axes() ->
    Axes = wings_pref:get_value(show_axes),
    ?CHECK_ERROR(),
    groundplane(Axes),
    ?CHECK_ERROR(),
    case Axes of
	true ->
	    axis(1, get_pref(x_color), get_pref(neg_x_color)),
	    axis(2, get_pref(y_color), get_pref(neg_y_color)),
	    axis(3, get_pref(z_color), get_pref(neg_z_color));
	false -> ok
    end.

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

dummy_axis_letter() ->
    %% Attempt to work around a crash occurring with Matrox cards.
    case wings_pref:get_value(dummy_axis_letter) andalso
	(wings_pref:get_value(show_axes) == false orelse
	 wings_pref:get_value(show_axis_letters) == false) of
	false -> ok;
	true ->
	    MM = list_to_tuple(gl:getDoublev(?GL_MODELVIEW_MATRIX)),
	    PM = list_to_tuple(gl:getDoublev(?GL_PROJECTION_MATRIX)),
	    Viewport = gl:getIntegerv(?GL_VIEWPORT),
	    dummy_axis_letter(MM, PM, Viewport)
    end.

dummy_axis_letter(_, _, _) ->
    wings_io:axis_text(10, 90, axisx, wings_pref:get_value(background_color)).

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

draw_vec(St) ->
    #du{vec=VecDl,src_vec=SrcVec0} = Du = get(?MODULE),
    case make_vec_dlist(St, SrcVec0, VecDl) of
	SrcVec0 -> ok;
	SrcVec -> put(?MODULE, Du#du{src_vec=SrcVec})
    end,
    gl:callList(VecDl).

make_vec_dlist(#st{vec=Vec}, Vec, _) -> Vec;
make_vec_dlist(#st{vec=none}, _, Dlist) ->
    gl:newList(Dlist, ?GL_COMPILE),
    gl:endList(),
    none;
make_vec_dlist(#st{vec={Center,Vec}=Src}, _, Dlist) ->
    gl:newList(Dlist, ?GL_COMPILE),
    do_draw_vec(Center, Vec),
    gl:endList(),
    Src;
make_vec_dlist(#st{vec=Center}, _, Dlist) ->
    gl:newList(Dlist, ?GL_COMPILE),
    Width = wings_pref:get_value(active_vector_width),
    gl:color3fv(wings_pref:get_value(active_vector_color)),
    gl:pointSize(Width*3.5),
    gl:'begin'(?GL_POINTS),
    gl:vertex3fv(Center),
    gl:'end'(),
    gl:endList(),
    Center.

do_draw_vec(Center, Vec0) ->
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
