%%
%%  wings_drag.erl --
%%
%%     This module handles interactive commands.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_drag.erl,v 1.33 2001/11/29 14:31:44 bjorng Exp $
%%

-module(wings_drag).
-export([init_drag/3,init_drag/4,do_drag/1,message/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(CTRL_BITS, (?KMOD_LCTRL bor ?KMOD_RCTRL)).
-define(ALT_BITS, (?KMOD_LALT bor ?KMOD_RALT)).
-define(SHIFT_BITS, (?KMOD_LSHIFT bor ?KMOD_RSHIFT)).

-define(DL_STATIC_FACES, (?DL_DRAW_BASE)).
-define(DL_DYNAMIC_FACES, (?DL_DRAW_BASE+1)).
-define(DL_SEL, (?DL_DRAW_BASE+2)).

-define(DL_DYNAMIC, (?DL_DRAW_BASE+3)).

-import(lists, [foreach/2,map/2,foldl/3,sort/1,reverse/1]).

-record(drag,
	{x,					%Original 2D position
	 y,					
	 xs=0,
	 ys=0,
	 tvs,					%[{Vertex,Vec}...]
	 ids=[],
	 constraint,				%Constraints for motion
	 unit,					%Unit that drag is done in.
	 shapes,				%Shapes before drag.
	 drag_faces=none,			%Faces being dragged.
	 matrices=none				%Transformation matrices.
	}).

-record(dlist,					%Display list.
	{faces,
	 edges}).

init_drag(Tvs, Constraint, St) ->
    init_drag_1(Tvs, Constraint, none, St).

init_drag(Tvs, Constraint, Unit, St) ->
    init_drag_1(Tvs, Constraint, Unit, St).

init_drag_1(Tvs, Constraint, Unit, #st{shapes=OldShapes}=St) ->
    wings_io:grab(),
    {_,X,Y} = sdl_mouse:getMouseState(),
    Drag = #drag{x=X,y=Y,constraint=Constraint,unit=Unit,shapes=OldShapes},
    init_drag_2(Tvs, Drag, St).

init_drag_2(Tvs0, Drag0, St) ->
    Tvs = combine(Tvs0),
    Faces = faces(Tvs, St),
    Drag1 = Drag0#drag{tvs=Tvs,drag_faces=Faces},
    Drag = static_display_list(Drag1, St),
    St#st{drag=Drag,dl=none}.

combine({matrix,Tvs0}) ->
    Ident = e3d_mat:identity(),
    Tvs = [{Id,Trans,Ident} || {Id,Trans} <- Tvs0],
    {matrix,sort(Tvs)};
combine(Tvs) ->
    S = sofs:relation(Tvs),
    F = sofs:relation_to_family(S),
    %% The rest of this function is an optimisation.
    map(fun({Id,[Fun|_]}=Tv) when is_function(Fun) -> Tv;
	   ({Id,L}) ->
		SS = sofs:from_term(L, [[{vec,[vertex]}]]),
		RR = sofs:union(SS),
 		FF = sofs:relation_to_family(RR),
  		FU = sofs:family_union(FF),
		{Id,sofs:to_external(FU)}
	end, sofs:to_external(F)).

do_drag(St) ->
    {seq,{push,dummy},get_drag_event_1(St)}.

get_drag_event(St) ->
    redraw(St),
    get_drag_event_1(St).

get_drag_event_1(St) ->
    {replace,fun(Ev) -> handle_drag_event(Ev, St) end}.

handle_drag_event(Event, St) ->
    case wings_camera:event(Event, fun() -> redraw(St) end) of
	next -> handle_drag_event_1(Event, St);
	Other -> Other
    end.

handle_drag_event_1(#mousemotion{x=X,y=Y}, #st{drag=Drag0}=St0) ->
    #drag{shapes=Shapes,tvs=Tvs} = Drag0,
    {Dx0,Dy0,Drag} = mouse_range(X, Y, Drag0),
    {Dx,Dy} = constrain(Dx0, Dy0, Drag),
    St1 = St0#st{shapes=Shapes,drag=Drag},
    St = motion_update(Tvs, Dx, Dy, St1),
    get_drag_event(St);
handle_drag_event_1(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED}, St0) ->
    wings_io:ungrab(),
    St1 = motion(X, Y, St0),
    St = normalize(St1),
    cleanup(St),
    DragEnded = {drag_ended,St#st{drag=none,dl=none}},
    wings_io:putback_event(DragEnded),
    pop;
handle_drag_event_1(#mousebutton{button=3,x=X,y=Y,state=?SDL_RELEASED}, St) ->
    cleanup(St),
    wings_io:ungrab(),
    wings_io:putback_event(drag_aborted),
    pop;
handle_drag_event_1(view_changed, St) ->
    get_drag_event(view_changed(St));
handle_drag_event_1(Event, St0) ->
    St = case wings_hotkey:event(Event) of
	     next -> St0;
	     {view,Cmd} ->
		 wings_view:command(Cmd, St0),
		 view_changed(St0);
	     {select,less} ->
		 magnet_radius(-1, St0);
	     {select,more} ->
		 magnet_radius(1, St0);
	     Other -> St0
	 end,
    get_drag_event(St).

cleanup(#st{drag=#drag{matrices=none}}) -> ok;
cleanup(#st{drag=#drag{matrices=Mtxs}}) ->
    foreach(fun({Id,Matrix}) ->
		    gl:deleteLists(?DL_DYNAMIC+Id, 1)
	    end, Mtxs).

magnet_radius(Sign, #st{inf_r=InfR0}=St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    case InfR0+Sign*?GROUND_GRID_SIZE/4 of
	InfR when InfR > 0 ->
	    motion(X, Y, St#st{inf_r=InfR});
	Other -> St
    end.

view_changed(#st{drag=#drag{constraint=view_dependent}=Drag0}=St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    Drag = Drag0#drag{x=X,y=Y,xs=0,ys=0,shapes=St#st.shapes},
    view_changed_1(St#st{drag=Drag});
view_changed(St) -> St.

view_changed_1(#st{drag=#drag{matrices=none}}=St) -> St;
view_changed_1(#st{drag=#drag{tvs={matrix,Tvs0},matrices=Mtxs}=Drag}=St) ->
    Tvs = view_changed_2(Tvs0, sort(Mtxs)),
    St#st{drag=Drag#drag{tvs={matrix,Tvs}}}.

view_changed_2([{Id,Trans,_}|Tvs], [{Id,Matrix}|Ms]) ->    
    [{Id,Trans,Matrix}|view_changed_2(Tvs, Ms)];
view_changed_2([], []) -> [].

motion(X, Y, #st{drag=#drag{shapes=Shapes,tvs=Tvs}=Drag0}=St0) ->
    {Dx0,Dy0,Drag} = mouse_range(X, Y, Drag0),
    {Dx,Dy} = constrain(Dx0, Dy0, Drag),
    St = St0#st{drag=Drag,shapes=Shapes},
    motion_update(Tvs, Dx, Dy, St).

mouse_range(X0, Y0, #drag{x=OX,y=OY,xs=Xs,ys=Ys}=Drag) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    if
	X0 =:= W-1 ->
	    NewX = W div 2,
	    warp(NewX, Y0, NewX, 0, Drag);
	X0 =:= 0 ->
	    NewX = W div 2,
	    warp(NewX, Y0, -NewX, 0, Drag);
	Y0 =:= H-1 ->
	    NewY = H div 2,
	    warp(X0, NewY, 0, NewY, Drag);
	Y0 =:= 0 ->
	    NewY = H div 2,
	    warp(X0, NewY, 0, -NewY, Drag);
	true ->
	    X = X0 + Xs,
	    Y = Y0 + Ys,
	    Dx = (X-OX) / ?MOUSE_DIVIDER,
	    Dy = (OY-Y) / ?MOUSE_DIVIDER,
	    {Dx,Dy,Drag}
    end.

warp(X, Y, XsInc, YsInc, #drag{xs=Xs,ys=Ys}=Drag) ->
    warp_mouse(X, Y),
    mouse_range(X, Y, Drag#drag{xs=Xs+XsInc,ys=Ys+YsInc}).

warp_mouse(X, Y) ->
    %% Strangely enough, on Solaris the warp doesn't seem to
    %% work unless the mouse cursor is visible.
    %% On Windows, the mouse cursor must not be visible.
    case os:type() of
	{unix,solaris} ->
	    sdl_mouse:showCursor(true),
	    sdl_mouse:warpMouse(X, Y),
	    sdl_mouse:showCursor(false);
	_ ->
	    sdl_mouse:warpMouse(X, Y)
    end.

constrain(Dx0, Dy0, #drag{constraint=Constraint}) ->
    {Dx,Dy} = case sdl_keyboard:getModState() of
  		  Mod when Mod band ?SHIFT_BITS =/= 0,
			   Mod band ?CTRL_BITS =/= 0 ->
  		      {trunc(100*Dx0)/100,trunc(100*Dy0)/100};
		  Mod when Mod band ?CTRL_BITS =/= 0 ->
		      {trunc(10*Dx0)/10,trunc(10*Dy0)/10};
		  Mod when Mod band ?SHIFT_BITS =/= 0 ->
		      {float(trunc(Dx0)),float(trunc(Dy0))};
		  Mod -> {Dx0,Dy0}
	      end,
    case Constraint of
	none ->
	    {Dx,Dy};
	view_dependent ->
	    {Dx,Dy};
	{Min,Max} when Dx < Min ->
	    {Min,Dy};
	{Min,Max} when Dx > Max ->
	    {Max,Dy};
	{_,_} ->
	    {Dx,Dy}
    end.

motion_update({matrix,Tvs}, Dx, Dy, #st{drag=Drag,shapes=Shapes}=St) ->
    gl:newList(?DL_DYNAMIC_FACES, ?GL_COMPILE),
    sel_color(),
    Mtxs = foldl(fun({Id,Trans,Matrix0}, Acc) when function(Trans) ->
			 Matrix = Trans(Matrix0, Dx, Dy, St),
			 gl:pushMatrix(),
			 gl:multMatrixf(e3d_mat:expand(Matrix)),
			 gl:callList(?DL_DYNAMIC+Id),
			 gl:popMatrix(),
			 [{Id,Matrix}|Acc]
		 end, [], Tvs),
    gl:endList(),
    St#st{drag=Drag#drag{matrices=Mtxs}};
motion_update(Tvs, Dx, Dy, #st{shapes=Shapes0}=St0) ->
    Shapes =
	foldl(fun({Id,[F0|_]=Trs}, Shs0) when is_function(F0) ->
		      Sh = foldl(fun(Tr, Sh0) ->
					 case Tr(Sh0, Dx, Dy, St0) of
					     {shape,Sh} ->
						 Sh;
					     {tvs,List} ->
						 transform_vs(List, Dx, Dy, St0, Sh0)
					 end
				 end, gb_trees:get(Id, Shs0), Trs),
		      gb_trees:update(Id, Sh, Shs0);
		 ({Id,List}, Shapes) ->
		      Sh0 = gb_trees:get(Id, Shapes),
		      Sh = transform_vs(List, Dx, Dy, St0, Sh0),
		      gb_trees:update(Id, Sh, Shapes)
	      end, Shapes0, Tvs),
    St1 = St0#st{shapes=Shapes},
    St = update_display_lists(St1),
    make_sel_dlist(St).

transform_vs(Tvs, Dx, Dy, St, #shape{sh=#we{vs=Vtab0}=We}=Shape0) ->
    show_message(Tvs, Dx, Dy, St),
    Vtab = foldl(fun ({Vec,Vs}, Tab) ->
			 trans_vec(Vec, Dx, Dy, Vs, St, Tab)
		 end, Vtab0, Tvs),
    Shape0#shape{sh=We#we{vs=Vtab}}.

trans_vec({rot,{Cx,Cy,Cz},Vec}, Dx, Dy, Vs, St, Vtab) ->
    A = 15*Dx,
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(A, Vec)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    foldl(fun(V, Tab) -> 
		  #vtx{pos=Pos0}= Vtx = gb_trees:get(V, Tab),
		  Pos = e3d_mat:mul_point(M, Pos0),
		  gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab)
	  end, Vtab, Vs);
trans_vec({free,Matrix}, Dx, Dy, Vs, St, Vtab) ->
    {Xt,Yt,Zt} = e3d_mat:mul_point(Matrix, {Dx,Dy,0.0}),
    foldl(fun(V, Tab) -> 
		  #vtx{pos={X,Y,Z}}= Vtx = gb_trees:get(V, Tab),
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab)
	  end, Vtab, Vs);
trans_vec({Xt0,Yt0,Zt0}, Dx, Dy, Vs, St, Vtab) ->
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    translate(Xt, Yt, Zt, Vs, Vtab).

show_message([{Vec,Vs}|_], Dx, Dy, St) ->
    show_message_1(Vec, Dx, Dy, St);
show_message([], Dx, Dy, St) -> ok.

show_message_1({rot,_,Vec}, Dx, Dy, St) ->
    message([15*Dx], St);
show_message_1({free,Matrix}, Dx, Dy, St) ->
    message([Dx,Dy], St);
show_message_1({_,_,_}, Dx, Dy, St) ->
    message([Dx], St);
show_message_1(Other, _, _, _) -> ok.

message(L, #st{drag=#drag{unit=Unit}}) ->
    message_0(L, Unit);
message(L, Unit) when atom(Unit) ->
    message_0(L, Unit).

message_0([_]=L, none) ->
    message_1("~-10.2f", L);
message_0([U], percent) ->
    message_1("P:~10.2f%", [U*100.0]);
message_0([_]=L, distance) ->
    message_1("D:~-10.2f", L);
message_0([_]=L, angle) ->
    message_1("A:~-10.2f", L);
message_0([_,_]=L, none) ->
    message_1("~-10.2f ~-10.2f", L);
message_0([_,_]=L, distance) ->
    message_1("DX:~-10.2f DY:~-10.2f", L).

message_1(Format, List) ->
    wings_io:message(lists:flatten(io_lib:format(Format, List))).

translate(Xt, Yt, Zt, Vs, Vtab) ->
    foldl(fun(V, Tab) -> 
		  #vtx{pos={X,Y,Z}}= Vtx = gb_trees:get(V, Tab),
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab)
	  end, Vtab, Vs).

faces({matrix,Tvs}, St) ->
    {matrix,[{Id,matrix} || {Id,Trans,Matrix} <- Tvs]};
faces(Tvs, #st{shapes=Shapes}) ->
    [{Id,faces_1(Vs, gb_trees:get(Id, Shapes))} || {Id,Vs} <- Tvs].

faces_1([Fun|_]=Tr, Sh) when is_function(Fun) -> all_faces;
faces_1(Vs0, #shape{sh=#we{}=We}) ->
    foldl(fun ({_,Vs}, Acc) ->
		  faces_2(Vs, We, Acc)
	  end, gb_sets:empty(), Vs0).

faces_2(Vs, We, FaceSet) ->
    foldl(fun(V, Acc) ->
		  wings_vertex:fold(fun(_, Face, _, A) ->
					    gb_sets:add(Face, A)
				    end, Acc, V, We)
	  end, FaceSet, Vs).

normalize(#st{drag=#drag{matrices=none}}=St) -> St;
normalize(#st{drag=#drag{matrices=Mtxs},shapes=Shapes0}=St) ->
    Shapes = foldl(fun({Id,Matrix}, A) ->
			   normalize(Id, Matrix, A)
		   end, Shapes0, Mtxs),
    St#st{shapes=Shapes}.

normalize(Id, Matrix, Shapes) ->
    #shape{sh=We0}= Sh0 = gb_trees:get(Id, Shapes),
    We = wings_we:transform_vs(Matrix, We0),
    Sh = Sh0#shape{sh=We},
    gb_trees:update(Id, Sh, Shapes).

%%%
%%% Redrawing while dragging.
%%%

redraw(St0) ->
    St = render(St0),
    wings_io:update(St).

render(#st{shapes=Shapes}=St) ->
    ?CHECK_ERROR(),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:enable(?GL_DEPTH_TEST),
    wings_view:projection(),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    wings_view:model_transformations(),
    wings_draw:ground_and_axes(),
    draw_shapes(St),
    gl:popAttrib(),
    ?CHECK_ERROR(),
    St.

draw_shapes(#st{selmode=SelMode}=St) ->
    Wire = wings_pref:get_value(wire_mode),
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),

    %% Draw faces for winged-edge-objects.
    case Wire of
	true -> ok;
	false ->
 	    FaceColor = wings_pref:get_value(face_color),
 	    gl:color3fv(FaceColor),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    gl:polygonOffset(2.0, 2.0),
	    gl:shadeModel(?GL_SMOOTH),
	    gl:enable(?GL_LIGHTING),
	    draw_we(St),
	    gl:disable(?GL_LIGHTING),
	    gl:shadeModel(?GL_FLAT)
    end,

    %% Draw edges if they are turned on.
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
	    draw_we(St)
    end,

    %% If vertex selection mode, draw vertices.
    case SelMode of
	vertex ->
	    gl:color3f(0.0, 0.0, 0.0), 
	    gl:pointSize(wings_pref:get_value(vertex_size)),
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

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_sel(#st{selmode=edge}) ->
    sel_color(),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:callList(?DL_SEL);
draw_sel(#st{selmode=vertex}) ->
    sel_color(),
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:callList(?DL_SEL);
draw_sel(St) ->
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    sel_color(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:callList(?DL_SEL).
    
draw_we(St) ->
    gl:callList(?DL_STATIC_FACES),
    gl:disable(?GL_LIGHTING),
    gl:callList(?DL_DYNAMIC_FACES).

%% Collect the static display list - faces that will not be moved.
static_display_list(#drag{drag_faces={matrix,Faces},shapes=Shs0}=Drag, St) ->
    make_dlist(?DL_STATIC_FACES, Faces, false, St),
    Shs = gb_trees:to_list(Shs0),
    make_dlist_1(Shs, Faces, true),
    gl:newList(?DL_SEL, ?GL_COMPILE),
    gl:endList(),
    Drag;
static_display_list(#drag{drag_faces=Faces}=Drag, St) ->
    make_dlist(?DL_STATIC_FACES, Faces, false, St),
    Drag.

update_display_lists(#st{drag=#drag{drag_faces=Faces}=Drag}=St) ->
    %% Collect the dynamic display list - everything that will be moved.
    make_dlist(?DL_DYNAMIC_FACES, Faces, true, St),
    St.

make_sel_dlist(St) ->
    DlistSel = ?DL_SEL,
    gl:newList(DlistSel, ?GL_COMPILE),
    draw_selection(St),
    gl:endList(),
    St.

make_dlist(DlistId, Faces, DrawMembers, #st{shapes=Shapes0}=St) ->
    gl:newList(DlistId, ?GL_COMPILE),
    make_dlist_1(gb_trees:to_list(Shapes0), Faces, DrawMembers),
    gl:endList(),
    DlistId.

make_dlist_1([{Id,Shape}|Shs], [{Id,matrix}|Fs], false) ->
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{Id,Shape}|Shs], [{Id,matrix}|Fs], true) ->
    gl:newList(?DL_DYNAMIC+Id, ?GL_COMPILE),
    Draw = fun(Face, Edge, We) ->
		   wings_draw_util:face(Face, Edge, We)
	   end,
    mkdl_draw_faces(Shape, Draw),
    gl:endList(),
    make_dlist_1(Shs, Fs, true);
make_dlist_1([{Id,Shape}|Shs], [{Id,all_faces}|Fs], false) ->
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{Id,Shape}|Shs], [{Id,all_faces}|Fs], true) ->
    Draw = fun(Face, Edge, We) ->
		   wings_draw_util:face(Face, Edge, We)
	   end,
    mkdl_draw_faces(Shape, Draw),
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{Id,Shape}|Shs], [{Id,Faces}|Fs], false) ->
    Draw = fun(F, Fs0, Edge, We) ->
		   case gb_sets:is_member(F, Fs0) of
		       false -> wings_draw_util:face(F, Edge, We);
		       true -> ok
		   end
	   end,
    mkdl_draw_faces(Shape, Faces, Draw),
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{Id,Shape}|Shs], [{Id,Faces}|Fs], true) ->
    Draw = fun(F, Fs0, Edge, We) ->
		   case gb_sets:is_member(F, Fs0) of
		       true -> wings_draw_util:face(F, Edge, We);
		       false -> ok
		   end
	   end,
    mkdl_draw_faces(Shape, Faces, Draw),
    make_dlist_1(Shs, Fs, true);
make_dlist_1([{Id,Shape}|Shs], Fs, false) ->
    Draw = fun(F, Fs0, Edge, We) ->
		   wings_draw_util:face(F, Edge, We)
	   end,
    mkdl_draw_faces(Shape, dummy, Draw),
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{Id,Shape}|Shs], Fs, true) ->
    Draw = fun(F, Fs0, Edge, We) -> ok end,
    mkdl_draw_faces(Shape, dummy, Draw),
    make_dlist_1(Shs, Fs, true);
make_dlist_1([], Fs, Draw) -> ok.

mkdl_draw_faces(#shape{sh=#we{}=We}, Faces, Draw) ->
    wings_util:fold_face(
      fun(Face, #face{edge=Edge}, _) ->
	      Draw(Face, Faces, Edge, We)
      end, [], We);
mkdl_draw_faces(_, _, _) -> ok.

mkdl_draw_faces(#shape{sh=#we{}=We}, Draw) ->
    wings_util:fold_face(
      fun(Face, #face{edge=Edge}, _) ->
	      Draw(Face, Edge, We)
      end, [], We);
mkdl_draw_faces(_, _) -> ok.

draw_faces(#we{}=We, St) ->
    wings_util:fold_face(
      fun(Face, #face{edge=Edge}, _) ->
	      wings_draw_util:face(Face, Edge, We)
      end, [], We).

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
%% Draw the currently selected items.
%% 

draw_selection(#st{selmode=body}=St) ->
    wings_sel:foreach(
      fun(_, #shape{sh=Data}) ->
	      draw_faces(Data, St)
      end, St),
    St;
draw_selection(#st{selmode=face}=St) ->
    wings_sel:foreach(
      fun(Face, #shape{sh=#we{fs=Ftab}=We}) ->
	      wings_draw_util:sel_face(Face, We)
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
