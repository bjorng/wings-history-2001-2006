%%
%%  wings_drag.erl --
%%
%%     This module handles interactive commands and the "camera mode"
%%     (rotation, zooming, and panning using the middle mouse button).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_drag.erl,v 1.23 2001/11/17 13:16:11 bjorng Exp $
%%

-module(wings_drag).
-export([init_drag/3,init_drag/4,do_drag/1,message/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(CTRL_BITS, (?KMOD_LCTRL bor ?KMOD_RCTRL)).
-define(ALT_BITS, (?KMOD_LALT bor ?KMOD_RALT)).
-define(SHIFT_BITS, (?KMOD_LSHIFT bor ?KMOD_RSHIFT)).

-import(lists, [foreach/2,map/2,foldl/3,sort/1,reverse/1]).

-record(drag,
	{x,					%Original 2D position
	 y,					
	 xs=0,
	 ys=0,
	 tvs,					%[{Vertex,Vec}...]
	 constraint,				%Constraints for motion
	 unit,					%Unit that drag is done in.
	 shapes,				%Shapes before drag.
	 dl					%Private version of dl
						% (temporary hack).
	}).

%% Display lists. XXX Temporary, should go into the drag record.
-record(adl,
	{we=none,				%Winged edge objects.
	 dragging=none,				%WE faces being dragged.
	 drag_faces=none,			%GbSet containing faces.
	 old_sel,				%Actual selection.
	 sel=none,				%For selected faces.
	 matrix=e3d_mat:identity()}).

init_drag(Tvs, Constraint, St) ->
    init_drag(Tvs, Constraint, none, St).

init_drag(Tvs0, Constraint, Unit, #st{shapes=OldShapes}=St0) ->
    Tvs = combine(Tvs0),
    Faces = faces(Tvs, St0),
    wings_io:grab(),
    {_,X,Y} = sdl_mouse:getMouseState(),
    Drag0 = #drag{x=X,y=Y,tvs=Tvs,constraint=Constraint,
		  unit=Unit,shapes=OldShapes},

    %% Temporary hack to enable us to have our private display list
    %% record here. Should be moved into drag.
    St1 = St0#st{saved=false,drag=Drag0,dl=#adl{drag_faces=Faces}},
    #st{dl=Dl} = St = motion(X, Y, St1),
    Drag = Drag0#drag{dl=Dl},
    St#st{drag=Drag,dl=none}.

combine(Tvs) ->
    S = sofs:relation(Tvs),
    F = sofs:relation_to_family(S),
    %% The rest of this function is an optimisation.
    map(fun({Id,[Fun]}) when function(Fun) -> {Id,Fun};
	   ({Id,L}) ->
		SS = sofs:from_term(L, [[{vec,[vertex]}]]),
		RR = sofs:union(SS),
 		FF = sofs:relation_to_family(RR),
  		FU = sofs:family_union(FF),
		{Id,sofs:to_external(FU)}
	end, sofs:to_external(F)).

do_drag(#st{drag=#drag{dl=Dl}}=St0) ->
    St = St0#st{dl=Dl},
    {seq,{push,dummy},get_drag_event(St)}.

get_drag_event(St) ->
    redraw(St),
    {replace,fun(Ev) -> handle_drag_event(Ev, St) end}.

handle_drag_event(Event, St) ->
    case wings_camera:event(Event, St) of
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
    St = motion(X, Y, St0),
    DragEnded = {drag_ended,normalize(St#st{drag=undefined,dl=none})},
    wings_io:putback_event(DragEnded),
    pop;
handle_drag_event_1(#mousebutton{button=3,x=X,y=Y,state=?SDL_RELEASED}, St) ->
    wings_io:ungrab(),
    wings_io:putback_event(drag_aborted),
    pop;
handle_drag_event_1(view_changed, St) ->
    get_drag_event(view_changed(St));
handle_drag_event_1(Event, St0) ->
    St = case wings_hotkey:event(Event) of
	     next -> St0;
	     {view,aim} ->
		 wings_view:aim(St0),
		 view_changed(St0),
		 St0;
	     {view,{along,Axis}} ->
		 wings_view:along(Axis, St0),
		 view_changed(St0),
		 St0;
	     {view,reset} ->
		 wings_view:reset(),
		 view_changed(St0),
		 St0;
	     {select,less} ->
		 magnet_radius(-1, St0);
	     {select,more} ->
		 magnet_radius(1, St0);
	     Other -> St0
	 end,
    get_drag_event(St).


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
    St#st{drag=Drag};
view_changed(St) -> St.

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

motion_update(Tvs, Dx, Dy, #st{shapes=Shapes0,dl=Dl}=St) ->
    {Matrix,Shapes} =
	foldl(fun({Id,Tr}, {Matrix0,Shapes0}) when function(Tr) ->
		      Sh0 = gb_trees:get(Id, Shapes0),
		      case Tr(Sh0, Dx, Dy, St) of
			  {shape_matrix,Matrix} ->
			      Sh = Sh0#shape{matrix=Matrix},
			      Shapes = gb_trees:update(Id, Sh, Shapes0),
			      {Matrix,Shapes};
			  {shape,Sh} ->
			      Shapes = gb_trees:update(Id, Sh, Shapes0),
			      {Matrix0,Shapes};
			  {tvs,List} ->
			      Sh0 = gb_trees:get(Id, Shapes0),
			      Sh = transform_vs(List, Dx, Dy, St, Sh0),
			      {Matrix0,gb_trees:update(Id, Sh, Shapes0)}
		      end;
		 ({Id,List}, {Matrix0,Shapes}) ->
		      Sh0 = gb_trees:get(Id, Shapes),
		      Sh = transform_vs(List, Dx, Dy, St, Sh0),
		      {Matrix0,gb_trees:update(Id, Sh, Shapes)}
	      end, {none,Shapes0}, Tvs),
    case Matrix of
	none ->
	    St#st{shapes=Shapes,dl=Dl#adl{dragging=none,sel=none,
					 matrix=e3d_mat:identity()}};
	Other ->
	    St#st{shapes=Shapes,dl=Dl#adl{matrix=Matrix,sel=none}}
    end.

transform_vs(Tvs, Dx, Dy, St, #shape{sh=#we{vs=Vtab0}=We}=Shape0) ->
    show_message(Tvs, Dx, Dy, St),
    Vtab = foldl(fun ({Vec,Vs}, Tab) ->
			 trans_vec(Vec, Dx, Dy, Vs, St, Tab)
		 end, Vtab0, Tvs),
    Shape0#shape{sh=We#we{vs=Vtab}}.

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

message(L, #st{drag=#drag{unit=Unit}}) ->
    message_0(L, Unit);
message(L, Unit) when atom(Unit) ->
    message_0(L, Unit).

message_0([_]=L, none) ->
    message_1("~-10.2f", L);
message_0([U]=L, percent) ->
    message_1("~p%", [round(100*U)]);
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

faces(Tvs, #st{shapes=Shapes}) ->
    [{Id,faces_1(Vs, gb_trees:get(Id, Shapes))} || {Id,Vs} <- Tvs].

faces_1(Tr, #shape{sh=#we{fs=Ftab}=We}) when function(Tr) ->
    gb_sets:from_list(gb_trees:keys(Ftab));
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

normalize(#st{shapes=Shapes0}=St) ->
    Ident = e3d_mat:identity(),
    Shapes = foldl(fun(Sh, A) ->
			   normalize(Sh, Ident, A)
		   end, Shapes0, gb_trees:to_list(Shapes0)),
    St#st{shapes=Shapes}.

normalize({Id,#shape{matrix=Ident}}, Ident, A) -> A;
normalize({Id,#shape{matrix=Matrix,sh=#we{vs=Vtab0}=We}=Sh0}, Ident, A) ->
    Vtab1 = foldl(
	      fun({V,Vtx}, A) -> 
		      #vtx{pos=Pos0}= Vtx,
		      Pos = e3d_mat:mul_point(Matrix, Pos0),
		      [{V,Vtx#vtx{pos=Pos}}|A]
	      end, [], gb_trees:to_list(Vtab0)),
    Vtab = gb_trees:from_orddict(reverse(Vtab1)),
    Sh = Sh0#shape{matrix=Ident,sh=We#we{vs=Vtab}},
    gb_trees:update(Id, Sh, A).

%%%
%%% Redrawing while dragging.
%%%

redraw(St0) ->
    St = render(St0),
    wings_io:update(St).

render(#st{shapes=Shapes}=St0) ->
    ?CHECK_ERROR(),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:enable(?GL_DEPTH_TEST),
    wings_view:projection(),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    wings_view:model_transformations(St0),
    wings_draw:ground_and_axes(),
    St1 = update_display_lists(St0),
    St = make_sel_dlist(St1),
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
	    draw_we(St)
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
    draw_we(St),

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

draw_sel(#st{sel=[],dl=#adl{sel=none}}=St) -> ok;
draw_sel(#st{selmode=edge,dl=#adl{sel=DlistSel}}) ->
    sel_color(),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:callList(DlistSel);
draw_sel(#st{selmode=vertex,dl=#adl{sel=DlistSel}}) ->
    sel_color(),
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:callList(DlistSel);
draw_sel(#st{dl=#adl{sel=DlistSel}}) ->
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    sel_color(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    case catch gl:callList(DlistSel) of
	{'EXIT',_} -> exit({bad_call_list,DlistSel});
	_ -> ok
    end.
    
draw_we(#st{dl=#adl{we=DlistWe,dragging=WeDrag,matrix=Matrix}}) ->
    gl:callList(DlistWe),
    case WeDrag of
	none -> ok;
	Other ->
	    gl:pushMatrix(),
	    gl:multMatrixf(e3d_mat:expand(Matrix)),
	    gl:callList(WeDrag),
	    gl:popMatrix()
    end.

update_display_lists(#st{shapes=Shapes,dl=none}=St) ->
    DlistWe = 98,
    gl:newList(DlistWe, ?GL_COMPILE),
    foreach(fun(Sh) ->
		    shape(Sh, St)
	    end, gb_trees:values(Shapes)),
    gl:endList(),
    St#st{dl=#adl{we=DlistWe}};
update_display_lists(#st{dl=#adl{drag_faces=none}}=St) -> St;
update_display_lists(#st{dl=#adl{we=none,drag_faces=Faces}=DL}=St) ->
    %% Collect the static display list - faces that will not be moved.
    DlistId = make_dlist(98, Faces, false, St),
    update_display_lists(St#st{dl=DL#adl{we=DlistId}});
update_display_lists(#st{dl=#adl{dragging=none,drag_faces=Faces}=DL}=St) ->
    %% Collect the dynamic display list - everything that will be moved.
    DlistId = make_dlist(97, Faces, true, St),
    update_display_lists(St#st{dl=DL#adl{dragging=DlistId}});
update_display_lists(St) -> St.

make_sel_dlist(#st{sel=[],dl=DL}=St) ->
    St#st{dl=DL#adl{sel=none}};
make_sel_dlist(#st{dl=#adl{sel=none}}=St) ->
    do_make_sel_dlist(St);
make_sel_dlist(#st{sel=Sel,dl=#adl{old_sel=Sel}}=St) ->
    St;
make_sel_dlist(#st{dl=DL}=St) ->
    do_make_sel_dlist(St);
make_sel_dlist(St) -> St.

do_make_sel_dlist(#st{sel=Sel,dl=DL}=St) ->
    DlistSel = 95,
    gl:newList(DlistSel, ?GL_COMPILE),
    draw_selection(St),
    gl:endList(),
    St#st{dl=DL#adl{old_sel=Sel,sel=DlistSel}}.

make_dlist(DlistId, Faces, DrawMembers, #st{shapes=Shapes0}=St) ->
    gl:newList(DlistId, ?GL_COMPILE),
    make_dlist_1(gb_trees:to_list(Shapes0), Faces, DrawMembers),
    gl:endList(),
    DlistId.

make_dlist_1([{Id,Shape}|Shs], [{Id,Faces}|Fs], DrawMembers) ->
    Draw = fun(F, Fs0) -> DrawMembers =:= gb_sets:is_member(F, Fs0) end,
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

shape(#shape{sh=Data}, St) ->
    draw_faces(Data, St).

draw_faces(#we{}=We, St) ->
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
      fun(_, #shape{matrix=Matrix,sh=Data}) ->
	      gl:pushMatrix(),
	      gl:multMatrixf(e3d_mat:expand(Matrix)),
	      draw_faces(Data, St),
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
