%%
%%  wings_drag.erl --
%%
%%     This module handles interactive commands and the "camera mode"
%%     (rotation, zooming, and panning using the middle mouse button).
%%
%%  Copyright (c) 2001 Jakob Cederlund, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_drag.erl,v 1.8 2001/09/17 07:19:18 bjorng Exp $
%%

-module(wings_drag).
-export([start_camera/3,stop_camera/1,view_changed/1,
	 init_drag/3,abort_drag/1,click/3,motion/3]).

-include("wings.hrl").
-include("gl.hrl").
-include("sdl_video.hrl").
-include("sdl_events.hrl").
-include("sdl_keyboard.hrl").

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
	 shapes					%Shapes before drag
	 }).

-record(camera,
	{x,y,					%Current mouse position.
	 ox,oy,					%Original mouse position.
	 xs=0,ys=0				%Current virtual position.
	}).

start_camera(X, Y, St) ->
    sdl_mouse:showCursor(false),
    sdl_video:wm_grabInput(?SDL_GRAB_ON),
    St#st{camera=#camera{x=X,y=Y,ox=X,oy=Y}}.

stop_camera(#st{camera=#camera{ox=OX,oy=OY}}=St0) ->
    case St0#st{camera=undefined} of
	#st{drag=#drag{}}=St ->
	    sdl_mouse:warpMouse(OX, OY),
	    view_changed(St);
	St ->
	    sdl_mouse:showCursor(true),
	    sdl_video:wm_grabInput(?SDL_GRAB_OFF),
	    St
    end;
stop_camera(St) -> St.

init_drag(Tvs0, Constraint, #st{shapes=OldShapes}=St0) ->
    sdl_mouse:showCursor(false),
    sdl_video:wm_grabInput(?SDL_GRAB_ON),
    Tvs = combine(Tvs0),
    {_,X,Y} = sdl_mouse:getMouseState(),
    Drag = #drag{x=X,y=Y,tvs=Tvs,constraint=Constraint,shapes=OldShapes},
    Faces = faces(Tvs, St0),
    St = St0#st{saved=false,drag=Drag,dl=#dl{drag_faces=Faces}},
    motion(X, Y, St).

view_changed(#st{drag=#drag{constraint=view_dependent}=Drag0}=St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    Drag = Drag0#drag{x=X,y=Y,xs=0,ys=0,shapes=St#st.shapes},
    St#st{drag=Drag};
view_changed(St) -> St.

combine(Tvs) ->
    S = sofs:relation(Tvs),
    F = sofs:relation_to_family(S),
    map(fun({Id,[Fun]}) when function(Fun) -> {Id,Fun};
	   ({Id,L}) ->
		RR = sofs:relation(lists:append(L)),
		FF = sofs:relation_to_family(RR),
 		EF = sofs:to_external(FF),
 		{Id,map(fun({Vec,Vs}) -> {Vec,lists:append(Vs)} end, EF)}
	end, sofs:to_external(F)).

abort_drag(#st{drag=undefined}=St) -> no_drag;
abort_drag(#st{drag=#drag{}}) ->
    sdl_mouse:showCursor(true),
    sdl_video:wm_grabInput(?SDL_GRAB_OFF),
    drag_aborted.

click(X, Y, #st{drag=undefined}=St) ->
    {select,St};
click(X, Y, #st{drag=#drag{}}=St0) ->
    sdl_mouse:showCursor(true),
    sdl_video:wm_grabInput(?SDL_GRAB_OFF),
    St = motion(X, Y, St0),
    {drag_ended,normalize(St#st{drag=undefined,dl=none})}.

motion(X, Y, #st{camera=#camera{}=Camera0}=St0) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    St = St0#st{camera=Camera},
    case sdl_keyboard:getModState() of
	Mod when Mod band ?SHIFT_BITS =/= 0 ->
	    #st{pan_x=PanX0,pan_y=PanY0} = St,
	    PanX = PanX0 + Dx,
	    PanY = PanY0 - Dy,
	    St#st{pan_x=PanX,pan_y=PanY};
	Mod when Mod band ?CTRL_BITS =/= 0 ->
	    #st{distance=Dist} = St,
	    St#st{distance=Dist-Dy};
	Other ->
	    #st{azimuth=Az0,elevation=El0} = St,
	    Az = Az0 + Dx,
	    El = El0 + Dy,
	    St#st{azimuth=Az,elevation=El}
    end;
motion(X, Y, #st{drag=undefined}=St) -> St;
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

camera_mouse_range(X0, Y0, #camera{x=OX,y=OY,xs=Xs,ys=Ys}=Camera) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    if
	X0 =:= W-1 ->
	    NewX = W div 2,
	    camera_warp(NewX, Y0, NewX, 0, Camera);
	X0 =:= 0 ->
	    NewX = W div 2,
	    camera_warp(NewX, Y0, -NewX, 0, Camera);
	Y0 =:= H-1 ->
	    NewY = H div 2,
	    camera_warp(X0, NewY, 0, NewY, Camera);
	Y0 =:= 0 ->
	    NewY = H div 2,
	    camera_warp(X0, NewY, 0, -NewY, Camera);
	true ->
	    X = X0 + Xs,
	    Y = Y0 + Ys,
	    Dx = (X-OX) / 5,
	    Dy = (Y-OY) / 5,
	    {Dx,Dy,Camera#camera{x=X,y=Y}}
    end.

camera_warp(X, Y, XsInc, YsInc, #camera{xs=Xs,ys=Ys}=Camera) ->
    warp_mouse(X, Y),
    camera_mouse_range(X, Y, Camera#camera{xs=Xs+XsInc,ys=Ys+YsInc}).

warp_mouse(X, Y) ->
    %% Strangely enough, on Solaris the warp doesn't seem to
    %% work unless the mouse cursor is visible.
    %% On Windows, the mouse cursor must no be visible.
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
		  Mod when Mod band ?ALT_BITS =/= 0,
			   Mod band ?CTRL_BITS =:= 0 ->
		      {trunc(10*Dx0)/10,trunc(10*Dy0)/10};
		  Mod when Mod band ?SHIFT_BITS =/= 0,
			   Mod band ?CTRL_BITS =:= 0 ->
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
	    St#st{shapes=Shapes,dl=Dl#dl{dragging=none,
					 matrix=e3d_mat:identity()}};
	Other ->
	    St#st{shapes=Shapes,dl=Dl#dl{matrix=Matrix}}
    end.

transform_vs(Vs0, Dx, Dy, St, #shape{sh=#we{vs=Vtab0}=We}=Shape0) ->
    Vtab = foldl(fun ({Vec,Vs}, Tab) ->
			 trans_vec(Vec, Dx, Dy, Vs, Tab)
		 end, Vtab0, Vs0),
    Shape0#shape{sh=We#we{vs=Vtab}}.

trans_vec({rot,{Cx,Cy,Cz},Vec}, Dx, Dy, Vs, Vtab) ->
    A = 15*Dx,
    wings_io:message(lists:flatten(io_lib:format("A:~10p", [A]))),
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(A, Vec)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    foldl(fun(V, Tab) -> 
		  #vtx{pos=Pos0}= Vtx = gb_trees:get(V, Tab),
		  Pos = e3d_mat:mul_point(M, Pos0),
		  gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab)
	  end, Vtab, Vs);
trans_vec({free,Matrix}, Dx, Dy, Vs, Vtab) ->
    wings_io:message(lists:flatten(io_lib:format("X:~10p Y:~10p", [Dx,Dy]))),
    {Xt,Yt,Zt} = e3d_mat:mul_point(Matrix, {Dx,Dy,0.0}),
    foldl(fun(V, Tab) -> 
		  #vtx{pos={X,Y,Z}}= Vtx = gb_trees:get(V, Tab),
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab)
	  end, Vtab, Vs);
trans_vec({Xt0,Yt0,Zt0}, Dx, Dy, Vs, Vtab) ->
    wings_io:message(lists:flatten(io_lib:format("X:~10p", [Dx]))),
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    translate(Xt, Yt, Zt, Vs, Vtab).

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
faces_1(Tr, #shape{}) when function(Tr) -> gb_sets:empty();
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
