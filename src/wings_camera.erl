%%
%%  wings_camera.erl --
%%
%%     This module handles camera moves (rotation, zooming, and panning).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_camera.erl,v 1.2 2001/11/17 07:02:37 bjorng Exp $
%%

-module(wings_camera).
-export([sub_menu/1,command/1,event/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(CTRL_BITS, (?KMOD_LCTRL bor ?KMOD_RCTRL)).
-define(ALT_BITS, (?KMOD_LALT bor ?KMOD_RALT)).
-define(SHIFT_BITS, (?KMOD_LSHIFT bor ?KMOD_RSHIFT)).

-import(lists, [foreach/2,map/2,foldl/3,sort/1,reverse/1,append/1]).

-record(camera,
	{x,y,					%Current mouse position.
	 ox,oy,					%Original mouse position.
	 xs=0,ys=0				%Current virtual position.
	}).

sub_menu(St) ->
    Mode = wings_pref:get_value(camera_mode, blender),
    Modes0 = [item(Mode, blender),
	      item(Mode, nendo),
	      item(Mode, tds),
	      item(Mode, maya)],
    Modes = list_to_tuple(append(Modes0)),
    {"Camera Mode: " ++ mode_desc(Mode),
     {camera_mode,Modes}}.

item(Mode, Mode) -> [];
item(Other, Mode) -> [{mode_desc(Mode),Mode}].

mode_desc(blender) -> "Wings/Blender";
mode_desc(nendo) -> "Nendo";
mode_desc(tds) -> "3ds max";
mode_desc(maya) -> "Maya".
    
command(Mode) ->
    wings_pref:set_value(camera_mode, Mode).

%% Event handler.

event(Ev, St) ->
    case wings_pref:get_value(camera_mode, blender) of
	blender -> blender(Ev, St);
	nendo -> nendo(Ev, St);
	tds -> tds(Ev, St);
	maya -> maya(Ev, St)
    end.

%%%
%%% Default Wings/Blender style camera.
%%%

blender(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}=Mb, St) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    blender(Mb#mousebutton{button=2}, St);
	Mod -> next
    end;
blender(#mousebutton{button=2,x=X,y=Y,state=?SDL_PRESSED}, St) ->
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    wings_io:grab(),
    {seq,{push,dummy},get_blender_event(Camera, St)};
blender(_, _) -> next.

blender_event(#mousebutton{button=1,state=?SDL_RELEASED}=Mb, Camera, St) ->
    blender_event(Mb#mousebutton{button=2}, Camera, St);
blender_event(#mousebutton{button=2,state=?SDL_RELEASED}, Camera, St) ->
    stop_camera(Camera);
blender_event(#mousemotion{x=X,y=Y}, Camera0, St0) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case sdl_keyboard:getModState() of
	Mod when Mod band ?SHIFT_BITS =/= 0 ->
	    pan(Dx/10, Dy/10);
	Mod when Mod band ?CTRL_BITS =/= 0 ->
	    zoom(Dy/10);
	Other ->
	    rotate(Dx, Dy)
    end,
    St = wings:redraw(St0),
    get_blender_event(Camera, St);
blender_event(Other, Camera, St) -> keep.

get_blender_event(Camera, St) ->
    {replace,fun(Ev) -> blender_event(Ev, Camera, St) end}.

%%%
%%% Nendo style camera.
%%%

nendo(#mousebutton{button=3,x=X,y=Y,state=?SDL_RELEASED}=Mb, St) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?CTRL_BITS =/= 0 ->
	    nendo(Mb#mousebutton{button=2}, St);
	Mod -> next
    end;
nendo(#mousebutton{button=2,x=X,y=Y,state=?SDL_RELEASED}, St) ->
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    wings_io:grab(),
    {seq,{push,dummy},get_nendo_event(Camera, St)};
nendo(#keyboard{keysym=#keysym{sym=Sym}}, St) ->
    nendo_pan(Sym, St);
nendo(_, _) -> next.

nendo_event(#mousebutton{button=1,state=?SDL_RELEASED}, Camera, St) ->
    stop_camera(Camera);
nendo_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, St0) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 6 of
	0 ->					%None of MMB/RMB pressed.
	    rotate(-Dx, -Dy);
	Other ->				%MMB and/or RMB pressed.
	    zoom(Dy/10)
    end,
    St = wings:redraw(St0),
    get_nendo_event(Camera, St);
nendo_event(Event, Camera, St) ->
    case wings_hotkey:event(Event) of
	{view,aim} ->
	    wings_view:aim(St),
	    St = wings:redraw(St),
	    get_nendo_event(Camera, St);
	{view,{along,Axis}} ->
	    wings_view:along(Axis, St),
	    St = wings:redraw(St),
	    get_nendo_event(Camera, St);
	{view,reset} ->
	    wings_view:reset(),
	    St = wings:redraw(St),
	    get_nendo_event(Camera, St);
	{view,orthogonal_view} ->
	    wings_view:command(orthogonal_view, St),
	    St = wings:redraw(St),
	    get_nendo_event(Camera, St);
	next ->
	    case Event of
		#keyboard{keysym=#keysym{sym=Sym}} ->
		    nendo_pan(Sym, St);
		Other -> keep
	    end;
	Other -> keep
    end.
    
nendo_pan(?SDLK_LEFT, St) ->
    nendo_pan(0.1, 0.0, St);
nendo_pan(?SDLK_RIGHT, St) ->
    nendo_pan(-0.1, 0.0, St);
nendo_pan(?SDLK_UP, St) ->
    nendo_pan(0.0, 0.1, St);
nendo_pan(?SDLK_DOWN, St) ->
    nendo_pan(0.0, -0.1, St);
nendo_pan(_, _) -> next.

nendo_pan(Dx, Dy, St) ->
    pan(Dx, Dy),
    St = wings:redraw(St),
    keep.
    
get_nendo_event(Camera, St) ->
    {replace,fun(Ev) -> nendo_event(Ev, Camera, St) end}.

%%%
%%% 3ds max style camera.
%%%

tds(#mousebutton{button=2,x=X,y=Y,state=?SDL_PRESSED}, St) ->
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    wings_io:grab(),
    {seq,{push,dummy},get_tds_event(Camera, St)};
tds(_, _) -> next.

tds_event(#mousebutton{button=1,state=?SDL_RELEASED}=Mb, Camera, St) ->
    tds_event(Mb#mousebutton{button=2}, Camera, St);
tds_event(#mousebutton{button=2,state=?SDL_RELEASED}, Camera, St) ->
    stop_camera(Camera);
tds_event(#mousemotion{x=X,y=Y}, Camera0, St0) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case sdl_keyboard:getModState() of
	Mod when Mod band ?CTRL_BITS =/= 0, Mod band ?ALT_BITS =/= 0 ->
	    zoom(Dy/10);
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    rotate(Dx, Dy);
	Other ->
	    pan(Dx/10, Dy/10)
    end,
    St = wings:redraw(St0),
    get_tds_event(Camera, St);
tds_event(Other, Camera, St) -> keep.

get_tds_event(Camera, St) ->
    {replace,fun(Ev) -> tds_event(Ev, Camera, St) end}.

%%%
%%% Maya style camera.
%%%

maya(#mousebutton{button=B,x=X,y=Y,state=?SDL_PRESSED}, St)
  when B == 1; B == 2  ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    sdl_events:eventState(?SDL_KEYUP, ?SDL_ENABLE),
	    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
	    wings_io:grab(),
	    {seq,{push,dummy},get_maya_event(Camera, St)};
	Mod -> next
    end;
maya(_, _) -> next.

maya_event(#keyboard{keysym=#keysym{sym=?SDLK_LALT},state=?SDL_RELEASED},
	   Camera, St) ->
    maya_stop_camera(Camera);
maya_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, St0) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case sdl_keyboard:getModState() of
	Mod when Mod band ?ALT_BITS == 0 ->	%Just in case.
	    maya_stop_camera(Camera);
	Mod ->
	    if
		Buttons band 3 == 3 ->		%LMB+MMB
		    zoom(Dy/10);
		Buttons band 1 == 1 ->		%LMB
		    rotate(Dx, Dy);
		Buttons band 2 == 2 ->		%MMB
		    pan(Dx/10, Dy/10);
		true -> ok
	    end
    end,
    St = wings:redraw(St0),
    get_maya_event(Camera, St);
maya_event(Other, Camera, St) -> keep.

get_maya_event(Camera, St) ->
    {replace,fun(Ev) -> maya_event(Ev, Camera, St) end}.

maya_stop_camera(Camera) ->
    sdl_events:eventState(?SDL_KEYUP, ?SDL_IGNORE),
    stop_camera(Camera).
    
%%%
%%% Common utilities.
%%%		     

rotate(Dx, Dy) ->
    #view{azimuth=Az0,elevation=El0} = View = wings_view:current(),
    Az = Az0 + Dx,
    El = El0 + Dy,
    wings_view:set_current(View#view{azimuth=Az,elevation=El}).

zoom(Delta) ->
    #view{distance=Dist} = View = wings_view:current(),
    wings_view:projection(),
    wings_view:set_current(View#view{distance=Dist+Delta}).

pan(Dx, Dy) ->
    #view{pan_x=PanX0,pan_y=PanY0} = View = wings_view:current(),
    PanX = PanX0 + Dx,
    PanY = PanY0 - Dy,
    wings_view:set_current(View#view{pan_x=PanX,pan_y=PanY}).
    
stop_camera(#camera{ox=OX,oy=OY}) ->
    case wings_io:ungrab() of
	still_grabbed ->
	    sdl_mouse:warpMouse(OX, OY),
	    wings_io:putback_event(view_changed),
	    pop;
	no_grab -> pop
    end.

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
    %% On Windows, the mouse cursor must not be visible.
    case os:type() of
	{unix,solaris} ->
	    sdl_mouse:showCursor(true),
	    sdl_mouse:warpMouse(X, Y),
	    sdl_mouse:showCursor(false);
	_ ->
	    sdl_mouse:warpMouse(X, Y)
    end.
