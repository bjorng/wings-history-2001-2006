%%
%%  wings_drag.erl --
%%
%%     This module handles camera moves (rotation, zooming, and panning).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_camera.erl,v 1.1 2001/11/16 18:19:41 bjorng Exp $
%%

-module(wings_camera).
-export([event/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(CTRL_BITS, (?KMOD_LCTRL bor ?KMOD_RCTRL)).
-define(ALT_BITS, (?KMOD_LALT bor ?KMOD_RALT)).
-define(SHIFT_BITS, (?KMOD_LSHIFT bor ?KMOD_RSHIFT)).

-import(lists, [foreach/2,map/2,foldl/3,sort/1,reverse/1]).

-record(camera,
	{x,y,					%Current mouse position.
	 ox,oy,					%Original mouse position.
	 xs=0,ys=0				%Current virtual position.
	}).

event(Ev, St) ->
    case wings_pref:get_value(camera_style, blender) of
	blender -> blender(Ev, St)
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
    #camera{ox=OX,oy=OY} = Camera,
    case wings_io:ungrab() of
	still_grabbed ->
	    sdl_mouse:warpMouse(OX, OY),
	    wings_io:putback_event(view_changed),
	    pop;
	no_grab -> pop
    end;
blender_event(#mousemotion{x=X,y=Y}, Camera0, St0) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case sdl_keyboard:getModState() of
	Mod when Mod band ?SHIFT_BITS =/= 0 ->
	    #view{pan_x=PanX0,pan_y=PanY0} = View = wings_view:current(),
	    PanX = PanX0 + Dx / 10,
	    PanY = PanY0 - Dy / 10,
	    wings_view:set_current(View#view{pan_x=PanX,pan_y=PanY});
	Mod when Mod band ?CTRL_BITS =/= 0 ->
	    #view{distance=Dist} = View = wings_view:current(),
	    wings_view:projection(),
	    wings_view:set_current(View#view{distance=Dist-Dy/10});
	Other ->
	    #view{azimuth=Az0,elevation=El0} = View = wings_view:current(),
	    Az = Az0 + Dx,
	    El = El0 + Dy,
	    wings_view:set_current(View#view{azimuth=Az,elevation=El})
	end,
    St = wings:redraw(St0),
    get_blender_event(Camera, St);
blender_event(Other, Camera, St) -> keep.

get_blender_event(Camera, St) ->
    {replace,fun(Ev) -> blender_event(Ev, Camera, St) end}.

%%%
%%% Common utilities.
%%%		     

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
