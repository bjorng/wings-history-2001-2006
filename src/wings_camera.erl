%%
%%  wings_camera.erl --
%%
%%     This module handles camera moves (rotation, zooming, and panning).
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_camera.erl,v 1.26 2002/04/19 18:38:44 bjorng Exp $
%%

-module(wings_camera).
-export([sub_menu/1,command/2,help/0,event/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,map/2,foldl/3,sort/1,reverse/1,append/1]).

-define(ZOOM_FACTOR, 20).

-record(camera,
	{x,y,					%Current mouse position.
	 ox,oy,					%Original mouse position.
	 xt=0,yt=0				%Last warp length.
	}).

sub_menu(_St) ->
    {"Camera Mode",camera_mode}.

command(camera_mode, St) ->
    DefVar = {mode,wings_pref:get_value(camera_mode, blender)},
    ZoomFlag0 = wings_pref:get_value(wheel_zooms, true),
    ZoomFactor0 = wings_pref:get_value(wheel_zoom_factor, ?ZOOM_FACTOR),
    Qs = [{vframe,[{alt,DefVar,"Wings/Blender",blender},
		   {alt,DefVar,"Nendo",nendo},
		   {alt,DefVar,"3ds max",tds},
		   {alt,DefVar,"Maya",maya}],
	   [{title,"Camera Mode"}]},
	  {vframe,
	   [{"Wheel zooms",ZoomFlag0},
	    {hframe,[{label,"Zoom Factor"},{text,ZoomFactor0},{label,"%"}]}],
	   [{title,"Scroll Wheel"}]}],
    wings_ask:dialog(Qs, St,
		  fun([Mode,ZoomFlag,ZoomFactor]) ->
			  wings_pref:set_value(camera_mode, Mode),
			  wings_pref:set_value(wheel_zooms, ZoomFlag),
			  wings_pref:set_value(wheel_zoom_factor, ZoomFactor),
			  ignore
		  end).

help() ->
    case wings_pref:get_value(camera_mode, blender) of
	blender ->
	    [mmb] ++ " Tumble [Shift]+" ++
		[mmb] ++ " Track [Ctrl]+" ++
    		[mmb] ++ " Dolly";
	nendo ->
	    [mmb] ++ " or [Ctrl]+" ++ [rmb] ++ " Start camera";
	tds ->
	    "[Alt]+" ++ [mmb] ++ " Tumble  " ++
		[mmb] ++ " Track [Ctrl]+[Alt]+" ++
		[mmb] ++ " Dolly";
	maya ->
	    "[Alt]+" ++ [lmb] ++ " Tumble [Alt]+" ++
		[mmb] ++ " Track [Alt]+" ++
		[rmb] ++ " Dolly"
    end.
						   
%% Event handler.

event(#mousebutton{button=4,state=?SDL_RELEASED}, _Redraw) ->
    zoom_step(-1);
event(#mousebutton{button=5,state=?SDL_RELEASED}, _Redraw) ->
    zoom_step(1);
event(Ev, Redraw) ->
    case wings_pref:get_value(camera_mode, blender) of
	blender -> blender(Ev, Redraw);
	nendo -> nendo(Ev, Redraw);
	tds -> tds(Ev, Redraw);
	maya -> maya(Ev, Redraw)
    end.

%%%
%%% Default Wings/Blender style camera.
%%%

blender(#mousebutton{button=1,state=?SDL_PRESSED}=Mb, Redraw) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    blender(Mb#mousebutton{button=2}, Redraw);
	Mod -> next
    end;
blender(#mousebutton{button=2,x=X,y=Y,state=?SDL_PRESSED}, Redraw) ->
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    wings_io:grab(),
    wings_io:clear_message(),
    wings_io:message(help()),
    {seq,{push,dummy},get_blender_event(Camera, Redraw)};
blender(_, _) -> next.

blender_event(#mousebutton{button=1,state=?SDL_RELEASED}=Mb, Camera, Redraw) ->
    blender_event(Mb#mousebutton{button=2}, Camera, Redraw);
blender_event(#mousebutton{button=2,state=?SDL_RELEASED}, Camera, _Redraw) ->
    stop_camera(Camera);
blender_event(#mousemotion{x=X,y=Y}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case sdl_keyboard:getModState() of
	Mod when Mod band ?SHIFT_BITS =/= 0 ->
	    pan(Dx/10, Dy/10);
	Mod when Mod band ?CTRL_BITS =/= 0 ->
	    zoom(Dy);
	_Other ->
	    rotate(Dx, Dy)
    end,
    wings_wm:dirty(),
    get_blender_event(Camera, Redraw);
blender_event(Other, Camera, Redraw) ->
    generic_event(Other, Camera, Redraw).

get_blender_event(Camera, Redraw) ->
    {replace,fun(Ev) -> blender_event(Ev, Camera, Redraw) end}.

%%%
%%% Nendo style camera.
%%%

nendo(#mousebutton{button=3,state=?SDL_PRESSED}, _Redraw) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?CTRL_BITS =/= 0 ->
	    %% Make sure that no menu pop ups.
	    keep;
	Mod -> next
    end;
nendo(#mousebutton{button=3,state=?SDL_RELEASED}=Mb, Redraw) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?CTRL_BITS =/= 0 ->
	    nendo(Mb#mousebutton{button=2}, Redraw);
	Mod -> next
    end;
nendo(#mousebutton{button=2,x=X,y=Y,state=?SDL_RELEASED}, Redraw) ->
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    wings_io:grab(),
    wings_io:clear_message(),
    Help = "Click " ++ [lmb] ++ " to exit camera mode  Move mouse to tumble  Drag " ++
	[mmb] ++ " or " ++ [rmb] ++ " to dolly  Use arrows to track",
    wings_io:message(Help),
    {seq,{push,dummy},get_nendo_event(Camera, Redraw)};
nendo(#keyboard{keysym=#keysym{sym=Sym}}, _Redraw) ->
    nendo_pan(Sym);
nendo(_, _) -> next.

nendo_event(#mousebutton{button=1,state=?SDL_RELEASED}, Camera, _Redraw) ->
    stop_camera(Camera);
nendo_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 6 of
	0 ->					%None of MMB/RMB pressed.
	    rotate(-Dx, -Dy);
	_Other ->				%MMB and/or RMB pressed.
	    zoom(Dy)
    end,
    get_nendo_event(Camera, Redraw);
nendo_event(#keyboard{keysym=#keysym{sym=Sym}}=Event, _Camera, Redraw) ->
    case nendo_pan(Sym) of
	keep -> keep;
	next ->
	    case wings_hotkey:event(Event) of
		{view,smooth_preview} -> ok;
		{view,Cmd} ->
		    wings_view:command(Cmd, get_st(Redraw));
		_Other -> ok
	    end
    end,
    keep;
nendo_event(Event, Camera, Redraw) ->
    generic_event(Event, Camera, Redraw).
    
nendo_pan(?SDLK_LEFT) ->
    nendo_pan(0.1, 0.0);
nendo_pan(?SDLK_RIGHT) ->
    nendo_pan(-0.1, 0.0);
nendo_pan(?SDLK_UP) ->
    nendo_pan(0.0, 0.1);
nendo_pan(?SDLK_DOWN) ->
    nendo_pan(0.0, -0.1);
nendo_pan(_) -> next.

nendo_pan(Dx, Dy) ->
    pan(Dx, Dy),
    wings_wm:dirty(),
    keep.
    
get_nendo_event(Camera, Redraw) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> nendo_event(Ev, Camera, Redraw) end}.

%%%
%%% 3ds max style camera.
%%%

tds(#mousebutton{button=2,x=X,y=Y,state=?SDL_PRESSED}, Redraw) ->
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    wings_io:grab(),
    wings_io:clear_message(),
    wings_io:message(help()),
    {seq,{push,dummy},get_tds_event(Camera, Redraw)};
tds(_, _) -> next.

tds_event(#mousebutton{button=1,state=?SDL_RELEASED}=Mb, Camera, Redraw) ->
    tds_event(Mb#mousebutton{button=2}, Camera, Redraw);
tds_event(#mousebutton{button=2,state=?SDL_RELEASED}, Camera, _Redraw) ->
    stop_camera(Camera);
tds_event(#mousemotion{x=X,y=Y}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case sdl_keyboard:getModState() of
	Mod when Mod band ?CTRL_BITS =/= 0, Mod band ?ALT_BITS =/= 0 ->
	    zoom(Dy);
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    rotate(Dx, Dy);
	_Other ->
	    pan(Dx/10, Dy/10)
    end,
    get_tds_event(Camera, Redraw);
tds_event(Event, Camera, Redraw) ->
    generic_event(Event, Camera, Redraw).

get_tds_event(Camera, Redraw) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> tds_event(Ev, Camera, Redraw) end}.

%%%
%%% Maya style camera.
%%%

maya(#mousebutton{x=X,y=Y,state=?SDL_PRESSED}, Redraw) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    sdl_events:eventState(?SDL_KEYUP, ?SDL_ENABLE),
	    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
	    wings_io:grab(),
	    wings_io:clear_message(),
	    wings_io:message(help()),
	    {seq,{push,dummy},get_maya_event(Camera, Redraw)};
	Mod -> next
    end;
maya(_, _) -> next.

maya_event(#keyboard{keysym=#keysym{sym=?SDLK_LALT},state=?SDL_RELEASED},
	   Camera, _Redraw) ->
    maya_stop_camera(Camera);
maya_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    if
	Buttons band 4 == 4 ->			%RMB
	    zoom(-Dx);
	Buttons band 3 == 3 ->			%LMB+MMB
	    zoom(-Dx);
	Buttons band 1 == 1 ->			%LMB
	    rotate(Dx, Dy);
	Buttons band 2 == 2 ->			%MMB
	    pan(Dx/10, Dy/10);
	true -> ok
    end,
    get_maya_event(Camera, Redraw);
maya_event(Event, Camera, Redraw) ->
    generic_event(Event, Camera, Redraw).

get_maya_event(Camera, Redraw) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> maya_event(Ev, Camera, Redraw) end}.

maya_stop_camera(Camera) ->
    sdl_events:eventState(?SDL_KEYUP, ?SDL_IGNORE),
    stop_camera(Camera).
    
%%%
%%% Common utilities.
%%%		     

generic_event(redraw, _Camera, #st{}=St) ->
    wings:redraw(St),
    keep;
generic_event(redraw, _Camera, Redraw) when is_function(Redraw) ->
    Redraw(),
    keep;
generic_event(#mousebutton{button=4,state=?SDL_RELEASED}, _Camera, _Redraw) ->
    zoom_step(-1);
generic_event(#mousebutton{button=5,state=?SDL_RELEASED}, _Camera, _Redraw) ->
    zoom_step(1);
generic_event(_, _, _) -> keep.

get_st(#st{}=St) -> St;
get_st(Redraw) when is_function(Redraw) ->
    #st{}.

rotate(Dx, Dy) ->
    #view{azimuth=Az0,elevation=El0} = View = wings_view:current(),
    Az = Az0 + Dx,
    El = El0 + Dy,
    wings_view:set_current(View#view{azimuth=Az,elevation=El,along_axis=none}).

zoom_step(Dir) ->
    case wings_pref:get_value(wheel_zooms, true) of
	false -> keep;
	true ->
	    wings_wm:dirty(),
	    #view{distance=Dist} = View = wings_view:current(),
	    ZoomPercent = wings_pref:get_value(wheel_zoom_factor, ?ZOOM_FACTOR)/100,
	    Delta = Dir*Dist*ZoomPercent,
	    wings_view:set_current(View#view{distance=Dist+Delta}),
	    keep
    end.

zoom(Delta0) ->
    #view{distance=Dist} = View = wings_view:current(),
    Delta = Delta0/10,
    wings_view:set_current(View#view{distance=Dist+Delta}).

pan(Dx, Dy) ->
    #view{pan_x=PanX0,pan_y=PanY0} = View = wings_view:current(),
    PanX = PanX0 + Dx,
    PanY = PanY0 - Dy,
    wings_view:set_current(View#view{pan_x=PanX,pan_y=PanY}).
    
stop_camera(#camera{ox=OX,oy=OY}) ->
    wings_io:clear_message(),
    wings_io:putback_event(redraw),
    case wings_io:ungrab() of
	still_grabbed ->
	    sdl_mouse:warpMouse(OX, OY),
	    wings_io:putback_event(view_changed),
	    pop;
	no_grab -> pop
    end.

-define(CAMDIV, 4).

camera_mouse_range(X0, Y0, #camera{x=OX,y=OY, xt=Xt0, yt=Yt0}=Camera) ->
    %%io:format("Camera Mouse Range ~p ~p~n", [{X0,Y0}, {OX,OY,Xt0,Yt0}]),
    XD0 = (X0 - OX),
    YD0 = (Y0 - OY),
    case {XD0,YD0} of
	{0,0} ->
	    {float(0), float(0), Camera#camera{xt=0,yt=0}};
	_ ->
	    XD = XD0 + Xt0,
	    YD = YD0 + Yt0,
	    wings_io:warp(OX, OY),
	    {XD/?CAMDIV, YD/?CAMDIV, Camera#camera{xt=XD0, yt=YD0}}
    end.
