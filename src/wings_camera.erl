%%
%%  wings_camera.erl --
%%
%%     This module handles camera moves (rotation, zooming, and panning).
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_camera.erl,v 1.76 2003/06/20 16:14:53 bjorng Exp $
%%

-module(wings_camera).
-export([init/0,sub_menu/1,command/2,help/0,event/2]).
-export([desc/1,button_names/0,free_rmb_modifier/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,map/2,foldl/3,sort/1,reverse/1,append/1]).

-define(ZOOM_FACTOR, 20).
-define(CAMDIV, 4).
-define(CAMMAX, 150).  %% Always larger than 300 on my pc

-record(camera,
	{x,y,					%Current mouse position.
	 ox,oy,					%Original mouse position.
	 xt=0,yt=0				%Last warp length.
	}).

init() ->
    wings_pref:set_default(camera_mode, mirai),
    wings_pref:set_default(num_buttons, 3),
    wings_pref:set_default(pan_speed, 25),
    case {wings_pref:get_value(num_buttons),wings_pref:get_value(camera_mode)} of
	{3,_} -> ok;
	{_,nendo} -> ok;
	{_,blender} -> ok;
	{_,_} -> wings_pref:set_value(camera_mode, nendo)
    end.
    
sub_menu(_St) ->
    [{"Camera Mode...",camera_mode}].

command(camera_mode, _St) ->
    Active = wings_wm:this(),
    ZoomFlag0 = wings_pref:get_value(wheel_zooms, true),
    ZoomFactor0 = wings_pref:get_value(wheel_zoom_factor, ?ZOOM_FACTOR),
    PanSpeed0 = wings_pref:get_value(pan_speed),
    View0 = wings_wm:get_prop(Active, current_view),
    #view{fov=Fov0,hither=Hither0,yon=Yon0} = View0,
    Qs = [{vframe,
	   [{menu,[{"One",1},{"Two",2},{"Three",3}],
	     wings_pref:get_value(num_buttons)}],
	   [{title,"Mouse Buttons"}]},
	  {vframe,camera_modes(),[{title,"Camera Mode"}]},
	  {vframe,
	   [{hframe,[{slider,{text,PanSpeed0,
			      [{range,{1,50}}]}}]}],
	   [{title,"Pan Speed"}]},
	  {vframe,
	   [{"Wheel Zooms",ZoomFlag0},
	    {hframe,[{label,"Zoom Factor"},
		     {text,ZoomFactor0,[{range,{1,50}}]},{label,"%"}]}],
	   [{title,"Scroll Wheel"}]},
	  {vframe,
	   [{label_column,
	     [{"Field of View",{text,Fov0,[{range,1.0,180.0}]}},
	      {"Near Clipping Plane",{text,Hither0,
				      [{range,0.001,1000.0}]}},
	      {"Far Clipping Plane",{text,Yon0,
				     [{range,100.0,9.9e307}]}}]}],
	   [{title,"Camera Parameters"}]}],
    wings_ask:dialog("Camera Settings", Qs,
		     fun([Buttons,Mode,PanSpeed,ZoomFlag,ZoomFactor,
			  Fov,Hither,Yon]) ->
			     validate(Buttons, Mode),
			     wings_pref:set_value(camera_mode, Mode),
			     wings_pref:set_value(num_buttons, Buttons),
			     wings_pref:set_value(pan_speed, PanSpeed),
			     wings_pref:set_value(wheel_zooms, ZoomFlag),
			     wings_pref:set_value(wheel_zoom_factor, ZoomFactor),
			     View = View0#view{fov=Fov,hither=Hither,yon=Yon},
			     wings_wm:set_prop(Active, current_view, View),
			     wings_wm:translation_change(),
			     ignore
		     end).

validate(3, _) -> ok;
validate(2, nendo) -> ok;
validate(2, blender) -> ok;
validate(2, Mode) -> again(Mode, 3);
validate(1, nendo) -> ok;
validate(1, blender) -> again(blender, 2);
validate(1, Mode) -> again(Mode, 3).

again(Mode, Buttons) ->
    wings_util:error("The " ++ desc(Mode) ++ " camera mode requires at least " ++
		     integer_to_list(Buttons) ++ " buttons.").
      
camera_modes() ->
    Modes = [mirai,nendo,maya,tds,blender,mb],
    [{menu,[{desc(Mode),Mode} || Mode <- Modes],wings_pref:get_value(camera_mode)}].

desc(blender) -> "Blender";
desc(nendo) -> "Nendo";
desc(mirai) -> "Mirai";
desc(tds) -> "3ds max";
desc(maya) -> "Maya";
desc(mb) -> "Motionbuilder".

help() ->
    case wings_pref:get_value(camera_mode) of
	blender ->
	    case wings_pref:get_value(num_buttons) of
		3 -> "[M] Tumble  [Shift]+[M] Track  [Ctrl]+[M] Dolly";
		_ -> "[Alt]+[L] Tumble  [Alt]+[Shift]+[M] Track  "
			 "[Alt]+[Ctrl]+[M] Dolly"
	    end;
	nendo ->
	    case wings_pref:get_value(num_buttons) of
		1 -> "[Alt]+[L]";
		2 -> "[Ctrl]+[R]";
		3 -> "[M]"
	    end ++ " Start camera";
	mirai ->
	    "[M] Start camera";
	tds ->
	    "[Alt]+[M] Tumble  [M] Track  [Ctrl]+[Alt]+[M] Dolly";
	maya ->
	    "[Alt]+[L] Tumble  [Alt]+[M] Track  [Alt]+[R] Dolly";
	mb ->
	    "[Shift]+[Ctrl]+[L] Tumble  [Shift]+[L] Track  [Ctrl]+[L] Dolly"
    end.

button_names() ->
    case wings_pref:get_value(num_buttons) of
	3 -> {"[L]","[M]","[R]"};
	2 ->
	    case wings_pref:get_value(camera_mode) of
		blender -> {"[L]","[Alt]+[L]","[R]"};
		nendo -> {"[L]","[Ctrl]+[R]","[R]"}
	    end;
	1 -> {"[L]","[Alt]+[L]","[Ctrl]+[L]"}
    end.

free_rmb_modifier() ->
    case wings_pref:get_value(camera_mode) of
	maya -> ?CTRL_BITS;
	_ -> ?ALT_BITS
    end.
						   
%% Event handler.

event(#mousebutton{button=4,state=?SDL_RELEASED}, _Redraw) ->
    zoom_step(-1);
event(#mousebutton{button=5,state=?SDL_RELEASED}, _Redraw) ->
    zoom_step(1);
event(#mousebutton{button=B}, _Redraw) when B==4; B==5 ->
    keep;
event(Ev, Redraw) ->
    case wings_pref:get_value(camera_mode) of
	blender -> blender(Ev, Redraw);
	nendo -> nendo(Ev, Redraw);
	mirai -> mirai(Ev, Redraw);
	tds -> tds(Ev, Redraw);
	maya -> maya(Ev, Redraw);
	mb -> mb(Ev, Redraw)
    end.

%%%
%%% Blender style camera.
%%%

blender(#mousebutton{button=2,state=?SDL_PRESSED,x=X0,y=Y0,mod=Mod}, Redraw)
  when Mod band ?ALT_BITS =:= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    message(help()),
    {seq,push,get_blender_event(Camera, Redraw)};
blender(_, _) -> next.

blender_event(#mousebutton{button=2,state=?SDL_RELEASED}, Camera, _Redraw) ->
    stop_camera(Camera);
blender_event(#mousemotion{x=X,y=Y,mod=Mod}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Mod of
	Mod when Mod band ?SHIFT_BITS =/= 0 ->
	    pan(Dx, Dy);
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

nendo(#mousebutton{button=2,x=X0,y=Y0,mod=Mod,state=?SDL_RELEASED}, Redraw)
  when Mod band ?CTRL_BITS =:= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    nendo_message(true),
    {seq,push,get_nendo_event(Camera, Redraw, true)};
nendo(#keyboard{sym=Sym}, _Redraw) ->
    nendo_pan(Sym);
nendo(_, _) -> next.

nendo_event(#mousebutton{button=1,state=?SDL_RELEASED}, Camera, _, _) ->
    stop_camera(Camera);
nendo_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw, true) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 6 of
	0 ->					%None of MMB/RMB pressed.
	    rotate(-Dx, -Dy);
	_Other ->				%MMB and/or RMB pressed.
	    zoom(Dy)
    end,
    get_nendo_event(Camera, Redraw, true);
nendo_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw, false) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 6 of
	0 ->					%None of MMB/RMB pressed.
	    pan(-Dx, -Dy);
	_Other ->				%MMB and/or RMB pressed.
	    zoom(Dy)
    end,
    get_nendo_event(Camera, Redraw, false);
nendo_event(#keyboard{unicode=$q}, Camera, Redraw, MR0) ->
    MR = not MR0,
    nendo_message(MR),
    get_nendo_event(Camera, Redraw, MR);
nendo_event(#keyboard{sym=Sym}=Event, _Camera, Redraw, _) ->
    case nendo_pan(Sym) of
	keep -> keep;
	next -> view_hotkey(Event, Redraw)
    end,
    keep;
nendo_event(Event, Camera, Redraw, _) ->
    generic_event(Event, Camera, Redraw).
    
nendo_pan(?SDLK_LEFT) ->
    nendo_pan(0.5, 0.0);
nendo_pan(?SDLK_RIGHT) ->
    nendo_pan(-0.5, 0.0);
nendo_pan(?SDLK_UP) ->
    nendo_pan(0.0, 0.5);
nendo_pan(?SDLK_DOWN) ->
    nendo_pan(0.0, -0.5);
nendo_pan(_) -> next.

nendo_pan(Dx, Dy) ->
    pan(Dx, Dy),
    wings_wm:dirty(),
    keep.
    
get_nendo_event(Camera, Redraw, MouseRotates) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> nendo_event(Ev, Camera, Redraw, MouseRotates) end}.

nendo_message(true) ->
    Mbutton = nendo_mbutton(),
    Help = ["[L] Accept  Move mouse to tumble  "
	    "Drag ",Mbutton," to dolly  [Q] Move move to track"],
    message(Help);
nendo_message(false) ->
    Mbutton = nendo_mbutton(),
    Help = ["[L] Accept  Move mouse to track  "
	    "Drag ",Mbutton," Dolly  [Q] Move mouse to rotate"],
    message(Help).

nendo_mbutton() ->
    case wings_pref:get_value(num_buttons) of
	1 -> "[Alt]+[L]";
	2 -> "[R]";
	3 -> "[M]"
    end.

%%%
%%% Mirai style camera.
%%%

mirai(#mousebutton{button=2,x=X0,y=Y0,mod=Mod,state=?SDL_RELEASED}, Redraw)
  when Mod band ?CTRL_BITS =:= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    mirai_message(true),
    View = wings_view:current(),
    {seq,push,get_mirai_event(Camera, Redraw, true, View)};
mirai(#keyboard{sym=Sym}, _Redraw) ->
    mirai_pan(Sym);
mirai(_, _) -> next.

mirai_event(#mousebutton{button=1,state=?SDL_RELEASED}, Camera, _, _, _) ->
    stop_camera(Camera);
mirai_event(#mousebutton{button=3,state=?SDL_RELEASED}, Camera, _, _, View) ->
    wings_view:set_current(View),
    stop_camera(Camera);
mirai_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw, true, View) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 2 of
	0 ->					%MMB not pressed.
	    rotate(-Dx, -Dy);
	_Other ->				%MMB pressed.
	    zoom(Dy)
    end,
    get_mirai_event(Camera, Redraw, true, View);
mirai_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw, false, View) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 2 of
	0 ->					%MMB pressed.
	    pan(-Dx, -Dy);
	_Other ->				%MMB pressed.
	    zoom(Dy)
    end,
    get_mirai_event(Camera, Redraw, false, View);
mirai_event(#keyboard{unicode=$q}, Camera, Redraw, MR0, View) ->
    MR = not MR0,
    mirai_message(MR),
    get_mirai_event(Camera, Redraw, MR, View);
mirai_event(#keyboard{sym=Sym}=Event, _Camera, Redraw, _, _) ->
    case mirai_pan(Sym) of
	keep -> keep;
	next -> view_hotkey(Event, Redraw)
    end,
    keep;
mirai_event(Event, Camera, Redraw, _, _) ->
    generic_event(Event, Camera, Redraw).
    
mirai_pan(?SDLK_LEFT) ->
    mirai_pan(0.5, 0.0);
mirai_pan(?SDLK_RIGHT) ->
    mirai_pan(-0.5, 0.0);
mirai_pan(?SDLK_UP) ->
    mirai_pan(0.0, 0.5);
mirai_pan(?SDLK_DOWN) ->
    mirai_pan(0.0, -0.5);
mirai_pan(_) -> next.

mirai_pan(Dx, Dy) ->
    pan(Dx, Dy),
    wings_wm:dirty(),
    keep.
    
get_mirai_event(Camera, Redraw, MouseRotates, View) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> mirai_event(Ev, Camera, Redraw, MouseRotates, View) end}.

mirai_message(true) ->
    Help = ["[L] Accept  [R] Cancel/restore view  Move mouse to tumble  "
	    "Drag [M] to dolly  [Q] Move mouse to track"],
    message(Help);
mirai_message(false) ->
    Help = ["[L] Accept  [R] Cancel/restore view  Move mouse to track  "
	    "Drag [M] Dolly  [Q] Move mouse to rotate"],
    message(Help).

%%%
%%% 3ds max style camera.
%%%

tds(#mousebutton{button=2,x=X0,y=Y0,state=?SDL_PRESSED}, Redraw) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    message(["[R] Restore view  "|help()]),
    View = wings_view:current(),
    {seq,push,get_tds_event(Camera, Redraw, View)};
tds(_, _) -> next.

tds_event(#mousebutton{button=1,state=?SDL_RELEASED}=Mb, Camera, Redraw, View) ->
    tds_event(Mb#mousebutton{button=2}, Camera, Redraw, View);
tds_event(#mousebutton{button=2,state=?SDL_RELEASED}, Camera, _, _) ->
    stop_camera(Camera);
tds_event(#mousebutton{button=3,state=?SDL_RELEASED}, Camera, _, View) ->
    wings_view:set_current(View),
    stop_camera(Camera);
tds_event(#mousemotion{x=X,y=Y,mod=Mod}, Camera0, Redraw, View) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    if
	Mod band ?CTRL_BITS =/= 0, Mod band ?ALT_BITS =/= 0 ->
	    zoom(Dy);
	Mod band ?ALT_BITS =/= 0 ->
	    rotate(Dx, Dy);
	true ->
	    pan(Dx, Dy)
    end,
    get_tds_event(Camera, Redraw, View);
tds_event(Event, Camera, Redraw, _) ->
    generic_event(Event, Camera, Redraw).

get_tds_event(Camera, Redraw, View) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> tds_event(Ev, Camera, Redraw, View) end}.

%%%
%%% Maya style camera.
%%%

maya(#mousebutton{x=X0,y=Y0,mod=Mod,state=?SDL_PRESSED}, Redraw)
  when Mod band ?ALT_BITS =/= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    sdl_events:eventState(?SDL_KEYUP, ?SDL_ENABLE),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    message(help()),
    {seq,push,get_maya_event(Camera, Redraw)};
maya(_, _) -> next.

maya_event(#keyboard{sym=Alt,state=?SDL_RELEASED},
	   Camera, _Redraw) when Alt == ?SDLK_LALT; Alt == ?SDLK_RALT ->
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
	    pan(Dx, Dy);
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
%%% Motionbuilder style camera.
%%%

mb(#mousebutton{button=1,mod=Mod,x=X0,y=Y0,state=?SDL_PRESSED}, Redraw)
  when Mod band (?SHIFT_BITS bor ?CTRL_BITS) =/= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    message(help()),
    {seq,push,get_mb_event(Camera, Redraw)};
mb(_, _) -> next.

mb_event(#mousebutton{button=1,state=?SDL_RELEASED}, Camera, _) ->
    stop_camera(Camera);
mb_event(#mousemotion{x=X,y=Y,mod=Mod}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    if
	Mod band ?CTRL_BITS =/= 0, Mod band ?SHIFT_BITS =/= 0 ->
	    rotate(Dx, Dy),
	    get_mb_event(Camera, Redraw);
	Mod band ?CTRL_BITS =/= 0 ->
	    zoom(Dy),
	    get_mb_event(Camera, Redraw);
	Mod band ?SHIFT_BITS =/= 0 ->
	    pan(Dx, Dy),
	    get_mb_event(Camera, Redraw);
	true ->
	    stop_camera(Camera)
    end;
mb_event(Event, Camera, Redraw) ->
    generic_event(Event, Camera, Redraw).

get_mb_event(Camera, Redraw) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> mb_event(Ev, Camera, Redraw) end}.
    
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
    #st{shapes=gb_trees:empty()}.

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
    Delta = Dist*Delta0/80,
    wings_view:set_current(View#view{distance=Dist+Delta}).

pan(Dx0, Dy0) ->
    #view{pan_x=PanX0,pan_y=PanY0,distance=D} = View = wings_view:current(),
    S = D*(1/8)/(51-wings_pref:get_value(pan_speed)),
    Dx = Dx0*S,
    Dy = Dy0*S,
    PanX = PanX0 + Dx,
    PanY = PanY0 - Dy,
    wings_view:set_current(View#view{pan_x=PanX,pan_y=PanY}).
    
stop_camera(#camera{ox=Ox,oy=Oy}) ->
    case wings_io:ungrab(Ox, Oy) of
	still_grabbed ->
	    wings_wm:later(view_changed);
	no_grab ->
	    wings_wm:release_focus(),
	    wings_wm:dirty()
    end,
    case wings_pref:get_value(hide_sel_in_camera_moves) of
	false ->
	    ok;
	true ->
	    wings_draw_util:map(fun show_sel_fun/2, [])
    end,
    pop.

camera_mouse_range(X0, Y0, #camera{x=OX,y=OY, xt=Xt0, yt=Yt0}=Camera) ->
%%    io:format("Camera Mouse Range ~p ~p~n", [{X0,Y0}, {OX,OY,Xt0,Yt0}]),
    {X1,Y1} = wings_wm:local2global(X0, Y0),
    XD0 = (X1 - OX),
    YD0 = (Y1 - OY),
    XD = XD0 + Xt0,
    YD = YD0 + Yt0,

    if (XD0 == 0), (YD0 == 0) ->
	    {float(0), float(0), Camera#camera{xt=0,yt=0}};
       %% Linux gets really large jumps sometime, 
       %% so we throw events with large delta movements.
       (XD > ?CAMMAX); (YD > ?CAMMAX) -> 
	    wings_io:warp(OX, OY),
	    {0.0, 0.0, Camera#camera{xt=XD0, yt=YD0}};
       true ->
	    wings_io:warp(OX, OY),
	    {XD/?CAMDIV, YD/?CAMDIV, Camera#camera{xt=XD0, yt=YD0}}
    end.

view_hotkey(Ev, Redraw) ->
    case wings_hotkey:event(Ev) of
	{view,smooth_preview} -> ok;
	{view,smoothed_preview} -> ok;
	{view,Cmd} -> wings_view:command(Cmd, get_st(Redraw));
	_ -> ok
    end.

message(Message) ->
    wings_wm:message(Message),
    wings_wm:message_right([]).
    
grab() ->
    wings_io:grab(),
    wings_wm:grab_focus(),
    case wings_pref:get_value(hide_sel_in_camera_moves) of
	false ->
	    ok;
	true ->
	    wings_draw_util:map(fun hide_sel_fun/2, [])
    end.

hide_sel_fun(#dlo{sel=Sel}=D, _) ->
    D#dlo{sel={call,none,Sel}}.

show_sel_fun(#dlo{sel={call,none,Sel}}=D, _) ->
    D#dlo{sel=Sel}.
