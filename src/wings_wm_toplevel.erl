%%
%%  wings_wm_toplevel.erl --
%%
%%     Implements toplevel windows.
%%
%%  Copyright (c) 2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_wm_toplevel.erl,v 1.5 2003/01/21 11:02:36 bjorng Exp $
%%

-module(wings_wm_toplevel).

%% Don't call any functions in this module directly. Used the supported
%% API in wings_wm.

-export([toplevel/6,set_knob/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,keysearch/3,sort/1]).

-compile(inline).

toplevel(Name, Title, Pos, Size, Flags, Op) ->
    wings_wm:new(Name, Pos, Size, Op),
    new_controller(Name, Title, Flags).

-record(ctrl,
	{title,					%Title of window.
	 state=idle,				%idle|moving
	 local,
	 prev_focus				%Previous focus holder.
	}).

new_controller(Client, Title, Flags) ->
    TitleBarH = ?LINE_HEIGHT+3,
    {{X,Y},{W0,_}} = wings_wm:win_rect(Client),
    Z = wings_wm:win_z(Client),
    Controller = {controller,Client},
    ctrl_create_windows(reverse(sort(Flags)), Client),
    W = case wings_wm:is_window({vscroller,Client}) of
	    false -> W0;
	    true -> W0 + vscroller_width()
	end,
    Size = {W,TitleBarH},
    Cs = #ctrl{title=Title},
    wings_wm:new(Controller, {X,Y-TitleBarH,Z-0.5}, Size,
		 {seq,push,get_ctrl_event(Cs)}),
    wings_wm:link(Client, Controller),
    ctrl_anchor(Client, Flags, Size, TitleBarH),
    keep.

ctrl_create_windows([vscroller|Flags], Client) ->
    {X,Y} = wings_wm:win_ur(Client),
    Z = wings_wm:win_z(Client),
    Name = vscroller(Client, {X,Y,Z+0.1}),
    wings_wm:link(Client, Name),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([{toolbar,Create}|Flags], Client) ->
    {{X,Y},{W,_}} = wings_wm:win_rect(Client),
    Z = wings_wm:win_z(Client),
    Toolbar = {toolbar,Client},
    Create(Toolbar, {X,Y,Z+0.1}, W),
    {_,H} = wings_wm:win_size(Toolbar),
    wings_wm:update_window(Client, [{dy,H},{dh,-H}]),
    wings_wm:link(Client, Toolbar),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([resizable|Flags], Client) ->
    Name = ctrl_new_resizer(Client),
    wings_wm:link(Client, Name),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([_|Flags], Client) ->
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([], _) -> [].

ctrl_anchor(Controlled, Flags, Size, TitleBarH) ->
    case keysearch(anchor, 1, Flags) of
	false -> ok;
	{value,{anchor,Anchor}} ->
	    ctrl_anchor_1(Anchor, Controlled, Size, TitleBarH)
    end.

ctrl_anchor_1(nw, Client, _, Th) ->
    wings_wm:update_window(Client, [{dy,Th}]);
ctrl_anchor_1(ne, Client, {W,_}, Th) ->
    wings_wm:update_window(Client, [{dx,-W},{dy,Th}]);
ctrl_anchor_1(sw, Client, _, Th) ->
    {_,H} = wings_wm:win_size(Client),
    wings_wm:update_window(Client, [{dy,Th-H}]).

get_ctrl_event(Cs) ->
    {replace,fun(Ev) -> ctrl_event(Ev, Cs) end}.
		     
ctrl_event(redraw, Cs) ->
    ctrl_redraw(Cs);
ctrl_event(#mousebutton{button=1,state=?SDL_PRESSED},
	   #ctrl{state=moving,prev_focus=Focus}=Cs) ->
    wings_wm:grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});
ctrl_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, Cs) ->
    Focus = wings_wm:focus_window(),
    wings_wm:grab_focus(wings_wm:active_window()),
    get_ctrl_event(Cs#ctrl{local={X,Y},state=moving,prev_focus=Focus});
ctrl_event(#mousebutton{button=1,state=?SDL_RELEASED}, #ctrl{prev_focus=Focus}=Cs) ->
    wings_wm:grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});
ctrl_event(#mousebutton{button=2,state=?SDL_RELEASED}, Cs) ->
    ctrl_command({fit,both}, Cs),
    keep;
ctrl_event(#mousemotion{x=X0,y=Y0,state=?SDL_PRESSED},
	   #ctrl{state=moving,local={LocX,LocY}}) ->
    {X1,Y1} = wings_wm:local2global(X0, Y0),
    X = X1 - LocX,
    Y = Y1 - LocY,
    {OldX,OldY} = wings_wm:win_ul(),
    Dx0 = X-OldX,
    Dy0 = Y-OldY,
    {Dx,Dy} = ctrl_constrain_move(Dx0, Dy0),
    {controller,Client} = wings_wm:active_window(),
    wings_wm:update_window(Client, [{dx,Dx},{dy,Dy}]),
    keep;
ctrl_event(#mousemotion{state=?SDL_RELEASED},
	   #ctrl{state=moving,prev_focus=Focus}=Cs) ->
    wings_wm:grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});
ctrl_event(#mousemotion{state=?SDL_RELEASED}, _) ->
    {One,Two,Three} = wings_camera:button_names(),
    wings_wm:message(["Drag ",One," Move  ",Two," Fit  ",Three," Show menu"]),
    keep;
ctrl_event(#mousebutton{}=Ev, _) ->
    case wings_menu:is_popup_event(Ev) of
	{yes,X,Y,_} -> ctrl_menu(X, Y);
	no -> ok
    end,
    keep;
ctrl_event({window_updated,Client}, _) ->
    {{X,Y},{W,_}} = wings_wm:win_rect(Client),
    H = ?LINE_HEIGHT+3,
    Updates0 = case wings_wm:is_window({vscroller,Client}) of
		   false -> [];
		   true -> [{dw,vscroller_width()}]
	       end,
    Toolbar = {toolbar,Client},
    Updates1 = case wings_wm:is_window(Toolbar) andalso
		   not wings_wm:is_hidden(Toolbar) of
		   false ->
		       Updates0;
		   true ->
		       {_,ToolbarH} = wings_wm:win_size(Toolbar),
		       [{dy,-ToolbarH}|Updates0]
	       end,
    Updates = [{pos,{X,Y-H}},{w,W},{h,H}|Updates1],
    Self = {controller,Client},
    wings_wm:update_window(Self, Updates),
    keep;
ctrl_event({action,{titlebar,Action}}, Cs) ->
    ctrl_command(Action, Cs);
ctrl_event(_, _) -> keep.

ctrl_redraw(#ctrl{title=Title}) ->
    TitleBarH = ?LINE_HEIGHT+3,
    wings_io:ortho_setup(),
    {W,_} = wings_wm:win_size(),
    Color = {0.3,0.4,0.3},
    wings_io:border(0, 0, W-0.5, TitleBarH, Color),
    gl:color3f(1, 1, 1),
    wings_io:text_at(10, TitleBarH-5, Title),
    keep.

ctrl_constrain_move(Dx0, Dy0) ->
    {{DeskX,DeskY},{DeskW,DeskH}} = wings_wm:win_rect(desktop),
    {{X0,Y0},{W,_}} = wings_wm:win_rect(),
    Dx = case X0+Dx0-DeskX of
	     X when X < 0 ->
		 DeskX-X0;
	     X when DeskX+DeskW < X+W ->
		 DeskX+DeskW-X0-W;
	     _ ->
		 Dx0
	 end,
    {_,Client} = wings_wm:active_window(),
    {{_,Cy},{_,Ch}} = wings_wm:win_rect(Client),
    Dy = if 
	     Y0+Dy0 < DeskY ->
		 DeskY-Y0;
	     Cy+Ch+Dy0 >= DeskY+DeskH ->
		 DeskY+DeskH-Cy-Ch;
	     true ->
		 Dy0
	 end,
    {Dx,Dy}.

ctrl_menu(X, Y) ->
    Menu = [{"Fit",
	     {fit,
	      [{"Both",both,
		"Let window use all available space by expanding in all directions"},
	       {"Horizontal",horizontal,
		"Let window use all available space by expanding it horizontally"},
	       {"Vertical",vertical,
		"Let window use all available space by expanding it vertically"}
	      ]}},
	    {"Size",size,"Set size numerically"}|ctrl_menu_toolbar()],
    wings_menu:popup_menu(X, Y, titlebar, Menu).

ctrl_menu_toolbar() ->
    {_,Client} = wings_wm:active_window(),
    Toolbar = {toolbar,Client},
    case wings_wm:is_window(Toolbar) of
	false -> [];
	true ->
	    case wings_wm:is_hidden(Toolbar) of
		false ->
		    [{"Hide Toolbar",hide_toolbar}];
		true ->
		    [{"Show Toolbar",show_toolbar}]
	    end
    end.

ctrl_command(hide_toolbar, _) ->
    wings_wm:dirty(),
    {_,Client} = wings_wm:active_window(),
    Toolbar = {toolbar,Client},
    wings_wm:hide(Toolbar),
    {_,H} = wings_wm:win_size(Toolbar),
    wings_wm:update_window(Client, [{dy,-H},{dh,H}]),
    keep;
ctrl_command(show_toolbar, _) ->
    wings_wm:dirty(),
    {_,Client} = wings_wm:active_window(),
    Toolbar = {toolbar,Client},
    wings_wm:show({toolbar,Client}),
    {_,H} = wings_wm:win_size(Toolbar),
    wings_wm:update_window(Client, [{dy,H},{dh,-H}]),
    keep;
ctrl_command({fit,Fit}, _) ->
    ctrl_fit(Fit),
    keep;
ctrl_command(size, _) ->
    {_,Client} = wings_wm:active_window(),
    {W0,H0} = wings_wm:win_size(Client),
    Qs = [{"Width",W0},
	  {"Height",H0}],
    wings_ask:ask("Set Window Size", Qs,
		  fun([W,H]) ->
			  wings_wm:update_window(Client, [{w,W},{h,H}]),
			  ignore
		  end).

ctrl_fit(both) ->
    fit_horizontal(),
    fit_vertical();
ctrl_fit(horizontal) ->
    fit_horizontal();
ctrl_fit(vertical) ->
    fit_vertical().

fit_horizontal() ->
    {_,Client} = wings_wm:active_window(),
    {_,Ch} = wings_wm:win_size(Client),
    {{Left0,Y},{_,H}} = wings_wm:win_rect(),
    Win0 = wings_wm:windows(),
    Win = [Wi || Wi <- Win0, have_vertical_overlap(Wi, Y, Ch+H)],
    {DeskLeft,_} = wings_wm:win_ul(desktop),
    Left = fit_hor_constrain_1(Win, Left0, DeskLeft),
    wings_wm:update_window(Client, [{dx,Left-Left0},{dw,Left0-Left}]),
    {Right0,_} = wings_wm:win_ur(wings_wm:active_window()),
    {DeskRight,_} = wings_wm:win_ur(desktop),
    Right = fit_hor_constrain_2(Win, Right0, DeskRight),
    wings_wm:update_window(Client, [{dw,Right-Right0}]).

fit_hor_constrain_1([N|Ns], Left, Leftmost) ->
    case wings_wm:win_ur(N) of
	{RightEdge,_} when Leftmost < RightEdge, RightEdge =< Left ->
	    fit_hor_constrain_1(Ns, Left, RightEdge);
	_ ->
	    fit_hor_constrain_1(Ns, Left, Leftmost)
    end;
fit_hor_constrain_1([], _, Leftmost) -> Leftmost.

fit_hor_constrain_2([N|Ns], Right, Rightmost) ->
    case wings_wm:win_ul(N) of
	{LeftEdge,_} when LeftEdge >= Right, LeftEdge < Rightmost ->
	    fit_hor_constrain_2(Ns, Right, LeftEdge);
	_ ->
	    fit_hor_constrain_2(Ns, Right, Rightmost)
    end;
fit_hor_constrain_2([], _, Rightmost) -> Rightmost.
    
have_vertical_overlap(Name, Y, H) ->
    {{_,Oy},{_,Oh}} = wings_wm:win_rect(Name),
    (Oy =< Y andalso Y < Oy+Oh) orelse (Y =< Oy andalso Oy < Y+H).
    
fit_vertical() ->
    {{X,Y0},{W,_}} = wings_wm:win_rect(),
    {_,DeskY} = wings_wm:win_ul(desktop),
    Win0 = wings_wm:windows(),
    Win = [Wi || Wi <- Win0, have_horizontal_overlap(Wi, X, W)],
    Y = fit_vert_constrain_1(Win, Y0, DeskY),
    {_,Client} = wings_wm:active_window(),
    wings_wm:update_window(Client, [{dy,Y-Y0},{dh,Y0-Y}]),
    {_,BotY0} = wings_wm:win_ll(Client),
    {_,DeskBot} = wings_wm:win_ll(desktop),
    BotY = fit_vert_constrain_2(Win, BotY0, DeskBot),
    wings_wm:update_window(Client, [{dh,BotY-BotY0}]).

fit_vert_constrain_1([Name|T], LowestY, HighestY) ->
    case wings_wm:win_ll(Name) of
	{_,LowerEdge} when LowerEdge > HighestY, LowerEdge =< LowestY ->
	    fit_vert_constrain_1(T, LowestY, LowerEdge);
	_ ->
	    fit_vert_constrain_1(T, LowestY, HighestY)
    end;
fit_vert_constrain_1([], _, HighestY) -> HighestY.

fit_vert_constrain_2([Name|T], LowestY, BottomY) ->
    case wings_wm:win_ul(Name) of
	{_,UpperEdge} when BottomY < UpperEdge, UpperEdge =< LowestY ->
	    fit_vert_constrain_2(T, LowestY, UpperEdge);
	_ ->
	    fit_vert_constrain_2(T, LowestY, BottomY)
    end;
fit_vert_constrain_2([], _, BottomY) -> BottomY.

have_horizontal_overlap(Name, X, W) ->
    {{Ox,_},{Ow,_}} = wings_wm:win_rect(Name),
    (Ox =< X andalso X < Ox+Ow) orelse (X =< Ox andalso Ox < X+W).

%%%
%%% Resizer window.
%%%

-record(rsz,
	{state=idle,				%idle|moving
	 aspect=none,				%Aspect ratio to preserve.
	 local,
	 prev_focus				%Previous focus holder.
	}).

ctrl_new_resizer(Client) ->
    Name = {resizer,Client},
    Rst = #rsz{},
    Pos = resize_pos(Client),
    wings_wm:new(Name, Pos, {12,12},
		 {seq,push,get_resize_event(Rst)}),
    Name.

get_resize_event(Rst) ->
    {replace,fun(Ev) -> resize_event(Ev, Rst) end}.

resize_event(redraw, _) ->
    wings_io:ortho_setup(),
    wings_wm:draw_resizer(0, 0),
    wings_util:button_message("Resize", "Resize, keeping current aspect ratio"),
    keep;
resize_event(#mousebutton{button=1,state=?SDL_PRESSED},
	     #rsz{state=moving,prev_focus=Focus}=Rst) ->
    wings_wm:grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, Rst) ->
    Focus = wings_wm:focus_window(),
    wings_wm:grab_focus(get(wm_active)),
    get_resize_event(Rst#rsz{local={X,Y},state=moving,aspect=none,prev_focus=Focus});
resize_event(#mousebutton{button=2,x=X,y=Y,state=?SDL_PRESSED}, Rst) ->
    {_,Client} = wings_wm:active_window(),
    {W,H} = wings_wm:win_size(Client),
    Focus = wings_wm:focus_window(),
    wings_wm:grab_focus(get(wm_active)),
    get_resize_event(Rst#rsz{local={X,Y},state=moving,aspect=W/H,prev_focus=Focus});
resize_event(#mousebutton{button=1,state=?SDL_RELEASED}, #rsz{prev_focus=Focus}=Rst) ->
    wings_wm:grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(#mousemotion{state=0}, #rsz{state=moving,prev_focus=Focus}=Rst) ->
    wings_wm:grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(#mousemotion{x=X0,y=Y0},
	     #rsz{state=moving,local={LocX,LocY},aspect=Aspect}) ->
    {X1,Y1} = wings_wm:local2global(X0, Y0),
    X = X1 - LocX,
    Y = Y1 - LocY,
    {OldX,OldY} = wings_wm:win_ul(),
    Dx0 = X-OldX,
    Dy0 = Y-OldY,
    {resizer,Client} = wings_wm:active_window(),
    {Dx,Dy} = resize_constrain(Client, Dx0, Dy0, Aspect),
    wings_wm:update_window(Client, [{dw,Dx},{dh,Dy}]),
    keep;
resize_event({window_updated,Client}, _) ->
    Pos = resize_pos(Client),
    wings_wm:move(wings_wm:active_window(), Pos),
    keep;
resize_event(_, _) -> keep.

resize_pos(Client) ->
    {{X,Y},{W,H}} = wings_wm:win_rect(Client),
    Z = wings_wm:win_z(Client),
    case wings_wm:is_window({vscroller,Client}) of
	false ->  {X+W-13,Y+H-13,Z+0.5};
	true -> {X+W,Y+H-13,Z+0.5}
    end.

resize_constrain(Client, Dx0, Dy0, Aspect) ->
    {{DeskX,DeskY},{DeskW,DeskH}} = wings_wm:win_rect(desktop),
    {{X,Y},{W,H}} = wings_wm:win_rect(),
    Dx = if
	     DeskX+DeskW =< X+W+Dx0 ->
		 DeskX+DeskW-X-W;
	     true ->
		 Dx0
	 end,
    Dy = if 
	     DeskY+DeskH =< Y+H+Dy0 ->
		 DeskY+DeskH-Y-H;
	     true ->
		 Dy0
	 end,
    resize_constrain_1(Client, Dx, Dy, Aspect).

resize_constrain_1(_, Dx, Dy, none) -> {Dx,Dy};
resize_constrain_1(Client, Dx, Dy, Aspect) ->
    {W,H} = wings_wm:win_size(Client),
    if
	Dx > Dy ->{round(Aspect*(H+Dy)-W),Dy};
	true -> {Dx,round((W+Dx)/Aspect)-H}
    end.

%%%
%%% A vertical scroller.
%%%

-record(ss,
	{knob_pos,				%Position of knob (0-1).
	 knob_prop,				%Proportion of knob (0-1).
	 track_pos=none
	}).

vscroller(Name0, Pos) ->
    Name = {vscroller,Name0},
    Ss = #ss{knob_pos=0.0,knob_prop=1.0},
    wings_wm:new(Name, Pos, {vscroller_width(),1},
		 {seq,push,get_event(Ss)}),
    Name.

set_knob({vscroller,_}=Name, Pos, Proportion) ->
    wings_wm:send(Name, {set_knob,Pos,Proportion});
set_knob(Name, Pos, Proportion) ->
    set_knob({vscroller,Name}, Pos, Proportion).

vscroller_width() ->
    13.

%%%
%%% Implementation.
%%%

get_event(Ss) ->
    {replace,fun(Ev) -> event(Ev, Ss) end}.

event(redraw, Ss) ->
    redraw(Ss);
event({set_knob,Pos0,Prop0}, #ss{knob_pos=OldPos,knob_prop=OldProp}=Ss) ->
    case {max(0.0, min(1.0, Pos0)),max(0.0, min(1.0, Prop0))} of
	{OldPos,OldProp} -> keep;
	{Pos,Prop} ->
	    wings_wm:dirty(),
	    get_event(Ss#ss{knob_pos=Pos,knob_prop=Prop})
    end;
event(#mousebutton{button=1,y=Y,state=?SDL_PRESSED}, Ss) ->
    down(Y, Ss);
event(#mousebutton{button=1,state=?SDL_RELEASED}, Ss) ->
    wings_wm:release_focus(),
    get_event(Ss#ss{track_pos=none});
event(#mousemotion{y=Y,state=?SDL_PRESSED}, #ss{track_pos=Pos}=Ss)
  when Pos =/= none ->
    drag(Y, Ss);
event({window_updated,Client}, _) ->
    UR = wings_wm:win_ur(Client),
    {_,H} = wings_wm:win_size(Client),
    Updates = case wings_wm:is_window({resizer,Client}) of
		  false -> [{h,H}];
		  true -> [{h,H-13}]
	      end,
    wings_wm:update_window({vscroller,Client}, [{pos,UR}|Updates]),
    keep;
event(_, _) -> keep.

down(Y0, #ss{knob_pos=Pos,knob_prop=Prop}=Ss) ->
    {_,H} = wings_wm:win_size(),
    Y = Y0/H,
    {vscroller,Client} = wings_wm:active_window(),
    if
	Y < Pos ->
	    wings_wm:send(Client, scroll_page_up),
	    keep;
	Y < Pos+Prop ->
	    wings_wm:grab_focus(),
	    get_event(Ss#ss{track_pos=Pos-Y});
	true ->
	    wings_wm:send(Client, scroll_page_down),
	    keep
    end.

drag(Y0, #ss{knob_prop=Prop,track_pos=TrackPos}) ->
    {_,H} = wings_wm:win_size(),
    Y = case Y0/H + TrackPos of
	    Y1 when Y1 < 0 -> 0.0;
	    Y1 when Y1 < 1-Prop -> Y1;
	    _ -> 1-Prop
	end,
    {vscroller,Client} = wings_wm:active_window(),
    wings_wm:send(Client, {set_knob_pos,Y}),
    keep.

redraw(#ss{knob_pos=Pos,knob_prop=Prop}) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0.0, 0.0, W-0.5, H, ?PANE_COLOR),
    gl:color3f(0.2, 0.2, 0.2),
    gl:rectf(2.5, H*Pos, W-4.5, H*(Pos+Prop)),
    keep.

min(A, B) when A < B -> A;
min(_, B) -> B.

max(A, B) when A > B -> A;
max(_, B) -> B.
