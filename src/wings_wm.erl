%%
%%  wings_wm.erl --
%%
%%     Window manager for Wings.
%%
%%  Copyright (c) 2002-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_wm.erl,v 1.56 2003/01/10 19:14:56 bjorng Exp $
%%

-module(wings_wm).
-export([init/0,enter_event_loop/0,dirty/0,clean/0,reinit_opengl/0,
	 new/4,delete/1,toplevel/5,toplevel/6,
	 message/1,message/2,message_right/1,send/2,send_after_redraw/2,
	 menubar/1,menubar/2,get_menubar/1,
	 set_timer/2,cancel_timer/1,
	 active_window/0,offset/3,move/2,move/3,pos/1,windows/0,is_window/1,exists/1,
	 update_window/2,
	 callback/1,current_state/1,
	 grab_focus/0,grab_focus/1,release_focus/0,has_focus/1,focus_window/0,
	 top_size/0,viewport/0,viewport/1,
	 win_size/0,win_ul/0,win_rect/0,win_size/1,win_ul/1,win_rect/1,
	 local2global/1,local2global/2,global2local/2,local_mouse_state/0,
	 translation_change/0,me_modifiers/0,set_me_modifiers/1,
	 draw_message/1,draw_completions/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [map/2,last/1,sort/1,keysort/2,keysearch/3,
		reverse/1,foreach/2,member/2]).
-compile(inline).

-define(BUTTON_WIDTH, 44).
-define(BUTTON_HEIGHT, 32).

-record(win,
	{z,					%Z order.
	 x,y,					%Position.
	 w,h,					%Size.
	 name,					%Name of window.
	 stk					%Event handler stack.
	}).

-record(se,					%Stack entry record.
	{h,					%Handler (fun).
	 msg=[],				%Current message.
	 msg_right=[],				%Right-side message.
	 menubar=none				%Menubar for this window.
	}).

%%%
%%% Process dictionary usage:
%%%
%%% wm_active		Currently active window (handling current event).
%%% wm_main		Last active window that has a menu.
%%% wm_focus		Window name of focus window or undefined.
%%% wm_windows		All windows.
%%% wm_dirty		Exists if redraw is needed.
%%% wm_top_size         Size of top window.
%%% wm_viewport		Current viewport.
%%%
%%% Event loop.
%%%

init() ->
    wings_pref:set_default(window_size, {780,570}),
    {W,H} = TopSize = wings_pref:get_value(window_size),
    put(wm_top_size, TopSize),
    case get(wings_os_type) of
	{unix,sunos} ->
	    set_video_mode(W, H);			%Needed on Solaris/Sparc.
	_ -> ok
    end,
    translation_change(),
    put(wm_windows, gb_trees:empty()),
    new(desktop, {0,0,?Z_DESKTOP}, {0,0}, {push,fun(_) -> keep end}),
    new(message, {0,0,?Z_MESSAGE}, {0,0}, {push,fun message_event/1}),
    ButtonH = ?BUTTON_HEIGHT+4,
    new(buttons, {0,0,?Z_BUTTONS}, {W,ButtonH}, init_button()),
    new(menubar, {0,0,?Z_MENUBAR}, {0,0}, init_menubar()),
    put(wm_main, geom),
    init_opengl(),
    resize_windows(W, H).

message(Message) ->
    wings_io:putback_event({wm,{message,get(wm_active),Message}}).

message_right(Right) ->
    wings_io:putback_event({wm,{message_right,get(wm_active),Right}}).

message(Message, Right) ->
    message(Message),
    message_right(Right).

menubar(Menubar) ->
    menubar(get(wm_active), Menubar).

menubar(Name, Menubar) ->
    wings_io:putback_event({wm,{menubar,Name,Menubar}}).

get_menubar(Name) ->
    #win{stk=[#se{menubar=Bar}|_]} = get_window_data(Name),
    Bar.

send(Name, Ev) ->
    wings_io:putback_event({wm,{send_to,Name,Ev}}),
    keep.

send_after_redraw(Name, Ev) ->
    wings_io:putback_event({wm,{send_after_redraw,Name,Ev}}).

current_state(St) ->
    case put(wm_current_state, St) of
	St -> ok;
	_ ->
	    NewState = {current_state,St},
	    foreach(fun(geom) -> ok;
		       (top) -> ok;
		       (Name) -> send(Name, NewState)
		    end, gb_trees:keys(get(wm_windows)))
    end.

dirty() ->
    put(wm_dirty, dirty).

clean() ->
    erase(wm_dirty).

callback(Cb) ->
    wings_io:putback_event({wm,{callback,Cb}}).
    
new(Name, {X,Y,Z}, {W,H}, Op) when is_integer(X), is_integer(Y),
				   is_integer(W), is_integer(H) ->
    dirty(),
    Stk = handle_response(Op, dummy_event, default_stack(Name)),
    Win = #win{x=X,y=Y,z=Z,w=W,h=H,name=Name,stk=Stk},
    put(wm_windows, gb_trees:insert(Name, Win, get(wm_windows))),
    keep.

delete(Name) ->
    case get(wm_focus) of
	Name -> erase(wm_focus);
	_ -> ok
    end,
    dirty(),
    Windows = delete_windows(Name, get(wm_windows)),
    put(wm_windows, Windows),
    keep.

delete_windows(Name, W0) ->
    W1 = gb_trees:delete_any(Name, W0),
    W2 = gb_trees:delete_any({controller,Name}, W1),
    W = gb_trees:delete_any({resizer,Name}, W2),
    gb_trees:delete_any({vscroller,Name}, W).

active_window() ->
    case get(wm_active) of
	undefined -> none;
	Active -> Active
    end.

windows() ->	    
    gb_trees:keys(get(wm_windows)).

exists(Name) ->
    gb_trees:is_defined(Name, get(wm_windows)).

is_window(Name) ->
    gb_trees:is_defined(Name, get(wm_windows)).

offset(Name, Xoffs, Yoffs) ->
    #win{x=X,y=Y} = Win = get_window_data(Name),
    put_window_data(Name, Win#win{x=X+Xoffs,y=Y+Yoffs}).

move(Name, {X,Y,Z}) ->
    dirty(),
    Win = get_window_data(Name),
    put_window_data(Name, Win#win{x=X,y=Y,z=Z}).

move(Name, {X,Y,Z}, {W,H}) ->
    dirty(),
    Win = get_window_data(Name),
    put_window_data(Name, Win#win{x=X,y=Y,z=Z,w=W,h=H}).

update_windows(Names, Updates) ->
    update_windows_1(Names, Updates, get(wm_windows)).

update_windows_1([N|Ns], Updates, Windows0) ->
    Win0 = gb_trees:get(N, Windows0),
    Win = update_window_1(Updates, Win0),
    Windows = gb_trees:update(N, Win, Windows0),
    update_windows_1(Ns, Updates, Windows);
update_windows_1([], _, Windows) ->
    put(wm_windows, Windows),
    dirty().

update_window(Name, Updates) ->
    send(Name, resized),
    put_window_data(Name, update_window_1(Updates, get_window_data(Name))),
    dirty().

update_window_1([{dx,Dx}|T], #win{x=X}=Win) ->
    update_window_1(T, Win#win{x=X+Dx});
update_window_1([{dy,Dy}|T], #win{y=Y}=Win) ->
    update_window_1(T, Win#win{y=Y+Dy});
update_window_1([{dw,Dw}|T], #win{w=W}=Win) ->
    update_window_1(T, Win#win{w=W+Dw});
update_window_1([{dh,Dh}|T], #win{h=H}=Win) ->
    update_window_1(T, Win#win{h=H+Dh});
update_window_1([{x,X}|T], Win) ->
    update_window_1(T, Win#win{x=X});
update_window_1([{w,W}|T], Win) ->
    update_window_1(T, Win#win{w=W});
update_window_1([{h,H}|T], Win) ->
    update_window_1(T, Win#win{h=H});
update_window_1([], Win) ->
    range_check(Win).

range_check(#win{w=W}=Win) when W < 1 ->
    range_check(Win#win{w=1});
range_check(#win{h=H}=Win) when H < 1 ->
    range_check(Win#win{h=1});
range_check(Win) -> Win.

pos(Name) ->
    #win{x=X,y=Y} = get_window_data(Name),
    {X,Y}.

grab_focus() -> 
    grab_focus(get(wm_active)).
	   
grab_focus(Name) -> 
    case exists(Name) of
	true -> put(wm_focus, Name);
	false -> erase(wm_focus)
    end.

release_focus() -> 
    erase(wm_focus).

has_focus(Name) ->
    get(wm_focus) =:= Name.

focus_window() ->
    get(wm_focus).

top_size() ->
    get(wm_top_size).

set_timer(Time, Event) ->
    Active = get(wm_active),
    wings_io:set_timer(Time, {wm,{send_to,Active,Event}}).

cancel_timer(Ref) ->
    wings_io:cancel_timer(Ref).

viewport() ->
    get(wm_viewport).

viewport(Name) ->
    #win{x=X,y=Y0,w=W,h=H} = get_window_data(Name),
    {_,TopH} = get(wm_top_size),
    Y = TopH-(Y0+H),
    {X,Y,W,H}.

win_size() ->
    win_size(active_window()).

win_ul() ->
    win_ul(active_window()).

win_rect() ->
    win_rect(active_window()).

win_size(Name) ->
    #win{w=W,h=H} = get_window_data(Name),
    {W,H}.

win_ul(Name) ->
    #win{x=X,y=Y} = get_window_data(Name),
    {X,Y}.

win_rect(Name) ->
    #win{x=X,y=Y,w=W,h=H} = get_window_data(Name),
    {{X,Y},{W,H}}.

local2global(#mousebutton{x=X0,y=Y0}=Ev) ->
    {X,Y} = local2global(X0, Y0),
    Ev#mousebutton{x=X,y=Y};
local2global(#mousemotion{x=X0,y=Y0}=Ev) ->
    {X,Y} = local2global(X0, Y0),
    Ev#mousemotion{x=X,y=Y};
local2global(Ev) -> Ev.

local2global(X, Y) ->
    {_,TopH} = get(wm_top_size),
    {Xorig,Yorig,_,H} = viewport(),
    {Xorig+X,(TopH-Yorig-H)+Y}.

global2local(X, Y) ->
    {_,TopH} = get(wm_top_size),
    {Xorig,Yorig,_,H} = viewport(),
    {X-Xorig,Y-(TopH-Yorig-H)}.

local_mouse_state() ->
    {B,X0,Y0} = sdl_mouse:getMouseState(),
    {X,Y} = global2local(X0, Y0),
    {B,X,Y}.

enter_event_loop() ->
    init_opengl(),
    event_loop().

event_loop() ->
    case get(wm_dirty) of
	undefined -> get_and_dispatch();
	_ -> redraw_all()
    end.

get_and_dispatch() ->
    Event = wings_io:get_event(),
    dispatch_event(Event),
    event_loop().

dispatch_matching(Filter) ->
    Evs = wings_io:get_matching_events(Filter),
    foreach(fun dispatch_event/1, Evs).

dispatch_event(#resize{w=W,h=H}) ->
    ?CHECK_ERROR(),
    {SaveW,SaveH} =
	case sdl_video:wm_isMaximized() of
	    false -> {W,H};
	    true ->  {W-8,H-10}
	end,
    wings_pref:set_value(window_size, {SaveW,SaveH}),
    put(wm_top_size, {W,H}),
    init_opengl(),
    resize_windows(W, H),
    dirty();
dispatch_event({wm,WmEvent}) ->
    wm_event(WmEvent);
dispatch_event(Event) ->
    case find_active(Event) of
	none -> ok;
	Active -> do_dispatch(Active, Event)
    end.

resize_windows(W, H) ->    
    Event = #resize{w=W,h=H},

    MenubarData0 = get_window_data(menubar),
    MenubarH = ?CHAR_HEIGHT+6,
    MenubarData1 = MenubarData0#win{x=0,y=0,w=W,h=MenubarH},
    put_window_data(menubar, MenubarData1),
    MenubarData = send_event(MenubarData1, Event#resize{w=W}),
    put_window_data(menubar, MenubarData),

    MsgData0 = get_window_data(message),
    MsgH = ?CHAR_HEIGHT+8,
    MsgY = H-MsgH,
    MsgData1 = MsgData0#win{x=0,y=MsgY,w=W,h=MsgH},
    put_window_data(message, MsgData1),
    MsgData = send_event(MsgData1, Event#resize{w=W}),
    put_window_data(message, MsgData),

    #win{h=ButtonH} = ButtonData0 = get_window_data(buttons),
    ButtonY = MsgY-ButtonH,
    ButtonData1 = ButtonData0#win{x=0,y=ButtonY,w=W},
    put_window_data(buttons, ButtonData1),
    ButtonData = send_event(ButtonData1, Event#resize{w=W}),
    put_window_data(buttons, ButtonData),

    DesktopData0 = get_window_data(desktop),
    DesktopData = DesktopData0#win{x=0,y=MenubarH,w=W,h=H-MsgH-ButtonH-MenubarH},
    put_window_data(desktop, DesktopData),

    case is_window(autouv) of
	false -> ok;
	true ->
	    AutoUVData0 = get_window_data(autouv),
	    AutoUVData = send_event(AutoUVData0, Event),
	    put_window_data(autouv, AutoUVData)
    end.

do_dispatch(Active, Ev) ->
    case gb_trees:lookup(Active, get(wm_windows)) of
	none -> ok;
	{value,Win0} ->
	    case send_event(Win0, Ev) of
		#win{name=Name,stk=delete} ->
		    delete(Name);
		#win{stk=[]} ->
		    ok;
		Win ->
		    put_window_data(Active, Win)
	    end
    end.

redraw_all() ->
    EarlyBC = wings_pref:get_value(early_buffer_clear),
    maybe_clear(late, EarlyBC),			%Clear right before
						%drawing (late).
    Windows = keysort(2, gb_trees:to_list(get(wm_windows))),
    foreach(fun({Name,_}) ->
		    dispatch_matching(fun({wm,{send_to,N,_}}) ->
					      N =:= Name;
					 (_) -> false
				      end),
		    do_dispatch(Name, redraw)
	    end, Windows),
    gl:swapBuffers(),
    maybe_clear(early, EarlyBC),		%Clear immediately after
						%buffer swap (early).
    wings_io:arrow(),
    erase(wm_dirty),
    event_loop().

maybe_clear(early, true) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT);
maybe_clear(late, false) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT);
maybe_clear(_, _) -> ok.

reinit_opengl() ->
    wings_io:putback_event({wm,init_opengl}).

init_opengl() ->
    {W,H} = get(wm_top_size),
    set_video_mode(W, H),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    wings_io:resize(),
    {R,G,B} = wings_pref:get_value(background_color),
    gl:clearColor(R, G, B, 1.0),
    dirty(),
    foreach(fun(Name) ->
		    do_dispatch(Name, init_opengl)
	    end, gb_trees:keys(get(wm_windows))).

send_event(Win, {expose}) ->
    dirty(),
    Win;
send_event(#win{name=Name,x=X,y=Y0,w=W,h=H,stk=[Se|_]=Stk0}, Ev0) ->
    put(wm_active, Name),
    Ev = translate_event(Ev0, X, Y0),
    {_,TopH} = get(wm_top_size),
    Y = TopH-(Y0+H),
    ViewPort = {X,Y,W,H},
    case put(wm_viewport, ViewPort) of
	ViewPort -> ok;
	_ -> gl:viewport(X, Y, W, H)
    end,
    Stk = handle_event(Se, Ev, Stk0),
    Win = get_window_data(Name),
    erase(wm_active),
    Win#win{stk=Stk}.

translate_event(#mousemotion{state=Mask0,x=X,y=Y}=M, Ox, Oy) ->
    Mask = translate_bmask(Mask0),
    M#mousemotion{state=Mask,x=X-Ox,y=Y-Oy};
translate_event(#mousebutton{button=B0,x=X,y=Y,state=?SDL_PRESSED}=M, Ox, Oy) ->
    B = translate_button(B0),
    M#mousebutton{button=B,x=X-Ox,y=Y-Oy};
translate_event(#mousebutton{button=B0,x=X,y=Y,state=?SDL_RELEASED}=M, Ox, Oy) ->
    B = case erase({button_up,B0}) of
	    undefined -> B0;
	    Other -> Other
	end,
    M#mousebutton{button=B,x=X-Ox,y=Y-Oy};
translate_event({drop,{X,Y},DropData}, Ox, Oy) ->
    {drop,{X-Ox,Y-Oy},DropData};
translate_event(Ev, _, _) -> Ev.
    
handle_event(#se{h=Handler}, Event, Stk) ->
    case catch Handler(Event) of
	{'EXIT',normal} ->
	    exit(normal);
	{'EXIT',Reason} ->
	    #se{h=CrashHandler} = last(Stk),
	    handle_response(CrashHandler({crash,Reason}),
			    Event, default_stack(unknown_window));
	Res ->
	    handle_response(Res, Event, Stk)
    end.

handle_response(Res, Event, Stk0) ->
    case Res of
	keep -> Stk0;
	next -> next_handler(Event, Stk0);
	pop -> pop(Stk0);
	delete -> delete;
	push ->
	    [OldTop|_] = Stk0,
	    [OldTop#se{h=dummy}|Stk0];
	{push,Handler} ->
	    [OldTop|_] = Stk0,
	    [OldTop#se{h=Handler}|Stk0];
	{seq,First,Then} ->
	    Stk = handle_response(First, Event, Stk0),
	    handle_response(Then, Event, Stk);
	{replace,Top} when is_function(Top) -> replace_handler(Top, Stk0);
	Top when is_function(Top) -> replace_handler(Top, Stk0)
    end.

pop([_|Stk]) -> Stk.

replace_handler(Handler, [Top|Stk]) -> [Top#se{h=Handler}|Stk].

next_handler(Event, [_|[Next|_]=Stk]) ->
    handle_event(Next, Event, Stk).

default_stack(Name) ->
    Handler = fun(Crash) ->
		      io:format("Window ~p crashed: ~p\n", [Name,Crash]),
		      exit(too_bad)
	      end,
    [#se{h=Handler}].

%%%
%%% Handling Wm Events.
%%%

wm_event({message,Name,Msg}) ->
    case lookup_window_data(Name) of
	none -> ok;
	#win{stk=[#se{msg=Msg}|_]} -> ok;
	#win{stk=[Top|Stk]}=Data0 ->
	    Data = Data0#win{stk=[Top#se{msg=Msg}|Stk]},
	    put_window_data(Name, Data),
	    wings_wm:dirty()
    end;
wm_event({message_right,Name,Right0}) ->
    Right = lists:flatten(Right0),
    case lookup_window_data(Name) of
	none -> ok;
	#win{stk=[#se{msg_right=Right}|_]} -> ok;
	#win{stk=[Top|Stk]}=Data0 ->
	    Data = Data0#win{stk=[Top#se{msg_right=Right}|Stk]},
	    put_window_data(Name, Data),
	    wings_wm:dirty()
    end;
wm_event({menubar,Name,Menubar}) ->
    case lookup_window_data(Name) of
	none -> ok;
	#win{stk=[#se{menubar=Menubar}|_]} -> ok;
	#win{stk=[Top|Stk]}=Data0 ->
	    Data = Data0#win{stk=[Top#se{menubar=Menubar}|Stk]},
	    put_window_data(Name, Data),
	    wings_wm:dirty()
    end;
wm_event({send_to,Name,Ev}) ->
    case gb_trees:is_defined(Name, get(wm_windows)) of
	false -> ok;
	true -> do_dispatch(Name, Ev)
    end;
wm_event({send_after_redraw,Name,Ev}) ->
    case gb_trees:is_defined(Name, get(wm_windows)) of
	false -> ok;
	true -> do_dispatch(Name, Ev)
    end;
wm_event({callback,Cb}) ->
    Cb();
wm_event(init_opengl) ->
    init_opengl().

%%%
%%% Finding the active window.
%%%

find_active(Ev) ->
    case Ev of
	#mousebutton{x=X,y=Y} -> ok;
	#mousemotion{x=X,y=Y} -> ok;
	_ -> {_,X,Y} = sdl_mouse:getMouseState()
    end,
    find_active_1(reverse(sort(gb_trees:values(get(wm_windows)))), X, Y).

find_active_1([#win{x=Wx,y=Wy,w=W,h=H,name=Name}|T], X, Y) ->
    case {X-Wx,Y-Wy} of
	{Rx,Ry} when 0 =< Rx, Rx < W,0 =< Ry, Ry < H ->
	    find_active_2(Name);
	_ -> find_active_1(T, X, Y)
    end;
find_active_1(_, _, _) -> find_active_2(none).

find_active_2({controller,Client}=Name) ->
    case get(wm_focus) of
 	undefined -> Name;
	Client -> Name;
 	Focus -> Focus
    end;
find_active_2(Name) ->
    case get(wm_focus) of
 	undefined -> Name;
 	Focus -> Focus
    end.

%%%
%%% Utility functions.
%%%

lookup_window_data(Name) ->
    case gb_trees:lookup(Name, get(wm_windows)) of
	none -> none;
	{value,Val} -> Val
    end.

get_window_data(Name) ->
    gb_trees:get(Name, get(wm_windows)).

put_window_data(Name, Data) ->
    put(wm_windows, gb_trees:update(Name, Data, get(wm_windows))).

set_video_mode(W, H) ->
    sdl_video:setVideoMode(W, H, 0, ?SDL_OPENGL bor ?SDL_RESIZABLE).

%%%
%%% Button translation.
%%%

me_modifiers() ->
    get(mouse_event_modifiers).

set_me_modifiers(Mod) ->
    put(mouse_event_modifiers, Mod).

translation_change() ->
    case wings_pref:get_value(num_buttons) of
	3 -> erase(mouse_translation);
	Buttons ->
	    Mode = wings_pref:get_value(camera_mode),
	    put(mouse_translation, {Mode,Buttons})
    end.

translate_bmask(0) ->
    put(mouse_event_modifiers, sdl_keyboard:getModState()),
    0;
translate_bmask(Mask) ->
    translate_bmask(Mask, 2#001, 1) bor
	translate_bmask(Mask, 2#010, 2) bor
	translate_bmask(Mask, 2#100, 3).

translate_bmask(Mask, Bit, _) when Mask band Bit == 0 -> 0;
translate_bmask(_Mask, Bit, B0) ->
    case translate_button(B0) of
	B0 -> Bit;
	B -> (1 bsl (B-1))
    end.
    
translate_button(B0) ->
    Type = get(mouse_translation),
    {B,M} = translate_button_1(B0, Type, sdl_keyboard:getModState()),
    put(mouse_event_modifiers, M),
    if
	B =/= B0 -> put({button_up,B0}, B);
	true -> ok
    end,
    B.

translate_button_1(1, {blender,1}, Mod) when Mod band ?CTRL_BITS =/= 0 ->
    {3,Mod band (bnot ?CTRL_BITS)};
translate_button_1(1, {blender,_}, Mod) when Mod band ?ALT_BITS =/= 0 ->
    {2,Mod band (bnot ?ALT_BITS)};
translate_button_1(1, {nendo,1}, Mod) when Mod band ?CTRL_BITS =/= 0 ->
    {3,Mod band (bnot ?CTRL_BITS)};
translate_button_1(1, {nendo,1}, Mod) when Mod band ?ALT_BITS =/= 0 ->
    {2,Mod band (bnot ?ALT_BITS)};
translate_button_1(3, {nendo,2}, Mod) when Mod band ?CTRL_BITS =/= 0 ->
    {2,Mod band (bnot ?CTRL_BITS)};
translate_button_1(B, _, Mod) ->
    {B,Mod}.

%%
%% The message window.
%%

message_event(redraw) ->
    dispatch_matching(fun({wm,{message,_,_}}) -> true;
			 ({wm,{message_right,_,_}}) -> true;
			 (_) -> false
		      end),
    case find_active(redraw) of
	none -> message_redraw([], []);
	Active ->
	    #win{stk=[#se{msg=Msg,msg_right=Right}|_]} = get_window_data(Active),
	    message_redraw(Msg, Right)
    end;
message_event({action,_}=Action) ->
    send(geom, Action);
message_event(_) -> keep.

message_redraw(Msg, Right) ->
    {W,_} = message_setup(),
    if
	Msg == [] -> ok;
	true -> wings_io:text_at(0, Msg)
    end,
    Cw = wings_text:width(),
    case Right of
	[] -> ok;
	Right when length(Msg)+length(Right) < W div Cw - 5 ->
	    L = length(Right),
	    Pos = W-?CHAR_WIDTH*(L+5),
	    wings_io:set_color(?MENU_COLOR),
	    gl:recti(Pos-?CHAR_WIDTH, -?LINE_HEIGHT+3,
		     Pos+(L+1)*?CHAR_WIDTH, 3),
	    gl:color3f(0, 0, 0),
	    wings_io:text_at(Pos, Right);
	_ -> ok
    end,
    case os:type() of
	{unix,darwin} -> draw_resizer(W-23, -4);
	_ -> ok
    end,
    keep.

draw_resizer(X, Y) ->
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    wings_io:draw_icon(X, Y, 12, 12, 16, 16, resize),
    gl:disable(?GL_TEXTURE_2D).

message_setup() ->
    wings_io:ortho_setup(),
    {_,_,W,H} = viewport(),
    wings_io:set_color(?PANE_COLOR),
    gl:recti(0, 0, W, H),
    wings_io:border(6, 0, W-20, H-2, ?PANE_COLOR),
    gl:translatef(10, H-6.5, 0),
    {W,H}.

%% Dirty hack to draw in the front buffer.
draw_message(F) ->
    ?CHECK_ERROR(),
    gl:flush(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:drawBuffer(?GL_FRONT),
    OldViewport = viewport(),
    {X,Y,W,H} = Viewport = viewport(message),
    gl:viewport(X, Y, W, H),
    put(wm_viewport, Viewport),
    message_setup(),
    Res = F(),
    put(wm_viewport, OldViewport),
    gl:drawBuffer(?GL_BACK),
    gl:popAttrib(),
    Res.

draw_completions(F) ->
    gl:flush(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),

    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),

    OldViewport = viewport(),
    {X,Y,W,H} = Viewport = viewport(geom),
    gl:viewport(X, Y, W, H),
    put(wm_viewport, Viewport),

    gl:drawBuffer(?GL_FRONT),
    wings_io:ortho_setup(),
    gl:loadIdentity(),
    Margin = 10,
    gl:translatef(float(Margin), H / 6, 0),
    wings_io:border(0, 0, W-2*Margin, 4*H div 6, ?MENU_COLOR),
    gl:translatef(10.0, float(?LINE_HEIGHT), 0.0),
    Res = F(),
    gl:drawBuffer(?GL_BACK),

    put(wm_viewport, OldViewport),

    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW),

    gl:popAttrib(),

    Res.

%%
%% The button window.
%%
-record(but,
	{mode,
	 buttons,
	 all_buttons,
	 restr=none
	}).

init_button() ->
    {seq,push,get_button_event(#but{mode=face})}.

get_button_event(But) ->
    {replace,fun(Ev) -> button_event(Ev, But) end}.
		     
button_event(#resize{w=W}, #but{restr=Restr}=But) ->
    AllButtons = buttons_place(W),
    Buttons = button_restrict(AllButtons, Restr),
    get_button_event(But#but{buttons=Buttons,all_buttons=AllButtons});
button_event(redraw, But) ->
    button_redraw(But),
    keep;
button_event(#mousebutton{button=1,x=X,state=?SDL_PRESSED}, But) ->
    button_was_hit(X, But),
    keep;
button_event(#mousebutton{button=1,x=X,state=?SDL_RELEASED}, But) ->
    button_help(X, But),
    keep;
button_event(#mousemotion{x=X}, But) ->
    button_help(X, But),
    keep;
button_event({action,_}=Action, _) ->
    send(geom, Action);
button_event({current_state,#st{selmode=Mode}}, #but{mode=Mode}) ->
    keep;
button_event({current_state,#st{selmode=Mode}}, But) ->
    dirty(),
    get_button_event(But#but{mode=Mode});
button_event({mode_restriction,Restr}, #but{restr=Restr}) ->
    keep;
button_event({mode_restriction,Restr}, #but{all_buttons=AllButtons}=But) ->
    Buttons = button_restrict(AllButtons, Restr),
    dirty(),
    get_button_event(But#but{buttons=Buttons,restr=Restr});
button_event(#keyboard{}=Ev, _) ->
    send(geom, Ev);
button_event(_, _) -> keep.

button_redraw(#but{mode=Mode,buttons=Buttons}) ->
    {_,_,W,H} = viewport(),
    wings_io:ortho_setup(),
    wings_io:set_color(?PANE_COLOR),
    gl:rectf(0, 0, W, H),
    gl:color3f(0.20, 0.20, 0.20),
    gl:'begin'(?GL_LINES),
    gl:vertex2f(0.5, 0.5),
    gl:vertex2f(W, 0.5),
    gl:'end'(),
    gl:color3f(0, 0, 0),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    foreach(fun({X,Name}) ->
		    wings_io:draw_icon(X, 3, button_value(Name, Mode))
	    end, Buttons),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D).

buttons_place(W) ->
    Mid = W div 2,
    Lmarg = 5,
    Rmarg = 20,
    [{Lmarg,flatshade},{Lmarg+?BUTTON_WIDTH,smooth},
     {Mid-2*?BUTTON_WIDTH,vertex},{Mid-?BUTTON_WIDTH,edge},
     {Mid,face},{Mid+?BUTTON_WIDTH,body},
     {W-3*?BUTTON_WIDTH-Rmarg,perspective},
     {W-2*?BUTTON_WIDTH-Rmarg,groundplane},
     {W-?BUTTON_WIDTH-Rmarg,axes}].

button_value(groundplane=Name, _) ->
    button_value(Name, show_groundplane, true);
button_value(axes=Name, _) ->
    button_value(Name, show_axes, true);
button_value(flatshade=Name, _) ->
    button_value(Name, workmode, true);
button_value(smooth=Name, _) ->
    button_value(Name, workmode, false);
button_value(perspective=Name, _) ->
    button_value(Name, orthogonal_view, true);
button_value(Mode, Mode) -> {Mode,down};
button_value(Name, _St) -> {Name,up}.

button_value(Name, Key, Val) ->
    case wings_pref:get_value(Key) of
	Val -> {Name,down};
	_ -> {Name,up}
    end.

button_was_hit(X, #but{buttons=Buttons}) ->
    button_was_hit_1(X, Buttons).

button_was_hit_1(X, [{Pos,Name}|_]) when Pos =< X, X < Pos+?BUTTON_WIDTH ->
    Action = case Name of
		 groundplane -> {view,show_groundplane};
		 axes -> {view,show_axes};
		 flatshade -> {view,flatshade};
		 smooth -> {view,smoothshade};
		 perspective -> {view,orthogonal_view};
		 Other -> {select,Other}
	     end,
    send(geom, {action,Action});
button_was_hit_1(X, [_|Is]) ->
    button_was_hit_1(X, Is);
button_was_hit_1(_X, []) ->
    send(geom, {action,{select,deselect}}).

button_help(X, #but{mode=Mode,buttons=Buttons}) ->
    message(button_help_1(X, Buttons, Mode)).

button_help_1(X, [{Pos,Name}|_], Mode) when Pos =< X, X < Pos+?BUTTON_WIDTH ->
    button_help_2(Name, Mode);
button_help_1(X, [_|Is], Mode) ->
    button_help_1(X, Is, Mode);
button_help_1(_, [], _) ->
    "Deselect".

button_help_2(vertex, vertex) -> "Select adjacent vertices";
button_help_2(vertex, _) -> "Change to vertex selection mode";
button_help_2(edge, edge) -> "Select adjcacent edges";
button_help_2(edge, _) -> "Change to edge selection mode";
button_help_2(face, face) -> "Select adjacent faces";
button_help_2(face, _) -> "Change to face selection mode";
button_help_2(body, body) -> "";
button_help_2(body, _) -> "Change to body selection mode";
button_help_2(Button, _) -> button_help_3(Button).

button_help_3(groundplane) ->
    [choose(show_groundplane, true, "Hide", "Show")|" ground plane"];
button_help_3(axes) ->
    [choose(show_axes, true, "Hide", "Show")|" axes"];
button_help_3(perspective) ->
    ["Change to ",choose(orthogonal_view, false,
			 "orthogonal", "perspective")|" view"];
button_help_3(smooth) ->
    choose(workmode, true, "Show objects with smooth shading", "");
button_help_3(flatshade) ->
    choose(workmode, false, "Show objects with flat shading", "").

button_restrict(Buttons, none) -> Buttons;
button_restrict(Buttons0, Restr) ->
    Buttons1 = sofs:from_external(Buttons0, [{atom,atom}]),
    Buttons = sofs:restriction(2, Buttons1, sofs:set(Restr)),
    sofs:to_external(Buttons).

choose(Key, Val, First, Second) ->
    case wings_pref:get_value(Key) of
	Val -> First;
	_ -> Second
    end.

%%
%% The menubar window.
%%

-define(MENU_MARGIN, 8).
-define(MENU_ITEM_SPACING, 3).

-record(mb,
	{bar=none,
	 sel=none,
	 win=none,
	 st
	}).

init_menubar() ->
    {seq,push,get_menu_event(#mb{sel=none})}.

get_menu_event(Mb) ->
    {replace,fun(Ev) -> menubar_event(Ev, Mb) end}.

menubar_event(redraw, Mb) ->
    menubar_redraw(Mb);
menubar_event(quit, _) ->
    send(geom, quit);
menubar_event({action,_}=Action, _) ->
    send(geom, Action);
menubar_event(clear_menu_selection, Mb) ->
    dirty(),
    get_menu_event(Mb#mb{sel=none});
menubar_event({current_state,St}, Mb) ->
    get_menu_event(Mb#mb{st=St});
menubar_event(#mousebutton{button=1,x=X0,state=?SDL_PRESSED},
	      #mb{sel=Sel}=Mb) ->
    case menubar_hit(X0, Mb) of
	none -> keep;
	{_,Sel,_} -> keep;
	{X,Name,Fun} -> menu_open(X, Name, Fun, Mb)
    end;
menubar_event(#mousemotion{x=X0}, #mb{sel=Sel}=Mb) ->
    case menubar_hit(X0, Mb) of
	none -> keep;
	{_,Sel,_} -> keep;
	{X,Name,Fun} when Sel =/= none ->
	    menu_open(X, Name, Fun, Mb);
	_ -> keep
    end;
menubar_event(_, _) -> keep.

menu_open(X, Name, Fun, #mb{win=Win,st=St}=Mb) ->
    Menu = Fun(St),
    {_,_,_,H} = viewport(),
    wings_menu:menu(X, H-1, Win, Name, Menu),
    get_menu_event(Mb#mb{sel=Name}).

menubar_redraw(Mb) ->
    {_,_,W,H} = viewport(),
    wings_io:ortho_setup(),
    wings_io:set_color(?PANE_COLOR),
    gl:rectf(0, 0, W, H-1),
    gl:color3f(0.2, 0.2, 0.2),
    gl:'begin'(?GL_LINES),
    gl:vertex2f(0.5, H-1),
    gl:vertex2f(W, H-1),
    gl:'end'(),
    Main = get(wm_main),
    case get_menubar(Main) of
	none -> keep;
	Menubar ->
	    menubar_redraw_1(Menubar, Mb),
	    get_menu_event(Mb#mb{bar=Menubar,win=Main})
    end.

menubar_redraw_1(Menubar, #mb{sel=Sel}) ->
    menubar_draw(Menubar, ?MENU_MARGIN, Sel).

menubar_draw([{Desc,Name,_}|T], X, Sel) ->
    W = ?CHAR_WIDTH*(?MENU_ITEM_SPACING+length(Desc)),
    if
	Name =:= Sel ->
	    {_,_,_,H} = viewport(),
	    wings_io:border(X+2-?MENU_MARGIN, 0,
			    W, H-2, ?MENU_COLOR);
	true -> ok
    end,
    gl:color3f(0, 0, 0),
    wings_io:text_at(X, ?CHAR_HEIGHT, Desc),
    menubar_draw(T, X+W, Sel);
menubar_draw([], _, _) -> keep.

menubar_hit(X0, #mb{bar=Bar}=Mb) ->
    case X0-?MENU_MARGIN of
	X when X < 0 -> none;
	X -> menubar_hit_1(Bar, Mb, X, 0)
    end.

menubar_hit_1([{Desc,Name,Fun}|T], Mb, RelX, X) ->
    case ?CHAR_WIDTH*length(Desc) of
	W when RelX < W ->
	    {X+2,Name,Fun};
	W ->
	    Iw = W+?MENU_ITEM_SPACING*?CHAR_WIDTH,
	    menubar_hit_1(T, Mb, RelX-Iw, X+Iw)
    end;
menubar_hit_1([], _, _, _) -> none.

%%%
%%% Toplevel: create a window and a controller window at the same time.
%%% The controller adds a title bar (allowing the window to be moved) and
%%% can optionally add other things such as a scroller.
%%%

toplevel(Name, Title, Pos, Size, Op) ->
    toplevel(Name, Title, Pos, Size, [], Op).
    
toplevel(Name, Title, Pos, Size, Flags, Op) ->
    new(Name, Pos, Size, Op),
    new_controller(Name, Title, Flags).

-record(ctrl,
	{title,					%Title of window.
	 children,		                %Windows being controlled.
	 state=idle,				%idle|moving
	 local,
	 prev_focus				%Previous focus holder.
	}).

new_controller(Client, Title, Flags) ->
    TitleBarH = ?LINE_HEIGHT+3,
    #win{x=X,y=Y,z=Z,w=W0} = Win = get_window_data(Client),
    Controller = {controller,Client},
    Controlled0 = ctrl_create_windows(reverse(sort(Flags)), Client, Win),
    Controlled = [Client,Controller|Controlled0],
    W = case is_window({vscroller,Client}) of
	    false -> W0;
	    true -> W0 + wings_win_scroller:width()
	end,
    Size = {W,TitleBarH},
    Cs = #ctrl{title=Title,children=Controlled},
    new(Controller, {X,Y-TitleBarH,Z-0.5}, Size,
	{seq,push,get_ctrl_event(Cs)}),
    ctrl_anchor(Controlled, Flags, Size, TitleBarH),
    keep.

ctrl_create_windows([vscroller|Flags], Client, #win{x=X,y=Y,z=Z,w=W}=Win) ->
    Name = wings_win_scroller:vscroller(Client, {X+W,Y,Z+0.1}),
    [Name|ctrl_create_windows(Flags, Client, Win)];
ctrl_create_windows([resizable|Flags], Client, Win) ->
    Name = ctrl_new_resizer(Client),
    [Name|ctrl_create_windows(Flags, Client, Win)];
ctrl_create_windows([_|Flags], Client, Win) ->
    ctrl_create_windows(Flags, Client, Win);
ctrl_create_windows([], _, _) -> [].

ctrl_anchor(Controlled, Flags, Size, TitleBarH) ->
    case keysearch(anchor, 1, Flags) of
	false -> ok;
	{value,{anchor,Anchor}} ->
	    ctrl_anchor_1(Anchor, Controlled, Size, TitleBarH)
    end.

ctrl_anchor_1(nw, Controlled, _, Th) ->
    update_windows(Controlled, [{dy,Th}]);
ctrl_anchor_1(ne, Controlled, {W,_}, Th) ->
    update_windows(Controlled, [{dx,-W},{dy,Th}]);
ctrl_anchor_1(sw, [Client|_]=Controlled, _, Th) ->
    {_,_,_,H} = viewport(Client),
    update_windows(Controlled, [{dy,Th-H}]).

get_ctrl_event(Cs) ->
    {replace,fun(Ev) -> ctrl_event(Ev, Cs) end}.
		     
ctrl_event(redraw, Cs) ->
    ctrl_redraw(Cs);
ctrl_event(#mousebutton{button=1,state=?SDL_PRESSED},
	   #ctrl{state=moving,prev_focus=Focus}=Cs) ->
    grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});
ctrl_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, Cs) ->
    Focus = focus_window(),
    grab_focus(get(wm_active)),
    get_ctrl_event(Cs#ctrl{local={X,Y},state=moving,prev_focus=Focus});
ctrl_event(#mousebutton{button=1,state=?SDL_RELEASED}, #ctrl{prev_focus=Focus}=Cs) ->
    grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});
ctrl_event(#mousemotion{x=X0,y=Y0,state=?SDL_PRESSED},
	   #ctrl{state=moving,children=Children,local={LocX,LocY}}) ->
    {X1,Y1} = local2global(X0, Y0),
    X = X1 - LocX,
    Y = Y1 - LocY,
    {OldX,OldY} = win_ul(),
    Dx0 = X-OldX,
    Dy0 = Y-OldY,
    {Dx,Dy} = ctrl_constrain_move(Dx0, Dy0),
    update_windows(Children, [{dx,Dx},{dy,Dy}]),
    keep;
ctrl_event(#mousemotion{state=?SDL_RELEASED},
	   #ctrl{state=moving,prev_focus=Focus}=Cs) ->
    grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});
ctrl_event({client_resized,Client}, _) ->
    {_,_,W,_} = viewport(Client),
    Self = {controller,Client},
    case is_window({vscroller,Client}) of
	false -> update_window(Self, [{w,W}]);
	true -> update_window(Self, [{w,W+wings_win_scroller:width()}])
    end,
    keep;
ctrl_event(_, _) -> keep.

ctrl_redraw(#ctrl{title=Title}) ->
    TitleBarH = ?LINE_HEIGHT+3,
    wings_io:ortho_setup(),
    {_,_,W,_} = viewport(),
    Color = {0.3,0.4,0.3},
    wings_io:border(0, 0, W-0.5, TitleBarH, Color),
    gl:color3f(1, 1, 1),
    wings_io:text_at(10, TitleBarH-5, Title),
    keep.

ctrl_constrain_move(Dx0, Dy0) ->
    {{DeskX,DeskY},{DeskW,DeskH}} = win_rect(desktop),
    {{X0,Y0},{W,_}} = win_rect(),
    Dx = case X0+Dx0-DeskX of
	     X when X < 0 ->
		 DeskX-X0;
	     X when DeskX+DeskW < X+W ->
		 DeskX+DeskW-X0-W;
	     _ ->
		 Dx0
	 end,
    {_,Client} = get(wm_active),
    {{_,Cy},{_,Ch}} = win_rect(Client),
    Dy = if 
	     Y0+Dy0 < DeskY ->
		 DeskY-Y0;
	     Cy+Ch+Dy0 >= DeskY+DeskH ->
		 DeskY+DeskH-Cy-Ch;
	     true ->
		 Dy0
	 end,
    {Dx,Dy}.
    
%%%
%%% Resizer window.
%%%

-record(rsz,
	{state=idle,				%idle|moving
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
    draw_resizer(0, 0),
    keep;
resize_event(#mousebutton{button=1,state=?SDL_PRESSED},
	     #rsz{state=moving,prev_focus=Focus}=Rst) ->
    grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, Rst) ->
    Focus = focus_window(),
    grab_focus(get(wm_active)),
    get_resize_event(Rst#rsz{local={X,Y},state=moving,prev_focus=Focus});
resize_event(#mousebutton{button=1,state=?SDL_RELEASED}, #rsz{prev_focus=Focus}=Rst) ->
    grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(#mousemotion{x=X0,y=Y0,state=?SDL_PRESSED},
	     #rsz{state=moving,local={LocX,LocY}}) ->
    {X1,Y1} = local2global(X0, Y0),
    X = X1 - LocX,
    Y = Y1 - LocY,
    {OldX,OldY} = win_ul(),
    Dx0 = X-OldX,
    Dy0 = Y-OldY,
    {Dx,Dy} = resize_constrain(Dx0, Dy0),
    {resizer,Client} = Self = get(wm_active),
    update_window(Client, [{dw,Dx},{dh,Dy}]),
    NewPos = resize_pos(Client),
    move(Self, NewPos),
    ResizeMsg = {client_resized,Client},
    send({controller,Client}, ResizeMsg),
    send({vscroller,Client}, ResizeMsg),
    keep;
resize_event(#mousemotion{state=?SDL_RELEASED},
	   #rsz{state=moving,prev_focus=Focus}=Rst) ->
    grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(_, _) -> keep.

resize_pos(Client) ->
    #win{x=X,y=Y,z=Z,w=W,h=H} = get_window_data(Client),
    case is_window({vscroller,Client}) of
	false ->  {X+W-13,Y+H-13,Z+0.5};
	true -> {X+W,Y+H-13,Z+0.5}
    end.

resize_constrain(Dx0, Dy0) ->
    {{DeskX,DeskY},{DeskW,DeskH}} = win_rect(desktop),
    {{X,Y},{W,H}} = win_rect(),
    Dx = if
	     DeskX+DeskW =< X+H+Dx0 ->
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
    {Dx,Dy}.

