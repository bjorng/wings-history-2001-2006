%%
%%  wings_wm.erl --
%%
%%     Window manager for Wings.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_wm.erl,v 1.11 2002/08/01 19:25:04 bjorng Exp $
%%

-module(wings_wm).
-export([init/0,top_window/1,dirty/0,new/4,delete/1,
	 offset/3,pos/1,set_active/1,top_size/0,wsize/0,viewport/0,
	 local2global/2,global2local/2,local_mouse_state/0,
	 is_window_active/1,window_under/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [map/2,last/1,sort/1,keysort/2,reverse/1]).

-record(win,
	{z,					%Z order.
	 x,y,					%Position.
	 w,h,					%Size.
	 name,					%Name of window.
	 stk					%Event handler stack.
	}).

%%%
%%% Process dictionary usage:
%%%
%%% wm_active		Window name for active window.
%%% wm_windows		All windows.
%%% wm_dirty		Exists if redraw is needed.
%%% wm_top_size         Size of top window.
%%% wm_viewport		Current viewport.
%%%
%%% Event loop.
%%%

init() ->
    wings_pref:set_default(window_size, {780,570}),
    {W,H} = wings_pref:get_value(window_size),
    set_video_mode(W, H),			%Needed on Solaris/Sparc.
    Win = #win{x=0,y=0,w=W,h=H,z=0,name=top,stk=[crash_handler()]},
    put(wm_windows, gb_trees:from_orddict([{top,Win}])),
    put(wm_active, top),
    ok.

dirty() ->
    put(wm_dirty, dirty).

new(Name, {X,Y,Z}, {W,H}, Op) when is_integer(X), is_integer(Y),
				   is_integer(W), is_integer(H) ->
    dirty(),
    Stk = handle_response(Op, dummy_event, [crash_handler()]),
    Win = #win{x=X,y=Y,z=Z,w=W,h=H,name=Name,stk=Stk},
    put(wm_windows, gb_trees:insert(Name, Win, get(wm_windows))),
    keep.

delete(Name) ->
    case get(wm_active) of
	Name -> put(wm_active, top);
	_ -> ok
    end,
    dirty(),
    put(wm_windows, gb_trees:delete_any(Name, get(wm_windows))),
    keep.

offset(Name, Xoffs, Yoffs) ->
    #win{x=X,y=Y} = Win = get_window_data(Name),
    put_window_data(Name, Win#win{x=X+Xoffs,y=Y+Yoffs}).

pos(Name) ->
    #win{x=X,y=Y} = get_window_data(Name),
    {X,Y}.
	    
set_active(Name) ->
    put(wm_active, Name).

is_window_active(Name) ->
    get(wm_active) =:= Name.

top_size() ->
    #win{w=W,h=H} = get_window_data(top),
    {W,H}.

viewport() ->
    get(wm_viewport).

wsize() ->
    {_,_,W,H} = get(wm_viewport),
    {W,H}.

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

window_under(X, Y) ->
    Wins = reverse(gb_trees:values(get(wm_windows))),
    window_under(Wins, X, Y).

window_under([#win{x=X,y=Y,w=W,h=H,name=Name}|_], X0, Y0)
  when X =< X0, X0 < X+W,
       Y =< Y0, Y0 < Y+H -> Name;
window_under([_|T], X, Y) ->
    window_under(T, X, Y).
    
top_window(Op) ->
    #win{z=0,w=W,h=H} = Win0 = get_window_data(top),
    Stk = handle_response(Op, dummy_event, [crash_handler()]),
    Win = Win0#win{stk=Stk},
    put_window_data(top, Win),
    erase(wm_dirty),
    wings_io:putback_event(#resize{w=W,h=H}),
    event_loop().

event_loop() ->
    case get(wm_dirty) of
	undefined ->
	    Event = wings_io:get_event(),
	    dispatch_event(Event);
	_ -> redraw_all()
    end.

dispatch_event(#resize{w=W,h=H}=Event) ->
    put(wm_top_size, {W,H}),
    Win0 = get_window_data(top),
    Win1 = Win0#win{w=W,h=H},
    put_window_data(top, Win1),
    set_video_mode(W, H),
    {R,G,B} = wings_pref:get_value(background_color),
    gl:clearColor(R, G, B, 1.0),
    Win = send_event(Win1, Event),
    put_window_data(top, Win),
    event_loop();
dispatch_event(Event) ->
    Active = get(wm_active),
    Win0 = get_window_data(Active),
    case send_event(Win0, Event) of
	#win{name=Name,stk=delete} ->
	    delete(Name),
	    event_loop();
	#win{stk=[]} -> ok;
	Win ->
	    put_window_data(Active, Win),
	    event_loop()
    end.

redraw_all() ->
    EarlyBC = wings_pref:get_value(early_buffer_clear),
    maybe_clear(late, EarlyBC),			%Clear right before drawing (late).
    Ws = map(fun({Name,Win}) ->
		     {Name,send_event(Win, redraw)}
	     end, keysort(2, gb_trees:to_list(get(wm_windows)))),
    put(wm_windows, gb_trees:from_orddict(sort(Ws))),
    gl:swapBuffers(),
    maybe_clear(early, EarlyBC),		%Clear immediately after buffer swap (early).
    wings_io:arrow(),
    erase(wm_dirty),
    event_loop().

maybe_clear(early, true) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT);
maybe_clear(late, false) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT);
maybe_clear(_, _) -> ok.

send_event(Win, {expose}) ->
    dirty(),
    Win;
send_event(#win{name=Name,x=X,y=Y0,w=W,h=H,stk=[Handler|_]=Stk0}, Ev0) ->
    Ev = translate_event(Ev0, X, Y0),
    {_,TopH} = get(wm_top_size),
    Y = TopH-(Y0+H),
    ViewPort = {X,Y,W,H},
    case put(wm_viewport, {X,Y,W,H}) of
	ViewPort -> ok;
	_ -> gl:viewport(X, Y, W, H)
    end,
    Stk = handle_event(Handler, Ev, Stk0),
    Win = get_window_data(Name),
    Win#win{stk=Stk}.

translate_event(#mousemotion{x=X,y=Y}=M, Ox, Oy) ->
    M#mousemotion{x=X-Ox,y=Y-Oy};
translate_event(#mousebutton{x=X,y=Y}=M, Ox, Oy) ->
    M#mousebutton{x=X-Ox,y=Y-Oy};
translate_event({drop,{X,Y},DropData}, Ox, Oy) ->
    {drop,{X-Ox,Y-Oy},DropData};
translate_event(Ev, _, _) -> Ev.
    
handle_event(Handler, Event, Stk) ->
    case catch Handler(Event) of
	{'EXIT',normal} ->
	    exit(normal);
	{'EXIT',Reason} ->
	    CrashHandler = last(Stk),
	    handle_response(CrashHandler({crash,Reason}),
			    Event, [crash_handler()]);
	Res ->
	    handle_response(Res, Event, Stk)
    end.

handle_response(Res, Event, Stk0) ->
    case Res of
	keep -> Stk0;
	next -> next_handler(Event, Stk0);
	pop -> pop(Stk0);
	delete -> delete;
	{push,Top} -> [Top|Stk0];
	{seq,First,Then} ->
	    Stk = handle_response(First, Event, Stk0),
	    handle_response(Then, Event, Stk);
	{replace,Top} when is_function(Top) -> replace_top(Top, Stk0);
	Top when is_function(Top) -> replace_top(Top, Stk0)
    end.

pop([_|Stk]) -> Stk.

replace_top(Top, [_|Stk]) -> [Top|Stk].

next_handler(Event, [_|[Next|_]=Stk]) ->
    handle_event(Next, Event, Stk).

crash_handler() ->
    fun(Crash) ->
	    io:format("Crashed: ~p\n", [Crash]),
	    exit(too_bad)
    end.

%%%
%%% Utility functions.
%%%

get_window_data(Name) ->
    gb_trees:get(Name, get(wm_windows)).

put_window_data(Name, Data) ->
    put(wm_windows, gb_trees:update(Name, Data, get(wm_windows))).

set_video_mode(W, H) ->
    sdl_video:setVideoMode(W, H, 0, ?SDL_OPENGL bor ?SDL_RESIZABLE).
