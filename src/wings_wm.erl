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
%%     $Id: wings_wm.erl,v 1.4 2002/07/12 07:31:27 bjorng Exp $
%%

-module(wings_wm).
-export([init/0,top_window/1,dirty/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [map/2,last/1]).

-record(win,
	{z,					%Z order.
	 x,y,					%Position.
	 w,h,					%Size.
	 parent=none,				%Parent window.
	 children=[],				%Child windows.
	 stk					%Event handler stack.
	}).

%%%
%%% Process dictionary usage:
%%%
%%% wings_active	#win record for active window.
%%% wings_windows	All windows
%%% wings_dirty		Exists if redraw is needed.
%%%
%%% Event loop.
%%%

init() ->
    wings_pref:set_default(window_size, {780,570}),
    {W,H} = wings_pref:get_value(window_size),
    set_video_mode(W, H),			%Needed on Solaris/Sparc.
    Win = #win{x=0,y=0,w=W,h=H,z=0,stk=[crash_handler()]},
    put(wings_windows, gb_trees:from_orddict([{top,Win}])),
    put(wings_active, top),
    ok.

dirty() ->
    put(wings_dirty, dirty).

top_window(Op) ->
    #win{w=W,h=H} = Win0 = get_window_data(top),
    Stk = handle_response(Op, dummy_event, [crash_handler()]),
    Win = Win0#win{stk=Stk},
    put_window_data(top, Win),
    erase(wings_dirty),
    wings_io:putback_event(#resize{w=W,h=H}),
    event_loop().

event_loop() ->
    case get(wings_dirty) of
	undefined ->
	    Event = get_event(),
	    Active = get(wings_active),
	    Win0 = get_window_data(Active),
	    case send_event(Win0, Event) of
		#win{stk=[]} -> ok;
		Win ->
		    put_window_data(Active, Win),
		    event_loop()
	    end;
	_ ->
	    redraw_all()
    end.

redraw_all() ->
    EarlyBC = wings_pref:get_value(early_buffer_clear),
    maybe_clear(late, EarlyBC),			%Clear right before drawing (late).
    Ws = map(fun({Name,Win}) ->
		     {Name,send_event(Win, redraw)}
	     end, gb_trees:to_list(get(wings_windows))),
    put(wings_windows, gb_trees:from_orddict(Ws)),
    gl:swapBuffers(),
    maybe_clear(early, EarlyBC),		%Clear immediately after buffer swap (early).
    wings_io:arrow(),
    erase(wings_dirty),
    event_loop().

maybe_clear(early, true) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT);
maybe_clear(late, false) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT);
maybe_clear(_, _) -> ok.

get_event() ->
    case wings_io:get_event() of
	#resize{w=W,h=H}=Resize ->
	    Win = get_window_data(top),
	    put_window_data(top, Win#win{w=W,h=H}),
	    set_video_mode(W, H),
	    {R,G,B} = wings_pref:get_value(background_color),
	    gl:clearColor(R, G, B, 1.0),
	    dirty(),
	    Resize;
	Other -> Other
    end.

send_event(Win, {expose}) ->
    dirty(),
    Win;
send_event(#win{x=X,y=Y,w=W,h=H,stk=[Handler|_]=Stk0}=Win, Event) ->
    gl:viewport(X, Y, W, H),
    Stk = handle_event(Handler, Event, Stk0),
    Win#win{stk=Stk}.
    
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
    gb_trees:get(Name, get(wings_windows)).

put_window_data(Name, Data) ->
    put(wings_windows, gb_trees:update(Name, Data, get(wings_windows))).

set_video_mode(W, H) ->
    sdl_video:setVideoMode(W, H, 0, ?SDL_OPENGL bor ?SDL_RESIZABLE).

