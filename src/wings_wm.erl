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
%%     $Id: wings_wm.erl,v 1.1 2002/04/11 16:12:04 bjorng Exp $
%%

-module(wings_wm).
-export([top_window/1,dirty/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [map/2,last/1]).

-record(win,
	{z,					%Z order (0 highest).
	 x,y,					%Position.
	 w,h,					%Size.
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

dirty() ->
    put(wings_dirty, dirty).

top_window(Op) ->
    Stk = handle_response(Op, dummy_event, [crash_handler()]),
    [X,Y,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Win = #win{x=X,y=Y,w=W,h=H,z=0,stk=Stk},
    put(wings_active, top),
    put(wings_windows, gb_trees:from_orddict([{top,Win}])),
    event_loop().

event_loop() ->
    case get(wings_dirty) of
	undefined ->
	    Ws = get(wings_windows),
	    Active = get(wings_active),
	    Win0 = gb_trees:get(Active, Ws),
	    Event = wings_io:get_event(),
	    case send_event(Win0, Event) of
		#win{stk=[]} -> ok;
		Win ->
		    put(wings_windows, gb_trees:update(Active, Win, Ws)),
		    event_loop()
	    end;
	_ ->
	    redraw_all()
    end.

redraw_all() ->
    Ws = map(fun({Name,Win}) ->
		     {Name,send_event(Win, redraw)}
	     end, gb_trees:to_list(get(wings_windows))),
    put(wings_windows, gb_trees:from_orddict(Ws)),
    gl:swapBuffers(),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings_io:arrow(),
    erase(wings_dirty),
    event_loop().

send_event(Win, {expose}) ->
    dirty(),
    Win;
send_event(#win{stk=[Handler|_]=Stk0}=Win, Event) ->
    Stk = handle_event(Handler, Event, Stk0),
    Win#win{stk=Stk}.
    
handle_event(Handler, Event, Stk) ->
    case catch Handler(Event) of
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
