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
%%     $Id: wings_wm.erl,v 1.17 2002/11/23 08:48:50 bjorng Exp $
%%

-module(wings_wm).
-export([init/0,enter_event_loop/0,dirty/0,clean/0,new/4,delete/1,
	 offset/3,pos/1,set_active/1,
	 top_size/0,viewport/0,viewport/1,
	 local2global/2,global2local/2,local_mouse_state/0,
	 is_window_active/1,window_under/2,
	 translation_change/0,me_modifiers/0,set_me_modifiers/1,
	 draw_message/1,draw_completions/1]).

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

-record(se,					%Stack entry record.
	{h,					%Handler (fun).
	 msg=""}).				%Current message.

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
    translation_change(),
    put(wm_windows, gb_trees:empty()),
    new(top, {0,0,0}, {W,H}, {push,fun(_) -> keep end}),
    MsgH = 2*?LINE_HEIGHT-4,
    new(message, {0,H-MsgH,10000}, {W,MsgH},
	{seq,{push,dummy},{replace,fun message_event/1}}),
    ok.
    
dirty() ->
    put(wm_dirty, dirty).

clean() ->
    erase(wm_dirty).

new(Name, {X,Y,Z}, {W,H}, Op) when is_integer(X), is_integer(Y),
				   is_integer(W), is_integer(H) ->
    dirty(),
    Stk = handle_response(Op, dummy_event, default_stack()),
    Win = #win{x=X,y=Y,z=Z,w=W,h=H,name=Name,stk=Stk},
    put(wm_windows, gb_trees:insert(Name, Win, get(wm_windows))),
    keep.

delete(Name) ->
    case get(wm_active) of
	Name -> put(wm_active, geom);
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

viewport(Name) ->
    #win{x=X,y=Y0,w=W,h=H} = get_window_data(Name),
    {_,TopH} = get(wm_top_size),
    Y = TopH-(Y0+H),
    {X,Y,W,H}.

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
    
enter_event_loop() ->
    {W,H} = top_size(),
    dispatch_event(#resize{w=W,h=H}).

event_loop() ->
    case get(wm_dirty) of
	undefined ->
	    get_and_dispatch();
	_ ->
	    case wings_io:poll_event() of
		{new_state,_} -> get_and_dispatch();
		_ -> redraw_all()
	    end
    end.

get_and_dispatch() ->
    Event = wings_io:get_event(),
    dispatch_event(Event).

dispatch_event(#resize{w=W,h=H}=Event) ->
    ?CHECK_ERROR(),
    put(wm_top_size, {W,H}),
    Win0 = get_window_data(top),
    Win1 = Win0#win{w=W,h=H},
    put_window_data(top, Win1),
    set_video_mode(W, H),
    {R,G,B} = wings_pref:get_value(background_color),
    gl:clearColor(R, G, B, 1.0),
    Win = send_event(Win1, Event),
    put_window_data(top, Win),

    #win{h=MsgH} = MsgData0 = get_window_data(message),
    MsgData1 = MsgData0#win{x=0,y=H-MsgH,w=W},
    put_window_data(message, MsgData1),
    MsgData = send_event(MsgData1, Event#resize{w=W}),
    put_window_data(message, MsgData),

    GeomData0 = get_window_data(geom),
    GeomData1 = GeomData0#win{x=0,y=0,w=W,h=H-MsgH},
    put_window_data(geom, GeomData1),
    GeomData = send_event(GeomData1, Event#resize{h=H-MsgH}),
    put_window_data(geom, GeomData),

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
send_event(#win{name=Name,x=X,y=Y0,w=W,h=H,stk=[Se|_]=Stk0}, Ev0) ->
    Ev = translate_event(Ev0, X, Y0),
    {_,TopH} = get(wm_top_size),
    Y = TopH-(Y0+H),
    ViewPort = {X,Y,W,H},
    %%io:format("~p: ~p ~p\n", [Name,{X,Y0},ViewPort]),
    case put(wm_viewport, {X,Y,W,H}) of
	ViewPort -> ok;
	_ -> gl:viewport(X, Y, W, H)
    end,
    Stk = handle_event(Se, Ev, Stk0),
    Win = get_window_data(Name),
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
			    Event, default_stack());
	Res ->
	    handle_response(Res, Event, Stk)
    end.

handle_response(Res, Event, Stk0) ->
    case Res of
	keep -> Stk0;
	next -> next_handler(Event, Stk0);
	pop -> pop(Stk0);
	delete -> delete;
	{push,Top} -> [#se{h=Top}|Stk0];
	{seq,First,Then} ->
	    Stk = handle_response(First, Event, Stk0),
	    handle_response(Then, Event, Stk);
	{replace,Top} when is_function(Top) -> replace_top(Top, Stk0);
	Top when is_function(Top) -> replace_top(Top, Stk0)
    end.

pop([_|Stk]) -> Stk.

replace_top(Top, [_|Stk]) -> [#se{h=Top}|Stk].

next_handler(Event, [_|[Next|_]=Stk]) ->
    handle_event(Next, Event, Stk).

default_stack() ->
    Handler = fun(Crash) ->
		      io:format("Crashed: ~p\n", [Crash]),
		      exit(too_bad)
	      end,
    [#se{h=Handler}].

%%%
%%% Utility functions.
%%%

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
    ?CHECK_ERROR(),
    {W,_} = message_setup(),
    Msg0 = get(wm_message),
    Msg = if
	      Msg0 == undefined -> [];
	      true ->
		  wings_io:text_at(0, Msg0),
		  Msg0
	  end,
    case get(wm_message_right) of
	undefined -> ok;
	Right when length(Msg)+length(Right) < W div ?CHAR_WIDTH-3 ->
	    L = length(Right),
	    Pos = W-?CHAR_WIDTH*(L+3),
	    wings_io:set_color(?MENU_COLOR),
	    gl:recti(Pos-?CHAR_WIDTH, -?LINE_HEIGHT+3,
		     Pos+(L+1)*?CHAR_WIDTH, 3),
	    gl:color3f(0, 0, 0),
	    wings_io:text_at(Pos, Right);
	_ -> ok
    end,
    ?CHECK_ERROR(),
    keep;
message_event(_) -> keep.

message_setup() ->
    wings_io:ortho_setup(),
    {_,_,W,H} = viewport(),
    wings_io:set_color(?PANE_COLOR),
    gl:recti(0, 0, W, H),
    wings_io:border(6, 3, W-10, H-5, ?PANE_COLOR),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:translatef(10, H-8, 0),
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
