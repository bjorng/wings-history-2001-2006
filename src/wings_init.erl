%%
%%  wings_init.erl --
%%
%%     Initialization of Wings video and event handling.
%%
%%  Copyright (c) 2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_init.erl,v 1.1 2003/11/08 16:02:55 bjorng Exp $
%%

-module(wings_init).
-export([init/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

init() ->
    os:putenv("SDL_HAS3BUTTONMOUSE", "true"),

    wings_pref:set_default(window_size, {780,570}),
    TopSize = wings_pref:get_value(window_size),
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_NOPARACHUTE),
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),

    %% Make sure that some video mode works. Otherwise crash early.
    %% From best to worst.
    try_video_modes(opengl_modes(), TopSize),
    wings_util:init_gl_extensions(),
    wings_util:init_gl_restrictions(),

    %% Initialize event handling and other stuff.
    sdl_events:eventState(?SDL_ALLEVENTS,?SDL_IGNORE),
    sdl_events:eventState(?SDL_MOUSEMOTION, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_MOUSEBUTTONDOWN, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_MOUSEBUTTONUP, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_KEYDOWN, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEORESIZE, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEOEXPOSE, ?SDL_ENABLE),
    sdl_keyboard:enableUNICODE(true),
    sdl_keyboard:enableKeyRepeat(?SDL_DEFAULT_REPEAT_DELAY,
				 ?SDL_DEFAULT_REPEAT_INTERVAL),
    ok.

opengl_modes() ->
    [[{buffer_size,32},{depth_size,32},{stencil_size,8},{accum_size,16}],
     [{buffer_size,24},{depth_size,32},{stencil_size,8},{accum_size,16}],
     [{buffer_size,24},{depth_size,24},{stencil_size,8},{accum_size,16}],
     [{buffer_size,24},{depth_size,24},{stencil_size,0},{accum_size,16}],
     [{buffer_size,16},{depth_size,24},{stencil_size,8},{accum_size,16}],
     [{buffer_size,16},{depth_size,16},{stencil_size,8},{accum_size,16}],
     [{buffer_size,16},{depth_size,16},{stencil_size,0},{accum_size,16}],
     [{buffer_size,16},{depth_size,16},{stencil_size,0},{accum_size,0}],
     [{buffer_size,15},{depth_size,16},{stencil_size,8},{accum_size,16}],
     [{buffer_size,15},{depth_size,16},{stencil_size,0},{accum_size,16}],
     [{buffer_size,15},{depth_size,16},{stencil_size,0},{accum_size,0}],

     %% Fallback - use default for all.
     [{buffer_size,0},{depth_size,0},{stencil_size,0},{accum_size,0}]].

try_video_modes(Modes, TopSize) ->
    io:format("Trying hardware-accelerated OpenGL modes\n"),
    case try_video_modes_1(Modes, TopSize, false) of
	ok -> ok;
	error ->
	    io:format("Trying software OpenGL modes:\n"),
	    case try_video_modes_1(Modes, TopSize, true) of
		ok -> ok;
		error -> video_mode_failure()
	    end
    end.

video_mode_failure() ->
    io:format("\n###########################################\n\n"),
    io:format("Failed to find any suitable OpenGL mode.\n\n"),
    io:format("Make sure that OpenGL drivers are installed.\n\n"),
    io:format("###########################################\n\n"),
    erlang:fault("No suitable OpenGL mode found (are OpenGL drivers installed?)").

try_video_modes_1([Mode|Modes], TopSize, AcceptSoftware) ->
    io:format("  ~p\n", [Mode]),
    case try_video_mode(Mode, TopSize, AcceptSoftware) of
	ok -> ok;
	error -> try_video_modes_1(Modes, TopSize, AcceptSoftware)
    end;
try_video_modes_1([], _, _) -> error.

try_video_mode(Ps, {W,H}, AcceptSoftware) ->
    set_video_props(Ps),
    case catch set_video_mode(W, H) of
	ok ->
	    case AcceptSoftware orelse
		gl:getString(?GL_RENDERER) =/= "GDI Generic" of
		false -> error;
		true ->
		    %% We have found an acceptable video mode.
		    %% On Windows, a bug in SDL makes it necessary
		    %% to reinitialize SDL to get keyboard input working
		    %% in case we have been changing video modes
		    %% (because we rejected video modes because they were
		    %% not hw-accelerated).
		    sdl:quit(),
		    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_NOPARACHUTE),
		    Ebin = filename:dirname(code:which(?MODULE)),
		    IconFile = filename:join(Ebin, "wings.icon"),
		    catch sdl_video:wm_setIcon(sdl_video:loadBMP(IconFile),
					       null),
		    set_video_props(Ps),
		    catch set_video_mode(W, H),
		    display_actual_mode()
	    end;
	_ -> error
    end.

set_video_props([{Prop,Val}|Ps]) ->
    set_video_prop(Prop, Val),
    set_video_props(Ps);
set_video_props([]) -> ok.

set_video_prop(buffer_size, Bits) ->
    sdl_video:gl_setAttribute(?SDL_GL_BUFFER_SIZE, Bits);
set_video_prop(depth_size, Depth) ->
    sdl_video:gl_setAttribute(?SDL_GL_DEPTH_SIZE, Depth);
set_video_prop(stencil_size, Bits) ->
    sdl_video:gl_setAttribute(?SDL_GL_STENCIL_SIZE, Bits);
set_video_prop(accum_size, Bits) ->
    sdl_video:gl_setAttribute(?SDL_GL_ACCUM_RED_SIZE, Bits),
    sdl_video:gl_setAttribute(?SDL_GL_ACCUM_GREEN_SIZE, Bits),
    sdl_video:gl_setAttribute(?SDL_GL_ACCUM_BLUE_SIZE, Bits),
    sdl_video:gl_setAttribute(?SDL_GL_ACCUM_ALPHA_SIZE, Bits).

display_actual_mode() ->
    Attrs = [?GL_RED_BITS,
	     ?GL_GREEN_BITS,
	     ?GL_BLUE_BITS,
	     ?GL_ALPHA_BITS,
	     ?GL_DEPTH_BITS,
	     ?GL_STENCIL_BITS,
	     ?GL_ACCUM_RED_BITS,
	     ?GL_ACCUM_GREEN_BITS,
	     ?GL_ACCUM_BLUE_BITS,
	     ?GL_ACCUM_ALPHA_BITS],
    io:format("Actual: RGBA: ~p ~p ~p ~p Depth: ~p Stencil: ~p Accum: ~p ~p ~p ~p\n",
	      [hd(gl:getIntegerv(A)) || A <- Attrs]).

set_video_mode(W, H) ->
    {surfacep,_} = sdl_video:setVideoMode(W, H, 0, ?SDL_OPENGL bor ?SDL_RESIZABLE),
    ok.
