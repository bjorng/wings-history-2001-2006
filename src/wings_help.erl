%%
%%  wings_help.erl --
%%
%%     This module implements the Help menu.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_help.erl,v 1.24 2002/08/28 05:44:16 bjorng Exp $
%%

-module(wings_help).
-export([menu/3,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3]).

menu(X, Y, _) ->
    Menu = [{"Getting Started",getting_started},
	    separator,
	    {"Accessing the Third Dimension",third_dimension},
	    {"Light Basics",lights},
	    {"Assigning Hotkeys",defining_hotkeys},
	    {"Advanced Menus",advanced_menus},
	    separator,
	    {"One-Button Mouse (Mac)",one_button},
	    {"Two-Button Mouse",two_button},
	    separator,
	    {"OpenGL Info",opengl_info},
	    separator,
	    {"About Wings 3D",about}],
    wings_menu:menu(X, Y, help, Menu).

command(getting_started, _St) ->
    getting_started();
command(third_dimension, _St) ->
    third_dimension();
command(one_button, _St) ->
    one_button();
command(two_button, _St) ->
    two_button();
command(defining_hotkeys, _St) ->
    def_hotkeys();
command(lights, _St) ->
    lights();
command(advanced_menus, _St) ->
    advanced_menus();
command(opengl_info, _St) ->
    opengl_info();
command(about, _St) ->
    about().

getting_started() ->
    Help = ["Getting Started",
	    "When learning Wings, keep an eye at the status line at "
	    "at the bottom of the screen. Generally it shows what the "
	    "mouse buttons will do at any given moment.",

	    "Note that [L] means the left mouse button, not the key L "
	    "on the keyboard. Similarily, [M] means the middle mouse "
	    "button (or the scroll wheel), and [R] means the right mouse "
	    "button.",

	    "See \"One-Button Mouse\" or \"Two-Button Mouse\" "
	    "if your mouse doesn't have three buttons.",

	    "Generally, [L] (left mouse button) is used for selecting and "
	    "accepting, the [M] (middle mouse button) for operating the camera, "
	    "and the [R] (right mouse button) to access the context-sensitive "
	    "pop-up menus."
	   ],
    help_window(Help).

advanced_menus() ->
    Help = ["Advanced Menus",
	    "In the Edit|Preferences dialog box, there is a check box "
	    "for \"Advanced Menus\".",

	    "With Advanced Menus turned on, many menu commands do "
	    "different things depending on which mouse-button you invoke "
	    "them with. For instance, [R] clicking on the Move command "
	    "allow you to specify a direction vector to move along."],
    help_window(Help).

third_dimension() ->
    Help = ["Accessing the Third Dimension",
	    "Some commands, such as Move|Free, "
	    "allow movements in three dimensions.",

	    "To move along the first and second dimensions, simply move "
	    "the mouse left/right and up/down.",

	    "To move along the third dimension, move up/down and hold "
	    "down:",
	    
	    "[Alt]+[M] or [Ctrl]+[R] if you use the default "
	    "Wings/Blender camera mode.",

	    "[M] or [Ctrl]+[R] if you use the Nendo camera mode.",

	    "[Ctrl]+[R] if you use the 3ds max camera mode.",

	    "[M] if you use the Maya camera mode."],
    help_window(Help).

one_button() ->
    Help = ["Using A One-Button Mouse (Mac)",
	    "To use the default Wings/Blender camera mode with an one-button "
	    "mouse, you must first enable \"One-button mouse\" in the "
	    "Edit|Compatibility dialog box. Note that you cannot access the "
	    "third dimension in the Wings/Blender mode.",

	    "To emulate the middle mouse button, hold the [Alt/Option] key.",

	    "To emulate the right mouse button, hold the [Command] key."],
    help_window(Help).

two_button() ->
    Help = ["Using A Two-Button Mouse",
	    "If you are using Wings in the default Wings/Blender "
	    "camera mode, you can use [Alt]+[L] "
	    "instead of [M].",

	    "If you are using Wings in Nendo "
	    "camera mode, you can use [Ctrl]+[R] "
	    "instead of [M].",

	    "The 3ds max and Maya camera modes require a three-button "
	    "mouse.",

	    "In the Advanced Menus (if enabled), "
	    "you can use either [Alt]+[L] "
	    "or [Ctrl]+[R] instead of [L]."],
    help_window(Help).

def_hotkeys() ->
    Help = ["Assigning Hotkeys",
	    "Any command that appears in a menu, can be assigned a "
	    "keyboard short-cut (hotkey).",
	    "To assign a hotkey to a command, open the menu containing "
	    "the command. "
	    "With the command high-lighted, press the [Insert] or [/] key, "
	    "and then press the key you want to assign the command to.",
	    "To delete a hotkey, similarily high-light the command in a "
	    " menu, and press the [Del] or [\\] key."],
    help_window(Help).

lights() ->
    Help = ["Light Basics",
	    "1. Create lights using the Light command in the primitives "
	    "menu ([R]-click when there is no selection).",
	    "2. Select a light by [L]-clicking on it. When any light is "
	    "selected, a special Light menu will pop up when you [R]-click.",
	    "3. To tell Wings to actually use the lights you have created, "
	    "use the View|Scene Lights command."],
    help_window(Help).

opengl_info() ->
    Help = ["Basic OpenGL Info",
	    "Vendor: " ++ gl:getString(?GL_VENDOR) ++ "\n" ++
	    "Renderer: " ++ gl:getString(?GL_RENDERER) ++ "\n" ++
	    "Version: " ++ gl:getString(?GL_VERSION),
	    "Detailed Info",
	    deep_info([{"Red size",?SDL_GL_RED_SIZE},
		       {"Green size",?SDL_GL_GREEN_SIZE},
		       {"Blue size",?SDL_GL_BLUE_SIZE},
		       {"Alpha size",?SDL_GL_ALPHA_SIZE},
		       {"Buffer size",?SDL_GL_BUFFER_SIZE},
		       {"Depth size",?SDL_GL_DEPTH_SIZE},
		       {"Stencil size",?SDL_GL_STENCIL_SIZE},
		       {"Accum. red size",?SDL_GL_ACCUM_RED_SIZE},
		       {"Accum. green size",?SDL_GL_ACCUM_GREEN_SIZE},
		       {"Accum. blue size",?SDL_GL_ACCUM_BLUE_SIZE},
		       {"Accum. alpha size",?SDL_GL_ACCUM_ALPHA_SIZE}])],
    help_window(Help).

deep_info([{Label,Attr}|T]) ->
    Label ++ ": " ++ integer_to_list(sdl_video:gl_getAttribute(Attr)) ++ "\n" ++
	deep_info(T);
deep_info([]) -> [].

about() ->
    Op = {push,fun(Ev) ->
		       handle_help_event(Ev, splash)
	       end},
    Xs = 280,
    Ys = 170+40,
    {W,H} = wings_wm:top_size(),
    X = trunc((W-Xs) / 2),
    Y = trunc((H-Ys) / 2),
    wings_wm:new(help, {X,Y,10}, {Xs,Ys+?LINE_HEIGHT}, Op),
    wings_wm:set_active(help),
    wings_wm:dirty(),
    keep.

help_window(Text) ->
    create_help_window(Text, 0, []).

create_help_window([S|T], Rows, Acc) ->
    break_line(S, T, Rows, Acc);
create_help_window([], Rows, Lines) ->
    Xs = 62*?CHAR_WIDTH,
    Ys = Rows*?LINE_HEIGHT,
    {W,H} = wings_wm:top_size(),
    X = trunc((W-Xs) / 2),
    Y = trunc((H-Ys) / 2),
    DrawData = {reverse(Lines),Xs,Ys},
    Op = {push,fun(Ev) ->
		       handle_help_event(Ev, DrawData)
	       end},
    wings_wm:new(help, {X,Y,10}, {Xs,Ys+?LINE_HEIGHT}, Op),
    wings_wm:set_active(help),
    wings_wm:dirty(),
    keep.

break_line(S, T, Rows, Acc) ->
    case break_line_1(S) of
	done ->
	    create_help_window(T, Rows+1, [[]|Acc]);
	{Line,More} ->
	    break_line(More, T, Rows+1, [Line|Acc])
    end.

break_line_1([$\s|T]) -> break_line_1(T);
break_line_1([]) -> done;
break_line_1(T) -> break_line_2(T, 0, [], []).

break_line_2(_, N, _Acc, {Bef,More}) when N > 60 ->
    {reverse(Bef),More};
break_line_2([$\n|T], _N, Acc, _Break) ->
    {reverse(Acc),T};
break_line_2([$\s|T0], N, Acc, _Break) ->
    T = skip_blanks(T0),
    break_line_2(T, N+1, [$\s|Acc], {Acc,T});
break_line_2([C|T], N, Acc, Break) ->
    break_line_2(T, N+1, [C|Acc], Break);
break_line_2([], _, Acc, _Break) -> {reverse(Acc),[]}.

skip_blanks([$\n|T]) -> skip_blanks(T);
skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks(T) -> T.

handle_help_event(redraw, DrawData) ->
    redraw(DrawData),
    keep;
handle_help_event(#mousemotion{}, _) ->
    keep;
handle_help_event(quit=Ev, _) ->
    wings_io:putback_event(Ev),
    delete;
handle_help_event(_, _) -> delete.

redraw({Lines,Xs,Ys}) ->
    wings_io:ortho_setup(),
    wings_io:raised_rect(0, 0, Xs, Ys),
    gl:color3f(0.0, 0.0, 0.0),
    gl:recti(3, 3, Xs-3, Ys-3),
    gl:color3f(1.0, 1.0, 1.0),
    gl:recti(4, 4, Xs-4, Ys-4),
    gl:color3f(1.0, 0.0, 1.0),
    gl:color3f(0.0, 0.0, 0.0),
    gl:translated(4, 4+?LINE_HEIGHT, 0),
    foldl(fun(L, Y) ->
		  wings_io:text_at(5, Y, L),
		  Y+?LINE_HEIGHT
	  end, 0, Lines);
redraw(splash) ->
    wings_io:ortho_setup(),
    {_,_,Xs,Ys} = wings_wm:viewport(),
    wings_io:raised_rect(0, 0, Xs, Ys),
    gl:color3f(0.0, 0.0, 0.0),
    gl:recti(3, 3, Xs-3, Ys-3),
    gl:color3f(1.0, 1.0, 1.0),
    gl:recti(4, 4, Xs-4, Ys-4),
    gl:color3f(1.0, 0.0, 1.0),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    wings_io:draw_icon(10, 10, 256, 128, wings),
    wings_io:draw_icon(90, 140, 128, 64, powered),
    gl:disable(?GL_TEXTURE_2D),
    gl:color3f(0.0, 0.0, 0.0),
    wings_io:text_at(10, 155, "Wings 3D " ++ ?WINGS_VERSION).
