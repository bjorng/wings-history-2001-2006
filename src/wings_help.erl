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
%%     $Id: wings_help.erl,v 1.30 2002/11/23 20:34:32 bjorng Exp $
%%

-module(wings_help).
-export([menu/3,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3]).

menu(X, Y, _) ->
    Menu = [{"Getting Started...",getting_started},
	    {"Using a Mouse with One or Two Buttons...",one_or_two},
	    {"French And German Keyboards...",international},
	    separator,
	    {"Light Basics...",lights},
	    {"Assigning Hotkeys...",defining_hotkeys},
	    {"Advanced Menus...",advanced_menus},
	    separator,
	    {"OpenGL Info...",opengl_info},
	    separator,
	    {"About Wings 3D...",about}],
    wings_menu:menu(X, Y, help, Menu).

command(getting_started, _St) ->
    getting_started();
command(one_or_two, _St) ->
    one_or_two();
command(international, _St) ->
    international();
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

	    "See \"Using a Mouse with One or Two Buttons\" "
	    "if your mouse doesn't have three buttons.",

	    "Generally, [L] (left mouse button) is used for selecting and "
	    "accepting, the [M] (middle mouse button) for operating the camera, "
	    "and the [R] (right mouse button) to access the context-sensitive "
	    "pop-up menus."
	   ],
    help_window(Help).

one_or_two() ->
    Help = ["Using a mouse with One or Two buttons",
	    "To use mice with only one or two buttons, "
	    "you must inform Wings how many buttons your mouse has "
	    "in the Edit|Camera Mode dialog box."],
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

international() ->
    Help = ["French And German Keyboards",
	    "Unfortunately, on French and German keyboards (and possibly "
	    "others), the Undo/Redo commands will not be bound to the [Z] "
	    "key. (That might be changed in a future release of Wings.)",

	    "On French keyboards, the Undo/Redo commands are found on the"
	    "[W] key ([Ctrl]+[W], [Ctrl]+[Alt]+[W] and so on).",

	    "On German keyboards, the Undo/Redo commands are found on the"
	    "[Y] key ([Ctrl]+[Y], [Ctrl]+[Alt]+[Y] and so on)."],
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
    wings_wm:grab_focus(help),
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
    wings_wm:grab_focus(help),
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
    wings_io:text_at(10, 155, "Wings 3D " ++ ?WINGS_VERSION),
    wings_io:text_at(10, 215, "http://www.wings3d.com").
    

