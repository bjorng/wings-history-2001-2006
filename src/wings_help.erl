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
%%     $Id: wings_help.erl,v 1.10 2002/04/11 16:12:04 bjorng Exp $
%%

-module(wings_help).
-export([menu/3,command/2,about/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3]).

menu(X, Y, St) ->
    Menu = [{"Two-Button Mouse",two_button},
	    {"Assigning Hotkeys",defining_hotkeys},
	    separator,
	    {"About",about}],
    wings_menu:menu(X, Y, help, Menu, St).

command(two_button, St) ->
    two_button(St);
command(defining_hotkeys, St) ->
    def_hotkeys(St);
command(about, St) ->
    about(St).

two_button(St) ->
    Help = ["Using A Two-Button Mouse",
	    "If you are using Wings in the default Wings/Blender "
	    "camera mode, you can use [Alt]+" ++ [lmb] ++
	    " instead of " ++ [mmb] ++ ".",

	    "If you are using Wings in Nendo "
	    "camera mode, you can use [Ctrl]+" ++ [rmb] ++
	    "instead of the " ++ [mmb] ++ ".",

	    "The 3ds max and Maya camera modes require a three-button "
	    "mouse.",

	    "In the Advanced Menus (if enabled), "
	    "you can use either [Alt]+" ++ [lmb] ++
	    " or [Ctrl]+" ++ [rmb] ++ " instead of " ++ [lmb] ++ "."],
    help_window(Help, St).

def_hotkeys(St) ->
    Help = ["Assigning Hotkeys",
	    "Any command that appears in a menu, can be assigned a "
	    "keyboard short-cut (hotkey).",
	    "To assign a hotkey to a command, open the menu containing "
	    "the command. "
	    "With the command high-lighted, press the [Insert] key, "
	    "and then press the key you want to assign the command to.",
	    "To delete a hotkey, similarily high-light the command in a "
	    " menu, and press the [Del] key."],
    help_window(Help, St).

about(St) ->
    Redraw = fun show_splash/2,
    wait_for_click({Redraw,St}).

wait_for_click(Redraw) ->
    {seq,{push,dummy},get_click_event(Redraw)}.

get_click_event(S) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> wait_click_handler(Ev, S) end}.
		     
wait_click_handler(redraw, {Redraw,St}) ->
    redraw(St),
    wings_io:ortho_setup(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Redraw(W, H),
    keep;
wait_click_handler(#mousemotion{}, _) -> keep;
wait_click_handler({resize,_,_}=Ev, _) ->
    wings_io:putback_event(Ev),
    pop;
wait_click_handler(quit=Ev, _) ->
    wings_io:putback_event(Ev),
    pop;
wait_click_handler(_, _) ->
    wings_wm:dirty(),
    pop.

show_splash(W, H) ->
    Xs = 280,
    Ys = 170+40,
    gl:translated((W-Xs) / 2, (H-Ys) / 2, 0.0),
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


help_window(Txt, St) ->
    Redraw = fun(_, _) -> help_window(Txt) end,
    wait_for_click({Redraw,St}).

help_window(Txt) ->
    help_window(Txt, 0, []).

help_window([S|T], Rows, Acc) ->
    break_line(S, T, Rows, Acc);
help_window([], Rows, Lines) ->
    Ys = Rows*?LINE_HEIGHT,
    Xs = 62*?CHAR_WIDTH,
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    gl:translated((W-Xs) / 2, (H-Ys) / 2, 0.0),
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
	  end, 0, reverse(Lines)).

break_line(S, T, Rows, Acc) ->
    case break_line_1(S) of
	done ->
	    help_window(T, Rows+1, [[]|Acc]);
	{Line,More} ->
	    break_line(More, T, Rows+1, [Line|Acc])
    end.

break_line_1([$\s|T]) -> break_line_1(T);
break_line_1([]) -> done;
break_line_1(T) -> break_line_2(T, 0, [], []).

break_line_2(_, N, _Acc, {Bef,More}) when N > 60 ->
    {reverse(Bef),More};
break_line_2([$\s|T0], N, Acc, _Break) ->
    T = skip_blanks(T0),
    break_line_2(T, N+1, [$\s|Acc], {Acc,T});
break_line_2([C|T], N, Acc, Break) ->
    break_line_2(T, N+1, [C|Acc], Break);
break_line_2([], _, Acc, _Break) -> {reverse(Acc),[]}.

skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks(T) -> T.

redraw(St0) ->
    St = wings_draw:render(St0),
    wings_io:update(St).
