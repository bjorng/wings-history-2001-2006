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
%%     $Id: wings_help.erl,v 1.8 2002/03/19 09:19:30 bjorng Exp $
%%

-module(wings_help).
-export([menu/3,command/2,about/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

menu(X, Y, St) ->
    Menu = [{"Defining Hotkeys",defining_hotkeys},
	    separator,
	    {"About",about}],
    wings_menu:menu(X, Y, help, Menu, St).

command(defining_hotkeys, St) ->
    def_hotkeys(St);
command(about, St) ->
    about(St).

def_hotkeys(St) ->
    Redraw = fun def_hotkeys/2,
    wait_for_click({Redraw,St}).

about(St) ->
    Redraw = fun show_splash/2,
    wait_for_click({Redraw,St}).

wait_for_click(Redraw) ->
    {seq,{push,dummy},get_click_event(Redraw)}.

get_click_event({Redraw,St}=S) ->
    redraw(St),
    wings_io:ortho_setup(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Redraw(W, H),
    wings_io:swap_buffers(),
    {replace,fun(Ev) -> wait_click_handler(Ev, S) end}.
		     
wait_click_handler(#mousemotion{state=?SDL_RELEASED}, _) -> keep;
wait_click_handler({resize,_,_}=Ev, _) ->
    wings_io:putback_event(Ev),
    pop;
wait_click_handler(quit=Ev, _) ->
    wings_io:putback_event(Ev),
    pop;
wait_click_handler(_, _) ->
    wings_io:putback_event(redraw),
    pop.

def_hotkeys(_W, _H) ->
    show_window(60*?CHAR_WIDTH, 7*?LINE_HEIGHT),
    wings_io:text_at(0, 0, "How To Define Hotkeys"),
    wings_io:text_at(0, 2*?LINE_HEIGHT,
		     "In any menu, press the Insert key, then press the key"),
    wings_io:text_at(0, 3*?LINE_HEIGHT,
		     "you want to bind the command to."),
    wings_io:text_at(0, 5*?LINE_HEIGHT,
		     "To delete a hotkey, press the Del key in a menu.").

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

show_window(Xs, Ys) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
%     Xs = 280,
%     Ys = 170+40,
    gl:translated((W-Xs) / 2, (H-Ys) / 2, 0.0),
    wings_io:raised_rect(0, 0, Xs, Ys),
    gl:color3f(0.0, 0.0, 0.0),
    gl:recti(3, 3, Xs-3, Ys-3),
    gl:color3f(1.0, 1.0, 1.0),
    gl:recti(4, 4, Xs-4, Ys-4),
    gl:color3f(1.0, 0.0, 1.0),
    gl:color3f(0.0, 0.0, 0.0),
    gl:translated(4, 4+?LINE_HEIGHT, 0).

redraw(St0) ->
    St = wings_draw:render(St0),
    wings_io:update(St).
