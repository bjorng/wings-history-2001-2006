%%
%%  wings_help.erl --
%%
%%     This module implements the Help menu.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_help.erl,v 1.44 2003/03/09 11:43:54 bjorng Exp $
%%

-module(wings_help).
-export([menu/1,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3]).

menu(_) ->
    [{"Getting Started",getting_started},
     {"Using a Mouse With One or Two Buttons",one_or_two},
     {"French and German Keyboards",international},
     separator,
     {"Light Basics",lights},
     {"Assigning Hotkeys",defining_hotkeys},
     separator,
     {"Advanced Menus",advanced_menus},
     {"Default Commands",default_commands},
     separator,
     {"OpenGL Info",opengl_info},
     separator,
     {"About Wings 3D",about}].

command(getting_started, _St) ->
    getting_started();
command(one_or_two, _St) ->
    one_or_two();
command(international, _St) ->
    international();
command(defining_hotkeys, _St) ->
    def_hotkeys();
command(default_commands, _St) ->
    def_commands();
command(lights, _St) ->
    lights();
command(advanced_menus, _St) ->
    advanced_menus();
command(opengl_info, _St) ->
    opengl_info();
command(about, _St) ->
    about().

getting_started() ->
    Help = ["When learning Wings, keep an eye at the status line at "
	    "bottom of the screen. Generally it shows what the "
	    "mouse buttons will do at any given moment.",

	    "Note that [L] means the left mouse button, not the key L "
	    "on the keyboard. Similarly, [M] means the middle mouse "
	    "button (or the scroll wheel), and [R] means the right mouse "
	    "button.",

	    "See \"Using a Mouse with One or Two Buttons\" "
	    "if your mouse doesn't have three buttons.",

	    "Generally, [L] (left mouse button) is used for selecting and "
	    "accepting, the [M] (middle mouse button) for operating the camera, "
	    "and the [R] (right mouse button) to access the context-sensitive "
	    "pop-up menus."
	   ],
    help_window("Getting Started", Help).

one_or_two() ->
    Help = ["To use mice with only one or two buttons, "
	    "you must inform Wings how many buttons your mouse has "
	    "in the Edit|Camera Mode dialog box."],
    help_window("Using a mouse with One or Two buttons", Help).

advanced_menus() ->
    Help = ["In the Edit|Advanced Preferences dialog box, there is a check box "
	    "for \"Advanced Menus\".",

	    "With Advanced Menus turned on, many menu commands do "
	    "different things depending on which mouse-button you invoke "
	    "them with. For instance, [R] clicking on the Move command "
	    "allows you to specify a direction vector to move along."],
    help_window("Advanced Menus", Help).

international() ->
    Help = ["Unfortunately, on French and German keyboards (and possibly "
	    "others), the Undo/Redo commands will not be bound to the [Z] "
	    "key. (That might be changed in a future release of Wings.)",

	    "On French keyboards, the Undo/Redo commands are found on the "
	    "[W] key ([Ctrl]+[W], [Ctrl]+[Alt]+[W] and so on).",

	    "On German keyboards, the Undo/Redo commands are found on the "
	    "[Y] key ([Ctrl]+[Y], [Ctrl]+[Alt]+[Y] and so on)."],
    help_window("French And German Keyboards", Help).

def_commands() ->
    Help = ["In the Edit|Advanced Preferences dialog box, you can turn on "
	    "\"Default Commands\".",

	    "Two default commands can be defined. To save the "
	    "previous command that was executed, use one of:",
	    "  [Shift]+[Ctrl]+[L]",
	    "  [Shift]+[Ctrl]+[M]",
	    "To use a command that has been defined this way, "
	    "use one of:",
	    "  [Ctrl]+[L]",
	    "  [Ctrl]+[M]",
	    "Note: When using the 3ds max or Blender camera modes, the second "
	    "default command cannot be used."],
    help_window("Assigning Default Commands", Help).

def_hotkeys() ->
    Help = ["Any command that appears in a menu, can be assigned a "
	    "keyboard short-cut (hotkey).",
	    "To assign a hotkey to a command, open the menu containing "
	    "the command. "
	    "With the command high-lighted, press the [Insert] or [/] key, "
	    "and then press the key you want to assign the command to.",
	    "To delete a hotkey, similarly high-light the command in a "
	    "menu, and press the [Del] or [\\] key."],
    help_window("Assigning Hotkeys", Help).

lights() ->
    Help = ["1. Create lights using the Light command in the primitives "
	    "menu ([R]-click when there is no selection).",
	    "2. Select a light by [L]-clicking on it. When any light is "
	    "selected, a special Light menu will pop up when you [R]-click.",
	    "3. To tell Wings to actually use the lights you have created, "
	    "use the View|Scene Lights command."],
    help_window("Light Basics", Help).

opengl_info() ->
    Help = [
	    "Vendor: " ++ gl:getString(?GL_VENDOR) ++ "\n" ++
	    "Renderer: " ++ gl:getString(?GL_RENDERER) ++ "\n" ++
	    "Version: " ++ gl:getString(?GL_VERSION),
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
		       {"Accum. alpha size",?SDL_GL_ACCUM_ALPHA_SIZE}]),
	    get_info([{"Max lights",?GL_MAX_LIGHTS},
		      {"Max clip planes",?GL_MAX_CLIP_PLANES},
		      {"Max modelview stack depth",?GL_MAX_MODELVIEW_STACK_DEPTH},
		      {"Max projection stack depth",?GL_MAX_PROJECTION_STACK_DEPTH},
		      {"Max texture stack depth",?GL_MAX_TEXTURE_STACK_DEPTH},
		      {"Subpixel bits",?GL_SUBPIXEL_BITS},
		      {"Max 3D texture size",?GL_MAX_3D_TEXTURE_SIZE},
		      {"Max texture size",?GL_MAX_TEXTURE_SIZE}
		      
]),
	    
%%	    "# compressed texture formats: " ++
%%	    integer_to_list(hd(gl:getIntegerv(?GL_NUM_COMPRESSED_TEXTURE_FORMATS))),
	    "OpenGL Extensions",gl:getString(?GL_EXTENSIONS)],
    help_window("OpenGL Info", Help).

deep_info([{Label,Attr}|T]) ->
    Label ++ ": " ++ integer_to_list(sdl_video:gl_getAttribute(Attr)) ++ "\n" ++
	deep_info(T);
deep_info([]) -> [].

get_info([{Label,Attr}|T]) ->
    Val = gl:getIntegerv(Attr),
    ValStr = integer_to_list(hd(Val)),
    Label ++ ": " ++ ValStr ++ "\n" ++ get_info(T);
get_info([]) -> [].

about() ->
    Xs = 280,
    Ys = 176,
    {W,H} = wings_wm:top_size(),
    X = trunc((W-Xs) / 2),
    Y = trunc((H-Ys) / 2),
    Op = {push,fun handle_splash_event/1},
    wings_wm:new(help, {X,Y,highest}, {Xs,Ys+?LINE_HEIGHT}, Op),
    wings_wm:grab_focus(help),
    wings_wm:dirty(),
    keep.

%%%
%%% Scrollable help window.
%%%
-record(ts,
	{lines,
	 first,
	 tw,
	 th
	 }).
	 

help_window(Title, Text) ->
    help_window(help, Title, Text).

help_window(Name, Title, Text) ->
    wings_wm:delete(Name),
    {Rows,Lines} = collect_lines(Text, 0, []),
    {W,H} = wings_wm:top_size(),
    MaxH = trunc(H*0.75),
    Xs = 64*?CHAR_WIDTH,
    Ys = case Rows*?LINE_HEIGHT of
	     Ys0 when Ys0 > MaxH -> MaxH;
	     Ys0 -> Ys0
	 end,
    X = trunc((W-Xs) / 2),
    Y = trunc((H-Ys) / 2),
    Ts = #ts{lines=reverse(Lines),first=0,tw=Xs,th=Ys0},
    Op = {seq,push,get_help_event(Ts)},
    Size = {Xs+?CHAR_WIDTH,Ys+?LINE_HEIGHT},
    wings_wm:toplevel(Name, Title, {X,Y,highest}, Size,
		      [closable,vscroller], Op),
    wings_wm:dirty().

collect_lines([S|T], Rows, Acc) ->
    break_line(S, T, Rows, Acc);
collect_lines([], Rows, Lines) ->
    {Rows,Lines}.

break_line(S, T, Rows, Acc) ->
    case break_line_1(S) of
	done ->
	    collect_lines(T, Rows+1, [[]|Acc]);
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

get_help_event(Ts) ->
    {replace,fun(Ev) ->
		     handle_help_event(Ev, Ts)
	     end}.

handle_help_event(redraw, DrawData) ->
    wings_wm:message("[L] Close help window"),
    redraw(DrawData),
    keep;
handle_help_event(close, _) -> delete;
handle_help_event({set_knob_pos,Pos}, #ts{th=Th}=Ts0) ->
    Ts = Ts0#ts{first=trunc(Th*Pos) div ?LINE_HEIGHT},
    update_scroller(Ts),
    get_help_event(Ts);
handle_help_event(scroll_page_up, Ts) ->
    zoom_step(-10, Ts);
handle_help_event(scroll_page_down, Ts) ->
    zoom_step(10, Ts);
handle_help_event(_, _) -> keep.

zoom_step(Step, #ts{first=First0}=Ts0) ->
    First = case First0+Step of
		Neg when Neg < 0 -> 0;
		First1 -> First1
	    end,
    Ts = Ts0#ts{first=First},
    update_scroller(Ts),
    get_help_event(Ts).

redraw(#ts{lines=Lines0,first=First}=Ts) ->
    update_scroller(Ts),
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, {1.0,1.0,1.0}),
    gl:color3f(0.0, 0.0, 0.0),
    gl:translated(4, 4+?LINE_HEIGHT, 0),
    Lines = lists:nthtail(First, Lines0),
    foldl(fun(L, Y) ->
		  wings_io:text_at(5, Y, L),
		  Y+?LINE_HEIGHT
	  end, 0, Lines).

update_scroller(#ts{first=First,th=Th}) ->
    {_,H} = wings_wm:win_size(),
    Name = wings_wm:active_window(),
    wings_wm:set_knob(Name, First*?LINE_HEIGHT/Th, H/Th).

%%%
%%% Help|About (splash screen).
%%%

handle_splash_event(redraw) ->
    wings_wm:message("[L] Close help window"),
    wings_io:ortho_setup(),
    {Xs,Ys} = wings_wm:win_size(),
    wings_io:raised_rect(0, 0, Xs, Ys),
    gl:color3f(0.0, 0.0, 0.0),
    gl:recti(3, 3, Xs-3, Ys-3),
    gl:color3f(1.0, 1.0, 1.0),
    gl:recti(4, 4, Xs-4, Ys-4),
    gl:color3f(1.0, 0.0, 1.0),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    wings_io:draw_icon(10, 10, 256, 128, wings),
    gl:disable(?GL_TEXTURE_2D),
    gl:color3f(0.0, 0.0, 0.0),
    wings_io:text_at(10, 155, "Wings 3D " ++ ?WINGS_VERSION),
    wings_io:text_at(10, 180, "http://www.wings3d.com"),
    keep;
handle_splash_event(#mousemotion{}) -> keep;
handle_splash_event(got_focus) -> keep;
handle_splash_event(lost_focus) -> keep;
handle_splash_event(_) -> delete.
