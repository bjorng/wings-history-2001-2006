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
%%     $Id: wings_help.erl,v 1.60 2003/12/06 08:34:36 bjorng Exp $
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
     {"Defined Hotkeys",hotkeys},
     {"How To Define Hotkeys",defining_hotkeys},
     separator,
     {"Light Basics",lights},
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
command(hotkeys, _St) ->
    hotkeys();
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
    Help = ["When learning Wings, keep an eye at the information line at "
	    "bottom of the screen. Generally it shows what the "
	    "mouse buttons will do at any given moment.",

	    "The actions for the mouse buttons are given first, "
	    "labeled L: (left mouse button), "
	    "M: (middle button or scroll wheel), R: (right button).",

	    "To use mice with only one or two buttons, "
	    "you must inform Wings how many buttons your mouse has "
	    "in the Edit|Preferences dialog box.",

	    "Generally, L (left mouse button) is used for selecting and "
	    "accepting, M (middle mouse button) for operating the camera, "
	    "and R (right mouse button) to access the context-sensitive "
	    "pop-up menus."
	   ],
    help_window("Getting Started", Help).

one_or_two() ->
    Help = ["To use mice with only one or two buttons, "
	    "you must inform Wings how many buttons your mouse has "
	    "in the Edit|Preferences dialog box.",

	    "Note that only the Nendo and Blender modes can be "
	    "used with a two-button mouse. Only the Nendo mode can "
	    "be used with one-button mouse."],
    help_window("Using a mouse with One or Two buttons", Help).

advanced_menus() ->
    Help = ["In the Edit|Preferences dialog box, there is a check box "
	    "for \"Advanced Menus\".",

	    "Activating advanced menus provide the following additional features:",

	    "New commands: Face|Put On and Face|Lift",

	    "Vector based operations: The means to specify an axis (or vector) "
	    "and to be able to re-locate it so it passes though a new point.",

	    "Magnet operations: A facility that provides a way of attaining "
	    "smooth modifications / transitions to surrounding geometry during "
	    "the operation of any valid tool. Magnet type and influence radius "
	    "settings provide further control options.",

	    "Vector and magnet operations can be combined.",

	    "With advanced menus turned on, many menu commands do "
	    "different things depending on which mouse button you invoke "
	    "them with.",

	    "For instance, R clicking on the Rotate command allows "
	    "you to specify an axis (or vector) to rotate your "
	    "selection around, while M clicking lets you "
	    "define an axis - and a new point through which that axis "
	    "will pass. (A vector does not have to be parallel to "
	    "an x, y or z axis.)",

	    "Reading the contents of the info line at the bottom of "
	    "the Wings window is highly recommended when "
	    "using advanced menus."],
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
    Help = ["In the Edit|Preferences dialog box, you can turn on "
	    "\"Default Commands\".",

	    "Two default commands can be defined. To save the "
	    "previous command that was executed, use one of:",
	    "  [Shift]+[Ctrl]+L:",
	    "  [Shift]+[Ctrl]+M:",
	    "To use a command that has been defined this way, "
	    "use one of:",
	    "  [Ctrl]+L:",
	    "  [Ctrl]+M:",
	    "Note: When using the 3ds max or Blender camera modes, the second "
	    "default command cannot be used."],
    help_window("Assigning Default Commands", Help).

hotkeys() ->
    Help = wings_hotkey:listing(),
    help_window("Defined Hotkeys", Help).

def_hotkeys() ->
    Help = ["Any command that appears in a menu, can be assigned a "
	    "keyboard short-cut (hotkey).",
	    "To assign a hotkey to a command, open the menu containing "
	    "the command. "
	    "With the command high-lighted, press the [Insert] or [/] key, "
	    "and then press the key you want to assign the command to.",
	    "To delete a hotkey, similarly high-light the command in a "
	    "menu, and press the [Del] or [\\] key."],
    help_window("How To Define Hotkeys", Help).

lights() ->
    Help = ["1. Create lights using the Light command in the primitives "
	    "menu (R-click when there is no selection).",
	    "2. Select a light by L-clicking on it. When any light is "
	    "selected, a special Light menu will pop up when you R-click.",
	    "3. To tell Wings to actually use the lights you have created, "
	    "use the View|Scene Lights command."],
    help_window("Light Basics", Help).

opengl_info() ->
    gl:getError(),			%Clear any previous error.
    Help = [
	    "Vendor: " ++ gl:getString(?GL_VENDOR) ++ "\n" ++
	    "Renderer: " ++ gl:getString(?GL_RENDERER) ++ "\n" ++
	    "Version: " ++ gl:getString(?GL_VERSION),
	    get_info([{"Red bits",?GL_RED_BITS},
		      {"Green bits",?GL_GREEN_BITS},
		      {"Blue bits",?GL_BLUE_BITS},
		      {"Alpha bits",?GL_ALPHA_BITS},
		      {"Depth bits",?GL_DEPTH_BITS},
		      {"Stencil bits",?GL_STENCIL_BITS},
		      {"Accum. red bits",?GL_ACCUM_RED_BITS},
		      {"Accum. green bits",?GL_ACCUM_GREEN_BITS},
		      {"Accum. blue bits",?GL_ACCUM_BLUE_BITS},
		      {"Accum. alpha bits",?GL_ACCUM_ALPHA_BITS},
		      {"Max number of lights",?GL_MAX_LIGHTS},
		      {"Max clip planes",?GL_MAX_CLIP_PLANES},
		      {"Max modelview stack depth",?GL_MAX_MODELVIEW_STACK_DEPTH},
		      {"Max projection stack depth",?GL_MAX_PROJECTION_STACK_DEPTH},
		      {"Max texture stack depth",?GL_MAX_TEXTURE_STACK_DEPTH},
		      {"Subpixel bits",?GL_SUBPIXEL_BITS},
		      {"Max 3D texture size",?GL_MAX_3D_TEXTURE_SIZE},
		      {"Max texture size",?GL_MAX_TEXTURE_SIZE},
		      {"Max pixel map table",?GL_MAX_PIXEL_MAP_TABLE},
		      {"Max name stack depth",?GL_MAX_NAME_STACK_DEPTH},
		      {"Max display-list call nesting",?GL_MAX_LIST_NESTING},
		      {"Max evaluator polynomial order",?GL_MAX_EVAL_ORDER},
		      {"Max viewport dimensions",?GL_MAX_VIEWPORT_DIMS},
		      {"Max depth of attribute stack",?GL_MAX_ATTRIB_STACK_DEPTH},
		      {"Max depth of client attribute stack",
		       ?GL_MAX_CLIENT_ATTRIB_STACK_DEPTH},
		      {"Number of auxiliary buffers",?GL_AUX_BUFFERS},
		      {"Color buffers store RGBA",?GL_RGBA_MODE},
		      {"Color buffers store indices",?GL_INDEX_MODE},
		      {"Double buffering",?GL_DOUBLEBUFFER},
		      {"Stereo buffers",?GL_STEREO},
		      {"Range of aliased point sizes",?GL_ALIASED_POINT_SIZE_RANGE},
		      {"Range of antialised point sizes",?GL_SMOOTH_POINT_SIZE_RANGE},
		      {"Range of aliased line widths",?GL_ALIASED_LINE_WIDTH_RANGE},
		      {"Range of antialised line widths",?GL_SMOOTH_LINE_WIDTH_RANGE},
		      {"Recommended max number of indices for drawRangeElement()",
		       ?GL_MAX_ELEMENTS_INDICES},
		      {"Recommended max number of vertices for drawRangeElement()",
		       ?GL_MAX_ELEMENTS_VERTICES}]),
	    get_info([{"Max number of texturing units",?GL_MAX_TEXTURE_UNITS},
		      {"Number of compression formats",
		       ?GL_NUM_COMPRESSED_TEXTURE_FORMATS},
		      {"Max number of vertex units",?GL_MAX_VERTEX_UNITS_ARB}]),
	    "OpenGL Extensions",extensions()],
    help_window("OpenGL Info", Help).

get_info([{Label,Attr}|T]) ->
    Val = gl:getIntegerv(Attr),
    ValStr = case {gl:getError(),Val} of
		 {0,List} ->
		     get_info_1(Attr, List);
		 _ -> "---"
	     end,
    Label ++ ": " ++ ValStr ++ "\n" ++ get_info(T);
get_info([]) -> [].

get_info_1(_, [A]) -> integer_to_list(A);
get_info_1(Enum, [A,B|_]) ->
    case has_one_elem(Enum) of
	false -> integer_to_list(A) ++ ", " ++ integer_to_list(B);
	true -> integer_to_list(A)
    end.

has_one_elem(?GL_MAX_ELEMENTS_VERTICES) -> true;
has_one_elem(?GL_MAX_ELEMENTS_INDICES) -> true;
has_one_elem(?GL_NUM_COMPRESSED_TEXTURE_FORMATS) -> true;
has_one_elem(?GL_MAX_VERTEX_UNITS_ARB) -> true;
has_one_elem(?GL_MAX_3D_TEXTURE_SIZE) -> true;
has_one_elem(_) -> false.

extensions() ->
    extensions(lists:sort(string:tokens(gl:getString(?GL_EXTENSIONS), " "))).

extensions([H|T]) -> H ++ "\n" ++ extensions(T);
extensions([]) -> [].

%%%
%%% Scrollable help window.
%%%

-record(ts,
	{lines,
	 first,
	 tw,
	 th,
	 wh
	 }).

help_window(Title, []) ->
    help_window(Title, ["No help text"]);
help_window(Title, Text) ->
    help_window(help, Title, Text).

help_window(Name, Title, Text) ->
    wings_wm:delete(Name),
    {Rows,Lines} = wings_text:break_lines(Text, 60),
    {W,H} = wings_wm:top_size(),
    MaxH = trunc(H*0.75),
    Xs = 64*?CHAR_WIDTH,
    Ys = case Rows*?LINE_HEIGHT of
	     Ys0 when Ys0 > MaxH -> MaxH;
	     Ys0 -> Ys0
	 end,
    X = trunc((W-Xs) / 2),
    Y = trunc((H-Ys) / 2),
    Ts = #ts{lines=Lines,first=0,tw=Xs,th=Ys0,wh=Ys},
    Op = {seq,push,get_help_event(Ts)},
    Size = {Xs+?CHAR_WIDTH,Ys+?LINE_HEIGHT},
    wings_wm:toplevel(Name, Title, {X,Y,highest}, Size,
		      [closable,vscroller], Op),
    wings_wm:dirty().

get_help_event(Ts) ->
    {replace,fun(Ev) ->
		     handle_help_event(Ev, Ts)
	     end}.

handle_help_event(redraw, DrawData) ->
    redraw(DrawData),
    keep;
handle_help_event(close, _) -> delete;
handle_help_event({set_knob_pos,Pos}, #ts{th=Th}=Ts0) ->
    Ts = Ts0#ts{first=trunc(Th*Pos) div ?LINE_HEIGHT},
    update_scroller(Ts),
    get_help_event(Ts);
handle_help_event(scroll_page_up, #ts{wh=Wh}=Ts) ->
    zoom_step(-Wh div ?LINE_HEIGHT, Ts);
handle_help_event(scroll_page_down, #ts{wh=Wh}=Ts) ->
    zoom_step(Wh div ?LINE_HEIGHT, Ts);
handle_help_event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-10, Ost);
handle_help_event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(10, Ost);
handle_help_event(_, _) -> keep.

zoom_step(Step, #ts{first=First0,th=Th}=Ts0) ->
    NumLines = Th div ?LINE_HEIGHT,
    First = case First0+Step of
		Neg when Neg < 0 -> 0;
		First1 when First1 < NumLines -> First1;
		_ -> First0
	    end,
    Ts = Ts0#ts{first=First},
    update_scroller(Ts),
    get_help_event(Ts).

redraw(#ts{lines=Lines0,first=First}=Ts) ->
    update_scroller(Ts),
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, {1.0,1.0,1.0}),
    gl:translated(4, 4+?LINE_HEIGHT, 0),
    Lines = lists:nthtail(First, Lines0),
    foldl(fun(L, Y) ->
		  wings_io:text_at(5, Y, L),
		  Y+?LINE_HEIGHT
	  end, 0, Lines).

update_scroller(#ts{first=First,th=Th}) ->
    {_,H} = wings_wm:win_size(),
    Name = wings_wm:this(),
    wings_wm:set_knob(Name, First*?LINE_HEIGHT/Th, H/Th).

%%%
%%% Help|About (splash screen).
%%%

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

handle_splash_event(redraw) ->
    message(),
    wings_io:ortho_setup(),
    {Xs,Ys} = wings_wm:win_size(),
    wings_io:raised_rect(0, 0, Xs, Ys),
    gl:recti(3, 3, Xs-3, Ys-3),
    gl:color3f(1, 1, 1),
    gl:recti(4, 4, Xs-4, Ys-4),
    gl:color3f(1, 0, 1),
    wings_io:draw_icons(fun() -> wings_io:draw_icon(10, 10, wings) end),
    gl:color3b(0, 0, 0),
    wings_io:text_at(10, 155, "Wings 3D " ++ ?WINGS_VERSION),
    wings_io:text_at(10, 180, "http://www.wings3d.com"),
    keep;
handle_splash_event(#mousemotion{}) -> keep;
handle_splash_event(got_focus) -> message();
handle_splash_event(lost_focus) -> keep;
handle_splash_event(_) -> delete.

message() ->
    wings_util:button_message("Close help window"),
    keep.
