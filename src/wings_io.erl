%%
%%  wings_io.erl --
%%
%%     This module contains most of the low-level GUI for Wings.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_io.erl,v 1.109 2003/07/03 14:44:34 bjorng Exp $
%%

-module(wings_io).
-export([init/0,resize/0,
	 set_cursor/1,arrow/0,hourglass/0,
	 info/1,
	 blend/2,
	 border/5,border/6,sunken_rect/5,raised_rect/4,raised_rect/5,
	 text_at/2,text_at/3,text/1,menu_text/3,space_at/2,
	 draw_icons/1,draw_icon/3,draw_char/1,
	 set_color/1]).
-export([putback_event/1,putback_event_once/1,get_event/0,get_matching_events/1,
	 poll_event/0,set_timer/2,cancel_timer/1]).

-export([reset_grab/0,grab/0,ungrab/2,is_grabbed/0,warp/2]).
-export([ortho_setup/0,ortho_setup/1]).

-compile(inline).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [flatmap/2,foldl/3,keysearch/3,member/2,
		reverse/1,reverse/2,foreach/2,last/1]).

-define(ICON_WIDTH, 32).
-define(ICON_HEIGHT, 28).

-define(TX_WIDTH, 256).
-define(TX_HEIGHT, 128).

-record(io,
	{eq,					%Event queue.
	 tex=[],				%Textures.
	 tx=0,					%Active texture.
	 grab_count=0,				%Number of grabs.
	 cursors,				%Mouse cursors.
	 raw_icons				%Raw icon bundle.
	}).

init() ->
    Cursors = build_cursors(),
    Icons = read_icons(),
    put_state(#io{eq=queue:new(),raw_icons=Icons,cursors=Cursors}).

hourglass() ->
    set_cursor(hourglass).

arrow() ->
    set_cursor(arrow).

set_cursor(Cursor) ->
    #io{cursors=Cursors} = get_state(),
    set_cursor_1(Cursors, Cursor).

set_cursor_1([{Name,none}|_], Name) ->
    ok;
set_cursor_1([{Name,Cursor}|_], Name) ->
    sdl_mouse:setCursor(Cursor);
set_cursor_1([_|Cs], Name) ->
    set_cursor_1(Cs, Name).

read_icons() ->
    IconFile = filename:join([wings:root_dir(),"ebin","wings_icon.bundle"]),
    case file:read_file(IconFile) of
	{ok,Bin} -> Bin;
	{error,enoent} ->
	    IconFile2 = filename:join([wings:root_dir(),"wings_icon.bundle"]),
	    {ok,Bin} = file:read_file(IconFile2),
	    Bin
    end.

resize() ->
    #io{raw_icons=RawIcons} = Io = get_state(),
    Tex = load_textures(RawIcons),
    put_state(Io#io{tex=Tex}),
    make_font_dlists().

make_font_dlists() ->
    Base = gl:genLists(256),
    make_font_dlists(0, Base),
    gl:listBase(Base).

make_font_dlists(256, _) -> ok;
make_font_dlists(C, Base) ->
    gl:newList(Base+C, ?GL_COMPILE),
    catch wings_text:char(C),
    gl:endList(),
    make_font_dlists(C+1, Base).

info(Info) ->
    ortho_setup(),
    blend(wings_pref:get_value(info_background_color),
	  fun(Color) ->
		  set_color(Color),
		  N = info_lines(Info),
		  {W,_} = wings_wm:win_size(),
		  gl:recti(0, 0, W, N*?LINE_HEIGHT)
	  end),
    set_color(wings_pref:get_value(info_color)),
    text_at(4, ?CHAR_HEIGHT, Info).

info_lines(Info) ->
    info_lines_1(Info, 1).

info_lines_1([$\n|T], Lines) ->
    info_lines_1(T, Lines+1);
info_lines_1([H|T], Lines) when is_list(H) ->
    info_lines_1(T, info_lines_1(H, Lines));
info_lines_1([_|T], Lines) ->
    info_lines_1(T, Lines);
info_lines_1([], Lines) -> Lines.

blend({_,_,_,0.0}, _) -> ok;
blend({_,_,_,1.0}=Color, Draw) -> Draw(Color);
blend(Color, Draw) ->
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    Draw(Color),
    gl:disable(?GL_BLEND).

border(X, Y, W, H, FillColor) ->
    border(X, Y, W, H, FillColor, {0.20,0.20,0.20}).

border(X0, Y0, Mw, Mh, FillColor, BorderColor)
  when is_integer(X0), is_integer(Y0), is_integer(Mw), is_integer(Mh) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    set_color(FillColor),
    gl:rectf(X0, Y0, X0+Mw, Y0+Mh),
    set_color(BorderColor),
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2f(X, Y+Mh),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+Mw, Y),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:'end'(),
    gl:color3b(0, 0, 0).

set_color({_,_,_}=RGB) -> gl:color3fv(RGB);
set_color({_,_,_,_}=RGBA) -> gl:color4fv(RGBA).

raised_rect(X, Y, Mw, Mh) ->
    raised_rect(X, Y, Mw, Mh, ?PANE_COLOR).

raised_rect(X, Y, Mw, Mh, FillColor) ->
    sunken_rect(X+Mw, Y+Mh, -Mw, -Mh, FillColor).

sunken_rect(X0, Y0, Mw0, Mh0, FillColor) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    Mw = Mw0 + 0.5,
    Mh = Mh0 + 0.5,
    set_color(FillColor),
    gl:rectf(X0, Y0, X0+Mw0, Y0+Mh0),
    gl:'begin'(?GL_LINES),
    set_color(?BEVEL_LOWLIGHT),
    gl:vertex2f(X, Y+Mh),
    gl:vertex2f(X, Y),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+Mw, Y),
    set_color(?BEVEL_HIGHLIGHT),
    gl:vertex2f(X+Mw, Y),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:vertex2f(X, Y+Mh),
    gl:'end'(),
    gl:color3b(0, 0, 0).

space_at(X, Y) ->
    set_color(?PANE_COLOR),
    gl:recti(X, Y-?LINE_HEIGHT+3, X+?CHAR_WIDTH, Y+3),
    gl:color3b(0, 0, 0).

text_at(X, S) ->
    setup_scissor(fun() -> text_at_1(X, S) end).

text_at_1(X, S) ->
    gl:rasterPos2i(X, 0),
    case catch text(S, []) of
	{newline,More} -> text_at(X, More);
	Other -> Other
    end.

text_at(X, Y, S) ->
    setup_scissor(fun() -> text_at_1(X, Y, S) end).

text_at_1(X, Y, S) ->
    gl:rasterPos2i(X, Y),
    case catch text(S, []) of
	{newline,More} -> text_at(X, Y+?LINE_HEIGHT, More);
	Other -> Other
    end.

text(S) ->
    setup_scissor(fun() -> text_1(S) end).

text_1(S) ->
    case catch text(S, []) of
	{newline,More} -> text(More);
	Other -> Other
    end.

text([$\n|Cs], []) ->
    throw({newline,Cs});
text([$\n|Cs], Acc) ->
    draw_reverse(Acc),
    throw({newline,Cs});
text([C|Cs], Acc) when is_integer(C) ->
    text(Cs, [C|Acc]);
text([Atom|Cs], Acc) when is_atom(Atom) ->
    draw_reverse(Acc),
    wings_text:char(Atom),
    text(Cs, []);
text([L|Cs], Acc) when is_list(L) ->
    draw_reverse(Acc),
    text(L, []),
    text(Cs, []);
text([], Acc) -> draw_reverse(Acc).

setup_scissor(DrawText) ->
    case wings_util:is_gl_restriction(broken_scissor) of
	true ->
	    %% Scissor cannot clip text, but slows down text drawing.
	    DrawText();
	false ->
	    {X,Y,W,H} = wings_wm:viewport(),
	    gl:scissor(X, Y, W, H),
	    gl:enable(?GL_SCISSOR_TEST),
	    DrawText(),
	    gl:disable(?GL_SCISSOR_TEST)
    end.

menu_text(X, Y, S) ->
    gl:rasterPos2i(X, Y),
    menu_text(S, []).

menu_text([Atom|Cs], Acc) when is_atom(Atom) ->
    draw_reverse(Acc),
    wings_text:char(Atom),
    menu_text(Cs, []);
menu_text([$&,C|T], Acc) when is_integer(C), C < 256 ->
    menu_text(T, [$_,8,C|Acc]);
menu_text([C|T], Acc) when is_integer(C), C < 256 ->
    menu_text(T, [C|Acc]);
menu_text([L|Cs], Acc) when is_list(L) ->
    draw_reverse(Acc),
    menu_text(L, []),
    menu_text(Cs, []);
menu_text([], Acc) -> draw_reverse(Acc).

draw_reverse([]) -> ok;
draw_reverse(S0) ->
    S = reverse(S0),
    case wings_pref:get_value(text_display_lists, false) of
	true -> gl:callLists(length(S), ?GL_UNSIGNED_BYTE, S);
	false -> wings_text:draw(S)
    end.

ortho_setup() ->
    gl:color3b(0, 0, 0),
    ortho_setup_1().

ortho_setup(Color) ->
    set_color(Color),
    ortho_setup_1().

ortho_setup_1() ->
    ?CHECK_ERROR(),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_DEPTH_TEST),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    {_,_,W,H} = wings_wm:viewport(),
    glu:ortho2D(0, W, H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity().

get_state() ->
    get(wings_io).

put_state(Io) ->
    put(wings_io, Io).

draw_icons(Body) ->
    Io0 = get_state(),
    put_state(Io0#io{tx=0}),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    Body(),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D),
    Io = get_state(),
    put_state(Io#io{tx=0}).

draw_icon(X, Y, Icon) ->
    #io{tex=Tex,tx=Tx} = Io = get_state(),
    case keysearch(Icon, 1, Tex) of
	false -> ok;
	{value,{Icon,{Id,W,H,MinU,MinV,MaxU,MaxV}}} ->
	    case Id of
		Tx -> ok;
		_ ->
		    gl:bindTexture(?GL_TEXTURE_2D, Id),
		    put_state(Io#io{tx=Id})
	    end,
	    gl:'begin'(?GL_QUADS),
	    gl:texCoord2f(MinU, MaxV),
	    gl:vertex2i(X, Y),
	    gl:texCoord2f(MinU, MinV),
	    gl:vertex2i(X, Y+H),
	    gl:texCoord2f(MaxU, MinV),
	    gl:vertex2i(X+W, Y+H),
	    gl:texCoord2f(MaxU, MaxV),
	    gl:vertex2i(X+W, Y),
	    gl:'end'()
    end.

draw_char({A,B,C,D,E,F,Bitmap}) ->
    gl:bitmap(A, B, C, D, E, F, Bitmap).

load_textures(Bin) ->
    case catch binary_to_term(Bin) of
	{'EXIT',_} -> [];
	Icons0 ->
	    gl:pushAttrib(?GL_TEXTURE_BIT),
	    Icons1 = create_buttons(Icons0),
	    Icons = lists:keysort(2, Icons1),
	    Tex = create_textures(Icons),
	    gl:popAttrib(),
	    Tex
    end.

create_textures(Icons) ->
    [TxId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    Mem = sdl_util:alloc(3*?TX_WIDTH*?TX_HEIGHT, ?GL_BYTE),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
		  ?TX_WIDTH, ?TX_HEIGHT, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    create_textures_1(Icons, TxId, 0, 0, 0).

create_textures_1([{_,{W,H,_}}|_]=Icons, Id, U, V, RowH)
  when W =< 32, H =< 32, U+W > ?TX_WIDTH ->
    create_textures_1(Icons, Id, 0, V+RowH, 0);
create_textures_1([{Name,{W,H,Icon}}|T], Id, U, V, RowH0)
  when W =< 32, H =< 32 ->
    gl:texSubImage2D(?GL_TEXTURE_2D, 0, U, V,
		     W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Icon),
    MinU = div_uv(U, ?TX_WIDTH),
    MinV = div_uv(V, ?TX_HEIGHT),
    MaxU = (U+W) / ?TX_WIDTH,
    MaxV = (V+H) / ?TX_HEIGHT,
    RowH = lists:max([RowH0,H]),
    [{Name,{Id,W,H,MinU,MinV,MaxU,MaxV}}|create_textures_1(T, Id, U+W, V, RowH)];
create_textures_1(Icons, _, _, _, _) ->
    create_textures_2(Icons).

create_textures_2([{Name,{W,H,Icon}}|T]) ->
    [TxId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
		  W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Icon),
    [{Name,{TxId,W,H,0,0,1,1}}|create_textures_2(T)];
create_textures_2([]) -> [].

div_uv(0, _) -> 0;
div_uv(X, X) -> 1;
div_uv(X, Y) -> X/Y.

create_buttons(Icons0) ->
    flatmap(fun({Name,{32,28,Icon}}) ->
		    [{{Name,down},create_button(fun active/5, Icon)},
		     {{Name,up},create_button(fun inactive/5, Icon)}];
	       (Other) -> [Other]
	    end, Icons0).

create_button(Tr, Icon) ->
    create_button(Tr, Icon, 0, 0, []).

create_button(Tr, T, 32, Y, Acc) ->
    create_button(Tr, T, 0, Y+1, Acc);
create_button(_Tr, <<>>, _X, _Y, Acc) ->
    {?ICON_WIDTH,?ICON_HEIGHT,list_to_binary(reverse(Acc))};
create_button(Tr, <<R:8,G:8,B:8,T/binary>>, X, Y, Acc) ->
    create_button(Tr, T, X+1, Y, [Tr(X, Y, R, G, B)|Acc]).

active(X, Y, R, G, B) ->
    if
	X < 1; X > 30; Y < 1; Y > 26 -> [255,255,255];
	true -> [R,G,B]
    end.

inactive(_X, _Y, R, G, B) -> [R,G,B].

%%%
%%% Input.
%%%

putback_event(Event) ->
    #io{eq={In,Out}} = Io = get_state(),
    put_state(Io#io{eq={In,[Event|Out]}}).

putback_event_once(Ev) ->
    #io{eq={In,Out}} = Io = get_state(),
    case member(Ev, In) orelse member(Ev, Out) of
	true -> ok;
	false -> put_state(Io#io{eq={In,[Ev|Out]}})
    end.

get_event() ->
    case get_sdl_event() of
	{quit} -> quit;
	Other -> Other
     end.

get_matching_events(Filter) ->
    #io{eq=Eq} = get_state(),
    get_matching_events_1(Filter, Eq, [], []).

get_matching_events_1(Filter, Eq0, Match, NoMatch) ->
    case queue:out(Eq0) of
	{{value,Ev},Eq} ->
	    case Filter(Ev) of
		false ->
		    get_matching_events_1(Filter, Eq, Match, [Ev|NoMatch]);
		true ->
		    get_matching_events_1(Filter, Eq, [Ev|Match], NoMatch)
	    end;
	{empty,{In,Out}} ->
	    case Match of
		[] -> [];
		_ ->
		    Io = get_state(),
		    put_state(Io#io{eq={In,reverse(NoMatch, Out)}}),
		    Match
	    end
    end.

poll_event() ->
    #io{eq=Eq} = get_state(),
    case queue:out(Eq) of
	{{value,Ev},_} -> Ev;
	{empty,_} -> none
    end.

get_sdl_event() ->
    Io0 = get_state(),
    {Event,Io} = get_sdl_event(Io0),
    put_state(Io),
    Event.

get_sdl_event(#io{eq=Eq0}=Io) ->
    {Event,Eq} = read_events(Eq0),
    {Event,Io#io{eq=Eq}}.

read_events(Eq0) ->
    case sdl_events:peepEvents() of
	[] -> read_out(Eq0);
	[_|_]=Evs -> read_events(enter_events(Evs, Eq0))
    end.

enter_events([no_event|Evs], Eq) ->
    enter_events(Evs, queue:in(redraw, Eq));
enter_events([E|Evs], Eq) ->
    enter_events(Evs, queue:in(E, Eq));
enter_events([], Eq) -> Eq.

read_out(Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,Event},Eq} ->
	    {Event,Eq};
	{empty,Eq} ->
            wait_for_event(Eq)
    end.

read_out(Motion, Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	_Other -> {Motion,Eq0}
    end.

wait_for_event(Eq) ->
    receive
        {timeout,Ref,{event,Event}} when is_reference(Ref) ->
            {Event,Eq}
    after 10 ->
            case sdl_events:peepEvents() of
                [] -> wait_for_event(Eq);
                [_|_]=Evs -> read_events(enter_events(Evs, Eq))
            end
    end.


%%%
%%% Timer support.
%%%

set_timer(Time, Event) ->
    erlang:start_timer(Time, self(), {event,Event}).

cancel_timer(Ref) ->
    Left = erlang:cancel_timer(Ref),
    receive
	{timeout,Ref,_} -> ok
    after 0 -> ok
    end,
    Left.

%%%
%%% Mouse grabbing.
%%%
reset_grab() ->
    Io = get_state(),
    put_state(Io#io{grab_count=0}),
    sdl_mouse:showCursor(true),
    sdl_video:wm_grabInput(?SDL_GRAB_OFF).

grab() ->
    %%io:format("Grab mouse~n", []),
    #io{grab_count=Cnt} = Io = get_state(),
    sdl_mouse:showCursor(false),
    do_grab(Cnt),
    put_state(Io#io{grab_count=Cnt+1}).

do_grab(0) ->
    case os:type() of
	{unix, darwin} ->
	    ignore;  %% GRAB doesn't work good enough on Darwin
	_ ->
	    %% Good for Linux to read out any mouse events here.
	    sdl_events:peepEvents(1, ?SDL_MOUSEMOTIONMASK),
	    sdl_video:wm_grabInput(?SDL_GRAB_ON)
    end;
do_grab(_N) -> ok.

ungrab(X, Y) ->
    %%io:format("UNGRAB mouse~n", []),
    case get_state() of
	#io{grab_count=0} -> no_grab;
	#io{grab_count=Cnt}=Io ->
	    put_state(Io#io{grab_count=Cnt-1}),
	    case Cnt-1 of
		0 ->
		    sdl_video:wm_grabInput(?SDL_GRAB_OFF),
		    sdl_mouse:warpMouse(X, Y),
		    sdl_mouse:showCursor(true),
		    no_grab;
		_ ->
		    still_grabbed
	    end
    end.

is_grabbed() ->
    case get_state() of
	#io{grab_count=0} -> false;
	_ -> true
    end.

warp(X, Y) ->
    %% Strangely enough, on Solaris the warp doesn't seem to
    %% work unless the mouse cursor is visible.
    %% On Windows, the mouse cursor must not be visible.
    case os:type() of
	{unix,sunos} ->
	    sdl_mouse:showCursor(true),
	    sdl_mouse:warpMouse(X, Y),
	    sdl_mouse:showCursor(false);
	_ ->
	    sdl_mouse:warpMouse(X, Y)
    end.

%%%
%%% Cursors.
%%%

build_cursors() ->
    [{stop,build_cursor(stop_data())},
     {pointing_hand,build_cursor(pointing_hand_data())},
     {closed_hand,build_cursor(closed_hand_data())}|
     case os:type() of
	 {unix,darwin} ->
	     [{arrow,sdl_mouse:getCursor()},
	      {hourglass,none}];
	 _ ->
	    [{arrow,build_cursor(arrow_data())},
	     {hourglass,build_cursor(hourglass_data())}]
     end].

build_cursor(Data0) ->
    case os:type() of
 	{unix,darwin} ->
 	    build_cursor(Data0, 0, 0);
	_ when length(Data0) =:= 256 ->
	    Data = build_cursor_dup(Data0, 0, []),
	    build_cursor(Data, 0, 0);
	_ ->
	    build_cursor(Data0, 0, 0)
    end.

build_cursor_dup(Cs, 16, Row0) ->
    Row = reverse(Row0),
    Row ++ lists:duplicate(16, $\s) ++
        build_cursor_dup(Cs, 0, []);
build_cursor_dup([C|Cs], N, Acc) ->
    build_cursor_dup(Cs, N+1, [C|Acc]);
build_cursor_dup([], _, _) ->
    lists:duplicate(16*16, $\s).

build_cursor([$.|T], Mask, Bits) ->
    build_cursor(T, (Mask bsl 1) bor 1, Bits bsl 1);
build_cursor([$X|T], Mask, Bits) ->
    build_cursor(T, (Mask bsl 1) bor 1, (Bits bsl 1) bor 1);
build_cursor([$x|T], Mask, Bits) ->
    build_cursor(T, (Mask bsl 1) bor 1, (Bits bsl 1) bor 1);
build_cursor([_|T], Mask, Bits) ->
    build_cursor(T, Mask bsl 1, Bits bsl 1);
build_cursor([], Mask0, Bits0) ->
    case os:type() of
	{unix,darwin} ->
	    Bits = <<Bits0:256>>,
	    Mask = <<Mask0:256>>;
	_ ->
	    Bits = <<Bits0:1024>>,
	    Mask = <<Mask0:1024>>
    end,
    sdl_mouse:createCursor(Bits, Mask, 32, 32, 0, 0).

hourglass_data() ->
        "  ............................	 "
	" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "
       	"X..............................X"
       	" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "
       	"   ..   X..............X   ..   "
       	"   ..   X..............X   ..   "
	"   ..   X..............X   ..   "
	"   ..   X..............X   ..   "
	"   ..    X............X    ..   "
	"   ..    X............X    ..   "
	"   ..    X............X    ..   "
	"   ..     X..........X     ..   "
	"   ..     X.X......X.X     ..   "
	"   ..     X.X.X..X.X.X     ..   "
       	"   ..      X.X.X.X.XX      ..   "
	"   ..       X..XX..X       ..   "
	"   ..       X......X       ..   "
	"   ..      X........X      ..   "
       	"   ..     X..........X     ..   "
	"   ..     X..........X     ..   "
	"   ..     X..........X     ..   "
	"   ..    X............X    ..   "
	"   ..    X............X    ..   "
	"   ..    X......X.....X    ..   "
	"   ..   X....X.X.X.X...X   ..   "
	"   ..   X...X.X.X.X.X..X   ..   "
	"   ..   X..X.X.X.X.X.X.X   ..   "
       	"   ..   X.X.X.X.X.X.XX.X   ..   "
       	" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "
       	"X..............................X"
       	" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "
       	"  ............................	 ".

arrow_data() ->
        "X                               "
	"XX                              "
	"X.X                             "
	"X..X                            "
	"X...X                           "
	"X....X                          "
	"X.....X                         "
	"X......X                        "
	"X.......X                       "
	"X........X                      "
	"X.....XXXXX                     "
	"X..X..X                         "
	"X.X X..X                        "
	"XX  X..X                        "
	"X    X..X                       "
	"     X..X                       "
	"      X..X                      "
	"      X..X                      "
	"       XX                       "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                ".

stop_data() ->
        "     xxxxxx     "
        "   xxxxxxxxxx   "
        "   xxx    xxx   "
	" xxxxx      xxx "
	" xxxxx      xxx "
        "xxxxxxx       xx"
        "xx   xxx      xx"
	"xx    xxx     xx"
	"xx     xxx    xx"
	"xx      xxx   xx"
	"xx       xxxxxxx"
	" xxx      xxxxx "
	" xxx      xxxxx "
        "   xxx    xxx   "
        "   xxxxxxxxxx   "
        "     xxxxxx     ".

pointing_hand_data() ->
        "       xx       "
        "      x..x      "
        "      x..x      "
       	"      x..x      "
       	"  xx  x..xx     "
        " x..x x..x.xx   "
        " x...xx..x.x.xx "
       	" x....x..x.x.x.x"
       	"  x...x......x.x"
	"   x...........x"
	"   x...........x"
       	"   x..........x "
        "    x.........x "
        "    x........x  "
        "    x........x  "
        "    x........x  ".

closed_hand_data() ->
        "                "
        "                "
        "                "
       	"                "
        "  x xx xx xx    "
        " x.x..x..x.x.xx "
       	"x..x..x..x..x..x"
       	"x...........x..x"
	" x.............x"
	"x..............x"
       	"x..............x"
        "x.............x "
        "x.............x "
        " x...........x  "
        "  x..........x  "
        "  x..........x  ".
		       
