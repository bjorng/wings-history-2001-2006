%%
%%  wings_io.erl --
%%
%%     This module contains most of the low-level GUI for Wings.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_io.erl,v 1.74 2002/11/24 09:46:28 bjorng Exp $
%%

-module(wings_io).
-export([init/0,menubar/1,resize/2,
	 icon_restriction/1,clear_icon_restriction/0,get_icon_restriction/0,
	 arrow/0,hourglass/0,
	 update/1,
	 event/1,button/2,info/1,
	 disable_progress/0,progress/1,progress_tick/0,
	 clear_menu_sel/0,
	 border/5,
	 sunken_rect/5,raised_rect/4,raised_rect/5,
	 text_at/2,text_at/3,text/1,menu_text/3,axis_text/4,space_at/2,
	 draw_icon/5,
	 set_color/1]).
-export([putback_event/1,get_event/0,poll_event/0,
	 set_timer/2,cancel_timer/1]).

-export([reset_grab/0,grab/0,ungrab/0,warp/2]).
-export([setup_for_drawing/0,cleanup_after_drawing/0,ortho_setup/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [flatmap/2,foldl/3,keysearch/3,member/2,
		reverse/1,foreach/2,last/1]).

-define(ICON_WIDTH, 44).
-define(ICON_HEIGHT, 32).

-record(io,
	{w,					%Width of screen (pixels).
	 h,					%Height of screen (pixels).
	 menubar,				%Menu bar at top.
	 sel,					%Selected item in menubar.
	 eq,					%Event queue.
	 icons=[],				%Position for Icons.
	 tex=[],				%Textures.
	 grab_count=0,				%Number of grabs.
	 hourglass,				%Hourglass cursor.
	 arrow,					%Arrow cursor.
	 raw_icons,				%Raw icon bundle.
         progress_pos,				%Progress position.
	 selmodes=all				%Which icons to show.
	}).

init() ->
    Icons = read_icons(),
    Arrow = build_cursor(arrow_data()),
    Hourglass = build_cursor(hourglass_data()),
    set_cursor(Arrow),
    put_state(#io{eq=queue:new(),raw_icons=Icons,
		  arrow=Arrow,hourglass=Hourglass}).

hourglass() ->
    #io{hourglass=Hg} = get_state(),
    set_cursor(Hg).

arrow() ->
    #io{arrow=Arrow} = get_state(),
    set_cursor(Arrow).

set_cursor(Cursor) ->
    case os:type() of
	{unix,darwin} -> ok;
	_ -> sdl_mouse:setCursor(Cursor)
    end.
	    
read_icons() ->
    IconFile = filename:join([wings:root_dir(),"ebin","wings_icon.bundle"]),
    case file:read_file(IconFile) of
	{ok,Bin} -> Bin;
	{error,enoent} ->
	    IconFile2 = filename:join([wings:root_dir(),"wings_icon.bundle"]),
	    {ok,Bin} = file:read_file(IconFile2),
	    Bin
    end.

resize(W0, H0) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    #io{raw_icons=RawIcons} = Io = get_state(),
    Icons = place_icons(W0),
    Tex = load_textures(RawIcons),
    put_state(Io#io{w=W0,h=H0,tex=Tex,icons=Icons}),
    case sdl_video:wm_isMaximized() of
	false -> wings_pref:set_value(window_size, {W0,H0});
	true ->  wings_pref:set_value(window_size, {W0-8,H0-10})
    end,
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

place_icons(W) ->
    Mid = W div 2,
    Lmarg = 5,
    Rmarg = 20,
    [{Lmarg,flatshade},{Lmarg+?ICON_WIDTH,smooth},
     {Mid-2*?ICON_WIDTH,vertex},{Mid-?ICON_WIDTH,edge},
     {Mid,face},{Mid+?ICON_WIDTH,body},
     {W-3*?ICON_WIDTH-Rmarg,perspective},
     {W-2*?ICON_WIDTH-Rmarg,groundplane},
     {W-?ICON_WIDTH-Rmarg,axes}].

menubar(Menubar) ->
    Io = get_state(),
    put_state(Io#io{menubar=Menubar}).

disable_progress() ->
    ok.
    
progress(_Message) ->
    ok.
    
progress_tick() ->
    ok.

info(Info) ->
    ortho_setup(),
    set_color(wings_pref:get_value(info_color)),
    text_at(4, 2*?LINE_HEIGHT+3, Info).
    
clear_menu_sel() ->
    put_state((get_state())#io{sel=undefined}).

icon_restriction(Modes) ->
    put_state((get_state())#io{selmodes=Modes}).

clear_icon_restriction() ->
    put_state((get_state())#io{selmodes=all}).

get_icon_restriction() ->
    #io{selmodes=Modes} = get_state(),
    Modes.

update(St) ->
    #io{w=W,h=H} = Io = get_state(),
    setup_for_drawing(W, H),
    draw_icons(Io, St),
    draw_panes(Io),
    maybe_show_mem_used(H),
    cleanup_after_drawing(),
    ok.

maybe_show_mem_used(H) ->
    case wings_pref:get_value(show_memory_used) of
	true ->
	    {memory,Sz} = process_info(self(), memory),
	    {N,M} = if
			Sz < 1024 ->
			    {Sz,"bytes"};
			Sz < 1024*1204 ->
			    {(Sz+512) div 1024,"Kb"};
			true ->
			    {(Sz+1024*512) div 1024 div 1024,"Mb"}
		    end,
	    Dl = gl:genLists(1),
	    gl:deleteLists(Dl, 1),
	    {binary,B} = process_info(self(), binary),
	    Mem = ["Memory: ",integer_to_list(N),M,
		   "  Dlist: ",integer_to_list(Dl),
		   "  Binaries: ",integer_to_list(length(B))],
	    ortho_setup(),
	    text_at(4, H-3*?LINE_HEIGHT+5, Mem);
	false -> ok
    end.

draw_panes(#io{w=W,menubar=Bar,sel=Sel}) ->
    border(0, -1, W, ?LINE_HEIGHT+6),
    draw_bar(0, Bar, Sel).

-define(MENU_MARGIN, 8).
-define(MENU_ITEM_SPACING, 3).

draw_bar(X, [{Name,Item}|T], Sel) ->
    W = ?CHAR_WIDTH*(?MENU_ITEM_SPACING+length(Name)),
    if
	Item =:= Sel ->
	    border(X+1, 3, W, ?LINE_HEIGHT);
	true -> ok
    end,
    text_at(?MENU_MARGIN+X, ?LINE_HEIGHT-1, Name),
    draw_bar(X+W, T, Sel);
draw_bar(_X, [], _Sel) -> ok.

draw_icons(#io{w=W,h=H,icons=Icons0,selmodes=Modes}, St) ->
    set_color(?PANE_COLOR),
    gl:rectf(0, H, W, H-2*?LINE_HEIGHT-2),
    gl:color3f(0.20, 0.20, 0.20),
    gl:'begin'(?GL_LINES),
    gl:vertex2f(0.5, H-2*?LINE_HEIGHT-2.5),
    gl:vertex2f(W-1.5, H-2*?LINE_HEIGHT-2.5),
    gl:'end'(),
    gl:color3f(0, 0, 0),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    Y = H-2*?LINE_HEIGHT+2,
    Icons = case Modes of
		all -> Icons0;
		_ -> [Icon || {_,Name}=Icon <- Icons0, member(Name, Modes)]
	    end,
    foreach(fun({X,Name}) ->
		    draw_icon(X, Y, icon_button(Name, St))
	    end, Icons),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D).

icon_button(groundplane=Name, _St) ->
    icon_button(Name, show_groundplane, true);
icon_button(axes=Name, _St) ->
    icon_button(Name, show_axes, true);
icon_button(flatshade=Name, _St) ->
    icon_button(Name, workmode, true);
icon_button(smooth=Name, _St) ->
    icon_button(Name, workmode, false);
icon_button(perspective=Name, _St) ->
    icon_button(Name, orthogonal_view, true);
icon_button(Name, #st{selmode=Name}) -> {Name,down};
icon_button(Name, _St) -> {Name,up}.

icon_button(Name, Key, Val) ->
    case wings_pref:get_value(Key) of
	Val -> {Name,down};
	_ -> {Name,up}
    end.

event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}) ->
    case button(X, Y) of
	none -> next;
	ignore -> keep;
	Other ->
	    putback_event({action,Other}),
	    keep
    end;
event(_) -> next.

button(X, Y) when Y > ?LINE_HEIGHT; X < ?MENU_MARGIN ->
    #io{h=H,icons=Icons} = get_state(),
    put_state((get_state())#io{sel=undefined}),
    case H-2*?LINE_HEIGHT of
	Low when Low =< Y, Y < Low + 32 ->
	    icon_row_hit(X, Icons),
	    ignore;
	_Other -> none
    end;
button(X0, _Y) ->
    X = X0 - ?MENU_MARGIN,
    #io{menubar=Bar} = get_state(),
    button_1(X, 0, Bar).

button_1(RelX, X, [{Name,Item}|T]) ->
    case ?CHAR_WIDTH*length(Name) of
	W when RelX < W ->
	    put_state((get_state())#io{sel=Item}),
	    {menu,Item,X+2,?LINE_HEIGHT+6};
	W ->
	    Iw = W+?MENU_ITEM_SPACING*?CHAR_WIDTH,
	    button_1(RelX-Iw, X+Iw, T)
    end;
button_1(_, _, []) ->
    put_state((get_state())#io{sel=undefined}),
    none.

icon_row_hit(X, [{Pos,Name}|_]) when Pos =< X, X < Pos+?ICON_WIDTH ->
    Action = case Name of
		 groundplane -> {view,show_groundplane};
		 axes -> {view,show_axes};
		 flatshade -> {view,flatshade};
		 smooth -> {view,smoothshade};
		 perspective -> {view,orthogonal_view};
		 Other -> {select,Other}
	     end,
    putback_event({action,Action}),
    none;
icon_row_hit(X, [_|Is]) ->
    icon_row_hit(X, Is);
icon_row_hit(_X, []) ->
    putback_event({action,{select,deselect}}),
    none.

border(X, Y, Mw, Mh) ->
    border(X, Y, Mw, Mh, ?PANE_COLOR).

border(X0, Y0, Mw0, Mh0, FillColor) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    Mw = Mw0 - 0.5,
    Mh = Mh0 + 0.5,
    set_color(FillColor),
    gl:rectf(X0, Y0, X0+Mw0, Y0+Mh0),
    gl:color3f(0.20, 0.20, 0.20),
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2f(X, Y+Mh),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+Mw, Y),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:'end'(),
    gl:color3f(0, 0, 0).

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
    gl:color3f(0, 0, 0).

space_at(X, Y) ->
    set_color(?PANE_COLOR),
    gl:recti(X, Y-?LINE_HEIGHT+3, X+?CHAR_WIDTH, Y+3),
    gl:color3f(0.0, 0.0, 0.0).

text_at(X, S) ->
    gl:rasterPos2i(X, 0),
    text(S).

text_at(X, Y, S) ->
    gl:rasterPos2i(X, Y),
    text(S).

text(S) ->
    text(S, []).

text([Atom|Cs], Acc) when is_atom(Atom) ->
    draw_reverse(Acc),
    wings_text:char(Atom),
    text(Cs, []);
text([C|Cs], Acc) when is_integer(C) ->
    text(Cs, [C|Acc]);
text([L|Cs], Acc) when is_list(L) ->
    draw_reverse(Acc),
    text(L, []),
    text(Cs, []);
text([], Acc) -> draw_reverse(Acc).

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

axis_text(X, Y, C, Color) ->
    #io{w=W,h=H} = get_state(),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    ClipX = min(trunc(X), W-9),
    ClipY = max(min(trunc(Y-10), H-35), 74),
    set_color(Color),
    gl:rasterPos2i(ClipX, ClipY),
    wings_text:char(C),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW).

min(A, B) when A < B -> A;
min(_, B) -> B.

max(A, B) when A > B -> A;
max(_, B) -> B.

setup_for_drawing() ->
    #io{w=W,h=H} = get_state(),
    gl:drawBuffer(?GL_FRONT),
    setup_for_drawing(W, H).

setup_for_drawing(W, H) ->
    ?CHECK_ERROR(),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_DEPTH_TEST),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W, H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:color3f(0, 0, 0),
    draw_panes(get_state()),
    ?CHECK_ERROR().

cleanup_after_drawing() ->
    gl:enable(?GL_DEPTH_TEST),
    gl:drawBuffer(?GL_BACK).

ortho_setup() ->
    ?CHECK_ERROR(),
    {_,_,W,H} = wings_wm:viewport(),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_DEPTH_TEST),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W, H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:color3f(0, 0, 0),
    ?CHECK_ERROR().

get_state() ->
    get(wings_io).

put_state(Io) ->
    put(wings_io, Io).

draw_icon(X, Y, Icon) ->
    draw_icon(X, Y, ?ICON_WIDTH, ?ICON_HEIGHT, 64, 32, Icon).

draw_icon(X, Y, W, H, Icon) ->
    draw_icon(X, Y, W, H, W, H, Icon).

draw_icon(X, Y, W, H, Wtot, Htot, Icon) ->
    #io{tex=Tex} = get_state(),
    case keysearch(Icon, 1, Tex) of
	false -> ok;
	{value,{Icon,Id}} ->
	    gl:bindTexture(?GL_TEXTURE_2D, Id),
	    gl:'begin'(?GL_QUADS),
	    gl:texCoord2f(0, H/Htot),
	    gl:vertex2i(X, Y),
	    gl:texCoord2f(0, 0),
	    gl:vertex2i(X, Y+H),
	    gl:texCoord2f(W/Wtot, 0),
	    gl:vertex2i(X+W, Y+H),
	    gl:texCoord2f(W/Wtot, H/Htot),
	    gl:vertex2i(X+W, Y),
	    gl:'end'()
    end,
    gl:color3f(0, 0, 0).


load_textures(Bin) ->
    case catch binary_to_term(Bin) of
	{'EXIT',_} -> [];
	Icons0 ->
	    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
	    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
	    Icons = create_buttons(Icons0),
	    TxIds = gl:genTextures(length(Icons)),
	    Tex = create_textures(Icons, TxIds),
	    gl:popAttrib(),
	    Tex
    end.

create_textures([{Name,{W,H,Icon}}|T], [Id|Ids]) ->
    gl:bindTexture(?GL_TEXTURE_2D, Id),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
		  W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Icon),
    [{Name,Id}|create_textures(T, Ids)];
create_textures([], _Id) -> [].

create_buttons(Icons0) ->
    flatmap(fun({Name,{64,32,Icon}}) ->
		    [{{Name,down},create_button(fun active/5, Icon)},
		     {{Name,up},create_button(fun inactive/5, Icon)}];
	       ({_Name,_Icon}=T) -> [T]
	    end, Icons0).

create_button(Tr, Icon) ->
    create_button(Tr, Icon, 0, 0, []).

create_button(Tr, T, 64, Y, Acc) ->
    create_button(Tr, T, 0, Y+1, Acc);
create_button(_Tr, <<>>, _X, _Y, Acc) ->
    {64,32,list_to_binary(reverse(Acc))};
create_button(Tr, <<R:8,G:8,B:8,T/binary>>, X, Y, Acc) ->
    create_button(Tr, T, X+1, Y, [Tr(X, Y, R, G, B)|Acc]).

active(X, Y, R, G, B) ->
    if
	X < 1; X > 42; Y < 1; Y > 30 -> [255,255,255];
	true -> [R,G,B]
    end.

inactive(_X, _Y, R, G, B) -> [R,G,B].

%%%
%%% Input.
%%%

putback_event(Event) ->
    #io{eq={In,Out}} = Io = get_state(),
    put_state(Io#io{eq={In,[Event|Out]}}),
    ok.

get_event() ->
    case get_sdl_event() of
	{quit} -> quit;
	Other -> Other
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
    case sdl_events:peepEvents(16, ?SDL_GETEVENT, ?SDL_ALLEVENTS) of
	{0,[]} -> read_out(Eq0);
	{_,Evs} -> read_events(enter_events(Evs, Eq0))
    end.

enter_events([E|Evs], Eq) ->
    enter_events(Evs, queue:in(E, Eq));
enter_events([], Eq) -> Eq.

read_out(Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,no_event},Eq} ->
	    {redraw,Eq};
	{{value,Event},Eq} ->
	    {Event,Eq};
	{empty,Eq} ->
	    receive
		{timeout,Ref,{event,Event}} when is_reference(Ref) ->
		    {Event,Eq0}
	    after 20 ->
		    read_events(Eq)
	    end
    end.

read_out(Motion, Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,no_event},Eq} ->
	    {redraw,Eq};
	_Other -> {Motion,Eq0}
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
	    sdl_events:peepEvents(1, ?SDL_GETEVENT, ?SDL_ALLEVENTS),
	    sdl_video:wm_grabInput(?SDL_GRAB_ON)
    end;
do_grab(_N) -> ok.

ungrab() ->
    %%io:format("UNGRAB mouse~n", []),
    #io{grab_count=Cnt} = Io = get_state(),
    put_state(Io#io{grab_count=Cnt-1}),
    case Cnt-1 of
	0 ->
	    sdl_mouse:showCursor(true),
	    sdl_video:wm_grabInput(?SDL_GRAB_OFF),
	    no_grab;
	_ ->
	    still_grabbed
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

build_cursor(Data) ->
    build_cursor(Data, 0, 0).

build_cursor([$\s|T], Mask, Bits) ->
    build_cursor(T, Mask bsl 1, Bits bsl 1);
build_cursor([$.|T], Mask, Bits) ->
    build_cursor(T, (Mask bsl 1) bor 1, Bits bsl 1);
build_cursor([_|T], Mask, Bits) ->
    build_cursor(T, (Mask bsl 1) bor 1, (Bits bsl 1) bor 1);
build_cursor([], Mask0, Bits0) ->
    Bits = <<Bits0:1024>>,
    Mask = <<Mask0:1024>>,
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
