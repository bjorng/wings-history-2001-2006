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
%%     $Id: wings_io.erl,v 1.30 2002/01/17 13:20:40 bjorng Exp $
%%

-module(wings_io).
-export([init/0,menubar/1,resize/2,display/1,
	 hourglass/0,
	 draw_ui/1,
	 update/1,
	 event/1,button/2,
	 info/1,message/1,clear_message/0,
	 progress/1,progress_tick/0,
	 clear_menu_sel/0,
	 sunken_rect/5,raised_rect/4,raised_rect/5,
	 text_at/2,text_at/3,menu_text/3,axis_text/4,space_at/2,
	 draw_icon/5,
	 draw_message/1,draw_completions/1]).
-export([putback_event/1,get_event/0,
	 set_timer/2,cancel_timer/1,
	 enter_event_loop/1]).
-export([grab/0,ungrab/0,warp/2]).
-export([setup_for_drawing/0,cleanup_after_drawing/0,ortho_setup/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [flatmap/2,foldl/3,keysearch/3,
		reverse/1,foreach/2,last/1]).

-define(ICON_WIDTH, 45).
-define(ICON_HEIGHT, 34).

-record(io,
	{w,					%Width of screen (pixels).
	 h,					%Height of screen (pixels).
	 menubar,				%Menu bar at top.
	 sel,					%Selected item in menubar.
	 message,				%Message to show (or undefined).
	 info="",				%Information message.
	 eq,					%Event queue.
	 icons=[],				%Position for Icons.
	 tex=[],				%Textures.
	 grab_count=0,				%Number of grabs.
	 hourglass,				%Hourglass cursor.
	 arrow,					%Arrow cursor.
	 raw_icons,				%Raw icon bundle.
         progress_pos				%Progress position.
	}).

init() ->
    Icons = read_icons(),
    Arrow = build_cursor(arrow_data()),
    Hourglass = build_cursor(hourglass_data()),
    sdl_mouse:setCursor(Arrow),
    put_state(#io{eq=queue:new(),raw_icons=Icons,
		  arrow=Arrow,hourglass=Hourglass}).

hourglass() ->
    #io{hourglass=Hg} = get_state(),
    sdl_mouse:setCursor(Hg).

arrow() ->
    #io{arrow=Arrow} = get_state(),
    sdl_mouse:setCursor(Arrow).

read_icons() ->
    IconFile = filename:join([wings:root_dir(),"ebin","wings_icon.bundle"]),
    {ok,Bin} = file:read_file(IconFile),
    Bin.

resize(W, H) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    #io{raw_icons=RawIcons} = Io = get_state(),
    Icons = place_icons(W, H),
    Tex = load_textures(RawIcons),
    put_state(Io#io{w=W,h=H,tex=Tex,icons=Icons}).

place_icons(W, H) ->
    Mid = W div 2,
    Lmarg = 5,
    Rmarg = 20,
    [{Lmarg,wire},{Lmarg+?ICON_WIDTH,flatshade},{Lmarg+2*?ICON_WIDTH,smooth},
     {Mid-2*?ICON_WIDTH,vertex},{Mid-?ICON_WIDTH,edge},
     {Mid,face},{Mid+?ICON_WIDTH,body},
     {W-3*?ICON_WIDTH-Rmarg,perspective},
     {W-2*?ICON_WIDTH-Rmarg,groundplane},
     {W-?ICON_WIDTH-Rmarg,axes}].

menubar(Menubar) ->
    Io = get_state(),
    put_state(Io#io{menubar=Menubar}).

progress(Message) ->
    display(fun(W, H) ->
		    draw_message(fun() -> text_at(0, Message) end)
	    end),
    Io = get_state(),
    put_state(Io#io{progress_pos=length(Message)*?CHAR_WIDTH}).
    
progress_tick() ->
    #io{w=W,h=H,progress_pos=Pos} = Io = get_state(),
    gl:drawBuffer(?GL_FRONT),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_DEPTH_TEST),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0.0, float(W), float(H), 0.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:color3f(0.0, 0.0, 0.0),
    draw_message(fun() -> text_at(Pos, ".") end),
    cleanup_after_drawing(),
    put_state(Io#io{progress_pos=Pos+?CHAR_WIDTH}).

info(Info) ->
    Io = get_state(),
    put_state(Io#io{info=Info}).

message(Message) ->
    Io = get_state(),
    put_state(Io#io{message=Message}).

clear_message() ->
    Io = get_state(),
    put_state(Io#io{message=undefined}).

clear_menu_sel() ->
    put_state((get_state())#io{sel=undefined}).

display(F) ->
    #io{w=W,h=H} = Io = get_state(),
    setup_for_drawing(),
    draw_panes(Io),
    Res = F(W, H),
    cleanup_after_drawing(),
    Res.

draw_ui(St) ->
    display(fun(Io) -> update(Io, St) end, ?GL_BACK).

update(St) ->
    display(fun(Io) -> update(Io, St) end, ?GL_BACK),
    gl:swapBuffers(),
    arrow(),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT).

draw_message(F) ->
    #io{w=W,h=H} = get_state(),
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:translatef(10.0, H-12.0, 0.0),
    Res = F(),
    gl:popMatrix(),
    Res.

draw_completions(F) ->
    #io{w=W,h=H} = get_state(),
    Margin = 10,
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:translatef(float(Margin), H / 6, 0.0),
    raised_rect(0, 0, W-2*Margin, 4*H div 6),
    gl:translatef(10.0, float(?LINE_HEIGHT), 0.0),
    Res = F(),
    gl:popMatrix(),
    Res.

%% Internal.
display(F, Buf) ->
    #io{w=W,h=H} = Io = get_state(),
    gl:drawBuffer(Buf),
    setup_for_drawing(W, H),
    put_state(F(Io)),
    cleanup_after_drawing(),
    ok.

update(#io{message=Msg,info=Info}=Io0, St) ->
    draw_icons(Io0, St),
    draw_panes(Io0),
    Text = case Msg of
	       undefined -> maybe_show_mem_used(Info);
	       Other -> Msg
	   end,
    draw_message(fun() -> text_at(0, Text) end),
    Io0.

maybe_show_mem_used(Info) ->
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
	    lists:concat(["[mem:",integer_to_list(N),M,"] ",Info]);
	false -> Info
    end.

draw_panes(#io{w=W,h=H,menubar=Bar,sel=Sel}=Io) ->
    raised_rect(-2, 0, W+2, ?LINE_HEIGHT+6),
    sunken_rect(6, H-2*?LINE_HEIGHT+5, W-10, 2*?LINE_HEIGHT-8),
    draw_bar(0, Bar, Sel).

-define(MENU_MARGIN, 8).
-define(MENU_ITEM_SPACING, 3).

draw_bar(X, [{Name,Item}|T], Sel) ->
    W = ?CHAR_WIDTH*(?MENU_ITEM_SPACING+length(Name)),
    if
	Item =:= Sel ->
	    sunken_rect(X+1, 3, W, ?LINE_HEIGHT);
	true -> ok
    end,
    text_at(?MENU_MARGIN+X, ?LINE_HEIGHT-1, Name),
    draw_bar(X+W, T, Sel);
draw_bar(X, [], Sel) -> ok.

draw_icons(#io{w=W,h=H,icons=Icons}, St) ->
    raised_rect(-2, H-4*?LINE_HEIGHT-3, W+2, 4*?LINE_HEIGHT+3),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    Y = H-4*?LINE_HEIGHT+2,
    foreach(fun({X,Name}) ->
		    draw_icon(X, Y, icon_button(Name, St))
	    end, Icons),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D).

icon_button(groundplane=Name, St) ->
    icon_button(Name, show_groundplane, true);
icon_button(axes=Name, St) ->
    icon_button(Name, show_axes, true);
icon_button(wire=Name, St) ->
    icon_button(Name, wire_mode, true);
icon_button(flatshade=Name, St) ->
    icon_button(Name, smooth_preview, false);
icon_button(smooth=Name, St) ->
    icon_button(Name, smooth_preview, true);
icon_button(perspective=Name, St) ->
    icon_button(Name, orthogonal_view, true);
icon_button(Name, #st{selmode=Name}) -> {Name,down};
icon_button(Name, St) -> {Name,up}.

icon_button(Name, Key, Val) ->
    case wings_pref:get_value(Key) of
	Val -> {Name,down};
	_ -> {Name,up}
    end.

event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}=Mb) ->
    case button(X, Y) of
	none -> next;
	ignore -> keep;
	Other ->
	    putback_event({action,Other}),
	    keep
    end;
event(Other) -> next.

button(X, Y) when Y > ?LINE_HEIGHT; X < ?MENU_MARGIN ->
    #io{h=H,icons=Icons} = get_state(),
    put_state((get_state())#io{sel=undefined}),
    case H-4*?LINE_HEIGHT of
	Low when Low =< Y, Y < Low + 32 ->
	    icon_row_hit(X, Icons),
	    ignore;
	Other -> none
    end;
button(X0, Y) ->
    X = X0 - ?MENU_MARGIN,
    #io{menubar=Bar} = get_state(),
    button_1(X, 0, Bar).

button_1(RelX, X, [{Name,Item}|T]) ->
    case ?CHAR_WIDTH*length(Name) of
	W when RelX < W ->
	    put_state((get_state())#io{sel=Item}),
	    {menu,Item,X+2,?LINE_HEIGHT+7};
	W ->
	    Iw = W+?MENU_ITEM_SPACING*?CHAR_WIDTH,
	    button_1(RelX-Iw, X+Iw, T)
    end;
button_1(XRel, X, []) ->
    put_state((get_state())#io{sel=undefined}),
    none.

icon_row_hit(X, [{Pos,Name}|Is]) when Pos =< X, X < Pos+?ICON_WIDTH ->
    Action = case Name of
		 groundplane -> {view,show_groundplane};
		 axes -> {view,show_axes};
		 wire -> {view,wire_mode};
		 flatshade -> {view,flatshade};
		 smooth -> {view,smoothshade};
		 perspective -> {view,orthogonal_view};
		 Other -> {select,Other}
	     end,
    putback_event({action,Action}),
    none;
icon_row_hit(X, [_|Is]) ->
    icon_row_hit(X, Is);
icon_row_hit(X, []) -> none.
    
raised_rect(X, Y, Mw, Mh) ->
    raised_rect(X, Y, Mw, Mh, ?PANE_COLOR).

raised_rect(X, Y, Mw, Mh, FillColor) ->
    sunken_rect(X+Mw, Y+Mh, -Mw, -Mh, FillColor).

sunken_rect(X, Y, W, H) ->
    sunken_rect(X, Y, W, H, ?PANE_COLOR).

sunken_rect(X0, Y0, Mw0, Mh0, FillColor) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    Mw = Mw0 + 0.5,
    Mh = Mh0 + 0.5,
    gl:color3fv(FillColor),
    gl:rectf(X0, Y0, X0+Mw0, Y0+Mh0),
    gl:'begin'(?GL_LINES),
    gl:color3fv(?BEVEL_LOWLIGHT),
    gl:vertex2f(X, Y+Mh),
    gl:vertex2f(X, Y),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+Mw, Y),
    gl:color3fv(?BEVEL_HIGHLIGHT),
    gl:vertex2f(X+Mw, Y),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:vertex2f(X, Y+Mh),
    gl:'end'(),
    gl:color3f(0.0, 0.0, 0.0).

space_at(X, Y) ->
    gl:color3f(0.52, 0.52, 0.52),
    gl:recti(X, Y-?LINE_HEIGHT+3, X+?CHAR_WIDTH, Y+3),
    gl:color3f(0.0, 0.0, 0.0).

text_at(X, S) ->
    gl:rasterPos2i(X, 0),
    catch wings_text:draw(S).

text_at(X, Y, S) ->
    gl:rasterPos2i(X, Y),
    catch wings_text:draw(S).

menu_text(X, Y, S) ->
    gl:rasterPos2i(X, Y),
    catch menu_text(S, Y).

menu_text([$&,C|T], Y) ->
    [X,_,_,_] = gl:getIntegerv(?GL_CURRENT_RASTER_POSITION),
    wings_text:char($_),
    gl:rasterPos2i(X, Y),
    wings_text:char(C),
    menu_text(T, Y);
menu_text([C|T], Y) ->
    wings_text:char(C),
    menu_text(T, Y);
menu_text([], Y) -> ok.

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
    ClipX = lists:min([trunc(X),W-9]),
    ClipY = lists:max([lists:min([trunc(Y-10),H-35]),74]),
    gl:color3fv(Color),
    gl:rasterPos2i(ClipX, ClipY),
    wings_text:char(C),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW).

min(A, B) when A < B -> A;
min(A, B) -> B.

max(A, B) when A > B -> A;
max(A, B) -> B.

setup_for_drawing() ->
    #io{w=W,h=H} = Io = get_state(),
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
    #io{w=W,h=H} = Io = get_state(),
    ?CHECK_ERROR(),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_DEPTH_TEST),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0.0, float(W), float(H), 0.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:color3f(0.0, 0.0, 0.0),
    ?CHECK_ERROR().

get_state() ->
    get(wings_io).

put_state(Io) ->
    put(wings_io, Io).

draw_icon(X, Y, Icon) ->
    draw_icon(X, Y, ?ICON_WIDTH, ?ICON_HEIGHT, 64, 64, Icon).

draw_icon(X, Y, W, H, Icon) ->
    draw_icon(X, Y, W, H, W, H, Icon).

draw_icon(X, Y, W, H, Wtot, Htot, Icon) ->
    #io{tex=Tex} = get_state(),
    {value,{Icon,Id}} = keysearch(Icon, 1, Tex),
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
    gl:'end'(),
    gl:color3f(0.0, 0.0, 0.0).

load_textures(Bin) ->
    case catch binary_to_term(Bin) of
	{'EXIT',_} -> [];
	Icons0 ->
	    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
	    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
	    Icons = create_buttons(Icons0),
	    Tex = create_textures(Icons, 1),
	    gl:popAttrib(),
	    Tex
    end.

create_textures([{Name,{W,H,Icon}}|T], Id) ->
    gl:bindTexture(?GL_TEXTURE_2D, Id),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
		  W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Icon),
    [{Name,Id}|create_textures(T, Id+1)];
create_textures([], Id) -> [].

create_buttons(Icons0) ->
    flatmap(fun({Name,{64,64,Icon}}) ->
		    [{{Name,down},create_button(fun active/5, Icon)},
		     {{Name,up},create_button(fun inactive/5, Icon)}];
	       ({Name,Icon}=T) -> [T]
	    end, Icons0).

create_button(Tr, Icon) ->
    create_button(Tr, Icon, 0, 0, []).

create_button(Tr, T, 64, Y, Acc) ->
    create_button(Tr, T, 0, Y+1, Acc);
create_button(Tr, <<>>, X, Y, Acc) ->
    {64,64,list_to_binary(reverse(Acc))};
create_button(Tr, <<R:8,G:8,B:8,T/binary>>, X, Y, Acc) ->
    create_button(Tr, T, X+1, Y, [Tr(X, Y, R, G, B)|Acc]).

active(X, Y, R, G, B) ->
    if
	X < 2; X > 42; Y < 2; Y > 31 -> [255,255,255];
	true -> [R,G,B]
    end.

inactive(X, Y, R, G, B) -> [R,G,B].

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
	{N,Evs} -> read_events(enter_events(Evs, Eq0))
    end.

enter_events(Evs, Eq0) ->
    foldl(fun(E, Q) -> queue:in(E, Q) end, Eq0, Evs).

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
	    after 3 ->
		    read_events(Eq)
	    end
    end.

read_out(Motion, Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,no_event},Eq} ->
	    {redraw,Eq};
	Other -> {Motion,Eq0}
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

grab() ->
    #io{grab_count=Cnt} = Io = get_state(),
    sdl_mouse:showCursor(false),
    sdl_video:wm_grabInput(?SDL_GRAB_ON),
    put_state(Io#io{grab_count=Cnt+1}).

ungrab() ->
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
%%% Event loop.
%%%

enter_event_loop(Op) ->
    Stk = handle_response(Op, dummy_event, [crash_handler()]),
    event_loop(Stk).

event_loop([Handler|_]=Stk0) ->
    Event = wings_io:get_event(),
    case handle_event(Handler, Event, Stk0) of
	[] -> ok;
	Stk -> event_loop(Stk)
    end.
    
handle_event(Handler, Event, Stk) ->
    case catch Handler(Event) of
	{'EXIT',Reason} ->
	    CrashHandler = last(Stk),
	    handle_response(CrashHandler({crash,Reason}),
			    Event, [crash_handler()]);
	Res ->
	    handle_response(Res, Event, Stk)
    end.

handle_response(Res, Event, Stk0) ->
    case Res of
	keep -> Stk0;
	next -> next_handler(Event, Stk0);
	pop -> pop(Stk0);
	{push,Top} -> [Top|Stk0];
	{seq,First,Then} ->
	    Stk = handle_response(First, Event, Stk0),
	    handle_response(Then, Event, Stk);
	{replace,Top} when is_function(Top) -> replace_top(Top, Stk0);
	Top when is_function(Top) -> replace_top(Top, Stk0)
    end.

pop([_|Stk]) -> Stk.

replace_top(Top, [_|Stk]) -> [Top|Stk].

next_handler(Event, [_|[Next|_]=Stk]) ->
    handle_event(Next, Event, Stk).

crash_handler() ->
    fun(Crash) ->
	    io:format("Crashed: ~p\n", [Crash]),
	    exit(too_bad)
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
build_cursor([C|T], Mask, Bits) ->
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
