%%
%%  wings_io.erl --
%%
%%     This module contains most of the GUI for Wings.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_io.erl,v 1.13 2001/11/25 13:47:14 bjorng Exp $
%%

-module(wings_io).
-export([init/0,menubar/1,resize/2,display/1,
	 draw_ui/1,
	 update/1,button/2,
	 info/1,message/1,clear_message/0,progress/2,
	 clear_menu_sel/0,
	 beveled_rect/4,text_at/2,text_at/3,menu_text/3,space_at/2,
	 draw_icon/3,draw_icon/5,
	 draw_message/1,draw_completions/1]).
-export([putback_event/1,get_event/0,flush_events/0,
	 set_timer/2,cancel_timer/1,
	 enter_event_loop/1]).
-export([grab/0,ungrab/0]).
-export([setup_for_drawing/0,cleanup_after_drawing/0,ortho_setup/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [flatmap/2,foldl/3,keysearch/3,
		reverse/1,foreach/2,last/1]).

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
	 grab_count=0				%Number of grabs.
	}).

init() ->
    Icons = [{10,vertex},{10+33,edge},{10+2*33,face},{10+3*33,body},
	     {10+4*40,groundplane},{10+5*40,axes},
	     {20+6*40,wire},{20+7*40,smooth},
	     {20+8*40,perspective}],
    put_state(#io{eq=queue:new(),icons=Icons}).

resize(W, H) ->
    Io = get_state(),
    Tex = load_textures(),
    put_state(Io#io{w=W,h=H,tex=Tex}).

menubar(Menubar) ->
    Io = get_state(),
    put_state(Io#io{menubar=Menubar}).

progress(Message, Percent) ->
    display(fun(W, H) ->
		    draw_message(fun() -> progress_1(Message, Percent) end)
	    end).

progress_1(Message, Percent) ->
    text_at(0, Message),
    X = length(Message) * ?CHAR_WIDTH,
    embossed_rect(X, -?LINE_HEIGHT+3, 100, ?LINE_HEIGHT+3),
    gl:color3f(0.0, 0.0, 1.0),
    gl:recti(X, -?CHAR_HEIGHT, X+Percent, 3).

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
    gl:swapBuffers().

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
    beveled_rect(0, 0, W-2*Margin, 4*H div 6),
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
	       undefined -> Info;
	       Other -> Msg
	   end,
    draw_message(fun() -> text_at(0, Text) end),
    Io0.

draw_panes(#io{w=W,h=H,menubar=Bar,sel=Sel}=Io) ->
    beveled_rect(-1, 0, W+2, ?LINE_HEIGHT+6),
    beveled_rect(-1, H-2*?LINE_HEIGHT, W+2, 4*?LINE_HEIGHT+1),
    embossed_rect(6, H-2*?LINE_HEIGHT+5, W-10, 2*?LINE_HEIGHT-8),
    draw_bar(0, Bar, Sel).

-define(MENU_MARGIN, 8).
-define(MENU_ITEM_SPACING, 3).

draw_bar(X, [{Name,Item}|T], Sel) ->
    W = ?CHAR_WIDTH*(?MENU_ITEM_SPACING+length(Name)),
    if
	Item =:= Sel ->
	    embossed_rect(X+1, 3, W, ?LINE_HEIGHT);
	true -> ok
    end,
    text_at(?MENU_MARGIN+X, ?LINE_HEIGHT-1, Name),
    draw_bar(X+W, T, Sel);
draw_bar(X, [], Sel) -> ok.

draw_icons(#io{w=W,h=H,icons=Icons}, St) ->
    beveled_rect(-1, H-4*?LINE_HEIGHT-4, W+2, 2*?LINE_HEIGHT+3),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    Y = H-4*?LINE_HEIGHT-2,
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
icon_button(smooth=Name, St) ->
    icon_button(Name, smooth_preview, true);
icon_button(perspective=Name, St) ->
    icon_button(Name, orthogonal_view, false);
icon_button(Name, #st{selmode=Name}) -> {Name,down};
icon_button(Name, St) -> {Name,up}.

icon_button(Name, Key, Val) ->
    case wings_pref:get_value(Key) of
	Val -> {Name,down};
	_ -> {Name,up}
    end.
	    
button(X, Y) when Y > ?LINE_HEIGHT; X < ?MENU_MARGIN ->
    #io{h=H,icons=Icons} = get_state(),
    put_state((get_state())#io{sel=undefined}),
    case H-4*?LINE_HEIGHT-3 of
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

icon_row_hit(X, [{Pos,Name}|Is]) when Pos =< X, X < Pos+32 ->
    Action = case Name of
		 groundplane -> {view,show_groundplane};
		 axes -> {view,show_axes};
		 wire -> {view,wire_mode};
		 smooth -> {view,smooth_preview};
		 perspective -> {view,orthogonal_view};
		 Other -> {select,Name}
	     end,
    putback_event({action,Action}),
    none;
icon_row_hit(X, [_|Is]) ->
    icon_row_hit(X, Is);
icon_row_hit(X, []) -> none.

embossed_rect(X, Y, Mw, Mh) ->
    beveled_rect(X+Mw, Y+Mh, -Mw, -Mh).

beveled_rect(X, Y, Mw, Mh) ->
    gl:color3f(0.75, 0.75, 0.75),
    gl:recti(X, Y, X+Mw, Y+Mh),
    gl:'begin'(?GL_LINE_LOOP),
    gl:color3f(0.95, 0.95, 0.95),
    gl:vertex2i(X, Y+Mh),
    gl:vertex2i(X, Y),
    gl:color3f(0.25, 0.25, 0.25),
    gl:vertex2i(X+Mw, Y),
    gl:vertex2i(X+Mw, Y+Mh),
    gl:'end'(),
    gl:color3f(0.0, 0.0, 0.0).

space_at(X, Y) ->
    gl:color3f(0.75, 0.75, 0.75),
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
    glu:ortho2D(0.0, float(W), float(H), 0.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:color3f(0.0, 0.0, 0.0),
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
    draw_icon(X, Y, 32, 32, Icon).
    
draw_icon(X, Y, W, H, Icon) ->
    #io{tex=Tex} = get_state(),
    {value,{Icon,Id}} = keysearch(Icon, 1, Tex),
    gl:bindTexture(?GL_TEXTURE_2D, Id),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0.0, 1.0),
    gl:vertex2i(X, Y),
    gl:texCoord2f(0.0, 0.0),
    gl:vertex2i(X, Y+H),
    gl:texCoord2f(1.0, 0.0),
    gl:vertex2i(X+W, Y+H),
    gl:texCoord2f(1.0, 1.0),
    gl:vertex2i(X+W, Y),
    gl:'end'(),
    gl:color3f(0.0, 0.0, 0.0),
    ok.

load_textures() ->
    Dir = filename:dirname(code:which(?MODULE)),
    IconFile = filename:join(Dir, "wings_icon.bundle"),
    case file:read_file(IconFile) of
	{ok,Bin} ->
	    case catch binary_to_term(Bin) of
		{'EXIT',_} -> [];
		Icons0 ->
		    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
		    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
		    Icons = create_buttons(Icons0),
		    Tex = create_textures(Icons, 1),
		    gl:popAttrib(),
		    Tex
	    end;
	{error,Reason} -> []
    end.

create_textures([{Name,{W,H,Icon}}|T], Id) ->
    Size = size(Icon),
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
    flatmap(fun({Name,{32,32,Icon}}) ->
		    [{{Name,down},create_button(fun button_down/5, Icon)},
		     {{Name,up},create_button(fun button_up/5, Icon)}];
	       ({Name,Icon}=T) -> [T]
	    end, Icons0).

create_button(Tr, Icon) ->
    create_button(Tr, Icon, 0, []).

create_button(Tr, <<R:8,G:8,B:8,T/binary>>, I, Acc) ->
    X = I band 16#1F,
    Y = 31 - (I bsr 5),
    create_button(Tr, T, I+1, [Tr(X, Y, R, G, B)|Acc]);
create_button(Tr, <<>>, I, Acc) ->
    {32,32,list_to_binary(reverse(Acc))}.

button_down(X, Y, R, G, B) ->
    if
	Y == 0; X == 0 -> [0,0,0];
	Y == 31; X == 31 -> [255,255,255];
	true -> [R,G,B]
    end.

button_up(X, Y, R, G, B) ->
    if
	Y == 0; X == 0 -> [255,255,255];
	Y == 31; X == 31 -> [0,0,0];
	true -> [R,G,B]
    end.

%%%
%%% Input.
%%%

flush_events() ->
    #io{eq={In,Out}} = Io = get_state(),
    flush_events_1(),
    put_state(Io#io{eq=queue:new()}),
    ok.

flush_events_1() ->
    case sdl_events:peepEvents(16, ?SDL_GETEVENT, ?SDL_ALLEVENTS) of
	{0,[]} -> ok;
	{N,Evs} -> flush_events_1()
    end.

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
    erlang:cancel_timer(Ref),
    receive
	{timeout,Ref,_} -> ok
    after 0 -> ok
    end.

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

%%%
%%% Event loop handling.
%%%

enter_event_loop(Init) ->
    handle_response(Init, system_dummy_event, []).
    
event_loop([Handler|_]=Stk) ->
    Event = get_event(),
    handle_event(Handler, Event, Stk);
event_loop([]) -> ok.

handle_event(_, {system_init_event,Handler}, Stk) ->
    Res = Handler(),
    handle_response(Res, system_dummy_event, Stk);
handle_event(Handler, Event, Stk) ->
    %%io:format("~p: ~p\n", [Event,Stk]),
    case catch Handler(Event) of
	{'EXIT',Reason} ->
	    CrashHandler = last(Stk),
	    CrashHandler({crash,Reason});
	Res ->
	    handle_response(Res, Event, Stk)
    end.

handle_response(Res, Event, Stk) ->
    case Res of
	keep -> event_loop(Stk);
	next -> next_handler(Event, Stk);
	pop -> pop(Stk);
	{seq,First,Then} ->
	    handle_response({init,fun() -> Then end,First},
			    Event, Stk);
	{init,More,NewRes} ->
	    putback_event({system_init_event,More}),
	    handle_response(NewRes, Event, Stk);
	{replace,Top} -> replace_top(Top, Stk);
	{push,Top} -> event_loop([Top|Stk])
    end.

pop([_|Stk]) ->
    event_loop(Stk).

replace_top(Top, [_|Stk]) ->
    event_loop([Top|Stk]).

next_handler(Event, [_|[Next|_]=Stk]) ->
    handle_event(Next, Event, Stk).
