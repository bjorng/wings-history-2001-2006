%%
%%  wings_menu.erl --
%%
%%     Implementation of pulldown and popup menus.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_menu.erl,v 1.24 2002/01/27 22:01:27 bjorng Exp $
%%

-module(wings_menu).
-export([is_popup_event/1,menu/5,popup_menu/5,build_command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,keysearch/3,duplicate/2]).

-define(SUB_MENU_TIME, 150).
-define(SEPARATOR_HEIGHT, 10).

%% Menu information kept for a popup menu.
-record(mi,
	{xleft,					%X left
	 ytop,					%Y top
	 ymarg,					%Margin at top and bottom
	 shortcut,				%Position for shortcut (chars)
	 w,					%Width of menu (pixels)
	 h,					%Height of menu (pixels)
	 hs,					%Tuple: height of each entry.
	 sel=none,				%Selected item (1..size(Menu))
	 ns=[],					%Name stack.
	 menu,					%Original menu term
	 new=true,				%If just opened, ignore
						%button release.
	 timer=make_ref(),			%Active submenu timer.
	 return_timer=none,			%Timer for returning to parent.
	 prev=[],				%Previous menu.
	 redraw,				%Redraw parent fun.
	 st,					%State record.
	 num_redraws=0,				%Total number of redraws.
	 adv					%Advanced menus (true|false).
	}).

%%%
%%% Inside this module, each entry in a menu is kept in the following
%%% normalized format:
%%%
%%%   separator      OR
%%%   {Text,Name,Hotkey,Help,Properties}
%%%

is_popup_event(#mousebutton{button=3,x=X,y=Y,state=State}) ->
    case wings_pref:get_value(advanced_menus) of
	true when State =:= ?SDL_RELEASED -> {yes,X,Y};
	false when State =:= ?SDL_PRESSED  -> {yes,X,Y};
	Other -> no
    end;
is_popup_event(Event) -> no.

menu(X, Y, Name, Menu, Redraw) ->
    Mi = menu_setup(plain, X, Y, Name, Menu, store_redraw(#mi{}, Redraw)),
    top_level(Mi).

popup_menu(X, Y, Name, Menu, Redraw) ->
    Mi = menu_setup(popup, X, Y, Name, Menu, store_redraw(#mi{}, Redraw)),
    top_level(Mi).

store_redraw(Mi, #st{}=St0) ->
    Redraw = fun() ->
		     St = wings_draw:render(St0),
		     wings_io:info(wings:info(St)),
		     wings_io:draw_ui(St)
	     end,
    Mi#mi{redraw=Redraw,st=St0,num_redraws=0};
store_redraw(Mi, Redraw) when is_function(Redraw) ->
    Mi#mi{redraw=Redraw}.

top_level(Mi) ->
    {seq,{push,dummy},get_menu_event(Mi)}.

menu_setup(Type, X0, Y0, Name, Menu0, #mi{ns=Names0}=Mi) when is_list(Menu0) ->
    Adv = wings_pref:get_value(advanced_menus),
    Names = [Name|Names0],
    Menu1 = wings_plugin:menu(list_to_tuple(reverse(Names)), Menu0),
    Hotkeys = hotkeys(Names),
    Menu = normalize_menu(Menu1, Hotkeys, Adv),
    {MwL,MwR,Hs} = menu_dims(Menu),
    TotalW = (MwL+MwR) * ?CHAR_WIDTH + 9*?CHAR_WIDTH,
    Mh = lists:sum(Hs),
    Margin = 3,
    {X1,Y1} = case Type of
		  plain ->
		      {X0,Y0};
		  popup when Adv == false ->
		      {X0-TotalW div 2,Y0 - Margin div 2};
		  popup ->
		      {X0-TotalW div 2,Y0 - Margin - ?CHAR_HEIGHT}
	      end,
    {X,Y} = move_if_outside(X1, Y1, TotalW, Mh+2*Margin, Mi),
    Mi#mi{xleft=X,ytop=Y,ymarg=Margin,
	  shortcut=MwL+2,w=TotalW-10,h=Mh,hs=Hs,
	  sel=none,ns=Names,menu=Menu,adv=Adv}.

menu_show(#mi{xleft=X,ytop=Y,ymarg=Margin,shortcut=Shortcut,w=Mw,h=Mh}=Mi) ->
    wings_io:raised_rect(X, Y, Mw, Mh + 2*Margin+3, ?MENU_COLOR),
    gl:color3f(0, 0, 0),
    menu_draw(X+3*?CHAR_WIDTH, Y+Margin+?CHAR_HEIGHT,
	      Shortcut, Mw, 1, Mi#mi.hs, Mi).

normalize_menu(Menu, Hotkeys, Adv) ->
    normalize_menu(Menu, Hotkeys, Adv, []).

normalize_menu([{advanced,_}|Els], Hotkeys, false, Acc) ->
    normalize_menu(Els, Hotkeys, false, Acc);
normalize_menu([{advanced,El}|Els], Hotkeys, true, Acc) ->
    normalize_menu([El|Els], Hotkeys, true, Acc);
normalize_menu([Elem0|Els], Hotkeys, Adv, Acc) ->
    Elem1 = case Elem0 of
		{S,Name,Help,Ps} ->
		    {S,Name,[],Help,Ps};
		{S,Name,[C|_]=Help} when is_integer(C) ->
		    {S,Name,[],Help,[]};
		{S,{Name},Ps} ->
		    {S,Name,[],[],[hotbox|Ps]};
		{S,Name,Ps} ->
		    {S,Name,[],[],Ps};
		{S,Name} ->
		    {S,Name,[],[],[]};
		separator ->
		    Name = none,
		    separator
	    end,
    Elem = case keysearch(Name, 1, Hotkeys) of
	       false -> Elem1;
	       {value,{_,Hotkey}} -> setelement(3, Elem1, Hotkey)
	   end,
    normalize_menu(Els, Hotkeys, Adv, [Elem|Acc]);
normalize_menu([], Hotkeys, Adv, Acc) -> list_to_tuple(reverse(Acc)).

menu_dims(Menu) ->
    menu_dims(Menu, 1, 0, 0, []).

menu_dims(Menu, Last, MaxA, MaxB, H) when Last > size(Menu) ->
    {MaxA,MaxB,reverse(H)};
menu_dims(Menu, I, MaxA0, MaxB0, Hacc) ->
    {Wa,Wb,H} =
	case element(I, Menu) of
	    {S,{_,_},Hotkey,_,Ps} ->
		{length(S)+1,length(Hotkey),?LINE_HEIGHT};
	    {S,_,Hotkey,_,Ps} ->
		case property_lists:is_defined(hotbox, Ps) of
		    false ->
			{length(S),length(Hotkey),?LINE_HEIGHT};
		    true ->
			{length(S)+1,length(Hotkey),?LINE_HEIGHT}
		end;
	    separator -> {0,0,?SEPARATOR_HEIGHT}
	end,
    menu_dims(Menu, I+1, max(Wa, MaxA0), max(Wb, MaxB0), [H|Hacc]).

max(A, B) when A > B -> A;
max(A, B) -> B.

hotkeys(Names) ->
    R = sofs:relation(wings_hotkey:matching(Names)),
    F = sofs:relation_to_family(R),
    [{Name,Key} || {Name,[Key|_]} <- sofs:to_external(F)].

%%%
%%% Event loop for menus.
%%%

get_menu_event(Mi0) ->
    Mi = redraw(Mi0),
    {replace,fun(Ev) -> handle_menu_event(Ev, Mi) end}.

handle_menu_event(Event, #mi{new=New}=Mi0) ->
    case Event of
	#keyboard{keysym=KeySym} ->
	    handle_key(KeySym, Mi0);
	#mousemotion{x=X,y=Y} ->
	    Mi1 = update_highlight(X, Y, Mi0),
	    case motion_outside(Event, Mi0) of
		none ->
		    Mi = set_submenu_timer(Mi1, Mi0, X, Y),
		    get_menu_event(Mi#mi{new=false});
		Other -> Other
	    end;
	#mousebutton{} ->
	    button_pressed(Event, Mi0);
	{go_back,NewEvent} ->
	    Mi1 = Mi0#mi.prev,
	    Mi = Mi1#mi{timer=short,num_redraws=0,new=false,sel=none},
	    handle_menu_event(NewEvent, Mi);
	redraw ->
	    get_menu_event(Mi0#mi{new=false,num_redraws=0});
	{expose} ->
	    get_menu_event(Mi0#mi{new=false,num_redraws=0});
	{resize,W,H}=Resize ->
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    wings_io:clear_menu_sel(),
	    wings_io:putback_event(Resize),
	    pop;
	#mi{}=Mi -> get_menu_event(Mi);
	quit ->
	    wings_io:clear_menu_sel(),
	    wings_io:putback_event(quit),
	    pop;
	IgnoreMe ->
	    get_menu_event(Mi0#mi{new=false})
    end.

button_pressed(#mousebutton{button=B0,x=X,y=Y,state=?SDL_RELEASED}=Event,
	       #mi{new=New}=Mi) when not New and (B0 =< 3) ->
    B = virtual_button(B0),
    button_pressed(Event, B, X, Y, Mi);
button_pressed(Other, Mi) ->
    get_menu_event(Mi#mi{new=false}).

button_pressed(Event, Button, X, Y, #mi{ns=Names,menu=Menu,adv=Adv}=Mi0) ->
    clear_timer(Mi0),
    Mi = update_highlight(X, Y, Mi0),
    case selected_item(X, Y, Mi) of
	outside -> button_outside(Event, Mi);
	none -> get_menu_event(Mi);
	Item when integer(Item) ->
	    case element(Item, Menu) of
		{_,{Name,Submenu},_,_,_} when Adv == true ->
		    popup_submenu(Button, X, Y, Name, Submenu, Mi);
		{_,{Name,Submenu},_,_,_} when Adv == false ->
		    submenu(Item, Name, Submenu, Mi);
		{_,Act0,_,_,_} when is_function(Act0) ->
		    case Act0(Button, Names) of
			ignore -> keep;
			Act when is_tuple(Act) ->
			    do_action(Act)
		    end;
		{_,Act0,_,_,Ps} when is_atom(Act0); is_integer(Act0) ->
		    Act = check_hotbox(Act0, X, Ps, Mi),
		    do_action(build_command(Act, Names))
	    end
    end.

do_action(Action) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings_io:clear_menu_sel(),
    wings_io:putback_event({action,Action}),
    wings_io:putback_event(redraw),
    pop.

handle_key(#keysym{sym=27}, Mi) ->		%Escape.
    wings_io:clear_menu_sel(),
    wings_io:putback_event(redraw),
    pop;
handle_key(#keysym{sym=?SDLK_DELETE}, Mi0) ->
    %% Delete hotkey bound to this entry.
    case current_command(Mi0) of
	none -> keep;
	Cmd ->
	    NextKey = wings_hotkey:delete_by_command(Cmd),
	    Mi = set_hotkey(NextKey, Mi0),
	    get_menu_event(Mi)
    end;
handle_key(#keysym{sym=?SDLK_INSERT}, Mi) ->
    %% Define new hotkey for this entry.
    case current_command(Mi) of
	none -> keep;
	Cmd -> get_hotkey(Cmd, Mi)
    end;
handle_key(_, Mi) -> keep.

current_command(#mi{sel=none}) -> none;
current_command(#mi{sel=Sel,menu=Menu,ns=Names}) ->
    case element(Sel, Menu) of
	{_,Name,_,_,_} when is_atom(Name) ->
	    build_command(Name, Names);
	Other -> none
    end.

virtual_button(1) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?ALT_BITS =/= 0 -> 2;
	Mod -> 1
    end;
virtual_button(2) ->2;
virtual_button(3) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?CTRL_BITS =/= 0 -> 2;
	Mod -> 3
    end.
    
set_hotkey(Val, #mi{sel=Sel,menu=Menu0}=Mi) ->
    case element(Sel, Menu0) of
	{A,B,_,D,E} ->
	    Menu = setelement(Sel, Menu0, {A,B,Val,D,E}),
	    Mi#mi{menu=Menu};
	Other -> Mi
    end.

popup_submenu(Button, X, Y, SubName, SubMenu0, #mi{ns=Ns}=Mi0) ->
    %% Only in advanced menu mode.
    case expand_submenu(Button, SubName, SubMenu0, Mi0) of
	Action when is_tuple(Action); is_atom(Action) ->
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    wings_io:clear_menu_sel(),
	    wings_io:putback_event({action,Action}),
	    wings_io:putback_event(redraw),
	    pop;
	SubMenu when is_list(SubMenu) ->
	    Mi = menu_setup(popup, X, Y, SubName, SubMenu, Mi0),
	    handle_menu_event(redraw, Mi)
    end.

submenu(I, Name, Menu0, Mi0) ->
    Menu = expand_submenu(1, Name, Menu0, Mi0),
    #mi{xleft=Xleft,ytop=Ytop,w=W,h=H,hs=Hs}=Mi0,
    X = Xleft+W-?CHAR_WIDTH,
    Y = get_item_pos(I, Hs, Ytop-?LINE_HEIGHT),
    Redraw = new_redraw_fun(Mi0),
    Mi1 = Mi0#mi{prev=Mi0,redraw=Redraw},
    Mi = menu_setup(plain, X, Y, Name, Menu, Mi1),
    get_menu_event(Mi).

get_item_pos(0, Hs, Pos) -> Pos;
get_item_pos(I, [H|Hs], Pos) -> get_item_pos(I-1, Hs, Pos+H).

expand_submenu(B, Name, Submenu0, #mi{ns=Ns}) when is_function(Submenu0) ->
    Submenu0(B, [Name|Ns]);
expand_submenu(Button, Name, Submenu, Mi) -> Submenu.

button_outside(#mousebutton{x=X,y=Y}=Event, #mi{prev=[]}=Mi) ->
    case wings_io:button(X, Y) of
	none -> ok;
	ButtonHit ->
	    wings_io:putback_event({action,ButtonHit})
    end,
    wings_io:putback_event(redraw),
    pop;
button_outside(#mousebutton{x=X,y=Y}=Event, #mi{prev=PrevMenu}=Mi) ->
    #mi{sel=PrevSel} = PrevMenu,
    case selected_item(X, Y, PrevMenu) of
	PrevSel -> get_menu_event(Mi);
	Other ->
	    wings_io:putback_event(Event),
	    wings_io:putback_event(redraw),
	    pop
    end.

motion_outside(#mousemotion{x=X,y=Y}=Event, #mi{prev=[]}=Mi) -> none;
motion_outside(#mousemotion{x=X,y=Y}=Event, #mi{prev=PrevMenu0}=Mi) ->
    #mi{sel=PrevSel} = PrevMenu0,
    case selected_item(X, Y, PrevMenu0) of
	outside -> none;
	none -> none;
	PrevSel -> keep;
	OtherSel ->
	    #mi{return_timer=Timer0} = Mi,
	    PrevMenu = PrevMenu0#mi{sel=OtherSel},
	    case Timer0 of
		none ->
		    Tim = wings_io:set_timer(?SUB_MENU_TIME, {go_back,Event}),
		    Timer = {OtherSel,Tim},
		    get_menu_event(Mi#mi{prev=PrevMenu,return_timer=Timer});
		{_,Tim0} ->
		    wings_io:cancel_timer(Tim0),
		    wings_io:putback_event({go_back,Event}),
		    get_menu_event(Mi#mi{prev=PrevMenu,return_timer=none})
	    end
    end.

clear_timer(#mi{timer=short}) -> ok;
clear_timer(#mi{timer=Timer}) -> wings_io:cancel_timer(Timer).

set_submenu_timer(#mi{return_timer=none}=Mi, OldMi, X, Y) ->
    set_submenu_timer_1(Mi, OldMi, X, Y);
set_submenu_timer(#mi{return_timer={_,Timer}}=Mi, OldMi, X, Y) ->
    wings_io:cancel_timer(Timer),
    set_submenu_timer_1(Mi, OldMi, X, Y).

set_submenu_timer_1(#mi{sel=Sel}=Mi, #mi{sel=Sel}, X, Y) -> Mi;
set_submenu_timer_1(#mi{sel=Sel,timer=OldTimer}=Mi, OldMi, X, Y) ->
    clear_timer(OldMi),
    case is_submenu(Sel, Mi) of
	false -> Mi;
	true ->
	    Event = #mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
	    Time = case OldTimer of
		       short -> 1;
		       _ -> ?SUB_MENU_TIME
		   end,
	    Timer = wings_io:set_timer(Time, Event),
	    Mi#mi{timer=Timer}
    end.

redraw(#mi{redraw=Redraw,st=St,num_redraws=NumRedraws}=Mi) ->
    if
	NumRedraws < 2 ->
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    Redraw();
	true -> ok
    end,
    wings_io:ortho_setup(),
    #mi{prev=PrevMenu} = Mi,
    case PrevMenu of
	[] -> ok;
	#mi{return_timer=none} -> ok;
	#mi{} -> menu_show(PrevMenu)
    end,
    wings_io:draw_ui(St),
    wings_io:ortho_setup(),
    menu_show(Mi),
    help_text(Mi),
    gl:swapBuffers(),
    Mi#mi{num_redraws=NumRedraws+1}.

update_highlight(X, Y, Mi) ->
    case selected_item(X, Y, Mi) of
	NoSel when NoSel == outside; NoSel == none ->
	    Mi#mi{sel=none};
	Item when integer(Item) ->
	    Mi#mi{sel=Item}
    end.

check_hotbox(Act, X, Ps, #mi{xleft=Xleft,w=W})
  when Xleft =< X, X < Xleft+W-2*?CHAR_WIDTH -> Act;
check_hotbox(Act, X, Ps, Mi) ->
    %% Hotbox position was clicked. Now - do we have a hotbox there?
    case property_lists:is_defined(hotbox, Ps) of
	false -> Act;
	true -> {Act}
    end.

new_redraw_fun(#mi{redraw=Redraw0}=Mi) ->
    fun() ->
	    Redraw0(),
	    wings_io:ortho_setup(),
	    menu_show(Mi)
    end.

selected_item(X0, Y0, #mi{xleft=Xleft,ytop=Ytop,ymarg=Margin,w=W,h=H}=Mi) ->
    if
	Xleft =< X0, X0 < Xleft+W,
	Ytop+Margin =< Y0, Y0 < Ytop+H+Margin ->
	    selected_item_1(Y0-Ytop-Margin, Mi);
	Xleft =< X0, X0 < Xleft+W,
	Ytop-2 =< Y0,  Y0 < Ytop+H+2*Margin -> none;
	true -> outside
    end.

selected_item_1(Y, #mi{hs=Hs,menu=Menu}) ->
    selected_item_1(Y, 1, Hs, Menu).
selected_item_1(Y0, I, [H|Hs], Menu) ->
    case Y0 - H of
	Y when Y =< 0 ->
	    case element(I, Menu) of
		separator -> none;
		{Text,ignore,_,_,_} -> none;
		Other -> I
	    end;
	Y -> selected_item_1(Y, I+1, Hs, Menu)
    end.
	    
is_submenu(I, #mi{menu=Menu,adv=true}) -> false;
is_submenu(I, #mi{menu=Menu}) when is_integer(I) ->
    case element(I, Menu) of
	separator -> false;
	{Text,{_,_},Hotkey,Help,Ps} -> true;
	Other -> false
    end;
is_submenu(I, Mi) -> false.

build_command(Name, Names) ->
    foldl(fun(N, A) -> {N,A} end, Name, Names).
	    
menu_draw(X, Y, Shortcut, Mw, I, [], Mi) -> ok;
menu_draw(X, Y, Shortcut, Mw, I, [H|Hs], #mi{menu=Menu}=Mi) ->
    ?CHECK_ERROR(),
    case element(I, Menu) of
	separator -> draw_separator(X, Y, Mw);
	{Text,{_,_}=Item,Hotkey,Help,Ps} ->
	    item_colors(Y, Ps, I, Mi),
	    wings_io:menu_text(X, Y, Text),
	    draw_submenu(Item, X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3);
	{Text,Item,Hotkey,Help,Ps} ->
	    Str = Text ++ duplicate(Shortcut-length(Text), $\s) ++ Hotkey,
	    item_colors(Y, Ps, I, Mi),
	    draw_menu_text(X, Y, Str, Ps),
	    draw_hotbox(Item, X+Mw-5*?CHAR_WIDTH,
			Y-?CHAR_HEIGHT div 3, Ps)
    end,
    ?CHECK_ERROR(),
    menu_draw(X, Y+H, Shortcut, Mw, I+1, Hs, Mi).

draw_menu_text(X, Y, Text, Props) ->
    case property_lists:is_defined(crossmark, Props) of
	false ->
	    wings_io:menu_text(X, Y, Text);
	true ->
	    wings_io:menu_text(X-2*?CHAR_WIDTH, Y, [crossmark,$\s|Text])
    end.

item_colors(Y, Ps, Sel, #mi{sel=Sel,xleft=Xleft,w=W,menu=Menu}) ->
    %% Draw blue background for highlighted item.
    gl:color3f(0, 0, 0.5),
    Right = case property_lists:is_defined(hotbox, Ps) of
		true -> Xleft+W-3*?CHAR_WIDTH;
		false -> Xleft+W-?CHAR_WIDTH
	    end,
    gl:recti(Xleft+?CHAR_WIDTH, Y-?CHAR_HEIGHT, Right, Y+3),
    gl:color3f(1, 1, 1);			%White text
item_colors(Y, Ps, I, Mi) -> gl:color3f(0, 0, 0). %Black text

help_text(#mi{sel=none}) ->
    %% We don't want info display as wings_io:clear_message() would give.
    wings_io:message("");
help_text(#mi{menu=Menu,sel=Sel,ns=Names,adv=Adv}=Mi) ->
    case element(Sel, Menu) of
	{Text,{_,_},_,_,_} when Adv == false ->
	    %% No specific help text for submenus in basic mode.
	    Help = "The " ++ Text ++ " submenu.",
	    wings_io:message(Help);
        {_,{Name,Fun},_,_,_} when is_function(Fun) ->
	    %% "Submenu" in advanced mode.
	    Help0 = Fun(help, [Name|Names]),
	    Help = help_text_1(Help0, Adv),
	    wings_io:message(Help);
	{_,_,_,Help0,_} ->
	    %% Plain entry - not submenu.
	    Help = help_text_1(Help0, Adv),
	    wings_io:message(Help);
	separator -> ok
    end.

help_text_1([_|_]=S, false) -> S;
help_text_1({[_|_]=S,_}, false) -> S;
help_text_1({[_|_]=S,_,_}, false) -> S;
help_text_1([_|_]=S, true) ->
    [lmb|" " ++ S];
help_text_1({S1,S2}, true) ->
    [lmb|" " ++ S1 ++ [$\s,mmb,$\s] ++ S2];
help_text_1({S1,[],S2}, true) ->
    [lmb|" " ++ S1 ++  [$\s,rmb,$\s] ++ S2];
help_text_1({S1,S2,S3}, true) ->
    [lmb|" " ++ S1 ++ [$\s,mmb,$\s] ++ S2 ++ [$\s,rmb,$\s] ++ S3];
help_text_1([]=S, _) -> S.

draw_hotbox(Item, X, Y, Ps) ->
    case property_lists:is_defined(hotbox, Ps) of
	false ->
	    ok;
	true ->
	    wings_io:sunken_rect(X, Y-3,
				 ?CHAR_WIDTH, ?CHAR_WIDTH,
				 ?MENU_COLOR)
    end.

draw_submenu(Item, X, Y) when atom(Item); integer(Item); list(Item) -> ok;
draw_submenu(Item, X, Y) ->
    case wings_pref:get_value(advanced_menus) of
	true ->
	    wings_io:menu_text(X, Y, [submenu]);
	false ->
	    ?CHECK_ERROR(),
	    gl:'begin'(?GL_TRIANGLES),
	    gl:vertex2i(X-?CHAR_WIDTH div 2, Y),
	    gl:vertex2i(X-?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3),
	    gl:vertex2i(X-?CHAR_WIDTH, Y+?CHAR_HEIGHT div 3),
	    gl:'end'(),
	    ?CHECK_ERROR()
    end.

draw_separator(X, Y, Mw) ->
    ?CHECK_ERROR(),
    LeftX = X-2*?CHAR_WIDTH,
    RightX = X+Mw-4*?CHAR_WIDTH,
    UpperY = Y - ?SEPARATOR_HEIGHT + 1,
    LowerY = UpperY + 1,
    gl:lineWidth(1.0),
    gl:'begin'(?GL_LINES),
    gl:color3f(0.10, 0.10, 0.10),
    gl:vertex2f(LeftX+0.5, UpperY+0.5),
    gl:vertex2f(RightX+0.5, UpperY+0.5),
    gl:color3f(0.90, 0.90, 0.90),
    gl:vertex2f(LeftX+1.5, LowerY+0.5),
    gl:vertex2f(RightX+0.5, LowerY+0.5),
    gl:'end'(),
    gl:color3f(0, 0, 0),
    ?CHECK_ERROR().

move_if_outside(X, Y, Mw, Mh, Mi) ->
    [_,_,W,H0] = gl:getIntegerv(?GL_VIEWPORT),
    H = H0 - 40,
    if
	X+Mw > W ->
	    NewX = left_of_parent(Mw, W, Mi),
	    move_if_outside(NewX, Y, Mw, Mh, Mi);
	Y+Mh > H ->
	    move_if_outside(X, H-Mh, Mw, Mh, Mi);
	true ->
	    move_if_outside_x(X, Y)
    end.

move_if_outside_x(X, Y) when X < 0 ->
    {0,Y};
move_if_outside_x(X, Y) ->
    {X,Y}.

left_of_parent(Mw, W, #mi{prev=[]}) -> W-Mw;
left_of_parent(Mw, _, #mi{prev=#mi{xleft=X}}) -> X-Mw+10.


%%%
%%% Get a key to bind a command to.
%%%

get_hotkey(Cmd, #mi{st=St}=Mi) ->
    wings_io:message("Press key to bind command to."),
    wings_io:draw_ui(St),
    gl:swapBuffers(),
    {seq,{push,dummy},
     {replace,fun(Ev) ->
		      handle_key_event(Ev, Cmd, Mi)
	      end}}.

handle_key_event(Ev, Cmd, Mi0) ->
    case wings_hotkey:bind_from_event(Ev, Cmd) of
	error -> keep;
	Keyname when is_list(Keyname) ->
	    Mi = set_hotkey(Keyname, Mi0),
	    wings_io:putback_event(Mi),
	    pop
    end.
