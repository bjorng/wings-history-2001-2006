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
%%     $Id: wings_menu.erl,v 1.51 2002/07/26 17:25:03 bjorng Exp $
%%

-module(wings_menu).
-export([is_popup_event/1,menu/4,popup_menu/4,popup_menu/5,build_command/2]).
-export([menu/5,popup_menu/5]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,keysearch/3]).

-define(SUB_MENU_TIME, 150).
-define(SEPARATOR_HEIGHT, 9).
-define(INITIAL_LEVEL, 100).

%% Menu information kept for a popup menu.
-record(mi,
	{ymarg,					%Margin at top and bottom
	 shortcut,				%Position for shortcut (chars)
	 w,					%Width of menu (pixels)
	 h,					%Height of menu (pixels)
	 hs,					%Tuple: height of each entry.
	 sel=none,				%Selected item (1..size(Menu))
	 ns=[],					%Name stack.
	 menu,					%Original menu term
	 timer=make_ref(),			%Active submenu timer.
	 level=?INITIAL_LEVEL,			%Menu level.
	 st,					%State record.
	 adv,					%Advanced menus (true|false).
	 ignore_rel=true,			%Ignore button release if
						% just openened.
	 type=plain				%Type of menu: plain|popup
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
	true when State =:= ?SDL_RELEASED ->
	    {yes,X,Y,sdl_keyboard:getModState()};
	false when State =:= ?SDL_PRESSED  ->
	    {yes,X,Y,sdl_keyboard:getModState()};
	_Other -> no
    end;
is_popup_event(_Event) -> no.

menu(X, Y, Name, Menu, _) ->			%Obsolete.
    menu(X, Y, Name, Menu).

popup_menu(X, Y, Name, Menu, _) ->		%Obsolete.
    popup_menu(X, Y, Name, Menu).

menu(X, Y, Name, Menu) ->
    menu_setup(plain, X, Y, Name, Menu, #mi{adv=false}).

popup_menu(X, Y, Name, Menu) ->
    Adv = wings_pref:get_value(advanced_menus),
    IgnoreRel = (Adv == false),
    menu_setup(popup, X, Y, Name, Menu, #mi{adv=Adv,ignore_rel=IgnoreRel}).

menu_setup(Type, X0, Y0, Name, Menu0, #mi{ns=Names0,adv=Adv}=Mi0) ->
    Names = [Name|Names0],
    Menu1 = wings_plugin:menu(list_to_tuple(reverse(Names)), Menu0),
    Hotkeys = hotkeys(Names),
    Menu = normalize_menu(Menu1, Hotkeys, Adv),
    {MwL,MwM,MwR,Hs} = menu_dims(Menu),
    TotalW = (MwL+MwM+MwR) * ?CHAR_WIDTH + 8*?CHAR_WIDTH,
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
    {X,Y} = move_if_outside(X1, Y1, TotalW, Mh+2*Margin, Mi0),
    W = TotalW-10,
    Mi = Mi0#mi{ymarg=Margin,shortcut=MwL+1,w=TotalW-10,h=Mh,hs=Hs,
		sel=none,ns=Names,menu=Menu,adv=Adv,type=Type},
    #mi{level=Level} = Mi,
    Op = {seq,{push,dummy},get_menu_event(Mi)},
    WinName = {menu,Level},
    wings_wm:delete(WinName),
    wings_wm:new(WinName, {X,Y,Level}, {W,Mh+10}, Op),
    if
	Level == ?INITIAL_LEVEL ->
	    wings_wm:set_active(WinName);
	true -> ok
    end,
    keep.

menu_show(#mi{ymarg=Margin,shortcut=Shortcut,w=Mw,h=Mh}=Mi) ->
    wings_io:border(0, 0, Mw, Mh + 2*Margin+3, ?MENU_COLOR),
    gl:color3f(0, 0, 0),
    menu_draw(3*?CHAR_WIDTH, Margin+?CHAR_HEIGHT,
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
		    {S,Name,[],Help,Props=adv_filter(Adv, Ps)};
		{S,Name,[C|_]=Help} when is_integer(C) ->
		    {S,Name,[],Help,Props=[]};
		{S,Name,Ps} ->
		    {S,Name,[],[],Props=adv_filter(Adv, Ps)};
		{S,Name} ->
		    {S,Name,[],[],Props=[]};
		separator ->
		    Name = none,
		    Props = [],
		    separator
	    end,
    Elem = norm_add_hotkey(Name, Elem1, Hotkeys, Props),
    normalize_menu(Els, Hotkeys, Adv, [Elem|Acc]);
normalize_menu([], _Hotkeys, _Adv, Acc) -> list_to_tuple(reverse(Acc)).

adv_filter(false, [magnet|T]) -> adv_filter(false, T);
adv_filter(Flag, [H|T]) -> [H|adv_filter(Flag, T)];
adv_filter(_, []) -> [].

norm_add_hotkey(_, separator, _, _) -> separator;
norm_add_hotkey(_, Elem, [], _) -> Elem;
norm_add_hotkey(Fun, Elem, Hotkeys, Props) when is_function(Fun) ->
    Name = reduce_name(Fun(1, [])),
    norm_add_hotkey(Name, Elem, Hotkeys, Props);
norm_add_hotkey(Name, Elem, Hotkeys, Props) ->
    Key = match_hotkey(Name, Hotkeys, have_option_box(Props)),
    setelement(3, Elem, Key).

match_hotkey(Name, [{Name,Key}|_], false) -> Key;
match_hotkey(Name, [{{Name,Bool},Key}|_], true) when Bool == true;
						     Bool == false ->
    Key;
match_hotkey(Name, [_|T], OptionBox) ->
    match_hotkey(Name, T, OptionBox);
match_hotkey(_, [], _) -> [].

reduce_name({Key,{_,_}=Tuple}) when is_atom(Key) ->
    reduce_name(Tuple);
reduce_name({Key,Val}) when is_atom(Key) -> Val;
reduce_name(Name) -> Name.
    
menu_dims(Menu) ->
    menu_dims(Menu, size(Menu), 0, 0, 0, []).

menu_dims(_Menu, 0, MaxA, MaxB, MaxC, H) -> {MaxA,MaxB,MaxC,H};
menu_dims(Menu, I, MaxA0, MaxB0, MaxC0, Hacc) ->
    {Wa,Wb,Wc,H} =
	case element(I, Menu) of
 	    {S,ignore,[],[],[]} when I == 1, length(S)+1 =< MaxA0+MaxB0 ->
 		{0,0,0,?LINE_HEIGHT};
	    {S,{_,_},Hotkey,_,Ps} ->
		{length(S),length(Hotkey),1,?LINE_HEIGHT};
	    {S,_,Hotkey,_,Ps} ->
		{length(S),length(Hotkey),right_width(Ps),?LINE_HEIGHT};
	    separator -> {0,0,0,?SEPARATOR_HEIGHT}
	end,
    menu_dims(Menu, I-1, max(Wa, MaxA0), max(Wb, MaxB0),
	      max(Wc, MaxC0), [H|Hacc]).

max(A, B) when A > B -> A;
max(_A, B) -> B.

right_width(Ps) ->
    case have_option_box(Ps) orelse have_magnet(Ps) of
	true -> 1;
	false -> 0
    end.

hotkeys(Names) ->
    R = sofs:relation(wings_hotkey:matching(Names)),
    F = sofs:relation_to_family(R),
    [{Name,Key} || {Name,[Key|_]} <- sofs:to_external(F)].

%%%
%%% Event loop for menus.
%%%

get_menu_event(Mi) ->
    {replace,fun(Ev) -> handle_menu_event(Ev, Mi) end}.

handle_menu_event(Event, Mi0) ->
    case Event of
	#keyboard{keysym=KeySym} ->
	    wings_wm:dirty(),
	    handle_key(KeySym, Mi0);
	#mousemotion{x=X,y=Y} ->
	    Mi1 = update_highlight(X, Y, Mi0),
	    case motion_outside(Event, Mi0) of
		none ->
		    Mi = set_submenu_timer(Mi1, Mi0, X, Y),
		    get_menu_event(Mi);
		Other -> Other
	    end;
	#mousebutton{} ->
	    button_pressed(Event, Mi0);
	redraw ->
	    redraw(Mi0),
	    keep;
	#mi{}=Mi ->				%Key bound/unbound.
	    help_text(Mi),
	    wings_wm:dirty(),
	    get_menu_event(Mi);
	clear_submenu ->
	    wings_wm:delete({menu,Mi0#mi.level+1}),
	    keep;
	quit ->
	    wings_io:clear_menu_sel(),
	    wings_io:putback_event(quit),
	    delete_all(Mi0);
	_IgnoreMe -> keep
    end.

button_pressed(#mousebutton{state=?SDL_RELEASED}, #mi{ignore_rel=true}=Mi) ->
    get_menu_event(Mi#mi{ignore_rel=false});
button_pressed(#mousebutton{button=B0,x=X,y=Y,state=?SDL_RELEASED}=Event,
	       #mi{adv=Adv}=Mi) when (B0 =< 3) ->
    wings_wm:dirty(),
    B = virtual_button(Adv, B0),
    button_pressed(Event, B, X, Y, Mi);
button_pressed(_, _) -> keep.

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
		{_,Act0,_,_,Ps} when is_function(Act0) ->
		    call_action(X, Act0, Button, Names, Ps, Mi);
		{_,Act0,_,_,Ps} when is_atom(Act0); is_integer(Act0) ->
		    Act = check_option_box(Act0, X, Ps, Mi),
		    do_action(build_command(Act, Names), Mi)
	    end
    end.

call_action(X, Act, Button, Ns, Ps, Mi) ->
    Mag = case have_magnet(Ps) andalso hit_right(X, Mi) of
	      true -> {magnet,Button};
	      false -> Button
	  end,
    case Act(Mag, Ns) of
	ignore -> keep;
	Cmd when is_tuple(Cmd) ->
	    do_action(Cmd, Mi)
    end.

do_action(Action, Mi) ->
    wings_io:clear_menu_sel(),
    wings_io:putback_event({action,Action}),
    delete_all(Mi).
	    
handle_key(#keysym{sym=27}, Mi) ->		%Escape.
    wings_io:clear_menu_sel(),
    delete_all(Mi);
handle_key(#keysym{sym=C}, Mi0) when C == ?SDLK_DELETE; C == $\\  ->
    %% Delete hotkey bound to this entry.
    case current_command(Mi0) of
	none -> keep;
	{Cmd0,OptionBox} ->
	    Cmd = add_option(OptionBox, Cmd0, false),
	    NextKey = wings_hotkey:delete_by_command(Cmd),
	    Mi = set_hotkey(NextKey, Mi0),
	    get_menu_event(Mi)
    end;
handle_key(#keysym{sym=C}, Mi) when C == ?SDLK_INSERT; C == $/ ->
    %% Define new hotkey for this entry.
    case current_command(Mi) of
	none -> keep;
	Cmd -> get_hotkey(Cmd, Mi)
    end;
handle_key(_, _) -> keep.

current_command(#mi{sel=none}) -> none;
current_command(#mi{sel=Sel,menu=Menu,ns=Names}) ->
    case element(Sel, Menu) of
	{_,Name,_,_,Ps} when is_atom(Name) ->
	    {build_command(Name, Names),have_option_box(Ps)};
	{_,Fun,_,_,Ps} when is_function(Fun) ->
	    {Fun(1, Names),have_option_box(Ps)};
	_Other -> none
    end.

virtual_button(false, _) -> 1;
virtual_button(true, 1) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?ALT_BITS =/= 0 -> 2;
	Mod -> 1
    end;
virtual_button(true, 2) -> 2;
virtual_button(true, 3) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?CTRL_BITS =/= 0 -> 2;
	Mod -> 3
    end.
    
set_hotkey(Val, #mi{sel=Sel,menu=Menu0}=Mi) ->
    case element(Sel, Menu0) of
	{A,B,_,D,E} ->
	    Menu = setelement(Sel, Menu0, {A,B,Val,D,E}),
	    Mi#mi{menu=Menu};
	_Other -> Mi
    end.

popup_submenu(Button, X0, Y0, SubName, SubMenu0, Mi) ->
    %% Only in advanced menu mode.
    case expand_submenu(Button, SubName, SubMenu0, Mi) of
	ignore -> keep;
	Action when is_tuple(Action); is_atom(Action) ->
	    wings_io:clear_menu_sel(),
	    wings_io:putback_event({action,Action}),
	    delete_all(Mi);
	SubMenu when is_list(SubMenu) ->
	    {X,Y} = wings_wm:local2global(X0, Y0),
	    Cb = fun() -> menu_setup(popup, X, Y, SubName, SubMenu, Mi) end,
	    wings_io:putback_event({callback,Cb}),
	    delete_all(Mi)
    end.

submenu(I, Name, Menu0, #mi{w=W,hs=Hs,level=Level}=Mi0) ->
    Menu = expand_submenu(1, Name, Menu0, Mi0),
    X0 = W-?CHAR_WIDTH,
    Y0 = get_item_pos(I, Hs, -?LINE_HEIGHT),
    Mi = Mi0#mi{level=Level+1},
    {X,Y} = wings_wm:local2global(X0, Y0),
    menu_setup(plain, X, Y, Name, Menu, Mi).

get_item_pos(0, _Hs, Pos) -> Pos;
get_item_pos(I, [H|Hs], Pos) -> get_item_pos(I-1, Hs, Pos+H).

expand_submenu(B, Name, Submenu0, #mi{ns=Ns}) when is_function(Submenu0) ->
    Submenu0(B, [Name|Ns]);
expand_submenu(_Button, _Name, Submenu, _Mi) -> Submenu.

button_outside(#mousebutton{x=X0,y=Y0}=Ev, Mi) ->
    wings_io:clear_menu_sel(),
    {X,Y} = wings_wm:local2global(X0, Y0),
    case wings_io:button(X, Y) of
	none -> ok;
	ignore -> ok;
	ButtonHit -> wings_io:putback_event({action,ButtonHit})
    end,
    wings_io:event(Ev#mousebutton{x=X,y=Y}),
    delete_all(Mi).

motion_outside(#mousemotion{x=X0,y=Y0}=Event, #mi{type=plain}=Mi) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    case wings_io:button(X, Y) of
	none -> motion_outside_1(X, Y, Mi);
	ignore -> motion_outside_1(X, Y, Mi);
	{_,Name,_,_}=ButtonHit ->
	    case same_menu(Name, Mi) of
		true -> motion_outside_1(X, Y, Mi);
		false ->
		    wings_io:putback_event({action,ButtonHit}),
		    wings_io:putback_event(Event),
		    delete
	    end
    end;
motion_outside(#mousemotion{x=X0,y=Y0}, Mi) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    motion_outside_1(X, Y, Mi).

same_menu(Name, #mi{ns=Names}) -> lists:last(Names) =:= Name.

motion_outside_1(X, Y, #mi{level=Lev}) ->
    case wings_wm:window_under(X, Y) of
	{menu,L} when L < Lev ->
	    wings_wm:set_active({menu,L}),
	    delete;
	{menu,L} when L > Lev ->
	    wings_wm:set_active({menu,L}),
	    none;
	Other -> none
    end.

clear_timer(#mi{timer=short}) -> ok;
clear_timer(#mi{timer=Timer}) -> wings_io:cancel_timer(Timer).

set_submenu_timer(#mi{sel=Sel}=Mi, #mi{sel=Sel}, _X, _Y) -> Mi;
set_submenu_timer(#mi{sel=Sel,timer=OldTimer}=Mi, OldMi, X0, Y0) ->
    clear_timer(OldMi),
    case is_submenu(Sel, Mi) of
	false ->
	    Timer = wings_io:set_timer(?SUB_MENU_TIME, clear_submenu),
	    Mi#mi{timer=Timer};
	true ->
	    {X,Y} = wings_wm:local2global(X0, Y0),
	    Event = #mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
	    Time = case OldTimer of
		       short -> 1;
		       _ -> ?SUB_MENU_TIME
		   end,
	    Timer = wings_io:set_timer(Time, Event),
	    Mi#mi{timer=Timer}
    end.

delete_all(#mi{level=L}=Mi) ->
    clear_timer(Mi),
    wings_wm:delete({menu,L+1}),
    delete_all_1(L-1).

delete_all_1(L) when L < ?INITIAL_LEVEL -> delete;
delete_all_1(L) ->
    wings_wm:delete({menu,L}),
    delete_all_1(L-1).

redraw(Mi) ->
    wings_io:ortho_setup(),
    menu_show(Mi),
    Mi.

update_highlight(X, Y, #mi{sel=OldSel}=Mi0) ->
    case selected_item(X, Y, Mi0) of
	OldSel -> Mi0;
	NoSel when NoSel == outside; NoSel == none ->
	    wings_wm:dirty(),
	    Mi = Mi0#mi{sel=none},
	    help_text(Mi),
	    Mi;
	Item when is_integer(Item), OldSel == none ->
	    wings_wm:dirty(),
	    Mi = Mi0#mi{sel=Item},
	    help_text(Mi),
	    Mi;
	Item when is_integer(Item) ->
	    wings_wm:dirty(),
	    Mi = Mi0#mi{sel=Item,ignore_rel=false},
	    help_text(Mi),
	    Mi
    end.

check_option_box(Act, X, Ps, Mi) ->
    case have_option_box(Ps) of
	false -> Act;
	true -> {Act,hit_right(X, Mi)}
    end.

hit_right(X, #mi{w=W}) ->
    X >= W-3*?CHAR_WIDTH.
    
selected_item(X0, Y0, #mi{ymarg=Margin,w=W,h=H}=Mi) ->
    if
	0 =< X0, X0 < W,
	Margin =< Y0, Y0 < H+Margin ->
	    selected_item_1(Y0-Margin, Mi);
	0 =< X0, X0 < W, -2 =< Y0, Y0 < H+2*Margin -> none;
	true -> outside
    end.

selected_item_1(Y, #mi{hs=Hs,menu=Menu}) ->
    selected_item_1(Y, 1, Hs, Menu).
selected_item_1(Y0, I, [H|Hs], Menu) ->
    case Y0 - H of
	Y when Y =< 0 ->
	    case element(I, Menu) of
		separator -> none;
		{_Text,ignore,_,_,_} -> none;
		_Other -> I
	    end;
	Y -> selected_item_1(Y, I+1, Hs, Menu)
    end.
	    
is_submenu(_I, #mi{adv=true}) -> false;
is_submenu(I, #mi{menu=Menu}) when is_integer(I) ->
    case element(I, Menu) of
	separator -> false;
	{_Text,{_,_},_Hotkey,_Help,_Ps} -> true;
	_Other -> false
    end;
is_submenu(_, _) -> false.

build_command(Name, Names) ->
    foldl(fun(N, A) -> {N,A} end, Name, Names).

menu_draw(_X, _Y, _Shortcut, _Mw, _I, [], _Mi) -> ok;
menu_draw(X, Y, Shortcut, Mw, I, [H|Hs], #mi{menu=Menu,adv=Adv}=Mi) ->
    ?CHECK_ERROR(),
    case element(I, Menu) of
	separator -> draw_separator(X, Y, Mw);
	{Text,ignore,_,_,Ps} ->
	    item_colors(Y, Ps, I, Mi),
	    wings_io:menu_text(X, Y, Text);
	{Text,{_,_}=Item,Hotkey,_Help,Ps} ->
	    item_colors(Y, Ps, I, Mi),
	    wings_io:menu_text(X, Y, Text),
	    draw_hotkey(X, Y, Shortcut, Hotkey),
	    draw_submenu(Adv, Item, X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3);
	{Text,Item,Hotkey,_Help,Ps} ->
	    item_colors(Y, Ps, I, Mi),
	    draw_menu_text(X, Y, Text, Ps),
	    draw_hotkey(X, Y, Shortcut, Hotkey),
	    draw_right(X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3, Ps)
    end,
    ?CHECK_ERROR(),
    menu_draw(X, Y+H, Shortcut, Mw, I+1, Hs, Mi).

draw_hotkey(_, _, _, []) -> ok;
draw_hotkey(X, Y, Pos, Hotkey) ->
    wings_io:text_at(X+Pos*?CHAR_WIDTH, Y, Hotkey).
    
draw_menu_text(X, Y, Text, Props) ->
    case property_lists:is_defined(crossmark, Props) of
	false ->
	    wings_io:menu_text(X, Y, Text);
	true ->
	    wings_io:menu_text(X-2*?CHAR_WIDTH, Y, [crossmark,$\s|Text])
    end.

item_colors(Y, Ps, Sel, #mi{sel=Sel,w=W}) ->
    %% Draw blue background for highlighted item.
    gl:color3f(0, 0, 0.5),
    HaveOptionBox = have_option_box(Ps) orelse have_magnet(Ps),
    Right = case HaveOptionBox of
		true -> W-3*?CHAR_WIDTH;
		false -> W-?CHAR_WIDTH
	    end,
    case wings_wm:local_mouse_state() of
	{_,Mx,_} when HaveOptionBox, Mx > Right ->
	    gl:recti(Right, Y-?CHAR_HEIGHT, Right+3*?CHAR_WIDTH-2, Y+3),
	    gl:color3f(0, 0, 0);		%Black text
	_ ->
	    gl:recti(?CHAR_WIDTH, Y-?CHAR_HEIGHT, Right, Y+3),
	    gl:color3f(1, 1, 1)			%White text
    end;
item_colors(_, _, _, _) -> gl:color3f(0, 0, 0). %Black text

help_text(#mi{sel=none}) ->
    %% We don't want info display as wings_io:clear_message() would give.
    wings_io:message("");
help_text(#mi{menu=Menu,sel=Sel}=Mi) ->
    Elem = element(Sel, Menu),
    case is_magnet_active(Elem, Mi) of
	true -> wings_io:message(wings_magnet:menu_help());
	false -> plain_help(Elem, Mi)
    end.

is_magnet_active({_,_,_,_,Ps}, Mi) ->
    case have_magnet(Ps) of
	false -> false;
	true ->
	    {_,X,_} = wings_wm:local_mouse_state(),
	    hit_right(X, Mi)
    end.
    
plain_help({Text,{_,_},_,_,_}, #mi{adv=false}) ->
    %% No specific help text for submenus in basic mode.
    Help = [Text|" submenu"],
    wings_io:message(Help);
plain_help({_,{Name,Fun},_,_,_}, #mi{ns=Ns,adv=Adv}) when is_function(Fun) ->
    %% "Submenu" in advanced mode.
    Help0 = Fun(help, [Name|Ns]),
    Help = help_text_1(Help0, Adv),
    wings_io:message(Help);
plain_help({_,_,_,Help0,_}, #mi{adv=Adv}) ->
    %% Plain entry - not submenu.
    Help = help_text_1(Help0, Adv),
    wings_io:message(Help);
plain_help(separator, _) -> ok.

help_text_1([_|_]=S, false) -> S;
help_text_1({[_|_]=S,_}, false) -> S;
help_text_1({[_|_]=S,_,_}, false) -> S;
help_text_1([_|_]=S, true) ->
    ["[L] "|S];
help_text_1({S1,S2}, true) ->
    ["[L] ",S1,"  [M] "|S2];
help_text_1({S1,[],S2}, true) ->
    ["[L] ",S1,"  [R] "|S2];
help_text_1({S1,S2,S3}, true) ->
    ["[L] ",S1,"  [M] ",S2,"  [R] "|S3];
help_text_1([]=S, _) -> S.

draw_right(X, Y, Ps) ->
    case have_option_box(Ps) of
	true ->
	    wings_io:sunken_rect(X, Y-3,
				 ?CHAR_WIDTH, ?CHAR_WIDTH,
				 ?MENU_COLOR);
	false ->
	    case have_magnet(Ps) of
		true ->
		    gl:color3f(1, 0, 0),
		    wings_io:text_at(X, Y, [magnet_red]),
		    gl:color3f(0, 0, 0),
		    wings_io:text_at(X, Y, [magnet_black]);
		false -> ok
	    end
    end.

draw_submenu(_Adv, Item, _X, _Y) when is_atom(Item);
				      is_integer(Item);
				      is_list(Item) ->
    ok;
draw_submenu(true, _Item, X, Y) ->
    wings_io:menu_text(X, Y, [submenu]);
draw_submenu(false, _Item, X, Y) ->
    ?CHECK_ERROR(),
    gl:'begin'(?GL_TRIANGLES),
    gl:vertex2i(X-?CHAR_WIDTH div 2, Y),
    gl:vertex2i(X-?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3),
    gl:vertex2i(X-?CHAR_WIDTH, Y+?CHAR_HEIGHT div 3),
    gl:'end'(),
    ?CHECK_ERROR().

draw_separator(X, Y, Mw) ->
    ?CHECK_ERROR(),
    LeftX = X-2*?CHAR_WIDTH,
    RightX = X+Mw-4*?CHAR_WIDTH,
    UpperY = Y-?SEPARATOR_HEIGHT,
    gl:lineWidth(1.0),
    gl:'begin'(?GL_LINES),
    gl:color3f(0.10, 0.10, 0.10),
    gl:vertex2f(LeftX+0.5, UpperY+0.5),
    gl:vertex2f(RightX+0.5, UpperY+0.5),
    gl:'end'(),
    gl:color3f(0, 0, 0),
    ?CHECK_ERROR().

move_if_outside(X, Y, Mw, Mh, Mi) ->
    {W,H0} = wings_wm:top_size(),
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

left_of_parent(Mw, W, #mi{level=?INITIAL_LEVEL}) -> W-Mw;
left_of_parent(Mw, _, _) -> -Mw+10.

have_option_box(Ps) ->
    property_lists:is_defined(option, Ps).

have_magnet(Ps) ->
    property_lists:is_defined(magnet, Ps).

%%%
%%% Get a key to bind a command to.
%%%

get_hotkey(Cmd, Mi) ->
    wings_wm:dirty(),
    wings_io:message("Press key to bind command to."),
    {seq,{push,dummy},
     {replace,fun(Ev) ->
		      handle_key_event(Ev, Cmd, Mi)
	      end}}.

handle_key_event(redraw, _Cmd, Mi) ->
    redraw(Mi),
    keep;
handle_key_event(Ev, {Cmd0,OptionBox}, Mi0) ->
    Cmd = add_option(OptionBox, Cmd0, false),
    case wings_hotkey:bind_from_event(Ev, Cmd) of
	error -> keep;
	Keyname when is_list(Keyname) ->
	    Mi = set_hotkey(Keyname, Mi0),
	    wings_io:putback_event(Mi),
	    pop
    end.

add_option(false, Cmd, _) -> Cmd;
add_option(true, Cmd, Val) ->
    add_option_1(Cmd, Val).

add_option_1({Desc,Tuple}, Val) when is_tuple(Tuple) ->
    {Desc,add_option_1(Tuple, Val)};
add_option_1({Desc,Leave}, Val) ->
    {Desc,{Leave,Val}}.
