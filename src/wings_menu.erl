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
%%     $Id: wings_menu.erl,v 1.72 2003/01/05 09:44:18 bjorng Exp $
%%

-module(wings_menu).
-export([is_popup_event/1,is_popup_event/3,menu/5,popup_menu/4,build_command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,keysearch/3,foreach/2]).

-define(SUB_MENU_TIME, 150).
-define(SEPARATOR_HEIGHT, (wings_text:height()-4)).
-define(INITIAL_LEVEL, 101).

%% Menu information kept for a popup menu.
-record(mi,
	{ymarg,					%Margin at top and bottom
	 shortcut,				%Position for shortcut (chars)
	 w,					%Width of menu (pixels)
	 h,					%Height of menu (pixels)
	 hs,					%Tuple: height of each entry.
	 sel=none,				%Selected item (1..size(Menu))
	 sel_side=left,				%Selection on left or right.
	 ns=[],					%Name stack.
	 menu,					%Original menu term
	 timer=make_ref(),			%Active submenu timer.
	 level=?INITIAL_LEVEL,			%Menu level.
	 st,					%State record.
	 adv,					%Advanced menus (true|false).
	 ignore_rel=true,			%Ignore button release if
						% just openened.
	 type=plain,				%Type of menu: plain|popup
	 owner					%Owning window.
	}).

%%%
%%% Inside this module, each entry in a menu is kept in the following
%%% normalized format:
%%%
%%%   separator      OR
%%%   {Text,Name,Hotkey,Help,Properties}
%%%

is_popup_event(#mousebutton{button=3,x=X0,y=Y0,state=State}) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    case wings_pref:get_value(advanced_menus) of
	true when State =:= ?SDL_RELEASED ->
	    {yes,X,Y,wings_wm:me_modifiers()};
	false when State =:= ?SDL_PRESSED  ->
	    {yes,X,Y,wings_wm:me_modifiers()};
	_Other -> no
    end;
is_popup_event(_Event) -> no.

is_popup_event(Ev0, PrefKey, St0) ->
    case is_popup_event(Ev0) of
	no -> no;
	{yes,Xglobal,Yglobal,_}=Res ->
	    {Xlocal,Ylocal} = wings_wm:global2local(Xglobal, Yglobal),
	    case wings_pref:get_value(PrefKey) of
		false -> Res;
		true ->
		    case wings_pick:do_pick(Xlocal, Ylocal, St0) of
			{add,_,St} ->
			    Ev = wings_wm:local2global(Ev0),
			    wings_io:putback_event(Ev),
			    wings_io:putback_event({new_state,St}),
			    keep;
			_Other -> Res
		    end
	    end
    end.

menu(X, Y, Owner, Name, Menu) ->
    menu_setup(plain, X, Y, Name, Menu,
	       #mi{adv=false,owner=Owner,ignore_rel=false}).

popup_menu(X, Y, Name, Menu) ->
    Active = wings_wm:active_window(),
    Adv = wings_pref:get_value(advanced_menus),
    IgnoreRel = (Adv == false),
    menu_setup(popup, X, Y, Name, Menu,
	       #mi{adv=Adv,ignore_rel=IgnoreRel,owner=Active}).

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
    setup_menu_killer(),
    Op = {seq,push,get_menu_event(Mi)},
    WinName = {menu,Level},
    wings_wm:delete({menu,Level}),
    wings_wm:new(WinName, {X,Y,Level}, {W,Mh+10}, Op),
    delete_from(Level+1),
    keep.

delete_from(Level) ->
    Name = {menu,Level},
    case wings_wm:is_window(Name) of
	false -> ok;
	true ->
	    wings_wm:delete(Name),
	    delete_from(Level+1)
    end.

setup_menu_killer() ->
    case wings_wm:exists(menu_killer) of
	true -> ok;
	false ->
	    Op = {push,fun menu_killer/1},
	    wings_wm:new(menu_killer, {0,0,?INITIAL_LEVEL-1},
			 wings_wm:top_size(), Op)
    end.

menu_killer(#mousebutton{button=1,state=?SDL_PRESSED}) ->
    wings_wm:send(menubar, clear_menu_selection),
    foreach(fun({menu,_}=Name) ->
		    wings_wm:delete(Name);
	       (_) -> ok
	    end, wings_wm:windows()),
    delete;
menu_killer(_) -> keep.
    
menu_show(#mi{ymarg=Margin,shortcut=Shortcut,w=Mw,h=Mh}=Mi) ->
    wings_io:border(0, 0, Mw-1, Mh + 2*Margin+3, ?MENU_COLOR),
    menu_draw(3*?CHAR_WIDTH, Margin+?CHAR_HEIGHT,
	      Shortcut, Mw, 1, Mi#mi.hs, Mi).

normalize_menu(Menu, Hotkeys, Adv) ->
    normalize_menu(Menu, Hotkeys, Adv, []).

normalize_menu([{basic,_}|Els], Hotkeys, true, Acc) ->
    normalize_menu(Els, Hotkeys, true, Acc);
normalize_menu([{basic,El}|Els], Hotkeys, false, Acc) ->
    normalize_menu([El|Els], Hotkeys, false, Acc);
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
	    {S,{_,_},Hotkey,_,_} ->
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
	    Mi = set_submenu_timer(Mi1, Mi0, X, Y),
	    get_menu_event(Mi);
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
	    wings_wm:send(menubar, clear_menu_selection),
	    wings_io:putback_event(quit),
	    delete_all(Mi0);
	_IgnoreMe -> keep
    end.

button_pressed(#mousebutton{state=?SDL_RELEASED}, #mi{ignore_rel=true}=Mi) ->
    get_menu_event(Mi#mi{ignore_rel=false});
button_pressed(#mousebutton{button=B,x=X,y=Y,state=?SDL_RELEASED},
	       #mi{adv=false}=Mi) when (B =< 3) ->
    wings_wm:dirty(),
    button_pressed(1, X, Y, Mi);
button_pressed(#mousebutton{button=B,x=X,y=Y,state=?SDL_RELEASED}, Mi)
  when (B =< 3) ->
    wings_wm:dirty(),
    button_pressed(B, X, Y, Mi);
button_pressed(_, _) -> keep.

button_pressed(Button, X, Y, #mi{ns=Names,menu=Menu,adv=Adv}=Mi0) ->
    clear_timer(Mi0),
    Mi = update_highlight(X, Y, Mi0),
    case selected_item(X, Y, Mi) of
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

do_action(Action, #mi{owner=Owner}=Mi) ->
    wings_wm:send(menubar, clear_menu_selection),
    wings_wm:send(Owner, {action,Action}),
    delete_all(Mi).
	    
handle_key(#keysym{sym=27}, Mi) ->		%Escape.
    wings_wm:send(menubar, clear_menu_selection),
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
	    Cmd = Fun(1, Names),
	    case is_ascii_clean(Cmd) of
		true -> {Cmd,have_option_box(Ps)};
		false -> none
	    end;
	_Other -> none
    end.

%% Test if a term can be represtend in a text file and read back.
is_ascii_clean([H|T]) ->
    is_ascii_clean(H) andalso is_ascii_clean(T);
is_ascii_clean([]) -> true;
is_ascii_clean(T) when is_tuple(T) ->
    is_tuple_ascii_clean(1, size(T), T);
is_ascii_clean(Num) when is_number(Num) -> true;
is_ascii_clean(Atom) when is_atom(Atom) -> true;
is_ascii_clean(_) -> false.

is_tuple_ascii_clean(I, N, T) when I =< N ->
    is_ascii_clean(element(I, T)) andalso is_tuple_ascii_clean(I+1, N, T);
is_tuple_ascii_clean(_, _, _) -> true.
    
set_hotkey(Val, #mi{sel=Sel,menu=Menu0}=Mi) ->
    case element(Sel, Menu0) of
	{A,B,_,D,E} ->
	    Menu = setelement(Sel, Menu0, {A,B,Val,D,E}),
	    Mi#mi{menu=Menu};
	_Other -> Mi
    end.

popup_submenu(Button, X0, Y0, SubName, SubMenu0,
	      #mi{owner=Owner,level=Level}=Mi) ->
    %% Only in advanced menu mode.
    case expand_submenu(Button, SubName, SubMenu0, Mi) of
	ignore -> keep;
	Action when is_tuple(Action); is_atom(Action) ->
	    wings_wm:send(menubar, clear_menu_selection),
	    wings_wm:send(Owner, {action,Action}),
	    delete_all(Mi);
	SubMenu when is_list(SubMenu) ->
	    clear_timer(Mi),
	    {X,Y} = wings_wm:local2global(X0, Y0),
	    menu_setup(popup, X, Y, SubName, SubMenu, Mi#mi{level=Level+1}),
	    delete
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

clear_timer(#mi{timer=Timer}) -> wings_wm:cancel_timer(Timer).

set_submenu_timer(#mi{sel=Sel}=Mi, #mi{sel=Sel}, _X, _Y) -> Mi;
set_submenu_timer(#mi{sel=Sel}=Mi, OldMi, X0, Y0) ->
    clear_timer(OldMi),
    clear_timer(Mi),
    case is_submenu(Sel, Mi) of
	false ->
	    Timer = wings_wm:set_timer(?SUB_MENU_TIME, clear_submenu),
	    Mi#mi{timer=Timer};
	true ->
	    {X,Y} = wings_wm:local2global(X0, Y0),
	    Event = #mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
	    Timer = wings_wm:set_timer(?SUB_MENU_TIME, Event),
	    Mi#mi{timer=Timer}
    end.

delete_all(Mi) ->
    clear_timer(Mi),
    case wings_wm:exists(menu_killer) of
	true ->
	    wings_wm:send(menu_killer,
			  #mousebutton{button=1,x=0,y=0,state=?SDL_PRESSED});
	false -> ok
    end,
    delete.

redraw(Mi) ->
    wings_io:ortho_setup(),
    menu_show(Mi),
    Mi.

update_highlight(X, Y, #mi{menu=Menu,sel=OldSel,sel_side=OldSide,w=W}=Mi0) ->
    case selected_item(X, Y, Mi0) of
	OldSel when is_integer(OldSel) ->
	    Ps = element(5, element(OldSel, Menu)),
	    RightWidth = right_width(Ps),
	    Right = W - (2*?CHAR_WIDTH*RightWidth) - ?CHAR_WIDTH,
	    Side = if
		       X < Right; RightWidth == 0 -> left;
		       true -> right
		   end,
	    if
		Side =:= OldSide -> Mi0;
		true ->
		    wings_wm:dirty(),
		    help_text(Mi0),
		    Mi0#mi{sel_side=Side}
	    end;
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
	true -> none
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
	{Text,_,Hotkey,_Help,Ps} ->
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
    case proplists:is_defined(crossmark, Props) of
	false ->
	    wings_io:menu_text(X, Y, Text);
	true ->
	    wings_io:menu_text(X-2*?CHAR_WIDTH, Y, [crossmark,$\s|Text])
    end.

item_colors(Y, Ps, Sel, #mi{sel=Sel,sel_side=Side,w=W}) ->
    %% Draw blue background for highlighted item.
    gl:color3f(0, 0, 0.5),
    Right = W - (2*?CHAR_WIDTH*right_width(Ps)) - ?CHAR_WIDTH,
    case Side of
	right ->
	    gl:recti(Right, Y-?CHAR_HEIGHT, Right+3*?CHAR_WIDTH-2, Y+3),
	    gl:color3f(0, 0, 0);		%Black text
	left ->
	    gl:recti(?CHAR_WIDTH, Y-?CHAR_HEIGHT, Right, Y+3),
	    gl:color3f(1, 1, 1)			%White text
    end;
item_colors(_, _, _, _) -> gl:color3f(0, 0, 0). %Black text

help_text(#mi{sel=none}) ->
    wings_wm:message("");
help_text(#mi{menu=Menu,sel=Sel}=Mi) ->
    Elem = element(Sel, Menu),
    case is_magnet_active(Elem, Mi) of
	true -> wings_magnet:menu_help();
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
    wings_wm:message(Help, "");
plain_help({_,{Name,Fun},_,_,_}, #mi{ns=Ns,adv=Adv}) when is_function(Fun) ->
    %% "Submenu" in advanced mode.
    Help0 = Fun(help, [Name|Ns]),
    Help = help_text_1(Help0, Adv),
    wings_wm:message(Help, "");
plain_help({_,_,_,Help0,_}, #mi{adv=Adv}) ->
    %% Plain entry - not submenu.
    Help = help_text_1(Help0, Adv),
    wings_wm:message(Help, "");
plain_help(separator, _) -> ok.

help_text_1([_|_]=S, false) -> S;
help_text_1({[_|_]=S,_}, false) -> S;
help_text_1({[_|_]=S,_,_}, false) -> S;
help_text_1([_|_]=S, true) ->
    ["[L] "|S];
help_text_1({S1,S2}, true) ->
    {_,M,_} = wings_camera:button_names(),
    ["[L] ",S1,"  ",M," "|S2];
help_text_1({S1,[],S2}, true) ->
    {_,_,R} = wings_camera:button_names(),
    ["[L] ",S1,"  ",R," "|S2];
help_text_1({S1,S2,S3}, true) ->
    {_,M,R} = wings_camera:button_names(),
    ["[L] ",S1,"  ",M," ",S2,"  ",R," "|S3];
help_text_1([]=S, _) -> S.

draw_right(X, Y, Ps) ->
    case have_option_box(Ps) of
	true ->
	    wings_io:sunken_rect(X, Y-3,
				 ?CHAR_WIDTH, ?CHAR_WIDTH,
				 ?MENU_COLOR);
	false -> draw_right_1(X, Y, Ps)
    end.

draw_right_1(X, Y, Ps) ->
    case have_magnet(Ps) of
	true ->
	    gl:color3f(1, 0, 0),
	    wings_io:text_at(X, Y, [magnet_red]),
	    gl:color3f(0, 0, 0),
	    wings_io:text_at(X, Y, [magnet_black]);
	false -> draw_right_2(X, Y, Ps)
    end.

draw_right_2(X, Y, Ps) ->
    case proplists:get_value(color, Ps, none) of
	none -> ok;
	Color ->
	    wings_io:border(X, Y-7,
			    ?CHAR_WIDTH, ?CHAR_HEIGHT-1,
			    Color);
	false -> draw_right_2(X, Y, Ps)
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
    {W,H} = wings_wm:top_size(),
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
left_of_parent(Mw, _, #mi{level=Level}) ->
    {X,_,_,_} = wings_wm:viewport({menu,Level-1}),
    X-Mw+10.

have_option_box(Ps) ->
    proplists:is_defined(option, Ps).

have_magnet(Ps) ->
    proplists:is_defined(magnet, Ps).

%%%
%%% Get a key to bind a command to.
%%%

get_hotkey(Cmd, Mi) ->
    wings_wm:dirty(),
    wings_wm:message("Press key to bind command to."),
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
