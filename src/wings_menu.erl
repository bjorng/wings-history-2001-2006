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
%%     $Id: wings_menu.erl,v 1.20 2002/01/25 09:04:36 bjorng Exp $
%%

-module(wings_menu).
-export([is_popup_event/1,menu/5,popup_menu/5]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,keysearch/3,duplicate/2]).

-define(SUB_MENU_TIME, 200).

%% Menu information kept for a popup menu.
-record(mi,
	{xleft,					%X left
	 ytop,					%Y top
	 ymarg,					%Margin at top and bottom
	 shortcut,				%Position for shortcut (chars)
	 w,					%Width of menu (pixels)
	 h,					%Height of menu (pixels)
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

menu(X, Y, Name, Menu0, Redraw) ->
    Menu = wings_plugin:menu(Name, Menu0),
    Mi = menu_setup(plain, X, Y, Name, Menu, store_redraw(#mi{}, Redraw)),
    top_level(Mi).

popup_menu(X, Y, Name, Menu0, Redraw) ->
    Menu = wings_plugin:menu(Name, Menu0),
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

menu_setup(Type, X0, Y0, Name, Menu0, #mi{ns=Names0}=Mi) ->
    Names = [Name|Names0],
    Hotkeys = hotkeys(Names),
    Menu = normalize_menu(Menu0, Hotkeys),
    {MwL,MwR} = menu_width(Menu),
    TotalW = (MwL+MwR) * ?CHAR_WIDTH + 9*?CHAR_WIDTH,
    Mh = size(Menu) * ?LINE_HEIGHT,
    Margin = 3,
    Adv = wings_pref:get_value(advanced_menus),
    {X1,Y1} = case Type of
		  plain ->
		      {X0,Y0};
		  popup when Adv == false ->
		      {X0-TotalW div 2,Y0 - Margin div 2};
		  popup ->
		      {X0-TotalW div 2,(Y0 - Margin div 2)-(Mh div 4)}
	      end,
    {X,Y} = move_if_outside(X1, Y1, TotalW, Mh+2*Margin, Mi),
    Mi#mi{xleft=X,ytop=Y,ymarg=Margin,
	  shortcut=MwL+2,w=TotalW-10,h=Mh,
	  sel=none,ns=Names,menu=Menu,adv=Adv}.

menu_show(#mi{xleft=X,ytop=Y,ymarg=Margin,shortcut=Shortcut,w=Mw,h=Mh}=Mi) ->
    wings_io:raised_rect(X, Y, Mw, Mh + 2*Margin+3, ?MENU_COLOR),
    gl:color3f(0.0, 0.0, 0.0),
    menu_draw(X+3*?CHAR_WIDTH, Y+Margin+?CHAR_HEIGHT,
	      Shortcut, Mw, 1, Mi).

normalize_menu(Menu, Hotkeys) ->
    normalize_menu(Menu, Hotkeys, 1, []).

normalize_menu(Menu, Hotkeys, Last, Acc) when Last > size(Menu) ->
    list_to_tuple(reverse(Acc));
normalize_menu(Menu, Hotkeys, I, Acc) ->
    Elem0 = case element(I, Menu) of
		{S,{Name},[C|_]=Help,Ps} when is_integer(C) ->
		    {S,Name,[],Help,[hotbox|Ps]};
		{S,Name,[C|_]=Help,Ps} when is_integer(C) ->
		    {S,Name,[],Help,Ps};
		{S,{Name},[C|_]=Help} when is_integer(C) ->
		    {S,Name,[],Help,[hotbox]};
		{S,Name,[C|_]=Help} when is_integer(C) ->
		    {S,Name,[],Help,[]};
		{S,{Name},Ps} ->
		    {S,Name,[],[],[hotbox|Ps]};
		{S,Name,Ps} ->
		    {S,Name,[],[],Ps};
		{S,{Name}} ->
		    {S,Name,[],[],[hotbox]};
		{S,Sub}=El0 when is_tuple(Sub) ->
		    Name = none,
		    El0;
		{S,Name} ->
		    {S,Name,[],[],[]};
		separator ->
		    Name = none,
		    separator
	    end,
    Elem = case keysearch(Name, 1, Hotkeys) of
	       false -> Elem0;
	       {value,{_,Hotkey}} -> setelement(3, Elem0, Hotkey)
	   end,
    normalize_menu(Menu, Hotkeys, I+1, [Elem|Acc]).

menu_width(Menu) ->
    menu_width(Menu, 1, 0, 0).
menu_width(Menu, Last, MaxA, MaxB) when Last > size(Menu) -> {MaxA,MaxB};
menu_width(Menu, I, MaxA0, MaxB0) ->
    {Wa,Wb} = case element(I, Menu) of
		  {S,_,Hotkey,_,Ps} ->
		      case property_lists:is_defined(hotbox, Ps) of
			  false -> {length(S),length(Hotkey)};
			  true -> {length(S)+1,length(Hotkey)}
		      end;
		  {S,Sub} when is_tuple(Sub) ->
		      {length(S)+1,0};
		  separator -> {0,0}
	      end,
    menu_width(Menu, I+1, max(Wa, MaxA0), max(Wb, MaxB0)).

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
	#keyboard{keysym=#keysym{sym=27}} ->	%escape
	    wings_io:cleanup_after_drawing(),
	    wings_io:putback_event(redraw),
	    next;
	#mousemotion{x=X,y=Y} ->
	    Mi1 = update_highlight(X, Y, Mi0),
	    case motion_outside(Event, Mi0) of
		none ->
		    Mi = set_submenu_timer(Mi1, Mi0, X, Y),
		    get_menu_event(Mi#mi{new=false});
		Other -> Other
	    end;
	#mousebutton{button=B,x=X,y=Y,state=?SDL_RELEASED}=Button
	when not New and (B =< 3) ->
	    clear_timer(Mi0),
	    Mi1 = update_highlight(X, Y, Mi0),
	    case select_item(B, X, Y, Mi1) of
		{popup,{Name,SubMenu}} ->
		    popup_submenu(B, X, Y, Name, SubMenu, Mi1);
		{submenu,{Name,SubMenu,Pos}} ->
		    submenu(Name, SubMenu, Pos, Mi1);
		{seq,_,_}=Seq -> Seq;
		#mi{}=Mi -> get_menu_event(Mi);
		outside -> button_outside(Event, Mi1);
		{action,_}=Action when is_tuple(Action) ->
		    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
		    wings_io:cleanup_after_drawing(),
		    wings_io:clear_menu_sel(),
		    wings_io:putback_event(Action),
		    wings_io:putback_event(redraw),
		    pop
	    end;
	{go_back,NewEvent} ->
	    Mi1 = Mi0#mi.prev,
	    Mi = Mi1#mi{num_redraws=0,new=false,sel=none},
	    handle_menu_event(NewEvent, Mi);
	redraw ->
	    get_menu_event(Mi0#mi{new=false,num_redraws=0});
	{expose} ->
	    get_menu_event(Mi0#mi{new=false,num_redraws=0});
	{resize,W,H}=Resize ->
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    wings_io:cleanup_after_drawing(),
	    wings_io:clear_menu_sel(),
	    wings_io:putback_event(Resize),
	    pop;
	IgnoreMe ->
	    get_menu_event(Mi0#mi{new=false})
    end.

popup_submenu(Button, X, Y, SubName, SubMenu0, #mi{ns=Ns}=Mi0) ->
    %% Only in advanced menu mode.
    case expand_submenu(Button, SubName, SubMenu0, Mi0) of
	{action,_}=Action ->
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    wings_io:cleanup_after_drawing(),
	    wings_io:clear_menu_sel(),
	    wings_io:putback_event(Action),
	    wings_io:putback_event(redraw),
	    pop;
	SubMenu ->
	    Mi = menu_setup(popup, X, Y, SubName, SubMenu, Mi0),
	    handle_menu_event(redraw, Mi)
    end.

submenu(Name, Menu0, SelPos, Mi0) ->
    Menu = expand_submenu(submenu, Name, Menu0, Mi0),
    #mi{xleft=Xleft,ytop=Ytop,ymarg=Margin,w=W,h=H}=Mi0,
    X = Xleft+W,
    Y = Ytop+(SelPos-1)*?LINE_HEIGHT+Margin,
    Redraw = new_redraw_fun(Mi0),
    Mi1 = Mi0#mi{prev=Mi0,redraw=Redraw},
    Mi = menu_setup(plain, X, Y, Name, Menu, Mi1),
    get_menu_event(Mi).

expand_submenu(Button, Name, Submenu0, #mi{adv=Adv,ns=Ns})
  when is_function(Submenu0) ->
    Submenu = case Adv of
		  false -> Submenu0(submenu, [Name|Ns]);
		  true -> Submenu0(Button, [Name|Ns])
	      end,
    case Submenu of
	[_|_] ->  list_to_tuple(Submenu);
	Other -> Other
    end;
expand_submenu(Button, Name, Submenu, Mi) -> Submenu.

button_outside(#mousebutton{x=X,y=Y}=Event, #mi{prev=[]}=Mi) ->
    wings_io:cleanup_after_drawing(),
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

clear_timer(#mi{timer=Timer}) ->
    wings_io:cancel_timer(Timer).

set_submenu_timer(#mi{return_timer=none}=Mi, OldMi, X, Y) ->
    set_submenu_timer_1(Mi, OldMi, X, Y);
set_submenu_timer(#mi{return_timer={_,Timer}}=Mi, OldMi, X, Y) ->
    wings_io:cancel_timer(Timer),
    set_submenu_timer_1(Mi, OldMi, X, Y).

set_submenu_timer_1(#mi{sel=Sel}=Mi, #mi{sel=Sel}, X, Y) -> Mi;
set_submenu_timer_1(#mi{sel=Sel}=Mi, OldMi, X, Y) ->
    clear_timer(OldMi),
    case is_submenu(Sel, Mi) of
	false -> Mi;
	true ->
	    Event = #mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
	    Timer = wings_io:set_timer(?SUB_MENU_TIME, Event),
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
    menu_show(Mi),
    gl:swapBuffers(),
    Mi#mi{num_redraws=NumRedraws+1}.

update_highlight(X, Y, Mi) ->
    case selected_item(X, Y, Mi) of
	NoSel when NoSel == outside; NoSel == none ->
	    Mi#mi{sel=none};
	Item when integer(Item) ->
	    Mi#mi{sel=Item}
    end.

select_item(Button, X0, Y0,
	    #mi{ns=Names,menu=Menu,adv=Adv}=Mi) ->
    case selected_item(X0, Y0, Mi) of
	outside -> outside;
	none -> Mi;
	Item when integer(Item) ->
	    {Action,Ps} =
		case element(Item, Menu) of
		    {_,A0,_,_,Ps0} -> {A0,Ps0};
		    {_,A0} -> {A0,[]}
		end,
	    case Action of
		{_,_}=SubMenu when Adv == true ->
		    {popup,SubMenu};
		{Name,SubMenu} when Adv == false ->
		    {submenu,{Name,SubMenu,Item}};
		[Act] when is_function(Act) ->
		    {action,Act(Button, Names)};
		Act0 when is_atom(Act0); is_integer(Act0);
			  is_tuple(Act0); is_list(Act0) ->
		    Act = check_hotbox(Act0, X0, Ps, Mi),
		    {action,foldl(fun(N, A) -> {N,A} end, Act, Names)}
	    end
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
	    Item = ((Y0-Ytop-Margin) div ?LINE_HEIGHT) + 1,
	    case selectable(Item, Mi) of
		true -> Item;
		false -> none
	    end;
	Xleft =< X0, X0 < Xleft+W,
	Ytop-2 =< Y0,  Y0 < Ytop+H+2*Margin -> none;
	true -> outside
    end.

selectable(Item, #mi{menu=Menu}) ->
    case element(Item, Menu) of
	separator -> false;
	{Text,ignore,_,_,_} -> false;
	Other -> true
    end.
	    
have_hotbox(Item, Menu) ->
    case element(Item, Menu) of
	{_,_,_,_,Ps} when is_list(Ps) ->
	    property_lists:is_defined(hotbox, Ps);
	Other -> false
    end.

is_submenu(I, #mi{menu=Menu,adv=true}) -> false;
is_submenu(I, #mi{menu=Menu}) when is_integer(I) ->
    case element(I, Menu) of
	separator -> false;
	{Text,Item} ->
	    not(is_atom(Item) or is_integer(Item) or is_list(Item));
	{Text,Item,Hotkey,Help,Ps} -> false;
	Fun when is_function(Fun) -> true
    end;
is_submenu(I, Mi) -> false.
	    
menu_draw(X, Y, Shortcut, Mw, I, #mi{menu=Menu}=Mi) when I > size(Menu) ->
    help_text(Mi);
menu_draw(X, Y, Shortcut, Mw, I, #mi{menu=Menu}=Mi) ->
    ?CHECK_ERROR(),
    case element(I, Menu) of
	separator -> draw_separator(X, Y, Mw);
	{Text,Item,Hotkey,Help,Ps} ->
	    Str = Text ++ duplicate(Shortcut-length(Text), $\s) ++ Hotkey,
	    item_colors(I, Mi),
	    draw_menu_text(X, Y, Str, Ps),
	    draw_hotbox(Item, X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3, Ps);
	{Text,Item} when is_list(Text) ->
	    item_colors(I, Mi),
	    wings_io:menu_text(X, Y, Text),
	    draw_submenu(Item, X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3)
    end,
    ?CHECK_ERROR(),
    menu_draw(X, Y+?LINE_HEIGHT, Shortcut, Mw, I+1, Mi).

draw_menu_text(X, Y, Text, Props) ->
    case property_lists:is_defined(crossmark, Props) of
	false ->
	    wings_io:menu_text(X, Y, Text);
	true ->
	    wings_io:menu_text(X-2*?CHAR_WIDTH, Y, [crossmark,$\s|Text])
    end.

help_text(#mi{sel=none}) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    wings_io:sunken_rect(6, H-2*?LINE_HEIGHT+5, W-10,
			 2*?LINE_HEIGHT-8, ?PANE_COLOR);
help_text(#mi{menu=Menu,sel=Sel,ns=Names,adv=Adv}=Mi) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    wings_io:sunken_rect(6, H-2*?LINE_HEIGHT+5, W-10,
			 2*?LINE_HEIGHT-8, ?PANE_COLOR),
    case element(Sel, Menu) of
        {_,{_,Fun}} when is_function(Fun) ->
	    HelpAtom = case Adv of
			   true -> adv_help;
			   false -> help
		       end,
	    Help = Fun(HelpAtom, Names),
	    wings_io:menu_text(8, H-?LINE_HEIGHT+5, Help);
	{_,_,_,Help,_} ->
	    wings_io:menu_text(8, H-?LINE_HEIGHT+5, Help);
	Other -> ok
    end.
    
item_colors(Sel, #mi{sel=Sel}=Mi) ->
    draw_blue_rect(Sel, Mi),
    gl:color3f(1.0, 1.0, 1.0);
item_colors(I, Mi) -> gl:color3f(0.0, 0.0, 0.0).

draw_blue_rect(Item, #mi{xleft=Xleft,ytop=Ytop,
			 ymarg=Margin,w=W,menu=Menu}) ->
    gl:color3f(0.0, 0.0, 0.5),
    Right = case have_hotbox(Item, Menu) of
		true -> Xleft+W-3*?CHAR_WIDTH;
		false -> Xleft+W-?CHAR_WIDTH
	    end,
    gl:recti(Xleft+?CHAR_WIDTH, Ytop+Margin+(Item-1)*?LINE_HEIGHT,
	     Right, Ytop+Margin+Item*?LINE_HEIGHT).

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
    UpperY = Y-?CHAR_HEIGHT div 2 + 2,
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
    gl:color3f(0.0, 0.0, 0.0),
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
