%%
%%  wings_menu.erl --
%%
%%     Implementation of pulldown and popup menus.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_menu.erl,v 1.11 2001/11/24 18:38:50 bjorng Exp $
%%

-module(wings_menu).
-export([menu/4,popup_menu/4,reactivate/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

%% Menu information kept for a popup menu.
-record(mi,
	{xleft,					%X left
	 ytop,					%Y top
	 ymarg,					%Margin at top and bottom
	 shortcut,				%Position for shortcut (chars)
	 w,					%Width of menu (pixels)
	 h,					%Height of menu (pixels)
	 sel=none,				%Selected item (1..size(Menu))
	 name,					%Name of menu (atom)
	 menu,					%Original menu term
	 new=true,				%If just opened, ignore
						%button release.
	 timer=make_ref(),			%Active submenu timer.
	 prev=[]				%Previous menu.
	}).

menu(X, Y, Name, Menu0) ->
    Menu = wings_plugin:menu(Name, Menu0),
    Mi = menu_setup(plain, X, Y, Name, Menu, #mi{}),
    top_level(Mi).

popup_menu(X, Y, Name, Menu0) ->
    Menu = wings_plugin:menu(Name, Menu0),
    Mi = menu_setup(popup, X, Y, Name, Menu, #mi{}),
    top_level(Mi).

menu(X0, Y0, Name, Menu, Mi0) ->
    Mi = menu_setup(plain, X0, Y0, Name, Menu, Mi0#mi{prev=Mi0}),
    top_level(Mi).

reactivate(Mi) ->
    top_level(Mi).

top_level(#mi{name=Name}=Mi) ->
    wings_io:setup_for_drawing(),
    menu_show(Mi),
    {seq,{push,dummy},get_menu_event(Mi)}.

menu_setup(Type, X0, Y0, Name, Menu, Mi) -> 
    {MwL,MwR} = menu_width(Menu),
    TotalW = (MwL+MwR) * ?CHAR_WIDTH + 9*?CHAR_WIDTH,
    Mh = size(Menu) * ?LINE_HEIGHT,
    Margin = 3,
    {X1,Y1} = case Type of
		  plain -> {X0,Y0};
		  popup -> {X0-TotalW div 2,Y0 - Margin div 2}
	      end,
    {X,Y} = move_if_outside(X1, Y1, TotalW, Mh+2*Margin),
    Mi#mi{xleft=X,ytop=Y,ymarg=Margin,
	  shortcut=MwL+2,w=TotalW-10,h=Mh,
	  sel=none,name=Name,menu=Menu}.

menu_show(#mi{xleft=X,ytop=Y,ymarg=Margin,shortcut=Shortcut,w=Mw,h=Mh,
	      sel=Item,name=Name,menu=Menu}=Mi) ->
    wings_io:beveled_rect(X, Y, Mw, Mh + 2*Margin+3),
    gl:color3f(0.0, 0.0, 0.0),
    menu_draw(X+3*?CHAR_WIDTH, Y+Margin+?CHAR_HEIGHT,
	      Shortcut, Mw, 1, Menu),
    if
	integer(Item) -> toggle_highlight(Item, Mi);
	true -> ok
    end,
    gl:flush().

menu_width(Menu) ->
    menu_width(Menu, 1, 0, 0).
menu_width(Menu, Last, MaxA, MaxB) when Last > size(Menu) -> {MaxA,MaxB};
menu_width(Menu, I, MaxA0, MaxB0) ->
    {Wa,Wb} = case element(I, Menu) of
		  {S,{_}} -> {length(S)+1,0};
		  {S,Sub} when tuple(Sub) -> {length(S)+1,0};
		  {S,_} -> {length(S),0};
		  {S1,S2,_} -> {length(S1),length(S2)};
		  separator -> {0,0}
	      end,
    menu_width(Menu, I+1, max(Wa, MaxA0), max(Wb, MaxB0)).

max(A, B) when A > B -> A;
max(A, B) -> B.
    
get_menu_event(Mi) ->
    {replace,fun(Ev) -> handle_menu_event(Ev, Mi) end}.

handle_menu_event(Event, #mi{name=Name,new=New}=Mi0) ->
    case Event of
	{reactivate_menu,Mi} ->
	    reactivate(Mi);
	#keyboard{keysym=#keysym{sym=27}} ->	%escape
	    wings_io:cleanup_after_drawing(),
	    wings_io:putback_event(redraw),
	    next;
	#mousemotion{x=X,y=Y} ->
	    Mi1 = highlight_item(X, Y, Mi0),
	    case motion_outside(Event, Mi0) of
		none ->
		    Mi = set_submenu_timer(Mi1, Mi0, X, Y),
		    get_menu_event(Mi#mi{new=false});
		Other -> Other
	    end;
	#mousebutton{button=B,x=X,y=Y,state=?SDL_RELEASED}=Button
	when not New and ((B == 1) or (B == 3)) ->
	    clear_timer(Mi0),
	    Mi1 = highlight_item(X, Y, Mi0),
	    case select_item(X, Y, Mi1) of
		{seq,_,_}=Seq -> Seq;
		#mi{}=Mi -> get_menu_event(Mi);
		outside -> button_outside(Event, Mi1);
		Other ->
		    wings_io:cleanup_after_drawing(),
		    wings_io:clear_menu_sel(),
		    wings_io:putback_event({menu_action,{Name,Other}}),
		    pop
	    end;
	{menu_action,Action} ->
	    wings_io:putback_event({menu_action,{Name,Action}}),
	    pop;
	redraw ->
	    wings_io:cleanup_after_drawing(),
	    wings_io:putback_event({reactivate_menu,Mi0}),
	    next;
	IgnoreMe -> get_menu_event(Mi0#mi{new=false})
    end.

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
motion_outside(#mousemotion{x=X,y=Y}=Event, #mi{prev=PrevMenu}=Mi) ->
    #mi{sel=PrevSel} = PrevMenu,
    case selected_item(X, Y, PrevMenu) of
	outside -> motion_outside(Event, PrevMenu);
	none -> none;
	PrevSel -> none;
	Other ->
	    wings_io:cleanup_after_drawing(),
	    wings_io:putback_event(Event),
	    wings_io:putback_event(redraw),
	    pop
    end.

clear_timer(#mi{timer=Timer}) ->
    wings_io:cancel_timer(Timer).

set_submenu_timer(#mi{sel=Sel}=Mi, #mi{sel=Sel}, X, Y) -> Mi;
set_submenu_timer(#mi{sel=Sel}=Mi, OldMi, X, Y) ->
    clear_timer(OldMi),
    case is_submenu(Sel, Mi) of
	false -> Mi;
	true ->
	    Event = #mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
	    Timer = wings_io:set_timer(350, Event),
	    Mi#mi{timer=Timer}
    end.

highlight_item(X0, Y0, Mi) ->
    case selected_item(X0, Y0, Mi) of
	NoSel when NoSel == outside; NoSel == none ->
	    case Mi#mi.sel of
		none -> Mi;
		Item ->
		    toggle_highlight(Item, Mi),
		    Mi#mi{sel=none}
	    end;
	Item when integer(Item) ->
	    case Mi#mi.sel of
		none ->
		    toggle_highlight(Item, Mi),
		    Mi#mi{sel=Item};
		Item -> Mi;
		OtherItem ->
		    toggle_highlight(OtherItem, Mi),
		    toggle_highlight(Item, Mi),
		    Mi#mi{sel=Item}
	    end
    end.

select_item(X0, Y0, #mi{menu=Menu,xleft=Xleft,w=W}=Mi) ->
    case selected_item(X0, Y0, Mi) of
	outside -> outside;
	none -> Mi;
	Item when integer(Item) ->
	    Action = case element(Item, Menu) of
			 {_,A} -> A;
			 {_,_,A} -> A
		     end,
	    case Action of
		{What,SubMenu} ->
		    #mi{xleft=Xleft,ytop=Ytop,ymarg=Margin,w=W,h=H}=Mi,
		    SubX = Xleft+W,
		    SubY = Ytop+(Item-1)*?LINE_HEIGHT+Margin,
		    menu(SubX, SubY, What, SubMenu, Mi);
		{Act} when Xleft =< X0, X0 < Xleft+W-2*?CHAR_WIDTH ->
		    Act;
		Act when atom(Act); integer(Act);
			 tuple(Act); list(Act) -> Act
	    end
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
	{S,ignore} -> false;
	_ -> true
    end.
	    
toggle_highlight(Item, #mi{xleft=Xleft,ytop=Ytop,
			   ymarg=Margin,w=W,menu=Menu}) ->
    gl:color3f(1.0, 1.0, 0.5),
    gl:enable(?GL_COLOR_LOGIC_OP),
    gl:logicOp(?GL_XOR),
    Right = case has_options(Item, Menu) of
		true -> Xleft+W-3*?CHAR_WIDTH;
		false -> Xleft+W-?CHAR_WIDTH
	    end,
    gl:recti(Xleft+?CHAR_WIDTH, Ytop+Margin+(Item-1)*?LINE_HEIGHT,
	     Right, Ytop+Margin+Item*?LINE_HEIGHT),
    gl:flush(),
    gl:disable(?GL_COLOR_LOGIC_OP).

has_options(Item, Menu) ->
    case element(Item, Menu) of
	{_,{_}} -> true;
	{_,_,{_}}-> true;
	_ -> false
    end.

is_submenu(I, #mi{menu=Menu}) when is_integer(I) ->
    case element(I, Menu) of
	separator -> false;
	{S,Item} -> is_submenu_1(Item);
	{S1,S2,Item} -> is_submenu_1(Item)
    end;
is_submenu(I, Mi) -> false.

is_submenu_1({Item}) -> false;
is_submenu_1(Item) ->
    not(is_atom(Item) or is_integer(Item) or is_list(Item));
is_submenu_1(Other) -> true.
	    
menu_draw(X, Y, Shortcut, Mw, I, Menu) when I > size(Menu) -> ok;
menu_draw(X, Y, Shortcut, Mw, I, Menu) ->
    ?CHECK_ERROR(),
    case element(I, Menu) of
	separator ->
	    draw_separator(X, Y, Mw);
	{S,Item} when list(S) ->
	    wings_io:menu_text(X, Y, S),
	    draw_submenu(Item, X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3);
	{S1,S2,Item} when list(S1) ->
	    S = S1 ++ lists:duplicate(Shortcut-length(S1), $\s) ++ S2,
	    wings_io:menu_text(X, Y, S),
	    draw_submenu(Item, X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3)
    end,
    ?CHECK_ERROR(),
    menu_draw(X, Y+?LINE_HEIGHT, Shortcut, Mw, I+1, Menu).

draw_submenu({Item}, X, Y) ->
    wings_io:beveled_rect(X, Y-3, ?CHAR_WIDTH, ?CHAR_WIDTH),
    ?CHECK_ERROR();
draw_submenu(Item, X, Y) when atom(Item); integer(Item); list(Item) -> ok;
draw_submenu(Item, X, Y) ->
    ?CHECK_ERROR(),
    gl:color3f(0.0, 0.0, 0.0),
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
    UpperY = Y-?CHAR_HEIGHT div 2 + 2,
    LowerY = UpperY + 1,
    gl:lineWidth(0.1),
    gl:color3f(0.0, 0.0, 0.0),
    gl:'begin'(?GL_LINES),
    gl:vertex2i(LeftX, UpperY),
    gl:vertex2i(RightX, UpperY),
    gl:'end'(),
    gl:color3f(1.0, 1.0, 1.0),
    gl:'begin'(?GL_LINES),
    gl:vertex2i(LeftX+1, LowerY),
    gl:vertex2i(RightX, LowerY),
    gl:'end'(),
    gl:color3f(0.0, 0.0, 0.0),
    ?CHECK_ERROR().


move_if_outside(X, Y, Mw, Mh) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    if
	X+Mw > W ->
	    move_if_outside(W-Mw, Y, Mw, Mh);
	Y+Mh > H ->
	    move_if_outside(X, H-Mh, Mw, Mh);
	true ->
	    move_if_outside_x(X, Y)
    end.

move_if_outside_x(X, Y) when X < 0 ->
    {0,Y};
move_if_outside_x(X, Y) ->
    {X,Y}.

