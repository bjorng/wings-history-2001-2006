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
%%     $Id: wings_menu.erl,v 1.8 2001/11/14 16:41:12 bjorng Exp $
%%

-module(wings_menu).
-export([menu/4,reactivate/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

%% Menu information kept for a popup menu.
-record(mi,
	{xleft,					%X left
	 ytop,					%Y top
	 shortcut,				%Position for shortcut (chars)
	 w,					%Width of menu (pixels)
	 h,					%Height of menu (pixels)
	 sel=none,				%Selected item (1..size(Menu))
	 name,					%Name of menu (atom)
	 menu,					%Original menu term
	 prev=none}).				%Previous mi record or `none'.

menu(X, Y, Name, Menu0) ->
    Menu = wings_plugin:menu(Name, Menu0),
    Mi = menu_setup(X, Y, Name, Menu, #mi{}),
    top_level(Mi).

top_level(Mi) ->
    R = wings_io:display(
	  fun(_, _) ->
		  menu_show(Mi),
		  catch get_menu_event(Mi)
	  end),
    wings_io:clear_menu_sel(),
    case R of
	{'EXIT',Reason} -> exit(Reason);
	ignore -> ok;
	Other -> wings_io:putback_event({action,Other})
    end,
    ignore.

reactivate(X, Y, none) -> none;
reactivate(X, Y, #mi{name=Name,prev=Prev}=Mi) ->
    case selected_item(X, Y, Mi) of
	outside ->
	    wings_io:set_current_menu(Prev),
	    if
		Prev =/= none -> ignore;
		true -> none
	    end;
	Item when Item =:= none; integer(Item) ->
 	    wings_io:putback_event(#mousebutton{button=1,x=X,y=Y,
 						state=?SDL_RELEASED}),
	    redraw(Mi),
	    top_level(Mi#mi{sel=Item})
    end.

redraw(#mi{prev=none}) -> ok;
redraw(#mi{prev=Prev}=Mi) ->
    redraw(Prev),
    menu_show(Mi).

menu(X0, Y0, Name, Menu, Mi0) ->
    Mi = menu_setup(X0, Y0, Name, Menu, Mi0),
    menu_show(Mi),
    get_menu_event(Mi).

menu_setup(X0, Y0, Name, Menu, Mi) ->
    {MwL,MwR} = menu_width(Menu),
    TotalW = (MwL+MwR) * ?CHAR_WIDTH + 9*?CHAR_WIDTH,
    Mh = size(Menu) * ?LINE_HEIGHT,
    {Xleft,Ytop} = move_if_outside(X0, Y0, TotalW, Mh),
    Mi#mi{xleft=Xleft,ytop=Ytop,shortcut=MwL+2,w=TotalW-10,h=Mh,
	  sel=none,name=Name,menu=Menu}.

menu_show(#mi{xleft=X,ytop=Y,shortcut=Shortcut,w=Mw,h=Mh,
	      sel=Item,name=Name,menu=Menu}=Mi) ->
    wings_io:beveled_rect(X, Y, Mw, Mh + ?LINE_HEIGHT div 3),
    gl:color3f(0.0, 0.0, 0.0),
    menu_draw(X+3*?CHAR_WIDTH, Y+?CHAR_HEIGHT, Shortcut, Mw, 1, Menu),
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
    handle_menu_event(wings_io:get_event(), Mi).

handle_menu_event(Event, #mi{name=Name,prev=Prev}=Mi0) ->
    case Event of
	#keyboard{keysym=#keysym{sym=27}} ->	%escape
	    ignore;
	#mousemotion{x=X,y=Y} ->
	    Mi = highlight_item(X, Y, Mi0),
	    get_menu_event(Mi);
	#mousebutton{button=B,x=X,y=Y,state=?SDL_RELEASED}=Button
	when B == 1; B == 3 ->
	    case select_item(X, Y, Mi0) of
		#mi{}=Mi -> get_menu_event(Mi);
		ignore ->
		    wings_io:set_current_menu(Prev),
		    wings_io:putback_event(Button),
		    throw(ignore);
		Other ->
		    {Name,Other}
	    end;
	_ ->
	    get_menu_event(Mi0)
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
	outside -> ignore;
	none -> Mi;
	Item when integer(Item) ->
	    Action = case element(Item, Menu) of
			 {_,A} -> A;
			 {_,_,A} -> A
		     end,
	    case Action of
		{What,SubMenu} ->
		    #mi{xleft=Xleft,ytop=Ytop,w=W,h=H}=Mi,
		    SubX = Xleft+W,
		    SubY = Ytop+(Item-1)*?LINE_HEIGHT,
		    menu(SubX, SubY, What, SubMenu, Mi#mi{prev=Mi});
		{Act} when Xleft =< X0, X0 < Xleft+W-2*?CHAR_WIDTH ->
		    Act;
		Act when atom(Act); integer(Act);
			 tuple(Act); list(Act) -> Act
	    end
    end.

selected_item(X0, Y0, #mi{xleft=Xleft,ytop=Ytop,w=W,h=H}=Mi) ->
    if
	Xleft =< X0, X0 < Xleft+W,
	Ytop =< Y0,  Y0 < Ytop+H ->
	    Item = ((Y0-Ytop) div ?LINE_HEIGHT) + 1,
	    case selectable(Item, Mi) of
		true -> Item;
		false -> none
	    end;
	true -> outside
    end.

selectable(Item, #mi{menu=Menu}) ->
    case element(Item, Menu) of
	separator -> false;
	{S,ignore} -> false;
	_ -> true
    end.
	    
toggle_highlight(Item, #mi{xleft=Xleft,ytop=Ytop,w=W,menu=Menu}) ->
    gl:color3f(1.0, 1.0, 0.5),
    gl:enable(?GL_COLOR_LOGIC_OP),
    gl:logicOp(?GL_XOR),
    Right = case has_options(Item, Menu) of
		true -> Xleft+W-3*?CHAR_WIDTH;
		false -> Xleft+W-?CHAR_WIDTH
	    end,
    gl:recti(Xleft+?CHAR_WIDTH, Ytop+(Item-1)*?LINE_HEIGHT,
	     Right, Ytop+Item*?LINE_HEIGHT),
    gl:flush(),
    gl:disable(?GL_COLOR_LOGIC_OP).

has_options(Item, Menu) ->
    case element(Item, Menu) of
	{_,{_}} -> true;
	{_,_,{_}}-> true;
	_ -> false
    end.
	    
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
draw_submenu(Item, X, Y) when atom(Item);integer(Item); list(Item) -> ok;
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
	true -> {X,Y}
    end.
