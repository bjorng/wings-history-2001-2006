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
%%     $Id: wings_menu.erl,v 1.15 2001/12/07 08:40:06 bjorng Exp $
%%

-module(wings_menu).
-export([menu/5,popup_menu/5]).

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
	 return_timer=none,			%Timer for returning to parent.
	 prev=[],				%Previous menu.
	 redraw,				%Redraw parent fun.
	 st,					%State record.
	 num_redraws=0				%Total number of redraws.
	}).

menu(X, Y, Name, Menu0, Redraw) ->
    Menu = wings_plugin:menu(Name, Menu0),
    Mi = menu_setup(plain, X, Y, Name, Menu, store_redraw(#mi{}, Redraw)),
    top_level(Mi).

popup_menu(X, Y, Name, Menu0, Redraw) ->
    Menu = wings_plugin:menu(Name, Menu0),
    Mi = menu_setup(popup, X, Y, Name, Menu, store_redraw(#mi{}, Redraw)),
    top_level(Mi).

menu(X0, Y0, Name, Menu, Mi0, Redraw) ->
    Mi = menu_setup(plain, X0, Y0, Name, Menu,
		    store_redraw(Mi0#mi{prev=Mi0}, Redraw)),
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

menu_setup(Type, X0, Y0, Name, Menu, Mi) -> 
    {MwL,MwR} = menu_width(Menu),
    TotalW = (MwL+MwR) * ?CHAR_WIDTH + 9*?CHAR_WIDTH,
    Mh = size(Menu) * ?LINE_HEIGHT,
    Margin = 3,
    {X1,Y1} = case Type of
		  plain -> {X0,Y0};
		  popup -> {X0-TotalW div 2,Y0 - Margin div 2}
	      end,
    {X,Y} = move_if_outside(X1, Y1, TotalW, Mh+2*Margin, Mi),
    Mi#mi{xleft=X,ytop=Y,ymarg=Margin,
	  shortcut=MwL+2,w=TotalW-10,h=Mh,
	  sel=none,name=Name,menu=Menu}.

menu_show(#mi{xleft=X,ytop=Y,ymarg=Margin,shortcut=Shortcut,w=Mw,h=Mh}=Mi) ->
    wings_io:raised_rect(X, Y, Mw, Mh + 2*Margin+3, ?MENU_COLOR),
    gl:color3f(0.0, 0.0, 0.0),
    menu_draw(X+3*?CHAR_WIDTH, Y+Margin+?CHAR_HEIGHT,
	      Shortcut, Mw, 1, Mi).

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
    
get_menu_event(Mi0) ->
    Mi = redraw(Mi0),
    {replace,fun(Ev) -> handle_menu_event(Ev, Mi) end}.

handle_menu_event(Event, #mi{name=Name,new=New}=Mi0) ->
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
	when not New and ((B == 1) or (B == 3)) ->
	    clear_timer(Mi0),
	    Mi1 = update_highlight(X, Y, Mi0),
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
	{go_back,NewEvent} ->
 	    wings_io:cleanup_after_drawing(),
 	    wings_io:putback_event(NewEvent),
 	    wings_io:putback_event(redraw),
 	    pop;
	{menu_action,Action} ->
	    wings_io:putback_event({menu_action,{Name,Action}}),
	    pop;
	redraw ->
	    get_menu_event(Mi0#mi{new=false,num_redraws=0});
	IgnoreMe ->
	    get_menu_event(Mi0#mi{new=false})
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
motion_outside(#mousemotion{x=X,y=Y}=Event, #mi{prev=PrevMenu0}=Mi) ->
    #mi{sel=PrevSel} = PrevMenu0,
    case selected_item(X, Y, PrevMenu0) of
	outside -> motion_outside(Event, PrevMenu0);
	none -> none;
	PrevSel -> keep;
	OtherSel ->
	    #mi{return_timer=Timer0} = Mi,
	    PrevMenu = PrevMenu0#mi{sel=OtherSel},
	    case Timer0 of
		none ->
		    Tim = wings_io:set_timer(350, {go_back,Event}),
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
	    Timer = wings_io:set_timer(350, Event),
	    Mi#mi{timer=Timer}
    end.

redraw(#mi{redraw=Redraw,st=St,num_redraws=NumRedraws}=Mi) ->
    if
	NumRedraws < 2 ->
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
		    Redraw = new_redraw_fun(Mi),
		    menu(SubX, SubY, What, SubMenu, Mi, Redraw);
		{Act} when Xleft =< X0, X0 < Xleft+W-2*?CHAR_WIDTH ->
		    Act;
		Act when atom(Act); integer(Act);
			 tuple(Act); list(Act) -> Act
	    end
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
	{S,ignore} -> false;
	_ -> true
    end.
	    
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
	    
menu_draw(X, Y, Shortcut, Mw, I, #mi{menu=Menu}) when I > size(Menu) -> ok;
menu_draw(X, Y, Shortcut, Mw, I, #mi{menu=Menu}=Mi) ->
    ?CHECK_ERROR(),
    case element(I, Menu) of
	separator ->
	    draw_separator(X, Y, Mw);
	{Text,Item} when is_list(Text) ->
	    item_colors(I, Mi),
	    wings_io:menu_text(X, Y, Text),
	    draw_submenu(Item, X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3);
	{S1,S2,Item} when is_list(S1) ->
	    Text = S1 ++ lists:duplicate(Shortcut-length(S1), $\s) ++ S2,
	    item_colors(I, Mi),
	    wings_io:menu_text(X, Y, Text),
	    draw_submenu(Item, X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3)
    end,
    ?CHECK_ERROR(),
    menu_draw(X, Y+?LINE_HEIGHT, Shortcut, Mw, I+1, Mi).

item_colors(Sel, #mi{sel=Sel}=Mi) ->
    draw_blue_rect(Sel, Mi),
    gl:color3f(1.0, 1.0, 1.0);
item_colors(I, Mi) -> gl:color3f(0.0, 0.0, 0.0).

draw_blue_rect(Item, #mi{xleft=Xleft,ytop=Ytop,
			 ymarg=Margin,w=W,menu=Menu}) ->
    gl:color3f(0.0, 0.0, 0.5),
    Right = case has_options(Item, Menu) of
		true -> Xleft+W-3*?CHAR_WIDTH;
		false -> Xleft+W-?CHAR_WIDTH
	    end,
    gl:recti(Xleft+?CHAR_WIDTH, Ytop+Margin+(Item-1)*?LINE_HEIGHT,
	     Right, Ytop+Margin+Item*?LINE_HEIGHT).

draw_submenu({Item}, X, Y) ->
    wings_io:sunken_rect(X, Y-3, ?CHAR_WIDTH, ?CHAR_WIDTH, ?MENU_COLOR),
    ?CHECK_ERROR();
draw_submenu(Item, X, Y) when atom(Item); integer(Item); list(Item) -> ok;
draw_submenu(Item, X, Y) ->
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
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
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
