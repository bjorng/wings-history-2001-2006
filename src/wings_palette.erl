%%
%%  wings_palette.erl --
%%
%%     Maintains the vertex palette window.
%%
%%  Copyright (c) 2004 Bjorn Gustavsson, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_palette.erl,v 1.6 2004/05/18 14:54:21 dgud Exp $
%%
-module(wings_palette).

-export([window/1,window/3]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [map/2,foldl/3,reverse/1,reverse/2,
		keymember/3,keysearch/3,keydelete/3,sort/1]).

-define(BOX_W, 16).
-define(BOX_H, 16).

-define(COLS_W, 8).
-define(COLS_H, 8).

-define(BORD, 2).

-record(pst, {st, sel=none, cols, w=?COLS_W, h=?COLS_H, knob=0}).

window(St) ->
    case wings_wm:is_window(palette) of
	true ->
	    wings_wm:raise(palette),
	    keep;
	false ->
	    {{_,DeskY},{DeskW,_DeskH}} = wings_wm:win_rect(desktop),
	    Pos  = {DeskW-5,DeskY+55},
	    Size = {?COLS_W*?BOX_W+?COLS_W*?BORD+?BORD*2, 
		    ?COLS_H*?BOX_H+?COLS_H*?BORD+?BORD*2},
	    window(Pos, Size, St),
	    keep
    end.

window(Pos, Size = {W,_}, St) ->
    Cols0 = get_all_colors(St),
    ColsW = W div (?BOX_W+?BORD),
    ColsH0 = length(Cols0) div ColsW,
    ColsH = if (length(Cols0) rem ColsW) == 0 -> ColsH0; true -> ColsH0+1 end,
    Cols = Cols0 ++ lists:duplicate(ColsH*ColsW-length(Cols0),none),
    Pst = #pst{st=St, cols=Cols, w=ColsW, h=ColsH},
    Op  = {seq,push,event({current_state,St}, Pst)},
    Props = [{display_lists,geom_display_lists}],
    wings_wm:toplevel(palette, "Palette", Pos, Size,
		      [{sizeable,?PANE_COLOR},
		       closable, vscroller,
		       {anchor,ne},
		       {properties,Props}], Op),
    F = fun({color,_}) -> yes;
	   ({material, _}) -> yes;
	   (_) -> no
	end,
    wings_wm:set_prop(palette, drag_filter, F),
    wings_wm:dirty().
    
default_cols() ->
    [{1.0,1.0,1.0},{1.0,0.0,0.0},{1.0,1.0,0.0},{0.0,1.0,0.0},
     {0.0,1.0,1.0},{0.0,0.0,1.0},{1.0,0.0,1.0},{0.0,0.0,0.0}].

get_all_colors(_St) ->
    case get(?MODULE) of
	undefined ->
	    Def = default_cols(),
	    Def ++ lists:duplicate(?COLS_W*?COLS_H-length(Def), none);
	Cols ->
	    Cols
    end.

get_event(Pst) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> event(Ev, Pst) end}.

event(redraw, Pst) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    draw_objs(Pst),
    keep;

event(resized, Pst=#pst{cols=Cols0,w=CW,h=CH}) ->
    {_,_,W,H} = wings_wm:viewport(),
    ColsW0 = W div (?BOX_W+?BORD),
    ColsW = if ColsW0 < 1 -> 1; true -> ColsW0 end,
    ColsH0 = H div (?BOX_H+?BORD),
    NewCols = ColsW*ColsH0,
    OldCols = CW*CH,
    {Cols,ColsH} = 
	if OldCols < NewCols ->
		New = lists:duplicate(NewCols-OldCols,none),
		{Cols0 ++ New, ColsH0};
	   (OldCols rem ColsW) == 0 -> 
		ColsH1 = OldCols div ColsW,
		{Cols0, ColsH1};
	   true ->
		{N, NonEmpty} = del_empty(reverse(Cols0)),
		Old = OldCols - N,
		ColsH1 = (Old div ColsW) + 1,
		if ColsH1 > ColsH0 ->
 			New = lists:duplicate(ColsH1*ColsW-Old,none),
			{reverse(NonEmpty) ++ New, ColsH1};
		   true ->
 			New = lists:duplicate(ColsH0*ColsW-Old,none),
			{reverse(NonEmpty) ++ New, ColsH0}
		end
	end,
%     io:format("W ~p H ~p {~p,~p}= ~p {~p,~p}=~p Real ~p ~n", 
% 	      [W,H,ColsW,ColsH0,ColsW*ColsH0,ColsW,ColsH,ColsW*ColsH,length(Cols)]),
    update_scroller(0,ColsH0,ColsH),
    get_event(Pst#pst{knob=0,cols=Cols,w=ColsW,h=ColsH});

event({set_knob_pos, Pos}, Pst = #pst{h=N,knob=Knob}) ->
    case round(Pos*N) of
	Knob -> keep;
	New0  ->
	    New = if New0 < N -> New0; true -> N end,
	    {_,_,_,H} = wings_wm:viewport(),
	    Visible = H div (?BOX_H+?BORD),
	    update_scroller(New,Visible,N),
	    get_event(Pst#pst{knob=New})
    end;
event(close, #pst{cols=Cols}) ->
    put(?MODULE, Cols),
    delete;
event(got_focus, _) ->
    Msg = wings_util:button_format("Assign color to selection  [Ctrl]+L: Clear Color",
				   "Edit Color",
				   "Show menu"),
    wings_wm:message(Msg),
    wings_wm:dirty();
event({current_state,St}, Pst) ->
    get_event(Pst#pst{st=St});
event(Ev = #mousemotion{state=Bst,x=X,y=Y, mod=Mod}, Pst = #pst{sel=Sel,cols=Cols})
  when Bst band ?SDL_BUTTON_LMASK =/= 0 ->
    Delete = Mod band ?CTRL_BITS =/= 0,
    case select(X,Y,Pst) of
	none -> keep;
 	Sel -> keep;
	Id when Delete ->
	    {Bef,[_Prev|Rest]} = lists:split(Id, Cols),
	    get_event(Pst#pst{sel=none, cols=Bef++[none|Rest]});
	_ when Sel == none ->
	    keep;
	_ ->
	    case lists:nth(Sel+1, Cols) of
		none -> 
		    get_event(Pst#pst{sel=none});		
		_ ->
		    drag_and_drop(Ev, lists:nth(Sel+1, Cols)),
		    keep	    
	    end
    end;
event(#mousebutton{button=Butt,x=X,y=Y,mod=Mod,state=?SDL_PRESSED}, #pst{cols=Cols0}=Pst) 
  when Butt =:= 1; Butt =:= 2 ->
    case Mod band ?CTRL_BITS =/= 0 of
	false ->
	    get_event(Pst#pst{sel=select(X,Y,Pst)});
	true when Butt =:= 1 ->
	    case select(X,Y,Pst) of
		none -> keep;
		Id ->
		    {Bef,[_Prev|Rest]} = lists:split(Id, Cols0),		    
		    get_event(Pst#pst{sel=none, cols=Bef++[none|Rest]})
	    end;
	true ->
	    keep
    end;

event(#mousebutton{button=1,state=?SDL_RELEASED}, #pst{sel=none}) ->
    keep;
event(#mousebutton{button=2,x=X,y=Y,state=?SDL_RELEASED}, Pst = #pst{sel=Sel}) ->
    case select(X,Y,Pst) of
	Sel -> 
	    command({edit,Sel}, Pst);
	_ -> 
	    get_event(Pst#pst{sel=none})
    end;
event(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED}, #pst{sel=Sel,cols=Cols,st=St0}=Pst) ->
    Color = lists:nth(Sel+1, Cols),
    case select(X,Y,Pst) of
	none -> keep;
	Sel when Color /= none -> 
	    St = case St0#st.selmode of
		     vertex ->
			 wings_vertex_cmd:set_color(Color, St0);
		     edge ->
			 wings_edge:set_color(Color, St0);
		     face ->
			 wings_face_cmd:set_color(Color, St0);
		     object -> %BUGBUG
			 %% 		    St=wings_object_cmd:set_color(Color, St0),
			 %% 		    wings_wm:send(geom, {new_state,St}),
			 St0;
		     _ ->
			 St0
		 end,
	    wings_wm:send(geom, {new_state,St}),
	    get_event(Pst#pst{sel=none});
	Sel when Color == none -> 
	    command({edit,Sel}, Pst);
	_DropSpot ->
	    get_event(Pst#pst{sel=none})
    end;
event(#mousebutton{x=X0,y=Y0}=Ev, Pst) ->
    Id = select(X0,Y0,Pst),
    case wings_menu:is_popup_event(Ev) of
 	{yes,X,Y,_} when is_integer(Id) ->
	    do_menu(Id, X, Y, Pst);
	_ -> keep
    end;
event({action,{palette,Cmd}}, Pst) ->
    command(Cmd, Pst);
event(lost_focus, Pst) ->
    wings_wm:allow_drag(false),
    get_event(Pst#pst{sel=none});
event({new_color,Cols}, Pst) ->
    get_event(Pst#pst{cols=Cols});
event({drop,{X,Y},{color,Color}}, #pst{cols=Cols0}=Pst) ->
    case select(X,Y,Pst) of
	none -> keep;
	Id ->
	    {Bef,[_Prev|Rest]} = lists:split(Id, Cols0),
	    Cols = Bef ++ [color(Color)|Rest],
	    get_event(Pst#pst{cols=Cols})
    end;
event({drop,{X,Y},{material,Name}}, #pst{cols=Cols0,st=St}=Pst) ->
    case select(X,Y,Pst) of
	none -> keep;
	Id ->
	    {Bef,[_Prev|Rest]} = lists:split(Id, Cols0),
	    Mat = gb_trees:get(list_to_atom(Name), St#st.mat),
	    Color = proplists:get_value(diffuse, proplists:get_value(opengl, Mat)),
	    Cols = Bef ++ [color(Color)|Rest],
	    get_event(Pst#pst{cols=Cols})
    end;
event(_Ev, _Pst) ->
%%    io:format("Missed Ev ~p~n", [_Ev]),
    keep.

update_scroller(First,Visible,Total) ->
    Name = wings_wm:this(),
    wings_wm:set_knob(Name, First/Total, Visible/Total).

do_menu(Id,X,Y,#pst{cols=Cols}) ->    
    Menu = [{"Edit",{'VALUE',{edit,Id}}, "Edit color"}],
    Smooth = case lists:nth(Id+1, Cols) of
		 none ->
		     [{"Interpolate",{'VALUE',{smooth,Id}}, 
		       "Interpolate Empty Colors"}];
		 _ -> []
	     end,
    Rest = [{"Clear All", {'VALUE',clear_all}, "Clear palette"}],
    wings_menu:popup_menu(X,Y,palette,Menu ++ Smooth ++ Rest).

command(clear_all, Pst) ->
    get_event(Pst#pst{sel=none,cols=lists:duplicate(?COLS_W*?COLS_H, none)});
command({_,none}, _) -> keep;
command({smooth,Id}, Pst = #pst{cols=Cols0}) ->
    {Bef0,After0} = lists:split(Id, Cols0),
    {BC, Bef1} = del_empty(reverse(Bef0)),
    {AC, Aft1} = del_empty(After0),
    case interpolate(Bef1,Aft1,AC+BC) of
	no_start -> 
	    wings_util:message("No start color found."),
	    keep;
	no_end -> 
	    wings_util:message("No end color found."),
	    keep;
	IntCols ->
	    Cols = reverse(Bef1) ++ IntCols ++ Aft1,
	    get_event(Pst#pst{sel=none,cols=Cols})
    end;
command({edit,Id}, #pst{cols=Cols0}) ->
    {Bef,[Prev|Rest]} = lists:split(Id, Cols0),
    Send = fun(Color) ->
		   Cols = Bef ++ [Color|Rest],				    
		   wings_wm:send(palette, {new_color,Cols}),
		   ignore
	   end,
    wings_color:choose(color(Prev), Send).

del_empty(L) ->
    del_empty(L,0).
del_empty([none|L],N) ->
    del_empty(L,N+1);
del_empty(L,N) -> {N,L}.

interpolate([{R1,G1,B1}|_],[Start={R2,G2,B2}|_], N) ->
    R = (R2-R1)/(N+1),    B = (B2-B1)/(N+1),    G = (G2-G1)/(N+1),
    interpolate(N,R,G,B,Start,[]);
interpolate([],_,_) -> no_start;
interpolate(_,[],_) -> no_end.

interpolate(0,_R,_G,_B,_,Acc) -> Acc;
interpolate(N,R,G,B,{PR,PG,PB},Acc) -> 
    Col = {PR-R,PG-G,PB-B},
    interpolate(N-1,R,G,B,Col,[Col|Acc]).

drag_and_drop(Ev, What) ->
    DropData = {color, color(What)},
    wings_wm:drag(Ev, {?BOX_W,?BOX_H}, DropData).

draw_objs(#pst{cols=Cols0, w=W, h=_H, knob=Knob}) ->
    length(Cols0) == W*_H, %% Assert
    {_Bef,Cols} = lists:split(Knob*W, Cols0),
    draw_objs(0, ?BORD, ?BORD, W, Cols).
draw_objs(_,_,_,_,[]) ->    ok;
draw_objs(N,X,Y,W,[Color|Cols]) when N < W ->
    case Color of
	none ->
	    wings_io:border(X, Y, ?BOX_W, ?BOX_H, ?PANE_COLOR);
	_ ->
	    wings_io:border(X, Y, ?BOX_W, ?BOX_H, Color)
    end,
    draw_objs(N+1, X+?BOX_W+?BORD, Y, W, Cols);
draw_objs(_,_,Y,W,Cols) ->
    draw_objs(0, ?BORD, Y+?BOX_H+?BORD,W,Cols).

color(none) -> {1.0,1.0,1.0};
color({_,_,_}=C) -> C;
color({R,G,B,_}) -> {R,G,B}.

select(X,Y,#pst{w=ColsW,h=ColsH, knob=Knob}) ->
    Col = X div (?BORD+?BOX_W),
    Row = Knob + (Y div (?BORD+?BOX_H)),
    Id =  (Row*ColsW+Col),
    if 
	X > ColsW *(?BORD+?BOX_W) -> none;
	Y > ColsH *(?BORD+?BOX_H) -> none;
	Id < 0 -> none;
	Id >= (ColsW*ColsH) -> none;
	true -> Id
    end.
