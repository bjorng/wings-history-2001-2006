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
%%     $Id: wings_palette.erl,v 1.3 2004/05/14 05:25:40 bjorng Exp $
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

-record(pst, {st, sel=none, cols}).

window(St) ->
    case wings_wm:is_window(palette) of
	true ->
	    wings_wm:raise(palette),
	    keep;
	false ->
	    {{_,DeskY},{DeskW,_DeskH}} = wings_wm:win_rect(desktop),
	    Pos = {DeskW-5,DeskY+55},
	    Size = {?COLS_W*?BOX_W+?COLS_W*?BORD+?BORD*2, 
		    ?COLS_H*?BOX_H+?COLS_H*?BORD+?BORD*2},
	    window(Pos, Size, St),
	    keep
    end.

window(Pos, Size, St) ->
    Cols = get_all_colors(St),
    Pst = #pst{st=St, cols=Cols},
    Op  = {seq,push,event({current_state,St}, Pst)},
    Props = [{display_lists,geom_display_lists}],
    wings_wm:toplevel(palette, "Palette", Pos, Size,
		      [%{sizeable,?PANE_COLOR},
		       closable, %vscroller,
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
% event(resized, Pst) ->
%     update_scroller(Pst),
%     keep;
event(close, #pst{cols=Cols}) ->
    put(?MODULE, Cols),
    delete;
event(got_focus, _) ->
    Msg = wings_util:button_format("Assign color to selection", [],
				   "Show menu"),
    wings_wm:message(Msg),
    wings_wm:dirty();
event({current_state,St}, Pst) ->
    get_event(Pst#pst{st=St});
event(Ev = #mousemotion{state=Bst,x=X,y=Y}, #pst{sel=Sel,cols=Cols})
  when Sel /= none, Bst band ?SDL_BUTTON_LMASK =/= 0 ->    
    case select(X,Y) of
 	Sel -> keep;
 	_ ->
 	    drag_and_drop(Ev, lists:nth(Sel+1, Cols)),
 	    keep
    end;
event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, #pst{}=Pst) ->
    get_event(Pst#pst{sel=select(X,Y)});

event(#mousebutton{button=1,state=?SDL_RELEASED}, #pst{sel=none}) ->
    keep;
event(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED}, #pst{sel=Sel,cols=Cols,st=St0}=Pst) ->
    Color = lists:nth(Sel+1, Cols),
    case select(X,Y) of
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
	Sel -> 
	    get_event(Pst#pst{sel=none});
	_DropSpot ->
	    get_event(Pst#pst{sel=none})
    end;
event(#mousebutton{x=X0,y=Y0}=Ev, Pst) ->
    Id = select(X0,Y0),
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
    case select(X,Y) of
	none -> keep;
	Id ->
	    {Bef,[_Prev|Rest]} = lists:split(Id, Cols0),
	    Cols = Bef ++ [color(Color)|Rest],
	    get_event(Pst#pst{cols=Cols})
    end;
event({drop,{X,Y},{material,Name}}, #pst{cols=Cols0,st=St}=Pst) ->
    case select(X,Y) of
	none -> keep;
	Id ->
	    {Bef,[_Prev|Rest]} = lists:split(Id, Cols0),
	    Mat = gb_trees:get(list_to_atom(Name), St#st.mat),
	    Color = proplists:get_value(diffuse, proplists:get_value(opengl, Mat)),
	    Cols = Bef ++ [color(Color)|Rest],
	    get_event(Pst#pst{cols=Cols})
    end;

event(_Ev, _Pst) ->
    keep.

do_menu(Id,X,Y,_Pst) ->
    Menu = [{"Edit",{'VALUE',{edit,Id}}, "Edit color"}],
    wings_menu:popup_menu(X, Y, palette, Menu).

command({edit,Id}, #pst{cols=Cols0}) ->
    {Bef,[Prev|Rest]} = lists:split(Id, Cols0),
    wings_color:choose(color(Prev), fun(Color) ->
					    Cols = Bef ++ [Color|Rest],				    
					    wings_wm:send(palette, {new_color,Cols}),
					    ignore
				    end).

drag_and_drop(Ev, What) ->
    DropData = {color, color(What)},
    wings_wm:drag(Ev, {?BOX_W,?BOX_H}, DropData).

draw_objs(#pst{cols=Cols}) ->
    draw_objs(0, ?BORD, ?BORD, Cols).
draw_objs(_,_,_,[]) ->    ok;
draw_objs(N,X,Y,[Color|Cols]) when N < ?COLS_W ->
    case Color of
	none ->
	    wings_io:border(X, Y, ?BOX_W, ?BOX_H, ?PANE_COLOR);
	_ ->
	    wings_io:border(X, Y, ?BOX_W, ?BOX_H, Color)
    end,
    draw_objs(N+1, X+?BOX_W+?BORD, Y, Cols);
draw_objs(_,_,Y,Cols) ->
    draw_objs(0, ?BORD, Y+?BOX_H+?BORD,Cols).

color(none) -> {1.0,1.0,1.0};
color({_,_,_}=C) -> C;
color({R,G,B,_}) -> {R,G,B}.

select(X,Y) ->
    Col = X div (?BORD+?BOX_W),
    Row = Y div (?BORD+?BOX_H),
    Id =  Row*?COLS_W+Col,
    if Id < 0 -> none;
       Id >= (?COLS_W*?COLS_H) -> none;
       true -> Id
    end.
	    
	     
	

