%%
%%  wings_wm.erl --
%%
%%     Window manager for Wings.
%%
%%  Copyright (c) 2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_win_scroller.erl,v 1.1 2003/01/02 14:49:38 bjorng Exp $
%%

-module(wings_win_scroller).

-export([vscroller/3,set_knob/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-record(ss,
	{knob_pos,				%Position of knob (0-1).
	 knob_prop,				%Proportion of knob (0-1).
	 track_pos=none
	}).

vscroller(Name, Pos, {_,H}) ->
    Ss = #ss{knob_pos=0.0,knob_prop=1.0},
    wings_wm:new({vscroller,Name}, Pos, {10,H},
		 {seq,push,get_event(Ss)}).

set_knob({vscroller,_}=Name, Pos, Proportion) ->
    wings_wm:send(Name, {set_knob,Pos,Proportion});
set_knob(Name, Pos, Proportion) ->
    set_knob({vscroller,Name}, Pos, Proportion).

%%%
%%% Implementation.
%%%

get_event(Ss) ->
    {replace,fun(Ev) -> event(Ev, Ss) end}.

event(redraw, Ss) ->
    redraw(Ss);
event({set_knob,Pos,Proportion}, #ss{knob_pos=Pos,knob_prop=Proportion}) ->
    keep;
event({set_knob,Pos,Proportion}, Ss) ->
    wings_wm:dirty(),
    get_event(Ss#ss{knob_pos=Pos,knob_prop=Proportion});
event(#mousebutton{button=1,y=Y,state=?SDL_PRESSED}, Ss) ->
    down(Y, Ss);
event(#mousebutton{button=1,state=?SDL_RELEASED}, Ss) ->
    wings_wm:release_focus(),
    get_event(Ss#ss{track_pos=none});
event(#mousemotion{y=Y,state=?SDL_PRESSED}, #ss{track_pos=Pos}=Ss)
  when Pos =/= none ->
    drag(Y, Ss);
event(_, _) -> keep.

down(Y0, #ss{knob_pos=Pos,knob_prop=Prop}=Ss) ->
    {_,_,_,H} = wings_wm:viewport(),
    Y = Y0/H,
    {vscroller,Client} = wings_wm:active_window(),
    if
	Y < Pos ->
	    wings_wm:send(Client, scroll_page_up),
	    keep;
	Y < Pos+Prop ->
	    wings_wm:grab_focus(),
	    get_event(Ss#ss{track_pos=Pos-Y});
	true ->
	    wings_wm:send(Client, scroll_page_down),
	    keep
    end.

drag(Y0, #ss{knob_prop=Prop,track_pos=TrackPos}) ->
    {_,_,_,H} = wings_wm:viewport(),
    Y = case Y0/H + TrackPos of
	    Y1 when Y1 < 0 -> 0.0;
	    Y1 when Y1 < 1-Prop -> Y1;
	    _ -> 1-Prop
	end,
    {vscroller,Client} = wings_wm:active_window(),
    wings_wm:send(Client, {set_knob_pos,Y}),
    keep.

redraw(#ss{knob_pos=Pos,knob_prop=Prop}) ->
    wings_io:ortho_setup(),
    {_,_,W,H} = wings_wm:viewport(),
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    gl:color3f(0.2, 0.2, 0.2),
    gl:rectf(2, H*Pos, W-2, H*(Pos+Prop)),
    keep.
