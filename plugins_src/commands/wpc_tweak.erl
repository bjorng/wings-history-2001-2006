%%
%%  wpc_tweak.erl --
%%
%%     Tweak mode plugin.
%%
%%  Copyright (c) 2001-2002 Howard Trickey,
%%                     2002 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(wpc_tweak).

-export([init/0,menu/2,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").

-record(tweak,
	{tmode,		% wait or drag
	 ox,oy,		% Original X,Y
	 cx,cy,		% Current X,Y
	 st}).		% wings st record

init() -> true.

menu({tools}, Menu0) ->
    Menu0 ++ [separator,
	      {"Tweak", tweak,
	       "Mode for quickly changing vertex positions"}
	     ];
menu(_, Menu) -> Menu.

command({tools, tweak}, St0) ->
    Modes = [vertex],
    wings_io:icon_restriction(Modes),
    wings_io:message(help()),
    St = St0#st{selmode=vertex,sel=[]},
    wings_draw:update_dlists(St),
    T = #tweak{tmode=wait,st=St},
    wings_wm:dirty(),
    {seq,{push,dummy},update_tweak_handler(T)};
command({tools, tweak_end, #tweak{st=St}}, _) -> St;
command(_, _) -> next.

help() ->
    [lmb] ++ " Drag vertices freely   " ++ [rmb] ++ " Exit tweak mode".

%% Event handler for tweak mode

update_tweak_handler(T) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> handle_tweak_event(Ev, T) end}.

handle_tweak_event(redraw, #tweak{st=St}) ->
    wings_io:message(help()),
    redraw(St),
    keep;
handle_tweak_event(Ev, #tweak{st=St}=T) ->
    case wings_io:event(Ev) of
	next ->
	    case wings_camera:event(Ev, St) of
		next -> handle_tweak_event0(Ev, T);
		Other -> Other
	    end;
	Other -> Other
    end.

handle_tweak_event0(#mousemotion{}=Ev, #tweak{tmode=wait,st=St}=T) ->
    Redraw = fun() -> redraw(St) end,
    case wings_pick:hilite_event(Ev, St, Redraw) of
	next -> handle_tweak_event1(Ev, T);
	Other -> Other
    end;
handle_tweak_event0(Ev, T) -> handle_tweak_event1(Ev, T).

handle_tweak_event1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED},
		    #tweak{tmode=wait,st=St0}=T0) ->
    case wpa:pick(X, Y, St0) of
	{add,MM,St} ->
	    begin_drag(MM, St),
	    do_tweak(0.0, 0.0),
	    T = T0#tweak{tmode=drag,ox=X,oy=Y,cx=X,cy=Y},
	    update_tweak_handler(T);
	_ ->
	    update_tweak_handler(T0)
    end;
handle_tweak_event1(#mousemotion{x=X,y=Y,state=?SDL_PRESSED},
		    #tweak{tmode=drag,cx=CX,cy=CY}=T0) ->
    DX = float(X-CX),
    DY = float(Y-CY),
    do_tweak(DX, DY),
    T = T0#tweak{cx=X,cy=Y},
    update_tweak_handler(T);
handle_tweak_event1(#mousebutton{button=1,state=?SDL_RELEASED},
		    #tweak{tmode=drag}=T) ->
    end_drag(T);
handle_tweak_event1(#mousemotion{state=?SDL_RELEASED},
		    #tweak{tmode=drag}=T) ->
    end_drag(T);
handle_tweak_event1(#mousebutton{button=3,state=?SDL_RELEASED}, T) ->
    wings_io:putback_event({action,{tools,tweak_end,T}}),
    pop;
handle_tweak_event1(Ev, #tweak{st=St}=T) ->
    case wings_hotkey:event(Ev, St) of
	{view,Cmd} ->
	    St1 = wings_view:command(Cmd,St),
	    update_tweak_handler(T#tweak{st=St1});
	_ -> keep
    end.

redraw(St) ->
    wings_draw:update_sel_dlist(),
    wings_draw_util:render(St),
    wings_io:update(St),
    keep.

begin_drag(MM, St) ->
    wings_draw:update_dlists(St),
    wings_draw_util:map(fun begin_drag_fun/2, MM).

begin_drag_fun(#dlo{src_sel={vertex,Vs0},src_we=#we{vs=Vtab}}=D0, MM) ->
    [V] = Vs = gb_sets:to_list(Vs0),
    Vtx = gb_trees:get(V, Vtab),
    {D,SplitData} = wings_draw:split(D0, Vs),
    {D#dlo{drag={V,Vtx,MM,SplitData}},MM};
begin_drag_fun(D, _) -> D.

end_drag(#tweak{st=St0}=T) ->
    St = wings_draw_util:map(fun end_drag/2, St0),
    wings_draw:update_dlists(St),
    wings_io:message(help()),
    update_tweak_handler(T#tweak{tmode=wait,st=St}).

end_drag(#dlo{src_we=#we{id=Id},drag={V,Vtx,_,_}}=D, #st{shapes=Shs0}=St0) ->
    #we{vs=Vtab0} = We = gb_trees:get(Id, Shs0),
    Vtab = gb_trees:update(V, Vtx, Vtab0),
    Shs = gb_trees:update(Id, We#we{vs=Vtab}, Shs0),
    St = St0#st{shapes=Shs},
    {D#dlo{vs=none,sel=none,drag=none},St};
end_drag(D, St) -> {D,St}.

do_tweak(DX, DY) ->
    wings_draw_util:map(fun(D, _) ->
				do_tweak(D, DX, DY)
			end, []).
				
do_tweak(#dlo{drag={V,#vtx{pos=Pos0}=Vtx0,MM,SplitData},
	      src_we=#we{id=Id}}=D0, DX, DY) ->
    Matrices = wings_util:get_matrices(Id, MM),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    Pos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    Vtx = Vtx0#vtx{pos=Pos},
    Vtab = [{V,Vtx}],
    D = D0#dlo{sel=none,drag={V,Vtx,MM,SplitData}},
    wings_draw:update_dynamic(D, SplitData, Vtab);
do_tweak(D, _, _) -> D.

obj_to_screen({MVM,PM,VP}, {X,Y,Z}) ->
    {true, Xs,Ys,Zs} = glu:project(X, Y, Z, MVM, PM, VP),
    {Xs,Ys,Zs}.

screen_to_obj({MVM,PM,VP}, {Xs,Ys,Zs}) ->
    {true, X,Y,Z} = glu:unProject(Xs, Ys, Zs, MVM, PM, VP),
    {X,Y,Z}.
