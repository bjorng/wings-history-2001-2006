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
%%
%%     $Id: wpc_tweak.erl,v 1.20 2002/11/27 06:20:36 bjorng Exp $
%%

-module(wpc_tweak).

-export([init/0,menu/2,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").

-import(lists, [member/2,foldl/3]).

-record(tweak,
	{tmode,					% wait or drag
	 magnet=false,				% true/false
	 mag_type=dome,				% magnet type: Type
	 mag_r=1.0,				% magnet influence radius
	 ox,oy,					% original X,Y
	 cx,cy,					% current X,Y
	 orig_st,				% keeps undo, selection
	 st}).					% wings st record (working)

-record(drag,
	{v,
	 vtx,
	 mag,
	 mm,					%original|mirror
	 plane
	 }).

-record(mag,
	{orig,					%Orig pos of vertex
						% being moved.
	 vs,					%[{V,Vtx,Distance,Influence}]
						%  (not changed while dragging)
	 vtab=[]				%[{V,Vtx}] (latest)
	}).

init() -> true.

menu({tools}, Menu0) ->
    Menu0 ++ [separator,
	      {"Tweak", tweak,
	       "Mode for quickly changing vertex positions"}
	     ];
menu(_, Menu) -> Menu.

command({tools,tweak}, St0) ->
    Modes = [vertex],
    wings_io:icon_restriction(Modes),
    wings_util:menu_restriction(geom, [view]),
    St = wings_undo:init(St0#st{selmode=vertex,sel=[]}),
    wings_draw:update_dlists(St),
    T = #tweak{tmode=wait,orig_st=St0,st=St},
    help(T),
    wings_wm:dirty(),
    {seq,push,update_tweak_handler(T)};
command(_, _) -> next.

%% Event handler for tweak mode

update_tweak_handler(T) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> handle_tweak_event(Ev, T) end}.

handle_tweak_event(redraw, #tweak{st=St}=T) ->
    help(T),
    redraw(St),
    draw_magnet(T),
    keep;
handle_tweak_event(Ev, #tweak{st=St}=T) ->
    case wings_camera:event(Ev, St) of
	next -> handle_tweak_event0(Ev, T);
	Other -> Other
    end.

handle_tweak_event0(#keyboard{keysym=#keysym{sym=?SDLK_ESCAPE}}, T) ->
    exit_tweak(T);
handle_tweak_event0(#keyboard{keysym=#keysym{unicode=C}}=Ev, T0) ->
    case magnet_hotkey(C, T0) of
	none -> handle_tweak_event1(Ev, T0);
	T -> update_tweak_handler(T)
    end;
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
	    begin_drag(MM, St, T0),
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
    exit_tweak(T);
handle_tweak_event1(#resize{}=Ev, T) ->
    wings_io:putback_event(Ev),
    exit_tweak(T);
handle_tweak_event1(quit=Ev, T) ->
    wings_io:putback_event(Ev),
    exit_tweak(T);
handle_tweak_event1(Ev, #tweak{st=St0}=T) ->
    case wings_hotkey:event(Ev, St0) of
	{select,less} ->
	    update_tweak_handler(magnet_radius(-1, T));
	{select,more} ->
	    update_tweak_handler(magnet_radius(1, T));
	{view,auto_rotate} -> keep;
	{view,smoothed_preview} -> keep;
	{view,Cmd} ->
	    St = wings_view:command(Cmd, St0),
	    refresh_dlists(Cmd, St),
	    update_tweak_handler(T#tweak{st=St});
	{edit,undo_toggle} ->
	    St = wings_undo:undo_toggle(St0),
	    wings_draw:update_dlists(St),
	    update_tweak_handler(T#tweak{st=St});
	{edit,undo} ->
	    St = wings_undo:undo(St0),
	    wings_draw:update_dlists(St),
	    update_tweak_handler(T#tweak{st=St});
	{edit,redo} ->
	    St = wings_undo:redo(St0),
	    wings_draw:update_dlists(St),
	    update_tweak_handler(T#tweak{st=St});
	_Ignore -> keep
    end.

exit_tweak(#tweak{orig_st=St,st=#st{shapes=Shs}}) ->
    wings_io:putback_event({new_state,St#st{shapes=Shs}}),
    pop.

refresh_dlists(wireframe_selected, _) -> ok;
refresh_dlists(shade_selected, _) -> ok;
refresh_dlists(toggle_wireframe, _) -> ok;
refresh_dlists(orthogonal_view, _) -> ok;
refresh_dlists(aim, _) -> ok;
refresh_dlists(frame, _) -> ok;
refresh_dlists(toggle_lights, _) -> ok;
refresh_dlists({along,_}, _) -> ok;
refresh_dlists({toggle_lights,_}, _) -> ok;
refresh_dlists(_, St) -> wings_draw:update_dlists(St).

redraw(St) ->
    wings_draw:update_sel_dlist(),
    wings_draw_util:render(St),
    wings_io:update(St),
    keep.

begin_drag(MM, St, T) ->
    wings_draw:update_dlists(St),
    wings_draw_util:map(fun(D, _) ->
				begin_drag_fun(D, MM, St, T)
			end, []).

begin_drag_fun(#dlo{src_we=We}=D, _, _, _) when ?IS_LIGHT(We) -> D;
begin_drag_fun(#dlo{src_sel={vertex,Vs0},src_we=#we{vs=Vtab}=We}=D0,
	       MM, St, T) ->
    [V] = gb_sets:to_list(Vs0),
    Vtx = gb_trees:get(V, Vtab),
    {Vs,Magnet} = begin_magnet(T, V, Vtx, We),
    D = wings_draw:split(D0, Vs, St),
    Plane = mirror_plane(V, We),
    D#dlo{drag=#drag{v=V,vtx=Vtx,mag=Magnet,mm=MM,plane=Plane}};
begin_drag_fun(D, _, _, _) -> D.

end_drag(#tweak{st=St0}=T) ->
    St1 = wings_draw_util:map(fun end_drag/2, St0),
    St = wings_undo:save(St0, St1),
    wings_draw:update_dlists(St),
    help(T),
    update_tweak_handler(T#tweak{tmode=wait,st=St}).

end_drag(#dlo{src_we=#we{id=Id},drag=#drag{v=V,vtx=Vtx,mag=Mag}}=D,
	 #st{shapes=Shs0}=St0) ->
    #we{vs=Vtab0} = We0 = gb_trees:get(Id, Shs0),
    Vtab1 = gb_trees:update(V, Vtx, Vtab0),
    Vtab = magnet_end(Mag, Vtab1),
    We = We0#we{vs=Vtab},
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    {D#dlo{vs=none,sel=none,drag=none,src_we=We},St};
end_drag(D, St) -> {D,St}.

do_tweak(DX, DY) ->
    wings_draw_util:map(fun(D, _) ->
				do_tweak(D, DX, DY)
			end, []).
    				
do_tweak(#dlo{drag=#drag{v=V,vtx=#vtx{pos=Pos0}=Vtx0,
			 mag=Mag0,mm=MM,plane=Plane}=Drag,
	      src_we=#we{id=Id}}=D0, DX, DY) ->
    Matrices = wings_util:get_matrices(Id, MM),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    Pos1 = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    Pos = mirror_constrain(Plane, Pos1),
    Vtx = Vtx0#vtx{pos=Pos},
    {MagVtab,Mag} = magnet_tweak(Mag0, Pos),
    Vtab = [{V,Vtx}|MagVtab],
    D = D0#dlo{sel=none,drag=Drag#drag{vtx=Vtx,mag=Mag}},
    wings_draw:update_dynamic(D, Vtab);
do_tweak(D, _, _) -> D.

obj_to_screen({MVM,PM,VP}, {X,Y,Z}) ->
    {true, Xs,Ys,Zs} = glu:project(X, Y, Z, MVM, PM, VP),
    {Xs,Ys,Zs}.

screen_to_obj({MVM,PM,VP}, {Xs,Ys,Zs}) ->
    {true, X,Y,Z} = glu:unProject(Xs, Ys, Zs, MVM, PM, VP),
    {X,Y,Z}.

mirror_plane(_, #we{mirror=none}) -> none;
mirror_plane(V, #we{mirror=Face}=We) ->
    MirrorVs = wpa:face_vertices(Face, We),
    mirror_plane(V, MirrorVs, We).

mirror_plane(V, MirrorVs, We) ->
    case member(V, MirrorVs) of
	false -> none;
	true ->
	    {wings_face:face_normal(MirrorVs, We),
	     wpa:vertex_pos(hd(MirrorVs), We)}
    end.

mirror_constrain(none, Pos) -> Pos;
mirror_constrain({Plane,Point}, Pos) ->
    ToPoint = e3d_vec:sub(Point, Pos),
    Dot = e3d_vec:dot(ToPoint, Plane),
    ToPlane = e3d_vec:mul(Plane, Dot),
    e3d_vec:add(Pos, ToPlane).

%%%
%%% Magnetic tweak.
%%%

help(#tweak{magnet=false}) ->
    Msg = "[L] Drag vertices freely  [R] Exit tweak mode",
    wings_wm:message(Msg ++ "  " ++ wings_camera:help(),
		     "[1] Magnet On");
help(#tweak{magnet=true,mag_type=Type}) ->
    Msg = "[L] Drag  [R] Exit",
    MagMsg = "[1] Magnet Off  [+]/[-] Tweak R  " ++
	help_1(Type, [{2,dome},{3,straight},{4,spike}]),
    wings_wm:message(Msg, MagMsg).

help_1(Type, [{Digit,Type}|T]) ->
    "[" ++ [$0+Digit] ++ "] <<" ++
	wings_util:cap(atom_to_list(Type)) ++ ">> " ++ help_1(Type, T);
help_1(Type, [{Digit,ThisType}|T]) ->
    "[" ++ [$0+Digit] ++ "] " ++
	wings_util:cap(atom_to_list(ThisType)) ++ " " ++ help_1(Type, T);
help_1(_, []) -> [].

magnet_hotkey(C, #tweak{magnet=Mag,mag_type=Type0}=T) ->
    case hotkey(C) of
	none -> none;
	toggle when Mag == true ->
	    setup_magnet(T#tweak{magnet=false});
	toggle when Mag == false ->
	    setup_magnet(T#tweak{magnet=true});
	Type0 -> T;
	Type -> setup_magnet(T#tweak{magnet=true,mag_type=Type})
    end.

hotkey($1) -> toggle;
hotkey($2) -> dome;
hotkey($3) -> straight;
hotkey($4) -> spike;
hotkey(_) -> none.

setup_magnet(#tweak{tmode=drag}=T) ->
    wings_draw_util:map(fun(D, _) ->
				setup_magnet_fun(D, T)
			end, []),
    do_tweak(0.0, 0.0),
    wings_wm:dirty(),
    T;
setup_magnet(T) -> T.

setup_magnet_fun(#dlo{drag=#drag{v=V}=Drag}=Dl0, #tweak{st=St}=T) ->
    #we{vs=Vtab} = We = wings_draw:original_we(Dl0),
    Center = gb_trees:get(V, Vtab),
    {Vs,Mag} = begin_magnet(T, V, Center, We),
    Dl = wings_draw:split(Dl0, Vs, St),
    Dl#dlo{drag=Drag#drag{mag=Mag}};
setup_magnet_fun(Dl, _) -> Dl.

begin_magnet(#tweak{magnet=false}, V, _, _) -> {[V],none};
begin_magnet(#tweak{magnet=true}=T, CenterV, #vtx{pos=Center}, We) ->
    MirrorVs = mirror_vertices(We),
    Near = near(Center, CenterV, MirrorVs, T, We),
    Mag = #mag{orig=Center,vs=Near},
    {[CenterV|[Va || {Va,_,_,_,_} <- Near]],Mag}.

mirror_vertices(#we{mirror=none}) -> [];
mirror_vertices(#we{mirror=Face}=We) -> wpa:face_vertices(Face, We).

near(Center, CenterV, MirrorVs, #tweak{mag_r=R,mag_type=Type},
     #we{vs=Vtab}=We) ->
    foldl(fun({V,#vtx{pos=P}=Vtx}, A) when V =/= CenterV ->
		  case e3d_vec:dist(P, Center) of
		      D when D =< R ->
			  Inf = mf(Type, D, R),
			  Plane = mirror_plane(V, MirrorVs, We),
			  [{V,Vtx,Plane,D,Inf}|A];
		      _ -> A
		  end;
	     (_, A) -> A
	  end, [], gb_trees:to_list(Vtab)).
    
mf(dome, D, R) -> math:sin((R-D)/R*math:pi()/2);
mf(straight, D, R) -> (R-D)/R;
mf(spike, D0, R) when is_float(D0), is_float(R) ->
    D = (R-D0)/R,
    D*D.

magnet_tweak(none, _)  -> {[],none};
magnet_tweak(#mag{orig=Orig,vs=Vs}=Mag, Pos) ->
    Vec = e3d_vec:sub(Pos, Orig),
    Vtab = foldl(fun({V,#vtx{pos=P0}=Vtx,Plane,_,Inf}, A) ->
			 P1 = e3d_vec:add(P0, e3d_vec:mul(Vec, Inf)),
			 P = mirror_constrain(Plane, P1),
			 [{V,Vtx#vtx{pos=P}}|A]
		 end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

magnet_end(none, Vtab) -> Vtab;
magnet_end(#mag{vtab=Vs}, Vtab) ->
    foldl(fun({V,Vtx}, Vt) ->
		  gb_trees:update(V, Vtx, Vt)
	  end, Vtab, Vs).

magnet_radius(Sign, #tweak{mag_r=Falloff0}=T0) ->
    case Falloff0+Sign*?GROUND_GRID_SIZE/10 of
	Falloff when Falloff > 0 ->
	    setup_magnet(T0#tweak{mag_r=Falloff});
	_Falloff -> T0
    end.

draw_magnet(#tweak{magnet=false}) -> ok;
draw_magnet(#tweak{mag_r=R}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:disable(?GL_DEPTH_TEST),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    wings_view:projection(),
    wings_view:model_transformations(),
    gl:color4f(0, 0, 1, 0.1),
    wings_draw_util:fold(fun(D, _) ->
				 draw_magnet_1(D, R)
			 end, []),
    gl:popAttrib().

draw_magnet_1(#dlo{mirror=Mtx,
		   drag=#drag{mm=Side,vtx=#vtx{pos={X,Y,Z}}}}, R) ->
    case Side of
	mirror -> gl:multMatrixf(Mtx);
	original -> ok
    end,
    gl:translatef(X, Y, Z),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, R, 50, 50),
    glu:deleteQuadric(Obj);
draw_magnet_1(_, _) -> [].
