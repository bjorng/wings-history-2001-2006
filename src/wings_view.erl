%%
%%  wings_view.erl --
%%
%%     This module implements most of the command in the View menu.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_view.erl,v 1.41 2002/02/03 07:22:41 bjorng Exp $
%%

-module(wings_view).
-export([menu/3,command/2,init/0,init_light/0,
	 current/0,set_current/1,
	 projection/0,perspective/0,
	 model_transformations/0,eye_point/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,foldl/3]).
-import(wings_draw, [model_changed/1]).

menu(X, Y, St) ->
    L = wings_pref:get_value(number_of_lights),
    Menu = [{"Ground plane",show_groundplane,crossmark(show_groundplane)},
	    {"Axes",show_axes,crossmark(show_axes)},
	    separator,
	    {"Wireframe",wire_mode,crossmark(wire_mode)},
	    {"Smooth Preview",smooth_preview,crossmark(smooth_preview)},
	    separator,
	    {"Show Saved BB",show_bb,crossmark(show_bb)},
	    {"Show Edges",show_edges,crossmark(show_edges)},
	    {"Show Normals",show_normals,crossmark(show_normals)},
	    {"Show Wireframe Backfaces",show_wire_backfaces,
	     crossmark(show_wire_backfaces)},
	    separator,
	    {"Reset View",reset},
	    {"Aim",aim},
	    {"Ortographic View",orthogonal_view,
	     crossmark(orthogonal_view)},
	    {one_of(L == 1, "Two lights", "One light"),toggle_lights},
	    separator,
	    {"View Along",{along,[{"+X",x},
				  {"+Y",y},
				  {"+Z",z},
				  {"-X",neg_x},
				  {"-Y",neg_y},
				  {"-Z",neg_z}]}},
	    separator,
	    {"Align to Selection",align_to_selection},
	    separator,
	    {"Auto Rotate",auto_rotate}],
    wings_menu:menu(X, Y, view, Menu, St).

crossmark(Key) ->
    case wings_pref:get_value(Key) of
	false -> [];
	true -> [crossmark]
    end.
	     
command(reset, St) ->
    reset(),
    St;
command(smooth_preview, St) ->
    toggle_option(smooth_preview),
    ?SLOW(model_changed(St));
command(flatshade, St) ->
    wings_pref:set_value(wire_mode, false),
    wings_pref:set_value(smooth_preview, false),
    model_changed(St);
command(smoothshade, St) ->
    wings_pref:set_value(wire_mode, false),
    wings_pref:set_value(smooth_preview, true),
    model_changed(St);
command(orthogonal_view, St) ->
    toggle_option(orthogonal_view),
    projection(),
    St;
command(show_edges, St) ->
    toggle_option(show_edges),
    St;
command(aim, St) ->
    aim(St),
    St;
command({along,Axis}, St) ->
    along(Axis),
    St;
command(auto_rotate, St) ->
    {seq,{push,dummy},set_auto_rotate_timer(St)};
command(rotate_left, St) ->
    #view{azimuth=Az0} = View = wings_view:current(),
    Az = Az0 + wings_pref:get_value(auto_rotate_angle),
    set_current(View#view{azimuth=Az}),
    St;
command(align_to_selection, St) ->
    align_to_selection(St);
command(toggle_lights, St) ->
    Lights = case wings_pref:get_value(number_of_lights) of
		 1 -> 2;
		 2 -> 1
	     end,
    wings_pref:set_value(number_of_lights, Lights),
    init_light(),
    St;
command(Key, St) ->
    toggle_option(Key),
    St.

auto_rotate_event(Event, Timer, St) ->
    case wings_camera:event(Event, fun() -> wings:redraw(St) end) of
	next -> auto_rotate_event_1(Event, Timer, St);
	Other ->
	    {seq,fun(Ev) ->
			 wings_io:putback_event(Ev),
			 set_auto_rotate_timer(St)
		 end,Other}
    end.

auto_rotate_event_1(#mousemotion{}, Timer, St) -> keep;
auto_rotate_event_1(#mousebutton{state=?SDL_PRESSED}, Timer, ST) -> keep;
auto_rotate_event_1({view,rotate_left=Cmd}, Timer, St) ->
    command(Cmd, dummy),
    wings:redraw(St),
    set_auto_rotate_timer(St);
auto_rotate_event_1(Other, Timer, St) ->
    wings_io:cancel_timer(Timer),
    pop.

set_auto_rotate_timer(St) ->
    Delay = wings_pref:get_value(auto_rotate_delay),
    Timer = wings_io:set_timer(Delay, {view,rotate_left}),
    {replace,fun(Ev) -> auto_rotate_event(Ev, Timer, St) end}.

toggle_option(Key) ->
    wings_pref:set_value(Key, not wings_pref:get_value(Key)).

current() ->
    [{_,View}] = ets:lookup(wings_state, view),
    View.

set_current(View) ->
    true = ets:insert(wings_state, {view,View}),
    View.

init() ->
    wings_pref:set_default(show_groundplane, true),
    wings_pref:set_default(show_axes, true),
    wings_pref:set_default(show_edges, true),
    wings_pref:set_default(number_of_lights, 1),
    wings_pref:set_default(show_normals, false),
    wings_pref:set_default(show_bb, true),
    wings_pref:set_default(show_wire_backfaces, false),

    %% Always reset the following preferences + the view itself.
    wings_pref:set_value(wire_mode, false),
    wings_pref:set_value(smooth_preview, false),
    wings_pref:set_value(orthogonal_view, false),
    reset().

init_light() ->
    case wings_pref:get_value(number_of_lights) of
	1 ->
	    gl:enable(?GL_LIGHT0),
	    gl:disable(?GL_LIGHT1);
	2 ->
	    gl:lightfv(?GL_LIGHT1, ?GL_DIFFUSE, {0.5,0.5,0.5,1}),
	    gl:enable(?GL_LIGHT0),
	    gl:enable(?GL_LIGHT1)
    end.

reset() ->
    set_current(#view{origo={0.0,0.0,0.0},
		      azimuth=-45.0,elevation=25.0,
		      distance=?CAMERA_DIST,
		      pan_x=0.0,pan_y=0.0}).

projection() ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    perspective(),
    gl:matrixMode(?GL_MODELVIEW).

perspective() ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    case wings_pref:get_value(orthogonal_view) of
	false ->
	    glu:perspective(45.0, W/H, 0.25, 1000.0);
	true ->
	    #view{distance=D0} = current(),
	    Aspect = W/H,
	    Sz = 4.0 * D0 / ?CAMERA_DIST,
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, 0.25, 1000.0)
    end.

model_transformations() ->
    #view{origo=Origo,distance=Dist0,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    case wings_pref:get_value(number_of_lights) of
	1 ->
	    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0.0,0.71,0.71,0.0});
	2 ->
	    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0.71,0.71,0.0,0.0}),
	    gl:lightfv(?GL_LIGHT1, ?GL_POSITION, {-0.71,-0.71,0.0})
    end,
    Dist = Dist0 * math:sqrt((W*H) / (640*480)),
    gl:translatef(PanX, PanY, -Dist),
    gl:rotatef(El, 1.0, 0.0, 0.0),
    gl:rotatef(Az, 0.0, 1.0, 0.0),
    {OX,OY,OZ} = Origo,
    gl:translatef(OX, OY, OZ).

eye_point() ->
    #view{origo=Origo,distance=Dist0,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Dist = Dist0 * math:sqrt((W*H) / (640*480)),
    M0 = e3d_mat:translate(e3d_vec:neg(Origo)),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
    M2 = e3d_mat:mul(M1, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    M = e3d_mat:mul(M2, e3d_mat:translate(-PanX, -PanY, Dist)),
    e3d_mat:mul_point(M, {0.0,0.0,0.0}).

aim(#st{sel=[]}) ->
    View = current(),
    set_current(View#view{origo=e3d_vec:zero()});
aim(St) ->
    Centers = wings_sel:centers(St),
    Origo0 = e3d_vec:average(Centers),
    Origo = e3d_vec:neg(Origo0),
    #view{distance=Dist0} = View = current(),
    Dist = case e3d_vec:dist(eye_point(), Origo0) of
	       D when D < Dist0 -> D;
 	       Other -> Dist0
 	   end,
    set_current(View#view{origo=Origo,distance=Dist,pan_x=0.0,pan_y=0.0}).

along(x) -> along(x, -90.0, 0.0);
along(y) -> along(y, 0.0, 90.0);
along(z) -> along(z, 0.0, 0.0);
along(neg_x) -> along(x, 90.0, 0.0);
along(neg_y) -> along(y, 0.0, -90.0);
along(neg_z) -> along(z, 180.0, 0.0).

along(Along, Az, El) ->
    View = current(),
    set_current(View#view{azimuth=Az,elevation=El,along_axis=Along}).

align_to_selection(#st{sel=[]}=St) -> St;
align_to_selection(#st{selmode=vertex}=St) ->
    N = average_normals(
	  fun(Vs, We, Acc) ->
		  foldl(fun(V, A) ->
				[wings_vertex:normal(V, We)|A]
			end, Acc, Vs)
	  end, St),
    align_to_selection(N, St);
align_to_selection(#st{selmode=edge}=St) ->
    N = average_normals(
	  fun(Edges, #we{es=Etab}=We, Acc) ->
		  foldl(fun(Edge, A) ->
				#edge{lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
				[wings_face:normal(Lf, We),
				 wings_face:normal(Rf, We)|A]
			end, Acc, Edges)
	  end, St),
    align_to_selection(N, St);
align_to_selection(#st{selmode=face}=St) ->
    N = average_normals(
	  fun(Faces, We, Acc) ->
		  foldl(fun(Face, A) ->
				[wings_face:normal(Face, We)|A]
			end, Acc, Faces)
	  end, St),
    align_to_selection(N, St);
align_to_selection(St) -> St.

average_normals(CalcNormals, St) ->
    Ns = wings_sel:fold(
	   fun(Items, We, Acc) ->
		   CalcNormals(gb_sets:to_list(Items), We, Acc)
	   end, [], St),
    e3d_vec:norm(e3d_vec:add(Ns)).

align_to_selection({Nx,Ny,Nz}=N, St) ->
    Z = {0.0,0.0,1.0},
    Az0 = e3d_vec:dot(e3d_vec:norm({Nx,0.0,Nz}), Z),
    Az1 = to_degrees(arccos(Az0)),
    Az = if
	     Nx < 0 -> Az1;
	     true -> -Az1
	 end,
    El0 = if
	      Nz > Nx ->
		  Nyz = e3d_vec:norm({0.0,Ny,Nz}),
		  e3d_vec:cross(Nyz, Z);
	      true ->
		  X = {1.0,0.0,0.0},
		  Nxy = e3d_vec:norm({Nx,Ny,0.0}),
		  e3d_vec:cross(Nxy, X)
	  end,
    El1 = e3d_vec:len(El0),
    El2 = to_degrees(arcsin(El1)),
    El = if
	     Ny < 0.0 -> -El2;
	     true -> El2
	 end,
    View = current(),
    set_current(View#view{azimuth=Az,elevation=El}),
    St.
    
arccos(X) when float(X) ->
    math:atan2(math:sqrt(1.0-X*X), X).

arcsin(X) when float(X) ->
    math:atan2(X, math:sqrt(1.0-X*X)).

to_degrees(A) when float(A) ->
    A*180.0/3.1416.

one_of(true, S, _) -> S;
one_of(false,_, S) -> S.
