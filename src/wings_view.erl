%%
%%  wings_view.erl --
%%
%%     This module implements most of the command in the View menu.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_view.erl,v 1.18 2001/11/21 07:06:06 bjorng Exp $
%%

-module(wings_view).
-export([menu/3,command/2,init/0,current/0,set_current/1,
	 reset/0,projection/0,perspective/0,
	 model_transformations/1,aim/1,along/2,
	 align_to_selection/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2]).
-import(wings_draw, [model_changed/1]).

menu(X, Y, St) ->
    Wire = wings_pref:get_value(wire_mode, false),
    G = wings_pref:get_value(show_groundplane),
    A = wings_pref:get_value(show_axes),
    S = wings_pref:get_value(smooth_preview),
    O = wings_pref:get_value(orthogonal_view),
    Menu = {{one_of(G, "Hide", "Show") ++ " ground plane",show_groundplane},
	    {one_of(A, "Hide", "Show") ++ " axes",show_axes},
	    separator,
	    {one_of(Wire, "Filled", "Wireframe"),"w",wire_mode},
	    {one_of(S, "Flat Apperance", "Smooth Preview"),
	     "Tab",smooth_preview},
	    separator,
	    {"Reset View","r",reset},
	    {"Aim","a",aim},
	    {one_of(O, "Perspective View", "Ortographic View"),
	     "o",orthogonal_view},
	    separator,
	    {"View Along",{along,{{"+X","x",x},
				  {"+Y","y",y},
				  {"+Z","z",z},
				  {"-X","X",neg_x},
				  {"-Y","Y",neg_y},
				  {"-Z","Z",neg_z}}}},
	    separator,
	    {"Align to Selection",align_to_selection}},
    wings_menu:menu(X, Y, view, Menu).

command(reset, St) ->
    reset(),
    St;
command(smooth_preview, St) ->
    toggle_option(smooth_preview),
    model_changed(St);
command(orthogonal_view, St) ->
    toggle_option(orthogonal_view),
    projection(),
    St;
command(aim, St) ->
    aim(St),
    St;
command({along,Axis}, St) ->
    along(Axis, St),
    St;
command(flyaround, St) ->
    case wings_io:has_periodic_event() of
	true -> wings_io:cancel_periodic_event();
	false -> wings_io:periodic_event(60, {view,rotate_left})
    end,
    St;
command(rotate_left, St) ->
    #view{azimuth=Az0} = View = wings_view:current(),
    Az = Az0 + 1.0,
    set_current(View#view{azimuth=Az}),
    St;
command(align_to_selection, St) ->
    align_to_selection(St);
command(Key, St) ->
    toggle_option(Key),
    St.

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

    %% Always reset the following preferences + the view itself.
    wings_pref:set_value(wire_mode, false),
    wings_pref:set_value(smooth_preview, false),
    wings_pref:set_value(orthogonal_view, false),
    reset().

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

model_transformations(St) ->
    #view{origo=Origo,distance=Dist0,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0.0,0.0,1.0,0.0}),
    Dist = Dist0 * math:sqrt((W*H) / (640*480)),
    gl:translatef(PanX, PanY, 0.0),
    gl:translatef(0.0, 0.0, -Dist),
    gl:rotatef(El, 1.0, 0.0, 0.0),
    gl:rotatef(Az, 0.0, 1.0, 0.0),
    {OX,OY,OZ} = Origo,
    gl:translatef(OX, OY, OZ).
    
aim(#st{sel=[]}=St) -> St;
aim(St) ->
    Centers = wings_sel:centers(St),
    Avg = e3d_vec:average(Centers),
    Origo = e3d_vec:neg(Avg),
    View = current(),
    set_current(View#view{origo=Origo,pan_x=0.0,pan_y=0.0}).

along(x, St) ->
    along(-90.0, 0.0, St);
along(y, St) ->
    along(0.0, 90.0, St);
along(z, St) ->
    along(0.0, 0.0, St);
along(neg_x, St) ->
    along(90.0, 0.0, St);
along(neg_y, St) ->
    along(0.0, -90.0, St);
along(neg_z, St) ->
    along(180.0, 0.0, St).

along(Az, El, St) ->
    View = current(),
    set_current(View#view{azimuth=Az,elevation=El}),
    St.

align_to_selection(#st{sel=[]}=St) -> St;
align_to_selection(#st{selmode=vertex}=St) ->
    Ns = wings_sel:fold(
	   fun(Id, V, We, Acc) ->
		   [wings_vertex:normal(V, We)|Acc]
	   end, [], St),
    N = e3d_vec:norm(e3d_vec:add(Ns)),
    align_to_selection(N, St);
align_to_selection(#st{selmode=face}=St) ->
    Ns = wings_sel:fold(
	   fun(Id, Face, We, Acc) ->
		   [wings_face:normal(Face, We)|Acc]
	   end, [], St),
    N = e3d_vec:norm(e3d_vec:add(Ns)),
    align_to_selection(N, St);
align_to_selection(St) -> St.

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
