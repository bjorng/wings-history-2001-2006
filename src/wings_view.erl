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
%%     $Id: wings_view.erl,v 1.8 2001/09/24 07:24:53 bjorng Exp $
%%

-module(wings_view).
-export([default_view/1,projection/1,perspective/1,
	 model_transformations/1,aim/1,along/2,
	 align_to_selection/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

default_view(St) ->
    wings_drag:view_changed(St#st{origo={0.0,0.0,0.0},
				  azimuth=-45.0,elevation=25.0,
				  distance=?CAMERA_DIST,
				  pan_x=0.0,pan_y=0.0}).

projection(St) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    perspective(St),
    gl:matrixMode(?GL_MODELVIEW),
    St.

perspective(#st{opts=#opt{ortho=false}}) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    glu:perspective(45.0, W/H, 0.25, 1000.0);
perspective(#st{opts=#opt{ortho=true}}) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Aspect = W/H,
    Sz = 4.0,
    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, 0.25, 1000.0).

model_transformations(#st{origo=Origo,distance=Dist0,azimuth=Az,
			  elevation=El,pan_x=PanX,pan_y=PanY}) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
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
    wings_drag:view_changed(St#st{origo=Origo,pan_x=0.0,pan_y=0.0}).

along(x, St) ->
    wings_drag:view_changed(St#st{azimuth=-90.0,elevation=0.0});
along(y, St) ->
    wings_drag:view_changed(St#st{azimuth=0.0,elevation=90.0});
along(z, St) ->
    wings_drag:view_changed(St#st{azimuth=0.0,elevation=0.0});
along(neg_x, St) ->
    wings_drag:view_changed(St#st{azimuth=90.0,elevation=0.0});
along(neg_y, St) ->
    wings_drag:view_changed(St#st{azimuth=0.0,elevation=-90.0});
along(neg_z, St) ->
    wings_drag:view_changed(St#st{azimuth=180.0,elevation=0.0}).

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
    St#st{azimuth=Az,elevation=El}.
    
arccos(X) when float(X) ->
    math:atan2(math:sqrt(1.0-X*X), X).

arcsin(X) when float(X) ->
    math:atan2(X, math:sqrt(1.0-X*X)).

to_degrees(A) when float(A) ->
    A*180.0/3.1416.
