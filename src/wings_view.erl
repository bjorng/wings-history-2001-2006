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
%%     $Id: wings_view.erl,v 1.1 2001/08/14 18:16:39 bjorng Exp $
%%

-module(wings_view).
-export([default_view/1,projection/1,perspective/1,
	 model_transformations/1,aim/1,along/2]).

-include("wings.hrl").
-include("gl.hrl").

default_view(St) ->
    Zero = 0.0,
    wings_drag:view_changed(St#st{origo={Zero,Zero,Zero},
				  azimuth=-45.0,elevation=25.0,
				  distance=?CAMERA_DIST,
				  pan_x=Zero,pan_y=Zero}).

projection(St) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    perspective(St),
    gl:matrixMode(?GL_MODELVIEW),
    St.

perspective(#st{opts=#opt{ortho=false}}) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    glu:perspective(45.0, W/H, 2.0, 1000.0);
perspective(#st{opts=#opt{ortho=true}}) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Aspect = W/H,
    Sz = 40.0,
    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, 2.0, 1000.0).

model_transformations(#st{origo=Origo,distance=Dist0,azimuth=Az,
			  elevation=El,pan_x=PanX,pan_y=PanY}) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Dist = Dist0 * math:sqrt((W*H) / (640*480)),
    {OX,OY,OZ} = Origo,
    gl:translatef(PanX, PanY, 0.0),
    gl:translatef(0.0, 0.0, -Dist),
    gl:rotatef(El, 1.0, 0.0, 0.0),
    gl:rotatef(Az, 0.0, 1.0, 0.0),
    gl:translatef(OX, OY, OZ).
    
aim(#st{sel=[]}=St) -> St;
aim(St) ->
    Centers = wings_sel:centers(St),
    Avg = wings_mat:average(Centers),
    Origo = wings_mat:negate(Avg),
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
