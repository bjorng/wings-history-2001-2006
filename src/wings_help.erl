%%
%%  wings_help.erl --
%%
%%     This module implements the Help menu.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_help.erl,v 1.4 2001/09/25 09:39:18 bjorng Exp $
%%

-module(wings_help).
-export([about/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

about(St) ->
    wings_io:display(
      fun(W, H) ->
	      Xs = 280,
	      Ys = 170+40,
	      gl:translated((W-Xs) / 2, (H-Ys) / 2, 0.0),
	      wings_io:beveled_rect(0, 0, Xs, Ys),
	      gl:color3f(0.0, 0.0, 0.0),
	      gl:recti(3, 3, Xs-3, Ys-3),
	      gl:color3f(1.0, 1.0, 1.0),
 	      gl:recti(4, 4, Xs-4, Ys-4),
	      gl:color3f(1.0, 0.0, 1.0),
	      gl:enable(?GL_TEXTURE_2D),
	      gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
	      wings_io:draw_icon(10, 10, 256, 128, wings),
	      wings_io:draw_icon(90, 140, 128, 64, powered),
	      gl:disable(?GL_TEXTURE_2D),
	      gl:flush(),
	      gl:color3f(0.0, 0.0, 0.0),
	      wings_io:text_at(10, 155, "Wings 3D " ++ ?WINGS_VERSION),
	      gl:flush(),
	      wait_for_click(),
	      St
      end).

wait_for_click() ->
    case wings_io:get_event() of
	#mousemotion{} -> wait_for_click();
	Other -> ok
    end.
