%%
%%  wings_material.erl --
%%
%%     This module manages the face materials (i.e. colors and textures).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_material.erl,v 1.1 2001/08/14 18:16:39 bjorng Exp $
%%

-module(wings_material).
-export([default/0,add/3,edit/2]).

-include("gl.hrl").
-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_keyboard.hrl").
-include("wings.hrl").

-import(lists, [sort/1]).

default() ->
    M0 = [{default,wings_util:share(0.5, 0.5, 0.5)},
	  {hole,wings_util:share(0.5, 0.5, 0.0)},
	  {black,wings_util:share(0.0, 0.0, 0.0)},
	  {red,wings_util:share(1.0, 0.0, 0.0)},
	  {green,wings_util:share(0.0, 1.0, 0.0)},
	  {blue,wings_util:share(0.0, 0.0, 1.0)},
	  {white,wings_util:share(1.0, 1.0, 1.0)}],
    M = [{Key,make_default(Color)} || {Key,Color} <- M0],
    gb_trees:from_orddict(sort(M)).

make_default({R,G,B}) ->
    Opacity = 1.0,
    Color = {R,G,B},
    White = {1.0,1.0,1.0},
    setup_fun(#mat{ambient=Color,diffuse=Color,specular=White,
		   shininess=0.0,opacity=Opacity}).

add(Name, Mat0, #st{mat=MatTab}=St) ->
    Mat = setup_fun(Mat0),
    St#st{mat=gb_trees:enter(Name, Mat, MatTab)}.

setup_fun(Mat) ->
    #mat{ambient=Amb0,diffuse=Diff0,specular=Spec0,
	 shininess=Shine,opacity=Opac} = Mat,
    Amb = erlang:append_element(Amb0, Opac),
    Diff = erlang:append_element(Diff0, Opac),
    Spec = erlang:append_element(Spec0, Opac),
    F = fun() ->
		gl:materialfv(?GL_FRONT, ?GL_AMBIENT, Amb),
		gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, Diff),
		gl:materialfv(?GL_FRONT, ?GL_SPECULAR, Spec),
		gl:materialfv(?GL_FRONT, ?GL_SHININESS, (1.0-Shine)*128.0)
	end,
    Mat#mat{setup=F}.

edit(Name, #st{mat=Mtab0}=St) ->
    Mat0 = gb_trees:get(Name, Mtab0),
    Mat = wings_io:display(fun(_, _) -> edit_1(Name, Mat0) end),
    Mtab = gb_trees:update(Name, Mat, Mtab0),
    St#st{mat=Mtab}.

edit_1(Name, Mat) ->
    wings_io:draw_completions(
      fun() ->
	      #mat{ambient=Amb,diffuse=Diff,specular=Spec,
		   shininess=Shine,opacity=Opac,attr=Attr} = Mat,
	      wings_io:text_at(0, 0, format("Material: ~p", [Name])),

	      wings_io:text_at(0, 2*?LINE_HEIGHT, "Ambient:"),
	      color_box(10, 2*?LINE_HEIGHT, Amb),

	      wings_io:text_at(0, 3*?LINE_HEIGHT, "Diffuse:"),
	      color_box(10, 3*?LINE_HEIGHT, Diff),

	      wings_io:text_at(0, 4*?LINE_HEIGHT, "Specular:"),
	      color_box(10, 4*?LINE_HEIGHT, Spec),
	      
	      wings_io:text_at(0, 5*?LINE_HEIGHT,
			       format("Shininess: ~p%",
				      [round(Shine*100)])),
	      wings_io:text_at(0, 6*?LINE_HEIGHT,
			       format("Opacity: ~p%", [round(Opac*100)])),
	      attr(7*?LINE_HEIGHT, Attr),
	      gl:flush(),
	      key(),
	      Mat
      end).

attr(Y, [{Tag,Value}|T]) when integer(Tag) ->
    wings_io:text_at(0, Y, format("~p: ~p", [hex4(Tag),Value])),
    attr(Y+?LINE_HEIGHT, T);
attr(Y, [{Tag,Value}|T]) ->
    wings_io:text_at(0, Y, format("~p: ~p", [Tag,Value])),
    attr(Y+?LINE_HEIGHT, T);
attr(Y, [Prop|T]) ->
    wings_io:text_at(0, Y, format("~p", [Prop])),
    attr(Y+?LINE_HEIGHT, T);
attr(Y, []) -> ok.

hex4(Num) ->
    hex(4, Num, []).

hex(0, Num, Acc) -> Acc;
hex(N, Num, Acc) ->
    hex(N-1, Num div 10, [hexd(Num rem 16)|Acc]).

hexd(D) when 0 =< D, D =< 9 -> D+$0;
hexd(D) when 10 =< D, D =< 16 -> D+$A-10.
    
format(F, L) ->
    lists:flatten(io_lib:format(F, L)).

key() ->	      
    case wings_io:get_event() of
	#keyboard{} -> ok;
	Other -> key()
    end.

color_box(X0, Y, Color) ->
    gl:color3fv(Color),
    X = X0 * ?CHAR_WIDTH,
    gl:recti(X, Y-?CHAR_HEIGHT+3, X+3*?CHAR_WIDTH, Y+3),
    gl:color3i(0, 0, 0).
