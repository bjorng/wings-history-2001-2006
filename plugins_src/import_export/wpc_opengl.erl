%%
%%  wpc_opengl.erl --
%%
%%     OpenGL renderer.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_opengl.erl,v 1.1 2002/07/08 17:13:09 bjorng Exp $

-module(wpc_opengl).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3,map/2,foreach/2,reverse/1,seq/2,
		flat_length/1,append/1,append/2]).

init() ->
    true.

menu({file,render}, Menu0) ->
    [{"Render using OpenGL",opengl,[option]}] ++ Menu0;
menu(_, Menu) -> Menu.

command({file,{render,{opengl,Ask}}}, St) ->
    do_render(Ask, St);
command(_, _) -> next.

dialog_qs(render) ->
    DefVar = {aa,get_pref(aa, regular)},
    [{hframe,
      [{vframe,
	[{key_alt,DefVar,"Draft (no AA)",draft},
	 {key_alt,DefVar,"Regular (normal AA)",regular},
	 {key_alt,DefVar,"Super",super},
	 {key_alt,DefVar,"Premium",premium}],
	[{title,"Quality"}]}]}].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

%%%
%%% Rendering.
%%%

-record(r,
	{pass=1,
	 acc_size=6}).

do_render(Ask, St) when is_atom(Ask) ->
    wpa:dialog(Ask, dialog_qs(render), St,
	       fun(Res) ->
		       {file,{render,{opengl,Res}}}
	       end);
do_render(Attr, St) ->
    set_pref(Attr),
    render_dlist(St),
    wings_wm:dirty(),
    Aa = property_lists:get_value(aa, Attr),
    AccSize = translate_aa(Aa),
    Rr = #r{acc_size=AccSize},
    {seq,{push,dummy},get_render_event(Rr)}.

translate_aa(draft) -> 1;
translate_aa(regular) -> 4;
translate_aa(super) -> 8;
translate_aa(premium) -> 16.
    
get_render_event(Rr) ->
    {replace,fun(Ev) -> render_event(Ev, Rr) end}.

render_event(redraw, Rr) ->
    render_redraw(Rr),
    get_render_event(Rr);
render_event(#mousemotion{}, _) -> keep;
render_event(#mousebutton{state=?SDL_RELEASED}, _) -> render_exit();
render_event(#keyboard{keysym=#keysym{sym=?SDLK_ESCAPE}}, _) ->
    render_exit();
render_event({resize,_,_}=Resize, _) ->
    wings_io:putback_event(Resize),
    render_exit();
render_event(quit, _) ->
    wings_io:putback_event(quit),
    render_exit();
render_event(_, _) ->
    render_exit().

render_exit() ->
    wings_wm:dirty(),
    pop.

render_dlist(St) ->
    wings_draw_util:update(fun(D, []) ->
				   render_dlist(D, St)
			   end, []).

render_dlist(eol, _) -> eol;
render_dlist(#dlo{smooth=none,src_we=We}=D, St) ->
    wings_io:disable_progress(),
    {List,Tr} = wings_draw:smooth_dlist(We, St),
    {D#dlo{smooth=List,transparent=Tr},[]};
render_dlist(D, _) -> {D,[]}.

render_redraw(Rr) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:enable(?GL_DEPTH_TEST),
    gl:cullFace(?GL_BACK),
    gl:readBuffer(?GL_BACK),
    jitter_draw(Rr),
    gl:drawBuffer(?GL_BACK),
    gl:popAttrib().

jitter_draw(#r{pass=Pass,acc_size=AccSize}=Rr) ->
    gl:clear(?GL_ACCUM_BUFFER_BIT),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    J = jitter(AccSize),
    jitter_draw_1(J, Pass, W, H, Rr).

jitter_draw_1([{Jx,Jy}|J], Pass, W, H, #r{acc_size=AccSize}=Rr) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    [Fov,Hither,Yon] = wpa:camera_info([fov,hither,yon]),
    accPerspective(Fov, W/H, Hither, Yon, Jx, Jy, 0, 0, 1),
    draw_all(Rr),
    if
	Pass == 1 -> gl:accum(?GL_LOAD, 1/AccSize);
	true ->     gl:accum(?GL_ACCUM, 1/AccSize)
    end,
    if
	Pass < AccSize ->
	    gl:drawBuffer(?GL_FRONT),
	    gl:accum(?GL_RETURN, AccSize/Pass),
	    gl:flush(),
	    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
	    wings_io:ortho_setup(),
	    gl:drawBuffer(?GL_FRONT),
	    gl:disable(?GL_LIGHTING),
	    wings_io:text_at(20, H-20,
			     io_lib:format("Pass ~p of ~p\n", [Pass,AccSize])),
	    gl:flush(),
	    gl:popAttrib(),
	    gl:drawBuffer(?GL_BACK),
	    jitter_draw_1(J, Pass+1, W, H, Rr);
	true ->
	    gl:drawBuffer(?GL_BACK),
	    gl:accum(?GL_RETURN, AccSize/Pass)
    end.

draw_all(Rr) ->
    wings_view:model_transformations(),
    wings_draw_util:fold(fun(D, _) -> render_redraw(D, Rr, false) end, []),
    wings_draw_util:fold(fun(D, _) -> render_redraw(D, Rr, true) end, []).

render_redraw(#dlo{mirror=none}=D, Rr, Flag) ->
    render_redraw_1(D, Rr, Flag);
render_redraw(#dlo{mirror=Matrix}=D, Rr, Flag) ->
    gl:cullFace(?GL_BACK),
    render_redraw_1(D, Rr, Flag),
    gl:cullFace(?GL_FRONT),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_redraw_1(D, Rr, Flag),
    gl:popMatrix(),
    gl:cullFace(?GL_BACK);
render_redraw(_, _, _) -> ok.

render_redraw_1(#dlo{smooth=Dlist,transparent=Trans}, _Rr, RenderTrans) ->
    ?CHECK_ERROR(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:shadeModel(?GL_SMOOTH),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    case Trans of
	true -> gl:disable(?GL_CULL_FACE);
	false -> gl:enable(?GL_CULL_FACE)
    end,
    case {Dlist,RenderTrans} of
	{[Op,_],false} -> gl:callList(Op);
	{[_,Tr],true} -> gl:callList(Tr);
	{Smooth,true} when is_integer(Smooth) -> gl:callList(Smooth);
	{_,_} -> ok
    end,
    ?CHECK_ERROR().

accFrustum(Left, Right, Bottom, Top, ZNear, ZFar,
	   Pixdx, Pixdy, Eyedx, Eyedy, Focus) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),

    Xwsize = Right - Left,
    Ywsize = Top - Bottom,
    Dx = -(Pixdx*Xwsize/W + Eyedx*ZNear/Focus),
    Dy = -(Pixdy*Ywsize/H + Eyedy*ZNear/Focus),
    
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:frustum(Left+Dx, Right+Dx, Bottom+Dy, Top+Dy, ZNear, ZFar),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:translatef(-Eyedx, -Eyedy, 0).

accPerspective(Fovy, Aspect, ZNear, ZFar,
	       Pixdx, Pixdy, Eyedx, Eyedy, Focus) ->
    Fov = ((Fovy*math:pi())/180)/2,
    Top = ZNear / (math:cos(Fov) / math:sin(Fov)),
    Bottom = -Top,
    Right = Top * Aspect,
    Left = -Right,
    accFrustum(Left, Right, Bottom, Top, ZNear, ZFar,
	       Pixdx, Pixdy, Eyedx, Eyedy, Focus).

jitter(1) ->
    [{0.0,0.0}];
jitter(2) ->
    [{0.25,0.75},{0.75,0.25}];
jitter(3) ->
    [{0.5033922635,0.8317967229},
     {0.7806016275,0.2504380877},
     {0.22261828938,0.4131553612}];
jitter(4) ->
    [{0.375,0.25},{0.125,0.75},{0.875,0.25},{0.625,0.75}];
jitter(5) ->
    [{0.375,0.25},{0.125,0.75},{0.875,0.25},{0.625,0.75}];
jitter(6) ->
    [{0.4646464646,0.4646464646},{0.1313131313,0.7979797979},
     {0.5353535353,0.8686868686},{0.8686868686,0.5353535353},
     {0.7979797979,0.1313131313},{0.2020202020,0.2020202020}];
jitter(8) ->
    [{0.5625,0.4375},{0.0625,0.9375},{0.3125,0.6875},{0.6875,0.8125},
     {0.8125,0.1875},{0.9375,0.5625},{0.4375,0.0625},{0.1875,0.3125}];
jitter(9) ->
    [{0.5,0.5},{0.1666666666,0.9444444444},{0.5,0.1666666666},
     {0.5,0.8333333333},{0.1666666666,0.2777777777},
     {0.8333333333,0.3888888888},{0.1666666666,0.6111111111},
     {0.8333333333,0.7222222222},{0.8333333333,0.0555555555}];
jitter(12) ->
    [{0.4166666666,0.625},{0.9166666666,0.875},{0.25,0.375},
     {0.4166666666,0.125},{0.75,0.125},{0.0833333333,0.125},{0.75,0.625},
     {0.25,0.875},{0.5833333333,0.375},{0.9166666666,0.375},
     {0.0833333333,0.625},{0.583333333,0.875}];
jitter(16) ->
    [{0.375,0.4375},{0.625,0.0625},{0.875,0.1875},{0.125,0.0625},
     {0.375,0.6875},{0.875,0.4375},{0.625,0.5625},{0.375,0.9375},
     {0.625,0.3125},{0.125,0.5625},{0.125,0.8125},{0.375,0.1875},
     {0.875,0.9375},{0.875,0.6875},{0.125,0.3125},{0.625,0.8125}].
