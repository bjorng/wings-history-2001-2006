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
%%     $Id: wpc_opengl.erl,v 1.6 2002/07/21 17:34:24 bjorng Exp $

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
    [{"OpenGL",opengl,[option]}] ++ Menu0;
menu(_, Menu) -> Menu.

command({file,{render,{opengl,Ask}}}, St) ->
    do_render(Ask, St);
command(_, _) -> next.

dialog_qs(render) ->
    DefVar = {output_type,get_pref(output_type, preview)},
    Back = get_pref(background_color, {0.4,0.4,0.4}),
    Alpha = get_pref(render_alpha, false),
    [{hframe,
      [{vframe,
	[{key_alt,DefVar,"Preview Window",preview},
	 {key_alt,DefVar,"File",file}],
	[{title,"Output"}]}]},
     aa_frame(),
     {hframe,
      [{label,"Background Color"},{color,Back,[{key,background_color}]}]},
     {"Render Alpha Channel",Alpha,[{key,render_alpha}]}].

aa_frame() ->
    HaveAccum = have_accum(),
    DefVar = {aa,get_pref(aa, if HaveAccum -> regular; true -> draft end)},
    {hframe,
     [{vframe,
       [{key_alt,DefVar,"Draft (no AA)",draft}|
	case have_accum() of
	    false -> [];
	    true ->
		[{key_alt,DefVar,"Regular (normal AA)",regular},
		 {key_alt,DefVar,"Super",super},
		 {key_alt,DefVar,"Premium",premium}]
	end],
       [{title,"Quality"}]}]}.

have_accum() ->
    sdl_video:gl_getAttribute(?SDL_GL_ACCUM_RED_SIZE) >= 8 andalso
	sdl_video:gl_getAttribute(?SDL_GL_ACCUM_GREEN_SIZE) >= 8 andalso
	sdl_video:gl_getAttribute(?SDL_GL_ACCUM_BLUE_SIZE) >= 8.

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

%%%
%%% Rendering.
%%%

-record(r,
	{pass=1,
	 acc_size=6,
	 attr}).

do_render(Ask, St) when is_atom(Ask) ->
    wpa:dialog(Ask, dialog_qs(render), St,
	       fun(Res) ->
		       {file,{render,{opengl,Res}}}
	       end);
do_render(Attr0, St) ->
    set_pref(Attr0),
    case get_filename(Attr0, St) of
	aborted -> keep;
	Attr ->
	    render_dlist(St),
	    wings_wm:dirty(),
	    Aa = property_lists:get_value(aa, Attr),
	    AccSize = translate_aa(Aa),
	    Rr = #r{acc_size=AccSize,attr=Attr},
	    {seq,{push,dummy},get_render_event(Rr)}
    end.

translate_aa(draft) -> 1;
translate_aa(regular) -> 4;
translate_aa(super) -> 8;
translate_aa(premium) -> 16.

get_filename(Attr, St) ->
    case property_lists:get_value(output_type, Attr) of
	preview -> Attr;
	file ->
	    Props = [{ext,".tga"},{ext_desc,"Targa File"}],
	    case wpa:export_filename(Props, St) of
		aborted -> aborted;
		File -> [{output_file,File}|Attr]
	    end
    end.

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
    wings_draw_util:map(fun(D, []) -> D#dlo{smooth=none,smoothed=none} end, []),
    wings_wm:dirty(),
    pop.

render_dlist(St0) ->
    St = invisible_holes(St0),
    wings_draw_util:map(fun(D, []) ->
				render_dlist(D, St)
			end, []).

render_dlist(#dlo{src_we=We}=D, St) ->
    wings_io:disable_progress(),
    {List,Tr} = wings_draw:smooth_dlist(We, St),
    Mask = dlist_mask(We),
    {D#dlo{smooth=List,transparent=Tr,smoothed=Mask},[]}.

dlist_mask(#we{fs=Ftab}=We) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    wings_draw_util:begin_end(
      fun() ->
	      dlist_mask(gb_trees:to_list(Ftab), We)
      end),
    gl:endList(),
    List.

dlist_mask([{Face,#face{edge=Edge}}|Fs], We) ->
    wings_draw_util:flat_face(Face, Edge, We),
    dlist_mask(Fs, We);
dlist_mask([], _We) -> ok.

%% Make the hole material a true hole (entirely invisible).
invisible_holes(#st{mat=Mat}=St) ->
    Hole0 = gb_trees:get('_hole_', Mat),
    OpenGl0 = property_lists:get_value(opengl, Hole0),
    OpenGl = map(fun({Key,{R,G,B,_}}) -> {Key,{R,G,B,0.0}};
		    (Other) -> Other end, OpenGl0),
    Hole = [{opengl,OpenGl}|lists:keydelete(opengl, 1, Hole0)],
    St#st{mat=gb_trees:update('_hole_', Hole, Mat)}.

%%%
%%% Rendering.
%%%

render_redraw(#r{attr=Attr}=Rr) ->
    case property_lists:get_value(output_type, Attr) of
	preview -> render_image(Rr);
	file ->
	    render_to_file(Rr),
	    wings_io:putback_event(time_to_quit)
    end.

render_to_file(#r{attr=Attr}=Rr) ->
    render_image(Rr#r{attr=[{render_alpha,true}|Attr]}),
    MaskImage = capture(1, ?GL_RED),
    render_image(Rr#r{attr=[{render_alpha,false}|Attr]}),
    ObjectImage = capture(3, ?GL_RGB),
    Image = combine_images(ObjectImage, MaskImage),
    RendFile = property_lists:get_value(output_file, Attr),
    ok = e3d_image:save(Image, RendFile).

render_image(#r{attr=Attr}=Rr) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    case property_lists:get_bool(render_alpha, Attr) of
	false ->
	    {R,G,B} = property_lists:get_value(background_color, Attr),
	    gl:clearColor(R, G, B, 1);
	true ->
	    gl:clearColor(0, 0, 0, 1),
	    gl:color3f(1, 1, 1)
    end,
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

render_redraw_1(Dl, #r{attr=Attr}, RenderTrans) ->
    case property_lists:get_bool(render_alpha, Attr) of
	false -> render_redraw_2(Dl, RenderTrans);
	true -> render_mask(Dl)
    end.
	    
render_redraw_2(#dlo{smooth=Dlist,transparent=Trans}, RenderTrans) ->
    ?CHECK_ERROR(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:shadeModel(?GL_SMOOTH),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:enable(?GL_CULL_FACE),

    case RenderTrans of
	true ->
	    %% Transparent materials should not update the depth buffer.
	    gl:enable(?GL_BLEND),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:depthMask(?GL_FALSE);
	false ->
	    gl:disable(?GL_BLEND),
	    gl:depthMask(?GL_TRUE)
    end,

    %% Backsides of opaque objects should be drawn if the object has any transparency.
    case Trans andalso not RenderTrans of
	true -> gl:disable(?GL_CULL_FACE);
	false -> gl:enable(?GL_CULL_FACE)
    end,

    case {Dlist,RenderTrans} of
	{[Op,_],false} -> gl:callList(Op);
	{[_,Tr],true} -> gl:callList(Tr);
	{Smooth,true} when is_integer(Smooth) -> gl:callList(Smooth);
	{_,_} -> ok
    end,
    gl:depthMask(?GL_TRUE),
    ?CHECK_ERROR().

render_mask(#dlo{smoothed=Dlist,transparent=Trans}) ->
    ?CHECK_ERROR(),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHTING),
    gl:enable(?GL_CULL_FACE),
    gl:callList(Dlist),
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

%%%
%%% Capture generated image.
%%%

capture(N, Type) ->
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
    gl:readBuffer(?GL_BACK),
    [X,Y,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    NumBytes = N*W*H,
    Mem = sdl_util:malloc(NumBytes, ?GL_UNSIGNED_BYTE),
    gl:readPixels(X, Y, W, H, Type, ?GL_UNSIGNED_BYTE, Mem),
    Pixels = sdl_util:readBin(Mem, NumBytes),
    sdl_util:free(Mem),
    #e3d_image{bytes_pp=N,order=lower_left,width=W,height=H,image=Pixels}.

combine_images(#e3d_image{image=Pixels0}=Image, #e3d_image{image=Mask}) ->
    Pixels = combine_img_1(Pixels0, Mask, []),
    Image#e3d_image{type=b8g8r8a8,bytes_pp=4,image=Pixels}.

combine_img_1(<<RGB:768/binary,P/binary>>, <<A:256/binary,M/binary>>, Acc) ->
    Chunk = combine_img_2(binary_to_list(RGB), binary_to_list(A), []),
    combine_img_1(P, M, [Chunk|Acc]);
combine_img_1(<<R:8,G:8,B:8,P/binary>>, <<A:8,M/binary>>, Acc) ->
    combine_img_1(P, M, [<<B:8,G:8,R:8,A:8>>|Acc]);
combine_img_1(<<>>, <<>>, Acc) ->
    list_to_binary(reverse(Acc)).

combine_img_2([R,G,B|P], [A|M], Acc) ->
    combine_img_2(P, M, [[B,G,R,A]|Acc]);
combine_img_2([], [], Acc) -> list_to_binary(reverse(Acc)).
