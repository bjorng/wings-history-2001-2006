%%
%%  wpc_opengl.erl --
%%
%%     OpenGL renderer.
%%
%%  Copyright (c) 2002-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_opengl.erl,v 1.27 2003/04/21 10:16:55 bjorng Exp $

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
    DefOutput = get_pref(output_type, preview),
    SubDiv = get_pref(subdivisions, 0),
    Back = get_pref(background_color, {0.4,0.4,0.4}),
    Alpha = get_pref(render_alpha, false),
    [{menu,[{"Render to Image Window",preview},
	    {"Render to File",file}],
      DefOutput,[{key,output_type}]},
     aa_frame(),
     {hframe,
      [{label,"Sub-division Steps"},{text,SubDiv,[{key,subdivisions},
						  {range,1,4}]}]},
     {hframe,
      [{label,"Background Color"},{color,Back,[{key,background_color}]}]},
     {"Render Alpha Channel",Alpha,[{key,render_alpha}]}].

aa_frame() ->
    HaveAccum = have_accum(),
    Def0 = get_pref(aa, if HaveAccum -> regular; true -> draft end),
    Def = case HaveAccum of
	      false -> draft;
	      true -> Def0
	  end,
    {hframe,
     [{menu,
       [{"Draft Quality (no AA)",draft}|
	case have_accum() of
	    false -> [];
	    true ->
		[{"Regular Quality (normal AA)",regular},
		 {"Super Quality",super},
		 {"Premium Quality",premium}]
	end],Def,[{key,aa}]}]}.

have_accum() ->
    [R] = gl:getIntegerv(?GL_ACCUM_RED_BITS),
    [G] = gl:getIntegerv(?GL_ACCUM_GREEN_BITS),
    [B] = gl:getIntegerv(?GL_ACCUM_BLUE_BITS),
    R >= 16 andalso G >= 16 andalso B >= 16.

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

%%%
%%% Rendering.
%%%

-record(r,
	{acc_size=6,
	 attr
	}).

do_render(Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "Render Options",
	       dialog_qs(render),
	       fun(Res) ->
		       {file,{render,{opengl,Res}}}
	       end);
do_render(Attr0, St) ->
    set_pref(Attr0),
    case get_filename(Attr0, St) of
	aborted -> keep;
	Attr ->
	    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
	    gl:drawBuffer(?GL_FRONT),
	    gl:clearColor(0.5, 0.5, 0.5, 1),
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    wings_io:ortho_setup(),
	    {_,H} = wings_wm:win_size(),
	    wings_io:text_at(10, H-20, "Rendering..."),
	    gl:drawBuffer(?GL_BACK),
	    gl:popAttrib(),
	    gl:flush(),

	    render_dlist(St, Attr),
	    Aa = proplists:get_value(aa, Attr),
	    AccSize = translate_aa(Aa),
	    Rr = #r{acc_size=AccSize,attr=Attr},
	    render_redraw(Rr),
	    render_exit()
    end.

translate_aa(draft) -> 1;
translate_aa(regular) -> 4;
translate_aa(super) -> 8;
translate_aa(premium) -> 16.

get_filename(Attr, St) ->
    case proplists:get_value(output_type, Attr) of
	preview -> Attr;
	file ->
	    Props = [{ext,".tga"},{ext_desc,"Targa File"}],
	    case wpa:export_filename(Props, St) of
		aborted -> aborted;
		File -> [{output_file,File}|Attr]
	    end
    end.

render_exit() ->
    gl:getError(),
    wings_draw_util:map(fun(D, []) ->
				D#dlo{smooth=none,smoothed=none}
			end, []),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings_wm:dirty().

render_dlist(St0, Attr) ->
    St = invisible_holes(St0),
    SubDiv = proplists:get_value(subdivisions, Attr),
    RenderAlpha = proplists:get_bool(render_alpha, Attr),
    wings_draw_util:map(fun(D, []) ->
				render_dlist(D, St, SubDiv, RenderAlpha)
			end, []).

render_dlist(#dlo{src_we=We0}=D, St, SubDiv, RenderAlpha) ->
    We = sub_divide(SubDiv, We0),
    {List,Tr} = wings_draw:smooth_dlist(We, St),
    Mask = case RenderAlpha of
	       true -> dlist_mask(We);
	       false -> none
	   end,
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

dlist_mask([{Face,Edge}|Fs], We) ->
    wings_draw_util:flat_face(Face, Edge, We),
    dlist_mask(Fs, We);
dlist_mask([], _We) -> ok.

%% Make the hole material a true hole (entirely invisible).
invisible_holes(#st{mat=Mat}=St) ->
    Hole0 = gb_trees:get('_hole_', Mat),
    OpenGl0 = proplists:get_value(opengl, Hole0),
    OpenGl = map(fun({Key,{R,G,B,_}}) -> {Key,{R,G,B,0.0}};
		    (Other) -> Other end, OpenGl0),
    Hole = [{opengl,OpenGl}|lists:keydelete(opengl, 1, Hole0)],
    St#st{mat=gb_trees:update('_hole_', Hole, Mat)}.

sub_divide(0, We) -> We;
sub_divide(N, We) -> sub_divide(N-1, wings_subdiv:smooth(We)).
    
%%%
%%% Rendering.
%%%

render_redraw(#r{attr=Attr}=Rr) ->
    render_one(Rr, false),
    ObjectImage = capture(3, ?GL_RGB),
    Image = case proplists:get_bool(render_alpha, Attr) of
		true ->
		    render_one(Rr, true),
		    MaskImage = capture(1, ?GL_RED),
		    combine_images(ObjectImage, MaskImage);
		false -> ObjectImage
	    end,
    case proplists:get_value(output_type, Attr) of
	preview ->
	    Id = wings_image:new("Rendered", Image),
	    wings_image:window(Id);
	file ->
	    RendFile = proplists:get_value(output_file, Attr),
	    ok = e3d_image:save(Image, RendFile)
    end.

render_one(#r{attr=Attr0}=Rr0, RenderAlpha) ->
    Attr = [{render_alpha,RenderAlpha}|Attr0],
    render_image(Rr0#r{attr=Attr}).

render_image(#r{acc_size=AccSize,attr=Attr}=Rr) ->
    gl:clear(?GL_ACCUM_BUFFER_BIT),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    case proplists:get_bool(render_alpha, Attr) of
 	false ->
 	    {R,G,B} = proplists:get_value(background_color, Attr),
 	    gl:clearColor(R, G, B, 1);
 	true ->
 	    gl:clearColor(0, 0, 0, 1),
 	    gl:color3f(1, 1, 1)
    end,
    J = jitter(AccSize),
    jitter_draw(J, ?GL_LOAD, Rr),
    gl:accum(?GL_RETURN, 1.0),
    gl:popAttrib().

jitter_draw([{Jx,Jy}|J], Op, #r{acc_size=AccSize}=Rr) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:enable(?GL_DEPTH_TEST),
    gl:frontFace(?GL_CCW),
    {_,_,W,H} = wings_wm:viewport(),
    [Fov,Hither,Yon] = wpa:camera_info([fov,hither,yon]),
    accPerspective(Fov, W/H, Hither, Yon, Jx, Jy, 0, 0, 1),
    draw_all(Rr),
    gl:accum(Op, 1/AccSize),
    jitter_draw(J, ?GL_ACCUM, Rr);
jitter_draw([], _, _) -> ok.

draw_all(Rr) ->
    wings_view:model_transformations(true),
    wings_draw_util:fold(fun(D, _) -> render_redraw(D, Rr, false) end, []),
    wings_draw_util:fold(fun(D, _) -> render_redraw(D, Rr, true) end, []).

render_redraw(#dlo{mirror=none}=D, Rr, Flag) ->
    render_redraw_1(D, Rr, Flag);
render_redraw(#dlo{mirror=Matrix}=D, Rr, Flag) ->
    gl:frontFace(?GL_CCW),
    render_redraw_1(D, Rr, Flag),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    render_redraw_1(D, Rr, Flag),
    gl:popMatrix(),
    gl:frontFace(?GL_CCW);
render_redraw(_, _, _) -> ok.

render_redraw_1(Dl, #r{attr=Attr}, RenderTrans) ->
    case proplists:get_bool(render_alpha, Attr) of
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

    case Trans of
	false -> gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_FALSE);
	true -> gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_TRUE)
    end,

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

    %% Backsides of opaque objects should be drawn
    %% if the object has any transparency.
    case Trans andalso not RenderTrans of
	true -> gl:disable(?GL_CULL_FACE);
	false -> gl:enable(?GL_CULL_FACE)
    end,

    case {Dlist,RenderTrans} of
	{[Op,_],false} -> gl:callList(Op);
	{[_,Tr],true} -> gl:callList(Tr);
	{_,_} -> ok
    end,
    gl:depthMask(?GL_TRUE),
    ?CHECK_ERROR().

render_mask(#dlo{smoothed=Dlist}) ->
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
    {_,_,W,H} = wings_wm:viewport(),

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
    {X,Y,W,H} = wings_wm:viewport(),
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
