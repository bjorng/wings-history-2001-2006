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
%%     $Id: wpc_opengl.erl,v 1.35 2003/08/21 08:15:23 dgud Exp $

-module(wpc_opengl).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3,map/2,foreach/2,reverse/1,seq/2,
		flat_length/1,append/1,append/2]).


%% 
-ifndef(GL_DEPTH_CLAMP_NV).
-define(GL_DEPTH_CLAMP_NV,      16#864F).
-endif.

-define(STENCIL_INIT, 128).

-record(r,
	{acc_size=6,
	 attr,
	 mat,
	 data,
	 lights,
	 mask,
	 shadow
	}).

-record(d, {s,  % smooth display list
	    f,  % fast display list
	    tr, % transparent bool
	    trl, % transp list
	    we}).

-record(l, {pos,    % Position
	    l,      % Light record
	    s_dl=[]}). % List of display lists of shadow volumes

%% Light record in We.
-record(light,
	{type,
	 diffuse={1.0,1.0,1.0,1.0},
	 ambient={0.0,0.0,0.0,1.0},
	 specular={1.0,1.0,1.0,1.0},
	 aim, %Aim point for spot/infinite.
	 lin_att, %Linear attenuation.
	 quad_att, %Quadratic attenuation.
	 spot_angle,
	 spot_exp, %Spot exponent.
	 prop=[] %Extra properties.
	}).

init() ->
    true.

menu({file,render}, Menu0) ->
    [{"Opengl",opengl,[option]}] ++ Menu0;
menu(_, Menu) -> Menu.

command({file,{render,{opengl,Ask}}}, St) ->
    do_render(Ask, St);
command(_, _) -> next.

dialog_qs(render) ->
    DefOutput = get_pref(output_type, preview),
    SubDiv = get_pref(subdivisions, 0),
    Back = get_pref(background_color, {0.4,0.4,0.4}),
    Alpha = get_pref(render_alpha, false),
    Shadow = get_pref(render_shadow, false),
    [{menu,[{"Render to Image Window",preview},
	    {"Render to File",file}],
      DefOutput,[{key,output_type}]},
     aa_frame(),
     {hframe,
      [{label,"Sub-division Steps"},{text,SubDiv,[{key,subdivisions},
						  {range,1,4}]}]},
     {hframe,
      [{label,"Background Color"},{color,Back,[{key,background_color}]}]},
     {"Render Alpha Channel",Alpha,[{key,render_alpha}]},
     {"Render Shadows (test)",Shadow,[{key,render_shadow}]}].

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

	    Aa = proplists:get_value(aa, Attr),
	    AccSize = translate_aa(Aa),
	    RenderAlpha = proplists:get_bool(render_alpha, Attr),
	    RenderShadow = proplists:get_bool(render_shadow, Attr),

	    %% BUGBUG check stencil bufffer also..

	    {Data,Lights} = ?TC(update_st(St, Attr, RenderShadow)),

	    Rr = #r{acc_size=AccSize,attr=Attr,
		    data=Data,lights=Lights,mat=St#st.mat,
		    mask = RenderAlpha,shadow = RenderShadow},
	    render_redraw(Rr),
	    render_exit(Rr)
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

render_exit(#r{lights={Lights,_}, data=D}) ->
    gl:getError(),
    foreach(fun(#l{s_dl=L}) ->
		    foreach(fun(DL) -> gl:deleteLists(DL,1) end,L)
	    end, Lights),
    foreach(fun(#d{s=S,f=F,trl=Tr}) ->
		    foreach(fun(DL) when integer(DL) -> 
				    gl:deleteLists(DL,1);
			    (_) -> ok
			    end,[S,F,Tr])
	    end, D),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings_wm:dirty().

update_st(St0, Attr, Shadows) ->
    St = invisible_holes(St0),
    SubDiv = proplists:get_value(subdivisions, Attr),
    {Ds, {Ls0, Amb}} = 
	foldl(fun(We, {Wes,Lights}) ->
		      update_st(We, SubDiv, Wes, St, Lights)
	      end, {[],{[],none}}, gb_trees:values(St#st.shapes)),

    CLDL = 
	fun(L) ->
		SDL = 
		    lists:map(fun(D) ->
				      List = gl:genLists(1),
				      gl:newList(List, ?GL_COMPILE),    
				      create_shadow_volume(L, D),
				      gl:endList(),
				      List
			      end, Ds),
		L#l{s_dl = SDL}
	end,
    ?CHECK_ERROR(),
    Ls = 
	case Shadows of
	    true -> 
		lists:map(CLDL, Ls0);
	    false ->
		Ls0
	end,
    ?CHECK_ERROR(),
    {Ds, {Ls, Amb}}.

update_st(We0, SubDiv, Wes, St,AL={Lights,Amb}) ->
    case We0#we.light of
	none ->
	    We = case SubDiv of
		     0 ->
			 %% If no sub-divisions requested, it is safe to
			 %% first triangulate and then freeze any mirror.
			 We1 = wpa:triangulate(We0),
			 wpa:vm_freeze(We1);
		     _ ->
			 %% Otherwise, we must do it in this order
			 %% (slower if there is a virtual mirror).
			 We1 = wpa:vm_freeze(We0),
			 We2 = sub_divide(SubDiv, We1),
			 wpa:triangulate(We2)
		 end,
	    Fast = dlist_mask(We),
	    {[Smooth,TrL],Tr} = wings_draw:smooth_dlist(We, St),
	    {[#d{s=Smooth,f=Fast,we=We,tr=Tr,trl=TrL}|Wes],AL};
	L = #light{type=ambient} ->
	    {Wes,{Lights,L}};
	L = #light{} ->
	    Pos = light_pos(We0),
	    {Wes,{[#l{pos=Pos,l=L}|Lights],Amb}}
    end.

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
    wings_draw_util:unlit_tri(Face, Edge, We),
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
    render_image(Rr, false),
    ObjectImage = capture(3, ?GL_RGB),
    Image = case proplists:get_bool(render_alpha, Attr) of
		true ->
		    render_image(Rr, true),
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

render_image(#r{acc_size=AccSize,attr=Attr}=Rr, RendMask) ->
    gl:clear(?GL_ACCUM_BUFFER_BIT),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    case RendMask of
	false ->
	    {R,G,B} = proplists:get_value(background_color, Attr),
	    gl:clearColor(R, G, B, 1);
	true ->
	    gl:clearColor(0, 0, 0, 1),
	    gl:color3f(1, 1, 1)
    end,
    J = jitter(AccSize),
    jitter_draw(J, ?GL_LOAD, Rr, RendMask),
    gl:accum(?GL_RETURN, 1.0),
    gl:popAttrib().

jitter_draw([{Jx,Jy}|J], Op, #r{acc_size=AccSize}=Rr, RendMask) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:enable(?GL_DEPTH_TEST),
    gl:frontFace(?GL_CCW),
    {_,_,W,H} = wings_wm:viewport(),
    [Fov,Hither,Yon] = wpa:camera_info([fov,hither,yon]),
    accPerspective(Fov, W/H, Hither, Yon, Jx, Jy, 0, 0, 1),
    draw_all(Rr, RendMask),
    gl:accum(Op, 1/AccSize),
    jitter_draw(J, ?GL_ACCUM, Rr, RendMask);
jitter_draw([], _, _,_) -> ok.

draw_all(#r{data=Wes,lights={Ligths,Amb},mat=Mat,shadow=true},false) ->
    wings_view:modelview(false),
    case wings_util:is_gl_ext('GL_NV_depth_clamp') of
	true -> gl:enable(?GL_DEPTH_CLAMP_NV);
	false -> setup_projection_matrix()
    end,
    ?CHECK_ERROR(),
    %% Carmack's reversed shadow algorithm
    gl:clearStencil(?STENCIL_INIT),
    disable_lights(),
    gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_FALSE),    
    gl:clear(?GL_COLOR_BUFFER_BIT bor 
	     ?GL_DEPTH_BUFFER_BIT bor 
	     ?GL_STENCIL_BUFFER_BIT),

    gl:disable(?GL_ALPHA_TEST),

    gl:frontFace(?GL_CCW),
    gl:shadeModel(?GL_SMOOTH),
    gl:enable(?GL_CULL_FACE),

    gl:enable(?GL_DEPTH_TEST),
    gl:depthMask(?GL_TRUE),
    gl:depthFunc(?GL_LESS),

    ?CHECK_ERROR(),
    %% Set the depth-buffer and ambient colors

    setup_light(Amb,0),
    gl:enable(?GL_LIGHTING),
    foreach(fun(#d{s=DL}) ->         
		    gl:callList(DL) end, Wes),
    disable_lights(),
    gl:disable(?GL_LIGHTING),
    gl:enable(?GL_BLEND),
    %% Blend in the other lights
    Count = 
	case wings_util:is_gl_ext('GL_ARB_imaging') of
	    true -> 
		gl:blendFunc(?GL_CONSTANT_COLOR, ?GL_ONE_MINUS_CONSTANT_COLOR),
		C = case Amb of
			none -> 1;
			_ -> 2
		    end, 
		PerLight = 1/C,
		gl:blendColor(PerLight,PerLight,PerLight,PerLight),
		C;
	    false ->
		gl:blendFunc(?GL_ONE, ?GL_ONE),
		0
	end,
    %% No more depth writes...
    gl:depthMask(?GL_FALSE),
    gl:enable(?GL_STENCIL_TEST),
    foldl(fun(L,Clear) -> draw_with_shadows(Clear, L, Wes, Mat) end, 
	  {false, Count}, Ligths),
    gl:disable(?GL_STENCIL_TEST),
    gl:depthMask(?GL_TRUE),
    ok;

draw_all(RR, RMask) ->
    %% No shadows variant
    wings_view:modelview(true),
    foreach(fun(Data) -> render_redraw(Data, RMask, false) end, 
	    RR#r.data),
    foreach(fun(Data) -> render_redraw(Data, RMask, true) end, 
	    RR#r.data).

draw_with_shadows({true,LNo0}, L, Wes, _Mats) ->
    %% New light, clear and update stencil
    gl:clear(?GL_STENCIL_BUFFER_BIT),
    LNo = if LNo0 > 0 -> %% Blend in the light with less power every loop
		  C = LNo0+1,
		  PerLight = 1/C,
		  gl:blendColor(PerLight,PerLight,PerLight,PerLight),
		  C;
	     true ->  %% ARB_imaging not available  
		  0
	  end,
    draw_with_shadows({false,LNo}, L, Wes, _Mats);
draw_with_shadows({false,LNo}, #l{pos=LPos,l=LRec,s_dl=Shadow}, Wes, _Mats) ->
    gl:colorMask(?GL_FALSE,?GL_FALSE,?GL_FALSE,?GL_FALSE),

    gl:stencilFunc(?GL_ALWAYS, ?STENCIL_INIT, 16#FFFFFFFF),
    gl:depthFunc(?GL_LESS),
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_FRONT),
    gl:stencilOp(?GL_KEEP, ?GL_INCR, ?GL_KEEP),    
    foreach(fun(DL) -> gl:callList(DL) end, Shadow),
    gl:cullFace(?GL_BACK),
    gl:stencilOp(?GL_KEEP, ?GL_DECR, ?GL_KEEP),
    foreach(fun(DL) -> gl:callList(DL) end, Shadow),

    gl:stencilFunc(?GL_EQUAL, ?STENCIL_INIT, 16#FFFFFFFF),
    gl:stencilOp(?GL_KEEP,?GL_KEEP,?GL_INCR),
    gl:colorMask(?GL_TRUE,?GL_TRUE,?GL_TRUE,?GL_TRUE),
    gl:depthFunc(?GL_EQUAL),
    setup_light(LRec,LPos),
    gl:enable(?GL_LIGHTING),
    foreach(fun(#d{s=DL}) -> gl:callList(DL) end, Wes),
    gl:disable(?GL_LIGHTING),
    disable_lights(),
    gl:disable(?GL_CULL_FACE),
    gl:depthFunc(?GL_LESS),
%    debug_shad(Shadow),
    {true,LNo}.


%%%%%%%%%%%%%%%%%%%%
-ifdef(DEBUG).
debug_shad(Lists) ->
    %% DEBUG draw shadows...
    gl:stencilFunc(?GL_ALWAYS, ?STENCIL_INIT, 16#FFFFFFFF),
    gl:stencilOp(?GL_KEEP,?GL_KEEP,?GL_KEEP),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_SRC_ALPHA),
    gl:color4f(0.0,0.0,1.0,0.5),
    gl:shadeModel(?GL_FLAT),
    %%    gl:disable(?GL_BLEND),
    gl:depthFunc(?GL_GEQUAL),

    %% Toggle DEPTH TEST to see shadows
%%    gl:disable(?GL_DEPTH_TEST),  
    %%    gl:disable(?GL_STENCIL_TEST),
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_FRONT),
    foreach(fun(DL) -> gl:callList(DL) end, Lists),
    gl:color4f(1.0,0.0,0.0,0.5), 
    gl:cullFace(?GL_BACK),
    foreach(fun(DL) -> gl:callList(DL) end, Lists),
    gl:disable(?GL_CULL_FACE),
    gl:polygonMode(?GL_FRONT_AND_BACK,?GL_FILL),
    true.
-endif.

create_shadow_volume(#l{pos=LPos,l=#light{type=infinite,aim=Aim}}, 
		     #d{we= We = #we{fs=FTab}}) ->
    LightDir = e3d_vec:norm(e3d_vec:sub(Aim, LPos)),
    {FF,_BF,Loops} = partition_model(We, LightDir, dir),
    %% Draw shadow walls.
    foreach(fun(Vs) -> build_shadow_edge_ext_infinite(Vs,LightDir,We) end, Loops),    
    %% Draw Top Cap
    wings_draw_util:begin_end(
      fun() -> foreach(fun(Face) -> 
			       Edge = gb_trees:get(Face, FTab),
			       wings_draw_util:unlit_tri(Face, Edge, We)
		       end, FF) 
      end);
create_shadow_volume(#l{pos=LPos},#d{we= We = #we{fs=FTab}}) ->
    {FF,BF,Loops} = partition_model(We, LPos, pos),
    %% Draw walls
    foreach(fun(Vs) -> build_shadow_edge_ext(Vs,LPos,We) end, Loops),
    %% Draw Top Cap
    wings_draw_util:begin_end(
      fun() -> foreach(fun(Face) -> 
			       Edge = gb_trees:get(Face, FTab),
			       wings_draw_util:unlit_tri(Face, Edge, We)
		       end, FF) 
      end),
    %% Draw bottom cap
    gl:'begin'(?GL_TRIANGLES),
    foreach(fun(Face) -> 
		    Vs = wings_face:vertices_ccw(Face, We),
		    draw_bottom_face(Vs,LPos,We#we.vp) 
	    end, BF),
    gl:'end'().

render_redraw(D, RMask, RenderTrans) ->
    case RMask of
	false -> render_redraw_1(D, RenderTrans);
	true -> render_mask(D)
    end.

render_redraw_1(#d{s=Dlist,tr=Trans,trl=TRL}, RenderTrans) ->
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

    case RenderTrans of
	false -> wings_draw_util:call(Dlist);
	true ->  wings_draw_util:call(TRL)
    end,
    gl:depthMask(?GL_TRUE),
    ?CHECK_ERROR().

render_mask(#d{f=Dlist}) ->
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
    Mem = sdl_util:alloc(NumBytes, ?GL_UNSIGNED_BYTE),
    gl:readPixels(X, Y, W, H, Type, ?GL_UNSIGNED_BYTE, Mem),
    Pixels = sdl_util:getBin(Mem),
    #e3d_image{bytes_pp=N,order=lower_left,width=W,height=H,image=Pixels}.

combine_images(#e3d_image{image=Pixels0}=Image, #e3d_image{image=Mask}) ->
    Pixels = combine_img_1(Pixels0, Mask, []),
    Image#e3d_image{type=b8g8r8a8,bytes_pp=4,image=Pixels}.

combine_img_1(<<RGB:768/binary,P/binary>>, <<A:256/binary,M/binary>>, 
	      Acc) ->
    Chunk = combine_img_2(binary_to_list(RGB), binary_to_list(A), []),
    combine_img_1(P, M, [Chunk|Acc]);
combine_img_1(<<R:8,G:8,B:8,P/binary>>, <<A:8,M/binary>>, Acc) ->
    combine_img_1(P, M, [<<B:8,G:8,R:8,A:8>>|Acc]);
combine_img_1(<<>>, <<>>, Acc) ->
    list_to_binary(reverse(Acc)).

combine_img_2([R,G,B|P], [A|M], Acc) ->
    combine_img_2(P, M, [[B,G,R,A]|Acc]);
combine_img_2([], [], Acc) -> list_to_binary(reverse(Acc)).

%% Utilities
setup_projection_matrix() ->
    #view{hither=Near} = wings_view:current(),
    [ M0, M4, M8, M12,
      M1, M5, M9, M13,
      M2, M6, _M10,_M14,
      M3, M7, _M11,M15] =
	gl:getDoublev(?GL_PROJECTION_MATRIX),
    %% Patch to get infinitive far plane
    Infinite = [ M0, M4, M8,  M12,
		 M1, M5, M9,  M13,
		 M2, M6, -1.0,-1.0,
		 M3, M7, -2.0*Near ,M15],
    gl:matrixMode(?GL_PROJECTION),
    gl:loadMatrixd(Infinite),
    gl:matrixMode(?GL_MODELVIEW).

%% Returns {FacesFacingLigth,FacesAwayFromLigth,CW_VsLoops}
partition_model(We=#we{fs=FTab,vp=Vtab}, LPos, DirOrPos) ->
    Sort = 
	fun(Face, {FF,BF,EL}) ->
		{Vs,Es} = 
		    wings_face:fold(fun(V,Edge,_E,{Vs,Es}) ->
					    {[gb_trees:get(V, Vtab)|Vs],
					     [{Edge,Face}|Es]}
				    end,{[],EL},Face,We),
		LDir = case DirOrPos of
			   dir ->
			       LPos;
			   _ ->
			       Aver = e3d_vec:average(Vs),
			       e3d_vec:sub(Aver,LPos)
		       end,
		Normal = e3d_vec:normal(Vs),
		case e3d_vec:dot(Normal, LDir) >= 0 of
		    true -> %% BackFacing
			{FF,[Face|BF],EL};
		    false -> %% FrontFacing
			{[Face|FF],BF,Es}
		end
	end,
    {FF,BF,EL} = foldl(Sort, {[],[],[]}, gb_trees:keys(FTab)),
    Eds   = outer_edges_1(lists:sort(EL), []),   
    Loops = case wings_edge_loop:edge_loop_vertices(Eds,We) of
		none -> [];
		Ls -> 
		    %% Fix cw order
		    [fix_cw_order(Loop, FF, We) || Loop <- Ls]
	    end,
    {FF,BF,Loops}.
outer_edges_1([{E,_},{E,_}|T], Out) ->
    outer_edges_1(T, Out);
outer_edges_1([{E,_F}|T], Out) -> %% Only the edges I'm interested of
    outer_edges_1(T, [E|Out]);
outer_edges_1([], Out) -> reverse(Out).

fix_cw_order(Vs = [V1,V2|_], FF, We = #we{es=Etab}) ->
    Eds = wings_edge:from_vs([V1,V2],We),
    E = get_edge(lists:sort(Eds)),
    case gb_trees:get(E,Etab) of
	#edge{vs=V1,ve=V2,lf=Face} -> 
	    case lists:member(Face, FF) of
		false ->
		    Vs;
		true ->
		    lists:reverse(Vs)
	    end;
	#edge{vs=V2,ve=V1, lf=Face} ->
	    case lists:member(Face, FF) of
		false ->
		    lists:reverse(Vs);
		true ->
		    Vs
	    end
    end.

get_edge([A,A|_]) -> A;
get_edge([_|R]) -> get_edge(R).

%%%%%%%%% Shadow Building functions %%%% 
%% Edge loop is input, see wings_edge_loop:edge_loop_vertices/2
build_shadow_edge_ext_infinite([V0|_] = Vs, {X,Y,Z}, #we{vp=Vtab}) ->
    gl:'begin'(?GL_TRIANGLE_FAN),
    gl:vertex4f(X,Y,Z,0.0),
    foreach(fun(V) -> gl:vertex3fv(gb_trees:get(V, Vtab)) end, Vs),
    gl:vertex3fv(gb_trees:get(V0, Vtab)),
    gl:'end'().

build_shadow_edge_ext(Vs0, L, #we{vp=VP}) ->
    Vs = [V0|_] = lists:reverse(Vs0),
    gl:'begin'(?GL_QUAD_STRIP),
    build_shadow_edge_ext(Vs, V0, L, VP).
build_shadow_edge_ext([V|Vs], V0, L, Vtab) ->
    Vp = gb_trees:get(V, Vtab),
    gl:vertex3fv(Vp),
    {X,Y,Z} = e3d_vec:sub(Vp,L),
    gl:vertex4f(X,Y,Z,0.0),
    build_shadow_edge_ext(Vs,V0,L,Vtab);
build_shadow_edge_ext([], V, L, Vtab) ->
    Vp = gb_trees:get(V, Vtab),
    gl:vertex3fv(Vp),
    {X,Y,Z} = e3d_vec:sub(Vp,L),
    gl:vertex4f(X,Y,Z,0.0),
    gl:'end'().

draw_bottom_face([V1,V2,V3],LPos,Vtab) ->
    draw_bottom_face(gb_trees:get(V1,Vtab),LPos),
    draw_bottom_face(gb_trees:get(V2,Vtab),LPos),
    draw_bottom_face(gb_trees:get(V3,Vtab),LPos).

draw_bottom_face(V,L) ->
    {X,Y,Z} = e3d_vec:sub(V,L),
    gl:vertex4f(X,Y,Z,0.0).

%%% Lights 

light_pos(#we{vp=Vtab}) ->
    gb_trees:get(1, Vtab);
light_pos(Pos) ->
    Pos.

disable_lights() ->
    disable_lights(?GL_LIGHT0).
disable_lights(Lnum) when Lnum > ?GL_LIGHT7 -> 
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.0,0.0,0.0,1.0}),
    ok;
disable_lights(Lnum) ->
    gl:disable(Lnum),
    disable_lights(Lnum+1).

setup_light(none,_We) ->
    ok;
setup_light(#light{}=L,We) ->
    setup_light(?GL_LIGHT0, L,We).    
setup_light(_Lnum, #light{type=ambient,ambient=Amb}, _We) ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, Amb);
setup_light(Lnum, #light{type=infinite,aim=Aim}=L, We) ->
    {X,Y,Z} = e3d_vec:norm(e3d_vec:sub(light_pos(We), Aim)),
    gl:lightfv(Lnum, ?GL_POSITION, {X,Y,Z,0}),
    gl:enable(Lnum),
    setup_color(Lnum, L);
setup_light(Lnum, #light{type=point}=L, We) ->
    {X,Y,Z} = light_pos(We),
    gl:lightfv(Lnum, ?GL_POSITION, {X,Y,Z,1}),
    gl:lightf(Lnum, ?GL_SPOT_CUTOFF, 180.0),
    gl:enable(Lnum),
    setup_color(Lnum, L),
    setup_attenuation(Lnum, L);
setup_light(Lnum, #light{type=spot,aim=Aim,spot_angle=Angle,spot_exp=Exp}=L, We) ->
    Pos = {X,Y,Z} = light_pos(We),
    Dir = e3d_vec:norm(e3d_vec:sub(Aim, Pos)),
    gl:lightfv(Lnum, ?GL_POSITION, {X,Y,Z,1}),
    gl:lightf(Lnum, ?GL_SPOT_CUTOFF, Angle),
    gl:lightf(Lnum, ?GL_SPOT_EXPONENT, Exp),
    gl:lightfv(Lnum, ?GL_SPOT_DIRECTION, Dir),
    gl:enable(Lnum),
    setup_color(Lnum, L),
    setup_attenuation(Lnum, L).

setup_color(Lnum, #light{diffuse=Diff,ambient=Amb,specular=Spec}) ->
    gl:lightfv(Lnum, ?GL_DIFFUSE, Diff),
    gl:lightfv(Lnum, ?GL_AMBIENT, Amb),
    gl:lightfv(Lnum, ?GL_SPECULAR, Spec).

setup_attenuation(Lnum, #light{lin_att=Lin,quad_att=Quad}) ->
    gl:lightf(Lnum, ?GL_LINEAR_ATTENUATION, Lin),
    gl:lightf(Lnum, ?GL_QUADRATIC_ATTENUATION, Quad).

