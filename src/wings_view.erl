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
%%     $Id: wings_view.erl,v 1.72 2002/08/01 20:13:36 bjorng Exp $
%%

-module(wings_view).
-export([menu/3,command/2,
	 virtual_mirror/2,
	 init/0,init_light/0,
	 current/0,set_current/1,
	 projection/0,perspective/0,
	 model_transformations/0,eye_point/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,foldl/3]).

menu(X, Y, _) ->
    L = wings_pref:get_value(number_of_lights),
    Menu = [{"Ground plane",show_groundplane,crossmark(show_groundplane)},
	    {"Axes",show_axes,crossmark(show_axes)},
	    separator,
	    {"Workmode",workmode,crossmark(workmode)},
	    {"Smoothed Preview",smoothed_preview},
	    separator,
	    {"Wireframe",wireframe},
	    {"Shade",shade},
	    {"Toggle wireframed/shaded",toggle_wireframe},
	    separator,
	    {"Show Saved BB",show_bb,crossmark(show_bb)},
	    {"Show Edges",show_edges,crossmark(show_edges)},
	    {"Show Normals",show_normals,crossmark(show_normals)},
	    {"Show Wireframe Backfaces",show_wire_backfaces,
	     crossmark(show_wire_backfaces)},
	    separator,
	    {"Reset View",reset},
	    {"Aim",aim},
	    {"Frame",frame},
	    {"Ortographic View",orthogonal_view,
	     crossmark(orthogonal_view)},
	    {one_of(L == 1, "Two lights", "One light"),toggle_lights},
	    separator,
 	    {"Show Colors",show_colors,crossmark(show_colors)},
 	    {"Show Materials",show_materials,crossmark(show_materials)},
	    {"Show Textures",show_textures,crossmark(show_textures)},
	    separator,
	    {"View Along",{along,[{"+X",x},
				  {"+Y",y},
				  {"+Z",z},
				  {"-X",neg_x},
				  {"-Y",neg_y},
				  {"-Z",neg_z}]}},
	    separator,
	    {"Align to Selection",align_to_selection},
	    {"Auto Rotate",auto_rotate}],
    wings_menu:menu(X, Y, view, Menu).

crossmark(Key) ->
    case wings_pref:get_value(Key) of
	false -> [];
	true -> [crossmark]
    end.
	     
command(reset, St) ->
    reset(),
    St;
command(workmode, St) ->
    ?SLOW(toggle_option(workmode)),
    St;
command(smoothed_preview, St) ->
    ?SLOW(smoothed_preview(St));
command(flatshade, St) ->
    wings_pref:set_value(workmode, true),
    St;
command(smoothshade, St) ->
    wings_pref:set_value(workmode, false),
    St;
command(toggle_wireframe, #st{sel=[]}=St) ->
    mode_change_all(toggle),
    St;
command(toggle_wireframe, St) ->
    mode_change_sel(toggle),
    St;
command(wireframe, #st{sel=[]}=St) ->
    mode_change_all(true),
    St;
command(wireframe, St) ->
    mode_change_sel(true),
    St;
command(shade, #st{sel=[]}=St) ->
    mode_change_all(false),
    St;
command(shade, St) ->
    mode_change_sel(false),
    St;
command(orthogonal_view, St) ->
    toggle_option(orthogonal_view),
    St;
command(show_textures, St) ->
    toggle_option(show_textures),
    wings_draw_util:map(
      fun(#dlo{src_we=#we{mode=uv}}=D, _) ->
	      D#dlo{work=none,smooth=none,smoothed=none};
	 (D, _) -> D
      end, []),
    St;
command(show_materials, St) ->
    toggle_option(show_materials),
    wings_draw_util:map(
      fun(#dlo{src_we=#we{mode=material}}=D, _) ->
	      D#dlo{work=none,smooth=none,smoothed=none};
	 (D, _) -> D
      end, []),
    St;
command(show_colors, St) ->
    toggle_option(show_colors),
    wings_draw_util:map(
      fun(#dlo{src_we=#we{mode=vertex}}=D, _) ->
	      D#dlo{work=none,smooth=none,smoothed=none};
	 (D, _) -> D
      end, []),
    St;
command(aim, St) ->
    aim(St),
    St;
command(frame, St) ->
    frame(St),
    St;
command({along,Axis}, St) ->
    along(Axis),
    St;
command(auto_rotate, St) ->
    auto_rotate(St);
command(rotate_left, St) ->
    #view{azimuth=Az0} = View = wings_view:current(),
    Az = Az0 + wings_pref:get_value(auto_rotate_angle),
    set_current(View#view{azimuth=Az}),
    St;
command(align_to_selection, St) ->
    aim(St),
    align_to_selection(St);
command(toggle_lights, St) ->
    toggle_lights(),
    St;
command(Key, St) ->
    toggle_option(Key),
    St.

virtual_mirror(create, #st{selmode=face}=St0) ->
    St = wings_sel:map(fun virtual_mirror_fun/2, St0),
    {save_state,St#st{sel=[]}};
virtual_mirror(create, _) ->
    wings_util:error("Virtual mirror requires a face selection.");
virtual_mirror(break, #st{shapes=Shs0}=St) ->
    case break_mirror(Shs0) of
	Shs0 -> St;
	Shs -> {save_state,St#st{shapes=Shs}}
    end;
virtual_mirror(freeze, #st{shapes=Shs0}=St) ->
    case freeze_mirror(Shs0) of
	Shs0 -> St;
	Shs -> {save_state,St#st{shapes=Shs}}
    end.

mode_change_all(Wire) ->
    wings_draw_util:update(fun mode_change_all/2, Wire).

mode_change_all(eol, _) -> eol;
mode_change_all(#dlo{wire=Wire}=D, toggle) ->
    {D#dlo{wire=not Wire},toggle};
mode_change_all(D, Wire) ->
    {D#dlo{wire=Wire},Wire}.

mode_change_sel(Wire) ->
    wings_draw_util:map(fun mode_change_sel/2, Wire).

mode_change_sel(#dlo{wire=Wire,src_sel={_,_}}=D, toggle) ->
    {D#dlo{wire=not Wire},toggle};
mode_change_sel(#dlo{src_sel={_,_}}=D, Wire) ->
    {D#dlo{wire=Wire},Wire};
mode_change_sel(D, Wire) -> {D,Wire}.

virtual_mirror_fun(Faces, We) ->
    case gb_sets:to_list(Faces) of
	[Face] ->
	    We#we{mirror=Face};
	_ ->
	    wings_util:error("Only a single face must be selected per object.")
    end.

break_mirror(Shapes) ->
    foldl(fun(#we{mirror=none}, Shs) -> Shs;
	     (#we{id=Id}=We, Shs) ->
		  gb_trees:update(Id, We#we{mirror=none}, Shs)
	  end, Shapes, gb_trees:values(Shapes)).

freeze_mirror(Shapes) ->
    foldl(fun(#we{mirror=none}, Shs) -> Shs;
	     (#we{id=Id,mirror=Face}=We0, Shs) ->
		  We = wings_face_cmd:mirror_faces([Face], We0),
		  gb_trees:update(Id, We#we{mirror=none}, Shs)
	  end, Shapes, gb_trees:values(Shapes)).

%%%
%%% The Auto Rotate command.
%%%

-record(tim,
	{timer,					%Current timer.
	 delay,					%Current delay.
	 st					%St record.
	 }).

auto_rotate(St) ->
    auto_rotate_help(),
    Delay = wings_pref:get_value(auto_rotate_delay),
    Tim = #tim{delay=Delay,st=St},
    {seq,{push,dummy},set_auto_rotate_timer(Tim)}.
    
auto_rotate_event(Event, #tim{timer=Timer,st=St}=Tim) ->
    case wings_camera:event(Event, St) of
	next -> auto_rotate_event_1(Event, Tim);
	Other ->
	    wings_io:cancel_timer(Timer),
	    {seq,fun(Ev) ->
			 auto_rotate_help(),
			 wings_io:putback_event(Ev),
			 set_auto_rotate_timer(Tim)
		 end,Other}
    end.

auto_rotate_event_1(redraw, Tim) ->
    auto_rotate_redraw(Tim),
    keep;
auto_rotate_event_1(#mousemotion{}, _) -> keep;
auto_rotate_event_1(#mousebutton{state=?SDL_PRESSED}, _) -> keep;
auto_rotate_event_1(#keyboard{}=Kb, #tim{delay=Delay}=Tim) ->
    case wings_hotkey:event(Kb) of
	{select,more} ->
	    get_event(Tim#tim{delay=Delay-10});
	{select,less} ->
	    get_event(Tim#tim{delay=Delay+10});
	_ ->
	    keep
    end;
auto_rotate_event_1({view,rotate_left=Cmd}, #tim{st=St}=Tim) ->
    command(Cmd, St),
    wings_wm:dirty(),
    set_auto_rotate_timer(Tim);
auto_rotate_event_1(_Event, #tim{timer=Timer}) ->
    wings_wm:dirty(),
    wings_io:clear_message(),
    wings_io:cancel_timer(Timer),
    pop.

auto_rotate_redraw(#tim{st=St}) ->
    wings_draw:render(St),
    wings_io:update(St).

auto_rotate_help() ->
    Help = ["[L] Stop rotating ",wings_camera:help()],
    wings_io:message_right("[+] Increase speed [-] Decrease speed"),
    wings_io:message(Help).

set_auto_rotate_timer(#tim{delay=Delay}=Tim) when Delay < 0 ->
    set_auto_rotate_timer(Tim#tim{delay=0});
set_auto_rotate_timer(#tim{delay=Delay}=Tim0) ->
    Timer = wings_io:set_timer(Delay, {view,rotate_left}),
    Tim = Tim0#tim{timer=Timer},
    get_event(Tim).

get_event(Tim) ->
    {replace,fun(Ev) -> auto_rotate_event(Ev, Tim) end}.

%%%
%%% Smoothed Preview
%%%

-record(sm,
	{st,					%State
	 wire=true,				%Show wireframe: true|false
	 cage=true}).				%Show cage: true|false

smoothed_preview(St) ->
    Sm = #sm{st=St},
    smooth_help(Sm),
    smooth_dlist(St),
    wings_wm:dirty(),
    {seq,{push,dummy},get_smooth_event(Sm)}.

smooth_help(#sm{wire=Wire,cage=Cage}) ->
    Help = ["[L] Normal Mode ",wings_camera:help(),
	    "  [W] ",
	    case Cage of
		false -> "Show cage";
		true -> "Hide cage"
	    end,
	    "  [E] ",
	    case Wire of
		false -> "Show edges";
		true -> "Hide edges"
	    end],
    wings_io:message(Help).
    
get_smooth_event(Sm) ->
    {replace,fun(Ev) -> smooth_event(Ev, Sm) end}.

smooth_event(Ev, Sm) ->
    case wings_camera:event(Ev, fun() -> smooth_redraw(Sm) end) of
	next -> smooth_event_1(Ev, Sm);
	Other -> Other
    end.

smooth_event_1(redraw, Sm) ->
    smooth_help(Sm),
    smooth_redraw(Sm),
    get_smooth_event(Sm);
smooth_event_1(#mousemotion{}, _) -> keep;
smooth_event_1(#mousebutton{state=?SDL_PRESSED}, _) -> keep;
smooth_event_1(#keyboard{keysym=#keysym{sym=?SDLK_ESCAPE}}, _) ->
    smooth_exit();
smooth_event_1(#keyboard{keysym=#keysym{unicode=$e}}, #sm{wire=Wire}=Sm) ->
    wings_wm:dirty(),
    get_smooth_event(Sm#sm{wire=not Wire});
smooth_event_1(#keyboard{keysym=#keysym{unicode=$w}}, #sm{cage=Cage}=Sm) ->
    wings_wm:dirty(),
    get_smooth_event(Sm#sm{cage=not Cage});
smooth_event_1(#keyboard{}=Kb, #sm{st=St}) ->
    case wings_hotkey:event(Kb) of
	{view,workmode} ->
	    smooth_exit();
	{view,smoothed_preview} ->
	    smooth_exit();
	{view,{along,_Axis}=Cmd} ->
	    command(Cmd, St),
	    wings_wm:dirty(),
	    keep;
	{view,reset} ->
	    reset(),
	    wings_wm:dirty(),
	    keep;
	{view,orthogonal_view} ->
	    toggle_option(orthogonal_view),
	    wings_wm:dirty(),
	    keep;
	{view,toggle_lights} ->
	    toggle_lights(),
	    wings_wm:dirty(),
	    keep;
	_ ->
	    keep
    end;
smooth_event_1({resize,_,_}=Resize, _) ->
    wings_io:putback_event(Resize),
    smooth_exit();
smooth_event_1(quit, _) ->
    wings_io:putback_event(quit),
    smooth_exit();
smooth_event_1(_, _) ->
    smooth_exit().

smooth_exit() ->
    wings_io:clear_message(),
    wings_wm:dirty(),
    pop.

smooth_dlist(St) ->
    wings_draw_util:update(fun(D, []) ->
				   smooth_dlist(D, St)
			   end, []).

smooth_dlist(eol, _) -> eol;
smooth_dlist(#dlo{smoothed=none,src_we=We0}=D, St) ->
    wings_io:disable_progress(),
    #we{es=Etab,vs=Vtab} = We = wings_subdiv:smooth(We0),
    {List,Tr} = wings_draw:smooth_dlist(We, St),
    Edges = gl:genLists(1),
    gl:newList(Edges, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    foreach(fun(#edge{vs=Va,ve=Vb}) ->
		    gl:vertex3fv(wings_vertex:pos(Va, Vtab)),
		    gl:vertex3fv(wings_vertex:pos(Vb, Vtab))
	    end, gb_trees:values(Etab)),
    gl:'end'(),
    gl:endList(),
    {D#dlo{smoothed=[List,Edges],transparent=Tr},[]};
smooth_dlist(D, _) -> {D,[]}.

smooth_redraw(#sm{st=St}=Sm) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:enable(?GL_DEPTH_TEST),
    gl:cullFace(?GL_BACK),
    wings_view:projection(),
    wings_view:model_transformations(),
    wings_draw_util:fold(fun(D, _) -> smooth_redraw(D, Sm, false) end, []),
    wings_draw_util:fold(fun(D, _) -> smooth_redraw(D, Sm, true) end, []),
    gl:popAttrib(),
    wings_io:update(St).

smooth_redraw(#dlo{mirror=none}=D, Sm, Flag) ->
    smooth_redraw_1(D, Sm, Flag);
smooth_redraw(#dlo{mirror=Matrix}=D, Sm, Flag) ->
    gl:cullFace(?GL_BACK),
    smooth_redraw_1(D, Sm, Flag),
    gl:cullFace(?GL_FRONT),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    smooth_redraw_1(D, Sm, Flag),
    gl:popMatrix(),
    gl:cullFace(?GL_BACK);
smooth_redraw(_, _, _) -> ok.

smooth_redraw_1(#dlo{smoothed=[Dlist,Es],transparent=Trans}=D, Sm, RenderTrans) ->
    ?CHECK_ERROR(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:shadeModel(?GL_SMOOTH),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(2.0, 2.0),

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
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    ?CHECK_ERROR(),
    gl:depthMask(?GL_TRUE),
    wireframe(D, Sm),
    smooth_wireframe(Es, RenderTrans, Sm).

wireframe(_, #sm{cage=false}) -> ok;
wireframe(#dlo{work=Work}, #sm{cage=true}) ->
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_CULL_FACE),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:enable(?GL_POLYGON_OFFSET_LINE),
    gl:color3f(0, 0, 1),
    wings_draw_util:call(Work).

smooth_wireframe(_, true, _) -> ok;
smooth_wireframe(_, false, #sm{wire=false}) -> ok;
smooth_wireframe(Dlist, false, #sm{wire=true}) ->
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:color3f(1, 1, 1),
    wings_draw_util:call(Dlist).

%%%
%%% Other stuff.
%%%

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
    wings_pref:set_default(show_colors, true),
    wings_pref:set_default(show_materials, true),
    wings_pref:set_default(show_textures, true),

    wings_pref:set_default(camera_fov, 45.0),
    wings_pref:set_default(camera_hither, 0.25),
    wings_pref:set_default(camera_yon, 1000.0),

    %% Always reset the following preferences + the view itself.
    wings_pref:set_value(workmode, true),
    wings_pref:set_value(orthogonal_view, false),

    View = #view{fov=wings_pref:get_value(camera_fov),
		 hither=wings_pref:get_value(camera_hither),
		 yon=wings_pref:get_value(camera_yon)},
    set_current(View),
    reset().

init_light() ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.1,0.1,0.1,1.0}),
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
    View = current(),
    set_current(View#view{origin={0.0,0.0,0.0},
			  azimuth=-45.0,elevation=25.0,
			  distance=?CAMERA_DIST,
			  pan_x=0.0,pan_y=0.0,
			  along_axis=none}).

projection() ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    perspective(),
    gl:matrixMode(?GL_MODELVIEW).

perspective() ->
    {_,_,W,H} = wings_wm:viewport(),
    #view{distance=D,fov=Fov,hither=Hither,yon=Yon} = current(),
    case wings_pref:get_value(orthogonal_view) of
	false ->
	    glu:perspective(Fov, W/H, Hither, Yon);
	true ->
	    Aspect = W/H,
	    Sz = 4.0 * D / ?CAMERA_DIST,
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, Hither, Yon)
    end.

model_transformations() ->
    #view{origin=Origin,distance=Dist0,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    {_,_,W,H} = wings_wm:viewport(),
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
    {OX,OY,OZ} = Origin,
    gl:translatef(OX, OY, OZ).

eye_point() ->
    #view{origin=Origin,distance=Dist0,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    {_,_,W,H} = wings_wm:viewport(),
    Dist = Dist0 * math:sqrt((W*H) / (640*480)),
    M0 = e3d_mat:translate(e3d_vec:neg(Origin)),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
    M2 = e3d_mat:mul(M1, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    M = e3d_mat:mul(M2, e3d_mat:translate(-PanX, -PanY, Dist)),
    e3d_mat:mul_point(M, {0.0,0.0,0.0}).

aim(#st{sel=[]}) ->
    View = current(),
    set_current(View#view{origin=e3d_vec:zero()});
aim(St) ->
    Centers = wings_sel:centers(St),
    Origin0 = e3d_vec:average(Centers),
    Origin = e3d_vec:neg(Origin0),
    #view{distance=Dist0} = View = current(),
    Dist = case e3d_vec:dist(eye_point(), Origin0) of
	       D when D < Dist0 -> D;
 	       _Other -> Dist0
 	   end,
    set_current(View#view{origin=Origin,distance=Dist,pan_x=0.0,pan_y=0.0}).

frame(#st{sel=[],shapes=Shs}) ->
    BB = foldl(fun(We, BB) -> wings_vertex:bounding_box(We, BB) end,
	       none, gb_trees:values(Shs)),
    frame_1(BB);
frame(St) ->
    frame_1(wings_sel:bounding_box(St)).

frame_1(none) -> ok;
frame_1(BB) ->
    {_,_,W,H} = wings_wm:viewport(),
    C = e3d_vec:average(BB),
    R = e3d_vec:len(e3d_vec:sub(C, hd(BB))),
    #view{fov=Fov} = View = current(),
    Dist = R/math:tan(Fov*3.1416/2/180) / math:sqrt((W*H) / (640*480)),
    set_current(View#view{origin=e3d_vec:neg(C),
			  distance=Dist,pan_x=0.0,pan_y=0.0}).

toggle_lights() ->
    Lights = case wings_pref:get_value(number_of_lights) of
		 1 -> 2;
		 2 -> 1
	     end,
    wings_pref:set_value(number_of_lights, Lights),
    init_light().

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

align_to_selection({Nx,Ny,Nz}, St) ->
    Z = {0.0,0.0,1.0},
    Az0 = e3d_vec:dot(e3d_vec:norm({Nx,0.0,Nz}), Z),
    Az1 = to_degrees(math:acos(Az0)),
    Az = if
	     Nx < 0 -> Az1;
	     true -> -Az1
	 end,
    El0 = if
	      abs(Nx) < abs(Nz) ->
		  Nyz = e3d_vec:norm({0.0,Ny,Nz}),
		  e3d_vec:cross(Nyz, Z);
	      true ->
		  X = {1.0,0.0,0.0},
		  Nxy = e3d_vec:norm({Nx,Ny,0.0}),
		  e3d_vec:cross(Nxy, X)
	  end,
    El1 = e3d_vec:len(El0),
    El2 = to_degrees(math:asin(El1)),
    El = if
	     Ny < 0.0 -> -El2;
	     true -> El2
	 end,
    View = current(),
    set_current(View#view{azimuth=Az,elevation=El}),
    St.

to_degrees(A) when is_float(A) ->
    A*180.0/3.1416.

one_of(true, S, _) -> S;
one_of(false,_, S) -> S.
