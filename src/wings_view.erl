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
%%     $Id: wings_view.erl,v 1.52 2002/05/10 14:02:59 bjorng Exp $
%%

-module(wings_view).
-export([menu/3,command/2,init/0,init_light/0,
	 current/0,set_current/1,
	 projection/0,perspective/0,
	 model_transformations/0,eye_point/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(FOV, 45).

-import(lists, [foreach/2,foldl/3]).

menu(X, Y, St) ->
    L = wings_pref:get_value(number_of_lights),
    Menu = [{"Ground plane",show_groundplane,crossmark(show_groundplane)},
	    {"Axes",show_axes,crossmark(show_axes)},
	    separator,
	    {"Wireframe",wire_mode,crossmark(wire_mode)},
	    {"Workmode",workmode,crossmark(workmode)},
	    {"Smoothed Preview",smoothed_preview},
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
% 	    {"Show Colors",show_colors,crossmark(show_colors)},
% 	    {"Show Materials",show_materials,crossmark(show_materials)},
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
	    separator,
	    {"Auto Rotate",auto_rotate}],
    wings_menu:menu(X, Y, view, Menu, St).

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
    wings_pref:set_value(wire_mode, false),
    wings_pref:set_value(workmode, true),
    St;
command(smoothshade, St) ->
    wings_pref:set_value(wire_mode, false),
    wings_pref:set_value(workmode, false),
    St;
command(orthogonal_view, St) ->
    toggle_option(orthogonal_view),
    St;
command(show_textures, St) ->
    toggle_option(show_textures),
    wings_draw:model_changed(),
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
    Lights = case wings_pref:get_value(number_of_lights) of
		 1 -> 2;
		 2 -> 1
	     end,
    wings_pref:set_value(number_of_lights, Lights),
    init_light(),
    St;
command(Key, St) ->
    toggle_option(Key),
    St.

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
    Help = [lmb|" Stop rotating "] ++ wings_camera:help(),
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
	 wire=false}).				%Show wireframe: true|false

smoothed_preview(St) ->
    smooth_help(),
    smooth_dlist(St),
    wings_wm:dirty(),
    Sm = #sm{st=St},
    {seq,{push,dummy},get_smooth_event(Sm)}.

smooth_help() ->
    Help = [lmb|" Normal Mode "] ++ wings_camera:help(),
    wings_io:message_right("[W] Toggle wireframe"),
    wings_io:message(Help).
    
get_smooth_event(Sm) ->
    {replace,fun(Ev) -> smooth_event(Ev, Sm) end}.

smooth_event(Ev, Sm) ->
    case wings_camera:event(Ev, fun() -> smooth_redraw(Sm) end) of
	next -> smooth_event_1(Ev, Sm);
	Other -> Other
    end.

smooth_event_1(redraw, Sm) ->
    smooth_help(),
    smooth_redraw(Sm),
    get_smooth_event(Sm);
smooth_event_1(#mousemotion{}, _) -> keep;
smooth_event_1(#mousebutton{state=?SDL_PRESSED}, _) -> keep;
smooth_event_1(#keyboard{keysym=#keysym{sym=?SDLK_ESCAPE}}, _) ->
    smooth_exit();
smooth_event_1(#keyboard{}=Kb, #sm{st=St,wire=Wire}=Sm) ->
    case wings_hotkey:event(Kb) of
	{view,workmode} ->
	    smooth_exit();
	{view,smoothed_preview} ->
	    smooth_exit();
	{view,wire_mode} ->
	    wings_wm:dirty(),
	    get_smooth_event(Sm#sm{wire=not Wire});
	{view,{along,_Axis}=Cmd} ->
	    command(Cmd, St),
	    wings_wm:dirty(),
	    keep;
	{view,reset} ->
	    reset(),
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
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    We = wings_subdiv:smooth(We0),
    wings_draw:smooth_faces(We, St),
    gl:endList(),
    {D#dlo{smoothed=List},[]};
smooth_dlist(D, _) -> {D,[]}.

smooth_redraw(#sm{st=St}=Sm) ->
    ok.
%     ?CHECK_ERROR(),
%     gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
%     gl:enable(?GL_DEPTH_TEST),
%     wings_view:projection(),
%     wings_view:model_transformations(),
%     gl:enable(?GL_CULL_FACE),
%     gl:cullFace(?GL_BACK),
%     gl:shadeModel(?GL_SMOOTH),
%     gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
%     ?CHECK_ERROR(),
%     gl:enable(?GL_LIGHTING),
%     gl:enable(?GL_POLYGON_OFFSET_FILL),
%     gl:enable(?GL_BLEND),
%     gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
%     gl:callList(?DL_SMOOTHED),
%     wireframe(Sm),
%     gl:popAttrib(),
%     ?CHECK_ERROR(),
%     wings_io:update(St),
%     ?CHECK_ERROR().

% smooth_dlist(St) ->
%     case wings_draw:get_dlist() of
% 	#dl{smoothed=none} -> build_smooth_dlist(St);
% 	_ -> ok
%     end.
	    
% build_smooth_dlist(#st{shapes=Shs}=St) ->
%     wings_io:disable_progress(),
%     gl:newList(?DL_SMOOTHED, ?GL_COMPILE),
%     foreach(fun(#we{perm=Perm}=We0) when ?IS_VISIBLE(Perm) ->
% 		    We = wings_subdiv:smooth(We0),
% 		    wings_draw:smooth_faces(We, St);
% 	       (#we{}) -> ok
% 	    end, gb_trees:values(Shs)),
%     gl:endList(),
%     DL = wings_draw:get_dlist(),
%     wings_draw:put_dlist(DL#dl{smoothed=?DL_SMOOTHED}).

% smooth_redraw(#sm{st=St}=Sm) ->
%     ?CHECK_ERROR(),
%     gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
%     gl:enable(?GL_DEPTH_TEST),
%     wings_view:projection(),
%     wings_view:model_transformations(),
%     gl:enable(?GL_CULL_FACE),
%     gl:cullFace(?GL_BACK),
%     gl:shadeModel(?GL_SMOOTH),
%     gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
%     ?CHECK_ERROR(),
%     gl:enable(?GL_LIGHTING),
%     gl:enable(?GL_POLYGON_OFFSET_FILL),
%     gl:enable(?GL_BLEND),
%     gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
%     gl:callList(?DL_SMOOTHED),
%     wireframe(Sm),
%     gl:popAttrib(),
%     ?CHECK_ERROR(),
%     wings_io:update(St),
%     ?CHECK_ERROR().

% wireframe(#sm{wire=false}) -> ok;
% wireframe(#sm{st=St}) ->
%     wings_draw:update_display_lists(true, St),
%     gl:disable(?GL_POLYGON_OFFSET_FILL),
%     gl:disable(?GL_LIGHTING),
%     gl:shadeModel(?GL_FLAT),
%     gl:disable(?GL_CULL_FACE),
%     gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
%     gl:enable(?GL_POLYGON_OFFSET_LINE),
%     gl:color3f(0, 0, 1),
%     draw_faces().

% draw_faces() ->
%     #dl{faces=DlistFaces} = wings_draw:get_dlist(),
%     gl:callList(DlistFaces).

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

    %% Always reset the following preferences + the view itself.
    wings_pref:set_value(wire_mode, false),
    wings_pref:set_value(workmode, true),
    wings_pref:set_value(orthogonal_view, false),
    reset().

init_light() ->
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
	    glu:perspective(?FOV, W/H, 0.25, 1000.0);
	true ->
	    #view{distance=D0} = current(),
	    Aspect = W/H,
	    Sz = 4.0 * D0 / ?CAMERA_DIST,
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, 0.25, 1000.0)
    end.

model_transformations() ->
    #view{origo=Origo,distance=Dist0,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
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
    {OX,OY,OZ} = Origo,
    gl:translatef(OX, OY, OZ).

eye_point() ->
    #view{origo=Origo,distance=Dist0,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Dist = Dist0 * math:sqrt((W*H) / (640*480)),
    M0 = e3d_mat:translate(e3d_vec:neg(Origo)),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
    M2 = e3d_mat:mul(M1, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    M = e3d_mat:mul(M2, e3d_mat:translate(-PanX, -PanY, Dist)),
    e3d_mat:mul_point(M, {0.0,0.0,0.0}).

aim(#st{sel=[]}) ->
    View = current(),
    set_current(View#view{origo=e3d_vec:zero()});
aim(St) ->
    Centers = wings_sel:centers(St),
    Origin0 = e3d_vec:average(Centers),
    Origin = e3d_vec:neg(Origin0),
    #view{distance=Dist0} = View = current(),
    Dist = case e3d_vec:dist(eye_point(), Origin0) of
	       D when D < Dist0 -> D;
 	       _Other -> Dist0
 	   end,
    set_current(View#view{origo=Origin,distance=Dist,pan_x=0.0,pan_y=0.0}).

frame(#st{sel=[],shapes=Shs}) ->
    BB = foldl(fun(We, BB) -> wings_vertex:bounding_box(We, BB) end,
	       none, gb_trees:values(Shs)),
    frame_1(BB);
frame(St) ->
    frame_1(wings_sel:bounding_box(St)).

frame_1(BB) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    C = e3d_vec:average(BB),
    R = e3d_vec:len(e3d_vec:sub(C, hd(BB))),
    Dist = R/math:tan(?FOV*3.1416/2/180) / math:sqrt((W*H) / (640*480)),
    View = current(),
    set_current(View#view{origo=e3d_vec:neg(C),
			  distance=Dist,pan_x=0.0,pan_y=0.0}).

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
