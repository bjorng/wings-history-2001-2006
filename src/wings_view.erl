%%
%%  wings_view.erl --
%%
%%     This module implements most of the command in the View menu.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_view.erl,v 1.122 2003/06/03 17:29:45 bjorng Exp $
%%

-module(wings_view).
-export([menu/1,command/2,
	 virtual_mirror/2,
	 init/0,initial_properties/0,
	 current/0,set_current/1,
	 projection/0,perspective/0,
	 model_transformations/0,model_transformations/1,eye_point/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,foldl/3]).

menu(St) ->
    L = wings_pref:get_value(number_of_lights),
    [{"Ground Plane",show_groundplane,"Show the ground plane",
      crossmark(show_groundplane)},
     {"Axes",show_axes,"Show the coordinate axes",crossmark(show_axes)},
     separator,
     {"Workmode",workmode,"Toggle flat/smooth shading",
      crossmark(workmode)},
     separator,
     {"Wireframe",wireframe,"Display selected objects as a wireframe "
      "(same for all objects if nothing is selected)"},
     {"Shade",shade,"Display selected objects as shaded "
      "(same for all objects if nothing is selected)"},
     {"Toggle Wireframe",toggle_wireframe,
      "Toggle display mode for selected objects "
      "(same for all objects if nothing is selected)",wireframe_crossmark(St)},
     separator,
     {"Toggle Proxy Mode",smooth_proxy,
      "Toggle the smooth proxy mode for selected objects"},
     {"Quick Smoothed Preview",quick_preview,
      "Toggle the smooth proxy mode for all objects"},
     separator,
     {"Show Saved BB",show_bb,"Display any saved bounding box",crossmark(show_bb)},
     {"Show Edges",show_edges,"Show edges in workmode",crossmark(show_edges)},
     {"Show Normals",show_normals,"Show normals for selected elements",
      crossmark(show_normals)},
     separator,
     {"Reset View",reset,"Reset view to the default position"},
     {"Aim",aim,"Aim the camera at the selected element"},
     {"Frame",frame,"Dolly to show all selected elements "
      "(or all objects if nothing is selected)"},
     {"Orthographic View",orthogonal_view,
      "Toggle between orthographic and perspective views",
      crossmark(orthogonal_view)},
     separator,
     {"Scene Lights",scene_lights,
      "Use the lights defined in the scene",
      crossmark(scene_lights)},
     {one_of(L == 1, "Two Lights", "One Light"),toggle_lights,
      one_of(L == 1, "Use two work lights",
	     "Use one work light")},
     separator,
     {"Show Colors",show_colors,
      "Show vertex colors on objects in \"vertex\" mode",
      crossmark(show_colors)},
     {"Show Materials",show_materials,
      "Show materials on objects in \"material\" or \"uv\" modes",
      crossmark(show_materials)},
     {"Show Textures",show_textures,
      "Show the texture on objects in \"uv\" mode",
      crossmark(show_textures)},
     {"Show Info Text",show_info_text,
      "Show an informational text at the top of this Geometry window",
      crossmark(show_info_text)},
     separator,
     {"View Along",{along,[{"+X",x},
			   {"+Y",y},
			   {"+Z",z},
			   {"-X",neg_x},
			   {"-Y",neg_y},
			   {"-Z",neg_z}]}},
     separator,
     {"Align to Selection",align_to_selection,
      "Align the view to the normal of the selection"},
     {"Auto Rotate",auto_rotate,"Spin the view"}].

crossmark(Key) ->
    Val = case wings_pref:get_value(Key) of
	      undefined ->
		  {_,Client} = wings_wm:this(),
		  wings_wm:get_prop(Client, Key);
	      Other -> Other
	  end,
    case Val of
	false -> [];
	true -> [crossmark]
    end.

wireframe_crossmark(#st{sel=[],shapes=Shs}) ->
    {menubar,Client} = wings_wm:this(),
    Wire = wings_wm:get_prop(Client, wireframed_objects),
    case {gb_sets:size(Wire),gb_trees:size(Shs)} of
	{0,_} -> [];
	{Same,Same} -> [crossmark];
	{_,_} -> [grey_crossmark]
    end;
wireframe_crossmark(#st{sel=Sel0}) ->
    {menubar,Client} = wings_wm:this(),
    Wire0 = wings_wm:get_prop(Client, wireframed_objects),
    Sel = gb_sets:from_list([Id || {Id,_} <- Sel0]),
    Wire = gb_sets:intersection(Sel, Wire0),
    case {gb_sets:size(Wire),gb_sets:size(Sel)} of
	{0,_} -> [];
	{Same,Same} -> [crossmark];
	{_,_} -> [grey_crossmark]
    end.
	     
command(reset, St) ->
    reset(),
    St;
command(workmode, St) ->
    ?SLOW(toggle_option(workmode)),
    St;
command(toggle_wireframe, #st{sel=[]}=St) ->
    mode_change_all(toggle, St),
    St;
command(toggle_wireframe, St) ->
    mode_change_sel(toggle, St),
    St;
command(wireframe, #st{sel=[]}=St) ->
    mode_change_all(true, St),
    St;
command(wireframe, St) ->
    mode_change_sel(true, St),
    St;
command(shade, #st{sel=[]}=St) ->
    mode_change_all(false, St),
    St;
command(shade, St) ->
    mode_change_sel(false, St),
    St;
command(smooth_proxy, St) ->
    wings_subdiv:setup(St),
    St;
command(quick_preview, St) ->
    wings_subdiv:quick_preview(St),
    St;
command(orthogonal_view, St) ->
    toggle_option(orthogonal_view),
    St;
command(show_textures, St) ->
    toggle_option(show_textures),
    wings_draw_util:map(
      fun(#dlo{src_we=#we{mode=uv}}=D, _) ->
	      D#dlo{work=none,smooth=none,proxy_faces=none};
	 (D, _) -> D
      end, []),
    St;
command(show_materials, St) ->
    toggle_option(show_materials),
    wings_draw_util:map(
      fun(#dlo{src_we=#we{mode=material}}=D, _) ->
	      D#dlo{work=none,smooth=none,proxy_faces=none};
	 (D, _) -> D
      end, []),
    St;
command(show_colors, St) ->
    toggle_option(show_colors),
    wings_draw_util:map(
      fun(#dlo{src_we=#we{mode=vertex}}=D, _) ->
	      D#dlo{work=none,smooth=none,proxy_faces=none};
	 (D, _) -> D
      end, []),
    St;
command(show_normals, St) ->
    Bool = wings_pref:get_value(show_normals),
    wings_pref:set_value(show_normals, not Bool),
    case Bool of
	false -> St;
	true ->
	    wings_draw_util:map(fun(D, _) -> D#dlo{normals=none} end, []),
	    St
    end;
command(show_edges, St) ->
    Bool = wings_pref:get_value(show_edges),
    wings_pref:set_value(show_edges, not Bool),
    case Bool of
	false -> St;
	true ->
	    wings_draw_util:map(fun(D, _) -> D#dlo{hard=none} end, []),
	    St
    end;
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
    #view{azimuth=Az0} = View = current(),
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
	Shs -> {save_state,wings_sel:valid_sel(St#st{shapes=Shs})}
    end.

mode_change_all(false, _) ->
    wings_wm:set_prop(wireframed_objects, gb_sets:empty());
mode_change_all(true, #st{shapes=Shs}) ->
    All = gb_sets:from_ordset(gb_trees:keys(Shs)),
    wings_wm:set_prop(wireframed_objects, All);
mode_change_all(toggle, #st{shapes=Shs}) ->
    Prev = wings_wm:get_prop(wireframed_objects),
    All = gb_sets:from_ordset(gb_trees:keys(Shs)),
    New = gb_sets:difference(All, Prev),
    wings_wm:set_prop(wireframed_objects, New).

mode_change_sel(false, St) ->
    Prev = wings_wm:get_prop(wireframed_objects),
    Sel = sel_to_set(St),
    New = gb_sets:difference(Prev, Sel),
    wings_wm:set_prop(wireframed_objects, New);
mode_change_sel(true, St) ->
    Prev = wings_wm:get_prop(wireframed_objects),
    Sel = sel_to_set(St),
    New = gb_sets:union(Prev, Sel),
    wings_wm:set_prop(wireframed_objects, New);
mode_change_sel(toggle, St) ->
    W0 = wings_wm:get_prop(wireframed_objects),
    Sel = sel_to_set(St),
    W1 = gb_sets:difference(W0, Sel),
    W = gb_sets:union(W1, gb_sets:difference(Sel, W0)),
    wings_wm:set_prop(wireframed_objects, W).

sel_to_set(#st{sel=Sel0}) ->
    Sel = foldl(fun({Id,_}, A) -> [Id|A] end, [], Sel0),
    gb_sets:from_list(Sel).

virtual_mirror_fun(Faces, We) ->
    case gb_sets:to_list(Faces) of
	[Face] ->
	    wings_material:assign('_hole_', [Face], We#we{mirror=Face});
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
    Active = wings_wm:this(),
    wings_wm:callback(fun() -> wings_util:menu_restriction(Active, []) end),
    {seq,push,set_auto_rotate_timer(Tim)}.
    
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
    wings_io:cancel_timer(Timer),
    pop.

auto_rotate_redraw(#tim{st=Redraw}) when is_function(Redraw) ->
    Redraw();
auto_rotate_redraw(#tim{st=#st{}=St}) ->
    wings_wm:clear_background(),
    wings_draw_util:render(St).

auto_rotate_help() ->
    Help = ["[L] Stop rotating ",wings_camera:help()],
    wings_wm:message(Help, "[+] Increase speed [-] Decrease speed").

set_auto_rotate_timer(#tim{delay=Delay}=Tim) when Delay < 0 ->
    set_auto_rotate_timer(Tim#tim{delay=0});
set_auto_rotate_timer(#tim{delay=Delay}=Tim0) ->
    Timer = wings_io:set_timer(Delay, {view,rotate_left}),
    Tim = Tim0#tim{timer=Timer},
    get_event(Tim).

get_event(Tim) ->
    {replace,fun(Ev) -> auto_rotate_event(Ev, Tim) end}.

%%%
%%% Other stuff.
%%%

toggle_option(Key) ->
    case wings_wm:lookup_prop(Key) of
	none ->
	    wings_pref:set_value(Key, not wings_pref:get_value(Key, false));
	{value,Bool} ->
	    wings_wm:set_prop(Key, not Bool)
    end.

current() ->
    wings_wm:get_prop(current_view).

set_current(View) ->
    wings_wm:set_prop(current_view, View),
    View.

init() ->
    wings_pref:set_default(show_edges, true),
    wings_pref:set_default(number_of_lights, 1),
    wings_pref:set_default(show_normals, false),
    wings_pref:set_default(show_bb, true),
    wings_pref:set_default(show_colors, true),
    wings_pref:set_default(show_materials, true),
    wings_pref:set_default(show_textures, true),

    wings_pref:set_default(scene_lights, false),

    wings_pref:set_default(smoothed_preview_cage, false),
    wings_pref:set_default(smoothed_preview_edges, false).

initial_properties() ->
    [{workmode,true},
     {orthogonal_view,false},
     {show_axes,true},
     {show_groundplane,true},
     {wireframed_objects,gb_sets:empty()},
     {current_view,default_view()},
     {show_info_text,true}].

reset() ->
    reset(current()).

reset(View) ->
    set_current(View#view{origin={0.0,0.0,0.0},
			  azimuth=-45.0,elevation=25.0,
			  distance=?CAMERA_DIST,
			  pan_x=0.0,pan_y=0.0,
			  along_axis=none}).

default_view() ->
    #view{origin={0.0,0.0,0.0},
	  azimuth=-45.0,elevation=25.0,
	  distance=?CAMERA_DIST,
	  pan_x=0.0,pan_y=0.0,
	  along_axis=none,
	  fov=45.0,
	  hither=0.1,
	  yon=10000.0}.

projection() ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    perspective(),
    gl:matrixMode(?GL_MODELVIEW).

perspective() ->
    {W,H} = wings_wm:win_size(),
    Aspect = W/H,
    #view{distance=D,fov=Fov,hither=Hither,yon=Yon} = current(),
    case wings_wm:get_prop(orthogonal_view) of
	false ->
	    glu:perspective(Fov, Aspect, Hither, Yon);
	true ->
	    Sz = 4.0 * D / ?CAMERA_DIST,
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, Hither, Yon)
    end.

model_transformations() ->
    model_transformations(false).

model_transformations(IncludeLights) ->
    #view{origin=Origin,distance=Dist,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    if
	IncludeLights -> wings_light:camera_lights();
	true -> ok
    end,
    gl:translatef(PanX, PanY, -Dist),
    gl:rotatef(El, 1.0, 0.0, 0.0),
    gl:rotatef(Az, 0.0, 1.0, 0.0),
    {OX,OY,OZ} = Origin,
    gl:translatef(OX, OY, OZ),
    if
	IncludeLights -> wings_light:global_lights();
	true -> ok
    end.

eye_point() ->
    #view{origin=Origin,distance=Dist0,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    {W,H} = wings_wm:win_size(),
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
    Origin0 = wings_sel:center(St),
    Origin = e3d_vec:neg(Origin0),
    #view{distance=Dist0} = View = current(),
    Dist = case e3d_vec:dist(eye_point(), Origin0) of
	       D when D < Dist0 -> D;
 	       _Other -> Dist0
 	   end,
    set_current(View#view{origin=Origin,distance=Dist,pan_x=0.0,pan_y=0.0}).

frame(#st{sel=[],shapes=Shs}) ->
    BB = foldl(fun(#we{perm=P}=We, BB) when ?IS_VISIBLE(P) ->
		       wings_vertex:bounding_box(We, BB);
		  (_, BB) -> BB
	       end,
	       none, gb_trees:values(Shs)),
    frame_1(BB);
frame(St) -> frame_1(wings_sel:bounding_box(St)).

frame_1(none) -> ok;
frame_1(BB) ->
    {W,H} = wings_wm:win_size(),
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
    wings_pref:set_value(number_of_lights, Lights).

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
