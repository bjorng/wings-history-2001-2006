%%
%%  wings_view.erl --
%%
%%     This module implements most of the commands in the View menu.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_view.erl,v 1.146 2004/05/03 10:33:59 raimo_niskanen Exp $
%%

-module(wings_view).
-export([menu/1,command/2,
	 virtual_mirror/2,
	 init/0,initial_properties/0,
	 current/0,set_current/1,
	 load_matrices/1,projection/0,
	 modelview/0,modelview/1,
	 eye_point/0,export_views/1,import_views/2,camera_info/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,foldl/3]).

menu(#st{views=Views}=St) ->
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
     {"Show Wireframe Backfaces",show_wire_backfaces,
      "Show wireframe backfaces",crossmark(show_wire_backfaces)},
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
     {"Saved Views: "++integer_to_list(queue:len(Views)),
      {views,views_submenu(Views)}},
     separator,
     {"Camera Settings...",camera_settings,"Set field of view, and near and far clipping planes"},
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

views_submenu(Views) ->
    [{"Next",next,views_submenu_help(Views, next)},
     {"Current",current,views_submenu_help(Views, current)},
     {"Prev",prev,views_submenu_help(Views, prev)},
     views_jumpmenu(Views),
     {"Save",save,"Save this view as current",[option]},
     {"Rename...",rename,views_submenu_help(Views, rename)},
     {"Delete",delete,views_submenu_help(Views, delete)},
     {"Delete All...",delete_all,views_submenu_help(Views, delete_all)}].

views_submenu_help(Views0, Action) ->
    case queue:is_empty(Views0) of
	true -> "No saved views!";
	false ->
	    case Action of
		next -> 
		    {_,Legend} = queue:head(Views0),
		    "Jump to \""++Legend++"\"";
		current -> 
		    {_,Legend} = queue:last(Views0),
		    "Jump to \""++Legend++"\"";
		prev -> 
		    {{value,VL},Views} = queue:out_r(Views0),
		    {_,Legend} = queue:last(queue:in_r(VL, Views)),
		    "Jump to \""++Legend++"\"";
		rename -> 
		    {_,Legend} = queue:last(Views0),
		    "Rename \""++Legend++"\"";
		delete ->
		    {_,Legend} = queue:last(Views0),
		    "Delete \""++Legend++"\"";
		delete_all ->
		    "Delete all saved views"
	    end
    end.
	    
views_jumpmenu(Views0) ->
    case queue:is_empty(Views0) of
	true -> 
	    {"Jump to",current,"No saved views!"};
	false ->
	    {{value,VL},Views} = queue:out_r(Views0),
	    {"Jump to",{jump,views_jumpmenu_1(queue:in_r(VL, Views), 
					      0, [], [])}}
    end.

views_jumpmenu_1(Views0, N, Next, Prev) ->
    case queue:out(Views0) of
	{empty,Views0} ->
	    lists:reverse(Next, Prev);
	{{value,{_,Legend}},Views} when N =:= 0 ->
	    views_jumpmenu_1(Views, N+1, 
			     [{Legend,N,"Current (jump +0)"}|Next], Prev);
	{{value,{_,Legend}},Views} ->
	    Help = if N =:= 1 -> "Next (jump +1)";
		      true    -> "Jump +"++integer_to_list(N)
		   end,
	    views_jumpmenu_2(Views, N, 
			     [{Legend,N,Help}|Next], Prev)
    end.

views_jumpmenu_2(_Views, N, Next, Prev) when N > 5 ->
    lists:reverse(Next, [separator|Prev]);
views_jumpmenu_2(Views0, N, Next, Prev) ->
    case queue:out_r(Views0) of
	{empty,Views0} ->
	    lists:reverse(Next, Prev);
	{{value,{_,Legend}},Views} ->
	    Help = if N =:= 1 -> "Prev (jump -1)";
		      true    -> "Jump -"++integer_to_list(N)
		   end,
	    views_jumpmenu_1(Views, N+1, 
			     Next, [{Legend,-N,Help}|Prev])
    end.

command(reset, St) ->
    reset(),
    St;
command(workmode, St) ->
    ?SLOW(toggle_option(workmode)),
    St;
command(toggle_wireframe, #st{sel=[]}=St) ->
    wireframe_all(toggle, St),
    St;
command(toggle_wireframe, St) ->
    wireframe_sel(toggle, St),
    St;
command(wireframe, #st{sel=[]}=St) ->
    wireframe_all(true, St),
    St;
command(wireframe, St) ->
    wireframe_sel(true, St),
    St;
command(shade, #st{sel=[]}=St) ->
    wireframe_all(false, St),
    St;
command(shade, St) ->
    wireframe_sel(false, St),
    St;
command(smooth_proxy, St) ->
    wings_subdiv:setup(St),
    St;
command(quick_preview, St) ->
    ?SLOW(wings_subdiv:quick_preview(St)),
    St;
command(orthogonal_view, St) ->
    toggle_option(orthogonal_view),
    St;
command(show_textures, St) ->
    toggle_option(show_textures),
    wings_draw_util:map(
      fun(#dlo{src_we=#we{mode=material}}=D, _) ->
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
command({views,Views}, St) ->
    views(Views, St);
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
command(camera_settings, St) ->
    camera(),
    St;
command(Key, St) ->
    toggle_option(Key),
    St.

virtual_mirror(create, #st{selmode=face}=St0) ->
    St = wings_sel:map(fun virtual_mirror_fun/2, St0),
    {save_state,St#st{sel=[]}};
virtual_mirror(create, _) ->
    wings_util:error("Virtual mirror requires a face selection.");
virtual_mirror(break, St0) ->
    case break_mirror(St0) of
	St0 -> St0;
	St -> {save_state,St}
    end;
virtual_mirror(freeze, St0) ->
    case freeze_mirror(St0) of
	St0 -> St0;
	St -> {save_state,wings_sel:valid_sel(St)}
    end.

wireframe_all(false, _) ->
    wings_wm:set_prop(wireframed_objects, gb_sets:empty());
wireframe_all(true, St) ->
    All = wings_shape:all_selectable(St),
    wings_wm:set_prop(wireframed_objects, All);
wireframe_all(toggle, St) ->
    Selectable = wings_shape:all_selectable(St),
    Prev = wings_wm:get_prop(wireframed_objects),
    Changed = gb_sets:difference(Selectable, Prev),
    New = gb_sets:union(Changed, gb_sets:difference(Prev, Selectable)),
    wings_wm:set_prop(wireframed_objects, New).

wireframe_sel(false, St) ->
    Prev = wings_wm:get_prop(wireframed_objects),
    Sel = sel_to_set(St),
    New = gb_sets:difference(Prev, Sel),
    wings_wm:set_prop(wireframed_objects, New);
wireframe_sel(true, St) ->
    Prev = wings_wm:get_prop(wireframed_objects),
    Sel = sel_to_set(St),
    New = gb_sets:union(Prev, Sel),
    wings_wm:set_prop(wireframed_objects, New);
wireframe_sel(toggle, St) ->
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
	    We#we{mirror=Face};
	_ ->
	    wings_util:error("Only a single face must be selected per object.")
    end.

break_mirror(#st{shapes=Shs0}=St) ->
    Shs = foldl(fun(#we{id=Id}=We, Shs) ->
			gb_trees:update(Id, We#we{mirror=none}, Shs)
		end, Shs0, sel_mirror_objects(St)),
    St#st{shapes=Shs}.

freeze_mirror(#st{shapes=Shs0}=St) ->
    Shs = foldl(fun(#we{id=Id,mirror=Face}=We0, Shs) ->
			We = wings_face_cmd:mirror_faces([Face], We0),
			gb_trees:update(Id, We#we{mirror=none}, Shs)
		end, Shs0, sel_mirror_objects(St)),
    St#st{shapes=Shs}.

sel_mirror_objects(#st{sel=[],shapes=Shs}) ->
    foldl(fun(#we{mirror=none}, A) -> A;
	     (#we{perm=P}, A) when ?IS_NOT_SELECTABLE(P) -> A;
	     (We, A) -> [We|A]
	  end, [], gb_trees:values(Shs));
sel_mirror_objects(St) ->
    wings_sel:fold(fun(_, #we{mirror=none}, A) -> A;
		      (_, #we{perm=P}, A) when ?IS_NOT_SELECTABLE(P) -> A;
		      (_, We, A) -> [We|A]
		   end, [], St).

camera() ->
    Active = wings_wm:this(),
    View0 = wings_wm:get_prop(Active, current_view),
    #view{fov=Fov0,hither=Hither0,yon=Yon0} = View0,
    Qs = [{label_column,
	   [{"Field of View",{text,Fov0,[{range,1.0,180.0}]}},
	    {"Near Clipping Plane",{text,Hither0,
				      [{range,0.001,1000.0}]}},
	    {"Far Clipping Plane",{text,Yon0,
				   [{range,100.0,9.9e307}]}}]}],
    wings_ask:dialog("Camera Settings", Qs,
		     fun([Fov,Hither,Yon]) ->
			     View = View0#view{fov=Fov,hither=Hither,yon=Yon},
			     wings_wm:set_prop(Active, current_view, View),
			     ignore
		     end).


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
    Msg1 = wings_util:button_format("Stop rotating"),
    Msg2 = wings_camera:help(),
    Message = wings_util:join_msg([Msg1,Msg2,
				   "[+] Increase speed",
				   "[-] Decrease speed"]),
    wings_wm:message(Message).

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
     {allow_rotation,true},
     {show_info_text,true},
     {show_wire_backfaces,false}
    ].

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

load_matrices(IncludeLights) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    projection(),
    modelview(IncludeLights).

projection() ->
    {W,H} = wings_wm:win_size(),
    Aspect = W/H,
    #view{distance=D,fov=Fov,hither=Hither,yon=Yon} = current(),
    case wings_wm:get_prop(orthogonal_view) of
	false ->
	    glu:perspective(Fov, Aspect, Hither, Yon);
	true ->
	    Sz = D*math:tan(Fov*math:pi()/180/2),
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, Hither, Yon)
    end.

modelview() ->
    modelview(false).

modelview(IncludeLights) ->
    #view{origin=Origin,distance=Dist,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    if
	IncludeLights -> wings_light:camera_lights();
	true -> ok
    end,
    gl:translatef(PanX, PanY, -Dist),
    gl:rotatef(El, 1, 0, 0),
    gl:rotatef(Az, 0, 1, 0),
    {OX,OY,OZ} = Origin,
    gl:translatef(OX, OY, OZ),
    if
	IncludeLights -> wings_light:global_lights();
	true -> ok
    end.

%% Calculate the location of the viewer in 3D space.
%% (The (0,0,0) point multiplied by the inverse model transformation matrix.)
eye_point() ->
    e3d_mat:mul_point(view_matrix(current()), {0.0,0.0,0.0}).

view_matrix(#view{origin=Origin,distance=Dist,azimuth=Az,elevation=El,
		 pan_x=PanX, pan_y=PanY}) ->
    M0 = e3d_mat:translate(e3d_vec:neg(Origin)),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
    M2 = e3d_mat:mul(M1, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    e3d_mat:mul(M2, e3d_mat:translate(-PanX, -PanY, Dist)).

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
    BB = foldl(fun(#we{perm=P,vp=Vtab}=We, BB) when ?IS_VISIBLE(P) ->
		       case gb_sets:is_empty(Vtab) of
			   false -> wings_vertex:bounding_box(We, BB);
			   true -> BB
		       end;
		  (_, BB) -> BB
	       end,
	       none, gb_trees:values(Shs)),
    frame_1(BB);
frame(St) -> frame_1(wings_sel:bounding_box(St)).

frame_1(none) -> ok;
frame_1([A,B]=BB) ->
    C = e3d_vec:average(BB),
    R = e3d_vec:len(e3d_vec:sub(A, B)) / 2,
    #view{fov=Fov} = View = current(),
    Dist = R/math:tan(Fov*3.14159/2/180),
    set_current(View#view{origin=e3d_vec:neg(C),
			  distance=Dist,pan_x=0.0,pan_y=0.0}).

views(next, #st{views=Views0}=St) ->
    case queue:out(Views0) of
	{empty,_} ->
	    wings_util:message("No saved views");
	{{value,{View,_}=VL},Views} ->
	    set_current(View),
	    St#st{views=queue:in(VL, Views)}
    end;
views(current, #st{views=Views}) ->
    case queue:is_empty(Views) of
	true ->
	    wings_util:message("No saved views");
	false ->
	    {View,_} = queue:last(Views),
	    set_current(View),
	    wings_wm:dirty()
    end;
views(prev, #st{views=Views0}=St) ->
    case queue:out_r(Views0) of
	{empty,_} ->
	    wings_util:message("No saved views");
	{{value,VL1},Views1} ->
	    Views = queue:in_r(VL1, Views1),
	    {View,_} = queue:last(Views),
	    set_current(View),
	    St#st{views=Views}
    end;
views({jump,J}, #st{views=Views}=St) ->
    views_jump(J, St, Views);
views({save,[Legend]}, #st{views=Views}=St) ->
    {save_state,St#st{views=queue:in({current(),Legend}, Views)}};
views({save,Ask}, #st{views=Views}) when is_atom(Ask) ->
    View = current(),
    case queue:is_empty(Views) of
	true ->
	    wings_ask:dialog(Ask, "Save view as", 
			     views_rename_qs([view_legend(View)]),
			     fun(Opts) -> {view,{views,{save,Opts}}} end);
	false ->
	    case queue:last(Views) of
		{View,_} ->
		    wings_util:message("This view is alreay the current");
		_ ->
		    wings_ask:dialog(Ask, "Save view as", 
				     views_rename_qs([view_legend(View)]),
				     fun(Opts) -> 
					     {view,{views,{save,Opts}}} 
				     end)
	    end
    end;
views(rename, #st{views=Views0}=St) ->
    case queue:out_r(Views0) of 
	{empty,Views0} ->
	    wings_util:message("No saved views");
	{{value,{View,Legend}},Views} ->
	    wings_ask:dialog("Rename view",
			     views_rename_qs([Legend]),
			     fun([NewLegend]) ->
				     St#st{views=queue:in({View,NewLegend},
							  Views)}
			     end)
    end;
views(delete, #st{views=Views}=St) ->
    case queue:is_empty(Views) of
	true ->
	    wings_util:message("No saved views");
	false ->
	    View = current(),
	    case queue:last(Views) of
		{View,_} ->
		    {save_state,St#st{views=queue:init(Views)}};
		_ ->
		    wings_util:message("You have to be at the current view")
	    end
    end;
views(delete_all, #st{views=Views}=St) ->
    case queue:is_empty(Views) of
	true ->
	    wings_util:message("No saved views");
	false ->
	    This = wings_wm:this(),
	    wings_util:yes_no(
	      "Are you sure you want to delete all saved views?",
	      fun() -> 
		      wings_wm:send(This, 
				    {new_state,St#st{views=queue:new()}}),
		      ignore
	      end)
    end.
    

views_jump(0, St, Views) ->
    views(current, St#st{views=Views});
views_jump(1, St, Views) ->
    views(next, St#st{views=Views});
views_jump(-1, St, Views) ->
    views(prev, St#st{views=Views});
views_jump(J, St, Views0) when J > 0 ->
    case queue:out(Views0) of
	{empty,Views0} ->
	    wings_util:message("No saved views");
	{{value,VL},Views} ->
	    views_jump(J-1, St, queue:in(VL, Views))
    end;
views_jump(J, St, Views0) -> % when J < 0
    case queue:out_r(Views0) of
	{empty,Views0} ->
	    wings_util:message("No saved views");
	{{value,VL},Views} ->
	    views_jump(J+1, St, queue:in_r(VL, Views))
    end.

views_rename_qs([Legend]) ->
    [{hframe,[{label,"Name"},{text,Legend}]}].

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

along(-90.0, 0.0) -> x;
along(0.0,  90.0) -> y;
along(0.0,   0.0) -> z;
along(90.0,  0.0) -> neg_x;
along(0.0,  -0.0) -> neg_y;
along(180.0, 0.0) -> neg_z;
along(_Az,   _El) -> none.

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
    A*180.0/3.1415926536.

one_of(true, S, _) -> S;
one_of(false,_, S) -> S.

%%%
%%% Export and import of views.
%%%

export_views(St) ->
    export_views_1(queue:to_list(St#st.views)).

export_views_1([{View,Name}|Views]) ->
    Tags = [aim,distance_to_aim,azimuth,elevation,tracking,fov,hither,yon],
    Props = [{name,Name}|zip(Tags, camera_info(Tags, View))],
    [{view,Props}|export_views_1(Views)];
export_views_1([]) -> [].

zip([H1|T1], [H2|T2]) ->
    [{H1,H2}|zip(T1, T2)];
zip([], []) -> [].

import_views(Views, St) ->
    St#st{views=queue:from_list(import_views_1(Views))}.

import_views_1([{view,As}|Views]) ->
    [import_view(As)|import_views_1(Views)];
import_views_1([]) -> [].

import_view(As) ->
    import_view(As, #view{}, undefined).

import_view([{aim,Aim}|As], View, Name) ->
    import_view(As, View#view{origin=Aim}, Name);
import_view([{distance_to_aim,Dist}|As], View, Name) ->
    import_view(As, View#view{distance=Dist}, Name);
import_view([{azimuth,Az}|As], View, Name) ->
    import_view(As, View#view{azimuth=Az}, Name);
import_view([{elevation,El}|As], View, Name) ->
    import_view(As, View#view{elevation=El}, Name);
import_view([{tracking,{X,Y}}|As], View, Name) ->
    import_view(As, View#view{pan_x=X,pan_y=Y}, Name);
import_view([{fov,Fov}|As], View, Name) ->
    import_view(As, View#view{fov=Fov}, Name);
import_view([{hither,Hither}|As], View, Name) ->
    import_view(As, View#view{hither=Hither}, Name);
import_view([{yon,Yon}|As], View, Name) ->
    import_view(As, View#view{yon=Yon}, Name);
import_view([{name,Name}|As], View, _) ->
    import_view(As, View, Name);
import_view([], #view{azimuth=Az,elevation=El}=View, undefined) -> 
    {View#view{along_axis=along(Az, El)},view_legend(View)};
import_view([], #view{azimuth=Az,elevation=El}=View, Name) -> 
    {View#view{along_axis=along(Az, El)},Name}.

%%%
%%% Camera info.
%%%

camera_info([aim|As], #view{origin=Aim}=View) ->
    [Aim|camera_info(As, View)];
camera_info([distance_to_aim|As], #view{distance=Dist}=View) ->
    [Dist|camera_info(As, View)];
camera_info([azimuth|As], #view{azimuth=Az}=View) ->
    [Az|camera_info(As, View)];
camera_info([elevation|As], #view{elevation=El}=View) ->
    [El|camera_info(As, View)];
camera_info([tracking|As], #view{pan_x=X,pan_y=Y}=View) ->
    [{X,Y}|camera_info(As, View)];
camera_info([fov|As], #view{fov=Fov}=View) ->
    %% Field of view.
    [Fov|camera_info(As, View)];
camera_info([hither|As], #view{hither=Hither}=View) ->
    %% Near clipping plane.
    [Hither|camera_info(As, View)];
camera_info([yon|As], #view{yon=Yon}=View) ->
    %% Far clipping plane.
    [Yon|camera_info(As, View)];
camera_info([pos_dir_up|As], View) ->
    [camera_pos_dir_up(View)|camera_info(As, View)];
camera_info([], _) -> [].

camera_pos_dir_up(#view{distance=Dist}=View) ->
    M = view_matrix(View),
    Pos = e3d_mat:mul_point(M, {0.0,0.0,0.0}),
    Aim = e3d_mat:mul_point(M, {0.0,0.0,-Dist}),
    Above = e3d_mat:mul_point(M, {0.0,1.0,0.0}),
    Dir = e3d_vec:sub(Aim, Pos),
    Up = e3d_vec:sub(Above, Pos),
    %% Pos is position of camera.
    %% Dir is the vector from Pos to the Aim point.
    %% Up points up from Pos and is normalized, so Pos+Up is a point 1.0
    %% above the camera, with the cameras notion of above.
    %% Up is also orthogonal to Dir, so Up x Dir = Left.
    {Pos,Dir,Up}.

view_legend(#view{distance=Dist,along_axis=Along}=View) ->
    [{Pos,Dir,_}] = camera_info([pos_dir_up], View),
    Legend = 
	io_lib:format(
	  "From ~s ~s distance ~.4g",
	  [pos_legend(Pos),
	   case Along of
	       x -> "along +X";
	       y -> "along +Y";
	       z -> "along +Z";
	       neg_x -> "along -X";
	       neg_y -> "along -Y";
	       neg_z -> "along -Z";
	       none ->
		   Aim = e3d_vec:add(Pos, Dir),
		   AimLen = e3d_vec:len(Aim),
		   if AimLen < Dist*0.000001 ->
			   %% Close enough to Origin
			   "towards Origin";
		      true ->
			   "towards "++pos_legend(e3d_vec:neg(Dir))
		   end
	   end,
	   Dist]),
    lists:flatten(Legend).

pos_legend({X,Y,Z}) ->
    %% Sort the coordinates as greatest absolute value first.
    %% Remove the ones with absolute value lower then tan(45/2) times
    %% the maximum. This should use the same style as N, NW, W type 
    %% compass direction notation where each value has 
    %% a precision of +-45/2 degrees.
    %% Return e.g "+X+Y", "-Z+X" depending on the sign of the remaining
    %% coordinates, largest first, as a deep charlist.
    [{Max,_,_}|_] = L =
	    lists:reverse(lists:sort(
			    [{abs(X),$X,X},{abs(Y),$Y,Y},{abs(Z),$Z,Z}])),
    [[if Val < 0 -> $-;
	 true    -> $+ end,Char] 
     || {Abs,Char,Val} <- L, Abs >= Max*0.4142135624]. % tan(22.5 degrees)
