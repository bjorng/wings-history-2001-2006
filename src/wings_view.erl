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
%%     $Id: wings_view.erl,v 1.104 2003/03/01 06:37:42 bjorng Exp $
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

menu(_) ->
    L = wings_pref:get_value(number_of_lights),
    [{"Ground Plane",show_groundplane,"Show the ground plane",
      crossmark(show_groundplane)},
     {"Axes",show_axes,"Show the coordinate axes",crossmark(show_axes)},
     separator,
     {"Workmode",workmode,"Toggle flat/smooth shading",
      crossmark(workmode)},
     {"Smoothed Preview",smoothed_preview,"Show a smoothed preview of the scene"},
     separator,
     {"Wireframe",wireframe,"Display selected objects as a wireframe "
      "(same for all objects if nothing is selected)"},
     {"Shade",shade,"Display selected objects as shaded "
      "(same for all objects if nothing is selected)"},
     {"Toggle Wireframed/Shaded",toggle_wireframe,
      "Toggle display mode for selected objects "
      "(same for all objects if nothing is selected)"},
     separator,
     {"Show Saved BB",show_bb,"Display any saved bounding box",crossmark(show_bb)},
     {"Show Edges",show_edges,"Show edges in workmode",crossmark(show_edges)},
     {"Show Normals",show_normals,"Show normals for selected elements",
      crossmark(show_normals)},
     {"Show Wireframe Backfaces",show_wire_backfaces,
      "Show back-facing faces for wireframed objects",
     crossmark(show_wire_backfaces)},
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
		  {_,Client} = wings_wm:active_window(),
		  wings_wm:get_prop(Client, Key);
	      Other -> Other
	  end,
    case Val of
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

virtual_mirror_fun(Faces, #we{fs=Ftab0}=We) ->
    case gb_sets:to_list(Faces) of
	[Face] ->
	    Frec = gb_trees:get(Face, Ftab0),
	    Ftab = gb_trees:update(Face, Frec#face{mat='_hole_'}, Ftab0),
	    We#we{fs=Ftab,mirror=Face};
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
    Active = wings_wm:active_window(),
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
%%% Smoothed Preview
%%%

-record(sm,
	{st,					%State
	 edge_style=none,		        %Edge style: none|cool|plain
	 cage=true}).				%Show cage: true|false

smoothed_preview(St) ->
    EdgeStyle = case wings_pref:get_value(smoothed_preview_edges) of
		    false -> none;
		    true -> plain;
		    Other -> Other
		end,
    Cage = wings_pref:get_value(smoothed_preview_cage),
    Sm = #sm{st=St,edge_style=EdgeStyle,cage=Cage},
    smooth_help(Sm),
    smooth_dlist(St),
    wings_wm:dirty(),
    Active = wings_wm:active_window(),
    wings_wm:callback(fun() ->
			      wings_util:menu_restriction(Active, [view])
		      end),
    {seq,push,get_smooth_event(Sm)}.

smooth_help(#sm{edge_style=EdgeStyle,cage=Cage}) ->
    Normal = wings_util:button_format([], [], "Normal Mode"),
    Help = [Normal," ",wings_camera:help(),
	    " [W] ",
	    case Cage of
		false -> "Show cage";
		true -> "Hide cage"
	    end,
	    " [E] ",
	    case EdgeStyle of
		none -> "Show all edges";
		plain -> "Show some edges";
		cool -> "Hide edges"
	    end],
    wings_wm:message(Help).
    
get_smooth_event(Sm) ->
    {replace,fun(Ev) -> smooth_event(Ev, Sm) end}.

smooth_event(Ev, Sm) ->
    case wings_camera:event(Ev, fun() -> smooth_redraw(Sm) end) of
	next -> smooth_event_1(Ev, Sm);
	Other -> Other
    end.

smooth_event_1(redraw, Sm) ->
    smooth_redraw(Sm),
    keep;
smooth_event_1(got_focus, Sm) ->
    smooth_help(Sm),
    wings_wm:dirty();
smooth_event_1(#mousemotion{}, _) -> keep;
smooth_event_1(#mousebutton{state=?SDL_PRESSED}, _) -> keep;
smooth_event_1(#mousebutton{button=3,state=?SDL_RELEASED}, Sm) ->
    smooth_exit(Sm);
smooth_event_1(#keyboard{keysym=#keysym{sym=?SDLK_ESCAPE}}, Sm) ->
    smooth_exit(Sm);
smooth_event_1(#keyboard{keysym=#keysym{unicode=$e}}, #sm{edge_style=Estyle0}=Sm) ->
    wings_wm:dirty(),
    Estyle = case Estyle0 of
		 none -> plain;
		 plain -> cool;
		 cool -> none
	     end,
    wings_pref:set_value(smoothed_preview_edges, Estyle),
    get_smooth_event(Sm#sm{edge_style=Estyle});
smooth_event_1(#keyboard{keysym=#keysym{unicode=$w}}, #sm{cage=Cage0}=Sm) ->
    wings_wm:dirty(),
    Cage = not Cage0,
    wings_pref:set_value(smoothed_preview_cage, Cage),
    get_smooth_event(Sm#sm{cage=Cage});
smooth_event_1(#keyboard{}=Kb, _) ->
    case wings_hotkey:event(Kb) of
	next -> keep;
	Action -> wings_wm:send(wings_wm:active_window(), {action,Action})
    end;
smooth_event_1({action,{view,View}}, #sm{st=St}=Sm) ->
    case View of
	workmode ->
	    smooth_exit(Sm);
	smoothed_preview ->
	    smooth_exit(Sm);
	{along,_Axis}=Cmd ->
	    command(Cmd, St),
	    wings_wm:dirty(),
	    keep;
	reset ->
	    reset(),
	    wings_wm:dirty(),
	    keep;
	orthogonal_view ->
	    toggle_option(orthogonal_view),
	    wings_wm:dirty(),
	    keep;
	toggle_lights ->
	    toggle_lights(),
	    wings_wm:dirty(),
	    keep;
	auto_rotate ->
	    auto_rotate(fun() -> smooth_redraw(Sm) end);
	_ ->
	    keep
    end;
smooth_event_1(init_opengl, #sm{st=St}) ->
    wings:init_opengl(St),
    pop;
smooth_event_1(quit, Sm) ->
    wings_wm:later(quit),
    smooth_exit(Sm);
smooth_event_1({current_state,#st{shapes=Shs}=St}, #sm{st=#st{shapes=Shs}}=Sm) ->
    refresh_dlist(St),
    get_smooth_event(Sm#sm{st=St});
smooth_event_1({current_state,St}, Sm) ->
    smooth_dlist(St),
    wings_wm:dirty(),
    get_smooth_event(Sm#sm{st=St});
smooth_event_1(_, _) -> keep.

smooth_exit(#sm{st=St}) ->
    wings_wm:later({new_state,St}),
    wings_wm:dirty(),
    pop.

refresh_dlist(St) ->
    wings_draw_util:map(fun(D, []) ->
				refresh_dlist(D, St)
			end, []).

refresh_dlist(#dlo{src_we=#we{light=L}}=D, _) when L =/= none -> {D,[]};
refresh_dlist(#dlo{smoothed=none,drag=none,src_we=We}=D, St) ->
    smooth_dlist_1(D, We, St);
refresh_dlist(#dlo{smoothed=none}=D, St) ->
    smooth_dlist_1(D, wings_draw:original_we(D), St);
refresh_dlist(D, _) -> D.

smooth_dlist(St) ->
    wings_draw_util:map(fun(D, []) ->
				smooth_dlist(D, St)
			end, []).

smooth_dlist(#dlo{src_we=#we{light=L}}=D, _) when L =/= none -> {D,[]};
smooth_dlist(#dlo{drag=none,src_we=We}=D, St) ->
    smooth_dlist_1(D, We, St);
smooth_dlist(#dlo{smoothed=none}=D, St) ->
    smooth_dlist_1(D, wings_draw:original_we(D), St);
smooth_dlist(D, _) -> D.

smooth_dlist_1(D, We0, St) ->
    #we{es=Etab,vp=Vtab} = We = wings_subdiv:smooth(We0),
    {List,Tr} = wings_draw:smooth_dlist(We, St),

    Edges = gl:genLists(2),
    CoolEdges = Edges+1,
    gl:newList(Edges, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    foreach(fun(#edge{vs=Va,ve=Vb}) ->
		    gl:vertex3fv(gb_trees:get(Va, Vtab)),
		    gl:vertex3fv(gb_trees:get(Vb, Vtab))
	    end, gb_trees:values(Etab)),
    gl:'end'(),
    gl:endList(),

    gl:newList(CoolEdges, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    foreach(fun(Edge) ->
		    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		    gl:vertex3fv(gb_trees:get(Va, Vtab)),
		    gl:vertex3fv(gb_trees:get(Vb, Vtab))
	    end, smooth_cool_edges(We0, We)),
    gl:'end'(),
    gl:endList(),
    
    D#dlo{smoothed=[List,Edges,CoolEdges],transparent=Tr}.

smooth_cool_edges(#we{vp=Vtab}, We) ->
    Vs = gb_trees:keys(Vtab),
    wings_edge:from_vs(Vs, We).

smooth_redraw(Sm) ->
    wings_wm:clear_background(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    gl:color3i(0, 0, 0),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:rectf(0, 0, W-0.5, H-0.5),
    gl:enable(?GL_DEPTH_TEST),
    gl:frontFace(?GL_CCW),
    projection(),
    model_transformations(true),
    wings_draw_util:fold(fun(D, _) -> smooth_redraw(D, Sm, false) end, []),
    wings_draw_util:fold(fun(D, _) -> smooth_redraw(D, Sm, true) end, []),
    gl:popAttrib().

smooth_redraw(#dlo{mirror=none}=D, Sm, Flag) ->
    smooth_redraw_1(D, Sm, Flag);
smooth_redraw(#dlo{mirror=Matrix}=D, Sm, Flag) ->
    gl:frontFace(?GL_CCW),
    smooth_redraw_1(D, Sm, Flag),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    smooth_redraw_1(D, Sm, Flag),
    gl:popMatrix(),
    gl:frontFace(?GL_CCW);
smooth_redraw(_, _, _) -> ok.

smooth_redraw_1(#dlo{src_we=#we{light=L}}, _Sm, _RenderTrans) when L =/= none ->
    ok;
smooth_redraw_1(#dlo{smoothed=[Dlist,Es,Cool],transparent=Trans}=D, Sm, RenderTrans) ->
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
    smooth_cage(D, Sm),
    smooth_edges(Es, Cool, RenderTrans, Sm).

smooth_cage(_, #sm{cage=false}) -> ok;
smooth_cage(#dlo{work=Work}, #sm{cage=true}) ->
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_CULL_FACE),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:enable(?GL_POLYGON_OFFSET_LINE),
    gl:color3f(0, 0, 1),
    wings_draw_util:call(Work).

smooth_edges(_, _, true, _) -> ok;
smooth_edges(_, _, false, #sm{edge_style=none}) -> ok;
smooth_edges(Plain, _Cool, false, #sm{edge_style=plain}) ->
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:color3f(1, 1, 1),
    wings_draw_util:call(Plain);
smooth_edges(_Plain, Cool, false, #sm{edge_style=cool}) ->
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:color3f(1, 1, 1),
    wings_draw_util:call(Cool).

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
    case wings_wm:lookup_prop(current_view) of
	none ->
	    View = #view{fov=wings_pref:get_value(camera_fov),
			 hither=wings_pref:get_value(camera_hither),
			 yon=wings_pref:get_value(camera_yon)},
	    reset(View);
	{value,View} -> View
    end.

set_current(View) ->
    wings_wm:set_prop(current_view, View),
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

    wings_pref:set_default(scene_lights, false),

    wings_pref:set_default(smoothed_preview_cage, false),
    wings_pref:set_default(smoothed_preview_edges, false).

initial_properties() ->
    [{workmode,true},
     {orthogonal_view,false},
     {show_axes,wings_pref:get_value(show_axes)},
     {show_groundplane,wings_pref:get_value(show_groundplane)},
     {wireframed_objects,gb_sets:empty()}].

reset() ->
    reset(current()).

reset(View) ->
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
    BB = foldl(fun(#we{perm=P}=We, BB) when ?IS_VISIBLE(P) ->
		       wings_vertex:bounding_box(We, BB);
		  (_, BB) -> BB
	       end,
	       none, gb_trees:values(Shs)),
    frame_1(BB);
frame(St) ->
    frame_1(wings_sel:bounding_box(St)).

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
