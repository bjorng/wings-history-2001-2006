%%
%%  wings.erl --
%%
%%     The main module of Wings 3D.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings.erl,v 1.214 2003/02/19 20:49:51 bjorng Exp $
%%

-module(wings).
-export([start/0,start/1,start_halt/1,start_halt/2]).
-export([root_dir/0,caption/1,redraw/1,init_opengl/1,command/2]).
-export([mode_restriction/1,clear_mode_restriction/0,get_mode_restriction/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,map/2,filter/2,foldl/3,sort/1,
		keymember/3,reverse/1]).

start() ->
    %% Only for development use.
    RootEbin = filename:dirname(filename:absname(code:which(?MODULE))),
    Split = filename:split(RootEbin),
    Root = filename:join(Split -- ["ebin"]),
    spawn(fun() ->
		  {ok,Cwd} = file:get_cwd(),
		  process_flag(trap_exit, true),
		  Wings = do_spawn(none, Root, [link]),
		  wait_for_exit(Wings, Cwd)
	  end).

wait_for_exit(Wings, Cwd) ->
    receive
	{'EXIT',Wings,_} ->
	    file:set_cwd(Cwd);
	_ ->					%Can't happen.
	    wait_for_exit(Wings, Cwd)
    end.

start(Root) ->
    do_spawn(none, Root).

start_halt(Root) ->
    spawn_halt(none, Root).

start_halt([File|_], Root) ->
    spawn_halt(File, Root).

spawn_halt(File, Root) ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  Wings = do_spawn(File, Root, [link]),
		  halt_loop(Wings)
		  end).

halt_loop(Wings) ->
    receive
	{'EXIT',Wings,normal} ->
	    halt();
	{'EXIT',Wings,{window_crash,Name,Reason}} ->
	    Log = wings_util:crash_log(Name, Reason),
	    io:format("\n\n"),
	    io:format("Fatal internal error - log written to ~s\n", [Log]),
	    ok;
	{'EXIT',Wings,Reason} ->
	    Log = wings_util:crash_log("<Unknown Window Name>", Reason),
	    io:format("\n\n"),
	    io:format("Fatal internal error - log written to ~s\n", [Log]),
	    ok;
	_Other ->				%Can't happen.
	    halt_loop(Wings)
    end.

do_spawn(File, Root) ->
    do_spawn(File, Root, []).

do_spawn(File, Root, Flags) ->
    %% Set a minimal heap size to avoiding garbage-collecting
    %% all the time. Don't set it too high to avoid keeping binaries
    %% too long.
    Fun = fun() -> init(File, Root) end,
    spawn_opt(erlang, apply, [Fun,[]],
	      [{fullsweep_after,16384},{min_heap_size,128*1204}|Flags]).

root_dir() ->
    get(wings_root_dir).

init(File, Root) ->
    register(wings, self()),
    os:putenv("SDL_HAS3BUTTONMOUSE", "true"),
    put(wings_os_type, os:type()),
    put(wings_root_dir, Root),
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER bor
	     ?SDL_INIT_NOPARACHUTE),
    Icon = locate("wings.icon"),
    catch sdl_video:wm_setIcon(sdl_video:loadBMP(Icon), null),
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
    sdl_events:eventState(?SDL_ALLEVENTS,?SDL_IGNORE),
    sdl_events:eventState(?SDL_ALLEVENTS,?SDL_IGNORE),
    sdl_events:eventState(?SDL_MOUSEMOTION, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_MOUSEBUTTONDOWN, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_MOUSEBUTTONUP, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEORESIZE, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEOEXPOSE, ?SDL_ENABLE),
    sdl_keyboard:enableUNICODE(true),
    sdl_keyboard:enableKeyRepeat(?SDL_DEFAULT_REPEAT_DELAY,
				 ?SDL_DEFAULT_REPEAT_INTERVAL),

    wings_pref:init(),
    wings_text:init(),
    wings_image:init(),
    wings_plugin:init(),
    wings_text:choose_font(),
    wings_color:init(),
    wings_io:init(),

    wings_camera:init(),
    wings_vec:init(),

    Empty = gb_trees:empty(),
    St0 = #st{shapes=Empty,
	      selmode=face,
	      sel=[],
	      ssels=Empty,
	      mat=wings_material:default(),
	      saved=true,
	      onext=1,
	      repeatable=ignore,
	      args=none,
	      def={ignore,ignore}
	     },
    St1 = wings_sel:reset(St0),
    St = wings_undo:init(St1),
    wings_view:init(),
    wings_file:init(),
    put(wings_hitbuf, sdl_util:malloc(?HIT_BUF_SIZE, ?GL_UNSIGNED_INT)),
    caption(St),
    wings_wm:init(),
    init_menubar(),
    Op = main_loop_noredraw(St),		%Replace crash handler
						%with this handler.
    Props = wings_view:initial_properties(),
    {{X,Y},{W,H}} = wings_wm:win_rect(desktop),
    wings_wm:toplevel(geom, "Geometry", {X,Y,1}, {W,H-20},
		      [resizable,{anchor,nw},{toolbar,fun create_toolbar/3},
		       {properties,Props}],
		      Op),
    open_file(File),

    restore_windows(St),
    wings_wm:current_state(St),
    case catch wings_wm:enter_event_loop() of
	{'EXIT',normal} ->
	    wings_file:finish(),
	    wings_pref:finish(),
	    sdl_util:free(get(wings_hitbuf)),
	    sdl:quit();
	{'EXIT',Reason} ->
	    io:format("~P\n", [Reason,20]),
	    sdl_util:free(get(wings_hitbuf)),
	    sdl:quit(),
	    exit(Reason)
    end.

new_viewer(St) ->
    {{X,Y},{W,H}} = wings_wm:win_rect(desktop),
    Op = main_loop_noredraw(St),
    N = free_viewer_num(2),
    Name = "Geometry #" ++ integer_to_list(N),
    Props = wings_view:initial_properties(),
    wings_wm:toplevel({geom,N}, Name, {X+20,Y+100,highest}, {W div 2-40,H div 2-40},
		      [resizable,closable,{anchor,nw},
		       {toolbar,fun create_toolbar/3},
		       {properties,Props}],
		      Op),
    keep.

free_viewer_num(N) ->
    case wings_wm:is_window({geom,N}) of
	false -> N;
	true -> free_viewer_num(N+1)
    end.

open_file(none) -> ok;
open_file(Name) -> wings_wm:send(geom, {open_file,Name}).

locate(Name) ->
    case filelib:is_file(Name) of
	true -> Name;
	false ->
	    Root = root_dir(),
	    Path = filename:join(Root, Name),
	    case filelib:is_file(Path) of
		true -> Path;
		false -> filename:join([Root,"ebin",Name])
	    end
    end.

init_opengl(_) ->
    wings_draw_util:init(),
    keep.

redraw(St) ->
    wings_draw_util:render(St),
    wings_io:info(info(St)).

clean_state(St) ->
    caption(St).

save_state(St0, St1) ->
    St = wings_undo:save(St0, St1#st{vec=none}),
    wings_wm:current_state(St),
    case St of
	#st{saved=false} -> main_loop(St);
	_Other -> main_loop(caption(St#st{saved=false}))
    end.

main_loop(St) ->
    ?VALIDATE_MODEL(St),
    clear_mode_restriction(),
    wings_draw:update_dlists(St),
    wings_wm:current_state(St),
    wings_wm:dirty(),
    main_loop_noredraw(St).

main_loop_noredraw(St) ->
    {replace,fun(Event) -> handle_event(Event, St) end}.

handle_event({crash,_}=Crash, St) ->
    crash_logger(Crash, St);
handle_event({open_file,Name}, St0) ->
    case catch ?SLOW(wings_ff_wings:import(Name, St0)) of
	#st{}=St ->
	    wings_pref:set_value(current_directory, filename:dirname(Name)),
	    main_loop(caption(St#st{saved=true,file=Name}));
	{error,_} ->
	    main_loop(St0)
    end;
handle_event(Ev, St) ->
    case wings_camera:event(Ev, St) of
	next -> handle_event_0(Ev, St);
	Other -> Other
    end.

handle_event_0(#mousebutton{button=But,state=ButSt}=Ev, St) ->
    case wings_pref:get_value(default_commands) of
	false -> handle_event_1(Ev, St);
	true ->
	    Mod = wings_wm:me_modifiers(),
	    case But of
		But when But < 3, Mod band ?CTRL_BITS =/= 0,
			 Mod band ?SHIFT_BITS =/= 0 ->
		    define_command(ButSt, But, St);
		But when But < 3, Mod band ?CTRL_BITS =/= 0 ->
		    use_command(ButSt, But, St);
		_ ->
		    handle_event_1(Ev, St)
	    end
    end;
handle_event_0(Ev, St) -> handle_event_1(Ev, St).

handle_event_1(Ev, St) ->
    case wings_pick:event(Ev, St) of
	next -> handle_event_2(Ev, St);
	Other -> Other
    end.

handle_event_2(Event, St) ->
    case wings_menu:is_popup_event(Event, right_click_sel_in_geom, St) of
	no -> handle_event_3(Event, St);
	{yes,X,Y,_} -> popup_menu(X, Y, St);
	Other -> Other
    end.
	    
handle_event_3(#keyboard{}=Event, St) ->
    case wings_hotkey:event(Event, St) of
	next -> keep;
	Cmd -> do_command(Cmd, St)
    end;
handle_event_3({action,Cmd}, St) ->
    do_command(Cmd, St);
handle_event_3({action,Cmd,Args}, St) ->
    do_command(Cmd, Args, St);
handle_event_3(#mousebutton{}, _St) -> keep;
handle_event_3(#mousemotion{}, _St) -> keep;
handle_event_3(init_opengl, St) ->
    init_opengl(St);
handle_event_3(#expose{}, St) ->
    handle_event_3(redraw, St);
handle_event_3(resized, _) -> keep;
handle_event_3(redraw, St) ->
    wings_draw_util:render(St),
    wings_io:info(info(St)),
    main_loop_noredraw(St#st{vec=none});
handle_event_3(quit, St) ->
    do_command({file,quit}, St);
handle_event_3({new_state,St}, St0) ->
    wings_wm:dirty(),
    save_state(St0, St);
handle_event_3({current_state,St}, _) ->
    main_loop_noredraw(St);
handle_event_3(revert_state, St) ->
    main_loop(St);
handle_event_3(got_focus, _) ->
    {One,_,Three} = wings_camera:button_names(),
    Message = [One," Select  ",Three," Show menu  "|wings_camera:help()],
    wings_wm:message(Message),
    wings_wm:dirty();
handle_event_3(lost_focus, _) -> keep;
handle_event_3({note,_}, _) -> keep;
handle_event_3(ignore, _St) -> keep.

do_command(Cmd, St) ->    
    do_command(Cmd, none, St).
    
do_command(Cmd, Args, St0) ->
    St1 = remember_command(Cmd, St0),
    Res = (catch do_command_1(Cmd, St1)),
    case Res of
	{'EXIT',Reason} -> exit(Reason);
	{command_error,Error} -> wings_util:message(Error);
	#st{}=St -> main_loop(St);
	{drag,Drag} -> wings_drag:do_drag(Drag, Args);
	{save_state,#st{}=St} -> save_state(St1, St);
	{saved,St}=Res ->
	    main_loop(wings_undo:save(St1, St));
	{new,St}=Res -> main_loop(clean_state(wings_undo:init(St)));
	{push,_}=Push -> Push;
	{init,_,_}=Init -> Init;
	{seq,_,_}=Seq -> Seq;
	keep -> keep;
	quit ->
	    save_windows(),
	    exit(normal)
    end.

do_command_1(Cmd, St0) ->
    case wings_plugin:command(Cmd, St0) of
	next -> command(Cmd, St0);
	St0 -> St0;
	#st{}=St -> {save_state,St};
	Other -> Other
    end.

remember_command({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
				     C =:= face; C =:= body ->
    St#st{repeatable=Cmd,args=none};
remember_command(_Cmd, St) -> St.

%% Test if the saved command can be safely repeated, and
%% rewrite it with the current selection mode if needed.
repeatable(Mode, Cmd) ->
    case Cmd of
	{Mode,_} -> Cmd;			%Same mode is always OK.

	%% Commands safe in all modes.
	{_,{move,normal}} when Mode == body -> no;
	{_,{move,_}=C} -> {Mode,C};
	{_,{rotate,normal}} when Mode == body -> no;
	{_,{rotate,_}=C} -> {Mode,C};
	{_,{scale,_}=C} -> {Mode,C};

	%% Some special cases.
	{_,tighten=C} when Mode == vertex; Mode == body -> {Mode,C};
	{_,smooth=C} when Mode == face; Mode == body -> {Mode,C};
	
	%% No more commands are safe in body mode.
	{_,_} when Mode == body -> no;
	{_,{flatten,_}=C} when Mode == vertex; Mode == face -> {Mode,C};
	{_,dissolve} when Mode == vertex -> no;
	{_,dissolve=C} -> {Mode,C};
	{_,bevel=C} -> {Mode,C};
	{_,{extrude,_}=C} -> {Mode,C};
	{_,collapse=C} -> {Mode,C};

	%% Other special commands.
	{_,connect} when Mode == face -> no;
	{_,connect=C} -> {Mode,C};

	%% Other commands only work in the saved mode.
	_ -> no
    end.

%% Vector and secondary-selection commands.
command({vector,What}, St) ->
    wings_vec:command(What, St);
command({secondary_selection,aborted}, St) -> St;
command({shape,Shape}, St0) ->
    case wings_shapes:command(Shape, St0) of
    	St0 -> St0;
	#st{}=St -> {save_state,St};
	Other -> Other
    end;
command({help,What}, St) ->
    wings_help:command(What, St);

%% File menu.
command({file,Command}, St) ->
    wings_file:command(Command, St);

%% Edit menu.
command({edit,undo_toggle}, St) ->
    clean_state(wings_undo:undo_toggle(St));
command({edit,undo}, St) ->
    clean_state(wings_undo:undo(St));
command({edit,redo}, St) ->
    clean_state(wings_undo:redo(St));
command({edit,repeat}, #st{sel=[]}=St) -> St;
command({edit,repeat}, #st{selmode=Mode,repeatable=Cmd0}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> ok;
	Cmd when tuple(Cmd) ->
	    wings_wm:later({action,Cmd})
    end,
    St;
command({edit,repeat}, St) -> St;
command({edit,repeat_drag}, #st{sel=[]}=St) -> St;
command({edit,repeat_drag}, #st{selmode=Mode,repeatable=Cmd0,args=Args}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> ok;
	Cmd when tuple(Cmd) ->
	    wings_wm:later({action,Cmd,Args})
    end,
    St;
command({edit,repeat_drag}, St) -> St;
command({edit,camera_mode}, St) ->
    wings_camera:command(camera_mode, St);
command({edit,font}, St) ->
    wings_text:command(font, St);
command({edit,purge_undo}, St) ->
    wings_undo:purge(St);
command({edit,enable_patches}, St) ->
    wings_start:enable_patches(),
    St;
command({edit,disable_patches}, St) ->
    wings_start:disable_patches(),
    St;
command({edit,{_,Pref}}, St) ->
    wings_pref:command(Pref, St);
command({edit,{preferences,Pref}}, St) ->
    wings_pref:command(Pref, St);

%% Select menu.
command({select,Command}, St) ->
    wings_sel_cmd:command(Command, St);

%% View menu.
command({view,Command}, St) ->
    wings_view:command(Command, St);

%% Window menu.
command({window,geom_viewer}, St) ->
    new_viewer(St);
command({window,outliner}, St) ->
    wings_outliner:window(St);
command({window,object}, St) ->
    wings_shape:window(St);

%% Body menu.
command({body,Cmd}, St) ->
    wings_body:command(Cmd, St);

%% Face menu.
command({face,Cmd}, St) ->
    wings_face_cmd:command(Cmd, St);

%% Edge commands.
command({edge,Cmd}, St) ->
    wings_edge:command(Cmd, St);

%% Vertex menu.
command({vertex,Cmd}, St) ->
    wings_vertex_cmd:command(Cmd, St);

%% Light menu.
command({light,Cmd}, St) ->
    wings_light:command(Cmd, St);

%% Material commands.
command({material,Cmd}, St) ->
    wings_material:command(Cmd, St);

%% Tools menu.

command({tools,set_default_axis}, St) ->
    Cmd = {pick,[axis,point],[],[set_default_axis,tools]},
    wings_vec:command(Cmd, St);
command({tools,{set_default_axis,{Axis,Point}}}, St) ->
    wings_pref:set_value(default_axis, {Point,Axis}),
    St;
command({tools,{align,Dir}}, St) ->
    {save_state,wings_align:align(Dir, St)};
command({tools,{center,Dir}}, St) ->
    {save_state,wings_align:center(Dir, St)};
command({tools,save_bb}, St) ->
    wings_align:copy_bb(St);
command({tools,{scale_to_bb,Dir}}, St) ->
    {save_state,wings_align:scale_to_bb(Dir, St)};
command({tools,{scale_to_bb_prop,Dir}}, St) ->
    {save_state,wings_align:scale_to_bb_prop(Dir, St)};
command({tools,{move_to_bb,Dir}}, St) ->
    {save_state,wings_align:move_to_bb(Dir, St)};
command({tools,{virtual_mirror,Cmd}}, St) ->
    wings_view:virtual_mirror(Cmd, St).

popup_menu(X, Y, #st{sel=[]}=St) ->
    wings_shapes:menu(X, Y, St);
popup_menu(X, Y, #st{selmode=Mode}=St) ->
    case wings_light:is_any_light_selected(St) of
	true -> wings_light:menu(X, Y, St);
	false ->
	    case Mode of
		vertex -> wings_vertex_cmd:menu(X, Y, St);
		edge -> wings_edge:menu(X, Y, St);
		face -> wings_face_cmd:menu(X, Y, St);
		body -> wings_body:menu(X, Y, St)
	    end
    end.

init_menubar() ->
    Menus = [{"File",file,fun(St) -> wings_file:menu(St) end},
	     {"Edit",edit,fun edit_menu/1},
	     {"View",view,fun(St) -> wings_view:menu(St) end},
	     {"Select",select,fun(St) -> wings_sel_cmd:menu(St) end},
	     {"Tools",tools,fun tools_menu/1},
	     {"Window",window,fun window_menu/1},
	     {"Help",help,fun(St) -> wings_help:menu(St) end}],
    wings_wm:menubar(geom, Menus).

edit_menu(St) ->
    [{"Undo/Redo",undo_toggle},
     {"Redo",redo},
     {"Undo",undo},
     separator,
     {command_name("Repeat", St),repeat},
     {command_name("Repeat Drag", St),repeat_drag},
     separator|wings_camera:sub_menu(St)++wings_text:sub_menu(St)++
     [separator|wings_pref:menu(St)++
      [separator,{"Purge Undo History",purge_undo}|patches()]]].

tools_menu(_) ->
    Dirs = [{"All",all},
	    {"X",x},
	    {"Y",y},
	    {"Z",z},
	    {"Radial X",radial_x},
	    {"Radial Y",radial_y},
	    {"Radial Z",radial_z}],
    [{"Align",{align,Dirs}},
     {"Center",{center,Dirs}},
     separator,
     {"Save Bounding Box",save_bb},
     {"Scale to Saved BB",{scale_to_bb,Dirs}},
     {"Scale to Saved BB Proportionally",{scale_to_bb_prop,Dirs}},
     {"Move to Saved BB",{move_to_bb,wings_menu_util:all_xyz()}},
     separator,
     {"Set Default Axis",set_default_axis},
     separator,
     {"Virtual Mirror",
      {virtual_mirror,
       [{"Create",create,
	 "Given a face selection, set up a virtual mirror"},
	{"Break",break,
	 "Remove virtual mirrors for all objects"},
	{"Freeze",freeze,
	 "Create real geometry from the virtual mirrors"}]}}].

window_menu(_) ->
    [{"Outliner",outliner,[],win_crossmark(outliner)},
     {"Objects",object,[],win_crossmark(object)},
     separator,
     {"Geometry",geom_viewer}].

win_crossmark(Name) ->
    case wings_wm:is_window(Name) of
	false -> [];
	true -> [crossmark]
    end.

patches() ->
    case wings_start:get_patches() of
	none -> [];
	{enabled,Desc} ->
	    [separator,{"Use "++Desc,disable_patches,[crossmark]}];
	{disabled,Desc} ->
	    [separator,{"Use "++Desc,enable_patches}]
    end.

info(#st{sel=[]}) -> [];
info(#st{shapes=Shapes,selmode=body,sel=[{Id,_}]}) ->
    Sh = gb_trees:get(Id, Shapes),
    shape_info(Sh);
info(#st{shapes=Shapes,selmode=body,sel=Sel}) ->
    shape_info(Sel, Shapes);
info(#st{selmode=vertex,sel=[{_Id,Sel}]}=St) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [V] = gb_sets:to_list(Sel),
	    measure(io_lib:format("Vertex ~p selected", [V]), St);
	N when N < 5 ->
	    Vs = gb_sets:to_list(Sel),
	    measure(item_list(Vs, "Vertices"), St);
	N ->
	    io_lib:format("~p vertices selected", [N])
    end;
info(#st{selmode=edge,sel=[{_,Sel}]}=St) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [Edge] = gb_sets:to_list(Sel),
	    measure(io_lib:format("Edge ~p selected", [Edge]), St);
	N when N < 5 ->
	    Edges = gb_sets:to_list(Sel),
	    item_list(Edges, "Edges");
	N ->
	    io_lib:format("~p edges selected", [N])
    end;
info(#st{selmode=face,sel=[{_,Sel}]}=St) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [Face] = gb_sets:to_list(Sel),
	    measure(io_lib:format("Face ~p selected", [Face]), St);
	N when N < 5 ->
	    Faces = gb_sets:to_list(Sel),
	    item_list(Faces, "Faces");
	N ->
	    io_lib:format("~p faces selected", [N])
    end;
info(#st{selmode=Mode,sel=Sel}=St) ->
    On = length(Sel),
    N = foldl(fun({_,S}, A) -> A+gb_sets:size(S) end, 0, Sel),
    Str = case Mode of
	      vertex ->
		  io_lib:format("~p vertices selected in ~p objects", [N,On]);
	      edge ->
		  io_lib:format("~p edges selected in ~p objects", [N,On]);
	      face ->
		  io_lib:format("~p faces selected in ~p objects", [N,On])
	  end,
    measure(Str, St).

measure(Base, #st{selmode=vertex,sel=[{Id,Vs}],shapes=Shs}) ->
    case gb_sets:size(Vs) of
	1 ->
	    We = gb_trees:get(Id, Shs),
 	    [Va] = gb_sets:to_list(Vs),
	    {X,Y,Z} = wings_vertex:pos(Va, We),
	    [Base|io_lib:format(". Position ~s ~s ~s",
				[wings_util:nice_float(X),
				 wings_util:nice_float(Y),
				 wings_util:nice_float(Z)])];
	2 ->
	    We = gb_trees:get(Id, Shs),
 	    [Va,Vb] = gb_sets:to_list(Vs),
 	    Dist = e3d_vec:dist(wings_vertex:pos(Va, We),
				wings_vertex:pos(Vb, We)),
	    [Base|io_lib:format(". Distance ~s",
				[wings_util:nice_float(Dist)])];
	_ -> Base
    end;
measure(Base, #st{selmode=vertex,sel=[{IdA,VsA},{IdB,VsB}],shapes=Shs}) ->
    case gb_sets:size(VsA) == 1 andalso gb_sets:size(VsB) == 1 of
	false -> Base;
	true ->
	    WeA = gb_trees:get(IdA, Shs),
	    WeB = gb_trees:get(IdB, Shs),
 	    [Va] = gb_sets:to_list(VsA),
 	    [Vb] = gb_sets:to_list(VsB),
 	    Dist = e3d_vec:dist(wings_vertex:pos(Va, WeA),
				wings_vertex:pos(Vb, WeB)),
	    [Base|io_lib:format(". Distance ~s",
				[wings_util:nice_float(Dist)])];
	_ -> Base
    end;
measure(Base, #st{selmode=edge,sel=[{Id,Es}],shapes=Shs}) ->
    case gb_sets:size(Es) of
	1 ->
	    We = gb_trees:get(Id, Shs),
 	    [Edge] = gb_sets:to_list(Es),
	    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, We#we.es),
	    PosA = wings_vertex:pos(Va, We),
	    PosB = wings_vertex:pos(Vb, We),
 	    Dist = e3d_vec:dist(PosA, PosB),
	    {X,Y,Z} = e3d_vec:average([PosA,PosB]),
	    [Base|io_lib:format(". Midpt ~s ~s ~s. Length ~s",
				[wings_util:nice_float(X),
				 wings_util:nice_float(Y),
				 wings_util:nice_float(Z),
				 wings_util:nice_float(Dist)])];
	_ -> Base
    end;
measure(Base, #st{selmode=face,sel=[{Id,Fs}],shapes=Shs}) ->
    case gb_sets:size(Fs) of
	1 ->
	    #we{fs=Ftab} = We = gb_trees:get(Id, Shs),
 	    [Face] = gb_sets:to_list(Fs),
	    Vs = wings_face:surrounding_vertices(Face, We),
	    {X,Y,Z} = wings_vertex:center(Vs, We),
	    #face{mat=Mat} = gb_trees:get(Face, Ftab),
	    [Base|io_lib:format(". Midpt ~s ~s ~s. Material ~s",
				[wings_util:nice_float(X),
				 wings_util:nice_float(Y),
				 wings_util:nice_float(Z),
				 Mat])];
	_ -> Base
    end;
measure(Base, _) -> Base.

item_list(Items, Desc) ->
    item_list(Items, " ", Desc).

item_list([Item|Items], Sep, Desc) ->
    item_list(Items, ", ", [Desc,Sep|integer_to_list(Item)]);
item_list([], _Sep, Desc) -> [Desc|" selected"].

shape_info(We) when ?IS_LIGHT(We) ->
    wings_light:info(We);
shape_info(#we{name=Name,fs=Ftab,es=Etab,vp=Vtab,mode=Mode}) ->
    Faces = gb_trees:size(Ftab),
    Edges = gb_trees:size(Etab),
    Vertices = gb_trees:size(Vtab),
    io_lib:format("Object ~s has ~p polygons, ~p edges, ~p vertices. Mode is ~p",
		  [Name,Faces,Edges,Vertices,Mode]).

shape_info(Objs, Shs) ->
    shape_info(Objs, Shs, 0, 0, 0, 0).

shape_info([{Id,_}|Objs], Shs, On, Vn, En, Fn) ->
    #we{fs=Ftab,es=Etab,vp=Vtab} = gb_trees:get(Id, Shs),
    Faces = gb_trees:size(Ftab),
    Edges = gb_trees:size(Etab),
    Vertices = gb_trees:size(Vtab),
    shape_info(Objs, Shs, On+1, Vn+Vertices, En+Edges, Fn+Faces);
shape_info([], _Shs, N, Vertices, Edges, Faces) ->
    io_lib:format("~p objects, ~p faces, ~p edges, ~p vertices",
		[N,Faces,Edges,Vertices]).

caption(#st{file=undefined}=St) ->
    Caption = wings(),
    sdl_video:wm_setCaption(Caption, Caption),
    St;
caption(#st{saved=true,file=Name}=St) ->
    Caption = wings() ++ " - " ++ filename:basename(Name),
    sdl_video:wm_setCaption(Caption, Caption),
    St;
caption(#st{saved=auto,file=Name}=St) ->
    Caption = wings() ++ " - " ++ filename:basename(Name) ++ "* [auto-saved]",
    sdl_video:wm_setCaption(Caption, Caption),
    St;
caption(#st{file=Name}=St) ->
    Caption = wings() ++ " - " ++ filename:basename(Name) ++ "*",
    sdl_video:wm_setCaption(Caption, Caption),
    St.

command_name(_Repeat, #st{repeatable=ignore}) ->
    "(Can't repeat)";
command_name(Repeat, #st{repeatable={_,Cmd}}=St) ->
    CmdStr = wings_util:stringify(Cmd),
    command_name(Repeat, CmdStr, St).

command_name(_Repeat, CmdStr, #st{sel=[]}) ->
    lists:flatten(["(Can't repeat \"",CmdStr,"\")"]);
command_name(Repeat, CmdStr, #st{selmode=Mode,repeatable=Cmd}) ->
    S = case repeatable(Mode, Cmd) of
	    no -> ["(Can't repeat \"",CmdStr,"\")"];
	    _ ->  [Repeat++" \"",CmdStr,"\""]
	end,
    lists:flatten(S).

define_command(?SDL_RELEASED, N, #st{repeatable=Cmd,def=DefCmd0}=St) ->
    CmdStr = wings_util:stringify(Cmd),
    Button = case N of
		 1 -> "L";
		 2 -> "M"
	     end,
    case wings_util:yes_no("Do you want to define \"" ++ CmdStr ++
			   "\" as a default command ([Ctrl]+[" ++ Button ++
			   "])?") of
	no -> keep;
	aborted -> keep;
	yes ->
	    DefCmd = setelement(N, DefCmd0, Cmd),
	    main_loop_noredraw(St#st{def=DefCmd})
    end;
define_command(_, _, _) -> keep.

use_command(_, _, #st{sel=[]}) -> keep;
use_command(?SDL_RELEASED, N, #st{selmode=Mode,def=DefCmd}) ->
    case repeatable(Mode, element(N, DefCmd)) of
	no -> keep;
	Cmd when tuple(Cmd) ->
	    wings_wm:later({action,Cmd}),
	    keep
    end;
use_command(_, _, _) -> keep.

crash_logger(Crash, St) ->
    LogName = wings_util:crash_log(geom, Crash),
    get_crash_event(LogName, St).

get_crash_event(Log, St) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> crash_handler(Ev, Log, St) end}.

crash_handler(redraw, Log, _St) ->
    wings_io:ortho_setup(),
    wings_io:text_at(10, 2*?LINE_HEIGHT,
		     "Internal error - log written to " ++ Log),
    wings_io:text_at(10, 4*?LINE_HEIGHT,
		     "Click a mouse button to continue working"),
    wings_wm:message("[L] Continue working", ""),
    keep;
crash_handler(#mousebutton{}, _, St) ->
    init_menubar(),
    main_loop(St);
crash_handler(_, Log, St) ->
    get_crash_event(Log, St).

-ifdef(DEBUG).
wings() -> "Wings 3D [debug]".
-else.
wings() -> "Wings 3D".
-endif.

%%%
%%% Saving and restoring of window layouts.
%%%
save_windows() ->
    {Pos,Size} = wings_wm:win_rect(),
    ToolbarHidden = wings_wm:is_hidden({toolbar,geom}),
    Geom = {geom,Pos,Size,ToolbarHidden},
    Wins = [Geom|save_windows_1([outliner,object])],
    wings_pref:set_value(saved_windows, Wins).

save_windows_1([N|Ns]) ->
    case wings_wm:is_window(N) of
	false -> save_windows_1(Ns);
	true ->
	    Pos = wings_wm:win_ur({controller,N}),
	    Size = wings_wm:win_size(N),
	    [{N,Pos,Size}|save_windows_1(Ns)]
    end;
save_windows_1([]) -> [].

restore_windows(St) ->
    restore_windows_1(wings_pref:get_value(saved_windows, []), St).

restore_windows_1([{geom,{_,_}=Pos0,{_,_}=Size,ToolbarHidden}|Ws], St) ->
    if
	ToolbarHidden -> wings_wm:hide({toolbar,geom});
	true -> ok
    end,
    Pos = geom_pos(Pos0),
    wings_wm:move(geom, Pos, Size),
    restore_windows_1(Ws, St);
restore_windows_1([{object,{_,_}=Pos,{_,_}=Size}|Ws], St) ->
    wings_shape:window(Pos, Size, St),
    restore_windows_1(Ws, St);
restore_windows_1([{outliner,{_,_}=Pos,{_,_}=Size}|Ws], St) ->
    wings_outliner:window(Pos, Size, St),
    restore_windows_1(Ws, St);
restore_windows_1([_|Ws], St) ->
    restore_windows_1(Ws, St);
restore_windows_1([], _) -> ok.

geom_pos({X,Y}=Pos) ->
    {_,Upper0} = wings_wm:win_ul(desktop),
    Upper1 = case wings_wm:is_hidden({toolbar,geom}) of
		 true -> Upper0;
		 false ->
		     {_,ToolbarH} = wings_wm:win_size({toolbar,geom}),
		     Upper0+ToolbarH
	     end,
    {_,TitleH} = wings_wm:win_size({controller,geom}),
    case Upper1 + TitleH of
	Upper when Y < Upper -> {X,Upper};
	_ -> Pos
    end.

%%%
%%% The toolbar window with buttons.
%%%

-define(BUTTON_WIDTH, 34).
-define(BUTTON_HEIGHT, 28).

-record(but,
	{mode,					%Selection mode.
	 sh,					%Smart highlighting (true|false).
	 buttons,				%Buttons to show.
	 all_buttons,				%All buttons.
	 restr=none				%Restriction (none|[Mode]).
	}).

mode_restriction(none) ->
    put(wings_mode_restriction, [edge,vertex,face,body]),
    wings_wm:send({toolbar,geom}, {mode_restriction,none});
mode_restriction(Modes) ->
    put(wings_mode_restriction, Modes),
    wings_wm:send({toolbar,geom}, {mode_restriction,Modes}).

clear_mode_restriction() ->
    mode_restriction(none).

get_mode_restriction() ->
    get(wings_mode_restriction).

create_toolbar(Name, Pos, W) ->
    ButtonH = ?BUTTON_HEIGHT+6,
    wings_wm:new(Name, Pos, {W,ButtonH}, init_button()).

init_button() ->
    {seq,push,get_button_event(#but{mode=face})}.

get_button_event(But) ->
    {replace,fun(Ev) -> button_event(Ev, But) end}.

button_event(got_focus, _) ->
    wings_wm:dirty(),
    keep;
button_event({window_updated,_}, But) ->
    get_button_event(button_resized(But));
button_event(redraw, #but{buttons=undefined}=But0) ->
    But = button_resized(But0),
    button_redraw(But),
    get_button_event(But);
button_event(redraw, But) ->
    button_redraw(But),
    keep;
button_event(#mousebutton{button=B,x=X,state=?SDL_PRESSED}, But) when B =< 3 ->
    button_was_hit(X, But),
    keep;
button_event(#mousebutton{button=B,x=X,state=?SDL_RELEASED}, But) when B =< 3 ->
    button_help(X, But),
    keep;
button_event(#mousemotion{x=X}, But) ->
    button_help(X, But),
    keep;
button_event({action,_}=Action, _) ->
    {toolbar,Client} = wings_wm:active_window(),
    wings_wm:send(Client, Action);
button_event({current_state,#st{selmode=Mode,sh=Sh}}, #but{mode=Mode,sh=Sh}) ->
    keep;
button_event({current_state,#st{selmode=Mode,sh=Sh}}, But) ->
    wings_wm:dirty(),
    get_button_event(But#but{mode=Mode,sh=Sh});
button_event({mode_restriction,Restr}, #but{restr=Restr}) ->
    keep;
button_event({mode_restriction,Restr}, #but{all_buttons=AllButtons}=But) ->
    Buttons = button_restrict(AllButtons, Restr),
    wings_wm:dirty(),
    get_button_event(But#but{buttons=Buttons,restr=Restr});
button_event(#keyboard{}=Ev, _) ->
    {toolbar,Client} = wings_wm:active_window(),
    wings_wm:send(Client, Ev);
button_event(_, _) -> keep.

button_resized(#but{restr=Restr}=But) ->
    {toolbar,Client} = Self = wings_wm:active_window(),
    {{X,Y},{W,_}} = wings_wm:win_rect(Client),
    {_,H} = wings_wm:win_size(),
    wings_wm:update_window(Self, [{x,X},{y,Y-H},{w,W}]),
    AllButtons = buttons_place(W),
    Buttons = button_restrict(AllButtons, Restr),
    But#but{buttons=Buttons,all_buttons=AllButtons}.

button_redraw(#but{mode=Mode,buttons=Buttons,sh=Sh0}) ->
    Sh = button_sh_filter(Mode, Sh0),
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0.5, W-0.5, H-1.5, ?PANE_COLOR),
    gl:color3f(0.20, 0.20, 0.20),
    gl:'begin'(?GL_LINES),
    gl:vertex2f(0.5, 0.5),
    gl:vertex2f(W, 0.5),
    gl:'end'(),
    gl:color3f(0, 0, 0),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    foreach(fun({X,Name}) ->
		    wings_io:draw_icon(X, 3, button_value(Name, Mode, Sh))
	    end, Buttons),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D),
    button_redraw_sh(Sh, Buttons).

button_redraw_sh(false, _) -> ok;
button_redraw_sh(true, Buttons) ->
    Pos = [X || {X,M} <- Buttons, button_sh_filter(M, true)],
    case Pos of
	[] -> ok;
	[Left|_] ->
	    gl:pushAttrib(?GL_POLYGON_BIT bor ?GL_LINE_BIT),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:lineStipple(2, 2#0101010101010101),
	    gl:enable(?GL_LINE_STIPPLE),
	    Right = lists:last(Pos),
	    gl:color3f(1, 1, 1),
	    gl:recti(Left-1, 3, Right+?BUTTON_WIDTH, 3+?BUTTON_HEIGHT),
	    gl:popAttrib()
    end.

button_sh_filter(_, false) -> false;
button_sh_filter(vertex, true) ->
    wings_pref:get_value(vertex_hilite);
button_sh_filter(edge, true) ->
    wings_pref:get_value(edge_hilite);
button_sh_filter(face, true) ->
    wings_pref:get_value(face_hilite);
button_sh_filter(_, _) -> false.

buttons_place(W) when W < 325 ->
    Mid = (W - ?BUTTON_WIDTH) div 2,
    [{Mid-trunc(1.5*?BUTTON_WIDTH),vertex},
     {Mid-trunc(0.5*?BUTTON_WIDTH),edge},
     {Mid+trunc(0.5*?BUTTON_WIDTH),face},
     {Mid+trunc(1.5*?BUTTON_WIDTH),body}];
buttons_place(W) ->
    Mid = (W - ?BUTTON_WIDTH) div 2,
    Lmarg = 5,
    Rmarg = 5,
    [{Lmarg,smooth},{Lmarg+?BUTTON_WIDTH,perspective},
     {Mid-trunc(1.5*?BUTTON_WIDTH),vertex},
     {Mid-trunc(0.5*?BUTTON_WIDTH),edge},
     {Mid+trunc(0.5*?BUTTON_WIDTH),face},
     {Mid+trunc(1.5*?BUTTON_WIDTH),body},
     {W-2*?BUTTON_WIDTH-Rmarg,groundplane},
     {W-?BUTTON_WIDTH-Rmarg,axes}].

button_value(groundplane=Name, _, _) ->
    button_value_1(Name, show_groundplane, true);
button_value(axes=Name, _, _) ->
    button_value_1(Name, show_axes, true);
button_value(smooth=Name, _, _) ->
    button_value_1(Name, workmode, false);
button_value(perspective=Name, _, _) ->
    button_value_1(Name, orthogonal_view, true);
button_value(Mode, Mode, false) -> {Mode,down};
button_value(Name, _, _) -> {Name,up}.

button_value_1(Name, Key, Val) ->
    {toolbar,Client} = wings_wm:active_window(),
    case wings_wm:get_prop(Client, Key) of
	Val -> {Name,down};
	_ -> {Name,up}
    end.

button_was_hit(X, #but{buttons=Buttons}) ->
    button_was_hit_1(X, Buttons).

button_was_hit_1(X, [{Pos,Name}|_]) when Pos =< X, X < Pos+?BUTTON_WIDTH ->
    Action = case Name of
		 groundplane -> {view,show_groundplane};
		 axes -> {view,show_axes};
		 flatshade -> {view,flatshade};
		 smooth -> {view,workmode};
		 perspective -> {view,orthogonal_view};
		 Other -> {select,Other}
	     end,
    {toolbar,Client} = wings_wm:active_window(),
    wings_wm:send(Client, {action,Action});
button_was_hit_1(X, [_|Is]) ->
    button_was_hit_1(X, Is);
button_was_hit_1(_X, []) ->
    wings_wm:send(geom, {action,{select,deselect}}).

button_help(X, #but{mode=Mode,buttons=Buttons}) ->
    wings_wm:message(button_help_1(X, Buttons, Mode)).

button_help_1(X, [{Pos,Name}|_], Mode) when Pos =< X, X < Pos+?BUTTON_WIDTH ->
    button_help_2(Name, Mode);
button_help_1(X, [_|Is], Mode) ->
    button_help_1(X, Is, Mode);
button_help_1(_, [], _) ->
    "Deselect".

button_help_2(vertex, vertex) -> "Select adjacent vertices";
button_help_2(vertex, _) -> "Change to vertex selection mode";
button_help_2(edge, edge) -> "Select adjcacent edges";
button_help_2(edge, _) -> "Change to edge selection mode";
button_help_2(face, face) -> "Select adjacent faces";
button_help_2(face, _) -> "Change to face selection mode";
button_help_2(body, body) -> "";
button_help_2(body, _) -> "Change to body selection mode";
button_help_2(Button, _) -> button_help_3(Button).

button_help_3(groundplane) ->
    [choose(show_groundplane, true, "Hide", "Show")|" ground plane"];
button_help_3(axes) ->
    [choose(show_axes, true, "Hide", "Show")|" axes"];
button_help_3(perspective) ->
    ["Change to ",choose(orthogonal_view, false,
			 "orthogonal", "perspective")|" view"];
button_help_3(smooth) ->
    ["Show objects with ",choose(workmode, true,
				 "smooth", "flat")|" shading"].

button_restrict(Buttons, none) -> Buttons;
button_restrict(Buttons0, Restr) ->
    Buttons1 = sofs:from_external(Buttons0, [{atom,atom}]),
    Buttons = sofs:restriction(2, Buttons1, sofs:set(Restr)),
    sofs:to_external(Buttons).

choose(Key, Val, First, Second) ->
    {toolbar,Client} = wings_wm:active_window(),
    case wings_wm:get_prop(Client, Key) of
	Val -> First;
	_ -> Second
    end.
