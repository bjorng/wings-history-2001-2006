%%
%%  wings.erl --
%%
%%     The main module of Wings 3D.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings.erl,v 1.155 2002/08/18 08:43:06 bjorng Exp $
%%

-module(wings).
-export([start/0,start/1,start_halt/1,start_halt/2]).
-export([root_dir/0,caption/1,redraw/1]).

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
	{'EXIT',Wings,_} ->
	    ok;
	_ ->					%Can't happen.
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
    put(wings_root_dir, Root),
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER bor
	     ?SDL_INIT_NOPARACHUTE),
    Icon = locate("wings.icon"),
    catch sdl_video:wm_setIcon(sdl_video:loadBMP(Icon), null),
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
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
    wings_plugin:init(),
    wings_color:init(),
    wings_io:init(),

    wings_io:menubar([{"File",file},
		      {"Edit",edit},
		      {"View",view},
		      {"Select",select},
		      {"Tools",tools},
		      {"Objects",objects},
		      {"Help",help}]),

    wings_vec:init(),

    Empty = gb_trees:empty(),
    St0 = #st{shapes=Empty,
	      selmode=face,
	      sel=[],
	      ssel={face,[]},
	      mat=wings_material:default(),
	      saved=true,
	      onext=1,
	      repeatable=ignore,
	      args=none
	    },
    St = wings_undo:init(St0),
    wings_view:init(),
    wings_file:init(),
    put(wings_hitbuf, sdl_util:malloc(?HIT_BUF_SIZE, ?GL_UNSIGNED_INT)),
    caption(St),
    wings_wm:init(),
    open_file(File),
    case catch wings_wm:top_window(main_loop(St)) of
	{'EXIT',normal} ->
	    wings_file:finish(),
	    wings_pref:finish(),
	    sdl_util:free(get(wings_hitbuf)),
	    sdl:quit();
	{'EXIT',_} ->
	    sdl_util:free(get(wings_hitbuf)),
	    sdl:quit()
    end.

open_file(none) -> ok;
open_file(Name) -> wings_io:putback_event({open_file,Name}).

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

resize(W, H, St) ->
    wings_io:resize(W, H),
    wings_draw_util:init(),
    wings_material:init(St).

redraw(St0) ->
    St = wings_draw:render(St0),
    wings_io:info(info(St)),
    wings_io:update(St).

clean_state(St) ->
    caption(St).

save_state(St0, St1) ->
    St = wings_undo:save(St0, St1#st{vec=none}),
    case St of
	#st{saved=false} -> main_loop(St);
	_Other -> main_loop(caption(St#st{saved=false}))
    end.

main_loop(St) ->
    ?VALIDATE_MODEL(St),
    wings_io:clear_icon_restriction(),
    wings_wm:dirty(),
    main_loop_noredraw(St).

main_loop_noredraw(St) ->
    {replace,fun(Event) -> handle_event(Event, St) end}.

handle_event({crash,_}=Crash, St) ->
    LogName = wings_util:crash_log(Crash),
    get_crash_event(LogName, St);
handle_event({open_file,Name}, St0) ->
    case catch ?SLOW(wings_ff_wings:import(Name, St0)) of
	#st{}=St ->
	    wings_pref:set_value(current_directory, filename:dirname(Name)),
	    main_loop(caption(St#st{saved=true,file=Name}));
	{error,_} ->
	    main_loop(St0)
    end;
handle_event(Event, St) ->
    case wings_io:event(Event) of
	next -> handle_event_0(Event, St);
	Other -> Other
    end.

handle_event_0(Event, St) ->
    case wings_camera:event(Event, St) of
	next -> handle_event_1(Event, St);
	Other -> Other
    end.

handle_event_1(Event, St) ->
    case wings_pick:event(Event, St) of
	next -> handle_event_2(Event, St);
	Other -> Other
    end.

handle_event_2(Event, St) ->
    case wings_menu:is_popup_event(Event) of
	no -> handle_event_3(Event, St);
	{yes,X,Y,_} -> popup_menu(X, Y, St)
    end.
	    
handle_event_3(#keyboard{}=Event, St) ->
    case wings_hotkey:event(Event, St) of
	next -> keep;
	Cmd -> do_command(Cmd, St)
    end;
handle_event_3({action,Cmd}, St) ->
    do_command(Cmd, St);
handle_event_3(#mousebutton{}, _St) -> keep;
handle_event_3(#mousemotion{}, _St) -> keep;
handle_event_3(#resize{w=W,h=H}, St0) ->
    St = resize(W, H, St0),
    main_loop(St);
handle_event_3(#expose{}, St) ->
    handle_event_3(redraw, St);
handle_event_3(redraw, St) ->
    wings_draw:render(St),
    case wings_wm:is_window_active(top) of
	true ->
	    Message = ["[L] Select  [R] Show menu  "|wings_camera:help()],
	    wings_io:message(Message);
	false -> ok
    end,
    wings_io:info(info(St)),
    wings_io:update(St),
    main_loop(St#st{vec=none});
handle_event_3(quit, St) ->
    do_command({file,quit}, St);
handle_event_3({new_state,St}, St0) ->
    wings_wm:dirty(),
    wings_io:clear_message(),
    save_state(St0, St);
handle_event_3({callback,Cb}, _) -> Cb();
handle_event_3(ignore, _St) -> keep.
    
do_command(Cmd, St0) ->
    St1 = remember_command(Cmd, St0),
    Res = (catch do_command_1(Cmd, St1)),
    case Res of
	{'EXIT',Reason} -> exit(Reason);
	{command_error,Error} ->
	    wings_util:message(Error, St1);
	#st{}=St -> main_loop(St);
	{drag,Drag} -> wings_drag:do_drag(Drag);
	{save_state,#st{}=St} -> save_state(St1, St);
	{saved,St}=Res ->
	    main_loop(wings_undo:save(St1, St));
	{new,St}=Res -> main_loop(clean_state(wings_undo:init(St)));
	{push,_}=Push -> Push;
	{init,_,_}=Init -> Init;
	{seq,_,_}=Seq -> Seq;
	keep -> keep;
	quit -> exit(normal)
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

%% Wings reset.
command({wings,reset}, St0) ->
    {W,H} = wings_pref:get_value(window_size),
    St = resize(W, H, St0),
    wings_io:reset_grab(),
    wings_view:command(reset, St),
    St;

%% Vector and secondary-selection commands.
command({vector,What}, St) ->
    wings_vec:command(What, St);
command({secondary_selection,aborted}, St) -> St;
command({menu,Menu,X,Y}, St) ->
    menu(X, Y, Menu, St);
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
command({edit,{material,_}}=Cmd, St) ->
    wings_material:command(Cmd, St);
command({edit,repeat}, #st{sel=[]}=St) -> St;
command({edit,repeat}, #st{selmode=Mode,repeatable=Cmd0}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> ok;
	Cmd when tuple(Cmd) -> wings_io:putback_event({action,Cmd})
    end,
    St;
command({edit,repeat}, St) -> St;
command({edit,repeat_drag}, #st{sel=[]}=St) -> St;
command({edit,repeat_drag}, #st{selmode=Mode,repeatable=Cmd0,args=Args}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> ok;
	Cmd when tuple(Cmd) ->
	    case Args of
		none -> ok;
		_Other -> wings_io:putback_event({drag_arguments,Args})
	    end,
	    wings_io:putback_event({action,Cmd})
    end,
    St;
command({edit,repeat_drag}, St) -> St;
command({edit,camera_mode}, St) ->
    wings_camera:command(camera_mode, St);
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
    wings_view:virtual_mirror(Cmd, St);

%% Objects menu.

command({objects,Obj}, St) ->
    wings_shape:command(Obj, St).

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

menu(X, Y, file, St) ->
    wings_file:menu(X, Y, St);
menu(X, Y, edit, St) ->
    Menu = [{"Undo/redo",undo_toggle},
	    {"Redo",redo},
	    {"Undo",undo},
	    separator,
	    {command_name("Repeat", St),repeat},
	    {command_name("Repeat Drag", St),repeat_drag},
	    separator,
	    wings_material:sub_menu(edit, St),
	    separator,
	    wings_camera:sub_menu(St)|wings_pref:menu(St)++
	    [separator,
	     {"Purge Undo History",purge_undo}|patches()]],
    wings_menu:menu(X, Y, edit, Menu);
menu(X, Y, view, St) ->
    wings_view:menu(X, Y, St);
menu(X, Y, select, St) ->
    wings_sel_cmd:menu(X, Y, St);
menu(X, Y, tools, _) ->
    Dirs = [{"All",all},
	    {"X",x},
	    {"Y",y},
	    {"Z",z},
	    {"Radial X",radial_x},
	    {"Radial Y",radial_y},
	    {"Radial Z",radial_z}],
    Menu = [{"Align",{align,Dirs}},
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
		"Remove the virtul mirror for all objects"},
	       {"Freeze",freeze,
		"Create real geometry from the virtual mirrors"}]}}
	   ],
    wings_menu:menu(X, Y, tools, Menu);
menu(X, Y, objects, St) ->
    wings_shape:menu(X, Y, St);
menu(X, Y, help, St) ->
    wings_help:menu(X, Y, St).

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
 	    Dist = e3d_vec:dist(wings_vertex:pos(Va, We),
				wings_vertex:pos(Vb, We)),
	    [Base|io_lib:format(". Length ~s",
				[wings_util:nice_float(Dist)])];
	_ -> Base
    end;
measure(Base, #st{selmode=face,sel=[{Id,Fs}],shapes=Shs}) ->
    case gb_sets:size(Fs) of
	1 ->
	    We = gb_trees:get(Id, Shs),
 	    [Face] = gb_sets:to_list(Fs),
	    Vs = wings_face:surrounding_vertices(Face, We),
	    {X,Y,Z} = wings_vertex:center(Vs, We),
	    [Base|io_lib:format(". Midpt ~s ~s ~s",
				[wings_util:nice_float(X),
				 wings_util:nice_float(Y),
				 wings_util:nice_float(Z)])];
	_ -> Base
    end;
measure(Base, _) -> Base.

item_list(Items, Desc) ->
    item_list(Items, " ", Desc).

item_list([Item|Items], Sep, Desc) ->
    item_list(Items, ", ", [Desc,Sep|integer_to_list(Item)]);
item_list([], _Sep, Desc) -> [Desc|" selected"].

shape_info(#we{name=Name,fs=Ftab,es=Etab,vs=Vtab}) ->
    Faces = gb_trees:size(Ftab),
    Edges = gb_trees:size(Etab),
    Vertices = gb_trees:size(Vtab),
    io_lib:format("Object ~s has ~p polygons, ~p edges, ~p vertices",
		[Name,Faces,Edges,Vertices]).

shape_info(Objs, Shs) ->
    shape_info(Objs, Shs, 0, 0, 0, 0).

shape_info([{Id,_}|Objs], Shs, On, Vn, En, Fn) ->
    #we{fs=Ftab,es=Etab,vs=Vtab} = gb_trees:get(Id, Shs),
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

get_crash_event(Log, St) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> crash_handler(Ev, Log, St) end}.

crash_handler(redraw, Log, _St) ->
    wings_io:ortho_setup(),
    wings_io:text_at(10, 2*?LINE_HEIGHT,
		     "Internal error - log written to " ++ Log),
    wings_io:text_at(10, 4*?LINE_HEIGHT,
		     "Click a mouse button to continue working"),
    keep;
crash_handler(#mousebutton{}, _Log, St) ->
    main_loop(St);
crash_handler(_, Log, St) ->
    get_crash_event(Log, St).

-ifdef(DEBUG).
wings() -> "Wings 3D [debug]".
-else.
wings() -> "Wings 3D".
-endif.
