%%
%%  wings.erl --
%%
%%     The main module of Wings 3D.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings.erl,v 1.293 2004/02/17 11:18:16 dgud Exp $
%%

-module(wings).
-export([start/0,start_halt/0,start_halt/1]).
-export([caption/1,redraw/1,redraw/2,init_opengl/1,command/2]).
-export([mode_restriction/1,clear_mode_restriction/0,get_mode_restriction/0]).
-export([create_toolbar/3]).
-export([ask/3]).

-export([register_postdraw_hook/3,unregister_postdraw_hook/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,map/2,filter/2,foldl/3,sort/1,
		keymember/3,reverse/1]).

start() ->
    do_spawn(none).

start_halt() ->
    spawn_halt(none).

start_halt([File|_]) ->
    spawn_halt(File).

spawn_halt(File) ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  Wings = do_spawn(File, [link]),
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

do_spawn(File) ->
    do_spawn(File, []).

do_spawn(File, Flags) ->
    %% Set a minimal heap size to avoiding garbage-collecting
    %% all the time. Don't set it too high to avoid keeping binaries
    %% too long.
    Fun = fun() -> init(File) end,
    spawn_opt(erlang, apply, [Fun,[]],
	      [{fullsweep_after,16384},{min_heap_size,32*1204}|Flags]).

init(File) ->
    register(wings, self()),

    OsType = os:type(),
    put(wings_os_type, OsType),

    case OsType of
	{win32,_} ->
	    io:format("\n\nNote: The above messages about failing "
		      "to locate TCP/IP parameters are normal.\n"
		      "It is done to prevent Erlang from contacting "
		      "DNS name servers on the Internet\n"
		      "(harmless, but no need for Wings to do it)\n\n");
	_ -> ok
    end,

    wings_pref:init(),
    wings_init:init(),
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
	      ask_args=none,
	      drag_args=none,
	      def={ignore,ignore}
	     },
    St1 = wings_sel:reset(St0),
    St = wings_undo:init(St1),
    wings_view:init(),
    wings_file:init(),
    caption(St),
    put(wings_hitbuf, sdl_util:alloc(?HIT_BUF_SIZE, ?GL_INT)),
    wings_wm:init(),
    wings_file:init_autosave(),
    init_menubar(),
    wings_pb:init(),
    wings_ask:init(),

    Op = main_loop_noredraw(St),		%Replace crash handler
						%with this handler.
    
    Props = initial_properties(),
    {{X,Y},{W,H}} = wings_wm:win_rect(desktop),
    wings_wm:toplevel(geom, "Geometry", {X,Y,highest}, {W,H-80},
		      [resizable,{anchor,nw},{toolbar,fun create_toolbar/3},
		       menubar,{properties,Props}],
		      Op),
    wings_wm:menubar(geom, get(wings_menu_template)),
    set_drag_filter(geom),

    open_file(File),
    restore_windows(St),
    case catch wings_wm:enter_event_loop() of
	{'EXIT',normal} ->
	    wings_pref:finish(),
	    erase(wings_hitbuf),
	    sdl:quit();
	{'EXIT',Reason} ->
	    io:format("~P\n", [Reason,20]),
	    erase(wings_hitbuf),
	    sdl:quit(),
	    exit(Reason)
    end.

new_viewer(St) ->
    {Pos,{W,H}} = wings_wm:win_rect(desktop),
    Size = {W div 2-40,H div 2-40},
    N = free_viewer_num(2),
    Active = wings_wm:this(),
    Props = wings_wm:get_props(Active),
    ToolbarHidden = wings_wm:is_hidden({toolbar,Active}),
    Name = {geom,N},
    new_viewer(Name, Pos, Size, Props, ToolbarHidden, St).

new_viewer({geom,N}=Name, {X,Y}, Size, Props, ToolbarHidden, St) ->
    Op = main_loop_noredraw(St),
    Title = "Geometry #" ++ integer_to_list(N),
    wings_wm:toplevel(Name, Title, {X,Y,highest}, Size,
		      [resizable,closable,{anchor,nw},
		       {toolbar,fun create_toolbar/3},
		       menubar,
		       {properties,Props}],
		      Op),
    wings_wm:menubar(Name, get(wings_menu_template)),
    wings_wm:send({menubar,Name}, {current_state,St}),
    set_drag_filter(Name),
    if
	ToolbarHidden -> wings_wm:hide({toolbar,Name});
	true -> ok
    end,
    Name.

free_viewer_num(N) ->
    case wings_wm:is_window({geom,N}) of
	false -> N;
	true -> free_viewer_num(N+1)
    end.

open_file(none) -> ok;
open_file(Name) -> wings_wm:send(geom, {open_file,Name}).

init_opengl(_) ->
    wings_draw_util:init(),
    keep.

redraw(St) ->
    redraw(info(St), St).

redraw(Info, St) ->
    wings_wm:clear_background(),
    wings_draw_util:render(St),
    call_post_hook(St),
    case Info =/= [] andalso wings_wm:get_prop(show_info_text) of
	true -> wings_io:info(Info);
	false -> ok
    end.

call_post_hook(St) ->
    This = wings_wm:this(),
    case get({post_hook, This}) of
	undefined -> 
	    ok;
	{_Id, Fun} ->
	    Fun(St)
    end.

register_postdraw_hook(Window, Id, Fun) ->
    put({post_hook, Window}, {Id, Fun}).

unregister_postdraw_hook(Window,Id) ->
    case get({post_hook, Window}) of
	undefined -> %% Cancel was called from other win
	    ok;
	{Id, _} ->
	    erase({post_hook,Window});
	_ -> ok
    end.

save_state(St0, St1) ->
    St2 = wings_undo:save(St0, St1),
    St = case St2 of
	     #st{saved=false} -> St2;
	     _Other -> caption(St2#st{saved=false})
	 end,
    main_loop(clear_temp_sel(St)).

ask(Ask, St, Cb) ->
    {replace,
     fun(Ev) -> handle_event(Ev, St) end,
     fun() -> wings_vec:do_ask(Ask, St, Cb) end}.

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

handle_event_0(#mousebutton{button=But,state=ButSt,mod=Mod}=Ev, St)
  when But < 3, Mod band ?CTRL_BITS =/= 0 ->
    case wings_pref:get_value(default_commands) of
	false ->
	    handle_event_1(Ev, St);
	true ->
	    if
		Mod band ?SHIFT_BITS =/= 0 ->
		    define_command(ButSt, But, St);
		true ->
		    use_command(Ev, St)
	    end
    end;
handle_event_0(Ev, St) -> handle_event_1(Ev, St).

handle_event_1(Ev, St) ->
    case wings_pick:event(Ev, St) of
	next -> handle_event_2(Ev, St);
	Other -> Other
    end.

handle_event_2(#mousebutton{x=X,y=Y}=Ev0, #st{sel=Sel}=St0) ->
    case wings_menu:is_popup_event(Ev0) of
	no ->
	    handle_event_3(Ev0, St0);
	{yes,Xglobal,Yglobal,_} ->
	    case Sel =:= [] andalso wings_pref:get_value(use_temp_sel) of
		false ->
		    popup_menu(Xglobal, Yglobal, St0);
		true ->
		    case wings_pick:do_pick(X, Y, St0) of
			{add,_,St} ->
			    Ev = wings_wm:local2global(Ev0),
			    wings_io:putback_event(Ev),
			    wings_wm:later({temporary_selection,St});
			_ ->
			    popup_menu(Xglobal, Yglobal, St0)
		    end
	    end;
	Other -> Other
    end;
handle_event_2(Ev, St) -> handle_event_3(Ev, St).
	    
handle_event_3(#keyboard{}=Ev, St0) ->
    case do_hotkey(Ev, St0) of
	next -> keep;
	{Cmd,St} -> do_command(Cmd, St)
    end;
handle_event_3({action,Callback}, _) when is_function(Callback) ->
    Callback();
handle_event_3({action,Cmd}, St) ->
    do_command(Cmd, St);
handle_event_3({command,Command}, St) when is_function(Command) ->
    command_response(catch Command(St), none, St);
handle_event_3(#mousebutton{}, _St) -> keep;
handle_event_3(#mousemotion{}, _St) -> keep;
handle_event_3(init_opengl, St) ->
    wings_wm:current_state(St),
    init_opengl(St),
    wings_draw:update_dlists(St),
    keep;
handle_event_3(#expose{}, St) ->
    handle_event_3(redraw, St);
handle_event_3(resized, _) -> keep;
handle_event_3(close, _) ->
    Active = wings_wm:this(),
    wings_wm:delete({object,Active}),
    delete;
handle_event_3(redraw, St) ->
    redraw(St),
    main_loop_noredraw(St);
handle_event_3(quit, St) ->
    case wings_wm:this() of
	geom -> do_command({file,quit}, St);
	_ -> keep
    end;
handle_event_3({new_state,St}, St0) ->
    save_state(St0, St);
handle_event_3({temporary_selection,St}, St0) ->
    main_loop(set_temp_sel(St0, St));
handle_event_3({current_state,St}, _) ->
    main_loop_noredraw(St);
handle_event_3({current_state,_,_}, _) ->
    keep;
handle_event_3(revert_state, St) ->
    main_loop(clear_temp_sel(St));
handle_event_3(need_save, St) ->
    main_loop(caption(St#st{saved=false}));
handle_event_3({new_default_command,DefCmd}, St) ->
    main_loop_noredraw(St#st{def=DefCmd});
handle_event_3(got_focus, _) ->
    Msg1 = wings_util:button_format("Select"),
    Msg2 = wings_camera:help(),
    Msg3 = wings_util:button_format([], [], "Show menu"),
    Message = wings_util:join_msg([Msg1,Msg2,Msg3]),
    wings_wm:message(Message),
    wings_wm:dirty();
handle_event_3(lost_focus, _) -> keep;
handle_event_3({note,menu_aborted}, St) ->
    main_loop(clear_temp_sel(St));
handle_event_3({note,_}, _) ->
    keep;
handle_event_3({drop,Pos,DropData}, St) ->
    handle_drop(DropData, Pos, St);
handle_event_3(ignore, _St) -> keep.

do_hotkey(Ev, #st{sel=[]}=St0) ->
    case wings_pref:get_value(use_temp_sel) of
	false ->
	    do_hotkey_1(Ev, St0);
	true ->
	    {_,X,Y} = wings_wm:local_mouse_state(),
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} ->
		    case wings_hotkey:event(Ev, St) of
			next -> next;
			Cmd ->
			    case highlight_sel_style(Cmd) of
				none -> {Cmd,St0};
				temporary -> {Cmd,set_temp_sel(St0, St)};
				permanent -> {Cmd,St}
			    end
		    end;
		_Other -> do_hotkey_1(Ev, St0)
	    end
    end;
do_hotkey(Ev, St) -> do_hotkey_1(Ev, St).

do_hotkey_1(Ev, St) ->
    case wings_hotkey:event(Ev, St) of
 	next -> next;
	Cmd -> {Cmd,St}
    end.

highlight_sel_style({vertex,_}) -> temporary;
highlight_sel_style({edge,_}) -> temporary;
highlight_sel_style({face,_}) -> temporary;
highlight_sel_style({body,_}) -> temporary;
highlight_sel_style({edit,repeat}) -> temporary;
highlight_sel_style({edit,repeat_args}) -> temporary;
highlight_sel_style({edit,repeat_drag}) -> temporary;
highlight_sel_style({select,vertex}) -> none;
highlight_sel_style({select,edge}) -> none;
highlight_sel_style({select,face}) -> none;
highlight_sel_style({select,body}) -> none;
highlight_sel_style({select,{adjacent,_}}) -> none;
highlight_sel_style({select,_}) -> permanent;
highlight_sel_style({view,align_to_selection}) -> temporary;
highlight_sel_style({view,aim}) -> temporary;
highlight_sel_style(_) -> none.

do_command(Cmd, St0) ->
    St = remember_command(Cmd, St0),
    {replace,
     fun(Ev) -> handle_event(Ev, St) end,
     fun() -> raw_command(Cmd, none, St) end}.

raw_command(Cmd, Args, St) ->
    command_response(catch do_command_1(Cmd, St), Args, St).

command_response({'EXIT',Reason}, _, _) ->
    exit(Reason);
command_response({command_error,Error}, _, _) ->
    wings_util:message(Error);
command_response(#st{}=St, _, _) ->
    main_loop(clear_temp_sel(St));
command_response({drag,Drag}, Args, _) ->
    wings_drag:do_drag(Drag, Args);
command_response({save_state,#st{}=St}, _, St0) ->
    save_state(St0, St);
command_response({saved,St}, _, _) ->
    main_loop(St);
command_response({new,St}, _, _) ->
    main_loop(caption(wings_undo:init(St)));
command_response({push,_}=Push, _, _) ->
    Push;
command_response({init,_,_}=Init, _, _) ->
    Init;
command_response({seq,_,_}=Seq, _, _) ->
    Seq;
command_response({replace,_}=Replace, _, _) ->
    Replace;
command_response({replace,_,_}=Replace, _, _) ->
    Replace;
command_response(keep, _, _) ->
    keep;
command_response(quit, _, _) ->
    save_windows(),
    exit(normal).

do_command_1(Cmd, St0) ->
    case wings_plugin:command(Cmd, St0) of
	next -> command(Cmd, St0);
	St0 -> St0;
	#st{}=St -> {save_state,St};
	Other -> Other
    end.

remember_command({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
				     C =:= face; C =:= body ->
    St#st{repeatable=Cmd,ask_args=none,drag_args=none};
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
command({shape,Shape}, St0) ->
    case wings_shapes:command(Shape, St0) of
    	St0 -> St0;
	#st{}=St -> {save_state,St};
	Other -> Other
    end;
command({help,What}, St) ->
    wings_help:command(What, St);

%% Drag & drop.
command({drop,What}, St) ->
    drop_command(What, St);

%% File menu.
command({file,Command}, St) ->
    wings_file:command(Command, St);

%% Edit menu.
command({edit,undo_toggle}, St) ->
    caption(wings_undo:undo_toggle(St));
command({edit,undo}, St) ->
    caption(wings_undo:undo(St));
command({edit,redo}, St) ->
    caption(wings_undo:redo(St));
command({edit,repeat}, #st{sel=[]}=St) -> St;
command({edit,repeat}, #st{selmode=Mode,repeatable=Cmd0}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd when is_tuple(Cmd) -> raw_command(Cmd, none, St)
    end;
command({edit,repeat}, St) -> St;
command({edit,repeat_args}, #st{sel=[]}=St) -> St;
command({edit,repeat_args}, #st{selmode=Mode,repeatable=Cmd0,
				ask_args=AskArgs}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd1 when is_tuple(Cmd1) ->
	    Cmd = replace_ask(Cmd1, AskArgs),
	    raw_command(Cmd, none, St)
    end;
command({edit,repeat_args}, St) -> St;
command({edit,repeat_drag}, #st{sel=[]}=St) -> St;
command({edit,repeat_drag}, #st{selmode=Mode,repeatable=Cmd0,
				ask_args=AskArgs,drag_args=DragArgs}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd1 when is_tuple(Cmd1) ->
	    Cmd = replace_ask(Cmd1, AskArgs),
	    raw_command(Cmd, DragArgs, St)
    end;
command({edit,repeat_drag}, St) -> St;
command({edit,purge_undo}, St) ->
    purgo_undo(St);
command({edit,confirmed_purge_undo}, St) ->
    wings_undo:init(St);
command({edit,enable_patches}, St) ->
    wings_start:enable_patches(),
    St;
command({edit,disable_patches}, St) ->
    wings_start:disable_patches(),
    St;
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
    new_viewer(St),
    keep;
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
    wings:ask({[axis,point],[]}, St,
	      fun({Axis,Point}, _) ->
		      wings_pref:set_value(default_axis, {Point,Axis}),
		      keep
	      end);
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
command({tools, screenshot}, St) ->
    wings_image:screenshot(),
    St.

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
    put(wings_menu_template, Menus).

edit_menu(St) ->
    UndoInfo = lists:flatten(["Delete undo history to reclaim memory (",
			      undo_info(St),")"]),
    [{"Undo/Redo",undo_toggle,"Undo or redo the last command"},
     {"Redo",redo,"Redo the last command that was undone"},
     {"Undo",undo,"Undo the last command"},
     separator,
     {command_name("Repeat", St),repeat},
     {command_name("Repeat Args", St),repeat_args},
     {command_name("Repeat Drag", St),repeat_drag},
     separator,
     wings_pref:menu(St),
     {"Plug-in Preferences",{plugin_preferences,[]}},
     separator,
     {"Purge Undo History",purge_undo,UndoInfo}|patches()].

undo_info(St) ->
    {Un,Rn} = wings_undo:info(St),
    Undo = case Un of
	       0 -> "there are no undo states";
	       1 -> "there is one undo state";
	       _ -> io_lib:format("there are ~p undo states", [Un])
	   end,
    case Rn of
	0 -> Undo;
	1 -> [Undo|"; one operation can be redone"];
	_ -> [Undo|io_lib:format("; ~p operations can be redone", [Rn])]
    end.

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
     {"Set Default Axis",set_default_axis,
      "Define and store axis (with ref. point) for later use with any "
      "\"Default Axis\" command (e.g. Scale|Default Axis)"},
     separator,
     {"Virtual Mirror",
      {virtual_mirror,
       [{"Create",create,
	 "Given a face selection, set up a virtual mirror"},
	{"Break",break,
	 "Remove virtual mirrors for all objects"},
	{"Freeze",freeze,
	 "Create real geometry from the virtual mirrors"}]}},
     separator,
     {"Screenshot", screenshot, 
      "Grab an image of the window (export it from the outliner)"}].

window_menu(_) ->
    Name = case wings_wm:this() of
	       {_,geom} ->
		   "Geometry Graph";
	       {_,{geom,N}} ->
		   "Geometry Graph #" ++ integer_to_list(N)
	   end,
    [{"Outliner",outliner,[]},
     {Name,object,[]},
     separator,
     {"New Geometry Window",geom_viewer}].

patches() ->
    case wings_start:get_patches() of
	none -> [];
	{enabled,Desc} ->
	    [separator,{"Use "++Desc,disable_patches,[crossmark]}];
	{disabled,Desc} ->
	    [separator,{"Use "++Desc,enable_patches}]
    end.

set_temp_sel(#st{sh=Sh,selmode=Mode}, St) ->
    St#st{temp_sel={Mode,Sh}}.

clear_temp_sel(#st{temp_sel=none}=St) -> St;
clear_temp_sel(#st{temp_sel={Mode,Sh}}=St) ->
    St#st{temp_sel=none,selmode=Mode,sh=Sh,sel=[]}.


purgo_undo(St) ->
    This = wings_wm:this(),
    {Un,Rn} = wings_undo:info(St),
    Qs = {vframe,
	  [{label,"Undo states: " ++ integer_to_list(Un)},
	   {label,"Redo states: " ++ integer_to_list(Rn)},
	   separator|
	   if
	       Un+Rn =:= 0 ->
		   [{label,"Nothing to remove"},
		    {hframe,[{button,ok}]}];
	       true ->
		   [{label,"Remove all states (NOT undoable)?"},
		    {hframe,[{button,"Yes",
			      fun(_) ->
				      Action = {action,{edit,confirmed_purge_undo}},
				      wings_wm:send(This, Action)
			      end},
			     {button,"No",cancel,[cancel]}]}]
	   end]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

info(#st{sel=[]}) -> [];
info(St) ->
    case wings_wm:get_prop(show_info_text) of
	false -> [];
	true -> info_1(St)
    end.
	    
info_1(#st{shapes=Shapes,selmode=body,sel=[{Id,_}]}) ->
    Sh = gb_trees:get(Id, Shapes),
    shape_info(Sh);
info_1(#st{shapes=Shapes,selmode=body,sel=Sel}) ->
    shape_info(Sel, Shapes);
info_1(#st{selmode=vertex,sel=[{_Id,Sel}]}=St) ->
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
info_1(#st{selmode=edge,sel=[{_,Sel}]}=St) ->
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
info_1(#st{selmode=face,sel=[{_,Sel}]}=St) ->
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
info_1(#st{selmode=Mode,sel=Sel}=St) ->
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
	    We = gb_trees:get(Id, Shs),
 	    [Face] = gb_sets:to_list(Fs),
	    {X,Y,Z} = wings_face:center(Face, We),
	    Mat = wings_material:get(Face, We),
	    [Base|io_lib:format(". Midpt ~s ~s ~s.\nMaterial ~s",
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
    io_lib:format("\"~s\" has ~p polygons, ~p edges, ~p vertices.\nMode is ~p",
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

replace_ask(Term, none) -> Term;
replace_ask({'ASK',_}, AskArgs) -> AskArgs;
replace_ask(Tuple0, AskArgs) when is_tuple(Tuple0) ->
    Tuple = [replace_ask(El, AskArgs) || El <- tuple_to_list(Tuple0)],
    list_to_tuple(Tuple);
replace_ask(Term, _) -> Term.

define_command(?SDL_RELEASED, N, #st{repeatable=Cmd,def=DefCmd0}) ->
    This = wings_wm:this(),
    CmdStr = wings_util:stringify(Cmd),
    Button = case N of
		 1 -> "L";
		 2 -> "M"
	     end,
    wings_util:yes_no("Do you want to define \"" ++ CmdStr ++
		      "\" as a default command ([Ctrl]+[" ++ Button ++
		      "])?",
		      fun() ->
			      DefCmd = setelement(N, DefCmd0, Cmd),
			      wings_wm:send(This, {new_default_command,DefCmd}),
			      ignore
		      end, ignore);
define_command(_, _, _) -> keep.

use_command(#mousebutton{state=?SDL_RELEASED,button=N}=Ev,
	    #st{selmode=Mode,def=DefCmd}=St) ->
    case repeatable(Mode, element(N, DefCmd)) of
	no -> keep;
	Cmd when is_tuple(Cmd) ->
	    do_use_command(Ev, Cmd, St)
    end;
use_command(_, _) -> keep.

do_use_command(#mousebutton{x=X,y=Y}, Cmd0, #st{sel=[]}=St0) ->
    case wings_pref:get_value(use_temp_sel) of
	false ->
	    keep;
	true ->
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,#st{selmode=Mode}=St} ->
		    %% The selection mode may have changed.
		    %% Must check (and possibly convert) the command again.
		    case repeatable(Mode, Cmd0) of
			no -> keep;
			Cmd ->
			    wings_wm:later({action,Cmd}),
			    main_loop_noredraw(set_temp_sel(St0, St))
		    end;
		_Other -> keep
	    end
    end;
do_use_command(_, Cmd, _) -> wings_wm:later({action,Cmd}).

crash_logger(Crash, St) ->
    LogName = wings_util:crash_log(geom, Crash),
    get_crash_event(LogName, St).

get_crash_event(Log, St) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> crash_handler(Ev, Log, St) end}.

crash_handler(redraw, Log, _St) ->
    wings_wm:clear_background(),
    wings_io:ortho_setup(),
    wings_io:text_at(10, 2*?LINE_HEIGHT,
		     "Internal error - log written to " ++ Log),
    wings_io:text_at(10, 4*?LINE_HEIGHT,
		     "Click a mouse button to continue working"),
    wings_wm:message("[L] Continue working", ""),
    keep;
crash_handler(#mousebutton{}, _, St) ->
    wings_wm:message(""),
    wings_wm:menubar(wings_wm:this(), get(wings_menu_template)),
    main_loop(St);
crash_handler(_, Log, St) ->
    get_crash_event(Log, St).

-ifdef(DEBUG).
wings() -> "Wings 3D [debug]".
-else.
wings() -> "Wings 3D".
-endif.

%%%
%%% Drag & Drop.
%%%

set_drag_filter(Name) ->
    F = fun({material,_}) -> yes;
	   (_) -> no
	end,
    wings_wm:set_prop(Name, drag_filter, F).

handle_drop(DropData, {X0,Y0}, St) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    handle_drop_1(DropData, X, Y, St).

handle_drop_1(_, X, Y, #st{sel=[]}) ->
    wings_menu:popup_menu(X, Y, drop,
			  [{"No Selection",cancel_drop,"Cancel drop operation"}]);
handle_drop_1({material,Name}, X, Y, #st{selmode=face}) ->
    Menu = [{"Assign material to selected faces",menu_cmd(assign_to_sel, Name),
	     "Assign material \""++Name++"\" only to selected faces"},
	    {"Assign material to all faces",
	     menu_cmd(assign_to_body, Name),
	     "Assign material \""++Name++
	     "\" to all faces in objects having a selection"}],
    wings_menu:popup_menu(X, Y, drop, Menu);
handle_drop_1({material,Name}, X, Y, _) ->
    Menu = [{"Assign material to all faces",
	     menu_cmd(assign_to_body, Name),
	     "Assign material \""++Name++
	     "\" to all faces in objects having a selection"}],
    wings_menu:popup_menu(X, Y, drop, Menu).
    
menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.

drop_command({assign_to_sel,Name}, St) ->
    wings_material:command({assign,Name}, St);
drop_command({assign_to_body,Name}, #st{selmode=Mode}=St0) ->
    St = wings_material:command({assign,Name}, St0#st{selmode=body}),
    St#st{selmode=Mode};
drop_command(cancel_drop, St) -> St.

%%%
%%% Saving and restoring of window layouts.
%%%

save_windows() ->
    Saved = save_windows_1(wings_wm:windows()),
    wings_pref:set_value(saved_windows, Saved).

save_windows_1([outliner|Ns]) ->
    save_window(outliner, Ns);
save_windows_1([{object,_}=N|Ns]) ->
    save_window(N, Ns);
save_windows_1([geom=N|Ns]) ->
    save_geom_window(N, Ns);
save_windows_1([{geom,_}=N|Ns]) ->
    save_geom_window(N, Ns);
save_windows_1([_|T]) -> save_windows_1(T);
save_windows_1([]) -> [].

save_window(Name, Ns) ->
    Pos = wings_wm:win_ur({controller,Name}),
    Size = wings_wm:win_size(Name),
    [{Name,Pos,Size}|save_windows_1(Ns)].

save_geom_window(Name, Ns) ->
    {Pos,Size} = wings_wm:win_rect(Name),
    Ps0 = [{toolbar_hidden,wings_wm:is_hidden({toolbar,Name})}],
    Ps = save_geom_props(wings_wm:get_props(Name), Ps0),
    Geom = {Name,Pos,Size,Ps},
    [Geom|save_windows_1(Ns)].

save_geom_props([{show_axes,_}=P|T], Acc) ->
    save_geom_props(T, [P|Acc]);
save_geom_props([{show_groundplane,_}=P|T], Acc) ->
    save_geom_props(T, [P|Acc]);
save_geom_props([{current_view,View}|T], Acc) ->
    #view{fov=Fov,hither=Hither,yon=Yon} = View,
    save_geom_props(T, [{fov,Fov},{clipping_planes,Hither,Yon}|Acc]);
save_geom_props([{show_info_text,_}=P|T], Acc) ->
    save_geom_props(T, [P|Acc]);
save_geom_props([_|T], Acc) ->
    save_geom_props(T, Acc);
save_geom_props([], Acc) -> Acc.

restore_windows(St) ->
    %% Sort windows using names as keys to make sure we
    %% create the geometry windows before the object windows.
    %% (Because we set up links.)
    Windows0 = wings_pref:get_value(saved_windows, []),
    Windows1 = sort([{element(1, W),W} || W <- Windows0]),
    Windows = [W || {_,W} <- Windows1],
    restore_windows_1(Windows, St).

restore_windows_1([{geom,{_,_}=Pos0,{_,_}=Size,Ps0}|Ws], St) ->
    Ps = geom_props(Ps0),
    case proplists:get_bool(toolbar_hidden, Ps) of
	true -> wings_wm:hide({toolbar,geom});
	false -> ok
    end,
    Pos = geom_pos(Pos0),
    wings_wm:move(geom, Pos, Size),
    set_geom_props(Ps, geom),
    restore_windows_1(Ws, St);
restore_windows_1([{{geom,_}=Name,Pos0,Size,Ps0}|Ws], St) ->
    Ps = geom_props(Ps0),
    ToolbarHidden = proplists:get_bool(toolbar_hidden, Ps),
    new_viewer(Name, {0,0}, Size, initial_properties(), ToolbarHidden, St),
    Pos = geom_pos(Pos0),
    wings_wm:move(Name, Pos, Size),
    set_geom_props(Ps, Name),
    restore_windows_1(Ws, St);
restore_windows_1([{{object,_}=Name,{_,_}=Pos,{_,_}=Size}|Ws], St) ->
    wings_shape:window(Name, Pos, Size, St),
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

geom_props(B) when B == false; B == true ->
    [{toolbar_hidden,B}];
geom_props(L) when is_list(L) -> L;
geom_props(_) -> [].

set_geom_props([{show_axes,B}|T], Name) ->
    wings_wm:set_prop(Name, show_axes, B),
    set_geom_props(T, Name);
set_geom_props([{show_groundplane,B}|T], Name) ->
    wings_wm:set_prop(Name, show_groundplane, B),
    set_geom_props(T, Name);
set_geom_props([{show_info_text,B}|T], Name) ->
    wings_wm:set_prop(Name, show_info_text, B),
    set_geom_props(T, Name);
set_geom_props([{fov,Fov}|T], Name) ->
    View = wings_wm:get_prop(Name, current_view),
    wings_wm:set_prop(Name, current_view, View#view{fov=Fov}),
    set_geom_props(T, Name);
set_geom_props([{clipping_planes,Hither,Yon}|T], Name)
  when Hither > 0, Hither < Yon  ->
    View = wings_wm:get_prop(Name, current_view),
    wings_wm:set_prop(Name, current_view, View#view{hither=Hither,yon=Yon}),
    set_geom_props(T, Name);
set_geom_props([_|T], Name) ->
    set_geom_props(T, Name);
set_geom_props([], _) -> ok.

initial_properties() ->
    [{display_lists,geom_display_lists}|wings_view:initial_properties()].

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

mode_restriction(Modes) ->
    Win = {toolbar,wings_wm:this()},
    wings_wm:send(Win, {mode_restriction,Modes}),
    case Modes of
	none ->
	    wings_wm:erase_prop(Win, mode_restriction);
	_ ->
	    wings_wm:set_prop(Win, mode_restriction, Modes)
    end.

clear_mode_restriction() ->
    mode_restriction(none).

get_mode_restriction() ->
    Name = wings_wm:this(),
    Toolbar = {toolbar,Name},
    case wings_wm:lookup_prop(Toolbar, mode_restriction) of
	none -> [edge,vertex,face,body];
	{value,Other} -> Other
    end.

create_toolbar({toolbar,Client}=Name, Pos, W) ->
    ButtonH = ?BUTTON_HEIGHT+6,
    wings_wm:new(Name, Pos, {W,ButtonH}, init_button()),
    wings_wm:set_prop(Name, display_lists, wings_wm:get_prop(Client, display_lists)).

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
    {toolbar,Client} = wings_wm:this(),
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
    {toolbar,Client} = wings_wm:this(),
    wings_wm:send(Client, Ev);
button_event(_, _) -> keep.

button_resized(#but{restr=Restr}=But) ->
    {toolbar,Client} = Self = wings_wm:this(),
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
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    wings_io:draw_icons(
      fun() ->
	      foreach(fun({X,Name}) ->
			      wings_io:draw_icon(X, 3, button_value(Name, Mode, Sh))
		      end, Buttons)
      end),
    button_redraw_sh(Sh, Buttons).

button_redraw_sh(false, _) -> ok;
button_redraw_sh(true, Buttons) ->
    Pos = [X || {X,M} <- Buttons, button_sh_filter(M, true)],
    case Pos of
	[] -> ok;
	[Left|_] ->
	    gl:pushAttrib(?GL_POLYGON_BIT bor ?GL_LINE_BIT),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
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
    {toolbar,Client} = wings_wm:this(),
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
    {toolbar,Client} = wings_wm:this(),
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
    {toolbar,Client} = wings_wm:this(),
    case wings_wm:get_prop(Client, Key) of
	Val -> First;
	_ -> Second
    end.
