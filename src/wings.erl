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
%%     $Id: wings.erl,v 1.103 2002/02/03 07:26:27 bjorng Exp $
%%

-module(wings).
-export([start/0,start/1,start_halt/1,start_halt/2]).
-export([root_dir/0,caption/1,redraw/1,info/1]).
-export([menu/4,popup_menu/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(COLOR_BITS, 16).
-define(INTERESTING_BITS, (?CTRL_BITS bor ?ALT_BITS)).
-import(lists, [foreach/2,map/2,filter/2,foldl/3,sort/1,
		keymember/3,reverse/1]).
-import(wings_draw, [model_changed/1]).

start() ->
    %% Only for development use.
    RootEbin = filename:dirname(filename:absname(code:which(?MODULE))),
    Split = filename:split(RootEbin),
    Root = filename:join(Split -- ["ebin"]),
    spawn(fun() -> init(none, Root) end).

start(Root) ->
    spawn(fun() -> init(none, Root) end).

start_halt(Root) ->
    spawn(fun() ->
		  init(none, Root),
		  halt()
	  end).

start_halt([File|_], Root) ->
    spawn(fun() ->
		  init(File, Root),
		  halt()
	  end).

root_dir() ->
    get(wings_root_dir).

init(File, Root) ->
    register(wings, self()),
    put(wings_root_dir, Root),
    case
	catch
	init_1(File) of
	{'EXIT',Reason} -> io:format("Crashed: ~P\n", [Reason,30]);
	ok -> ok
    end.

init_1(File) ->
    {ok,Cwd} = file:get_cwd(),
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER),
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
    wings_draw_util:init(),

    wings_io:menubar([{"File",file},
		      {"Edit",edit},
		      {"View",view},
		      {"Select",select},
		      {"Tools",tools},
		      {"Objects",objects},
		      {"Help",help}]),
    Empty = gb_trees:empty(),

    St0 = #st{shapes=Empty,
	      selmode=face,
	      sel=[],
	      ssel={face,[]},
	      mat=wings_material:default(),
	      saved=true,
	      onext=0,
	      repeatable=ignore,
	      args=none
	    },
    St1 = wings_undo:init(St0),
    wings_view:init(),
    wings_file:init(),
    put(wings_hitbuf, sdl_util:malloc(?HIT_BUF_SIZE, ?GL_UNSIGNED_INT)),

    %% On Solaris/Sparc, we must initialize twice the first time to
    %% get the requested size. Should be harmless on other platforms.
    caption(St1),
    St2 = resize(780, 570, St1),
    resize(780, 570, St1),
    St = open_file(File, St2),
    wings_io:enter_event_loop(main_loop(St)),
    wings_file:finish(),
    wings_pref:finish(),
    sdl:quit(),
    ok = file:set_cwd(Cwd),
    ok.


open_file(none, St) -> St;
open_file(Name, St0) ->
    case ?SLOW(wings_ff_wings:import(Name, St0)) of
	#st{}=St ->
	    wings_getline:set_cwd(filename:dirname(Name)),
	    caption(St#st{saved=true,file=Name});
	{error,Reason} ->
	    wings_io:message("Read failed: " ++ Reason),
	    St0
    end.

locate(Name) ->
    case filelib:is_file(Name) of
	true -> Name;
	false ->
	    Root = root_dir(),
	    Path = filename:join(Root, Name),
	    case filelib:is_file(Path) of
		true -> Path;
		false -> filename:join([Root,"src",Name])
	    end
    end.

resize(W, H, St) ->
    sdl_video:setVideoMode(W, H, ?COLOR_BITS, ?SDL_OPENGL bor ?SDL_RESIZABLE),
    wings_view:init_light(),
    gl:enable(?GL_DEPTH_TEST),
    {R,G,B} = wings_pref:get_value(background_color),
    gl:clearColor(R, G, B, 1.0),
    gl:viewport(0, 0, W, H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    wings_view:perspective(),
    gl:matrixMode(?GL_MODELVIEW),
    wings_io:resize(W, H),
    wings_material:init(St).

redraw(St0) ->
    St = wings_draw:render(St0),
    wings_io:info(info(St)),
    wings_io:update(St),
    St.

clean_state(St) ->
    caption(wings_draw:model_changed(St)).

save_state(St0, St1) ->
    St = wings_undo:save(St0, St1#st{vec=none}),
    wings_io:clear_message(),
    case St of
	#st{saved=false} -> main_loop(St);
	Other -> main_loop(caption(St#st{saved=false}))
    end.

main_loop(St) ->
    ?VALIDATE_MODEL(St),
    redraw(St),
    main_loop_noredraw(St).

main_loop_noredraw(St) ->
    fun(Event) -> handle_event(Event, St) end.

handle_event({crash,_}=Crash, St) ->
    LogName = wings_util:crash_log(Crash),
    wings_io:message("Internal error - log written to " ++ LogName),
    main_loop(St);
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
	{yes,X,Y} -> popup_menu(X, Y, St)
    end.
	    
handle_event_3(drag_aborted, St) ->
    wings_io:clear_message(),
    main_loop(model_changed(St#st{vec=none}));
handle_event_3({drag_ended,St}, St0) ->
    wings_io:clear_message(),
    save_state(St0, St);
handle_event_3({new_selection,St}, St0) ->
    save_state(St0, St);
handle_event_3(Event, St0) ->
    case translate_event(Event, St0) of
	#mousebutton{} -> keep;
	#mousemotion{} -> keep;
	ignore -> keep;
	redraw ->
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    main_loop(St0);
 	{resize,W,H} ->
 	    St = resize(W, H, St0),
 	    main_loop(model_changed(St));
 	{edit,undo_toggle} ->
	    St = wings_undo:undo_toggle(St0),
	    main_loop(clean_state(St));
 	{edit,undo} ->
	    St = wings_undo:undo(St0),
	    main_loop(clean_state(St));
 	{edit,redo} ->
	    St = wings_undo:redo(St0),
	    main_loop(clean_state(St));
	Cmd -> do_command(Cmd, St0)
    end.
    
do_command(Cmd, St0) ->
    St1 = remember_command(Cmd, St0),
    Res = (catch do_command_1(Cmd, St1)),
    case Res of
	{'EXIT',Reason} -> exit(Reason);
	{command_error,Error} ->
	    wings_util:message(Error),
	    main_loop(St0);
	#st{}=St -> main_loop(St);
	{drag,Drag} ->
	    St = model_changed(St0),
	    {seq,{replace,fun(Event) -> handle_event(Event, St) end},
	     wings_drag:do_drag(Drag)};
	{save_state,#st{}=St} -> save_state(St1, St);
	{saved,St}=Res ->
	    main_loop(wings_undo:save(St1, St));
	{new,St}=Res -> main_loop(clean_state(wings_undo:init(St)));
	{push,_}=Push -> Push;
	{init,_,_}=Init -> Init;
	{seq,_,_}=Seq -> Seq;
	aborted -> main_loop(St0);
	quit ->
	    sdl_util:free(get(wings_hitbuf)),
	    pop
    end.

do_command_1(Cmd, St0) ->
    case wings_plugin:command(Cmd, St0) of
	next -> command(Cmd, St0);
	St0 -> St0;
	#st{}=St -> {save_state,model_changed(St)};
	{drag,_}=Drag -> Drag;
	aborted -> St0
    end.

remember_command({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
				     C =:= face; C =:= body ->
    St#st{repeatable=Cmd,args=none};
remember_command(Cmd, St) -> St.

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

command({vector,What}, St) ->
    wings_vec:command(What, St);
command({secondary_selection,aborted}, St) -> St;
command({menu,Menu,X,Y}, St) ->
    menu(X, Y, Menu, St);
command({shape,{Shape,Ask}}, St0) ->
    case wings_shapes:command(Shape, Ask, St0) of
	aborted -> St0;
	St -> {save_state,model_changed(St)}
    end;
command({shape,Shape}, St0) ->
    St = wings_shapes:command(Shape, false, St0),
    {save_state,model_changed(St)};
command({help,What}, St) ->
    wings_help:What(St);

%% File menu.
command({file,Command}, St) ->
    wings_file:command(Command, St);

%% Edit menu.
command({edit,{material,Mat}}=Cmd, St) ->
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
		Other -> wings_io:putback_event({drag_arguments,Args})
	    end,
	    wings_io:putback_event({action,Cmd})
    end,
    St;
command({edit,repeat_drag}, St) -> St;
command({edit,{camera_mode,Mode}}, St) ->
    wings_camera:command(Mode),
    St;
command({edit,purge_undo}, St) ->
    wings_undo:purge(St);
command({edit,enable_patches}, St) ->
    wings_start:enable_patches(),
    St;
command({edit,disable_patches}, St) ->
    wings_start:disable_patches(),
    St;
command({edit,{_,Pref}}, St) ->
    wings_pref:command(Pref),
    St;

%% Select menu.
command({select,Command}, St) ->
    wings_sel_cmd:command(Command, St);

%% View menu.
command({view,Command}, St) ->
    wings_view:command(Command, St);

%% Body menu.
command({body,invert}, St) ->
    {save_state,model_changed(wings_body:invert_normals(St))};
command({body,{duplicate,Dir}}, St) ->
    wings_body:duplicate(Dir, St);
command({body,delete}, St) ->
    {save_state,model_changed(wings_body:delete(St))};
command({body,tighten}, St) ->
    wings_body:tighten(St);
command({body,smooth}, St) ->
    ?SLOW({save_state,model_changed(wings_body:smooth(St))});
command({body,combine}, St) ->
    {save_state,model_changed(wings_body:combine(St))};
command({body,separate}, St) ->
    {save_state,model_changed(wings_body:separate(St))};
command({body,auto_smooth}, St) ->
    {save_state,model_changed(wings_body:auto_smooth(St))};
command({body,{flip,Plane}}, St) ->
    {save_state,model_changed(wings_body:flip(Plane, St))};
command({body,cleanup}, St) ->
    {save_state,model_changed(wings_body:cleanup(St))};

%% Face menu.
command({face,{extrude,Type}}, St) ->
    ?SLOW(wings_face_cmd:extrude(Type, St));
command({face,{extrude_region,Type}}, St) ->
    ?SLOW(wings_face_cmd:extrude_region(Type, St));
command({face,{extract_region,Type}}, St) ->
    wings_face_cmd:extract_region(Type, St);
command({face,bump}, St) ->
    ?SLOW(wings_extrude_edge:bump(St));
command({face,{flatten,Plane}}, St) ->
    {save_state,model_changed(wings_face_cmd:flatten(Plane, St))};
command({face,{flatten_move,Type}}, St) ->
    {save_state,model_changed(wings_face_cmd:flatten_move(Type, St))};
command({face,bevel}, St) ->
    ?SLOW(wings_extrude_edge:bevel_faces(St));
command({face,inset}, St) ->
    ?SLOW(wings_face_cmd:inset(St));
command({face,mirror}, St) ->
    ?SLOW({save_state,model_changed(wings_face_cmd:mirror(St))});
command({face,intrude}, St) ->
    ?SLOW(wings_face_cmd:intrude(St));
command({face,dissolve}, St) ->
    {save_state,model_changed(wings_face_cmd:dissolve(St))};
command({face,{material,Mat}}=Cmd, St) ->
    {save_state,model_changed(wings_material:command(Cmd, St))};
command({face,bridge}, St) ->
    {save_state,model_changed(wings_face_cmd:bridge(St))};
command({face,smooth}, St) ->
    ?SLOW({save_state,model_changed(wings_face_cmd:smooth(St))});
command({face,auto_smooth}, St) ->
    {save_state,model_changed(wings_body:auto_smooth(St))};
command({face,{lift,Lift}}, St) ->
    wings_face_cmd:lift(Lift, St);
    
%% Edge commands.
command({edge,bevel}, St) ->
    ?SLOW(wings_extrude_edge:bevel(St));
command({edge,{extrude,Type}}, St) ->
    ?SLOW(wings_extrude_edge:extrude(Type, St));
command({edge,{cut,Num}}, St) ->
    {save_state,model_changed(wings_edge:cut(Num, St))};
command({edge,connect}, St) ->
    {save_state,model_changed(wings_edge:connect(St))};
command({edge,dissolve}, St) ->
    {save_state,model_changed(wings_edge:dissolve(St))};
command({edge,{hardness,Type}}, St) ->
    {save_state,model_changed(wings_edge:hardness(Type, St))};
command({edge,loop_cut}, St) ->
    ?SLOW({save_state,model_changed(wings_edge:loop_cut(St))});
command({edge,auto_smooth}, St) ->
    {save_state,model_changed(wings_body:auto_smooth(St))};

%% Vertex menu.
command({vertex,{flatten,Plane}}, St) ->
    {save_state,model_changed(wings_vertex_cmd:flatten(Plane, St))};
command({vertex,{flatten_move,Type}}, St) ->
    {save_state,model_changed(wings_vertex_cmd:flatten_move(Type, St))};
command({vertex,connect}, St) ->
    {save_state,model_changed(wings_vertex_cmd:connect(St))};
command({vertex,tighten}, St) ->
    wings_vertex_cmd:tighten(St);
command({vertex,bevel}, St) ->
    ?SLOW(wings_vertex_cmd:bevel(St));
command({vertex,{extrude,Type}}, St) ->
    ?SLOW(wings_vertex_cmd:extrude(Type, St));
command({vertex,{deform,Deform}}, St0) ->
    ?SLOW(wings_deform:command(Deform, St0));
command({vertex,auto_smooth}, St) ->
    {save_state,model_changed(wings_body:auto_smooth(St))};

%% Magnetic commands.
command({vertex,{magnet,Magnet}}, St) ->
    ?SLOW(wings_magnet:command(Magnet, St));

%% Tools menu.

command({tools,{align,Dir}}, St) ->
    {save_state,model_changed(wings_align:align(Dir, St))};
command({tools,{center,Dir}}, St) ->
    {save_state,model_changed(wings_align:center(Dir, St))};
command({tools,save_bb}, St) ->
    wings_align:copy_bb(St);
command({tools,{scale_to_bb,Dir}}, St) ->
    {save_state,model_changed(wings_align:scale_to_bb(Dir, St))};
command({tools,{scale_to_bb_prop,Dir}}, St) ->
    {save_state,model_changed(wings_align:scale_to_bb_prop(Dir, St))};
command({tools,{move_to_bb,Dir}}, St) ->
    {save_state,model_changed(wings_align:move_to_bb(Dir, St))};

%% Objects menu.

command({objects,Obj}, St) ->
    wings_shape:command(Obj, St);

%% Common commands.
command({_,collapse}, St) ->
    {save_state,model_changed(wings_collapse:collapse(St))};
command({_,{move,Type}}, St) ->
    wings_move:setup(Type, St);
command({_,{rotate,Type}}, St) ->
    wings_rotate:setup(Type, St);
command({_,{scale,Type}}, St) ->
    wings_scale:setup(Type, St).

popup_menu(X, Y, #st{selmode=Mode,sel=Sel}=St) ->
    case {Sel,Mode} of
 	{[],_} -> wings_shapes:menu(X, Y, St);
 	{_,vertex} -> vertex_menu(X, Y, St);
 	{_,edge} -> edge_menu(X, Y, St);
 	{_,face} -> face_menu(X, Y, St);
 	{_,body} -> body_menu(X, Y, St)
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
    wings_menu:menu(X, Y, edit, Menu, St);
menu(X, Y, view, St) ->
    wings_view:menu(X, Y, St);
menu(X, Y, select, St) ->
    wings_sel_cmd:menu(X, Y, St);
menu(X, Y, tools, St) ->
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
	    {"Move to Saved BB",{move_to_bb,all_xyz()}}],
    wings_menu:menu(X, Y, tools, Menu, St);
menu(X, Y, objects, St) ->
    wings_shape:menu(X, Y, St);
menu(X, Y, help, St) ->
    Menu = [{"About",about}],
    wings_menu:menu(X, Y, help, Menu, St).

vertex_menu(X, Y, St) ->
    Dir = directions(St),
    Menu = [{"Vertex operations",ignore},
	    separator,
	    {"Move",{move,Dir}},
	    {"Rotate",{rotate,Dir}},
	    scale(),
	    separator,
	    {"Extrude",{extrude,Dir}},
	    separator,
	    {"Flatten",{flatten,flatten_dir(St)}},
	    {advanced,{"Flatten Move",{flatten_move,flatten_dir(St)}}},
	    separator,
	    {"Connect",connect,
	     "Create a new edge to connect selected vertices"},
	    {"Tighten",tighten},
	    {"Bevel",bevel,"Create faces of selected vertices"},
	    {"Collapse",collapse,"Delete selected vertices"},
	    separator,
	    wings_magnet:sub_menu(St),
	    {"Deform",wings_deform:sub_menu(St)}|wings_vec:menu(St)],
    wings_menu:popup_menu(X, Y, vertex, Menu, St).

edge_menu(X, Y, St) ->
    Dir = directions(St),
    Menu = [{"Edge operations",ignore},
	    separator,
	    {"Move",{move,Dir}},
	    {"Rotate",{rotate,Dir}},
	    scale(),
	    separator,
	    {"Extrude",{extrude,Dir}},
	    separator,
	    {"Cut",{cut,cut_fun()}},
	    {"Connect",connect,"Create a new edge to connect selected edges"},
	    {"Bevel",bevel,"Round off selected edges"},
	    separator,
	    {"Dissolve",dissolve,"Eliminate selected edges"},
	    {"Collapse",collapse,"Delete edges, replacing them with vertices"},
	    separator,
	    {"Hardness",{hardness,[{"Soft",soft},
				   {"Hard",hard}]}},
	    separator,
	    {"Loop Cut",loop_cut,"Cut into two objects along edge loop"} |
	    wings_vec:menu(St)],
    wings_menu:popup_menu(X, Y, edge, Menu, St).

face_menu(X, Y, St) ->
    Dir = directions(St),
    Menu = [{"Face operations",ignore},
	    separator,
	    {"Move",{move,Dir}},
	    {"Rotate",{rotate,Dir}},
	    scale(),
	    separator,
	    {"Extrude",{extrude,Dir}},
	    {"Extrude Region",{extrude_region,Dir}},
	    {"Extract Region",{extract_region,Dir}},
	    separator,
	    {"Flatten",{flatten,flatten_dir(St)}},
	    {advanced,{"Flatten Move",{flatten_move,flatten_dir(St)}}},
	    separator,
	    {"Inset",inset,"Inset a face inside the selected face"},
	    {"Intrude",intrude,"Carve out interior of object, "
	     "making selected faces holes"},
	    {"Bevel",bevel,"Round off edges of selected faces"},
	    {"Bridge",bridge,"Create a bridge or tunnel between two faces"},
	    {advanced,separator},
	    {"Bump",bump,"Create bump of selected faces"},
	    {advanced,{"Lift",lift_fun(St),
		       "Lifts selected face, with one one edge pinned down"}},
	    separator,
	    {"Mirror",mirror,"Make mirror of object around selected faces"},
    	    {"Dissolve",dissolve,"Eliminate all edges between selected faces"},
	    {"Collapse",collapse,"Delete faces, replacing them with vertices"},
	    separator,
	    {"Smooth",smooth,"Subdivide selected faces to smooth them"},
	    separator,
	    wings_material:sub_menu(face, St)|wings_vec:menu(St)],
    wings_menu:popup_menu(X, Y, face, Menu, St).

body_menu(X, Y, St) ->
    Dir = directions(St),
    XYZ = xyz(),
    Menu = [{"Object operations",ignore},
	    separator,
	    {"Move",{move,Dir}},
	    {"Rotate",{rotate,Dir}},
	    scale(),
	    separator,
	    {"Flip",{flip,XYZ}},
	    separator,
	    {"Invert",invert},
	    separator,
	    {"Tighten",tighten},
	    {"Smooth",smooth},
	    {"Combine",combine},
	    {"Separate",separate},
	    separator,
	    {"Cleanup",cleanup},
	    {"Auto-Smooth",auto_smooth},
	    separator,
	    {"Duplicate",{duplicate,Dir}},
	    {"Delete",delete}|wings_vec:menu(St)],
    wings_menu:popup_menu(X, Y, body, Menu, St).

directions(#st{selmode=Mode}) ->
    fun(B, Ns) ->
	    dirs(B, Mode, Ns)
    end.

dirs(1, Mode, Ns) -> dirs_1(Mode, Ns);
dirs(2, Mode, Ns) -> {vector,{pick_named,Ns}};
dirs(3, Mode, Ns) -> {vector,{pick_new,Ns}};
dirs(help, Mode, Ns) -> dirs_help(Ns).

dirs_help([move|_]) ->
    {"Move along std. axis","Move along named axis",
     "Pick vector to move along"};
dirs_help([rotate|_]) ->
    {"Rotate around std. axis","Rotate around named axis",
     "Pick vector to rotate around"};
dirs_help([scale|_]) -> "Scale selected elements";
dirs_help([extrude|_]) ->
    {"Extrude along std. axis","Extrude along named axis",
     "Pick vector to extrude along"};
dirs_help([extrude_region|_]) ->
    {"Extrude along std. axis","Extrude along named axis",
     "Pick vector to extrude along"};
dirs_help(Ns) -> "".

dirs_1(body, Ns) -> directions([free,x,y,z], Ns);
dirs_1(vertex, [rotate|_]=Ns) -> directions([free,x,y,z], Ns);
dirs_1(Other, Ns) -> directions([normal,free,x,y,z], Ns).

xyz() ->
    [{"X",x},
     {"Y",y},
     {"Z",z}].

all_xyz() ->
    [{"All",all},
     {"X",x},
     {"Y",y},
     {"Z",z}].

scale() ->
    {"Scale",{scale,fun scale/2},[]}.

scale(help, Ns) -> "";
scale(_, Ns) ->
    [scale_fun(uniform),
     scale_fun(x),
     scale_fun(y),
     scale_fun(z),
     scale_fun(radial_x),
     scale_fun(radial_y),
     scale_fun(radial_z)].

scale_fun(Dir) ->
    DirString = stringify(Dir),
    F = fun(1, Ns) -> wings_menu:build_command(Dir, Ns);
	   (2, Ns) -> {vector,{pick_named,[Dir|Ns]}};
	   (3, Ns) -> {vector,{pick_new,[Dir|Ns]}}
	end,
    Help0 = dir_help(Dir, [scale]),
    Help = {Help0,"Scale to named vector","Pick vector to scale to"},
    {DirString,F,Help,[]}.

%%%
%%% Flatten submenu.
%%%

flatten_dir(#st{selmode=Mode}) ->
    fun(B, Ns) -> flatten_dir_1(B, Mode, Ns) end.

flatten_dir_1(help, _, Ns) ->
    {"Flatten to std. planes","Flatten to named plane","Pick plane"};
flatten_dir_1(1, _, [flatten_move|_]=Ns) ->
    directions([x,y,z], Ns);
flatten_dir_1(1, vertex, Ns) ->
    directions([x,y,z], Ns);
flatten_dir_1(1, face, Ns) ->
    directions([normal,x,y,z], Ns);
flatten_dir_1(2, Mode, Ns) ->
    {vector,{pick_named,Ns}};
flatten_dir_1(3, Mode, Ns) ->
    {vector,{pick_new,Ns}};
flatten_dir_1(_, _, _) -> ignore.

%%%
%%% General directions.
%%%

directions([D|Dirs], Ns) ->
    [direction(D, Ns)|directions(Dirs, Ns)];
directions([], Ns) -> [].

direction(Dir, Ns) ->
    Help = dir_help(Dir, Ns),
    {stringify(Dir),Dir,Help}.

dir_help(Axis, Ns) when Axis == x; Axis == y; Axis == z ->
    dir_help_1(Ns, "the " ++ stringify(Axis) ++ " axis");
dir_help(radial_x, Ns) ->
    dir_help_1(Ns, [around|"around the X axis"]);
dir_help(radial_y, Ns) ->
    dir_help_1(Ns, [around|"around the Y axis"]);
dir_help(radial_z, Ns) ->
    dir_help_1(Ns, [around|"around the Z axis"]);
dir_help(normal, Ns) ->
    dir_help_1(Ns, [normal|"along its normal"]);
dir_help(free, Ns) ->
    dir_help_1(Ns, [free|"freely in all directions"]);
dir_help(uniform, [scale]) ->
    "Scale equally in all directions".

%% Normal/Free.
dir_help_1([move|_], [NF|Text]) when NF == normal; NF == free ->
    "Move each element " ++ Text;
dir_help_1([rotate|_], [free|Text]) ->
    "Rotate freely";
dir_help_1([rotate|_], [normal|Text]) ->
    "Rotate around each element's normal";
dir_help_1([extrude|_], [NF|Text]) when NF == normal; NF == free ->
    "Extrude each element, then move it " ++ Text;
dir_help_1([extrude_region|_], [normal|_]) ->
    "Extrude faces as region, then move faces along the region's normal";
dir_help_1([extrude_region|_], [free|Text]) ->
    "Extrude faces as region, then move faces " ++ Text;
dir_help_1([flatten|_], [normal|Text]) ->
    "Flatten elements to normal plane";

%% Axis
dir_help_1([move|_], Text) ->
    "Move each element along " ++ Text;
dir_help_1([extrude|_], Text) ->
    "Extrude elements, then move along " ++ Text;
dir_help_1([extrude_region|_], Text) ->
    "Extrude faces as region, then move along " ++ Text;
dir_help_1([rotate|_], Text) ->
    "Rotate around " ++ Text;
dir_help_1([scale|_], [around|Text]) ->
    "Scale " ++ Text;
dir_help_1([scale|_], Text) ->
    "Scale along " ++ Text;
dir_help_1([flatten|_], Text) ->
    "Flatten to " ++ Text;
dir_help_1([flatten_move|_], Text) ->
    "Flatten and move to " ++ Text;
dir_help_1(_, _) -> "".

lift_fun(St) ->
    fun(help, Ns) -> "";
       (1, Ns) ->
	    Funs = wings_face_cmd:lift_selection(St),
	    {vector,{pick_special,Funs}};
       (_, _) -> ignore
    end.

cut_fun() ->
    fun(help, Ns) -> "";
       (1, Ns) ->
	    [cut_entry(2),
	     cut_entry(3),
	     cut_entry(4),
	     cut_entry(5),
	     separator,
	     cut_entry(10)];
       (_, _) -> ignore
    end.

cut_entry(N) ->
    Str = integer_to_list(N),
    {Str,N,"Cut into " ++ Str ++ " edges of equal length"}.
    
patches() ->
    case wings_start:get_patches() of
	none -> [];
	{enabled,Desc} ->
	    [separator,{"Use "++Desc,disable_patches,[crossmark]}];
	{disabled,Desc} ->
	    [separator,{"Use "++Desc,enable_patches}]
    end.

info(#st{sel=[]}) -> "";
info(#st{shapes=Shapes,selmode=body,sel=[{Id,_}]}) ->
    Sh = gb_trees:get(Id, Shapes),
    shape_info(Sh);
info(#st{shapes=Shapes,selmode=body,sel=Sel}) ->
    shape_info(Sel, Shapes);
info(#st{shapes=Shapes,selmode=vertex,sel=[{Id,Sel}]}) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [V] = gb_sets:to_list(Sel),
	    flat_format("Vertex: ~p", [V]);
	N when N < 5 ->
	    Faces = gb_sets:to_list(Sel),
	    item_list(Faces, "Vertices");
	N ->
	    flat_format("~p vertices selected", [N])
    end;
info(#st{shapes=Shapes,selmode=edge,sel=[{Id,Sel}]}) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [Edge] = gb_sets:to_list(Sel),
	    #we{es=Etab} = gb_trees:get(Id, Shapes),
	    #edge{a=A,b=B} = gb_trees:get(Edge, Etab),
	    flat_format("Edge: ~p", [Edge]);
	N when N < 5 ->
	    Faces = gb_sets:to_list(Sel),
	    item_list(Faces, "Edges");
	N ->
	    flat_format("~p edges selected", [N])
    end;
info(#st{shapes=Shapes,selmode=face,sel=[{Id,Sel}]}) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [Face] = gb_sets:to_list(Sel),
	    flat_format("Face: ~p", [Face]);
	N when N < 5 ->
	    Faces = gb_sets:to_list(Sel),
	    item_list(Faces, "Faces");
	N ->
	    flat_format("~p faces selected", [N])
    end;
info(#st{selmode=Mode,sel=Sel}) ->
    On = length(Sel),
    N = foldl(fun({_,S}, A) -> A+gb_sets:size(S) end, 0, Sel),
    case Mode of
	vertex -> flat_format("~p vertices selected in ~p objects", [N,On]);
	edge -> flat_format("~p edges selected in ~p objects", [N,On]);
	face -> flat_format("~p faces selected in ~p objects", [N,On])
    end.

item_list(Items, Desc) ->
    item_list(Items, ": ", Desc).

item_list([Item|Items], Sep, Desc) ->
    item_list(Items, ", ", Desc++Sep++integer_to_list(Item));
item_list([], Sep, Desc) -> Desc.

shape_info(#we{name=Name,fs=Ftab,es=Etab,vs=Vtab}) ->
    Faces = gb_trees:size(Ftab),
    Edges = gb_trees:size(Etab),
    Vertices = gb_trees:size(Vtab),
    flat_format("~s: ~p polygons, ~p edges, ~p vertices",
		[Name,Faces,Edges,Vertices]).

shape_info(Objs, Shs) ->
    shape_info(Objs, Shs, 0, 0, 0, 0).

shape_info([{Id,_}|Objs], Shs, On, Vn, En, Fn) ->
    #we{fs=Ftab,es=Etab,vs=Vtab} = gb_trees:get(Id, Shs),
    Faces = gb_trees:size(Ftab),
    Edges = gb_trees:size(Etab),
    Vertices = gb_trees:size(Vtab),
    shape_info(Objs, Shs, On+1, Vn+Vertices, En+Edges, Fn+Faces);
shape_info([], Shs, N, Vertices, Edges, Faces) ->
    flat_format("~p objects, ~p faces, ~p edges, ~p vertices",
		[N,Faces,Edges,Vertices]).

flat_format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

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

translate_event(#keyboard{}=Event, St) ->
    case wings_hotkey:event(Event, St) of
	next -> ignore;
	Other -> Other
    end;
translate_event(quit, St) -> {file,quit};
translate_event(ignore, St) -> ignore;
translate_event(#mousebutton{}, St) -> ignore;
translate_event(#mousemotion{x=X,y=Y}, St) -> ignore;
translate_event(#resize{w=W,h=H}, St) -> {resize,W,H};
translate_event(#expose{}, St) -> redraw;
translate_event(redraw_menu, St) -> ignore;
translate_event(redraw, St) -> redraw;
translate_event({action,Action}, St) -> Action.

command_name(Repeat, #st{repeatable=ignore}) ->
    "(Can't repeat)";
command_name(Repeat, #st{repeatable={_,Cmd}}=St) ->
    CmdStr = stringify(Cmd),
    command_name(Repeat, CmdStr, St).

command_name(Repeat, CmdStr, #st{sel=[]}) ->
    lists:flatten(["(Can't repeat \"",CmdStr,"\")"]);
command_name(Repeat, CmdStr, #st{selmode=Mode,repeatable=Cmd}) ->
    S = case repeatable(Mode, Cmd) of
	    no -> ["(Can't repeat \"",CmdStr,"\")"];
	    _ ->  [Repeat++" \"",CmdStr,"\""]
	end,
    lists:flatten(S).

stringify({{_,_,_},{_,_,_}}) ->
    "(vector)";
stringify({Atom,Other}) when is_atom(Atom) ->
    wings_util:cap(atom_to_list(Atom)) ++ "|" ++ stringify(Other);
stringify(Atom) when is_atom(Atom) ->
    wings_util:cap(atom_to_list(Atom));
stringify(Int) when integer(Int) ->
    integer_to_list(Int);
stringify(Other) -> "UNKNOWN".

-ifdef(DEBUG).
wings() -> "Wings 3D [debug]".
-else.
wings() -> "Wings 3D".
-endif.
