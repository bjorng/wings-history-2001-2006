%%
%%  wings.erl --
%%
%%     The main module of Wings 3D.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings.erl,v 1.47 2001/11/17 18:25:11 bjorng Exp $
%%

-module(wings).
-export([start/0,start_halt/0]).
-export([caption/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(CTRL_BITS, (?KMOD_LCTRL bor ?KMOD_RCTRL)).
-define(ALT_BITS, (?KMOD_LALT bor ?KMOD_RALT)).
-define(SHIFT_BITS, (?KMOD_LSHIFT bor ?KMOD_RSHIFT)).
-define(INTERESTING_BITS, (?CTRL_BITS bor ?ALT_BITS)).
-import(lists, [foreach/2,map/2,filter/2,foldl/3,sort/1,
		keymember/3,reverse/1]).

start() ->
    spawn_link(fun init/0).

start_halt() ->
    spawn_link(fun() ->
		       init(),
		       halt()
	       end).

init() ->
    register(wings, self()),
    case catch init_1() of
	{'EXIT',Reason} -> io:format("Crasched: ~P\n", [Reason,30]);
	ok -> ok
    end.

init_1() ->
    {ok,Cwd} = file:get_cwd(),
    wings_plugin:init(),
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER),
    Icon = locate("wings.icon"),
    catch sdl_video:wm_setIcon(sdl_video:loadBMP(Icon), null),
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
    sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_IGNORE),
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
    wings_io:init(),
    wings_io:menubar([{"File",file},
		      {"Edit",edit},
		      {"View",view},
		      {"Select",select},
		      {"Tools",tools},
		      {"Objects",objects},
		      {"Help",help}]),
    Empty = gb_trees:empty(),
    St = #st{shapes=Empty,
	     hidden=Empty,
	     selmode=face,
	     sel=[],
	     hsel=Empty,
	     ssel={face,[]},
	     mat=wings_material:default(),
	     saved=true,
	     onext=0,
	     repeatable=ignore,
	     hit_buf=sdl_util:malloc(?HIT_BUF_SIZE, ?GL_UNSIGNED_INT)
	    },
    wings_view:init(),
    wings_file:init(),
    resize(800, 600, St),
    caption(St),
    enter_top_level(wings_undo:init(St)),
    wings_file:finish(),
    wings_pref:finish(),
    sdl:quit(),
    ok = file:set_cwd(Cwd),
    ok.

locate(Name) ->
    case filelib:is_file(Name) of
	true -> Name;
	false ->
	    Base = filename:dirname(code:which(?MODULE)),
	    Path = filename:join(Base, Name),
	    case filelib:is_file(Path) of
		true -> Path;
		false ->
		    filename:join([Base,"../src",Name])
	    end
    end.

resize(W, H, St) ->
    sdl_video:setVideoMode(W, H, 16, ?SDL_OPENGL bor ?SDL_RESIZABLE),
    gl:enable(?GL_LIGHT0),
    gl:enable(?GL_LIGHT1),
    gl:lightfv(?GL_LIGHT0, ?GL_DIFFUSE, {0.75,0.75,0.75}),
    gl:lightfv(?GL_LIGHT1, ?GL_DIFFUSE, {0.25,0.25,0.25}),
    gl:enable(?GL_DEPTH_TEST),
    {R,G,B} = wings_pref:get_value(background_color),
    gl:clearColor(R, G, B, 1.0),
    gl:viewport(0, 0, W, H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    wings_view:perspective(),
    gl:matrixMode(?GL_MODELVIEW),
    wings_io:resize(W, H).

enter_top_level(St) ->
    Init = {init,fun() -> top_level_1(St) end,{push,dummy}},
    wings_io:enter_event_loop(Init).

top_level(St) ->
    wings_io:clear_message(),
    top_level_1(St).

top_level_1(St) ->
    {init,
     fun() ->
	     {init,fun() -> main_loop(St) end,{push,dummy}}
     end,
     {replace,fun(Event) -> handle_top_event(Event, St) end}}.

handle_top_event(Event, St0) ->
    case Event of
	#st{}=St1 ->				%Undoable operation.
	    ?ASSERT(St1#st.drag == none),
	    St = wings_undo:save(St0, St1),
	    top_level(St#st{saved=false});
	{saved,St} ->
	    top_level(wings_undo:save(St0, St));
	{new,St} ->
	    top_level(wings_undo:init(St));
	drag_aborted ->
	    top_level(clean_state(St0));
	{undo,St1} -> 
	    St = wings_undo:undo(St1),
	    top_level(clean_state(St));
	{redo,St1} -> 
	    St = wings_undo:redo(St1),
	    top_level(clean_state(St));
	{undo_toggle,St1} -> 
	    St = wings_undo:undo_toggle(St1),
	    top_level(clean_state(St));
	quit ->
	    sdl_util:free(St0#st.hit_buf),
	    pop;
	{crash,Crash} ->
	    LogName = wings_util:crasch_log(Crash),
	    wings_io:message("Internal error - log written to " ++ LogName),
	    enter_top_level(St0)
    end.

clean_state(St0) ->
    St = St0#st{drag=none},
    caption(wings_draw:model_changed(St)).

main_loop(St0) ->
    ?VALIDATE_MODEL(St0),
    St = redraw(St0),
    {replace,fun(Event) -> handle_event(Event, St) end}.

handle_event(Event, St) ->
    case wings_camera:event(Event, fun() -> redraw(St) end) of
	next -> handle_event_1(Event, St);
	Other -> Other
    end.

handle_event_1({reactivate_menu,Mi}, St) -> wings_menu:reactivate(Mi);
handle_event_1(drag_aborted=S, St) -> return_to_top(S);
handle_event_1({drag_ended,St}, _) -> return_to_top(St);
handle_event_1(Event, St1) ->
    case translate_event(Event, St1) of
	ignore -> keep;
	redraw -> main_loop(St1);
	{left_click,X,Y} ->
	    #st{sel=Sel} = St1,
	    case wings_pick:pick(St1, X, Y) of
		#st{sel=Sel}=St -> main_loop(St);
		St -> return_to_top(St)
	    end;
 	{right_click,X,Y} ->
	    popup_menu(X, Y, St1);
 	{resize,W,H} ->
 	    resize(W, H, St1),
 	    main_loop(model_changed(St1));
 	{edit,undo_toggle} -> execute_or_ignore(undo_toggle, St1);
 	{edit,undo} -> execute_or_ignore(undo, St1);
 	{edit,redo} -> execute_or_ignore(redo, St1);
	{crash,_}=Crash -> next;
	Cmd -> do_command(Cmd, St1)
    end.

execute_or_ignore(Cmd, #st{drag=Drag}=St) when Drag =/= none ->
    main_loop(St);
execute_or_ignore(Cmd, St) -> return_to_top({Cmd,St}).
    
do_command(Cmd, St0) ->
    St1 = remember_command(Cmd, St0),
    case do_command_1(Cmd, St1) of
	#st{drag=none}=St -> main_loop(St);
	#st{}=StDrag ->
	    St = model_changed(St1#st{drag=none}),
	    {seq,{replace,fun(Event) -> handle_event(Event, St) end},
	     wings_drag:do_drag(StDrag)};
	{save_state,#st{}=St} -> return_to_top(St);
	{saved,#st{}}=Res -> return_to_top(Res);
	{new,#st{}}=Res -> return_to_top(Res);
	{push,_}=Push -> Push;
	{init,_,_}=Init -> Init;
	{seq,_,_}=Seq -> Seq;
	quit -> return_to_top(quit)
    end.

return_to_top(Res) ->
    wings_io:putback_event(Res),
    pop.

redraw(St0) ->
    St = wings_draw:render(St0),
    wings_io:info(info(St)),
    wings_io:update(St),
    St.
    
do_command_1(Cmd, St) ->
    command(Cmd, St).

remember_command({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
				     C =:= face; C =:= body ->
    St#st{repeatable=Cmd};
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

command({_,{[_|_]}=Plugin}, St0) ->
    case wings_plugin:command(Plugin, St0) of
	St0 -> St0;
	#st{drag=none}=St -> {save_state,model_changed(St)};
	St -> St
    end;
command({_,[_|_]=Plugin}, St0) ->
    case wings_plugin:command(Plugin, St0) of
	St0 -> St0;
	#st{drag=none}=St -> {save_state,model_changed(St)};
	St -> St
    end;
command({menu,Menu,X,Y}, St) ->
    menu(X, Y, Menu, St);
command({shape,{Shape}}, St) ->
    create_shape(true, Shape, St);
command({shape,Shape}, St) ->
    create_shape(false, Shape, St);
command({help,What}, St) ->
    wings_help:What(St);

%% File menu.
command({file,Command}, St) ->
    wings_file:command(Command, St);

%% Edit menu.
command({edit,{material,Mat}}, St) ->
    wings_material:edit(Mat, St);
command({edit,repeat}, #st{sel=[]}=St) -> St;
command({edit,repeat}, #st{selmode=Mode,repeatable=Cmd0}=St) ->
    ?ASSERT(St#st.drag == none),
    case repeatable(Mode, Cmd0) of
	no -> ok;
	Cmd when tuple(Cmd) -> wings_io:putback_event({action,Cmd})
    end,
    St;
command({edit,repeat}, St) -> St;
command({edit,{camera_mode,Mode}}, St) ->
    wings_camera:command(Mode),
    St;
command({edit,{preferences,Pref}}, St) ->
    wings_pref:command(Pref),
    St;

%% Select menu
command({select,edge_loop}, St) ->
    {save_state,wings_edge_loop:select_loop(St)};
command({select,next_edge_loop}, St) ->
    {save_state,wings_edge_loop:select_next(St)};
command({select,prev_edge_loop}, St) ->
    {save_state,wings_edge_loop:select_prev(St)};
command({select,select_region}, St) ->
    {save_state,wings_edge:select_region(St)};
command({select,more}, St) ->
    wings_sel:select_more(St);
command({select,less}, St) ->
    wings_sel:select_less(St);
command({select,{material,Material}}, St) ->
    wings_face_cmd:select_material(Material, St);
command({select,all}, St) ->
    {save_state,wings_sel:select_all(St)};
command({select,{all,Mode}}, St) ->
    wings_sel:select_all(St#st{selmode=Mode});
command({select,hard_edges}, St) ->
    Sel = fun(Edge, #we{he=Htab}) ->
		  gb_sets:is_member(Edge, Htab)
	  end,
    {save_state,wings_sel:make(Sel, edge, St)};
command({select,{vertices_with,N}}, St) ->
    Sel = fun(V, We) ->
		  Cnt = wings_vertex:fold(
			  fun(_, _, _, Cnt) ->
				  Cnt+1
			  end, 0, V, We),
		  Cnt =:= N
	  end, 
    {save_state,wings_sel:make(Sel, vertex, St)};
command({select,{faces_with,5}}, St) ->
    Sel = fun(Face, We) ->
		    length(wings_face:surrounding_vertices(Face, We)) >= 5
	    end,
    {save_state,wings_sel:make(Sel, face, St)};
command({select,{faces_with,N}}, St) ->
    Sel = fun(Face, We) ->
		  N =:= length(wings_face:surrounding_vertices(Face, We))
	  end,
    {save_state,wings_sel:make(Sel, face, St)};
command({select,similar}, St) ->
    {save_state,wings_sel:similar(St)};
command({select,{random,Percent}}, St) ->
    {save_state,wings_sel:random(Percent, St)};
command({select,save}, St) ->
    {save_state,wings_sel:save(St)};
command({select,load}, St) ->
    {save_state,wings_sel:load(St)};
command({select,exchange}, St) ->
    {save_state,wings_sel:exchange(St)};
command({select,union}, St) ->
    {save_state,wings_sel:union(St)};
command({select,subtract}, St) ->
    {save_state,wings_sel:subtract(St)};
command({select,intersection}, St) ->
    {save_state,wings_sel:intersection(St)};
command({select,inverse}, St) ->
    {save_state,wings_sel:inverse(St)};

command({select,Type}, St) ->
    set_select_mode(Type, St);

command({view,Command}, St) ->
    wings_view:command(Command, St);

%% Body menu.
command({body,invert}, St) ->
    {save_state,model_changed(wings_body:invert_normals(St))};
command({body,{duplicate,Dir}}, St) ->
    model_changed(wings_body:duplicate(Dir, St));
command({body,delete}, St) ->
    {save_state,model_changed(wings_body:delete(St))};
command({body,tighten}, St) ->
    wings_body:tighten(St);
command({body,smooth}, St) ->
    {save_state,model_changed(wings_body:smooth(St))};
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
    wings_face_cmd:extrude(Type, St);
command({face,{extrude_region,Type}}, St) ->
    wings_face_cmd:extrude_region(Type, St);
command({face,{extract_region,Type}}, St) ->
    wings_face_cmd:extract_region(Type, St);
command({face,{flatten,Plane}}, St) ->
    {save_state,model_changed(wings_face_cmd:flatten(Plane, St))};
command({face,bevel}, St) ->
    wings_face_cmd:bevel_faces(St);
command({face,inset}, St) ->
    wings_face_cmd:inset(St);
command({face,mirror}, St) ->
    {save_state,model_changed(wings_face_cmd:mirror(St))};
command({face,intrude}, St) ->
    wings_face_cmd:intrude(St);
command({face,dissolve}, St) ->
    {save_state,model_changed(wings_face_cmd:dissolve(St))};
command({face,{set_material,Mat}}, St) ->
    {save_state,model_changed(wings_face_cmd:set_material(Mat, St))};
command({face,bridge}, St0) ->
    case wings_face_cmd:bridge(St0) of
	{error,Message} ->
	    wings_io:message(Message),
	    St0;
	St -> {save_state,model_changed(St)}
    end;
command({face,smooth}, St) ->
    {save_state,model_changed(wings_face_cmd:smooth(St))};
    
%% Edge commands.
command({edge,bevel}, St) ->
    wings_extrude_edge:bevel(St);
command({edge,{extrude,Type}}, St) ->
    wings_extrude_edge:extrude(Type, St);
command({edge,{cut,Num}}, St) ->
    {save_state,model_changed(wings_edge:cut(Num, St))};
command({edge,connect}, St) ->
    {save_state,model_changed(wings_edge:connect(St))};
command({edge,dissolve}, St) ->
    {save_state,model_changed(wings_edge:dissolve(St))};
command({edge,{hardness,Type}}, St) ->
    {save_state,model_changed(wings_edge:hardness(Type, St))};
command({edge,loop_cut}, St) ->
    {save_state,model_changed(wings_edge:loop_cut(St))};

%% Vertex menu.
command({vertex,{flatten,Plane}}, St) ->
    {save_state,model_changed(wings_vertex_cmd:flatten(Plane, St))};
command({vertex,connect}, St) ->
    {save_state,model_changed(wings_vertex_cmd:connect(St))};
command({vertex,tighten}, St) ->
    wings_vertex_cmd:tighten(St);
command({vertex,bevel}, St) ->
    wings_vertex_cmd:bevel(St);
command({vertex,{extrude,Type}}, St) ->
    wings_vertex_cmd:extrude(Type, St);
command({vertex,{deform,Deform}}, St) ->
    wings_deform:command(Deform, St);

%% Magnetic commands.
command({_,{magnet,{Type,Dir}}}, St) ->
    wings_magnet:setup(Type, Dir, St);

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

command({objects,show_all}, #st{hidden=Hidden}=St) ->
    toggle_list(gb_trees:to_list(Hidden), St);
command({objects,hide_all}, #st{shapes=Shapes}=St) ->
    toggle_list(gb_trees:to_list(Shapes), St);
command({objects,toggle_all}, #st{hidden=Hidden,shapes=Shapes}=St) ->
    toggle_list(gb_trees:to_list(Shapes)++gb_trees:to_list(Hidden), St);
command({objects,hide_selected}, #st{sel=Sel}=St) ->
    toggle_list(Sel, St);
command({objects,hide_unselected}, #st{sel=Sel,shapes=Shapes}=St) ->
    Unsel = [Obj || {Id,_}=Obj <- gb_trees:to_list(Shapes),
		    not keymember(Id, 1, Sel)],
    toggle_list(Unsel, St);
command({objects,{Id}}, St) when integer(Id) ->
    {save_state,rename_object(Id, St)};
command({objects,Id}, St0) when integer(Id) ->
    model_changed(toggle_visibility(Id, St0));

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
 	{[],_} -> shape_menu(X, Y, St);
 	{_,vertex} -> vertex_menu(X, Y, St);
 	{_,edge} -> edge_menu(X, Y, St);
 	{_,face} -> face_menu(X, Y, St);
 	{_,body} -> body_menu(X, Y, St)
    end.

menu(X, Y, file, St) ->
    wings_file:menu(X, Y, St);
menu(X, Y, edit, St) ->
    Menu = {{"Undo/redo","Ctrl-Z",undo_toggle},
	    {"Redo","Shift-Ctrl-Z",redo},
	    {"Undo","Alt-Ctrl-Z",undo},
	    separator,
	    {command_name(St),"d",repeat},
	    separator,
	    {"View Material [ALPHA]",{material,materials(St)}},
	    separator,
	    wings_camera:sub_menu(St),
	    {"Preferences",{preferences,wings_pref:sub_menu(St)}}},
    wings_menu:menu(X, Y, edit, Menu);
menu(X, Y, view, St) ->
    wings_view:menu(X, Y, St);
menu(X, Y, select, St) ->
    Menu = {{"Deselect","Space",deselect},
	    separator,
	    {"More","+",more},
	    {"Less","-",less},
	    {"Region","L",select_region},
	    {"Edge Loop","l",edge_loop},
	    {"Previous Edge Loop [ALPHA]","F3",prev_edge_loop},
	    {"Next Edge Loop [ALPHA]","F4",next_edge_loop},
	    {"Similar","i",similar},
	    separator,
	    {"Adjacent vertices","v",vertex},
	    {"Adjacent edges","e",edge},
	    {"Adjacent faces","f",face},
	    separator,
	    {"All",{all,{menu_item_sel_all(vertex, "vertices", St),
			 menu_item_sel_all(face, "faces", St),
			 menu_item_sel_all(edge, "edges", St),
			 menu_item_sel_all(body, "objects", St)}}},
	    separator,
	    {"Hard edges",hard_edges},
	    {"Vertices with",{vertices_with,
			      {{"2 edges",2},
			       {"3 edges",3},
			       {"4 edges",4},
			       {"5 edges",5}}}},
	    {"Faces with",{faces_with,
			   {{"2 edges",2},
			    {"3 edges",3},
			    {"4 edges",4},
			    {"5 or more edges","F5",5}}}},
	    {"Material",{material,materials(St)}},
	    {"Random",{random,{{"10%",10},
			       {"20%",20},
			       {"30%",30},
			       {"40%",40},
			       {"50%",50},
			       {"60%",60},
			       {"70%",70},
			       {"80%",80},
			       {"90%",90}}}},
	    separator,
	    {"Inverse","Ctrl-Shift-I",inverse},
	    separator,
	    {"Save selection",save},
	    {"Load selection",load},
	    {"Exchange selection",exchange},
	    separator,
	    {"Union with saved",union},
	    {"Subtract with saved",subtract},
	    {"Intersection with saved",intersection}},
    wings_menu:menu(X, Y, select, Menu);
menu(X, Y, tools, St) ->
    Dirs = {{"All",all},
	    {"X",x},
	    {"Y",y},
	    {"Z",z},
	    {"Radial X (YZ)",radial_x},
	    {"Radial Y (XZ)",radial_y},
	    {"Radial Z (XY)",radial_z}},
    Menu = {{"Align",{align,Dirs}},
	    {"Center",{center,Dirs}},
	    separator,
	    {"Save Bounding Box",save_bb},
	    {"Scale to Saved BB",{scale_to_bb,Dirs}},
	    {"Scale to Saved BB Proportionally",{scale_to_bb_prop,Dirs}},
	    {"Move to Saved BB",{move_to_bb,all_xyz()}}},
    wings_menu:menu(X, Y, tools, Menu);
menu(X, Y, objects, #st{shapes=Shapes,hidden=Hidden}=St) ->
    All = sort(gb_trees:to_list(Shapes)++gb_trees:to_list(Hidden)),
    Menu0 = map(fun({Id,#shape{name=Name}}) ->
			case is_visible(Id, St) of
			    true ->  {"Hide "++Name,{Id}};
			    false -> {"Show "++Name,Id}
			end
		end, All),
    Menu = list_to_tuple([{"Show All",show_all},
			  {"Hide All",hide_all},
			  {"Toggle Visibility",toggle_all},
			  separator,
			  {"Hide Selected",hide_selected},
			  {"Hide Unselected",hide_unselected},
			  separator|Menu0]),
    wings_menu:menu(X, Y, objects, Menu);
menu(X, Y, help, St) ->
    Menu = {{"About",about}},
    wings_menu:menu(X, Y, help, Menu).

menu_item_sel_all(Mode, What, #st{selmode=Mode}) ->
    {cap(What),"Ctrl-A",Mode};
menu_item_sel_all(Mode, What, St) ->
    {cap(What),Mode}.

shape_menu(X, Y, St0) ->
    Menu = {{"Tetrahedron",tetrahedron},
	    {"Octahedron",octahedron},
	    {"Dodecahedron",dodecahedron},
	    {"Icosahedron",icosahedron},
	    separator,
	    {"Cube",cube},
	    separator,
	    {"Cylinder",{cylinder}},
	    {"Cone",{cone}},
	    {"Sphere",{sphere}},
	    {"Torus",{torus}},
	    separator,
	    {"Grid",{grid}}},
    wings_menu:menu(X, Y, shape, Menu).

vertex_menu(X, Y, St) ->
    XYZ = xyz(),
    Menu = {{"Vertex operations",ignore},
	    separator,
	    {"Move",{move,directions()}},
	    {"Rotate",{rotate,XYZ}},
	    scale(),
	    separator,
	    {"Extrude",{extrude,directions()}},
	    separator,
	    {"Flatten",{flatten,XYZ}},
	    separator,
	    {"Connect","C",connect},
	    {"Tighten",tighten},
	    {"Bevel",bevel},
	    {"Collapse","Bksp",collapse},
	    separator,
	    {"Magnet",{magnet,{{"Gaussian",{gaussian,directions()}},
			       {"Linear",{linear,directions()}}}}},
	    {"Deform",wings_deform:sub_menu(St)}},
    wings_menu:menu(X, Y, vertex, Menu).

edge_menu(X, Y, St) ->
    Menu = {{"Edge operations",ignore},
	    separator,
	    {"Move",{move,directions()}},
	    {"Rotate",{rotate,
		       {{"Normal",normal},
			{"X",x},
			{"Y",y},
			{"Z",z}}}},
	    scale(),
	    separator,
	    {"Extrude",{extrude,directions()}},
	    separator,
	    {"Cut",{cut,{{"2",2},
			 {"3",3},
			 {"4",4},
			 {"5",5}}}},
	    {"Connect","C",connect},
	    {"Bevel",bevel},
	    separator,
	    {"Dissolve","Bksp",dissolve},
	    {"Collapse",collapse},
	    separator,
	    {"Hardness",{hardness,{{"Soft",soft},
				   {"Hard",hard}}}},
	    separator,
	    {"Loop Cut",loop_cut}},
        wings_menu:menu(X, Y, edge, Menu).
 
face_menu(X, Y, St) ->
    Menu = {{"Face operations",ignore},
	    separator,
	    {"Move",{move,directions()}},
	    {"Rotate",{rotate,
		       {{"Normal",normal},
			{"X",x},
			{"Y",y},
			{"Z",z}}}},
	    scale(),
	    separator,
	    {"Extrude",{extrude,directions()}},
	    {"Extrude Region",{extrude_region,directions()}},
	    {"Extract Region",{extract_region,directions()}},
	    separator,
	    {"Flatten",{flatten,
			{{"Normal",normal},
			 {"X",x},
			 {"Y",y},
			 {"Z",z}}}},
	    separator,
	    {"Inset",inset},
	    {"Intrude",intrude},
	    {"Bevel",bevel},
	    {"Bridge",bridge},
	    separator,
	    {"Mirror",mirror},
    	    {"Dissolve","Bksp",dissolve},
	    {"Collapse", collapse},
	    separator,
	    {"Smooth",smooth},
	    separator,
	    {"Set Material",{set_material,materials(St)}}},
    wings_menu:menu(X, Y, face, Menu).
body_menu(X, Y, St) ->
    Dir = {{"Free",free},
	   {"X",x},
	   {"Y",y},
	   {"Z",z}},
    XYZ = xyz(),
    Menu = {{"Object operations",ignore},
	    separator,
	    {"Move",{move,Dir}},
	    {"Rotate",{rotate,XYZ}},
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
	    {"Auto-Smooth","S",auto_smooth},
	    separator,
	    {"Duplicate",{duplicate,Dir}},
	    {"Delete","Bksp",delete}},
    wings_menu:menu(X, Y, body, Menu).

materials(#st{mat=Mat0}) ->
    L0 = map(fun(Id) ->
		     Name = case atom_to_list(Id) of
				[H|T] when $a =< H, H =< $z ->
				    [H-$\s|T];
				Name0 -> Name0
			    end,
		     {Name,Id}
	     end, gb_trees:keys(Mat0)),
    list_to_tuple(L0).

directions() ->
    {{"Normal",normal},
     {"Free",free},
     {"X",x},
     {"Y",y},
     {"Z",z}}.

xyz() ->
    {{"X",x},
     {"Y",y},
     {"Z",z}}.

all_xyz() ->
    {{"All",all},
     {"X",x},
     {"Y",y},
     {"Z",z}}.

scale() ->
    {"Scale",{scale,
	      {{"Uniform",uniform},
	       {"X",x},
	       {"Y",y},
	       {"Z",z},
	       {"Radial X (YZ)",radial_x},
	       {"Radial Y (XZ)",radial_y},
	       {"Radial Z (XY)",radial_z}}}}.

model_changed(St) ->
    wings_draw:model_changed(St).

create_shape(Ask, Shape, St0) ->
    case wings_shapes:Shape(Ask, St0) of
	aborted -> St0;
	St -> {save_state,model_changed(St)}
    end.

set_select_mode(Mode, #st{drag=Drag}=St) when Drag =/= none ->
    St;
set_select_mode(deselect, St) ->
    {save_state,model_changed(St#st{sel=[]})};
set_select_mode(Type, St) ->
    {save_state,model_changed(wings_sel:convert_selection(Type, St))}.

info(#st{shapes=Shapes,selmode=body,sel=[{Id,_}]}) ->
    Sh = gb_trees:get(Id, Shapes),
    shape_info(Sh);
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
info(St) -> "".

item_list(Items, Desc) ->
    item_list(Items, ": ", Desc).

item_list([Item|Items], Sep, Desc) ->
    item_list(Items, ", ", Desc++Sep++integer_to_list(Item));
item_list([], Sep, Desc) -> Desc.

shape_info(#shape{name=Name,sh=#we{fs=Ftab,es=Etab,vs=Vtab}}) ->
    Faces = gb_trees:size(Ftab),
    Edges = gb_trees:size(Etab),
    Vertices = gb_trees:size(Vtab),
    flat_format("~s: ~p polygons, ~p edges, ~p vertices",
		[Name,Faces,Edges,Vertices]).
    
flat_format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

caption(#st{file=undefined}=St) ->
    Caption = wings(),
    sdl_video:wm_setCaption(Caption, Caption),
    St;
caption(#st{file=Name}=St) ->
    Caption = wings() ++ " - " ++ filename:basename(Name),
    sdl_video:wm_setCaption(Caption, Caption),
    St.

is_visible(Id, #st{shapes=Shapes}) ->
    gb_trees:is_defined(Id, Shapes).

toggle_list(Ids, St) ->
    foldl(fun({Id,_}, A) ->
		  toggle_visibility(Id, A)
	  end, model_changed(St), Ids).

toggle_visibility(Id, #st{selmode=SelMode,sel=Sel0,hsel=Hsel0,
			  shapes=Shapes0,hidden=Hidden0}=St) ->
    case gb_trees:is_defined(Id, Shapes0) of
	true ->
	    {Shapes,Hidden} = move_shape(Id, Shapes0, Hidden0),
	    case orddict:find(Id, Sel0) of
		error ->
		    St#st{shapes=Shapes,hidden=Hidden};
		{ok,Items} ->
		    Sel = orddict:erase(Id, Sel0),
		    Hsel = gb_trees:insert(Id, {SelMode,Items}, Hsel0),
		    St#st{sel=Sel,hsel=Hsel,shapes=Shapes,hidden=Hidden}
	    end;
	false ->
	    {Hidden,Shapes} = move_shape(Id, Hidden0, Shapes0),
	    case gb_trees:lookup(Id, Hsel0) of
		none ->
		    St#st{shapes=Shapes,hidden=Hidden};
		{value,{SelMode,Items}} ->
		    Hsel = gb_trees:delete(Id, Hsel0),
		    Sel = orddict:store(Id, Items, Sel0),
		    St#st{sel=Sel,hsel=Hsel,shapes=Shapes,hidden=Hidden};
		{value,{OtherMode,Items}} ->
		    Hsel = gb_trees:delete(Id, Hsel0),
		    St#st{hsel=Hsel,shapes=Shapes,hidden=Hidden}
	    end
    end.

move_shape(Id, From, To) ->    
    Item = gb_trees:get(Id, From),
    {gb_trees:delete(Id, From),gb_trees:insert(Id, Item, To)}.

rename_object(Id, #st{shapes=Shapes0}=St) ->
    #shape{name=Name0} = Sh = gb_trees:get(Id, Shapes0),
    case wings_getline:string("New name: ", Name0) of
	aborted -> St;
	Name when list(Name) ->
	    Shapes = gb_trees:update(Id, Sh#shape{name=Name}, Shapes0),
	    St#st{shapes=Shapes}
    end.

translate_event(#keyboard{}=Event, St) ->
    translate_key(Event, St);
translate_event(quit, St) -> {file,quit};
translate_event(ignore, St) -> ignore;
translate_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, St) ->
    ignore;
translate_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED}=Mb, St) ->
    case wings_io:button(X, Y) of
	none -> {left_click,X,Y};
	Other -> Other
    end;
translate_event(#mousebutton{button=3,x=X,y=Y,state=?SDL_PRESSED}, St) ->
    ignore;
translate_event(#mousebutton{button=3,x=X,y=Y,state=?SDL_RELEASED}, St) ->
    {right_click,X,Y};
translate_event(#mousebutton{}, St) ->
    %% Some mouse drivers map the scroll wheel to button 4 and 5.
    ignore;
translate_event(#mousemotion{x=X,y=Y}, St) -> ignore;
translate_event(#resize{w=W,h=H}, St) -> {resize,W,H};
translate_event(#expose{}, St) -> redraw;
translate_event(redraw_menu, St) -> ignore;
translate_event({menu_action,Action}, St) ->
    wings_io:putback_event({action,Action}),
    redraw;
translate_event(redraw, St) -> redraw;
translate_event({action,Action}, St) -> Action.

translate_key(Event, St) ->
    case wings_hotkey:event(Event) of
	next -> translate_key_1(Event, St);
	Other -> Other
    end.

translate_key_1(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=C}}, St) ->
    translate_key_1(Sym, Mod, C, St).

translate_key_1($\b, Mod, C, #st{selmode=vertex}) -> {vertex,collapse};
translate_key_1($\b, Mod, C, #st{selmode=edge}) -> {edge,dissolve};
translate_key_1($\b, Mod, C, #st{selmode=face}) -> {face,dissolve};
translate_key_1($\b, Mod, C, #st{selmode=body}) -> {body,delete};
translate_key_1(Sym, Mod, C, St) -> translate_key_2(C, St).
    
translate_key_2($c, #st{selmode=vertex}) -> {vertex,connect};
translate_key_2($c, #st{selmode=edge}) -> {edge,connect};
translate_key_2($L, #st{selmode=edge}) -> {select,select_region};
translate_key_2(_, _) -> ignore.

command_name(#st{repeatable=ignore}) ->
    "(Can't repeat)";
command_name(#st{repeatable={_,Cmd}}=St) ->
    CmdStr = stringify(Cmd),
    command_name(CmdStr, St).

command_name(CmdStr, #st{sel=[]}) ->
    lists:flatten(["(Can't repeat \"",CmdStr,"\")"]);
command_name(CmdStr, #st{selmode=Mode,repeatable=Cmd}) ->
    S = case repeatable(Mode, Cmd) of
	    no -> ["(Can't repeat \"",CmdStr,"\")"];
	    _ ->  ["Repeat \"",CmdStr,"\""]
	end,
    lists:flatten(S).

stringify({Atom,Other}) when atom(Atom) ->
    cap(atom_to_list(Atom)) ++ "->" ++ stringify(Other);
stringify(Atom) when atom(Atom) ->
    cap(atom_to_list(Atom));
stringify(Int) when integer(Int) ->
    integer_to_list(Int).

cap(Str) ->
    cap(Str, true).
cap([Lower|T], true) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|cap(T, false)];
cap([$_|T], Any) ->
    [$\s|cap(T, true)];
cap([H|T], Any) ->
    [H|cap(T, false)];
cap([], Flag) -> [].

-ifdef(DEBUG).
wings() -> "Wings 3D [debug]".
-else.
wings() -> "Wings 3D".
-endif.
