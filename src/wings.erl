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
%%     $Id: wings.erl,v 1.4 2001/08/27 07:34:52 bjorng Exp $
%%

-module(wings).
-export([start/0]).
-export([caption/1]).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").
-include("glu.hrl").
-include("wings.hrl").

-define(CTRL_BITS, (?KMOD_LCTRL bor ?KMOD_RCTRL)).
-define(ALT_BITS, (?KMOD_LALT bor ?KMOD_RALT)).
-define(SHIFT_BITS, (?KMOD_LSHIFT bor ?KMOD_RSHIFT)).
-define(INTERESTING_BITS, (?CTRL_BITS bor ?ALT_BITS bor ?SHIFT_BITS)).
-import(lists, [foreach/2,map/2,filter/2,foldl/3,sort/1,
		keymember/3,reverse/1]).

start() ->
    spawn_link(fun init/0).

init() ->
    register(wings, self()),
    case catch init_1() of
	{'EXIT',Reason} -> io:format("Crasched: ~P\n", [Reason,30]);
	ok -> ok
    end.

init_1() ->
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER),
    catch sdl_video:wm_setIcon(sdl_video:loadBMP(locate("wings.icon")), null),
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

    wings_io:init(),
    wings_io:menubar([{"File",file},
		      {"Edit",edit},
		      {"View",view},
		      {"Select",select},
		      {"Align",align},
		      {"Objects",objects},
		      {"Help",help}]),
    Empty = gb_trees:empty(),
    St0 = #st{shapes=Empty,
	      hidden=Empty,
	      selmode=face,
	      sel=[],
	      hsel=Empty,
	      ssel={face,[]},
	      mat=wings_material:default(),
	      saved=true,
	      opts=#opt{},
	      onext=0,
	      last_command=ignore,
	      hit_buf=sdl_util:malloc(?HIT_BUF_SIZE, ?GL_UNSIGNED_INT)},
    St = wings_view:default_view(St0),
    resize(800, 600, St),
    caption(St),
    top_level(St, wings_undo:new(24)),
    sdl:quit(),
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
    gl:enable(?GL_DEPTH_TEST),
    gl:clearColor(0.6, 0.6, 0.5, 1.0),
    gl:viewport(0, 0, W, H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    wings_view:perspective(St),
    gl:matrixMode(?GL_MODELVIEW),
    wings_io:resize(W, H).

top_level(St, Undo) ->
    wings_io:clear_message(),
    top_level_1(St, Undo).

top_level_1(St0, Undo0) ->
    case
	catch
	main_loop(St0) of
	St when record(St, st) ->		%Undoable operation.
	    ?ASSERT(St#st.drag == undefined),
	    ?ASSERT(St#st.camera == undefined),
	    Undo = wings_undo:save(St0, Undo0),
	    top_level(St#st{saved=false}, Undo);
	{saved,St} ->
	    Undo = wings_undo:save(St0, Undo0),
	    top_level(St, Undo);
	{new,St} ->
	    top_level(St, wings_undo:new(?UNDO_LEVELS));
	drag_aborted ->
	    top_level(clean_state(St0), Undo0);
	{undo,St1} -> 
	    {St,Undo} = wings_undo:undo(St1, Undo0),
	    top_level(clean_state(St), Undo);
	{redo,St1} -> 
	    {St,Undo} = wings_undo:redo(St1, Undo0),
	    top_level(clean_state(St), Undo);
	{undo_toggle,St1} -> 
	    {St,Undo} = wings_undo:undo_toggle(St1, Undo0),
	    top_level(clean_state(St), Undo);
	quit ->
	    sdl_util:free(St0#st.hit_buf),
	    ok;
	{'EXIT',Crasch} ->
	    LogName = wings_util:crasch_log(Crasch),
	    wings_io:message("Internal error - log written to " ++ LogName),
	    top_level_1(St0, Undo0)
    end.

clean_state(St0) ->
    St = St0#st{camera=undefined,drag=undefined},
    caption(wings_draw:model_changed(St)).

main_loop(St0) ->
    ?VALIDATE_MODEL(St0),
    St1 = wings_draw:render(St0),
    wings_io:info(info(St1)),
    wings_io:update(St1),
    
    %%
    %% To support undo, operations that have changed the
    %% state of the model should return the new state.
    %% Other operations (such as view changes) should
    %% continue to loop in this function.
    %%
    
    case translate_event(wings_io:get_event(), St1) of
	redraw ->
	    main_loop(St1);
	{mousemotion,X,Y} ->
	    main_loop(wings_drag:motion(X, Y, St1));
	{left_click,X,Y} ->
	    case wings_drag:click(X, Y, St1) of
		{select,#st{sel=Sel}=St2} ->
		    case wings_draw:select(St2, X, Y) of
			#st{sel=Sel}=St -> main_loop(St);
			St -> St
		    end;
		{drag_ended,St} -> St
	    end;
 	{start_camera,X,Y} ->
 	    main_loop(wings_drag:start_camera(X, Y, St1));
	{stop_camera,X,Y} ->
	    main_loop(wings_drag:stop_camera(St1));
 	{right_click,X,Y} when St1#st.camera == undefined ->
 	    case wings_drag:abort_drag(St1) of
 		no_drag ->
		    popup_menu(X, Y, St1),
		    main_loop(St1);
 		drag_aborted ->
		    drag_aborted
 	    end;
	{right_click,X,Y} ->
	    main_loop(St1);
 	{resize,W,H} ->
 	    resize(W, H, St1),
 	    main_loop(model_changed(St1));
 	{edit,undo_toggle} -> execute_or_ignore(undo_toggle, St1);
 	{edit,undo} -> execute_or_ignore(undo, St1);
 	{edit,redo} -> execute_or_ignore(redo, St1);
	Cmd -> do_command(Cmd, St1)
    end.

execute_or_ignore(Cmd, #st{camera=Camera}=St) when Camera =/= undefined ->
    main_loop(St);
execute_or_ignore(Cmd, #st{drag=Drag}=St) when Drag =/= undefined ->
    main_loop(St);
execute_or_ignore(Cmd, St) -> {Cmd,St}.
    
do_command(Cmd, St0) ->
    St1 = remember_command(Cmd, St0),
    case do_command_1(Cmd, St1) of
	quit -> quit;
	#st{}=St -> main_loop(St);
	{save_state,#st{}=St} -> St;
	{saved,#st{}}=Res -> Res;
	{new,#st{}}=Res -> Res
    end.

% do_command_1({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
% 				 C =:= face; C =:= body ->
%     {Time,Res} = timer:tc(erlang, apply, [fun command/2,[Cmd,St]]),
%     io:format("~w: ~s\n", [Time,command_name(St)]),
%     Res;
do_command_1(Cmd, St) ->
    command(Cmd, St).
    

remember_command({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
				     C =:= face; C =:= body ->
    St#st{last_command=Cmd};
remember_command(Cmd, St) -> St.

command(ignore, St) ->
    St;
command({menu,Menu,X,Y}, St) ->
    menu(X, Y, Menu, St),
    St;
command({shape,{Shape}}, St0) ->
    case wings_shapes:Shape(dummy, St0) of
	aborted -> St0;
	St -> {save_state,model_changed(St)}
    end;
command({shape,Shape}, St) ->
    {save_state,model_changed(wings_shapes:Shape(St))};
command({help,What}, St) ->
    wings_help:What(St);

%% File menu.
command({file,new}, St0) ->
    case wings_file:new(St0) of
	aborted -> St0;
	St0 -> St0;
	St -> {new,model_changed(St)}
    end;
command({file,open}, St0) ->
    case wings_file:read(St0) of
	St0 -> St0;
	St -> {new,model_changed(St)}
    end;
command({file,merge}, St0) ->
    case wings_file:merge(St0) of
	St0 -> St0;
	St -> {save_state,model_changed(St)}
    end;
command({file,save}, St) ->
    save(St);
command({file,save_as}, St0) ->
    case wings_file:save_as(St0) of
	aborted -> St0;
	#st{}=St -> {saved,St}
    end;
command({file,delete}, St) ->
    wings_file:delete(St),
    St;
command({file,{import,Type}}, St0) ->
    case wings_file:import(Type, St0) of
	St0 -> St0;
	St -> {save_state,model_changed(St)}
    end;
command({file,{export,Type}}, St) ->
    wings_file:export(Type, St),
    St;
command({file,quit}, St) ->
    quit(St);

%% Edit menu.
command({edit,{material,Mat}}, St) ->
    wings_material:edit(Mat, St);
command({edit,repeat}, #st{sel=[]}=St) -> St;
command({edit,repeat}, #st{drag=undefined,camera=undefined,
			   selmode=Mode,last_command={Mode,_}=Cmd}=St) ->
    wings_io:putback_event({action,Cmd}),
    St;
command({edit,repeat}, St) -> St;
command({edit,copy_bb}, St) ->
    wings_align:copy_bb(St);
command({edit,paste_bb}, St) ->
    {save_state,model_changed(wings_align:paste_bb(St))};

%% Select menu
command({select,edge_loop}, St) ->
    {save_state,wings_edge:select_loop(St)};
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
    wings_sel:make(fun(Edge, #we{he=Htab}) ->
			   gb_sets:is_member(Edge, Htab)
		   end, edge, St);
command({select,{vertices_with,N}}, St) ->
    wings_sel:make(
      fun(V, We) ->
	      Cnt = wings_vertex:fold(
		      fun(_, _, _, Cnt) ->
			      Cnt+1
		      end, 0, V, We),
	      Cnt =:= N
      end, vertex, St);
command({select,{faces_with,N}}, St) ->
    wings_sel:make(
      fun(Face, We) ->
	      N =:= length(wings_face:surrounding_vertices(Face, We))
      end, face, St);
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

%% Miscellanous.
command({view,reset}, St) ->
    wings_view:default_view(St);
command({view,toggle_wireframe}, St) ->
    toggle_option(#opt.wire, St);
command({view,toggle_groundplane}, St) ->
    toggle_option(#opt.ground, St);
command({view,toggle_axes}, St) ->
    toggle_option(#opt.axes, St);
command({view,smooth}, St) ->
    model_changed(toggle_option(#opt.smooth, St));
command({view,toggle_ortho}, St) ->
    wings_view:projection(toggle_option(#opt.ortho, St));
command({view,aim}, St) ->
    wings_view:aim(St);
command({view,info}, St) ->
    print_info(St),
    St;
command({view,{along,Axis}}, St) ->
    wings_view:along(Axis, St);
command({view,flyaround}, St) ->
    case wings_io:has_periodic_event() of
	true -> wings_io:cancel_periodic_event();
	false -> wings_io:periodic_event(60, {view,rotate_left})
    end,
    St;
command({view,rotate_left}, #st{azimuth=Az0}=St) ->
    Az = Az0 + 1.0,
    St#st{azimuth=Az};

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
    {save_state,wings_body:cleanup(St)};

%% Face menu.
command({face,{extrude,Type}}, St) ->
    wings_face_cmd:extrude(Type, St);
command({face,{extrude_region,Type}}, St) ->
    wings_face_cmd:extrude_regions(Type, St);
command({face,{extract_region,Type}}, St) ->
    wings_face_cmd:extract_region(Type, St);
command({face,{flatten,Plane}}, St) ->
    {save_state,model_changed(wings_face_cmd:flatten(Plane, St))};
command({face,bevel}, St) ->
    wings_bevel:bevel_faces(St);
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
command({vertex,{deform,{Deformer,{Primary,Effect}}}}, St) ->
    wings_deform:Deformer(Primary, Effect, St);
command({vertex,{deform,{Deformer,Axis}}}, St) ->
    wings_deform:Deformer(Axis, St);
command({vertex,{deform,Deformer}}, St) ->
    wings_deform:Deformer(St);

%% Magnetic commands.
command({influence_radius,Sign}, #st{inf_r=InfR0}=St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    case InfR0+Sign*?GROUND_GRID_SIZE/4 of
	InfR when InfR > 0 ->
	    wings_drag:motion(X, Y, St#st{inf_r=InfR});
	Other -> St
    end;
command({_,{magnet,{Type,Dir}}}, St) ->
    wings_magnet:setup(Type, Dir, St);

%% Align menu.

command({align,{align,Dir}}, St) ->
    {save_state,model_changed(wings_align:align(Dir, St))};
command({align,{center,Dir}}, St) ->
    {save_state,model_changed(wings_align:center(Dir, St))};

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
    Menu = {{"New","Ctrl-N",new},
	    {"Open","Ctrl-O",open},
	    {"Merge","Ctrl-L",merge},
	    separator,
	    {"Save","Ctrl-S",save},
	    {"Save As",save_as},
	    separator,
	    {"Import",{import,
		       {{"3D Studio (.3ds)",tds},
			{"Wawefront (.obj)",obj}}}},
	    {"Export",{export,
		       {{"3D Studio (.3ds)",tds},
			{"Wawefront (.obj)",obj},
			{"RenderMan (.rib)",rib}}}},
	    separator,
	    {"Delete File",delete},
	    separator,
	    {"Exit","Ctrl-Q",quit}},
    wings_menu:menu(X, Y, file, Menu);
menu(X, Y, edit, St) ->
    Menu = {{"Undo/redo","Ctrl-Z",undo_toggle},
	    {"Redo","Shift-Ctrl-Z",redo},
	    {"Undo","Alt-Ctrl-Z",undo},
	    separator,
	    {"Copy Bounding Box","Alt-C",copy_bb},
	    {"Paste Bounding Box","Alt-V",copy_bb},
	    separator,
	    {command_name(St),"d",repeat},
	    separator,
	    {"Material",{material,materials(St)}}},
    wings_menu:menu(X, Y, edit, Menu);
menu(X, Y, view, #st{opts=#opt{wire=Wire,ground=G,axes=A,smooth=S,ortho=O}}) ->
    Menu = {{one_of(G, "Hide", "Show") ++ " ground plane",toggle_groundplane},
	    {one_of(A, "Hide", "Show") ++ " axes",toggle_axes},
	    separator,
	    {one_of(Wire, "Filled", "Wireframe"),"w",toggle_wireframe},
	    {one_of(S, "Flat Apperance", "Smooth Preview"),"Tab",smooth},
	    separator,
	    {"Reset View","r",reset},
	    {"Aim","a",aim},
	    {one_of(O, "Perspective View", "Ortographic View"),"o",toggle_ortho},
	    separator,
	    {"View Along",{along,{{"+X","x",x},
				  {"+Y","y",y},
				  {"+Z","z",z},
				  {"-X","X",neg_x},
				  {"-Y","Y",neg_y},
				  {"-Z","Z",neg_z}}}}},
    wings_menu:menu(X, Y, view, Menu);
menu(X, Y, select, St) ->
    Menu = {{"Deselect","Space",deselect},
	    separator,
	    {"More","+",more},
	    {"Less","-",less},
	    {"Edge Loop","l",edge_loop},
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
			    {"5 edges",5}}}},
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
menu(X, Y, align, St) ->
    Dirs = {{"XYZ",all},
	    {"X",x},
	    {"Y",y},
	    {"Z",z},
	    {"Radial X (YZ)",radial_x},
	    {"Radial Y (XZ)",radial_y},
	    {"Radial Z (XY)",radial_z}},
    Menu = {{"Align",{align,Dirs}},
	    {"Center",{center,Dirs}}},
    wings_menu:menu(X, Y, align, Menu);
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
	    {"Deform",{deform,{{"Crumple",crumple},
			       {"Inflate",inflate},
			       {"Taper",{taper,
					 {{"Along",ignore},
					  separator,
					  {"X",taper(x)},
					  {"Y",taper(y)},
					  {"Z",taper(z)}}}},
			       {"Twist",{twist,XYZ}}}}}},
    wings_menu:menu(X, Y, vertex, Menu).

taper(x) -> taper_1([yz,y,z], x, []);
taper(y) -> taper_1([xz,x,z], y, []);
taper(z) -> taper_1([xy,x,y], z, []).

taper_1([H|T], Label, Acc) ->		    
    taper_1(T, Label, [{upper(atom_to_list(H)),H}|Acc]);
taper_1([], Label, Acc) ->
    {Label,list_to_tuple([{"Effect",ignore},
			  separator|reverse(Acc)])}.
    
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
	    {"Auto-Smooth",auto_smooth},
	    separator,
	    {"Duplicate",{duplicate,Dir}},
	    {"Delete","Bksp",delete}},
    wings_menu:menu(X, Y, body, Menu).

materials(#st{mat=Mat0}) ->
    L0 = map(fun({Id,_}) ->
		     Name = case atom_to_list(Id) of
				[H|T] when $a =< H, H =< $z ->
				    [H-$\s|T];
				Name0 -> Name0
			    end,
		     {Name,Id}
	     end, gb_trees:to_list(Mat0)),
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

save(St0) ->
    case wings_file:save(St0) of
	aborted -> St0;
	St0 -> St0;
	St  -> {saved,St}
    end.

quit(#st{saved=true}) -> quit;
quit(St) ->
    case wings_getline:yes_no("Do you want to save before quitting?") of
	no -> quit;
	yes ->
	    case save(St) of
		aborted -> St;
		Other -> quit
	    end;
	aborted -> St
    end.

set_select_mode(Mode, #st{drag=Drag,camera=Camera}=St)
  when Drag =/= undefined; Camera =/= undefined ->
    St;
set_select_mode(Mode, #st{camera=Camera}=St) when Camera =/= undefined ->
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
    
print_info(#st{shapes=Shapes}) ->
    io:format("Memory used: ~p\n", [process_info(self(),memory)]),
    foreach(fun(Sh) -> print_info_1(Sh) end, gb_trees:to_list(Shapes)).

print_info_1({Id,Sh}) ->
    io:put_chars(shape_info(Sh)),
    io:nl().

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

translate_event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod}}, St) ->
    translate_key(Sym, Mod, St);
translate_event(quit, St) -> {file,quit};
translate_event(ignore, ST) -> ignore;
translate_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, St) ->
    ignore;
translate_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED}=Mb, St) ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    translate_event(Mb#mousebutton{button=2}, St);
	_ ->
	    case wings_io:button(X, Y) of
		none -> {left_click,X,Y};
		Other -> Other
	    end
    end;
translate_event(#mousebutton{button=2,x=X,y=Y,state=?SDL_PRESSED}, St) ->
    {start_camera,X,Y};
translate_event(#mousebutton{button=2,x=X,y=Y,state=?SDL_RELEASED}, St) ->
    {stop_camera,X,Y};
translate_event(#mousebutton{button=3,x=X,y=Y,state=?SDL_PRESSED}, St) ->
    ignore;
translate_event(#mousebutton{button=3,x=X,y=Y,state=?SDL_RELEASED}, St) ->
    {right_click,X,Y};
translate_event(#mousebutton{}, St) ->
    %% Some mouse drivers map the scroll wheel to button 4 and 5.
    ignore;
translate_event(#mousemotion{x=X,y=Y}, St) ->
    {mousemotion,X,Y};
translate_event(#resize{w=W,h=H}, St) -> {resize,W,H};
translate_event(#expose{}, St) -> ignore;
translate_event({action,Action}, St) -> Action.

translate_key($c, Mod, St) when Mod band ?ALT_BITS =/= 0 -> {edit,copy_bb};
translate_key($v, Mod, St) when Mod band ?ALT_BITS =/= 0 -> {edit,paste_bb};
translate_key($a, Mod, St) when Mod band ?CTRL_BITS =/= 0 -> {select,all};
translate_key($i, Mod, St) when Mod band ?CTRL_BITS =/= 0,
				Mod band ?SHIFT_BITS =/= 0 -> {select,inverse};
translate_key($l, Mod, St) when Mod band ?CTRL_BITS =/= 0 -> {file,merge};
translate_key($n, Mod, St) when Mod band ?CTRL_BITS =/= 0 -> {file,new};
translate_key($o, Mod, St) when Mod band ?CTRL_BITS =/= 0 -> {file,open};
translate_key($q, Mod, St) when Mod band ?CTRL_BITS =/= 0 -> {file,quit};
translate_key($s, Mod, St) when Mod band ?CTRL_BITS =/= 0 -> {file,save};
translate_key($z, Mod, St) when Mod band ?ALT_BITS =/= 0,
			    Mod band ?CTRL_BITS =/= 0 -> {edit,undo};
translate_key($z, Mod, St) when Mod band ?SHIFT_BITS =/= 0,
			    Mod band ?CTRL_BITS =/= 0 -> {edit,redo};
translate_key($z, Mod, St) when Mod band ?CTRL_BITS =/= 0 -> {edit,undo_toggle};
%% Backspace.
translate_key($\b, Mod, #st{selmode=vertex}) -> {vertex,collapse};
translate_key($\b, Mod, #st{selmode=edge}) -> {edge,dissolve};
translate_key($\b, Mod, #st{selmode=face}) -> {face,dissolve};
translate_key($\b, Mod, #st{selmode=body}) -> {body,delete};
translate_key($x, Mod, St) when Mod band ?SHIFT_BITS =/= 0 ->
    {view,{along,neg_x}};
translate_key($y, Mod, St) when Mod band ?SHIFT_BITS =/= 0 ->
    {view,{along,neg_y}};
translate_key($z, Mod, St) when Mod band ?SHIFT_BITS =/= 0 ->
    {view,{along,neg_z}};
translate_key(Sym, Mod, #st{drag=Drag}=St)
  when Mod band ?INTERESTING_BITS == 0 ->
    case {Sym,Drag} of
	{?SDLK_KP_PLUS,undefined} -> {select,more};
	{?SDLK_KP_MINUS,undefined} -> {select,less};
	{?SDLK_KP_PLUS,_} -> {influence_radius,1};
	{?SDLK_KP_MINUS,_} -> {influence_radius,-1};
	{_,_} -> translate_key(Sym, St)
    end;
translate_key(_, _, St) -> ignore.

translate_key($\s, St) -> {select,deselect};
translate_key($a, St) -> {view,aim};
translate_key($b, St) -> {select,body};
translate_key($c, #st{selmode=vertex}) -> {vertex,connect};
translate_key($c, #st{selmode=edge}) -> {edge,connect};
translate_key($d, St) -> {edit,repeat};
translate_key($e, St) -> {select,edge};
translate_key($f, St) -> {select,face};
translate_key($i, St) -> {select,similar};
translate_key($l, St) -> {select,edge_loop};
translate_key($o, St) -> {view,toggle_ortho};
translate_key($p, St) -> {view,info};
translate_key($r, St) -> {view,reset};
translate_key($u, St) -> {view,flyaround};
translate_key($v, St) -> {select,vertex};
translate_key($w, St) -> {view,toggle_wireframe};
translate_key($x, St) -> {view,{along,x}};
translate_key($y, St) -> {view,{along,y}};
translate_key($z, St) -> {view,{along,z}};
translate_key(?SDLK_TAB, St)  -> {view,smooth};
translate_key(?SDLK_PLUS, St)  -> {select,more};
translate_key(?SDLK_MINUS, St)  -> {select,less};
translate_key(_, _) -> ignore.

toggle_option(Field, #st{opts=Opts}=St) ->
    Val = element(Field, Opts),
    St#st{opts=setelement(Field, Opts, not Val)}.

one_of(true, S, _) -> S;
one_of(false,_, S) -> S.

command_name(#st{last_command=ignore}) ->
    "(Can't repeat)";
command_name(#st{last_command={_,Cmd}}=St) ->
    CmdStr = stringify(Cmd),
    command_name(CmdStr, St).

command_name(CmdStr, #st{sel=[]}) ->
    lists:flatten(["(Can't repeat \"",CmdStr,"\")"]);
command_name(CmdStr, #st{selmode=Mode,last_command={Mode,_}}) ->
    lists:flatten(["Repeat \"",CmdStr,"\""]);
command_name(CmdStr, St) ->
    lists:flatten(["(Can't repeat \"",CmdStr,"\")"]).

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

upper([Lower|T]) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|upper(T)];
upper([H|T]) ->
    [H|upper(T)];
upper([]) -> [].

-ifdef(DEBUG).
wings() -> "Wings [debug]".
-else.
wings() -> "Wings".
-endif.
