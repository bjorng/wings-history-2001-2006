%%
%%  wings_pref.erl --
%%
%%     Preference management.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pref.erl,v 1.94 2003/09/06 06:58:20 bjorng Exp $
%%

-module(wings_pref).
-export([init/0,finish/0,
	 menu/1,command/2,
	 get_value/1,get_value/2,set_value/2,set_default/2,
	 delete_value/1]).

-define(NEED_ESDL, 1).    %% Some keybindings
-include("wings.hrl").
-import(lists, [foreach/2,keysearch/3,map/2,reverse/1,sort/1]).

-define(MAC_PREFS, "Library/Preferences/Wings 3D Preferences.txt").
-define(WIN32_PREFS, "Preferences").

init() ->
    ets:new(wings_state, [named_table,public,ordered_set]),
    ets:insert(wings_state, defaults()),
    wings_hotkey:set_default(),
    case old_pref_file() of
	none -> ok;
	Pref ->
	    case file:consult(Pref) of
		{ok,List0} ->
		    List = clean(List0),
		    catch ets:insert(wings_state, List);
		{error,_Reason} ->
		    ok
	    end
    end.

finish() ->
    PrefFile = new_pref_file(),
    List0 = ets:tab2list(wings_state),
    List = prune_defaults(List0),
    Write = fun({{bindkey,_},_,default}) -> [];
	       ({{bindkey,_},_,plugin}) -> [];
	       ({{bindkey,_,_},_,default}) -> [];
	       ({{bindkey,_,_},_,plugin}) -> [];
	       (Else) -> io_lib:format("~p. \n", [Else])
	    end,
    Str = lists:map(Write, List),
    catch file:write_file(PrefFile, Str),
    ok.

prune_defaults(List) ->
    List -- defaults().

menu(_St) ->
    [{"Preferences...",
      fun(_, _) ->
	      {edit,{preferences,prefs}}
      end,"Edit the general preferences",[]},
     {"Compatibility...",
      fun(_, _) ->
	      {edit,{preferences,compatibility}}
      end,"Edit some compatibility preferences",[]},
     {"Advanced Preferences...",
      fun(_, _) ->
	      {edit,{preferences,advanced}}
      end,"Edit preferences for advanced users",[]},
     {"Proxy Mode Preferences...",
      fun(_, _) ->
	      {edit,{preferences,proxy}}
      end,"Edit preferences for the proxy mode",[]},
     {"UI Preferences...",
      fun(_, _) ->
	      {edit,{preferences,ui}}
      end,"Edit colors of some user-interface elements",[]},
     {"Plugin Preferences",{plugin_preferences,[]}}
    ].

command(prefs, _St) ->
    Qs = [{hframe,
	   [{vframe,
	     [{label_column,
	       [{"Unselected Size",vertex_size},
		{"Selected Size",selected_vertex_size}]}],
	     [{title,"Vertex Display"}]},
	    {vframe,
	     [{label_column,
	       [{"Unselected Width",edge_width},
		{"Selected Width",selected_edge_width}]}],
	     [{title,"Edge Display"}]}]},
	  {hframe,
	   [{label,"Info Text"},{color,info_color},
	    {label,"Info Background"},{color,info_background_color},
	    {label,"Selection"},{color,selected_color},
	    {label,"Edges"},{color,edge_color},
	    {label,"Hard Edges"},{color,hard_edge_color}],
	   [{title,"Colors"}]},
	  {hframe,
	   [{label,"Color"},{color,grid_color},
	    {"Force Axis-Aligned Grid",force_show_along_grid}],
	   [{title,"Grid"}]},
	  {hframe,
	   [{vframe,
	     [{hframe,
	       [{"Vertices",vertex_hilite},
		{"Edges",edge_hilite},
		{"Faces",face_hilite},
		{"Objects",body_hilite}]},
	      {hframe,
	       [{label,"Unselected"},{color,unselected_hlite},
		{label,"Selected"},{color,selected_hlite}]},
	      {"Smart Highlighting",smart_highlighting}],
	     [{title,"Highlighting"}]},
	    {vframe,
	     [{menu,[{"Solid Face Selections",solid},
		     {"Stippled Face Selections",stippled}],
	       selection_style},
	      {"Hide Selection While Dragging",hide_sel_while_dragging},
	      {"Hide Selection While Moving Camera",hide_sel_in_camera_moves}
	     ],
	     [{title,"Selection Options"}]}]},
	  {hframe,
	   [{vframe,
	     [{label_column,
	       [{"Length",active_vector_size},
		{"Width",active_vector_width},
		{color,"Color",active_vector_color}]}],
	     [{title,"Vector Display"}]},
	    {vframe,
	     [{label_column,
	       [{"Angle",auto_rotate_angle},
		{"Delay (ms)",auto_rotate_delay}]}],
	     [{title,"Auto Rotate"}]}]},
	  {hframe,
	   [{vframe,
	     [{"Show Axis Letters",show_axis_letters},
	      {hframe,
	       [{label_column,
		 [{color,"+X Color",x_color},
		  {color,"+Y Color",y_color},
		  {color,"+Z Color",z_color}]},
		{label_column,
		 [{color,"-X Color",neg_x_color},
		  {color,"-Y Color",neg_y_color},
		  {color,"-Z Color",neg_z_color}]}]}],
	     [{title,"Axes"}]},
	    {vframe,
	     [{label_column,
	       [{"Auto-Save Interval (min)",autosave_time}]}
	     ],
	     [{title,"Auto Save"}]}]}],
    dialog("Preferences", Qs);
command(compatibility, _St) ->
    Qs = [{vframe,
	   [{"Optimized Display-List Use",display_list_opt},
	    {"Use Display Lists for Text",text_display_lists},
	    {"Show Dummy Axis Letter",dummy_axis_letter},
	    {"Clear Back Buffer Early",early_buffer_clear}]}],
    dialog("Compatibility Settings", Qs);
command(advanced, _St) ->
    Qs = [{vframe,
	   [{"Advanced Menus",advanced_menus},
	    {"Default Commands",default_commands},
	    {"Use Highlight as Temporary Selection",
	     use_temp_sel}
	   ]}],
    dialog("Advanced Preferences", Qs);
command(proxy, _St) ->
    Qs = [{vframe,
	   [{menu,[{"Cage",cage},
		   {"Some Edges",some},
		   {"All Edges",all}],
	     proxy_shaded_edge_style}],
	   [{title,"Shaded Mode Edge Style"}]},
	  {hframe,
	   case wings_util:is_gl_ext('GL_ARB_imaging') of
	       false ->
		   [{vframe,[{label,"(Opacity preferences only supported when"},
			     {label,"using OpenGL 1.2 or higher.)"}]}];
	       true ->
		   [{vframe,
		     [{label,"Stationary Opacity"},
		      {label,"Moving Opacity"}]},
		    {vframe,
		     [{slider,{text,proxy_static_opacity,[{range,{0.0,1.0}}]}},
		      {slider,{text,proxy_moving_opacity,[{range,{0.0,1.0}}]}}]}]
	   end}],
    dialog("Proxy Mode Preferences", Qs);
command(ui, _St) ->
    Qs = [{vframe,
	   [{hframe,[{vframe,
		      [{label,"Desktop/Geometry Background"},
		       {label,"Menu Text"},
		       {label,"Menu Highlight"},
		       {label,"Menu Highlighted Text"},
		       {label,"Menu Background"},
		       {label,"Dialog Background"},
		       {label,"Title Text"},
		       {label,"Title (Passive) Background"},
		       {label,"Title (Active) Background"}]},
		     {vframe,
		      [{color,background_color},
		       {color,menu_text},
		       {color,menu_hilite},
		       {color,menu_hilited_text},
		       {color,menu_color},
		       {color,dialog_color},
		       {color,title_text_color},
		       {color,title_passive_color},
		       {color,title_active_color}]}]}]}],
	   dialog("UI Preferences", Qs);
command({set,List}, St) ->
    foreach(fun({Key,Val}) ->
		    smart_set_value(Key, Val, St)
	    end, List),
    wings_wm:dirty().

dialog(Title, Qs0) ->
    Qs = make_query(Qs0),
    wings_ask:dialog(Title, Qs,
		     fun(Res) ->
			     {edit,{preferences,{set,Res}}}
		     end).

smart_set_value(Key, Val, St) ->
    case ets:lookup(wings_state, Key) of
	[] -> set_value(Key, Val);
	[{Key,Val}] -> ok;
	[_] ->
	    set_value(Key, Val),
	    case Key of
		vertex_size ->
		    clear_vertex_dlist();
		background_color ->
		    {R,G,B} = Val,
		    gl:clearColor(R, G, B, 1.0);
		autosave_time ->
		    wings_file:init_autosave();
		proxy_shaded_edge_style ->
		    clear_proxy_edges(St);
		display_list_opt ->
		    wings_draw_util:init();
		_Other -> ok
	    end
    end.

clear_vertex_dlist() ->
    wings_draw_util:map(fun clear_vertex_dlist/2, []).

clear_vertex_dlist(D, _) -> D#dlo{vs=none}.

clear_proxy_edges(St) ->
    wings_draw_util:map(fun(D, _) -> clear_proxy_edges(D, St) end, []).

clear_proxy_edges(D, St) ->
    wings_subdiv:update(D#dlo{proxy_edges=none}, St).

make_query([_|_]=List)  ->
    [make_query(El) || El <- List];
make_query({color,Key}) ->
    Def = get_value(Key),
    {color,Def,[{key,Key}]};
make_query({color,Str,Key}) when is_list(Str) ->
    Def = get_value(Key),
    {Str,{color,Def,[{key,Key}]}};
make_query({Str,Key}) when is_list(Str) ->
    case get_value(Key) of
	Def when Def == true; Def == false ->
	    {Str,Def,[{key,Key}]};
	Def ->
	    {Str,{text,Def,[{key,Key}]}}
    end;
make_query({alt,Key,Label,Val}) ->
    Def = get_value(Key),
    {key_alt,{Key,Def},Label,Val};
make_query({menu,List,Key}) ->
    Def = get_value(Key),
    {menu,List,{Key,Def}};
make_query({slider,{text,Key,Flags}}) ->
    Def = get_value(Key),
    {slider,{text,Def,[{key,Key}|Flags]}};
make_query(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([make_query(El) || El <- tuple_to_list(Tuple)]);
make_query(Other) -> Other.

old_pref_file() ->
    case os:type() of
	{unix,darwin} ->
	    case try_location(os:getenv("HOME"), ?MAC_PREFS) of
		none -> unix_pref();
		File -> File
	    end;
	{unix,_} -> unix_pref();
	{win32,_} -> win32_pref()
    end.

unix_pref() ->
    try_location(os:getenv("HOME"), ".wings").

win32_pref() ->
    case try_location(wings:root_dir(), ?WIN32_PREFS) of
	none -> win32_pref_1();
	File -> File
    end.

win32_pref_1() ->
    %% Try to locate a preference file in an old installation of Wings.
    none.

try_location(Dir, File) ->
    Name = filename:join(Dir, File),
    case filelib:is_file(Name) of
	true -> Name;
	false -> none
    end.

new_pref_file() ->
    case os:type() of
	{unix,darwin} ->
	    filename:join(os:getenv("HOME"), ?MAC_PREFS);
	{unix,_} ->
	    filename:join(os:getenv("HOME"), ".wings");
	{win32,_} ->
	    filename:join(wings:root_dir(), ?WIN32_PREFS)
    end.

get_value(Key) ->
    get_value(Key, undefined).

get_value(Key, Default) ->
    case ets:lookup(wings_state, Key) of
	[] -> Default;
	[{Key,Val}] -> Val
    end.

set_value(Key, Value) ->
    ets:insert(wings_state, {Key,Value}),
    ok.

set_default(Key, Value) ->
    case ets:member(wings_state, Key) of
	true -> ok;
	false ->
	    ets:insert(wings_state, {Key,Value}),
	    ok
    end.

delete_value(Key) ->
    ets:delete(wings_state, Key),
    ok.

defaults() ->
    [{background_color,{0.8,0.8,0.8}},
     {info_color,{0.0,0.0,0.0}},
     {info_background_color,{0.8,0.8,0.8,0.5}},
     {grid_color,{0.7,0.7,0.7}},
     {edge_color,{0.0,0.0,0.0}},
     {hard_edge_color,{0.0,0.5,0.0}},
     {selected_color,{0.65,0.0,0.0}},
     {unselected_hlite,{0.0,0.65,0.0}},
     {selected_hlite,{0.70,0.70,0.0}},
     {x_color,{0.6,0.0,0.0}},
     {y_color,{0.0,0.6,0.0}},
     {z_color,{0.0,0.0,0.6}},
     {neg_x_color,{0.6,0.6,0.6}},
     {neg_y_color,{0.6,0.6,0.6}},
     {neg_z_color,{0.6,0.6,0.6}},

     {vertex_size,4.0},
     {selected_vertex_size,5.0},
     {edge_width,2.0},
     {selected_edge_width,2.0},
     {show_axis_letters,true},
     {force_show_along_grid,false},
     {vertex_hilite,true},
     {edge_hilite,true},
     {face_hilite,true},
     {body_hilite,true},
     {auto_rotate_angle,1.0},
     {auto_rotate_delay,60},
     {autosave_time,10},
     {active_vector_size,1.0},
     {active_vector_width,2.0},
     {active_vector_color,{0.0,0.0,0.65}},
     {smart_highlighting,false},

     {selection_style,solid},
     {hide_sel_while_dragging,false},
     {hide_sel_in_camera_moves,false},

     %% Compatibility preferences.
     {display_list_opt,true},
     {text_display_lists,true},
     {dummy_axis_letter,false},
     {early_buffer_clear,os:type() =/= {unix,darwin}},

     %% Advanced features.
     {advanced_menus,false},
     {default_commands,false},
     {use_temp_sel,false},

     %% Proxy preferences.
     {proxy_shaded_edge_style,some},
     {proxy_static_opacity,1.0},
     {proxy_moving_opacity,1.0},

     %% User interface preferences.
     {menu_text,{0.0,0.0,0.0}},
     {menu_hilite,{0.0,0.0,0.5}},
     {menu_hilited_text,{1.0,1.0,1.0}},
     {menu_color,{0.75,0.75,0.75,1.0}},
     {dialog_color,{0.75,0.75,0.75,1.0}},
     {title_active_color,{0.41,0.55,0.41,1.0}},
     {title_passive_color,{0.325,0.4,0.325,1.0}},
     {title_text_color,{1.0,1.0,1.0}}
    ].

clean(List) ->
    clean(List, []).

clean([{Key,Val}=Pair|T], Acc) ->
    case not_bad(Key, Val) of
	true -> clean(T, [Pair|Acc]);
	false ->
	    io:format("Removed pref: ~p\n", [Pair]),
	    clean(T, Acc)
    end;
clean([{{bindkey,_}=Bk,{view,{virtual_mirror,Cmd}},user}|T], Acc) ->
    clean(T, [{Bk,{tools,{virtual_mirror,Cmd}},user}|Acc]);
clean([{{bindkey,_},Cmd,user}=Bk|T], Acc) ->
    case bad_command(Cmd) of
	false -> clean(T, [Bk|Acc]);
	true ->
	    io:format("Removed pref: ~p\n", [Bk]),
	    clean(T, Acc)
    end;
clean([{{bindkey,_,_},Cmd,user}=Bk|T], Acc) ->
    case bad_command(Cmd) of
	false -> clean(T, [Bk|Acc]);
	true ->
	    io:format("Removed pref: ~p\n", [Bk]),
	    clean(T, Acc)
    end;
clean([H|T], Acc) ->
    clean(T, [H|Acc]);
clean([], Acc) -> Acc.

%% First, get rid of obsolete stuff.
not_bad(last_point, _) -> false;
not_bad(default_point, _) -> false;
not_bad(smooth_preview, _) -> false;
not_bad(wire_mode, _) -> false;
not_bad(none, _) -> false;
not_bad(use_front_buffer, _) -> false;
not_bad(one_button_mouse, _) -> false;
not_bad(face_color, _) -> false;
not_bad(workmode, _) -> false;
not_bad(orthogonal_view, _) -> false;
not_bad(show_memory_used, _) -> false;
not_bad(show_axes, _) -> false;
not_bad(show_groundplane, _) -> false;
not_bad(current_view, _) -> false;
not_bad(camera_fov, _) -> false;
not_bad(camera_hither, _) -> false;
not_bad(camera_yon, _) -> false;
not_bad(right_click_sel_in_ss, _) -> false;
not_bad(right_click_sel_in_geom, _) -> false;
not_bad(wire_edge_color, _) -> false;

%% Crashes have occurred.
not_bad(last_axis, Val) -> is_wings_vector(Val);
not_bad(default_axis, Val) -> is_wings_vector(Val);
not_bad(magnet_radius, Val) -> is_number(Val);
not_bad(_, _) -> true.

is_wings_vector({{Px,Py,Pz},{Vx,Vy,Vz}})
  when is_number(Px), is_number(Py), is_number(Pz),
       is_number(Vx), is_number(Vy), is_number(Vz) ->
    true;
is_wings_vector(_) -> false.

bad_command({_,{rotate,Atom}}) when is_atom(Atom) -> true;
bad_command({view,virtual_mirror}) -> true;
bad_command({select,edge_loop}) -> true;
bad_command({select,select_region}) -> true;
bad_command({select,edge_ring}) -> true;
bad_command({select,prev_edge_loop}) -> true;
bad_command({select,next_edge_loop}) -> true;
bad_command(_) -> false.
