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
%%     $Id: wings_pref.erl,v 1.106 2003/11/30 07:53:19 bjorng Exp $
%%

-module(wings_pref).
-export([init/0,finish/0,
	 menu/1,command/2,
	 get_value/1,get_value/2,set_value/2,set_default/2,
	 delete_value/1]).

-define(NEED_ESDL, 1).    %% Some keybindings
-include("wings.hrl").
-import(lists, [foreach/2,keysearch/3,map/2,reverse/1,sort/1,last/1]).

-define(MAC_PREFS, "Library/Preferences/Wings 3D Preferences.txt").
-define(WIN32_OLD_PREFS, "Preferences").
-define(WIN32_PREFS, "Wings3D/Preferences.txt").

init() ->
    ets:new(wings_state, [named_table,public,ordered_set]),
    ets:insert(wings_state, defaults()),
    wings_hotkey:set_default(),
    case old_pref_file() of
	none -> ok;
	PrefFile ->
            io:format("Reading preferences from: ~s\n", [PrefFile]),
	    case file:consult(PrefFile) of
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
    Format = "~p. \n",
    PostProcess = case os:type() of
                      {win32,_} -> fun insert_crs/1;
                      _ -> fun(L) -> L end
                  end,
    Write = fun({{bindkey,_},_,default}) -> [];
	       ({{bindkey,_},_,plugin}) -> [];
	       ({{bindkey,_,_},_,default}) -> [];
	       ({{bindkey,_,_},_,plugin}) -> [];
	       (Else) -> PostProcess(io_lib:format(Format, [Else]))
	    end,
    Str = lists:map(Write, List),
    catch file:write_file(PrefFile, Str),
    ok.

%% Insert CRs into a deep list to produce a correct Windows
%% text file.
insert_crs([H|T]) -> [insert_crs(H)|insert_crs(T)];
insert_crs([]) -> [];
insert_crs($\n) -> "\r\n";
insert_crs(C) when is_integer(C) -> C.

prune_defaults(List) ->
    List -- defaults().

menu(_St) ->
    [{"Preferences...",
      fun(_, _) ->
	      {edit,{preferences,prefs}}
      end,"Edit the general preferences",[]}].

command(prefs, _St) ->
    PrefQs0 = [{"General",gen_prefs()},
	       {"Advanced",advanced_prefs()},
 	       {"Compatibility",compatibility_prefs()},
	       {"User Interface",ui_prefs()},
	       {"Misc",misc_prefs()}],
    PrefQs = [{Lbl,make_query(Ps)} || {Lbl,Ps} <- PrefQs0],
    Qs = [{oframe,PrefQs,1,[{style,buttons}]}],
    wings_ask:dialog("Preferences", Qs,
		     fun([_|Res]) ->
			     {edit,{preferences,{set,Res}}}
		     end);
command({set,List}, St) ->
    foreach(fun({Key,Val}) ->
		    smart_set_value(Key, Val, St)
	    end, List),
    wings_wm:dirty().

gen_prefs() ->
    {vframe,
     [{hframe,
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
	{label,"Edges"},{color,edge_color},
	{label,"Hard Edges"},{color,hard_edge_color}],
       [{title,"Colors"}]},
      {hframe,
       [{label,"Color"},{color,grid_color},
	{"Force Axis-Aligned Grid",force_show_along_grid,
	 [{info,"Always show the grid when the view is aligned along one of the major axes"}]}],
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
	  {hframe,[{label,"Selection Color"},{color,selected_color}]}
	 ],
	 [{title,"Selection"}]}]},
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
	 [{title,"Axes"}]}
       ]}]}.

compatibility_prefs() ->
    {vframe,
     [{"Optimized Display-List Use",display_list_opt,
       [{info,"Try turning this preference off if get strange display artifacts such as missing edges"}]},
      {"Use Display Lists for Text",text_display_lists,
       [{info,"Try turning this preference off if there are problems displaying text"}]},
      {"Show Dummy Axis Letter",dummy_axis_letter,
       [{info,"Showing a dummy axis letter can prevent a crash with some Matrox graphics cards/drivers"}]}
     ]}.

advanced_prefs() ->
    DisableHook = fun (is_disabled, {_Var,_I,Store}) ->
			  not gb_trees:get(advanced_menus, Store);
		      (_, _) ->	void
		  end,
    Flags = [{hook,DisableHook}],
    {vframe,
     [{"Advanced Menus",advanced_menus},
      separator,
      {"Default Commands",default_commands},
      {"Use Highlight as Temporary Selection",use_temp_sel},
      {"Hide Selection While Dragging",hide_sel_while_dragging},
      {"Hide Selection While Moving Camera",hide_sel_in_camera_moves},
      separator,
      {vframe,
       [{label_column,
	 [{"Length",active_vector_size,Flags},
	  {"Width",active_vector_width,Flags},
	  {color,"Color",active_vector_color,Flags}]}],
       [{title,"Vector Display"}]}
     ]}.

ui_prefs() ->
    {vframe,
     [{vframe,
       [{menu,wings_text:fonts(),system_font}],
       [{title,"Font"}]},
      {hframe,
       [{vframe,
	 [{label,"Desktop/Geometry Background"},
	  {label,"Menu Text"},
	  {label,"Menu Highlight"},
	  {label,"Menu Highlighted Text"},
	  {label,"Menu Background"},
	  {label,"Dialog Text"},
	  {label,"Dialog (Disabled) Text"},
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
	  {color,dialog_text},
	  {color,dialog_disabled},
	  {color,dialog_color},
	  {color,title_text_color},
	  {color,title_passive_color},
	  {color,title_active_color}]}],
       [{title,"Colors"}]}
     ]}.

misc_prefs() ->
    {vframe,
       [
	{vframe,
	 [{label_column,
	   [{"Angle",auto_rotate_angle},
	    {"Delay (ms)",auto_rotate_delay}]}],
	 [{title,"Auto Rotate"}]},
	{vframe,
	 [{label_column,
	   [{"Auto-Save Interval (min)",autosave_time}]}
	 ],
	 [{title,"Auto Save"}]},
	{vframe,
	 [{vframe,
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
	   end}],[{title,"Proxy Mode"}]}
       ]}.

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
		system_font ->
		    wings_wm:reinit_opengl(),
		    wings_text:choose_font();
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
make_query({color,[_|_]=Str,Key}) ->
    Def = get_value(Key),
    {Str,{color,Def,[{key,Key}]}};
make_query({color,[_|_]=Str,Key,Flags}) ->
    Def = get_value(Key),
    {Str,{color,Def,[{key,Key}|Flags]}};
make_query({[_|_]=Str,Key}) ->
    case get_value(Key) of
	Def when Def == true; Def == false ->
	    {Str,Def,[{key,Key}]};
	Def ->
	    {Str,{text,Def,[{key,Key}]}}
    end;
make_query({[_|_]=Str,Key,Flags}) ->
    case get_value(Key) of
	Def when Def == true; Def == false ->
	    {Str,Def,[{key,Key}|Flags]};
	Def ->
	    {Str,{text,Def,[{key,Key}|Flags]}}
    end;
make_query({alt,Key,Label,Val}) ->
    Def = get_value(Key),
    {key_alt,{Key,Def},Label,Val};
make_query({menu,List,Key}) ->
    Def = get_value(Key),
    {menu,List,Def,[{key,Key}]};
make_query({slider,{text,Key,Flags}}) ->
    Def = get_value(Key),
    {slider,{text,Def,[{key,Key}|Flags]}};
make_query(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([make_query(El) || El <- tuple_to_list(Tuple)]);
make_query(Other) -> Other.

%%%
%%% Search for a pre-existing preference file.
%%%

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
    {ok,R} = win32reg:open([read]),
    Res = win32_pref_1(R, ["AppData","Personal"]),
    ok = win32reg:close(R),
    Res.

%% Search for a preference file in "special folders", such as "AppData"
%% and "My Documents".
win32_pref_1(R, [FolderType|T]) ->
    case win32_special_folder(R, FolderType) of
        none -> win32_pref_1(R, T);
        Path ->
            case try_location(Path, ?WIN32_PREFS) of
                none -> win32_pref_1(R, T);
                File -> File
            end
    end;
win32_pref_1(R, []) ->
    case try_location(code:lib_dir(wings), ?WIN32_PREFS) of
        none -> win32_pref_2(R);
        File -> File
    end.
            
%% No preferences found so far. Search in old installations of
%% Wings for preference files.
win32_pref_2(R) ->
    case win32_9816(R) of
        none -> win32_pref_pre9816(R);
        File -> File
    end.

win32_9816(R) ->
    %% Search for a preference file in a Wings installation in 0.98.16.
    %% (Too bad... in a special place in this release only.)
    case win32reg:change_key(R, "\\hklm\\SOFTWARE\\Wings 3D") of
        ok ->
            case win32reg:sub_keys(R) of
                {ok,SubKeys0} ->
                    SubKeys = reverse(sort(SubKeys0)),
                    {ok,Curr} = win32reg:current_key(R),
                    win32_9816_1(R, SubKeys, Curr);
                {error,Error} ->
                    io:format("Can't read sub keys for 'Wings 3D': ~p\n",
                              [Error]),
                    none
            end;
        {error,Error} ->
            io:format("No 'Wings 3D' key (this is STRANGE): ~p\n", [Error]),
            none
    end.

win32_9816_1(R, [K|Keys], Curr) ->
    ok = win32reg:change_key(R, K),
    Dir = reg_get_default(R),
    ok = win32reg:change_key(R, Curr),
    WingsDirs = filelib:wildcard(Dir++"/lib/wings-*"),
    case try_locations(WingsDirs, ?WIN32_OLD_PREFS) of
        none -> win32_9816_1(R, Keys, Curr);
        File -> File
    end;
win32_9816_1(_, [], _) -> none.

win32_pref_pre9816(R) ->
    %% Search for a preference file in a Wings installation older than 0.98.16
    %% using the uninstall string.
    case win32reg:change_key(R, "\\hklm\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Wings 3D") of
        ok ->
            case win32reg:value(R, "UninstallString") of
                {ok,Str0} ->
                    Str = strip_quotes(Str0),
                    try_location(filename:dirname(Str), ?WIN32_OLD_PREFS);
                {error,_} -> none
            end;
        {error,_} -> none
    end.

win32_special_folder(R, FolderType) ->
    Key = "\\hkcu\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders",
    case win32reg:change_key(R, Key) of
        ok ->
            case win32reg:value(R, FolderType) of
                {error,_} -> error;
                {ok,Value} -> Value
            end;
        _ -> error
    end.

reg_get_default(R) ->
    %% There seems to be a bug in win32reg:value/2 preventing
    %% us from retrieving the default value. Workaround follows.
    case win32reg:values(R) of
        {ok,Values} ->
            case keysearch(default, 1, Values) of
                {value,{default,Val}} -> Val;
                false -> ""
            end;
        {error,_} -> ""
    end.
    
strip_quotes([$"|T0]) ->
    case reverse(T0) of
        [$"|T] -> reverse(T);
        _ -> T0
    end;
strip_quotes(S) -> S.

%%%
%%% Return a suitable path for a new preference file.
%%%

new_pref_file() ->
    case os:type() of
	{unix,darwin} ->
	    filename:join(os:getenv("HOME"), ?MAC_PREFS);
	{unix,_} ->
	    filename:join(os:getenv("HOME"), ".wings");
	{win32,_} ->
            win32_new_pref()
    end.

win32_new_pref() ->
    {ok,R} = win32reg:open([read]),
    Res = win32_new_pref_1(R, ["AppData","Personal"]),
    ok = win32reg:close(R),
    Res.

win32_new_pref_1(R, [FolderType|T]) ->
    case win32_special_folder(R, FolderType) of
        none -> win32_pref_1(R, T);
        Path ->
            File = filename:join(Path, ?WIN32_PREFS),
            filelib:ensure_dir(File),
            File
    end;
win32_new_pref_1(_, []) ->
    %% Desperate fallback for very old Window systems.
    %% (No "My Documents" folder.)
    filename:join(code:lib_dir(wings), ?WIN32_PREFS).

%%%
%%% Utilities.
%%%

try_locations([D|Ds], File) ->
    case try_location(D, File) of
        none -> try_locations(Ds, File);
        Name -> Name
    end;
try_locations([], _) -> none.

try_location(Dir, File) ->
    Name = filename:join(Dir, File),
    case filelib:is_file(Name) of
	true -> Name;
	false -> none
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
     {dialog_text,{0.0,0.0,0.0}},
     {dialog_disabled,{0.5,0.5,0.5}},
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
clean([{{bindkey,Key},{vector,{pick,[],Res0,Ns}},Type}|T], Acc) ->
    Res = list_to_tuple(reverse(Res0)),
    Mode = last(Ns),
    Bk = {{bindkey,Mode,Key},wings_menu:build_command(Res, Ns),Type},
    clean(T, [Bk|Acc]);
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
