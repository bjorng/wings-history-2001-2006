%%
%%  wings_pref.erl --
%%
%%     Preference management.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pref.erl,v 1.33 2002/03/01 19:56:42 bjorng Exp $
%%

-module(wings_pref).
-export([init/0,finish/0,
	 menu/1,command/2,
	 get_value/1,get_value/2,set_value/2,set_default/2,
	 delete_value/1,browse/1]).

-define(NEED_ESDL, 1).    %% Some keybindings
-include("wings.hrl").
-import(lists, [foreach/2,keysearch/3,map/2,reverse/1,sort/1]).

init() ->
    ets:new(wings_state, [named_table,public,ordered_set]),
    ets:insert(wings_state, defaults()),
    wings_hotkey:set_default(),
    case old_pref_file() of
	none -> ok;
	Pref ->
	    case file:consult(Pref) of
		{ok,List} -> catch ets:insert(wings_state, List);
		{error,_Reason} -> ok
	    end
    end.

finish() ->
    ets:delete(wings_state, neg_axes_intensity),
    PrefFile = new_pref_file(),
    List0 = ets:tab2list(wings_state),
    List = prune_defaults(List0),
    Write = fun({{bindkey,_},_,default}) -> [];
	       ({{bindkey,_},_,plugin}) -> [];
	       ({{bindkey,_,_},_,default}) -> [];
	       ({{bindkey,_,_},_,plugin}) -> [];
	       (Else) ->
		    io_lib:format("~p. \n", [Else])
	    end,
    Str = lists:map(Write, List),
    catch file:write_file(PrefFile, Str),
    ok.
    
prune_defaults(List) ->
    List -- defaults().

menu(_St) ->
    [{"Color Preferences",fun(_, _) ->
				  {edit,{preferences,color_prefs}}
			  end,[],[]},
     {"Other Preferences",fun(_, _) ->
				  {edit,{preferences,other_prefs}}
			  end,[],[]}].

command(color_prefs, St) ->
    Qs0 = [{"Background Color",background_color},
	   {"Grid Color",grid_color},
	   {"Face Color",face_color},
	   {"Hard Edge Color",hard_edge_color},
	   separator,
	   {"Selection Color",selected_color},
	   {"Unselected Hilite",unselected_hlite},
	   {"Selected Hilite",selected_hlite},
	   separator,
	   {"+X Color",x_color},
	   {"+Y Color",y_color},
	   {"+Z Color",z_color},
	   {"-X Color",neg_x_color},
	   {"-Y Color",neg_y_color},
	   {"-Z Color",neg_z_color}],
    Qs = make_query(Qs0),
    wings_ask:ask(Qs, St, fun(Res) -> {edit,{preferences,{set,Res}}} end);
command(other_prefs, St) ->
    Qs0 = [{"Vertex Size",vertex_size},
	   {"Selected Vertex Size",selected_vertex_size},
	   {"Edge Width",edge_width},
	   {"Selected Edge Width",selected_edge_width},
	   {"Auto-save interval (min)",autosave_time},
	   {hframe,[{"Vertices",vertex_hilite},
		    {"Edges",edge_hilite},
		    {"Faces",face_hilite},
		    {"Objects",body_hilite}],
	    [{title,"Highlighting"}]},
	   {hframe,[{"Angle",auto_rotate_angle},
		    {"Delay (ms)",auto_rotate_delay}],
	    [{title,"Auto Rotate"}]},
	   {hframe,[{"Size",active_vector_size},
		    {"Width",active_vector_width},
		    {"Color",active_vector_color}],
	    [{title,"Vector Display"}]},
	   {"Show Axis Letters",show_axis_letters},
	   {"Force Axis-aligned Grid",force_show_along_grid},
	   {"Show Memory Used",show_memory_used},
	   {"Display List Optimisation",display_list_opt},
	   {"Advanced Menus",advanced_menus}],
    Qs = make_query(Qs0),
    wings_ask:ask(Qs, St, fun(Res) -> {edit,{preferences,{set,Res}}} end);
command({set,List}, _St) ->
    foreach(fun({Key,Val}) ->
		    set_value(Key, Val),
		    case Key of
			background_color ->
			    {R,G,B} = Val,
			    gl:clearColor(R, G, B, 1.0);
			display_list_opt ->
			    wings_draw_util:init();
			_Other -> ok
		    end
	    end, List),
    wings_io:putback_event(redraw),
    keep.

make_query([_|_]=List)  ->
    [make_query(El) || El <- List];
make_query({Str,Key}) when is_list(Str) ->
    Def = get_value(Key),
    {Str,Def,[{key,Key}]};
make_query(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([make_query(El) || El <- tuple_to_list(Tuple)]);
make_query(Other) -> Other.

old_pref_file() ->
    case os:type() of
	{unix,_} ->
	    Name = filename:join(os:getenv("HOME"), ".wings"),
	    case filelib:is_file(Name) of
		true -> Name;
		false -> none
	    end;
	{win32,_} -> locate("Preferences")
    end.

new_pref_file() ->
    case os:type() of
	{unix,_} ->
	    filename:join(os:getenv("HOME"), ".wings");
	{win32,_} ->
	    new_pref_file_win32()
    end.

new_pref_file_win32() ->
    case locate("Preferences") of
	none ->
	    case locate("vsn.mk") of
		none -> ok;
		Vsn ->
		    Vsn = locate("vsn.mk"),
		    Dir = filename:dirname(Vsn),
		    filename:join(Dir, "Preferences")
	    end;
	Pref -> Pref
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

browse(Prefix) ->
    ets:select(wings_state, [{{{Prefix,'$1'},'$2'},[],[{{'$1','$2'}}]}]).

locate(File) ->
    Dir = wings:root_dir(),
    Name = filename:absname(File, Dir),
    case filelib:is_file(Name) of
	true -> Name;
	false -> none
    end.

defaults() ->
    [{background_color,{0.4,0.4,0.4}},
     {grid_color,{0.0,0.0,0.0}},
     {face_color,{0.5,0.5,0.5}},
     {hard_edge_color,{0.0,0.5,0.0}},
     {selected_color,{0.65,0.0,0.0}},
     {unselected_hlite,{0.0,0.65,0.0}},
     {selected_hlite,{0.70,0.70,0.0}},
     {x_color,{1.0,0.0,0.0}},
     {y_color,{0.0,1.0,0.0}},
     {z_color,{0.0,0.0,1.0}},
     {neg_x_color,{0.0,0.8,0.8}},
     {neg_y_color,{0.8,0.0,0.8}},
     {neg_z_color,{0.8,0.8,0.0}},

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
     {show_memory_used,false},
     {auto_rotate_angle,1.0},
     {auto_rotate_delay,60},
     {autosave_time,2},
     {active_vector_size,1.0},
     {active_vector_width,2.0},
     {active_vector_color,{0.0,0.0,0.65}},
     {display_list_opt,false},
     {advanced_menus,false}
    ].
