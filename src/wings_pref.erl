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
%%     $Id: wings_pref.erl,v 1.30 2002/02/11 20:07:07 bjorng Exp $
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
		{error,Reason} -> ok
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
    List -- [{Key,Val} || {_,Key,Val} <- presets()].

menu(St) ->
    [{"Color Preferences",fun(_, _) ->
				  {edit,{preferences,color_prefs}}
			  end,[],[]},
     {"Other Preferences",fun(_, _) ->
				  {edit,{preferences,other_prefs}}
			  end,[],[]}].

command(color_prefs, St) ->
    Qs0 = [{vframe,[{"Background Color",background_color},
		    {"Grid Color",grid_color},
		    {"Face Color",face_color},
		    {"Hard Edge Color",hard_edge_color}],[]},
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
    Qs0 = [{"Show Axis Letters",show_axis_letters},
	   {"Force Axis-aligned Grid",force_show_along_grid},
	   separator,
	   {"Vertex highlighting",vertex_hilite},
	   {"Edge highlighting",edge_hilite},
	   {"Face highlighting",face_hilite},
	   {"Object highlighting",body_hilite},
	   separator,
	   {"Show Memory Used",show_memory_used},
	   separator,
	   {"Auto-rotate angle",auto_rotate_angle},
	   {"Auto-rotate delay (ms)",auto_rotate_delay},
	   separator,
	   {"Auto-save interval (min) [0=off]",autosave_time},
	   separator,
	   {"Vector Display Size",active_vector_size},
	   {"Vector Display Width",active_vector_width},
	   {"Vector Display Color",active_vector_color},
	   separator,
	   {"Advanced Menus",advanced_menus}],
    Qs = make_query(Qs0),
    wings_ask:ask(Qs, St, fun(Res) -> {edit,{preferences,{set,Res}}} end);
command({set,List}, St) ->
    foreach(fun({Key,Val}) ->
		    set_value(Key, Val),
		    case Key of
			background_color ->
			    {R,G,B} = Val,
			    gl:clearColor(R, G, B, 1.0);
			Other -> ok
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
    [{Key,Val} || {_,Key,Val} <- presets()].

presets() ->
    [{"Color Preferences",color_prefs},
     {"Background Color",background_color,{0.4,0.4,0.4}},
     {"Grid Color",grid_color,{0.0,0.0,0.0}},
     {"Face Color",face_color,{0.5,0.5,0.5}},
     {"Hard Edge Color",hard_edge_color,{0.0,0.5,0.0}},
     separator,
     {"Selection Color",selected_color,{0.65,0.0,0.0}},
     {"Unselected Hilite",unselected_hlite,{0.0,0.65,0.0}},
     {"Selected Hilite",selected_hlite,{0.70,0.70,0.0}},
     separator,
     {"+X Color",x_color,{1.0,0.0,0.0}},
     {"+Y Color",y_color,{0.0,1.0,0.0}},
     {"+Z Color",z_color,{0.0,0.0,1.0}},
     {"-X Color",neg_x_color,{0.0,0.8,0.8}},
     {"-Y Color",neg_y_color,{0.8,0.0,0.8}},
     {"-Z Color",neg_z_color,{0.8,0.8,0.0}},

     {"Other Preferences",other_prefs},
     {"Vertex Size",vertex_size,4.0},
     {"Selected Vertex Size",selected_vertex_size,5.0},
     {"Edge Width",edge_width,2.0},
     {"Selected Edge Width",selected_edge_width,2.0},
     separator,
     {"Show Axis Letters",show_axis_letters,true},
     {"Force Axis-aligned Grid",force_show_along_grid,false},
     separator,
     {"Vertex highlighting",vertex_hilite,true},
     {"Edge highlighting",edge_hilite,true},
     {"Face highlighting",face_hilite,true},
     {"Object highlighting",body_hilite,true},
     separator,
     {"Show Memory Used",show_memory_used,false},
     separator,
     {"Auto-rotate angle",auto_rotate_angle,1.0},
     {"Auto-rotate delay (ms)",auto_rotate_delay,60},
     separator,
     {"Auto-save interval (minutes) [0 is off]",autosave_time,2},
     separator,
     {"Vector Display Size",active_vector_size,1.0},
     {"Vector Display Width",active_vector_width,2.0},
     {"Vector Display Color",active_vector_color,{0.0,0.0,0.65}},
     separator,
     {"Advanced Menus",advanced_menus,false}
    ].
