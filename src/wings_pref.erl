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
%%     $Id: wings_pref.erl,v 1.29 2002/02/11 12:27:01 bjorng Exp $
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
    menu_1(presets()).

-ifdef(DIALOG_BOXES).
%% XXX Doesn't fully work yet.
menu_1([{Desc,Key}|T0]) ->
    {Items,T} = collect_items(T0, []),
    Fun = fun(1, Ns) -> {edit,{preferences,Items}} end,
    [{Desc,Fun,"Preferences...",[]}|menu_1(T)];
menu_1([]) -> [].

collect_items([{Desc,Key,_}|T], A) ->
    V = get_value(Key),
    collect_items(T, [{Desc,V}|A]);
collect_items([separator|T], A) ->
    collect_items(T, [separator|A]);
collect_items([{Desc,Key}|_]=T, A) ->
    {reverse(A),T};
collect_items([], A) ->
    {reverse(A),[]}.
command(Qs, St) when is_list(Qs) ->
    wings_ask:ask(Qs, St, fun(Res) -> {edit,{preferences,{set,Res}}} end).
-else.
menu_1([{Desc,Key}|T0]) ->
    {Items,T} = collect_items(T0, []),
    [{Desc,{Key,Items}}|menu_1(T)];
menu_1([]) -> [].

collect_items([{Desc,Key,Bool}|T], A) when Bool == false; Bool == true ->
    I = case get_value(Key) of
	    false -> {Desc,Key};
	    true ->  {Desc,Key,[crossmark]}
	end,
    collect_items(T, [I|A]);
collect_items([{Desc,Key,_}|T], A) -> collect_items(T, [{Desc,Key}|A]);
collect_items([separator|T], A) -> collect_items(T, [separator|A]);
collect_items([{Desc,Key}|_]=T, A) -> {reverse(A),T};
collect_items([], A) -> {reverse(A),[]}.

command(Key, St) when is_atom(Key) ->
    {value,{Prompt,_,Def}} = keysearch(Key, 2, presets()),
    command_1(Key, Prompt, Def, St).

command_1(Key, Prompt, Bool, St) when Bool == false; Bool == true ->
    set_value(Key, not get_value(Key)),
    keep;
command_1(Key, Prompt, _, St) ->
    Def = get_value(Key),
    wings_ask:ask([{Prompt,Def}], St,
		  fun([Val]) ->
			  set_value(Key, Val),
			  case Key of
			      background_color ->
				  {R,G,B} = Val,
				  gl:clearColor(R, G, B, 1.0);
			      Other -> ok
			  end,
			  ignore
		  end).
-endif.

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
