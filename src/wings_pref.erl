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
%%     $Id: wings_pref.erl,v 1.23 2002/01/25 09:04:36 bjorng Exp $
%%

-module(wings_pref).
-export([init/0,finish/0,
	 menu/1,command/1,
	 get_value/1,get_value/2,set_value/2,set_default/2,
	 delete_value/1,browse/1]).

-define(NEED_ESDL, 1).    %% Some keybindings
-include("wings.hrl").
-import(lists, [foreach/2,keysearch/3,map/2,reverse/1,sort/1]).

init() ->
    ets:new(wings_state, [named_table,public,ordered_set]),
    ets:insert(wings_state, defaults()),

    Setup = fun({{bindkey,Key,List}, Action}) ->
		    ets:insert(wings_state,
			       {{bindkey,Key,sort(List)},Action});
	       (Other) -> ets:insert(wings_state, Other)
	    end,
    foreach(Setup, default_keybindings()),

    case old_pref_file() of
	none -> ok;
	Pref ->
	    case file:consult(Pref) of
		{ok,List} -> catch lists:foreach(Setup, List);
		{error,Reason} -> ok
	    end
    end.

finish() ->
    ets:delete(wings_state, neg_axes_intensity),
    PrefFile = new_pref_file(),
    List0 = ets:tab2list(wings_state),
    List = prune_defaults(List0),
    Write = fun({{bindkey,Key,Mods}, Action}) ->
		    io_lib:format("{{bindkey, $~c, ~p}, ~p}.~n",
				  [Key,Mods,Action]);
	       ({{bindkey, Key}, Action}) ->
		    io_lib:format("{{bindkey, $~c}, ~p}.~n",
				  [Key,Action]);
	       (Else) ->
		    io_lib:format("~p. \n", [Else])
	    end,
    Str = lists:map(Write, List),
    catch file:write_file(PrefFile, Str),
    ok.

prune_defaults(List) ->
    NonPresets = List -- [{Key,Val} || {_,Key,Val} <- presets()],
    NonPresets -- default_keybindings().

menu(St) ->
    menu_1(presets()).

menu_1([{Desc,Key}|T0]) ->
    {Items,T} = collect_items(T0, []),
    [{Desc,{Key,list_to_tuple(Items)}}|menu_1(T)];
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

command(Key) ->
    {value,{Prompt,_,Def}} = keysearch(Key, 2, presets()),
    command_1(Key, Prompt, Def).

command_1(Key, Prompt, Bool) when Bool == false; Bool == true ->
    set_value(Key, not get_value(Key));
command_1(Key, Prompt, _) ->
    Def = get_value(Key),
    wings_util:ask(true,
		   [{Prompt,Def}],
		   fun([Val]) ->
			   set_value(Key, Val),
			   case Key of
			       background_color ->
				   {R,G,B} = Val,
				   gl:clearColor(R, G, B, 1.0);
			       Other -> ok
			   end
		   end).

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

default_keybindings() ->
    [{{bindkey,{$a,[ctrl]}},          {select,all}},
     {{bindkey,{$i,[ctrl,shift]}},     {select,inverse}},
     {{bindkey,{$l,[ctrl]}},           {file,merge}},
     {{bindkey,{$n,[ctrl]}},           {file,new}},
     {{bindkey,{$o,[ctrl]}},           {file,open}},
     {{bindkey,{$q,[ctrl]}},           {file,quit}},
     {{bindkey,{$s,[ctrl,shift]}},     {file,save_as}},
     {{bindkey,{$s,[ctrl]}},           {file,save}},
     {{bindkey,{$z,[alt,ctrl]}},       {edit,undo}},
     {{bindkey,{$z,[ctrl,shift]}},     {edit,redo}},
     {{bindkey,{$z,[ctrl]}},           {edit,undo_toggle}},
     {{bindkey,{?SDLK_KP_PLUS,[]}},   {select,more}},
     {{bindkey,{?SDLK_KP_MINUS,[]}},  {select,less}},
     {{bindkey,{?SDLK_F3,[]}},        {select,prev_edge_loop}},
     {{bindkey,{?SDLK_F4,[]}},        {select,next_edge_loop}},
     {{bindkey,{?SDLK_F5,[]}},        {select,{by,{faces_with,5}}}},
     {{bindkey, $\t},                 {view,smooth_preview}},
     {{bindkey, $\s},                 {select,deselect}},
     {{bindkey, $a},                  {view,aim}},
     {{bindkey, $b},                  {select,body}},
     {{bindkey, $d},                  {edit,repeat}},
     {{bindkey, $e},                  {select,edge}},
     {{bindkey, $f},                  {select,face}},
     {{bindkey, $i},                  {select,similar}},
     {{bindkey, $l},                  {select,edge_loop}},
     {{bindkey, $o},                  {view,orthogonal_view}},
     {{bindkey, $r},                  {view,reset}},
     {{bindkey, $R},                  {select,edge_ring}},
     {{bindkey, $s},                  {body,auto_smooth}},
     {{bindkey, $u},                  {view,auto_rotate}},
     {{bindkey, $v},                  {select,vertex}},
     {{bindkey, $w},                  {view,wire_mode}},
     {{bindkey, $x},                  {view,{along,x}}},
     {{bindkey, $y},                  {view,{along,y}}},
     {{bindkey, $z},                  {view,{along,z}}},
     {{bindkey, $X},                  {view,{along,neg_x}}},
     {{bindkey, $Y},                  {view,{along,neg_y}}},
     {{bindkey, $Z},                  {view,{along,neg_z}}},
     {{bindkey, $2},                  {edge,{cut,2}}},
     {{bindkey, $3},                  {edge,{cut,3}}},
     {{bindkey, $4},                  {edge,{cut,4}}},
     {{bindkey, $5},                  {edge,{cut,5}}},
     {{bindkey, $6},                  {edge,{cut,6}}},
     {{bindkey, $7},                  {edge,{cut,7}}},
     {{bindkey, $8},                  {edge,{cut,8}}},
     {{bindkey, $9},                  {edge,{cut,9}}},
     {{bindkey, $0},                  {edge,{cut,10}}},
     {{bindkey, $+},                  {select,more}},
     {{bindkey, $=},                  {select,more}},
     {{bindkey, $-},                  {select,less}}
    ].
