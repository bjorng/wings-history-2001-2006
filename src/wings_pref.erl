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
%%     $Id: wings_pref.erl,v 1.13 2001/12/31 23:53:55 bjorng Exp $
%%

-module(wings_pref).
-export([init/0,finish/0,
	 sub_menu/1,command/1,
	 get_value/1,get_value/2,set_value/2,set_default/2,
	 delete_value/1,browse/1]).

-include("wings.hrl").
-import(lists, [foreach/2,keysearch/3,map/2]).

init() ->
    ets:new(wings_state, [named_table,public,ordered_set]),
    ets:insert(wings_state, defaults()),
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
    Str = [io_lib:format("~p. \n", [P]) || P <- List],
    catch file:write_file(PrefFile, Str),
    ok.

prune_defaults(List) ->
    List -- [{Key,Val} || {_,Key,Val} <- presets()].

sub_menu(St) ->
    M = map(fun({Desc,Key,_}) -> {Desc,Key};
	       (separator) -> separator
	    end, presets()),
    list_to_tuple(M).

command(Key) ->
    {value,{Prompt,_,_}} = keysearch(Key, 2, presets()),
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
    Dir = filename:dirname(code:which(?MODULE)),
    Name = filename:absname(File, Dir),
    case filelib:is_file(Name) of
	true -> Name;
	false -> locate(File, Dir)
    end.

locate(File, Dir0) ->
    case filename:basename(Dir0) of
	"ebin" ->
	    Dir = filename:dirname(Dir0),
	    Name = filename:absname(File, Dir),
	    case filelib:is_file(Name) of
		true -> Name;
		false -> none
	    end;
	_ -> none
    end.

defaults() ->
    [{Key,Val} || {_,Key,Val} <- presets()].

presets() ->
    [{"Background Color",background_color,{0.4,0.4,0.4}},
     {"Grid Color",grid_color,{0.0,0.0,0.0}},
     {"Face Color",face_color,{0.5,0.5,0.5}},
     {"Selection Color",selected_color,{0.65,0.0,0.0}},
     {"Hard Edge Color",hard_edge_color,{0.0,0.5,0.0}},
     separator,
     {"+X Color",x_color,{1.0,0.0,0.0}},
     {"+Y Color",y_color,{0.0,1.0,0.0}},
     {"+Z Color",z_color,{0.0,0.0,1.0}},
     {"-X Color",neg_x_color,{0.0,0.8,0.8}},
     {"-Y Color",neg_y_color,{0.8,0.0,0.8}},
     {"-Z Color",neg_z_color,{0.8,0.8,0.0}},
     separator,
     {"Vertex Size",vertex_size,4.0},
     {"Selected Vertex Size",selected_vertex_size,5.0},
     {"Edge Width",edge_width,2.0},
     {"Selected Edge Width",selected_edge_width,2.0},
     separator,
     {"Show Memory Used",show_memory_used,false},
     separator,
     {"Auto-rotate angle",auto_rotate_angle,1.0},
     {"Auto-rotate delay (ms)",auto_rotate_delay,60}
    ].
