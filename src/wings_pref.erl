%%
%%  wings_pref.erl --
%%
%%     Preference management.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pref.erl,v 1.4 2001/11/07 07:09:59 bjorng Exp $
%%

-module(wings_pref).
-export([init/0,finish/0,
	 sub_menu/3,command/1,
	 get_value/1,get_value/2,set_value/2,set_default/2,
	 locate/1]).

-include("wings.hrl").
-import(lists, [foreach/2,keysearch/3,map/2]).

init() ->
    ets:new(wings_state, [named_table]),
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
    PrefFile = new_pref_file(),
    List = ets:tab2list(wings_state),
    Str = [io_lib:format("~p. \n", [P]) || P <- List],
    catch file:write_file(PrefFile, Str),
    ok.

sub_menu(X, Y, St) ->
    M = map(fun({Desc,Key,_}) -> {Desc,Key};
	       (separator) -> separator
	    end, presets()),
    list_to_tuple(M).

command(Key) ->
    {value,{Prompt,_,_}} = keysearch(Key, 2, presets()),
    Def = get_value(Key),
    wings_util:ask(true,
		   [{Prompt,Def}],
		   fun([Val]) -> set_value(Key, Val) end).

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
    [{"Background Color",background_color,{0.6,0.6,0.5}},
     {"Face Color",face_color,{0.5,0.5,0.5}},
     {"Selection Color",selected_color,{0.65,0.0,0.0}},
     {"Hard Edge Color",hard_edge_color,{0.0,0.5,0.0}},
     separator,
     {"Negative Axes Intensity",neg_axes_intensity,0.8},
     separator,
     {"Vertex Size",vertex_size,4.0},
     {"Vertex Size",selected_vertex_size,5.0},
     {"Edge Width",edge_width,2.0},
     {"Selected Edge Width",selected_edge_width,3.0}
    ].
