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
%%     $Id: wings_pref.erl,v 1.3 2001/11/06 14:54:58 dgud Exp $
%%

-module(wings_pref).
-export([init/0,finish/0,
	 get_value/1,get_value/2,set_value/2,
	 locate/1]).

-include("wings.hrl").
-import(lists, [foreach/2]).

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

defaults() ->
    [{recent_files,[]}].

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
