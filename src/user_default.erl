%%
%%  user_default.erl --
%%
%%     Extends the Erlang shell with Wings utilities.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: user_default.erl,v 1.12 2003/05/08 07:00:55 bjorng Exp $
%% 

-module(user_default).

-export([help/0,wh/0,
	 wx/0,wxp/0,wxe/0,wxu/1,wxu/3,wxunref/0,wxundef/0]).

-import(lists, [foldl/3]).

help() ->
    shell_default:help(),
    p("** Wings commands **\n"),
    p("wh()       -- print help for Wings\n"),
    ok.

wh() ->
    p("** Xref for Wings modules **\n"),
    p("wx()       -- collect xref information\n"),
    p("wxp()      -- add xref information for plug-ins\n"),
    p("wxe()      -- add xref information for ESDL\n"),
    p("wxunref()  -- print unused functions\n"),
    p("wxundef()  -- print calls to undefined functions\n"),
    p("wxu(M)     -- print uses of module M\n"),
    p("wxu(M, F, A) -- print uses of M:F/A\n"),
    ok.

%%%
%%% Xref support.
%%%


wx() ->
    WingsLib = code:lib_dir(wings),
    WingsEbin = filename:join(WingsLib, "ebin"),
    xref:start(s),
    xref:set_default(s, [{verbose,false},{warnings,false},{builtins,true}]),
    xref:set_library_path(s, code:get_path() -- [WingsEbin]),
    {ok,Ms} = xref:add_directory(s, WingsEbin),
    length(Ms).

wxp() ->
    Dirs = get_plugin_dirs(),
    foldl(fun(D, N) -> 
		  {ok,Ms} = xref:add_directory(s, D),
		  N+length(Ms)
	  end, 0, Dirs).

wxe() ->
    Dir = filename:dirname(code:which(gl)),
    {ok,Ms} = xref:add_directory(s, Dir),
    length(Ms).

wxu(Mod) when is_atom(Mod) ->
    result(xref:q(s, make_query("domain(E || ~p) - ~p", [Mod,Mod])));
wxu({M,_,_}=MFA) ->
    result(xref:q(s, make_query("domain(E || ~p) - ~p", [MFA,M]))).

wxu(M, F, A) ->
    MFA = {M,F,A},
    result(xref:q(s, make_query("domain(E || ~p) - ~p", [MFA,M]))).

wxundef() ->
    xref:analyze(s, undefined_function_calls).

wxunref() ->
    io:format("~p\n", [xref:analyze(s, exports_not_used)]).

result({ok,List}) ->
    io:format("~p\n", [List]);
result(Other) -> Other.

make_query(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%%%
%%% Internal functions.
%%%

p(String) ->
    io:put_chars(String).

get_plugin_dirs() ->
    Prefix = filename:join(code:lib_dir(wings), "plugins"),
    Path = code:get_path(),
    get_plugin_dirs(Path, Prefix, []).

get_plugin_dirs([D|Ds], Prefix, Acc) ->
    case lists:prefix(Prefix, D) of
	false -> get_plugin_dirs(Ds, Prefix, Acc);
	true -> get_plugin_dirs(Ds, Prefix, [D|Acc])
    end;
get_plugin_dirs([], _, Acc) -> Acc.
