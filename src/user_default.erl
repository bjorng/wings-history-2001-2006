%%
%%  user_default.erl --
%%
%%     Extends the Erlang shell with Wings utilities.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: user_default.erl,v 1.16 2003/12/08 19:15:45 bjorng Exp $
%% 

-module(user_default).

-export([help/0,wh/0,
	 wx/0,wxe/0,wxu/1,wxu/3,wxunref/0,wxundef/0]).

-import(lists, [foldl/3]).

help() ->
    shell_default:help(),
    p("** Wings commands **\n"),
    p("wh()       -- print help for Wings\n"),
    ok.

wh() ->
    p("** Xref for Wings modules **\n"),
    p("wx()       -- collect xref information\n"),
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
    Dirs = get_plugin_dirs(),
    foldl(fun(D, N) -> 
		  {ok,PMs} = xref:add_directory(s, D),
		  N+length(PMs)
	  end, length(Ms), Dirs).

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
    {ok,Unref0} = xref:analyze(s, exports_not_used),
    Unref = filter_unref(Unref0),
    io:format("~p\n", [Unref]).

filter_unref([{M,F,A}=MFA|T]) ->
    case filter_unref(M, F, A) of
	true -> [MFA|filter_unref(T)];
	false -> filter_unref(T)
    end;
filter_unref([]) -> [].

filter_unref(user_default, _, _) -> false;
filter_unref(wings_start, start, 0) -> false;
filter_unref(wings_start, start, 1) -> false;
filter_unref(wings_start, start_halt, 0) -> false;
filter_unref(wings_start, start_halt, 1) -> false;
filter_unref(M, F, A) ->
    case atom_to_list(M) of
	"wpc_"++_ ->
	    filter_standard_plugin(F, A);
	"wp9_"++_ ->
	    filter_ui_plugin(F, A);
	"wpf_"++_ ->
	    filter_font_plugin(F, A);
	_ ->
	    true
    end.

filter_standard_plugin(init, 0) -> false;
filter_standard_plugin(command, 2) -> false;
filter_standard_plugin(menu, 2) -> false;
filter_standard_plugin(_, _) -> true.

filter_ui_plugin(init, 1) -> false;
filter_ui_plugin(_, _) -> true.

filter_font_plugin(char, 1) -> false;
filter_font_plugin(desc, 0) -> false;
filter_font_plugin(draw, 1) -> false;
filter_font_plugin(height, 0) -> false;
filter_font_plugin(width, 0) -> false;
filter_font_plugin(height, 1) -> false;
filter_font_plugin(width, 1) -> false;
filter_font_plugin(_, _) -> true.

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
