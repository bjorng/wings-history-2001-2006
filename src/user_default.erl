%%
%%  user_default.erl --
%%
%%     Extends the Erlang shell with Wings utilities.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: user_default.erl,v 1.1 2001/12/13 15:59:57 bjorng Exp $
%%

-module(user_default).

-export([help/0,wh/0,w/0,
	 wua/2,wul/0,wud/1,
	 wm/0,wicons/0,wtar/0]).

help() ->
    shell_default:help(),
    p("** Wings commands **\n"),
    p("wh()       -- print help for Wings\n"),
    ok.

wh() ->
    p("** Compiling and running Wings **\n"),
    p("w()        -- make and run Wings\n"),
    p("wm()       -- make Wings\n"),
    p("wicons()   -- collect Wings icons (must be done once)\n"),
    p("** User defined expressions for Magnet **\n"),
    p("wua(Str, Fun)   -- add user-defined expression\n"),
    p("wul()           -- list user-defined expressions\n"),
    p("wud(N)          -- delete user-defined expression\n"),
    ok.

%%%
%%% User defined expressions for the Magnet.
%%%

wua(Name, Fun) when is_list(Name),is_function(Fun) ->
    Body = extract_body(Fun),
    wings_magnet:add_user_expr(Name, Body).

wul() ->
    wings_magnet:list_user_exprs().

wud(N) ->
    wings_magnet:delete_user_expr(N).

%%%
%%% Compiling, running and so on.
%%%

w() ->
    case make() of
	up_to_date -> wings:start();
	Other -> Other
    end.

wm() ->
    make().

wicons() ->
    make:all(opts()),
    c:l(collect_bmp),
    collect_bmp:start().

wtar() ->
    tar().

%%%
%%% Internal functions.
%%%

extract_body(Fun) ->
    case erlang:fun_info(Fun, module) of
	{module,erl_eval} ->
	    case erlang:fun_info(Fun, env) of
		{env,[{eval,{shell,local_func},_},[],Body]} -> Body;
		_ -> exit(failed_to_extract_expression_from_fun)
	    end;
	_ -> exit(not_a_shell_fun)
    end.

p(String) ->
    io:put_chars(String).

make() ->
    R = make:all(opts()),
    lists:foreach(fun(F) ->
			  Mod = list_to_atom(filename:rootname(F)),
			  c:l(Mod)
		  end, filelib:wildcard("*.erl")),
    R.

basic_opts() ->
%%    [{d,'DEBUG'},debug_info,report].
    [debug_info,report].

opts() ->
    Opts = basic_opts(),
    Vsn = get_vsn(),
    [{d,wings_version,Vsn}|Opts].

get_vsn() ->
    {ok,Bin} = file:read_file("vsn.mk"),
    get_vsn(binary_to_list(Bin)).

get_vsn([Bl|T]) when Bl < $\s ->
    get_vsn(T);
get_vsn("WINGS_VSN="++T) ->
    get_vsn_1(T).

get_vsn_1([C|T]) when C > $\s ->
    [C|get_vsn_1(T)];
get_vsn_1(_) -> [].

tar() ->
    Name = tar_file_name(),
    Files = filelib:wildcard("*.{erl,hrl,c,bundle,icon,mk}") ++
	filelib:wildcard("icons/*.bmp") ++
	filelib:wildcard("plugins/default/*.erl") ++
	filelib:wildcard("plugins/win32_file/*.{erl,c}"),
    {Name,erl_tar:create(Name, Files, [compressed,verbose])}.

tar_file_name() ->
    {{Y,Mo,D},{H,Mi,_}} = erlang:localtime(),
    Time = lists:flatten(io_lib:format("~p-~s-~s_~s-~s",
				       [Y,two(Mo),two(D),two(H),two(Mi)])),
    "wings_" ++ Time ++ ".tar.gz".

two(I) when I < 100 ->
    [_|Str] = integer_to_list(I+100),
    Str.
