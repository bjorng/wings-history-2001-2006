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
%%     $Id: user_default.erl,v 1.7 2002/03/13 11:57:39 bjorng Exp $
%% 

-module(user_default).

-export([help/0,wh/0,w/0,wcp/1,wcp/2,
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
    p("wcp(Mod)   -- compile plugin-in (in plugins_src, to plugins)\n"),
    p("wcp(Mod, Kind) -- compile plugin-in (in plugins_src/Kind,\n"
      "                  to plugins/Kind)\n"),
    ok.

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

wcp(Mod) when is_atom(Mod) ->
    wcp(atom_to_list(Mod));
wcp(Mod) ->
    File = filename:join("plugins_src", Mod),
    Outdir = "plugins",
    c:c(File, [{outdir,Outdir},report]).

wcp(Mod, Kind) when is_atom(Mod), is_atom(Kind) ->
    wcp(atom_to_list(Mod), atom_to_list(Kind));
wcp(Mod, Kind) ->
    File = filename:join(filename:join("plugins_src", Kind), Mod),
    Outdir = filename:join("plugins",Kind),
    c:c(File, [{outdir,Outdir},report]).

wtar() ->
    tar().

%%%
%%% Internal functions.
%%%

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
    [debug_info,report,warn_unused_vars].

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
    Files = filelib:wildcard("*.{erl,hrl,icon,mk}") ++
	filelib:wildcard("icons/*.bmp") ++
	filelib:wildcard("plugins/default/*.erl") ++
	filelib:wildcard("plugins/win32_file/*.{erl,c}") ++
	filelib:wildcard("plugins_src/primitives/*.{erl,c}") ++
	filelib:wildcard("plugins_src/commands/*.{erl,c}") ++
	filelib:wildcard("plugins_src/import_export/*.{erl,c}"),
    {Name,erl_tar:create(Name, Files, [compressed,verbose])}.

tar_file_name() ->
    {{Y,Mo,D},{H,Mi,_}} = erlang:localtime(),
    Time = lists:flatten(io_lib:format("~p-~s-~s_~s-~s",
				       [Y,two(Mo),two(D),two(H),two(Mi)])),
    "wings_" ++ Time ++ ".tar.gz".

two(I) when I < 100 ->
    [_|Str] = integer_to_list(I+100),
    Str.
