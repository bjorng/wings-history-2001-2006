%%
%%  wings_lang.erl --
%%
%%     Implementation of languages.
%%
%%  Copyright (c) 2004 Riccardo Venier, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%       
%% 
%%
%% wings_lang.erl : INTERNATIONALISATION UTILITIES FOR WINGS3D, 
%%   riccardo venier (verme@insiberia.net)
%%
%%  Totally rewritten but Riccardo is still the one who did the hard work.
%%  /Dan

-module(wings_lang).
-include("wings.hrl").

-export([generate_template/1,diff/2]).
%%-compile(export_all).
-import(lists, [reverse/1]).


%% Tools
diff(LangFile,EngTmplFile) ->
    {ok, Lang} = file:consult(LangFile),
    {ok, Eng} = file:consult(EngTmplFile),
    diff(Eng,EngTmplFile,Lang,LangFile,[]).

diff(_,_,_,_,[_,_,_]) -> ok;
diff([{Key,Info}|ER],EF,Lang0,LF,Mode) ->
    case get_key(Key,Lang0) of
	{Info2, Lang} ->
	    diff(Info,EF,Info2,LF,[Key|Mode]),
	    diff(ER,EF,Lang,LF,Mode);
	Lang ->
	    io:format("Missing ~p in ~s~n", 
		      [reverse([Key|Mode]),LF]),
	    diff(ER,EF,Lang,LF,Mode)
    end;
diff([],_,Keys,LF,Mode) ->
    Info = fun({Key,_}) -> 
		   io:format("Not used ~p in ~p~n", 
			     [reverse([Key|Mode]),LF])
	   end,
    lists:foreach(Info, Keys).

get_key(Key, List) ->
    get_key(List,Key,[]).
get_key([{Key,Found}|Rest],Key,Acc) -> {Found,lists:reverse(Acc,Rest)};
get_key([Miss|Rest],Key,Acc) -> get_key(Rest,Key,[Miss|Acc]);
get_key([],_,Acc) -> lists:reverse(Acc).

generate_template(Dir) ->
    Fs = files(Dir),
    OutFile0 = case filename:basename(Dir) of
		   "." -> "wings";
		   "src" -> "wings";
		   DirName -> DirName
	       end,
    OutFile = OutFile0 ++ "_en.lang",
    {ok, Fd} = file:open(OutFile, [write]),
    io:format("Writing ~p~n", [filename:absname(OutFile)]),
    Write = fun(File) -> 
		    io:format("Scanning ~p~n", [File]),
		    scan_file(File, Fd)
	    end,
    try lists:foreach(Write, reverse(Fs))
    after file:close(Fd)
    end.

scan_file(FileName, Out) ->
    Mod = list_to_atom(filename:rootname(filename:basename(FileName))),
    {ok,Bin} = file:read_file(FileName),
    File = binary_to_list(Bin),
    search_and_gen_strs(File, Mod, Out, []).

search_and_gen_strs([], Mod, Out, Acc0) ->
    generate_strs(lists:usort(Acc0), Mod, Out);
search_and_gen_strs(File0, Mod, Out, Acc0) ->
    File1 = skip(File0),
    {File, Acc} = get_args(File1, Acc0),
    search_and_gen_strs(File,Mod,Out,Acc).

generate_strs([],Mod,_Out) -> 
    io:format("No strings in ~p~n", [Mod]),
    skip;
generate_strs(Refs,Mod,Out) -> 
    io:format(Out,"{~p,~n [~n",[Mod]),
    format_lang(Refs,Mod,undefined,Out).

format_lang([This={Func,Id,Str}|R], Mod, Func, Fd) ->
    io:format(Fd,"    {~p,~p}",[Id,Str]),
    close_expr(This,R,Fd),
    format_lang(R,Mod,Func,Fd);
format_lang([This={Func,Id,Str}|R], Mod, _, Fd) ->
    io:format(Fd,"  {~p,~n   [{~p,~p}",[Func,Id,Str]),
    close_expr(This,R,Fd),
    format_lang(R,Mod,Func,Fd);
format_lang([],_,_,_) -> ok.

close_expr({Func,_,_}, [{Func,_,_}|_],Fd) ->
    io:format(Fd,",~n",[]);
close_expr({_,_,_}, [],Fd) ->
    io:format(Fd,"]}]}.~n",[]);
close_expr({_,_,_}, _,Fd) ->
    io:format(Fd,"]},~n",[]).

get_args([], Acc) -> {[], Acc};
get_args(Str0, Acc) ->
    {Func, Str1} = get_id(Str0, []),
    {Id, Str2}   = get_id(Str1, []),    
    {Info,Str}   = get_info(Str2, []),
    {Str, [{Func,Id,Info}|Acc]}.

get_id([$,|Rest], Acc) -> {to_erltype(lists:reverse(Acc)),Rest};
get_id([$   |R],Acc) -> get_id(R,Acc);
get_id([$\t |R],Acc) -> get_id(R,Acc);
get_id([$\n |R],Acc) -> get_id(R,Acc);
get_id([C|R],Acc) -> get_id(R,[C|Acc]).

to_erltype(List) ->
    case catch list_to_integer(List) of
	{'EXIT', _} ->
	    list_to_atom(List);
	Int -> 
	    Int
    end.

get_info(Str0, Acc0) ->
    [$"|Str1] = skip_ws(Str0),
    {Acc,Str2} = get_info1(Str1,Acc0),
    case skip_ws(Str2) of
	Str = [$"|_] -> get_info(Str, Acc);
	[$)|Str] -> {lists:reverse(Acc), Str}
    end.

get_info1([$"|R], Acc) -> {Acc, R};
get_info1([$\\,$t|R], Acc) -> get_info1(R,[$\t|Acc]);
get_info1([$\\,$n|R], Acc) -> get_info1(R,[$\n|Acc]);
get_info1([$\\,C|R], Acc) -> get_info1(R,[C|Acc]);
get_info1([C|R],Acc) -> get_info1(R,[C|Acc]).
    
skip_ws([$   |R]) -> skip_ws(R);
skip_ws([$\t |R]) -> skip_ws(R);
skip_ws([$\n |R]) -> skip_ws(R);
skip_ws(R) -> R.

skip([$?,$S,$T,$R,$( | Rest]) -> Rest;
skip([_|Rest]) -> skip(Rest);
skip([]) -> [].

files(Dir) ->
    {ok,Fs0} = file:list_dir(Dir),
    Filter = 
	fun(File,Acc) ->
		F = filename:join(Dir,File),
		case filelib:is_file(F) andalso 
		    filename:extension(F) == ".erl" of
		    true ->
			[F|Acc];
		    false ->
			Acc
		end
	end,
    lists:foldl(Filter, [], lists:sort(Fs0)).

try_location(Dir, File) ->
    Name = filename:join(Dir, File),
    case filelib:is_file(Name) of
	true -> Name;        
	false -> none
    end.
