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
%%     $Id: wings_lang.erl,v 1.8 2004/10/30 09:19:07 bjorng Exp $
%%
%%  Totally rewritten but Riccardo is still the one who did the hard work.
%%

-module(wings_lang).
-include("wings.hrl").

%% Wings API.
-export([init/0,str/2,
	 available_languages/0,load_language/1]).

%% Translation support tools.
-export([generate_template/0,generate_template/1,diff/1,diff/2]).

-import(lists, [reverse/1,reverse/2]).

-define(DEF_LANG_STR, "en").			% English
-define(DEF_LANG_ATOM, en).			% English
-define(LANG_DIRS, ["ebin","src","plugins"]).

str(Key, DefStr) ->
    case get(?MODULE) of
	?DEF_LANG_ATOM -> DefStr;
	_ ->
	    try ets:lookup_element(?MODULE, Key, 2) of
		Str -> binary_to_list(Str)
	    catch
		error:_ -> DefStr
	    end
    end.

init() ->
    wings_pref:set_default(language, ?DEF_LANG_STR),
    Lang = case wings_pref:get_value(language) of 
	       ?DEF_LANG_STR=L -> L;
	       Other ->
		   case lists:member(Other, available_languages()) of
		       true -> Other;
		       false -> ?DEF_LANG_STR
		   end
	   end,
    load_language(Lang).

load_language(Lang0) when is_list(Lang0) ->
    Lang = list_to_atom(Lang0),
    put(?MODULE, Lang), 
    catch ets:delete(?MODULE), 
    case Lang of
	?DEF_LANG_ATOM -> ok;
	_  ->
	    ets:new(?MODULE, [named_table]),
	    Root = code:lib_dir(wings),
	    LangFile = "_"++Lang0++".lang",
	    load_language(Root, ?LANG_DIRS, LangFile)
    end.

load_language(_, [],_) -> ok;
load_language(Root, [Dir|Dirs], Lang) ->
    Path = filename:join(Root, Dir),
    case file:list_dir(Path) of
	{ok, List} ->
	    load_language2(Path, List, Lang),
	    load_language(Root,Dirs,Lang);
	_ -> 
	    load_language(Root,Dirs, Lang)
    end.

load_language2(Dir, [File|Fs], Lang) ->
    case catch lists:nthtail(length(File)-length(Lang), File) of
	Lang -> 
	    case file:consult(filename:join(Dir, File)) of
		{ok,Terms} -> load_file(Terms);
		{error,{Line,Mod,Info}} ->
		    io:format("~s, line ~p: ~s\n",
			      [File,Line,Mod:format_error(Info)]);
		Other ->
 		    io:format("Problem reading language file ~p:\n~p\n",
			      [File,Other])
	    end;
	_ ->
	    Path = filename:join(Dir,File),
	    case filelib:is_dir(Path) of
		true ->
		    load_language(Dir, [File], Lang);
		false ->
		    ignore
	    end
    end,
    load_language2(Dir,Fs,Lang);
load_language2(_,[],_) -> ok.

load_file(Trans) ->
    Add = fun(Level,Str) -> 
		  Key = list_to_tuple(reverse(Level)),
		  ets:insert(?MODULE,{Key,list_to_binary(Str)}) 
	  end,
    Trav = 
	fun(Data = [Cont|_], Level, Trav) when is_tuple(Cont) ->
		Insert = fun({Key, Next}) ->
				 Trav(Next, [Key|Level], Trav)
			 end,
		lists:foreach(Insert, Data);
	   (Str, Level, _) ->
		Add(Level,Str)
	end,
    Trav(Trans,[],Trav).

available_languages() ->
    Root = code:lib_dir(wings),
    Files = 
	filelib:wildcard(filename:join([Root,"src","*.lang"])) ++ 
	filelib:wildcard(filename:join([Root,"ebin","*.lang"])),
    Extract = fun(File, Acc) ->
		      case string:tokens(filename:rootname(File),[$_]) of
			  [_Name,Lang] -> [Lang|Acc];
			  _ -> Acc
		      end
	      end,
    lists:usort([?DEF_LANG_STR|lists:foldl(Extract, [], Files)]).
    			     
%%%%%%%%%% Tools %%%%%%%%%%%

diff(LangFile) ->
    EngTemplFile = get_en_template(LangFile),
    diff(LangFile, EngTemplFile).
    
diff(LangFile, EngTmplFile) ->
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

generate_template() ->
    {ok,Cwd} = file:get_cwd(),
    generate_template(Cwd).

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
    
skip_ws([$\s |R]) -> skip_ws(R);
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

get_en_template(Name) ->
    get_en_template_1(reverse(filename:rootname(Name))).

get_en_template_1([$_|T]) ->
    reverse(T, "_en.lang");
get_en_template_1([_|T]) ->
    get_en_template_1(T).
    
    
