%%
%%  wp9_dialogs.erl --
%%
%%     Standard plugin for dialogs.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp9_dialogs.erl,v 1.5 2001/12/11 07:46:51 bjorng Exp $
%%

-module(wp9_dialogs).
-export([menus/0,init/1]).
-import(lists, [reverse/1]).

menus() -> [].

init(Next) ->
    fun(What) -> ui(What, Next) end.

ui({file,merge,Prop}, Next) ->
    Ext = property_lists:get_value(ext, Prop, ".wings"),
    wings_getline:filename("Merge file: ", Ext);
ui({file,open,Prop}, Next) ->
    Ext = property_lists:get_value(ext, Prop, ".wings"),
    wings_getline:filename("Open file: ", Ext);
ui({file,save,Prop}, Next) ->
    Ext = property_lists:get_value(ext, Prop, ".wings"),
    wings_getline:filename("Save: ", Ext);
ui({file,import,Prop}, Next) ->
    Ext = property_lists:get_value(ext, Prop),
    wings_getline:filename("Import file: ", Ext);
ui({file,export,Prop}, Next) ->
    Ext = property_lists:get_value(ext, Prop),
    wings_getline:filename("Export file: ", Ext);
ui({file,overwrite,Prop}, Next) ->
    File = property_lists:get_value(existing_file, Prop),
    wings_getline:yes_no("File \"" ++ File ++ "\" exists; overwrite?");
ui({failure,Message,Prop}, Next) ->
    wings_io:message(Message),
    aborted;
ui({message,Message}, Next) ->
    wings_io:message(Message);
ui({question,Question}, Next) ->
    wings_getline:yes_no(Question);
ui({serious_question,Question}, Next) ->
    wings_getline:yes_no(Question);
ui({ask,Qs}, Next) ->
    ask(Qs);
ui(What, Next) -> Next(What).

ask([{Prompt,Default,Min,Max}|T]=T0) when is_integer(Default) ->
    case wings_getline:number(Prompt ++ ": ", Default) of
	aborted -> aborted;
	N ->
	    case ask(T) of
		aborted -> ask(T0);
		Ns -> [N|Ns]
	    end
    end;
ask([{Prompt,Def}|T]=T0) ->
    Str0 = print_term(Def),
    case wings_getline:string(Prompt ++ ": ", Str0) of
	aborted -> aborted;
	Str ->
	    case catch make_term(Str) of
		error -> ask(T0);
		N ->
		    case ask(T) of
			aborted -> ask(T0);
			Ns -> [N|Ns]
		    end
	    end
    end;
ask([]) -> [].

print_term(Term) ->
    lists:flatten(print_term_1(Term)).

print_term_1(Tuple) when is_tuple(Tuple) ->
    ["{",print_tuple(1, size(Tuple), Tuple),"}"];
print_term_1(Float) when is_float(Float) ->
    S0 = io_lib:format("~f", [Float]),
    S = reverse(lists:flatten(S0)),
    reverse(simplify_float(S));
print_term_1(Term) ->
    io_lib:format("~p", [Term]).

print_tuple(I, I, T) ->
    print_term_1(element(I, T));
print_tuple(I, Sz, T) ->
    [print_term_1(element(I, T)),$,|print_tuple(I+1, Sz, T)].

simplify_float("0."++_=F) -> F;
simplify_float("0"++F) -> simplify_float(F);
simplify_float(F) -> F.

make_term(Str) ->
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
		{ok, Term} -> Term;
		{error, {_,_,Reason}} ->
		    io:format("~s: ~s~n", [Reason, Str]),
		    throw(error)
	    end;
	{error, {_,_,Reason}, _} ->
	    io:format("~s: ~s~n", [Reason, Str]),
	    throw(error)
    end.

