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
%%     $Id: wp9_dialogs.erl,v 1.1 2001/10/24 08:51:39 bjorng Exp $
%%

-module(wp9_dialogs).
-export([menus/0,init/1]).

menus() -> [].

init(Next) ->
    fun(What) -> ui(What, Next) end.

ui({failure,Message}, Next) ->
    wings_io:message(Message),
    aborted;
ui({ask,Qs}, Next) ->
    ask(Qs);
ui({file,merge}, Next) ->
    wings_getline:filename("Merge file: ", ".wings");
ui({file,ask_save_changes}, Next) ->
    wings_getline:yes_no("Do you want to save changes to your model?");
ui({file,{open,Ext}}, Next) ->
    wings_getline:filename("Open file: ", Ext);
ui({file,{save,Ext}}, Next) ->
    wings_getline:filename("Save: ", Ext);
ui({file,{import,Ext}}, Next) ->
    wings_getline:filename("Import file: ", Ext);
ui({file,{export,Ext}}, Next) ->
    wings_getline:filename("Export file: ", Ext);
ui({file,{overwrite,File}}, Next) ->
    wings_getline:yes_no("File \"" ++ File ++
			 "\" exists; overwrite?");
ui({quit,ask_save_changes}, Next) ->
    wings_getline:yes_no("Do you want to save before quitting?");
ui(What, Next) -> Next(What).


ask([{Prompt,Default,Min,Max}|T]=T0) ->
    case wings_getline:number(Prompt ++ ": ", Default) of
	aborted -> aborted;
	N ->
	    case ask(T) of
		aborted -> ask(T0);
		Ns -> [N|Ns]
	    end
    end;
ask([]) -> [].
