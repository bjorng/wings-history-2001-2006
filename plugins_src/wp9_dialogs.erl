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
%%     $Id: wp9_dialogs.erl,v 1.2 2001/10/25 14:24:04 bjorng Exp $
%%

-module(wp9_dialogs).
-export([menus/0,init/1]).

menus() -> [].

init(Next) ->
    fun(What) -> ui(What, Next) end.

ui({file,merge,Prop}, Next) ->
    Ext = property_lists:get_value(ext, Prop, ".wings"),
    wings_getline:filename("Merge file: ", Ext);
ui({file,ask_save_changes,Prop}, Next) ->
    wings_getline:yes_no("Do you want to save changes to your model?");
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
ui({quit,ask_save_changes,Prop}, Next) ->
    wings_getline:yes_no("Do you want to save before quitting?");
ui({failure,Message,Prop}, Next) ->
    wings_io:message(Message),
    aborted;
ui({ask,Qs}, Next) ->
    ask(Qs);
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
