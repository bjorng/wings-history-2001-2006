%%
%%  wp9_dialogs.erl --
%%
%%     Standard plugin for dialogs.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp9_dialogs.erl,v 1.7 2002/05/05 07:47:33 bjorng Exp $
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
    message(Message);
ui({question,Question}, Next) ->
    wings_getline:yes_no(Question);
ui({serious_question,Question}, Next) ->
    wings_getline:yes_no(Question);
ui(What, Next) -> Next(What).

message(Message) ->
    St = get(wings_st_kludge),
    Qs = {vframe,
	  [{label,Message},
	   {button,ok}],
	  [{title,"Wings Error"}]},
    wings_ask:dialog(Qs, St, fun(Res) -> ignore end).

