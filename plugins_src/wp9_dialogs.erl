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
%%     $Id: wp9_dialogs.erl,v 1.12 2002/11/13 13:21:30 bjorng Exp $
%%

-module(wp9_dialogs).
-export([menus/0,init/1]).
-import(lists, [reverse/1]).

menus() -> [].

init(Next) ->
    fun(What) -> ui(What, Next) end.

ui({file,open,Prop}, _Next) ->
    open_file("Open file: ", Prop);
ui({file,merge,Prop}, _Next) ->
    open_file("Merge file: ", Prop);
ui({file,import,Prop}, _Next) ->
    open_file("Import file: ", Prop);
ui({file,save,Prop}, _Next) ->
    save_file("Save: ", Prop);
ui({file,export,Prop}, _Next) ->
    save_file("Export file: ", Prop);
ui({image,formats,Formats}, _Next) ->
    image_formats(Formats);
ui({image,read,Prop}, _Next) ->
    read_image(Prop);
ui({failure,Message,_Prop}, _Next) ->
    wings_io:message(Message),
    aborted;
ui({message,Message}, _Next) ->
    message(Message);
ui({question,Question}, _Next) ->
    wings_getline:yes_no(Question);
ui({serious_question,Question}, _Next) ->
    wings_getline:yes_no(Question);
ui(What, Next) -> Next(What).

message(Message) ->
    Qs = {vframe,
	  [{label,Message},
	   {button,ok}],
	  [{title,"Wings Error"}]},
    wings_ask:dialog(Qs, fun(_) -> ignore end).

open_file(Prompt, Prop) ->
    Ext = proplists:get_value(ext, Prop, ".wings"),
    case wings_getline:filename(Prompt, Ext) of
	aborted -> aborted;
	Name -> ensure_extension(Name, Ext)
    end.

save_file(Prompt, Prop) ->
    Ext = proplists:get_value(ext, Prop, ".wings"),
    case wings_getline:filename(Prompt, Ext) of
	aborted -> aborted;
	Name0 ->
	    Name = ensure_extension(Name0, Ext),
	    case filelib:is_file(Name) of
		false ->
		    Name;
		true ->
		    case wings_getline:yes_no("File \"" ++ Name ++ "\" exists; overwrite?") of
			no -> aborted;
			yes -> Name;
			aborted -> aborted
		    end
	    end
    end.

ensure_extension(Name, Ext) ->
    case eq_extensions(Ext, filename:extension(Name)) of
	true -> Name;
	false -> Name ++ Ext
    end.
eq_extensions(Ext, Actual) when length(Ext) =/= length(Actual) ->
    false;
eq_extensions(Ext, Actual) ->
    IgnoreCase = case os:type() of
		     {win32,_} -> true;
		     _ -> false
		 end,
    eq_extensions(Ext, Actual, IgnoreCase).

eq_extensions([C|T1], [C|T2], _IgnoreCase) ->
    eq_extensions(T1, T2);
eq_extensions([L|T1], [C|T2], true) when $A =< C, C =< $Z, L-C =:= 32 ->
    eq_extensions(T1, T2);
eq_extensions([_|_], [_|_], _IgnoreCase) -> false;
eq_extensions([], [], _IgnoreCase) -> true.

read_image(Prop) ->
    Name = proplists:get_value(filename, Prop),
    e3d_image:load(Name, Prop).

image_formats(Fs0) ->
    Fs1 = [{".bmp","BMP Bitmap File"},
	   {".tif","Tiff Bitmap"},
	   {".tga","Targa File"}|Fs0],
    Fs2 = sofs:relation(Fs1),
    Fs3 = sofs:relation_to_family(Fs2),
    Fs = sofs:to_external(Fs3),
    [{Ext,Desc} || {Ext,[Desc|_]} <- Fs].
