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
%%     $Id: wp9_dialogs.erl,v 1.14 2002/12/28 22:10:27 bjorng Exp $
%%

-module(wp9_dialogs).
-export([menus/0,init/1]).
-import(lists, [reverse/1]).

menus() -> [].

init(Next) ->
    fun(What) -> ui(What, Next) end.

ui({file,open_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Open"),
    open_file(Title ++ ": ", Prop);
ui({file,save_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Save"),
    save_file(Title ++ ": ", Prop);
ui({image,formats,Formats}, _Next) ->
    image_formats(Formats);
ui({image,read,Prop}, _Next) ->
    read_image(Prop);
ui({message,Message}, _Next) ->
    message(Message);
ui({question,Question}, _Next) ->
    wings_getline:yes_no(Question);
ui(What, Next) -> Next(What).

message(Message) ->
    Qs = {vframe,
	  [{label,Message},
	   {button,ok}]},
    wings_ask:dialog("Error!", Qs, fun(_) -> ignore end).

open_file(Prompt, Prop) ->
    Exts = file_filters(Prop),
    case wings_getline:filename(Prompt, Exts) of
	aborted -> aborted;
	Name -> Name
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

file_filters(Prop) ->
    case proplists:get_value(extensions, Prop, none) of
	none ->
	    Ext = proplists:get_value(ext, Prop, ".wings"),
	    [Ext];
	Exts ->
	    file_filters_1(Exts, [])
    end.

file_filters_1([{Ext,_Desc}|T], Acc) ->
    file_filters_1(T, [Ext|Acc]);
file_filters_1([], Acc) -> reverse(Acc).

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
