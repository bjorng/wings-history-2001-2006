%%
%%  wp9_dialogs.erl --
%%
%%     Standard plugin for dialogs.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp9_dialogs.erl,v 1.21 2003/12/21 19:12:02 bjorng Exp $
%%

-module(wp9_dialogs).
-export([init/1]).
-import(lists, [reverse/1]).

init(Next) ->
    fun(What) -> ui(What, Next) end.

ui({file,open_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Open"),
    open_file(Title ++ ": ", Prop);
ui({file,open_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, "Open"),
    open_dialog(Title, Prop, Cont);
ui({file,save_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Save"),
    save_file(Title ++ ": ", Prop);
ui({image,formats,Formats}, _Next) ->
    image_formats(Formats);
ui({image,read,Prop}, _Next) ->
    read_image(Prop);
ui(What, Next) -> Next(What).

open_file(Prompt, Prop) ->
    Exts = file_filters_old(Prop),
    case wings_getline:filename(Prompt, Exts) of
	aborted -> aborted;
	Name -> ensure_extension(Name, Exts)
    end.

save_file(Prompt, Prop) ->
    Exts = file_filters_old(Prop),
    case wings_getline:filename(Prompt, Exts) of
	aborted -> aborted;
	Name0 ->
	    Name = ensure_extension(Name0, Exts),
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

file_filters_old(Prop) ->
    case proplists:get_value(extensions, Prop, none) of
	none ->
	    Ext = proplists:get_value(ext, Prop, ".wings"),
	    [Ext];
	Exts ->
	    file_filters_old_1(Exts, [])
    end.

file_filters_old_1([{Ext,_Desc}|T], Acc) ->
    file_filters_old_1(T, [Ext|Acc]);
file_filters_old_1([], Acc) -> reverse(Acc).

ensure_extension(Name, [Ext]) ->
    case eq_extensions(Ext, filename:extension(Name)) of
	true -> Name;
	false -> Name ++ Ext
    end;
ensure_extension(Name, [_|_]) -> Name.

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


%%%
%%% File dialogs implemented using the dialog handler.
%%%

open_dialog(Title, Props, Cont) ->
    [{_,Def}|_] = Exts = file_filters(Props),
    Qs = {vframe,
	  [{hframe,
	    [{vframe,
	      [{label,"File name"},
	       {label,"File format"}]},
	     {vframe,
	      [{text,""},
	       {menu,Exts,Def}]},
	     {vframe,[{button,Title,fun([Name|_]) -> Cont(Name) end,[ok]},
		      {button,"Cancel",fun(_) -> Cont(aborted) end,[cancel]}]}]}]},
    wings_ask:dialog(Title, Qs, fun(_) -> ignore end).

file_filters(Prop) ->
    Exts0 = case proplists:get_value(extensions, Prop, none) of
		none ->
		    Ext = proplists:get_value(ext, Prop, ".wings"),
		    ExtDesc = proplists:get_value(ext_desc, Prop,  "Wings File"),
		    [{Ext,ExtDesc}];
		Other -> Other
	    end,
    Exts = Exts0 ++ [{".*","All Files"}],
    [file_filter(Es) || Es <- Exts].

file_filter({"."++Ext0,Desc0}) ->
    Ext = list_to_atom(Ext0),
    Desc = Desc0 ++ " (*." ++ Ext0 ++ ")",
    {Desc,Ext}.
