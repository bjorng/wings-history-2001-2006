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
%%     $Id: wp9_dialogs.erl,v 1.27 2003/12/27 15:30:40 bjorng Exp $
%%

-module(wp9_dialogs).
-export([init/1]).
-import(lists, [reverse/1,reverse/2,sort/1]).

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
    [{_,Def}|_] = Types = file_filters(Props),
    Dir = proplists:get_value(directory, Props, "/"),
    Ps = [{directory,Dir},{filetype,Def},{filename,""}],
    {dialog,Qs,Ask} = do_dialog(Types, Title, Cont, Ps),
    wings_ask:dialog(Title, Qs, Ask).

do_dialog(Types, Title, Cont, Ps) ->
    Dir = proplists:get_value(directory, Ps),
    DefType = proplists:get_value(filetype, Ps),
    Filename = proplists:get_value(filename, Ps),
    Wc = atom_to_list(DefType),
    FileList = file_list(Dir, Wc),
    DirMenu = dir_menu(Dir, []),
    Qs = {vframe,
	  [{hframe,[{label,"Look in:"},
		    {menu,DirMenu,Dir,[{key,directory},{hook,fun menu_hook/2}]},
		    {button,"Up",fun(_) -> ignore end,[{key,up},{hook,fun up_button/2}]}]},
	   panel,
	   FileList,
	   panel,
	   {hframe,
	    [{vframe,
	      [{label,"File name"},
	       {label,"File format"}]},
	     {vframe,
	      [{text,Filename,[{key,filename}]},
	       {menu,Types,DefType,[{key,filetype},{hook,fun menu_hook/2}]}]},
	     {vframe,[{button,Title,
		       fun(Res) ->
			       Dir = proplists:get_value(directory, Res),
			       Name = proplists:get_value(filename, Res),
			       NewName = filename:join(Dir, Name),
			       Cont(NewName)
		       end,[ok,{hook,fun ok_hook/2}]},
		      {button,"Cancel",fun(_) -> Cont(aborted) end,[cancel]}]}]}]},
    Ask = fun(Res) ->
		  do_dialog(Types, Title, Cont, Res)
	  end,
    {dialog,Qs,Ask}.

dir_menu(Dir0, Acc) ->
    Entry = {Dir0,Dir0},
    case filename:dirname(Dir0) of
	Dir0 -> reverse(Acc, [Entry]);
	Dir -> dir_menu(Dir, [Entry|Acc])
    end.

menu_hook(update, {Var,_I,Val,Sto}) ->
    {done,gb_trees:update(Var, Val, Sto)};
menu_hook(_, _) -> void.

up_button(update, {Var,_I,Val,Sto0}) ->
    Dir0 = gb_trees:get(directory, Sto0),
    Dir = filename:dirname(Dir0),
    Sto1 = gb_trees:update(directory, Dir, Sto0),
    Sto2 = gb_trees:update(filename, "", Sto1),
    Sto = gb_trees:update(Var, Val, Sto2),
    {done,Sto};
up_button(_, _) -> void.

ok_hook(is_disabled, {_Var,_I,Store}) ->
    gb_trees:get(filename, Store) == [];
ok_hook(_, _) -> void.

file_filters(Prop) ->
    Exts = case proplists:get_value(extensions, Prop, none) of
	       none ->
		   Ext = proplists:get_value(ext, Prop, ".wings"),
		   ExtDesc = proplists:get_value(ext_desc, Prop,  "Wings File"),
		   [{Ext,ExtDesc}];
	       Other -> Other
	   end,
    [file_filter(Es) || Es <- Exts] ++ [{"All Files (*)",''}].

file_filter({"."++Ext0,Desc0}) ->
    Ext = list_to_atom(Ext0),
    Desc = Desc0 ++ " (*." ++ Ext0 ++ ")",
    {Desc,Ext}.

file_list(Dir, Wc) ->
    {ok,Files0} = file:list_dir(Dir),
    {Folders,Files} = file_list_filter(Files0, Dir, Wc),
    All0 = sort(Folders) ++ sort(Files),
    All = [{F} || F <- All0],
    {table,[{"Filename"}|All],[{hook,fun choose_file/2}]}.

file_list_filter(Files0, Dir, Wc) ->
    {Folders,Files} = file_list_folders(Files0, Dir, [], []),
    {Folders,file_list_filter_1(Files, Wc)}.

file_list_filter_1(Files, []) ->
    [{F,[space2|F]} || F <- Files];
file_list_filter_1(Files, Wc) ->
    Ext = [$.|Wc],
    [{F,[space2|F]} || F <- Files, lists:suffix(Ext, F)].

file_list_folders(["."++_|Fs], Dir, DirAcc, FileAcc) ->
    file_list_folders(Fs, Dir, DirAcc, FileAcc);
file_list_folders([F|Fs], Dir, DirAcc, FileAcc) ->
    case filelib:is_dir(filename:join(Dir, F)) of
	true ->
	    file_list_folders(Fs, Dir, [{{dir,F},[folder|F]}|DirAcc], FileAcc);
	false ->
	    file_list_folders(Fs, Dir, DirAcc, [F|FileAcc])
    end;
file_list_folders([], _, DirAcc, FileAcc) -> {DirAcc,FileAcc}.

choose_file(update, {Var,_I,File,Sto0}) ->
    Sto1 = gb_trees:update(Var, File, Sto0),
    Dir0 = gb_trees:get(directory, Sto1),
    Full = filename:join(Dir0, File),
    case filelib:is_dir(Full) of
	true ->
	    Dir = filename:join(Dir0, File),
	    Sto = gb_trees:update(filename, "", Sto1),
	    {done,gb_trees:update(directory, Dir, Sto)};
	false ->
	    {store,gb_trees:update(filename, File, Sto0)}
    end;
choose_file(_, _) -> void.
