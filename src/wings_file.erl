%%
%%  wings_file.erl --
%%
%%     This module contains the commands in the File menu.
%%
%%  Copyright (c) 2001-2000 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_file.erl,v 1.65 2002/06/14 12:59:22 bjorng Exp $
%%

-module(wings_file).
-export([init/0,finish/0,menu/3,command/2,
	 export/3,export_filename/2,import/3]).

-include("e3d.hrl").
-include("wings.hrl").
-include_lib("kernel/include/file.hrl").

-import(lists, [sort/1,reverse/1,flatten/1,foldl/3,keymember/3]).
-import(filename, [dirname/1]).

-define(WINGS,    ".wings").
-define(AUTOSAVE, "_as").
-define(BACKUP,   "_bup").

init() ->
    case wings_pref:get_value(current_directory) of
	undefined -> ok;
	Cwd -> file:set_cwd(Cwd)
    end,
    set_autosave_timer().

finish() ->
    case file:get_cwd() of
	{ok,Cwd} -> wings_pref:set_value(current_directory, Cwd);
	{error,_} -> ok
    end.

menu(X, Y, St) ->
    ExpFormats = [{"Nendo (.ndo)",ndo}],
    Menu = [{"New",new},
	    {"Open",open},
	    {"Merge",merge},
	    separator,
	    {"Save",save},
	    {"Save As",save_as},
	    {"Save Selected",save_selected},
	    separator,
	    {"Revert",revert},
	    separator,
	    {"Import",{import,[{"Nendo (.ndo)",ndo}]}},
	    {"Export",{export,ExpFormats}},
	    {"Export Selected",{export_selected,ExpFormats}},
	    separator,
	    {"Render",{render,[]}},
	    separator|recent_files([{"Exit",quit}])],
    wings_menu:menu(X, Y, file, Menu, St).

command(new, St0) ->
    case new(St0) of
	aborted -> St0;
	St0 -> St0;
	St -> {new,St}
    end;
command(open, St0) ->
    case read(St0) of
	St0 -> St0;
	St -> {new,St}
    end;
command(merge, St0) ->
    case merge(St0) of
	St0 -> St0;
	St -> {save_state,St}
    end;
command(save, St) ->
    save(St);
command(save_as, St0) ->
    case save_as(St0) of
	aborted -> St0;
	#st{}=St -> {saved,St}
    end;
command(save_selected, St) ->
    save_selected(St),
    St;
command(autosave, St) ->
    autosave(St);
command(revert, St0) ->
    case revert(St0) of
	{error,Reason} ->
	    wings_util:error("Revert failed: " ++ Reason),
	    St0;
	#st{}=St -> {save_state,St}
    end;
command({import,ndo}, St0) ->
    case import_ndo(St0) of
	{warning,Warn,St} ->
	    wings_util:message(Warn, St),
	    {save_state,St};
	St -> {save_state,St}
    end;
command({export,ndo}, St) ->
    export_ndo(St),
    St;
command({export_selected,ndo}, St) ->
    Shs0 = wings_sel:fold(fun(_, #we{id=Id}=We, A) ->
				  [{Id,We}|A]
			  end, [], St),
    Shs = gb_trees:from_orddict(reverse(Shs0)),
    export_ndo(St#st{shapes=Shs}),
    St;
command(quit, St) ->
    quit(St);
command(Key, St) when is_integer(Key) ->
    Recent = wings_pref:get_value(recent_files, []),
    {_,File} = lists:nth(Key, Recent),
    {new,named_open(File, St)}.

quit(#st{saved=true}) -> quit;
quit(St) ->
    case wings_util:yes_no("Do you want to save your changes before "
			   "quitting?") of
	no -> quit;
	yes ->
	    case save(St) of
		aborted -> St;
		_Other -> quit
	    end;
	aborted -> St
    end.

new(#st{saved=true}=St) ->
    wings:caption(St#st{file=undefined,shapes=gb_trees:empty(),sel=[]});
new(St0) -> %% File is not saved or autosaved.
    wings:caption(St0#st{saved=false}), 
    case wings_util:yes_no("Do you want to save your changes?") of
	no -> %% Remove autosaved file, user has explicitly said so.
	    catch file:delete(autosave_filename(St0#st.file)),
	    new(St0#st{saved=true});
	yes ->
	    case save(St0) of
		aborted -> aborted;
		{saved,St} -> new(St);
		#st{}=St -> new(St)
	    end;
	aborted -> aborted
    end.

read(St0) ->
    case new(St0) of
	aborted -> St0;
	St1 ->
	    case wings_plugin:call_ui({file,open,wings_prop()}) of
		aborted -> St0;
		Name0 ->
		    Name = ensure_extension(Name0, ?WINGS, wings_extensions()),
		    add_recent(Name),
		    File = use_autosave(Name),
		    case ?SLOW(wings_ff_wings:import(File, St1)) of
			#st{}=St ->
			    wings_getline:set_cwd(dirname(Name)),
			    wings:caption(St#st{saved=true,file=Name});
			{error,Reason} ->
			    wings_util:error("Read failed: " ++ Reason),
			    St0
		    end
	    end
    end.

named_open(Name, St0) ->
    case new(St0) of
	aborted -> St0;
	St1 ->
	    add_recent(Name),
	    File = use_autosave(Name),
	    case ?SLOW(wings_ff_wings:import(File, St1)) of
		#st{}=St ->
		    wings_getline:set_cwd(dirname(Name)),
		    wings:caption(St#st{saved=true,file=Name});
		{error,Reason} ->
		    wings_util:error("Read failed: " ++ Reason),
		    St0
	    end
    end.

merge(St0) ->
    case wings_plugin:call_ui({file,merge,wings_prop()}) of
	aborted -> St0;
	Name0 ->
	    Name = ensure_extension(Name0, ?WINGS, wings_extensions()),
	    File = use_autosave(Name),
	    case ?SLOW(wings_ff_wings:import(File, St0)) of
		{error,Reason} ->
		    wings_util:error("Read failed: " ++ Reason),
		    St0;
		#st{}=St -> St
	    end
    end.

save(St0) ->
    case save_1(St0) of
	aborted -> St0;
	St0 -> St0;
	St  -> {saved,St}
    end.

save_1(#st{saved=true}=St) -> St;
save_1(#st{file=undefined}=St) ->
    save_as(St);
save_1(#st{file=Name}=St) ->
    Backup = backup_filename(Name),
    file:rename(Name, Backup),
    file:delete(autosave_filename(Name)),
    case ?SLOW(wings_ff_wings:export(Name, St)) of
	ok ->
	    wings_getline:set_cwd(dirname(Name)),
	    wings:caption(St#st{saved=true});
	{error,Reason} ->
	    wings_plugin:call_ui({failure,"Save failed: " ++ Reason})
    end.

save_as(St) ->
    case output_file(save, wings_prop()) of
	false -> St;
	aborted -> aborted;
	Name ->
	    add_recent(Name),
	    case ?SLOW(wings_ff_wings:export(Name, St)) of
		ok ->
		    wings_getline:set_cwd(dirname(Name)),
		    wings:caption(St#st{saved=true,file=Name});
		{error,Reason} ->
		    wings_util:error("Save failed: " ++ Reason),
		    aborted
	    end
    end.

save_selected(#st{sel=[]}) ->
    wings_util:error("This command requires a selection.");
save_selected(#st{shapes=Shs0,sel=Sel}=St0) ->
    case output_file(save, wings_prop()) of
	aborted -> ok;
	Name when is_list(Name) ->
	    Shs = [Sh || {Id,_}=Sh <- gb_trees:to_list(Shs0),
			 keymember(Id, 1, Sel)],
	    St = St0#st{shapes=gb_trees:from_orddict(Shs)},
	    case ?SLOW(wings_ff_wings:export(Name, St)) of
		ok -> ok;
		{error,Reason} ->
		    wings_util:error("Save failed: " ++ Reason)
	    end
    end.

wings_prop() ->
    %% Should we add autosaved wings files ??
    %% It's pretty nice to NOT see them in the file chooser /Dan
    [{ext,?WINGS},{ext_desc,"Wings File"}].    

use_autosave(File) ->
    case file:read_file_info(File) of
	{ok, SaveInfo} ->
	    Auto = autosave_filename(File),
	    case file:read_file_info(Auto) of
		{ok, AutoInfo} ->
		    SaveTime = calendar:datetime_to_gregorian_seconds(SaveInfo#file_info.mtime),
		    AutoTime = calendar:datetime_to_gregorian_seconds(AutoInfo#file_info.mtime),
		    if
			AutoTime > SaveTime ->
			    Msg = "An autosaved file with later time stamp exists, "
				"do you want to load the autosaved file instead?",
			    case wings_util:yes_no(Msg) of
				yes -> 
				    Auto;
				no -> 
				    File;
				aborted ->  %% ???
				    File
			    end;
			SaveTime >= AutoTime ->
			    File
		    end;
		{error, _} ->  %% No autosave file
		    File
	    end;
	{error, _} -> %% use autosave if exists 
	    Auto = autosave_filename(File),
	    case file:read_file_info(Auto) of
		{ok, AutoInfo} ->
		    Auto;
		_ -> %% Let reader take care of error
		    File
	    end
    end.

set_autosave_timer() ->
    case wings_pref:get_value(autosave_time) of
	0 -> ok;
	N when is_number(N) ->
	    wings_io:set_timer(trunc(N*60000), {action,{file,autosave}})
    end.
    
autosave(#st{file=undefined} = St) -> 
    set_autosave_timer(),
    St;
autosave(#st{saved=true} = St) ->
    set_autosave_timer(),
    St;
autosave(#st{saved=auto} = St) ->
    set_autosave_timer(),
    St;
autosave(#st{file=Name}=St) ->
    Auto = autosave_filename(Name),
    %% Maybe this should be spawned to another process
    %% to let the autosaving be done in the background.
    %% But I don't want to copy a really big model either.
    case ?SLOW(wings_ff_wings:export(Auto, St)) of
	ok ->
	    set_autosave_timer(),
	    wings:caption(St#st{saved=auto});
	{error,Reason} ->
	    set_autosave_timer(),
	    wings_util:error("AutoSave failed: " ++ Reason),
            St
    end.

autosave_filename(File) ->
    File ++ ?AUTOSAVE.

backup_filename(File) ->
    File ++ ?BACKUP.

wings_extensions() ->
    [?WINGS,?WINGS ++ ?BACKUP,?WINGS ++ ?AUTOSAVE].

add_recent(Name) ->
    Base = filename:basename(Name),
    case filename:extension(Base) of
	?WINGS ->
	    File = {Base,Name},
	    Recent0 = wings_pref:get_value(recent_files, []),
	    Recent1 = Recent0 -- [File],
	    Recent = add_recent(File, Recent1),
	    wings_pref:set_value(recent_files, Recent);
	_Other -> ok
    end.

add_recent(File, [A,B,C|_]) -> [File,A,B,C];
add_recent(File, Recent) -> [File|Recent].

recent_files(Rest) ->
    case wings_pref:get_value(recent_files, []) of
	[] -> Rest;
	Files -> number_files(Files, 1, [separator|Rest])
    end.

number_files([{Base,_}|T], I, Rest) ->
    [{Base,I}|number_files(T, I+1, Rest)];
number_files([], _I, Rest) -> Rest.
    
%%
%% The Revert command.
%%

revert(#st{file=undefined}=St) -> St;
revert(#st{file=File}=St0) ->
    St1 = St0#st{shapes=gb_trees:empty(),sel=[]},
    case ?SLOW(wings_ff_wings:import(File, St1)) of
	#st{}=St -> St;
	{error,_}=Error -> Error
    end.

%%
%% Import.
%%

import(Prop, Importer, St0) ->
    case wings_plugin:call_ui({file,import,Prop}) of
	aborted -> aborted;
	Name ->
	    case ?SLOW(do_import(Importer, Name, St0)) of
		#st{}=St ->
		    wings_getline:set_cwd(dirname(Name)),
		    St;
		{warning,Warn,St} ->
		    wings_util:message(Warn, St),
		    St;
	    	{error,Reason} ->
		    wings_util:error("Import failed: " ++ Reason),
		    St0
	    end
    end.

import_ndo(St0) ->
    Ext = ".ndo",
    Prop = [{ext,".ndo"},{ext_desc,"Nendo File"}],
    case wings_plugin:call_ui({file,import,Prop}) of
	aborted -> St0;
	Name0 ->
	    Name = ensure_extension(Name0, Ext),
	    case ?SLOW(wings_ff_ndo:import(Name, St0)) of
		#st{}=St ->
		    wings_getline:set_cwd(dirname(Name)),
		    St;
	    	{error,Reason} ->
		    wings_util:error("Import failed: " ++ Reason),
		    St0
	    end
    end.

%%
%% Export.
%%

export_filename(Prop, St) ->
    case output_file(export, export_file_prop(Prop, St)) of
	aborted -> ok;
	Name ->
	    wings_getline:set_cwd(dirname(Name)),
	    Name
    end.

export(Props0, Exporter, St) ->
    case export_file_prop(Props0, St) of
	none ->
	    ?SLOW(do_export(Exporter, none, St));
	Props ->
	    case output_file(export, Props) of
		aborted -> ok;
		Name ->
		    wings_getline:set_cwd(dirname(Name)),
		    ?SLOW(do_export(Exporter, Name, St))
	    end
    end.

export_ndo(St) ->
    Prop = [{ext,".ndo"},{ext_desc,"Nendo File"}],
    case output_file(export, export_file_prop(Prop, St)) of
	aborted -> St;
	Name ->
	    wings_getline:set_cwd(dirname(Name)),
	    wings_ff_ndo:export(Name, St)
    end.

%%% Utilities.

export_file_prop(none, _) -> none;
export_file_prop(Prop, #st{file=undefined}) -> Prop;
export_file_prop(Prop, #st{file=File}) ->
    Ext = property_lists:get_value(ext, Prop),
    Def = filename:rootname(filename:basename(File), ?WINGS) ++ Ext,
    [{default_filename,Def}|Prop].

ensure_extension(Name, Ext) ->
    case eq_extensions(Ext, filename:extension(Name)) of
	true -> Name;
	false -> Name ++ Ext
    end.

ensure_extension(Name, Default, [Ext|Rest]) when list(Ext) ->
    case eq_extensions(filename:extension(Name), Ext) of
	true -> Name;
	false -> ensure_extension(Name, Default, Rest)
    end;
ensure_extension(Name, Default, []) -> Name ++ Default.

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

output_file(Tag, Prop) ->
    case wings_plugin:call_ui({file,Tag,Prop}) of
	aborted -> aborted;
	Name0 ->
	    Ext = property_lists:get_value(ext, Prop),
	    Name = ensure_extension(Name0, Ext),
	    case filelib:is_file(Name) of
		true ->
		    Base = filename:basename(Name),
		    OProp = [{existing_file,Base}],
		    case wings_plugin:call_ui({file,overwrite,OProp}) of
			yes -> Name;
			_Other -> aborted
		    end;
		false -> Name
	    end
    end.

%%%
%%% Generic import code.
%%%

do_import(Importer, Name, St0) ->
    wings_io:progress("Reading " ++ filename:basename(Name)),
    case Importer(Name) of
	{ok,#e3d_file{objs=Objs,mat=Mat}} ->
	    NumObjs = length(Objs),
	    Suffix = " of " ++ integer_to_list(NumObjs),
	    {UsedMat,St1} = translate_objects(Objs, gb_sets:empty(),
					     1, Suffix, St0),
	    {St2,NameMap} = add_materials(UsedMat, Mat, St1),
	    St = rename_materials(NameMap, St0, St2),
	    case gb_trees:size(St#st.shapes)-gb_trees:size(St0#st.shapes) of
		NumObjs -> St;
		N ->
		    Warn = integer_to_list(NumObjs-N) ++
			" object(s) out of " ++
			integer_to_list(NumObjs) ++
			" object(s) could not be converted.",
		    {warning,Warn,St}
	    end;
	{error,Reason} ->
	    wings_util:error(Reason)
    end.

add_materials(UsedMat0, Mat0, St) ->
    UsedMat = sofs:from_external(gb_sets:to_list(UsedMat0), [name]),
    Mat1 = sofs:relation(Mat0, [{name,data}]),
    Mat2 = sofs:restriction(Mat1, UsedMat),
    NotDefined0 = sofs:difference(UsedMat, sofs:domain(Mat2)),
    DummyData = sofs:from_term([], data),
    NotDefined = sofs:constant_function(NotDefined0, DummyData),
    Mat = sofs:to_external(sofs:union(Mat2, NotDefined)),
    wings_material:add_materials(Mat, St).

rename_materials([], _, St) -> St;
rename_materials(NameMap0, #st{onext=FirstId}, #st{shapes=Shs0}=St) ->
    NameMap = gb_trees:from_orddict(sort(NameMap0)),
    Shs = rename_mat(gb_trees:to_list(Shs0), NameMap, FirstId, []),
    St#st{shapes=Shs}.

rename_mat([{Id,_}=Obj|Objs], NameMap, FirstId, Acc) when Id < FirstId ->
    rename_mat(Objs, NameMap, FirstId, [Obj|Acc]);
rename_mat([{Id,#we{fs=Ftab0}=We}|Objs], NameMap, FirstId, Acc) ->
    Ftab1 = foldl(fun({Face,#face{mat=Mat0}=Rec}=Pair, A) ->
			  case gb_trees:lookup(Mat0, NameMap) of
			      none -> [Pair|A];
			      {value,Mat} -> [{Face,Rec#face{mat=Mat}}|A]
			  end
		  end, [], gb_trees:to_list(Ftab0)),
    Ftab = gb_trees:from_orddict(reverse(Ftab1)),
    rename_mat(Objs, NameMap, FirstId, [{Id,We#we{fs=Ftab}}|Acc]);
rename_mat([], _, _, Acc) ->
    gb_trees:from_orddict(reverse(Acc)).

translate_objects([#e3d_object{name=Name,obj=Obj0}|Os], UsedMat0,
		  I, Suffix, St0) ->
    wings_io:progress("Converting obj " ++ integer_to_list(I) ++ Suffix),
    Obj1 = e3d_mesh:clean(Obj0),
    Obj = e3d_mesh:make_quads(Obj1),
    #e3d_mesh{matrix=Matrix,vs=Vs0,tx=Tx0,fs=Fs0,he=He} = Obj,
    Vs = scale_objects(Vs0, Matrix),
    Tx = list_to_tuple(Tx0),
    {Fs,UsedMat} = translate_faces(Fs0, Tx, [], UsedMat0),
    ObjType = obj_type(Tx0),
    wings_io:progress("Building Wings obj " ++ integer_to_list(I) ++ Suffix),
    St = build_object(Name, ObjType, Fs, Vs, He, St0),
    translate_objects(Os, UsedMat, I+1, Suffix, St);
translate_objects([], UsedMat, _, _, St) -> {UsedMat,St}.
    
obj_type([]) -> material;
obj_type([_|_]) -> uv.

scale_objects(Vs, none) -> Vs;
scale_objects(Vs, Matrix) -> [e3d_mat:mul_point(Matrix, P) || P <- Vs].

translate_faces([#e3d_face{vs=Vs,tx=Tx0,mat=Mat0}|Fs], Txs, Acc, UsedMat0) ->
    UsedMat = add_used_mat(Mat0, UsedMat0),
    Mat = translate_mat(Mat0),
    FaceData = case Tx0 of
		   [] -> {Mat,Vs};
		   Tx1 ->
		       Tx = [element(Tx+1, Txs) || Tx <- Tx1],
		       {Mat,Vs,Tx}
	       end,
    translate_faces(Fs, Txs, [FaceData|Acc], UsedMat);
translate_faces([], _Txs, Acc, UsedMat) -> {reverse(Acc),UsedMat}.

add_used_mat([], UsedMat) -> UsedMat;
add_used_mat([M|Ms], UsedMat) -> add_used_mat(Ms, gb_sets:add(M, UsedMat)).
    
translate_mat([]) -> default;
translate_mat([Mat]) -> Mat;
translate_mat([_|_]=List) -> List.

build_object(Name, Type, Fs, Vs, He, St) ->
    case catch wings_we:build(Type, Fs, Vs, He) of
	{'EXIT',Reason} ->
	    io:format("Conversion failed: ~P\n", [Reason,20]),
	    St;
	We -> store_object(Name, We, St)
    end.

store_object(undefined, We, #st{onext=Oid}=St) ->
    Name = "unnamed_object" ++ integer_to_list(Oid),
    wings_shape:new(Name, We, St);
store_object(Name, We, St) ->
    wings_shape:new(Name, We, St).

%%%
%%% Generic export code.
%%%

do_export(Exporter, Name, #st{shapes=Shs}=St) ->
    Objs = foldl(fun do_export/2, [], gb_trees:values(Shs)),
    Creator = "Wings 3D " ++ ?WINGS_VERSION,
    Mat = wings_material:used_materials(St),
    Contents = #e3d_file{objs=Objs,mat=Mat,creator=Creator},
    Exporter(Name, Contents).

do_export(#we{perm=Perm}, Acc) when ?IS_NOT_VISIBLE(Perm) -> Acc;
do_export(#we{name=Name}=We, Acc) ->
    Mesh = make_mesh(We),
    [#e3d_object{name=Name,obj=Mesh}|Acc].

make_mesh(We0) ->
    #we{fs=Ftab,vs=Vs0,es=Etab,he=He0} = We = wings_we:renumber(We0, 0),
    Vs = [P || #vtx{pos=P} <- gb_trees:values(Vs0)],
    {ColTab0,UvTab0} = make_tables(We),
    ColTab1 = gb_trees:from_orddict(ColTab0),
    UvTab1 = gb_trees:from_orddict(UvTab0),
    Fs0 = foldl(fun({_,#face{mat='_hole_'}}, A) ->
			A;
		   ({Face,#face{mat=Mat}}, A) ->
			[make_face(Face, Mat, ColTab1, UvTab1, We)|A]
		end, [], gb_trees:to_list(Ftab)),
    Fs = reverse(Fs0),
    He = hard_edges(gb_sets:to_list(He0), Etab, []),
    Matrix = e3d_mat:identity(),
    ColTab = strip_numbers(ColTab0),
    UvTab = strip_numbers(UvTab0),
    Mesh = #e3d_mesh{type=polygon,fs=Fs,vs=Vs,tx=UvTab,he=He,
		     vc=ColTab,matrix=Matrix},
    e3d_mesh:renumber(Mesh).

make_face(Face, Mat, _ColTab, UvTab, #we{mode=uv}=We) ->
    {Vs,UVs} = wings_face:fold_vinfo(
		 fun(V, {_,_}=UV, {VAcc,UVAcc}) ->
			 {[V|VAcc],[gb_trees:get(UV, UvTab)|UVAcc]};
		    (V, _, {VAcc,UVAcc}) ->
			 {[V|VAcc],UVAcc}
		 end, {[],[]}, Face, We),
    #e3d_face{vs=Vs,tx=UVs,mat=make_face_mat(Mat)};
make_face(Face, Mat, ColTab, _UvTab, #we{mode=vertex}=We) ->
    {Vs,Cols} = wings_face:fold_vinfo(
		  fun(V, {_,_,_}=Col, {VAcc,ColAcc}) ->
			  {[V|VAcc],[gb_trees:get(Col, ColTab)|ColAcc]};
		     (V, _Info, {VAcc,ColAcc}) ->
			  {[V|VAcc],ColAcc}
		  end, {[],[]}, Face, We),
    #e3d_face{vs=Vs,vc=Cols,mat=make_face_mat(Mat)};
make_face(Face, Mat, _, _, We) ->
    Vs = wings_face:surrounding_vertices(Face, We),
    #e3d_face{vs=Vs,mat=make_face_mat(Mat)}.

make_tables(#we{mode=vertex}=We) ->
    {make_table(We),[]};
make_tables(#we{mode=uv}=We) ->
    {[],make_table(We)};
make_tables(_) ->
    {[],[]}.

make_table(#we{es=Etab}) ->
    Cuvs = foldl(fun(#edge{a=A,b=B}, Acc) ->
			 [A,B|Acc]
		 end, [], gb_trees:values(Etab)),
    number(ordsets:from_list(Cuvs)).

number(L) ->
    number(L, 0, []).
number([H|T], I, Acc) ->
    number(T, I+1, [{H,I}|Acc]);
number([], _, Acc) -> reverse(Acc).

strip_numbers(L) ->
    strip_numbers(L, []).
strip_numbers([{H,_}|T], Acc) ->
    strip_numbers(T, [H|Acc]);
strip_numbers([], Acc) -> reverse(Acc).

make_face_mat([_|_]=Mat) -> Mat;
make_face_mat(Mat) -> [Mat].

hard_edges([E|Es], Etab, Acc) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
    hard_edges(Es, Etab, [hard(Va, Vb)|Acc]);
hard_edges([], _Etab, Acc) -> Acc.

hard(A, B) when A < B -> {A,B};
hard(A, B) -> {B,A}.

