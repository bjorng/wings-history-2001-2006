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
%%     $Id: wings_file.erl,v 1.50 2002/01/28 21:53:47 bjorng Exp $
%%

-module(wings_file).
-export([init/0,finish/0,menu/3,command/2,export/3,import/3]).

-include("e3d.hrl").
-include("wings.hrl").
-include_lib("kernel/include/file.hrl").

-import(lists, [sort/1,reverse/1,flatten/1,foldl/3]).
-import(filename, [dirname/1]).
-import(wings_draw, [model_changed/1]).

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
	    separator,
	    {"Revert",revert},
	    separator,
	    {"Import",{import,[{"Nendo (.ndo)",ndo}]}},
	    {"Export",{export,ExpFormats}},
	    {"Export Selected",{export_selected,ExpFormats}},
	    separator|recent_files([{"Exit",quit}])],
    wings_menu:menu(X, Y, file, Menu, St).

command(new, St0) ->
    case new(St0) of
	aborted -> St0;
	St0 -> St0;
	St -> {new,model_changed(St)}
    end;
command(open, St0) ->
    case read(St0) of
	St0 -> St0;
	St -> {new,model_changed(St)}
    end;
command(merge, St0) ->
    case merge(St0) of
	St0 -> St0;
	St -> {save_state,model_changed(St)}
    end;
command(save, St) ->
    save(St);
command(save_as, St0) ->
    case save_as(St0) of
	aborted -> St0;
	#st{}=St -> {saved,St}
    end;
command(autosave, St) ->
    autosave(St);
command(revert, St0) ->
    case revert(St0) of
	{error,Reason} ->
	    wings_io:message("Revert failed: " ++ Reason),
	    St0;
	#st{}=St -> {save_state,model_changed(St)}
    end;
command({import,ndo}, St0) ->
    case import_ndo(St0) of
	{warning,Warn,St} ->
	    wings_util:message(Warn),
	    {save_state,model_changed(St)};
	St -> {save_state,model_changed(St)}
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
    {new,model_changed(named_open(File, St))}.

quit(#st{saved=true}) -> quit;
quit(St) ->
    case wings_util:yes_no("Do you want to save your changes before "
			   "quitting?") of
	no -> quit;
	yes ->
	    case save(St) of
		aborted -> St;
		Other -> quit
	    end;
	aborted -> St
    end.

new(#st{saved=false}=St0) ->
    case wings_util:yes_no("Do you want to save your changes?") of
	no ->
	    new(St0#st{saved=true});
	yes ->
	    case save(St0) of
		aborted -> aborted;
		{saved,St} -> new(St);
		#st{}=St -> new(St)
	    end;
	aborted -> aborted
    end;
new(#st{saved=true}=St) ->
    wings:caption(St#st{file=undefined,shapes=gb_trees:empty(),sel=[]}).

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
			    wings_io:message("Read failed: " ++ Reason),
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
		    wings_io:message("Read failed: " ++ Reason),
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
		    wings_io:message("Read failed: " ++ Reason),
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
save_1(#st{shapes=Shapes,file=Name}=St) ->
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

save_as(#st{shapes=Shapes}=St) ->
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
		    wings_io:message("Save failed: " ++ Reason),
		    aborted
	    end
    end.

wings_prop() ->
    %% Should we add autosaved wings files ??
    %% It's pretty nice to NOT see them in the file chooser /Dan
    [{ext,?WINGS},{ext_desc,"Wings File"}].    

use_autosave(File) ->
    {ok, SaveInfo} = file:read_file_info(File),
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
			abort ->  %% ???
			    File
		    end;
		SaveTime >= AutoTime ->
		    File
	    end;
	{error, _} ->  %% No autosave file
	    File
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
	    wings_io:message("AutoSave failed: " ++ Reason),
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
	Other -> ok
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
number_files([], I, Rest) -> Rest.
    
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
    Ext = property_lists:get_value(ext, Prop),
    case wings_plugin:call_ui({file,import,Prop}) of
	aborted -> aborted;
	Name ->
	    case ?SLOW(do_import(Importer, Name, St0)) of
		#st{}=St ->
		    wings_getline:set_cwd(dirname(Name)),
		    St;
		{warning,Warn,St} ->
		    wings_util:message(Warn),
		    St;
	    	{error,Reason} ->
		    wings_io:message("Import failed: " ++ Reason),
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
		    wings_io:message("Import failed: " ++ Reason),
		    St0
	    end
    end.

%%
%% Export.
%%

export(Prop, Exporter, St) ->
    case output_file(export, export_file_prop(Prop, St)) of
	aborted -> ok;
	Name ->
	    wings_getline:set_cwd(dirname(Name)),
	    ?SLOW(do_export(Exporter, Name, St))
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

eq_extensions([C|T1], [C|T2], IgnoreCase) ->
    eq_extensions(T1, T2);
eq_extensions([L|T1], [C|T2], true) when $A =< C, C =< $Z, L-C =:= 32 ->
    eq_extensions(T1, T2);
eq_extensions([_|_], [_|_], IgnoreCase) -> false;
eq_extensions([], [], IgnoreCase) -> true.

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
			Other -> aborted
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
	    St = add_materials(UsedMat, Mat, St1),
	    case gb_trees:size(St#st.shapes)-gb_trees:size(St0#st.shapes) of
		NumObjs -> St;
		N ->
		    Warn = integer_to_list(NumObjs-N) ++
			" object(s) out of " ++
			integer_to_list(NumObjs) ++
			" object(s) could not be converted.",
		    {warning,Warn,St}
	    end;
	{error,Reason}=Error ->
	    throw({command_error,Reason})
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

translate_objects([#e3d_object{name=Name,obj=Obj0}|Os], UsedMat0,
		  I, Suffix, St0) ->
    wings_io:progress("Converting obj " ++ integer_to_list(I) ++ Suffix),
    Obj1 = e3d_mesh:clean(Obj0),
    Obj = e3d_mesh:make_quads(Obj1),
    #e3d_mesh{matrix=Matrix,vs=Vs0,tx=Tx0,fs=Fs0,he=He} = Obj,
    io:format("Name ~p\n", [Name]),
    Vs = scale_objects(Vs0, Matrix),
    Tx = list_to_tuple(Tx0),
    {Fs,UsedMat} = translate_faces(Fs0, Tx, [], UsedMat0),
    ObjType = obj_type(Tx0),
    wings_io:progress("Building Wings obj " ++ integer_to_list(I) ++ Suffix),
    St = build_object(Name, ObjType, Fs, Vs, He, St0),
    translate_objects(Os, UsedMat, I+1, Suffix, St);
translate_objects([], UsedMat, _, _, St) -> {UsedMat,St}.
    
obj_type([]) -> material;
obj_type(L) -> uv.

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
translate_faces([], Txs, Acc, UsedMat) -> {reverse(Acc),UsedMat}.

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

do_export(#we{name=Name}=We, Acc) ->
    Mesh = make_mesh(We),
    [#e3d_object{name=Name,obj=Mesh}|Acc].

make_mesh(We0) ->
    #we{fs=Ftab,vs=Vs0,es=Etab,he=He0} = We = wings_we:renumber(We0, 0),
    Vs = [P || #vtx{pos=P} <- gb_trees:values(Vs0)],
    Fs0 = foldl(fun({Face,#face{mat=Mat}}, A) ->
			[make_face(Face, Mat, We)|A]
		end, [], gb_trees:to_list(Ftab)),
    Fs1 = reverse(Fs0),
    {Fs,UVTab} = make_uv(Fs1),
    He = hard_edges(gb_sets:to_list(He0), Etab, []),
    Matrix = e3d_mat:identity(),
    #e3d_mesh{type=polygon,fs=Fs,vs=Vs,tx=UVTab,he=He,matrix=Matrix}.

make_face(Face, Mat, #we{mode=uv}=We) ->
    {Vs,UVs} = wings_face:fold_vinfo(
		 fun(V, {_,_}=UV, {VAcc,UVAcc}) ->
			 {[V|VAcc],[UV|UVAcc]};
		    (V, Info, {VAcc,UVAcc}) ->
			 {[V|VAcc],UVAcc}
		 end, {[],[]}, Face, We),
    {Vs,UVs,Mat};
make_face(Face, Mat, We) ->
    Vs = wings_face:surrounding_vertices(Face, We),
    {Vs,[],Mat}.

make_uv(Faces) ->
    R0 = sofs:from_external(Faces, [{vertices,[uv],mat}]),
    P0 = sofs:projection(2, R0),
    P1 = sofs:union(P0),
    P = sofs:to_external(P1),
    UVmap = gb_trees:from_orddict(sort(number(P, 0, []))),
    make_uv_1(Faces, UVmap, []).

make_uv_1([{Vs,UVs0,Mat}|Fs], UVMap, Acc) ->
    UVs = [gb_trees:get(UV, UVMap) || UV <- UVs0],
    Rec = #e3d_face{vs=Vs,tx=UVs,mat=make_face_mat(Mat)},
    make_uv_1(Fs, UVMap, [Rec|Acc]);
make_uv_1([], UVMap, Acc) ->
    UVTab0 = gb_trees:to_list(UVMap),
    UVTab1 = sofs:from_external(UVTab0, [{uv,index}]),
    UVTab2 = sofs:converse(UVTab1),
    UVTab = [UV || {I,UV} <- sofs:to_external(UVTab2)],
    {reverse(Acc),UVTab}.

number([UV|UVs], I, Acc) ->
    number(UVs, I+1, [{UV,I}|Acc]);
number([], I, Acc) -> Acc.

make_face_mat([_|_]=Mat) -> Mat;
make_face_mat(Mat) -> [Mat].

hard_edges([E|Es], Etab, Acc) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
    hard_edges(Es, Etab, [hard(Va, Vb)|Acc]);
hard_edges([], Etab, Acc) -> Acc.

hard(A, B) when A < B -> {A,B};
hard(A, B) -> {B,A}.
