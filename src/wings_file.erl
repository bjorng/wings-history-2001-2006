%%
%%  wings_file.erl --
%%
%%     This module contains the commands in the File menu.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_file.erl,v 1.27 2001/11/07 07:09:59 bjorng Exp $
%%

-module(wings_file).
-export([menu/3,command/2]).

-include("e3d.hrl").
-include("wings.hrl").

-import(lists, [sort/1,reverse/1,flatten/1,foldl/3]).
-import(filename, [dirname/1]).
-import(wings_draw, [model_changed/1]).

menu(X, Y, St) ->
    Menu = [{"New","Ctrl-N",new},
	    {"Open","Ctrl-O",open},
	    {"Merge","Ctrl-L",merge},
	    separator,
	    {"Save","Ctrl-S",save},
	    {"Save As",save_as},
	    separator,
	    {"Revert",revert},
	    separator,
	    {"Import",{import,
		       {{"Nendo (.ndo)",ndo},
			{"3D Studio (.3ds)",tds},
			{"Wawefront (.obj)",obj}}}},
	    {"Export",{export,
		       {{"3D Studio (.3ds)",tds},
			{"Wawefront (.obj)",obj},
			{"RenderMan (.rib)",rib}}}},
	    separator|recent_files([{"Exit","Ctrl-Q",quit}])],
    wings_menu:menu(X, Y, file, list_to_tuple(Menu)).

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
command(revert, St0) ->
    case revert(St0) of
	{error,Reason} ->
	    wings_io:message("Revert failed: " ++ Reason),
	    St0;
	#st{}=St -> {save_state,model_changed(St)}
    end;
command({import,Type}, St0) ->
    case import(Type, St0) of
	St0 -> St0;
	St -> {save_state,model_changed(St)}
    end;
command({export,Type}, St) ->
    export(Type, St),
    St;
command(quit, St) ->
    quit(St);
command(Key, St) when is_integer(Key) ->
    Recent = wings_pref:get_value(recent_files, []),
    {_,File} = lists:nth(Key, Recent),
    {new,model_changed(named_open(File, St))}.

quit(#st{saved=true}) -> quit;
quit(St) ->
    case wings_plugin:call_ui({quit,ask_save_changes,[]}) of
	no -> quit;
	yes ->
	    case save(St) of
		aborted -> St;
		Other -> quit
	    end;
	aborted -> St
    end.

new(#st{saved=false}=St0) ->
    case wings_plugin:call_ui({file,ask_save_changes,[]}) of
	no ->
	    new(St0#st{saved=true});
	yes ->
	    case save(St0) of
		aborted -> aborted;
		St -> new(St)
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
		    Name = ensure_extension(Name0, ".wings"),
		    add_recent(Name),
		    case wings_ff_wings:import(Name, St1) of
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
	    case wings_ff_wings:import(Name, St1) of
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
	    Name = ensure_extension(Name0, ".wings"),
	    case wings_ff_wings:import(Name, St0) of
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
    case wings_ff_wings:export(Name, St) of
	ok ->
	    wings_getline:set_cwd(dirname(Name)),
	    St#st{saved=true};
	{error,Reason} ->
	    wings_plugin:call_ui({failure,"Save failed: " ++ Reason})
    end.

save_as(#st{shapes=Shapes}=St) ->
    case output_file(save, wings_prop()) of
	false -> St;
	aborted -> aborted;
	Name ->
	    add_recent(Name),
	    case wings_ff_wings:export(Name, St) of
		ok ->
		    wings_getline:set_cwd(dirname(Name)),
		    wings:caption(St#st{saved=true,file=Name});
		{error,Reason} ->
		    wings_io:message("Save failed: " ++ Reason),
		    aborted
	    end
    end.

wings_prop() ->
    [{ext,".wings"},{ext_desc,"Wings File"}].

add_recent(Name) ->
    Base = filename:basename(Name),
    File = {Base,Name},
    Recent0 = wings_pref:get_value(recent_files, []),
    Recent1 = Recent0 -- [File],
    Recent = add_recent(File, Recent1),
    wings_pref:set_value(recent_files, Recent).

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
    case wings_ff_wings:import(File, St1) of
	#st{}=St -> St;
	{error,_}=Error -> Error
    end.

%%
%% Import.
%%

import(tds, St) -> import(".3ds", e3d_tds, St);
import(obj, St) -> import(".obj", e3d_obj, St);
import(ndo, St) -> import_ndo(St).

import(Ext, Mod, St0) ->
    Prop = file_prop(Ext),
    case wings_plugin:call_ui({file,import,Prop}) of
	aborted -> St0;
	Name0 ->
	    Name = ensure_extension(Name0, Ext),
	    case do_import(Mod, Name, St0) of
		#st{}=St ->
		    wings_getline:set_cwd(dirname(Name)),
		    St;
	    	{error,Reason} ->
		    wings_io:message("Import failed: " ++ Reason),
		    St0
	    end
    end.

import_ndo(St0) ->
    Ext = ".ndo",
    Prop = file_prop(Ext),
    case wings_plugin:call_ui({file,import,Prop}) of
	aborted -> St0;
	Name0 ->
	    Name = ensure_extension(Name0, Ext),
	    case wings_ff_ndo:import(Name, St0) of
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

export(tds, St) -> export(e3d_tds, ".3ds", St);
export(rib, St) -> export(e3d_rib, ".rib", St);
export(obj, St) -> export(e3d_obj, ".obj", St).

export(Mod, Ext, St) ->
    case output_file(export, file_prop(Ext)) of
	aborted -> St;
	Name ->
	    wings_getline:set_cwd(dirname(Name)),
	    do_export(Mod, Name, St)
    end.

%%% Utilities.

file_prop(".ndo"=Ext) -> file_prop(Ext, "Nendo File");
file_prop(".3ds"=Ext) -> file_prop(Ext, "3D Studio File");
file_prop(".obj"=Ext) -> file_prop(Ext, "Wawefront");
file_prop(".rib"=Ext) -> file_prop(Ext, "Renderman").

file_prop(Ext, Desc) ->
    [{ext,Ext},{ext_desc,Desc}].

ensure_extension(Name, Ext) ->
    case filename:extension(Name) of
	Ext -> Name;
	Other -> Name ++ Ext
    end.

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

do_import(Mod, Name, St0) ->
    wings_io:progress("Importing: ", 0),
    case Mod:import(Name) of
	{ok,#e3d_file{objs=Objs,mat=Mat}} ->
	    wings_io:progress("Importing: ", 50),
	    {UsedMat,St} = translate_objects(Objs, gb_sets:empty(), St0),
	    add_materials(UsedMat, Mat, St);
	{error,Reason}=Error ->
	    Error
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

translate_objects([#e3d_object{name=Name,obj=Obj0}|Os], UsedMat0, St0) ->
    Obj1 = e3d_mesh:clean(Obj0),
    Obj = e3d_mesh:make_quads(Obj1),
    #e3d_mesh{matrix=Matrix0,type=Type,vs=Vs,tx=Tx0,fs=Fs0,he=He} = Obj,
    io:format("Name ~p\n", [Name]),
    Matrix = case Matrix0 of
		 none -> identity;
		 _ -> Matrix0
	     end,
    {Fs,UsedMat} = translate_faces(Fs0, [], UsedMat0),
    St = build_object(Name, Matrix, Fs, Vs, He, St0),
    translate_objects(Os, UsedMat, St);
translate_objects([], UsedMat, St) -> {UsedMat,St}.

translate_faces([#e3d_face{vs=Vs,tx=Tx,mat=Mat0}|Fs], Acc, UsedMat0) ->
    UsedMat = add_used_mat(Mat0, UsedMat0),
    Mat = translate_mat(Mat0),
    translate_faces(Fs, [{Mat,Vs}|Acc], UsedMat);
translate_faces([], Acc, UsedMat) -> {Acc,UsedMat}.

add_used_mat([], UsedMat) -> UsedMat;
add_used_mat([M|Ms], UsedMat) -> add_used_mat(Ms, gb_sets:add(M, UsedMat)).
    
translate_mat([]) -> default;
translate_mat([Mat]) -> Mat;
translate_mat([_|_]=List) -> List.

build_object(Name, Matrix0, Fs, Vs, He, St) ->
    Matrix = identity,
    case
	%%catch
	wings_we:build(Matrix, Fs, Vs, He) of
	{'EXIT',Reason} ->
	    io:format("Conversion failed\n"),
	    St;
	We -> wings_shape:new(Name, We, St)
    end.

%%%
%%% Generic export code.
%%%

do_export(Mod, Name, St) ->
    Objs = wings_util:fold_shape(fun do_export/2, [], St),
    Creator = "Wings 3D " ++ ?WINGS_VERSION,
    Mat = wings_material:used_materials(St),
    Contents = #e3d_file{objs=Objs,mat=Mat,creator=Creator},
    Mod:export(Name, Contents).

do_export(#shape{name=Name,matrix=Matrix,sh=#we{}=We}, Acc) ->
    Mesh = make_mesh(Matrix, We),
    [#e3d_object{name=Name,obj=Mesh}|Acc].

make_mesh(Matrix, We0) ->
    #we{vs=Vs0,es=Etab,he=He0} = We = wings_we:renumber(We0, 0),
    Vs = [P || #vtx{pos=P} <- gb_trees:values(Vs0)],
    Fs1 = wings_util:fold_face(
	    fun(Face, #face{mat=Mat}, A) ->
		    [make_face(Face, Mat, We)|A]
	    end, [], We),
    Fs = reverse(Fs1),
    He = hard_edges(gb_sets:to_list(He0), Etab, []),
    #e3d_mesh{type=polygon,fs=Fs,vs=Vs,he=He,matrix=Matrix}.

make_face(Face, Mat, We) ->
    Vs = wings_face:surrounding_vertices(Face, We),
    #e3d_face{vs=Vs,mat=make_face_mat(Mat)}.

make_face_mat([_|_]=Mat) -> Mat;
make_face_mat(Mat) -> [Mat].

hard_edges([E|Es], Etab, Acc) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
    hard_edges(Es, Etab, [hard(Va, Vb)|Acc]);
hard_edges([], Etab, Acc) -> Acc.

hard(A, B) when A < B -> {A,B};
hard(A, B) -> {B,A}.
