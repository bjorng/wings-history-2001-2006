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
%%     $Id: wings_file.erl,v 1.6 2001/08/31 09:46:13 bjorng Exp $
%%

-module(wings_file).
-export([new/1,read/1,merge/1,save/1,save_as/1,import/2,export/2,delete/1]).

-include("e3d.hrl").
-include("wings.hrl").

-import(lists, [sort/1,reverse/1,flatten/1,foldl/3]).
-import(filename, [dirname/1]).

new(#st{saved=false}=St0) ->
    case wings_getline:yes_no("Do you want to save changes to your model?") of
	no -> new(St0#st{saved=true});
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
	    case wings_getline:filename("Read file: ", ".wings") of
		aborted -> St0;
		Name0 ->
		    Name = ensure_extension(Name0, ".wings"),
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

merge(St0) ->
    case wings_getline:filename("Merge file: ", ".wings") of
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

save(#st{saved=true}=St) -> St;
save(#st{file=undefined}=St) ->
    save_as(St);
save(#st{shapes=Shapes,file=Name}=St) ->
    case wings_ff_wings:export(Name, St) of
	ok ->
	    wings_getline:set_cwd(dirname(Name)),
	    St#st{saved=true};
	{error,Reason} ->
	    wings_io:message("Save failed: " ++ Reason),
	    aborted
    end.

save_as(#st{shapes=Shapes}=St) ->
    case output_file("Save: ", ".wings") of
	false -> St;
	aborted -> aborted;
	Name ->
	    case wings_ff_wings:export(Name, St) of
		ok ->
		    wings_getline:set_cwd(dirname(Name)),
		    wings:caption(St#st{saved=true,file=Name});
		{error,Reason} ->
		    wings_io:message("Save failed: " ++ Reason),
		    aborted
	    end
    end.

import(tds, St) -> import(".3ds", e3d_tds, St);
import(obj, St) -> import(".obj", e3d_obj, St).

import(Ext, Mod, St0) ->
    case wings_getline:filename("Import file: ", Ext) of
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

export(tds, St) -> export(e3d_tds, ".3ds", St);
export(rib, St) -> export(e3d_rib, ".rib", St);
export(obj, St) -> export(e3d_obj, ".obj", St).

export(Mod, Ext, St) ->
    case output_file("Export file: ", Ext) of
	aborted -> St;
	Name ->
	    wings_getline:set_cwd(dirname(Name)),
	    do_export(Mod, Name, St)
    end.

ensure_extension(Name, Ext) ->
    case filename:extension(Name) of
	Ext -> Name;
	Other -> Name ++ Ext
    end.

output_file(Prompt, Ext) ->
    case wings_getline:filename(Prompt, Ext) of
	aborted -> aborted;
	Name0 ->
	    Name = ensure_extension(Name0, Ext),
	    case filelib:is_file(Name) of
		true ->
		    Base = filename:basename(Name),
		    case wings_getline:yes_no("File \"" ++ Base ++
					      "\" exists; overwrite? ") of
			yes -> Name;
			no -> aborted
		    end;
		false -> Name
	    end
    end.

delete(#st{file=undefined}=St) -> aborted;
delete(#st{file=Name}=St) ->
    case wings_getline:yes_no("Delete file \"" ++ Name ++ "\"? ") of
	yes -> file:delete(Name);
	no -> aborted
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
    UsedMat = sofs:set(gb_sets:to_list(UsedMat0)),
    Mat1 = sofs:relation(Mat0),
    Mat1 = sofs:restriction(Mat1, UsedMat),
    NotDefined0 = sofs:difference(UsedMat, sofs:domain(Mat1)),
    %% XXX In the future, mayby sofs:adjoin() will work.
    NotDefined1 = [{M,[]} || M <- sofs:to_external(NotDefined0)],
    NotDefined = sofs:relation(NotDefined1),
    Mat = sofs:to_external(sofs:union(Mat1, NotDefined)),
    add_materials(Mat, St).
    
add_materials([{Name,Prop}|Ms], St0) ->
    Mat = translate_mat(Prop, #mat{}),
    St = wings_material:add(Name, Mat, St0),
    add_materials(Ms, St);
add_materials([], St) -> St.

translate_mat([{ambient,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{ambient=RGB});
translate_mat([{diffuse,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{diffuse=RGB});
translate_mat([{specular,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{specular=RGB});
translate_mat([{shininess,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{shininess=RGB});
translate_mat([{opacity,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{opacity=RGB});
translate_mat([{twosided,Boolean}|T], Mat) ->
    translate_mat(T, Mat#mat{twosided=Boolean});
translate_mat([Other|T], #mat{attr=Attr}=Mat) ->
    translate_mat(T, Mat#mat{attr=[Other|Attr]});
translate_mat([], Mat) -> Mat.

translate_objects([#e3d_object{name=Name,obj=Obj0}|Os], UsedMat0, St0) ->
    Obj1 = e3d_mesh:clean(Obj0),
    Obj = e3d_mesh:make_quads(Obj1),
    #e3d_mesh{matrix=Matrix0,type=Type,vs=Vs,fs=Fs0,he=He} = Obj,
    io:format("Name ~p\n", [Name]),
    Matrix = case Matrix0 of
		 none -> wings_mat:identity();
		 _ -> Matrix0
	     end,
    {Fs,UsedMat} = translate_faces(Fs0, [], UsedMat0),
    St = build_object(Name, Matrix, Fs, Vs, He, St0),
    translate_objects(Os, UsedMat, St);
translate_objects([], UsedMat, St) -> {UsedMat,St}.

translate_faces([#e3d_face{vs=Vs,mat=Mat0}|Fs], Acc, UsedMat0) ->
    UsedMat = add_used_mat(Mat0, UsedMat0),
    Mat = translate_mat(Mat0),
    translate_faces(Fs, [{Mat,Vs}|Acc], UsedMat);
translate_faces([], Acc, UsedMat) ->
    {Acc,UsedMat}.

add_used_mat([], UsedMat) -> UsedMat;
add_used_mat([M|Ms], UsedMat) ->
    add_used_mat(Ms, gb_sets:add(M, UsedMat)).
    
translate_mat([]) -> default;
translate_mat([Mat]) -> Mat;
translate_mat([_|_]=List) -> List.
    
build_object(Name, Matrix0, Fs0, Vs, He, St0) ->
    Matrix = wings_mat:identity(),
    case wings_we:build(Name, Matrix, Fs0, Vs, He, St0) of
	{'EXIT',Reason} ->
	    io:format("~P\n", [Reason,20]),
	    io:format("Conversion failed\n"),
	    St0;
	St -> St
    end.

%%%
%%% Generic export code.
%%%

do_export(Mod, Name, St) ->
    Objs = wings_util:fold_shape(fun do_export/2, [], St),
    {Major,Minor} = ?WINGS_VERSION,
    Creator = flatten(io_lib:format("Wings 3D ~p.~p", [Major,Minor])),
    Mat = export_mat(St),
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

export_mat(#st{mat=Mat0}=St) ->
    Mat = [{Name,export_mat_1(M)} || {Name,M} <- gb_trees:to_list(Mat0)],
    used_materials(Mat, St).

export_mat_1(#mat{ambient=Amb,diffuse=Diff,specular=Spec,
		  shininess=Shine,opacity=Opacity,twosided=TwoSided,
		  attr=Attr}) ->
    [{ambient,Amb},{diffuse,Diff},{specular,Spec},
     {shininess,Shine},{opacity,Opacity},{twosided,TwoSided}|Attr].

used_materials(Mat0, St) ->
    Used0 = wings_util:fold_shape(
	      fun(#shape{sh=#we{fs=Ftab}}, A) ->
		      used_materials_1(Ftab, A)
	      end, gb_sets:empty(), St),
    Used = sofs:set(gb_sets:to_list(Used0)),
    Mat = sofs:relation(Mat0),
    sofs:to_external(sofs:restriction(Mat, Used)).

used_materials_1(Ftab, Acc) ->
    foldl(fun(#face{mat=[_|_]=Mat}, A) ->
		  gb_sets:union(A, gb_sets:from_list(Mat));
	     (#face{mat=Mat}, A) ->
		  gb_sets:add(Mat, A)
	  end, Acc, gb_trees:values(Ftab)).
