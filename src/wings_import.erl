%%
%%  wings_import.erl --
%%
%%     This module handles import of foreign objects.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_import.erl,v 1.15 2003/11/03 22:56:58 dgud Exp $
%%

-module(wings_import).
-export([import/2]).

-include("e3d.hrl").
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3,sort/1]).

%%-define(DUMP, 1).

import(File0, St) ->
    #e3d_file{objs=Objs} = distribute_materials(File0),
    translate_objects(Objs, St).

translate_objects([#e3d_object{name=Name,mat=Mat}=Obj|Os], St0) ->
    case import_object(Obj) of
        error ->
            translate_objects(Os, St0);
        We0 ->
            {St1,NameMap} = wings_material:add_materials(Mat, St0),
            We1 = rename_materials(NameMap, We0),
            We = import_attributes(We1, Obj),
            St = store_object(Name, We, St1),
            translate_objects(Os, St)
    end;
translate_objects([], St) -> St.

import_attributes(We, #e3d_object{attr=Attr}) ->
    Visible = proplists:get_value(visible, Attr, true),
    Locked = proplists:get_value(false, Attr, false),
    wings_shape:permissions(We, Visible, Locked).

store_object(undefined, We, #st{onext=Oid}=St) ->
    Name = "unnamed_object" ++ integer_to_list(Oid),
    wings_shape:new(Name, We, St);
store_object(Name, We, St) ->
    wings_shape:new(Name, We, St).

import_object(#e3d_object{obj=Mesh0}) ->
    Mesh1 = e3d_mesh:clean_faces(Mesh0),
    {Mesh,ObjType} = prepare_mesh(Mesh1),
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_R} ->
	    %% The mesh needs cleaning up. Unfortunately,
	    %% that will change the vertex numbering.
	    import_1(e3d_mesh:partition(Mesh), ObjType, []);
	We -> We
    end.

import_1([Mesh|T], ObjType, Acc) ->
    case import_mesh(Mesh, ObjType) of
	error -> import_1(T, ObjType, Acc);
	[_|_]=Wes -> import_1(T, ObjType, Wes++Acc);
	We -> import_1(T, ObjType, [We|Acc])
    end;
import_1([], _, []) -> error;
import_1([], _, [#we{mode=Mode}|_]=Wes) ->
    We = wings_we:merge(Wes),
    We#we{mode=Mode}.

prepare_mesh(Mesh0) ->
    Mesh1 = e3d_mesh:make_quads(Mesh0),
    Mesh = e3d_mesh:transform(Mesh1),
    {Mesh,material}.

import_mesh(Mesh, ObjType) ->
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_R} ->
	    build_1(ObjType, Mesh);
	We -> We
    end.

build_1(ObjType, Mesh0) ->
    %% Orient normals consistently and try to build again.
    Mesh = e3d_mesh:orient_normals(Mesh0),
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_R} ->
	    io:format("~p\n", [_R]),
	    %% Rip apart the object. It can't fail.
	    dump(Mesh),
	    rip_apart(ObjType, Mesh);
	We -> We
    end.

rip_apart(Mode, #e3d_mesh{fs=Fs}=Mesh) ->
    rip_apart(Fs, Mode, Mesh, []).

rip_apart([#e3d_face{vs=Vs,tx=Tx}=Face|T], Mode, Template, Acc) ->
    BackFace = Face#e3d_face{vs=reverse(Vs),tx=reverse(Tx),mat=['_hole_']},
    Fs = [Face,BackFace],
    Mesh = e3d_mesh:renumber(Template#e3d_mesh{fs=Fs,he=[]}),
    We = wings_we:build(Mode, Mesh),
    rip_apart(T, Mode, Template, [We|Acc]);
rip_apart([], _, _, Wes) -> Wes.

%% rename_materials(NameMap, We0) -> We
rename_materials([], We) -> We;
rename_materials([_|_]=NameMap0, We) ->
    NameMap = gb_trees:from_orddict(sort(NameMap0)),
    rename_materials(NameMap, We);
rename_materials(NameMap, We) ->
    MatTab0 = wings_material:get_all(We),
    MatTab = foldl(fun({Face,Mat0}=Pair, A) ->
			   case gb_trees:lookup(Mat0, NameMap) of
			       none -> [Pair|A];
			       {value,Mat} -> [{Face,Mat}|A]
			   end
		   end, [], MatTab0),
    wings_material:assign_materials(MatTab, We).

%% If there are materials in the #e3d_file{} record, distribute
%% them down to each object.
distribute_materials(#e3d_file{objs=Objs0,mat=Mat0}=File) ->
    Mat = sofs:relation(Mat0, [{name,data}]),
    Objs = distribute_materials_1(Objs0, Mat),
    File#e3d_file{objs=Objs}.

distribute_materials_1([#e3d_object{mat=[]}=Obj0|T], Mat) ->
    %% No material in the #e3d_object{} - use material from %e3d_file{}.
    Obj = distribute_materials_2(Obj0, Mat),
    [Obj|distribute_materials_1(T, Mat)];
distribute_materials_1([#e3d_object{mat=ObjMat0}=Obj0|T], Mat) ->
    %% Use the material from the #e3d_object{} itself.
    ObjMat = sofs:relation(ObjMat0, [{name,data}]),
    Obj = distribute_materials_2(Obj0, ObjMat),
    [Obj|distribute_materials_1(T, Mat)];
distribute_materials_1([], _) -> [].

%% distribute_materials_2(Obj, Mat)
%%  Remove any material not used; added default definitions for any
%%  material referenced but not defined.
distribute_materials_2(#e3d_object{obj=Mesh}=Obj, Mat) ->
    Used0 = e3d_mesh:used_materials(Mesh),
    Used = sofs:from_external(Used0, [name]),
    ObjMat0 = sofs:restriction(Mat, Used),
    ObjMat1 = sofs:extension(ObjMat0, Used, sofs:from_term([], data)),
    ObjMat = sofs:to_external(ObjMat1),
    Obj#e3d_object{mat=ObjMat}.

-ifndef(DUMP).
dump(_) -> ok.
-else.
dump(Mesh) ->
    TempName = "bad_object.obj",
    File = #e3d_file{objs=[#e3d_object{name="bad",obj=Mesh}]},
    e3d_obj:export(TempName, File).
-endif.
