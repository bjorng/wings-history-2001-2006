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
%%     $Id: wings_import.erl,v 1.11 2003/05/11 19:08:44 bjorng Exp $
%%

-module(wings_import).
-export([import/2,import_object/2,add_materials/3,rename_materials/2]).

-include("e3d.hrl").
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3,sort/1]).

%%-define(DUMP, 1).

import(#e3d_file{objs=Objs,mat=Mat}, St0) ->
    NumObjs = length(Objs),
    Suffix = " of " ++ integer_to_list(NumObjs),
    {UsedMat,St1} = translate_objects(Objs, gb_sets:empty(),
				      1, Suffix, St0),
    {St,NameMap} = add_materials(UsedMat, Mat, St1),
    rename_materials(NameMap, St0, St).

translate_objects([#e3d_object{name=Name}=Obj|Os], UsedMat0,
		  I, Suffix, St0) ->
    {St,UsedMat} = case import_object(Obj, UsedMat0) of
		       error ->
			   {St0,UsedMat0};
		       {We0,Us0} ->
			   We = import_attributes(We0, Obj),
			   {store_object(Name, We, St0),Us0}
		   end,
    translate_objects(Os, UsedMat, I+1, Suffix, St);
translate_objects([], UsedMat, _, _, St) -> {UsedMat,St}.

import_attributes(We, #e3d_object{attr=Attr}) ->
    Visible = proplists:get_value(visible, Attr, true),
    Locked = proplists:get_value(false, Attr, false),
    wings_shape:permissions(We, Visible, Locked).

store_object(undefined, We, #st{onext=Oid}=St) ->
    Name = "unnamed_object" ++ integer_to_list(Oid),
    wings_shape:new(Name, We, St);
store_object(Name, We, St) ->
    wings_shape:new(Name, We, St).

rename_materials([], _, St) -> St;
rename_materials(NameMap0, #st{onext=FirstId}, #st{shapes=Shs0}=St) ->
    NameMap = gb_trees:from_orddict(sort(NameMap0)),
    Shs = rename_mat(gb_trees:to_list(Shs0), NameMap, FirstId, []),
    St#st{shapes=Shs}.

rename_mat([{Id,_}=Obj|Objs], NameMap, FirstId, Acc) when Id < FirstId ->
    rename_mat(Objs, NameMap, FirstId, [Obj|Acc]);
rename_mat([{Id,We0}|Objs], NameMap, FirstId, Acc) ->
    We = rename_materials(NameMap, We0),
    rename_mat(Objs, NameMap, FirstId, [{Id,We}|Acc]);
rename_mat([], _, _, Acc) -> gb_trees:from_orddict(reverse(Acc)).

import_object(#e3d_object{obj=Mesh0}, UsedMat0) ->
    Mesh1 = e3d_mesh:clean_faces(Mesh0),
    {Mesh,ObjType,UsedMat} = prepare_mesh(Mesh1, UsedMat0),
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_R} ->
	    %% The mesh needs cleaning up. Unfortunately,
	    %% that will change the vertex numbering.
	    {import_1(e3d_mesh:partition(Mesh), ObjType, []),UsedMat};
	We -> {We,UsedMat}
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

prepare_mesh(Mesh0, UsedMat0) ->
    Mesh1 = e3d_mesh:make_quads(Mesh0),
    Mesh = e3d_mesh:transform(Mesh1),
    #e3d_mesh{tx=Tx0,fs=Fs0} = Mesh,
    UsedMat = used_mat(Fs0, UsedMat0),
    ObjType = obj_type(Tx0),
    {Mesh,ObjType,UsedMat}.

import_mesh(Mesh, ObjType) ->
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_R} ->
	    io:format("~p\n", [_R]),
	    build_1(ObjType, Mesh);
	We -> We
    end.

build_1(ObjType, Mesh0) ->
    %% Orient normals consistently and try to build again.
    Mesh = e3d_mesh:orient_normals(Mesh0),
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_R} ->
	    %% Rip apart the object. It can't fail.
	    dump(Mesh),
	    rip_apart(ObjType, Mesh);
	We -> We
    end.

obj_type(#e3d_mesh{tx=Tx}) -> obj_type(Tx);
obj_type([]) -> material;
obj_type([_|_]) -> uv.

used_mat([#e3d_face{mat=Mat}|Fs], UsedMat0) ->
    UsedMat = add_used_mat(Mat, UsedMat0),
    used_mat(Fs, UsedMat);
used_mat([], UsedMat) -> UsedMat.

add_used_mat([], UsedMat) -> UsedMat;
add_used_mat([M|Ms], UsedMat) -> add_used_mat(Ms, gb_sets:add(M, UsedMat)).

rip_apart(Mode, #e3d_mesh{fs=Fs}=Mesh) ->
    rip_apart(Fs, Mode, Mesh, []).

rip_apart([#e3d_face{vs=Vs,tx=Tx}=Face|T], Mode, Template, Acc) ->
    BackFace = Face#e3d_face{vs=reverse(Vs),tx=reverse(Tx),mat=['_hole_']},
    Fs = [Face,BackFace],
    Mesh = e3d_mesh:renumber(Template#e3d_mesh{fs=Fs,he=[]}),
    We = wings_we:build(Mode, Mesh),
    rip_apart(T, Mode, Template, [We|Acc]);
rip_apart([], _, _, Wes) -> Wes.

%% add_materials(UsedMat, Materials, St0) -> {St,NameMap}
add_materials(UsedMat0, Mat0, St0) ->
    UsedMat = sofs:from_external(gb_sets:to_list(UsedMat0), [name]),
    Mat1 = sofs:relation(Mat0, [{name,data}]),
    Mat2 = sofs:restriction(Mat1, UsedMat),
    Mat3 = sofs:extension(Mat2, UsedMat, sofs:from_term([], data)),
    Mat = sofs:to_external(Mat3),
    wings_material:add_materials(Mat, St0).

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

-ifndef(DUMP).
dump(_) -> ok.
-else.
dump(Mesh) ->
    TempName = "bad_object.obj",
    File = #e3d_file{objs=[#e3d_object{name="bad",obj=Mesh}]},
    e3d_obj:export(TempName, File).
-endif.
