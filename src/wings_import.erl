%%
%%  wings_import.erl --
%%
%%     This module handles import of foreign objects.
%%
%%  Copyright (c) 2001-2000 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_import.erl,v 1.6 2002/12/02 14:59:43 bjorng Exp $
%%

-module(wings_import).
-export([import/2,add_materials/3,rename_materials/2]).

-include("e3d.hrl").
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3,sort/1]).

%%-define(DUMP, 1).

import(#e3d_object{obj=Mesh0}, UsedMat) ->
    Mesh = e3d_mesh:clean_faces(Mesh0),
    import_1(e3d_mesh:partition(Mesh), UsedMat, []).

import_1([Mesh|T], UsedMat0, Acc) ->
    case import_mesh(Mesh, UsedMat0) of
	error ->
	    import_1(T, UsedMat0, Acc);
	{[_|_]=Wes,UsedMat} -> import_1(T, UsedMat, Wes++Acc);
	{We,UsedMat} -> import_1(T, UsedMat, [We|Acc])
    end;
import_1([], _, []) -> error;
import_1([], UsedMat, [#we{mode=Mode}|_]=Wes) ->
    We = wings_we:merge(Wes),
    {We#we{mode=Mode},UsedMat}.

import_mesh(Mesh0, UsedMat0) ->	
    Mesh1 = e3d_mesh:make_quads(Mesh0),
    Mesh = e3d_mesh:transform(Mesh1),
    #e3d_mesh{tx=Tx0,fs=Fs0} = Mesh,
    UsedMat = used_mat(Fs0, UsedMat0),
    ObjType = obj_type(Tx0),
    We = build(ObjType, Mesh),
    {We,UsedMat}.

build(ObjType, Mesh) ->
    %% First attempt to build winged-edge object.
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_R} ->
	    build_1(ObjType, Mesh);
	We -> We
    end.

build_1(ObjType, Mesh0) ->
    %% Second attempt: Orient normals consistently.
    Mesh = e3d_mesh:orient_normals(Mesh0),
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_R} ->
	    %% Rip apart the object. It can't fail.
	    dump(Mesh),
	    rip_apart(ObjType, Mesh);
	We -> We
    end.
    
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
rename_materials(NameMap, #we{fs=Ftab0}=We) ->
    Ftab1 = foldl(fun({Face,#face{mat=Mat0}=Rec}=Pair, A) ->
			  case gb_trees:lookup(Mat0, NameMap) of
			      none -> [Pair|A];
			      {value,Mat} -> [{Face,Rec#face{mat=Mat}}|A]
			  end
		  end, [], gb_trees:to_list(Ftab0)),
    Ftab = gb_trees:from_orddict(reverse(Ftab1)),
    We#we{fs=Ftab}.

-ifndef(DUMP).
dump(_) -> ok.
-else.
dump(Mesh) ->
    TempName = "bad_object.obj",
    File = #e3d_file{objs=[#e3d_object{name="bad",obj=Mesh}]},
    e3d_obj:export(TempName, File).
-endif.
