%%
%%  wings_import.erl --
%%
%%     This module handles import of foreign objects.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_import.erl,v 1.19 2004/06/30 08:11:04 bjorng Exp $
%%

-module(wings_import).
-export([import/2,import_mesh/2]).

-include("e3d.hrl").
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3,sort/1]).

%%-define(DUMP, 1).

import(File0, St) ->
    wings_pb:start("importing"),
    #e3d_file{objs=Objs} = distribute_materials(File0),
    N = length(Objs),
    Faces = foldl(fun(#e3d_object{obj=#e3d_mesh{fs=Ftab}}, S) ->
			  S+length(Ftab)+1
		  end, 0, Objs),
    wings_pb:done(translate_objects(Objs, 1, N, 0, Faces, St)).

translate_objects([#e3d_object{name=Name,mat=Mat,obj=#e3d_mesh{fs=Fs}}=Obj|Os],
		  I, N, Fsum0, Faces, St0) ->
    Fsum = Fsum0 + length(Fs) + 1,
    wings_pb:update(Fsum/Faces,
		    integer_to_list(I) ++ " of " ++ integer_to_list(N)),
    case import_object(Obj) of
        error ->
            translate_objects(Os, I+1, N, Fsum, Faces, St0);
        We0 ->
            {St1,NameMap} = wings_material:add_materials(Mat, St0),
            We1 = rename_materials(NameMap, We0),
            We = import_attributes(We1, Obj),
            St = store_object(Name, We, St1),
            translate_objects(Os, I+1, N, Fsum, Faces, St)
    end;
translate_objects([], _, _, _, _, St) -> St.

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
    import_mesh(Mesh, ObjType).

import_mesh(Mesh0, ObjType) ->
    case catch wings_we:build(ObjType, Mesh0) of
	{'EXIT',_R} ->
	    %% The mesh needs cleaning up. Unfortunately,
	    %% that will change the vertex numbering.
	    wings_pb:start(""),
	    wings_pb:update(0.20, "retrying"),
	    Mesh = e3d_mesh:partition(Mesh0),
	    case length(Mesh) of
		1 ->
		    wings_pb:update(0.50, "retrying");
		N ->
		    wings_pb:update(0.50, "retrying ("++
				    integer_to_list(N)++" sub-objects)")
	    end,
	    wings_pb:done(import_1(Mesh, ObjType, []));
	We -> We
    end.

import_1([Mesh|T], ObjType, Acc) ->
    case import_mesh_1(Mesh, ObjType) of
	error -> import_1(T, ObjType, Acc);
	[_|_]=Wes -> import_1(T, ObjType, Wes++Acc);
	We -> import_1(T, ObjType, [We|Acc])
    end;
import_1([], _, []) -> error;
import_1([], _, [We]) -> We;
import_1([], _, [#we{mode=Mode}|_]=Wes) ->
    wings_pb:update(1.0, "re-combining "++
		    integer_to_list(length(Wes))++
		    " sub-objects"),
    We = wings_we:merge(Wes),
    We#we{mode=Mode}.

prepare_mesh(Mesh0) ->
    Mesh1 = e3d_mesh:make_quads(Mesh0),
    Mesh = e3d_mesh:transform(Mesh1),
    {Mesh,material}.

import_mesh_1(Mesh, ObjType) ->
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
	    io:format("~P\n", [_R,10]),
	    %% Rip apart the object. It can't fail.
	    dump(Mesh),
	    rip_apart(ObjType, Mesh);
	We -> We
    end.

rip_apart(Mode, Mesh) ->
    rip_apart_1(Mesh, Mode, []).

rip_apart_1(#e3d_mesh{fs=Fs}=Mesh0, Mode, Acc0) ->
    case length(Fs) of
	N when N > 512 ->
	    Mid = N div 2,
	    First = lists:sublist(Fs, Mid),
	    Second = lists:nthtail(Mid, Fs),
 	    Mesh1 = e3d_mesh:renumber(Mesh0#e3d_mesh{fs=First}),
 	    Mesh2 = e3d_mesh:renumber(Mesh0#e3d_mesh{fs=Second}),
	    Acc = rip_apart_1(Mesh1, Mode, Acc0),
	    rip_apart_1(Mesh2, Mode, Acc);
	_ ->
	    rip_apart_2(Fs, Mode, Mesh0, Acc0)
    end.

rip_apart_2([#e3d_face{vs=Vs,tx=Tx}=Face|T], Mode, Template, Acc) ->
    BackFace = Face#e3d_face{vs=reverse(Vs),tx=reverse(Tx),mat=['_hole_']},
    Fs = [Face,BackFace],
    Mesh = e3d_mesh:renumber(Template#e3d_mesh{fs=Fs,he=[]}),
    We = wings_we:build(Mode, Mesh),
    rip_apart_2(T, Mode, Template, [We|Acc]);
rip_apart_2([], _, _, Wes) -> Wes.

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
