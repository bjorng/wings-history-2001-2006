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
%%     $Id: wings_import.erl,v 1.1 2002/06/16 18:08:23 bjorng Exp $
%%

-module(wings_import).
-export([import/2]).

-include("e3d.hrl").
-include("wings.hrl").
-import(lists, [reverse/1]).

import(#e3d_object{obj=Mesh}, UsedMat) ->
    import_1(e3d_mesh:partition(Mesh), UsedMat, []).

import_1([Mesh|T], UsedMat0, Acc) ->
    case import_mesh(Mesh, UsedMat0) of
	error -> import_1(T, UsedMat0, Acc);
	{We,UsedMat} -> import_1(T, UsedMat, [We|Acc])
    end;
import_1([], _, []) -> error;
import_1([], UsedMat, [#we{mode=Mode}|_]=Wes) ->
    We = wings_we:merge(Wes),
    {We#we{mode=Mode},UsedMat}.

import_mesh(Obj0, UsedMat0) ->	
    Obj1 = e3d_mesh:clean(Obj0),
    Obj2 = e3d_mesh:make_quads(Obj1),
    Obj = e3d_mesh:transform(Obj2),
    #e3d_mesh{vs=Vs,tx=Tx0,fs=Fs0,he=He} = Obj,
    Tx = list_to_tuple(Tx0),
    {Fs,UsedMat} = translate_faces(Fs0, Tx, [], UsedMat0),
    ObjType = obj_type(Tx0),
    case catch wings_we:build(ObjType, Fs, Vs, He) of
	{'EXIT',Reason} ->
	    io:format("Conversion failed: ~P\n", [Reason,20]),
	    error;
	We -> {We,UsedMat}
    end.
    
obj_type([]) -> material;
obj_type([_|_]) -> uv.

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
