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
%%     $Id: wings_import.erl,v 1.2 2002/06/17 07:58:12 bjorng Exp $
%%

-module(wings_import).
-export([import/2]).

-include("e3d.hrl").
-include("wings.hrl").
-import(lists, [reverse/1]).

%%-define(DUMP, 1).

import(#e3d_object{obj=Mesh}, UsedMat) ->
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
    Mesh1 = e3d_mesh:clean(Mesh0),
    Mesh2 = e3d_mesh:make_quads(Mesh1),
    Mesh = e3d_mesh:transform(Mesh2),
    #e3d_mesh{tx=Tx0,fs=Fs0} = Mesh,
    UsedMat = used_mat(Fs0, UsedMat0),
    ObjType = obj_type(Tx0),
    case catch wings_we:build(ObjType, Mesh) of
	{'EXIT',_} ->
	    io:format("Ripping apart\n"),
	    dump(Mesh),
	    {rip_apart(ObjType, Mesh),UsedMat};
	We ->
	    {We,UsedMat}
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
    Mesh = e3d_mesh:renumber(Template#e3d_mesh{fs=Fs}),
    We = wings_we:build(Mode, Mesh),
    rip_apart(T, Mode, Template, [We|Acc]);
rip_apart([], _, _, Wes) -> Wes.

-ifndef(DUMP).
dump(_) -> ok.
-else.
dump(Mesh) ->
    TempName = "bad_object.obj",
    File = #e3d_file{objs=[#e3d_object{name="bad",obj=Mesh}]},
    e3d_obj:export(TempName, File).
-endif.
