%%
%%  e3d__meshclean.erl --
%%
%%     Internal module for cleaning E3D meshes.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d__meshclean.erl,v 1.1 2001/09/03 11:01:39 bjorng Exp $
%%

-module(e3d__meshclean).
-export([clean/1]).
-import(lists, [reverse/1,reverse/2]).

-include("e3d.hrl").

clean(#e3d_mesh{fs=Fs0}=Mesh) ->
    InteriorBad = interior_bad_faces(Fs0),
    Fs = turn_normals(0, InteriorBad, Fs0, []),
    Mesh#e3d_mesh{fs=Fs}.

turn_normals(Face, [Face|BadFs], [#e3d_face{vs=Vs}=Rec|Fs], Acc) ->
    turn_normals(Face+1, BadFs, Fs, [Rec#e3d_face{vs=reverse(Vs)}|Acc]);
turn_normals(Face, BadFs, [Rec|Fs], Acc) ->
    turn_normals(Face+1, BadFs, Fs, [Rec|Acc]);
turn_normals(Face, [], Fs, Acc) -> reverse(Acc, Fs).
    
interior_bad_faces(Fs) ->
    Faces = number_faces(Fs),
    Edges = build_edges(Faces),
    NameEdgeFace = sofs:relation(Edges, [{name,{edge,face}}]),
    Fam = sofs:relation_to_family(NameEdgeFace),
    Bad = sofs:specification(fun({_,[{{Va,Vb},_},{{Vb,Va},_}]}) -> false;
				(_) -> true end, Fam),
    BadFaces = sofs:range(sofs:union_of_family(Bad)),
    BadNameEdgeFace = sofs:restriction(fun({_,{_,F}}) -> F end,
				       NameEdgeFace, BadFaces),
    BadNameFace = sofs:projection(fun({N,{_,F}}) -> {N,F} end,
				  BadNameEdgeFace),
    BadNameFaceFam = sofs:relation_to_family(BadNameFace),
    SuspectFaces = sofs:range(BadNameFaceFam),
    Connected0 = sofs:specification(
		   fun(Set) ->
			   sofs:no_elements(Set) =:= 2
		   end, SuspectFaces),
    Unconnected0 = sofs:difference(SuspectFaces, Connected0),
    Connected = sofs:union(Connected0),
    Unconnected = sofs:union(Unconnected0),
    Interior = sofs:difference(Connected, Unconnected),
    sofs:to_external(Interior).

build_edges(Fs) ->
    build_edges(Fs, []).

build_edges([{Face,Vs}|Fs], Acc0) ->
    Edges0 = pairs(Vs),
    Acc = [{edge_name(Edge),{Edge,Face}} || Edge <- Edges0] ++ Acc0,
    build_edges(Fs, Acc);
build_edges([], Acc) -> Acc.

pairs(Vs) ->
    pairs(Vs, Vs, []).
pairs([V2|[V1|_]=Vs], First, Acc) ->
    pairs(Vs, First, [{V1,V2}|Acc]);
pairs([V2], [V1|_], Acc) ->
    [{V1,V2}|Acc].

edge_name({Vs,Ve}=Name) when Vs < Ve -> Name;
edge_name({Vs,Ve}) -> {Ve,Vs}.

number_faces(Fs) ->
    number_faces(Fs, 0, []).
number_faces([#e3d_face{vs=Vs}|Fs], Face, Acc) ->
    number_faces(Fs, Face+1, [{Face,Vs}|Acc]);
number_faces([], Face, Acc) -> Acc.
