%%
%%  e3d__meshclean.erl --
%%
%%     Internal module for cleaning E3D meshes.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d__meshclean.erl,v 1.8 2002/06/29 19:21:15 bjorng Exp $
%%

-module(e3d__meshclean).
-export([orient_normals/1,clean_faces/1]).
-import(lists, [reverse/1,reverse/2]).

-include("e3d.hrl").

%% orient_normals(Mesh0) -> Mesh
%%  Orient the face normals consistently.
orient_normals(#e3d_mesh{fs=Fs0}=Mesh) ->
    InteriorBad = interior_bad_faces(Fs0),
    Fs = turn_normals(0, InteriorBad, Fs0, []),
    Mesh#e3d_mesh{fs=Fs}.

turn_normals(Face, [Face|BadFs], [#e3d_face{vs=Vs}=Rec|Fs], Acc) ->
    turn_normals(Face+1, BadFs, Fs, [Rec#e3d_face{vs=reverse(Vs)}|Acc]);
turn_normals(Face, BadFs, [Rec|Fs], Acc) ->
    turn_normals(Face+1, BadFs, Fs, [Rec|Acc]);
turn_normals(_Face, [], Fs, Acc) -> reverse(Acc, Fs).
    
interior_bad_faces(Fs) ->
    Faces = number_faces(Fs),
    Edges = build_edges(Faces),
    NameEdgeFace = sofs:relation(Edges, [{name,{edge,face}}]),
    Fam = sofs:relation_to_family(NameEdgeFace),
    SpecFun = {external,fun([{{Va,Vb},_},{{Vb,Va},_}]) -> false;
			   (_) -> true end},
    Bad = sofs:family_specification(SpecFun, Fam),
    BadFaces = sofs:range(sofs:union_of_family(Bad)),
    RestrFun = {external,fun({_,{_,F}}) -> F end},
    BadNameEdgeFace = sofs:restriction(RestrFun, NameEdgeFace, BadFaces),

    ProjFun = {external,fun({N,{_,F}}) -> {N,F} end},
    BadNameFace = sofs:projection(ProjFun, BadNameEdgeFace),
    SuspectFaces = sofs:range(sofs:relation_to_family(BadNameFace)),

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
number_faces([], _Face, Acc) -> Acc.

%% clean_faces(Mesh0) -> Mesh
%%  Remove duplicate vertices and faces with fewer than three edges.
clean_faces(#e3d_mesh{fs=Fs0}=Mesh0) ->
    Fs = clean_faces_1(Fs0, []),
    Mesh = Mesh0#e3d_mesh{fs=Fs},
    e3d_mesh:renumber(Mesh).

clean_faces_1([#e3d_face{vs=Vs0}=Face|Fs], Acc) ->
    case clean_dup_vs(Vs0, Vs0) of
	[_,_,_|_]=Vs ->
	    case length(ordsets:from_list(Vs)) =:= length(Vs) of
		true ->
		    clean_faces_1(Fs, [Face#e3d_face{vs=Vs}|Acc]);
		false ->
		    clean_faces_1(Fs, Acc)
	    end;
	_ -> clean_faces_1(Fs, Acc)
    end;
clean_faces_1([], Acc) -> reverse(Acc).
	    
clean_dup_vs([V|[V|_]=Vs], First) ->
    clean_dup_vs(Vs, First);
clean_dup_vs([V], [V|_]) -> [];
clean_dup_vs([V|Vs], First) ->
    [V|clean_dup_vs(Vs, First)];
clean_dup_vs([], _) -> [].

    
    
