%%
%%  e3d__meshclean.erl --
%%
%%     Internal module for cleaning E3D meshes.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d__meshclean.erl,v 1.11 2004/06/29 08:35:12 bjorng Exp $
%%

-module(e3d__meshclean).
-export([orient_normals/1,clean_faces/1]).

-include("e3d.hrl").

-import(lists, [reverse/1,reverse/2,foldl/3]).

%% orient_normals(Mesh0) -> Mesh
%%  Orient the face normals consistently.
orient_normals(#e3d_mesh{fs=Fs0}=Mesh) ->
    Faces = number_faces(Fs0),
    Edges = build_edges(Faces),

    Ws0 = sofs:relation(Edges, [{face,{edge,side}}]),
    Ws1 = sofs:relation_to_family(Ws0),
    Ws = gb_trees:from_orddict(sofs:to_external(Ws1)),

    ProjFun = {external,fun({F,{E,S}}) -> {E,{S,F}} end},
    E2F0 = sofs:projection(ProjFun, Ws0),
    E2F1 = sofs:relation_to_family(E2F0),
    E2F = ets:new(e2f, []),

    ets:insert(E2F, sofs:to_external(E2F1)),
    Res0 = orient_1(Ws, E2F, gb_trees:empty()),
    ets:delete(E2F),

    Ftab0 = sofs:from_term(Faces, [{face,data}]),
    Res = sofs:from_term(gb_trees:to_list(Res0), [{face,ok}]),
    Ftab2 = sofs:relative_product1(Ftab0, Res),
    Ftab = foldl(fun({F,true}, A) -> [F|A];
		    ({#e3d_face{vs=Vs}=F,false}, A) ->
			 [F#e3d_face{vs=reverse(Vs)}|A]
		 end, [], sofs:to_external(Ftab2)),
    Mesh#e3d_mesh{fs=Ftab}.
    
orient_1(Ws0, E2F, Res0) ->
    case gb_trees:is_empty(Ws0) of
	true -> Res0;
	false ->
	    {Face,Es,Ws1} = gb_trees:take_smallest(Ws0),
	    Res1 = gb_trees:insert(Face, true, Res0),
	    {Ws,Res} = orient_2(gb_sets:from_list(Es), E2F, Ws1, Res1),
	    orient_1(Ws, E2F, Res)
    end.

orient_2(Es0, E2F, Ws0, Res0) ->
    case gb_sets:is_empty(Es0) of
	true -> {Ws0,Res0};
	false ->
	    {{E,Side},Es1} = gb_sets:take_smallest(Es0),
	    case ets:lookup(E2F, E) of
		[{E,[_,_]=L}] ->
		    {Es,Ws,Res} = orient_3(L, Side, Es1, Ws0, Res0),
		    orient_2(Es, E2F, Ws, Res);
		[_] ->
		    orient_2(Es1, E2F, Ws0, Res0)
	    end
    end.

orient_3([{Side,Face}|T], Side, Es0, Ws0, Res0) ->
    case gb_trees:lookup(Face, Ws0) of
	{value,FaceEs0} ->
	    Ws = gb_trees:delete(Face, Ws0),
	    Res = gb_trees:insert(Face, false, Res0),
	    FaceEs = [{E,not S} || {E,S} <- FaceEs0],
	    Es = gb_sets:union(gb_sets:from_list(FaceEs), Es0),
	    orient_3(T, Side, Es, Ws, Res);
	none ->
	    orient_3(T, Side, Es0, Ws0, Res0)
    end;
orient_3([{_,Face}|T], Side, Es0, Ws0, Res0) ->
    case gb_trees:lookup(Face, Ws0) of
	{value,FaceEs} ->
	    Ws = gb_trees:delete(Face, Ws0),
	    Res = gb_trees:insert(Face, true, Res0),
	    Es = gb_sets:union(gb_sets:from_list(FaceEs), Es0),
	    orient_3(T, Side, Es, Ws, Res);
	none ->
	    orient_3(T, Side, Es0, Ws0, Res0)
    end;
orient_3([], _, Es, Ws, Res) -> {Es,Ws,Res}.

build_edges(Fs) ->
    build_edges(Fs, []).

build_edges([{Face,#e3d_face{vs=Vs}}|Fs], Acc0) ->
    Edges = pairs(Vs),
    Acc = foldl(fun(Edge, A) ->
			Name = edge_name(Edge),
			Side = Name =:= Edge,
			[{Face,{edge_name(Edge),Side}}|A]
		end, Acc0, Edges),
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
number_faces([Rec|Fs], Face, Acc) ->
    number_faces(Fs, Face+1, [{Face,Rec}|Acc]);
number_faces([], _, Acc) -> reverse(Acc).

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

    
    
