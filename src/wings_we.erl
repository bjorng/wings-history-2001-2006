%%
%%  wings_we.erl --
%%
%%     This module contains functions to build and manipulate
%%     we records (winged-edged records, the central data structure
%%     in Wings 3D).
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_we.erl,v 1.23 2002/01/20 11:10:25 bjorng Exp $
%%

-module(wings_we).
-export([build/2,build/3,build/4,
	 new_wrap_range/3,id/2,bump_id/1,
	 new_id/1,new_ids/2,
	 invert_normals/1,
	 merge/1,merge/2,renumber/1,renumber/2,renumber/3,
	 transform_vs/2,
	 separate/1,
	 get_sub_object/2,
	 normals/1,
	 new_items/3,
	 is_consistent/1]).

%% For reading wings files.
-export([build_edges_only/1,build_rest/5,vpairs_to_edges/2]).

-include("wings.hrl").
-import(lists, [map/2,foreach/2,foldl/3,sort/1,keysort/2,
		last/1,reverse/1,duplicate/2,seq/2,filter/2]).

%%%
%%% Build Winged-Edges.
%%%

build(Fs, Vs) ->
    build(material, Fs, Vs).

build(Type, Fs, Vs) ->
    build(Type, Fs, Vs, []).

build(Type, Fs0, Vs, HardEdges) ->
    {Good0,Bad0} = build_edges(Fs0),
    {Es0,Fs} = if
		   Bad0 =:= [] -> {Good0,Fs0};
		   true ->
		       Fs1 = fill_holes(Bad0, Fs0),
		       {Good,Bad} = build_edges(Fs1),
		       [] = Bad,
		       {Good,Fs1}
	       end,
    Es = number_edges(Es0),
    build_rest(Type, Es, Fs, Vs, HardEdges).

build_edges_only(Faces) ->
    {Good,[]} = build_edges(Faces),
    number_edges(Good).

build_rest(Type, Es, Fs, Vs, HardEdges) ->
    Htab = vpairs_to_edges(HardEdges, Es),
    {Vtab0,Etab,Ftab0} = build_tables(Es),
    Ftab = build_faces(Ftab0, Fs),
    Vtab = fill_in_vertices(Vs, Vtab0),
    NextId = 2+last(sort([gb_trees:size(Etab),
			  gb_trees:size(Ftab),
			  gb_trees:size(Vtab)])),
    #we{mode=Type,es=Etab,fs=Ftab,vs=Vtab,he=Htab,first_id=0,next_id=NextId}.

build_edges(Fs) ->
    build_edges(Fs, 0, []).

build_edges([{Material,Vs,Tx}|Fs], Face, Eacc0) ->
    build_edges_1(Vs, Tx, Fs, Face, Eacc0);
build_edges([{Material,Vs}|Fs], Face, Eacc0) ->
    build_edges_1(Vs, tx_filler(Vs), Fs, Face, Eacc0);
build_edges([Vs|Fs], Face, Eacc0) ->
    build_edges_1(Vs, tx_filler(Vs), Fs, Face, Eacc0);
build_edges([], Face, Eacc) ->
    R = sofs:relation(Eacc, [{name,{side,data}}]),
    F = sofs:relation_to_family(R),
    combine_half_edges(sofs:to_external(F)).

build_edges_1(Vs, UVs, Fs, Face, Acc0) ->
    Vuvs = zip(Vs, UVs),
    Pairs = pairs(Vuvs),
    Acc = build_face_edges(Pairs, Face, Acc0),
    build_edges(Fs, Face+1, Acc).

build_face_edges([{Pred,_}|[{E0,{UVa,UVb}},{Succ,_}|_]=Es], Face, Acc0) ->
    Acc = case E0 of
	      {Vs,Ve}=Name when Vs < Ve ->
		  enter_half_edge(right, Name, Face, Pred, Succ, UVb, Acc0);
	      {Vs,Ve} when Ve < Vs ->
		  Name = {Ve,Vs},
		  enter_half_edge(left, Name, Face, Pred, Succ, UVb, Acc0);
	      Equal=Name ->
		  enter_half_edge(same, Name, Face, Pred, Succ, UVb, Acc0)
	  end,
    build_face_edges(Es, Face, Acc);
build_face_edges([_,_], Face, Acc) -> Acc.

enter_half_edge(Side, Name, Face, Pred, Succ, UV,Tab0) ->
    Rec = {Face,UV,edge_name(Pred),edge_name(Succ)},
    [{Name,{Side,Rec}}|Tab0].

% pairs([V,V,X]) ->
%     pairs([V,X]);
% pairs([V,X,V]) ->
%     pairs([V,X]);
% pairs([X,V,V]) ->
%     pairs([V,X]);
% pairs([V1,V2]) ->
%     [{V2,V1},{V1,V2},{V2,V1},{V1,V2}];
% pairs(Vs) ->
%     pairs(Vs, Vs, []).
    
% pairs([V1|[V2|_]=Vs], First, Acc) ->
%     pairs(Vs, First, [{V2,V1}|Acc]);
% pairs([V], [V1,V2,V3|_], Acc) ->
%     [{V3,V2},{V2,V1},{V1,V}|Acc].

pairs(Vs) ->
    pairs(Vs, Vs, []).
    
pairs([{V1,T1}|[{V2,T2}|_]=Vs], First, Acc) ->
    pairs(Vs, First, [{{V2,V1},{T2,T1}}|Acc]);
pairs([{V,T}], [{V1,T1},{V2,T2},{V3,T3}|_], Acc) ->
    [{{V3,V2},{T3,T2}},{{V2,V1},{T2,T1}},{{V1,V},{T1,T}}|Acc].

zip([V|Vs], [UV|UVs]) ->
    [{V,UV}|zip(Vs, UVs)];
zip([], []) -> [].

edge_name({Vs,Ve}=Name) when Vs < Ve -> Name;
edge_name({Vs,Ve}) -> {Ve,Vs}.

tx_filler(Vs) ->
    tx_filler(Vs, wings_color:default(), []).
tx_filler([_|Vs], Col, Acc) ->
    tx_filler(Vs, Col, [Col|Acc]);
tx_filler([], Col, Acc) -> Acc.

combine_half_edges(HalfEdges) ->
    combine_half_edges(HalfEdges, [], []).
    
combine_half_edges([{Name,[{left,Ldata},{right,Rdata}]}|Hes], Good, Bad) ->
    combine_half_edges(Hes, [{Name,{Ldata,Rdata}}|Good], Bad);
combine_half_edges([{Name,Other}=BadEdge|Hes], Good0, Bad) ->
    Left = filter(fun({Side,_}) -> Side =:= left end, Other),
    Right = filter(fun({Side,_}) -> Side =:= right end, Other),
    case length(Left) =:= length(Right) of
	true ->
	    Good = combine_half_edges_1(Name, Left, Right, Good0),
	    combine_half_edges(Hes, Good, Bad);
	false -> combine_half_edges(Hes, Good0, [BadEdge|Bad])
    end;
combine_half_edges([], Good, Bad) ->
    {reverse(Good),reverse(Bad)}.

combine_half_edges_1(Name, [{left,Ldata}|Les], [{right,Rdata}|Res], Good0) ->
    Good = [{Name,{Ldata,Rdata}}|Good0],
    combine_half_edges_1(Name, Les, Res, Good);
combine_half_edges_1(Name, [], [], Good) -> Good.

number_edges(Es) ->
    number_edges(Es, 1, []).

number_edges([{Name,{Ldata,Rdata}=Data}|Es], Edge, Tab0) ->
    Tab = [{Name,{Edge,Data}}|Tab0],
    number_edges(Es, Edge+1, Tab);
number_edges([], Edge, Tab) -> reverse(Tab).

vpairs_to_edges([], Es) -> gb_sets:empty();
vpairs_to_edges(HardNames0, Es) ->
    HardNames = sofs:set([edge_name(He) || He <- HardNames0], [name]),
    SofsEdges = sofs:from_external(Es, [{name,{edge,info}}]),
    SofsHard = sofs:image(SofsEdges, HardNames),
    Htab = sofs:to_external(sofs:domain(SofsHard)),
    gb_sets:from_list(Htab).

build_tables(Edges) ->
    Emap = make_edge_map(Edges),
    build_tables(Edges, Emap, [], [], []).

build_tables([H|T], Emap, Vtab0, Etab0, Ftab0) ->
    {{Vs,Ve},{Edge,{Ldata,Rdata}}} = H,
    {Lf,LUV,Lpred,Lsucc} = Ldata,
    {Rf,RUV,Rpred,Rsucc} = Rdata,
    Erec = #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,a=LUV,b=RUV,
		 ltpr=edge_num(Lf, Lpred, Emap),
		 ltsu=edge_num(Lf, Lsucc, Emap),
		 rtpr=edge_num(Rf, Rpred, Emap),
		 rtsu=edge_num(Rf, Rsucc, Emap)},
    Etab = [{Edge,Erec}|Etab0],
    Ftab = [{Lf,Edge},{Rf,Edge}|Ftab0],
    Vtab = [{Vs,Edge},{Ve,Edge}|Vtab0],
    build_tables(T, Emap, Vtab, Etab, Ftab);
build_tables([], Etree, Vtab, Etab0, Ftab) ->
    Etab = gb_trees:from_orddict(reverse(Etab0)),
    {Vtab,Etab,Ftab}.

make_edge_map(Es) ->
    make_edge_map(Es, []).

make_edge_map([{Name,{Edge,{{Lf,_,_,_},{Rf,_,_,_}}}}|Es], Acc) ->
    make_edge_map(Es, [{{Lf,Name},Edge},{{Rf,Name},Edge}|Acc]);
make_edge_map([], Acc) -> gb_trees:from_orddict(sort(Acc)).

edge_num(Face, Name, Emap) ->
    gb_trees:get({Face,Name}, Emap).

fill_in_vertices(Ps, Vtab0) ->
    Vtab1 = sofs:relation(Vtab0),
    Vtab2 = sofs:relation_to_family(Vtab1),
    Vtab = sofs:to_external(Vtab2),
    fill_in_vertice_pos_1(Vtab, Ps, []).

fill_in_vertice_pos_1([{V,[Edge|_]}|Vs], [Pos|Ps], Vtab0) ->
    Vtab = [{V,#vtx{edge=Edge,pos=Pos}}|Vtab0],
    fill_in_vertice_pos_1(Vs, Ps, Vtab);
fill_in_vertice_pos_1([], [], Vtab) ->
    gb_trees:from_orddict(reverse(Vtab)).

build_faces(Ftab0, Fs) ->
    Ftab1 = sofs:relation(Ftab0),
    Ftab2 = sofs:relation_to_family(Ftab1),
    Ftab = sofs:to_external(Ftab2),
    build_faces(Ftab, Fs, []).

build_faces([{Face,[Edge|_]}|Fs0], [{Material,Vs,Tx}|Fs1], Acc) ->
    build_faces(Fs0, Fs1, [{Face,#face{edge=Edge,mat=Material}}|Acc]);
build_faces([{Face,[Edge|_]}|Fs0], [{Material,Vs}|Fs1], Acc) ->
    build_faces(Fs0, Fs1, [{Face,#face{edge=Edge,mat=Material}}|Acc]);
build_faces([{Face,[Edge|_]}|Fs0], [_|Fs1], Acc) ->
    build_faces(Fs0, Fs1, [{Face,#face{edge=Edge}}|Acc]);
build_faces([], [], Acc) ->
    gb_trees:from_orddict(reverse(Acc)).

fill_holes(Es, Acc) ->
    G = digraph:new(),
    make_digraph(Es, G),
    C = digraph_utils:cyclic_strong_components(G),
    Holes = make_hole_faces(G, C, Acc),
    digraph:delete(G),
    Holes.

make_hole_faces(G, [[V|_]|Cs], Acc) ->
    case digraph:get_cycle(G, V) of
	[_|Vs] when length(Vs) >= 3 ->
	    make_hole_faces(G, Cs, [{hole,Vs}|Acc]);
	Other ->
	    make_hole_faces(G, Cs, Acc)
    end;
make_hole_faces(G, [], Acc) -> Acc.
    
make_digraph([{{Va,Vb},[{right,Data}]}|Es], G) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb),
    make_digraph(Es, G);
make_digraph([{{Vb,Va},[{left,Data}]}|Es], G) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb),
    make_digraph(Es, G);
make_digraph([_|Es], G) -> make_digraph(Es, G);
make_digraph([], G) -> ok.

%%% Utilities for allocating IDs.

new_wrap_range(Items, Inc, #we{next_id=Id}=We) ->
    NumIds = Items*Inc,
    {{0,Id,Inc,NumIds},We#we{next_id=Id+NumIds}}.

id(N, {Current,BaseId,Inc,NumIds}) ->
    BaseId + ((Current+N) rem NumIds).

bump_id({Id,BaseId,Inc,NumIds}) ->
    {Id+Inc,BaseId,Inc,NumIds}.

new_id(#we{next_id=Id}=We) ->
    {Id,We#we{next_id=Id+1}}.

new_ids(N, #we{next_id=Id}=We) ->
    {Id,We#we{next_id=Id+N}}.

invert_normals(#we{es=Etab0}=We) ->
    Etab1 = [invert_dir(E) || E <- gb_trees:to_list(Etab0)],
    Etab = gb_trees:from_orddict(Etab1),
    We#we{es=Etab}.

invert_dir({N,#edge{vs=Vs,ve=Ve,ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu}=E}) ->
    {N,E#edge{vs=Ve,ve=Vs,ltpr=Ltsu,ltsu=Ltpr,rtpr=Rtsu,rtsu=Rtpr}}.

%%% Merge two winged-edge structures.
merge(We0, We1) ->
    merge([We0,We1]).

%%% Merge a list of winged-edge structures.
merge([]) -> [];
merge([We]) -> We;
merge([#we{id=Id,name=Name}|_]=Wes0) ->
    {Wes1,First,Next} = merge_renumber(Wes0),
    We = merge_1(Wes1),
    We#we{id=Id,name=Name,first_id=First,next_id=Next}.

merge_1([We]) -> We;
merge_1(Wes) ->
    merge_1(Wes, [], [], [], []).

merge_1([#we{vs=Vs,es=Es,fs=Fs,he=He}|Wes], Vt0, Et0, Ft0, Ht0) ->
    Vt = [gb_trees:to_list(Vs)|Vt0],
    Et = [gb_trees:to_list(Es)|Et0],
    Ft = [gb_trees:to_list(Fs)|Ft0],
    Ht = [gb_sets:to_list(He)|Ht0],
    merge_1(Wes, Vt, Et, Ft, Ht);
merge_1([], Vt0, Et0, Ft0, Ht0) ->
    Vt = gb_trees:from_orddict(lists:merge(Vt0)),
    Et = gb_trees:from_orddict(lists:merge(Et0)),
    Ft = gb_trees:from_orddict(lists:merge(Ft0)),
    Ht = gb_sets:from_ordset(lists:merge(Ht0)),
    #we{vs=Vt,es=Et,fs=Ft,he=Ht}.
			       
merge_renumber(Wes0) ->
    [We0|Wes1] = keysort(#we.first_id, Wes0),
    {Wes,Next} = merge_renumber(Wes1, [We0], []),
    {Wes,We0#we.first_id,Next}.

merge_renumber([#we{first_id=Low}=We|Wes], [#we{next_id=Next}|_]=Done, NotDone)
  when Low >= Next ->
    merge_renumber(Wes, [We|Done], NotDone);
merge_renumber([We|Wes], Done, NotDone) ->
    merge_renumber(Wes, Done, [We|NotDone]);
merge_renumber([], [#we{next_id=Next}|_]=Done, NotDone) ->
    merge_renumber_rest(NotDone, Next, Done).

merge_renumber_rest([We0|Wes], Next0, Acc) ->
    #we{next_id=Next} = We = renumber(We0, Next0),
    merge_renumber_rest(Wes, Next, [We|Acc]);
merge_renumber_rest([], Next, Acc) -> {Acc,Next}.

%%% Renumber a winged-edge structure.
renumber(#we{next_id=Id}=We) ->
    renumber(We, Id).

renumber(We0, Id) ->
    {We,_} = renumber(We0, Id, []),
    We.

renumber(#we{mode=Mode,vs=Vtab0,es=Etab0,fs=Ftab0,he=Htab0}=We0,
	 Id, RootSet0) ->
    Etab1 = gb_trees:to_list(Etab0),
    {Emap,IdE} = make_map(Etab1, Id),

    Vtab1 = gb_trees:to_list(Vtab0),
    {Vmap,IdV} = make_map(Vtab1, Id),
    Vtab2 = foldl(fun(V, A) ->
			  renum_vertex(V, Emap, Vmap, A)
		  end, [], Vtab1),
    Vtab = gb_trees:from_orddict(reverse(Vtab2)),

    Ftab1 = gb_trees:to_list(Ftab0),
    {Fmap,IdF} = make_map(Ftab1, Id),
    Ftab2 = foldl(fun(F, A) ->
			  renum_face(F, Emap, Fmap, A)
		  end, [], Ftab1),
    Ftab = gb_trees:from_orddict(reverse(Ftab2)),

    Etab2 = foldl(fun(E, A) ->
			  renum_edge(E, Emap, Vmap, Fmap, A)
		  end, [], Etab1),
    Etab = gb_trees:from_orddict(reverse(Etab2)),

    Htab1 = foldl(fun(E, A) ->
			  renum_hard_edge(E, Emap, A)
		  end, [], gb_sets:to_list(Htab0)),
    Htab = gb_sets:from_list(Htab1),

    RootSet = map_rootset(RootSet0, Emap, Vmap, Fmap),
    NextId = last(sort([IdV,IdE,IdF])),
    We = We0#we{mode=Mode,vs=Vtab,es=Etab,fs=Ftab,he=Htab,
		first_id=Id,next_id=NextId},
    {We,RootSet}.

map_rootset([{vertex,Vs}|T], Emap, Vmap, Fmap) when list(Vs) ->
    [map_all(vertex, Vs, Vmap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{vertex,V}|T], Emap, Vmap, Fmap) ->
    [{vertex,gb_trees:get(V, Vmap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{edge,Edges}|T], Emap, Vmap, Fmap) when list(Edges) ->
    [map_all(edge, Edges, Emap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{edge,Edge}|T], Emap, Vmap, Fmap) ->
    [{edge,gb_trees:get(Edge, Emap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{face,Faces}|T], Emap, Vmap, Fmap) when list(Faces)  ->
    [map_all(face, Faces, Fmap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{face,Face}|T], Emap, Vmap, Fmap) ->
    [{face,gb_trees:get(Face, Fmap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{body,Empty}|T], Emap, Vmap, Fmap) ->
    [{body,Empty}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([], _, _, _) -> [].

map_all(What, Items, Map) ->
    {What,[gb_trees:get(Key, Map) || Key <- Items]}.

make_map(Tab, Id0) ->
    make_map(Tab, Id0, []).
make_map([{Old,_}|T], Id, Map) ->
    make_map(T, Id+1, [{Old,Id}|Map]);
make_map([], Id, Map) ->
    {gb_trees:from_orddict(reverse(Map)),Id}.

renum_edge({Edge0,Rec0}, Emap, Vmap, Fmap, New) ->
    Edge = gb_trees:get(Edge0, Emap),
    #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,ltpr=Ltpr,ltsu=Ltsu,
	  rtpr=Rtpr,rtsu=Rtsu} = Rec0,
    Rec = Rec0#edge{vs=gb_trees:get(Vs, Vmap),ve=gb_trees:get(Ve, Vmap),
		    lf=gb_trees:get(Lf, Fmap),rf=gb_trees:get(Rf, Fmap),
		    ltpr=gb_trees:get(Ltpr, Emap),
		    ltsu=gb_trees:get(Ltsu, Emap),
		    rtpr=gb_trees:get(Rtpr, Emap),
		    rtsu=gb_trees:get(Rtsu, Emap)},
    [{Edge,Rec}|New].
    
renum_vertex({V0,#vtx{edge=Edge}=Rec0}, Emap, Vmap, New) ->
    V = gb_trees:get(V0, Vmap),
    Rec = Rec0#vtx{edge=gb_trees:get(Edge, Emap)},
    [{V,Rec}|New].

renum_face({Face0,#face{edge=Edge}=Rec0}, Emap, Fmap, New) ->
    Face = gb_trees:get(Face0, Fmap),
    Rec = Rec0#face{edge=gb_trees:get(Edge, Emap)},
    [{Face,Rec}|New].

renum_hard_edge(Edge0, Emap, New) ->
    Edge = gb_trees:get(Edge0, Emap),
    [Edge|New].

%%%
%%% Separate a combined winged-edge structure.
%%%

%% get_sub_object(Edge, We) -> We'
%%  Returns a copy of the sub-object that is reachable from Edge.

get_sub_object(Edge, #we{es=Etab0,vs=Vtab,fs=Ftab,he=Htab}=We) ->
    Ws = gb_sets:singleton(Edge),
    {EtabLeft,NewEtab} = separate(Ws, Etab0, gb_trees:empty()),
    NewWe = copy_dependents(NewEtab, We),
    NewWe#we{es=NewEtab}.

separate(We) ->
    separate(We, []).

separate(#we{es=Etab0,vs=Vtab,fs=Ftab,he=Htab}=We, Acc) ->
    case gb_trees:is_empty(Etab0) of
	true -> Acc;
	false ->
	    {Edge,Rec,_} = gb_trees:take_smallest(Etab0),
	    Ws = gb_sets:singleton(Edge),
	    {EtabLeft,NewEtab} = separate(Ws, Etab0, gb_trees:empty()),
	    NewWe = copy_dependents(NewEtab, We),
	    separate(We#we{es=EtabLeft}, [NewWe#we{es=NewEtab}|Acc])
    end.

separate(Ws0, Etab0, Acc0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {Etab0,Acc0};
	false ->
	    {Edge,Ws1} = gb_sets:take_smallest(Ws0),
	    case gb_trees:is_defined(Edge, Acc0) of
		true ->
		    separate(Ws1, Etab0, Acc0);
		false ->
		    Rec = gb_trees:get(Edge, Etab0),
		    Etab = gb_trees:delete(Edge, Etab0),
		    Acc = gb_trees:insert(Edge, Rec, Acc0),
		    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
		    Set = gb_sets:from_list([LP,LS,RP,RS]),
		    Ws = gb_sets:union(Ws1, Set),
		    separate(Ws, Etab, Acc)
	    end
    end.

copy_dependents(Es0, We) ->
    [{E,_}|_] = Es = gb_trees:to_list(Es0),
    copy_dependents(Es, We, [], [], [], E, E).

copy_dependents([], We, VsEs0, Fs, Hs, Min, Max) ->
    #we{vs=OldVtab,fs=OldFtab} = We,
    VsEs1 = sofs:relation(VsEs0, [{vertex,edge}]),
    VsEs = sofs:relation_to_family(VsEs1),
    Vtab0 = sofs:relation(gb_trees:to_list(OldVtab), [{vertex,data}]),
    Vtab1 = sofs:restriction(Vtab0, sofs:domain(VsEs)),
    Vtab = update_vtab(sofs:to_external(Vtab1), sofs:to_external(VsEs), []),
    Ftab0 = sofs:relation(gb_trees:to_list(OldFtab), [{face,data}]),
    Ftab1 = sofs:restriction(Ftab0, sofs:set(Fs, [face])),
    Ftab = gb_trees:from_orddict(sofs:to_external(Ftab1)),
    Htab = gb_sets:from_list(Hs),
    We#we{vs=Vtab,fs=Ftab,he=Htab,first_id=Min,next_id=Max+1};
copy_dependents([{Edge,Rec}|Es], We, Vs0, Fs0, Hs0, Min0, Max0) ->
    #we{he=OldHtab} = We,
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = Rec,
    Vs = [{Va,Edge},{Vb,Edge}|Vs0],
    Fs = [Lf,Rf|Fs0],
    Hs = case gb_sets:is_member(Edge, OldHtab) of
	     true -> [Edge|Hs0];
	     false -> Hs0
	 end,
    Ids = [Va,Vb,Lf,Rf,Edge],
    Min = foldl(fun(Id, Min) when Id < Min -> Id;
		   (_, Min) -> Min
		end, Min0, Ids),
    Max = foldl(fun(Id, Max) when Id > Max -> Id;
		   (_, Max) -> Max
		end, Max0, Ids),
    copy_dependents(Es, We, Vs, Fs, Hs, Min, Max).

update_vtab([{V,Vtx}|Vs], [{V,[E|_]}|VsEs], Acc) ->
    update_vtab(Vs, VsEs, [{V,Vtx#vtx{edge=E}}|Acc]);
update_vtab([], [], Acc) -> gb_trees:from_orddict(reverse(Acc)).

%%%
%%% Transform all vertices according to the matrix.
%%%

transform_vs(Matrix, #we{vs=Vtab0}=We) ->
    Vtab1 = foldl(fun({V,#vtx{pos=Pos0}=Vtx}, A) ->
			  Pos = e3d_mat:mul_point(Matrix, Pos0),
			  [{V,Vtx#vtx{pos=Pos}}|A]
		  end, [], gb_trees:to_list(Vtab0)),
    Vtab = gb_trees:from_orddict(reverse(Vtab1)),
    We#we{vs=Vtab}.

%%%
%%% Calculate normals.
%%%

normals(#we{fs=Ftab}=We) ->
    FaceNormals0 = foldl(fun({Face,FaceRec}, Acc) ->
				 [{Face,face_normal(Face, FaceRec, We)}|Acc]
			 end, [], gb_trees:to_list(Ftab)),
    FaceNormals1 = reverse(FaceNormals0),
    FaceNormals = gb_trees:from_orddict(FaceNormals1),
    G = new_digraph(We),
    VtxNormals = vertex_normals(We, G, FaceNormals),
    Ns = foldl(fun({Face,#face{mat=Mat}}, Acc) ->
		       [n_face(Face, Mat, G, FaceNormals, VtxNormals, We)|Acc]
	       end, [], gb_trees:to_list(Ftab)),
    delete_digraph(G),
    Ns.

vertex_normals(#we{vs=Vtab,es=Etab,he=Htab}=We, G, FaceNormals) ->
    {SoftVs,HardVs} =
	case gb_sets:is_empty(Htab) of
	    true -> {gb_trees:to_list(Vtab),[]};
	    false ->
		He0 = gb_sets:to_list(Htab),
		He = sofs:from_external(He0, [edge]),
		Es0 = gb_trees:to_list(Etab),
		Es1 = sofs:from_external(Es0, [{edge,data}]),
		Es = sofs:image(Es1, He),
		Hvs0 = foldl(fun(#edge{vs=Va,ve=Vb}, A) ->
				     [Va,Vb|A]
			     end, [], sofs:to_external(Es)),
		Hvs = sofs:set(Hvs0, [vertex]),
		Vs = sofs:from_external(gb_trees:to_list(Vtab),
					[{vertex,data}]),
		Svs = sofs:drestriction(Vs, Hvs),
		{sofs:to_external(Svs),sofs:to_external(Hvs)}
	end,
    foreach(fun(V) -> update_digraph(G, V, We) end, HardVs),
    Soft = foldl(
	     fun({V,#vtx{pos=Pos}}, Acc) ->
		     N = soft_vtx_normal(V, FaceNormals, We),
		     [{V,{Pos,N}}|Acc]
	     end, [], SoftVs),
    gb_trees:from_orddict(reverse(Soft)).

soft_vtx_normal(V, FaceNormals, We) ->
    Ns = wings_vertex:fold(
	   fun(Edge, Face, _, A) ->
		   [gb_trees:get(Face, FaceNormals)|A]
	   end, [], V, We),
    e3d_vec:mul(e3d_vec:add(Ns), 1/length(Ns)).

n_face(Face, Mat, G, FaceNormals, VtxNormals, #we{vs=Vtab}=We) ->
    Vs = wings_face:fold_vinfo(
	   fun (V, VInfo, Acc) ->
		   case gb_trees:lookup(V, VtxNormals) of
		       {value,{Pos,Normal}} ->
			   [{Pos,{VInfo,Normal}}|Acc];
		       none ->
			   #vtx{pos=Pos} = gb_trees:get(V, Vtab),
			   Normal = hard_vtx_normal(G, V, Face, FaceNormals),
 			   [{Pos,{VInfo,Normal}}|Acc]
		   end
	   end, [], Face, We),
    {Mat,Vs}.

hard_vtx_normal(G, V, Face, FaceNormals) ->
    Reachable = digraph_utils:reachable([{V,Face}], G),
    case [gb_trees:get(AFace, FaceNormals) || {_,AFace} <- Reachable] of
 	[N] -> N;
 	Ns -> e3d_vec:mul(e3d_vec:add(Ns), 1/length(Ns))
    end.

new_digraph(#we{he=He}) ->
    case gb_sets:is_empty(He) of
	true -> none;
	false -> digraph:new()
    end.

delete_digraph(none) -> none;
delete_digraph(G) -> digraph:delete(G).

update_digraph(G, V, #we{he=Htab}=We) ->
    wings_vertex:fold(
      fun(Edge, _, #edge{lf=Lf0,rf=Rf0}, A) ->
	      case gb_sets:is_member(Edge, Htab) of
		  true -> ok;
		  false ->
		      Lf = {V,Lf0},
		      Rf = {V,Rf0},
		      digraph:add_vertex(G, Lf),
		      digraph:add_vertex(G, Rf),
		      digraph:add_edge(G, Lf, Rf),
		      digraph:add_edge(G, Rf, Lf)
	      end
      end, [], V, We).
    
%% Extra fast normal calculation.

face_normal(Face, #face{edge=Edge}, #we{es=Etab,vs=Vtab}) ->
    face_normal_1(Face, gb_trees:get(Edge, Etab), Etab, Vtab).

face_normal_1(Face, #edge{vs=A,ve=B,lf=Face,ltpr=NextEdge}, Etab, Vtab) ->
    face_normal_2(A, B, NextEdge, Etab, Vtab);
face_normal_1(Face, #edge{ve=A,vs=B,rf=Face,rtpr=NextEdge}, Etab, Vtab) ->
    face_normal_2(A, B, NextEdge, Etab, Vtab).

face_normal_2(A, B, Edge, Etab, Vtab) ->
    C = case gb_trees:get(Edge, Etab) of
	    #edge{vs=B,ve=A0} -> A0;
	    #edge{ve=B,vs=A0} -> A0
	end,
    APos = wings_vertex:pos(A, Vtab),
    BPos = wings_vertex:pos(B, Vtab),
    CPos = wings_vertex:pos(C, Vtab),
    e3d_vec:normal(APos, BPos, CPos).

%%%
%%% Returns sets of newly created items.
%%%

%% new_items(vertex|edge|face, OldWe, NewWe) -> NewItemsGbSet.
%%  Return all items in NewWe that are not in OldWe (as a GbSet).

new_items(vertex, #we{next_id=Wid}, #we{next_id=NewWid,vs=Tab}) ->
    new_items_1(Tab, Wid, NewWid);
new_items(edge, #we{next_id=Wid}, #we{next_id=NewWid,es=Tab}) ->
    new_items_1(Tab, Wid, NewWid);
new_items(face, #we{next_id=Wid}, #we{next_id=NewWid,fs=Tab}) ->
    new_items_1(Tab, Wid, NewWid).

new_items_1(Tab, Wid, NewWid) when NewWid-Wid < 32 ->
    new_items_2(Wid, NewWid, Tab, []);
new_items_1(Tab, Wid, NewWid) ->
    Items = [Item || Item <- gb_trees:keys(Tab), Item >= Wid],
    gb_sets:from_ordset(Items).

new_items_2(Wid, NewWid, Tab, Acc) when Wid < NewWid ->
    case gb_trees:is_defined(Wid, Tab) of
	true -> new_items_2(Wid+1, NewWid, Tab, [Wid|Acc]);
	false -> new_items_2(Wid+1, NewWid, Tab, Acc)
    end;
new_items_2(Wid, NewWid, Tab, Acc) ->
    gb_sets:from_ordset(reverse(Acc)).

%%%
%%% Test if a winged-edge record is consistent.
%%%

is_consistent(#we{}=We) ->
    case catch validate_we(We) of
	{'EXIT',Reason} ->
	    io:format("~P\n", [Reason,30]),
	    false;
	true -> true
    end.

validate_we(We) ->
    validate_vertex_tab(We),
    validate_faces(We),
    true.
    
validate_faces(#we{fs=Ftab}=We) ->
    foreach(fun({Face,#face{edge=Edge}}) ->
		    Cw = walk_face_cw(Face, Edge, Edge, We, []),
		    Ccw = walk_face_ccw(Face, Edge, Edge, We, []),
 		    case reverse(Ccw) of
 			Cw -> ok;
 			Other -> exit({face_cw_ccw_inconsistency,Face})
 		    end
	    end, gb_trees:to_list(Ftab)).

walk_face_cw(Face, LastEdge, LastEdge, We, [_|_]=Acc) -> Acc;
walk_face_cw(Face, Edge, LastEdge, We, Acc) ->
    #we{es=Etab} = We,
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=Next} ->
	    walk_face_cw(Face, Next, LastEdge, We, [V|Acc]);
	#edge{ve=V,rf=Face,rtsu=Next} ->
	    walk_face_cw(Face, Next, LastEdge, We, [V|Acc])
    end.

walk_face_ccw(Face, LastEdge, LastEdge, We, [_|_]=Acc) -> Acc;
walk_face_ccw(Face, Edge, LastEdge, We, Acc) ->
    #we{es=Etab} = We,
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=Next} ->
	    walk_face_ccw(Face, Next, LastEdge, We, [V|Acc]);
	#edge{vs=V,rf=Face,rtpr=Next} ->
	    walk_face_ccw(Face, Next, LastEdge, We, [V|Acc])
    end.

validate_vertex_tab(#we{es=Etab,vs=Vtab}=We) ->
    foreach(fun({V,#vtx{edge=Edge}}) ->
		    case gb_trees:get(Edge, Etab) of
			#edge{vs=V}=Rec ->
			    validate_edge_rec(Rec);
			#edge{ve=V}=Rec ->
			    validate_edge_rec(Rec)
		    end
	    end, gb_trees:to_list(Vtab)).

validate_edge_rec(#edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS})
  when is_integer(LP+LS+RP+RS) -> ok.
