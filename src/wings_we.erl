%%
%%  wings_we.erl --
%%
%%     This module contains functions to build and manipulate
%%     we records (Winged-Edged records, the central data structure
%%     in Wings 3D).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_we.erl,v 1.3 2001/08/24 08:45:48 bjorng Exp $
%%

-module(wings_we).
-export([empty/0,build/5,build/6,
	 new_wrap_range/3,id/2,bump_id/1,
	 new_id/1,new_ids/2,
	 invert_normals/1,
	 merge/2,renumber/1,renumber/2,renumber/3,
	 separate/1,
	 normals/1,
	 new_items/3]).
-include("wings.hrl").

-import(lists, [map/2,foreach/2,foldl/3,sort/1,last/1,reverse/1,merge/1]).

%%%
%%% Build Winged-Edges.
%%%

empty() ->
    Empty = gb_trees:empty(),
    #we{es=Empty,fs=Empty,vs=Empty,he=gb_sets:empty(),first_id=0,next_id=0}.

build(Name, Matrix, Fs, Vs, St) ->
    build(Name, Matrix, Fs, Vs, [], St).

build(Name, Matrix, Fs0, Vs, HardEdges, #st{onext=Id,shapes=Shapes}=St) ->
    Es0 = build_edges(Fs0),
    {Es1,Fs} = case fill_holes(Es0) of
		   [] -> {Es0,Fs0};
		   Holes ->
		       {build_edges(Fs0 ++ Holes),
			Fs0 ++ [{hole,F} || F <- Holes]}
	       end,
    Es = number_edges(Es1),
    Ftab0 = build_faces(Fs),
    {Vtab0,Etab,Ftab,Htab} = build_tables(Es, Ftab0, HardEdges),
    Vtab = fill_in_vertice_pos(0, Vs, Matrix, Vtab0),
    [{NextId0,_}|_] = reverse(merge([gb_trees:to_list(Etab),
				     gb_trees:to_list(Ftab),
				     gb_trees:to_list(Vtab)])),
    NextId = NextId0 + 1,
    Shape = #shape{id=Id,name=Name,
		   sh=#we{es=Etab,fs=Ftab,vs=Vtab,he=Htab,
			  first_id=0,next_id=NextId}},
    St#st{onext=Id+1,shapes=gb_trees:insert(Id, Shape, Shapes)}.

%% build_faces(Fs) -> FaceTab
%%  Build the original face table. The 'mat' field will be filled in.
%%  The 'edge' field will be filled in later.
build_faces(Fs) ->
    build_faces(Fs, 1, []).
build_faces([{Material,Vs}|Fs], FaceNum, Acc) ->
    build_faces(Fs, FaceNum+1, [{FaceNum,#face{mat=Material}}|Acc]);
build_faces([_|Fs], FaceNum, Acc) ->
    build_faces(Fs, FaceNum+1, [{FaceNum,#face{}}|Acc]);
build_faces([], FaceNum, Acc) ->
    gb_trees:from_orddict(sort(Acc)).

build_edges(Fs) ->
    build_edges(Fs, 1, gb_trees:empty()).
build_edges([{Material,Vs}|Fs], Face, Eacc0) ->
    build_edges_1(Vs, Fs, Face, Eacc0);
build_edges([Vs|Fs], Face, Eacc0) ->
    build_edges_1(Vs, Fs, Face, Eacc0);
build_edges([], Face, Eacc) -> Eacc.

build_edges_1(Vs, Fs, Face, Eacc0) ->
    Eacc = case try_build(Vs, Fs, Face, Eacc0) of
	       please_invert_face ->
		   case try_build(reverse(Vs), Fs, Face, Eacc0) of
		       please_invert_face ->
			   erlang:fault({already_inverted_face,[trimesh|Vs]});
		       Other -> Other
		   end;
	       Other -> Other
	   end,
    build_edges(Fs, Face+1, Eacc).

try_build([First,Second|_]=Vs, Fs, Face, Eacc0) ->
    Pairs0 = pairs(Vs, First, []),
    Pred = {Second,First},
    Pairs = [Pred|Pairs0++[hd(Pairs0)]],
    case catch build_face_edges(Pairs, Face, Eacc0) of
	{'EXIT',Reason} -> exit(Reason);
	Other -> Other
    end.

build_face_edges([Pred|[E0,Succ|_]=Es], Face, Acc0) ->
    Acc = case E0 of
	      {Vs,Ve}=Name when Vs < Ve ->
		  enter_half_edge(right, Name, Face, Pred, Succ, Acc0);
	      {Vs,Ve} when Ve < Vs ->
		  Name = {Ve,Vs},
		  enter_half_edge(left, Name, Face, Pred, Succ, Acc0)
	  end,
    build_face_edges(Es, Face, Acc);
build_face_edges([_,_], Face, Acc) -> Acc.

enter_half_edge(Side, Name, Face, Pred, Succ, Tab0) ->
    Rec = {Face,edge_name(Pred),edge_name(Succ)},
    case {Side,gb_trees:lookup(Name, Tab0)} of
	{left,{value,{none,Right}}} ->
	    gb_trees:update(Name, {Rec,Right}, Tab0);
	{right,{value,{Left,none}}} ->
	    gb_trees:update(Name, {Left,Rec}, Tab0);
	{left,none} ->
	    gb_trees:insert(Name, {Rec,none}, Tab0);
	{right,none} ->
	    gb_trees:insert(Name, {none,Rec}, Tab0);
	%% "Wrong" side.
% 	{left,{value,{Left,none}}} ->
% 	    gb_trees:update(Name, {Left,turn(Rec)}, Tab0);
% 	{right,{value,{none,Right}}} ->
% 	    gb_trees:update(Name, {turn(Rec),Right}, Tab0);
	Other ->
	    io:format("~w\n ~w\n", [Rec,Other]),
	    throw(please_invert_face)
    end.

%%turn({Face,L,R}) -> {Face,R,L}.
	    
edge_name({Vs,Ve}=Name) when Vs < Ve -> Name;
edge_name({Vs,Ve}) -> {Ve,Vs};
edge_name([Vs|Ve]) when integer(Vs), integer(Ve), Vs < Ve -> {Vs,Ve};
edge_name([Vs|Ve]) when integer(Vs), integer(Ve) -> {Ve,Vs}.

pairs([V1|[V2|_]=Vs], First, Acc) ->
    pairs(Vs, First, [{V2,V1}|Acc]);
pairs([V], First, Acc) -> [{First,V}|Acc].

number_edges(Es) ->
    number_edges(gb_trees:to_list(Es), 1, gb_trees:empty()).
number_edges([{Name,{Ldata,Rdata}}|Es], Edge, Tab0) ->
    Tab = gb_trees:insert(Name, {Edge,Ldata,Rdata}, Tab0),
    number_edges(Es, Edge+1, Tab);
number_edges([], Edge, Tab) -> Tab.

build_tables(Edges, Ftab, HardEdges0) ->
    HardEdges = gb_sets:from_list(map(fun edge_name/1, HardEdges0)),
    Empty = gb_trees:empty(),
    build_tables(gb_trees:iterator(Edges), Edges, HardEdges,
		 Empty, Empty, Ftab, gb_sets:empty()).

build_tables(Iter0, Etree, HardEdges, Vtab0, Etab0, Ftab0, Htab0) ->
    case gb_trees:next(Iter0) of
	none -> {Vtab0,Etab0,Ftab0,Htab0};
	{Name,{Edge,Ldata,Rdata},Iter} ->
	    {Vs,Ve} = Name,
	    {Lface,Lpred,Lsucc} = Ldata,
	    {Rface,Rpred,Rsucc} = Rdata,
	    Htab = case gb_sets:is_member(Name, HardEdges) of
		       true  -> gb_sets:insert(Edge, Htab0);
		       false -> Htab0
		   end,
	    Erec = #edge{vs=Vs,ve=Ve,lf=Lface,rf=Rface,
			 ltpr=edge_num(Lpred, Etree),
			 ltsu=edge_num(Lsucc, Etree),
			 rtpr=edge_num(Rpred, Etree),
			 rtsu=edge_num(Rsucc, Etree)},
	    Etab = gb_trees:insert(Edge, Erec, Etab0),
	    Ftab1 = update_face(Lface, Edge, Ftab0),
	    Ftab = update_face(Rface, Edge, Ftab1),
	    VertexRec = #vtx{edge=Edge},
	    Vtab1 = gb_trees:enter(Vs, VertexRec, Vtab0),
	    Vtab = gb_trees:enter(Ve, VertexRec, Vtab1),
	    build_tables(Iter, Etree, HardEdges, Vtab, Etab, Ftab, Htab)
    end.

update_face(Face, Edge, Ftab) ->
    Rec0 = gb_trees:get(Face, Ftab),
    Rec = Rec0#face{edge=Edge},
    gb_trees:update(Face, Rec, Ftab).

edge_num(Edge, Etree) ->
    {Num,_,_} = gb_trees:get(Edge, Etree),
    Num.

fill_in_vertice_pos(Key, Ps, Matrix0, Vtab) ->    
    Matrix = wings_mat:transpose(Matrix0),
    fill_in_vertice_pos_1(Key, Ps, Matrix, Vtab).
    
fill_in_vertice_pos_1(Key, [{X0,Y0,Z0}|Ps], Matrix, Vtab0) ->
    {X,Y,Z,_} = wings_mat:mult(Matrix, {X0,Y0,Z0,1.0}),
    VtxRec = gb_trees:get(Key, Vtab0),
    Pos = wings_util:share(float(X), float(Y), float(Z)),
    Vtab = gb_trees:update(Key, VtxRec#vtx{pos=Pos}, Vtab0),
    fill_in_vertice_pos_1(Key+1, Ps, Matrix, Vtab);
fill_in_vertice_pos_1(Key, [], Matrix, Vtab) -> Vtab.

fill_holes(Es) ->
    Uncon = get_unconnected(gb_trees:to_list(Es), []),
    fill_holes(Uncon, []).

fill_holes([E|Es0], Acc) ->
    {Edges,Es} = collect_one_face(E, Es0),
    fill_holes(Es, [Edges|Acc]);
fill_holes([], Acc) -> Acc.

get_unconnected([{Name,{none,_}}|Es], Acc) ->
    get_unconnected(Es, [{Name,left}|Acc]);
get_unconnected([{Name,{_,none}}|Es], Acc) ->
    get_unconnected(Es, [{Name,right}|Acc]);
get_unconnected([_|Es], Acc) ->
    get_unconnected(Es, Acc);
get_unconnected([], Acc) -> Acc.

collect_one_face({{Va,Vb}=Name,right}, Es) ->
    collect_one_face(Es, Va, Vb, [], [Va]);
collect_one_face({{Va,Vb}=Name,left}, Es) ->
    collect_one_face(Es, Vb, Va, [], [Vb]).

collect_one_face(Es, V, V, Rev, Acc) -> {reverse(Acc),Es++Rev};
collect_one_face([{{V,Other}=Name,left}|Es], V, First, Rev, Acc) ->
    collect_one_face(Es, Other, First, Rev, [Other|Acc]);
collect_one_face([{{Other,V}=Name,right}|Es], V, First, Rev, Acc) ->
    collect_one_face(Es, Other, First, Rev, [Other|Acc]);
collect_one_face([E|Es], V, First, Rev, Acc) ->
    collect_one_face(Es, V, First, [E|Rev], Acc);
collect_one_face([], V, First, Rev, Acc) when Rev =/= [] ->
    collect_one_face(Rev, V, First, [], Acc).

%%% Utilities for allocating IDs.

new_wrap_range(Items, Inc, We0) ->
    #we{next_id=Id}= We = debug_bump(We0),
    NumIds = Items*Inc,
    {{0,Id,Inc,NumIds},We#we{next_id=Id+NumIds}}.

id(N, {Current,BaseId,Inc,NumIds}) ->
    BaseId + ((Current+N) rem NumIds).

bump_id({Id,BaseId,Inc,NumIds}) ->
    {Id+Inc,BaseId,Inc,NumIds}.

new_id(We0) ->
    #we{next_id=Id}= We = debug_bump(We0),
    {Id,We#we{next_id=Id+1}}.

new_ids(N, We0) ->
    #we{next_id=Id}= We = debug_bump(We0),
    {Id,We#we{next_id=Id+N}}.

-ifdef(DEBUG).
debug_bump(#we{next_id=Id}=We) ->
    We#we{next_id=100 * ((Id+99) div 100)}.
-else.
debug_bump(We) -> We.
-endif.

invert_normals(#we{es=Etab0}=We) ->
    Etab1 = [invert_dir(E) || E <- gb_trees:to_list(Etab0)],
    Etab = gb_trees:from_orddict(Etab1),
    We#we{es=Etab}.

invert_dir({N,#edge{vs=Vs,ve=Ve,ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu}=E}) ->
    {N,E#edge{vs=Ve,ve=Vs,ltpr=Ltsu,ltsu=Ltpr,rtpr=Rtsu,rtsu=Rtpr}}.

%%%
%%% Merge two winged-edge structures.
%%% Renumber a winged-edge structure.
%%%

merge(#we{next_id=Ah}=WeA, #we{first_id=Bl}=WeB) when Ah =< Bl ->
    #we{vs=VtabA,es=EtabA,fs=FtabA,he=HtabA,first_id=First} = WeA,
    #we{vs=VtabB,es=EtabB,fs=FtabB,he=HtabB,next_id=Next} = WeB,
    #we{vs=merge_tab(VtabA, VtabB),
	es=merge_tab(EtabA, EtabB),
	fs=merge_tab(FtabA, FtabB),
	he=gb_sets:union(HtabA, HtabB),
	first_id=First,next_id=Next};
merge(#we{first_id=Al}=WeA, #we{next_id=Bh}=WeB) when Bh =< Al ->
    merge(WeB, WeA);
merge(#we{next_id=Low}=WeA, #we{next_id=High}=WeB0) when Low =< High ->
    WeB = renumber(WeB0, Low),
    merge(WeA, WeB);
merge(WeA, WeB) ->
    merge(WeB, WeA).

merge_tab(A, B) ->
    gb_trees:from_orddict(lists:merge([gb_trees:to_list(A),
				       gb_trees:to_list(B)])).

renumber(#we{next_id=Id}=We) ->
    renumber(We, Id).

renumber(We0, Id) ->
    {We,_} = renumber(We0, Id, []),
    We.

renumber(#we{vs=Vtab0,es=Etab0,fs=Ftab0,he=Htab0}, Id, RootSet0) ->
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
    We = #we{vs=Vtab,es=Etab,fs=Ftab,he=Htab,first_id=Id,next_id=NextId},
    {We,RootSet}.

map_rootset([{face,Face}|T], Emap, Vmap, Fmap) ->
    [{face,gb_trees:get(Face, Fmap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{vertex,V}|T], Emap, Vmap, Fmap) ->
    [{vertex,gb_trees:get(V, Vmap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{edge,Edge}|T], Emap, Vmap, Fmap) ->
    [{edge,gb_trees:get(Edge, Emap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([], _, _, _) -> [].

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
    Rec = #edge{vs=gb_trees:get(Vs, Vmap),ve=gb_trees:get(Ve, Vmap),
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

separate(We) ->
    separate(We, []).

separate(#we{es=Etab0,vs=Vtab,fs=Ftab,he=Htab}=We, Acc) ->
    case gb_trees:is_empty(Etab0) of
	true -> Acc;
	false ->
	    {Edge,Rec,_} = gb_trees:take_smallest(Etab0),
	    Ws = gb_sets:singleton(Edge),
	    {EtabLeft,NewEtab} = separate(Ws, Etab0, gb_trees:empty()),
	    Iter = gb_trees:iterator(NewEtab),
	    Empty = gb_trees:empty(),
	    EmptySet = gb_sets:empty(),
	    Huge = 1.0E200,
	    NewWe = copy_dependents(Iter, We, Empty, Empty, EmptySet, Huge, -1),
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

copy_dependents(Iter0, We, Vtab0, Ftab0, Htab0, Min0, Max0) ->
    #we{vs=OldVtab,fs=OldFtab,he=OldHtab} = We,
    case gb_trees:next(Iter0) of
	none ->
	    #we{vs=Vtab0,fs=Ftab0,he=Htab0,first_id=Min0,next_id=Max0+1};
	{Edge,Rec,Iter} ->
	    #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf} = Rec,
	    VsRec = gb_trees:get(Vs, OldVtab),
	    Vtab1 = gb_trees:enter(Vs, VsRec, Vtab0),
	    VeRec = gb_trees:get(Ve, OldVtab),
	    Vtab = gb_trees:enter(Ve, VeRec, Vtab1),
	    LfRec = gb_trees:get(Lf, OldFtab),
	    Ftab1 = gb_trees:enter(Lf, LfRec, Ftab0),
	    RfRec = gb_trees:get(Rf, OldFtab),
	    Ftab = gb_trees:enter(Rf, RfRec, Ftab1),
	    Htab = case gb_sets:is_member(Edge, OldHtab) of
		       true -> gb_sets:insert(Edge, Htab0);
		       false -> Htab0
		   end,
	    Ids = [Vs,Ve,Lf,Rf,Edge],
	    Min = foldl(fun(Id, Min) when Id < Min -> Id;
			   (_, Min) -> Min
			end, Min0, Ids),
	    Max = foldl(fun(Id, Max) when Id > Max -> Id;
			   (_, Max) -> Max
			end, Max0, Ids),
	    copy_dependents(Iter, We, Vtab, Ftab, Htab, Min, Max)
    end.

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
		He = sofs:from_external(He0, [atom]),
		Es0 = gb_trees:to_list(Etab),
		Es1 = sofs:from_external(Es0, [{atom,atom}]),
		Es = sofs:restriction(Es1, He),
		Hvs0 = foldl(fun({_,#edge{vs=Va,ve=Vb}}, A) ->
				     [Va,Vb|A]
			     end, [], sofs:to_external(Es)),
		Hvs = sofs:from_term(Hvs0),
		Vs = sofs:from_external(gb_trees:to_list(Vtab), [{atom,atom}]),
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

n_face(Face, Mat, G, FaceNormals, VtxNormals, We) ->
    Vs = wings_face:fold(
	   fun (V, _, _, Acc) ->
		   case gb_trees:lookup(V, VtxNormals) of
		       {value,PosNormal} ->
			   [PosNormal|Acc];
		       none ->
			   Pos = wings_vertex:pos(V, We),
			   Normal = hard_vtx_normal(G, V, Face, FaceNormals),
 			   [{Pos,Normal}|Acc]
		   end
	   end, [], Face, We),
    {Mat,Vs}.

soft_vtx_normal(V, FaceNormals, We) ->
    Ns = wings_vertex:fold(
	   fun(Edge, Face, _, A) ->
		   [gb_trees:get(Face, FaceNormals)|A]
	   end, [], V, We),
    e3d_vec:mul(e3d_vec:add(Ns), 1/length(Ns)).

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
new_items(vertex, #we{next_id=Wid}, #we{vs=Tab}) ->
    new_items(Tab, Wid);
new_items(edge, #we{next_id=Wid}, #we{es=Tab}) ->
    new_items(Tab, Wid);
new_items(face, #we{next_id=Wid}, #we{fs=Tab}) ->
    new_items(Tab, Wid).

new_items(Tab, Wid) ->
    Items = [Item || Item <- gb_trees:keys(Tab), Item >= Wid],
    gb_sets:from_ordset(Items).
