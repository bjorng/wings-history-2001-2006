%%
%%  wings_extrude_face.erl --
%%
%%     This module contains the Extrude command for faces and face regions.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_extrude_face.erl,v 1.4 2001/09/24 07:24:53 bjorng Exp $
%%

-module(wings_extrude_face).
-export([faces/2,region/2]).

-include("wings.hrl").
-import(lists, [foldl/3,foreach/2,last/1,reverse/1,reverse/2,sort/1,merge/1]).

%%%
%%% Extrusion of faces individually (used by Extrude, Inset, Bevel).
%%%

faces(Faces, We) when list(Faces) ->
    inner_extrude(Faces, We, []);
faces(Faces, We) ->
    faces(gb_sets:to_list(Faces), We).
    
inner_extrude([Face|Faces], #we{next_id=AnEdge,fs=Ftab0}=We0, EdgeAcc0) ->
    #face{mat=Mat} = FaceRec = gb_trees:get(Face, Ftab0),
    Ftab = gb_trees:update(Face, FaceRec#face{edge=AnEdge}, Ftab0),
    We1 = We0#we{fs=Ftab},
    Edges = inner_extrude_edges(Face, We0),
    NumVs = length(Edges),
    {Ids,We2} = wings_we:new_wrap_range(NumVs, 2, We1),
    PrevEdge = last(Edges),
    {We,EdgeAcc} = inner_extrude_1(Edges, PrevEdge, Face, Mat,
				   Ids, We2, EdgeAcc0),
    inner_extrude(Faces, We, EdgeAcc);
inner_extrude([], #we{es=Etab0}=We, EdgeAcc) ->
    Etab1 = merge([sort(EdgeAcc),gb_trees:to_list(Etab0)]),
    Etab = gb_trees:from_orddict(Etab1),
    We#we{es=Etab}.
    
inner_extrude_edges(Face, We) ->
    reverse(wings_face:fold(fun(_, E, _, A) -> [E|A] end, [], Face, We)).

inner_extrude_1([Edge|Es], PrevEdge, Face, Mat, Ids0, We0, EdgeAcc0) ->
    PrevHor = wings_we:id(2-2, Ids0),
    PrevFace = PrevHor,

    HorEdge = wings_we:id(2, Ids0),
    VertEdge = HorEdge + 1,
    V = NewFace = HorEdge,

    NextHor = wings_we:id(2+2, Ids0),
    NextVert = NextHor + 1,
    NextV = NextHor,

    Ids = wings_we:bump_id(Ids0),

    #we{fs=Ftab0,es=Etab0,vs=Vtab0} = We0,

    {Va,Erec} =
	case gb_trees:get(Edge, Etab0) of
	    #edge{lf=Face,ve=V0}=Erec0 ->
		{V0,Erec0#edge{lf=NewFace,ltpr=VertEdge,ltsu=NextVert}};
	    #edge{rf=Face,vs=V0}=Erec0 ->
		{V0,Erec0#edge{rf=NewFace,rtpr=VertEdge,rtsu=NextVert}}
	end,
    Etab = gb_trees:update(Edge, Erec, Etab0),

    EdgeAcc = [{HorEdge,#edge{vs=V,ve=NextV,lf=NewFace,rf=Face,
			      ltpr=NextVert,ltsu=VertEdge,
			      rtpr=PrevHor,rtsu=NextHor}},
	       {VertEdge,#edge{vs=V,ve=Va,lf=PrevFace,rf=NewFace,
			       ltpr=PrevEdge,ltsu=PrevHor,
			       rtpr=HorEdge,rtsu=Edge}}|EdgeAcc0],

    Vrec = gb_trees:get(Va, Vtab0),
    Vtab = gb_trees:insert(V, Vrec#vtx{edge=HorEdge}, Vtab0),

    Ftab = gb_trees:insert(NewFace, #face{mat=Mat,edge=HorEdge}, Ftab0),

    We = We0#we{fs=Ftab,es=Etab,vs=Vtab},
    inner_extrude_1(Es, Edge, Face, Mat, Ids, We, EdgeAcc);
inner_extrude_1([], PrevEdge, Face, Mat, Ids, We, EdgeAcc) ->
    {We,EdgeAcc}.

%%%
%%% Extrude entire regions (does NOT work for single faces).
%%%

region(Faces, #we{es=Etab}=We0) ->
    ?ASSERT(gb_sets:size(Faces) > 1),
    Edges0 = wings_face:outer_edges(Faces, We0),
    G = digraph:new(),
    foreach(fun(Edge) ->
		    digraph_edge(G, Faces, gb_trees:get(Edge, Etab))
	    end, Edges0),
    Vs0 = digraph:vertices(G),
    Vs1 = sofs:relation(Vs0),
    Vs = sofs:to_external(sofs:domain(Vs1)),
    Edges = gb_sets:from_list(Edges0),
    We1 = foldl(fun(V, A) ->
			new_vertices(V, G, Edges, Faces, A)
		end, We0, Vs),
    We = connect(G, We1),
    digraph:delete(G),
    Sel = selection(Edges0, Faces, We0, We),
    {We,Sel}.

selection(Edges, Faces0, We0, #we{es=Etab}=We) ->
    Faces = wings_sel:validate_items(Faces0, face, We),
    All = gb_sets:union(wings_we:new_items(face, We0, We), Faces),
    OuterFaces0 = foldl(fun(E, A) ->
				#edge{lf=Lf,rf=Rf} = gb_trees:get(E, Etab),
				[Lf,Rf|A]
			end, [], Edges),
    OuterFaces = gb_sets:from_list(OuterFaces0),
    gb_sets:difference(All, OuterFaces).

new_vertices(V, G, Edges, Faces, We0) ->
    Pos = wings_vertex:pos(V, We0),
    wings_vertex:fold(
      fun(Edge, _, _, #we{es=Etab}=W0) ->
	      case gb_sets:is_member(Edge, Edges) of
		  true -> W0;
		  false ->
		      #edge{lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
		      case gb_sets:is_member(Lf, Faces) of
			  true ->
			      {We,NewV} = wings_edge:fast_cut(Edge, Pos, W0),
			      NewE = NewV,
			      Rec = get_edge_rec(V, NewV, Edge, NewE, We),
			      digraph_edge(G, Faces, Rec),
			      We;
			  false -> W0
		      end
	      end
      end, We0, V, We0).

get_edge_rec(Va, Vb, EdgeA, EdgeB, #we{es=Etab}) ->
    case gb_trees:get(EdgeA, Etab) of
	#edge{vs=Va,ve=Vb}=Rec -> Rec;
	#edge{vs=Vb,ve=Va}=Rec -> Rec;
	Other -> gb_trees:get(EdgeB, Etab)
    end.

digraph_edge(G, Faces, #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb}) ->
    case gb_sets:is_member(Lf, Faces) of
	true -> digraph_insert(G, Va, Vb, Lf);
	false -> ok
    end,
    case gb_sets:is_member(Rf, Faces) of
	true -> digraph_insert(G, Vb, Va, Rf);
	false -> ok
    end.

digraph_insert(G, Va0, Vb0, Face) ->
    Va = {Va0,Face},
    Vb = {Vb0,Face},
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb).

connect(G, We0) ->
    Cs = get_edge_chains(G),
    {We,Edges} = foldl(fun(C, {W,A}) ->
			       connect(C, W, A)
		       end, {We0,[]}, Cs),
    foldl(fun(E, W) -> wings_collapse:collapse_edge(E, W) end, We, Edges).

connect(C, We0, Acc) ->
    case C of
	[Va,_,Vb] ->
	    Face = get_face(Va, Vb, We0),
	    {We,NewEdge} = wings_vertex:force_connect(Va, Vb, Face, We0),
	    {We,[NewEdge|Acc]};
	[Va|Path] ->
	    {connect_inner(Va, Path, We0),Acc}
    end.

% get_edge_chains(G) ->
%% XXX Not yet.
%     Vs = digraph:source_vertices(G),
%     get_edge_chains(G, Vs, []).

% get_edge_chains(G, [V|Vs], Acc) ->
%     Chain = collect_chain(G, V, []),
%     get_edge_chains(G, Vs, [Chain|Acc]);
% get_edge_chains(G, [], Acc) -> Acc.

get_edge_chains(G) ->
    Vs = digraph:vertices(G),
    get_edge_chains(G, Vs, []).

get_edge_chains(G, [V|Vs], Acc) ->
    case digraph:in_degree(G, V) of
	0 ->
	    Chain = collect_chain(G, V, []),
	    get_edge_chains(G, Vs, [Chain|Acc]);
	Other -> get_edge_chains(G, Vs, Acc)
    end;
get_edge_chains(G, [], Acc) -> Acc.

collect_chain(G, {V,_}=Va, Acc) ->
    case digraph:out_neighbours(G, Va) of
	[] -> reverse(Acc, [V]);
	[Vb] -> collect_chain(G, Vb, [V|Acc])
    end.

connect_inner(Current0, [A|[B,C,_|_]=Next], We0) ->
    {We,Current} = connect_one_inner(Current0, A, B, C, We0),
    connect_inner(Current, Next, We);
connect_inner(Current, [_|[_,_]=Next], We) ->
    connect_inner(Current, Next, We);
connect_inner(Current, [_,Last], We0) ->
    Face = get_face(Current, Last, We0),
    {We,_} = wings_vertex:force_connect(Current, Last, Face, We0),
    We.

connect_one_inner(Current, A, B, C, We0) ->
    Face = get_face(Current, B, We0),
    {We1,Edge} = wings_vertex:force_connect(Current, B, Face, We0),
    Pos = wings_vertex:pos(B, We1),
    wings_edge:fast_cut(Edge, Pos, We1).

get_face(Va, Vb, We) ->
    per_face([Va,Vb], We, []).

per_face([V|Vs], We, Acc) ->
    Fs = wings_vertex:fold(
	   fun(_, Face, _, A) ->
		   [Face|A]
	   end, [], V, We),
    per_face(Vs, We, [Fs|Acc]);
per_face([], We, Acc) ->
    R = sofs:from_term(Acc, [[face]]),
    [Face] = sofs:to_external(sofs:intersection(R)),
    Face.

