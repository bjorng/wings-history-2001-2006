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
%%     $Id: wings_extrude_face.erl,v 1.1 2001/09/14 09:58:03 bjorng Exp $
%%

-module(wings_extrude_face).
-export([faces/2,region/2]).
-include("wings.hrl").
-import(lists, [foldl/3,foreach/2,last/1,reverse/1,sort/1,merge/1]).

%%%
%%% Extrusion of faces individually (used by Extrude, Inset, Bevel).
%%%

faces(Faces, We) when list(Faces) ->
    inner_extrude(Faces, We, []);
faces(Faces, We) ->
    faces(gb_sets:to_list(Faces), We).
    
inner_extrude([Face|Faces], We0, EdgeAcc0) ->
    Edges = inner_extrude_edges(Face, We0),
    NumVs = length(Edges),
    {Ids,We1} = wings_we:new_wrap_range(NumVs, 4, We0),
    PrevEdge = last(Edges),
    #face{mat=Mat} = gb_trees:get(Face, We0#we.fs),
    {We2,EdgeAcc} = inner_extrude_1(Edges, PrevEdge, Face, Mat,
				    Ids, We1, EdgeAcc0),
    AnEdge = wings_we:id(1, Ids),
    #we{fs=Ftab0} = We2,
    Ftab = wings_face:patch_face(Face, AnEdge, Ftab0),
    We = We2#we{fs=Ftab},
    inner_extrude(Faces, We, EdgeAcc);
inner_extrude([], #we{es=Etab0}=We, EdgeAcc) ->
    Etab1 = merge([sort(EdgeAcc),gb_trees:to_list(Etab0)]),
    Etab = gb_trees:from_orddict(Etab1),
    We#we{es=Etab}.
    
inner_extrude_edges(Face, We) ->
    reverse(wings_face:fold(fun(_, E, _, A) -> [E|A] end, [], Face, We)).

inner_extrude_1([Edge|Es], PrevEdge, Face, Mat, Ids0, We0, EdgeAcc0) ->
    PrevHor = wings_we:id(5-4, Ids0),
    PrevFace = wings_we:id(7-4, Ids0),

    V = wings_we:id(4, Ids0),
    HorEdge = wings_we:id(5, Ids0),
    VertEdge = wings_we:id(6, Ids0),
    NewFace = wings_we:id(7, Ids0),

    NextV = wings_we:id(4+4, Ids0),
    NextHor = wings_we:id(5+4, Ids0),
    NextVert = wings_we:id(6+4, Ids0),

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
    Ids = wings_we:bump_id(Ids0),
    inner_extrude_1(Es, Edge, Face, Mat, Ids, We, EdgeAcc);
inner_extrude_1([], PrevEdge, Face, Mat, Ids, We, EdgeAcc) ->
    {We,EdgeAcc}.

%%%
%%% Extrude entire regions (does NOT work for single faces).
%%%

region(Faces, #we{next_id=Wid,es=Etab}=We0) ->
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
    We = connect(G, Wid, We1),
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
			      {We,NewV,NewE} =
				  wings_edge:fast_cut(Edge, Pos, W0),
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

connect(G, Wid, We0) ->
    Cs = digraph_utils:components(G),
    {We,Edges} = foldl(fun(C, {W,A}) ->
			       connect(G, C, Wid, W, A)
		       end, {We0,[]}, Cs),
    foldl(fun(E, W) -> wings_collapse:collapse_edge(E, W) end, We, Edges).

connect(G, C, Wid, We0, Acc) ->
    [Va0,Vb0] = Vs = [VF || {V,_}=VF <- C, V >= Wid],
    case digraph_get_path(G, Va0, Vb0) of
	[{Va,_},_,{Vb,_}] ->
	    Face = get_face(Va, Vb, We0),
	    {We,NewFace} = wings_vertex:force_connect(Va, Vb, Face, We0),
	    NewEdge = NewFace+1,
	    {We,[NewEdge|Acc]};
	[{Va,_}|Path0] ->
	    Path = [V || {V,_} <- Path0],
	    {connect_inner(Va, Path, We0),Acc}
    end.

digraph_get_path(G, Va, Vb) ->
    case digraph:get_path(G, Va, Vb) of
	false -> digraph:get_path(G, Vb, Va);
	Path -> Path
    end.

connect_inner(Current0, [A|[B,C,_|_]=Next], We0) ->
    {We,Current,_} = connect_one_inner(Current0, A, B, C, We0),
    connect_inner(Current, Next, We);
connect_inner(Current, [_|[_,_]=Next], We) ->
    connect_inner(Current, Next, We);
connect_inner(Current, [_,Last], We0) ->
    Face = get_face(Current, Last, We0),
    {We,_} = wings_vertex:force_connect(Current, Last, Face, We0),
    We.

connect_one_inner(Current, A, B, C, We0) ->
    Face = get_face(Current, B, We0),
    {We1,NewFace} = wings_vertex:force_connect(Current, B, Face, We0),
    Edge = NewFace + 1,
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

