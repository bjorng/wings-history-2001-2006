%%
%%  wings_extrude_edge.erl --
%%
%%     This module contains the Extrude (edge), Bevel (face/edge) and
%%     Bump commands. (All based on edge extrusion.)
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_extrude_edge.erl,v 1.27 2002/08/23 08:13:00 bjorng Exp $
%%

-module(wings_extrude_edge).
-export([bump/1,bevel/1,bevel_faces/1,extrude/2]).

-include("wings.hrl").
-import(lists, [foldl/3,keydelete/3,member/2,sort/1,
		reverse/1,reverse/2,last/1,foreach/2]).

-define(EXTRUDE_DIST, 0.2).

%%%
%%% The Bump command.
%%%

bump(St0) ->
    {St,Tvs} = wings_sel:mapfold(fun bump/3, [], St0),
    wings_move:plus_minus(normal, Tvs, St).

bump(Faces, We0, Acc) ->
    Edges = gb_sets:from_list(wings_face:outer_edges(Faces, We0)),
    {We,_} = extrude_edges(Edges, Faces, We0),
    NewVs = gb_sets:to_list(wings_we:new_items(vertex, We0, We)),
    {We,[{Faces,NewVs,We}|Acc]}.

%%
%% The Bevel command (for edges).
%%

bevel(St0) ->
    {St,{Tvs,Sel,Limit}} =
	wings_sel:mapfold(fun bevel_edges/3, {[],[],1.0E307}, St0),
    wings_drag:setup(Tvs, [{distance,{0.0,Limit}}],
		     wings_sel:set(face, Sel, St)).

bevel_edges(Edges, #we{id=Id}=We0, {Tvs,Sel0,Limit0}) ->
    {We1,OrigVs} = extrude_edges(Edges, We0),
    We2 = wings_edge:dissolve_edges(Edges, We1),
    Tv0 = bevel_tv(OrigVs, We2),
    We3 = foldl(fun(V, W0) ->
			wings_collapse:collapse_vertex(V, W0)
		end, We2, OrigVs),
    Vtab = bevel_reset_pos(OrigVs, We2, We3#we.vs),
    We = We3#we{vs=Vtab},
    {Tv,Limit} = bevel_limit(Tv0, We, Limit0),
    Sel = [{Id,wings_we:new_items(face, We0, We)}|Sel0],
    {We,{[{Id,Tv}|Tvs],Sel,Limit}}.

%%
%% The Bevel command (for faces).
%%

bevel_faces(St0) ->
    {St,{Tvs,C}} = wings_sel:mapfold(fun bevel_faces/3, {[],1.0E300}, St0),
    wings_drag:setup(Tvs, [{distance,{0.0,C}}], St).

bevel_faces(Faces, #we{id=Id}=We0, {Tvs,Limit0}) ->
    Edges = wings_edge:from_faces(Faces, We0),
    {We1,OrigVs} = extrude_edges(Edges, We0),
    case {gb_trees:size(We0#we.es),gb_trees:size(We1#we.es)} of
	{Same,Same} ->
	    wings_util:error("Object is too small to bevel.");
	{_,_} ->
	    We2 = wings_edge:dissolve_edges(Edges, We1),
	    Tv0 = bevel_tv(OrigVs, We2),
	    #we{vs=Vtab0} = We3 =
		foldl(fun(V, W0) ->
			      wings_collapse:collapse_vertex(V, W0)
		      end, We2, OrigVs),
	    Vtab = bevel_reset_pos(OrigVs, We2, Vtab0),
	    We = We3#we{vs=Vtab},
	    {Tv,Limit} = bevel_limit(Tv0, We, Limit0),
	    {We,{[{Id,Tv}|Tvs],Limit}}
    end.

%%
%% Common bevel utilities.
%%

bevel_tv(Vs, We) ->
    foldl(fun(V, A) -> bevel_tv_1(V, We, A) end, [], Vs).

bevel_tv_1(V, We, Acc) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(_, _, Rec, Tv0) ->
	      OtherV = wings_vertex:other(V, Rec),
	      Pos = wings_vertex:pos(OtherV, We),
	      Vec = e3d_vec:norm(e3d_vec:sub(Pos, Center)),
	      [{Vec,[OtherV]}|Tv0]
      end, Acc, V, We).

bevel_reset_pos(Vs, We, Vtab) ->
    foldl(fun(V, A) -> bevel_reset_pos_1(V, We, A) end, Vtab, Vs).

bevel_reset_pos_1(V, We, Vtab) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(_, _, Rec, Vt) ->
	      OtherV = wings_vertex:other(V, Rec),
	      Vtx = gb_trees:get(OtherV, Vt),
	      gb_trees:update(OtherV, Vtx#vtx{pos=Center}, Vt)
      end, Vtab, V, We).

bevel_limit(Tv0, We, Limit0) ->
    {Tv,L0} = foldl(fun({Vec,[V]}, A) ->
			    bevel_limit_1(V, Vec, We, A)
		    end, {[],[]}, Tv0),
    L1 = sofs:relation(L0, [{edge,data}]),
    L2 = sofs:relation_to_family(L1),
    L3 = sofs:range(L2),
    L = sofs:to_external(L3),
    Limit = bevel_min_limit(L, Limit0),
    {Tv,Limit}.

bevel_limit_1(V, Vec, #we{vs=Vtab}=We, Acc) ->
    Pos = wings_vertex:pos(V, Vtab),
    L = wings_vertex:fold(
	  fun(_, _, Rec, A) ->
		  OtherV = wings_vertex:other(V, Rec),
		  OtherPos = wings_vertex:pos(OtherV, Vtab),
		  Evec = e3d_vec:norm(e3d_vec:sub(OtherPos, Pos)),
		  Dot = e3d_vec:dot(Vec, Evec),
		  [{Dot,OtherV,OtherPos}|A]
	  end, [], V, We),
    bevel_limit_2(reverse(sort(L)), V, Pos, Vec, Acc).

bevel_limit_2([{Dot,Va,Vpos}|_], V, Pos, Vec, {A,Lacc}) when Dot > 0.998 ->
    Lim = e3d_vec:len(e3d_vec:sub(Pos, Vpos)),
    Ea = edge_name(V, Va),
    {[{Vec,[V]}|A],[{Ea,{Va,Lim}}|Lacc]};
bevel_limit_2([{DotA,Va,Apos},{DotB,Vb,Bpos}|_], V, Pos, Vec0, {A,Lacc})
  when DotA+DotB > 0.998 ->
    Vec = e3d_vec:mul(Vec0, 1/DotA),
    LimA = e3d_vec:len(e3d_vec:sub(Pos, Apos)),
    LimB = e3d_vec:len(e3d_vec:sub(Pos, Bpos)),
    Ea = edge_name(V, Va),
    Eb = edge_name(V, Vb),
    {[{Vec,[V]}|A],[{Ea,{Va,LimA}},{Eb,{Vb,LimB}}|Lacc]};
bevel_limit_2(_Other, V, _Pos, Vec, {A,Lacc}) ->
    %% Ignore - degenerated case.
    {[{Vec,[V]}|A],Lacc}.

edge_name(Va, Vb) when Va < Vb -> {Va,Vb};
edge_name(Va, Vb) -> {Vb,Va}.

bevel_min_limit([[{_,L}]|T], Min) when L > Min ->
    bevel_min_limit(T, Min);
bevel_min_limit([Ls|T], Min) ->
    N = length(Ls),
    Sum = foldl(fun({_,L}, S) -> S+L end, 0, Ls),
    case Sum/N/N of
	M when M < Min -> bevel_min_limit(T, M);
	_ -> bevel_min_limit(T, Min)
    end;
bevel_min_limit([], Min) -> Min.

%%
%% The Extrude command (for edges).
%%

extrude(Type, St0) ->
    Vec = extrude_vector(Type),
    {St,Tvs} = wings_sel:mapfold(
		 fun(Edges, We, A) ->
			 extrude_1(Edges, Vec, We, A)
		 end, [], St0),
    wings_move:plus_minus(Type, Tvs, St).

extrude_vector({_,{_,_,_}=Vec}) -> Vec;
extrude_vector(Vec) -> wings_util:make_vector(Vec).

extrude_1(Edges, _Vec, We0, Acc) ->
    {We,_} = extrude_edges(Edges, We0),
    NewVs = gb_sets:to_list(wings_we:new_items(vertex, We0, We)),
    {We,[{Edges,NewVs,We}|Acc]}.

extrude_edges(Edges, We) ->
    extrude_edges(Edges, gb_sets:empty(), We).

extrude_edges(Edges, Faces, #we{next_id=Wid,es=Etab}=We0) ->
    G = digraph:new(),
    foreach(fun(Edge) ->
		    digraph_edge(G, Faces, gb_trees:get(Edge, Etab))
	    end, gb_sets:to_list(Edges)),
    Vs0 = digraph:vertices(G),
    Vs1 = sofs:relation(Vs0),
    Vs = sofs:to_external(sofs:domain(Vs1)),
    We1 = foldl(fun(V, A) ->
			new_vertex(V, G, Edges, Faces, Wid, A)
		end, We0, Vs),
    We = connect(G, Wid, We1),
    digraph:delete(G),
    {We,Vs}.

new_vertex(V, G, Edges, Faces, Wid, We0) ->
    Center = wings_vertex:pos(V, We0),
    wings_vertex:fold(
      fun(Edge, Face, Rec, W0) ->
	      case gb_sets:is_member(Edge, Edges) orelse
		  gb_sets:is_member(Face, Faces) of
		  true -> W0;
		  false ->
		      OtherV = wings_vertex:other(V, Rec),
		      MeetsNew = OtherV >= Wid,
		      do_new_vertex(V, MeetsNew, G, Edge, Faces, Center, W0)
	      end
      end, We0, V, We0).

do_new_vertex(V, MeetsNew, G, Edge, Faces, Center, #we{es=Etab}=We0) ->
    {We,NewE=NewV} = wings_edge:cut(Edge, 2, We0),
    Rec = get_edge_rec(V, NewV, Edge, NewE, We),
    digraph_edge(G, Rec),
    case {gb_trees:is_empty(Faces),MeetsNew} of
	{true,_} ->
	    %% Extrude-edge case.
	    move_vertex(NewV, Center, We);
	{false,false} ->
	    %% Bump case.
	    We;
	{false,true} ->
	    %% Bump case, meeting another new vertex.
	    OtherV = wings_vertex:other(V, gb_trees:get(Edge, Etab)),
	    Pos = wings_vertex:pos(OtherV, We),
	    Vtab0 = We#we.vs,
	    Vtx = gb_trees:get(NewV, Vtab0),
	    Vtab = gb_trees:update(NewV, Vtx#vtx{pos=Pos}, Vtab0),
	    We#we{vs=Vtab}
    end.

move_vertex(V, Center, #we{vs=Vtab0}=We) ->
    #vtx{pos=Pos0} = Rec = gb_trees:get(V, Vtab0),
    Dir = e3d_vec:sub(Pos0, Center),
    case e3d_vec:len(Dir) of
	D when D < ?EXTRUDE_DIST ->
	    We;
	_ ->
	    Pos = e3d_vec:add(Center,
			      e3d_vec:mul(e3d_vec:norm(Dir),
					  ?EXTRUDE_DIST)),
	    Vtab = gb_trees:update(V, Rec#vtx{pos=Pos}, Vtab0),
	    We#we{vs=Vtab}
    end.

get_edge_rec(Va, Vb, EdgeA, EdgeB, #we{es=Etab}) ->
    case gb_trees:get(EdgeA, Etab) of
	#edge{vs=Va,ve=Vb}=Rec -> Rec;
	#edge{vs=Vb,ve=Va}=Rec -> Rec;
	_Other -> gb_trees:get(EdgeB, Etab)
    end.

digraph_edge(G, #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb}) ->
    digraph_insert(G, Va, Vb, Lf),
    digraph_insert(G, Vb, Va, Rf).

digraph_edge(G, Faces, #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb}) ->
    case gb_sets:is_member(Lf, Faces) of
	false -> digraph_insert(G, Va, Vb, Lf);
	true -> ok
    end,
    case gb_sets:is_member(Rf, Faces) of
	false -> digraph_insert(G, Vb, Va, Rf);
	true -> ok
    end.

digraph_insert(G, Va0, Vb0, Face) ->
    Va = {Va0,Face},
    Vb = {Vb0,Face},
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb).

connect(G, Wid, We) ->
    Cs0 = digraph_utils:components(G),
    Cs = remove_winged_vs(Cs0),
    connect(G, Cs, Wid, We, []).

connect(G, [C|Cs], Wid, We0, Closed) ->
    case [VF || {V,_}=VF <- C, V >= Wid] of
	[] ->
	    [{_,Face}|_] = C,
	    connect(G, Cs, Wid, We0, [Face|Closed]);
	[Va0,Vb0] ->
	    [{Va,Face}|Path0] = digraph_get_path(G, Va0, Vb0),
	    Path = [V || {V,_} <- Path0],
	    N = wings_face:normal(Face, We0),
	    We = connect_inner(Va, Path, N, Face, We0),
 	    connect(G, Cs, Wid, We, Closed)
    end;
connect(_, [], _Wid, We0, Closed) ->
    We = wings_extrude_face:faces(Closed, We0),
    move_vertices(Closed, We).

digraph_get_path(G, Va, Vb) ->
    case digraph:get_path(G, Va, Vb) of
	false -> digraph:get_path(G, Vb, Va);
	Path -> Path
    end.

connect_inner(Current0, [A|[B,C,_|_]=Next], N, Face, We0) ->
    {We,Current} = connect_one_inner(Current0, A, B, C, N, Face, We0),
    connect_inner(Current, Next, N, Face, We);
connect_inner(Current, [_|[_,_]=Next], N, Face, We) ->
    connect_inner(Current, Next, N, Face, We);
connect_inner(Current, [_,Last], _, Face, We0) ->
    {We,_} = wings_vertex:force_connect(Last, Current, Face, We0),
    We.

connect_one_inner(Current, A, B, C, N, Face, We0) ->
    {We1,Edge} = wings_vertex:force_connect(B, Current, Face, We0),
    #we{vs=Vtab} = We1,
    Pos = new_vertex_pos(A, B, C, N, Vtab),
    wings_edge:fast_cut(Edge, Pos, We1).

move_vertices([Face|Fs], #we{vs=Vtab0}=We0) ->
    N = wings_face:normal(Face, We0),
    Vs = wings_face:surrounding_vertices(Face, We0),
    Vtab = move_vertices(Vs, Vs, N, Vtab0, Vtab0),
    We = We0#we{vs=Vtab},
    move_vertices(Fs, We);
move_vertices([], We) -> We.

move_vertices([Va|[Vb,Vc|_]=Vs], First, N, OldVtab, Vtab0) ->
    Pos = new_vertex_pos(Va, Vb, Vc, N, OldVtab),
    Vrec0 = gb_trees:get(Vb, Vtab0),
    Vrec = Vrec0#vtx{pos=wings_util:share(Pos)},
    Vtab = gb_trees:update(Vb, Vrec, Vtab0),
    move_vertices(Vs, First, N, OldVtab, Vtab);
move_vertices([Va,Vb], [Vc,Vd|_], N, OldVtab, Vtab) ->
    move_vertices([Va,Vb,Vc,Vd], [], N, OldVtab, Vtab);
move_vertices([_,_], [], _, _, Vtab) -> Vtab.

new_vertex_pos(A, B, C, N, Vtab) ->
    APos = wings_vertex:pos(A, Vtab),
    BPos = wings_vertex:pos(B, Vtab),
    CPos = wings_vertex:pos(C, Vtab),
    VecA = e3d_vec:norm(e3d_vec:sub(APos, BPos)),
    VecB = e3d_vec:norm(e3d_vec:sub(CPos, BPos)),
    Vec = e3d_vec:norm(e3d_vec:add(VecA, VecB)),
    case e3d_vec:len(e3d_vec:cross(VecA, Vec)) of
	Sin when Sin < 1.0E-3 ->
	    %% The edges have the same direction.
	    %% Simply move the vertices outwards at a right angle.
	    e3d_vec:add(BPos, e3d_vec:mul(e3d_vec:cross(VecA, N),
					  ?EXTRUDE_DIST));
	Sin ->
	    e3d_vec:add(BPos, e3d_vec:mul(Vec, ?EXTRUDE_DIST/Sin))
    end.

remove_winged_vs(Cs0) ->
    Cs = sofs:from_term(Cs0, [[{vertex,face}]]),
    P = sofs:partition(fun(C) -> sofs:domain(C) end, Cs),
    G = sofs:specification(fun(L) -> sofs:no_elements(L) =:= 1 end, P),
    sofs:to_external(sofs:union(G)).
