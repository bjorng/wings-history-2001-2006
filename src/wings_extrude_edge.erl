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
%%     $Id: wings_extrude_edge.erl,v 1.36 2002/12/26 09:47:08 bjorng Exp $
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
    {We,_,_,_} = extrude_edges(Edges, Faces, We0),
    NewVs = gb_sets:to_list(wings_we:new_items(vertex, We0, We)),
    {We,[{Faces,NewVs,gb_sets:empty(),We}|Acc]}.

%%
%% The Bevel command (for edges).
%%

bevel(St0) ->
    {St,{Tvs,Sel,Limit}} =
	wings_sel:mapfold(fun bevel_edges/3, {[],[],1.0E307}, St0),
    wings_drag:setup(Tvs, [{distance,{0.0,Limit}}],
		     wings_sel:set(face, Sel, St)).

bevel_edges(Edges, #we{id=Id}=We0, {Tvs,Sel0,Limit0}) ->
    {We1,OrigVs,_,Forbidden} = extrude_edges(Edges, We0),
    We2 = wings_edge:dissolve_edges(Edges, We1),
    Tv0 = bevel_tv(OrigVs, We2, Forbidden),
    We3 = foldl(fun(V, W0) ->
			WW = wings_collapse:collapse_vertex(V, W0),
			wings_util:validate(WW),
			WW
		end, We2, OrigVs),
    Vtab = bevel_reset_pos(OrigVs, We2, Forbidden, We3#we.vp),
    We = We3#we{vp=Vtab},
    {Tv,Limit} = bevel_limit(Tv0, We, Limit0),
    Sel = case gb_sets:is_empty(Forbidden) of
	      true -> [{Id,wings_we:new_items(face, We0, We)}|Sel0];
	      false -> Sel0
	  end,
    {We,{[{Id,Tv}|Tvs],Sel,Limit}}.

%%
%% The Bevel command (for faces).
%%

bevel_faces(St0) ->
    {St,{Tvs,C}} = wings_sel:mapfold(fun bevel_faces/3, {[],1.0E307}, St0),
    wings_drag:setup(Tvs, [{distance,{0.0,C}}], St).

bevel_faces(Faces, #we{id=Id}=We0, {Tvs,Limit0}) ->
    Edges = wings_edge:from_faces(Faces, We0),
    {We1,OrigVs,_,Forbidden} = extrude_edges(Edges, We0),
    case {gb_trees:size(We0#we.es),gb_trees:size(We1#we.es)} of
	{Same,Same} ->
	    wings_util:error("Object is too small to bevel.");
	{_,_} ->
	    We2 = wings_edge:dissolve_edges(Edges, We1),
	    Tv0 = bevel_tv(OrigVs, We2, Forbidden),
	    #we{vp=Vtab0} = We3 =
		foldl(fun(V, W0) ->
			      wings_collapse:collapse_vertex(V, W0)
		      end, We2, OrigVs),
	    Vtab = bevel_reset_pos(OrigVs, We2, Forbidden, Vtab0),
	    We = We3#we{vp=Vtab},
	    {Tv,Limit} = bevel_limit(Tv0, We, Limit0),
	    {We,{[{Id,Tv}|Tvs],Limit}}
    end.

%%
%% Common bevel utilities.
%%

bevel_tv(Vs, We, Forbidden) ->
    foldl(fun(V, A) -> bevel_tv_1(V, We, Forbidden, A) end, [], Vs).

bevel_tv_1(V, We, Forbidden, Acc) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Edge, _, Rec, Tv0) ->
	      case gb_sets:is_member(Edge, Forbidden) of
		  true -> Tv0;
		  false ->
		      OtherV = wings_vertex:other(V, Rec),
		      Pos = wings_vertex:pos(OtherV, We),
		      Vec = e3d_vec:norm(e3d_vec:sub(Pos, Center)),
		      [{Vec,[OtherV]}|Tv0]
	      end
      end, Acc, V, We).

bevel_reset_pos(Vs, We, Forbidden, Vtab) ->
    foldl(fun(V, A) -> bevel_reset_pos_1(V, We, Forbidden, A) end, Vtab, Vs).

bevel_reset_pos_1(V, We, Forbidden, Vtab) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Edge, _, Rec, Vt) ->
	      case gb_sets:is_member(Edge, Forbidden) of
		  true -> Vt;
		  false ->
		      OtherV = wings_vertex:other(V, Rec),
		      gb_trees:update(OtherV, Center, Vt)
	      end
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

bevel_limit_1(V, Vec, #we{vp=Vtab}=We, Acc) ->
    Pos = gb_trees:get(V, Vtab),
    L = wings_vertex:fold(
	  fun(_, _, Rec, A) ->
		  OtherV = wings_vertex:other(V, Rec),
		  OtherPos = gb_trees:get(OtherV, Vtab),
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
    {St,Tvs} = wings_sel:mapfold(
		 fun(Edges, We, A) ->
			 extrude_1(Edges, We, A)
		 end, [], St0),
    wings_move:plus_minus(Type, Tvs, St).

extrude_1(Edges, We0, Acc) ->
    {We1,_,New,Forbidden} = extrude_edges(Edges, We0),
    Ns = orig_normals(Edges, We1),
    We = straighten(Ns, New, We1),
    NewVs = gb_sets:to_list(wings_we:new_items(vertex, We0, We)),
    {We,[{Edges,NewVs,Forbidden,We}|Acc]}.

orig_normals(Es0, #we{es=Etab,vp=Vtab}) ->
    VsVec0 = foldl(fun(E, A) ->
			   #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
			   Vec0 = e3d_vec:sub(gb_trees:get(Va, Vtab),
					      gb_trees:get(Vb, Vtab)),
			   Vec = e3d_vec:norm(Vec0),
			   [{Va,{Vec,Vb}},{Vb,{Vec,Va}}|A]
		   end, [], gb_sets:to_list(Es0)),
    VsVec1 = sofs:relation(VsVec0, [{vertex,info}]),
    VsVec2 = sofs:relation_to_family(VsVec1),
    VsVec = sofs:to_external(VsVec2),
    orig_normals_1(VsVec, gb_trees:from_orddict(VsVec), []).

orig_normals_1([{V,[{VecA,_},{VecB,_}]}|T], VsVec, Acc) ->
    orig_normals_1(T, VsVec, [{V,e3d_vec:cross(VecA, VecB)}|Acc]);
orig_normals_1([{V,[{VecA,OtherV}]}|T], VsVec, Acc) ->
    OtherRec = gb_trees:get(OtherV, VsVec),
    case [Vec || {Vec,Vertex} <- OtherRec, Vertex =/= V] of
	[VecB] ->
	    orig_normals_1(T, VsVec, [{V,e3d_vec:cross(VecA, VecB)}|Acc]);
	_ ->
	    orig_normals_1(T, VsVec, Acc)
    end;
orig_normals_1([_|T], VsVec, Acc) ->
    orig_normals_1(T, VsVec, Acc);
orig_normals_1([], _, Acc) -> reverse(Acc).

straighten([{V,N0}|Ns], New, #we{vp=Vtab0}=We0) ->
    Pos = wings_vertex:pos(V, We0),
    Vtab = wings_vertex:fold(
	     fun(_, _, R, Vt0) ->
		     OtherV = wings_vertex:other(V, R),
		     case gb_sets:is_member(OtherV, New) of
			 false -> Vt0;
			 true ->
			     OPos0 = gb_trees:get(OtherV, Vt0),
			     Vec = e3d_vec:norm(e3d_vec:sub(Pos, OPos0)),
			     case e3d_vec:dot(N0, Vec) of
				 Dot when abs(Dot) < 0.87 ->
				     Vt0;
				 Dot when Dot < 0 ->
				     N = e3d_vec:neg(N0),
				     straighten_1(Vec, N, Pos, OtherV, OPos0, Vt0);
				 _ ->
				     straighten_1(Vec, N0, Pos, OtherV, OPos0, Vt0)
			     end
		     end
	     end, Vtab0, V, We0),
    We = We0#we{vp=Vtab},
    straighten(Ns, New, We);
straighten([], _, We) -> We.

straighten_1(Vec, N, {Cx,Cy,Cz}, OtherV, OPos0, Vt) ->
    case catch e3d_mat:rotate_s_to_t(Vec, N) of
	{'EXIT',_} -> Vt;
        Rot ->
	    M0 = e3d_mat:translate(Cx, Cy, Cz),
	    M1 = e3d_mat:mul(M0, Rot),
	    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
	    OPos = e3d_mat:mul_point(M, OPos0),
	    gb_trees:update(OtherV, OPos, Vt)
    end.
    
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
    {We1,Forbidden} =
	foldl(fun(V, A) ->
		      new_vertex(V, G, Edges, Faces, Wid, A)
	      end, {We0,[]}, Vs),
    NewVs = wings_we:new_items(vertex, We0, We1),
    We = connect(G, Wid, We1),
    digraph:delete(G),
    {We,Vs,NewVs,gb_sets:from_list(Forbidden)}.

new_vertex(V, G, Edges, Faces, Wid, {We0,F0}=Acc) ->
    case wings_vertex:fold(fun(E, F, R, A) -> [{E,F,R}|A] end, [], V, We0) of
	[_,_]=Es ->
	    case filter_edges(Es, Edges, Faces) of
		[] -> Acc;
		[{Edge,_,#edge{lf=Lf,rf=Rf}}] ->
		    New = {new,V},
		    digraph_insert(G, New, V, Lf),
		    digraph_insert(G, V, New, Lf),
		    digraph_insert(G, V, New, Rf),
		    digraph_insert(G, New, V, Rf),
		    {We0,[Edge|F0]}
	    end;
	Es0 ->
	    Es = filter_edges(Es0, Edges, Faces),
	    Center = wings_vertex:pos(V, We0),
	    We = foldl(fun({Edge,_,Rec}, W0) ->
			       OtherV = wings_vertex:other(V, Rec),
			       MeetsNew = OtherV >= Wid,
			       do_new_vertex(V, MeetsNew, G, Edge, Faces, Center, W0)
		       end, We0, Es),
	    {We,F0}
    end.

filter_edges(Es, EdgeSet, FaceSet) ->
    foldl(fun({Edge,Face,_}=E, A) ->
		  case gb_sets:is_member(Edge, EdgeSet) orelse
		      gb_sets:is_member(Face, FaceSet) of
		      true -> A;
		      false -> [E|A]
		  end
	  end, [], Es).

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
	    #we{vp=Vtab0} = We,
	    Pos = gb_trees:get(OtherV, Vtab0),
	    Vtab = gb_trees:update(NewV, Pos, Vtab0),
	    We#we{vp=Vtab}
    end.

move_vertex(V, Center, #we{vp=Vtab0}=We) ->
    Pos0 = gb_trees:get(V, Vtab0),
    Dir = e3d_vec:sub(Pos0, Center),
    case e3d_vec:len(Dir) of
	D when D < ?EXTRUDE_DIST ->
	    We;
	_ ->
	    Pos = e3d_vec:add(Center,
			      e3d_vec:mul(e3d_vec:norm(Dir),
					  ?EXTRUDE_DIST)),
	    Vtab = gb_trees:update(V, Pos, Vtab0),
	    We#we{vp=Vtab}
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
    Cs = digraph_utils:components(G),
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

connect_inner({new,Va}, [Va,Vb,{new,Vb}], N, Face, We0) ->
    {EdgeThrough,_,_} = wings_vertex:edge_through(Va, Vb, We0),
    {We1,TempE} = wings_edge:fast_cut(EdgeThrough, default, We0),
    {We2,Edge} = wings_vertex:force_connect(Vb, Va, Face, We1),
    #we{vp=Vtab} = We2,
    APos = gb_trees:get(Va, Vtab),
    BPos = gb_trees:get(Vb, Vtab),
    Vec = e3d_vec:sub(APos, BPos),
    Pos1 = e3d_vec:add(BPos, e3d_vec:mul(e3d_vec:cross(Vec, N), ?EXTRUDE_DIST)),
    {We3,NewE} = wings_edge:fast_cut(Edge, Pos1, We2),
    Pos2 = e3d_vec:add(APos, e3d_vec:mul(e3d_vec:cross(Vec, N), ?EXTRUDE_DIST)),
    We4 = wings_edge:dissolve_edge(TempE, We3),
    {We,_} = wings_edge:fast_cut(NewE, Pos2, We4),
    wings_util:validate(We),
    We;
connect_inner({new,V}, [V|[B,C,_|_]=Next], N, Face, We0) ->
    {We1,Current} = connect_one_inner(V, V, B, C, N, Face, We0),
    #we{vp=Vtab} = We2 = connect_inner(Current, Next, N, Face, We1),
    Edge = wings_vertex:fold(
	     fun(E, _, R, A) ->
		     case wings_vertex:other(V, R) of
			 Current -> E;
			 _ -> A
		     end
	     end, none, V, We2),
    VPos = gb_trees:get(V, Vtab),
    BPos = gb_trees:get(B, Vtab),
    Vec = e3d_vec:sub(VPos, BPos),
    Pos = e3d_vec:add(VPos, e3d_vec:mul(e3d_vec:cross(Vec, N), ?EXTRUDE_DIST)),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We2),
    We;
connect_inner({new,_}, [A|[B,C]], _, Face, We0) ->
    {We1,Edge} = wings_vertex:force_connect(C, A, Face, We0),
    #we{vp=Vtab} = We1,
    APos = gb_trees:get(A, Vtab),
    BPos = gb_trees:get(B, Vtab),
    CPos = gb_trees:get(C, Vtab),
    Pos = e3d_vec:add(APos, e3d_vec:sub(CPos, BPos)),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We1),
    We;
connect_inner(C, [B|[A,{new,_}]], N, Face, We0) ->
    {We1,Edge} = wings_vertex:force_connect(A, C, Face, We0),
    #we{vp=Vtab} = We1,
    APos = gb_trees:get(A, Vtab),
    BPos = gb_trees:get(B, Vtab),
    Vec = e3d_vec:sub(BPos, APos),
    Pos = e3d_vec:add(APos, e3d_vec:mul(e3d_vec:cross(Vec, N), ?EXTRUDE_DIST)),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We1),
    We;
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
    #we{vp=Vtab} = We1,
    Pos = new_vertex_pos(A, B, C, N, Vtab),
    wings_edge:fast_cut(Edge, Pos, We1).

move_vertices([Face|Fs], #we{vp=Vtab0}=We0) ->
    N = wings_face:normal(Face, We0),
    Vs = wings_face:surrounding_vertices(Face, We0),
    Vtab = move_vertices(Vs, Vs, N, Vtab0, Vtab0),
    We = We0#we{vp=Vtab},
    move_vertices(Fs, We);
move_vertices([], We) -> We.

move_vertices([Va|[Vb,Vc|_]=Vs], First, N, OldVtab, Vtab0) ->
    Pos = new_vertex_pos(Va, Vb, Vc, N, OldVtab),
    Vtab = gb_trees:update(Vb, wings_util:share(Pos), Vtab0),
    move_vertices(Vs, First, N, OldVtab, Vtab);
move_vertices([Va,Vb], [Vc,Vd|_], N, OldVtab, Vtab) ->
    move_vertices([Va,Vb,Vc,Vd], [], N, OldVtab, Vtab);
move_vertices([_,_], [], _, _, Vtab) -> Vtab.

new_vertex_pos(A, B, C, N, Vtab) ->
    APos = gb_trees:get(A, Vtab),
    BPos = gb_trees:get(B, Vtab),
    CPos = gb_trees:get(C, Vtab),
    VecA0 = e3d_vec:norm(e3d_vec:sub(APos, BPos)),
    VecB0 = e3d_vec:norm(e3d_vec:sub(BPos, CPos)),
    VecA = e3d_vec:norm(e3d_vec:cross(VecA0, N)),
    VecB = e3d_vec:norm(e3d_vec:cross(VecB0, N)),
    Vec = average(VecA, VecB),
    e3d_vec:add(BPos, e3d_vec:mul(Vec, ?EXTRUDE_DIST)).

average(Na, Nb) ->
    N = e3d_vec:norm(e3d_vec:add(Na, Nb)),
    case e3d_vec:dot(N, Na) of
	Dot when abs(Dot) < 1.0E-6 ->
	    e3d_vec:add(Na, Nb);
	Dot ->
	    e3d_vec:divide(N, Dot)
    end.
