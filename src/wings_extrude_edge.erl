%%
%%  wings_extrude_edge.erl --
%%
%%     This module contains the Extrude (edge), Bevel (face/edge) and
%%     Bump commands. (All based on edge extrusion.)
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_extrude_edge.erl,v 1.17 2002/01/19 07:53:49 bjorng Exp $
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
    wings_drag:init_drag(Tvs, {radius,none}, distance, St#st{inf_r=1.0}).

bump(Faces, #we{id=Id}=We0, Acc) ->
    Edges = gb_sets:from_list(wings_face:outer_edges(Faces, We0)),
    {We,_} = extrude_edges(Edges, Faces, We0),
    NewVs = gb_sets:to_list(wings_we:new_items(vertex, We0, We)),
    Tv = wings_move:setup_we(face, normal, Faces, We),
    plus_minus_move(Tv, NewVs, We, Acc).

%%
%% The Bevel command (for edges).
%%

bevel(St0) ->
    {St,{Tvs,Sel0,Limit}} =
	wings_sel:mapfold(fun bevel_edges/3, {[],[],1.0E300}, St0),
    Sel = reverse(Sel0),
    wings_drag:init_drag(Tvs, {0.0,Limit}, St#st{selmode=face,sel=Sel}).

bevel_edges(Edges, #we{id=Id,es=Etab,next_id=Next}=We0, {Tvs,Ss,Limit0}) ->
    {We1,OrigVs} = extrude_edges(Edges, We0),
    OrigFaces = bevel_orig_faces(Edges, We1),
    We2 = wings_edge:dissolve_edges(Edges, We1),
    Tv0 = bevel_tv(OrigVs, We2),
    #we{fs=Ftab,vs=Vtab0} = We3 =
	foldl(fun(V, W0) ->
		      wings_collapse:collapse_vertex(V, W0)
	      end, We2, OrigVs),
    Vtab = bevel_reset_pos(OrigVs, We2, Vtab0),
    We = We3#we{vs=Vtab},
    FaceSel0 = [Face || Face <- OrigFaces, gb_trees:is_defined(Face, Ftab)],
    FaceSel = gb_sets:from_ordset(FaceSel0),
    {Tv,Limit} = bevel_limit(Tv0, We, Limit0),
    {We,{[{Id,Tv}|Tvs],[{Id,FaceSel}|Ss],Limit}}.

%%
%% The Bevel command (for faces).
%%

bevel_faces(St0) ->
    {St,{Tvs,C}} = wings_sel:mapfold(fun bevel_faces/3, {[],1.0E300}, St0),
    wings_drag:init_drag(Tvs, {0.0,C}, St).

bevel_faces(Faces, #we{id=Id,es=Etab,next_id=Next}=We0, {Tvs,Limit0}) ->
    Edges = wings_edge:from_faces(Faces, We0),
    {We1,OrigVs} = extrude_edges(Edges, We0),
    We2 = wings_edge:dissolve_edges(Edges, We1),
    Tv0 = bevel_tv(OrigVs, We2),
    #we{fs=Ftab,vs=Vtab0} = We3 =
	foldl(fun(V, W0) ->
		      wings_collapse:collapse_vertex(V, W0)
	      end, We2, OrigVs),
    Vtab = bevel_reset_pos(OrigVs, We2, Vtab0),
    We = We3#we{vs=Vtab},
    {Tv,Limit} = bevel_limit(Tv0, We, Limit0),
    {We,{[{Id,Tv}|Tvs],Limit}}.

%%
%% Common bevel utilities.
%%

bevel_tv(Vs, We) ->
    foldl(fun(V, A) -> bevel_tv_1(V, We, A) end, [], Vs).

bevel_tv_1(V, We, Acc) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Face, Edge, Rec, Tv0) ->
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
      fun(Face, Edge, Rec, Vt) ->
	      OtherV = wings_vertex:other(V, Rec),
	      Vtx = gb_trees:get(OtherV, Vt),
	      gb_trees:update(OtherV, Vtx#vtx{pos=Center}, Vt)
      end, Vtab, V, We).

bevel_orig_faces(Edges, #we{es=Etab}=We0) ->
    Faces = foldl(fun(E, A) ->
			  #edge{lf=Lf,rf=Rf} = gb_trees:get(E, Etab),
			  [Lf,Rf|A]
		  end, [], gb_sets:to_list(Edges)),
    ordsets:from_list(Faces).

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
    bevel_limit_2(reverse(sort(L)), V, Pos, Vec, We, Acc).

bevel_limit_2([{Dot,Va,Vpos}|_], V, Pos, Vec, We, {A,Lacc}) when Dot > 0.998 ->
    Lim = e3d_vec:len(e3d_vec:sub(Pos, Vpos)),
    Ea = edge_name(V, Va),
    {[{Vec,[V]}|A],[{Ea,{Va,Lim}}|Lacc]};
bevel_limit_2([{DotA,Va,Apos},{DotB,Vb,Bpos}|_], V, Pos, Vec0, We, {A,Lacc})
  when DotA+DotB > 0.998 ->
    Vec = e3d_vec:mul(Vec0, 1/DotA),
    LimA = e3d_vec:len(e3d_vec:sub(Pos, Apos)),
    LimB = e3d_vec:len(e3d_vec:sub(Pos, Bpos)),
    Ea = edge_name(V, Va),
    Eb = edge_name(V, Vb),
    {[{Vec,[V]}|A],[{Ea,{Va,LimA}},{Eb,{Vb,LimB}}|Lacc]};
bevel_limit_2(Other, V, Pos, Vec, We, {A,Lacc}) ->
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
	M -> bevel_min_limit(T, Min)
    end;
bevel_min_limit([], Min) -> Min.

%%
%% The Extrude command (for edges).
%%

extrude(Type, St0) ->
    Vec = wings_util:make_vector(Type),
    {St,Tvs} = wings_sel:mapfold(
		 fun(Edges, We, A) ->
			 extrude_1(Edges, Vec, We, A)
		 end, [], St0),
    Constraint = case Type of
		     free -> view_dependent;
		     Other -> none
		 end,
    wings_drag:init_drag(Tvs, {radius,Constraint},
			 distance, St#st{inf_r=1.0}).

extrude_1(Edges, Vec, We0, Acc) ->
    {We,_} = extrude_edges(Edges, We0),
    NewVs = gb_sets:to_list(wings_we:new_items(vertex, We0, We)),
    Tv = wings_move:setup_we(edge, Vec, Edges, We),
    plus_minus_move(Tv, NewVs, We, Acc).

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
    We2 = foldl(fun(V, A) ->
			new_vertices(V, G, Edges, Faces, A)
		end, We0, Vs),
    We = connect(G, Wid, We2),
    digraph:delete(G),
    {We,Vs}.

new_vertices(V, G, Edges, Faces, We0) ->
    Center = wings_vertex:pos(V, We0),
    wings_vertex:fold(
      fun(Edge, Face, _, #we{es=Etab}=W0=A) ->
	      case gb_sets:is_member(Edge, Edges) orelse
		  gb_sets:is_member(Face, Faces) of
		  true -> A;
		  false ->
		      {W1,NewV} = wings_edge:cut(Edge, 2, W0),
		      NewE = NewV,
		      Rec = get_edge_rec(V, NewV, Edge, NewE, W1),
		      digraph_edge(G, Rec),
		      case gb_trees:is_empty(Faces) of
			  true -> move_vertex(NewV, Center, W1);
			  false -> W1
		      end
	      end
      end, We0, V, We0).

move_vertex(V, Center, #we{vs=Vtab0}=We) ->
    #vtx{pos=Pos0} = Rec = gb_trees:get(V, Vtab0),
    Dir = e3d_vec:sub(Pos0, Center),
    case e3d_vec:len(Dir) of
	D when D < ?EXTRUDE_DIST ->
	    We;
	D ->
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
	Other -> gb_trees:get(EdgeB, Etab)
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
	[Va0,Vb0]=Vs ->
	    [{Va,Face}|Path0] = digraph_get_path(G, Va0, Vb0),
	    Path = [V || {V,_} <- Path0],
	    N = wings_face:normal(Face, We0),
	    We = connect_inner(Va, Path, N, Face, We0),
	    connect(G, Cs, Wid, We, Closed)
    end;
connect(G, [], Wid, We0, Closed) ->
    We = wings_extrude_face:faces(Closed, We0),
    move_vertices(Closed, We).

digraph_get_path(G, Va, Vb) ->
    case digraph:get_path(G, Va, Vb) of
	false -> digraph:get_path(G, Vb, Va);
	Path -> Path
    end.

connect_inner(Current0, [A|[B,C,_|_]=Next], N, DefFace, We0) ->
    {We,Current} = connect_one_inner(Current0, A, B, C, N, DefFace, We0),
    connect_inner(Current, Next, N, DefFace, We);
connect_inner(Current, [_|[_,_]=Next], N, DefFace, We) ->
    connect_inner(Current, Next, N, DefFace, We);
connect_inner(Current, [_,Last], N, DefFace, We0) ->
    Face = get_face(Current, Last, DefFace, We0),
    {We,_} = wings_vertex:force_connect(Current, Last, Face, We0),
    We.

connect_one_inner(Current, A, B, C, N, DefFace, We0) ->
    Face = get_face(Current, B, DefFace, We0),
    {We1,Edge} = wings_vertex:force_connect(Current, B, Face, We0),
    #we{vs=Vtab} = We1,
    Pos = new_vertex_pos(A, B, C, N, Vtab),
    wings_edge:fast_cut(Edge, Pos, We1).

get_face(Va, Vb, DefFace, We) ->
    FaceVs = wings_vertex:per_face([Va,Vb], We),
    case [Face || {Face,[_,_]} <- FaceVs] of
	[Face] -> Face;
	[_,_|_]=Fs ->
	    [Face] = [Face || Face <- Fs, Face =:= DefFace],
	    Face
    end.

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
move_vertices([_,_], [], N, OldVtab, Vtab) -> Vtab.

new_vertex_pos(A, B, C, N, Vtab) ->
    APos = wings_vertex:pos(A, Vtab),
    BPos = wings_vertex:pos(B, Vtab),
    CPos = wings_vertex:pos(C, Vtab),
    VecA = e3d_vec:norm(e3d_vec:sub(APos, BPos)),
    VecB = e3d_vec:norm(e3d_vec:sub(CPos, BPos)),
    Vec = e3d_vec:norm(e3d_vec:add(VecA, VecB)),
    case e3d_vec:len(Vec) of
	Short when Short < 1.0E-6 ->
	    %% A "winged vertex" - the edges have the same direction.
	    e3d_vec:add(BPos, e3d_vec:mul(e3d_vec:cross(VecA, N),
					  ?EXTRUDE_DIST));
	Other ->
	    Sin = e3d_vec:len(e3d_vec:cross(VecA, Vec)),
	    e3d_vec:add(BPos, e3d_vec:mul(Vec, ?EXTRUDE_DIST/Sin))
    end.

remove_winged_vs(Cs0) ->
    Cs = sofs:from_term(Cs0, [[{vertex,face}]]),
    P = sofs:partition(fun(C) -> sofs:domain(C) end, Cs),
    G = sofs:specification(fun(L) -> sofs:no_elements(L) =:= 1 end, P),
    sofs:to_external(sofs:union(G)).

%%
%% Move handling for Extrude/Bump.
%%

plus_minus_move([_|_]=Tv0, NewVs, #we{id=Id}=We, Acc) ->
    Tv = [{Vec,wings_util:add_vpos(Vs0, We)} || {Vec,Vs0} <- Tv0],
    Affected0 = lists:append([Vs0 || {Vec,Vs0} <- Tv0]),
    MoveSel = {Affected0,move_fun(Tv)},
    Vecs = move_vectors(NewVs, gb_sets:from_list(Affected0), We, []),
    Affected = [V || {V,Vec,Pos} <- Vecs],
    MoveAway = {Affected,move_away_fun(Vecs)},
    {We,[{Id,MoveSel},{Id,MoveAway}|Acc]};
plus_minus_move({Affected0,MoveFun0}, NewVs, #we{id=Id}=We, Acc) ->
    MoveFun = free_move_fun(MoveFun0),
    MoveSel = {Affected0,MoveFun},
    Vecs = move_vectors(NewVs, gb_sets:from_list(Affected0), We, []),
    Affected = [V || {V,Vec,Pos} <- Vecs],
    MoveAway = {Affected,move_away_fun(Vecs)},
    {We,[{Id,MoveSel},{Id,MoveAway}|Acc]}.

move_fun(Tv) ->
    fun({Dx,R}, Acc) ->
	    wings_drag:message([Dx], distance),
	    foldl(fun({Vec,VsPos}, A) ->
			  wings_drag:translate(Vec, Dx, VsPos, A)
		  end, Acc, Tv)
    end.

free_move_fun(MoveSel) ->
    fun({Dx,Dy,R}, Acc) ->
	    MoveSel({Dx,Dy}, Acc)
    end.

move_away_fun(Tv) ->
    fun({Dx,R}, Acc) -> move_away(R, Tv, Acc);
       ({Dx,Dy,R}, Acc) -> move_away(R, Tv, Acc)
    end.

move_away(R0, Tv, Acc) ->
    R = R0-1.0,
    foldl(fun({V,Vec,#vtx{pos={X,Y,Z}}=Rec}, A) -> 
		  {Xt,Yt,Zt} = e3d_vec:mul(Vec, R),
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  [{V,Rec#vtx{pos=Pos}}|A]
	  end, Acc, Tv).
    
move_vectors([V|Vs], VsSet, #we{vs=Vtab}=We, Acc0) ->
    Acc = wings_vertex:fold(
	    fun(_, _, Rec, A) ->
		    OtherV = wings_vertex:other(V, Rec),
		    case gb_sets:is_member(OtherV, VsSet) of
			false -> A;
			true ->
			    Pa = wings_vertex:pos(OtherV, Vtab),
			    #vtx{pos=Pb} = Pos = gb_trees:get(V, Vtab),
			    Vec = e3d_vec:sub(Pb, Pa),
			    [{V,Vec,Pos}|A]
		    end
	    end, Acc0, V, We),
    move_vectors(Vs, VsSet, We, Acc);
move_vectors([], VsSet, We, Acc) -> Acc.

