%%
%%  wings_extrude_edge.erl --
%%
%%     This module contains the Extrude and Bevel commands for edges.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_extrude_edge.erl,v 1.8 2001/09/24 07:24:53 bjorng Exp $
%%

-module(wings_extrude_edge).
-export([bevel/1,extrude/2]).

-include("wings.hrl").
-import(lists, [foldl/3,keydelete/3,member/2,
		reverse/1,reverse/2,last/1,foreach/2]).

-define(EXTRUDE_DIST, 0.2).

%%
%% The Bevel command (for edges).
%%

bevel(St0) ->
    {St,{Tvs,Sel}} = wings_sel:mapfold_shape(fun bevel_edges/4, {[],[]}, St0),
    wings_drag:init_drag(Tvs, {0.0,1.0E200}, St#st{selmode=face,sel=Sel}).

bevel_edges(Id, Edges, #we{es=Etab,next_id=Next}=We0, {Tvs,Ss}) ->
    {We1,OrigVs} = extrude_edges(Edges, We0),
    {We2,FaceSel0} = bevel_dissolve(Edges, We1),
    Tv = bevel_tv(OrigVs, We2),
    #we{fs=Ftab,vs=Vtab0} = We3 =
	foldl(fun(V, W0) ->
		      wings_collapse:collapse_vertex(V, W0)
	      end, We2, OrigVs),
    Vtab = bevel_reset_pos(OrigVs, We2, Vtab0),
    We = We3#we{vs=Vtab},
    FaceSel1 = [Face || Face <- FaceSel0, gb_trees:is_defined(Face, Ftab)],
    FaceSel = gb_sets:from_ordset(FaceSel1),
    {We,{[{Id,Tv}|Tvs],[{Id,FaceSel}|Ss]}}.

bevel_tv(Vs, We) ->
    foldl(fun(V, A) -> bevel_tv_1(V, We, A) end, [], Vs).

bevel_tv_1(V, We, Acc) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Face, Edge, Rec, Tv0) ->
	      OtherV = wings_vertex:other(V, Rec),
	      Pos = wings_vertex:pos(OtherV, We),
	      Vec = e3d_vec:divide(e3d_vec:sub(Pos, Center), ?EXTRUDE_DIST),
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

bevel_dissolve(Edges, #we{es=Etab}=We0) ->
    Faces = foldl(fun(E, A) ->
			  #edge{lf=Lf,rf=Rf} = gb_trees:get(E, Etab),
			  [Lf,Rf|A]
		  end, [], gb_sets:to_list(Edges)),
    #we{fs=Ftab} = We = wings_edge:dissolve_edges(Edges, We0),
    {We,Faces}.

%%
%% The Extrude command (for edges).
%%

extrude(Type, St0) ->
    St = wings_sel:map_shape(
	   fun(Edges, We0) ->
		   {We,_} = extrude_edges(Edges, We0),
		   We
	   end, St0),
    wings_move:setup(Type, St).

extrude_edges(Edges, #we{next_id=Wid,es=Etab}=We0) ->
    G = digraph:new(),
    foreach(fun(Edge) ->
		    digraph_edge(G, gb_trees:get(Edge, Etab))
	    end, gb_sets:to_list(Edges)),
    Vs0 = digraph:vertices(G),
    Vs1 = sofs:relation(Vs0),
    Vs = sofs:to_external(sofs:domain(Vs1)),
    We2 = foldl(fun(V, A) ->
			new_vertices(V, G, Edges, A)
		end, We0, Vs),
    We = connect(G, Wid, We2),
    digraph:delete(G),
    {We,Vs}.

new_vertices(V, G, Edges, We0) ->
    Center = wings_vertex:pos(V, We0),
    wings_vertex:fold(
      fun(Edge, _, _, #we{es=Etab}=W0=A) ->
	      case gb_sets:is_member(Edge, Edges) of
		  true -> A;
		  false ->
		      {W1,NewV} = wings_edge:cut(Edge, 2, W0),
		      NewE = NewV,
		      Rec = get_edge_rec(V, NewV, Edge, NewE, W1),
		      digraph_edge(G, Rec),
		      move_vertex(NewV, Center, W1)
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
