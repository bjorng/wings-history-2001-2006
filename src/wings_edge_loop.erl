%%
%%  wings_edge_loop.erl --
%%
%%     This module handles edge-loop commands.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_edge_loop.erl,v 1.4 2001/12/26 14:46:26 bjorng Exp $
%%

-module(wings_edge_loop).
-export([select_next/1,select_prev/1,select_loop/1]).

%% Utilities.
-export([edge_loop_vertices/2]).

-include("wings.hrl").
-import(lists, [sort/1,append/1,reverse/1]).

%%%
%%% Select next/previous edge loop.
%%%

select_next(#st{selmode=edge,sel=[_]}=St) ->
    find_loop(St, next);
select_next(St) -> St.

select_prev(#st{selmode=edge,sel=[_]}=St) ->
    find_loop(St, previous);
select_prev(St) -> St.

find_loop(#st{sel=[{Id,Edges}=PrevSel],shapes=Shapes}=St, Dir0) ->
    We = gb_trees:get(Id, Shapes),
    #we{es=Etab} = We,
    G = digraph:new(),
    build_digraph(G, gb_sets:to_list(Edges), Edges, Etab),
    Cs0 = digraph_utils:components(G),
    Cs1 = get_edges(G, Cs0),
    Cs = [C || C <- Cs1, is_closed_loop(C, We)],
    digraph:delete(G),
    {Dir,PrevLoop} = prev_loop(Dir0, St),
    Sel = case pick_loop(Cs, Dir, PrevLoop, St) of
	      none ->
		  case pick_loop(Cs, Dir, PrevLoop, St) of
		      none -> PrevSel;
		      Sel0 -> Sel0
		  end;
	      Sel0 -> Sel0
	  end,
    St#st{sel=[Sel],edge_loop={Dir0,PrevSel}}.

is_closed_loop(Edges, We) ->
    case edge_loop_vertices(Edges, We) of
	[_] -> true;
        _ -> false
    end.

get_edges(G, [C|Cs]) ->
    Es = gb_sets:from_list(append([digraph:edges(G, V) || V <- C])),
    [Es|get_edges(G, Cs)];
get_edges(G, []) -> [].

prev_loop(Dir, #st{edge_loop=none}) -> {none,none};
prev_loop(Same, #st{sel=[{Id,_}],edge_loop={Same,{Id,L}}}) ->
    {away,L};
prev_loop(_, #st{sel=[{Id,_}],edge_loop={_,{Id,L}}}) ->
    {towards,L};
prev_loop(_, _) -> {away,none}.
    
pick_loop([C|Cs], Dir, PrevLoop, #st{sel=[{Id,_}]}=St) ->
    IsPrev = PrevLoop =:= C,
    if
	(Dir == away) and IsPrev ->
	    pick_loop(Cs, Dir, PrevLoop, St);
	(Dir == towards) and (not IsPrev) ->
	    pick_loop(Cs, Dir, PrevLoop, St);
	true -> {Id,C}
    end;
pick_loop([], Dir, PrevLoop, #st{sel=[Sel]}) -> none.

build_digraph(G, [E|Es], Edges, Etab) ->
    Erec = gb_trees:get(E, Etab),
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,ltpr=Lp,ltsu=Ls,rtpr=Rp,rtsu=Rs} = Erec,
    follow_edge(G, Ls, Edges, Etab),
    follow_edge(G, Rp, Edges, Etab),
    follow_edge(G, Lp, Edges, Etab),
    follow_edge(G, Rs, Edges, Etab),
    build_digraph(G, Es, Edges, Etab);
build_digraph(G, [], Edges, We) -> ok.

follow_edge(G, E, Edges, Etab) ->
    case gb_sets:is_member(E, Edges) of
	true -> ok;
	false ->
	    #edge{ltpr=Lp,ltsu=Ls,rtpr=Rp,rtsu=Rs} =
		gb_trees:get(E, Etab),
	    follow_edge_1(G, Lp, Edges, Etab),
	    follow_edge_1(G, Ls, Edges, Etab),
	    follow_edge_1(G, Rp, Edges, Etab),
	    follow_edge_1(G, Rs, Edges, Etab)
    end.

follow_edge_1(G, E, Edges, Etab) ->
    case gb_sets:is_member(E, Edges) of
	true -> ok;
	false ->
	    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
	    add_edge(G, E, Va, Vb)
    end.

add_edge(G, E, Va, Vb) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, E, Va, Vb, []).

%%%
%%% The Select Edge Loop command.
%%%

select_loop(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun select_loop/3, [], St),
    St#st{sel=reverse(Sel)};
select_loop(St) -> St.

select_loop(Edges0, #we{id=Id,es=Etab}=We, Acc) ->
    Edges = select_loop_1(Edges0, Etab, gb_sets:empty()),
    [{Id,Edges}|Acc].

select_loop_1(Edges0, Etab, Sel0) ->
    case gb_sets:is_empty(Edges0) of
	true -> Sel0;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    Sel = gb_sets:insert(Edge, Sel0),
	    Edges = select_loop_edges(Edge, Etab, Sel, Edges1),
	    select_loop_1(Edges, Etab, Sel)
    end.

select_loop_edges(Edge, Etab, Sel, Edges0) ->
    #edge{vs=Va,ve=Vb} = Erec = gb_trees:get(Edge, Etab),
    Edges = try_edge_from(Va, Edge, Erec, Etab, Sel, Edges0),
    try_edge_from(Vb, Edge, Erec, Etab, Sel, Edges).

try_edge_from(V, FromEdge, Erec, Etab, Sel, Edges) ->
    case try_edge_from_1(V, FromEdge, Erec, Etab) of
	none -> Edges;
	Edge ->
	    case gb_sets:is_member(Edge, Sel) of
		true -> Edges;
		false -> gb_sets:add(Edge, Edges)
	    end
    end.

try_edge_from_1(V, From, Erec, Etab) ->
    case Erec of
	#edge{vs=V,lf=FL,rf=FR,ltsu=EL,rtpr=ER} -> ok;
	#edge{ve=V,lf=FL,rf=FR,ltpr=EL,rtsu=ER} -> ok
    end,
    if
	EL =:= ER -> EL;
	true ->
	    case {next_edge(From, V, FL, EL, Etab),
		  next_edge(From, V, FR, ER, Etab)} of
		{Edge,Edge} -> Edge;
		{_,_} -> none
	    end
    end.

next_edge(From, V, Face, Edge, Etab) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,ve=Ov,rf=Face,rtpr=From,ltsu=To} -> To;
	#edge{vs=V,ve=Ov,lf=Face,ltsu=From,rtpr=To} -> To;
	#edge{ve=V,vs=Ov,rf=Face,rtsu=From,ltpr=To} -> To;
	#edge{ve=V,vs=Ov,lf=Face,ltpr=From,rtsu=To} -> To
    end.

%% edge_loop_vertices(EdgeSet, WingedEdge) -> [[Vertex]] | none
%%  Given a set of edges that is supposed to form
%%  one or more simple closed loops, this function returns
%%  the vertices that make up each loop in the correct order.

edge_loop_vertices(Edges, We) ->
    edge_loop_vertices(Edges, We, []).

edge_loop_vertices(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    #edge{vs=V,ve=Vend} = gb_trees:get(Edge, Etab),
	    case edge_loop_vertices(Edges1, V, Edge, Vend, We, [Vend]) of
		none -> none;
		{Vs,Edges} -> edge_loop_vertices(Edges, We, [Vs|Acc])
	    end
    end.

edge_loop_vertices(Edges, Vend, Edge, Vend, We, Acc) -> {Acc,Edges};
edge_loop_vertices(Edges0, V, PrevEdge, Vend, We, Acc) ->
    Res = wings_vertex:until(
	    fun(Edge, _, Rec, A) ->
		    case gb_sets:is_member(Edge, Edges0) of
			true -> {Edge,wings_vertex:other(V, Rec)};
			false -> A
		    end
	    end, none, V, We),
    case Res of
	none -> none;
	{Edge,OtherV} ->
	    Edges = gb_sets:delete(Edge, Edges0),
	    edge_loop_vertices(Edges, OtherV, Edge, Vend, We, [V|Acc])
    end.
