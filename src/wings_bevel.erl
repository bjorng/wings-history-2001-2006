%%
%%  wings_bevel.erl --
%%
%%     This module contains the Bevel command for faces.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_bevel.erl,v 1.7 2001/09/14 09:58:02 bjorng Exp $
%%

-module(wings_bevel).
-export([bevel_faces/1]).
-include("wings.hrl").
-import(lists, [foldl/3,sort/1,last/1,filter/2]).

bevel_faces(St0) ->
    {St,OrigVs} = wings_sel:mapfold_region(fun bevel_faces/4, [], St0),
    wings_scale:bevel_face(sort(OrigVs), St).

bevel_faces(ShId, Faces, We0, Acc) ->
    DisEdges = wings_face:inner_edges(Faces, We0),
    OrigVs = wings_face:to_vertices(Faces, We0),
    MoveEdges = move_edges(Faces, We0),
    We1 = wings_extrude_face:faces(Faces, We0),
    NewVs = wings_we:new_items(vertex, We0, We1),
    We2 = dissolve_edges(DisEdges, We1),
    We3 = connect(OrigVs, NewVs, We2),
    We = dissolve_more_edges(OrigVs, NewVs, We3),
    {We,[{ShId,MoveEdges}|Acc]}.

connect(Vs, NewVs, We) ->
    gb_sets:fold(fun(V, A) -> connect_1(V, NewVs, A) end, We, Vs).

connect_1(V, NewVs, We) ->
    Vs = wings_vertex:fold(
	  fun(Edge, _, Rec, A) ->
		  OtherV = wings_vertex:other(V, Rec),
		  case gb_sets:is_member(OtherV, NewVs) of
		      false -> A;
		      true ->
			  wings_vertex:fold(
			    fun(_, Face, _, A1) ->
				    [{Face,OtherV}|A1]
			    end, A, OtherV, We)
		  end
	  end, [], V, We),
    R = sofs:relation(Vs),
    Family = sofs:relation_to_family(R),
    foldl(fun ({Face,[_]}, A) -> A;
	      ({Face,[Vstart,Vend]}, A0) ->
		  {A,_} = wings_vertex:force_connect(Vstart, Vend, Face, A0),
		  A
	  end, We, sofs:to_external(Family)).

dissolve_more_edges(Vs, NewVs, We) ->
    Delete = gb_sets:fold(fun(V, A) ->
				  dissolve_more_edges_1(V, NewVs, We, A)
			  end, gb_sets:empty(), Vs),
    dissolve_edges(gb_sets:to_list(Delete), We).

dissolve_more_edges_1(V, NewVs, We, Acc) ->
    Dis = wings_vertex:fold(
	    fun (Edge, Face, Rec, A) ->
		    OtherV = wings_vertex:other(V, Rec),
		    case gb_sets:is_member(OtherV, NewVs) of
			false -> [other|A];
			true -> [Edge|A]
		    end
	    end, [], V, We),
    dissolve_0(Dis, Acc).

dissolve_0([E|_]=Es, Acc) ->
    case length(filter(fun(other) -> false;
			  (_) -> true
		       end, Es)) of
	Len when Len > 2 ->
	    dissolve_1([last(Es)|Es++[E]], Acc);
	Len -> Acc
    end.

dissolve_1([other|Es], Acc) ->
    dissolve_1(Es, Acc);
dissolve_1([_,Edge,other|Es], Acc) ->
    dissolve_1(Es, Acc);
dissolve_1([_|[other|_]=Es], Acc) ->
    dissolve_1(Es, Acc);
dissolve_1([_|[Edge|_]=Es], Acc) ->
    dissolve_1(Es, gb_sets:add(Edge, Acc));
dissolve_1(Other, Acc) -> Acc.

dissolve_edges(Edges, We0) ->
    foldl(fun(E, W) -> wings_edge:dissolve_edge(E, W) end, We0, Edges).

move_edges(Faces, We) ->
    R0 = wings_face:fold_faces(
	   fun(F, _, E, _, A) ->
		   [{F,E}|A]
	   end, [], Faces, We),
    R = sofs:relation(R0),
    P = sofs:partition(2, R),
    M = sofs:specification(fun(L) -> sofs:no_elements(L) =:= 1 end, P),
    U = sofs:union(M),
    sofs:to_external(sofs:relation_to_family(U)).
