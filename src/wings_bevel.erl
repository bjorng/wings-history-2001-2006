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
%%     $Id: wings_bevel.erl,v 1.1 2001/08/14 18:16:40 bjorng Exp $
%%

-module(wings_bevel).
-export([bevel_faces/1]).
-include("wings.hrl").
-import(lists, [foldl/3,sort/1,last/1,filter/2]).

bevel_faces(St0) ->
    {St,OrigVs} = wings_sel:mapfold_region(fun bevel_faces/4, [], St0),
    wings_scale:bevel_face(sort(OrigVs), St).

bevel_faces(ShId, Faces, #we{next_id=Id}=We0, Acc) ->
    Neighbors = wings_face:faces_outside(Faces, We0),
    DisEdges = edges_to_dissolve(Faces, We0),
    OrigVs = wings_face:to_vertices(Faces, We0),
    We1 = gb_sets:fold(fun bevel_face/2, We0, Faces),
    We2 = dissolve_edges(DisEdges, We1),
    We3 = connect(OrigVs, Faces, We2),
    We = dissolve_more_edges(OrigVs, Faces, We3),
    wings_util:validate(We),
    MoveEdges = move_edges(Neighbors, Faces, We),
    {We,[{ShId,MoveEdges}|Acc]}.

bevel_face(Face, We) ->
    Es = get_edges(Face, We),
    wings_extrude:extrude_face(gb_sets:singleton(Face), Es, We).

get_edges(Face, We) ->
    wings_face:fold(fun(_, Edge, Rec, Acc0) ->
			    [Edge|Acc0]
		    end, [], Face, We).

edges_to_dissolve(Faces, We) ->
    gb_sets:fold(fun(Face, Acc) ->
			 edges_to_dissolve(Face, Faces, We, Acc)
		 end, dict:new(), Faces).

edges_to_dissolve(Face, Faces, We, Acc) ->
    wings_face:fold(
      fun(_, _, Rec, A) ->
	      Other = wings_face:other(Face, Rec),
	      case gb_sets:is_member(Other, Faces) of
		  true ->
		      if
			  Face < Other ->
			      dict:append(Face, Other, A);
			  true ->
			      dict:append(Other, Face, A)
		      end;
		  false -> A
	      end
      end, Acc, Face, We).
		      
dissolve_edges(Dict, We) ->
    Es = dict:fold(fun(Face, Faces0, A) ->
			   Faces = gb_sets:from_list(Faces0),
			   dissolve_edges(Face, Faces, We, A)
		   end, [], Dict),
    foldl(fun(E, W) -> wings_edge:dissolve_edge(E, W) end, We, Es).

dissolve_edges(Face, Faces, We, Acc0) ->
    wings_face:fold(
      fun(_, Edge0, Rec, A) ->
	      OtherFace0 = wings_face:other(Face, Rec),
	      {OtherFace1,Edge} = skip_two(OtherFace0, Edge0, We),
	      {OtherFace,_} = skip_two(OtherFace1, Edge, We),
	      case gb_sets:is_member(OtherFace, Faces) of
		  true -> [Edge|A];
		  false -> A
	      end
      end, Acc0, Face, We).

skip_two(Face, Edge0, We) ->
    Iter0 = wings_face:iterator(Face, We),
    Iter1 = wings_face:skip_to_edge(Edge0, Iter0),
    {_,_,_,Iter2} = wings_face:next_cw(Iter1),
    {_,_,_,Iter3} = wings_face:next_cw(Iter2),
    {_,Edge,Rec,_} = wings_face:next_cw(Iter3),
    {wings_face:other(Face, Rec),Edge}.

connect(Vs, Faces, We) ->
    gb_sets:fold(fun(V, A) -> connect_1(V, Faces, A) end, We, Vs).

connect_1(V, Faces, We) ->
    Vs = wings_vertex:fold(
	  fun(Edge, _, Rec, A) ->
		  OtherV = wings_vertex:other(V, Rec),
		  case touching(OtherV, Faces, We) of
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

dissolve_more_edges(Vs, Faces, We) ->
    Delete = gb_sets:fold(fun(V, A) ->
				  dissolve_more_edges_1(V, Faces, We, A)
			  end, gb_sets:empty(), Vs),
    foldl(fun(Edge, A) ->
		  wings_edge:dissolve_edge(Edge, A)
	  end, We, gb_sets:to_list(Delete)).

dissolve_more_edges_1(V, Faces, We, Acc) ->
    Dis = wings_vertex:fold(
	    fun (Edge, Face, Rec, A) ->
	      OtherV = wings_vertex:other(V, Rec),
	      case touching(OtherV, Faces, We) of
		  false -> [other|A];
		  true -> [Edge|A]
	      end
	    end, [], V, We),
    dissolve_0(Dis, Acc).

dissolve_0(none, Acc) -> Acc;
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

touching(V, Faces, We) ->
    wings_vertex:fold(
      fun (_, _, _, true) -> true;
	  (_, Face, _, A) ->
	      case gb_sets:is_member(Face, Faces) of
		  true -> true;
		  false -> A
	      end
      end, false, V, We).

move_edges(Neighbors, Faces, #we{fs=Ftab}=We) ->
    gb_sets:fold(fun(Face, A) ->
			 move_edges_1(Face, Faces, We, A)
		 end, dict:new(), Neighbors).

move_edges_1(Face, Faces, We, A) -> 
    wings_face:fold(
      fun(_, Edge, Rec, A0) ->
	      OtherFace0 = wings_face:other(Face, Rec),
	      {OtherFace,_} = skip_two(OtherFace0, Edge, We),
	      case gb_sets:is_member(OtherFace, Faces) of
		  false -> A0;
		  true -> dict:append(OtherFace, Edge, A0)
	      end
      end, A, Face, We).
