%%
%%  wpa.erl --
%%
%%     Wings Plugin API.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpa.erl,v 1.2 2001/12/29 20:33:56 bjorng Exp $
%%
-module(wpa).
-export([ask/3,
	 sel_map/2,sel_fold/3,sel_convert/3,
	 sel_edge_regions/2,sel_face_regions/2,
	 vertices/1,vertex_pos/2,vertex_flatten/3,vertex_center/2,
	 faces/1,face_vertices/2,face_outer_vertices/2,face_outer_edges/2,
	 edge_loop_vertices/2,
	 obj_name/1,obj_id/1
	]).

-include("wings.hrl").
-import(lists, [reverse/1]).

ask(Ask, Qs, Fun) ->
    wings_util:ask(Ask, Qs, Fun).

%%% Selection utilities.

sel_map(F, St) ->
    wings_sel:map(
      fun(Items, We) ->
	      F(gb_sets:to_list(Items), We)
      end, St).

sel_fold(F, Acc, St) ->
    wings_sel:fold(
      fun(Items, We, A) ->
	      F(gb_sets:to_list(Items), We, A)
      end, Acc, St).

sel_convert(F, Mode, St) ->
    Sel = wings_sel:fold(
	    fun(Items0, #we{id=Id}=We, A) ->
		    case F(gb_sets:to_list(Items0), We) of
			[] -> A;
			Items -> [{Id,gb_sets:from_list(Items)}|A]
		    end
	    end, [], St),
    St#st{selmode=Mode,sel=reverse(Sel)}.

sel_edge_regions(Edges, We) ->
    wings_sel:edge_regions(Edges, We).

sel_face_regions(Faces, We) ->
    wings_sel:face_regions(Faces, We).

%%% Vertices.

vertices(#we{vs=Vtab}) -> gb_trees:keys(Vtab).

vertex_pos(V, #we{vs=Vtab}) ->
    #vtx{pos=Pos} = gb_trees:get(V, Vtab),
    Pos.

vertex_flatten(Vs, PlaneNormal, We) ->
    wings_vertex:flatten(Vs, PlaneNormal, We).

vertex_center(Vs, We) ->
    wings_vertex:center(Vs, We).

%%% Edges.

edge_loop_vertices(Edges, We) ->
    wings_edge_loop:edge_loop_vertices(Edges, We).

%%% Faces

faces(#we{fs=Ftab}) -> gb_trees:keys(Ftab).

face_vertices(Face, We) ->
    wings_face:surrounding_vertices(Face, We).

face_outer_vertices(Faces, We) ->
    outer_vertex_partition(Faces, We).

face_outer_edges(Faces, We) ->
    wings_face_cmd:outer_edge_partition(Faces, We).

%%% Objects.

obj_name(#we{name=Name}) -> Name.
obj_id(#we{id=Id}) -> Id.

%%%
%%% Utilities.
%%%

outer_vertex_partition(Faces, We) when is_list(Faces) ->
    collect_outer_edges(Faces, gb_sets:from_list(Faces), We, []);
outer_vertex_partition(Faces, We) ->
    collect_outer_edges(gb_sets:to_list(Faces), Faces, We, []).

collect_outer_edges([Face|Fs], Faces, We, Acc0) ->
    Acc = wings_face:fold(
	    fun(_, E, Erec, A) ->
		    outer_edge(E, Erec, Face, Faces, A)
	    end, Acc0, Face, We),
    collect_outer_edges(Fs, Faces, We, Acc);
collect_outer_edges([], Faces, We, Acc) ->
    R = sofs:relation(Acc),
    F = sofs:relation_to_family(R),
    partition_edges(gb_trees:from_orddict(sofs:to_external(F)), []).

outer_edge(Edge, Erec, Face, Faces, Acc) ->
    {V,OtherV,OtherFace} =
	case Erec of
	    #edge{vs=Vs,ve=Ve,lf=Face,rf=Other0,ltpr=Next0} ->
		{Vs,Ve,Other0};
	    #edge{vs=Vs,ve=Ve,rf=Face,lf=Other0,rtpr=Next0} ->
		{Ve,Vs,Other0}
	end,
    case gb_sets:is_member(OtherFace, Faces) of
	true -> Acc;
	false -> [{V,{Edge,V,OtherV,Face}}|Acc]
    end.

partition_edges(Es0, Acc) ->
    case gb_sets:is_empty(Es0) of
	true -> Acc;
	false ->
	    {Key,Val,Es1} = gb_trees:take_smallest(Es0),
	    {Part,Es} = partition_edges(Key, unknown, Val, Es1, []),
	    partition_edges(Es, [Part|Acc])
    end.

partition_edges(Va, _, [{Edge,Va,Vb,Face}], Es0, Acc0) ->
    Acc = [Va|Acc0],
    case gb_trees:lookup(Vb, Es0) of
	none -> {Acc,Es0};
	{value,Val} ->
	    Es = gb_trees:delete(Vb, Es0),
	    partition_edges(Vb, Face, Val, Es, Acc)
    end;
partition_edges(Va, unknown, [{_,Va,_,Face}|_]=Edges, Es, Acc) ->
    partition_edges(Va, Face, Edges, Es, Acc);
partition_edges(Va, Face, Edges0, Es0, Acc) ->
    [Val] = [E || {_,_,_,AFace}=E <- Edges0, AFace =:= Face],
    Edges = [E || {_,_,_,AFace}=E <- Edges0, AFace =/= Face],
    Es = gb_trees:insert(Va, Edges, Es0),
    partition_edges(Va, Face, [Val], Es, Acc).
