%%
%%  wings_sel.erl --
%%
%%     This module implements selection utilities.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_sel.erl,v 1.31 2002/03/11 14:39:04 bjorng Exp $
%%

-module(wings_sel).

-export([clear/1,set/2,set/3,
	 convert/3,convert_shape/3,convert_selection/2,
	 map_vs/2,map/2,fold/3,mapfold/3,
	 foreach/2,make/3,valid_sel/1,valid_sel/3,
	 centers/1,bounding_box/1,
	 face_regions/2,edge_regions/2,validate_items/3,
	 select_object/2,deselect_object/2,get_all_items/3,
	 inverse_items/3]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,reverse/2,sort/1,keydelete/3]).

clear(St) ->
    St#st{sel=[]}.

set(Sel, St) ->
    St#st{sel=sort(Sel)}.

set(Mode, Sel, St) ->
    St#st{selmode=Mode,sel=sort(Sel)}.

%%%
%%% Convert selection.
%%%

convert_selection(Mode, #st{sel=[]}=St) ->
    wings_draw:sel_changed(St#st{selmode=Mode});
convert_selection(Mode, St) ->
    ?SLOW(wings_draw:sel_changed(conv_sel(Mode, St))).

conv_sel(vertex, St) -> wings_vertex:convert_selection(St);
conv_sel(edge, St) -> wings_edge:convert_selection(St);
conv_sel(face, St) -> wings_face:convert_selection(St);
conv_sel(body, St) -> wings_body:convert_selection(St).

%%%
%%% Convert selection (helpers for wings_{vertex,edge,face,body}.
%%%

convert_shape(F, NewType, St) ->
    Sel = fold(fun(Items0, #we{id=Id}=We, Acc) ->
		       Items = F(Items0, We),
		       case gb_sets:is_empty(Items) of
			   true -> Acc;
			   false -> [{Id,Items}|Acc]
		       end
	       end, [], St),
    St#st{selmode=NewType,sel=reverse(Sel)}.

convert(F, NewType, St) ->
    Sel = fold(fun(Items0, #we{id=Id}=We, Acc) ->
		       Items = convert_items(F, Items0, We),
		       case gb_sets:is_empty(Items) of
			   true -> Acc;
			   false -> [{Id,Items}|Acc]
		       end
	       end, [], St),
    St#st{selmode=NewType,sel=reverse(Sel)}.

convert_items(F, Items, We) ->
    convert_items(F, gb_sets:empty(), gb_sets:iterator(Items), We).

convert_items(F, Acc0, Iter0, We) ->
    case gb_sets:next(Iter0) of
	none -> Acc0;
	{Item,Iter} ->
	    Acc = F(Item, We, Acc0),
	    convert_items(F, Acc, Iter, We)
    end.

%%%
%%% Map over the selection, allowing modifications only
%%% to vertex positions.
%%%

map_vs(F, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = gb_trees:to_list(Shs0),
    Shs = map_vs_1(F, Sel, Shs1, St, []),
    St#st{shapes=Shs}.

map_vs_1(F, [{Id,Items}|Sel], [{Id,We}|Shs], St, Acc) ->
    #we{vs=Vtab} = F(Items, We),
    map_vs_1(F, Sel, Shs, St, [{Id,We#we{vs=Vtab}}|Acc]);
map_vs_1(F, [_|_]=Sel, [Pair|Shs], St, Acc) ->
    map_vs_1(F, Sel, Shs, St, [Pair|Acc]);
map_vs_1(_F, [], Shs, _St, Acc) ->
    gb_trees:from_orddict(reverse(Acc, Shs)).

%%%
%%% Map over the selection, modifying the selected objects.
%%%

map(F, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = gb_trees:to_list(Shs0),
    Shs = map_1(F, Sel, Shs1, St, []),
    St#st{shapes=Shs}.

map_1(F, [{Id,Items}|Sel], [{Id,#we{mode=uv}=We0}|Shs], St, Acc) ->
    #we{es=Etab} = We1 = wings_we:uv_to_color(We0, St),
    We = case F(Items, We1) of
	     #we{es=Etab}=We2 -> We2#we{mode=uv,es=We0#we.es};
	     We2 -> We2
	 end,
    map_1(F, Sel, Shs, St, [{Id,We}|Acc]);
map_1(F, [{Id,Items}|Sel], [{Id,We0}|Shs], St, Acc) ->
    ?ASSERT(We0#we.id =:= Id),
    #we{es=Etab} = We = F(Items, wings_we:uv_to_color(We0, St)),
    case gb_sets:is_empty(Etab) of
	true -> map_1(F, Sel, Shs, St, Acc);
	false -> map_1(F, Sel, Shs, St, [{Id,We}|Acc])
    end;
map_1(F, [_|_]=Sel, [Pair|Shs], St, Acc) ->
    map_1(F, Sel, Shs, St, [Pair|Acc]);
map_1(_F, [], Shs, _St, Acc) ->
    gb_trees:from_orddict(reverse(Acc, Shs)).

%%%
%%% Fold over the selection.
%%%

fold(F, Acc, #st{sel=Sel,shapes=Shapes}) ->
    fold_1(F, Acc, Shapes, Sel).

fold_1(F, Acc0, Shapes, [{Id,Items}|T]) ->
    We = gb_trees:get(Id, Shapes),
    ?ASSERT(We#we.id =:= Id),
    fold_1(F, F(Items, We, Acc0), Shapes, T);
fold_1(_F, Acc, _Shapes, []) -> Acc.

%%%
%%% Map and fold over the selection.
%%%

mapfold(F, Acc0, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = gb_trees:to_list(Shs0),
    {Shs,Acc} = mapfold_1(F, Acc0, Sel, Shs1, St, []),
    {St#st{shapes=Shs},Acc}.

mapfold_1(F, Acc0, [{Id,Items}|Sel], [{Id,#we{mode=uv}=We0}|Shs],
	  St, ShsAcc) ->
    #we{es=Etab} = We1 = wings_we:uv_to_color(We0, St),
    We = case F(Items, We1, Acc0) of
	     {#we{es=Etab}=We2,Acc} -> We2#we{mode=uv,es=We0#we.es};
	     {We2,Acc} -> We2
	 end,
    mapfold_1(F, Acc, Sel, Shs, St, [{Id,We}|ShsAcc]);
mapfold_1(F, Acc0, [{Id,Items}|Sel], [{Id,We0}|Shs], St, ShsAcc) ->
    ?ASSERT(We0#we.id =:= Id),
    {#we{es=Etab}=We,Acc} = F(Items, wings_we:uv_to_color(We0, St), Acc0),
    case gb_trees:is_empty(Etab) of
	true -> mapfold_1(F, Acc0, Sel, Shs, St, ShsAcc);
	false -> mapfold_1(F, Acc, Sel, Shs, St, [{Id,We}|ShsAcc])
    end;
mapfold_1(F, Acc, [_|_]=Sel, [Pair|Shs], St, ShsAcc) ->
    mapfold_1(F, Acc, Sel, Shs, St, [Pair|ShsAcc]);
mapfold_1(_F, Acc, [], Shs, _St, ShsAcc) ->
    {gb_trees:from_orddict(reverse(ShsAcc, Shs)),Acc}.

%%%
%%% foreach functions (for drawing)
%%%

foreach(F, #st{sel=Sel,shapes=Shapes}) ->
    lists:foreach(fun({Id,Items}) ->
			  Sh = gb_trees:get(Id, Shapes),
			  Iter = gb_sets:iterator(Items),
			  foreach_1(F, Iter, Sh)
		  end, Sel).

foreach_1(F, Iter0, Sh) ->
    case gb_sets:next(Iter0) of
	none -> ok;
	{Item,Iter} ->
	    F(Item, Sh),
	    foreach_1(F, Iter, Sh)
    end.

%%%
%%% Make a selection.
%%%

make(Filter, Mode, #st{shapes=Shapes}=St) ->
    Sel0 = gb_trees:values(Shapes),
    Sel = make_1(Sel0, Filter, Mode),
    St#st{selmode=Mode,sel=Sel}.

make_1([#we{perm=Perm}|Shs], Filter, Mode) when ?IS_NOT_SELECTABLE(Perm) ->
    make_1(Shs, Filter, Mode);
make_1([#we{id=Id,vs=Vtab,es=Etab,fs=Ftab}=We|Shs], Filter, Mode) ->
    Tab = case Mode of
	      vertex -> Vtab;
	      edge -> Etab;
	      face -> Ftab
	  end,
    Keys = gb_trees:keys(Tab),
    case [Item || Item <- Keys, Filter(Item, We)] of
	[] -> make_1(Shs, Filter, Mode);
	Sel -> [{Id,gb_sets:from_ordset(Sel)}|make_1(Shs, Filter, Mode)]
    end;
make_1([], _Filter, _Mode) -> [].

%%%
%%% Calculate the centers for all selected objects.
%%%

centers(#st{selmode=Mode}=St) ->
    reverse(
      fold(
	fun(Items, We, A) ->
		[e3d_vec:average(bounding_box(Mode, Items, We, none))|A]
	end, [], St)).

%%%
%%% Calculate the bounding-box for the selection.
%%%

bounding_box(#st{selmode=Mode}=St) ->
    fold(fun(Items, We, A) ->
		 bounding_box(Mode, Items, We, A)
	 end, none, St).

bounding_box(vertex, Vs, We, BB) ->
    wings_vertex:bounding_box(Vs, We, BB);
bounding_box(face, Faces, We, BB) ->
    wings_vertex:bounding_box(wings_face:to_vertices(Faces, We), We, BB);
bounding_box(edge, Edges, We, BB) ->
    wings_vertex:bounding_box(wings_edge:to_vertices(Edges, We), We, BB);
bounding_box(body, _Items, We, BB) ->
    wings_vertex:bounding_box(We, BB).

%%%
%%% Here we want to divide the selection into regions of adjoining
%%% faces. We use a standard working-set algorithm.
%%%

face_regions(Faces, We) when is_list(Faces) ->
    find_face_regions(gb_sets:from_list(Faces), We, []);
face_regions(Faces, We) ->
    find_face_regions(Faces, We, []).

find_face_regions(Faces0, We, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    Ws = gb_sets:singleton(Face),
	    {Reg,Faces} = collect_face_region(Ws, We, gb_sets:empty(), Faces1),
	    find_face_regions(Faces, We, [Reg|Acc])
    end.

collect_face_region(Ws0, We, Reg0, Faces0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {Reg0,Faces0};
	false ->
	    {Face,Ws1} = gb_sets:take_smallest(Ws0),
	    Reg = gb_sets:add(Face, Reg0),
	    {Ws,Faces} = collect_adj_sel(Face, We, Ws1, Faces0),
	    collect_face_region(Ws, We, Reg, Faces)
    end.

collect_adj_sel(Face, We, Ws0, Faces0) ->
    wings_face:fold(
      fun(_, _, Rec, {W0,F0}=A) ->
	      Of = case Rec of
		       #edge{lf=Face,rf=Of0} -> Of0;
		       #edge{rf=Face,lf=Of0} -> Of0
		   end,
	      case gb_sets:is_member(Of, F0) of
		  true -> {gb_sets:insert(Of, W0),gb_sets:delete(Of, F0)};
		  false -> A
	      end
      end, {Ws0,Faces0}, Face, We).

edge_regions(Edges, We) when is_list(Edges) ->
    find_edge_regions(gb_sets:from_list(Edges), We, []);
edge_regions(Edges, We) ->
    find_edge_regions(Edges, We, []).

find_edge_regions(Edges0, We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    Ws = gb_sets:singleton(Edge),
	    Reg0 = gb_sets:empty(),
	    {Reg,Edges} = find_all_adj_edges(Ws, We, Reg0, Edges1),
	    find_edge_regions(Edges, We, [Reg|Acc])
    end.

find_all_adj_edges(Ws0, #we{es=Etab}=We, Reg0, Edges0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {Reg0,Edges0};
	false ->
	    {Edge,Ws1} = gb_sets:take_smallest(Ws0),
	    Reg = gb_sets:add(Edge, Reg0),
	    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
	    Adj0 = add_adjacent_edges(Va, We, []),
	    Adj1 = add_adjacent_edges(Vb, We, Adj0),
	    Adj = gb_sets:from_list(Adj1),
	    AdjSel = gb_sets:intersection(Adj, Edges0),
	    Ws = gb_sets:union(Ws1, AdjSel),
	    Edges = gb_sets:difference(Edges0, AdjSel),
	    find_all_adj_edges(Ws, We, Reg, Edges)
    end.

add_adjacent_edges(V, We, Acc) ->
    wings_vertex:fold(fun(Edge, _, _, A) -> [Edge|A] end, Acc, V, We).
	      
valid_sel(#st{sel=Sel,selmode=Mode}=St) ->
    St#st{sel=valid_sel(Sel, Mode, St)}.
    
valid_sel(Sel0, Mode, #st{shapes=Shapes}) ->
    Sel = foldl(
	    fun({Id,Items0}, A) ->
		    case gb_trees:lookup(Id, Shapes) of
			none -> A;
			{value,#we{perm=Perm}} when ?IS_NOT_SELECTABLE(Perm) ->
			    A;
			{value,We} ->
			    Items = validate_items(Items0, Mode, We),
			    case gb_trees:is_empty(Items) of
				false -> [{Id,Items}|A];
				true -> A
			    end
		    end
	    end, [], Sel0),
    reverse(Sel).

validate_items(Vs, vertex, #we{vs=Vtab}) ->
    tab_set_intersection(Vs, Vtab);
validate_items(Es, edge, #we{es=Etab}) ->
    tab_set_intersection(Es, Etab);
validate_items(Fs, face, #we{fs=Ftab}) ->
    tab_set_intersection(Fs, Ftab);
validate_items(Items, body, _We) -> Items.

tab_set_intersection(Set, Tab) ->
    Keys = gb_sets:from_ordset(gb_trees:keys(Tab)),
    gb_sets:intersection(Set, Keys).

select_object(Id, #st{selmode=Mode,sel=Sel0}=St) ->
    Sel = sort([{Id,get_all_items(Mode, Id, St)}|Sel0]),
    St#st{sel=Sel}.

deselect_object(Id, #st{sel=Sel0}=St) ->
    Sel = keydelete(Id, 1, Sel0),
    St#st{sel=Sel}.

get_all_items(Mode, Id, #st{shapes=Shapes}) ->
    We = gb_trees:get(Id, Shapes),
    Items = case Mode of
		vertex -> gb_trees:keys(We#we.vs);
		edge -> gb_trees:keys(We#we.es);
		face -> gb_trees:keys(We#we.fs);
		body -> [0]
	    end,
    gb_sets:from_ordset(Items).

inverse_items(vertex, Items, #we{vs=Tab}) ->
    inverse_items_1(Items, Tab);
inverse_items(edge, Items, #we{es=Tab}) ->
    inverse_items_1(Items, Tab);
inverse_items(face, Items, #we{fs=Tab}) ->
    inverse_items_1(Items, Tab).

inverse_items_1(Items, Tab) ->
    Keys0 = gb_trees:keys(Tab),
    Keys = gb_sets:from_ordset(Keys0),
    gb_sets:difference(Keys, Items).
