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
%%     $Id: wings_sel.erl,v 1.24 2001/12/31 13:55:19 bjorng Exp $
%%

-module(wings_sel).

-export([convert/3,convert_shape/3,convert_selection/2,
	 fold/3,map/2,mapfold/3,
	 foreach/2,make/3,valid_sel/1,valid_sel/3,
	 centers/1,bounding_box/1,
	 face_regions/2,edge_regions/2,validate_items/3,
	 select_object/2,deselect_object/2,get_all_items/3,
	 inverse_items/3]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,reverse/2,sort/1,keydelete/3]).

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
%%% Map over the selection, modifying the selected objects.
%%%

map(F, #st{shapes=Shapes0,sel=Sel}=St) ->
    Shapes = map_1(F, Sel, gb_trees:to_list(Shapes0), []),
    St#st{shapes=Shapes}.

map_1(F, [{Id,_}|_], [{Id,#we{mode=uv}}|_]=Shs, Acc) ->
    uvmap_error(Shs);
map_1(F, [{Id,Items}|Sel], [{Id,We0}|Shs], Acc) ->
    ?ASSERT(We0#we.id =:= Id),
    We = F(Items, We0),
    map_1(F, Sel, Shs, [{Id,We}|Acc]);
map_1(F, [_|_]=Sel, [Pair|Shs], Acc) ->
    map_1(F, Sel, Shs, [Pair|Acc]);
map_1(F, [], Shs, Acc) ->
    gb_trees:from_orddict(reverse(Acc, Shs)).

%%%
%%% Fold over the selection.
%%%

fold(F, Acc, #st{sel=Sel,shapes=Shapes}=St) ->
    fold_1(F, Acc, Shapes, Sel).

fold_1(F, Acc0, Shapes, [{Id,Items}|T]) ->
    We = gb_trees:get(Id, Shapes),
    ?ASSERT(We#we.id =:= Id),
    fold_1(F, F(Items, We, Acc0), Shapes, T);
fold_1(F, Acc, Shapes, []) -> Acc.

%%%
%%% Map and fold over the selection.
%%%

mapfold(F, Acc0, #st{shapes=Shapes0,sel=Sel}=St) ->
    {Shapes,Acc} = mapfold_1(F, Acc0, Sel, gb_trees:to_list(Shapes0), []),
    {St#st{shapes=Shapes},Acc}.

mapfold_1(F, Acc, [{Id,_}|_], [{Id,#we{mode=uv}=We0}|_]=Shs, Acc) ->
    uvmap_error(Shs);
mapfold_1(F, Acc0, [{Id,Items}|Sel], [{Id,We0}|Shs], ShsAcc) ->
    ?ASSERT(We0#we.id =:= Id),
    {We,Acc} = F(Items, We0, Acc0),
    mapfold_1(F, Acc, Sel, Shs, [{Id,We}|ShsAcc]);
mapfold_1(F, Acc, [_|_]=Sel, [Pair|Shs], ShsAcc) ->
    mapfold_1(F, Acc, Sel, Shs, [Pair|ShsAcc]);
mapfold_1(F, Acc, [], Shs, ShsAcc) ->
    {gb_trees:from_orddict(reverse(ShsAcc, Shs)),Acc}.


uvmap_error(Shs) ->
    Message = "This command is not allowed on objects with textures."
	" (" ++ uvmap_objects(Shs) ++ ")",
    throw({command_error,Message}).

uvmap_objects(Shs) ->
    case [Name || {Id,#we{mode=uv,name=Name}} <- Shs] of
	[Name] -> "Object " ++ Name;
	[Na,Nb] -> "Objects " ++ Na ++ " and " ++ Nb;
	[Na,Nb,Nc] -> "Objects " ++ Na ++ ", " ++ Nb ++ ", and " ++ Nc;
	[Na,Nb,Nc|_] -> "Objects " ++ Na ++ ", " ++ Nb ++ ", " ++ Nc ++ "..."
    end.

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

make_1([#we{perm=Perm}|_], Filter, Mode) when ?IS_NOT_SELECTABLE(Perm) ->
    [];
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
make_1([], Filter, Mode) -> [].

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
bounding_box(body, Items, #we{vs=Vtab}=We, BB) ->
    wings_vertex:bounding_box(We, BB).

%%%
%%% Here we want to divide the selection into regions of adjoining
%%% faces. We use a standard working-set algorithm.
%%%

face_regions(Faces, #we{es=Etab,fs=Ftab}) when is_list(Faces) ->
    find_face_regions(gb_sets:from_list(Faces), Ftab, Etab, []);
face_regions(Faces, #we{es=Etab,fs=Ftab}) ->
    find_face_regions(Faces, Ftab, Etab, []).

find_face_regions(Faces0, Ftab, Etab, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    Reg0 = gb_sets:singleton(Face),
	    Adj = find_adj(Face, Ftab, Etab),
	    Ws = gb_sets:intersection(Adj, Faces1),
	    case gb_sets:is_empty(Ws) of
		true ->
		    find_face_regions(Faces1, Ftab, Etab, [Reg0|Acc]);
		false ->
		    {Reg,Faces} = find_all_adj(Ws, Ftab, Etab, Reg0, Faces1),
		    find_face_regions(Faces, Ftab, Etab, [Reg|Acc])
	    end
    end.

find_all_adj(Ws0, Ftab, Etab, Reg0, Faces0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {Reg0,Faces0};
	false ->
	    {Face,Ws1} = gb_sets:take_smallest(Ws0),
	    Reg = gb_sets:add(Face, Reg0),
	    Adj = find_adj(Face, Ftab, Etab),
	    AdjSel = gb_sets:intersection(Adj, Faces0),
	    Ws = gb_sets:union(Ws1, AdjSel),
	    Faces = gb_sets:difference(Faces0, AdjSel),
	    find_all_adj(Ws, Ftab, Etab, Reg, Faces)
    end.

find_adj(Face, Ftab, Etab) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    find_adj(Face, Edge, Edge, Etab, gb_sets:singleton(Face), not_done).

find_adj(Face, LastEdge, LastEdge, Etab, Acc, done) -> Acc;
find_adj(Face, Edge, LastEdge, Etab, Acc, _) ->
    {Next,Other} =
	case gb_trees:get(Edge, Etab) of
	    #edge{lf=Face,rf=Other0,ltpr=Next0} -> {Next0,Other0};
	    #edge{rf=Face,lf=Other0,rtpr=Next0} -> {Next0,Other0}
	end,
    find_adj(Face, Next, LastEdge, Etab, gb_sets:add(Other, Acc), done).


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
    Sel = foldl(fun({Id,Items}, A) ->
			case gb_trees:lookup(Id, Shapes) of
			    none -> A;
			    {value,Shape} ->
				[{Id,validate_items(Items, Mode, Shape)}|A]
			end
		end, [], Sel0),
    reverse(Sel).

validate_items(Vs, vertex, #we{vs=Vtab}) ->
    tab_set_intersection(Vs, Vtab);
validate_items(Es, edge, #we{es=Etab}) ->
    tab_set_intersection(Es, Etab);
validate_items(Fs, face, #we{fs=Ftab}) ->
    tab_set_intersection(Fs, Ftab);
validate_items(Items, body, Shape) -> Items.

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

