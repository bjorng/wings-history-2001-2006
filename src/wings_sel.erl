%%
%%  wings_sel.erl --
%%
%%     This module implements selection utilities and some of the
%%     commands in the selection menu (the other commands are
%%     implemented in wings.erl).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_sel.erl,v 1.15 2001/10/03 09:24:11 bjorng Exp $
%%

-module(wings_sel).

%% Utilities.
-export([convert/3,convert_shape/3,convert_selection/2,
	 fold/3,fold_shape/3,fold_region/3,
	 map/2,map_shape/2,map_region/2,
	 mapfold/3,mapfold_shape/3,mapfold_region/3,
	 foreach/2,make/3,valid_sel/1,
	 centers/1,bounding_box/1,
	 find_face_regions/2,validate_items/3,inverse_items/3,
	 random/2]).

%% Selection commands.
-export([select_all/1,select_more/1,select_less/1,
	 save/1,load/1,
	 exchange/1,union/1,subtract/1,intersection/1,
	 inverse/1,similar/1]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,sort/1]).

%%%
%%% Convert selection.
%%%

convert_selection(vertex, St) ->
    wings_vertex:convert_selection(St);
convert_selection(edge, St) ->
    wings_edge:convert_selection(St);
convert_selection(face, St) ->
    wings_face:convert_selection(St);
convert_selection(body, St) ->
    wings_body:convert_selection(St).

%%%
%%% Convert selection (helpers for wings_{vertex,edge,face,body}.
%%%

convert_shape(F, NewType, St) ->
    Sel = fold_shape(
	    fun(#shape{id=Id,sh=We}=Sh, Items0, Acc) ->
		    Items = F(Items0, We),
		    case gb_sets:is_empty(Items) of
			true -> Acc;
			false -> [{Id,Items}|Acc]
		    end
	    end, [], St),
    St#st{selmode=NewType,sel=reverse(Sel)}.

convert(F, NewType, St) ->
    Sel = fold_shape(
	    fun(#shape{id=Id,sh=We}=Sh, Items0, Acc) ->
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
%%% fold functions.
%%%

fold(F, Acc, #st{selmode=body,sel=Sel,shapes=Shapes}=St) ->
    fold_body(F, Acc, Shapes, Sel);
fold(F, Acc, #st{sel=Sel,shapes=Shapes}=St) ->
    fold_outer_nonbody(F, Acc, Shapes, Sel).

fold_body(F, Acc, Shapes, [{Id,Items}|T]) ->
    Sh = gb_trees:get(Id, Shapes),
    fold_body(F, F(Sh, Acc), Shapes, T);
fold_body(F, Acc, Shapes, []) -> Acc.

fold_outer_nonbody(F, Acc0, Shapes, [{Id,Items}|T]) ->
    #shape{sh=We} = gb_trees:get(Id, Shapes),
    Iter = gb_sets:iterator(Items),
    Acc = fold_inner_nonbody(F, Acc0, Iter, Id, We),
    fold_outer_nonbody(F, Acc, Shapes, T);
fold_outer_nonbody(F, Acc, Shapes, []) -> Acc.

fold_inner_nonbody(F, Acc, Iter0, Id, We) ->
    case gb_sets:next(Iter0) of
	none -> Acc;
	{Item,Iter} ->
	    fold_inner_nonbody(F, F(Id, Item, We, Acc), Iter, Id, We)
    end.

fold_shape(F, Acc, #st{sel=Sel,shapes=Shapes}=St) ->
    fold_shape(F, Acc, Sel, Shapes).

fold_shape(F, Acc, [{Id,Items}|T], Shapes) ->
    Sh = gb_trees:get(Id, Shapes),
    fold_shape(F, F(Sh, Items, Acc), T, Shapes);
fold_shape(F, Acc, [], Shapes) -> Acc.

fold_region(F, Acc, #st{selmode=edge,shapes=Shapes,sel=Sel}=St) ->
    fold_region_nonbody(fun find_edge_regions/2, F, Acc, Shapes, Sel);
fold_region(F, Acc, #st{selmode=face,shapes=Shapes,sel=Sel}=St) ->
    fold_region_nonbody(fun find_face_regions/2, F, Acc, Shapes, Sel);
fold_region(F, Acc, St) ->
    erlang:fault(badarg, [F,Acc,St]).

fold_region_nonbody(RegFind, F, Acc0, Shapes, [{Id,Items}|T]) ->
    #shape{sh=#we{}=We} = gb_trees:get(Id, Shapes),
    Rs = RegFind(Items, We),
    Acc = foldl(fun(R, A) -> F(Id, R, We, A) end, Acc0, Rs),
    fold_region_nonbody(RegFind, F, Acc, Shapes, T);
fold_region_nonbody(RegFind, F, Acc, Shapes, []) -> Acc.

%%%
%%% map functions.
%%%

map(F, #st{selmode=body,shapes=Shapes0,sel=Sel}=St) ->
    Shapes = map_body(F, Sel, Shapes0),
    St#st{shapes=Shapes};
map(F, #st{shapes=Shapes0,sel=Sel}=St) ->
    Shapes = map_outer_nonbody(F, Sel, Shapes0),
    St#st{shapes=Shapes}.

map_body(F, [{Id,_}|T], Shapes) ->
    Sh0 = gb_trees:get(Id, Shapes),
    Sh = F(Sh0),
    map_body(F, T, gb_trees:update(Id, Sh, Shapes));
map_body(F, [], Shapes) -> Shapes.

map_outer_nonbody(F, [{Id,Items}|T], Shapes0) ->
    #shape{sh=#we{}=We0} = Sh = gb_trees:get(Id, Shapes0),
    We = map_inner_nonbody(F, gb_sets:iterator(Items), We0),
    Shapes = gb_trees:update(Id, Sh#shape{sh=We}, Shapes0),
    map_outer_nonbody(F, T, Shapes);
map_outer_nonbody(F, [], Shapes) -> Shapes.

map_inner_nonbody(F, Iter0, We0) ->
    case gb_sets:next(Iter0) of
	none -> We0;
	{Item,Iter} ->
	    We = F(Item, We0),
	    map_inner_nonbody(F, Iter, We)
    end.

map_shape(F, #st{selmode=body}=St) -> erlang:fault(badarg, [F,St]);
map_shape(F, #st{shapes=Shapes0,sel=Sel}=St) ->
    Shapes = map_shape_nonbody(F, Sel, Shapes0),
    St#st{shapes=Shapes}.

map_shape_nonbody(F, [{Id,Items}|T], Shapes0) ->
    #shape{sh=#we{}=We0} = Sh = gb_trees:get(Id, Shapes0),
    #we{es=Etab} = We = F(Items, We0),
    Shapes = case gb_trees:is_empty(Etab) of
		 true -> gb_trees:delete(Id, Shapes0);
		 false -> gb_trees:update(Id, Sh#shape{sh=We}, Shapes0)
	     end,
    map_shape_nonbody(F, T, Shapes);
map_shape_nonbody(F, [], Shapes) -> Shapes.

map_region(F, #st{selmode=body}=St) -> erlang:fault(badarg, [F,St]);
map_region(F, #st{selmode=vertex}=St) -> map(F, St);
map_region(F, #st{selmode=edge}=St) -> map(F, St);
map_region(F, #st{shapes=Shapes0,sel=Sel}=St) ->
    Shapes = map_region_nonbody(F, Sel, Shapes0),
    St#st{shapes=Shapes}.

map_region_nonbody(F, [{Id,Faces}|T], Shapes0) ->
    #shape{sh=#we{}=We0} = Sh = gb_trees:get(Id, Shapes0),
    Rs = find_face_regions(Faces, We0),
    #we{es=Etab} = We = foldl(F, We0, Rs),
    Shapes = case gb_trees:is_empty(Etab) of
		 true -> gb_trees:delete(Id, Shapes0);
		 false -> gb_trees:update(Id, Sh#shape{sh=We}, Shapes0)
	     end,
    map_region_nonbody(F, T, Shapes);
map_region_nonbody(F, [], Shapes) -> Shapes.

%%%
%%% mapfold functions.
%%%

mapfold(F, Acc0, #st{selmode=body,sel=Sel,shapes=Shapes0}=St) ->
    {Shapes,Acc} = mapfold_body_sel(F, Acc0, Sel, Shapes0),
    {St#st{shapes=Shapes},Acc};
mapfold(F, Acc0, #st{sel=Sel,shapes=Shapes0}=St) ->
    {Shapes,Acc} = mapfold_outer_nonbody(F, Acc0, Sel, Shapes0),
    {St#st{shapes=Shapes},Acc}.

mapfold_outer_nonbody(F, Acc0, [{Id,Items}|T], Shapes0) ->
    #shape{sh=We0} = Sh0 = gb_trees:get(Id, Shapes0),
    {We,Acc} = mapfold_inner_nonbody(F, Acc0, gb_sets:iterator(Items), We0),
    Sh = Sh0#shape{sh=We},
    Shapes = gb_trees:update(Id, Sh, Shapes0),
    mapfold_outer_nonbody(F, Acc, T, Shapes);
mapfold_outer_nonbody(F, Acc, [], Shapes) -> {Shapes,Acc}.

mapfold_inner_nonbody(F, Acc0, Iter0, We0) ->
    case gb_sets:next(Iter0) of
	none -> {We0,Acc0};
	{Item,Iter} ->
	    {We,Acc} = F(Item, We0, Acc0),
	    mapfold_inner_nonbody(F, Acc, Iter, We)
    end.

mapfold_body_sel(F, Acc0, [{Id,Items}|T], Shapes0) ->
    Sh0 = gb_trees:get(Id, Shapes0),
    {Sh,Acc} = F(Sh0, Acc0),
    Shapes = gb_trees:update(Id, Sh, Shapes0),
    mapfold_body_sel(F, Acc, T, Shapes);
mapfold_body_sel(F, Acc, [], Shapes) -> {Shapes,Acc}.

mapfold_shape(F, Acc0, #st{selmode=body}) ->
    erlang:fault(badarg);
mapfold_shape(F, Acc0, #st{shapes=Shapes0,sel=Sel}=St) ->
    {Shapes,Acc} = mapfold_shape(F, Acc0, Sel, Shapes0),
    {St#st{shapes=Shapes},Acc}.

mapfold_shape(F, Acc0, [{Id,Items}|T], Shapes0) ->
    #shape{sh=#we{}=We0} = Sh = gb_trees:get(Id, Shapes0),
    {We,Acc} = F(Id, Items, We0, Acc0),
    Shapes = gb_trees:update(Id, Sh#shape{sh=We}, Shapes0),
    mapfold_shape(F, Acc, T, Shapes);
mapfold_shape(F, Acc, [], Shapes) -> {Shapes,Acc}.

mapfold_region(F, Acc0, #st{selmode=face,shapes=Shapes0,sel=Sel}=St) ->
    {Shapes,Acc} = mapfold_region_nonbody(F, Acc0, Sel, Shapes0),
    {St#st{shapes=Shapes},Acc};
mapfold_region(F, Acc, St) -> erlang:fault(badarg, [F,Acc,St]).

mapfold_region_nonbody(F, Acc0, [{Id,Faces}|T], Shapes0) ->
    #shape{sh=#we{}=We0} = Sh = gb_trees:get(Id, Shapes0),
    Rs = find_face_regions(Faces, We0),
    {We,Acc} = foldl(fun(R, {W,A}) -> F(Id, R, W, A) end, {We0,Acc0}, Rs),
    Shapes = gb_trees:update(Id, Sh#shape{sh=We}, Shapes0),
    mapfold_region_nonbody(F, Acc, T, Shapes);
mapfold_region_nonbody(F, Acc, [], Shapes) -> {Shapes,Acc}.


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

make_1([#shape{id=Id,sh=#we{vs=Vtab,es=Etab,fs=Ftab}=We}|Shs], Filter, Mode) ->
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
    reverse(wings_sel:fold_shape(
	      fun(#shape{sh=We}, Items, A) ->
		      [e3d_vec:average(bounding_box(Mode, Items, We, none))|A]
	      end, [], St)).

%%%
%%% Calculate the bounding-box for the selection.
%%%

bounding_box(#st{selmode=Mode}=St) ->
    wings_sel:fold_shape(
      fun(#shape{sh=We}, Items, A) ->
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

find_face_regions(Faces, #we{es=Etab,fs=Ftab}) ->
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


find_edge_regions(Edges, We) ->
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

validate_items(Items, Mode, #shape{sh=We}) ->
    validate_items(Items, Mode, We);
validate_items(Vs, vertex, #we{vs=Vtab}) ->
    tab_set_intersection(Vs, Vtab);
validate_items(Es, edge, #we{es=Etab}) ->
    tab_set_intersection(Es, Etab);
validate_items(Fs, face, #we{fs=Ftab}) ->
    tab_set_intersection(Fs, Ftab);
validate_items(Items, body, Shape) -> Items.

%%%
%%% Selection commands.
%%%

select_all(#st{drag=Drag}=St) when Drag =/= undefined -> St;
select_all(#st{selmode=body,shapes=Shapes}=St) ->
    Items = gb_sets:singleton(0),
    Sel = [{Id,Items} || Id <- gb_trees:keys(Shapes)],
    St#st{sel=Sel};
select_all(#st{selmode=Mode,sel=[],shapes=Shapes}=St) ->
    case gb_trees:is_empty(Shapes) of
	true -> St;
	false ->
	    Sel0 = gb_trees:keys(Shapes),
	    Sel = [{Id,get_all_items(Mode, Id, St)} || Id <- Sel0],
	    St#st{sel=Sel}
    end;
select_all(#st{selmode=Mode,sel=Sel0}=St) ->
    Sel = [{Id,get_all_items(Mode, Id, St)} || {Id,_} <- Sel0],
    St#st{sel=Sel}.

get_all_items(Mode, Id, #st{shapes=Shapes}) ->
    #shape{sh=We} = gb_trees:get(Id, Shapes),
    Items = case Mode of
		vertex -> gb_trees:keys(We#we.vs);
		edge -> gb_trees:keys(We#we.es);
		face -> gb_trees:keys(We#we.fs)
	    end,
    gb_sets:from_ordset(Items).

select_more(St) ->
    selection_change(select_more, St).

select_less(St) ->
    selection_change(select_less, St).

selection_change(Change, #st{selmode=vertex}=St) ->
    wings_vertex:Change(St);
selection_change(Change, #st{selmode=edge}=St) ->
    wings_edge:Change(St);
selection_change(Change, #st{selmode=face}=St) ->
    wings_face:Change(St);
selection_change(Change, St) -> St.

save(#st{selmode=Mode,sel=Sel}=St) ->
    St#st{ssel={Mode,Sel}}.

exchange(#st{selmode=Mode,sel=OldSel,ssel={SMode,SSel}}=St) ->
    Sel = valid_sel(SSel, SMode, St),
    St#st{selmode=SMode,sel=Sel,ssel={Mode,OldSel}}.

load(#st{ssel={SMode,SSel}}=St) ->
    Sel = valid_sel(SSel, SMode, St),
    St#st{selmode=SMode,sel=Sel}.

union(#st{selmode=Mode,sel=Sel0,ssel={Mode,Ssel}}=St) ->
    SSel = valid_sel(Ssel, Mode, St),
    Sel = combine_sel(fun(Ss) -> gb_sets:union(Ss) end, Sel0, Ssel),
    St#st{sel=Sel};
union(#st{sel=Sel0}=St) ->			%Different selection modes.
    Ssel = coerce_ssel(St),
    Sel = combine_sel(fun(Ss) -> gb_sets:union(Ss) end, Sel0, Ssel),
    St#st{sel=Sel}.

subtract(#st{selmode=Mode,sel=Sel0,ssel={Mode,Ssel}}=St) ->
    Sel = subtract(Sel0, Ssel),
    St#st{sel=Sel};
subtract(#st{sel=Sel0}=St) ->		%Differenct selection modes.
    Ssel = coerce_ssel(St),
    Sel = subtract(Sel0, Ssel),
    St#st{sel=Sel}.

subtract([{Id1,_}=E1|Es1], [{Id2,_}|Es2]=Set2) when Id1 < Id2 ->
    [E1|subtract(Es1, Set2)];
subtract([{Id1,_}|Es1]=Set1, [{Id2,_}|Es2]) when Id1 > Id2 ->
    subtract(Set1, Es2);
subtract([{Id,E1}|Es1], [{Id,E2}|Es2]) ->	%E1 == E2
    E = gb_sets:subtract(E1, E2),
    case gb_sets:is_empty(E) of
	true -> subtract(Es1, Es2);
	false -> [{Id,E}|subtract(Es1, Es2)]
    end;
subtract([], Es2) -> [];
subtract(Es1, []) -> Es1.

intersection(#st{selmode=Mode,sel=Sel0,ssel={Mode,Ssel}}=St) ->
    Sel = intersection(Sel0, Ssel),
    St#st{sel=Sel};
intersection(#st{sel=Sel0}=St) ->		%Differenct selection modes.
    Ssel = coerce_ssel(St),
    Sel = intersection(Sel0, Ssel),
    St#st{sel=Sel}.

intersection(Sa, Sb) ->
    Empty = gb_sets:empty(),
    combine_sel(fun([_]) -> Empty;
		   (Ss) -> gb_sets:intersection(Ss)
		end, Sa, Sb).
			
combine_sel(Combine, Sa, Sb) ->
    combine_sel(Combine, lists:merge(Sa, Sb)).
combine_sel(Combine, [{Id,Sa},{Id,Sb}|T]) ->
    S = Combine([Sa,Sb]),
    case gb_sets:is_empty(S) of
	true -> combine_sel(Combine, T);
	false -> [{Id,S}|combine_sel(Combine, T)]
    end;
combine_sel(Combine, [{Id,S0}|T]) ->
    S = Combine([S0]),
    case gb_sets:is_empty(S) of
	true -> combine_sel(Combine, T);
	false -> [{Id,S}|combine_sel(Combine, T)]
    end;
combine_sel(Combine, []) -> [].

coerce_ssel(#st{selmode=Mode,ssel={Smode,Ssel0}}=St) ->
    StTemp = St#st{selmode=Smode,sel=valid_sel(Ssel0, Smode, St)},
    #st{sel=Ssel} = convert_selection(Mode, StTemp),
    Ssel.

tab_set_intersection(Set, Tab) ->
    Keys = gb_sets:from_ordset(gb_trees:keys(Tab)),
    gb_sets:intersection(Set, Keys).

%%%
%%% Select Inverse.
%%%

inverse(#st{selmode=body,sel=Sel0,shapes=Shapes}=St) ->
    Zero = gb_sets:singleton(0),
    All = [{Id,Zero} || Id <- gb_trees:keys(Shapes)],
    Sel = ordsets:subtract(All, Sel0),
    St#st{sel=Sel};
inverse(#st{selmode=Mode}=St) ->
    Sel = fold_shape(
	    fun(#shape{id=Id,sh=We}, Items, A) ->
		    Diff = inverse_items(Mode, Items, We),
		    case gb_sets:is_empty(Diff) of
			true -> [{Id,Items}|A];	%Can't inverse.
			false -> [{Id,Diff}|A]
		    end
	    end, [], St),
    St#st{sel=reverse(Sel)}.

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
    
%%%
%%% Select Similar.
%%%

similar(#st{selmode=vertex,sel=[{Id,Sel0}],shapes=Shapes}=St) ->
    #shape{sh=We} = gb_trees:get(Id, Shapes),
    Templates0 = [make_vertex_template(SelI, We) ||
		     SelI <- gb_sets:to_list(Sel0)],
    Templates = ordsets:from_list(Templates0),
    %%io:format("~w/~w\n", [length(Templates0),length(Templates)]),
    make(fun(V, W) ->
		 match_templates(make_vertex_template(V, W), Templates)
	 end, vertex, St);
similar(#st{selmode=edge,sel=[{Id,Sel0}],shapes=Shapes}=St) ->
    #shape{sh=We} = gb_trees:get(Id, Shapes),
    Templates0 = [make_edge_template(SelI, We) ||
		    SelI <- gb_sets:to_list(Sel0)],
    Templates = ordsets:from_list(Templates0),
    %%io:format("~w/~w\n", [length(Templates0),length(Templates)]),
    make(fun(Edge, W) ->
		 match_templates(make_edge_template(Edge, W), Templates)
	 end, edge, St);
similar(#st{selmode=face,sel=[{Id,Sel0}],shapes=Shapes}=St) ->
    #shape{sh=We} = gb_trees:get(Id, Shapes),
    Templates0 = [make_face_template(SelI, We) ||
		     SelI <- gb_sets:to_list(Sel0)],
    Templates = ordsets:from_list(Templates0),
    %%io:format("~w/~w\n", [length(Templates0),length(Templates)]),
    make(fun(Face, WeI) ->
		 match_templates(make_face_template(Face, WeI), Templates)
	 end, face, St);
similar(St) -> St.

match_templates(F, [Template|Ts]) ->
    case match_template(F, Template) of
	true -> true;
	false -> match_templates(F, Ts)
    end;
match_templates(F, []) -> false.

match_template({Len,Ad,As}, {Len,Bd,Bs}) ->
    case rel_compare(Ad, Bd, 1.0E-5) of
	true -> rel_compare(As, Bs, 1.0E-5);
	false -> false
    end;
match_template(_, _) -> false.

make_face_template(Face, #we{vs=Vtab}=We) ->
    Vs = wings_face:fold(
	   fun(V, _, _, Acc0) ->
		   [V|Acc0]
	   end, [], Face, We),
    {DotSum,SqSum} = face_dots_and_sqlens(Vs, Vtab),
    {length(Vs),DotSum,SqSum}.

face_dots_and_sqlens(Vs, Vtab) ->
    Vpos = [wings_vertex:pos(P, Vtab) || P <- Vs],
    face_dots_and_sqlens_1(Vpos).

face_dots_and_sqlens_1([Va,Vb|_]=Vpos) ->
    D = e3d_vec:sub(Va, Vb),
    face_dots_and_sqlens_2(D, Vpos, Vpos, 0, 0).

face_dots_and_sqlens_2(D1, [Va|[Vb,Vc|_]=Vs], More, Dot0, Sq0) ->
    ?ASSERT(D1 == e3d_vec:sub(Va, Vb)),
    D2 = e3d_vec:sub(Vb, Vc),
    Dot = Dot0 + e3d_vec:dot(D1, D2),
    Sq = Sq0 + e3d_vec:dot(D1, D1),
    face_dots_and_sqlens_2(D2, Vs, More, Dot, Sq);
face_dots_and_sqlens_2(D1, Vs, [Va,Vb|_], Dot, Sq) ->
    face_dots_and_sqlens_2(D1, Vs++[Va,Vb], [], Dot, Sq);
face_dots_and_sqlens_2(D1, Other, More, Dot, Sq) -> {Dot,Sq}.

make_edge_template(Edge, #we{vs=Vtab,es=Etab}=We) ->
    #edge{vs=Va,ve=Vb,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} =
	gb_trees:get(Edge, Etab),
    VaPos = wings_vertex:pos(Va, Vtab),
    VbPos = wings_vertex:pos(Vb, Vtab),
    Vec = e3d_vec:sub(VaPos, VbPos),
    DotSum = edge_dot(LP, Vb, VbPos, Vec, We) +
	edge_dot(RS, Vb, VbPos, Vec, We) +
	edge_dot(LS, Va, VaPos, Vec, We) +
	edge_dot(RP, Va, VaPos, Vec, We),
    {0,DotSum,e3d_vec:dot(Vec, Vec)}.

edge_dot(Edge, V, Pos, Vec, #we{es=Etab}=We) ->
    Rec = gb_trees:get(Edge, Etab),
    OtherPos = wings_vertex:other_pos(V, Rec, We),
    ThisVec = e3d_vec:sub(Pos, OtherPos),
    abs(e3d_vec:dot(ThisVec, Vec)).

make_vertex_template(V, #we{vs=Vtab}=We) ->
    Center = wings_vertex:pos(V, Vtab),
    Vecs = wings_vertex:fold(
	     fun(_, _, Rec, Acc0) ->
		     Pos = wings_vertex:other_pos(V, Rec, Vtab),
		     Vec = e3d_vec:sub(Pos, Center),
		     [Vec|Acc0]
	     end, [], V, We),
    {DotSum,SqSum} = vertex_dots_and_sqlens(Vecs, Vecs, 0, 0),
    {length(Vecs),DotSum,SqSum}.

vertex_dots_and_sqlens([VecA|[VecB|_]=T], More, Dot0, Sq0) ->
    Dot = Dot0 + abs(e3d_vec:dot(VecA, VecB)),
    Sq = Sq0 + e3d_vec:dot(VecA, VecA),
    vertex_dots_and_sqlens(T, More, Dot, Sq);
vertex_dots_and_sqlens(Vecs, [VecB|_], Dot, Sq) ->
    vertex_dots_and_sqlens(Vecs++[VecB], [], Dot, Sq);
vertex_dots_and_sqlens(Other, More, Dot, Sq) -> {Dot,Sq}.

rel_compare(A, B, Tresh) when abs(A) < Tresh ->
    abs(B) < Tresh;
rel_compare(A, B, Tresh) when abs(A) > abs(B) ->
    abs(A-B)/abs(A) < Tresh;
rel_compare(A, B, Tresh) ->
    abs(A-B)/abs(B) < Tresh.

%%
%% Select Random.
%%
random(Percent, #st{selmode=body}=St) -> St;
random(Percent, #st{selmode=Mode}=St) ->
    P = Percent / 100,
    make(fun(_, _) -> random:uniform() < P end, Mode, St).
