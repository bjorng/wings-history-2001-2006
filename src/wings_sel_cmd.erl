%%
%%  wings_sel_cmd.erl --
%%
%%     This module implements the commands in the selection menu.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_sel_cmd.erl,v 1.13 2002/04/11 08:20:39 bjorng Exp $
%%

-module(wings_sel_cmd).

-export([menu/3,command/2]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,reverse/2,sort/1,keydelete/3]).

menu(X, Y, St) ->
    Menu = [{"Deselect",deselect},
	    separator,
	    {"More",more},
	    {"Less",less},
	    {"Region",select_region},
	    {"Edge Loop",edge_loop},
	    {"Edge Ring",edge_ring},
	    {"Previous Edge Loop",prev_edge_loop},
	    {"Next Edge Loop",next_edge_loop},
	    {"Similar",similar},
	    separator,
	    {"Adjacent",{adjacent,[{"Vertices",vertex},
				   {"Edges",edge},
				   {"Faces",face}]}},
	    {"By",{by,[{"Hard edges",hard_edges},
		       {"Vertices with",{vertices_with,
					 [{"2 edges",2},
					  {"3 edges",3},
					  {"4 edges",4},
					  {"5 edges",5}]}},
		       {"Faces with",{faces_with,
				      [{"2 edges",2},
				       {"3 edges",3},
				       {"4 edges",4},
				       {"5 or more",5}]}},
		       wings_material:sub_menu(select, St),
		       {"Random",{random,[{"10%",10},
					  {"20%",20},
					  {"30%",30},
					  {"40%",40},
					  {"50%",50},
					  {"60%",60},
					  {"70%",70},
					  {"80%",80},
					  {"90%",90}]}},
		       {"Short edges",short_edges,[option]},
		       {"Id",id}]}},
	    separator,
	    {sel_all_str(St),all},
	    separator,
	    {"Inverse",inverse},
	    separator,
	    {"Store selection",save},
	    {"Recall selection",load},
	    {"Exchange selection",exchange},
	    separator,
	    {"Union with stored",union},
	    {"Subtract with stored",subtract},
	    {"Intersection with stored",intersection}],
    wings_menu:menu(X, Y, select, Menu, St).

sel_all_str(#st{selmode=vertex}) -> "All vertices";
sel_all_str(#st{selmode=edge}) -> "All edges";
sel_all_str(#st{selmode=face}) -> "All faces";
sel_all_str(#st{selmode=body}) -> "All objects".

command(edge_loop, #st{selmode=face}=St) ->
    {save_state,
     wings_sel:convert_shape(
       fun(Faces, We) ->
	       gb_sets:from_list(wings_face:outer_edges(Faces, We))
       end, edge, St)};
command(edge_loop, St) ->
    {save_state,wings_edge_loop:select_loop(St)};
command(edge_ring, St) ->
    {save_state,wings_edge:select_edge_ring(St)};
command(next_edge_loop, St) ->
    {save_state,wings_edge_loop:select_next(St)};
command(prev_edge_loop, St) ->
    {save_state,wings_edge_loop:select_prev(St)};
command(select_region, St) ->
    {save_state,wings_edge:select_region(St)};
command(deselect, St) ->
    {save_state,St#st{sel=[]}};
command(more, St) ->
    select_more(St);
command(less, St) ->
    select_less(St);
command(all, St) ->
    {save_state,select_all(St)};
command({by,Command}, St) ->
    by_command(Command, St);
command(similar, St) ->
    {save_state,similar(St)};
command(save, St) ->
    {save_state,save(St)};
command(load, St) ->
    {save_state,load(St)};
command(exchange, St) ->
    {save_state,exchange(St)};
command(union, St) ->
    {save_state,union(St)};
command(subtract, St) ->
    {save_state,subtract(St)};
command(intersection, St) ->
    {save_state,intersection(St)};
command(inverse, St) ->
    {save_state,inverse(St)};
command({adjacent,Type}, St) ->
    set_select_mode(Type, St);
command(Type, St) ->
    set_select_mode(Type, St).

by_command(hard_edges, St) ->
    Sel = fun(Edge, #we{he=Htab}) ->
		  gb_sets:is_member(Edge, Htab)
	  end,
    {save_state,wings_sel:make(Sel, edge, St)};
by_command({vertices_with,N}, St) ->
    Sel = fun(V, We) ->
		  Cnt = wings_vertex:fold(
			  fun(_, _, _, Cnt) ->
				  Cnt+1
			  end, 0, V, We),
		  Cnt =:= N
	  end, 
    {save_state,wings_sel:make(Sel, vertex, St)};
by_command({faces_with,5}, St) ->
    Sel = fun(Face, We) ->
		    length(wings_face:surrounding_vertices(Face, We)) >= 5
	    end,
    {save_state,wings_sel:make(Sel, face, St)};
by_command({faces_with,N}, St) ->
    Sel = fun(Face, We) ->
		  N =:= length(wings_face:surrounding_vertices(Face, We))
	  end,
    {save_state,wings_sel:make(Sel, face, St)};
by_command({material,_}=Cmd, St) ->
    wings_material:command({select,Cmd}, St);
by_command({random,Percent}, St) ->
    {save_state,random(Percent, St)};
by_command({short_edges,Ask}, St) ->
    short_edges(Ask, St);
by_command(id, St) ->
    by_id(St);
by_command({id,Sel}, St) ->
    {save_state,wings_sel:set(Sel, St)}.

%%%
%%% Selection commands.
%%%

set_select_mode(Type, St) ->
    {save_state,wings_sel:convert_selection(Type, St)}.

select_all(#st{selmode=body,shapes=Shapes}=St) ->
    Items = gb_sets:singleton(0),
    Sel = [{Id,Items} || #we{id=Id,perm=Perm} <- gb_trees:values(Shapes),
			 ?IS_SELECTABLE(Perm)],
    St#st{sel=Sel};
select_all(#st{selmode=Mode,sel=[],shapes=Shapes}=St) ->
    case gb_trees:is_empty(Shapes) of
	true -> St;
	false ->
	    Sel = [{Id,wings_sel:get_all_items(Mode, Id, St)} ||
		      #we{id=Id,perm=Perm} <- gb_trees:values(Shapes),
		      ?IS_SELECTABLE(Perm)],
	    St#st{sel=Sel}
    end;
select_all(#st{selmode=Mode,sel=Sel0}=St) ->
    Sel = [{Id,wings_sel:get_all_items(Mode, Id, St)} || {Id,_} <- Sel0],
    St#st{sel=Sel}.

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
    Sel = wings_sel:valid_sel(SSel, SMode, St),
    St#st{selmode=SMode,sel=Sel,ssel={Mode,OldSel}}.

load(#st{ssel={SMode,SSel}}=St) ->
    Sel = wings_sel:valid_sel(SSel, SMode, St),
    St#st{selmode=SMode,sel=Sel}.

union(#st{selmode=Mode,sel=Sel0,ssel={Mode,Ssel}}=St) ->
    SSel = wings_sel:valid_sel(Ssel, Mode, St),
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
combine_sel(_Combine, []) -> [].

coerce_ssel(#st{selmode=Mode,ssel={Smode,Ssel0}}=St) ->
    StTemp = St#st{selmode=Smode,sel=wings_sel:valid_sel(Ssel0, Smode, St)},
    #st{sel=Ssel} = wings_sel:convert_selection(Mode, StTemp),
    Ssel.

%%%
%%% Select Inverse.
%%%

inverse(#st{selmode=body,sel=Sel0,shapes=Shapes}=St) ->
    Items = gb_sets:singleton(0),
    All = [{Id,Items} || #we{id=Id,perm=Perm} <- gb_trees:values(Shapes),
			 ?IS_SELECTABLE(Perm)],
    Sel = ordsets:subtract(All, Sel0),
    St#st{sel=Sel};
inverse(#st{selmode=Mode}=St) ->
    Sel = wings_sel:fold(
	    fun(Items, #we{id=Id}=We, A) ->
		    Diff = wings_sel:inverse_items(Mode, Items, We),
		    case gb_sets:is_empty(Diff) of
			true -> [{Id,Items}|A];	%Can't inverse.
			false -> [{Id,Diff}|A]
		    end
	    end, [], St),
    St#st{sel=reverse(Sel)}.

%%%
%%% Select Similar.
%%%

similar(#st{selmode=vertex,sel=[{Id,Sel0}],shapes=Shapes}=St) ->
    We = gb_trees:get(Id, Shapes),
    Templates0 = [make_vertex_template(SelI, We) ||
		     SelI <- gb_sets:to_list(Sel0)],
    Templates = ordsets:from_list(Templates0),
    wings_sel:make(
      fun(V, W) ->
	      match_templates(make_vertex_template(V, W), Templates)
      end, vertex, St);
similar(#st{selmode=edge,sel=[{Id,Sel0}],shapes=Shapes}=St) ->
    We = gb_trees:get(Id, Shapes),
    Templates0 = [make_edge_template(SelI, We) ||
		    SelI <- gb_sets:to_list(Sel0)],
    Templates = ordsets:from_list(Templates0),
    wings_sel:make(
      fun(Edge, W) ->
	      match_templates(make_edge_template(Edge, W), Templates)
      end, edge, St);
similar(#st{selmode=face,sel=[{Id,Sel0}],shapes=Shapes}=St) ->
    We = gb_trees:get(Id, Shapes),
    Templates0 = [make_face_template(SelI, We) ||
		     SelI <- gb_sets:to_list(Sel0)],
    Templates = ordsets:from_list(Templates0),
    wings_sel:make(
      fun(Face, WeI) ->
	      match_templates(make_face_template(Face, WeI), Templates)
      end, face, St);
similar(St) -> St.

match_templates(F, [Template|Ts]) ->
    case match_template(F, Template) of
	true -> true;
	false -> match_templates(F, Ts)
    end;
match_templates(_, []) -> false.

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

face_dots_and_sqlens_2(D1, [_|[Vb,Vc|_]=Vs], More, Dot0, Sq0) ->
    D2 = e3d_vec:sub(Vb, Vc),
    Dot = Dot0 + e3d_vec:dot(D1, D2),
    Sq = Sq0 + e3d_vec:dot(D1, D1),
    face_dots_and_sqlens_2(D2, Vs, More, Dot, Sq);
face_dots_and_sqlens_2(D1, Vs, [Va,Vb|_], Dot, Sq) ->
    face_dots_and_sqlens_2(D1, Vs++[Va,Vb], [], Dot, Sq);
face_dots_and_sqlens_2(_D1, _Other, _More, Dot, Sq) -> {Dot,Sq}.

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
vertex_dots_and_sqlens(_Other, _More, Dot, Sq) -> {Dot,Sq}.

rel_compare(A, B, Tresh) when abs(A) < Tresh ->
    abs(B) < Tresh;
rel_compare(A, B, Tresh) when abs(A) > abs(B) ->
    abs(A-B)/abs(A) < Tresh;
rel_compare(A, B, Tresh) ->
    abs(A-B)/abs(B) < Tresh.

%%
%% Select Random.
%%

random(_Percent, #st{selmode=body}=St) -> St;
random(Percent, #st{selmode=Mode}=St) ->
    P = Percent / 100,
    wings_sel:make(fun(_, _) -> random:uniform() < P end, Mode, St).

%%
%% Select by numerical item id.
%%

short_edges(Ask, St) when is_atom(Ask) ->
    Qs = [{label,"Length tolerance"},{text,1.0E-3,[{range,{1.0E-5,10.0}}]}],
    wings_ask:dialog(Ask,
		  [{hframe, Qs, [{title,"Select Short Edges"}]}], St,
		  fun(Res) -> {select,{by,{short_edges,Res}}} end);
short_edges([Tolerance], St0) ->
    St = wings_sel:make(fun(Edge, We) ->
				short_edge(Tolerance, Edge, We)
			end, edge, St0),
    {save_state,St#st{selmode=edge}}.

short_edge(Tolerance, Edge, #we{es=Etab,vs=Vtab}) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    VaPos = wings_vertex:pos(Va, Vtab),
    VbPos = wings_vertex:pos(Vb, Vtab),
    abs(e3d_vec:dist(VaPos, VbPos)) < Tolerance.

%%
%% Select by numerical item id.
%%

by_id(#st{selmode=body}=St) ->
    ask([{"Object Id",0}], St,
	fun([Id]) ->
		valid_sel("", [{Id,gb_sets:singleton(0)}], St)
	end);
by_id(#st{selmode=vertex}=St) ->
    item_by_id("Vertex Id", St);
by_id(#st{selmode=edge}=St) ->
    item_by_id("Edge Id", St);
by_id(#st{selmode=face}=St) ->
    item_by_id("Face Id", St).

item_by_id(Prompt, #st{sel=[{Id,_}]}=St) ->
    ask([{Prompt,0}], St,
	fun([Item]) ->
		valid_sel(Prompt, [{Id,gb_sets:singleton(Item)}], St)
	end);
item_by_id(Prompt, St) ->
    ask([{"Object Id",0},
	 {Prompt,0}], St,
	fun([Id,Item]) ->
		valid_sel(Prompt, [{Id,gb_sets:singleton(Item)}], St)
	end).
    
valid_sel(Prompt, Sel, #st{shapes=Shs,selmode=Mode}=St) ->
    case wings_sel:valid_sel(Sel, Mode, St) of
	[] ->
	    [{Id,Item0}] = Sel,
	    [Item] = gb_sets:to_list(Item0),
	    case gb_trees:is_defined(Id, Shs) of
		false ->
		    throw({command_error,"The Object Id "++
			   integer_to_list(Id)++" is invalid."});
		true ->
		    throw({command_error,"The "++Prompt++" "++
			   integer_to_list(Item)++" is invalid."})
	    end;
	Sel -> Sel
    end.
    
ask(Qs, St, Fun) ->
    wings_ask:ask(Qs, St, fun(Res) ->
				  Sel = Fun(Res),
				  {select,{by,{id,Sel}}}
			  end).
