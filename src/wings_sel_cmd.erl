%%
%%  wings_sel_cmd.erl --
%%
%%     This module implements the commands in the selection menu.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_sel_cmd.erl,v 1.53 2004/10/25 14:50:09 dgud Exp $
%%

-module(wings_sel_cmd).

-export([menu/1,command/2]).

%% Utilities.
-export([select_all/1]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,
		keydelete/3,keymember/3,keysearch/3]).

menu(St) ->
    [{?STR(menu,1,"Deselect"),deselect,?STR(menu,2,"Clear the selection")},
     separator,
     {?STR(menu,3,"More"),more,more_help(St)},
     {?STR(menu,4,"Less"),less,less_help(St)},
     {?STR(menu,5,"Similar"),similar,similar_help(St)},
     separator,
     {?STR(menu,6,"Edge Loop"),
      {edge_loop,
       [{?STR(menu,7,"Edge Loop"),
	 edge_loop,?STR(menu,8,"Expand edge selection to loop; ")++
	 ?STR(menu,9,"convert face selection to selected border edges")},
	{?STR(menu,10,"Edge Loop to Region"),edge_loop_to_region,
	 ?STR(menu,11,"Select all faces on one side of an edge loop")},
	{?STR(menu,12,"Edge Ring"),
	 edge_ring,?STR(menu,13,"Expand edge selection to ring")},
	separator,
	{?STR(menu,14,"Previous Edge Loop"),
	 prev_edge_loop,?STR(menu,15,"Select the previous edge loop")},
	{?STR(menu,16,"Next Edge Loop"),
	 next_edge_loop,?STR(menu,17,"Select the next edge loop")},
	separator,
	{?STR(menu,18,"Grow Edge Loop"),edge_link_incr,
	 ?STR(menu,19,"Grow edge selection by one edge in loop directions")},
	{?STR(menu,20,"Shrink Edge Loop"),edge_link_decr,
	 ?STR(menu,21,"Shrink edge selection by one in loop direction")},
	{?STR(menu,22,"Grow Edge Ring"),edge_ring_incr,
	 ?STR(menu,23,"Grow edge selection by one edge in ring direction")},
	{?STR(menu,24,"Shrink Edge Ring"),edge_ring_decr,
	 ?STR(menu,25,"Shrink edge selection by one edge in ring directions")}]}},
     separator,
     {?STR(menu,26,"Adjacent"),
      {adjacent,[{?STR(menu,27,"Vertices"),vertex},
		 {?STR(menu,28,"Edges"),edge},
		 {?STR(menu,29,"Faces"),face},
		 {?STR(menu,30,"Objects"),body}]}},
     {?STR(menu,31,"By"),
      {by,[{?STR(menu,32,"Hard Edges"),
	    hard_edges,?STR(menu,33,"Select all hard edges")},
	   {?STR(menu,34,"Isolated Vertices"),
	    isolated_vertices,?STR(menu,35,"Select all isolated vertices")},
	   {?STR(menu,36,"Vertices With"),
	    {vertices_with,
	     [{?STR(menu,37,"2 Edges"),2},
	      {?STR(menu,38,"3 Edges"),3},
	      {?STR(menu,39,"4 Edges"),4},
	      {?STR(menu,40,"5 Edges"),5}]}},
	   {?STR(menu,41,"Faces With"),
	    {faces_with,
	     [{?STR(menu,42,"2 Edges"),2},
	      {?STR(menu,43,"3 Edges"),3},
	      {?STR(menu,44,"4 Edges"),4},
	      {?STR(menu,45,"5 or More"),5}]}},
	   {?STR(menu,46,"Random"),
	    {random,[{"10%",10},
		     {"20%",20},
		     {"30%",30},
		     {"40%",40},
		     {"50%",50},
		     {"60%",60},
		     {"70%",70},
		     {"80%",80},
		     {"90%",90}]}},
	   {?STR(menu,56,"Short Edges"),
	    short_edges,?STR(menu,57,"Select (too) short edges"),[option]},
	   {?STR(menu,58,"Material Edges"),material_edges,
	    ?STR(menu,59,"Select all edges between different materials")},
	   {?STR(menu,60,"UV-Mapped Faces"),uv_mapped_faces,
	    ?STR(menu,61,"Select all edges that have UV coordinates")},
	   {?STR(menu,62,"Id..."),id,?STR(menu,63,"Select by numeric id")}]}},
     {?STR(menu,64,"Lights"),lights,?STR(menu,65,"Select all lights")},
     separator,
     {sel_all_str(St),all,?STR(menu,66,"Select all elements")},
     separator,
     {?STR(menu,67,"Inverse"),inverse,?STR(menu,68,"Invert the selection")},
     separator,
     {?STR(menu,69,"Hide Selected"),
      hide_selected,
      ?STR(menu,70,"Hide all (partly or wholly) selected objects")},
     {?STR(menu,71,"Hide Unselected"),
      hide_unselected,?STR(menu,72,"Hide objects that have no selection")},
     {?STR(menu,73,"Lock Unselected"),
      lock_unselected,?STR(menu,74,"Lock objects that have no selection")},
     separator,
     {?STR(menu,75,"Show All"),
      show_all,?STR(menu,76,"Show all objects that have been hidden")},
     {?STR(menu,77,"Unlock All"),
      unlock_all,?STR(menu,78,"Unlock all locked objects")},
     separator,
     {?STR(menu,79,"Store Selection"),store_selection,
      ?STR(menu,80,"Store the selection into the selection group named \"StoredSelection\"")},
     {?STR(menu,81,"Recall Selection"),recall_selection,
      ?STR(menu,82,"Recall the selection from the selection group named \"StoredSelection\"")},
     separator,
     {?STR(menu,83,"New Group..."),new_group,?STR(menu,84,"Create a new selection group")}|groups_menu(St)].

sel_all_str(#st{selmode=vertex}) -> ?STR(sel_all_str,1,"All Vertices");
sel_all_str(#st{selmode=edge}) -> ?STR(sel_all_str,2,"All Edges");
sel_all_str(#st{selmode=face}) -> ?STR(sel_all_str,3,"All Faces");
sel_all_str(#st{selmode=body}) -> ?STR(sel_all_str,4,"All Objects").

groups_menu(#st{ssels=Ssels}=St) -> 
    case gb_trees:is_empty(Ssels) of
        true -> [];
        false ->
	    [{?STR(groups_menu,1,"Delete Group"),
	      {delete_group,
	       groups_and_help(?STR(groups_menu,2,"Delete group \""), "\"", St)}},
	     separator,
	     {?STR(groups_menu,4,"Add to Group"),
	      {add_to_group, 
	       groups_and_help(?STR(groups_menu,5,"Add current selection to group \""),"\"", St)}},
	     {?STR(groups_menu,7,"Subtract from Group"),
	      {subtract_from_group,
	       groups_and_help(?STR(groups_menu,8,"Subtract current selection from group \""),"\"", St)}},
	     separator,
	     {?STR(groups_menu,10,"Select Group"),
	      {select_group, 
	       groups_and_help(?STR(groups_menu,11,"Select group \""), "\"", St)}},
	     separator,
	     {?STR(groups_menu,13,"Union Group"),
	      {union_group, 
	       groups_and_help(?STR(groups_menu,14,"Union group \""),?STR(groups_menu,15,"\" with current selection"), St)}},
	     {?STR(groups_menu,16,"Subtract Group"),
	      {subtract_group, 
	       groups_and_help(?STR(groups_menu,17,"Subtract group \""),?STR(groups_menu,18,"\" from current selection"), St)}},
	     {?STR(groups_menu,19,"Intersect Group"),
	      {intersect_group, 
	       groups_and_help(?STR(groups_menu,20,"Intersect group \""),?STR(groups_menu,21,"\" with current selection"), St)}}]
    end.
      
groups_and_help(Help0, Help1, #st{ssels=Ssels}) ->
    map(fun({Mode,Name}=Key) ->
		Title = group_title(Name, Mode),
		{Title,fun(help, _) -> {Help0++Name++Help1};
			  (_, Ns) -> wings_menu:build_command(Key, Ns)
		       end,Help0++Name++Help1}
	end,
	gb_trees:keys(Ssels)).

group_title(Name, vertex) -> ?STR(group_title,1,"vertex: ")++Name;
group_title(Name, edge) -> ?STR(group_title,2,"edge: ")++Name;
group_title(Name, face) -> ?STR(group_title,3,"face: ")++Name;
group_title(Name, body) -> ?STR(group_title,4,"body: ")++Name.

more_help(#st{selmode=vertex}) ->
    ?STR(more_help,1,"Select all vertices adjacent to a selected vertex");
more_help(#st{selmode=edge}) ->
     ?STR(more_help,2,"Select all edges adjacent to a selected edge");
more_help(#st{selmode=face}) ->
     ?STR(more_help,3,"Select all faces sharing a vertex with a selected face");
more_help(_) -> "".

less_help(#st{selmode=vertex}) ->
     ?STR(less_help,1,"Deselect all vertices adjacent to an unselected vertex");
less_help(#st{selmode=edge}) ->
    ?STR(less_help,2,"Deselect all edges adjacent to an unselected edge");
less_help(#st{selmode=face}) ->
    ?STR(less_help,3,"Deselect all faces sharing a vertex with an unselected face");
less_help(_) -> "".

similar_help(#st{selmode=vertex}) ->
    ?STR(similar_help,1,"Select vertices similar to the already selected vertices");
similar_help(#st{selmode=edge}) ->
    ?STR(similar_help,2,"Select edges similar to the already selected edges");
similar_help(#st{selmode=face}) ->
    ?STR(similar_help,3,"Select faces similar to the already selected faces");
similar_help(#st{selmode=body}) ->
    ?STR(similar_help,4,"Select objects with the same number of edges, faces, and vertices");
similar_help(_) -> [].
    
command({edge_loop,edge_loop}, #st{selmode=face}=St) ->
    {save_state,
     wings_sel:convert_shape(
       fun(Faces, We) ->
	       gb_sets:from_list(wings_face:outer_edges(Faces, We))
       end, edge, St)};
command({edge_loop,edge_loop}, St) ->
    {save_state,wings_edge_loop:select_loop(St)};
command({edge_loop,edge_link_decr}, St) ->
    {save_state,wings_edge_loop:select_link_decr(St)};
command({edge_loop,edge_link_incr}, St) ->
    {save_state,wings_edge_loop:select_link_incr(St)};
command({edge_loop,edge_ring}, St) ->
    {save_state,wings_edge:select_edge_ring(St)};
command({edge_loop,edge_ring_incr}, St) ->
    {save_state,wings_edge:select_edge_ring_incr(St)};
command({edge_loop,edge_ring_decr}, St) ->
    {save_state,wings_edge:select_edge_ring_decr(St)};
command({edge_loop,next_edge_loop}, St) ->
    {save_state,wings_edge_loop:select_next(St)};
command({edge_loop,prev_edge_loop}, St) ->
    {save_state,wings_edge_loop:select_prev(St)};
command({edge_loop,edge_loop_to_region}, St) ->
    {save_state,wings_edge:select_region(St)};
command(deselect, St) ->
    {save_state,wings_sel:reset(St)};
command(more, St) ->
    select_more(St);
command(less, St) ->
    select_less(St);
command(all, St) ->
    {save_state,select_all(St)};
command(lights, St) ->
    {save_state,select_lights(St)};
command({by,Command}, St) ->
    by_command(Command, St);
command(similar, St) ->
    {save_state,similar(St)};
command({select_group,Id}, St) ->
    {save_state,select_group(Id, St)};
command({union_group, Id}, St) ->
    {save_state,union_group(Id, St)};
command({subtract_group, Id}, St) ->
    {save_state,subtract_group(Id, St)};
command({intersect_group, Id}, St) ->
    {save_state,intersect_group(Id, St)};
command({add_to_group, Id}, St) ->
    {save_state,add_to_group(Id, St)};
command({subtract_from_group, Id}, St) ->
    {save_state,subtract_from_group(Id, St)};
command({new_group_name, Name}, St) ->
    {save_state,new_group_name(Name, St)};
command(new_group, St) ->
    new_group(St);
command({delete_group,Id}, #st{ssels=Ssels}=St) ->
    {save_state,St#st{ssels=gb_trees:delete(Id, Ssels)}};
command(inverse, St) ->
    {save_state,inverse(St)};
command(hide_selected, St) ->
    {save_state,hide_selected(St)};
command(hide_unselected, St) ->
    {save_state,hide_unselected(St)};
command(lock_unselected, St) ->
    {save_state,lock_unselected(St)};
command(show_all, St) ->
    {save_state,wings_shape:show_all(St)};
command(unlock_all, St) ->
    {save_state,wings_shape:unlock_all(St)};
command({adjacent,Type}, St) ->
    set_select_mode(Type, St);
command(store_selection, #st{ssels=Ssels0,selmode=Mode,sel=Sel}=St) ->
    Key = {Mode,"StoredSelection"},
    Ssels = gb_trees:enter(Key, Sel, Ssels0),
    {save_state,St#st{ssels=Ssels}};
command(recall_selection, #st{selmode=Mode,ssels=Ssels}=St0) ->
    Key = {Mode, "StoredSelection"},
    case gb_trees:is_defined(Key, Ssels) of
	false -> St0;
	true ->
	    St = select_group(Key, St0),
	    {save_state,St}
    end;
command(Type, St) ->
    set_select_mode(Type, St).

by_command(hard_edges, St) ->
    Sel = fun(Edge, #we{he=Htab}) ->
		  gb_sets:is_member(Edge, Htab)
	  end,
    {save_state,wings_sel:make(Sel, edge, St)};
by_command(isolated_vertices, St) ->
    {save_state,select_isolated(St)};
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
		  wings_face:vertices(Face, We) >= 5
	    end,
    {save_state,wings_sel:make(Sel, face, St)};
by_command({faces_with,N}, St) ->
    Sel = fun(Face, We) ->
		  N =:= wings_face:vertices(Face, We)
	  end,
    {save_state,wings_sel:make(Sel, face, St)};
by_command(material_edges, St) ->
    material_edges(St);
by_command({random,Percent}, St) ->
    {save_state,random(Percent, St)};
by_command({short_edges,Ask}, St) ->
    short_edges(Ask, St);
by_command(uv_mapped_faces, St) ->
    uv_mapped_faces(St);
by_command(id, St) ->
    by_id(St);
by_command({id,Sel}, St) ->
    {save_state,sel_by_id(Sel, St)}.

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
    Sel = [{Id,wings_sel:get_all_items(Mode, We)} ||
	      #we{id=Id,perm=Perm}=We <- gb_trees:values(Shapes),
	      ?IS_SELECTABLE(Perm)],
    St#st{sel=Sel};
select_all(#st{selmode=Mode,sel=Sel0}=St) ->
    Sel = [{Id,wings_sel:get_all_items(Mode, Id, St)} || {Id,_} <- Sel0],
    St#st{sel=Sel}.

select_more(#st{selmode=vertex}=St) ->
    wings_vertex:select_more(St);
select_more(#st{selmode=edge}=St) ->
    wings_edge:select_more(St);
select_more(#st{selmode=face}=St) ->
    wings_face:select_more(St);
select_more(St) -> St.

select_less(#st{selmode=vertex}=St) ->
    wings_vertex:select_less(St);
select_less(#st{selmode=edge}=St) ->
    wings_edge:select_less(St);
select_less(#st{selmode=face}=St) ->
    wings_face:select_less(St);
select_less(St) -> St.

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
%%% Hide Selected
%%% Hide Unselected
%%% Lock Unselected
%%%

hide_selected(#st{selmode=Mode,shapes=Shs0,sel=Sel}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) ->
		       case keysearch(Id, 1, Sel) of
			   false -> {Id,We};
			   {value,{_,Set}} -> {Id,We#we{perm={Mode,Set}}}
		       end
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    St#st{shapes=Shs,sel=[]}.

hide_unselected(St) ->
    update_unsel([], St).

lock_unselected(St) ->
    update_unsel(1, St).

update_unsel(Perm, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = map(fun(#we{id=Id,perm=0}=We) ->
		       case keymember(Id, 1, Sel) of
			   true -> {Id,We};
			   false -> {Id,We#we{perm=Perm}}
		       end;
		  (#we{id=Id}=We) -> {Id,We}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    St#st{shapes=Shs}.
    
%%%
%%% Selection Groups
%%%

union_group(Key, #st{sel=Sel0}=St) ->
    Ssel = coerce_ssel(Key, St),
    Sel = union(Sel0, Ssel),
    St#st{sel=Sel}.

union(Sa, Sb) ->
    combine_sel(fun(Ss) -> gb_sets:union(Ss) end, Sa, Sb).

subtract_group(Key, #st{sel=Sel0}=St) ->		
    Ssel = coerce_ssel(Key, St),
    Sel = subtract(Sel0, Ssel),
    St#st{sel=Sel}.

subtract([{Id1,_}=E1|Es1], [{Id2,_}|_]=Set2) when Id1 < Id2 ->
    [E1|subtract(Es1, Set2)];
subtract([{Id1,_}|_]=Set1, [{Id2,_}|Es2]) when Id1 > Id2 ->
    subtract(Set1, Es2);
subtract([{Id,E1}|Es1], [{Id,E2}|Es2]) ->	%E1 == E2
    E = gb_sets:subtract(E1, E2),
    case gb_sets:is_empty(E) of
	true -> subtract(Es1, Es2);
	false -> [{Id,E}|subtract(Es1, Es2)]
    end;
subtract([], _Es2) -> [];
subtract(Es1, []) -> Es1.

intersect_group(Key, #st{sel=Sel0}=St) ->
    Ssel = coerce_ssel(Key, St),
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

coerce_ssel({Mode,_}=Key, #st{ssels=Ssels}=St) ->
    Ssel = gb_trees:get(Key, Ssels),
    coerce_ssel(Mode, Ssel, St).
    
coerce_ssel(Mode, Ssel, #st{selmode=Mode}) -> Ssel;
coerce_ssel(Smode, Ssel0, #st{selmode=Mode}=St) ->
    StTemp = St#st{selmode=Smode,sel=wings_sel:valid_sel(Ssel0, Smode, St)},
    #st{sel=Ssel} = wings_sel:convert_selection(Mode, StTemp),
    Ssel.

select_group({Mode,_}=Key, #st{ssels=Ssels}=St) ->
    Ssel = gb_trees:get(Key, Ssels),
    ValidSel = wings_sel:valid_sel(Ssel, Mode, St),
    St#st{selmode=Mode,sel=ValidSel}.
  
add_to_group({Mode,_}=Key, #st{ssels=Ssels}=St) ->
    Ssel0 = gb_trees:get(Key, Ssels),
    Ssel1 = wings_sel:valid_sel(Ssel0, Mode, St),
    #st{sel=Sel} = possibly_convert(Mode, St),
    Ssel = union(Ssel1, Sel),
    save_group(Key, Ssel, St).

subtract_from_group({Mode,_}=Key, #st{ssels=Ssels}=St) ->
    Ssel0 = gb_trees:get(Key, Ssels),
    Ssel1 = wings_sel:valid_sel(Ssel0, Mode, St),
    #st{sel=Sel} = possibly_convert(Mode, St),
    Ssel = subtract(Ssel1, Sel),
    save_group(Key, Ssel, St).
    
possibly_convert(Mode, #st{selmode=Mode}=St) -> St;
possibly_convert(Mode, St) -> wings_sel:convert_selection(Mode, St).
    
save_group(Key, Sel, #st{ssels=Ssels0}=St) ->
    Ssels = gb_trees:update(Key, Sel, Ssels0),
    St#st{ssels=Ssels}.
    
new_group(_) ->
    wings_ask:ask(?STR(new_group,1,"Create New Group"),
		  [{?STR(new_group,2,"Group Name"), ""}],
		  fun([String]) -> {select,{new_group_name,String}} end).

new_group_name(Name, #st{ssels=Ssels0,selmode=Mode,sel=Sel}=St) ->
    Key = {Mode,Name},
    case gb_trees:is_defined(Key, Ssels0) of
	false -> ok;
	true ->
	    wings_util:error(?STR(new_group_name,1,"~s selection group ~p already exists."),
			     [wings_util:cap(atom_to_list(Mode)),
			      Name])
    end,
    Ssels = gb_trees:insert(Key, Sel, Ssels0),
    St#st{ssels=Ssels}.

%%%
%%% Select Similar.
%%%

similar(#st{selmode=vertex}=St) ->
    Seed = wings_sel:fold(fun(Sel0, We, A) ->
				  [make_vertex_template(SelI, We) ||
				      SelI <- gb_sets:to_list(Sel0)] ++ A
			  end, [], St),
    Templates = ordsets:from_list(Seed),
    wings_sel:make(
      fun(V, W) ->
	      match_templates(make_vertex_template(V, W), Templates)
      end, vertex, St);
similar(#st{selmode=edge}=St) ->
    Seed = wings_sel:fold(fun(Sel0, We, A) ->
				  [make_edge_template(SelI, We) ||
				      SelI <- gb_sets:to_list(Sel0)] ++ A
			  end, [], St),
    Templates = ordsets:from_list(Seed),
    wings_sel:make(
      fun(Edge, W) ->
	      match_templates(make_edge_template(Edge, W), Templates)
      end, edge, St);
similar(#st{selmode=face}=St) ->
    Seed = wings_sel:fold(fun(Sel0, We, A) ->
				  [make_face_template(SelI, We) ||
				      SelI <- gb_sets:to_list(Sel0)] ++ A
			  end, [], St),
    Templates = ordsets:from_list(Seed),
    wings_sel:make(
      fun(Face, WeI) ->
	      match_templates(make_face_template(Face, WeI), Templates)
      end, face, St);
similar(#st{selmode=body}=St) ->
    Template0 = wings_sel:fold(fun(_, #we{vp=Vtab,es=Etab,fs=Ftab}, Acc) ->
				       [{gb_trees:size(Vtab),
					 gb_trees:size(Etab),
					 gb_trees:size(Ftab)}|Acc]
			       end, [], St),
    Template = ordsets:from_list(Template0),
    wings_sel:make(fun(_, We) -> match_body(Template, We) end, body, St).

match_body(Template, #we{vp=Vtab,es=Etab,fs=Ftab}) ->
    Sizes = {gb_trees:size(Vtab),gb_trees:size(Etab),gb_trees:size(Ftab)},
    match_body_1(Template, Sizes).

match_body_1([Sizes|_], Sizes) -> true;
match_body_1([_|T], Sizes) -> match_body_1(T, Sizes);
match_body_1([], _) -> false.
    
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

make_face_template(Face, #we{vp=Vtab}=We) ->
    Vs = wings_face:fold(
	   fun(V, _, _, Acc0) ->
		   [V|Acc0]
	   end, [], Face, We),
    {DotSum,SqSum} = face_dots_and_sqlens(Vs, Vtab),
    {length(Vs),DotSum,SqSum}.

face_dots_and_sqlens(Vs, Vtab) ->
    Vpos = [gb_trees:get(P, Vtab) || P <- Vs],
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

make_edge_template(Edge, #we{vp=Vtab,es=Etab}=We) ->
    #edge{vs=Va,ve=Vb,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} =
	gb_trees:get(Edge, Etab),
    VaPos = gb_trees:get(Va, Vtab),
    VbPos = gb_trees:get(Vb, Vtab),
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

make_vertex_template(V, #we{vp=Vtab}=We) ->
    Center = gb_trees:get(V, Vtab),
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

random(Percent, #st{selmode=Mode}=St) ->
    P = Percent / 100,
    wings_sel:make(fun(_, _) -> random:uniform() < P end, Mode, St).

%%
%% Select short edges.
%%

short_edges(Ask, _St) when is_atom(Ask) ->
    Qs = [{label,?STR(short_edges,1,"Length tolerance")},{text,1.0E-3,[{range,{1.0E-5,10.0}}]}],
	 wings_ask:dialog(Ask, ?STR(short_edges,2,"Select Short Edges"),
		     [{hframe,Qs}],
		     fun(Res) -> {select,{by,{short_edges,Res}}} end);
short_edges([Tolerance], St0) ->
    St = wings_sel:make(fun(Edge, We) ->
				short_edge(Tolerance, Edge, We)
			end, edge, St0),
    {save_state,St#st{selmode=edge}}.

short_edge(Tolerance, Edge, #we{es=Etab,vp=Vtab}) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    VaPos = gb_trees:get(Va, Vtab),
    VbPos = gb_trees:get(Vb, Vtab),
    abs(e3d_vec:dist(VaPos, VbPos)) < Tolerance.

%%
%% Select all edges between materials.
%%

material_edges(St) ->
    wings_sel:make(fun material_edges_fun/2, edge, St).

material_edges_fun(E, #we{es=Etab}=We) ->
    #edge{lf=Lf,rf=Rf} = gb_trees:get(E, Etab),
    wings_material:get(Lf, We) =/= wings_material:get(Rf, We).

%%
%% Select all faces that have UV coordinates.
%%

uv_mapped_faces(St) ->
    wings_sel:make(fun is_uv_mapped_face/2, face, St).

is_uv_mapped_face(Face, We) ->
    is_uv_mapped_face_1(wings_face:vertex_info(Face, We)).

is_uv_mapped_face_1([{_,_}|T]) ->
    is_uv_mapped_face_1(T);
is_uv_mapped_face_1([_|_]) -> false;
is_uv_mapped_face_1([]) -> true.

%%
%% Select by numerical item id.
%%

by_id(#st{selmode=body}=St) ->
    ask([{"Object Id",1}],
	fun([Id]) ->
		valid_sel("", [{Id,gb_sets:singleton(0)}], St)
	end);
by_id(#st{selmode=vertex}=St) ->
    item_by_id("Vertex Id", St);
by_id(#st{selmode=edge}=St) ->
    item_by_id("Edge Id", St);
by_id(#st{selmode=face}=St) ->
    item_by_id("Face Id", St).

item_by_id(Prompt, #st{sel=[{Id,_}]}) ->
    ask([{Prompt,0}],
	fun([Item]) ->
		{Prompt,[{Id,gb_sets:singleton(Item)}]}
	end);
item_by_id(Prompt, #st{shapes=Shs}) ->
    case gb_trees:to_list(Shs) of
	[] -> wings_util:error(?STR(item_by_id,1,"Nothing to select."));
	[{Id,_}] ->
	    ask([{Prompt,0}],
		fun([Item]) ->
			{Prompt,[{Id,gb_sets:singleton(Item)}]}
		end);
	[{Id0,_}|_] ->
	    ask([{?STR(item_by_id,2,"Object Id"),Id0},
		 {Prompt,0}],
		fun([Id,Item]) ->
			{Prompt,[{Id,gb_sets:singleton(Item)}]}
		end)
    end.

sel_by_id({Prompt,Sel}, St) ->
    wings_sel:set(valid_sel(Prompt, Sel, St), St).

valid_sel(Prompt, Sel, #st{shapes=Shs,selmode=Mode}=St) ->
    case wings_sel:valid_sel(Sel, Mode, St) of
	[] ->
	    [{Id,Item0}] = Sel,
	    [Item] = gb_sets:to_list(Item0),
	    case gb_trees:is_defined(Id, Shs) of
		false ->
		    wings_util:error(?STR(valid_sel,1,"The Object Id ")++
				     integer_to_list(Id)++?STR(valid_sel,2," is invalid."));
		true ->
		    wings_util:error(?STR(valid_sel,3,"The ")++Prompt++" "++
				     integer_to_list(Item)++?STR(valid_sel,4," is invalid."))
	    end;
	Sel -> Sel
    end.
    
ask(Qs, Fun) ->
    wings_ask:ask(?STR(ask,1,"Select By Id"), Qs,
		  fun(Res) ->
			  Sel = Fun(Res),
			  {select,{by,{id,Sel}}}
		  end).

%%%
%%% Select lights.
%%%

select_lights(#st{selmode=Mode,shapes=Shapes}=St) ->
    Sel = select_lights_1(gb_trees:values(Shapes), Mode),
    St#st{selmode=Mode,sel=Sel}.

select_lights_1([#we{perm=Perm}|Shs], Mode) when ?IS_NOT_SELECTABLE(Perm) ->
    select_lights_1(Shs, Mode);
select_lights_1([We|Shs], Mode) when not ?IS_LIGHT(We) ->
    select_lights_1(Shs, Mode);
select_lights_1([#we{id=Id}|Shs], body) ->
    [{Id,gb_sets:singleton(0)}|select_lights_1(Shs, body)];
select_lights_1([#we{id=Id,vp=Vtab,es=Etab,fs=Ftab}|Shs], Mode) ->
    Tab = case Mode of
	      vertex -> Vtab;
	      edge -> Etab;
	      face -> Ftab
	  end,
    Sel = gb_trees:keys(Tab),
    [{Id,gb_sets:from_ordset(Sel)}|select_lights_1(Shs, Mode)];
select_lights_1([], _) -> [].

%%%
%%% Select isolated vertices.
%%%

select_isolated(#st{shapes=Shs}=St) ->
    Sel = foldl(fun(#we{id=Id}=We, A) ->
			Isolated = gb_sets:from_list(wings_vertex:isolated(We)),
			case gb_sets:is_empty(Isolated) of
			    true -> A;
			    false -> [{Id,Isolated}|A]
			end
		end, [], gb_trees:values(Shs)),
    wings_sel:set(vertex, Sel, St).
