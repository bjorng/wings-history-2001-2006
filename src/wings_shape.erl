%%
%%  wings_shape.erl --
%%
%%     Utilities for shape records.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_shape.erl,v 1.15 2002/02/03 07:20:09 bjorng Exp $
%%

-module(wings_shape).
-export([new/3,insert/3,replace/3,
	 menu/3,command/2]).

-include("wings.hrl").
-import(lists, [map/2,reverse/1,reverse/2,keymember/3,keysearch/3,sort/1]).

new(Name, We0, #st{shapes=Shapes0,onext=Oid}=St) ->
    We = We0#we{name=Name,id=Oid},
    Shapes = gb_trees:insert(Oid, We, Shapes0),
    St#st{shapes=Shapes,onext=Oid+1}.

insert(#we{name=OldName}=We0, Suffix, #st{shapes=Shapes0,onext=Oid}=St) ->
    Name = new_name(OldName, Suffix, Oid),
    We = We0#we{id=Oid,name=Name},
    Shapes = gb_trees:insert(Oid, We, Shapes0),
    St#st{shapes=Shapes,onext=Oid+1}.
    
new_name(OldName, Suffix, Id) ->
    Base = base(reverse(OldName)),
    Name = reverse(Base, "_" ++ Suffix ++ integer_to_list(Id)).

base(OldName) ->
    case base_1(OldName) of
	error -> OldName;
	Base -> Base
    end.

base_1([H|T]) when $0 =< H, H =< $9 -> base_1(T);
base_1("ypoc_"++Base) -> Base;			%"_copy"
base_1("tcartxe_"++Base) -> Base;		%"_extract"
base_1("pes_"++Base) -> Base;			%"_sep"
base_1("tuc_"++Base) -> Base;			%"_cut"
base_1(Base) -> error.

replace(Id, We0, #st{shapes=Shapes0}=St) ->
    We = We0#we{id=Id},
    Shapes = gb_trees:update(Id, We, Shapes0),
    St#st{shapes=Shapes}.

%%%
%%% Objects menu.
%%%

menu(X, Y, #st{sel=Sel,shapes=Shapes}=St) ->
    Menu0 = map(fun(#we{id=Id,perm=Perm,name=Name}) ->
			IsSelected = keymember(Id, 1, Sel),
			NameSt = state(Perm, IsSelected, Name),
			Choices = choices(Perm, IsSelected),
			{NameSt,{Id,Choices}}
		end, gb_trees:values(Shapes)),
    Menu1 = case Menu0 of
		[] -> [];
		_ -> [separator|Menu0]
	    end,
    Menu2 = [{"Show And Unlock All",restore_all},
	     {"Hide Unselected",hide_unselected},
	     {"Lock Unselected",lock_unselected}|Menu1],
    Menu = Menu2,
    wings_menu:menu(X, Y, objects, Menu, St).

state(0, IsSel, Name) ->
    state_1(eye, IsSel, Name);
state(1, IsSel, Name) ->
    state_1(lock, IsSel, Name);
state(Sel, IsSel, Name) when is_tuple(Sel) ->
    state_1(hidden, IsSel, Name).

state_1(C, false, Name) -> [C|Name];
state_1(C, true, Name) -> "<" ++ [C] ++ "> " ++ Name.

choices(0, false) ->
    [{"Select",select}|more_choices()];
choices(0, true) ->
    [{"Deselect",deselect}|more_choices()];
choices(1, _Sel) ->
    [{"Unlock",restore}];
choices(Sel, _IsSel) when is_tuple(Sel) ->
    [{"Show",restore}].

more_choices() ->
    [{"Hide",hide},{"Hide Others",hide_others},
     {"Lock",lock},{"Rename",rename}].

command({Id,select}, St) ->
    {save_state,wings_sel:select_object(Id, St)};
command({Id,deselect}, St) ->
    {save_state,wings_sel:deselect_object(Id, St)};
command({Id,hide}, St) ->
    {save_state,hide_object(Id, St)};
command({Id,lock}, St) ->
    {save_state,lock_object(Id, St)};
command({Id,restore}, St) ->
    {save_state,restore_object(Id, St)};
command({Id,rename}, St) ->
    {save_state,rename_object(Id, St)};
command({Id,hide_others}, St) ->
    {save_state,hide_others(Id, St)};
command(restore_all, St) ->
    {save_state,restore_all(St)};
command(hide_unselected, St) ->
    {save_state,hide_unselected(St)};
command(lock_unselected, St) ->
    {save_state,lock_unselected(St)}.

rename_object(Id, #st{shapes=Shapes0}=St) ->
    #we{name=Name0} = We = gb_trees:get(Id, Shapes0),
    case wings_getline:string("New name: ", Name0) of
	aborted -> St;
	Name when list(Name) ->
	    Shapes = gb_trees:update(Id, We#we{name=Name}, Shapes0),
	    St#st{shapes=Shapes}
    end.

hide_object(Id, St0) ->
    St = wings_sel:deselect_object(Id, St0),
    update_permission(get_sel(Id, St0), Id, St).

lock_object(Id, St0) ->
    St = wings_sel:deselect_object(Id, St0),
    update_permission(1, Id, St).

restore_object(Id, St) ->
    update_permission(0, Id, St).

update_permission(Perm, Id, #st{sel=Sel0,shapes=Shs0}=St) ->
    We = gb_trees:get(Id, Shs0),
    Shs = gb_trees:update(Id, We#we{perm=Perm}, Shs0),
    Sel = update_sel(We, Sel0),
    wings_draw:model_changed(St#st{sel=Sel,shapes=Shs}).

hide_others(ThisId, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when ThisId =:= Id -> {Id,We};
		  (#we{id=Id}=We) ->
		       {Id,We#we{perm=get_sel(Id, St)}}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    Sel = [This || {Id,_}=This <- Sel0, Id =:= ThisId],
    wings_draw:model_changed(St#st{shapes=Shs,sel=Sel}).

restore_all(#st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = [{Id,We#we{perm=0}} || #we{id=Id}=We <- gb_trees:values(Shs0)],
    Shs = gb_trees:from_orddict(Shs1),
    Sel = [{Id,Set} || #we{id=Id,perm=Set}=We <- gb_trees:values(Shs0),
		       is_tuple(Set)] ++ Sel0,
    wings_draw:model_changed(St#st{shapes=Shs,sel=sort(Sel)}).

hide_unselected(St) ->
    update_unsel(gb_sets:empty(), St).

lock_unselected(St) ->
    update_unsel(1, St).

update_unsel(Perm, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) ->
		       case keymember(Id, 1, Sel) of
			   true -> {Id,We};
			   false -> {Id,We#we{perm=Perm}}
		       end
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    wings_draw:model_changed(St#st{shapes=Shs}).

get_sel(Id, #st{sel=Sel}) ->
    case keysearch(Id, 1, Sel) of
	false -> [];
	{value,{Id,Set}} -> Set
    end.

update_sel(#we{id=Id,perm=Set}, Sel) when is_tuple(Set) ->
    sort([{Id,Set}|Sel]);
update_sel(_, Sel) -> Sel.
