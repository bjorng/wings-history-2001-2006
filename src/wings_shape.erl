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
%%     $Id: wings_shape.erl,v 1.27 2003/01/01 19:23:56 bjorng Exp $
%%

-module(wings_shape).
-export([new/3,insert/3,replace/3,
	 menu/1,command/2,window/1]).

-define(NEED_ESDL, 1).
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
    reverse(Base, "_" ++ Suffix ++ integer_to_list(Id)).

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
base_1(_Base) -> error.

replace(Id, We0, #st{shapes=Shapes0}=St) ->
    We = We0#we{id=Id},
    Shapes = gb_trees:update(Id, We, Shapes0),
    St#st{shapes=Shapes}.

%%%
%%% Objects menu.
%%%

menu(#st{sel=Sel,shapes=Shapes}) ->
    Menu0 = map(fun(#we{id=Id,perm=Perm,name=Name}) ->
			IsSelected = keymember(Id, 1, Sel),
			NameSt = state(Perm, IsSelected, Name),
			Choices = choices(Perm, IsSelected),
			{NameSt,{Id,Choices}}
		end, gb_trees:values(Shapes)),
    Menu = case Menu0 of
	       [] -> [];
	       _ -> [separator|Menu0]
	   end,
    [{"Show and Unlock All",restore_all},
     {"Hide Selected",hide_selected},
     {"Hide Unselected",hide_unselected},
     {"Lock Unselected",lock_unselected}|Menu].

state(0, IsSel, Name) ->
    state_1(eye, IsSel, Name);
state(1, IsSel, Name) ->
    state_1(lock, IsSel, Name);
state([], IsSel, Name) ->
    state_1(hidden, IsSel, Name);
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
choices([], _IsSel) ->
    [{"Show",restore}];
choices(Sel, _IsSel) when is_tuple(Sel) ->
    [{"Show",restore}].

more_choices() ->
    [{"Hide",hide},{"Hide Others",hide_others},
     {"Lock",lock},{"Rename...",rename}].

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
command({Id,rename}, #st{shapes=Shs}) ->
    #we{name=Name} = gb_trees:get(Id, Shs),
    wings_ask:ask("Rename Object",
		  [{"New Name",Name}],
		  fun([NewName]) -> {objects,{Id,rename,NewName}} end);
command({Id,rename,Name}, #st{shapes=Shapes0}=St) ->
    We = gb_trees:get(Id, Shapes0),
    Shapes = gb_trees:update(Id, We#we{name=Name}, Shapes0),
    {save_state,St#st{shapes=Shapes}};
command({Id,hide_others}, St) ->
    {save_state,hide_others(Id, St)};
command(restore_all, St) ->
    {save_state,restore_all(St)};
command(hide_selected, St) ->
    {save_state,hide_selected(St)};
command(hide_unselected, St) ->
    {save_state,hide_unselected(St)};
command(lock_unselected, St) ->
    {save_state,lock_unselected(St)}.

hide_object(Id, St0) ->
    St = wings_sel:deselect_object(Id, St0),
    update_permission(get_sel(Id, St0), Id, St).

lock_object(Id, St0) ->
    St = wings_sel:deselect_object(Id, St0),
    update_permission(1, Id, St).

restore_object(Id, St) ->
    update_permission(0, Id, St).

update_permission(Perm, Id, #st{shapes=Shs0}=St) ->
    We = gb_trees:get(Id, Shs0),
    Shs = gb_trees:update(Id, We#we{perm=Perm}, Shs0),
    Sel = update_sel(We, St),
    St#st{sel=Sel,shapes=Shs}.

hide_others(ThisId, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when ThisId =:= Id -> {Id,We};
		  (#we{id=Id}=We) ->
		       {Id,We#we{perm=get_sel(Id, St)}}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    Sel = [This || {Id,_}=This <- Sel0, Id =:= ThisId],
    St#st{shapes=Shs,sel=Sel}.

restore_all(#st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = gb_trees:values(Shs0),
    Shs2 = [{Id,We#we{perm=0}} || #we{id=Id}=We <- Shs1],
    Shs = gb_trees:from_orddict(Shs2),
    Sel = sort(restore_all_sel(Shs1, St, Sel0)),
    St#st{shapes=Shs,sel=Sel}.

restore_all_sel([#we{id=Id,perm={Mode,Set}}|T],
		#st{selmode=Mode}=St, Acc) ->
    restore_all_sel(T, St, [{Id,Set}|Acc]);
restore_all_sel([#we{id=Id,perm={SMode,Set0}}|T],
		#st{selmode=Mode}=St, Acc) ->
    StTemp = St#st{selmode=SMode,sel=[{Id,Set0}]},
    #st{sel=[{Id,Set}]} = wings_sel:convert_selection(Mode, StTemp),
    restore_all_sel(T, St, [{Id,Set}|Acc]);
restore_all_sel([_|T], St, Acc) ->
    restore_all_sel(T, St, Acc);
restore_all_sel([], _St, Acc) -> Acc.

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

get_sel(Id, #st{selmode=Mode,sel=Sel}) ->
    case keysearch(Id, 1, Sel) of
	false -> [];
	{value,{Id,Set}} -> {Mode,Set}
    end.

update_sel(#we{id=Id,perm={Mode,Set}}, #st{selmode=Mode,sel=Sel}) ->
    sort([{Id,Set}|Sel]);
update_sel(#we{id=Id,perm={SMode,Elems0}}, #st{selmode=Mode,sel=Sel}=St) ->
    StTemp = St#st{selmode=SMode,sel=[{Id,Elems0}]},
    #st{sel=[{Id,Elems}]} = wings_sel:convert_selection(Mode, StTemp),
    sort([{Id,Elems}|Sel]);
update_sel(_, #st{sel=Sel}) -> Sel.

%%%
%%% Object window.
%%%

-record(ost,
	{st,					%Current St.
	 first,					%First object to show.
	 sel,					%Current selection.
	 os,					%All objects.
	 lh,					%Line height.
	 eye,					%Eye bitmap data.
	 lock					%Lock bitmap data.
	}).

window(St) ->
    case wings_wm:is_window(object) of
	true ->
	    wings_wm:delete(object);
	false ->
	    {_,GeomY,GeomW,GeomH} = wings_wm:viewport(),
	    W = 20*?CHAR_WIDTH,
	    Ost = #ost{first=0,eye=eye_bitmap(),lock=lock_bitmap(),lh=18},
	    Op = {seq,push,event({current_state,St}, Ost)},
	    wings_wm:new(object, {GeomW-W,GeomY,20}, {W,GeomH-50}, Op),
	    wings_wm:new_controller(object, "Objects"),
	    keep
    end.

get_event(Ost) ->
    {replace,fun(Ev) -> event(Ev, Ost) end}.

event(redraw, Ost) ->
    wings_io:ortho_setup(),
    {_,_,W,H} = wings_wm:viewport(),
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    draw_objects(Ost),
    keep;
event({current_state,#st{sel=Sel,shapes=Shs}=St},
      #ost{st=#st{sel=Sel,shapes=Shs}}=Ost) ->
    get_event(Ost#ost{st=St});
event({current_state,#st{sel=Sel,shapes=Shs}=St}, Ost) ->
    wings_wm:dirty(),
    get_event(Ost#ost{st=St,sel=Sel,os=gb_trees:values(Shs)});
event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-1, Ost);
event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(1, Ost);
event(_, _) -> keep.

zoom_step(Dir, #ost{first=First0,os=Objs}=Ost) ->
    L = length(Objs),
    First1 = case First0+20*Dir of
		 F when F < 0 -> 0;
		 F when F > L -> L;
		 F -> F
	     end,
    case First1 of
	First0 -> keep;
	First ->
	    wings_wm:dirty(),
	    get_event(Ost#ost{first=First})
    end.

draw_objects(#ost{os=Objs0,first=First,lh=Lh}=Ost) ->
    Objs = lists:nthtail(First, Objs0),
    Y = Lh-2,
    wings_io:text_at(5, Y, "V"),
    wings_io:text_at(20, Y, "L"),
    wings_io:text_at(37, Y, "Name"),
    {_,_,W,_} = wings_wm:viewport(),
    R = W-13,
    wings_io:text_at(R, Y, "S"),
    draw_objects_1(Objs, Ost, R, 2*Lh-2).

draw_objects_1([#we{id=Id,name=Name,perm=Perm}|Wes],
	       #ost{sel=Sel,lh=Lh,eye=Eye,lock=Lock}=Ost, R, Y) ->
    wings_io:sunken_rect(3, Y-11, 12, 13, ?PANE_COLOR),
    wings_io:sunken_rect(18, Y-11, 12, 13, ?PANE_COLOR),
    wings_io:sunken_rect(R, Y-9, 9, 11, ?PANE_COLOR),
    if
	?IS_VISIBLE(Perm) ->
	    gl:rasterPos2i(5, Y),
	    draw_char(Eye);
	true -> ok
    end,
    if
	?IS_SELECTABLE(Perm) -> ok;
	true ->
    	    gl:rasterPos2i(20, Y),
	    draw_char(Lock)
    end,
    case keymember(Id, 1, Sel) of
	false -> ok;
	true -> wings_io:text_at(R+2, Y, [crossmark])
    end,
    wings_io:text_at(37, Y, Name),
    draw_objects_1(Wes, Ost, R, Y+Lh);
draw_objects_1([], _, _, _) -> ok.

draw_char({A,B,C,D,E,F,Bitmap}) ->
    gl:bitmap(A, B, C, D, E, F, Bitmap).

eye_bitmap() ->
    {11,10,0.0,1.0,13.0,0.0,
     <<2#0001111000000000:16,
      2#0110000110000000:16,
      2#0001111001000000:16,
      2#0011111100100000:16,
      2#0010011100100000:16,
      2#1010111101000000:16,
      2#0111111110000000:16,
      2#0001111100000000:16,
      2#1110000011100000:16,
      2#0001111100000000:16>>}.

lock_bitmap() ->
    {11,9,0.0,0.0,13.0,0.0,
     <<2#1111111111100000:16,
       2#1111101111100000:16,
       2#1111101111100000:16,
       2#1111101111100000:16,
       2#1111111111100000:16,
       2#1111111111100000:16,
       2#0011000110000000:16,
       2#0001101100000000:16,
       2#0000111000000000:16>>}.
