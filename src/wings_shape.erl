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
%%     $Id: wings_shape.erl,v 1.29 2003/01/01 23:20:24 bjorng Exp $
%%

-module(wings_shape).
-export([new/3,insert/3,replace/3,window/1]).

-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [map/2,reverse/1,reverse/2,keymember/3,keysearch/3,sort/1]).
-compile(inline).

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
%%% Object window.
%%%
-record(ost,
	{st,					%Current St.
	 first,					%First object to show.
	 sel,					%Current selection.
	 os,					%All objects.
	 active,				%Number of active object.
	 lh,					%Line height.
	 op,					%Latest operation.
	 eye,					%Eye bitmap data.
	 lock					%Lock bitmap data.
	}).

window(St) ->
    case wings_wm:is_window(object) of
	true ->
	    wings_wm:delete(object);
	false ->
	    {_,GeomY,GeomW,GeomH} = wings_wm:viewport(),
	    W = 24*?CHAR_WIDTH,
	    Ost = #ost{first=0,eye=eye_bitmap(),lock=lock_bitmap(),lh=18,active=-1},
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
event({new_name,Id,Name}, #ost{st=#st{shapes=Shs0}=St}) ->
    We = gb_trees:get(Id, Shs0),
    Shs = gb_trees:update(Id, We#we{name=Name}, Shs0),
    wings_wm:send(geom, {new_state,St#st{shapes=Shs}}),
    keep;
event(#mousemotion{x=X,y=Y,state=State}, #ost{active=Act0}=Ost0) ->
    Act = active_object(Y, Ost0),
    Field = active_field(X),
    help(Act, active_field(X)),
    case Act of
	Act0 -> keep;
	Act ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{active=Act},
	    case State of
		?SDL_PRESSED ->
		    repeat_latest(Field, Ost);
		?SDL_RELEASED -> ok
	    end,
	    get_event(Ost)
    end;
event(#mousebutton{x=X,y=Y,button=B,state=?SDL_PRESSED}, Ost) ->
    do_action(X, Y, B, Ost);
event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-1, Ost);
event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(1, Ost);
event(_, _) -> keep.

help(-1, _) -> wings_wm:message("");
help(_, visibility) ->
    wings_wm:message("[L] Toggle visibility of active object  "
		     "[Alt]+[L] Toggle visibility of all other objects");
help(_, lock) ->
    wings_wm:message("[L] Lock/unlock active object  "
		     "[Alt]+[L] Lock/unlock all objects");
help(_, name) ->
    wings_wm:message("[R] Rename object");
help(_, selection) ->
    wings_wm:message("[L] Toggle selection for active object  "
		     "[Alt]+[L] Toggle selection for all other objects").

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

active_object(Y0, #ost{lh=Lh,first=First,os=Objs}) ->
    case Y0 - top_of_first_object() of
	Y when Y < 0 -> -1;
	Y1 ->
	    case Y1 div Lh of
		Y when First+Y < length(Objs) -> Y;
		_ -> -1
	    end
    end.

active_field(X) ->
    LockPos = lock_pos(),
    NamePos = name_pos(),
    R = right_pos(),
    if
	X < LockPos -> visibility;
	X < NamePos -> lock;
	X < R -> name;
	true -> selection
    end.

do_action(X, Y, Button, #ost{first=First,os=Objs}=Ost) ->
    case active_object(Y, Ost) of
	-1 -> keep;
	Obj ->
	    We = lists:nth(First+Obj+1, Objs),
	    Field = active_field(X),
	    do_action_1(Field, Button, We, Ost)
    end.

do_action_1(visibility, 1, We, Ost) -> toggle_visibility(We, Ost);
do_action_1(lock, 1, We, Ost) -> toggle_lock(We, Ost);
do_action_1(name, 3, We, _Ost) -> rename_object(We);
do_action_1(selection, 1, We, Ost) -> toggle_sel(We, Ost);
do_action_1(_, _, _, _) -> keep.

toggle_visibility(#we{id=Id,perm=Perm}=We, #ost{st=St0}=Ost) ->
    case wings_wm:me_modifiers() of
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    toggle_visibility_all(We, Ost),
	    get_event(Ost#ost{op=none});
	_ ->
	    {Op,St} = if
			  ?IS_VISIBLE(Perm) -> 
			      {hide,hide_object(Id, St0)};
			  true ->
			      {show,restore_object(Id, St0)}
		      end,
	    wings_wm:send(geom, {new_state,St}),
	    get_event(Ost#ost{op=Op})
    end.

toggle_visibility_all(#we{id=Id}, #ost{os=Objs,st=St0}) ->
    St = case are_all_visible(Objs, Id) of
	     false -> restore_all(St0);
	     true -> hide_others(Id, St0)
	 end,
    wings_wm:send(geom, {new_state,St}).

are_all_visible([#we{id=Id}|T], Id) ->
    are_all_visible(T, Id);
are_all_visible([#we{perm=P}|T], Id) ->
    case ?IS_VISIBLE(P) of
	false -> false;
	true -> are_all_visible(T, Id)
    end;
are_all_visible([], _) -> true.

toggle_lock(#we{id=Id,perm=Perm}, #ost{st=St0}=Ost) ->
    case wings_wm:me_modifiers() of
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    io:format("All\n"),
	    get_event(Ost#ost{op=none});
	_ ->
	    if
		?IS_NOT_VISIBLE(Perm) ->
		    keep;
		?IS_SELECTABLE(Perm) ->
		    wings_wm:send(geom, {new_state,lock_object(Id, St0)}),
		    get_event(Ost#ost{op=lock});
		true ->
		    wings_wm:send(geom, {new_state,restore_object(Id, St0)}),
		    get_event(Ost#ost{op=unlock})
	    end
    end.
rename_object(#we{id=Id,name=Name}) ->
    wings_ask:ask("Rename Object",
		  [{"New Name",Name}],
		  fun([NewName]) when NewName =/= Name ->
			  wings_wm:send(object, {new_name,Id,NewName});
		     (_) -> ignore
		  end).

toggle_sel(#we{id=Id}=We, #ost{st=St0,sel=Sel}=Ost) ->
    case wings_wm:me_modifiers() of
	Mod when Mod band ?ALT_BITS =/= 0 ->
	    toggle_sel_all(We, Ost),
	    get_event(Ost#ost{op=none});
	_ ->
	    {Op,St} = case keymember(Id, 1, Sel) of
			  false ->
			      {select,wings_sel:select_object(Id, St0)};
			  true ->
			      {deselect,wings_sel:deselect_object(Id, St0)}
		      end,
	    wings_wm:send(geom, {new_state,St}),
	    get_event(Ost#ost{op=Op})
    end.

toggle_sel_all(_, #ost{st=#st{sel=[]}=St0}) ->
    St = wings_sel_cmd:select_all(St0),
    wings_wm:send(geom, {new_state,St});
toggle_sel_all(#we{id=Id}, #ost{st=#st{sel=[{Id,_}]}=St0}) ->
    St = wings_sel_cmd:select_all(St0),
    wings_wm:send(geom, {new_state,St});
toggle_sel_all(#we{id=Id}, #ost{st=St}) ->
    wings_wm:send(geom, {new_state,wings_sel:select_object(Id, St#st{sel=[]})}).

repeat_latest(_, #ost{active=-1}) -> ok;
repeat_latest(Field, #ost{first=First,active=Obj,os=Objs}=Ost) ->
    We = lists:nth(First+Obj+1, Objs),
    repeat_latest_1(Field, We, Ost).

repeat_latest_1(visibility, #we{id=Id}, #ost{op=hide,st=St}) ->
    wings_wm:send(geom, {new_state,hide_object(Id, St)});
repeat_latest_1(visibility, #we{id=Id}, #ost{op=show,st=St}) ->
    wings_wm:send(geom, {new_state,restore_object(Id, St)});
repeat_latest_1(selection, #we{id=Id}, #ost{op=select,st=St}) ->
    wings_wm:send(geom, {new_state,wings_sel:select_object(Id, St)});
repeat_latest_1(selection, #we{id=Id}, #ost{op=deselect,st=St}) ->
    wings_wm:send(geom, {new_state,wings_sel:deselect_object(Id, St)});
repeat_latest_1(lock, #we{id=Id,perm=P}, #ost{op=Op,st=St}) ->
    if
	?IS_NOT_VISIBLE(P) -> ok;
	Op == lock ->
	    wings_wm:send(geom, {new_state,lock_object(Id, St)});
	Op == unlock ->
	    wings_wm:send(geom, {new_state,restore_object(Id, St)});
	true -> ok
    end;
repeat_latest_1(_, _, _) -> ok.

draw_objects(#ost{os=Objs0,first=First,lh=Lh,active=Active}=Ost) ->
    Objs = lists:nthtail(First, Objs0),
    Y = ?CHAR_HEIGHT,
    wings_io:text_at(5+3, Y, "V"),
    wings_io:text_at(lock_pos()+3, Y, "L"),
    wings_io:text_at(name_pos(), Y, "Name"),
    R = right_pos(),
    wings_io:text_at(R+3, Y, "S"),
    draw_objects_1(Objs, Ost, R, Active, Y+2+Lh-2).

draw_objects_1([#we{id=Id,name=Name,perm=Perm}|Wes],
	       #ost{sel=Sel,lh=Lh,eye=Eye,lock=Lock}=Ost, R, Active, Y) ->
    LockPos = lock_pos(),
    wings_io:sunken_rect(3, Y-11, 12, 13, ?PANE_COLOR),
    wings_io:sunken_rect(LockPos-2, Y-11, 12, 13, ?PANE_COLOR),
    wings_io:sunken_rect(R, Y-9, 9, 11, ?PANE_COLOR),
    if
	?IS_VISIBLE(Perm) ->
	    gl:rasterPos2i(5, Y),
	    draw_char(Eye);
	true -> ok
    end,
    if
	?IS_SELECTABLE(Perm); ?IS_NOT_VISIBLE(Perm) ->
	    ok;
	true ->
    	    gl:rasterPos2i(LockPos, Y),
	    draw_char(Lock)
    end,
    case keymember(Id, 1, Sel) of
	false -> ok;
	true -> wings_io:text_at(R+2, Y, [crossmark])
    end,
    if
	Active == 0 ->
	    gl:color3f(0, 0, 0.5),
	    gl:recti(name_pos()-2, Y-?CHAR_HEIGHT, R-2, Y+4),
	    gl:color3f(1, 1, 1);
	true -> ok
    end,
    wings_io:text_at(name_pos(), Y, Name),
    gl:color3f(0, 0, 0),
    draw_objects_1(Wes, Ost, R, Active-1, Y+Lh);
draw_objects_1([], _, _, _, _) -> ok.

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


top_of_first_object() ->
    ?LINE_HEIGHT.

right_pos() ->
    {_,_,W,_} = wings_wm:viewport(),
    W-13.

lock_pos() ->
    20.

name_pos() ->
    37.

%%%
%%% Utilities.
%%%

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
