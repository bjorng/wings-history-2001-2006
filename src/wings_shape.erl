%%
%%  wings_shape.erl --
%%
%%     Utilities for shape records.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_shape.erl,v 1.47 2003/01/22 21:34:40 bjorng Exp $
%%

-module(wings_shape).
-export([new/3,insert/3,replace/3,window/1]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
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
	 n,					%Number of objects.
	 first,					%First object to show.
	 sel,					%Current selection.
	 os,					%All objects.
	 active,				%Number of active object.
	 lh,					%Line height.
	 op					%Latest operation.
	}).

window(St) ->
    case wings_wm:is_window(object) of
	true ->
	    wings_wm:delete(object);
	false ->
	    {{_,DeskY},{DeskW,DeskH}} = wings_wm:win_rect(desktop),
	    W = 28*?CHAR_WIDTH,
	    Ost = #ost{first=0,lh=18,active=-1},
	    Current = {current_state,St},
	    Op = {seq,push,event(Current, Ost)},
	    Pos = {DeskW-5,DeskY+55,?Z_OBJECTS},
	    Size = {W,DeskH div 2},
	    wings_wm:toplevel(object, "Objects", Pos, Size,
			      [resizable,closable,vscroller,{anchor,ne}], Op),
	    wings_wm:send(object, Current),
	    keep
    end.

get_event(Ost) ->
    {replace,fun(Ev) -> event(Ev, Ost) end}.

event(resized, Ost) ->
    update_scroller(Ost),
    keep;
event(redraw, Ost) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-0.5, H-1, ?PANE_COLOR),
    draw_objects(Ost),
    keep;
event({current_state,St}, Ost0) ->
    Ost = update_state(St, Ost0),
    update_scroller(Ost),
    get_event(Ost);
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
	_ ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{active=Act},
	    case State band ?SDL_BUTTON_LMASK of
		?SDL_BUTTON_LMASK ->
		    repeat_latest(Field, Ost);
		0 -> ok
	    end,
	    get_event(Ost)
    end;
event(#mousebutton{x=X,y=Y,button=B,state=?SDL_PRESSED}, Ost) ->
    do_action(X, Y, B, Ost);
event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-1*lines(Ost) div 4, Ost);
event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(lines(Ost) div 4, Ost);
event(scroll_page_up, Ost) ->
    zoom_step(-lines(Ost), Ost);
event(scroll_page_down, Ost) ->
    zoom_step(lines(Ost), Ost);
event({set_knob_pos,Pos}, #ost{first=First0,n=N}=Ost0) ->
    case round(N*Pos) of
	First0 -> keep;
	First when First < N ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{first=First,active=-1},
	    update_scroller(Ost),
	    get_event(Ost);
	_ -> keep
    end;
event(_, _) -> keep.

help(-1, _) -> wings_wm:message("");
help(_, name) ->
    {_,_,Three} = wings_camera:button_names(),
    wings_wm:message([Three," Rename object"]);
help(_, visibility) ->
    help_1("Toggle visibility of active object",
	   "Toggle visibility of all other objects");
help(_, lock) ->
    help_1("Lock/unlock active object",
	   "Lock/unlock all objects");
help(_, selection) ->
    help_1("Toggle selection for active object",
	   "Toggle selection for all other objects").

help_1(OneMsg, ThreeMsg) ->
    {One,_,Three} = wings_camera:button_names(),
    wings_wm:message([One," ",OneMsg,"  ",Three," ",ThreeMsg]).

update_state(St, #ost{first=OldFirst}=Ost0) ->
    #ost{first=First0} = Ost = update_state_1(St, Ost0),
    case clamp(First0, Ost) of
	OldFirst -> Ost;
	First ->
	    wings_wm:dirty(),
	    Ost#ost{first=First}
    end.

update_state_1(#st{sel=Sel,shapes=Shs}=St, #ost{st=#st{sel=Sel,shapes=Shs}}=Ost) ->
    Ost#ost{st=St};
update_state_1(#st{sel=Sel,shapes=Shs0}=St, #ost{st=#st{sel=Sel},os=Objs}=Ost) ->
    Shs = gb_trees:values(Shs0),
    case have_objects_really_changed(Shs, Objs) of
	false -> ok;
	true -> wings_wm:dirty()
    end,
    Ost#ost{st=St,sel=Sel,os=Shs,n=gb_trees:size(Shs0)};
update_state_1(#st{sel=Sel,shapes=Shs}=St, #ost{st=#st{sel=Sel0}}=Ost) ->
    case has_sel_really_changed(Sel, Sel0) of
	false -> ok;
	true -> wings_wm:dirty()
    end,
    Ost#ost{st=St,sel=Sel,os=gb_trees:values(Shs),n=gb_trees:size(Shs)};
update_state_1(#st{sel=Sel,shapes=Shs}=St, Ost) ->
    Ost#ost{st=St,sel=Sel,os=gb_trees:values(Shs),n=gb_trees:size(Shs)}.

update_scroller(#ost{n=0}) ->
    Name = wings_wm:active_window(),
    wings_wm:set_knob(Name, 0.0, 1.0);
update_scroller(#ost{first=First,n=N}=Ost) ->
    Name = wings_wm:active_window(),
    Lines = lines(Ost),
    wings_wm:set_knob(Name, First/N, Lines/N).
    
has_sel_really_changed([{Id,_}|SelA], [{Id,_}|SelB]) ->
    has_sel_really_changed(SelA, SelB);
has_sel_really_changed([], []) -> false;
has_sel_really_changed(_, _) -> true.

have_objects_really_changed([#we{id=Id,name=Name,perm=P}|WesA],
			  [#we{id=Id,name=Name,perm=P}|WesB]) ->
    have_objects_really_changed(WesA, WesB);
have_objects_really_changed([], []) -> false;
have_objects_really_changed(_, _) -> true.

zoom_step(Step, #ost{first=First0}=Ost0) ->
    case clamp(First0+Step, Ost0) of
	First0 -> keep;
	First ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{first=First},
	    update_scroller(Ost),
	    get_event(Ost)
    end.

clamp(F, #ost{n=N}=Ost) ->
    Max = case N-lines(Ost) of
	      Neg when Neg < 0 -> 0;
	      Other -> Other
	  end,
    if
	F < 0 -> 0;
	F > Max -> Max;
	true -> F
    end.
    
active_object(Y0, #ost{lh=Lh,first=First,n=N}) ->
    case Y0 - top_of_first_object() of
	Y when Y < 0 -> -1;
	Y1 ->
	    case Y1 div Lh of
		Y when First+Y < N -> Y;
		_ -> -1
	    end
    end.

active_field(X) ->
    EyePos = eye_pos(),
    LockPos = lock_pos(),
    SelPos = sel_pos(),
    if
	X < EyePos -> name;
	X < LockPos -> visibility;
	X < SelPos -> lock;
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
do_action_1(visibility, 3, We, Ost) -> toggle_visibility_all(We, Ost);
do_action_1(lock, 1, We, Ost) -> toggle_lock(We, Ost);
do_action_1(lock, 3, We, Ost) -> toggle_lock_all(We, Ost);
do_action_1(name, 3, We, _Ost) -> rename_object(We);
do_action_1(selection, 1, We, Ost) -> toggle_sel(We, Ost);
do_action_1(selection, 3, We, Ost) -> toggle_sel_all(We, Ost);
do_action_1(_, _, _, _) -> keep.

toggle_visibility(#we{id=Id,perm=Perm}, #ost{st=St0}=Ost) ->
    {Op,St} = if
		  ?IS_VISIBLE(Perm) -> 
		      {hide,hide_object(Id, St0)};
		  true ->
		      {show,restore_object(Id, St0)}
	      end,
    wings_wm:send(geom, {new_state,St}),
    get_event(Ost#ost{op=Op}).

toggle_visibility_all(#we{id=Id}, #ost{os=Objs,st=St0}=Ost) ->
    St = case are_all_visible(Objs, Id) of
	     false -> restore_all(St0);
	     true -> hide_others(Id, St0)
	 end,
    wings_wm:send(geom, {new_state,St}),
    get_event(Ost#ost{op=none}).

are_all_visible([#we{id=Id}|T], Id) ->
    are_all_visible(T, Id);
are_all_visible([#we{perm=P}|T], Id) ->
    case ?IS_VISIBLE(P) of
	false -> false;
	true -> are_all_visible(T, Id)
    end;
are_all_visible([], _) -> true.

toggle_lock(#we{perm=Perm}, _) when ?IS_NOT_VISIBLE(Perm) -> keep;
toggle_lock(#we{id=Id,perm=Perm}, #ost{st=St0}=Ost) when ?IS_SELECTABLE(Perm) ->
    wings_wm:send(geom, {new_state,lock_object(Id, St0)}),
    get_event(Ost#ost{op=lock});
toggle_lock(#we{id=Id}, #ost{st=St0}=Ost) ->
    wings_wm:send(geom, {new_state,restore_object(Id, St0)}),
    get_event(Ost#ost{op=unlock}).

toggle_lock_all(#we{id=Id}, #ost{st=St0,os=Objs}=Ost) ->
    St = case are_all_visible_locked(Objs, Id) of
	     true -> restore_all(St0);
	     false -> lock_others(Id, St0)
	 end,
    wings_wm:send(geom, {new_state,St}),
    get_event(Ost#ost{op=none}).

are_all_visible_locked([#we{id=Id}|T], Id) ->
    are_all_visible_locked(T, Id);
are_all_visible_locked([#we{perm=P}|T], Id) ->
    case ?IS_VISIBLE(P) of
	false ->
	    are_all_visible_locked(T, Id);
	true when ?IS_NOT_SELECTABLE(P) ->
	    are_all_visible_locked(T, Id);
	true ->
	    false
    end;
are_all_visible_locked([], _) -> true.

rename_object(#we{id=Id,name=Name}) ->
    wings_ask:ask("Rename Object",
		  [{"New Name",Name}],
		  fun([NewName]) when NewName =/= Name ->
			  wings_wm:send(object, {new_name,Id,NewName}),
			  ignore;
		     (_) -> ignore
		  end).

toggle_sel(#we{id=Id,perm=P}, #ost{st=St0,sel=Sel}=Ost) ->
    case keymember(Id, 1, Sel) of
	false when ?IS_SELECTABLE(P) ->
	    St = wings_sel:select_object(Id, St0),
	    wings_wm:send(geom, {new_state,St}),
	    get_event(Ost#ost{op=select});
	true ->
	    St = wings_sel:deselect_object(Id, St0),
	    wings_wm:send(geom, {new_state,St}),
	    get_event(Ost#ost{op=deselect});
	false ->
	    get_event(Ost#ost{op=none})
    end.

toggle_sel_all(We, Ost) ->
    toggle_sel_all_1(We, Ost),
    get_event(Ost#ost{op=none}).

toggle_sel_all_1(_, #ost{sel=[],st=St0}) ->
    St = wings_sel_cmd:select_all(St0),
    wings_wm:send(geom, {new_state,St});
toggle_sel_all_1(#we{id=Id}, #ost{sel=[{Id,_}],st=St0}) ->
    St = wings_sel_cmd:select_all(St0#st{sel=[]}),
    wings_wm:send(geom, {new_state,St});
toggle_sel_all_1(#we{id=Id,perm=P}, #ost{st=St}) when ?IS_SELECTABLE(P) ->
    wings_wm:send(geom, {new_state,wings_sel:select_object(Id, St#st{sel=[]})});
toggle_sel_all_1(_, _) -> ok.

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

draw_objects(#ost{os=Objs0,first=First,lh=Lh,active=Active,n=N0}=Ost) ->
    if
	First < 0 -> erlang:fault({neg_first,First});
	true -> ok
    end,
    Objs = lists:nthtail(First, Objs0),
    R = right_pos(),
    Lines = lines(Ost),
    N = case N0-First of
	    N1 when N1 < Lines -> N1;
	    _ -> Lines
	end,
    draw_objects_1(N, Objs, Ost, R, Active, Lh-2).

draw_objects_1(0, _, _, _, _, _) -> ok;
draw_objects_1(N, [#we{id=Id,name=Name,perm=Perm}=We|Wes],
	       #ost{sel=Sel,lh=Lh}=Ost, R, Active, Y) ->
    EyePos = eye_pos(),
    LockPos = lock_pos(),
    SelPos = sel_pos(),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    if
	?IS_LIGHT(We) ->
	    wings_io:draw_icon(icon_pos(), Y-14, 16, 16, small_light);
	true ->
	    wings_io:draw_icon(icon_pos(), Y-14, 16, 16, small_object)
    end,
    if
	?IS_VISIBLE(Perm) ->
	    wings_io:draw_icon(EyePos, Y-14, 16, 16, small_eye);
	true ->
	    wings_io:draw_icon(EyePos, Y-14, 16, 16, small_closed_eye)
    end,
    if
	?IS_SELECTABLE(Perm) ->
	    wings_io:draw_icon(LockPos, Y-14, 16, 16, small_unlocked);
	true ->
	    wings_io:draw_icon(LockPos, Y-14, 16, 16, small_locked)
    end,
    case keymember(Id, 1, Sel) of
	false ->
	    wings_io:draw_icon(SelPos, Y-14, 16, 16, small_object);
	true ->
	    wings_io:draw_icon(SelPos, Y-14, 16, 16, small_sel)
    end,
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D),
    if
	Active == 0 ->
	    gl:color3f(0, 0, 0.5),
	    gl:recti(name_pos()-2, Y-?CHAR_HEIGHT, R-2, Y+4),
	    gl:color3f(1, 1, 1);
	true -> ok
    end,
    wings_io:text_at(name_pos(), Y, Name),
    gl:color3f(0, 0, 0),
    draw_objects_1(N-1, Wes, Ost, R, Active-1, Y+Lh).

top_of_first_object() ->
    0.

icon_pos() ->
    2.

name_pos() ->
    20.

eye_pos() ->
    right_pos().

lock_pos() ->
    right_pos()+16+2.

sel_pos() ->
    right_pos()+32+4.

right_pos() ->
    {W,_} = wings_wm:win_size(),
    W-3*(16+2).

lines(#ost{lh=Lh}) ->
    {_,_,_,H} = wings_wm:viewport(),
    H div Lh.

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

lock_others(ThisId, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when ThisId =:= Id ->
		       {Id,We};
		  (#we{id=Id,perm=P}=We) when ?IS_VISIBLE(P) ->
		       {Id,We#we{perm=1}};
		  (#we{id=Id}=We) ->
		       {Id,We}
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
