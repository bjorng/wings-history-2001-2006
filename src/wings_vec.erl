%%
%%  wings_vec.erl --
%%
%%     This module implements "vectors" and the secondary selection mode.
%%
%%  Copyright (c) 2002-2003 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vec.erl,v 1.72 2003/07/21 13:08:09 bjorng Exp $
%%

-module(wings_vec).

-export([init/0,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,keydelete/3,reverse/1,member/2,last/1]).

-record(ss, {f,					%Fun.
	     selmodes,				%Legal selection modes.
	     is_axis=false,			%True if axis.
	     info=""				%Info message.
	    }).

init() ->
    DefAxis = {{0.0,0.0,0.0},{1.0,0.0,0.0}},
    wings_pref:set_default(last_axis, DefAxis),
    wings_pref:set_default(default_axis, DefAxis),
    wings_pref:set_default(magnet_type, dome),
    wings_pref:set_default(magnet_distance_route, shortest),
    wings_pref:set_default(magnet_radius, 1.0).

command({pick,[],[Res],Ns}, St) ->
    Cmd = wings_menu:build_command(Res, Ns),
    wings_io:putback_event({action,Cmd}),
    St;
command({pick,[],Res,Ns}, St) ->
    Cmd = wings_menu:build_command(list_to_tuple(reverse(Res)), Ns),
    wings_io:putback_event({action,Cmd}),
    St;
command({pick,PickList,Acc,Names}, St) ->
    [{Type,Def,Desc}|More] = add_help_text(PickList, Names),
    MagnetPossible = magnet_possible(Names, More),
    command_1(Type, Desc, More, Acc, Names, MagnetPossible, St#st{vec=Def});
command({pick_special,{Modes,Init,Fun}}, St0) ->
    pick_init(St0),
    wings:mode_restriction(Modes),
    St = Init(St0),
    Ss = #ss{selmodes=Modes,f=Fun},
    {seq,push,get_event(Ss, St)}.

command_1(axis, Msg, More, Acc, Names, MagnetPossible, St0) ->
    pick_init(St0),
    Modes = [vertex,edge,face],
    St = mode_restriction(Modes, St0),
    Ss = #ss{f=fun(check, S) ->
		       check_vector(S);
		  (exit, {Mod,S}) ->
		       common_exit(Mod, More, Acc, Names, MagnetPossible, S);
		  (message, _) ->
		       common_message(Msg, More, Names, MagnetPossible)
	       end,
	     selmodes=Modes,
	     is_axis=true},
    {seq,push,get_event(Ss, wings_sel:reset(St))};
command_1(point, Msg, More, Acc, Names, MagnetPossible, St0) ->
    pick_init(St0),
    Modes = [vertex,edge,face],
    St = mode_restriction(Modes, St0),
    Ss = #ss{f=fun(check, S) ->
		       check_point(S);
		  (exit, {Mod,S}) ->
		       common_exit(Mod, More, Acc, Names, MagnetPossible, S);
		  (message, _) ->
		       common_message(Msg, More, Names, MagnetPossible)
	       end,
	     selmodes=Modes},
    {seq,push,get_event(Ss, wings_sel:reset(St))};
command_1(magnet, Msg, [], Acc, Names, _, St) ->
    pick_init(St),
    Modes = [vertex,edge,face],
    wings:mode_restriction(Modes),
    Ss = #ss{f=fun(check, S) ->
		       check_point(S);
		  (exit, {Mod,S}) ->
		       exit_magnet(Mod, Acc, Names, S);
		  (message, _) ->
		       magnet_message(Msg, Names)
	       end,
	     selmodes=Modes},
    {seq,push,get_event(Ss, wings_sel:reset(St#st{selmode=vertex}))}.

add_help_text([{Atom,Desc}|T], Names) when is_atom(Atom) ->
    [{Atom,none,Desc}|add_help_text(T, Names)];
add_help_text([{Atom,Def,Desc}|T], Names) when is_atom(Atom) ->
    [{Atom,Def,Desc}|add_help_text(T, Names)];
add_help_text([Type|T], Names) ->
    Val = {Type,none,
	   case Type of
	       axis -> "Pick axis";
	       point -> "Pick point";
	       magnet -> "Pick outer boundary point for magnet influence";
	       _ -> []
	   end},
    [Val|add_help_text(T, Names)];
add_help_text([], _) -> [].

magnet_possible([move|_], Pl) -> magnet_possible_1(Pl);
magnet_possible([rotate|_], Pl) -> magnet_possible_1(Pl);
magnet_possible([scale|_], Pl) -> magnet_possible_1(Pl);
magnet_possible(_, _) -> no.

magnet_possible_1([]) -> inactive;
magnet_possible_1(Pl) ->
    case last(Pl) of
	{magnet,_,_} -> active;
	_ -> inactive
    end.

common_message(Msg, [], Ns, MagnetPossible) ->
    Cmd = [command_name(Ns)|": "],
    Message = [Cmd,wings_util:button_format(Msg, [], "Execute ")|
	       common_magnet_message(MagnetPossible)],
    wings_wm:message(Message, "");
common_message(Msg, [_|_], Ns, MagnetPossible) ->
    Cmd = [command_name(Ns)|": "],
    Message = [Cmd,wings_util:button_format(Msg, [], "Continue")|
	       common_magnet_message(MagnetPossible)],
    wings_wm:message(Message, "").

common_magnet_message(no) -> [];
common_magnet_message(inactive) ->
    [$\s,wings_util:rmb_format("Magnet")];
common_magnet_message(active) ->
    ["  "|wings_util:magnet_string()].

magnet_message(Msg, Ns) ->
    Cmd = [command_name(Ns)|": "],
    Message = [Cmd,wings_util:button_format(Msg, [], "Execute "),
	       $\s,wings_util:rmb_format("Magnet options")],
    wings_wm:message(Message, "").

mode_restriction(Modes, #st{selmode=Mode}=St0) ->
    St = wings:clear_temp_sel(St0),
    wings:mode_restriction(Modes),
    case member(Mode, Modes) of
	true -> St;
	false -> St#st{selmode=last(Modes)}
    end.

pick_init(#st{selmode=Mode}) ->
    Active = wings_wm:this(),
    wings_wm:callback(fun() ->
			      wings_util:menu_restriction(Active, [view,select])
		      end),
    wings_draw_util:map(fun(D, _) -> pick_init_1(D, Mode) end, []).

pick_init_1(#dlo{orig_sel=none,sel=SelDlist}=D, Mode) ->
    D#dlo{orig_sel=SelDlist,orig_mode=Mode};
pick_init_1(D, _) -> D.

pick_finish() ->
    wings_wm:dirty(),
    wings_draw_util:map(fun clear_orig_sel/2, []).

clear_orig_sel(D, _) -> D#dlo{orig_sel=none,orig_mode=none}.

%%%
%%% Event handler for secondary selection mode.
%%%

get_event(Ss, St) ->
    wings_draw:update_dlists(St),
    wings_wm:dirty(),
    {replace,fun(Ev) -> handle_event(Ev, Ss, St) end}.

handle_event(Event, Ss, St) ->
    case wings_camera:event(Event, St) of
	next -> handle_event_1(Event, Ss, St);
	Other -> Other
    end.

handle_event_1(Event, Ss, St) ->
    case wings_pick:event(Event, St, fun() -> redraw(Ss, St) end) of
	next -> handle_event_2(Event, Ss, St);
	Other -> Other
    end.

handle_event_2(#mousebutton{x=X,y=Y}=Ev0, Ss, St0) ->
    case wings_menu:is_popup_event(Ev0) of
	{yes,Xglobal,Yglobal,Mod} ->
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} ->
		    Ev = wings_wm:local2global(Ev0),
		    wings_io:putback_event(Ev),
		    wings_wm:later({new_state,St});
		_ ->
		    exit_menu(Xglobal, Yglobal, Mod, Ss, St0)
	    end;
	no -> handle_event_3(Ev0, Ss, St0)
    end;
handle_event_2(Ev, Ss, St) -> handle_event_3(Ev, Ss, St).

handle_event_3(#keyboard{}=Ev, Ss, St0) ->
    case handle_key(Ev, Ss, St0) of
	next ->
	    case wings_hotkey:event(Ev, St0) of
		next -> handle_event_4(Ev, Ss, St0);
		{Menu,_}=Act when Menu == view; Menu == select->
		    wings_io:putback_event({action,Act}),
		    keep;
		_Other -> keep
	    end;
	Other -> Other
    end;
handle_event_3(Ev, Ss, St) -> handle_event_4(Ev, Ss, St).

handle_event_4({new_state,St}, #ss{f=Check}=Ss, _St0) ->
    case Check(check, St) of
	{Vec,Msg} -> 
	    get_event(Ss#ss{info=Msg}, St#st{vec=Vec});
	[{Vec,Msg}|_] ->
	    get_event(Ss#ss{info=Msg}, St#st{vec=Vec})
    end;
handle_event_4(redraw, Ss, St) ->
    redraw(Ss, St),
    keep;
handle_event_4({action,{select,Cmd}}, Ss, St0) ->
    case wings_sel_cmd:command(Cmd, St0) of
	St0 -> keep;
	{save_state,St} -> filter_sel_command(Ss, St);
	St -> filter_sel_command(Ss, St)
    end;
handle_event_4({action,{view,auto_rotate}}, _, _) ->
    keep;
handle_event_4({action,{view,Cmd}}, Ss, St0) ->
    St = wings_view:command(Cmd, St0),
    get_event(Ss, St);
handle_event_4({action,{secondary_selection,abort}}, _, _) ->
    wings_wm:later(revert_state),
    pick_finish(),
    pop;
handle_event_4({action,Cmd}, Ss, St) ->
    set_last_axis(Ss, St),
    wings_io:putback_event({action,Cmd}),
    pop;
handle_event_4(quit, _Ss, _St) ->
    wings_io:putback_event(quit),
    pop;
handle_event_4(init_opengl, _, St) ->
    wings:init_opengl(St);
handle_event_4({note,menu_aborted}, Ss, #st{temp_sel={_,_}}=St) ->
    get_event(Ss, wings:clear_temp_sel(St#st{sel=[],vec=none}));
handle_event_4(_Event, Ss, St) ->
    get_event(Ss, St).

redraw(#ss{info=Info,f=Message}, St) ->
    Message(message, St),
    wings:redraw(Info, St),
    wings_wm:current_state(St#st{vec=none}).

set_last_axis(#ss{is_axis=true}, #st{vec={{_,_,_},{_,_,_}}=Vec}) ->
    wings_pref:set_value(last_axis, Vec);
set_last_axis(_, _) -> ok.
			       
filter_sel_command(#ss{selmodes=Modes}=Ss, #st{selmode=Mode}=St) ->
    case member(Mode, Modes) of
	true -> handle_event({new_state,St}, Ss, St);
	false -> keep
    end.

handle_key(#keyboard{sym=$1}, _, St) ->	%1
    wings_io:putback_event({new_state,St}),
    keep;
handle_key(#keyboard{sym=$2}, #ss{f=Check}=Ss, St) -> %2
    case Check(check, St) of
	{Vec,Msg} -> 
	    get_event(Ss#ss{info=Msg}, St#st{vec=Vec});
	[_,{Vec,Msg}|_] ->
	    get_event(Ss#ss{info=Msg}, St#st{vec=Vec});
	[{Vec,Msg}] ->
	    get_event(Ss#ss{info=Msg}, St#st{vec=Vec})
    end;
handle_key(#keyboard{sym=27}, _, _) ->		%Escape
    wings_wm:later({action,{secondary_selection,abort}});
handle_key(_, _, _) -> next.

exit_menu(X, Y, Mod, #ss{f=Exit}=Ss, St) ->
    case Exit(exit, {Mod,St}) of
	error ->
	    Menu = [{"Cancel",abort,"Cancel current command"}],
	    wings_menu:popup_menu(X, Y, secondary_selection, Menu);
	keep ->
	    keep;
	Action ->
	    set_last_axis(Ss, St),
	    wings_wm:later({action,Action}),
	    wings_wm:dirty(),
	    pop
    end.

common_exit(_, _, _, _, _, #st{vec=none}) ->
    error;
common_exit(Mod, More, Acc, Ns, inactive, St) ->
    RmbMod = wings_camera:free_rmb_modifier(),
    if
	Mod band RmbMod =:= 0 ->
	    common_exit_1(More, Acc, Ns, St);
	More =:= [] ->
	    common_exit_1(add_magnet(More), Acc, Ns, St);
	true ->
	    case last(More) of
		{magnet,_,_} -> common_exit_1(More, Acc, Ns, St);
		_  -> common_exit_1(add_magnet(More), Acc, Ns, St)
	    end
    end;
common_exit(_, More, Acc, Ns, _, St) ->
    common_exit_1(More, Acc, Ns, St).

add_magnet(More) ->
    More ++ [{magnet,none,
	      "Pick outer boundary point for magnet influence"}].

common_exit_1([{point,_,Desc}|More], Acc, Ns, #st{vec={Point,Vec}}) ->
    PickList = [{point,Point,Desc}|More],
    {vector,{pick,PickList,add_to_acc(Vec, Acc),Ns}};
common_exit_1(PickList, Acc, Ns, #st{vec={_,Vec}}) ->
    {vector,{pick,PickList,add_to_acc(Vec, Acc),Ns}};
common_exit_1(PickList, Acc, Ns, #st{vec=Point}) ->
    {vector,{pick,PickList,add_to_acc(Point, Acc),Ns}}.

add_to_acc(Vec, [radial]) -> [{radial,Vec}];
add_to_acc(Vec, Acc) -> [Vec|Acc].

command_name([N|Ns]) ->
    wings_util:stringify(wings_menu:build_command(N, Ns)).

%%%
%%% Vector functions.
%%%

check_vector(#st{sel=[]}) -> {none,""};
check_vector(#st{selmode=Mode,sel=[{Id,Elems0}],shapes=Shs}) ->
    We = gb_trees:get(Id, Shs),
    Elems = gb_sets:to_list(Elems0),
    get_vec(Mode, Elems, We);
check_vector(_) -> {none,"Select parts of one object only"}.

%% Use single edge as axis
get_vec(edge, [Edge], #we{es=Etab,vp=Vtab}=We) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
    VaPos = gb_trees:get(Va, Vtab),
    VbPos = gb_trees:get(Vb, Vtab),
    Vec = e3d_vec:norm(e3d_vec:sub(VbPos, VaPos)),
    Center = wings_vertex:center([Va,Vb], We),
    Ln = wings_face:normal(Lf, We),
    Rn = wings_face:normal(Rf, We),
    Normal = e3d_vec:norm(e3d_vec:add(Ln, Rn)),
    [{{Center,Vec},"Edge saved as axis (press \"2\" to save edge normal)."},
     {{Center,Normal},
      "Edge normal saved as axis (press \"1\" to save edge direction)."}];
%% Use direction between two edges
get_vec(edge, [Edge1,Edge2], #we{es=Etab,vp=Vtab}) ->
    #edge{vs=Va1,ve=Vb1} = gb_trees:get(Edge1, Etab),
    #edge{vs=Va2,ve=Vb2} = gb_trees:get(Edge2, Etab),
    Va1Pos = gb_trees:get(Va1, Vtab),
    Vb1Pos = gb_trees:get(Vb1, Vtab),
    Va2Pos = gb_trees:get(Va2, Vtab),
    Vb2Pos = gb_trees:get(Vb2, Vtab),
    Center1 = e3d_vec:average([Va1Pos,Vb1Pos]),
    Center2 = e3d_vec:average([Va2Pos,Vb2Pos]),
    Center = e3d_vec:average([Center1,Center2]),
    Vec = e3d_vec:norm(e3d_vec:sub(Center1, Center2)),
    [{{Center,Vec},"Direction between edges saved as axis."}];
%% Use edge-loop normal.
get_vec(edge, Edges, #we{vp=Vtab}=We) ->
    case wings_edge_loop:edge_loop_vertices(Edges, We) of
	[Vs] -> 
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal_ccw(Vs, Vtab),
	    [{{Center,Vec},"Edge loop normal saved as axis."}];
	_Other ->
	    {none,"Multi-edge selection must form a single closed edge loop."}
    end;

%% Vertex normal
get_vec(vertex, [V], We) ->
    Vec = wings_vertex:normal(V, We),
    Center = wings_vertex:center([V], We),
    [{{Center,Vec},"Vertex normal saved."}];
%% Direction between 2 vertices as axis
get_vec(vertex, [Va,Vb]=Vs, We) ->
    VaPos = wings_vertex:pos(Va, We),
    VbPos = wings_vertex:pos(Vb, We),
    Vec = e3d_vec:norm(e3d_vec:sub(VaPos, VbPos)),
    Center = wings_vertex:center(Vs, We),
    [{{Center,Vec},"Direction between vertices saved as axis."}];
%% 3-point (defines face) perpendicular
get_vec(vertex, [_,_,_]=Vs, #we{vp=Vtab}=We) ->
    Vec = wings_face:face_normal_ccw(Vs, Vtab),
    Center = wings_vertex:center(Vs, We),
    [{{Center,Vec},"3-point perp. normal saved as axis."}];
%% Take the edge loop normal.
get_vec(vertex, Vs0, #we{vp=Vtab}=We) ->
    Edges = find_edges(Vs0, We),
    case wings_edge_loop:edge_loop_vertices(Edges, We) of
	[Vs] -> 
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal_cw(Vs, Vtab),
	    [{{Center,Vec},"Edge loop normal saved as axis."}];
	_Other ->
	    {none,"Multi-vertex selection must form a single closed edge loop."}
    end;

%% Face normal
get_vec(face, [Face], We) ->
    Vec = wings_face:normal(Face, We),
    Vs = wings_face:to_vertices([Face], We),
    Center = wings_vertex:center(Vs, We),
    [{{Center,Vec},"Face normal saved as axis."}];
%% Direction between two faces as axis
get_vec(face, [Face1,Face2], We) ->
    Center1 = wings_face:center(Face1, We),
    Center2 = wings_face:center(Face2, We),
    Center = e3d_vec:average([Center1,Center2]),
    Vec = e3d_vec:norm(e3d_vec:sub(Center1, Center2)),
    [{{Center,Vec},"Direction between faces saved as axis."}];
get_vec(face, Faces, #we{vp=Vtab}=We) ->
    case wings_vertex:outer_partition(Faces, We) of
	[Vs] ->
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal_cw(Vs, Vtab),
	    [{{Center,Vec},"Edge loop normal for region saved as axis."}];
	_Other ->
	    {none,"Multi-face selection must have a single edge loop."}
    end;

get_vec(_, _, _) -> {none,"Select vertices, edges, or faces."}.

%%%
%%% Point functions.
%%%

check_point(#st{sel=[]}) -> {none,""};
check_point(St) ->
    Center = e3d_vec:average(wings_sel:bounding_box(St)),
    [{Center,"Midpoint of selection saved."}].

%%%
%%% Magnet functions.
%%%

exit_magnet(Mod, Acc, Ns, St) ->
    case wings_camera:free_rmb_modifier() of
	ModRmb when Mod band ModRmb =/= 0 ->
	    Fun = fun(Mag) ->
			  {vector,{pick,[],[Mag|Acc],Ns}}
		  end,
	    case check_point(St) of
		{none,_} ->
		    wings_magnet:dialog(Fun);
		[{Point,_}] ->
		    wings_magnet:dialog(Point, Fun)
	    end;
	_ ->
	    case check_point(St) of
		{none,_} ->
		    error;
		[{Point,_}] ->
		    Mag = {magnet,wings_pref:get_value(magnet_type),
			   wings_pref:get_value(magnet_distance_route),Point},
		    {vector,{pick,[],[Mag|Acc],Ns}}
	    end
    end.

%%%
%%% Utilities.
%%%

find_edges(Vs, We) ->
    VsSet = gb_sets:from_list(Vs),
    Es = find_edges(Vs, VsSet, We, []),
    ordsets:from_list(Es).

find_edges([V|Vs], VsSet, We, Acc0) ->
    Acc = wings_vertex:fold(
	    fun(E, _, Rec, A) ->
		    OtherV = wings_vertex:other(V, Rec),
		    case gb_sets:is_member(OtherV, VsSet) of
			false -> A;
			true -> [E|A]
		    end
	    end, Acc0, V, We),
    find_edges(Vs, VsSet, We, Acc);
find_edges([], _VsSet, _We, Acc) -> Acc.
