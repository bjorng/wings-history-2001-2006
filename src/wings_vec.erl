%%
%%  wings_vec.erl --
%%
%%     This module implements "vectors" and the secondary selection mode.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vec.erl,v 1.27 2002/03/31 10:54:14 bjorng Exp $
%%

-module(wings_vec).

-export([init/0,menu/1,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,keydelete/3,reverse/1,member/2,last/1]).

-record(ss, {check,				%Check fun.
	     exit,				%Exit fun.
	     selmodes,				%Legal selection modes.
	     label=none,			%Description of type.
	     sti=none				%Store index.
	    }).

init() ->
    DefPoint = {0.0,0.0,0.0},
    DefAxis = {DefPoint,{1.0,0.0,0.0}},
    wings_pref:set_default(last_axis, DefAxis),
    wings_pref:set_default(default_axis, DefAxis),
    wings_pref:set_default(last_point, DefPoint),
    wings_pref:set_default(default_point, DefPoint),
    wings_pref:set_default(magnet_type, dome),
    wings_pref:set_default(magnet_distance_route, shortest),
    wings_pref:set_default(magnet_radius, 1.0).

menu(_St) -> [].

command({pick,[],[Res],Ns}, St) ->
    pick_init(St),
    Cmd = wings_menu:build_command(Res, Ns),
    wings_io:putback_event({action,Cmd}),
    St;
command({pick,[],Res,Ns}, St) ->
    pick_init(St),
    Cmd = wings_menu:build_command(list_to_tuple(reverse(Res)), Ns),
    wings_io:putback_event({action,Cmd}),
    St;
command({pick,[axis|More],Acc,Names}, St0) ->
    pick_init(St0),
    Modes = [vertex,edge,face],
    St1 = mode_restriction(Modes, St0),
    Check = fun vector_exit_check/1,
    Ss = #ss{check=fun check_vector/1,
	     exit=fun(_X, _Y, St) ->
			  common_exit(Check, More, Acc, Names, St)
		  end,
	     selmodes=Modes,
	     label="Axis",sti=last_axis},
    command_message("Select axis for ", Names),
    {seq,{push,dummy},get_event(Ss, St1#st{sel=[]})};
command({pick,[point|More],Acc,Names}, St0) ->
    pick_init(St0),
    Modes = [vertex,edge,face],
    St1 = mode_restriction(Modes, St0),
    Check = fun check_point/1,
    Ss = #ss{check=Check,
	     exit=fun(_X, _Y, St) ->
			  common_exit(Check, More, Acc, Names, St)
		  end,
	     selmodes=Modes},
    command_message("Select point for ", Names),
    {seq,{push,dummy},get_event(Ss, St1#st{sel=[]})};
command({pick,[magnet],Acc,Names}, St0) ->
    pick_init(St0),
    Modes = [vertex,edge,face],
    wings_io:icon_restriction(Modes),
    Ss = #ss{check=fun check_point/1,
	     exit=fun(_X, _Y, St) -> exit_magnet([], Acc, Names, St) end,
	     selmodes=Modes,
	     label=magnet},
    command_message("Select magnet falloff for ", Names),
    {seq,{push,dummy},get_event(Ss, St0#st{selmode=vertex,sel=[]})};
command({pick,[magnet_options],Acc,Names}, St) ->
    wings_magnet:dialog(St, fun(Mag) ->
				    {vector,{pick,[],[Mag|Acc],Names}}
			    end);
command({pick,[{magnet_options,Point}],Acc,Names}, St) ->
    wings_magnet:dialog(Point, St,
			fun(Mag) ->
				{vector,{pick,[],[Mag|Acc],Names}}
			end);
command({pick_special,{Modes,Init,Check,Exit}}, St0) ->
    pick_init(St0),
    wings_io:icon_restriction(Modes),
    St = Init(St0),
    Ss = #ss{selmodes=Modes,check=Check,exit=Exit},
    {seq,{push,dummy},get_event(Ss, St)}.

command_message(Prefix, Ns) ->
    wings_io:message_right(Prefix ++ command_name(Ns)),
    wings_io:message("").

mode_restriction(Modes, #st{selmode=Mode}=St) ->
    wings_io:icon_restriction(Modes),
    case member(Mode, Modes) of
	true -> St;
	false -> St#st{sel=[],selmode=last(Modes)}
    end.

pick_init(#st{selmode=Mode}) ->
    case wings_draw:get_dlist() of
	#dl{sel=SelDL,orig_sel=none}=DL ->
	    wings_draw:put_dlist(DL#dl{orig_sel={Mode,SelDL}});
	_ -> ok
    end.

%%%
%%% Event handler for secondary selection mode.
%%%

get_event(Ss, St) ->
    wings:redraw(St),
    {replace,fun(Ev) -> handle_event(Ev, Ss, St) end}.

handle_event(Event, Ss, St) ->
    case wings_io:event(Event) of
	next -> handle_event_0(Event, Ss, St);
	Other -> Other
    end.

handle_event_0(Event, Ss, St) ->
    case wings_camera:event(Event, St) of
	next -> handle_event_1(Event, Ss, St);
	Other -> Other
    end.

handle_event_1(Event, Ss, St) ->
    case wings_pick:event(Event, St) of
	next -> handle_event_2(Event, Ss, St);
	Other -> Other
    end.

handle_event_2(Event, Ss, #st{sel=[]}=St0) ->
    case wings_menu:is_popup_event(Event) of
	no -> handle_event_3(Event, Ss, St0);
	{yes,X,Y} ->
	    case wings_pick:do_pick(X, Y, St0) of
		{add,St} ->
		    wings_io:putback_event(Event),
		    wings_io:putback_event({new_state,St}),
		    keep;
		_Other -> exit_menu(X, Y, Ss, St0)
	    end
    end;
handle_event_2(Event, Ss, St) ->
    case wings_menu:is_popup_event(Event) of
	no -> handle_event_3(Event, Ss, St);
	{yes,X,Y} -> exit_menu(X, Y, Ss, St)
    end.

handle_event_3(Event, Ss, St) ->
    case translate_key(Event) of
	next -> handle_event_4(Event, Ss, St);
	Other -> Other
    end.
    
handle_event_4(Event, Ss, St0) ->
    case wings_hotkey:event(Event, St0) of
	next -> handle_event_5(Event, Ss, St0);
	{view,Cmd} ->
	    St = wings_view:command(Cmd, St0),
	    get_event(Ss, St);
	{select,Cmd} ->
	    case wings_sel_cmd:command(Cmd, St0) of
		St0 -> keep;
		{save_state,St} -> filter_sel_command(Ss, St);
		St -> filter_sel_command(Ss, St)
	    end;
	_Other -> keep
    end.

handle_event_5({new_state,St}, #ss{check=Check}=Ss, _St0) ->
    {Vec,Msg} = Check(St),
    wings_io:message(Msg),
    get_event(Ss, St#st{vec=Vec});
handle_event_5(#keyboard{}, Ss, St) ->
    get_event(Ss, St);
handle_event_5(redraw, _Ss, St) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings:redraw(St),
    keep;
handle_event_5({action,{select,Cmd}}, Ss, St0) ->
    case wings_sel_cmd:command(Cmd, St0) of
	St0 -> keep;
	{save_state,St} -> filter_sel_command(Ss, St);
	St -> filter_sel_command(Ss, St)
    end;
handle_event_5({action,{view,Cmd}}, Ss, St0) ->
    St = wings_view:command(Cmd, St0),
    get_event(Ss, St);
handle_event_5({action,{secondary_selection,Cmd}}, Ss, St) ->
    secondary_selection(Cmd, Ss, St);
handle_event_5({action,Cmd}, #ss{sti=StiA}, #st{vec=Vec}) ->
    wings_pref:set_value(StiA, Vec),
    wings_io:message(""),
    wings_io:putback_event({action,Cmd}),
    pop;
handle_event_5({action,Cmd}, _Ss, _St) ->
    wings_io:clear_message(),
    wings_io:putback_event({action,Cmd}),
    pop;
handle_event_5(quit, _Ss, _St) ->
    wings_io:putback_event(quit),
    pop;
handle_event_5(_Event, Ss, St) ->
    get_event(Ss, St).

secondary_selection(abort, _Ss, _St) ->
    wings_draw:clear_orig_sel(),
    wings_io:clear_message(),
    wings_io:putback_event(redraw),
    pop;
secondary_selection(shortest, Ss, St) ->
    wings_pref:set_value(magnet_distance_route, shortest),
    get_event(Ss, St);
secondary_selection(surface, Ss, St) ->
    wings_pref:set_value(magnet_distance_route, surface),
    get_event(Ss, St).
			       
filter_sel_command(#ss{selmodes=Modes}=Ss, #st{selmode=Mode}=St) ->
    case member(Mode, Modes) of
	true -> handle_event({new_state,St}, Ss, St);
	false -> keep
    end.

translate_key(#keyboard{keysym=KeySym}) ->
    translate_key_1(KeySym);
translate_key(_Event) -> next.

translate_key_1(#keysym{sym=27}) ->		%Escape
    wings_draw:clear_orig_sel(),
    wings_io:clear_message(),
    wings_io:message("Command aborted"),
    wings_io:putback_event(redraw),
    pop;
translate_key_1(_Other) -> next.

exit_menu(X, Y, #ss{exit=Exit}, St) ->
    case Exit(X, Y, St) of
	invalid_selection ->
	    exit_menu_invalid(X, Y, St);
	MenuEntry ->
	    exit_menu_done(X, Y, MenuEntry, St)
    end.

exit_menu_invalid(X, Y, St) ->
    Menu = [{"Invalid Selection",ignore},{"Abort Command",abort}],
    wings_menu:popup_menu(X, Y, secondary_selection, Menu, St).

exit_menu_done(X, Y, MenuEntry, St) ->
    Menu = [MenuEntry,{"Abort Command",abort}],
    wings_menu:popup_menu(X, Y, secondary_selection, Menu, St).

common_exit(Check, More, Acc, Ns, #st{vec=none}=St) ->
    case Check(St) of
	{none,Msg} ->
	    wings_io:message(Msg),
	    invalid_selection;
	{Vec,Msg} ->
	    wings_io:message(Msg),
	    common_exit_1(Vec, More, Acc, Ns)
    end;
common_exit(_Check, [point]=More, Acc, Ns, #st{vec={Point,Vec}}) ->
    Command = command_name(Ns),
    F = fun({magnet,1}, _) ->
		{vector,{pick,[magnet],[Point|add_to_acc(Vec, Acc)],Ns}};
	   ({magnet,2}, _) ->
		{vector,{pick,[magnet_options],
			 [Point|add_to_acc(Vec, Acc)],Ns}};
	   ({magnet,3}, _) ->
		Magnet = wings_menu_util:magnet_data(),
		{vector,{pick,[],[Magnet,Point|add_to_acc(Vec, Acc)],Ns}};
	   (1, _) ->
		{vector,{pick,[],[Point|add_to_acc(Vec, Acc)],Ns}};
	   (3, _) ->
		{vector,{pick,[point],add_to_acc(Vec, Acc),Ns}};
	   (_, _) -> ignore
	end,
    Ps = wings_menu_util:magnet_props(vector, Ns),
    {Command,F,{"Execute command",[],pick_more_help(More, Ns)},Ps};
common_exit(_Check, More, Acc, Ns, #st{vec={_,Vec}}) ->
    common_exit_1(Vec, More, Acc, Ns);
common_exit(_Check, More, Acc, Ns, #st{vec=Vec}) ->
    common_exit_1(Vec, More, Acc, Ns).

common_exit_1(Vec, [], Acc, Ns) ->
    Command = command_name(Ns),
    F = fun({magnet,1}, _) ->
		{vector,{pick,[magnet],add_to_acc(Vec, Acc),Ns}};
	   ({magnet,2}, _) ->
		{vector,{pick,[magnet_options],add_to_acc(Vec, Acc),Ns}};
	   ({magnet,3}, _) ->
		Magnet = wings_menu_util:magnet_data(),
		{vector,{pick,[],[Magnet|add_to_acc(Vec, Acc)],Ns}};
	   (_, _) ->
		{vector,{pick,[],add_to_acc(Vec, Acc),Ns}}
	end,
    Ps = wings_menu_util:magnet_props(vector, Ns),
    {Command,F,"Execute command",Ps};
common_exit_1(Vec, More, Acc, Ns) ->
    {"Continue",fun(_, _) ->
			{vector,{pick,More,add_to_acc(Vec, Acc),Ns}}
		end,pick_more_help(More, Ns),[]}.

pick_more_help([point|_], Ns) ->
    "Continue to select point for " ++ command_name(Ns);
pick_more_help([axis|_], Ns) ->
    "Continue to select axis for " ++ command_name(Ns).

add_to_acc(Vec, [radial]) -> [{radial,Vec}];
add_to_acc(Vec, Acc) -> [Vec|Acc].

command_name([N|Ns]) ->
    wings_util:stringify(wings_menu:build_command(N, Ns)).

%%%
%%% Vector functions.
%%%

vector_exit_check(St) ->
    case check_vector(St) of
	{none,_}=None -> None;
	{{_,Vec},Msg} -> {Vec,Msg}
    end.

check_vector(#st{sel=[]}) -> {none,"Nothing selected"};
check_vector(#st{selmode=Mode,sel=[{Id,Elems0}],shapes=Shs}) ->
    We = gb_trees:get(Id, Shs),
    Elems = gb_sets:to_list(Elems0),
    get_vec(Mode, Elems, We);
check_vector(_) -> {none,"Select parts of one object only"}.

%% Use single edge as axis
get_vec(edge, [Edge], #we{es=Etab,vs=Vtab}=We) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    VaPos = wings_vertex:pos(Va, Vtab),
    VbPos = wings_vertex:pos(Vb, Vtab),
    Vec = e3d_vec:norm(e3d_vec:sub(VbPos, VaPos)),
    Center = wings_vertex:center([Va,Vb], We),
    {{Center,Vec},"Edge saved as axis."};
%% Use direction between two edges
get_vec(edge, [Edge1,Edge2], #we{es=Etab,vs=Vtab}) ->
    #edge{vs=Va1,ve=Vb1} = gb_trees:get(Edge1, Etab),
    #edge{vs=Va2,ve=Vb2} = gb_trees:get(Edge2, Etab),
    Va1Pos = wings_vertex:pos(Va1, Vtab),
    Vb1Pos = wings_vertex:pos(Vb1, Vtab),
    Va2Pos = wings_vertex:pos(Va2, Vtab),
    Vb2Pos = wings_vertex:pos(Vb2, Vtab),
    Center1 = e3d_vec:average([Va1Pos,Vb1Pos]),
    Center2 = e3d_vec:average([Va2Pos,Vb2Pos]),
    Center = e3d_vec:average([Center1,Center2]),
    Vec = e3d_vec:norm(e3d_vec:sub(Center1, Center2)),
    {{Center,Vec},"Direction between edges saved as axis."};
%% Use edge-loop normal.
get_vec(edge, Edges, #we{vs=Vtab}=We) ->
    case wings_edge_loop:edge_loop_vertices(Edges, We) of
	[Vs] -> 
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal(reverse(Vs), Vtab),
	    {{Center,Vec},"Edge loop normal saved as axis."};
	_Other ->
	    {none,"Multi-edge selection must form a single closed edge loop."}
    end;

%% Vertex normal
get_vec(vertex, [V], We) ->
    Vec = wings_vertex:normal(V, We),
    Center = wings_vertex:center([V], We),
    {{Center,Vec}, "Vertex normal saved."};
%% Direction between 2 vertices as axis
get_vec(vertex, [Va,Vb]=Vs, We) ->
    VaPos = wings_vertex:pos(Va, We),
    VbPos = wings_vertex:pos(Vb, We),
    Vec = e3d_vec:norm(e3d_vec:sub(VaPos, VbPos)),
    Center = wings_vertex:center(Vs, We),
    {{Center,Vec},"Direction between vertices saved as axis."};
%% 3-point (defines face) perpendicular
get_vec(vertex, [_,_,_]=Vs, #we{vs=Vtab}=We) ->
    Vec = wings_face:face_normal(Vs, Vtab),
    Center = wings_vertex:center(Vs, We),
    {{Center,Vec},"3-point perp. normal saved as axis."};
%% Take the edge loop normal.
get_vec(vertex, Vs0, #we{vs=Vtab}=We) ->
    Edges = find_edges(Vs0, We),
    case wings_edge_loop:edge_loop_vertices(Edges, We) of
	[Vs] -> 
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal(reverse(Vs), Vtab),
	    {{Center,Vec},"Edge loop normal saved as axis."};
	_Other ->
	    {none,"Multi-vertex selection must form a single closed edge loop."}
    end;

%% Face normal
get_vec(face, [Face], We) ->
    Vec = wings_face:normal(Face, We),
    Vs = wings_face:to_vertices([Face], We),
    Center = wings_vertex:center(Vs, We),
    {{Center,Vec},"Face normal saved as axis."};
%% Direction between two faces as axis
get_vec(face, [Face1,Face2], We) ->
    VsList1 = wings_face:surrounding_vertices(Face1, We),
    Center1 = wings_vertex:center(VsList1, We),
    VsList2 = wings_face:surrounding_vertices(Face2, We),
    Center2 = wings_vertex:center(VsList2, We),
    Center = e3d_vec:average([Center1,Center2]),
    Vec = e3d_vec:norm(e3d_vec:sub(Center1, Center2)),
    {{Center,Vec},"Direction between faces saved as axis."};
get_vec(face, Faces, #we{vs=Vtab}=We) ->
    case wings_vertex:outer_partition(Faces, We) of
	[Vs] ->
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal(reverse(Vs), Vtab),
	    {{Center,Vec},"Edge loop normal for region saved as axis."};
	_Other ->
	    {none,"Multi-face selection must have a single edge loop."}
    end;

get_vec(_, _, _) -> {none,"Select vertices, edges, or faces."}.

%%%
%%% Point functions.
%%%

check_point(#st{sel=[]}) -> {none,"Nothing selected."};
check_point(St) ->
    Center = e3d_vec:average(wings_sel:bounding_box(St)),
    {Center,"Midpoint of selection saved."}.

%%%
%%% Magnet functions.
%%%

exit_magnet([], Acc, [N|Ns0]=Ns, St) ->
    %% Magnet must be last.
    case check_point(St) of
	{none,Msg} ->
	    wings_io:message(Msg),
	    invalid_selection;
	{Point,Msg} ->
	    wings_io:message(Msg),
	    Mag = {magnet,wings_pref:get_value(magnet_type),
		   wings_pref:get_value(magnet_distance_route),Point},
	    Cmd0 = wings_menu:build_command(N, Ns0),
	    Cmd = wings_util:stringify(Cmd0),
	    {Cmd,fun(1, _) -> {vector,{pick,[],[Mag|Acc],Ns}};
		    (2, _) -> {vector,{pick,[{magnet_options,Point}],Acc,Ns}};
		    (3, _) -> {vector,{pick,[{magnet_options,Point}],Acc,Ns}}
		 end,{"Execute command",[],"Set magnet options"},[]}
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
