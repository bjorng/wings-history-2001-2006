%%
%%  wings_sec_sel.erl --
%%
%%     This module implements the secondary selection mode.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vec.erl,v 1.1 2002/01/25 09:04:38 bjorng Exp $
%%

-module(wings_vec).

-export([pick/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3]).

-record(ss, {type,				%Type to pick.
	     names,				%Names (command reversed).
	     st					%Original st record.
	    }).

pick(Type, Names, St) ->
    Ss = #ss{type=Type,names=Names,st=St},
    wings_io:message("Select vector to use."),
    {seq,{push,dummy},get_event(Ss, St#st{sel=[]})}.

get_event(Ss, St) ->
    wings:redraw(St),
    {replace,fun(Ev) -> handle_event(Ev, Ss, St) end}.

handle_event(Event, Ss, St) ->
    Redraw = fun() -> wings:redraw(St) end,
    case wings_camera:event(Event, Redraw) of
	next -> handle_event_1(Event, Ss, St);
	Other -> Other
    end.

handle_event_1(Event, Ss, St) ->
    case wings_pick:event(Event, St) of
	next -> handle_event_2(Event, Ss, St);
	Other -> Other
    end.

handle_event_2(Event, Ss, St) ->
    case wings_menu:is_popup_event(Event) of
	no -> handle_event_3(Event, Ss, St);
	{yes,X,Y} -> exit_menu(X, Y, Ss, St)
    end.
handle_event_3(Event, Ss, St0) ->
    case wings_hotkey:event(Event) of
	next -> handle_event_4(Event, Ss, St0);
	{view,Cmd} ->
	    wings_view:command(Cmd, St0),
	    keep;
	{select,Cmd} ->
	    case wings_sel_cmd:command(Cmd, St0) of
		St0 -> keep;
		{save_state,St} -> handle_event({new_selection,St}, Ss, St)
	    end;
	Other -> keep
    end.

handle_event_4({new_selection,St}, Ss, St0) ->
    {Vec,Msg} = check_vector(St),
    wings_io:message(Msg),
    get_event(Ss, St#st{vec=Vec});
handle_event_4(#keyboard{}=Event, Ss, St) ->
    get_event(Ss, St);
handle_event_4(redraw, Ss, St) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings:redraw(St),
    keep;
handle_event_4({action,{secondary_selection,abort}}, Ss, St) ->
    wings_io:clear_message(),
    pop;
handle_event_4(Event, Ss, St) ->
    keep.

exit_menu(X, Y, Ss, St) ->
    case check_vector(St) of
	{none,Msg} ->
	    wings_io:message(Msg),
	    exit_menu_invalid(X, Y, Ss, St);
	{{Vec,_},Msg} ->
	    wings_io:message(Msg),
	    exit_menu_done(X, Y, Vec, Ss, St)
    end.

exit_menu_invalid(X, Y, #ss{type=Type,names=Names}, St) ->
    Reselect = [fun(_, _) -> {pick,{Type,Names}} end],
    Menu = {{"Invalid Vector Selection",ignore},
	    separator,
	    {"Abort Command",abort}},
    wings_menu:popup_menu(X, Y, secondary_selection, Menu, St).

exit_menu_done(X, Y, Vec, #ss{names=Names}, St) ->
    UseAction = [fun(_, _) -> use_current(Vec, Names) end],
    Menu = {{"Vector Selection",ignore},
	    separator,
	    {"Use Current Vector",UseAction},
	    separator,
	    {"Abort Command",aborted}},
    {seq,pop,wings_menu:popup_menu(X, Y, secondary_selection, Menu, St)}.

use_current(Vec, Names) ->
    foldl(fun(N, A) -> {N,A} end, Vec, Names).

%%%
%%% Vector functions.
%%%

check_vector(#st{sel=[]}) -> {none,"Nothing selected"};
check_vector(#st{selmode=Mode,sel=Sel}=St) ->
    case findvec(Mode, Sel, St) of
	{{none,_},Msg} ->
	    {none, "Invalid selection - " ++ Msg};
	{_,_}=VecMsg -> VecMsg
    end.

findvec(Type, Sel0, #st{shapes=Shs}=St) ->
    case wings_sel:valid_sel(Sel0, Type, St) of
	[] -> {{none,none},"Nothing selected"};
	[{Id,Sel1}] ->
            We = gb_trees:get(Id, Shs),
            Sel = gb_sets:to_list(Sel1),
            {Vec,Msg} = get_vec(Type, Sel, We),
            {{Vec,{Type,Sel}},Msg};
        _ -> {{none,none}, "Select parts of one object only"}
    end.

%% Use single edge as axis
get_vec(edge, [Edge], #we{es=Etab,vs=Vtab}=We) ->
    #edge{vs=Va,ve=Vb,lf=FaceL,rf=FaceR} = gb_trees:get(Edge, Etab),
    VaPos = wings_vertex:pos(Va, Vtab),
    VbPos = wings_vertex:pos(Vb, Vtab),
    Vec = e3d_vec:norm(e3d_vec:sub(VbPos, VaPos)),
    Center = wings_vertex:center([Va,Vb], We),
    {{Center,Vec},"Edge saved as axis."};
%% Use direction between two edges
get_vec(edge, [Edge1,Edge2], #we{es=Etab,vs=Vtab}=We) ->
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
%% Invalid # of edges selected
get_vec(edge, Edges, We) -> {none,"Select 1 or 2 edges only"};

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
get_vec(vertex, Vs, We) -> {none,"Select 1, 2, or 3 vertices only"};

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
	    Vec = wings_face:face_normal(Vs, Vtab),
	    {{Center,Vec},"Edge loop normal for region saved as axis."};
	Other ->
	    {none,"Multi-face selection must have a single edge loop."}
    end;

get_vec(_, _, _) -> {none,"Select vertices, edges, or faces."}.
