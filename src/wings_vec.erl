%%
%%  wings_vec.erl --
%%
%%     This module implements vectors and the secondary selection mode.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vec.erl,v 1.11 2002/02/17 20:01:56 bjorng Exp $
%%

-module(wings_vec).

-export([menu/1,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,keydelete/3,reverse/1,member/2]).

-record(ss, {check,				%Check fun.
	     exit,				%Exit fun.
	     selmodes				%Legal selection modes.
	    }).

menu(St) ->
    [{advanced,{"Named Vector",
		{named_vector,fun(Key, Ns) ->
				      named_vector(Key, St)
			      end}}}].

named_vector(help, St) -> "Rename or delete vector";
named_vector(1, St) -> {vector,rename};
named_vector(2, St) -> ignore;
named_vector(3, St) -> ignore.

command({save_unnamed,{Vec,Ns}}, St) ->
    wings_io:putback_event({action,{vector,{use_vector,{unnamed,Vec,Ns}}}}),
    case St of
	#st{svec=[{unnamed,_}|Vecs]} ->
	    St#st{svec=[{unnamed,Vec}|Vecs]};
	#st{svec=Vecs} ->
	    St#st{svec=[{unnamed,Vec}|Vecs]}
    end;
command({pick_named,Names}, St) ->
    pick_named(Names, St);
command({use_vector,{Name,{Vec,_}=Vec0,Ns}}, St) ->
    Cmd = wings_menu:build_command(Vec, Ns),
    wings_io:putback_event({action,Cmd}),
    move_to_front({Name,Vec0}, St#st{vec=Vec});
command({dynamic_use_vector,{Name,Vec0,Ns}}, St) ->
    Vec = get_dynamic_vector(Vec0, St),
    Cmd = wings_menu:build_command(Vec, Ns),
    wings_io:putback_event({action,Cmd}),
    move_to_front({Name,Vec0}, St#st{vec=Vec});
command({pick_new,Names}, St0) ->
    Modes = [vertex,edge,face],
    wings_io:icon_restriction(Modes),
    Ss = #ss{check=fun check_vector/1,
	     exit=fun(X, Y, St) -> exit_vector(X, Y, Names, St) end,
	     selmodes=Modes},
    wings_io:message("Select vector to use."),
    {seq,{push,dummy},get_event(Ss, St0#st{sel=[]})};
command({pick_special,{Modes,Init,Check,Exit}}, St0) ->
    wings_io:icon_restriction(Modes),
    St = Init(St0),
    Ss = #ss{selmodes=Modes,check=Check,exit=Exit},
    {seq,{push,dummy},get_event(Ss, St)};
command(rename, St) ->
    name_menu("Rename or Delete Vector", do_rename, dummy, St);
command({do_rename,{unnamed,Vec,_}}, St) ->
    wings_ask:ask([{"New name (leave empty to delete)","Unnamed"}], St,
		  fun([Name]) ->
			  {vector,{do_rename,Vec,unnamed,Name}}
		  end);
command({do_rename,{Name0,Vec,_}}, St) ->
    wings_ask:ask([{"New name (leave empty to delete)",
		    atom_to_list(Name0)}], St,
		  fun([Name]) ->
			  {vector,{do_rename,Vec,Name0,Name}}
		  end);
command({do_rename,Vec,OldName,NewName}, St) ->
    do_rename(OldName, NewName, Vec, St).

do_rename(Name, NewName0, Vec, #st{svec=Svec0}=St) ->
    Svec1 = keydelete(Name, 1, Svec0),
    Svec = case NewName0 of
	       [] ->
		   Svec1;
	       Other ->
		   NewName = list_to_atom(NewName0),
		   insert_in_front({NewName,Vec}, Svec1)
	   end,
    St#st{svec=Svec}.

move_to_front({Name,_}=Vec, #st{svec=Vecs0}=St) ->
    Vecs = keydelete(Name, 1, Vecs0),
    St#st{svec=insert_in_front(Vec, Vecs)}.
    
insert_in_front(Vec, [{unnamed,_}=Unnamed|Vecs]) ->
    [Unnamed,Vec|Vecs];
insert_in_front(Vec, Vecs) ->
    [Vec|Vecs].

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
	Other -> keep
    end.

handle_event_5({new_selection,St}, #ss{check=Check}=Ss, St0) ->
    {Vec,Msg} = Check(St),
    wings_io:message(Msg),
    get_event(Ss, St#st{vec=Vec});
handle_event_5(#keyboard{}=Event, Ss, St) ->
    get_event(Ss, St);
handle_event_5(redraw, Ss, St) ->
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
handle_event_5({action,{secondary_selection,abort}}, Ss, St) ->
    wings_io:clear_message(),
    wings_io:putback_event(redraw),
    pop;
handle_event_5({action,Cmd}, Ss, St) ->
    wings_io:clear_message(),
    wings_io:putback_event({action,Cmd}),
    pop;
handle_event_5(quit, Ss, St0) ->
    wings_io:putback_event(quit),
    pop;
handle_event_5(Event, Ss, St) ->
    get_event(Ss, St).

filter_sel_command(#ss{selmodes=Modes}=Ss, #st{selmode=Mode}=St) ->
    case member(Mode, Modes) of
	true -> handle_event({new_selection,St}, Ss, St);
	false -> keep
    end.

translate_key(#keyboard{keysym=KeySym}) ->
    translate_key_1(KeySym);
translate_key(Event) -> next.

translate_key_1(#keysym{sym=27}) ->		%Escape
    wings_io:message("Command aborted"),
    wings_io:putback_event(redraw),
    pop;
translate_key_1(Other) -> next.

exit_menu(X, Y, #ss{exit=Exit}, St) ->
    case Exit(X, Y, St) of
	invalid_selection ->
	    exit_menu_invalid(X, Y, St);
	MenuEntry ->
	    exit_menu_done(X, Y, MenuEntry, St)
    end.

exit_menu_invalid(X, Y, St) ->
    Menu = [{"Invalid Selection",ignore},
	    separator,
	    {"Abort Command",abort}],
    wings_menu:popup_menu(X, Y, secondary_selection, Menu, St).

exit_menu_done(X, Y, MenuEntry, St) ->
    Menu = [MenuEntry,
	    {"Abort Command",abort}],
    wings_menu:popup_menu(X, Y, secondary_selection, Menu, St).

%%%
%%% Show menu of named vectors.
%%%

name_menu(Title, CmdTag, Ns, #st{svec=Vecs}=St) ->
    Menu = [{Title,ignore},separator|named_vectors(Vecs, CmdTag, Ns)],
    {_,X,Y} = sdl_mouse:getMouseState(),
    wings_menu:popup_menu(X, Y, use_named_vector, Menu, St).

named_vectors([], CmdTag, Ns) -> [{"(No vectors)",ignore}];
named_vectors([{unnamed,Vec}|T], CmdTag, Ns) ->
    [{"(Last vector)",vec_fun(unnamed, Vec, CmdTag, Ns)}|
     named_vectors_1(T, CmdTag, Ns)];
named_vectors(T, CmdTag, Ns) -> named_vectors_1(T, CmdTag, Ns).

named_vectors_1([{Name,Vec}|T], CmdTag, Ns) ->
    [{atom_to_list(Name),vec_fun(Name, Vec, CmdTag, Ns)}|
     named_vectors_1(T, CmdTag, Ns)];
named_vectors_1([], CmdTag, Ns) -> [].
    
vec_fun(Name, Vec, CmdTag, Ns) ->
    fun(_, _) -> {vector,{CmdTag,{Name,Vec,Ns}}} end.

%%%
%%% Show menu of vectors to be used.
%%%

pick_named(Ns, #st{svec=Vecs}=St) ->
    Menu = [{"Use Vector",ignore},separator|pick_named_1(Vecs, Ns)],
    {_,X,Y} = sdl_mouse:getMouseState(),
    wings_menu:popup_menu(X, Y, use_named_vector, Menu, St).

pick_named_1(Vecs, Ns) ->
    pick_named(Vecs, Ns, []).

pick_named([], Ns, []) -> [{"(No vectors)",ignore}];
pick_named([], Ns, Acc) -> reverse(Acc);
pick_named([{Name0,Vec}|Vecs], Ns, Acc) ->
    F = fun(help, _) ->
		{"Use vector in saved position",[],
		 "Recalculate vector based on current geometry"};
	   (1, _) ->
		{vector,{use_vector,{Name0,Vec,Ns}}};
	   (3, _) ->
		{vector,{dynamic_use_vector,{Name0,Vec,Ns}}};
	   (_, _) ->
		ignore
	end,
    Name = stringify_name(Name0),
    pick_named(Vecs, Ns, [{Name,{use_vector,F}}|Acc]).

%%%
%%% Vector functions.
%%%

exit_vector(X, Y, Ns, St) ->
    case check_vector(St) of
	{none,Msg} ->
	    wings_io:message(Msg),
	    invalid_selection;
	{Vec,Msg} ->
	    wings_io:message(Msg),
	    UseAction = fun(_, _) -> {vector,{save_unnamed,{Vec,Ns}}} end,
	    FakeVec = {{0,0,0},{0,0,0}},
	    Command0 = wings_menu:build_command(FakeVec, Ns),
	    Command = wings_util:stringify(Command0),
	    {Command,UseAction}
    end.

check_vector(#st{sel=[]}) -> {none,"Nothing selected"};
check_vector(#st{selmode=Mode,sel=Sel}=St) ->
    case findvec(Mode, Sel, St) of
	{{none,_},Msg} ->
	    {none, "Invalid selection - " ++ Msg};
	{_,_}=VecMsg -> VecMsg
    end.

get_dynamic_vector({Vec0,{Type,Sel}}, St) ->
    case findvec(Type, Sel, St) of
	{{none,none},Msg} -> Vec0;
	{{Vec,_},_} -> Vec
    end.

findvec(Type, Sel, #st{shapes=Shs}=St) ->
    case wings_sel:valid_sel(Sel, Type, St) of
	[{Id,Items0}] ->
            We = gb_trees:get(Id, Shs),
	    Items = gb_sets:to_list(Items0),
            {Vec,Msg} = get_vec(Type, Items, We),
            {{Vec,{Type,Sel}},Msg};
	[] ->
	    {{none,none},"Nothing selected"};
        Other -> {{none,none},"Select parts of one object only"}
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
%% Use edge-loop normal.
get_vec(edge, Edges, #we{vs=Vtab}=We) ->
    case wings_edge_loop:edge_loop_vertices(Edges, We) of
	[Vs] -> 
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal(reverse(Vs), Vtab),
	    {{Center,Vec},"Edge loop normal saved as axis."};
	Other ->
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
	Other ->
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
	Other ->
	    {none,"Multi-face selection must have a single edge loop."}
    end;

get_vec(_, _, _) -> {none,"Select vertices, edges, or faces."}.

%%%
%%% Utilities.
%%%

stringify_name(unnamed) -> "(Last vector)";
stringify_name(Name) -> atom_to_list(Name).

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
find_edges([], VsSet, We, Acc) -> Acc.
