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
%%     $Id: wings_vec.erl,v 1.3 2002/01/27 11:50:28 bjorng Exp $
%%

-module(wings_vec).

-export([menu/1,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,keydelete/3]).

-record(ss, {type,				%Type to pick.
	     names,				%Names (command reversed).
	     st					%Original st record.
	    }).

menu(St) ->
    [{advanced,{"Named Vector",
		{named_vector,fun(Key, Ns) ->
				      named_vector(Key, St)
			      end}}}].

named_vector(help, St) ->
    {"Save selection as vector",[],"Rename or delete vector"};
named_vector(1, St) -> {vector,save_named};
named_vector(2, St) -> ignore;
named_vector(3, St) -> {vector,rename}.

command(save_named, St) ->
    St;
command({save_unnamed,{Vec,Ns}}, St) ->
    wings_io:putback_event({action,{vector,{use_vector,{unnamed,Vec,Ns}}}}),
    case St of
	#st{svec=[{unnamed,_}|Vecs]} ->
	    St#st{svec=[{unnamed,Vec}|Vecs]};
	#st{svec=Vecs} ->
	    St#st{svec=[{unnamed,Vec}|Vecs]}
    end;
command({pick_named,Names}, St) ->
    name_menu("Use Vector", use_vector, Names, St);
command({use_vector,{Name,{Vec0,_}=Vec,Ns}}, St) ->
    Cmd = wings_menu:build_command(Vec0, Ns),
    wings_io:putback_event({action,Cmd}),
    move_to_front({Name,Vec}, St);
command({pick_new,Names}, St) ->
    Ss = #ss{type=vector,names=Names,st=St},
    wings_io:message("Select vector to use."),
    {seq,{push,dummy},get_event(Ss, St#st{sel=[]})};
command(rename, St) ->
    name_menu("Rename or Delete Vector", do_rename, dummy, St);
command({do_rename,{unnamed,Vec,_}}, St) ->
    wings_util:prompt("New name (leave empty to delete)", "Unnamed",
		      fun(Name) ->
			      do_rename(unnamed, Name, Vec, St)
		      end);
command({do_rename,{Name0,Vec,_}}, St) ->
    wings_util:prompt("New name (leave empty to delete)",
		      atom_to_list(Name0),
		      fun(Name) ->
			      do_rename(Name0, Name, Vec, St)
		      end).

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
	{Vec,Msg} ->
	    wings_io:message(Msg),
	    exit_menu_done(X, Y, Vec, Ss, St)
    end.

exit_menu_invalid(X, Y, #ss{type=Type,names=Names}, St) ->
    Reselect = fun(_, _) -> {vector,{Type,Names}} end,
    Menu = [{"Invalid Vector Selection",ignore},
	    separator,
	    {"Abort Command",abort}],
    wings_menu:popup_menu(X, Y, secondary_selection, Menu, St).

exit_menu_done(X, Y, Vec, #ss{names=Ns}, St) ->
    UseAction = fun(_, _) -> {vector,{save_unnamed,{Vec,Ns}}} end,
    Menu = [{"Vector Selection",ignore},
	    separator,
	    {"Use Current Vector",UseAction},
	    separator,
	    {"Abort Command",aborted}],
    {seq,pop,wings_menu:popup_menu(X, Y, secondary_selection, Menu, St)}.

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
