%%
%%  wings_util.erl --
%%
%%     Various utility function that not obviously fit somewhere else.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_util.erl,v 1.29 2002/02/10 18:17:11 bjorng Exp $
%%

-module(wings_util).
-export([error/1,share/1,share/3,make_vector/1,
	 message/1,yes_no/1,serious_yes_no/1,
	 cap/1,upper/1,stringify/1,add_vpos/2,update_vpos/2,
	 delete_any/2,
	 tc/1,crash_log/1,validate/1]).
-export([check_error/2,dump_we/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [foreach/2,map/2,foldl/3,reverse/1,member/2]).

error(Message) when is_list(Message) ->
    throw({command_error,Message}).
    
share(X, X, X) -> {X,X,X};
share(X, X, Z) -> {X,X,Z};
share(X, Y, Y) -> {X,Y,Y};
share(X, Y, X) -> {X,Y,X};
share(X, Y, Z) -> {X,Y,Z}.

share({X,X,X}) -> {X,X,X};
share({X,X,Z}) -> {X,X,Z};
share({X,Y,Y}) -> {X,Y,Y};
share({X,Y,X}) -> {X,Y,X};
share(Other) -> Other.

make_vector(x) -> {1.0,0.0,0.0};
make_vector(y) -> {0.0,1.0,0.0};
make_vector(z) -> {0.0,0.0,1.0};
make_vector(free) -> free;
make_vector(normal) -> normal;
make_vector(intrude) -> normal.

message(Message) ->
    wings_plugin:call_ui({message,Message}).

yes_no(Question) ->
    wings_plugin:call_ui({question,Question}).

serious_yes_no(Question) ->
    wings_plugin:call_ui({serious_question,Question}).

stringify({{_,_,_},{_,_,_}}) ->
    "(vector)";
stringify({Atom,Other}) when is_atom(Atom) ->
    wings_util:cap(atom_to_list(Atom)) ++ "|" ++ stringify(Other);
stringify(Atom) when is_atom(Atom) ->
    wings_util:cap(atom_to_list(Atom));
stringify(Int) when integer(Int) ->
    integer_to_list(Int);
stringify(Other) -> "UNKNOWN".

cap(Str) when is_atom(Str) -> cap(atom_to_list(Str));
cap(Str) -> cap(Str, true).

cap([Lower|T], true) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|cap(T, false)];
cap([$_|T], Any) ->
    [$\s|cap(T, true)];
cap([H|T], Any) ->
    [H|cap(T, false)];
cap([], Flag) -> [].
    
upper(Str) when is_atom(Str) -> upper(atom_to_list(Str));
upper([Lower|T]) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|upper(T)];
upper([H|T]) ->
    [H|upper(T)];
upper([]) -> [].

add_vpos(Vs, #we{vs=Vtab}) -> add_vpos(Vs, Vtab);
add_vpos(Vs, Vtab) ->
    foldl(fun(V, A) ->
		  [{V,gb_trees:get(V, Vtab)}|A]
	  end, [], Vs).

update_vpos(Vs, #we{vs=Vtab}) -> update_vpos(Vs, Vtab);
update_vpos(Vs, Vtab) ->
    foldl(fun({V,_}, A) ->
		  [{V,gb_trees:get(V, Vtab)}|A]
	  end, [], reverse(Vs)).

delete_any(K, S) ->
    case gb_sets:is_member(K, S) of
	true -> gb_sets:delete(K, S);
	false -> S
    end.

%%
%% Timing.
%% 

tc(Fun) ->
    case timer:tc(erlang, apply, [Fun,[]]) of
	{T,{'EXIT',Reason}} -> exit(Reason);
	{T,R} ->
	    io:format("Time: ~p\n", [T]),
	    R
    end.

%%
%% Dumping of data structures.
%% 

show_edge(F, Edge, #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,ltpr=Lpred,ltsu=Lsucc,
			 rtpr=Rpred,rtsu=Rsucc}) ->
    io:format(F, "~p: vs=~p ve=~p\n", [Edge,Vs,Ve]),
    io:format(F, "  left: face=~p pred=~p succ=~p\n", [Lf,Lpred,Lsucc]),
    io:format(F, "  right: face=~p pred=~p succ=~p\n", [Rf,Rpred,Rsucc]).

show_face(F, Face, #face{edge=Edge}) ->
    io:format(F, "~p: edge=~p\n", [Face,Edge]).

show_vertex(F, Vertex, #vtx{edge=Edge,pos=Pos}) ->
    io:format(F, "~p: edge=~p pos=~p\n", [Vertex,Edge,Pos]).

%%%
%%% Crash log writing.
%%%

crash_log(BackTrace) ->
    LogFileDir = log_file_dir(),
    LogName = filename:absname("wings_crash.dump", LogFileDir),
    F = open_log_file(LogName),
    io:format(F, "Crashed in:\n~p\n\n", [BackTrace]),
    analyse(F, BackTrace),
    file:close(F),
    LogName.

log_file_dir() ->
    case catch log_file_dir(os:type()) of
	Dir when is_list(Dir) -> Dir;
	Other -> "."
    end.

log_file_dir({unix,_}) -> os:getenv("HOME");
log_file_dir({win32,_}) ->
    case code:which(?MODULE) of
	Name0 when is_list(Name0) ->
	    Name = filename:dirname(Name0),
	    case filename:basename(Name) of
		"ebin" -> filename:dirname(Name);
		"patches" -> filename:dirname(Name);
		Other -> Name
	    end
    end.
	
open_log_file(Name) ->
    {ok,F} = file:open(Name, [write]),
    {{Y,Mo,D},{H,Mi,_}} = erlang:localtime(),
    io:format(F, "Dump written ~p-~p-~p_~p-~p\n", [Y,Mo,D,H,Mi]),
    F.

analyse(F, {_,[{Mod,Fun,Args}|_]}) when list(Args) ->
    try_args(F, Args, 1);
analyse(F, _) -> ok.

try_args(F, [A|As], Num) ->
    try_arg(F, A, Num),
    try_args(F, As, Num+1);
try_args(F, _, _) -> ok.

try_arg(F, #st{shapes=Shapes}, N) ->
    arg(F, N),
    foreach(fun({Id,Sh}) ->
		    io:format(F, "Shape ~p\n", [Id]),
		    dump_shape(F, Sh)
	    end, gb_trees:to_list(Shapes));
try_arg(F, #we{}=We, N) ->
    arg(F, N),
    dump_we(F, We);
try_arg(F, {I,_}=GbTree, N) when integer(I) ->
    case catch gb_trees:to_list(GbTree) of
	{'EXIT',_} -> ok;
	[{Id,#edge{}}|_]=Es ->
	    arg(F, N),
	    dump_edges(F, Es);
	[{Id,#face{}}|_]=Fs ->
	    arg(F, N),
	    dump_faces(F, Fs);
	[{Id,#vtx{}}|_]=Vs ->
	    arg(F, N),
	    dump_vertices(F, Vs);
	_ -> ok
    end;
try_arg(F, A, N) -> ok.

arg(F, N) ->
    io:format(F, "Argument #~p:\n", [N]).

dump_shape(F, #we{}=We) ->
    dump_we(F, We).

dump_we(F, #we{es=Etab,vs=Vtab,fs=Ftab}) ->
    dump_vertices(F, gb_trees:to_list(Vtab)),
    dump_faces(F, gb_trees:to_list(Ftab)),
    dump_edges(F, gb_trees:to_list(Etab)).
    
dump_vertices(F, Vs) ->
    io:put_chars(F, "\n"),
    io:format(F, "Vertex table\n", []),
    io:format(F, "============\n\n", []),
    foreach(fun({Vertex,Vrec}) -> show_vertex(F, Vertex, Vrec) end, Vs).

dump_edges(F, Es) ->
    io:put_chars(F, "\n"),
    io:format(F, "Edge table\n", []),
    io:format(F, "===========\n\n", []),
    foreach(fun({Edge,Erec}) -> show_edge(F, Edge, Erec) end, Es).

dump_faces(F, Fs) ->
    io:put_chars(F, "\n"),
    io:format(F, "Face table\n", []),
    io:format(F, "===========\n\n", []),
    foreach(fun({Face,Frec}) -> show_face(F, Face, Frec) end, Fs).

%%
%% Validation of shapes.
%%

validate(X) ->
    validate_1(X),
    X.

validate_1(#st{shapes=Shapes}) ->
    foreach(fun ({_,#we{}=We}) ->
		    validate_we(We);
		({_,_}) -> ok end,
	    gb_trees:to_list(Shapes));
validate_1(#we{}=We) -> validate_we(We).

validate_we(#we{}=We) ->
    validate_vertex_tab(We),
    validate_faces(We).
    
validate_faces(#we{fs=Ftab}=We) ->
    foreach(fun({Face,#face{edge=Edge}}) ->
		    Cw = walk_face_cw(Face, Edge, Edge, We, []),
		    Ccw = walk_face_ccw(Face, Edge, Edge, We, []),
 		    case reverse(Ccw) of
 			Cw ->
 			    ok;
 			Other ->
 			    erlang:fault({crash,{face,Face},
					  {cw,Cw},{ccw_reversed,Other}},
 					  [We])
 		    end
	    end,
	    gb_trees:to_list(Ftab)).

walk_face_cw(Face, LastEdge, LastEdge, We, [_|_]=Acc) -> Acc;
walk_face_cw(Face, Edge, LastEdge, We, Acc) ->
    #we{es=Etab} = We,
    case catch gb_trees:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=Next} ->
	    walk_face_cw(Face, Next, LastEdge, We, [V|Acc]);
	#edge{ve=V,rf=Face,rtsu=Next} ->
	    walk_face_cw(Face, Next, LastEdge, We, [V|Acc]);
	{'EXIT',_} ->
	    [{make_ref(),crash,missing_edge,Edge,
	     [Face,Edge,LastEdge,We,Acc]}];
	Other ->
	    [{make_ref(),{crash,Other},
	      {face,Face,edge,Edge,last_edge,LastEdge,acc,Acc}}]
    end.

walk_face_ccw(Face, LastEdge, LastEdge, We, [_|_]=Acc) -> Acc;
walk_face_ccw(Face, Edge, LastEdge, We, Acc) ->
    #we{es=Etab} = We,
    case catch gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=Next} ->
	    walk_face_ccw(Face, Next, LastEdge, We, [V|Acc]);
	#edge{vs=V,rf=Face,rtpr=Next} ->
	    walk_face_ccw(Face, Next, LastEdge, We, [V|Acc]);
	{'EXIT',_} ->
	    [{make_ref(),crash,missing_edge,Edge,
	     [Face,Edge,LastEdge,We,Acc]}];
	Other ->
	    [{make_ref(),{crash,Other},
	      {face,Face,edge,Edge,last_edge,LastEdge,acc,Acc}}]
    end.

validate_vertex_tab(#we{es=Etab,vs=Vtab}=We) ->
    foreach(fun({V,#vtx{edge=Edge}}) ->
		    case gb_trees:get(Edge, Etab) of
			#edge{vs=V}=Rec ->
			    validate_edge_rec(Rec, We);
			#edge{ve=V}=Rec ->
			    validate_edge_rec(Rec, We);
			Other ->
			    erlang:fault({crasch,{vertex,V}}, [We])
		    end
	    end,
	    gb_trees:to_list(Vtab)).

validate_edge_rec(Rec, We) ->
    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
    if
	integer(LP+LS+RP+RS) -> ok;
	true -> erlang:fault(crasch, {Rec,We})
    end.

-ifdef(DEBUG).
check_error(Mod, Line) ->
    S = case gl:getError() of
	    ?GL_INVALID_VALUE -> "GL_INVALID_VALUE";
	    ?GL_INVALID_ENUM -> "GL_INVALID_ENUM";
	    ?GL_INVALID_OPERATION -> "GL_INVALID_OPERATION";
	    ?GL_STACK_OVERFLOW -> "GL_STACK_OVERFLOW";
	    ?GL_STACK_UNDERFLOW -> "GL_STACK_UNDERFLOW";
	    ?GL_OUT_OF_MEMORY -> "GL_OUT_OF_MEMORY";
	    0 -> no_error
	end,
    case S of
	no_error -> ok;
	Other ->
	    io:format("~p, line ~p: ~s\n", [Mod,Line,S]),
	    erlang:fault(gl_error)
    end.
-else.
check_error(Mod, Line) ->
    ok.
-endif.
