%%
%%  wings_util.erl --
%%
%%     Various utility function that not obviously fit somewhere else.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_util.erl,v 1.8 2001/09/17 07:19:18 bjorng Exp $
%%

-module(wings_util).
-export([share/1,share/3,fold_shape/3,fold_shape_all/3,
	 fold_face/3,fold_vertex/3,fold_edge/3,
	 foreach_shape/2,foreach_face/2,foreach_edge/2,
	 average_normals/1,
	 tc/1,crasch_log/1,validate/1]).
-export([check_error/2,dump_we/2]).

-include("gl.hrl").
-include("wings.hrl").
-import(lists, [foreach/2,map/2,foldl/3,reverse/1,member/2]).

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

%%%
%%% `fold' functions.
%%%

fold_shape(F, Acc, #st{shapes=Shapes}) ->
    Iter = gb_trees:iterator(Shapes),
    fold_shape_1(F, Acc, Iter).

fold_shape_1(F, Acc, Iter0) ->
    case gb_trees:next(Iter0) of
	none -> Acc;
	{Id,Shape,Iter} ->
	    fold_shape_1(F, F(Shape, Acc), Iter)
    end.

%% Folds over all objects (including hidden). Use for save.
fold_shape_all(F, Acc, #st{shapes=Shapes,hidden=Hidden}) ->
    foldl(fun({Id,Shape}, A) ->
		  F(Shape, A)
	  end, Acc, gb_trees:to_list(Hidden) ++ gb_trees:to_list(Shapes)).

fold_face(F, Acc, #we{fs=Ftab}) ->
    fold_face_1(F, Acc, gb_trees:iterator(Ftab));
fold_face(F, Acc, #shape{sh=#we{fs=Ftab}}) ->
    fold_face_1(F, Acc, gb_trees:iterator(Ftab)).

fold_face_1(F, Acc, Iter0) ->
    case gb_trees:next(Iter0) of
	none -> Acc;
	{Num,Face,Iter} ->
	    fold_face_1(F, F(Num, Face, Acc), Iter)
    end.

fold_vertex(F, Acc, #we{vs=Vtab}) ->
    fold_vertex_1(F, Acc, gb_trees:iterator(Vtab));
fold_vertex(F, Acc, #shape{sh=#we{vs=Vtab}}=Sh) ->
    fold_vertex_1(F, Acc, gb_trees:iterator(Vtab));
fold_vertex(F, Acc, Vtab) ->
    fold_vertex_1(F, Acc, gb_trees:iterator(Vtab)).

fold_vertex_1(F, Acc, Iter0) ->
    case gb_trees:next(Iter0) of
	none -> Acc;
	{Num,Vertex,Iter} ->
	    fold_vertex_1(F, F(Num, Vertex, Acc), Iter)
    end.

fold_edge(F, Acc, #we{es=Etab}) ->
    fold_edge_1(F, Acc, gb_trees:iterator(Etab));
fold_edge(F, Acc, #shape{sh=#we{es=Etab}}=Sh) ->
    fold_edge_1(F, Acc, gb_trees:iterator(Etab)).

fold_edge_1(F, Acc, Iter0) ->
    case gb_trees:next(Iter0) of
	none -> Acc;
	{Edge,Rec,Iter} ->
	    fold_edge_1(F, F(Edge, Rec, Acc), Iter)
    end.

%%%
%%% `foreach' functions.
%%%

foreach_shape(F, St) ->
    fold_shape(fun (#shape{id=Id}=Sh, _) -> F(Id, Sh)
	       end, [], St).

foreach_face(F, #shape{sh=Data}=Sh) ->
    case Data of
	#we{fs=Ftab} ->
	    foreach_face_2(F, gb_trees:iterator(Ftab), Sh);
	Other -> ok
    end;
foreach_face(F, #st{shapes=Shapes}) ->
    Iter = gb_trees:iterator(Shapes),
    foreach_face_1(F, Iter).

foreach_face_1(F, Iter0) ->
    case gb_trees:next(Iter0) of
	none -> ok;
	{Num,#shape{sh=Data}=Sh,Iter} ->
	    case Data of
		#we{fs=Ftab} ->
		    foreach_face_2(F, gb_trees:iterator(Ftab), Sh),
		    foreach_face_1(F, Iter);
		Other -> ok
	    end
    end.

foreach_face_2(F, Iter0, Sh) ->
    case gb_trees:next(Iter0) of
	none -> ok;
	{Num,Face,Iter} ->
	    F(Num, Face, Sh),
	    foreach_face_2(F, Iter, Sh)
    end.

foreach_edge(F, #we{es=Etab}=We) ->
    foreach_edge_2(F, gb_trees:iterator(Etab), We);
foreach_edge(F, #shape{sh=#we{es=Etab}}=Sh) ->
    foreach_edge_2(F, gb_trees:iterator(Etab), Sh);
foreach_edge(F, #st{shapes=Shapes}) ->
    Iter = gb_trees:iterator(Shapes),
    foreach_edge_1(F, Iter).

foreach_edge_1(F, Iter0) ->
    case gb_trees:next(Iter0) of
	none -> ok;
	{Num,#shape{sh=#we{es=Etab}}=Sh,Iter} ->
	    foreach_edge_2(F, gb_trees:iterator(Etab), Sh),
foreach_edge_1(F, Iter)
    end.

foreach_edge_2(F, Iter0, Sh) ->
    case gb_trees:next(Iter0) of
	none -> ok;
	{Num,Edge,Iter} ->
	    F(Num, Edge, Sh),
	    foreach_edge_2(F, Iter, Sh)
    end.

average_normals(Vs) ->
    R = sofs:relation(Vs),
    F = sofs:relation_to_family(R),
    foldl(fun average_normals/2, [], sofs:to_external(F)).

average_normals({V,Normals}, Acc) ->
    Normal = e3d_vec:mul(average_normals_1(Normals),
			 ?GROUND_GRID_SIZE),
    [{Normal,[V]}|Acc].

%% average_normals(Normals) -> Normal
%%  Average normals taking the angle between them into account.
%%  XXX Not the proper way.
average_normals_1([N|Ns]) ->
    average_normals_2(Ns, N).

average_normals_2([N0|Ns], Sum0) ->
    Sum1 = e3d_vec:add(N0, e3d_vec:norm(Sum0)),
    Dot = e3d_vec:dot(N0, Sum1),
    Sum = e3d_vec:add(e3d_vec:divide(N0, Dot), Sum0),
    average_normals_2(Ns, Sum);
average_normals_2([], Sum) -> Sum.

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
%%% Crasch log writing.
%%%

crasch_log(BackTrace) ->
    LogName = filename:absname("wings_crasch.dump"),
    F = open_log_file(LogName),
    io:format(F, "Crashed in:\n~p\n\n", [BackTrace]),
    analyse(F, BackTrace),
    file:close(F),
    LogName.

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
try_arg(F, #shape{}=Sh, N) ->
    arg(F, N),
    dump_shape(F, Sh);
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

dump_shape(F, #shape{sh=#we{}=We}) ->
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
    foreach(fun ({_,#shape{sh=#we{}=We}}) ->
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
	    is_vertice_seen(V, Acc, Face, Edge, We),
	    walk_face_cw(Face, Next, LastEdge, We, [V|Acc]);
	#edge{ve=V,rf=Face,rtsu=Next} ->
	    is_vertice_seen(V, Acc, Face, Edge, We),
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
	    is_vertice_seen(V, Acc, Face, Edge, We),
	    walk_face_ccw(Face, Next, LastEdge, We, [V|Acc]);
	#edge{vs=V,rf=Face,rtpr=Next} ->
	    is_vertice_seen(V, Acc, Face, Edge, We),
	    walk_face_ccw(Face, Next, LastEdge, We, [V|Acc]);
	{'EXIT',_} ->
	    [{make_ref(),crash,missing_edge,Edge,
	     [Face,Edge,LastEdge,We,Acc]}];
	Other ->
	    [{make_ref(),{crash,Other},
	      {face,Face,edge,Edge,last_edge,LastEdge,acc,Acc}}]
    end.

is_vertice_seen(V, Vs, Face, Edge, We) ->
    false.
%     case member(V, Vs) of
% 	true -> erlang:fault(validate, [V,Vs,Face,Edge,We]);
% 	false -> ok
%     end.

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
