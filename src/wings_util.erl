%%
%%  wings_util.erl --
%%
%%     Various utility functions that not obviously fit somewhere else.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_util.erl,v 1.89 2004/02/22 06:02:05 bjorng Exp $
%%

-module(wings_util).
-export([error/1,error/2,share/1,share/3,make_vector/1,
	 validate_mirror/1,rel2fam/1,
	 button_message/1,button_message/2,button_message/3,
	 button_format/1,button_format/2,button_format/3,
	 rmb_format/1,
	 join_msg/1,join_msg/2,
	 message/1,
	 magnet_string/0,
	 yes_no/2,yes_no/3,yes_no_cancel/3,
	 get_matrices/2,mirror_matrix/1,
	 mirror_flatten/2,
	 cap/1,upper/1,stringify/1,add_vpos/2,update_vpos/2,
	 gb_trees_smallest_key/1,gb_trees_largest_key/1,
	 nice_float/1,
	 menu_restriction/2,
	 unique_name/2,
	 init_gl_extensions/0,is_gl_ext/1,is_gl_ext/2,
	 init_gl_restrictions/0,is_gl_restriction/1,
	 geom_windows/0,
	 tc/3,export_we/2,crash_log/2,validate/1,validate/3,
	 gl_error_string/1,
	 min/2,max/2]).
-export([check_error/2,dump_we/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foreach/2,map/2,foldl/3,reverse/1,member/2,last/1]).

error(Message) when is_list(Message) ->
    wings_pb:cancel(),
    throw({command_error,Message}).

error(Format, Arg) ->
    error(lists:flatten(io_lib:format(Format, Arg))).
    
share(X, X, X) -> {X,X,X};
share(X, X, Z) -> {X,X,Z};
share(X, Y, Y) -> {X,Y,Y};
share(X, Y, X) -> {X,Y,X};
share(X, Y, Z) -> {X,Y,Z}.

share({X,X,X}) -> {X,X,X};
share({X,X,Z}) -> {X,X,Z};
share({X,Y,Y}) -> {X,Y,Y};
share({X,Y,X}) -> {X,Y,X};
%%
share({X,X,X,X}) -> {X,X,X,X};
%%
share({X,X,X,A}) -> {X,X,X,A};
share({X,X,Z,X}) -> {X,X,Z,X};
share({X,Y,X,X}) -> {X,Y,X,X};
share({X,Y,Y,Y}) -> {X,Y,Y,Y};
%%
share({X,X,Y,Y}) -> {X,X,Y,Y};
share({X,Y,X,Y}) -> {X,Y,X,Y};
share({X,Y,Y,X}) -> {X,Y,Y,X};
%%
share({X,X,Z,A}) -> {X,X,Z,A};
share({X,Y,X,A}) -> {X,Y,X,A};
share({X,Y,Z,X}) -> {X,Y,Z,X};
share({X,Y,Y,A}) -> {X,Y,Y,A};
share({X,Y,Z,Y}) -> {X,Y,Z,Y};
share({X,Y,Z,Z}) -> {X,Y,Z,Z};
%%
share(Other) -> Other.

make_vector({_,_,_}=Vec) -> Vec;
make_vector(x) -> {1.0,0.0,0.0};
make_vector(y) -> {0.0,1.0,0.0};
make_vector(z) -> {0.0,0.0,1.0};
make_vector(free) -> free;
make_vector(normal) -> normal;
make_vector(intrude) -> normal;
make_vector(Axis) when Axis == last_axis; Axis == default_axis ->
    {_,Vec} = wings_pref:get_value(Axis),
    Vec.

validate_mirror(#we{mirror=none}=We) -> We;
validate_mirror(#we{fs=Ftab,mirror=Face}=We) ->
    case gb_trees:is_defined(Face, Ftab) of
	false -> We#we{mirror=none};
	true -> We
    end.

button_message(LmbMsg) ->
    wings_wm:message(wings_camera:button_format(LmbMsg)).

button_message(LmbMsg, MmbMsg) ->
    wings_wm:message(wings_camera:button_format(LmbMsg, MmbMsg)).
    
button_message(LmbMsg, MmbMsg, RmbMessage) ->
    wings_wm:message(wings_camera:button_format(LmbMsg, MmbMsg, RmbMessage)).

button_format(LmbMsg) ->
    wings_camera:button_format(LmbMsg).

button_format(LmbMsg, MmbMsg) ->
    wings_camera:button_format(LmbMsg, MmbMsg).

button_format(LmbMsg, MmbMsg, RmbMsg) ->
    wings_camera:button_format(LmbMsg, MmbMsg, RmbMsg).

rmb_format(Msg) ->
    wings_camera:rmb_format(Msg).

join_msg(Messages) ->
    wings_camera:join_msg(Messages).

join_msg(Msg1, Msg2) ->
    wings_camera:join_msg(Msg1, Msg2).

message(Message) ->
    Qs = {vframe,
	  [{label,Message},
	   {hframe,[{button,ok,[ok]}]}]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

magnet_string() ->
    ["(Magnet route:",
     atom_to_list(wings_pref:get_value(magnet_distance_route))
     ,")"].

get_matrices(Id, MM) ->
    wings_view:load_matrices(false),
    case MM of
	mirror ->
	    Matrix = mirror_matrix(Id),
	    gl:multMatrixf(Matrix);
	original -> ok
    end,
    {_,_,W,H} =  wings_wm:viewport(),
    ModelMatrix = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    ProjMatrix = gl:getDoublev(?GL_PROJECTION_MATRIX),
    {ModelMatrix,ProjMatrix,{0,0,W,H}}.

mirror_matrix(Id) ->
    wings_draw_util:fold(fun mirror_matrix/2, Id).

mirror_matrix(#dlo{mirror=Matrix,src_we=#we{id=Id}}, Id) -> Matrix;
mirror_matrix(_, Acc) -> Acc.

mirror_flatten(_, #we{mirror=none}=We) -> We;
mirror_flatten(#we{mirror=OldFace}=OldWe, #we{mirror=Face,vp=Vtab0}=We) ->
    PlaneNormal = wings_face:normal(OldFace, OldWe),
    FaceVs = wings_face:to_vertices(gb_sets:singleton(OldFace), OldWe),
    Origin = wings_vertex:center(FaceVs, OldWe),
    M0 = e3d_mat:translate(Origin),
    M = e3d_mat:mul(M0, e3d_mat:project_to_plane(PlaneNormal)),
    Flatten = e3d_mat:mul(M, e3d_mat:translate(e3d_vec:neg(Origin))),
    Vtab = foldl(fun(V, Vt) ->
			 Pos0 = gb_trees:get(V, Vt),
			 Pos = e3d_mat:mul_point(Flatten, Pos0),
			 gb_trees:update(V, Pos, Vt)
		 end, Vtab0, wings_face:vertices_ccw(Face, We)),
    We#we{vp=Vtab}.

rel2fam(Rel) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(Rel))).

yes_no(Question, Yes) ->
    yes_no(Question, Yes, ignore).

yes_no(Question, Yes, No) ->
    Qs = {vframe,
	  [{label,Question,[{break,45}]},
	   {hframe,[{button,"Yes",yes_no_fun(Yes)},
		    {button,"No",yes_no_fun(No),[cancel]}]}]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

yes_no_cancel(Question, Yes, No) ->
    Qs = {vframe,
	  [{label,Question,[{break,45}]},
	   {hframe,[{button,"Yes",yes_no_fun(Yes)},
		    {button,"No",yes_no_fun(No)},
		    {button,"Cancel",yes_no_fun(ignore),[cancel]}]}]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

yes_no_fun(ignore) -> fun(_) -> ignore end;
yes_no_fun(Fun) ->
    This = wings_wm:this(),
    fun(_) ->
	    case Fun() of
		ignore -> ignore;
		Action -> wings_wm:send(This, {action,Action})
	    end
    end.

stringify({Atom,Other}) when is_atom(Atom) ->
    cap(atom_to_list(Atom)) ++
	case stringify(Other) of
	    [] -> [];
	    Str -> "|" ++ Str
	end;
stringify(Atom) when is_atom(Atom) ->
    cap(atom_to_list(Atom));
stringify(Int) when integer(Int) ->
    integer_to_list(Int);
stringify(_Other) -> [].

cap(Str) when is_atom(Str) -> cap(atom_to_list(Str));
cap(Str) -> cap(Str, true).

cap([Lower|T], true) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|cap(T, false)];
cap([$_|T], _Any) ->
    [$\s|cap(T, true)];
cap([H|T], _Any) ->
    [H|cap(T, false)];
cap([], _Flag) -> [].
    
upper(Str) when is_atom(Str) -> upper(atom_to_list(Str));
upper([Lower|T]) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|upper(T)];
upper([H|T]) ->
    [H|upper(T)];
upper([]) -> [].

add_vpos(Vs, #we{vp=Vtab}) -> add_vpos(Vs, Vtab);
add_vpos(Vs, Vtab) ->
    foldl(fun(V, A) ->
		  [{V,gb_trees:get(V, Vtab)}|A]
	  end, [], Vs).

update_vpos(Vs, #we{vp=Vtab}) -> update_vpos(Vs, Vtab);
update_vpos(Vs, Vtab) ->
    foldl(fun({V,_}, A) ->
		  [{V,gb_trees:get(V, Vtab)}|A];
	     ({V,_,Dist,Inf}, A) ->
		  [{V,gb_trees:get(V, Vtab),Dist,Inf}|A]
	  end, [], reverse(Vs)).

gb_trees_smallest_key({_, Tree}) ->
    smallest_key1(Tree).

smallest_key1({Key, _Value, nil, _Larger}) ->
    Key;
smallest_key1({_Key, _Value, Smaller, _Larger}) ->
    smallest_key1(Smaller).

gb_trees_largest_key({_, Tree}) ->
    largest_key1(Tree).

largest_key1({Key, _Value, _Smaller, nil}) ->
    Key;
largest_key1({_Key, _Value, _Smaller, Larger}) ->
    largest_key1(Larger).

nice_float(F) when is_float(F) ->
    simplify_float(lists:flatten(io_lib:format("~f", [F]))).

simplify_float(F) ->
    reverse(simplify_float_1(reverse(F))).

simplify_float_1("0."++_=F) -> F;
simplify_float_1("0"++F) -> simplify_float_1(F);
simplify_float_1(F) -> F.

menu_restriction(Win, Allowed) ->
    case wings_wm:get_menubar(Win) of
	none -> wings_wm:menubar(Win, []);
	Mb0 ->
	    Mb = [Item || {_,Name,_}=Item <- Mb0, member(Name, Allowed)],
	    wings_wm:menubar(Win, Mb)
    end.

geom_windows() ->
    geom_windows_1(wings_wm:windows()).

geom_windows_1([geom|T]) ->
    [geom|geom_windows_1(T)];
geom_windows_1([{geom,_}=Name|T]) ->
    [Name|geom_windows_1(T)];
geom_windows_1([_|T]) ->
    geom_windows_1(T);
geom_windows_1([]) -> [].

%%
%% Create a unique name by appending digits.
%%

unique_name(Name, Names) ->
    case member(Name, Names) of
	false -> Name;
	true -> unique_name_1(reverse(Name), Names)
    end.

unique_name_1([C|Cs], Names) when $0 =< C, C =< $9 ->
    unique_name_1(Cs, Names);
unique_name_1(Name, Names0) ->
    Base0 = [First|_] = reverse(Name),
    Names = [N || N <- Names0, hd(N) =:= First],
    Base = case member($\s, Base0) andalso last(Base0) =/= $\s of
	       true -> Base0 ++ " ";
	       false -> Base0
	   end,
    unique_name_2(Base, 2, gb_sets:from_list(Names)).

unique_name_2(Base, I, Names) ->
    Name = Base ++ integer_to_list(I),
    case gb_sets:is_member(Name, Names) of
	true -> unique_name_2(Base, I+1, Names);
	false -> Name
    end.

%%%
%%% OpenGL extensions.
%%%
init_gl_extensions() ->
    ets:new(wings_gl_ext, [named_table,public,ordered_set]),
    Exts0 = lists:sort(string:tokens(gl:getString(?GL_EXTENSIONS), " ")),
    Exts = [{list_to_atom(E)} || E <- Exts0],
    ets:insert(wings_gl_ext, Exts),
    Ver = case catch get_gl_version() of
	      {_,_,_}=V -> V;
	      _ -> {1,1,0}
	  end,
    ets:insert(wings_gl_ext, {version,Ver}).

get_gl_version() ->
    case string:tokens(gl:getString(?GL_VERSION), ". ") of
	[Major0,Minor0] ->
	    Patch = 0;
	[Major0,Minor0,Patch0|_] ->
	    case catch list_to_integer(Patch0) of
		{'EXIT',_} -> Patch = 0;
		Patch -> Patch
	    end
    end,
    Major = list_to_integer(Major0),
    Minor = list_to_integer(Minor0),
    {Major,Minor,Patch}.

%% Either check for a given version (or higher), or
%% for that all the given extensions are implemented.
is_gl_ext(Wanted) when is_tuple(Wanted), size(Wanted) >= 2 ->
    [{_,Actual}] = ets:lookup(wings_gl_ext, version),
    version_match(Wanted, Actual);
is_gl_ext(Ext) when is_atom(Ext); is_list(Ext) ->
    is_gl_ext_1(Ext).

%% Must be Wanted version or higher, or the List of extensions must match.
is_gl_ext(Wanted, []) ->
    is_gl_ext(Wanted);
is_gl_ext(Wanted, List) ->
    is_gl_ext(Wanted) orelse is_gl_ext_1(List).

is_gl_ext_1([]) ->
    true;
is_gl_ext_1([Name|R]) ->
    is_gl_ext_1(Name) andalso is_gl_ext(R);
is_gl_ext_1(Name) ->
    ets:member(wings_gl_ext, Name).

version_match({Ma1,Mi1}, {Ma2,Mi2,_}) 
  when Ma1 =< Ma2, Mi1 =< Mi2 -> true;
version_match({Ma1,Mi1,P1}, {Ma2,Mi2,P2}) 
  when Ma1 =< Ma2, Mi1 =< Mi2, P1 =< P2 -> true;
version_match(_,_) ->
    false.

%%%
%%% OpenGL restrictions (bugs and limitations).
%%%
init_gl_restrictions() ->
    ets:new(wings_gl_restriction, [named_table,public,ordered_set]),
    case os:type() of
	{unix,sunos} ->
	    %% Scissor does not work for clipping text.
	    ets:insert(wings_gl_restriction, [{broken_scissor}]);
	_ ->
	    ok
    end.

is_gl_restriction(Name) ->
    ets:member(wings_gl_restriction, Name).
    
%%
%% Timing.
%% 

tc(Fun,Mod,Line) ->
    case timer:tc(erlang, apply, [Fun,[]]) of
	{_,{'EXIT',Reason}} -> exit(Reason);
	{T,R} ->
	    io:format("~p:~p: Time: ~p\n", [Mod, Line, T]),
	    R
    end.

%%
%% Dumping of data structures.
%% 

show_edge(F, Edge, #edge{vs=Vs,ve=Ve,a=A,b=B,lf=Lf,rf=Rf,ltpr=Lpred,ltsu=Lsucc,
			 rtpr=Rpred,rtsu=Rsucc}) ->
    io:format(F, "~p: vs=~p ve=~p\n", [Edge,Vs,Ve]),
    io:format(F, "  a=~p b=~p\n", [A,B]),
    io:format(F, "  left: face=~p pred=~p succ=~p\n", [Lf,Lpred,Lsucc]),
    io:format(F, "  right: face=~p pred=~p succ=~p\n", [Rf,Rpred,Rsucc]).

show_face(F, Face, Edge) ->
    io:format(F, "~p: edge=~p\n", [Face,Edge]).

%%%
%%% Dump the winged-edge structure in a textual format.
%%%

export_we(Name, #st{shapes=Shs}) ->
    case file:open(Name, [write,delayed_write]) of
	{ok,F} ->
	    foreach(fun(We) -> dump_we(F, We) end, gb_trees:values(Shs)),
	    file:close(F);
	{error,_}=Error ->
	    Error
    end.

%%%
%%% Crash log writing.
%%%

crash_log(WinName, BackTrace) ->
    LogFileDir = log_file_dir(),
    LogName = filename:absname("wings_crash.dump", LogFileDir),
    F = open_log_file(LogName),
    io:format(F, "Window: ~p\n", [WinName]),
    io:format(F, "Crashed in:\n~p\n\n", [BackTrace]),
    analyse(F, BackTrace),
    file:close(F),
    LogName.

log_file_dir() ->
    case catch log_file_dir(os:type()) of
	Dir when is_list(Dir) -> Dir;
	_Other -> "."
    end.

log_file_dir({unix,_}) -> os:getenv("HOME");
log_file_dir({win32,_}) ->
    Root = code:root_dir(),
    case filelib:is_file("Wings3D.exe") of
        true -> Root;
        false ->
            %% Development system.
            case code:which(?MODULE) of
                Name0 when is_list(Name0) ->
                    Name = filename:dirname(Name0),
                    case filename:basename(Name) of
                        "ebin" -> filename:dirname(Name);
                        "patches" -> filename:dirname(Name);
                        _Other -> Name
                    end
            end
    end.
	
open_log_file(Name) ->
    {ok,F} = file:open(Name, [write]),
    {{Y,Mo,D},{H,Mi,_}} = erlang:localtime(),
    io:format(F, "Dump written ~p-~p-~p_~p-~p\n", [Y,Mo,D,H,Mi]),
    F.

analyse(F, {_,[{_Mod,_Fun,Args}|_]}) when is_list(Args) ->
    try_args(F, Args, 1);
analyse(_, _) -> ok.

try_args(F, [A|As], Num) ->
    try_arg(F, A, Num),
    try_args(F, As, Num+1);
try_args(_, _, _) -> ok.

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
	[{_,#edge{}}|_]=Es ->
	    arg(F, N),
	    dump_edges(F, Es);
	_ -> ok
    end;
try_arg(_, _, _) -> ok.

arg(F, N) ->
    io:format(F, "Argument #~p:\n", [N]).

dump_shape(F, #we{}=We) ->
    dump_we(F, We).

dump_we(F, #we{name=Name,id=Id,mode=Mode,es=Etab,fs=Ftab,
	       next_id=Next}) ->
    io:put_chars(F, "\n"),
    io:format(F, "OBJECT ~p: ~p\n", [Id,Name]),
    io:format(F, "=======================\n", []),
    io:format(F, "   mode=~p next_id=~p\n", [Mode,Next]),
    dump_faces(F, gb_trees:to_list(Ftab)),
    dump_edges(F, gb_trees:to_list(Etab)).
    
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

validate(Mod, Line, X) ->
    put(where, {Mod,Line}),
    validate_1(X),
    erase(where),
    X.

validate(X) ->
    validate_1(X),
    X.

validate_1(#st{shapes=Shapes}) ->
    foreach(fun ({_,#we{}=We}) ->
		    validate_we(We);
		({_,_}) -> ok end,
	    gb_trees:to_list(Shapes));
validate_1(#we{}=We) -> validate_we(We).

validate_we(We) ->
    validate_edge_tab(We),
    validate_vertex_tab(We),
    validate_faces(We),
    wings_material:validate(We).
    
validate_faces(#we{fs=Ftab}=We) ->
    foreach(fun({Face,Edge}) ->
		    Cw = walk_face_cw(Face, Edge, Edge, We, []),
		    Ccw = walk_face_ccw(Face, Edge, Edge, We, []),
 		    case reverse(Ccw) of
 			Cw -> ok;
 			Other ->
 			    crash({{face,Face},
				   {cw,Cw},{ccw_reversed,Other}},
				  We)
 		    end,
		    case {lists:sort(Cw),ordsets:from_list(Cw)} of
			{Same,Same} -> ok;
			_ ->
			    crash({{face,Face},{repeated_vertex,Cw}}, We)
		    end
	    end,
	    gb_trees:to_list(Ftab)).

walk_face_cw(_Face, LastEdge, LastEdge, _We, [_|_]=Acc) -> Acc;
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

walk_face_ccw(_Face, LastEdge, LastEdge, _We, [_|_]=Acc) -> Acc;
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

validate_vertex_tab(#we{es=Etab,vc=Vct,vp=Vtab}=We) ->
    case {gb_trees:keys(Vct),gb_trees:keys(Vtab)} of
	{Same,Same} -> ok;
	{_,_} ->
	    crash(vc_and_vp_have_different_keys, We)
    end,
    foreach(fun({V,Edge}) ->
		    case gb_trees:get(Edge, Etab) of
			#edge{vs=V}=Rec ->
			    validate_edge_rec(Rec, We);
			#edge{ve=V}=Rec ->
			    validate_edge_rec(Rec, We);
			_Other ->
			    crash({vertex,V}, We)
		    end
	    end,
	    gb_trees:to_list(Vct)).

validate_edge_tab(#we{es=Etab}=We) ->
    foreach(fun({E,#edge{vs=Va,ve=Vb}}) ->
		    verify_vertex(Va, E, We),
		    verify_vertex(Vb, E, We)
	    end,
	    gb_trees:to_list(Etab)).

validate_edge_rec(Rec, We) ->
    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
    if
	integer(LP+LS+RP+RS) -> ok;
	true -> crash({non_integer_edges,Rec}, We)
    end.

verify_vertex(V, Edge, #we{vc=Vct}=We) ->
    case gb_trees:is_defined(V, Vct) of
	false ->
	    crash({edge,Edge,referenced,undefined,vertex,V}, We);
	true -> ok
    end.

-ifdef(DEBUG).
check_error(Mod, Line) ->
    case gl_error_string(gl:getError()) of
	no_error ->
	    ok;
	Str ->
	    io:format("~p, line ~p: ~s\n", [Mod,Line,Str]),
	    erlang:fault(gl_error, [Mod,Line])
    end.
-else.
check_error(_Mod, _Line) ->
    ok.
-endif.

gl_error_string(0) -> no_error;
gl_error_string(?GL_INVALID_VALUE) -> "GL_INVALID_VALUE";
gl_error_string(?GL_INVALID_ENUM) -> "GL_INVALID_ENUM";
gl_error_string(?GL_INVALID_OPERATION) -> "GL_INVALID_OPERATION";
gl_error_string(?GL_STACK_OVERFLOW) -> "GL_STACK_OVERFLOW";
gl_error_string(?GL_STACK_UNDERFLOW) -> "GL_STACK_UNDERFLOW";
gl_error_string(?GL_OUT_OF_MEMORY) -> "GL_OUT_OF_MEMORY";
gl_error_string(Error) -> "Error: "++integer_to_list(Error).

crash(Reason, We) ->
    erlang:fault({crash,get(where),Reason}, [We]).

max(A, B) when A > B -> A;
max(_A, B) -> B.

min(A, B) when A < B -> A;
min(_A, B) -> B.
