%%
%%  wings_plugin.erl --
%%
%%     Experimental support of plugins.
%%
%%  Copyright (c) 2001-2002 Jakob Cederlund, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_plugin.erl,v 1.10 2002/01/26 11:21:05 bjorng Exp $
%%
-module(wings_plugin).
-export([init/0,menu/2,command/2,call_ui/1]).

-include("wings.hrl").
-include("e3d.hrl").
-import(lists, [append/1,flatmap/2,foreach/2,sort/1,reverse/1,foldl/3]).

%%%
%%% Currently, there can be a single directory for plugins, but
%%% sub-directories to any level will be searched.
%%% The plugin directory must be named 'plugins'. It must be located
%%% either in the same directory as the beam files, or in a directory
%%% parallell to the 'ebin' directory if the beam files are kept in
%%% a 'ebin' directory.
%%%
%%% To avoid name space clashing, plugins must be named according to
%%% the following convention:
%%%    wpT_*.beam
%%% where the T should be replaced with the type of the plugin.
%%%
%%% The types are defined as following:
%%%
%%% 0   Object creation plugin.
%%% 1   Import/export plugin (NYI).
%%% 8   External interface plugin.
%%% 9   Default interface plugin.
%%% u   General plugin. Not recommended for other than experimental
%%%     use.
%%%

init() ->
    put(wings_plugins, []),
    put(wings_ui, def_ui_plugin()),
    case try_dirs() of
	none -> ok;
	PluginDir -> init(PluginDir)
    end.

call_ui(What) ->
    Ui = get(wings_ui),
    Ui(What).

menu(Name, Menu) ->
    menu_1(get(wings_plugins), Name, Menu).

menu_1([{Type,M}|Ps], Name, Menu0) ->
    case catch M:menu(Name, Menu0) of
	Menu when is_list(Menu) ->
	    menu_1(Ps, Name, Menu);
	Other ->
	    io:format("~w:menu/2: bad return value: ~P\n", [M,Other,20]),
	    menu_1(Ps, Name, Menu0)
    end;
menu_1([], Name, Menu) -> Menu.

command(Cmd, St) ->
    command(get(wings_plugins), Cmd, St).

command([{Type,M}|Ps], Cmd, St) ->
    case catch M:command(Cmd, false, St) of
	next -> command(Ps, Cmd, St);
	Other ->
	    case check_result(M, Other, St) of
		next -> command(Ps, Cmd, St);
		Res -> Res
	    end
    end;
command([], Cmd, St) -> next.

%%%
%%% Local functions.
%%%

init(Dir) ->
    {Pas,Beams} = list_dir(Dir),
    foreach(fun(Pa) -> code:add_patha(Pa) end, Pas),
    TypeMods0 = [to_module(Beam) || Beam <- Beams],
    TypeMods = reverse(sort(load_modules(TypeMods0))),
    put(wings_plugins, init_plugins(TypeMods)).

init_plugins([{user_interface,M}|T]) ->
    Ui0 = get(wings_ui),
    case catch M:init(Ui0) of
	Ui when is_function(Ui) ->
	    put(wings_ui, Ui);
	Other ->
	    io:format("~w:init/1 bad return value: ~P\n", [M,Other,20])
    end,
    init_plugins(T);
init_plugins([{Type,M}=TypeMod|T]) ->
    case catch M:init() of
	true -> [TypeMod|init_plugins(T)];
	false -> init_plugins(T);
	Other ->
	    io:format("~w:init/0 bad return value: ~P\n", [M,Other,20]),
	    init_plugins(T)
    end;
init_plugins([]) -> [].
    
load_modules(TypeMods) ->
    foldl(fun({_,Mod}=TypeMod, A) ->
		  case c:l(Mod) of
		      {module,Mod} -> [TypeMod|A];
		      Error -> A
		  end
	  end, [], TypeMods).

def_ui_plugin() ->
    fun(Missing) ->
	    Msg = io_lib:format("Reinstall Wings. "
				"Missing plugin for ~p.",
				[Missing]),
	    wings_io:message(lists:flatten(Msg)),
	    aborted
    end.

try_dirs() ->
    Dir0 = wings:root_dir(),
    Dir = filename:absname("plugins", Dir0),
    case filelib:is_dir(Dir) of
	true -> Dir;
	false -> none
    end.

list_dir(Dir) ->
    list_dir([Dir], [], []).

list_dir([Dir|Dirs0], Pas, Beams0) ->
    case file:list_dir(Dir) of
	{ok,List} ->
	    case list_dir_1(List, Dir, Dirs0, Beams0) of
		{Dirs,Beams0} -> list_dir(Dirs, Pas, Beams0);
		{Dirs,Beams} -> list_dir(Dirs, [Dir|Pas], Beams)
	    end;
	{error,_} -> list_dir(Dirs0, Pas, Beams0)
    end;
list_dir([], Pas, Beams) -> {Pas,Beams}.

list_dir_1([[$~|_]|Ns], Dir0, Dirs, Beams) ->
    list_dir_1(Ns, Dir0, Dirs, Beams);
list_dir_1([[$w,$p,T,$_|_]=N|Ns], Dir0, Dirs, Beams) ->
    case filename:extension(N) of
	".beam" -> list_dir_1(Ns, Dir0, Dirs, [N|Beams]);
	_ -> list_dir_1(Ns, Dir0, Dirs, Beams)
    end;
list_dir_1([N|Ns], Dir0, Dirs, Beams) ->
    Dir = filename:join(Dir0, N),
    case filelib:is_dir(Dir) of
	true -> list_dir_1(Ns, Dir0, [Dir|Dirs], Beams);
	false -> list_dir_1(Ns, Dir0, Dirs, Beams)
    end;
list_dir_1([], Dir, Dirs, Beams) -> {Dirs,Beams}.
    
to_module([_,_,T|_]=Beam) ->
    {convert_type(T),list_to_atom(filename:rootname(Beam))}.

convert_type($0) -> creator;
convert_type($1) -> import_export;
convert_type($8) -> user_interface;
convert_type($9) -> user_interface;
convert_type($u) -> unsupported.

check_result(M, {new_shape,Prefix,#e3d_object{}=Obj,Mat}, St0) ->
    {We,UsedMat} = import_object(Obj),
    St = add_materials(UsedMat, Mat, St0),
    new_shape(Prefix, We, St);
check_result(M, {new_shape,Prefix,Fs,Vs}, St) ->
    We = wings_we:build(Fs, Vs),
    new_shape(Prefix, We, St);
check_result(M, aborted, St) -> aborted;
check_result(M, {drag,_}=Drag, _) -> Drag;
check_result(M, #st{}=St, _) -> St;
check_result(M, Other, St) ->
    io:format("~w:command/3: bad return value: ~P\n", [M,Other,20]),
    next.

new_shape(Prefix, We, #st{onext=Oid}=St) ->
    Name = Prefix++integer_to_list(Oid),
    wings_shape:new(Name, We, St).

import_object(Obj) ->
    import_object(Obj, gb_sets:empty()).

import_object(#e3d_object{name=Name,obj=Obj}, UsedMat0) ->
    #e3d_mesh{matrix=Matrix,vs=Vs0,tx=Tx0,fs=Fs0,he=He} = Obj,
    Vs = scale(Vs0, Matrix),
    ObjType = obj_type(Tx0),
    Tx = list_to_tuple(Tx0),
    {Fs,UsedMat} = import_faces(Fs0, Tx, [], UsedMat0),
    We = wings_we:build(ObjType, Fs, Vs, He),
    {We,UsedMat}.

import_faces([#e3d_face{vs=Vs,tx=Tx0,mat=Mat0}|Fs], Txs, Acc, UsedMat0) ->
    UsedMat = add_used_mat(Mat0, UsedMat0),
    Mat = translate_mat(Mat0),
    FaceData = case Tx0 of
		   [] -> {Mat,Vs};
		   Tx1 ->
		       Tx = [element(Tx+1, Txs) || Tx <- Tx1],
		       {Mat,Vs,Tx}
	       end,
    import_faces(Fs, Txs, [FaceData|Acc], UsedMat);
import_faces([], Txs, Acc, UsedMat) -> {reverse(Acc),UsedMat}.

scale(Vs, none) -> Vs;
scale(Vs, Matrix) -> [e3d_mat:mul_point(Matrix, P) || P <- Vs].

obj_type([]) -> material;
obj_type([{_,_}|_]) -> uv;
obj_type([{_,_,_}|_]) -> vertex.

add_used_mat([], UsedMat) -> UsedMat;
add_used_mat([M|Ms], UsedMat) -> add_used_mat(Ms, gb_sets:add(M, UsedMat)).
    
translate_mat([]) -> default;
translate_mat([Mat]) -> Mat;
translate_mat([_|_]=List) -> List.

add_materials(UsedMat0, Mat0, St) ->
    UsedMat = sofs:from_external(gb_sets:to_list(UsedMat0), [name]),
    Mat1 = sofs:relation(Mat0, [{name,data}]),
    Mat2 = sofs:restriction(Mat1, UsedMat),
    NotDefined0 = sofs:difference(UsedMat, sofs:domain(Mat2)),
    DummyData = sofs:from_term([], data),
    NotDefined = sofs:constant_function(NotDefined0, DummyData),
    Mat = sofs:to_external(sofs:union(Mat2, NotDefined)),
    wings_material:add_materials(Mat, St).
