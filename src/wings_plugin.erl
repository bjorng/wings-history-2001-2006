%%
%%  wings_plugin.erl --
%%
%%     Experimental support of plugins.
%%
%%  Copyright (c) 2001 Jakob Cederlund, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_plugin.erl,v 1.8 2002/01/17 13:20:40 bjorng Exp $
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
    Dict = case try_dirs() of
	       none -> [];
	       PluginDir -> init(PluginDir)
	   end,
    put(wings_plugins, gb_trees:from_orddict(Dict)).

init(Dir) ->
    {Pas,Beams} = list_dir(Dir),
    foreach(fun(Pa) -> code:add_patha(Pa) end, Pas),
    TypeMods0 = [to_module(Beam) || Beam <- Beams],
    TypeMods = load_modules(TypeMods0),
    UiMods = reverse(sort([Mod || {user_interface,Mod} <- TypeMods])),
    init_ui_plugins(UiMods),
    Menus0 = [rearrange(N, T, Type, M, C) ||
		 {Type,M} <- TypeMods, {N,{T,C}} <- catch M:menus()],
    Menus1 = sofs:relation(Menus0),
    Menus = sofs:relation_to_family(Menus1),
    sofs:to_external(Menus).

load_modules(TypeMods) ->
    foldl(fun({_,Mod}=TypeMod, A) ->
		  case c:l(Mod) of
		      {module,Mod} -> [TypeMod|A];
		      Error -> A
		  end
	  end, [], TypeMods).

init_ui_plugins(Ms) ->
    Def = fun(Missing) ->
		  Msg = io_lib:format("Reinstall Wings. "
				      "Missing plugin for ~p.",
				      [Missing]),
		  wings_io:message(lists:flatten(Msg)),
		  aborted
	  end,
    init_ui_plugins(Ms, Def).

init_ui_plugins([M|Ms], Ui0) ->
    case catch M:init(Ui0) of
	Ui when is_function(Ui) ->
	    init_ui_plugins(Ms, Ui);
	Other ->
	    io:format("~w:init/1 bad return value: ~p\n", [M,Other])
    end;
init_ui_plugins([], Ui) -> put(wings_ui, Ui).

call_ui(What) ->
    Ui = get(wings_ui),
    Ui(What).

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
    
rearrange(N, T, Type, M, {C}) -> {N,{T,{[Type,M,C]}}};
rearrange(N, T, Type, M, C) -> {N,{T,[Type,M,C]}}.

to_module([_,_,T|_]=Beam) ->
    {convert_type(T),list_to_atom(filename:rootname(Beam))}.

convert_type($0) -> creator;
convert_type($1) -> import_export;
convert_type($8) -> user_interface;
convert_type($9) -> user_interface;
convert_type($u) -> unsupported.

menu(Name, Menu) ->
    Dict = get(wings_plugins),
    case gb_trees:lookup(Name, Dict) of
	none ->
	    Menu;
	{value,PluginMenu} ->
	    list_to_tuple(tuple_to_list(Menu) ++ [separator|PluginMenu])
    end.       

command({[Type,Mod,Cmd]}, St) -> command(Type, Mod, Cmd, true, St);
command([Type,Mod,Cmd], St) -> command(Type, Mod, Cmd, false, St).
    
command(creator, Mod, Cmd, Ask, St) ->
    check_result(Mod:command(Cmd, Ask), St);
command(unsupported, Mod, Cmd, Ask, St) ->
    check_result(Mod:command(Cmd, Ask, St), St).

check_result({new_shape,Prefix,#e3d_object{}=Obj,Mat}, St0) ->
    {UsedMat,St} = import_object(Obj, St0),
    add_materials(UsedMat, Mat, St);
check_result({new_shape,Prefix,Fs,Vs}, #st{onext=Oid}=St) ->
    We = wings_we:build(Fs, Vs),
    Name = Prefix++integer_to_list(Oid),
    wings_shape:new(Name, We, St);
check_result(aborted, St) -> aborted;
check_result({drag,_}=Drag, _) -> Drag;
check_result(#st{}=St, _) -> St.

import_object(Obj, St) ->
    import_object(Obj, gb_sets:empty(), St).

import_object(#e3d_object{name=Name,obj=Obj}, UsedMat0, St0) ->
    #e3d_mesh{matrix=Matrix,vs=Vs0,tx=Tx0,fs=Fs0,he=He} = Obj,
    io:format("Name ~p\n", [Name]),
    Vs = scale(Vs0, Matrix),
    ObjType = obj_type(Tx0),
    io:format("~w\n", [ObjType]),
    Tx = list_to_tuple(Tx0),
    {Fs,UsedMat} = import_faces(Fs0, Tx, [], UsedMat0),
    St = build_object(Name, ObjType, Fs, Vs, He, St0),
    {UsedMat,St}.

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

build_object(Name, Type, Fs, Vs, He, St) ->
    case catch wings_we:build(Type, Fs, Vs, He) of
	{'EXIT',Reason} ->
	    io:format("Conversion failed: ~P\n", [Reason,20]),
	    St;
	We -> store_object(Name, We, St)
    end.

store_object(undefined, We, #st{onext=Oid}=St) ->
    Name = "unnamed_object" ++ integer_to_list(Oid),
    wings_shape:new(Name, We, St);
store_object(Name, We, St) ->
    wings_shape:new(Name, We, St).

add_materials(UsedMat0, Mat0, St) ->
    UsedMat = sofs:from_external(gb_sets:to_list(UsedMat0), [name]),
    Mat1 = sofs:relation(Mat0, [{name,data}]),
    Mat2 = sofs:restriction(Mat1, UsedMat),
    NotDefined0 = sofs:difference(UsedMat, sofs:domain(Mat2)),
    DummyData = sofs:from_term([], data),
    NotDefined = sofs:constant_function(NotDefined0, DummyData),
    Mat = sofs:to_external(sofs:union(Mat2, NotDefined)),
    wings_material:add_materials(Mat, St).
