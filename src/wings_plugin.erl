%%
%%  wings_plugin.erl --
%%
%%     Exeperimental support of plugins.
%%
%%  Copyright (c) 2001 Jakob Cederlund, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_plugin.erl,v 1.1 2001/10/17 07:48:25 bjorng Exp $
%%
-module(wings_plugin).
-export([init/0,menu/2,command/2]).

-include("wings.hrl").
-import(lists, [append/1,flatmap/2,foreach/2]).

init() ->
    PluginDir = filename:absname("plugins"),
    Dict = case filelib:is_dir(PluginDir) of
	       true -> init(PluginDir);
	       false -> []
	   end,
    put(wings_plugins, gb_trees:from_orddict(Dict)).

init(Dir) ->
    Beams = filelib:wildcard(Dir ++ "/*.beam"),
    Mods = [to_module(Beam) || Beam <- Beams],
    code:add_patha(Dir),
    foreach(fun(Mod) -> c:l(Mod) end, Mods),
    Menus0 = [rearrange(N, T, M, C) || M <- Mods, {N,{T,C}} <- M:menus()],
    Menus1 = sofs:relation(Menus0),
    Menus = sofs:relation_to_family(Menus1),
		 sofs:to_external(Menus).

rearrange(N, T, M, {C}) -> {N,{T,{[M,C]}}};
rearrange(N, T, M, C) -> {N,{T,[M,C]}}.

to_module(Beam) ->
    list_to_atom(filename:rootname(filename:basename(Beam))).

menu(Name, Menu) ->
    Dict = get(wings_plugins),
    case gb_trees:lookup(Name, Dict) of
	none ->
	    Menu;
	{value,PluginMenu} ->
	    list_to_tuple(tuple_to_list(Menu) ++ [separator|PluginMenu])
    end.       

command({[Mod,Cmd]}, St) -> command(Mod, Cmd, true, St);
command([Mod,Cmd], St) -> command(Mod, Cmd, false, St).
    
command(Mod, Cmd, Ask, #st{onext=Oid}=St0) ->
    case Mod:command(Cmd, Ask, St0) of
	{new_shape,Prefix,Fs,Vs} ->
	    We = wings_we:build(Fs, Vs),
	    Name = Prefix++integer_to_list(Oid),
	    wings_shape:new(Name, We, St0);
	aborted -> aborted;
	#st{}=St -> St
    end.
