%%
%%  wings_start.erl --
%%
%%     Starter of Wings 3D; might enable installed patches.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_start.erl,v 1.3 2002/10/02 15:10:35 bjorng Exp $
%%

-module(wings_start).
-export([start/0,start/1,start_halt/0,start_halt/1]).
-export([get_patches/0,enable_patches/0,disable_patches/0]).

start() ->
    common_start(fun(Root) -> wings:start(Root) end).

start(Args) ->
    common_start(fun(Root) -> wings:start_halt(Args, Root) end).

start_halt() ->
    common_start(fun(Root) -> wings:start_halt(Root) end).

start_halt(Args) ->
    common_start(fun(Root) -> wings:start_halt(Args, Root) end).

common_start(Start) ->
    case get_patches() of
	none -> ok;
	{disabled,_} -> ok;
	{enabled,_} -> code:add_patha(patch_dir())
    end,
    Root0 = filename:dirname(code:which(?MODULE)),
    Root = case filename:basename(Root0) of
	       "ebin" -> filename:dirname(Root0);
	       _Other -> Root0
	   end,
    Start(Root).

get_patches() ->
    Patches = patch_dir(),
    case filelib:wildcard(filename:join(Patches, "*.beam")) of
	[] -> none;
	_ ->
	    case filelib:is_file(filename:join(Patches, "PATCHES_ENABLED")) of
		true -> {enabled,patch_name(Patches)};
		false -> {disabled,patch_name(Patches)}
	    end
    end.

patch_name(Dir) ->
    case file:read_file(filename:join(Dir, "DESCRIPTION")) of
	{ok,<<Str:20,_/binary>>} -> binary_to_list(Str);
	{ok,Bin} -> binary_to_list(Bin);
	_Other -> "Installed Patches"
    end.
			
enable_patches() ->
    Name = filename:join(patch_dir(), "PATCHES_ENABLED"),
    file:write_file(Name, "").

disable_patches() ->    
    Name = filename:join(patch_dir(), "PATCHES_ENABLED"),
    file:delete(Name).

patch_dir() ->
    Base0 = filename:dirname(code:which(?MODULE)),
    Base = case filename:basename(Base0) of
	       "ebin" -> filename:dirname(Base0);
	       _Other -> Base0
	   end,
    filename:join(Base, "patches").
