%%
%%  collect_bmp.erl --
%%
%%     This module collects BMP files containing and write them into
%%     a single file that will be loaded by Wings 3D.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: collect_bmp.erl,v 1.1.1.1 2001/08/14 18:16:34 bjorng Exp $
%%

-module(collect_bmp).
-export([start/0,start/1]).
-import(lists, [reverse/1]).

start() ->
    start(["icons","wings_icon.bundle"]).

start([InDir,OutFile]=Arg) ->
    Icons = load_icons(filelib:wildcard(filename:join(InDir, "*.bmp"))),
    Bin = term_to_binary(Icons, [compressed]),
    io:format("Writing ~s\n", [OutFile]),
    ok = file:write_file(OutFile, Bin),
    ok.

load_icons([Name|Ns]) ->
    Id = list_to_atom(filename:rootname(filename:basename(Name))),
    Bin = loadTexture(Name),
    [{Id,Bin}|load_icons(Ns)];
load_icons([]) -> [].
    
loadTexture(File) ->
    io:format("Loading ~s\n", [File]),
    {ok,Bin} = file:read_file(File),
    <<$B:8,$M:8,_:8/binary,Offset:32/little,_:32,
     W:32/little,H:32/little,T/binary>> = Bin,
    {_,Pixels} = split_binary(Bin, Offset+2),
    {W,H,shuffle_colors(Pixels, [])}.

shuffle_colors(<<R:8,B:8,G:8,T/binary>>, Acc) ->
    shuffle_colors(T, [[R,G,B]|Acc]);
shuffle_colors(<<>>, Acc) ->
    list_to_binary(reverse(Acc)).
