%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 2001, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% Original Author: Bjorn Gustavsson
%% 
%%     $Id: collect_bmp.erl,v 1.2 2001/10/18 16:06:51 bjorng Exp $
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
    {ok,Bin0} = file:read_file(File),
    <<$B:8,$M:8,_:8/binary,Offset:32/little,Bin/binary>> = Bin0,
    <<_:32/little,W:32/little,H:32/little,
     _:16,BitCount:16/little,Compression:16/little,_/binary>> = Bin,
    BitCount = 24,
    Compression = 0,
    PixelsLen = H*W*3,
    <<_:Offset/binary,Pixels0:PixelsLen/binary,_/binary>> = Bin0,
    Pixels = shuffle_colors(Pixels0, []),
    {W,H,Pixels}.

shuffle_colors(<<B:8,G:8,R:8,T/binary>>, Acc) ->
    shuffle_colors(T, [[R,G,B]|Acc]);
shuffle_colors(<<>>, Acc) -> list_to_binary(reverse(Acc)).
