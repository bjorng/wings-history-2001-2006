%%
%%  e3d_rib.erl --
%%
%%     Functions for writing RenderMan compatible files (.rib).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_rib.erl,v 1.5 2001/08/27 07:34:51 bjorng Exp $
%%

-module(e3d_rib).
-export([export/2]).

-include("e3d.hrl").

-import(lists, [foldl/3,map/2,foreach/2]).

%% RenderMan export.

export(Name, #e3d_file{objs=Objs,mat=Mat,creator=Creator}=St) ->
    {ok,F} = file:open(Name, [write]),
    io:format(F, "# Exported from ~s\n", [Creator]),
    io:put_chars(F, "WorldBegin\n"),
    foreach(fun(Obj) -> export_object(F, Obj) end, Objs),
    io:put_chars(F, "WorldEnd\n"),
    ok = file:close(F).

export_object(F, #e3d_object{name=Name,obj=Mesh}) ->
    io:format(F, "# Object: ~s\n", [Name]),
    #e3d_mesh{fs=Fs,vs=Vs,he=He} = Mesh,

    io:put_chars(F, "AttributeBegin\n"),
    io:put_chars(F, "SubdivisionMesh \"catmull-clark\"\n"),

    io:put_chars(F, "[ "),
    foreach(fun(#e3d_face{vs=Vs}) ->
		    io:format(F, "~p ", [length(Vs)])
	    end, Fs),
    io:put_chars(F, "]\n"),

    io:put_chars(F, "[ "),
    foreach(fun(#e3d_face{vs=Vs}) ->
		    foreach(fun(V) ->
				    io:put_chars(F, integer_to_list(V)),
				    io:put_chars(F, " ")
			    end, Vs)
	    end, Fs),
    io:put_chars(F, "]\n"),

    io:put_chars(F, "[\"interpolateboundary\"] [0 0] [] []\n"),

    io:put_chars(F, "\"P\"\n[\n"),
    S = 50,
    foreach(fun({X,Y,Z}) ->
		    io:format(F, "~p ~p ~p\n", [X/S,Y/S,Z/S])
	    end, Vs),
    io:put_chars(F, "]\n"),
    io:put_chars(F, "AttributeEnd\n").
