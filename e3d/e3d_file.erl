%%
%%  e3d_file.erl --
%%
%%     Utility functions e3d_file records.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_file.erl,v 1.1 2002/06/14 13:02:16 bjorng Exp $
%%

-module(e3d_file).

-export([map/2,transform/2]).

-include("e3d.hrl").

map(F, #e3d_file{objs=Objs0}=File) ->
    Objs = lists:map(F, Objs0),
    File#e3d_file{objs=Objs}.

transform(File, Matrix) ->
    map(fun(#e3d_object{obj=Mesh0}=Obj) ->
		Mesh = e3d_mesh:transform(Mesh0, Matrix),
		Obj#e3d_object{obj=Mesh}
	end, File).


