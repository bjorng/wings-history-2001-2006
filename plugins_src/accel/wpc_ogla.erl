%%
%%  wpc_ogla.erl --
%%
%%     Plug-in for accelerating certain OpenGL operations.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_ogla.erl,v 1.1 2004/04/19 04:33:59 bjorng Exp $
%%

-module(wpc_ogla).
-export([init/0]).
-export([two/2,tri/3,quad_tri/4,quad/4]).

-define(FL32, :32/native-float).

init() ->
    Dir = filename:dirname(code:which(?MODULE)),
    Name = "wings_ogla_drv",
    case erl_ddll:load_driver(Dir, Name) of
	ok -> ok;
	{error,Reason} ->
	    io:format("Failed to load ~s in ~s\n~s\n",
		      [Name,Dir,erl_ddll:format_error(Reason)]),
	    erlang:fault(startup_fault)
    end,
    case open_port({spawn,Name},[]) of
	Port when is_port(Port) ->
	    register(wings_ogla_port, Port);
	_ ->
	    io:format("Failed to open port ~s\n", [Name]),
	    erlang:fault(startup_fault)
    end,

    false.

tri({Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}) ->
    %%  gl:vertex3fv(A),
    %%  gl:vertex3fv(B),
    %%  gl:vertex3fv(C);
    Bin = <<Ax?FL32,Ay?FL32,Az?FL32,
	   Bx?FL32,By?FL32,Bz?FL32,
	   Cx?FL32,Cy?FL32,Cz?FL32>>,
    erlang:port_control(wings_ogla_port, 0, Bin).

quad_tri({Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}, {Dx,Dy,Dz}) ->
    %%  gl:vertex3fv(A),
    %%  gl:vertex3fv(B),
    %%  gl:vertex3fv(C),
    %%  gl:vertex3fv(C),
    %%  gl:vertex3fv(D),
    %%  gl:vertex3fv(A)
    Bin = <<Ax?FL32,Ay?FL32,Az?FL32,
	   Bx?FL32,By?FL32,Bz?FL32,
	   Cx?FL32,Cy?FL32,Cz?FL32,
	   Dx?FL32,Dy?FL32,Dz?FL32>>,
    erlang:port_control(wings_ogla_port, 1, Bin).

quad({Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}, {Dx,Dy,Dz}) ->
    %%  gl:vertex3fv(A),
    %%  gl:vertex3fv(B),
    %%  gl:vertex3fv(C),
    %%  gl:vertex3fv(D)
    Bin = <<Ax?FL32,Ay?FL32,Az?FL32,
	   Bx?FL32,By?FL32,Bz?FL32,
	   Cx?FL32,Cy?FL32,Cz?FL32,
	   Dx?FL32,Dy?FL32,Dz?FL32>>,
    erlang:port_control(wings_ogla_port, 2, Bin).

two({Ax,Ay,Az}, {Bx,By,Bz}) ->
    %%  gl:vertex3fv(A),
    %%  gl:vertex3fv(B)
    Bin = <<Ax?FL32,Ay?FL32,Az?FL32,
	   Bx?FL32,By?FL32,Bz?FL32>>,
    erlang:port_control(wings_ogla_port, 3, Bin).
