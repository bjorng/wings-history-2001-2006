%%
%%  e3d_obj.erl --
%%
%%     Functions for reading and writing Wawefront ASCII files (.obj).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_obj.erl,v 1.3 2001/08/24 08:44:12 bjorng Exp $
%%

-module(e3d_obj).
-compile(export_all).
-export([import/1,export/2]).

-include("e3d.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,foreach/2,
		map/2,foldl/3]).

-record(ost,
	{v=[],					%Vertices
	 f=[]}).				%Faces

import(Name) ->
    case file:open(Name, [read]) of
	{ok,Fd} ->
	    Res = import_1(Fd),
	    file:close(Fd),
	    Res;
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_1(Fd) ->
    case catch import_2(Fd) of
	{'EXIT',Reason} -> exit(Reason);
	{error,_}=Error -> Error;
	#e3d_file{}=E3dFile -> {ok,E3dFile}
    end.

import_2(Fd) ->
    Ost = read(Fd, #ost{}),
    #ost{v=Vtab0,f=Ftab} = Ost,
    Vtab = reverse(Vtab0),
    Mesh0 = #e3d_mesh{type=polygon,vs=Vtab,fs=Ftab},
    Mesh = e3d_mesh:clean(Mesh0),
    Obj = #e3d_object{name="object",obj=Mesh},
    #e3d_file{objs=[Obj]}.

read(Fd, Acc) ->
    read(io:get_line(Fd, ''), Fd, Acc).

read("#" ++ Comment, Fd, Acc) -> read(Fd, Acc);
read("\r\n", Fd, Acc) -> read(Fd, Acc);
read("\n", Fd, Acc) -> read(Fd, Acc);
read(" " ++ Line, Fd, Acc) ->
    read(Line, Fd, Acc);
read(eof, Fd, Acc) -> Acc;
read(Line, Fd, Acc0) ->
    Acc = parse(collect(Line, [], []), Acc0),
    read(Fd, Acc).

collect([$\s|T], [], Tokens) ->
    collect(T, [], Tokens);
collect([$\s|T], Curr, Tokens) ->
    collect(T, [], [reverse(Curr)|Tokens]);
collect([$\r|T], Curr, Tokens) ->
    collect([$\s|T], Curr, Tokens);
collect([$\n|T], Curr, Tokens) ->
    collect([$\s|T], Curr, Tokens);
collect([H|T], Curr, Tokens) ->
    collect(T, [H|Curr], Tokens);
collect([], Curr, Tokens) ->
    reverse(Tokens, reverse(Curr)).

parse(["v",X0,Y0,Z0|_], #ost{v=Vtab}=Ost) ->
    X = list_to_float(X0),
    Y = list_to_float(Y0),
    Z = list_to_float(Z0),
    Ost#ost{v=[{X,Y,Z}|Vtab]};
parse(["f"|Vlist0], #ost{f=Ftab}=Ost) ->
    Vlist1 = collect_vs(Vlist0, Ost),
    Vlist = [V-1 || {V,_,_} <- Vlist1],
    Ost#ost{f=[#e3d_face{vs=Vlist}|Ftab]};
parse(Other, Acc) ->
    io:format("Ignoring: ~p\n", [Other]),
    Acc.

collect_vs([V|Vs], Ost) ->
    [collect_vertex(V)|collect_vs(Vs, Ost)];
collect_vs([], Ost) -> [].

collect_vertex(S) ->
    collect_vertex(S, []).

collect_vertex([$/|_], Acc) ->
    collect_vertex([], Acc);
collect_vertex([H|T], Acc) ->
    collect_vertex(T, [H|Acc]);
collect_vertex([], Acc) ->
    {list_to_integer(reverse(Acc)),none,none}.

%%%
%%% Export.
%%% 

export(Name, #e3d_file{objs=Objs,mat=Mat,creator=Creator}) ->
    {ok,MtlLib} = materials(Name, Mat, Creator),
    {ok,F} = file:open(Name, [write]),
    label(F, Creator),
    io:format(F, "mtllib ~s\n", [MtlLib]),
    foldl(fun(Obj, {Vbase,Nbase}) ->
		  export_object(F, Obj, Vbase, Nbase)
	  end, {1,1}, Objs),
    ok = file:close(F).

materials(Name0, Mats, Creator) ->
    Name = filename:rootname(Name0, ".obj") ++ ".mtl",
    {ok,F} = file:open(Name, [write]),
    label(F, Creator),
    foreach(fun(M) -> material(F, M) end, Mats),
    file:close(F),
    {ok,filename:basename(Name)}.

material(F, {Name,Mat}) ->
    {value,{_,{R,G,B}}} = keysearch(ambient, 1, Mat),
    io:format(F, "newmtl ~p\n", [Name]),
    io:format(F, "Ns 80\n", []),
    io:format(F, "d 1.000000\n", []),
    io:format(F, "illum 2\n", []),
    Ambience = 0.5,
    io:format(F, "Ka ~p ~p ~p\n", [R*Ambience,G*Ambience,B*Ambience]),
    One = 1.0,
    io:format(F, "Kd ~p ~p ~p\n", [One,One,One]),
    Specular = 0.2,
    io:format(F, "Ks ~p ~p ~p\n", [R*Specular,G*Specular,R*Specular]),
    io:nl(F).

export_object(F, #e3d_object{name=Name,obj=Mesh}, Vbase, Nbase) ->
    #e3d_mesh{vs=Vs} = Mesh,
    io:format(F, "g ~s\n", [Name]),
    foreach(fun({X,Y,Z}) ->
		    io:format(F, "v ~p ~p ~p\n", [X,Y,Z])
	    end, Vs),
    {Fs0,Ns} = e3d_mesh:vertex_normals(Mesh),
    foreach(fun({X,Y,Z}) ->
		    io:format(F, "vn ~p ~p ~p\n", [X,Y,Z])
	    end, Ns),
    Fs = sofs:to_external(sofs:relation_to_family(sofs:relation(Fs0))),
    foreach(fun(Face) -> face_mat(F, Name, Face, Vbase, Nbase) end, Fs),
    {Vbase+length(Vs),Nbase+length(Ns)}.

face_mat(F, Name, {Ms,Fs}, Vbase, Nbase) ->
    io:format(F, "g ~s", [Name]),
    foreach(fun(M) ->
		    io:format(F, " ~p", [M])
	    end, Ms),
    io:nl(F),
    io:put_chars(F, "usemtl"),
    foreach(fun(M) ->
		    io:format(F, " ~p", [M])
	    end, Ms),
    io:nl(F),
    foreach(fun(Vs) -> face(F, Vs, Vbase, Nbase) end, Fs).

face(F, Vs, Vbase, Nbase) ->
    io:put_chars(F, "f"),
    foreach(fun({V,N}) ->
		    io:format(F, " ~p//~p", [V+Vbase,N+Nbase])
	    end, Vs),
    io:nl(F).

label(F, Creator) ->
    io:format(F, "# Exported from ~s\n", [Creator]).
