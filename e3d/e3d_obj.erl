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
%%     $Id: e3d_obj.erl,v 1.9 2001/09/14 09:58:02 bjorng Exp $
%%

-module(e3d_obj).
-export([import/1,export/2]).

-include("e3d.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,foreach/2,
		map/2,foldl/3]).

-record(ost,
	{v=[],					%Vertices.
	 vt=[],					%Texture vertices.
	 vn=[],					%Vertice normals.
	 f=[],					%Faces.
	 mat=[],				%Current material.
	 matdef=[],				%Material definitions.
	 name,					%Object name.
	 dir,					%Directory of .obj file.
	 seen=gb_sets:empty()}).		%Unknown type seen.

import(Name) ->
    case file:open(Name, [read]) of
	{ok,Fd} ->
	    Dir = filename:dirname(Name),
	    Res = import_1(Fd, Dir),
	    file:close(Fd),
	    Res;
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_1(Fd, Dir) ->
    case catch import_2(Fd, Dir) of
	{'EXIT',Reason} -> exit(Reason);
	{error,_}=Error -> Error;
	#e3d_file{}=E3dFile -> {ok,E3dFile}
    end.

import_2(Fd, Dir) ->
    Ost = read(fun parse/2, Fd, #ost{dir=Dir}),
    #ost{name=Name,v=Vtab0,vt=TxTab0,f=Ftab0,matdef=Mat} = Ost,
    Vtab = reverse(Vtab0),
    TxTab = reverse(TxTab0),
    Ftab = make_ftab(Ftab0, []),
    Mesh = #e3d_mesh{type=polygon,vs=Vtab,fs=Ftab,tx=TxTab},
    Obj = #e3d_object{name=Name,obj=Mesh},
    #e3d_file{objs=[Obj],mat=Mat}.

make_ftab([{Mat,Vs0}|Fs], Acc) ->
    Vs = [V || {V,_,_} <- Vs0],
    Tx = case [Vt || {_,Vt,_} <- Vs0] of
	     [none|_] -> none;
	     Other -> Other
	 end,
    make_ftab(Fs, [#e3d_face{mat=Mat,vs=Vs,tx=Tx}|Acc]);
make_ftab([], Acc) -> Acc.

read(Parse, Fd, Acc) ->
    read(Parse, io:get_line(Fd, ''), Fd, Acc).

read(Parse, "#" ++ Comment, Fd, Acc) ->
    read(Parse, Fd, Acc);
read(Parse, "\r\n", Fd, Acc) ->
    read(Parse, Fd, Acc);
read(Parse, "\n", Fd, Acc) ->
    read(Parse, Fd, Acc);
read(Parse, " " ++ Line, Fd, Acc) ->
    read(Parse, Line, Fd, Acc);
read(Parse, eof, Fd, Acc) -> Acc;
read(Parse, "mtllib " ++ Name0, Fd, Acc0) ->
    Name = case reverse(Name0) of
	       [$\n,$\r|Name1] -> reverse(Name1);
	       [$\n|Name1] -> reverse(Name1)
	   end,
    case Parse(["mtllib",Name], Acc0) of
	eof -> Acc0;
	Acc -> read(Parse, Fd, Acc)
    end;
read(Parse, Line, Fd, Acc0) ->
    case Parse(collect(Line, [], []), Acc0) of
	eof -> Acc0;
	Acc -> read(Parse, Fd, Acc)
    end.

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
parse(["vt",U0,V0|_], #ost{vt=Vt}=Ost) ->
    U = list_to_float(U0),
    V = list_to_float(V0),
    Ost#ost{vt=[{U,V}|Vt]};
parse(["vn",X0,Y0,Z0|_], #ost{vn=Vn}=Ost) ->
    X = list_to_float(X0),
    Y = list_to_float(Y0),
    Z = list_to_float(Z0),
    Ost#ost{vn=[{X,Y,Z}|Vn]};
parse(["f"|Vlist0], #ost{f=Ftab,mat=Mat}=Ost) ->
    Vlist = collect_vs(Vlist0, Ost),
    Ost#ost{f=[{Mat,Vlist}|Ftab]};
parse(["g"|Names], #ost{name=OldName}=Ost) ->
    case {Names,OldName} of
	{[Name|_],undefined} -> Ost#ost{name=Name};
	{_,_} -> Ost
    end;
parse(["usemtl"|[Mat|_]], Ost) ->
    Ost#ost{mat=[list_to_atom(Mat)]};
parse(["mtllib",FileName], #ost{dir=Dir}=Ost) ->
    Mat = read_matlib(FileName, Dir),
    Ost#ost{matdef=Mat};
parse(["End","Of","File"], Ost) -> eof;		%In files written by ZBrush.
parse([Tag|Args]=Other, #ost{seen=Seen}=Ost) ->
    case gb_sets:is_member(Tag, Seen) of
	true -> Ost;
	false ->
	    io:format("Ignoring: ~p\n", [Other]),
	    Ost#ost{seen=gb_sets:insert(Tag, Seen)}
    end.

collect_vs([V|Vs], Ost) ->
    [collect_vtxref(V, Ost)|collect_vs(Vs, Ost)];
collect_vs([], Ost) -> [].

collect_vtxref(S, Ost) ->
    case collect_vtxref_1(S, []) of
	[V] -> collect_vtxref_2(V, none, none, Ost);
	[V,Vt] -> collect_vtxref_2(V, Vt, none, Ost);
	[V,Vt,Vn|_] -> collect_vtxref_2(V, Vt, Vn, Ost)
    end.

collect_vtxref_1([], Acc) -> reverse(Acc);
collect_vtxref_1(S0, Acc) ->
    {Ref,S} = collect_one_vtxref(S0),
    collect_vtxref_1(S, [Ref|Acc]).

collect_vtxref_2(V0, Vt0, Vn0, #ost{v=Vtab,vt=VtTab,vn=VnTab}=Ost) ->
    V = resolve_vtxref(V0, Vtab),
    Vt = resolve_vtxref(Vt0, VtTab),
    Vn = resolve_vtxref(Vn0, VnTab),
    {V,Vt,Vn}.

resolve_vtxref(none, _) -> none;
resolve_vtxref(V, _) when V > 0 -> V-1;
resolve_vtxref(V0, Tab) when V0 < 0 ->
    case length(Tab)+V0 of
	V when V >= 0 -> V
    end.

collect_one_vtxref(S) ->
    collect_one_vtxref(S, []).

collect_one_vtxref([$/|S], Acc) ->
    collect_one_vtxref_done(S, Acc);
collect_one_vtxref([H|T], Acc) ->
    collect_one_vtxref(T, [H|Acc]);
collect_one_vtxref([], Acc) ->
    collect_one_vtxref_done([], Acc).

collect_one_vtxref_done(S, []) -> {none,S};
collect_one_vtxref_done(S, V0) -> {list_to_integer(reverse(V0)),S}.

read_matlib(Name, Dir) ->
    case try_matlib(filename:join(Dir, Name)) of
	error ->
	    case try_matlib(filename:join(Dir, Name)) of
		error -> [];
		Other -> Other
	    end;
	Res -> Res
    end.

try_matlib(Name) ->
    case file:open(Name, [read]) of
	{ok,Fd} ->
	    Res = read(fun mtl_parse/2, Fd, []),
	    file:close(Fd),
	    Res;
	{error,Reason} -> error
    end.

mtl_parse(["newmtl",Name0], Ms) ->
    Name = list_to_atom(Name0),
    [{Name,[]}|Ms];
mtl_parse(["Ka"|RGB], Mtl) ->
    mtl_add({ambient,mtl_text_to_tuple(RGB)}, Mtl);
mtl_parse(["Kd"|RGB], Mtl) ->
    mtl_add({diffuse,mtl_text_to_tuple(RGB)}, Mtl);
mtl_parse(["map_Kd",Filename], Mtl) ->
    mtl_add({diffuse_map,Filename}, Mtl);
mtl_parse([_|_]=Other, [{Name,_}|_]=Mtl) ->
    io:format("Material ~w: ~p\n", [Name,Other]),
    Mtl.

mtl_add(P, [{Name,Props}|Ms]) ->
    [{Name,[P|Props]}|Ms].

mtl_text_to_tuple(L) ->
    list_to_tuple([list_to_float(F) || F <- L]).
    
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

export_object(F, #e3d_object{name=Name,obj=Mesh}, Vbase, Nbase) ->
    #e3d_mesh{vs=Vs} = Mesh,
    mesh_info(F, Mesh),
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

label(F, Creator) ->
    io:format(F, "# Exported from ~s\n", [Creator]).

mesh_info(F, #e3d_mesh{vs=Vs,fs=Fs}) ->
    io:format(F, "#~w vertices, ~w faces\n", [length(Vs),length(Fs)]).
