%%
%%  e3d_obj.erl --
%%
%%     Functions for reading and writing Wawefront ASCII files (.obj).
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_obj.erl,v 1.20 2002/05/03 10:19:55 bjorng Exp $
%%

-module(e3d_obj).
-export([import/1,export/2]).

-include("e3d.hrl").
-include("e3d_image.hrl").

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
	 ignore_groups=false,			%Ignore groups if "o" seen.
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
	     [none|_] -> [];
	     Other -> Other
	 end,
    make_ftab(Fs, [#e3d_face{mat=Mat,vs=Vs,tx=Tx}|Acc]);
make_ftab([], Acc) -> Acc.

read(Parse, Fd, Acc) ->
    read(Parse, io:get_line(Fd, ''), Fd, Acc).

read(Parse, "#" ++ _Comment, Fd, Acc) ->
    read(Parse, Fd, Acc);
read(Parse, "\r\n", Fd, Acc) ->
    read(Parse, Fd, Acc);
read(Parse, "\n", Fd, Acc) ->
    read(Parse, Fd, Acc);
read(Parse, " " ++ Line, Fd, Acc) ->
    read(Parse, Line, Fd, Acc);
read(Parse, "\t" ++ Line, Fd, Acc) ->
    read(Parse, Line, Fd, Acc);
read(_Parse, eof, _Fd, Acc) -> Acc;
read(Parse, "mtllib" ++ Name0, Fd, Acc0) ->
    Name1 = skip_blanks(Name0),
    Name = case reverse(Name1) of
	       [$\n,$\r|Name2] -> reverse(Name2);
	       [$\n|Name2] -> reverse(Name2)
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
    X = str2float(X0),
    Y = str2float(Y0),
    Z = str2float(Z0),
    Ost#ost{v=[{X,Y,Z}|Vtab]};
parse(["vt",U0,V0|_], #ost{vt=Vt}=Ost) ->
    U = str2float(U0),
    V = str2float(V0),
    Ost#ost{vt=[{U,V}|Vt]};
parse(["vn",X0,Y0,Z0|_], #ost{vn=Vn}=Ost) ->
    X = str2float(X0),
    Y = str2float(Y0),
    Z = str2float(Z0),
    Ost#ost{vn=[{X,Y,Z}|Vn]};
parse(["f"|Vlist0], #ost{f=Ftab,mat=Mat}=Ost) ->
    Vlist = collect_vs(Vlist0, Ost),
    Ost#ost{f=[{Mat,Vlist}|Ftab]};
parse(["g"], Ost) ->
    Ost;
parse(["g"|_Names], #ost{ignore_groups=true}=Ost) ->
    Ost;
parse(["g"|Names], #ost{name=OldName}=Ost) ->
    case {Names,OldName} of
	{[Name|_],Name} -> Ost;
	{[Name|_],_} -> Ost#ost{name=Name}
    end;
parse(["o"], Ost) ->
    Ost;
parse(["o"|Names], #ost{name=OldName}=Ost) ->
    case {Names,OldName} of
	{[Name|_],undefined} -> Ost#ost{ignore_groups=true,name=Name};
	{_,_} -> Ost
    end;
parse(["usemtl"|[Mat|_]], Ost) ->
    Ost#ost{mat=[list_to_atom(Mat)]};
parse(["mtllib",FileName], #ost{dir=Dir}=Ost) ->
    Mat = read_matlib(FileName, Dir),
    Ost#ost{matdef=Mat};
parse(["End","Of","File"], _Ost) -> eof;	%In files written by ZBrush.
parse([Tag|_]=Other, #ost{seen=Seen}=Ost) ->
    case gb_sets:is_member(Tag, Seen) of
	true -> Ost;
	false ->
	    io:format("Ignoring: ~p\n", [Other]),
	    Ost#ost{seen=gb_sets:insert(Tag, Seen)}
    end.

collect_vs([V|Vs], Ost) ->
    [collect_vtxref(V, Ost)|collect_vs(Vs, Ost)];
collect_vs([], _Ost) -> [].

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

collect_vtxref_2(V0, Vt0, Vn0, #ost{v=Vtab,vt=VtTab,vn=VnTab}) ->
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
	    [{Mat,[{maps,Maps},{opengl,OpenGL}]} || {Mat,OpenGL,Maps} <- Res];
	{error,_Reason} -> error
    end.

mtl_parse(["newmtl",Name0], Ms) ->
    Name = list_to_atom(Name0),
    [{Name,[],[]}|Ms];
mtl_parse(["Ka"|RGB], Mtl) ->
    mtl_add({ambient,mtl_text_to_tuple(RGB)}, Mtl);
mtl_parse(["Kd"|RGB], Mtl) ->
    mtl_add({diffuse,mtl_text_to_tuple(RGB)}, Mtl);
mtl_parse(["map_Kd"|Filename0], Mtl) ->
    Filename = space_concat(Filename0),
    map_add({diffuse,Filename}, Mtl);
mtl_parse(["map_Ka"|Filename0], Mtl) ->
    Filename = space_concat(Filename0),
    map_add({ambient,Filename}, Mtl);
mtl_parse(["map_Bump"|Filename0], Mtl) ->
    Filename = space_concat(Filename0),
    map_add({bump,Filename}, Mtl);
mtl_parse([_|_], [{_,_,_}|_]=Mtl) -> Mtl.

mtl_add(P, [{Name,OpenGL,Maps}|Ms]) ->
    [{Name,[P|OpenGL],Maps}|Ms].

map_add(P, [{Name,OpenGL,Maps}|Ms]) ->
    [{Name,OpenGL,[P|Maps]}|Ms].

mtl_text_to_tuple(L) ->
    list_to_tuple([str2float(F) || F <- L]).

str2float(S) ->
    case catch list_to_float(S) of
	{'EXIT',_} -> float(list_to_integer(S));
	F -> F
    end.

space_concat([Str|[_|_]=T]) ->
    Str ++ [$\s|space_concat(T)];
space_concat([S]) -> S;
space_concat([]) -> [].

skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks([$\t|T]) -> skip_blanks(T);
skip_blanks(S) -> S.
    
%%%
%%% Export.
%%% 

export(File, #e3d_file{objs=Objs,mat=Mat,creator=Creator}) ->
    {ok,MtlLib} = materials(File, Mat, Creator),
    {ok,F} = file:open(File, [write]),
    label(F, Creator),
    io:format(F, "mtllib ~s\n", [MtlLib]),
    foldl(fun(#e3d_object{name=Name}=Obj, {Vbase,UVbase,Nbase}) ->
		  io:format(F, "o ~s\n", [Name]),
		  export_object(F, Obj, Vbase, UVbase, Nbase)
	  end, {1,1,1}, Objs),
    ok = file:close(F).

export_object(F, #e3d_object{name=Name,obj=Mesh0}, Vbase, UVbase, Nbase) ->
    Mesh = e3d_mesh:vertex_normals(Mesh0),
    #e3d_mesh{fs=Fs0,vs=Vs,tx=Tx,ns=Ns} = Mesh,
    mesh_info(F, Mesh),
    foreach(fun({X,Y,Z}) ->
		    io:format(F, "v ~p ~p ~p\n", [X,Y,Z])
	    end, Vs),
    foreach(fun({U,V}) ->
		    io:format(F, "vt ~p ~p\n", [U,V])
	    end, Tx),
    foreach(fun({X,Y,Z}) ->
		    io:format(F, "vn ~p ~p ~p\n", [X,Y,Z])
	    end, Ns),
    Fs1 = [{Mat,FaceRec} || #e3d_face{mat=Mat}=FaceRec <- Fs0],
    Fs = sofs:to_external(sofs:relation_to_family(sofs:relation(Fs1))),
    foreach(fun(Face) ->
		    face_mat(F, Name, Face, Vbase, UVbase, Nbase)
	    end, Fs),
    {Vbase+length(Vs),UVbase+length(Tx),Nbase+length(Ns)}.

face_mat(F, Name, {Ms,Fs}, Vbase, UVbase, Nbase) ->
    io:format(F, "g ~s", [Name]),
    foreach(fun(M) ->
		    io:format(F, "_~s", [atom_to_list(M)])
	    end, Ms),
    io:nl(F),
    io:put_chars(F, "usemtl"),
    foldl(fun(M, Prefix) ->
		  io:format(F, "~c~s", [Prefix,atom_to_list(M)])
	  end, $\s, Ms),
    io:nl(F),
    foreach(fun(Vs) -> face(F, Vs, Vbase, UVbase, Nbase) end, Fs).

face(F, #e3d_face{vs=Vs,tx=[],ns=Ns}, Vbase, _UVbase, Nbase) ->
    io:put_chars(F, "f"),
    face_nouv(F, Vs, Ns, Vbase, Nbase),
    io:nl(F);
face(F, #e3d_face{vs=Vs,tx=Tx,ns=Ns}, Vbase, UVbase, Nbase) ->
    io:put_chars(F, "f"),
    face_uv(F, Vs, Tx, Ns, Vbase, UVbase, Nbase),
    io:nl(F).

face_nouv(F, [V|Vs], [N|Ns], Vbase, Nbase) ->
    io:format(F, " ~p//~p", [V+Vbase,N+Nbase]),
    face_nouv(F, Vs, Ns, Vbase, Nbase);
face_nouv(_, [], [], _, _) -> ok.

face_uv(F, [V|Vs], [UV|UVs], [N|Ns], Vbase, UVbase, Nbase) ->
    io:format(F, " ~p/~p/~p", [V+Vbase,UV+UVbase,N+Nbase]),
    face_uv(F, Vs, UVs, Ns, Vbase, UVbase, Nbase);
face_uv(_, [], [], [], _, _, _) -> ok.

materials(Name0, Mats, Creator) ->
    Root = filename:rootname(Name0, ".obj"),
    Name = Root ++ ".mtl",
    Base = filename:basename(Root),
    {ok,F} = file:open(Name, [write]),
    label(F, Creator),
    foreach(fun(M) -> material(F, Base, M) end, Mats),
    file:close(F),
    {ok,filename:basename(Name)}.

material(F, Base, {Name,Mat}) ->
    OpenGL = property_lists:get_value(opengl, Mat),
    io:format(F, "newmtl ~s\n", [atom_to_list(Name)]),
    io:format(F, "Ns 80\n", []),
    io:format(F, "d 1.000000\n", []),
    io:format(F, "illum 2\n", []),
    mat_color(F, "Kd", diffuse, OpenGL),
    mat_color(F, "Ka", ambient, OpenGL),
    mat_color(F, "Ks", specular, OpenGL),
    Maps = property_lists:get_value(maps, Mat),
    export_maps(F, Maps, Base, Name),
    io:nl(F).

mat_color(F, Label, Key, Mat) ->
    {R,G,B,_} = property_lists:get_value(Key, Mat),
    io:format(F, "~s ~p ~p ~p\n", [Label,R,G,B]).

export_maps(F, [{diffuse,Map}|T], Base, Name) ->
    export_map(F, "Kd", Map, Base, Name),
    export_maps(F, T, Base, Name);
export_maps(F, [{ambient,Map}|T], Base, Name) ->
    export_map(F, "Ka", Map, Base, Name),
    export_maps(F, T, Base, Name);
export_maps(F, [{bump,Map}|T], Base, Name) ->
    export_map(F, "Bump", Map, Base, Name),
    export_maps(F, T, Base, Name);
export_maps(F, [_|T], Base, Name) ->
    export_maps(F, T, Base, Name);
export_maps(_, [], _, _) -> ok.

export_map(_, _, none, _, _) -> ok;
export_map(F, Label0, {W,H,Map}, Base, Name) ->
    Label = "map_" ++ Label0,
    MapFile = Base ++ "_" ++ atom_to_list(Name) ++ "_" ++ Label ++ ".tga",
    io:format(F, "~s ~s\n", [Label,MapFile]),
    Image = #e3d_image{image=Map,width=W,height=H},
    ok = e3d_image:save(Image, MapFile).

label(F, Creator) ->
    io:format(F, "# Exported from ~s\n", [Creator]).

mesh_info(F, #e3d_mesh{vs=Vs,fs=Fs}) ->
    io:format(F, "#~w vertices, ~w faces\n", [length(Vs),length(Fs)]).
