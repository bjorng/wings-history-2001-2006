%%
%%  e3d_tds.erl --
%%
%%     Functions for reading and writing 3D Studio Max files (.tds),
%%     version 3.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_tds.erl,v 1.16 2002/09/18 13:16:06 bjorng Exp $
%%

-module(e3d_tds).
-export([import/1,export/2]).

-include("e3d.hrl").

-import(lists, [map/2,reverse/1,reverse/2,sort/1,foldl/3]).
-define(FLOAT, float-little).

%%%
%%% Import.
%%% 

import(Name) ->
    case file:read_file(Name) of
	{ok,Bin} ->
	    import_1(Bin);
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_1(Bin) ->
    case catch import_2(Bin) of
	{'EXIT',Reason} -> exit(Reason);
	{error,_}=Error -> Error;
	#e3d_file{}=E3dFile -> {ok,E3dFile}
    end.

import_2(<<16#4D4D:16/little,Size:32/little,Rest/binary>>) ->
    dbg("Main chunk ~p bytes~n", [Size]),
    main(Rest, #e3d_file{});
import_2(_) -> error("Not a .3ds file").

main(<<16#0002:16/little,10:32/little,Ver:32/little,Rest/binary>>, Acc) ->
    dbg("3DS Version ~p~n", [Ver]),
    main(Rest, Acc);
main(<<16#3D3D:16/little,Sz0:32/little,Rest0/binary>>, Acc0) ->
    dbg("Editor: ~p bytes\n", [Sz0]),
    Sz = Sz0 - 6,
    <<Ed:Sz/binary,Rest/binary>> = Rest0,
    Acc = editor(Ed, Acc0),
    main(Rest, Acc);
main(<<16#B000:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    dbg("Key Framer (ignoring) ~p bytes\n", [Sz0]),
    Sz = Sz0 - 6,
    <<_:Sz/binary,T/binary>> = T0,
    main(T, Acc);
main(<<Chunk:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    dbg("Ignoring unknown chunk ~s; ~p bytes\n", [hex4(Chunk),Sz0]),
    Sz = Sz0 - 6,
    <<_:Sz/binary,T/binary>> = T0,
    main(T, Acc);
main(<<>>, #e3d_file{mat=Mat0}=File) ->
    Mat = reformat_material(Mat0),
    File#e3d_file{mat=Mat}.

editor(<<16#4000:16/little,Sz0:32/little,T0/binary>>,
       #e3d_file{objs=Objs0}=Acc) ->
    Sz = Sz0 - 6,
    <<Obj0:Sz/binary,T/binary>> = T0,
    {Name,Obj1} = get_cstring(Obj0),
    dbg("Object block '~s'~n", [Name]),
    case block(Obj1) of
	no_mesh -> editor(T, Acc);
	Obj ->
	    Objs = [#e3d_object{name=Name,obj=Obj}|Objs0],
	    editor(T, Acc#e3d_file{objs=Objs})
    end;
editor(<<16#3d3e:16/little,_Sz:32/little,Ver:32/little,T/binary>>, Acc) ->
    dbg("Mesh Version ~p ~n", [Ver]),
    editor(T, Acc);
editor(<<16#0100:16/little,_Sz:32/little,Scale:32/float-little,T/binary>>,
       Acc) ->
    dbg("Object Scale ~p ~n", [Scale]),
    editor(T, Acc);
editor(<<16#AFFF:16/little,Sz0:32/little,T0/binary>>, #e3d_file{mat=M}=Acc) ->
    dbg("Material block ~p ~n", [Sz0]),
    Sz = Sz0 - 6,
    <<Mat0:Sz/binary,T/binary>> = T0,
    Mat = material(Mat0, M),
    editor(T, Acc#e3d_file{mat=Mat});
editor(<<>>, Acc) -> Acc.

block(Bin) ->
    block(Bin, no_mesh).
block(<<16#3d3e:16/little,_Sz:32/little,Ver:32/little,T/binary>>, Acc) ->
    dbg("Mesh Version ~p ~n", [Ver]),
    block(T, Acc);
block(<<16#4100:16/little,Sz0:32/little,T0/binary>>, no_mesh) ->
    dbg("Triangular mesh ~p~n", [Sz0]),
    Sz = Sz0 - 6,
    <<TriMesh0:Sz/binary,T/binary>> = T0,
    TriMesh1 = trimesh(TriMesh0, #e3d_mesh{type=triangle}),
    TriMesh2 = add_uv_to_faces(TriMesh1),
    TriMesh = clean_mesh(TriMesh2),
    block(T, TriMesh);
block(<<16#4700:16/little,Sz0:32/little,T0/binary>>, Mesh) ->
    dbg("Camera (ignoring) ~p bytes\n", [Sz0]),
    Sz = Sz0 - 6,
    <<_:Sz/binary,T/binary>> = T0,
    block(T, Mesh);
block(<<Chunk:16/little,Sz0:32/little,T0/binary>>, Mesh) ->
    dbg("Ignoring unknown chunk ~s; ~p bytes\n", [hex4(Chunk),Sz0]),
    Sz = Sz0 - 6,
    <<_:Sz/binary,T/binary>> = T0,
    block(T, Mesh);
block(<<>>, Acc) -> Acc.

trimesh(<<16#4110:16/little,Sz0:32/little,NumVs:16/little,T0/binary>>, Acc) ->
    dbg("Vertices ~p bytes NumVs=~p\n", [Sz0,NumVs]),
    Sz = Sz0 - 8,
    <<Vs0:Sz/binary,T/binary>> = T0,
    Vs = get_bin_vectors(Vs0),
    trimesh(T, Acc#e3d_mesh{vs=Vs});
trimesh(<<16#4120:16/little,Sz0:32/little,NFaces:16/little,T0/binary>>, Acc) ->
    dbg("~p faces, ~p bytes\n", [NFaces,Sz0]),
    Sz = Sz0 - 8,
    Fsz = NFaces * 2 * 4,
    Dsz = Sz - Fsz,
    <<Faces0:Fsz/binary,Desc:Dsz/binary,T/binary>> = T0,
    Faces1 = get_faces(Faces0),
    {Faces,Smooth} = face_desc(Desc, {Faces1,[]}),
    HardEdges = hard_edges(Smooth, Faces),
    trimesh(T, Acc#e3d_mesh{fs=Faces,he=HardEdges});
trimesh(<<16#4140:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    dbg("Texture coordinates ~w\n", [Sz0]),
    Sz = Sz0 - 8,
    <<_:16,Tx0:Sz/binary,T/binary>> = T0,
    Tx = get_uv(Tx0),
    trimesh(T, Acc#e3d_mesh{tx=Tx});
trimesh(<<16#4160:16/little,_Sz:32/little,T0/binary>>, Acc) ->
    <<V1X:32/float-little,V1Y:32/float-little,V1Z:32/float-little,
     V2X:32/float-little,V2Y:32/float-little,V2Z:32/float-little,
     V3X:32/float-little, V3Y:32/float-little,V3Z:32/float-little,
     OX:32/float-little,OY:32/float-little,OZ:32/float-little,
     T/binary>> = T0,
    CS = {1.0,0.0,0.0,0.0,
	  0.0,1.0,0.0,0.0,
	  0.0,0.0,1.0,0.0,
	  OX,OY,OZ,1.0},
    %%dbg("Local:~p\n", [CS]),
    %%trimesh(T, Acc#e3d_mesh{matrix=Id});
    %% Ignore local coordinate system.
    trimesh(T, Acc);
trimesh(<<Chunk:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    dbg("Ignoring chunk ~s, size ~p\n", [hex4(Chunk),Sz0]),
    Sz = Sz0 - 6,
    <<_:Sz/binary,T/binary>> = T0,
    trimesh(T, Acc);
trimesh(<<>>, Acc) -> Acc.

face_desc(<<16#4130:16/little,Sz0:32/little,T0/binary>>, {Faces0,SG}) ->
    Sz = Sz0 - 6,
    <<MatList0:Sz/binary,T/binary>> = T0,
    {Name0,MatList} = get_cstring(MatList0),
    Name = list_to_atom(Name0),
    dbg("Material ref: ~p\n", [Name]),
    MatFaces0 = get_mat_faces(MatList),
    MatFaces = sort(MatFaces0),
    Faces = insert_mat(Faces0, MatFaces, 0, Name, []),
    face_desc(T, {Faces,SG});
face_desc(<<16#4150:16/little,Sz0:32/little,T0/binary>>, {Faces,_}) ->
    dbg("Smoothing groups ~p bytes\n", [Sz0]),
    Sz = Sz0 - 6,
    <<Smooth0:Sz/binary,T/binary>> = T0,
    Smooth = get_smooth_groups(Smooth0),
    face_desc(T, {Faces,Smooth});
face_desc(<<>>, Acc) -> Acc.

material(<<16#A000:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    Sz = Sz0 - 6,
    <<Name0:Sz/binary,T/binary>> = T0,
    {Name1,<<>>} = get_cstring(Name0),
    Name = list_to_atom(Name1),
    dbg("Material: ~p\n", [Name]),
    material(T, [{Name,[]}|Acc]);
material(<<16#A010:16/little,Sz:32/little,T/binary>>, Acc) ->
    mat_chunk(ambient, Sz, T, Acc);
material(<<16#A020:16/little,Sz:32/little,T/binary>>, Acc) ->
    mat_chunk(diffuse, Sz, T, Acc);
material(<<16#A030:16/little,Sz:32/little,T/binary>>, Acc) ->
    mat_chunk(specular, Sz, T, Acc);
material(<<16#A040:16/little,Sz:32/little,T/binary>>, Acc) ->
    mat_chunk(shininess, Sz, T, Acc);
material(<<16#A041:16/little,Sz:32/little,T/binary>>, Acc) ->
    mat_chunk(shininess_level, Sz, T, Acc);
material(<<16#A050:16/little,Sz0:32/little,T0/binary>>, [{Name,Props}|Acc]) ->
    dbg("Material ~p, property opacity\n", [Name]),
    Sz = Sz0 - 6,
    <<Chunk0:Sz/binary,T/binary>> = T0,
    Chunk = general(Chunk0),
    material(T, [{Name,[{opacity,1-Chunk}|Props]}|Acc]);
material(<<16#A052:16/little,Sz:32/little,T/binary>>, Acc) ->
    mat_chunk(opacity_falloff, Sz, T, Acc);
material(<<16#A053:16/little,Sz:32/little,T/binary>>, Acc) ->
    mat_chunk(reflection_blur, Sz, T, Acc);
material(<<16#A081:16/little,6:32/little,T/binary>>,
	 [{Name,Props}|Acc]) ->
    dbg("Twosided material\n", []),
    material(T, [{Name,[{twosided,true}|Props]}|Acc]);
material(<<16#A084:16/little,Sz:32/little,T/binary>>, Acc) ->
    mat_chunk(emissive, Sz, T, Acc);
material(<<16#A200:16/little,T/binary>>, Acc) ->
    read_map(T, diffuse, Acc);
material(<<16#A210:16/little,T/binary>>, Acc) ->
    read_map(T, opacity, Acc);
material(<<16#A230:16/little,T/binary>>, Acc) ->
    read_map(T, bump, Acc);
material(<<Tag:16/little,Sz0:32/little,T0/binary>>, [{Name,Props}|Acc]) ->
    dbg("Unknown material tag ~s\n", [hex4(Tag)]),
    Sz = Sz0 - 6,
    <<Chunk:Sz/binary,T/binary>> = T0,
    material(T, [{Name,[{Tag,Chunk}|Props]}|Acc]);
material(<<>>, Acc) -> Acc.

read_map(<<Sz0:32/little,T0/binary>>, Type, [{Name,Props}|Acc]) ->
    dbg("Map: ~p\n", [Type]),
    Sz = Sz0 - 6,
    <<Chunk:Sz/binary,T/binary>> = T0,
    Map = read_map_chunks(Chunk, none),
    material(T, [{Name,[{map,Type,Map}|Props]}|Acc]).

read_map_chunks(<<16#A300:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    Sz = Sz0 - 6 - 1,
    <<Filename:Sz/binary,_:8,T/binary>> = T0,
    read_map_chunks(T, binary_to_list(Filename));
read_map_chunks(<<Tag:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    dbg("Unknown map sub-chunk: ~s\n", [hex4(Tag)]),
    Sz = Sz0 - 6,
    <<_:Sz/binary,T/binary>> = T0,
    read_map_chunks(T, Acc);
read_map_chunks(<<>>, Acc) -> Acc.

reformat_material([{Name,Mat}|T]) ->
    Opac = proplists:get_value(opacity, Mat, 1.0),
    [{Name,reformat_mat(Mat, Opac, [], [], [])}|reformat_material(T)];
reformat_material([]) -> [].

reformat_mat([{diffuse,_}=Col0|T], Opac, Ogl, Maps, Tds) ->
    Col = reformat_color(Col0, Opac),
    reformat_mat(T, Opac, [Col|Ogl], Maps, Tds);
reformat_mat([{ambient,_}=Col0|T], Opac, Ogl, Maps, Tds) ->
    Col = reformat_color(Col0, Opac),
    reformat_mat(T, Opac, [Col|Ogl], Maps, Tds);
reformat_mat([{specular,_}=Col0|T], Opac, Ogl, Maps, Tds) ->
    Col = reformat_color(Col0, Opac),
    reformat_mat(T, Opac, [Col|Ogl], Maps, Tds);
reformat_mat([{emission,_}=Col0|T], Opac, Ogl, Maps, Tds) ->
    Col = reformat_color(Col0, Opac),
    reformat_mat(T, Opac, [Col|Ogl], Maps, Tds);
reformat_mat([{shininess,Sh}|T], Opac, Ogl, Maps, Tds) ->
    reformat_mat(T, Opac, [{shininess,1.0-Sh}|Ogl], Maps, Tds);
reformat_mat([{opacity,_}|T], Opac, Ogl, Maps, Tds) ->
    reformat_mat(T, Opac, Ogl, Maps, Tds);
reformat_mat([{map,Name,Data}|T], Opac, Ogl, Maps, Tds) ->
    reformat_mat(T, Opac, Ogl, [{Name,Data}|Maps], Tds);
reformat_mat([Other|T], Opac, Ogl, Maps, Tds) ->
    reformat_mat(T, Opac, Ogl, Maps, [Other|Tds]);
reformat_mat([], _Opac, Ogl, Maps, Tds) ->
    [{opengl,Ogl},{maps,Maps},{tds,Tds}].

reformat_color({Key,{R,G,B}}, Opac) ->
    {Key,{R,G,B,Opac}}.
    
mat_chunk(Type, Sz0, Bin, [{Name,Props}|Acc]) ->
    Sz = Sz0 - 6,
    <<Chunk:Sz/binary,T/binary>> = Bin,
    Value = general(Chunk),
    dbg("Material ~p, property ~p = ~p\n", [Name,Type,Value]),
    material(T, [{Name,[{Type,Value}|Props]}|Acc]).

general(<<16#0010:16/little,_Sz:32/little, 
	 R:32/float-little,G:32/float-little,B:32/float-little,
	 T/binary>>) ->
    general_rest(T),
    {R,G,B};
general(<<16#0011:16/little,_Sz:32/little,R:8,G:8,B:8,T/binary>>) ->
    general_rest(T),
    {R/255,G/255,B/255};
general(<<16#0030:16/little,_Sz:32/little,Percent:16/little>>) ->
    Percent/100.

general_rest(<<>>) -> ok;
general_rest(Bin) ->
    io:format("general chunk tail not empty: ~p\n", [Bin]).
    
%%%
%%% Utilities.
%%%

insert_mat([_|_]=Fs, [MatFace|[MatFace|_]=Mfs], Face, Mat, Acc) ->
    insert_mat(Fs, Mfs, Face, Mat, Acc);
insert_mat([F|Fs], [MatFace|_]=Mfs, Face, Mat, Acc) when Face < MatFace ->
    insert_mat(Fs, Mfs, Face+1, Mat, [F|Acc]);
insert_mat([#e3d_face{mat=Mat0}=F|Fs], [Face|Mfs], Face, Mat, Acc) ->
    insert_mat(Fs, Mfs, Face+1, Mat, [F#e3d_face{mat=[Mat|Mat0]}|Acc]);
insert_mat([], _, _Face, _Mat, Acc) -> reverse(Acc);
insert_mat(Rest, [], _Face, _Mat, Acc) -> reverse(Acc, Rest).

get_mat_faces(<<N:16/little,T/binary>>) ->
    dbg("Mat num entries: ~p\n", [N]),
    get_mat_faces(T, []).

get_mat_faces(<<Face:16/little,T/binary>>, Acc) ->
    get_mat_faces(T, [Face|Acc]);
get_mat_faces(<<>>, Acc) -> reverse(Acc).

get_uv(Bin) ->
    get_uv(Bin, []).
get_uv(<<U:32/float-little,V:32/float-little,T/binary>>, Acc) ->
    get_uv(T, [{U,V}|Acc]);
get_uv(<<>>, Acc) -> reverse(Acc).

get_bin_vectors(Bin) ->
    get_bin_vectors(Bin, []).
get_bin_vectors(<<Pos:12/binary,T/binary>>, Acc) ->
    get_bin_vectors(T, [Pos|Acc]);
get_bin_vectors(<<>>, Acc) -> reverse(Acc).

get_faces(Bin) ->
    get_faces(Bin, []).
get_faces(<<A:16/little,B:16/little,C:16/little,Flag:16/little,T/binary>>, Acc) ->
    get_faces(T, [#e3d_face{vs=[A,B,C],vis=Flag band 7}|Acc]);
get_faces(<<>>, Acc) -> reverse(Acc).

get_smooth_groups(Bin) ->
    get_smooth_groups(Bin, []).
get_smooth_groups(<<SG:32/little,T/binary>>, Acc) ->
    get_smooth_groups(T, [SG|Acc]);
get_smooth_groups(<<>>, Acc) -> reverse(Acc).

get_cstring(Bin) ->
    get_cstring(Bin, []).
get_cstring(<<0:8,T/binary>>, Str) ->
    {reverse(Str),T};
get_cstring(<<C:8,T/binary>>, Str) ->
    get_cstring(T, [C|Str]);
get_cstring(<<>>=T, Str) ->
    {reverse(Str),T}.
    
error(Message) ->
    throw({error,Message}).

hard_edges(SmoothGroups, Faces) ->
    hard_edges(SmoothGroups, Faces, []).

hard_edges([SG|SGs], [#e3d_face{vs=[A,B,C]}|Fs], Acc0) ->
    Acc = [edge(A, B, SG),edge(B, C, SG),edge(C, A, SG)|Acc0],
    hard_edges(SGs, Fs, Acc);
hard_edges([], _, Acc) ->
    R = sofs:relation(Acc),
    F0 = sofs:relation_to_family(R),
    F = sofs:to_external(F0),
    foldl(fun({Edge,[SG|SGs]}, He) ->
		  case foldl(fun(SG, A) -> A band SG end, SG, SGs) of
		      0 -> [Edge|He];
		      _Other -> He
		  end
	  end, [], F).

edge(A, B, SG) when A < B -> {{A,B},SG};
edge(A, B, SG) -> {{B,A},SG}.

hex4(Num) ->
    hex(4, Num, []).

hex(0, _Num, Acc) -> Acc;
hex(N, Num, Acc) -> hex(N-1, Num div 16, [hexd(Num rem 16)|Acc]).

hexd(D) when 0 =< D, D =< 9 -> D+$0;
hexd(D) when 10 =< D, D =< 16 -> D+$A-10.

add_uv_to_faces(#e3d_mesh{tx=[]}=Mesh) -> Mesh;
add_uv_to_faces(#e3d_mesh{fs=Fs0}=Mesh) ->
    Fs = foldl(fun(#e3d_face{vs=Vs}=Face, A) ->
		       [Face#e3d_face{tx=Vs}|A]
	       end, [], Fs0),
    Mesh#e3d_mesh{fs=reverse(Fs)}.

clean_mesh(#e3d_mesh{fs=Fs0,vs=Vs0,tx=Tx}=Mesh0) ->
    %% Here we combines vertices that have exactly the same position
    %% and renumber vertices to leave no gaps.
    R = sofs:relation(append_index(Vs0), [{pos,old_vertex}]),
    S = sofs:range(sofs:relation_to_family(R)),
    CR = sofs:canonical_relation(S),
    Map = gb_trees:from_orddict(sofs:to_external(CR)),
    Fs = map_faces(Fs0, Map),
    #e3d_mesh{vs=Vs1} = Mesh = e3d_mesh:renumber(Mesh0#e3d_mesh{fs=Fs}),
    Vs = [{X,Y,Z} || <<X:32/?FLOAT,Y:32/?FLOAT,Z:32/?FLOAT>> <- Vs1],
    Mesh#e3d_mesh{vs=Vs}.

append_index(L) -> append_index(L, 0, []).
append_index([H|T], I, Acc) -> append_index(T, I+1, [{H,I}|Acc]);
append_index([], _I, Acc) -> Acc.

map_faces(Fs, Map) ->
    map_faces(Fs, Map, []).
map_faces([#e3d_face{vs=Vs0}=Face|Fs], Map, Acc) ->
    Vs = [begin [V|_] = gb_trees:get(V0, Map), V end || V0 <- Vs0],
    map_faces(Fs, Map, [Face#e3d_face{vs=Vs}|Acc]);
map_faces([], _Map, Acc) -> reverse(Acc).

%%dbg(_, _) -> ok;
dbg(Format, List) -> io:format(Format, List).

%%%
%%% Export.
%%% 

export(Name, Objs) ->
    Version = make_chunk(16#0002, <<3:32/little>>),
    Editor = make_editor(Objs),
    Main = make_chunk(16#4D4D, [Version|Editor]),
    ok = file:write_file(Name, Main).

make_editor(#e3d_file{objs=Objs0,mat=Mat0}) ->
    MeshVer = make_chunk(16#3d3e, <<3:32/little>>),
    Unit = make_chunk(16#0100, <<(1.0):32/float-little>>),
    Objs = make_objects(Objs0),
    Mat = make_material(Mat0),
    make_chunk(16#3D3D, [MeshVer,Mat,Unit,Objs]).

make_objects([#e3d_object{name=Name0,obj=Mesh0}|Objs]) ->
    Name = lists:sublist(Name0, 1, 10),
    Mesh = e3d_mesh:triangulate(Mesh0),
    MeshChunk = make_mesh(Mesh),
    Chunk = make_chunk(16#4000, [Name,0,MeshChunk]),
    [Chunk|make_objects(Objs)];
make_objects([]) -> [].

make_mesh(#e3d_mesh{vs=Vs,fs=Fs,he=He,matrix=Matrix0}) ->
    %% XXX Matrix0 should be used here.
    VsChunk = make_vertices(Vs),
    FsChunk = make_faces(Fs, He),
    MD = <<1.0:32/float-little,0.0:32/float-little,0.0:32/float-little,
	  0.0:32/float-little,1.0:32/float-little,0.0:32/float-little,
	  0.0:32/float-little,0.0:32/float-little,1.0:32/float-little,
	  0.0:32/float-little,0.0:32/float-little,0.0:32/float-little>>,
    Matrix = make_chunk(16#4160, MD),
    make_chunk(16#4100, [Matrix,VsChunk,FsChunk]).

make_vertices(Vs) ->
    Chunk = [<<(length(Vs)):16/little>>|make_vertices(Vs, [])],
    make_chunk(16#4110, Chunk).
    
make_vertices([{X,Y,Z}|Ps], Acc) ->
    Chunk = <<X:32/float-little,Y:32/float-little,Z:32/float-little>>,
    make_vertices(Ps, [Chunk|Acc]);
make_vertices([], Acc) -> reverse(Acc).

make_faces(Fs, He) ->
    FaceChunk = [<<(length(Fs)):16/little>>|make_faces_1(Fs, [])],
    MatChunk = make_face_mat(Fs),
    SmoothChunk = make_smooth_groups(Fs, He),
    make_chunk(16#4120, [FaceChunk,MatChunk,SmoothChunk]).

make_faces_1([#e3d_face{vs=[A,B,C],vis=Hidden}|Faces], Acc) ->
    Flag = Hidden band 7,
    Face = <<A:16/little,B:16/little,C:16/little,Flag:16/little>>,
    make_faces_1(Faces, [Face|Acc]);
make_faces_1([], Acc) -> reverse(Acc).

make_face_mat(Fs) ->
    R0 = make_face_mat_1(Fs, 0, []),
    R = sofs:relation(R0),
    F = sofs:to_external(sofs:relation_to_family(R)),
    map(fun({Name0,Faces}) ->
		Name = atom_to_list(Name0),
		Chunk = [Name,0,
			 <<(length(Faces)):16/little>>,
			 [<<Face:16/little>> || Face <- Faces]],
		make_chunk(16#4130, Chunk)
	end, F).

make_face_mat_1([#e3d_face{mat=Mat}|Fs], Face, Acc) ->
    make_face_mat_1(Fs, Face+1, [{M,Face} || M <- Mat] ++ Acc);
make_face_mat_1([], _Face, Acc) -> Acc.

make_smooth_groups(Fs, He) ->
    %% XXX We should use He to make smooth groups.
    Contents = lists:duplicate(length(Fs), <<1:32/little>>),
    make_chunk(16#4150, Contents).

make_material(Mat) ->
    io:format("~p\n", [Mat]),
    [make_material_1(M) || M <- Mat].

make_material_1({Name,Mat}) ->
    NameChunk = make_chunk(16#A000, [atom_to_list(Name),0]),
    OpenGL = proplists:get_value(opengl, Mat),
    MatChunks = make_material_2(OpenGL, []),
    make_chunk(16#AFFF, [NameChunk|MatChunks]).

make_material_2([{diffuse,{_,_,_,Opac0}=Color}|T], Acc) ->
    Chunk = make_chunk(16#A020, make_rgb(Color)),
    Opacity = make_chunk(16#A050, make_percent(1-Opac0)),
    make_material_2(T, [Chunk,Opacity|Acc]);
make_material_2([{ambient,RGB}|T], Acc) ->
    Chunk = make_chunk(16#A010, make_rgb(RGB)),
    make_material_2(T, [Chunk|Acc]);
make_material_2([{specular,RGB}|T], Acc) ->
    Chunk = make_chunk(16#A030, make_rgb(RGB)),
    make_material_2(T, [Chunk|Acc]);
make_material_2([{shininess,Percent}|T], Acc) ->
    Chunk = make_chunk(16#A040, make_percent(1.0-Percent)),
    make_material_2(T, [Chunk|Acc]);
make_material_2([_|T], Acc) ->
    make_material_2(T, Acc);
make_material_2([], Acc) -> Acc.

make_rgb({R0,G0,B0,_}) when float(R0), float(G0), float(B0) ->
    make_chunk(16#0010, <<R0:32/float-little,G0:32/float-little,
 			 B0:32/float-little>>);
make_rgb({R,G,B,_}) when integer(R), integer(G), integer(B) ->
    make_chunk(16#0011, <<R:8/little,G:8/little,B:8/little>>).

make_percent(0) ->
    make_percent(0.0);
make_percent(Percent0) when float(Percent0) ->
    Percent = trunc(Percent0*100.0),
    make_chunk(16#0030, <<Percent:16/little>>).
    
make_chunk(Tag, Contents) when binary(Contents) ->
    Size = size(Contents) + 6,
    [<<Tag:16/little,Size:32/little>>|Contents];
make_chunk(Tag, Contents) when list(Contents) ->
    make_chunk(Tag, list_to_binary(Contents)).
