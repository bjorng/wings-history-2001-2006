%%
%%  e3d_tds.erl --
%%
%%     Functions for reading and writing 3D Studio Max files (.tds),
%%     version 3.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_tds.erl,v 1.32 2004/01/11 17:58:23 bjorng Exp $
%%

-module(e3d_tds).
-export([import/1,export/2]).

-include("e3d.hrl").
-include("e3d_image.hrl").

-import(lists, [map/2,foldl/3,mapfoldl/3,reverse/1,reverse/2,
		sort/1,keysort/2,usort/1,keydelete/3,keyreplace/4]).

%% R9C required is required if debugging is turned on,
%% sinced the debug printouts use the new hex formatting characters.

%% -define(DEBUG, 1).

%% Inline dbg/2 so that the call will disappear completely if
%% DEBUG is turned off.
-compile({inline,[{dbg,2}]}).

-define(FLOAT, float-little).

-ifdef(DEBUG).
dbg(Format, List) -> io:format(Format, List).
-else.
dbg(_, _) -> ok.
-endif.


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
main(<<Tag:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    dbg("Ignoring unknown chunk ~.16#; ~p bytes\n", [Tag,Sz0]),
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
    dbg("\nObject block: ~s\n", [Name]),
    case block(Obj1) of
	no_mesh -> editor(T, Acc);
	Obj ->
	    Objs = [#e3d_object{name=Name,obj=Obj}|Objs0],
	    editor(T, Acc#e3d_file{objs=Objs})
    end;
editor(<<16#3d3e:16/little,_Sz:32/little,Ver:32/little,T/binary>>, Acc) ->
    dbg("Mesh Version ~p ~n", [Ver]),
    editor(T, Acc);
editor(<<16#0100:16/little,_Sz:32/little,Scale:32/?FLOAT,T/binary>>,
       Acc) ->
    dbg("Object Scale ~p ~n", [Scale]),
    editor(T, Acc);
editor(<<16#AFFF:16/little,Sz0:32/little,T0/binary>>, #e3d_file{mat=M}=Acc) ->
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
block(<<Tag:16/little,Sz0:32/little,T0/binary>>, Mesh) ->
    dbg("Ignoring unknown chunk ~.16#; ~p bytes\n", [Tag,Sz0]),
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
    trimesh(T, Acc#e3d_mesh{fs=Faces,he=Smooth});
trimesh(<<16#4140:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    dbg("Texture coordinates ~w\n", [Sz0]),
    Sz = Sz0 - 8,
    <<_:16,Tx0:Sz/binary,T/binary>> = T0,
    Tx = get_uv(Tx0),
    trimesh(T, Acc#e3d_mesh{tx=Tx});
trimesh(<<16#4160:16/little,_Sz:32/little,T0/binary>>, Acc) ->
    <<_V1X:32/?FLOAT,_V1Y:32/?FLOAT,_V1Z:32/?FLOAT,
     _V2X:32/?FLOAT,_V2Y:32/?FLOAT,_V2Z:32/?FLOAT,
     _V3X:32/?FLOAT,_V3Y:32/?FLOAT,_V3Z:32/?FLOAT,
     OX:32/?FLOAT,OY:32/?FLOAT,OZ:32/?FLOAT,
     T/binary>> = T0,
    _CS = {1.0,0.0,0.0,0.0,
	   0.0,1.0,0.0,0.0,
	   0.0,0.0,1.0,0.0,
	   OX,OY,OZ,1.0},
    %%dbg("Local:~p\n", [CS]),
    %%trimesh(T, Acc#e3d_mesh{matrix=Id});
    %% Ignore local coordinate system.
    trimesh(T, Acc);
trimesh(<<Tag:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    Sz = Sz0 - 6,
    <<Chunk:Sz/binary,T/binary>> = T0,
    dbg("Unknown mesh chunk: ~.16#: ~P\n", [Tag,Chunk,20]),
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
    dbg("\nMaterial: ~p\n", [Name]),
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
material(<<16#A087:16/little,10:32/little,Wiresize:32/?FLOAT,T/binary>>, Acc) ->
    dbg("Wire size: ~p\n", [Wiresize]),
    material(T, Acc);
material(<<16#A100:16/little,8:32/little,Shading:16/little,T/binary>>, Acc) ->
     Str = case Shading of
	       0 -> "Wire";
	       1 -> "Flat";
	       2 -> "Gouraud";
	       3 -> "Phong";
	       4 -> "Metal";
	       _ -> "Unknown"
	   end,
    dbg("Material shading: ~s\n", [Str]),
    material(T, Acc);
material(<<16#A200:16/little,T/binary>>, Acc) ->
    read_map(T, diffuse, Acc);
material(<<16#A210:16/little,T/binary>>, Acc) ->
    read_map(T, opacity, Acc);
material(<<16#A230:16/little,T/binary>>, Acc) ->
    read_map(T, bump, Acc);
material(<<Tag:16/little,Sz0:32/little,T0/binary>>, [{Name,Props}|Acc]) ->
    Sz = Sz0 - 6,
    <<Chunk:Sz/binary,T/binary>> = T0,
    dbg("Unknown material tag: ~.16#: ~P\n", [Tag,Chunk,20]),
    material(T, [{Name,[{Tag,Chunk}|Props]}|Acc]);
material(<<>>, Acc) -> Acc.

read_map(<<Sz0:32/little,T0/binary>>, Type, [{Name,Props}|Acc]) ->
    dbg("Map: ~p\n", [Type]),
    Sz = Sz0 - 6,
    <<Chunk:Sz/binary,T/binary>> = T0,
    Map = read_map_chunks(Chunk, none),
    material(T, [{Name,[{map,Type,Map}|Props]}|Acc]).

read_map_chunks(<<16#A300:16/little,Sz0:32/little,T0/binary>>, _Acc) ->
    Sz = Sz0 - 6 - 1,
    <<Filename0:Sz/binary,_:8,T/binary>> = T0,
    Filename = binary_to_list(Filename0),
    dbg("Filename: ~s\n", [Filename]),
    read_map_chunks(T, Filename);
read_map_chunks(<<16#A351:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    Sz = Sz0 - 6,
    <<Params:Sz/binary,T/binary>> = T0,
    dbg("Params: ~p\n", [Params]),
    read_map_chunks(T, Acc);
read_map_chunks(<<Tag:16/little,Sz0:32/little,T0/binary>>, Acc) ->
    Sz = Sz0 - 6,
    <<_Chunk:Sz/binary,T/binary>> = T0,
    dbg("Unknown map sub-chunk: ~.16E: ~P\n", [Tag,_Chunk,20]),
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
    dbg("property ~p = ~p\n", [Type,Value]),
    material(T, [{Name,[{Type,Value}|Props]}|Acc]).

general(<<16#0010:16/little,_Sz:32/little, 
	 R:32/?FLOAT,G:32/?FLOAT,B:32/?FLOAT,
	 T/binary>>) ->
    general_rest(T),
    {R,G,B};
general(<<16#0011:16/little,_Sz:32/little,R:8,G:8,B:8,T/binary>>) ->
    general_rest(T),
    {R/255,G/255,B/255};
general(<<16#0030:16/little,_Sz:32/little,Percent:16/little>>) ->
    Percent/100.

general_rest(<<>>) -> ok;
general_rest(<<Tag:16/little,Sz0:32/little,T0/binary>>) ->
    Sz = Sz0 - 6,
    <<Chunk:Sz/binary,T/binary>> = T0,
    dbg("Unknown general tag: ~.16#: ~P\n", [Tag,Chunk,20]),
    general_rest(T).

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
get_uv(<<U:32/?FLOAT,V:32/?FLOAT,T/binary>>, Acc) ->
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
    foldl(fun({Edge,[SG0|SGs]}, He) ->
		  case foldl(fun(SG, A) -> A band SG end, SG0, SGs) of
		      0 -> [Edge|He];
		      _Other -> He
		  end
	  end, [], F).

edge(A, B, SG) when A < B -> {{A,B},SG};
edge(A, B, SG) -> {{B,A},SG}.

add_uv_to_faces(#e3d_mesh{tx=[]}=Mesh) -> Mesh;
add_uv_to_faces(#e3d_mesh{fs=Fs0}=Mesh) ->
    Fs = foldl(fun(#e3d_face{vs=Vs}=Face, A) ->
		       [Face#e3d_face{tx=Vs}|A]
	       end, [], Fs0),
    Mesh#e3d_mesh{fs=reverse(Fs)}.

clean_mesh(#e3d_mesh{fs=Fs0,vs=Vs0,he=Smooth}=Mesh0) ->
    %% Here we combines vertices that have exactly the same position
    %% and renumber vertices to leave no gaps.
    R = sofs:relation(append_index(Vs0), [{pos,old_vertex}]),
    S = sofs:range(sofs:relation_to_family(R)),
    CR = sofs:canonical_relation(S),
    Map = gb_trees:from_orddict(sofs:to_external(CR)),
    Fs1 = map_faces(Fs0, Map),
    #e3d_mesh{vs=Vs1,fs=Fs} = Mesh =
	e3d_mesh:renumber(Mesh0#e3d_mesh{fs=Fs1,he=[]}),
    He = hard_edges(Smooth, Fs),
    Vs = [{X,Y,Z} || <<X:32/?FLOAT,Y:32/?FLOAT,Z:32/?FLOAT>> <- Vs1],
    Mesh#e3d_mesh{vs=Vs,he=He}.

append_index(L) -> append_index(L, 0, []).
append_index([H|T], I, Acc) -> append_index(T, I+1, [{H,I}|Acc]);
append_index([], _I, Acc) -> Acc.

map_faces(Fs, Map) ->
    map_faces(Fs, Map, []).
map_faces([#e3d_face{vs=Vs0}=Face|Fs], Map, Acc) ->
    Vs = [begin [V|_] = gb_trees:get(V0, Map), V end || V0 <- Vs0],
    map_faces(Fs, Map, [Face#e3d_face{vs=Vs}|Acc]);
map_faces([], _Map, Acc) -> reverse(Acc).

%%%
%%% Export.
%%% 

export(Name, Objs) ->
    Version = make_chunk(16#0002, <<3:32/little>>),
    Editor = make_editor(Name, Objs),
    Main = make_chunk(16#4D4D, [Version|Editor]),
    case file:write_file(Name, Main) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

make_editor(Name, #e3d_file{objs=Objs0,mat=Mat0}) ->
    MeshVer = make_chunk(16#3d3e, <<3:32/little>>),
    Unit = make_chunk(16#0100, <<(1.0):32/?FLOAT>>),
    {Mat1,MatMap} = make_materials_uniq(Mat0),
    Objs1 = make_names_uniq(Objs0),
    Objs = make_objects(Objs1, MatMap),
    Mat2 = make_tx_uniq(Mat1),
    Mat = make_material(Name, Mat2),
    make_chunk(16#3D3D, [MeshVer,Mat,Unit,Objs]).

make_objects([#e3d_object{name=Name,obj=Mesh0}|Objs], MatMap) ->
    Mesh = e3d_mesh:triangulate(Mesh0),
    MeshChunk = make_mesh(Mesh, MatMap),
    Chunk = make_chunk(16#4000, [Name,0,MeshChunk]),
    [Chunk|make_objects(Objs, MatMap)];
make_objects([], _) -> [].

make_names_uniq(Objs) ->
    Names = [Name || #e3d_object{name=Name} <- Objs],
    Map0 = e3d_util:make_uniq(Names, 10),
    Map = gb_trees:from_orddict(sort(Map0)),
    [Obj#e3d_object{name=gb_trees:get(Name, Map)} ||
	#e3d_object{name=Name}=Obj <- Objs].

make_tx_uniq(Mat) ->
    Names = foldl(fun({_,Ps}, A) ->
			  case get_map(diffuse, Ps) of
			      none -> A;
			      #e3d_image{name=Name} -> [Name|A]
			  end
		  end, [], Mat),
    MapTrans0 = e3d_util:make_uniq(Names, 8),
    MapTrans = gb_trees:from_orddict(sort(MapTrans0)),
    map(fun({N,Ps0}=M) ->
		case get_map(diffuse, Ps0) of
		    none -> M;
		    #e3d_image{name=Name0}=Image0 ->
			Name = gb_trees:get(Name0, MapTrans),
			Image = Image0#e3d_image{name=Name},
			Ps = replace_map(diffuse, Image, Ps0),
			{N,Ps}
		end
	end, Mat).

get_map(Type, Ps) ->
    Maps = proplists:get_value(maps, Ps, []),
    proplists:get_value(Type, Maps, none).

replace_map(MapType, Val, Ps) ->
    Maps0 = proplists:get_value(maps, Ps, []),
    Maps = [{MapType,Val}|keydelete(MapType, 1, Maps0)],
    keyreplace(maps, 1, Ps, {maps,Maps}).

make_mesh(Mesh0, MatMap) ->
    Mesh = split_vertices(Mesh0),
    make_mesh_1(Mesh, MatMap).

make_mesh_1(#e3d_mesh{vs=Vs,fs=Fs,he=He,tx=Tx,matrix=_Matrix0}, MatMap) ->
    %% XXX Matrix0 should be used here.
    VsChunk = make_vertices(Vs),
    FsChunk = make_faces(Fs, He, MatMap),
    MD = <<1.0:32/?FLOAT,0.0:32/?FLOAT,0.0:32/?FLOAT,
	  0.0:32/?FLOAT,1.0:32/?FLOAT,0.0:32/?FLOAT,
	  0.0:32/?FLOAT,0.0:32/?FLOAT,1.0:32/?FLOAT,
	  0.0:32/?FLOAT,0.0:32/?FLOAT,0.0:32/?FLOAT>>,
    Matrix = make_chunk(16#4160, MD),
    UVs = make_uvs(Tx),
    make_chunk(16#4100, [Matrix,VsChunk,UVs|FsChunk]).

make_vertices(Vs) ->
    Chunk = [<<(length(Vs)):16/little>>|make_vertices(Vs, [])],
    make_chunk(16#4110, Chunk).
    
make_vertices([{X,Y,Z}|Ps], Acc) ->
    Chunk = <<X:32/?FLOAT,Y:32/?FLOAT,Z:32/?FLOAT>>,
    make_vertices(Ps, [Chunk|Acc]);
make_vertices([], Acc) -> reverse(Acc).

make_uvs([]) -> [];
make_uvs(UVs) ->
    Chunk = [<<(length(UVs)):16/little>>|make_uvs(UVs, [])],
    make_chunk(16#4140, Chunk).
    
make_uvs([{U,V}|Ps], Acc) ->
    Chunk = <<U:32/?FLOAT,V:32/?FLOAT>>,
    make_uvs(Ps, [Chunk|Acc]);
make_uvs([], Acc) -> reverse(Acc).

make_faces(Fs, He, MatMap) ->
    FaceChunk = [<<(length(Fs)):16/little>>|make_faces_1(Fs, [])],
    MatChunk = make_face_mat(Fs, MatMap),
    SmoothChunk = make_smooth_groups(Fs, He),
    make_chunk(16#4120, [FaceChunk,MatChunk,SmoothChunk]).

make_faces_1([#e3d_face{vs=[A,B,C],vis=Hidden}|Faces], Acc) ->
    Flag = Hidden band 7,
    Face = <<A:16/little,B:16/little,C:16/little,Flag:16/little>>,
    make_faces_1(Faces, [Face|Acc]);
make_faces_1([], Acc) -> reverse(Acc).

make_face_mat(Fs, MatMap) ->
    R0 = make_face_mat_1(Fs, 0, []),
    R = sofs:relation(R0),
    F = sofs:to_external(sofs:relation_to_family(R)),
    map(fun({Name0,Faces}) ->
		Name1 = atom_to_list(Name0),
		Name = gb_trees:get(Name1, MatMap),
		Chunk = [Name,0,
			 <<(length(Faces)):16/little>>,
			 [<<Face:16/little>> || Face <- Faces]],
		make_chunk(16#4130, Chunk)
	end, F).

make_face_mat_1([#e3d_face{mat=Mat}|Fs], Face, Acc) ->
    make_face_mat_1(Fs, Face+1, [{M,Face} || M <- Mat] ++ Acc);
make_face_mat_1([], _Face, Acc) -> Acc.

make_materials_uniq(Mat0) ->
    Names = [atom_to_list(Name) || {Name,_} <- Mat0],
    Map0 = e3d_util:make_uniq(Names, 16),
    Map = gb_trees:from_orddict(sort(Map0)),
    Mat = [{list_to_atom(gb_trees:get(atom_to_list(Name), Map)),Ps} ||
	      {Name,Ps} <- Mat0],
    {Mat,Map}.

make_material(Filename, Mat) ->
    Base = filename:rootname(Filename, ".3ds"),
    [make_material_1(Base, M) || M <- Mat].

make_material_1(Base, {Name,Mat}) ->
    NameChunk = make_chunk(16#A000, [atom_to_list(Name),0]),
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat),
    MatChunks = make_material_2(OpenGL, []),
    TxChunks = make_texture_materials(Maps, Base, []),
    MatShading = make_chunk(16#A100, <<3:16/little>>), %Phong shading.
    make_chunk(16#AFFF, [NameChunk,MatChunks,MatShading|TxChunks]).

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

make_texture_materials([{diffuse,Map}|T], Base, Acc) ->
    Tx = export_map(16#A200, Map, Base),
    make_texture_materials(T, Base, [Tx|Acc]);
make_texture_materials([_|T], Base, Acc) ->
    make_texture_materials(T, Base, Acc);
make_texture_materials([], _, Acc) -> Acc.

export_map(_, none, _) -> ok;
export_map(ChunkId, #e3d_image{filename=none,name=Name}=Image, Root) ->
    MapFile = filename:join(filename:dirname(Root), Name ++ ".bmp"),
    ok = e3d_image:save(Image, MapFile),
    export_map(ChunkId, Image#e3d_image{filename=MapFile}, Root);
export_map(ChunkId, #e3d_image{filename=MapFile}, _Root) ->
    FnameChunk = make_chunk(16#A300, [filename:basename(MapFile),0]),
    ParamChunk = make_chunk(16#A351, [0,1]),
    make_chunk(ChunkId, [FnameChunk,ParamChunk]).

make_rgb({R0,G0,B0,_}) when is_float(R0), is_float(G0), is_float(B0) ->
    [make_chunk(16#0011, <<(trunc(255*R0)):8,(trunc(255*G0)):8,
			  (trunc(255*B0)):8>>),
     make_chunk(16#0012, <<(trunc(255*R0)):8,(trunc(255*G0)):8,
			  (trunc(255*B0)):8>>)].

make_percent(0) ->
    make_percent(0.0);
make_percent(Percent0) when is_float(Percent0) ->
    Percent = trunc(Percent0*100.0),
    make_chunk(16#0030, <<Percent:16/little>>).
    
make_chunk(Tag, Contents) when is_binary(Contents) ->
    Size = size(Contents) + 6,
    [<<Tag:16/little,Size:32/little>>|Contents];
make_chunk(Tag, Contents) when list(Contents) ->
    make_chunk(Tag, list_to_binary(Contents)).

%%%
%%% Create smoothing groups from the hard egdes.
%%%

make_smooth_groups(Fs, []) ->
    Contents = lists:duplicate(length(Fs), <<1:32/little>>),
    make_chunk(16#4150, Contents);
make_smooth_groups(Fs, He) ->
    Es = msg_edges(Fs, 0, []),
    R0 = sofs:relation(Es),
    ConvR = sofs:converse(R0),
    {Ws0,Rs0} = sofs:partition(2, R0, sofs:set(He)),
    Ws1 = sofs:relative_product(Ws0, ConvR),
    Ws2 = sofs:relation_to_family(Ws1),
    Ws = sofs:to_external(Ws2),
    Rs1 = sofs:restriction(Rs0, sofs:domain(Ws2)),
    Rs2 = sofs:relative_product(Rs1, ConvR),
    Rs3 = sofs:relation_to_family(Rs2),
    Rs = sofs:to_external(Rs3),
    Groups1 = msg_assign_hardness(Ws, gb_trees:empty()),
    Groups2 = msg_assign_softness(Rs, gb_trees:from_orddict(Ws), Groups1),
    Groups3 = gb_trees:to_list(Groups2),
    AllGroupsUsed = msg_all_groups_used(Groups3, 0),
    Groups4 = sofs:from_external(Groups3, [{atom,atom}]),
    Groups5 = sofs:extension(Groups4, sofs:set(lists:seq(0, length(Fs)-1)),
			     sofs:from_term(AllGroupsUsed)),
    Groups = sofs:to_external(Groups5),
    msg_chunk(Groups, []).

msg_all_groups_used([{_,G}|T], Acc) ->
    msg_all_groups_used(T, G bor Acc);
msg_all_groups_used([], Acc) -> Acc.
    
msg_chunk([{_,SG}|T], Acc) -> msg_chunk(T, [<<SG:32/little>>|Acc]);
msg_chunk([], Acc) -> make_chunk(16#4150, reverse(Acc)).

msg_edge(A, B) when A < B -> {A,B};
msg_edge(A, B) -> {B,A}.

msg_edges([#e3d_face{vs=[A,B,C]}|T], Face, Acc) ->
    E1 = msg_edge(A, B),
    E2 = msg_edge(B, C),
    E3 = msg_edge(C, A),
    msg_edges(T, Face+1, [{Face,E1},{Face,E2},{Face,E3}|Acc]);
msg_edges([], _, Acc) -> Acc.

msg_assign_hardness([{Face,HardFs}|T], Groups0) ->
    Groups = msg_assign_hardness_1(Face, HardFs, Groups0, 0),
    msg_assign_hardness(T, Groups);
msg_assign_hardness([], Groups) -> Groups.

msg_assign_hardness_1(Face, [Face|Fs], Groups, Acc) ->
    msg_assign_hardness_1(Face, Fs, Groups, Acc);
msg_assign_hardness_1(Face, [AFace|Fs], Groups, Acc) ->
    case gb_trees:lookup(AFace, Groups) of
	none -> msg_assign_hardness_1(Face, Fs, Groups, Acc);
	{value,G} -> msg_assign_hardness_1(Face, Fs, Groups, Acc bor G)
    end;
msg_assign_hardness_1(Face, [], Groups, NotAllowed) ->
    G = msg_find_group(NotAllowed),
    gb_trees:insert(Face, G, Groups).

msg_assign_softness([{Face,SoftFs}|T], HardTab, Groups0) ->
    Groups = msg_assign_softness_1(Face, SoftFs, HardTab, Groups0),
    msg_assign_softness(T, HardTab, Groups);
msg_assign_softness([], _, Groups) -> Groups.

msg_assign_softness_1(Face, [Face|Fs], HardTab, Groups) ->
    msg_assign_softness_1(Face, Fs, HardTab, Groups);
msg_assign_softness_1(Face, [AFace|Fs], HardTab, Groups0) ->
    case gb_trees:lookup(AFace, Groups0) of
	none ->					%Will get default value.
	    msg_assign_softness_1(Face, Fs, HardTab, Groups0);
	{value,Ga} ->
	    case gb_trees:get(Face, Groups0) of
		G when G band Ga =:= 0 ->
		    Groups = msg_assign_softness_2(Face, AFace, G, Ga,
						   HardTab, Groups0),
		    msg_assign_softness_1(Face, Fs, HardTab, Groups);
		_ ->				%Already soft.
		    msg_assign_softness_1(Face, Fs, HardTab, Groups0)
	    end
    end;
msg_assign_softness_1(_, [], _, Groups) -> Groups.

msg_assign_softness_2(Face, AFace, G0, Ga0, HardTab, Groups0) ->
    NotAllowed = msg_not_allowed(gb_trees:get(Face, HardTab), Groups0) bor
	msg_not_allowed(gb_trees:get(AFace, HardTab), Groups0),
    NewG = msg_find_group(NotAllowed),
    G = G0 bor NewG,
    Ga = Ga0 bor NewG,
    Groups = gb_trees:update(Face, G, Groups0),
    gb_trees:update(AFace, Ga, Groups).
				 
msg_not_allowed(Fs, Groups) ->
    msg_not_allowed(Fs, Groups, 0).
    
msg_not_allowed([F|Fs], Groups, Acc) ->
    msg_not_allowed(Fs, Groups, gb_trees:get(F, Groups) bor Acc);
msg_not_allowed([], _, Acc) -> Acc.
    
msg_find_group(NotAllowed) ->
    msg_find_group(1, NotAllowed).

msg_find_group(B, NotAllowed) when B band NotAllowed =/= 0 ->
    msg_find_group(B bsl 1, NotAllowed);
msg_find_group(B, _) -> B.
    
%%%
%%% Split vertices: Each vertex in the 3D Studio format can only have one
%%% UV coordinate. Therefore, we must split each vertex that have different
%%% UV coordinates in different faces.
%%%

split_vertices(#e3d_mesh{tx=[]}=Mesh) -> Mesh;
split_vertices(#e3d_mesh{vs=Vtab0,fs=Fs0,tx=Tx0}=Mesh) ->
    F = split_vertices_1(Fs0, []),
    NextV = length(Vtab0),
    Map = split_make_map(F, NextV, []),
    Fs = split_remap_faces(Fs0, gb_trees:from_orddict(sort(Map)), []),
    Rmap = keysort(2, Map),
    Vtab = split_extend_vtab(Rmap, list_to_tuple(Vtab0), reverse(Vtab0)),
    Tx = split_reorder_tx(Fs, list_to_tuple(Tx0), []),
    Mesh#e3d_mesh{vs=Vtab,fs=Fs,tx=Tx}.

split_vertices_1([#e3d_face{vs=[A,B,C],tx=[Ta,Tb,Tc]}|Faces], Acc) ->
    split_vertices_1(Faces, [{A,Ta},{B,Tb},{C,Tc}|Acc]);
split_vertices_1([], Acc) ->
    R = sofs:relation(Acc),
    F = sofs:relation_to_family(R),
    sofs:to_external(F).

split_make_map([{_,[_]}|T], NextV, Acc) ->
    split_make_map(T, NextV, Acc);
split_make_map([{V,[_|UVs]}|T], NextV, Acc0) ->
    Acc = split_make_map_1(UVs, V, NextV, Acc0),
    split_make_map(T, NextV+length(UVs), Acc);
split_make_map([], _, Acc) -> Acc.

split_make_map_1([UV|UVs], V, NextV, Acc) ->
    split_make_map_1(UVs, V, NextV+1, [{{V,UV},NextV}|Acc]);
split_make_map_1([], _, _, Acc) -> Acc.
    
split_remap_faces([#e3d_face{vs=[A0,B0,C0],tx=[Ta,Tb,Tc]}=F|Faces], Map, Acc) ->
    A = split_remap_vtx(A0, Ta, Map),
    B = split_remap_vtx(B0, Tb, Map),
    C = split_remap_vtx(C0, Tc, Map),
    split_remap_faces(Faces, Map, [F#e3d_face{vs=[A,B,C]}|Acc]);
split_remap_faces([], _, Acc) -> reverse(Acc).

split_remap_vtx(V, T, Map) ->
    case gb_trees:lookup({V,T}, Map) of
	none -> V;
	{value,NewV} -> NewV
    end.

split_extend_vtab([{{V,_},_}|T], OldVtab, Acc) ->
    split_extend_vtab(T, OldVtab, [element(V+1, OldVtab)|Acc]);
split_extend_vtab([], _, Acc) -> reverse(Acc).

split_reorder_tx([#e3d_face{vs=[A,B,C],tx=[Ta,Tb,Tc]}|Faces], OldTx, Acc0) ->
    Acc = [{A,element(Ta+1, OldTx)},
	   {B,element(Tb+1, OldTx)},
	   {C,element(Tc+1, OldTx)}|Acc0],
    split_reorder_tx(Faces, OldTx, Acc);
split_reorder_tx([], _, Acc) ->
    split_reorder_tx_1(usort(Acc), 0, []).

split_reorder_tx_1([{I,UV}|UVs], I, Acc) ->
    split_reorder_tx_1(UVs, I+1, [UV|Acc]);
split_reorder_tx_1([], _, Acc) -> reverse(Acc).
