%%
%%  wpc_md3.erl --
%%
%%     MD3 import/export
%%
%%

-module(wpc_md3).

-include("e3d.hrl").
-include_lib("kernel/include/file.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [map/2, reverse/1]).

-define(MAX_QPATH,         64).
-define(MD3_MAX_FRAMES,    1024).
-define(MD3_MAX_TAGS,      16).
-define(MD3_MAX_SURFACES,  32).
-define(MD3_MAX_SHADERS,   256).
-define(MD3_MAX_VERTS,     4096).
-define(MD3_MAX_TRIANGLES, 8192).

-define(MD3_MAGIC, "IDP3").

-define(S32(X), X:32/little-signed-integer).
-define(S16(X), X:16/little-signed-integer).
-define(F32(X), X:32/little-signed-float).
-define(NAME(X,Sz), X:Sz/binary).
-define(VEC3(X,Y,Z), X:32/little-signed-float,
	             Y:32/little-signed-float,
	             Z:32/little-signed-float).

-record(md3,
	{
	  ident,        %% /binary = <<?MD_MAGIC>>
	  version,      %% :32/little-signed-integer
	  name,         %% :?MAX_QPATH/cstring
	  flags,        %% :32/little-signed-integer
	  num_frames,   %% :32/little-signed-integer
	  num_tags,     %% :32/little-signed-integer
	  num_surfaces, %% :32/little-signed-integer
	  num_skins,    %% :32/little-signed-integer
	  ofs_frames,   %% :32/little-signed-integer
	  ofs_tags,     %% :32/little-signed-integer
	  ofs_surfaces, %% :32/little-signed-integer
	  ofs_eof,      %% :32/little-signed-integer
	  %% 
	  frames   = [],
	  tags     = [],
	  surfaces = []
	 }).

-record(md3_frame,
	{
	  min_bounds,    %% :3/tuple-little-signed-float-unit32
	  max_bounds,    %% :3/tuple-little-signed-float-unit32
	  local_origin,  %% :3/tuple-little-signed-float-unit32
	  radius,        %% :32/little-signed-float
	  name           %% :16
	 }).


-record(md3_tag,
	{
	  name,    %% :?MAX_QPATH/clist
	  origin,  %% :3/tuple-little-signed-float-unit32
	  axis     %% :VEC3 * 3
	 }).


-record(md3_surface,
	{
	  ident,         %% :4/binary = ?MD3_MAGIC
	  name,          %% :?MAX_QPATH/cstring
	  flags,         %% :32/little-signed-integer
	  num_frames,    %% :32/little-signed-integer
	  num_shaders,   %% :32/little-signed-integer
	  num_verts,     %% :32/little-signed-integer
	  num_triangles, %% :32/little-signed-integer
	  ofs_triangles, %% :32/little-signed-integer
	  ofs_shaders,   %% :32/little-signed-integer
	  ofs_st,        %% :32/little-signed-integer
	  ofs_verts,     %% :32/little-signed-integer
	  ofs_end,       %% :32/little-signed-integer

	  shaders = [],
	  vs = [],
	  ns = [],
	  ts = [],
	  st = []
	 }).


-record(md3_shader,
	{
	  name,    %% :?MAX_QPATH/cstring
	  index    %% :32/little-signed-integer
	 }).


-record(md3_triangle,
	{
	  a,  %% :32/little-signed-integer
	  b,  %% :32/little-signed-integer
	  c   %% :32/little-signed-integer
	 }).


-record(md3_vertex,
	{
	  x,  %% :16/little-signed-integer
	  y,  %% :16/little-signed-integer
	  z,  %% :16/little-signed-integer
	  normal %% :16/little-signed-integer
	 }).


init() ->
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{import,{md3,Ask}}}, St) ->
    do_import(Ask, St);
command({file,{export,{md3,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{md3,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"Quake3 (.md3)...",md3,[option]}].

props() ->
    [{ext,".md3"},{ext_desc,"Quake3 File"}].

%%%
%%% Import.
%%%

do_import(Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "Quake3 Import Options", dialog(import),
	       fun(Res) ->
		       {file,{import,{md3,Res}}}
	       end);
do_import(Attr, St) ->
    set_pref(Attr),
    wpa:import(props(), import_fun(Attr), St).

import_fun(Attr) ->
    fun(Filename) ->
	    case import_md3(Filename) of
		{ok,E3dFile0} ->
		    io:format("imported\n"),
		    E3dFile = import_transform(E3dFile0, Attr),
		    io:format("transformed\n"),
		    {ok,E3dFile};
		{error,Error} ->
		    {error,Error}
	    end
    end.

%%%
%%% Export.
%%%

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "Quake3 Export Options", dialog(export),
	       fun(Res) ->
		       {file,{Op,{md3,Res}}}
	       end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Ps = [{subdivisions,SubDivs}|props()],
    Exporter(Ps, export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr)
    end.

export_1(Filename, Contents0, Attr) ->
    Contents = export_transform(Contents0, Attr),
    case export_md3(Filename, Contents) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

dialog(import) ->
    [{label_column,
      [{"Import scale",{text,get_pref(import_scale, 1.0),[{key,import_scale}]}},
       {"(Export scale)",{text,get_pref(export_scale, 1.0),[{key,export_scale}]}}]}];
dialog(export) ->
    [{label_column,
      [{"(Import scale)",{text,get_pref(import_scale,1.0),[{key,import_scale}]}},
       {"Export scale",{text,get_pref(export_scale,1.0),[{key,export_scale}]}}
      ]}].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(proplists:get_value(export_scale, Attr, 1.0)),
    e3d_file:transform(Contents, Mat).

import_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(proplists:get_value(import_scale, Attr, 1.0)),
    e3d_file:transform(Contents, Mat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MD3 import function
%%     <a>[skin]  =>
%%              <a>/head.md3
%%              <a>/upper.md3
%%              <a>/lower.md3
%%              <a>/head_<skin>.skin
%%              <a>/upper_<skin>.skin
%%              <a>/lower_<skin>.skin
%%
%%     <a>.md3  - only the model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import_md3(File) ->
    case filename:extension(File) of
	".md3" ->
	    case rd_md3_file(File) of
		{ok,MD3} ->
		    convert_md3(MD3);
		Error ->
		    Error
	    end;
	[] ->
	    case file:read_file_info(File) of
		{ok,Info} when Info#file_info.type == directory ->
		    import_player(File);
		{error, enoent} ->
		    Dir = filename:dirname(File),
		    Skin = filename:basename(File),
		    import_player(Dir,default,Skin);
		{error,Reason} ->
		    {error,file:format_error(Reason)}
	    end;
	_ ->
	    {error, "bad extension"}
    end.
		

%% Read skin definitions
import_player(Dir) ->
    import_player(Dir,default,"default").
    
import_player(Dir,Number,Skin) ->
    N = if Number == default -> "";
	   true -> "_" ++ integer_to_list(Number)
	end,
    {ok,Head}      = rd_md3_file(filename:join(Dir, "head"++N++".md3")),
    {ok,Upper}     = rd_md3_file(filename:join(Dir, "upper"++N++".md3")),
    {ok,Lower}     = rd_md3_file(filename:join(Dir, "lower"++N++".md3")),
    {ok,HeadSkin}  = rd_skin_file(filename:join(Dir, "head_"++Skin++".skin")),
    {ok,UpperSkin} = rd_skin_file(filename:join(Dir, "upper_"++Skin++".skin")),
    {ok,LowerSkin} = rd_skin_file(filename:join(Dir, "lower_"++Skin++".skin")),
    ObjsHead  = map(fun convert_surface/1, Head#md3.surfaces),
    ObjsUpper = map(fun convert_surface/1, Upper#md3.surfaces),
    ObjsLower = map(fun convert_surface/1, Lower#md3.surfaces),
    Textures = make_materials([HeadSkin, UpperSkin, LowerSkin],Dir,[]),
    io:format("textures = ~p\n", [Textures]),
    Mat = map(fun({M,F}) -> {M, [{maps,[{M,F}]}]} end, Textures),
    {ok, #e3d_file { objs = ObjsHead ++ ObjsUpper ++ ObjsLower,
		     mat = Mat,
		     creator = "Wings3D" }}.
    
%%
%% Skin reader
%% return a list of
%%   {tag,T}
%%   {texture,S,File}
%%
rd_skin_file(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    {ok,rd_skin(string:tokens(binary_to_list(Bin),"\r\n,"))};
	Error  ->
	    Error
    end.

rd_skin([Tag = "tag_"++_ | Ls]) ->
    [{tag,Tag} | rd_skin(Ls)];
rd_skin([Surf,Tex | Ls]) ->
    [{texture,Surf,Tex} | rd_skin(Ls)];
rd_skin([]) ->
    [].

make_materials([Skin|Rest],Dir,Mat) ->
    make_materials(Rest,Dir,make_material(Skin,Dir,Mat));
make_materials([],_,Mat) -> Mat.

make_material([{texture,Name,File}|Ts],Dir,Mat) ->
    Nm = list_to_atom(Name),
    case lists:keysearch(Nm, 1, Mat) of
	false ->
	    %% FIXME
	    BaseName = filename:basename(File),
	    make_material(Ts, Dir, [{Nm,filename:join(Dir,BaseName)}| Mat]);
	_ ->
	    make_material(Ts,Dir, Mat)
    end;
make_material([_|Ts], Dir, Mat) ->
    make_material(Ts, Dir, Mat);
make_material([], _, Mat) ->
    Mat.
    


convert_md3(MD3) ->
    Objs = map(fun convert_surface/1, MD3#md3.surfaces),
    {ok, #e3d_file { objs = Objs, creator = MD3#md3.name }}.

%% surface => obj
convert_surface(#md3_surface { name = Nm,
			       vs   = Vs,
			       ns   = Ns,
			       ts   = Ts,
			       st   = St }) ->
    Mat = list_to_atom(Nm),
    Fs = map(fun(Ws) ->
		     #e3d_face { vs  = Ws,
				 tx  = Ws,
				 ns  = Ws
				 %% mat = [Mat] 
				}
	     end, Ts),
    Obj = #e3d_mesh { vs = Vs,
		      tx = St,
		      ns = Ns,
		      fs = Fs },
    #e3d_object { name = Nm, obj = Obj }.



rd_md3_file(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    rd_md3(Bin,Bin);
	Error -> Error
    end.


rd_md3(<<?MD3_MAGIC,
      ?S32(Version),
      ?NAME(Name,?MAX_QPATH),
      ?S32(Flags),
      ?S32(Num_Frames),
      ?S32(Num_Tags),
      ?S32(Num_Surfaces),
      ?S32(Num_Skins),
      ?S32(Ofs_Frames),
      ?S32(Ofs_Tags),
      ?S32(Ofs_Surfaces),
      ?S32(Ofs_Eof), _/binary>>, Data) ->
    Frames = rd_frames(Data, Ofs_Frames,Num_Frames),
    Tags = rd_tags(Data, Ofs_Tags, Num_Tags),
    Surfaces = rd_surfaces(Data, Ofs_Surfaces, Num_Surfaces),
    MD3 = #md3 { ident = ?MD3_MAGIC,
		 version   = Version,
		 name      = cname(Name),
		 flags     = Flags,
		 num_frames = Num_Frames,
		 num_tags   = Num_Tags,
		 num_surfaces = Num_Surfaces,
		 num_skins    = Num_Skins,
		 ofs_frames   = Ofs_Frames,
		 ofs_tags     = Ofs_Tags,
		 ofs_surfaces = Ofs_Surfaces,
		 ofs_eof      = Ofs_Eof,
		 %%
		 frames    = Frames,
		 tags      = Tags,
		 surfaces  = Surfaces },
    {ok, MD3};
rd_md3(<<?MD3_MAGIC, _/binary>>, _) ->
    {error, "MD3 file corrupted"};
rd_md3(<<_/binary>>, _) ->
    {error, "not a .md3 file"}.



%% Read frames
-define(SIZEOF_FRAME, (3*4+3*4+3*4+4+16)).

rd_frames(Data, Offs, N) ->
    rd_frames(Data,Offs,N,[]).

rd_frames(Data,Offs,I,Xs) when I > 0 ->
    <<_:Offs/binary, ?VEC3(MiX,MiY,MiZ),
     ?VEC3(MaX,MaY,MaZ),
     ?VEC3(Ox,Oy,Oz),
     ?F32(Radius),
     ?NAME(Name,16),_/binary>> = Data,
    Frame = #md3_frame { min_bounds = {MiX,MiY,MiZ},
			 max_bounds = {MaX,MaY,MaZ},
			 local_origin = {Ox,Oy,Oz},
			 radius = Radius,
			 name = cname(Name) },
    rd_frames(Data, Offs+?SIZEOF_FRAME, I-1, [Frame|Xs]);
rd_frames(_Data, _Offs, 0, Xs) -> reverse(Xs).



%% Read Tags
-define(SIZEOF_TAG, (?MAX_QPATH+3*4+3*4+3*4+3*4)).

rd_tags(Data, Offset, Num) ->
    rd_tags(Data, Offset, Num, []).

rd_tags(Data, Offs, I, Xs) when I > 0 ->
    <<_:Offs/binary,
     ?NAME(Name,?MAX_QPATH),
     ?VEC3(Ox,Oy,Oz),
     ?VEC3(Ax,Ay,Az),
     ?VEC3(Bx,By,Bz),
     ?VEC3(Cx,Cy,Cz), _/binary>> = Data,
    Tag = #md3_tag { name = cname(Name),
		     origin = {Ox,Oy,Oz},
		     axis = { {Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}} },
    rd_tags(Data, Offs+?SIZEOF_TAG, I-1, [Tag|Xs]);
rd_tags(_Data, _Offs, 0, Xs) -> reverse(Xs).


%% Read vertices
rd_vertices(Data, Offset, Num) ->
    rd_vertices(Data, Offset, Num,[],[]).

rd_vertices(Data, Offs, I, Vs,Ns) when I > 0 ->
    <<_:Offs/binary,
     ?S16(X), ?S16(Y), ?S16(Z), ?S16(N),
     _/binary>> = Data,
    Lat = (N band 255)/255 * 2*math:pi(),
    Lng = ((N bsr 8) band 255)/255 * 2*math:pi(),
    Xn = math:cos(Lat)*math:sin(Lng),
    Yn = math:sin(Lat)*math:sin(Lng),
    Zn = math:cos(Lng),
    rd_vertices(Data, Offs+8, I-1, 
		[{X/64, Y/64, Z/64}|Vs],
		[{Xn,Yn,Zn}|Ns]);
rd_vertices(_Data, _Offs, 0, Vs, Ns) ->
    {reverse(Vs), reverse(Ns)}.


%% Read triangles
rd_triangles(Data, Offs, Num) ->
    rd_triangles(Data, Offs, Num,[]).

rd_triangles(Data, Offs, I, Xs) when I > 0 ->
    <<_:Offs/binary,
     ?S32(P1),?S32(P2),?S32(P3),
     _/binary>> = Data,
    rd_triangles(Data,Offs+12,I-1,[[P1,P2,P3]|Xs]);
rd_triangles(_Data, _Offs, 0, Xs) ->
    lists:reverse(Xs).

%%
%% Read shaders
%%
rd_shaders(Data, Offset, Num) ->
    rd_shaders(Data, Offset, Num, []).


rd_shaders(Data, Offs, I, Xs) when I > 0 ->
    <<_:Offs/binary,
     ?NAME(Name,?MAX_QPATH),
     ?S32(Shader_Index),
     _/binary>> = Data,
    Shader = #md3_shader { name = cname(Name),
			   index = Shader_Index },
    rd_shaders(Data, Offs+?MAX_QPATH+4, I-1, [Shader|Xs]);
rd_shaders(_Data, _Offs, 0, Xs) -> reverse(Xs).


%%
%% Read texture coordinates
%%
rd_st(Data, Offset, Num) ->
    rd_st(Data, Offset, Num, []).

rd_st(Data, Offs, I, Xs) when I > 0 ->
    <<_:Offs/binary,
     ?F32(S),?F32(T),
     _/binary>> = Data,
    rd_st(Data, Offs+8, I-1, [{S,T}|Xs]);
rd_st(_Data, _Offs, 0, Xs) -> reverse(Xs).


rd_surfaces(Data, Offs, Num) ->
    rd_surfaces(Data, Offs, Num, []).

rd_surfaces(Data, Offs, I, Xs) when I>0 ->
    <<_:Offs/binary,
     ?MD3_MAGIC,
     ?NAME(Name,?MAX_QPATH),
     ?S32(Flags),
     ?S32(Num_Frames),
     ?S32(Num_Shaders),
     ?S32(Num_Verts),
     ?S32(Num_Triangles),
     ?S32(Ofs_Triangles),
     ?S32(Ofs_Shaders),
     ?S32(Ofs_St),
     ?S32(Ofs_Verts),
     ?S32(Ofs_End),
     _/binary>> = Data,

    Shaders = rd_shaders(Data, Offs+Ofs_Shaders, Num_Shaders),
    {Vertices,Normals} = rd_vertices(Data, Offs+Ofs_Verts, Num_Verts),
    Triangles = rd_triangles(Data, Offs+Ofs_Triangles, Num_Triangles),
    St = rd_st(Data, Offs+Ofs_St, Num_Verts),
    Surface = #md3_surface { ident = ?MD3_MAGIC,
			     name = cname(Name),
			     flags = Flags,
			     num_frames = Num_Frames,
			     num_shaders = Num_Shaders,
			     num_verts   = Num_Verts,
			     num_triangles = Num_Triangles,
			     ofs_triangles = Ofs_Triangles,
			     ofs_shaders = Ofs_Shaders,
			     ofs_st = Ofs_St,
			     ofs_verts = Ofs_Verts,
			     ofs_end = Ofs_End,
			     %%
			     shaders  = Shaders,
			     vs = Vertices,
			     ns = Normals,
			     ts = Triangles,
			     st = St
			    },
    rd_surfaces(Data, Offs+Ofs_End, I-1, [Surface|Xs]);
rd_surfaces(_Data, _Offs, 0, Xs) -> reverse(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MD3 export function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export_md3(_File, _Attrs) ->
    ok.


%%
%% Utils
%%

cname(Bin) when binary(Bin) ->
    cn(binary_to_list(Bin)).

cn([0|_]) -> [];
cn([C|Cs]) -> [C|cn(Cs)];
cn([]) -> [].

    
	
    


