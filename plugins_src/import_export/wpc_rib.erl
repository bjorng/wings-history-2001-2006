%%
%%  wpc_rib.erl --
%%
%%     Renderman exporter.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson, Danni Coy (KayosIII).
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rib.erl,v 1.3 2002/04/14 18:38:59 bjorng Exp $
%%

-module(wpc_rib).
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3,map/2,foreach/2,reverse/1,seq/2,flat_length/1]).

init() ->
    true.

menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu({file,render}, Menu) ->
    case os:find_executable("rendrib") of
	false -> Menu;
	_Path -> Menu ++ [{"Render to BMRT 2.6",bmrt,[option]}]
    end;
menu(_, Menu) -> Menu.

command({file,{export,{rib,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, Exporter, St);
command({file,{export_selected,{rib,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, Exporter, St);
command({file,{render,{bmrt,Ask}}}, St) ->
    render_bmrt(Ask, St);
command(_, _) -> next.

menu_entry(Menu) ->
    Menu ++ [{"RenderMan (.rib)",rib,[option]}].

props() ->
    [{ext,".rib"},{ext_desc,"RenderMan File"}].

render_props() ->
    [{ext,".tif"},{ext_desc,"Tiff Bitmap"}].

dialog_qs(export)->
    common_dialog();
dialog_qs(render)->
    DefVar = {render_type,get_pref(render_type, preview)},
    [{hframe,
      [{vframe,
	[{key_alt,DefVar,"Preview Window",preview},
	 {key_alt,DefVar,"File",file}],
	[{title,"Output"}]},
       {vframe,
	[{label_column,
	  [{"Width",{text,get_pref(width, 320),[{key,width}]}},
	   {"Height",{text,get_pref(height, 240),[{key,height}]}}]}],
	[{title,"Resolution"}]}]}|common_dialog()].

common_dialog() ->
    MeshVar = {mesh_type,get_pref(mesh_type, poly)},
    SaveVar = {save_by,get_pref(save_by, one_file)},
    [{vframe,
      [{key_alt,MeshVar,"Polygon Mesh (older renderer)",poly},
       {key_alt,MeshVar,"Subdivision Mesh (smoother)",subdiv}],
      [{title,"Mesh Type"}]},
     {vframe,
      [{key_alt,SaveVar,"One file",one_file},
       {key_alt,SaveVar,"One file per object",by_object},
       {key_alt,SaveVar,"One file per object and material",
	by_material}],
      [{title,"Save By"}]},
     {"Export UV Coordinates",get_pref(export_uv, true),[{key,export_uv}]},
     {"Triangulate Faces (needed for BMRT)",get_pref(triangulate, true),
      [{key,triangulate}]},
     {"Expand Faces (older renderer)",get_pref(expand_faces, true),
      [{key,expand_faces}]},
     {"Export Normals",get_pref(export_normals, true),[{key,export_normals}]},
     {vframe,
      [{label_column,
	[{"Light 1",
	  {slider,{text,get_pref(light1, 600),
		   [{range,{0,999}},{key,light1}]}}},
	 {"Light 2",
	  {slider,{text,get_pref(light2, 200),
		   [{range,{0,999}},{key,light2}]}}},
	 {"Light 3",
	  {slider,{text,get_pref(light3, 200),
		   [{range,{0,999}},{key,light3}]}}}]}],
      [{title,"Light Intensities"}]}].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

%%%
%%% Rendering.
%%%

render_bmrt(Ask, St) when is_atom(Ask) ->
    wpa:dialog(Ask, dialog_qs(render), St,
	       fun(Res) ->
		       {file,{render,{bmrt,Res}}}
	       end);
render_bmrt(Attr0, St) ->
    set_pref(Attr0),
    Attr = case property_lists:get_value(render_type, Attr0) of
	       file ->
		   RendFile = wpa:export_filename(render_props(), St),
		   [{render_file,RendFile}|Attr0];
	       preview -> Attr0
	   end,
    wpa:export(none, render_fun(Attr), St).

render_fun(Attr) ->
    fun(Filename, Contents) ->
	    case render(Filename, Contents, Attr) of
		ok -> ok;
		{error,Error} -> {error,Error}
	    end
    end.

render(none, Contents, Attr) ->
    TmpName = "wpc_rib_temp" ++ os:getpid() ++ ".rib",
    case export_1(TmpName, Contents, Attr) of
	ok -> ok;
	{error,_}=Error -> Error
    end,
    Width = property_lists:get_value(width, Attr),
    Height = property_lists:get_value(height, Attr),
    case property_lists:get_value(render_file, Attr) of
    	undefined ->
	    os:cmd("rendrib -silent -res " ++ integer_to_list(Width) ++
		   " " ++ integer_to_list(Height) ++ " -d 16 " ++ TmpName);
    	RendFile ->
	    os:cmd("rendrib -silent -res " ++ integer_to_list(Width) ++
		   " " ++ integer_to_list(Height) ++ " " ++ TmpName),
	    os:cmd("iv " ++ RendFile)
    end,
    ok = file:delete(TmpName).

%%%
%%% Export functions.
%%%

do_export(Ask, _Exporter, St) when is_atom(Ask) ->
    wpa:dialog(Ask, dialog_qs(export), St,
	    fun(Res) ->
		    {file,{export,{rib,Res}}}
	    end);
do_export(Attr, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    Exporter(props(), export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    case export_1(Filename, Contents, Attr) of
		ok -> ok;
		{error,Error} -> {error,Error}
	    end
    end.

export_1(Name, #e3d_file{objs=Objs,mat=Mat,creator=Creator}, Attr) ->
    {ok,F} = file:open(Name, [write]),
    Base = filename:basename(filename:rootname(Name, ".rib")),
    io:format(F, "# Exported from ~s\n", [Creator]),
    case property_lists:get_value(render_file, Attr) of
    	undefined -> ok;
	RenderFile0 ->
	    RenderFile = filename:basename(RenderFile0),
	    io:format(F, "Display ~p \"file\" \"rgba\"\n", [RenderFile])
    end,
    export_camera(F),
    io:put_chars(F, "WorldBegin\n"),
    export_lights(F, Attr),
    io:put_chars(F, "Identity\n"),
    export_materials(Mat, Base, Attr),
    foreach(fun(Obj) -> export_object(F, Obj, Mat, Base, Attr) end, Objs),
    io:put_chars(F, "WorldEnd\n"),
    ok = file:close(F).

export_object(F, #e3d_object{name=Name,obj=Mesh0}, Mat, Base, Attr) ->
    Mesh = case property_lists:get_bool(triangulate, Attr) of
	       true -> e3d_mesh:triangulate(Mesh0);
	       false -> Mesh0
	   end,

    {Fs0,Ns0} = e3d_mesh:vertex_normals(Mesh),
    Ns = list_to_tuple(Ns0),
    Fs1 = sofs:to_external(sofs:relation_to_family(sofs:relation(Fs0))),
    Fs = lists:append([Fs || {_Mat,Fs} <- Fs1]),
    {FsV,FsN,FsUV} = separate_faces(Fs),
    #e3d_mesh{vs=Vs0,he=He,tx=Tx0} = Mesh,
    Vs = list_to_tuple(Vs0),
    Tx = list_to_tuple(Tx0),
		        
    io:format(F, "# Object: ~s\n", [Name]),
    MeshType = property_lists:get_value(mesh_type, Attr),
    case property_lists:get_value(save_by, Attr) of
	one_file ->
	    io:put_chars(F, "AttributeBegin\n"),
	    [{[MatName],_}|_] = Fs1,
	    write_shader(F, MatName, Mat, Base),
	    export_mesh(F, Fs, FsV, Attr),
	    export_attr(F, FsV, FsN, FsUV, Vs, Ns, Tx, He, Attr),
	    io:put_chars(F, "AttributeEnd\n");
	by_object ->
	    ObjName = case MeshType of
			  subdiv -> Name ++ "_SubDivSurf.rib";
			  poly -> Name ++ "_PointsPoly.rib"
		      end,
	    io:format(F, "ReadArchive \"~s\"\n", [ObjName]),
	    [{[MatName],_}|_] = Fs1,
	    MatFile = Base ++ "_" ++ atom_to_list(MatName) ++ "_WingsMat.rib",
	    {ok,ObjFile} = file:open(ObjName, [write]),
	    io:put_chars(ObjFile, "AttributeBegin\n"),
	    io:format(ObjFile, "ReadArchive \"~s\"\n", [MatFile]),
	    export_mesh(ObjFile, Fs, FsV, Attr),
	    export_attr(ObjFile, FsV, FsN, FsUV, Vs, Ns, Tx, He, Attr),
	    io:put_chars(ObjFile, "AttributeEnd\n"),
	    ok = file:close(ObjFile);
	by_material ->
	    AttsName = Name ++ "_Atts.rib",
	    ObjName = case MeshType of
			  subdiv -> Name ++ "_SubDivSurf.rib";
			  poly -> Name ++ "_PointsPoly.rib"
		      end,
	    io:format(F, "ReadArchive \"~s\"\n", [ObjName]),
	    {ok,ObjFile} = file:open(ObjName, [write]),
	    foreach(fun({[MatName],FsList}) ->
			    io:put_chars(ObjFile, "AttributeBegin\n"),
			    MatFile = Base ++ "_" ++ 
				atom_to_list(MatName) ++ "_WingsMat.rib",
			    io:format(ObjFile,
				      "ReadArchive \"~s\"\n", [MatFile]),
			    export_mesh(ObjFile, FsList, FsV, Attr),
			    io:format(ObjFile, "ReadArchive ~p\n", [AttsName]),
			    io:put_chars(ObjFile, "AttributeEnd\n")
		    end, Fs1),
	    ok = file:close(ObjFile),
	    {ok,AttrFile} = file:open(AttsName, [write]),
	    export_attr(AttrFile, FsV, FsN, FsUV, Vs, Ns, Tx, He, Attr),
	    ok = file:close(AttrFile)
    end.

export_mesh(F, Fs, FsV0, Attr) ->
    case property_lists:get_value(mesh_type, Attr) of
    	subdiv ->
	    io:put_chars(F, "SubdivisionMesh \"catmull-clark\"\n");
    	poly ->
	    io:put_chars(F, "PointsPolygons\n")
    end,

    io:put_chars(F, "[ "),
    foreach(fun(Face) ->
		    io:format(F, " ~p", [length(Face)])
	    end, Fs),
    io:put_chars(F, "]\n"),

    io:put_chars(F, "[ "),
    FsV = case property_lists:get_bool(expand_faces, Attr) of
	      true -> seq(0, flat_length(Fs)-1);
	      false -> FsV0
	  end,
    foreach(fun(V) ->
		    io:format(F, "~p ", [V])
	    end, FsV),
    io:put_chars(F, "]\n").

export_attr(F, FsV, FsN, FsUV, Vs, Ns, Tx, He0, Attr) ->
    %% Attributes (i.e. creases)
    case property_lists:get_value(mesh_type, Attr) of
	subdiv ->
	    He = create_loops(He0),
	    io:put_chars(F, "[\"interpolateboundary\""),
	    foreach(fun(_) -> io:put_chars(F, " \"crease\"") end, He),
	    io:put_chars(F, "] [0 0"),
	    foreach(fun(H) -> io:format(F, " ~p 1", [length(H)]) end, He),
	    io:put_chars(F, "]\n["),
	    foreach(fun(H) ->
			    foreach(fun(V) ->
					    io:format(F, " ~p", [V])
				    end, H)
		    end, He),
	    io:put_chars(F, "]\n["),
	    foreach(fun(_) -> io:put_chars(F, " 2") end, He),
	    io:put_chars(F, "]\n");
	poly -> ok
    end,

    %% Vertex coords
    io:put_chars(F, "\"P\"\n[\n"),
    ExpandFaces = property_lists:get_bool(expand_faces, Attr),
    case ExpandFaces  of
	true ->
	    foreach(fun(V) ->
			    {X,Y,Z} = element(V+1, Vs),
			    io:format(F, "~p ~p ~p\n", [X,Y,Z])
		    end, FsV);
	false ->
	    foreach(fun({X,Y,Z}) ->
			    io:format(F, "~p ~p ~p\n", [X,Y,Z])
		    end, Vs)
    end,
    io:put_chars(F, "]\n"),

    %% Normals
    case property_lists:get_bool(export_normals, Attr) of
	true ->
	    case ExpandFaces of
		true ->
		    io:put_chars(F, "\"N\" \n[\n");
		false ->
		    io:put_chars(F, "\"facevarying float[3] N\" \n[\n")
	    end,
	    foreach(fun(N) ->
			    {X,Y,Z} = element(N+1, Ns),
			    io:format(F, "~p ~p ~p\n", [X,Y,Z])
		    end, FsN),
	    io:put_chars(F, "]\n");
	false -> ok
    end,

    %%UV coordinates
    case property_lists:get_bool(export_uv, Attr) andalso FsUV =/= [] of
	true ->
	    case ExpandFaces of
		true ->
		    io:put_chars(F, "\"st\" \n[\n");
		false ->
		    io:put_chars(F, "\"facevarying float[2] st\" \n[\n")
	    end,
	    foreach(fun(UV) ->
			    {S0,T0} = element(UV+1, Tx),
			    %%wings measures textures from bottom left;
			    %% Renderman from top left - must invert T0
			    io:format(F, "~p ~p\n", [S0,1-T0])
		    end, FsUV),
	    io:put_chars(F, "]\n");
	false -> ok
    end.

export_camera(F) ->
    [{OX,OY,OZ},Dist,Az,El,{TrackX,TrackY},Fov] =
	wpa:camera_info([aim,distance_to_aim,azimuth,elevation,tracking,fov]),
    io:format(F, "Projection \"perspective\" \"fov\" ~p\n", [Fov]),
    io:format(F, "Scale ~p ~p ~p\n", [1,1,-1]),
    io:format(F, "Translate ~p ~p ~p\n", [TrackX,TrackY,-Dist]),
    io:format(F, "Rotate ~p ~p ~p ~p\n", [El,1,0,0]),
    io:format(F, "Rotate ~p ~p ~p ~p\n", [Az,0,1,0]),
    io:format(F, "Translate ~p ~p ~p\n", [OX,OY,OZ]).

export_materials(Mat, Base, Attr) ->
    case property_lists:get_value(save_by, Attr) of
	one_file ->
	    export_materials_one(Mat, Base);
	_->
	    export_materials_many(Mat, Base)
    end.

export_materials_one(Mats, Base) ->	
    foreach(
      fun({Name,Mat}) ->
	      case property_lists:get_value(diffuse_map, Mat, none) of
		  none -> true;
		  {W,H,DiffMap} ->
		      MapFile = Base ++ "_" ++ atom_to_list(Name) ++
			  "_diffmap.tif",
		      Image = #e3d_image{image=DiffMap,width=W,height=H},
		      ok = e3d_image:save(Image, MapFile)
	      end
      end, Mats).

export_materials_many(Mats, Base) ->
    foreach(
      fun({Name,Mat}) ->
	      MatName = Base ++ "_" ++ atom_to_list(Name) ++ "_WingsMat.rib",
	      {ok,MatFile} = file:open(MatName, [write]),
	      export_material(MatFile, Name, Mat, Base),
	      ok = file:close(MatFile),
	      case property_lists:get_value(diffuse_map, Mat, none) of
		  none -> true;
		  {W,H,DiffMap} ->
		      MapFile = Base ++ "_" ++ atom_to_list(Name) ++
			  "_diffmap.tif",
		      Image = #e3d_image{image=DiffMap, width=W,height=H},
		      ok = e3d_image:save(Image, MapFile)
	      end
      end, Mats).

write_shader(F, Name, [{Name,Mat}|_], Base) ->
    export_material(F, Name, Mat, Base);
write_shader(F, Name, [_|T], Base) ->
    write_shader(F, Name, T, Base).

export_material(F,Name,Mat,Base) ->
    [{_,{Ar,Ag,Ab}},{_,{Dr,Dg,Db}},{_,{Sr,Sg,Sb}},
     {_,Shine},{_,Opacity}|_]=Mat,
    io:format(F, "Color ~p ~p ~p\n", [Dr,Dg,Db]),
    io:format(F, "Opacity ~p ~p ~p\n", [Opacity,Opacity,Opacity]),
    Ka = (Ar+Ag+Ab)/3,
    Kd = (Dr+Dg+Db)/3,
    Ks = (Sr+Sg+Sb)/3,
    case property_lists:get_value(diffuse_map, Mat, none) of
	none ->
	    io:format(F, "Surface \"plastic\"\n"
		      " \"float Ka\" [~p]\n"
		      " \"float Kd\" [~p]\n"
		      " \"float Ks\" [~p]\n"
		      " \"float roughness\" [~p]\n"
		      " \"color specularcolor\" [~p ~p ~p]\n",
		      [Ka,Kd,Shine,0.1,Sr,Sg,Sb]);
	{_,_,_DiffMap} ->
	    MapFile = Base ++ "_" ++ atom_to_list(Name) ++ "_diffmap.tif",
	    io:format(F, "Surface \"paintedplastic\"\n"
		      " \"float Ka\" [~p]\n"
		      " \"float Kd\" [~p]\n"
		      " \"float Ks\" [~p]\n"
		      " \"float roughness\" [~p]\n"
		      " \"color specularcolor\" [~p ~p ~p]\n"
		      " \"string texturename\" [~p]\n",
		      [Ka,Kd,Shine,0.1,Sr,Sg,Sb,MapFile])
    end.

export_lights(F, Attr) ->
    Light1 = property_lists:get_value(light1, Attr),
    Light2 = property_lists:get_value(light2, Attr),
    Light3 = property_lists:get_value(light3, Attr),
    io:format(F,
	      "# Lights!
TransformBegin
      ConcatTransform [1 0 0 0 0 1 0 0 0 0 1 0 -7.65848 6.78137 6.28592 1]
      Declare \"shadows\" \"string\"
      Attribute \"light\" \"shadows\" [\"off\"]
      AttributeBegin
      Declare \"intensity\" \"float\"
      Declare \"lightcolor\" \"color\"
      AreaLightSource \"arealight\" 1 \"intensity\" [~p] \"lightcolor\" [1 1 1]
      Interior \"arealight\" \"intensity\" [~p] \"lightcolor\" [1 1 1]
      AttributeBegin
      TransformBegin
      Sphere 1 -1 1 360
      TransformEnd
      AttributeEnd
      AttributeEnd
      Illuminate 1 1
      TransformEnd
      TransformBegin
      ConcatTransform [1 0 0 0 0 1 0 0 0 0 1 0 11.844 6.78137 3.38473 1]
      Declare \"shadows\" \"string\"
      Attribute \"light\" \"shadows\" [\"off\"]
      AttributeBegin
      Declare \"intensity\" \"float\"
      Declare \"lightcolor\" \"color\"
      AreaLightSource \"arealight\" 2 \"intensity\" [~p] \"lightcolor\" [1 1 1]
      Interior \"arealight\" \"intensity\" [~p] \"lightcolor\" [1 1 1]
      AttributeBegin
      TransformBegin
      Sphere 1 -1 1 360
      TransformEnd
      AttributeEnd
      AttributeEnd
      Illuminate 2 1
      TransformEnd
      TransformBegin
      ConcatTransform [1 0 0 0 0 1 0 0 0 0 1 0 -4.75729 6.78137 -12.0883 1]
      Declare \"shadows\" \"string\"
      Attribute \"light\" \"shadows\" [\"off\"]
      AttributeBegin
      Declare \"intensity\" \"float\"
      Declare \"lightcolor\" \"color\"
      AreaLightSource \"arealight\" 3 \"intensity\" [~p] \"lightcolor\" [1 1 1]
      Interior \"arealight\" \"intensity\" [~p] \"lightcolor\" [1 1 1]
      AttributeBegin
      TransformBegin
      Sphere 1 -1 1 360
      TransformEnd
      AttributeEnd
      AttributeEnd
      Illuminate 3 1
      TransformEnd\n",
	      [Light1,Light1,Light2,Light2,Light3,Light3]).

%%%
%%% Utilities.
%%%

separate_faces(L) -> separate_faces(L, [], [], []).

separate_faces([H|T], VAcc0, NAcc0, UVAcc0) ->
    {VAcc,NAcc,UVAcc} = separate_face(H, VAcc0, NAcc0, UVAcc0),
    separate_faces(T, VAcc, NAcc, UVAcc);
separate_faces([], VAcc, NAcc, UVAcc) ->
    {VAcc,NAcc,UVAcc}.

separate_face([{V,N,UV}|T], VAcc, NAcc, UVAcc) ->
    separate_face(T, [V|VAcc], [N|NAcc], [UV|UVAcc]);
separate_face([{V,N}|T], VAcc, NAcc, UVAcc) ->
    separate_face(T, [V|VAcc], [N|NAcc], UVAcc);
separate_face([], VAcc, NAcc, UVAcc) ->
    {reverse(VAcc),reverse(NAcc),reverse(UVAcc)}.

create_loops([]) -> [];
create_loops(L) -> create_loops(L, []).

create_loops(Vl, Acc) ->
    if
	length(Vl) >0 ->
	    Vh = hd(Vl),
	    Vt = tl(Vl),
	    {V0,V1} = Vh,
	    {Acc0,Nl} = create_loops0(V1,V0,Vt, [V0,V1]),
	    Acc1 = lists:append(Acc, [Acc0]),
	    create_loops(Nl,Acc1);
	length(Vl) =< 0 ->
	    Acc
    end.

create_loops0( V,V0,Vl,Acc) ->
    {Nxt,Nl} = get_next(V, Vl, []),
    case Nxt of
	V0 ->
	    Acc0 = lists:append(Acc,[V0]),
	    {Acc0,Nl};
	Nxt when Nxt >= 0 ->
	    Acc0 = lists:append(Acc,[Nxt]),
	    create_loops0(Nxt,V0,Nl,Acc0);
	Nxt when Nxt < 0 ->
	    {Acc,Nl}
    end.

%% Find first edge with shared vertex,
%% return other vertex in that edge, remove edge from list.

get_next(X, [{X,Z}|T], Acc) ->
    Nl = lists:append(Acc,T),
    {Z,Nl};
get_next(X,[{Z,X}|T],Acc)->
    Nl = lists:append(Acc,T),
    {Z,Nl};
get_next(X, [H|T],Acc) ->
    Nl = lists:append(Acc,[H]),
    get_next(X, T, Nl);
get_next(_, [], Acc) -> {-1,Acc}.
