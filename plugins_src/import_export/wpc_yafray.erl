%%
%%  wpc_yafray.erl --
%%
%%     YafRay Plugin User Interface.
%%
%%  Copyright (c) 2003 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_yafray.erl,v 1.1 2003/01/17 09:09:39 raimo_niskanen Exp $
%%

-module(wpc_yafray).

-export([init/0,menu/2,command/2]).

-include("e3d.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,foreach/2,
		map/2,foldl/3]).



init() ->
    true.

menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) ->
    Menu.

menu_entry(Menu) ->
    Menu ++ [{"YafRay [Experimental] (.xml)",yafray, []}].



command({file,{export,yafray}}, St) ->
    Props = props(St),
    wpa:export(Props, fun_export_2(Props), St);
command({file,{export_selected,yafray}}, St) ->
    Props = props(St),
    wpa:export_selected(Props, fun_export_2(Props), St);
command(_C, _St) ->
    next.

props(St) ->
    Lights = wpa:lights(St),
    [{ext,".xml"},{ext_desc,"YafRay File"},{lights,Lights}].

fun_export_2(Props) ->
    fun (Filename, Contents) ->
	    export(Props, Filename, Contents)
    end.



export(Props, Filename, #e3d_file{objs=Objs,mat=Mats,creator=Creator}) ->
    case file:open(Filename, [write]) of
	{error,_}=Error -> 
	    Error;
	{ok,F} ->
	    CameraName = "WingsDefaultCamera",
	    BackgroundName = "WingsDefaultBackground",
	    Outfile = filename:rootname(Filename)++".tga",
	    %%
	    Lights = proplists:get_value(lights, Props, []),
	    ok = io:format(F, "<!-- ~s: Exported from ~s -->\r\n\r\n", 
			   [Filename, Creator]),
	    ok = io:format(F, "<scene>\r\n", []),
	    foreach(fun ({Name, Mat}) -> 
			    export_material(F, Name, Mat) 
		    end, 
		    Mats),
	    foreach(fun (#e3d_object{name=NameStr,obj=Mesh}) ->
			    export_object(F, NameStr, Mesh)
		    end,
		    Objs),
	    foreach(fun (Light) -> 
			    export_light(F, Light) 
		    end,
		    Lights),
	    export_camera(F, CameraName),
	    export_background(F, BackgroundName),
	    export_render(F, CameraName, BackgroundName, Outfile),
	    ok = io:format(F, "</scene>\r\n", []),
	    ok = file:close(F)
    end.



export_material(F, Name, Mat) ->
    OpenGL = proplists:get_value(opengl, Mat),
    ok = io:format(F, "<shader type=\"generic\" name=\"~s\">\r\n", 
		   [atom_to_list(Name)]),
    ok = io:format(F, "    <attributes>\r\n", []),
    {Dr,Dg,Db,Opacity} = proplists:get_value(diffuse, OpenGL),
    Transparency = 1 - Opacity,
    export_rgb(F, color, 
	       {Dr*Opacity,Dg*Opacity,Db*Opacity,1.0}),
    export_rgb(F, specular, proplists:get_value(specular, OpenGL)),
    ok = io:format(F, "        <hard value=\"~.10f\"/>\r\n", 
		   [proplists:get_value(shininess, OpenGL)*100.0]),
    export_rgb(F, reflected, proplists:get_value(ambient, OpenGL)),
    export_rgb(F, transmited, 
	       {Dr*Transparency,Dg*Transparency,Db*Transparency,1.0}),
    ok = io:format(F, "    </attributes>\r\n", []),
    ok = io:format(F, "</shader>\r\n\r\n", []).
	  


export_rgb(F, Type, {R,G,B,_}) ->
    ok = io:format(F,
		   "        <~s r=\"~.10f\" g=\"~.10f\" b=\"~.10f\"/>\r\n", 
		   [atom_to_list(Type),R,G,B]).



export_object(F, NameStr, #e3d_mesh{}=Mesh) ->
    #e3d_mesh{fs=Fs,vs=Vs} = e3d_mesh:triangulate(Mesh),
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    [{_Count,DefaultMaterial}|_] = reverse(sort(count_equal(MM))),
    ok = io:format(F, "<object name=\"~s\" shader_name=\"~s\">\r\n",
	      [NameStr, atom_to_list(DefaultMaterial)]),
    ok = io:format(F, "    <attributes>\r\n", []),
    ok = io:format(F, "    </attributes>\r\n", []),
    ok = io:format(F, "    <mesh autosmooth=\"17\">\r\n", []),
    ok = io:format(F, "        <points>\r\n", []),
    export_vertices(F, Vs),
    ok = io:format(F, "        </points>\r\n", []),
    ok = io:format(F, "        <faces>\r\n", []),
    export_faces(F, Fs, DefaultMaterial),
    ok = io:format(F, "        </faces>\r\n", []),
    ok = io:format(F, "    </mesh>\r\n", []),
    ok = io:format(F, "</object>\r\n\r\n", []),
    ok.

export_vertices(_F, []) ->
    ok;
export_vertices(F, [Pos|T]) ->
    export_pos(F, p, Pos),
    export_vertices(F, T).



export_pos(F, Type, {X,Y,Z}) ->
    ok = io:format(F, "        <~s x=\"~.10f\" y=\"~.10f\" z=\"~.10f\"/>\r\n",
		   [atom_to_list(Type),X,Y,Z]).



export_faces(_F, [], _DefMat) ->
    ok;
export_faces(F, [#e3d_face{vs=[A,B,C],mat=[Mat|_]}|T], DefaultMaterial) ->
    case Mat of
	DefaultMaterial ->
	    ok = io:format(F, "        <f a=\"~w\" b=\"~w\" c=\"~w\"/>\r\n",
			   [A,B,C]);
	_ ->
	    ok = io:format(F, "        <f a=\"~w\" b=\"~w\" c=\"~w\" "++
			   " shader_name=\"~s\"/>\r\n",
			   [A,B,C,atom_to_list(Mat)])
    end,
    export_faces(F, T, DefaultMaterial).



export_light(F, {Name,Ps}) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    export_light(F, Name, Type, OpenGL).

export_light(F, Name, point, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    {_,_,_,A}=Diffuse = 
	proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    ok = io:format(F,
	   "<light type=\"pointlight\" name=\"~s\" power=\"~.10f\">\r\n", 
	   [Name, A*10.0]),
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),
    ok = io:format(F, "</light>\r\n\r\n", []);
export_light(F, Name, infinite, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    {_,_,_,A}=Diffuse = 
	proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    ok = io:format(F,
	   "<light type=\"sunlight\" name=\"~s\" power=\"~.10f\">\r\n", 
	   [Name, A*10.0]),
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),
    ok = io:format(F, "</light>\r\n\r\n", []);
export_light(F, Name, spot, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, 45.0),
    SpotExponent = proplists:get_value(spot_exponent, OpenGL, 2.0),
    {_,_,_,A}=Diffuse = 
	proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    ok = io:format(
	   F,
	   "<light type=\"spotlight\" name=\"~s\" power=\"~.10f\" "++
	   "size=\"~.10f\" beam_falloff=\"~.10f\">\r\n", 
	   [Name, A*10.0, ConeAngle, SpotExponent]),
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    ok = io:format(F, "</light>\r\n\r\n", []);
export_light(_F, Name, Type, _OpenGL) ->
    ok = io:format("Ignoring unknown light \"~s\" type: ~p~n", 
		   [Name, atom_to_list(Type)]).



export_camera(F, Name) ->
    [Aim,Distance,Az,El,{TrackX,TrackY},Fov] =
	wpa:camera_info([aim,distance_to_aim,azimuth,elevation,tracking,fov]),
    Ro = math:pi()/180.0,
    Dist = limit_dist(Distance),
    FocalDist = 0.5 / math:tan(limit_fov(Fov)*0.5*Ro),
    Dy = Dist * math:sin(El*Ro),
    P = Dist * math:cos(El*Ro),
    Dx = -(P * math:sin(Az*Ro)),
    Dz = P * math:cos(Az*Ro),
    Rev = {Dx,Dy,Dz},
    L = e3d_vec:norm_cross(Rev, {0.0,1.0,0.0}),
    LeftN = 
	case e3d_vec:is_zero(L) of
	    false ->
		L;
	    true ->
		{-math:cos(Az*Ro),0.0,-math:sin(Az*Ro)}
	end,
    DownN = e3d_vec:norm_cross(Rev, LeftN),
    Transl = e3d_vec:add(e3d_vec:mul(LeftN, TrackX), 
			 e3d_vec:mul(DownN, TrackY)),
    Pos = e3d_vec:add(Aim, Rev),
    From = e3d_vec:add(Pos, Transl),
    To = e3d_vec:add(Aim, Transl),
    Up = e3d_vec:sub(From, DownN),
    ok = io:format(F, "<camera name=\"~s\" "++
		   "resx=\"320\" resy=\"240\" focal=\"~.10f\">\r\n",
		   [Name,FocalDist]),
    export_pos(F, from, From),
    export_pos(F, to, To),
    export_pos(F, up, Up),
    ok = io:format(F, "</camera>\r\n\r\n", []).

limit_fov(Fov) when Fov < 1.0 -> 1.0;
limit_fov(Fov) when Fov > 179.0 -> 179.0;
limit_fov(Fov) -> Fov.

limit_dist(Dist) when Dist > 0.0 ->
    if Dist < 0.01 -> 0.01;
       true -> Dist
    end;
limit_dist(Dist) when Dist < 0.0 ->
    if Dist > -0.01 -> -0.01;
       true -> Dist
    end;
limit_dist(_) -> 0.01.



export_background(F, Name) ->
    ok = io:format(F, "<background type=\"constant\" "++
		   "name=\"~s\">\r\n", [Name]),
    export_rgb(F, color, {0.7,0.7,0.7,1.0}),
    ok = io:format(F, "</background>\r\n\r\n", []).



export_render(F, CameraName, BackgroundName, Outfile) ->
    ok = io:format(F, "<render camera_name=\"~s\" "++
		   "samples=\"4\" bias=\"0.3\">\r\n", [CameraName]),
    ok = io:format(F, "    <background_name value=\"~s\"/>\r\n",
		   [BackgroundName]),
    ok = io:format(F, "    <outfile value=\"~s\"/>\r\n", [Outfile]),
    ok = io:format(F, "</render>\r\n\r\n", []).



count_equal([H|T]) ->
    count_equal(T, 1, H, []).

count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).
