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
%%     $Id: wpc_yafray.erl,v 1.2 2003/01/17 14:24:36 raimo_niskanen Exp $
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
	    ConstBackgroundName = "WingsDefaultConstBackground",
	    Outfile = filename:rootname(Filename)++".tga",
	    %%
	    Lights = proplists:get_value(lights, Props, []),
	    ok = io:format(F, "<!-- ~s: Exported from ~s -->~n~n", 
			   [Filename, Creator]),
	    ok = io:format(F, "<scene>~n", []),
	    decomment(F, fun() -> 
				 export_texture(F, jpeg, 
						"WingsTemplateJpegTexture") 
			 end),
	    decomment(F, fun() -> 
				 export_texture(F, clouds, 
						"WingsTemplateCloudsTexture") 
			 end),
	    foreach(fun ({Name, Mat}) -> 
			    export_shader(F, Name, Mat) 
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
	    export_background(F, constant, ConstBackgroundName),
	    export_background(F, sunsky, "WingsDefaultSunskyBackground"),
	    export_render(F, CameraName, ConstBackgroundName, Outfile),
	    ok = io:format(F, "</scene>~n", []),
	    ok = file:close(F)
    end.



decomment(F, Fun_0) ->
    ok = io:format(F, "<!--~n", []),
    Fun_0(),
    ok = io:format(F, " -->~n~n", []).



export_texture(F, jpeg, Name) ->
    ok = io:format(F, "<texture type=\"jpeg\" name=\"~s\">~n", [Name]),
    ok = io:format(F, "    <filename value=\"~s.jpg\"/>~n", [Name]),
    ok = io:format(F, "</texture>~n~n", []);
export_texture(F, clouds, Name) ->
    ok = io:format(F, "<texture type=\"clouds\" name=\"~s\">~n", [Name]),
    ok = io:format(F, "    <depth value=\"2\"/>~n", []),
    export_rgb(F, color1, {0.0,0.0,0.0,1.0}),
    export_rgb(F, color2, {1.0,1.0,1.0,1.0}),
    ok = io:format(F, "</texture>~n~n", []).



export_shader(F, Name, Mat) ->
    OpenGL = proplists:get_value(opengl, Mat),
    ok = io:format(F, "<shader type=\"generic\" name=\"~s\">~n", 
		   [atom_to_list(Name)]),
    ok = io:format(F, "    <attributes>~n", []),
    {Dr,Dg,Db,Opacity} = proplists:get_value(diffuse, OpenGL),
    Transparency = 1 - Opacity,
    export_rgb(F, color, 
	       {Dr*Opacity,Dg*Opacity,Db*Opacity,1.0}),
    export_rgb(F, specular, proplists:get_value(specular, OpenGL)),
    ok = io:format(F, "        <hard value=\"~.10f\"/>~n", 
		   [proplists:get_value(shininess, OpenGL)*100.0]),
    export_rgb(F, reflected, proplists:get_value(ambient, OpenGL)),
    export_rgb(F, transmited, 
	       {Dr*Transparency,Dg*Transparency,Db*Transparency,1.0}),
    ok = io:format(F, "        <min_refle value=\"0.0\"/>~n", []),
    ok = io:format(F, "        <IOR value=\"1.0\"/>~n", []),
    ok = io:format(F, "    </attributes>~n", []),
    decomment(F, fun() -> export_modulator(F, "WingsTemplateModulator") end),
    ok = io:format(F, "</shader>~n~n", []).

export_modulator(F, Texname) ->
    ok = io:format(F, "        <modulator texname=\"~s\" "++
		   "mode=\"mix\" size=\"1.0\">~n", [Texname]),
    ok = io:format(F, "            <color value=\"0.0\"/>~n", []),
    ok = io:format(F, "            <specular value=\"0.0\"/>~n", []),
    ok = io:format(F, "            <hard value=\"0.0\"/>~n", []),
    ok = io:format(F, "            <transmission value=\"0.0\"/>~n", []),
    ok = io:format(F, "            <reflection value=\"0.0\"/>~n", []),
    ok = io:format(F, "        </modulator>~n", []).



export_rgb(F, Type, {R,G,B,_}) ->
    ok = io:format(F,
		   "        <~s r=\"~.10f\" g=\"~.10f\" b=\"~.10f\"/>~n", 
		   [atom_to_list(Type),R,G,B]).



export_object(F, NameStr, #e3d_mesh{}=Mesh) ->
    #e3d_mesh{fs=Fs,vs=Vs} = e3d_mesh:triangulate(Mesh),
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    [{_Count,DefaultMaterial}|_] = reverse(sort(count_equal(MM))),
    ok = io:format(F, "<object name=\"~s\" shader_name=\"~s\" "++
		   "shadow=\"on\" caus_IOR=\"1.0\" "++
		   "emit_rad=\"on\" recv_rad=\"on\">~n",
		   [NameStr, atom_to_list(DefaultMaterial)]),
    ok = io:format(F, "    <attributes>~n", []),
    export_rgb(F, caus_rcolor, {0.0,0.0,0.0,1.0}),
    export_rgb(F, caus_tcolor, {0.0,0.0,0.0,1.0}),
    ok = io:format(F, "    </attributes>~n", []),
    ok = io:format(F, "    <mesh><!-- <mesh autosmooth=\"50.0\"> -->~n", []),
    ok = io:format(F, "        <points>~n", []),
    export_vertices(F, Vs),
    ok = io:format(F, "        </points>~n", []),
    ok = io:format(F, "        <faces>~n", []),
    export_faces(F, Fs, DefaultMaterial),
    ok = io:format(F, "        </faces>~n", []),
    ok = io:format(F, "    </mesh>~n", []),
    ok = io:format(F, "</object>~n~n", []),
    ok.

export_vertices(_F, []) ->
    ok;
export_vertices(F, [Pos|T]) ->
    export_pos(F, p, Pos),
    export_vertices(F, T).



export_pos(F, Type, {X,Y,Z}) ->
    ok = io:format(F, "        <~s x=\"~.10f\" y=\"~.10f\" z=\"~.10f\"/>~n",
		   [atom_to_list(Type),X,Y,Z]).



export_faces(_F, [], _DefMat) ->
    ok;
export_faces(F, [#e3d_face{vs=[A,B,C],mat=[Mat|_]}|T], DefaultMaterial) ->
    case Mat of
	DefaultMaterial ->
	    ok = io:format(F, "        <f a=\"~w\" b=\"~w\" c=\"~w\"/>~n",
			   [A,B,C]);
	_ ->
	    ok = io:format(F, "        <f a=\"~w\" b=\"~w\" c=\"~w\" "++
			   " shader_name=\"~s\"/>~n",
			   [A,B,C,atom_to_list(Mat)])
    end,
    export_faces(F, T, DefaultMaterial).



export_light(F, {Name,Ps}) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    export_light(F, Name, Type, OpenGL).

export_light(F, Name, point, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    ok = io:format(F,"<light type=\"pointlight\" name=\"~s\" "++
		   "power=\"1.0\" cast_shadows=\"on\">~n", 
		   [Name]),
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),
    ok = io:format(F, "</light>~n~n", []);
export_light(F, Name, infinite, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    ok = io:format(F,"<light type=\"sunlight\" name=\"~s\" "++
		   "power=\"1.0\" cast_shadows=\"on\">~n", 
		   [Name]),
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),
    ok = io:format(F, "</light>~n~n", []);
export_light(F, Name, spot, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, 45.0),
    SpotExponent = proplists:get_value(spot_exponent, OpenGL, 2.0),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    ok = io:format(F,"<light type=\"spotlight\" name=\"~s\" "++
		   "power=\"1.0\" cast_shadows=\"on\" "++
		   "size=\"~.10f\" beam_falloff=\"~.10f\">~n", 
		   [Name, ConeAngle, SpotExponent]),
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    ok = io:format(F, "</light>~n~n", []);
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
		   "resx=\"640\" resy=\"480\" focal=\"~.10f\">~n",
		   [Name,FocalDist]),
    export_pos(F, from, From),
    export_pos(F, to, To),
    export_pos(F, up, Up),
    ok = io:format(F, "</camera>~n~n", []).

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



export_background(F, constant, Name) ->
    ok = io:format(F, "<background type=\"constant\" name=\"~s\">~n", [Name]),
    export_rgb(F, color, {0.0,0.0,0.0,1.0}),
    ok = io:format(F, "</background>~n~n", []);
export_background(F, sunsky, Name) ->
    ok = io:format(F, "<background type=\"sunsky\" name=\"~s\" "++
		   "turbidity=\"4.0\" add_sun=\"off\">~n", [Name]),
    export_pos(F, from, {1.0,1.0,1.0}),
    ok = io:format(F, "</background>~n~n", []).



export_render(F, CameraName, BackgroundName, Outfile) ->
    ok = io:format(F, "<render camera_name=\"~s\" "++
		   "samples=\"1\" raydepth=\"3\" "++
		   "bias=\"0.3\" tolerance=\"0.1\">~n", [CameraName]),
    ok = io:format(F, "    <background_name value=\"~s\"/>~n",
		   [BackgroundName]),
    ok = io:format(F, "    <outfile value=\"~s\"/>~n", [Outfile]),
    ok = io:format(F, "    <indirect_samples value=\"0\"/>~n", []),
    ok = io:format(F, "    <indirect_power value=\"1.0\"/>~n", []),
    ok = io:format(F, "    <exposure value=\"~.10f\"/>~n", [math:sqrt(2.0)]),
    ok = io:format(F, "    <gamma value=\"1.0\"/>~n", []),
    ok = io:format(F, "    <fog_density value=\"0.0\"/>~n", []),
    export_rgb(F, fog_color, {1.0,1.0,1.0,1.0}),
    ok = io:format(F, "</render>~n~n", []).



count_equal([H|T]) ->
    count_equal(T, 1, H, []).

count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).
