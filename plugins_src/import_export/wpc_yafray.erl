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
%%     $Id: wpc_yafray.erl,v 1.3 2003/01/17 23:16:07 raimo_niskanen Exp $
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
    case open(Filename, export) of
	{error,_}=Error -> 
	    Error;
	{ok,F} ->
	    CameraName = "WingsDefaultCamera",
	    ConstBackgroundName = "WingsDefaultConstBackground",
	    Basename = filename:basename(Filename),
	    Outfile = filename:rootname(Basename)++".tga",
	    %%
	    Lights = proplists:get_value(lights, Props, []),
	    println(F, "<!-- ~s: Exported from ~s -->~n"++
		    "~n"++
		    "<scene>", [Basename, Creator]),
	    %%
	    section(F, "Textures"),
	    template(F, fun() -> 
				 export_texture(F, jpeg, 
						"WingsTemplateJpegTexture") 
			 end),
	    println(F),
	    template(F, fun() -> 
				 export_texture(F, clouds, 
						"WingsTemplateCloudsTexture") 
			 end),
	    %%
	    section(F, "Shaders"),
	    foreach(fun ({Name, Mat}) -> 
			    export_shader(F, Name, Mat),
			    println(F)
		    end, 
		    Mats),
	    %%
	    section(F, "Objects"),
	    foreach(fun (#e3d_object{name=NameStr,obj=Mesh}) ->
			    export_object(F, NameStr, Mesh),
			    println(F)
		    end,
		    Objs),
	    %%
	    section(F, "Lights"),
	    foreach(fun (Light) -> 
			    export_light(F, Light),
			    println(F)
		    end,
		    Lights),
	    %%
	    section(F, "Background, Camera, Filter and Render"),
	    export_background(F, constant, ConstBackgroundName),
	    println(F),
	    export_background(F, sunsky, "WingsDefaultSunskyBackground"),
	    println(F),
	    export_camera(F, CameraName),
	    println(F),
	    export_render(F, CameraName, ConstBackgroundName, Outfile),
	    %%
	    println(F),
	    println(F, "</scene>"),
	    close(F)
    end.



template(F, Fun_0) ->
    println(F, "<!-- Begin Template"),
    Fun_0(),
    println(F, "End Template -->").

section(F, Name) ->
    println(F, [io_lib:nl(),"<!-- Section ",Name," -->",io_lib:nl()]).



export_texture(F, jpeg, Name) ->
    println(F, "<texture type=\"jpeg\" name=\"~s\">~n"++
	    "    <filename value=\"~s.jpg\"/>~n"++
	    "</texture>", [Name,Name]);
export_texture(F, clouds, Name) ->
    println(F, "<texture type=\"clouds\" name=\"~s\">~n"++
	    "    <depth value=\"2\"/>", [Name]),
    export_rgb(F, color1, {0.0,0.0,0.0,1.0}),
    export_rgb(F, color2, {1.0,1.0,1.0,1.0}),
    println(F, "</texture>").



export_shader(F, Name, Mat) ->
    OpenGL = proplists:get_value(opengl, Mat),
    println(F, "<shader type=\"generic\" name=\"~s\">~n"++ 
	    "    <attributes>", [atom_to_list(Name)]),
    {Dr,Dg,Db,Opacity} = proplists:get_value(diffuse, OpenGL),
    Transparency = 1 - Opacity,
    export_rgb(F, color, 
	       {Dr*Opacity,Dg*Opacity,Db*Opacity,1.0}),
    export_rgb(F, specular, proplists:get_value(specular, OpenGL)),
    println(F, "        <hard value=\"~.10f\"/>", 
		   [proplists:get_value(shininess, OpenGL)*100.0]),
    export_rgb(F, reflected, proplists:get_value(ambient, OpenGL)),
    export_rgb(F, transmited, 
	       {Dr*Transparency,Dg*Transparency,Db*Transparency,1.0}),
    println(F, "        <min_refle value=\"0.0\"/>~n"++
	    "        <IOR value=\"1.0\"/>~n"++
	    "    </attributes>~n", []),
    template(F, fun() -> export_modulator(F, "WingsTemplateModulator") end),
    println(F, "</shader>").

export_modulator(F, Texname) ->
    println(F, "        <modulator texname=\"~s\" mode=\"mix\""++
	    "                   sizex=\"1.0\" sizey=\"1.0\" sizez=\"1.0\">~n"++
	    "            <color value=\"0.0\"/>~n"++
	    "            <specular value=\"0.0\"/>~n"++
	    "            <hard value=\"0.0\"/>~n"++
	    "            <transmission value=\"0.0\"/>~n"++
	    "            <reflection value=\"0.0\"/>~n"++
	    "        </modulator>", [Texname]).



export_rgb(F, Type, {R,G,B,_}) ->
    println(F, "        <~s r=\"~.10f\" g=\"~.10f\" b=\"~.10f\"/>", 
	    [atom_to_list(Type),R,G,B]).



export_object(F, NameStr, #e3d_mesh{}=Mesh) ->
    #e3d_mesh{fs=Fs,vs=Vs} = e3d_mesh:triangulate(Mesh),
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    [{_Count,DefaultMaterial}|_] = reverse(sort(count_equal(MM))),
    println(F, "<object name=\"~s\" shader_name=\"~s\" "++
	    "shadow=\"on\" caus_IOR=\"1.0\"~n"++
	    "        emit_rad=\"on\" recv_rad=\"on\">~n"++
	    "    <attributes>",
	    [NameStr, atom_to_list(DefaultMaterial)]),
    export_rgb(F, caus_rcolor, {0.0,0.0,0.0,1.0}),
    export_rgb(F, caus_tcolor, {0.0,0.0,0.0,1.0}),
    println(F, "    </attributes>~n"++
	    "    <mesh>", []),
    template(F, fun () -> println(F, "    <mesh autosmooth=\"50.0\">") end),
    println(F, "        <points>"),
    export_vertices(F, Vs),
    println(F, "        </points>~n"++
	    "        <faces>", []),
    export_faces(F, Fs, DefaultMaterial),
    println(F, "        </faces>~n"++
	    "    </mesh>~n"++
	    "</object>", []).

export_vertices(_F, []) ->
    ok;
export_vertices(F, [Pos|T]) ->
    export_pos(F, p, Pos),
    export_vertices(F, T).



export_pos(F, Type, {X,Y,Z}) ->
    println(F, "        <~s x=\"~.10f\" y=\"~.10f\" z=\"~.10f\"/>",
		   [atom_to_list(Type),X,Y,Z]).



export_faces(_F, [], _DefMat) ->
    ok;
export_faces(F, [#e3d_face{vs=[A,B,C],mat=[Mat|_]}|T], DefaultMaterial) ->
    case Mat of
	DefaultMaterial ->
	    println(F, "        <f a=\"~w\" b=\"~w\" c=\"~w\"/>", [A,B,C]);
	_ ->
	    println(F, "        <f a=\"~w\" b=\"~w\" c=\"~w\" "++
		    " shader_name=\"~s\"/>", [A,B,C,atom_to_list(Mat)])
    end,
    export_faces(F, T, DefaultMaterial).



export_light(F, {Name,Ps}) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    export_light(F, Name, Type, OpenGL).

export_light(F, Name, point, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    println(F,"<light type=\"pointlight\" name=\"~s\" "++
	    "power=\"1.0\" cast_shadows=\"on\">", [Name]),
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),
    println(F, "</light>~n~n", []);
export_light(F, Name, infinite, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    println(F,"<light type=\"sunlight\" name=\"~s\" "++
	    "power=\"1.0\" cast_shadows=\"on\">", [Name]),
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),
    println(F, "</light>");
export_light(F, Name, spot, OpenGL) ->
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, 45.0),
    SpotExponent = proplists:get_value(spot_exponent, OpenGL, 2.0),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    println(F, "<light type=\"spotlight\" name=\"~s\" "++
	    "power=\"1.0\" cast_shadows=\"on\"~n"++
	    "       size=\"~.10f\" beam_falloff=\"~.10f\">", 
	    [Name, ConeAngle, SpotExponent]),
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    println(F, "</light>");
export_light(_F, Name, Type, _OpenGL) ->
    io:format("Ignoring unknown light \"~s\" type: ~p~n", 
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
    println(F, "<camera name=\"~s\" "++
	    "resx=\"640\" resy=\"480\" focal=\"~.10f\">",
	    [Name,FocalDist]),
    export_pos(F, from, From),
    export_pos(F, to, To),
    export_pos(F, up, Up),
    println(F, "</camera>").

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
    println(F, "<background type=\"constant\" name=\"~s\">", [Name]),
    export_rgb(F, color, {0.0,0.0,0.0,1.0}),
    println(F, "</background>");
export_background(F, sunsky, Name) ->
    println(F, "<background type=\"sunsky\" name=\"~s\"~n"++
	    "            turbidity=\"4.0\" add_sun=\"off\">", [Name]),
    export_pos(F, from, {1.0,1.0,1.0}),
    println(F, "</background>").



export_render(F, CameraName, BackgroundName, Outfile) ->
    println(F, "<render camera_name=\"~s\" "++
	    "samples=\"1\" raydepth=\"3\"~n"++
	    "        bias=\"0.3\" tolerance=\"0.1\">~n"++
	    "    <background_name value=\"~s\"/>~n"++
	    "    <outfile value=\"~s\"/>~n"++
	    "    <indirect_samples value=\"0\"/>~n"++
	    "    <indirect_power value=\"1.0\"/>~n"++
	    "    <exposure value=\"~.10f\"/>~n"++
	    "    <gamma value=\"1.0\"/>~n"++
	    "    <fog_density value=\"0.0\"/>",
	    [CameraName,BackgroundName,Outfile,math:sqrt(2.0)]),
    export_rgb(F, fog_color, {1.0,1.0,1.0,1.0}),
    println(F, "</render>").



open(Filename, export) ->
    file:open(Filename, [write,raw,{delayed_write,65536,2000}]).

println(F) ->
    println(F, "").

println(F, String) ->
    case file:write(F, [String,io_lib:nl()]) of
	ok ->
	    ok;
	Error ->
	    erlang:fault(Error, [F,String])
    end.

println(F, Format, Args) ->
    case file:write(F, [io_lib:format(Format, Args),io_lib:nl()]) of
	ok ->
	    ok;
	Error ->
	    erlang:fault(Error, [F,Format,Args])
    end.

close(F) ->
    case file:close(F) of
	ok ->
	    ok;
	Error ->
	    erlang:fault(Error, [F])
    end.



count_equal([H|T]) ->
    count_equal(T, 1, H, []).

count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).
