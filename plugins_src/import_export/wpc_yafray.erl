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
%%     $Id: wpc_yafray.erl,v 1.5 2003/01/20 23:04:19 raimo_niskanen Exp $
%%

-module(wpc_yafray).

-export([init/0,menu/2,dialog/2,command/2]).

-include("e3d.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,keydelete/3,
		foreach/2,map/2,foldl/3]).

-define(TAG, yafray).

%% Default values
-define(DEF_IOR, 1.0).
-define(DEF_MIN_REFLE, 0.0).
-define(DEF_SAMPLES, 1).
-define(DEF_RAYDEPTH, 3).
-define(DEF_BIAS, 0.1).
-define(DEF_TOLERANCE, 0.1).
-define(DEF_WIDTH, 100).
-define(DEF_HEIGHT, 100).



init() ->
    true.

menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) ->
    Menu.

menu_entry(Menu) ->
    Menu ++ [{"YafRay (.xml)",?TAG,[option]}].

command({file,{export,{?TAG,A}}}, St) ->
    command_file(export, A, St);
command({file,{export_selected,{?TAG,A}}}, St) ->
    command_file(export_selected, A, St);
command(_, _St) ->
    next.

command_file(Op, Attr, St) when is_list(Attr) ->
    set_pref(Attr),
    wpa:Op(props(), fun_export_2(attr(St, Attr)), St);
command_file(Op, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "YafRay Export Options", export_dialog(),
	       fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).
    

props() ->
    [{ext,".xml"},{ext_desc,"YafRay File"}].

attr(St, Attr) ->
    [{lights,wpa:lights(St)}|Attr].

fun_export_2(Props) ->
    fun (Filename, Contents) ->
	    export(Props, Filename, Contents)
    end.



dialog({material_editor_setup,_Name,Mat}, Dialog) ->
    YafRay = proplists:get_value(?TAG, Mat, []),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    Dialog ++ [{hframe,
		[{vframe, [{label,"Index Of Reflection"},
			   {label,"Minimum Reflection"}]},
		 {vframe, [{text,IOR,
				  [{range,{0.0,100.0}},
				   {key,ior}]},
			   {slider,{text,MinRefle,
				    [{range,{0.0,1.0}},
				     {key,min_refle}]}}]}],
		[{title,"YafRay Options"}]}];
dialog({material_editor_result,_Name,Mat0}, [A,B|Res]) ->
    Mat = [{?TAG,[A,B]}|keydelete(?TAG, 1, Mat0)],
    {Mat,Res};
dialog({light_editor_setup,_Name,Ps}, Dialog) ->
    YafRay = proplists:get_value(?TAG, Ps, []),
    Power = proplists:get_value(power, YafRay, 1.0),
    Dialog ++ [{hframe,
		[{vframe, [{label,"Power"}]},
		 {vframe, [{slider,{text,Power,
				    [{range,{0.0,1.0}},
				     {key,power}]}}]}],
		[{title,"YafRay Options"}]}];
dialog({light_editor_result,_Name,Ps0}, [A|Res]) ->
    Ps = [{?TAG,[A]}|keydelete(?TAG, 1, Ps0)],
    {Ps,Res};
dialog(_X, Dialog) ->
    io:format("~p\n", [{_X,Dialog}]),
    Dialog.



export_dialog() ->
    Samples = get_pref(samples, ?DEF_SAMPLES),
    Raydepth = get_pref(raydepth, ?DEF_RAYDEPTH),
    Bias = get_pref(bias, ?DEF_BIAS),
    Tolerance = get_pref(tolerance, ?DEF_TOLERANCE),
    Width = get_pref(width, ?DEF_WIDTH),
    Height = get_pref(height, ?DEF_HEIGHT),
    [{hframe,
      [{vframe,[{label,"Samples"},
		{label,"Raydepth"}]},
       {vframe,[{text,Samples,[{range,{1,1000}},{key,samples}]},
		{text,Raydepth,[{range,{1,1000}},{key,raydepth}]}]},
       {vframe,[{label,"Bias"},
		{label,"Tolerance"}]},
       {vframe,[{text,Bias,[{range,{0.0,1.0}},{key,bias}]},
		{text,Tolerance,[{range,{0.0,100.0}},{key,tolerance}]}]}],
      [{title,"Rendering"}]},
     {hframe,
      [{vframe,[{label,"Width"}]},
       {vframe,[{text,Width,[{range,{1,10000}},{key,width}]}]},
       {vframe,[{label,"Height"}]},
       {vframe,[{text,Height,[{range,{1,10000}},{key,height}]}]}],
      [{title,"Camera"}]}].



export(Attr, Filename, #e3d_file{objs=Objs,mat=Mats,creator=Creator}) ->
    case open(Filename, export) of
	{error,_}=Error -> 
	    Error;
	{ok,F} ->
	    CameraName = "WingsDefaultCamera",
	    ConstBackgroundName = "WingsDefaultConstBackground",
	    Basename = filename:basename(Filename),
	    Outfile = filename:rootname(Basename)++".tga",
	    %%
	    Lights = proplists:get_value(lights, Attr, []),
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
	    export_camera(F, CameraName, Attr),
	    println(F),
	    export_render(F, CameraName, ConstBackgroundName, Outfile, Attr),
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
    YafRay = proplists:get_value(?TAG, Mat, []),
    println(F, "<shader type=\"generic\" name=\"~s\">~n"++ 
	    "    <attributes>", [atom_to_list(Name)]),
    {Dr,Dg,Db,Opacity} = proplists:get_value(diffuse, OpenGL),
    Transparency = 1 - Opacity,
    export_rgb(F, color, 
	       {Dr*Opacity,Dg*Opacity,Db*Opacity,1.0}),
    export_rgb(F, specular, proplists:get_value(specular, OpenGL)),
    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "        <hard value=\"~.10f\"/>", 
		   [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, reflected, proplists:get_value(ambient, OpenGL)),
    export_rgb(F, transmited, 
	       {Dr*Transparency,Dg*Transparency,Db*Transparency,1.0}),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    println(F, "        <IOR value=\"~.10f\"/>~n"++
	    "        <min_refle value=\"~.10f\"/>~n"++
	    "    </attributes>~n", [IOR, MinRefle]),
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



export_camera(F, Name, Attr) ->
    [Aim,Distance,Az,El,{TrackX,TrackY},Fov] =
	wpa:camera_info([aim,distance_to_aim,azimuth,elevation,tracking,fov]),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    Ro = math:pi()/180.0,
    Dist = limit_dist(Distance),
    FocalDist = (0.5 / math:tan(limit_fov(Fov)*0.5*Ro)) * (Height / Width),
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
	    "resx=\"~w\" resy=\"~w\" focal=\"~.10f\">",
	    [Name,Width,Height,FocalDist]),
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



export_render(F, CameraName, BackgroundName, Outfile, Attr) ->
    Samples = proplists:get_value(samples, Attr),
    Raydepth = proplists:get_value(raydepth, Attr),
    Bias = proplists:get_value(bias, Attr),
    Tolerance = proplists:get_value(tolerance, Attr),
    println(F, "<render camera_name=\"~s\" "++
	    "samples=\"~w\" raydepth=\"~w\"~n"++
	    "        bias=\"~.10f\" tolerance=\"~.10f\">~n"++
	    "    <background_name value=\"~s\"/>~n"++
	    "    <outfile value=\"~s\"/>~n"++
	    "    <indirect_samples value=\"0\"/>~n"++
	    "    <indirect_power value=\"1.0\"/>~n"++
	    "    <exposure value=\"~.10f\"/>~n"++
	    "    <gamma value=\"1.0\"/>~n"++
	    "    <fog_density value=\"0.0\"/>",
	    [CameraName,Samples,Raydepth,Bias,Tolerance,
	     BackgroundName,Outfile,math:sqrt(2.0)]),
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



set_pref(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).



count_equal([H|T]) ->
    count_equal(T, 1, H, []).

count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).
