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
%%     $Id: wpc_yafray.erl,v 1.26 2003/04/17 14:13:22 raimo_niskanen Exp $
%%

-module(wpc_yafray).

-export([init/0,menu/2,dialog/2,command/2]).



-include_lib("kernel/include/file.hrl").

-include("e3d.hrl").
-include("e3d_image.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,keydelete/3,
		foreach/2,map/2,foldl/3]).

-define(TAG, yafray).
-define(TAG_RENDER, yafray_render).

%%% Default values

-define(DEF_DIALOGS, auto).
-define(DEF_RENDERER, "yafray").
-define(DEF_OPTIONS, "").
-define(DEF_LOAD_IMAGE, true).

%% Shader
-define(DEF_IOR, 1.0).
-define(DEF_MIN_REFLE, 0.0).
-define(DEF_AUTOSMOOTH_ANGLE, 0.0).

%% Render
-define(DEF_AA_PASSES, 1).
-define(DEF_RAYDEPTH, 3).
-define(DEF_BIAS, 0.1).
-define(DEF_AA_THRESHOLD, 0.1).
-define(DEF_WIDTH, 100).
-define(DEF_HEIGHT, 100).

%% Light
-define(DEF_ATTN_POWER, 10.0).
-define(DEF_POINT_TYPE, pointlight).
-define(DEF_CAST_SHADOWS, true).
%% Spotlight
-define(DEF_SPOT_TYPE, spotlight).
-define(DEF_CONE_ANGLE, 45.0).
-define(DEF_SPOT_EXPONENT, 2.0).
-define(DEF_BLEND, 5.0).
%% Photonlight
-define(DEF_MODE,diffuse).
-define(DEF_PHOTONS,5000).
-define(DEF_SEARCH,50).
-define(DEF_DEPTH,3).
-define(DEF_MINDEPTH,1).
-define(DEF_FIXEDRADIUS,1.0).
-define(DEF_CLUSTER,1.0).
%% Softlight
-define(DEF_RES, 100).
-define(DEF_RADIUS, 1).
%% Sunlight
-define(DEF_POWER, 1.0).
-define(DEF_BACKGROUND, undefined).
-define(DEF_BACKGROUND_COLOR, {0.0,0.0,0.0}).
-define(DEF_TURBIDITY, 4.0).

%% Modulator
-define(DEF_MOD_MODE, off).
-define(DEF_MOD_SIZE_X, 1.0).
-define(DEF_MOD_SIZE_Y, 1.0).
-define(DEF_MOD_SIZE_Z, 1.0).
-define(DEF_MOD_OPACITY, 1.0).
-define(DEF_MOD_DIFFUSE, 0.0).
-define(DEF_MOD_SPECULAR, 0.0).
-define(DEF_MOD_AMBIENT, 0.0).
-define(DEF_MOD_SHININESS, 0.0).
-define(DEF_MOD_TYPE, image).
-define(DEF_MOD_FILENAME, ".jpg").
-define(DEF_MOD_COLOR1, {0.0,0.0,0.0,1.0}).
-define(DEF_MOD_COLOR2, {1.0,1.0,1.0,1.0}).
-define(DEF_MOD_DEPTH, 2).



%% Exported plugin callback functions
%%

init() ->
    init_pref(),
    set_var(rendering, false),
    true.

menu({file,export}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,export_selected}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,render}, Menu) ->
    maybe_append(render, Menu, menu_entry(render));
menu({edit,plugin_preferences}, Menu) ->
    Menu++menu_entry(pref);
menu(_, Menu) ->
    Menu.

command({file,{export,{?TAG,A}}}, St) ->
    command_file(export, A, St);
command({file,{export_selected,{?TAG,A}}}, St) ->
    command_file(export_selected, A, St);
command({file,{render,{?TAG,A}}}, St) ->
    command_file(render, A, St);
command({file,{?TAG_RENDER,Data}}, St) ->
    command_file(?TAG_RENDER, Data, St);
command({edit,{plugin_preferences,?TAG}}, St) ->
    pref_edit(St);
command(_Spec, _St) ->
%    erlang:display({?MODULE,?LINE,Spec}),
    next.

dialog({material_editor_setup,Name,Mat}, Dialog) ->
    maybe_append(edit, Dialog, material_dialog(Name, Mat));
dialog({material_editor_result,Name,Mat}, Res) ->
    material_result(Name, Mat, Res);
dialog({light_editor_setup,Name,Ps}, Dialog) ->
    maybe_append(edit, Dialog, light_dialog(Name, Ps));
dialog({light_editor_result,Name,Ps0}, Res) ->
    light_result(Name, Ps0, Res);
dialog(_X, Dialog) ->
    io:format("~p\n", [{_X,Dialog}]),
    Dialog.

%%
%% End of exported plugin callback functions



init_pref() ->
    Renderer = get_pref(renderer, ?DEF_RENDERER),
    RendererPath =
	case filename:pathtype(Renderer) of
	    absolute -> 
		Renderer;
	    _ -> 
		case find_executable(Renderer) of
		    false ->
			false;
		    Path -> 
			Path
		end
	end,
    case get_pref(dialogs, ?DEF_DIALOGS) of
	auto ->
	    set_var(renderer, RendererPath),
	    set_var(dialogs, case RendererPath of 
				 false -> false; 
				 _ -> true 
			     end);
	enabled ->
	    set_var(renderer, RendererPath),
	    set_var(dialogs, true);
	disabled ->
	    set_var(renderer, false),
	    set_var(dialogs, false)
    end,
    ok.

maybe_append(Conditon, Menu, PluginMenu) ->
    Value =
	case Conditon of
	    export ->
		get_var(dialogs);
	    edit ->
		get_var(dialogs);
	    render ->
		get_var(renderer);
	    _ ->
		false
	end,
    case Value of
	false ->
	    Menu;
	_ ->
	    Menu++PluginMenu
    end.

menu_entry(render) ->
    [{"YafRay (.tga)",?TAG,[option]}];
menu_entry(export) ->
    [{"YafRay (.xml)",?TAG,[option]}];
menu_entry(pref) ->
    [{"YafRay",?TAG}].



command_file(render, Attr, St) when is_list(Attr) ->
    set_pref(Attr),
    case get_var(rendering) of
	false ->
	    wpa:export(props(tga), 
		       fun_export_2(attr(St, [{?TAG_RENDER,true}|Attr])), St);
       _RenderFile ->
	    wpa:error("Already rendering.")
    end;
command_file(render, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "YafRay Render Options", export_dialog(render),
	       fun(Attr) -> {file,{render,{?TAG,Attr}}} end);
command_file(?TAG_RENDER, {Pid,Result,Ack}, _St) ->
    Rendering = set_var(rendering, false),
    Pid ! Ack,
    case Rendering of
	false ->
	    keep;
	RenderFile ->
	    case Result of
		load_image ->
		    io:format("Loading rendered image~n"),
		    load_image(RenderFile);
		ok ->
		    keep;
		_ ->
		    wpa:error("Rendering error")
	    end
    end;
command_file(Op, Attr, St) when is_list(Attr) ->
    set_pref(Attr),
    wpa:Op(props(?TAG), fun_export_2(attr(St, Attr)), St);
command_file(Op, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "YafRay Export Options", export_dialog(export),
	       fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).

props(tga) ->
    [{ext,".tga"},{ext_desc,"Targa File"}];
props(?TAG) ->
    [{ext,".xml"},{ext_desc,"YafRay File"}].

attr(St, Attr) ->
    [{lights,wpa:lights(St)}|Attr].

fun_export_2(Attr) ->
    fun (Filename, Contents) ->
	    case catch export(Attr, Filename, Contents) of
		ok ->
		    ok;
		Error ->
		    io:format("ERROR: Failed to export:~n~p~n", [Error]),
		    {error,"Failed to export"}
	    end
    end.

load_image(Filename) ->
    case wpa:image_read([{filename,Filename},
			 {alignment,1}]) of
	#e3d_image{}=Image ->
	    Id = wings_image:new("Rendered (YafRay)", Image),
	    wings_image:window(Id),
	    keep;
	_ ->
	    wpa:error("No image rendered")
    end.



%%% Dialogues and results
%%%

material_dialog(_Name, Mat) ->
    Maps = proplists:get_value(maps, Mat, []),
    YafRay = proplists:get_value(?TAG, Mat, []),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    AutosmoothAngle = 
	proplists:get_value(autosmooth_angle, YafRay, ?DEF_AUTOSMOOTH_ANGLE),
    Modulators = proplists:get_value(modulators, YafRay, []),
    [{vframe,
      [{hframe,
	[{vframe, [{label,"Index Of Reflection"},
		   {label,"Minimum Reflection"},
		   {label,"Autosmooth Angle"}]},
	 {vframe, [{text,IOR,
		    [{range,{0.0,100.0}},
		     {key,ior}]},
		   {slider,{text,MinRefle,
			    [{range,{0.0,1.0}},
			     {key,min_refle}]}},
		   {slider,{text,AutosmoothAngle,
			    [{range,{0.0,180.0}},
			     {key,autosmooth_angle}]}}]}
	]}|modulator_dialogs(Modulators, 1, Maps)],
      [{title,"YafRay Options"}]}].

material_result(_Name, Mat0, [{ior,_}|_]=Res) ->
    {Ps,Res0} = split_list(Res, 3),
    {Modulators,Res1} = modulator_result(Res0),
    Mat1 = [{?TAG,[{modulators,Modulators}|Ps]}|keydelete(?TAG, 1, Mat0)],
    {Mat1,Res1};
material_result(Name, Mat, Res) ->
    exit({invalid_tag,{?MODULE,?LINE,[Name,Mat,Res]}}).

modulator_dialogs([], _, _Maps) ->
    [{"Create a Modulator",false,[{key,create_modulator}]}];
modulator_dialogs([Modulator|Modulators], M, Maps) ->
    modulator_dialog(Modulator, M, Maps)++
	modulator_dialogs(Modulators, M+1, Maps).

modulator_dialog({modulator,Ps}, M, Maps) when list(Ps) ->
%    erlang:display({?MODULE,?LINE,[Ps,M,Maps]}),
    Mode = proplists:get_value(mode, Ps, ?DEF_MOD_MODE),
    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
    Opacity = proplists:get_value(opacity, Ps, ?DEF_MOD_OPACITY),
    Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
    Ambient = proplists:get_value(ambient, Ps, ?DEF_MOD_AMBIENT),
    Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
    Type = 
	case proplists:get_value(type, Ps, ?DEF_MOD_TYPE) of
	    jpeg -> image;
	    T -> T
	end,
    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    TypeTag = list_to_atom("type"++integer_to_list(M)),
    TaggedType = {TypeTag,Type},
    MapsFrame = 
	case Maps of
	    [] ->
		[];
	    _ ->
		[{hframe,
		  lists:map(fun ({Map,_}) ->
				    {key_alt,TaggedType,atom_to_list(Map),
				     {map,Map}}
			    end, Maps)}]
	end,
    [{vframe,
      [{menu,[{"Delete",delete},
	      {if Mode==off ->"Disabled";
		  true -> "Disable"
	       end,off},
	      {"Mix",mix},{"Mul",mul},{"Add",add}],
	Mode},
       {hframe,[{label,"SizeX"},
		{text,SizeX,[{range,0.0,1000.0}]},
		{label,"SizeY"},
		{text,SizeY,[{range,0.0,1000.0}]},
		{label,"SizeZ"},
		{text,SizeZ,[{range,0.0,1000.0}]}]},
       {hframe,[{vframe,[{label,"Opacity"},
			 {label,"Diffuse "},
			 {label,"Specular"},
			 {label,"Ambient"},
			 {label,"Shininess"}]},
		{vframe,[{slider,{text,Opacity,[{range,{0.0,1.0}}]}},
			 {slider,{text,Diffuse,[{range,{0.0,1.0}}]}},
			 {slider,{text,Specular,[{range,{0.0,1.0}}]}},
			 {slider,{text,Ambient,[{range,{0.0,1.0}}]}},
			 {slider,{text,Shininess,[{range,{0.0,1.0}}]}}]}]}]++
      MapsFrame++
      [{hframe,[{vframe,[{key_alt,TaggedType,"Image",image},
			 {key_alt,TaggedType,"Clouds",clouds}]},
		{vframe,[{hframe,[{label,"Filename"},{text,Filename}]},
			 {hframe,[{label,"Color 1"},{color,Color1},
				  {label,"Color 2"},{color,Color2},
				  {label,"Depth"},
				  {text,Depth,[{range,{1,1000}}]}]}]}]}
      ],
      [{title,"Modulator"}]}];
modulator_dialog(_Modulator, _M, _Maps) ->
    []. % Discard old modulators that anyone may have

modulator_result(Res) ->
    modulator_result(Res, []).

modulator_result([], Ms) ->
    %% Should not happen
    {reverse(Ms), []};
modulator_result([{create_modulator,false}|Res], Ms) ->
    {reverse(Ms),Res};
modulator_result([{create_modulator,true}|Res], Ms) ->
    {reverse(Ms, [{modulator,[]}]),Res};
modulator_result([delete|Res0], Ms) ->
    {_,Res} = modulator(delete, Res0),
    modulator_result(Res, Ms);
modulator_result([Mode|Res0], Ms) ->
    {M,Res} = modulator(Mode, Res0),
    modulator_result(Res, [M|Ms]).

modulator(Mode, [SizeX,SizeY,SizeZ,Opacity,Diffuse,Specular,Ambient,Shininess,
		 {_,Type},Filename,Color1,Color2,Depth|Res]) ->
    Ps = [{mode,Mode},{size_x,SizeX},{size_y,SizeY},{size_z,SizeZ},
		 {opacity,Opacity},{diffuse,Diffuse},{specular,Specular},
		 {ambient,Ambient},{shininess,Shininess},
		 {type,Type},{filename,Filename},
		 {color1,Color1},{color2,Color2},{depth,Depth}],
    {{modulator,Ps},Res}.

light_dialog(Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafRay = proplists:get_value(?TAG, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    DefPower = case Type of
		   point -> ?DEF_ATTN_POWER;
		   spot -> ?DEF_ATTN_POWER;
		   _ -> ?DEF_POWER
	       end,
    Power = proplists:get_value(power, YafRay, DefPower),
    [{vframe,
      [{hframe,[{vframe, [{label,"Power"}]},
		{vframe,[{text,Power,
			  [{range,{0.0,10000.0}},{key,power}]}]}]}|
       light_dialog(Name, Type,YafRay)],
      [{title,"YafRay Options"}]}].

light_dialog(_Name, point, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_POINT_TYPE),
    TypeDef = {type,Type},
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Bias = proplists:get_value(bias, Ps, ?DEF_BIAS),
    Res = proplists:get_value(res, Ps, ?DEF_RES),
    Radius = proplists:get_value(radius, Ps, ?DEF_RADIUS),
    [{hframe,
      [{key_alt,TypeDef,"Pointlight",pointlight},
       {"Cast Shadows",CastShadows,[{key,cast_shadows}]}]},
     {hframe,
      [{key_alt,TypeDef,"Softlight",softlight},
       {vframe,[{label,"Bias"},
		{label,"Res"},
		{label,"Radius"}]},
       {vframe,[{text,Bias,[{range,0.0,1.0},{key,bias}]},
		{text,Res,[{range,0,10000},{key,res}]},
		{text,Radius,[{range,0,10000},{key,radius}]}]}]}];
light_dialog(_Name, spot, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_SPOT_TYPE),
    TypeDef = {type,Type},
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Blend = proplists:get_value(blend, Ps, ?DEF_BLEND),
    Mode = proplists:get_value(mode, Ps, ?DEF_MODE),
    Photons = proplists:get_value(photons, Ps, ?DEF_PHOTONS),
    Depth = proplists:get_value(depth, Ps, ?DEF_DEPTH),
    Fixedradius = proplists:get_value(fixedradius, Ps, ?DEF_FIXEDRADIUS),
    Search = proplists:get_value(search, Ps, ?DEF_SEARCH),
    Mindepth = proplists:get_value(mindepth, Ps, ?DEF_MINDEPTH),
    Cluster = proplists:get_value(cluster, Ps, ?DEF_CLUSTER),
    [{hframe,
      [{key_alt,TypeDef,"Spotlight",spotlight},
       {"Cast Shadows",CastShadows,[{key,cast_shadows}]},
       {label,"Blend"},{text,Blend,[{range,0.0,100.0},{key,blend}]}]},
     {hframe,
      [{key_alt,TypeDef,"Photonlight",photonlight},
       {vframe,
	[{menu,[{"Diffuse",diffuse},{"Caustic",caustic}],Mode,[{key,mode}]},
	 {hframe,[{vframe,[{label,"Photons"},
			   {label,"Depth"},
			   {label,"Fixedradius"}]},
		  {vframe,[{text,Photons,[{range,0,1000000},{key,photons}]},
			   {text,Depth,[{range,1,100},{key,depth}]},
			   {text,Fixedradius,[{range,1.0,1000000.0},
					      {key,fixedradius}]}]},
		  {vframe,[{label,"Search"},
			   {label,"Mindepth"},
			   {label,"Cluster"}]},
		  {vframe,[{text,Search,[{range,0,1000000},{key,search}]},
			   {text,Mindepth,[{range,0,1000000},{key,mindepth}]},
			   {text,Cluster,[{range,0.0,1000000.0},
					  {key,cluster}]}]}]}]}]}];
light_dialog(_Name, infinite, Ps) ->
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND),
    BgDef = {background,Bg},
    BgColor = proplists:get_value(background_color, Ps, ?DEF_BACKGROUND_COLOR),
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Turbidity = proplists:get_value(turbidity, Ps, ?DEF_TURBIDITY),
    [{"Cast Shadows",CastShadows,[{key,cast_shadows}]},
     {hframe,
      [{vframe,[{key_alt,BgDef,"Constant",constant},
		{key_alt,BgDef,"Sunsky",sunsky},
		{key_alt,BgDef,"None", undefined}]},
       {vframe,[{label,"Color"},
		{label,"Turbidity"}]},
       {vframe,[{color,BgColor,[{key,background_color}]},
		{hframe,
		 [{text,Turbidity,[{range,0.0,100.0},{key,turbidity}]}]}]}],
      [{title,"Background"}]}];
light_dialog(_Name, _Type, _Ps) ->
%    erlang:display({?MODULE,?LINE,{_Name,_Type,_Ps}}),
    [].

light_result(_Name, Ps0, [Power|Res0]) ->
    {LightPs,Res1} = light_result(Res0),
    Ps = [{?TAG,[Power|LightPs]}|keydelete(?TAG, 1, Ps0)],
%    erlang:display({?MODULE,?LINE,[Ps,Res1]}),
    {Ps,Res1}.

light_result([{type,pointlight}|_]=Res) ->
    split_list(Res, 5);
light_result([_,{type,softlight}|_]=Res) ->
    split_list(Res, 5);
light_result([{type,spotlight}|_]=Ps) ->
    split_list(Ps, 10);
light_result([_,_,{type,photonlight}|_]=Ps) ->
    split_list(Ps, 10);
light_result([_,{background,_}|_]=Ps) ->
    split_list(Ps, 4);
light_result(Tail) ->
%    erlang:display({?MODULE,?LINE,Tail}),
    {[],Tail}.



pref_edit(St) ->
    Dialogs = get_pref(dialogs, ?DEF_DIALOGS),
    Renderer = get_pref(renderer, ?DEF_RENDERER),
    Dialog =
	[{vframe,[{menu,[{"Disabled Dialogs",disabled},
			 {"Automatic Dialogs",auto},
			 {"Enabled Dialogs",enabled}],
		   Dialogs,[{key,dialogs}]},
		  {hframe,[{label,"Rendering Command"},
			   {text,Renderer,[{key,renderer}]}]}]}],
    wpa:dialog("YafRay Options", Dialog, 
	       fun (Attr) -> pref_result(Attr,St) end).

pref_result(Attr, St) ->
    set_pref(Attr),
    init_pref(),
    St.




export_dialog(Operation) ->
    AA_passes = get_pref(aa_passes, ?DEF_AA_PASSES),
    Raydepth = get_pref(raydepth, ?DEF_RAYDEPTH),
    Bias = get_pref(bias, ?DEF_BIAS),
    AA_threshold = get_pref(aa_threshold, ?DEF_AA_THRESHOLD),
    Width = get_pref(width, ?DEF_WIDTH),
    Height = get_pref(height, ?DEF_HEIGHT),
    BgColor = get_pref(background_color, ?DEF_BACKGROUND_COLOR),
    LoadImage = get_pref(load_image, ?DEF_LOAD_IMAGE),
    Options = get_pref(options, ?DEF_OPTIONS),
    [{hframe,
      [{vframe,[{label,"AA_passes"},
		{label,"AA_threshold"}]},
       {vframe,[{text,AA_passes,[{range,{1,1000}},{key,aa_passes}]},
		{text,AA_threshold,[{range,{0.0,100.0}},
				 {key,aa_threshold}]}]},
       {vframe,[{label,"Raydepth"},
		{label,"Bias"}]},
       {vframe,[{text,Raydepth,[{range,{1,1000}},{key,raydepth}]},
		{text,Bias,[{range,{0.0,1.0}},{key,bias}]}]}],
      [{title,"Render"}]},
     {hframe,
      [{vframe,[{label,"Default Color"}]},
       {vframe,[{color,BgColor,[{key,background_color}]}]}],
      [{title,"Background"}]},
     {hframe,
      [{vframe,[{label,"Width"}]},
       {vframe,[{text,Width,[{range,{1,10000}},{key,width}]}]},
       {vframe,[{label,"Height"}]},
       {vframe,[{text,Height,[{range,{1,10000}},{key,height}]}]}],
      [{title,"Camera"}]}|
     case Operation of render ->
	     [{hframe,[{label,"Options"},
		       {text,Options,[{key,options}]},
		       {"Load Image",LoadImage,[{key,load_image}]}],
	       [{title,"Rendering Job"}]}];
	 export ->
	     []
     end].



%%% Export and rendering functions
%%%

export(Attr, Filename, #e3d_file{objs=Objs,mat=Mats,creator=Creator}) ->
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    {ExportFile,RenderFile} =
	case Render of
	    true ->
		{A,B,C} = erlang:now(),
		{lists:flatten(io_lib:format("~s-~s.~w.~w.~w.xml",
					     [Filename,os:getpid(),A,B,C])),
		 Filename};
	    false ->
		{Filename,
		 filename:rootname(filename:basename(Filename))++".tga"}
	end,
    ExportDir = filename:dirname(ExportFile),
    F = open(ExportFile, export),
    CameraName = "x_Camera",
    ConstBgName = "x_ConstBackground",
    %%
    Lights = proplists:get_value(lights, Attr, []),
    println(F, "<!-- ~s: Exported from ~s -->~n"++
	    "~n"++
	    "<scene>", [filename:basename(ExportFile), Creator]),
    %%
    section(F, "Shaders"),
    foreach(fun ({Name, Mat}) -> 
		    export_shader(F, "w_"++format(Name), Mat,
				  ExportDir),
		    println(F)
	    end, 
	    Mats),
    %%
    section(F, "Objects"),
    foreach(fun (#e3d_object{name=Name,obj=Mesh}) ->
		    export_object(F, "w_"++format(Name), Mesh, Mats),
		    println(F)
	    end,
	    Objs),
    %%
    section(F, "Lights"),
    BgLights = 
	reverse(
	  foldl(fun ({Name, Ps}=Light, Bgs) -> 
			Bg = export_light(F, "w_"++format(Name), Ps),
			println(F),
			case Bg of
			    undefined -> Bgs;
			    _ -> [Light|Bgs]
			end
		end, [], Lights)),
    %%
    section(F, "Background, Camera, Filter and Render"),
    warn_multiple_backgrounds(BgLights),
    BgName = 
	case BgLights of
	    [] ->
		BgColor = proplists:get_value(background_color, Attr,
					      ?DEF_BACKGROUND_COLOR),
		Ps = [{?TAG,[{background,constant},
			     {background_color,BgColor}]}],
		export_background(F, ConstBgName, Ps),
		ConstBgName;
	    [{Name,Ps}|_] ->
		N = "w_"++format(Name),
		export_background(F, N, Ps),
		N
	end,
    println(F),
    export_camera(F, CameraName, Attr),
    println(F),
    export_render(F, CameraName, BgName, filename:basename(RenderFile), Attr),
    %%
    println(F),
    println(F, "</scene>"),
    close(F),
    Renderer = get_var(renderer),
    Options = proplists:get_value(options,Attr,?DEF_OPTIONS),
    LoadImage = proplists:get_value(load_image,Attr,?DEF_LOAD_IMAGE),
    case {Renderer,Render} of
	{false,_} ->
	    ok;
	{_,false} ->
	    ok;
	_ ->
	    Parent = self(),
	    spawn_link(
	      fun () -> 
		      file:delete(RenderFile),
		      render(Renderer, Options, ExportFile, 
			     Parent, LoadImage) 
	      end),
	    set_var(rendering, RenderFile),
	    ok
    end.

warn_multiple_backgrounds([]) ->
    ok;
warn_multiple_backgrounds([_]) ->
    ok;
warn_multiple_backgrounds(BgLights) ->
    io:format("WARNING: Multiple backgrounds - ", []),
    foreach(fun ({Name,_}) ->
		    io:put_chars([format(Name), $ ])
	    end, BgLights),
    io:nl(),
    ok.



render(Renderer, Options, Filename, Parent, LoadImage) ->
    process_flag(trap_exit, true),
    Dirname = filename:dirname(Filename),
    Basename = filename:basename(Filename),
    Cmd = Renderer++" "++Options++" "++Basename,
    PortOpts = [{line,8},{cd,Dirname},eof,exit_status,stderr_to_stdout],
    io:format("Rendering Job started ~p:~n>~s~n", [self(),Cmd]),
    case catch open_port({spawn,Cmd}, PortOpts) of
	Port when port(Port) ->
	    Result = render_job(Port),
	    render_done(Filename, Parent, Result, LoadImage);
	{'EXIT',Reason} ->
	    render_done(Filename, Parent, {error,Reason}, LoadImage)
    end.

render_done(Filename, Parent, ExitStatus, LoadImage) ->
    Status =
	case ExitStatus of
	    0 ->
		ok;
	    undefined ->
		ok;
	    _ ->
		ExitStatus
	end,
    Result =
	case {Status,LoadImage} of
	    {ok,true} ->
		load_image;
	    {ok,false} ->
		ok;
	    {{error,_},_} ->
		Status;
	    {Error,_} ->
		{error,Error}
	end,
    io:format("~nRendering Job returned: ~p~n", [ExitStatus]),
    file:delete(Filename),
    render_cleanup(Parent, make_ref(), Result).

render_cleanup(Parent, Ref, Result) ->
    Ack = {Parent,ack,Ref},
    Command = {file,{?TAG_RENDER,{self(),Result,Ack}}},
    Parent ! {timeout,Ref,{event,{action,Command}}},
    receive Ack -> 
	    io:format("Rendering Job ready~n"),
	    ok
    after 5000 -> 
	    render_cleanup(Parent, Ref, Result)
    end.

render_job(Port) ->
    receive
	{Port,eof} ->
	    receive 
		{Port,{exit_status,ExitStatus}} ->
		    ExitStatus
	    after 1 -> 
		    undefined 
	    end;
	{Port,{exit_status,ExitStatus}} ->
	    receive 
		{Port,eof} -> 
		    ok after 1 -> ok end,
	    ExitStatus;
	{Port,{data,{Tag,Data}}} ->
	    io:put_chars(cr_to_nl(Data)),
	    case Tag of	eol -> io:nl(); _ -> ok end,
	    render_job(Port);
	{'EXIT',Port,Reason} ->
	    {error,Reason};
	Other ->
	    io:format("WARNING: Unexpected at ~s:~p: ~p~n", 
		      [?MODULE_STRING,?LINE,Other]),
	    render_job(Port)
    end.



% template(F, Fun_0) ->
%     println(F, "<!-- Begin Template"),
%     Fun_0(),
%     println(F, "End Template -->").

section(F, Name) ->
    println(F, [io_lib:nl(),"<!-- Section ",Name," -->",io_lib:nl()]).



export_shader(F, Name, Mat, ExportDir) ->
    OpenGL = proplists:get_value(opengl, Mat),
    YafRay = proplists:get_value(?TAG, Mat, []),
    Modulators = proplists:get_value(modulators, YafRay, []),
    Maps = proplists:get_value(maps, Mat, []),
    foldl(fun ({modulator,Ps}=M, N) when list(Ps) ->
		  case proplists:get_value(mode, Ps) of
		      off ->
			  N+1;
		      _ ->
			  export_texture(F, [Name,$_,format(N)], 
					 Maps, ExportDir, M),
			  println(F),
			  N+1
		  end;
	      (_, N) ->
		  N % Ignore old modulators
	  end, 1, Modulators),
    println(F, "<shader type=\"generic\" name=\"~s\">~n"++ 
	    "    <attributes>", [Name]),
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
    export_rgb(F, transmitted, 
	       {Dr*Transparency,Dg*Transparency,Db*Transparency,1.0}),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    println(F, "        <IOR value=\"~.10f\"/>~n"++
	    "        <min_refle value=\"~.10f\"/>~n"++
	    "    </attributes>", [IOR, MinRefle]),
    foldl(fun ({modulator,Ps}=M, N) when list(Ps) ->
		  case proplists:get_value(mode, Ps) of
		      off ->
			  N+1;
		      _ ->
			  export_modulator(F, [Name,$_,format(N)], M),
			  println(F),
			  N+1
		  end;
	      (_, N) ->
		  N % Ignore old modulators
	  end, 1, Modulators),
    println(F, "</shader>").

export_texture(F, Name, Maps, ExportDir, {modulator,Ps}) when list(Ps) ->
    case proplists:get_value(type, Ps) of
	image ->
	    Filename = 
		proplists:get_value(filename, Ps, 
				    ?DEF_MOD_FILENAME),
	    export_texture_image(F, Name, Filename);
	jpeg -> %% Old tag
	    Filename = 
		proplists:get_value(filename, Ps, 
				    ?DEF_MOD_FILENAME),
	    export_texture_image(F, Name, Filename);
	clouds ->
	    Color1 = proplists:get_value(color1, Ps, 
					 ?DEF_MOD_COLOR1),
	    Color2 = proplists:get_value(color2, Ps, 
					 ?DEF_MOD_COLOR2),
	    Depth = proplists:get_value(depth, Ps, 
					?DEF_MOD_DEPTH),
	    export_texture_clouds(F, Name, Color1, Color2, Depth);
	{map,Map} ->
	    case proplists:get_value(Map, Maps, none) of
		none ->
		    exit({unknown_texture_map,{?MODULE,?LINE,[Name,Map]}});
		#e3d_image{name=ImageName}=Image ->
		    MapFile = ImageName++".tga",
		    ok = e3d_image:save(Image, 
					filename:join(ExportDir, MapFile)),
		    export_texture_image(F, Name, MapFile)
	    end
    end.

export_texture_image(F, Name, Filename) ->
    println(F, "<texture type=\"image\" name=\"~s\">~n"++
	    "    <filename value=\"~s\"/>~n"++
	    "</texture>", [Name,Filename]).

export_texture_clouds(F, Name, Color1, Color2, Depth) ->
    println(F, "<texture type=\"clouds\" name=\"~s\">~n"++
	    "    <depth value=\"~w\"/>", [Name, Depth]),
    export_rgb(F, color1, Color1),
    export_rgb(F, color2, Color2),
    println(F, "</texture>").

export_modulator(F, Texname, {modulator,Ps}) when list(Ps) ->
    Mode = proplists:get_value(mode, Ps, ?DEF_MOD_MODE),
    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
    Opacity = proplists:get_value(opacity, Ps, ?DEF_MOD_OPACITY),
    Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
    Ambient = proplists:get_value(ambient, Ps, ?DEF_MOD_AMBIENT),
    Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
    Color = Diffuse * Opacity,
    HardValue = Shininess,
    Transmission = Diffuse * (1.0 - Opacity),
    Reflection = Ambient,
    println(F, "        <modulator texname=\"~s\" mode=\"~s\"~n"++
	    "         sizex=\"~.3f\" sizey=\"~.3f\" sizez=\"~.3f\">~n"++
	    "            <color value=\"~.3f\"/>~n"++
	    "            <specular value=\"~.3f\"/>~n"++
	    "            <hard value=\"~.3f\"/>~n"++
	    "            <transmission value=\"~.3f\"/>~n"++
	    "            <reflection value=\"~.3f\"/>~n"++
	    "        </modulator>", 
	    [Texname,format(Mode),SizeX,SizeY,SizeZ,
	     Color,Specular,HardValue,Transmission,Reflection]).



export_rgb(F, Type, {R,G,B,_}) ->
    println(F, ["        <",format(Type)," r=\"",format(R),
		"\" g=\"",format(G),"\" b=\"",format(B),"\"/>"]).



export_object(F, NameStr, #e3d_mesh{}=Mesh, Mats) ->
    #e3d_mesh{fs=Fs,vs=Vs,tx=Tx} = e3d_mesh:triangulate(Mesh),
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    [{_Count,DefaultMaterial}|_] = reverse(sort(count_equal(MM))),
    MatPs = proplists:get_value(DefaultMaterial, Mats, []),
    OpenGL = proplists:get_value(opengl, MatPs),
    YafRay = proplists:get_value(?TAG, MatPs, []),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    AutosmoothAngle = 
	proplists:get_value(autosmooth_angle, YafRay, ?DEF_AUTOSMOOTH_ANGLE),
    {Dr,Dg,Db,Opacity} = proplists:get_value(diffuse, OpenGL),
    Transparency = 1 - Opacity,
    println(F, "<object name=\"~s\" shader_name=\"~s\" "++
	    "shadow=\"on\" caus_IOR=\"~.10f\"~n"++
	    "        emit_rad=\"on\" recv_rad=\"on\">~n"++
	    "    <attributes>",
	    [NameStr,"w_"++format(DefaultMaterial),IOR]),
    export_rgb(F, caus_rcolor, proplists:get_value(ambient, OpenGL)),
%    export_rgb(F, caus_rcolor, 
%	       {Dr*Opacity,Dg*Opacity,Db*Opacity,1.0}),
    export_rgb(F, caus_tcolor, 
	       {Dr*Transparency,Dg*Transparency,Db*Transparency,1.0}),
    println(F, "    </attributes>"),
    case AutosmoothAngle of
	0.0 ->
	    println(F, "    <mesh>");
	_ ->
	    println(F, "    <mesh autosmooth=\"~.3f\">", [AutosmoothAngle])
    end,
    println(F, "        <points>"),
    export_vertices(F, Vs),
    println(F, "        </points>~n"++
	    "        <faces>", []),
    export_faces(F, Fs, DefaultMaterial, list_to_tuple(Tx)),
    println(F, "        </faces>~n"++
	    "    </mesh>~n"++
	    "</object>", []).

export_vertices(_F, []) ->
    ok;
export_vertices(F, [Pos|T]) ->
    export_pos(F, p, Pos),
    export_vertices(F, T).



%% The coordinate system rotation is done to make the sunsky
%% background work as expected. 
%% It assumes X=South Y=East Z=Up in YafRay coordinates.
%% Hence Z=South, X=East, Y=Up in Wings coordinates.
export_pos(F, Type, {X,Y,Z}) ->
    println(F, ["        <",format(Type)," x=\"",format(Z),
		"\" y=\"",format(X),"\" z=\"",format(Y),"\"/>"]).



export_faces(_F, [], _DefMat, _TxT) ->
    ok;
export_faces(F, [#e3d_face{vs=[A,B,C],tx=Tx,mat=[Mat|_]}|T], 
	     DefaultMaterial, TxT) ->
    Shader =
	case Mat of
	    DefaultMaterial -> "";
	    _ -> [" shader_name=\"w_",format(Mat),"\""]
	end,
    UV = case {TxT,Tx} of
	     {{},_} -> "";
	     {_,[Ta,Tb,Tc]} ->
		 {Ua,Va} = element(1+Ta, TxT),
		 {Ub,Vb} = element(1+Tb, TxT),
		 {Uc,Vc} = element(1+Tc, TxT),
		 [" u_a=\"",format(Ua),"\" v_a=\"",format(-Va),
		  "\" u_b=\"",format(Ub),"\" v_b=\"",format(-Vb),
		  "\" u_c=\"",format(Uc),"\" v_c=\"",format(-Vc),"\""]
	 end,
    println(F, ["        <f a=\"",format(A),
		"\" b=\"",format(B),"\" c=\"",format(C),"\"",
		Shader,UV,"/>"]),
    export_faces(F, T, DefaultMaterial, TxT).



export_light(F, Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafRay = proplists:get_value(?TAG, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    export_light(F, Name, Type, OpenGL, YafRay).

export_light(F, Name, point, OpenGL, YafRay) ->
    Power = proplists:get_value(power, YafRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    Type = proplists:get_value(type, YafRay, ?DEF_POINT_TYPE),
    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\" ", 
	    [Type,Name,Power]),
    case Type of
	pointlight ->
	    CastShadows = 
		proplists:get_value(cast_shadows, YafRay, ?DEF_CAST_SHADOWS),
	    println(F,"       cast_shadows=\"~s\">", [format(CastShadows)]);
	softlight ->
	    Bias = proplists:get_value(bias, YafRay, ?DEF_BIAS),
	    Res = proplists:get_value(res, YafRay, ?DEF_RES),
	    Radius = proplists:get_value(radius, YafRay, ?DEF_RADIUS),
	    println(F,"       bias=\"~.6f\" res=\"~w\" radius=\"~w\">", 
		    [Bias,Res,Radius])
    end,
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),
    println(F, "</light>"),
    undefined;
export_light(F, Name, infinite, OpenGL, YafRay) ->
    Bg = proplists:get_value(background, YafRay, ?DEF_BACKGROUND),
    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
    case Power of
	0.0 ->
	    ok;
	_ ->
	    CastShadows = 
		proplists:get_value(cast_shadows, YafRay, ?DEF_CAST_SHADOWS),
	    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
	    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
	    println(F,"<light type=\"sunlight\" name=\"~s\" "++
		    "power=\"~.3f\" cast_shadows=\"~s\">", 
		    [Name, Power,format(CastShadows)]),
	    export_pos(F, from, Position),
	    export_rgb(F, color, Diffuse),
	    println(F, "</light>")
    end,
    Bg;
export_light(F, Name, spot, OpenGL, YafRay) ->
    Power = proplists:get_value(power, YafRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, ?DEF_CONE_ANGLE),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,0.1}),
    Type = proplists:get_value(type, YafRay, ?DEF_POINT_TYPE),
    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\" ", 
	    [Type,Name,Power]),
    case Type of
	spotlight ->
	    CastShadows = 
		proplists:get_value(cast_shadows, YafRay, ?DEF_CAST_SHADOWS),
	    SpotExponent = 
		proplists:get_value(spot_exponent, OpenGL, ?DEF_SPOT_EXPONENT),
	    println(F, "       cast_shadows=\"~s\" size=\"~.10f\"~n"++
		    "       beam_falloff=\"~.10f\">", 
		    [format(CastShadows), ConeAngle, SpotExponent]);
	photonlight ->
	    Mode = proplists:get_value(mode, YafRay, ?DEF_MODE),
	    Photons = proplists:get_value(photons, YafRay, ?DEF_PHOTONS),
	    Depth = proplists:get_value(depth, YafRay, ?DEF_DEPTH),
	    Fixedradius = 
		proplists:get_value(fixedradius, YafRay, ?DEF_FIXEDRADIUS),
	    Search = proplists:get_value(search, YafRay, ?DEF_SEARCH),
	    Mindepth = proplists:get_value(mindepth, YafRay, ?DEF_MINDEPTH),
	    Cluster = proplists:get_value(cluster, YafRay, ?DEF_CLUSTER),
	    case Mode of
		diffuse ->
		    println(F, "       mode=\"diffuse\"");
		_ ->
		    ok
	    end,
	    println(F, "       photons=\"~w\" depth=\"\~w\"~n"++
		    "       fixedradius=\"~.10f\" search=\"~w\"~n"++
		    "       mindepth=\"~w\" cluster=\"~.10f\">",
		    [Photons,Depth,Fixedradius,Search,Mindepth,Cluster])
    end,
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    println(F, "</light>"),
    undefined;
export_light(_F, Name, Type, _OpenGL, _YafRay) ->
    io:format("WARNING: Ignoring unknown light \"~s\" type: ~p~n", 
	      [Name, format(Type)]),
    undefined.



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
    Pos = e3d_vec:sub(Rev, Aim), % Aim is the vector from aim point to origo
    From = e3d_vec:add(Pos, Transl),
    To = e3d_vec:sub(Transl, Aim), % Aim is the vector from aim point to origo
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



export_background(F, Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafRay = proplists:get_value(?TAG, Ps, []),
    Bg = proplists:get_value(background, YafRay, ?DEF_BACKGROUND),
    print(F, "<background type=\"~s\" name=\"~s\"", 
	  [format(Bg),Name]),
    case Bg of
	constant ->
	    println(F, ">"),
	    {R,G,B} = proplists:get_value(background_color, YafRay, 
					  ?DEF_BACKGROUND_COLOR),
	    export_rgb(F, color, {R,G,B,1.0});
	sunsky ->
	    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
	    AddSun = (Power > 0.0),
	    Turbidity = proplists:get_value(turbidity, Ps, ?DEF_TURBIDITY),
	    Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),
	    println(F, "~n            turbidity=\"~.3f\" add_sun=\"~s\">", 
		    [Turbidity,format(AddSun)]),
	    export_pos(F, from, Position)
    end,
    println(F, "</background>").



export_render(F, CameraName, BackgroundName, Outfile, Attr) ->
    AA_passes = proplists:get_value(aa_passes, Attr),
    Raydepth = proplists:get_value(raydepth, Attr),
    Bias = proplists:get_value(bias, Attr),
    AA_threshold = proplists:get_value(aa_threshold, Attr),
    println(F, "<render camera_name=\"~s\" "++
	    "AA_passes=\"~w\" raydepth=\"~w\"~n"++
	    "        bias=\"~.10f\" AA_threshold=\"~.10f\">~n"++
	    "    <background_name value=\"~s\"/>~n"++
	    "    <outfile value=\"~s\"/>~n"++
	    "    <indirect_samples value=\"0\"/>~n"++
	    "    <indirect_power value=\"1.0\"/>~n"++
	    "    <exposure value=\"~.10f\"/>~n"++
	    "    <gamma value=\"1.0\"/>~n"++
	    "    <fog_density value=\"0.0\"/>",
	    [CameraName,AA_passes,Raydepth,Bias,AA_threshold,
	     BackgroundName,Outfile,math:sqrt(2.0)]),
    export_rgb(F, fog_color, {1.0,1.0,1.0,1.0}),
    println(F, "</render>").



%% Noisy file output functions. Fail if anything goes wrong.

open(Filename, export) ->
    case file:open(Filename, [write,raw,delayed_write]) of
	{ok, F} ->
	    F;
	Error ->
	    erlang:fault(Error, [Filename, export])
    end.

println(F) ->
    println(F, "").

% print(F, DeepString) ->
%     case file:write(F, DeepString) of
% 	ok ->
% 	    ok;
% 	Error ->
% 	    erlang:fault(Error, [F,DeepString])
%     end.

println(F, DeepString) ->
    case file:write(F, [DeepString,io_lib:nl()]) of
	ok ->
	    ok;
	Error ->
	    erlang:fault(Error, [F,DeepString])
    end.

print(F, Format, Args) ->
    case file:write(F, io_lib:format(Format, Args)) of
	ok ->
	    ok;
	Error ->
	    erlang:fault(Error, [F,Format,Args])
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



%% Convert certain terms to printable strings in an 
%% hopefully efficient way.

format(F) when is_float(F) ->
    I = abs(trunc(F)),
    D = abs(F) - float(I),
    if F < 0 ->
	    [$-,integer_to_list(I)|format_decimals(D)];
       true ->
	    [integer_to_list(I)|format_decimals(D)]
    end;
format(I) when is_integer(I) ->
    integer_to_list(I);
format(true) ->
    "on";
format(false) ->
    "off";
format(A) when is_atom(A) ->
    atom_to_list(A);
format(L) when is_list(L) ->
    L.

format_decimals(F) when float(F), F >= 0.0 ->
    format_decimals_1(F).

format_decimals_1(0.0) ->
    ".0";
format_decimals_1(F) when is_float(F) ->
    G = 10.0 * F,
    I = trunc(G),
    D = G - float(I),
    [$.,(I+$0)|format_decimals_2(D)].

format_decimals_2(0.0) ->
    [];
format_decimals_2(F) when is_float(F) ->
    G = 100.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->
	    [$0,(I+$0)|format_decimals_3(D)];
       true ->
	    [integer_to_list(I)|format_decimals_3(D)]
    end.

format_decimals_3(0.0) ->
    [];
format_decimals_3(F) when is_float(F) ->
    G = 1000.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->
	    [$0,$0,(I+$0)|format_decimals_4(D)];
       I < 100 ->
	    [$0,integer_to_list(I)|format_decimals_4(D)];
       true ->
	    [integer_to_list(I)|format_decimals_4(D)]
    end.

format_decimals_4(0.0) ->
    [];
format_decimals_4(F) when is_float(F) ->
    G = 10000.0 * F,
    I = trunc(G),
    if I < 100 ->
	    if I < 10 ->
		    [$0,$0,$0,(I+$0)];
	       true ->
		    [$0,$0|integer_to_list(I)]
	    end;
       true ->
	    if I < 1000 ->
		    [$0|integer_to_list(I)];
	       true ->
		    integer_to_list(I)
	    end
    end.

    

%% Set and get preference variables saved in the .wings file for this module

set_pref(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).



%% Set and get global variables (in the process dictionary) 
%% per wings session for this module.

set_var(Name, undefined) ->
    erase_var(Name);
set_var(Name, Value) ->
    put({?MODULE,Name}, Value).

get_var(Name) ->
    get({?MODULE,Name}).

erase_var(Name) ->
    erase({?MODULE,Name}).



%% Count the number of equal elements in a row in the list

count_equal([H|T]) ->
    count_equal(T, 1, H, []).

count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).



%% Like os:find_executable, but if the found executable is a .bat file
%% on windows; scan the .bat file for a real executable file.
%%
find_executable(Name) ->
    case os:find_executable(Name) of
	false ->
	    false;
	Filename ->
	    case os:type() of
		{win32,_} ->
		    case lowercase(filename:extension(Filename)) of
			".bat" ->
			    find_in_bat(Filename);
			_ ->
			    Filename
		    end;
		_ ->
		    Filename
	    end
    end.

%% Search .bat file for an executable file
find_in_bat(Filename) ->
    case file:open(Filename, [read]) of
	{ok,F} ->
	    R = scan_bat(F),
	    file:close(F),
	    R;
	_ ->
	    false
    end.

%% Scan each line of the .bat file
scan_bat(F) ->
    case rskip_warg(skip_walpha(io:get_line(F, ""))) of
	"" ->
	    scan_bat(F);
	"echo"++[C|_] when C==$ ; C==$\t; C==$\n ->
	    scan_bat(F);
	"set"++[C|_] when C==$ ; C==$\t; C==$\n ->
	    scan_bat(F);
	Line when list(Line) ->
	    %% Check if this is the name of an executable file
	    Filename = filename:nativename(Line),
	    case file:read_file_info(Filename) of
		{ok,#file_info{type=regular,access=A,mode=M}} ->
		    case A of
			read when (M band 8#500) == 8#500 ->
			    Filename;
			read_write when (M band 8#500) == 8#500 ->
			    Filename;
			_ ->
			    scan_bat(F)
		    end;
		_ ->
		    scan_bat(F)
	    end;
	_ ->
	    false
    end.



%% Skip whitespace and one '@' from beginning of line

skip_walpha([$ |T]) ->
    skip_walpha(T);
skip_walpha([$\t|T]) ->
    skip_walpha(T);
skip_walpha([$\n|T]) ->
    skip_walpha(T);
skip_walpha([$@|T]) ->
    skip_walpha1(T);
skip_walpha(L) ->
    L.

skip_walpha1([$ |T]) ->
    skip_walpha(T);
skip_walpha1([$\t|T]) ->
    skip_walpha(T);
skip_walpha1([$\n|T]) ->
    skip_walpha(T);
skip_walpha1(L) ->
    L.

%% Skip whitespace and '%d' .bat file arguments from end of line

rskip_warg(L) ->
    rskip_warg1(lists:reverse(L)).

rskip_warg1([$ |T]) ->
    rskip_warg1(T);
rskip_warg1([$\t|T]) ->
    rskip_warg1(T);
rskip_warg1([$\n|T]) ->
    rskip_warg1(T);
rskip_warg1([D,$%|T]) when D >= $0, D =< $9 ->
    rskip_warg1(T);
rskip_warg1(L) ->
    lists:reverse(L).

%% Convert all A-Z in string to lowercase

lowercase([]) ->
    [];
lowercase([C|T]) when C >= $A, C =< $Z ->
    [(C + $a - $A)|lowercase(T)];
lowercase([C|T]) ->
    [C|lowercase(T)].

%% Convert lonely CR to NL, and CRLF to NL

cr_to_nl([$\r,$\n|T]) ->
    [$\n|cr_to_nl(T)];
cr_to_nl([$\r|T]) ->
    [$\n|cr_to_nl(T)];
cr_to_nl([C|T]) ->
    [C|cr_to_nl(T)];
cr_to_nl([]) ->
    [].

%% Split a list into a list of length Pos, and the tail

split_list(List, Pos) when list(List), integer(Pos), Pos >= 0 ->
    case split_list1(List, Pos, []) of
	{_,_}=Result ->
	    Result;
	Error ->
	    erlang:fault(Error, [List, Pos])
    end.

split_list1(List, 0, Head) ->
    {lists:reverse(Head),List};
split_list1([], _Pos, _) ->
    error;
split_list1([H|T], Pos, Head) ->
    split_list1(T, Pos-1, [H|Head]).
