%%
%%  wpc_yafray.erl
%%
%%     YafRay Plugin User Interface.
%%
%%  Copyright (c) 2003 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_yafray.erl,v 1.67 2004/03/08 22:27:18 raimo_niskanen Exp $
%%

-module(wpc_yafray).

-export([init/0,menu/2,dialog/2,command/2]).



-include_lib("kernel/include/file.hrl").

-include("e3d.hrl").
-include("e3d_image.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,keydelete/3,
		foreach/2,foldl/3]).

-define(TAG, yafray).
-define(TAG_RENDER, yafray_render).

%%% Default values

-define(DEF_DIALOGS, auto).
-define(DEF_RENDERER, "yafray").
-define(DEF_OPTIONS, "").
-define(DEF_LOAD_IMAGE, true).
-define(DEF_SUBDIVISIONS, 0).
-define(DEF_SAVE_ALPHA, false).
-define(DEF_GAMMA, 1.0).
-define(DEF_EXPOSURE, 1.41421356237).
-define(DEF_FOG_DENSITY, 0.0).
-define(DEF_FOG_COLOR, {1.0,1.0,1.0,1.0}).

%% Shader
-define(DEF_CAUS, false).
-define(DEF_TIR, false).
-define(DEF_IOR, 1.0).
-define(DEF_MIN_REFLE, 0.0).
-define(DEF_USE_HARDNESS, false).
-define(DEF_AUTOSMOOTH, true).
-define(DEF_AUTOSMOOTH_ANGLE, 60.0).
-define(DEF_SHADOW,true).
-define(DEF_EMIT_RAD,true).
-define(DEF_RECV_RAD,true).
-define(DEF_FAST_FRESNEL,false).
%% Arealight
-define(DEF_AREALIGHT, false).
-define(DEF_AREALIGHT_SAMPLES, 50).
-define(DEF_AREALIGHT_PSAMPLES, 0).
-define(DEF_DUMMY, false).

%% Render
-define(DEF_AA_PASSES, 0).
-define(DEF_AA_MINSAMPLES, 1).
-define(DEF_AA_PIXELWIDTH, 1.0).
-define(DEF_AA_THRESHOLD, 0.125).
-define(DEF_RAYDEPTH, 3).
-define(DEF_BIAS, 0.1).
-define(DEF_WIDTH, 100).
-define(DEF_HEIGHT, 100).
-define(DEF_DOF_FILTER, false).
-define(DEF_NEAR_BLUR, 1.0).
-define(DEF_FAR_BLUR, 1.0).
-define(DEF_DOF_SCALE, 1.0).
-define(DEF_ANTINOISE_FILTER, false).
-define(DEF_ANTINOISE_RADIUS, 1.0).
-define(DEF_ANTINOISE_MAX_DELTA, 1.0).

%% Light
-define(DEF_ATTN_POWER, 10.0).
-define(DEF_POINT_TYPE, pointlight).
-define(DEF_CAST_SHADOWS, true).
-define(DEF_USE_QMC, false).
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
%% Hemilight and Pathlight
-define(DEF_AMBIENT_TYPE, hemilight).
-define(DEF_BACKGROUND_FILENAME, "").
-define(DEF_BACKGROUND_EXPOSURE_ADJUST, 0).
-define(DEF_BACKGROUND_MAPPING, probe).
-define(DEF_BACKGROUND_POWER, 1.0).
-define(DEF_SAMPLES, 256).

%% Modulator
-define(DEF_MOD_ENABLED, true).
-define(DEF_MOD_MODE, mix).
-define(DEF_MOD_SIZE_X, 1.0).
-define(DEF_MOD_SIZE_Y, 1.0).
-define(DEF_MOD_SIZE_Z, 1.0).
-define(DEF_MOD_OPACITY, 1.0).
-define(DEF_MOD_DIFFUSE, 0.0).
-define(DEF_MOD_SPECULAR, 0.0).
-define(DEF_MOD_AMBIENT, 0.0).
-define(DEF_MOD_SHININESS, 0.0).
-define(DEF_MOD_NORMAL, 0.0).
-define(DEF_MOD_TYPE, image).
-define(DEF_MOD_FILENAME, "").
-define(DEF_MOD_COLOR1, {0.0,0.0,0.0}).
-define(DEF_MOD_COLOR2, {1.0,1.0,1.0}).
-define(DEF_MOD_DEPTH, 2).
-define(DEF_MOD_HARD, false).
-define(DEF_MOD_TURBULENCE, 1.0).
-define(DEF_MOD_SHARPNESS, 1.0).
-define(DEF_MOD_RINGSCALE_X, 1.0).
-define(DEF_MOD_RINGSCALE_Z, 1.0).



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
    case is_plugin_active(edit) of
	false ->
	    {Mat,Res};
	_ ->
	    material_result(Name, Mat, Res)
    end;
dialog({light_editor_setup,Name,Ps}, Dialog) ->
    maybe_append(edit, Dialog, light_dialog(Name, Ps));
dialog({light_editor_result,Name,Ps0}, Res) ->
    case is_plugin_active(edit) of
	false ->
	    {Ps0,Res};
	_ ->
	    light_result(Name, Ps0, Res)
    end;
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

maybe_append(Condition, Menu, PluginMenu) ->
    case is_plugin_active(Condition) of
	false ->
	    Menu;
	_ ->
	    Menu++PluginMenu
    end.

is_plugin_active(Condition) ->
    case Condition of
	export ->
	    get_var(dialogs);
	edit ->
	    get_var(dialogs);
	render ->
	    get_var(renderer);
	_ ->
	    false
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
	    SubDiv = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
	    wpa:export(props(render, SubDiv),
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
    SubDiv = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
    wpa:Op(props(Op, SubDiv), fun_export_2(attr(St, Attr)), St);
command_file(Op, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "YafRay Export Options", export_dialog(Op),
	       fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).

props(render, SubDiv) ->
    [{title,"Render"},{ext,".tga"},{ext_desc,"Targa File"},
     {subdivisions,SubDiv}];
props(export, SubDiv) ->
    [{title,"Export"},{ext,".xml"},{ext_desc,"YafRay File"},
     {subdivisions,SubDiv}];
props(export_selected, SubDiv) ->
    [{title,"Export Selected"},{ext,".xml"},{ext_desc,"YafRay File"},
     {subdivisions,SubDiv}].

-record(camera_info, {aim,distance_to_aim,azimuth,elevation,tracking,fov}).

attr(St, Attr) ->
    [Aim,Dist,Az,El,Track,Fov] =
	wpa:camera_info([aim,distance_to_aim,azimuth,elevation,tracking,fov]),
    CameraInfo = #camera_info{aim=Aim,distance_to_aim=Dist,azimuth=Az,
			      elevation=El,tracking=Track,fov=Fov},
    [CameraInfo,{lights,wpa:lights(St)}|Attr].

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
	    Id = wings_image:new_temp("<<Rendered>>", Image),
	    wings_image:window(Id),
	    keep;
	_ ->
	    wpa:error("No image rendered")
    end.



%%% Dialogues and results
%%%

material_dialog(_Name, Mat) ->
    Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    Ambient = rgba2rgb(proplists:get_value(ambient, OpenGL)),
    DiffuseA = proplists:get_value(diffuse, OpenGL),
    DefTransmitted = def_transmitted(DiffuseA),
    Ambient = rgba2rgb(proplists:get_value(ambient, OpenGL)),
    YafRay = proplists:get_value(?TAG, Mat, []),
    Minimized = proplists:get_value(minimized, YafRay, true),
    ObjectMinimized = proplists:get_value(object_minimized, YafRay, true),
    Caus = proplists:get_value(caus, YafRay, ?DEF_CAUS),
    Shadow = proplists:get_value(shadow, YafRay, ?DEF_SHADOW),
    EmitRad = proplists:get_value(emit_rad, YafRay, ?DEF_EMIT_RAD),
    RecvRad = proplists:get_value(recv_rad, YafRay, ?DEF_RECV_RAD),
    UseHardness = proplists:get_value(use_hardness, YafRay, ?DEF_USE_HARDNESS),
    TIR = proplists:get_value(tir, YafRay, ?DEF_TIR),
    AutosmoothAngle = 
	proplists:get_value(autosmooth_angle, YafRay, ?DEF_AUTOSMOOTH_ANGLE),
    Autosmooth = proplists:get_value(autosmooth, YafRay, 
				     if AutosmoothAngle == 0.0 -> false;
					true -> ?DEF_AUTOSMOOTH end),
    FresnelMinimized = proplists:get_value(fresnel_minimized, YafRay, true),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    FastFresnel = proplists:get_value(fast_fresnel, YafRay, ?DEF_FAST_FRESNEL),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    Reflected = proplists:get_value(reflected, YafRay, Ambient),
    Transmitted = 
	proplists:get_value(transmitted, YafRay, DefTransmitted),
    Fresnel2 = proplists:get_value(fresnel2, YafRay, false),
    Reflected2 = proplists:get_value(reflected2, YafRay, Ambient),
    Transmitted2 = 
	proplists:get_value(transmitted2, YafRay, DefTransmitted),
    Modulators = proplists:get_value(modulators, YafRay, def_modulators(Maps)),
    ObjectVframe = 
	{vframe,
	 [{hframe,[help_button({material_dialog,object}),
		   {"Cast Shadow",Shadow,[{key,{?TAG,shadow}}]},
		   {"Emit Rad",EmitRad,[{key,{?TAG,emit_rad}}]},
		   {"Recv Rad",RecvRad,[{key,{?TAG,recv_rad}}]}]},
	  {hframe,[{"Use Edge Hardness",UseHardness,
		    [{key,{?TAG,use_hardness}}]},
		   {"Caustic",Caus,[{key,{?TAG,caus}}]}]},
	  {hframe,[{"Autosmooth",Autosmooth,[{key,{?TAG,autosmooth}}]},
		   {label,"Angle"},
		   {slider,{text,AutosmoothAngle,
			    [{range,{0.0,180.0}},{width,5},
			     {key,{?TAG,autosmooth_angle}}]}}]}],
	 [{title,"Object Parameters"},{minimized,ObjectMinimized},
	  {key,{?TAG,object_minimized}}]},
    FresnelVframe =
	{vframe,
	 [{hframe,[help_button({material_dialog,fresnel}),
		   {label,"Index Of Refraction"},
		   {text,IOR,[{range,{0.0,100.0}},{key,{?TAG,ior}}]}]},
	  {hframe,[{"Fast Fresnel",FastFresnel,[{key,{?TAG,fast_fresnel}}]},
		   {"Total Internal Reflection",TIR,[{key,{?TAG,tir}}]}]},
	  {hframe,[{label,"Minimum Reflection"},
		   {slider,{text,MinRefle,[{range,{0.0,1.0}},{width,5},
					   {key,{?TAG,min_refle}}]}}]},
	  {hframe,[{vframe,[{label,"Reflected"},
			    {label,"Transmitted"}]},
		   {vframe,[{slider,{color,Reflected,
				     [{key,{?TAG,reflected}}]}},
			    {slider,{color,Transmitted,
				     [{key,{?TAG,transmitted}}]}}]},
		   {vframe,[panel,
			    {button,"Set Default",keep,
			     [transmitted_hook({?TAG,transmitted})]}]}]},
	  {"Grazing Angle Colors",Fresnel2,[{key,{?TAG,fresnel2}},
					    layout]},
	  {hframe,[{vframe,[{label,"Reflected"},
			    {label,"Transmitted"}]},
		   {vframe,[{slider,{color,Reflected2,
				     [{key,{?TAG,reflected2}}]}},
			    {slider,{color,Transmitted2,
				     [{key,{?TAG,transmitted2}}]}}]},
		   {vframe,[panel,
			    {button,"Set Default",keep,
			     [transmitted_hook({?TAG,transmitted2})]}]}],
	   [bhook(maximized, {?TAG,fresnel2})]}],
	 [{title,"Fresnel Parameters"},{minimized,FresnelMinimized},
	  {key,{?TAG,fresnel_minimized}}]},
    %%
    [{vframe,
      [ObjectVframe,
       FresnelVframe
       |modulator_dialogs(Modulators, Maps)],
      [{title,"YafRay Options"},{minimized,Minimized},{key,{?TAG,minimized}}]}].

rgba2rgb({R,G,B,_}) -> {R,G,B}.

def_transmitted({Dr,Dg,Db,Da}) ->
    Dt = 1-Da,
    {Dr*Dt,Dg*Dt,Db*Dt}.

transmitted_hook(Tag) ->
    {hook,fun (update, {_Var,_I,_Val,Sto}) ->
		  {Dr,Dg,Db} = gb_trees:get(diffuse, Sto),
		  Da = gb_trees:get(opacity, Sto),
		  Transmitted = def_transmitted({Dr,Dg,Db,Da}),
		  {store,gb_trees:update(Tag, Transmitted, Sto)};
	      (_, _) -> void end}.

def_modulators([]) ->
    [];
def_modulators([{diffuse,_}|Maps]) ->
    [{modulator,[{type,{map,diffuse}},{diffuse,1.0}]}
     |def_modulators(Maps)];
def_modulators([{ambient,_}|Maps]) ->
    [{modulator,[{type,{map,ambient}},{ambient,1.0}]}
     |def_modulators(Maps)];
def_modulators([{bump,_}|Maps]) ->
    [{modulator,[{type,{map,bump}},{normal,1.0}]}
     |def_modulators(Maps)];
def_modulators([{gloss,_}|Maps]) ->
    [{modulator,[{type,{map,gloss}},{shininess,1.0}]}
     |def_modulators(Maps)];
def_modulators([_|Maps]) ->
    def_modulators(Maps).

material_result(_Name, Mat0, [{{?TAG,minimized},_}|_]=Res0) ->
    {Ps1,Res1} = split_list(Res0, 19),
    Ps2 = [{Key,Val} || {{?TAG,Key},Val} <- Ps1],
    {Ps,Res} = modulator_result(Ps2, Res1),
    Mat = [{?TAG,Ps}|keydelete(?TAG, 1, Mat0)],
    {Mat,Res};
material_result(Name, Mat, Res) ->
    exit({invalid_tag,{?MODULE,?LINE,[Name,Mat,Res]}}).

modulator_dialogs(Modulators, Maps) ->
    modulator_dialogs(Modulators, Maps, 1).

modulator_dialogs([], _Maps, M) ->
    [{hframe,
      [{button,"New Modulator",done,[{key,{?TAG,new_modulator}}]},
       panel|
       if M =:= 1 -> [{button,"Default Modulators",done}];
	 true -> [] end]}];
modulator_dialogs([Modulator|Modulators], Maps, M) ->
    modulator_dialog(Modulator, Maps, M)++
	modulator_dialogs(Modulators, Maps, M+1).

modulator_dialog({modulator,Ps}, Maps, M) when list(Ps) ->
%    erlang:display({?MODULE,?LINE,[Ps,M,Maps]}),
    {Enabled,Mode,Type} = mod_enabled_mode_type(Ps, Maps),
    Minimized = proplists:get_value(minimized, Ps, true),
    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
    Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
    Ambient = proplists:get_value(ambient, Ps, ?DEF_MOD_AMBIENT),
    Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
    Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
    BrowseProps = [{dialog_type,open_dialog},
		   {extensions,[{".jpg","JPEG compressed image"},
				{".tga","Targa bitmap"}]}],
%    erlang:display({?MODULE,?LINE,[Filename,AbsnameX,BrowseProps]}),
    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
    Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
    RingscaleX = proplists:get_value(ringscale_x, Ps, ?DEF_MOD_RINGSCALE_X),
    RingscaleZ = proplists:get_value(ringscale_z, Ps, ?DEF_MOD_RINGSCALE_Z),
    TypeTag = {?TAG,type,M},
    MapsFrame = [{hradio,[{atom_to_list(Map),{map,Map}} || {Map,_} <- Maps],
		  Type,[{key,TypeTag},mod_enable_hook(M),layout]}],
    [{vframe,
      [{hframe,[{"Enabled",Enabled,[{key,{?TAG,enabled,M}}]},
		{menu,[{"Mix",mix},{"Mul",mul},{"Add",add}],Mode,
		 [mod_enable_hook(M)]},
		{button,"Delete",done}]},
       {hframe,[{label,"SizeX"},
		{text,SizeX,[{range,0.0,1000.0},mod_enable_hook(M)]},
		{label,"SizeY"},
		{text,SizeY,[{range,0.0,1000.0},mod_enable_hook(M)]},
		{label,"SizeZ"},
		{text,SizeZ,[{range,0.0,1000.0},mod_enable_hook(M)]}]},
       {hframe,[{vframe,[{label,"Diffuse "},
			 {label,"Specular"},
			 {label,"Ambient"},
			 {label,"Shininess"},
			 {label,"Normal"}]},
		{vframe,[{slider,{text,Diffuse,
				  [{range,{0.0,1.0}},mod_enable_hook(M)]}},
			 {slider,{text,Specular,
				  [{range,{0.0,1.0}},mod_enable_hook(M)]}},
			 {slider,{text,Ambient,
				  [{range,{0.0,1.0}},mod_enable_hook(M)]}},
			 {slider,{text,Shininess,
				  [{range,{0.0,1.0}},mod_enable_hook(M)]}},
			 {slider,{text,Normal,
				  [{range,{0.0,1.0}},
				   mod_enable_hook(M)]}}]}]}]++
      MapsFrame++
      [{hradio,[{"Image",image},{"Clouds",clouds},
		  {"Marble",marble},{"Wood",wood}],
	  Type,[{key,TypeTag},mod_enable_hook(M),layout]},
       {hframe,
	[{vframe,[{label,"Filename",[mod_enable_hook(M, [image])]},
		  {label,"Color 1",
		   [mod_enable_hook(M, [marble,wood,clouds])]},
		  {label,"Turbulence",[mod_enable_hook(M, [marble,wood])]},
		  {label,"Ringscale X",[mod_enable_hook(M, [wood])]}]},
	 {vframe,[{button,{text,Filename,[mod_enable_hook(M, [image]),
					  {props,BrowseProps}]}},
		  {hframe,
		   [{vframe,[{color,Color1,
			      [mod_enable_hook(M, [marble,wood,clouds])]},
			     {text,Turbulence,
			      [{range,{0.01,100.0}},
			       mod_enable_hook(M, [marble,wood])]},
			     {text,RingscaleX,
			      [{range,{0.01,1000.0}},
			       mod_enable_hook(M, [wood])]}]},
		    {vframe,[{label,"Color 2",
			      [mod_enable_hook(M, [marble,wood,clouds])]},
			     {label,"Sharpness",
			      [mod_enable_hook(M, [marble])]},
			     {panel,[mod_enable_hook(M, [wood])]},
			     {label,"Ringscale Z",
			      [mod_enable_hook(M, [wood])]}]},
		    {vframe,[{color,Color2,
			      [mod_enable_hook(M, [marble,wood,clouds])]},
			     {text,Sharpness,
			      [{range,{1.0,100.0}},
			       mod_enable_hook(M, [marble])]},
			     {panel,[mod_enable_hook(M, [wood])]},
			     {text,RingscaleZ,
			      [{range,{0.01,1000.0}},
			       mod_enable_hook(M, [wood])]}]},
		    {vframe,[{hframe,
			      [{label,"Depth",
				[mod_enable_hook(M, 
						 [marble,wood,clouds])]},
			       {text,Depth,
				[{range,{1,1000}},
				 mod_enable_hook(M,
						 [marble,wood,clouds])]}]},
			     {"Hard Noise",Hard,
			      [mod_enable_hook(M, [marble, wood])]}
			    ]}]}]}]}],
      [{title,
	"Modulator "++integer_to_list(M)++mod_legend(Enabled, Mode, Type)},
       {minimized,Minimized}]}];
modulator_dialog(_Modulator, _Maps, _) ->
    []. % Discard old modulators that anyone may have

mod_enable_hook(M) ->
    {hook,fun (is_disabled, {_Var,_I,Sto}) ->
		  not gb_trees:get({?TAG,enabled,M}, Sto);
	      (_, _) -> void end}.

mod_enable_hook(M, Types) ->
    {hook,fun (is_disabled, {_Var,_I,Sto}) ->
		  not gb_trees:get({?TAG,enabled,M}, Sto);
	      (is_minimized, {_Var,_I,Sto}) ->
		  Type = gb_trees:get({?TAG,type,M}, Sto),
		  not lists:member(Type, Types);
	      (_, _) -> void end}.

mod_enabled_mode_type(Ps, Maps) ->
    {Enabled,Mode} = 
	case proplists:get_value(mode, Ps, ?DEF_MOD_MODE) of
	    off -> {false,?DEF_MOD_MODE};
	    Mode1 -> {proplists:get_value(enabled, Ps, ?DEF_MOD_ENABLED),Mode1}
	end,
    Type = proplists:get_value(type, Ps, ?DEF_MOD_TYPE),
    case Type of
	{map,Map} ->
	    case lists:keymember(Map, 1, Maps) of
		true -> {Enabled,Mode,Type};
		false -> {false,Mode,?DEF_MOD_TYPE}
	    end;
	_ -> {Enabled,Mode,Type}
    end.

mod_legend(Enabled, Mode, {map,Map}) ->
    mod_legend(Enabled, Mode, atom_to_list(Map));
mod_legend(Enabled, Mode, Type) when atom(Mode) ->
    mod_legend(Enabled, wings_util:cap(Mode), Type);
mod_legend(Enabled, Mode, Type) when atom(Type) ->
    mod_legend(Enabled, Mode, wings_util:cap(Type));
mod_legend(Enabled, Mode, Type) when list(Mode), list(Type) ->
    case Enabled of
	true -> " (enabled, ";
	false -> " (disabled, "
    end++Mode++", "++Type++")".


modulator_result(Ps, Res) ->
    modulator_result(Ps, Res, 1, []).

modulator_result(Ps, [], _, Modulators) ->
    %% Should not happen
    {[{modulators,reverse(Modulators)}|Ps], []};
modulator_result(Ps, [{{?TAG,new_modulator},false},false|Res], 1, []) ->
    {[{modulators,[]}|Ps],Res};
modulator_result(Ps, [{{?TAG,new_modulator},false},true|Res], 1, []) ->
    %% Default Modulators
    {Ps,Res};
modulator_result(Ps, [{{?TAG,new_modulator},true},_|Res], 1, []) ->
    {[{modulators,[{modulator,[]}]}|Ps],Res};
modulator_result(Ps, [{{?TAG,new_modulator},false}|Res], _, Modulators) ->
    {[{modulators,reverse(Modulators)}|Ps],Res};
modulator_result(Ps, [{{?TAG,new_modulator},true}|Res], _, Modulators) ->
    {[{modulators,reverse(Modulators, [{modulator,[]}])}|Ps],Res};
modulator_result(Ps, [_Minimized,{{?TAG,enabled,M},_},_Mode,true|Res0], 
		 M, Modulators) -> 
    %% Delete
    {_,Res} = split_list(Res0, 18),
    modulator_result(Ps, Res, M+1, Modulators);
modulator_result(Ps, [Minimized,{{?TAG,enabled,M},Enabled},Mode,false|Res0], 
		 M, Modulators) ->
    {Modulator,Res} = modulator(Minimized, Enabled, Mode, Res0, M),
    modulator_result(Ps, Res, M+1, [Modulator|Modulators]).

modulator(Minimized, Enabled, Mode, Res0, M) ->
    {Res1,Res} = split_list(Res0, 18),
    TypeTag = {?TAG,type,M},
    {value,{TypeTag,Type}} = lists:keysearch(TypeTag, 1, Res1),
    [SizeX,SizeY,SizeZ,
     Diffuse,Specular,Ambient,Shininess,Normal,
     Filename,
     Color1,Turbulence,RingscaleX,
     Color2,Sharpness,RingscaleZ,
     Depth,Hard] %% 17 values = 18-1
	= lists:keydelete(TypeTag, 1, Res1),
    Ps = [{minimized,Minimized},{enabled,Enabled},{mode,Mode},
	  {size_x,SizeX},{size_y,SizeY},{size_z,SizeZ},
	  {diffuse,Diffuse},{specular,Specular},{ambient,Ambient},
	  {shininess,Shininess},{normal,Normal},
	  {type,Type},
	  {filename,Filename},{color1,Color1},{color2,Color2},{depth,Depth},
	  {hard,Hard},{turbulence,Turbulence},{sharpness,Sharpness},
	  {ringscale_x,RingscaleX},{ringscale_z,RingscaleZ}],
    {{modulator,Ps},Res}.

light_dialog(Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafRay = proplists:get_value(?TAG, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    DefPower = case Type of
		   point -> ?DEF_ATTN_POWER;
		   spot -> ?DEF_ATTN_POWER;
		   area -> ?DEF_ATTN_POWER;
		   _ -> ?DEF_POWER
	       end,
    Minimized = proplists:get_value(minimized, YafRay, true),
    Power = proplists:get_value(power, YafRay, DefPower),
    [{vframe,
      [{hframe,[help_button(light_dialog),
		{vframe, [{label,"Power"}]},
		{vframe,[{text,Power,
			  [{range,{0.0,10000.0}},{key,{?TAG,power}}]}]}]}|
       light_dialog(Name, Type,YafRay)],
      [{title,"YafRay Options"},{key,{?TAG,minimized}},{minimized,Minimized}]}].

light_dialog(_Name, point, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_POINT_TYPE),
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Bias = proplists:get_value(bias, Ps, ?DEF_BIAS),
    Res = proplists:get_value(res, Ps, ?DEF_RES),
    Radius = proplists:get_value(radius, Ps, ?DEF_RADIUS),
    [{hframe,
      [{vradio,[{"Pointlight",pointlight},{"Softlight",softlight}],
	Type,[{key,{?TAG,type}},layout]},
       {"Cast Shadows",CastShadows,
	[{key,{?TAG,cast_shadows}},light_hook({?TAG,type}, pointlight)]},
       {hframe,
	[{vframe,[{label,"Bias"},
		  {label,"Res"},
		  {label,"Radius"}]},
	 {vframe,[{text,Bias,[{range,0.0,1.0},{key,{?TAG,bias}}]},
		  {text,Res,[{range,0,10000},{key,{?TAG,res}}]},
		  {text,Radius,[{range,0,10000},{key,{?TAG,radius}}]}]}],
	[light_hook({?TAG,type}, softlight)]}]}];
light_dialog(_Name, spot, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_SPOT_TYPE),
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Blend = proplists:get_value(blend, Ps, ?DEF_BLEND),
    Mode = proplists:get_value(mode, Ps, ?DEF_MODE),
    Photons = proplists:get_value(photons, Ps, ?DEF_PHOTONS),
    Depth = proplists:get_value(depth, Ps, ?DEF_DEPTH),
    Fixedradius = proplists:get_value(fixedradius, Ps, ?DEF_FIXEDRADIUS),
    Search = proplists:get_value(search, Ps, ?DEF_SEARCH),
    Mindepth = proplists:get_value(mindepth, Ps, ?DEF_MINDEPTH),
    Cluster = proplists:get_value(cluster, Ps, ?DEF_CLUSTER),
    UseQMC = proplists:get_value(use_QMC, Ps, ?DEF_USE_QMC),
    [{hframe,
      [{hradio,[{"Spotlight",spotlight},
		{"Photonlight",photonlight}],Type,[layout,{key,{?TAG,type}}]},
       {menu,[{"Diffuse",diffuse},{"Caustic",caustic}],Mode,
	[{key,{?TAG,mode}},light_hook({?TAG,type}, photonlight)]},
       {"Use QMC",UseQMC,[{key,{?TAG,use_QMC}},
		       light_hook({?TAG,type}, photonlight)]}]},
     {hframe,
      [{"Cast Shadows",CastShadows,[{key,{?TAG,cast_shadows}}]},
       {label,"Blend"},{text,Blend,[{range,0.0,100.0},{key,{?TAG,blend}}]}],
      [light_hook({?TAG,type}, spotlight)]},
     {hframe,[{vframe,[{label,"Photons"},
		       {label,"Depth"},
		       {label,"Fixedradius"}]},
	      {vframe,[{text,Photons,[{range,0,1000000},
				      {key,{?TAG,photons}}]},
		       {text,Depth,[{range,1,100},{key,{?TAG,depth}}]},
		       {text,Fixedradius,[{range,1.0,1000000.0},
					  {key,{?TAG,fixedradius}}]}]},
	      {vframe,[{label,"Search"},
		       {label,"Mindepth"},
		       {label,"Cluster"}]},
	      {vframe,[{text,Search,[{range,0,1000000},
				     {key,{?TAG,search}}]},
		       {text,Mindepth,[{range,0,1000000},
				       {key,{?TAG,mindepth}}]},
		       {text,Cluster,[{range,0.0,1000000.0},
				      {key,{?TAG,cluster}}]}]}],
      [light_hook({?TAG,type}, photonlight)]}];
light_dialog(_Name, infinite, Ps) ->
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND),
    BgColor = proplists:get_value(background_color, Ps, ?DEF_BACKGROUND_COLOR),
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Turbidity = proplists:get_value(turbidity, Ps, ?DEF_TURBIDITY),
    [{"Cast Shadows",CastShadows,[{key,{?TAG,cast_shadows}}]},
     {vframe,
      [{hradio,[{"Constant",constant},
		{"Sunsky",sunsky},
		{"None", undefined}],Bg,[layout,{key,{?TAG,background}}]},
       {hframe,[{label,"Color"},{color,BgColor,[{key,{?TAG,background_color}}]}],
	[light_hook({?TAG,background}, constant)]},
       {hframe,[{label,"Turbidity"},{text,Turbidity,[{range,0.0,100.0},
						     {key,{?TAG,turbidity}}]}],
	[light_hook({?TAG,background}, sunsky)]}],
      [{title,"Background"}]}];
light_dialog(_Name, ambient, Ps) ->
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND),
    BgFnameImage = proplists:get_value(background_filename_image, Ps, 
				       ?DEF_BACKGROUND_FILENAME),
    BrowsePropsImage = [{dialog_type,open_dialog},
			{extensions,[{".jpg","JPEG compressed image"},
				     {".tga","Targa bitmap"}]}],
    BgFnameHDRI = proplists:get_value(background_filename_HDRI, Ps, 
				      ?DEF_BACKGROUND_FILENAME),
    BrowsePropsHDRI = [{dialog_type,open_dialog},
		       {extensions,[{".hdr","High Dynamic Range Image"}]}],
    BgExpAdj = proplists:get_value(background_exposure_adjust, Ps, 
				   ?DEF_BACKGROUND_EXPOSURE_ADJUST),
    BgMapping = proplists:get_value(background_mapping, Ps, 
				    ?DEF_BACKGROUND_MAPPING),
    BgPower = proplists:get_value(background_power, Ps, 
				  ?DEF_BACKGROUND_POWER),
    Type = proplists:get_value(type, Ps, ?DEF_AMBIENT_TYPE),
    Samples = proplists:get_value(samples, Ps, ?DEF_SAMPLES),
    Depth = proplists:get_value(depth, Ps, ?DEF_DEPTH),
    UseQMC = proplists:get_value(use_QMC, Ps, ?DEF_USE_QMC),
    [{hframe,
      [{hradio,[{"Hemilight",hemilight},
		{"Pathlight",pathlight}],Type,[layout,{key,{?TAG,type}}]},
       {"Use QMC",UseQMC,[{key,{?TAG,use_QMC}}]}]},
     {hframe,[{label,"Samples"}, 
	      {text,Samples,[{range,1,1000000},{key,{?TAG,samples}}]},
	      {hframe,[{label,"Depth"},
		       {text,Depth,[{range,1,100},{key,{?TAG,depth}}]}],
	       [light_hook({?TAG,type}, pathlight)]}]},
     {vframe,
      [{hradio,[{"HDRI",'HDRI'},
		{"Image",image},
		{"None", undefined}],Bg,[layout,{key,{?TAG,background}}]},
       {hframe,[{label,"Filename"},
		{button,{text,BgFnameHDRI,
			 [{key,{?TAG,background_filename_HDRI}},
			  {props,BrowsePropsHDRI}]}}],
	[light_hook({?TAG,background}, ['HDRI'])]},
       {hframe,[{label,"Filename"},
		{button,{text,BgFnameImage,
			 [{key,{?TAG,background_filename_image}},
			  {props,BrowsePropsImage}]}}],
	[light_hook({?TAG,background}, [image])]},
       {hframe,[{label,"Exposure Adjust"},
		{text,BgExpAdj,[{key,{?TAG,background_exposure_adjust}},
				{range,{-128,127}}]},
		{menu,[{"Angular Map",probe},{"Spherical Map",spherical}],
		 BgMapping,[{key,{?TAG,background_mapping}}]}],
	[light_hook({?TAG,background}, 'HDRI')]},
       {hframe,[{label,"Power"},
		{text,BgPower,[{key,{?TAG,background_power}},
			       {range,{0.0,128.0}}]}],
	[light_hook({?TAG,background}, image)]}],
      [{title,"Background"}]}];
light_dialog(_Name, area, Ps) ->
    ArealightSamples = proplists:get_value(arealight_samples, Ps, 
					   ?DEF_AREALIGHT_SAMPLES),
    ArealightPsamples = proplists:get_value(arealight_psamples, Ps, 
					    ?DEF_AREALIGHT_PSAMPLES),
    Dummy = proplists:get_value(dummy, Ps, ?DEF_DUMMY),
    [{"Global Photonlight Dummy",Dummy,[{key,{?TAG,dummy}}]},
     {hframe,[{label,"Samples"},
	      {text,ArealightSamples,[{range,{1,1000000}},
				      {key,{?TAG,arealight_samples}},
				      bhook(disabled, {?TAG,dummy})]},
	      {label,"Penumbra Samples"},
	      {text,ArealightPsamples,[{range,{1,1000000}},
				       {key,{?TAG,arealight_psamples}},
				       bhook(disabled, {?TAG,dummy})]}]}];
light_dialog(_Name, _Type, _Ps) ->
%%%    erlang:display({?MODULE,?LINE,{_Name,_Type,_Ps}}),
    [].

light_hook(Key, Values) when list(Values) ->
    {hook,
     fun (is_minimized, {_Var,I,Sto}) when is_integer(Key) ->
	     not lists:member(gb_trees:get(I+Key, Sto), Values);
	 (is_minimized, {_Var,_I,Sto}) ->
	     not lists:member(gb_trees:get(Key, Sto), Values);
	 (_, _) -> void 
     end};
light_hook(Key, Value) -> light_hook(Key, [Value]).


light_result(_Name, Ps0, 
	     [{{?TAG,minimized},Minimized},{{?TAG,power},Power}|Res0]) ->
    {LightPs0,Res1} = light_result(Res0),
    LightPs = [{Key,Val} || {{?TAG,Key},Val} <- LightPs0],
    Ps = [{?TAG,[{minimized,Minimized},{power,Power}|LightPs]}
	  |keydelete(?TAG, 1, Ps0)],
%    erlang:display({?MODULE,?LINE,[Ps,Res1]}),
    {Ps,Res1}.

light_result([{{?TAG,type},pointlight}|_]=Ps) ->
    split_list(Ps, 5);
light_result([{{?TAG,type},softlight}|_]=Ps) ->
    split_list(Ps, 5);
light_result([{{?TAG,type},spotlight}|_]=Ps) ->
    split_list(Ps, 11);
light_result([{{?TAG,type},photonlight}|_]=Ps) ->
    split_list(Ps, 11);
light_result([_,{{?TAG,background},_}|_]=Ps) -> % infinite
    split_list(Ps, 4);
light_result([_,{{?TAG,arealight_samples},_}|_]=Ps) -> % area
    split_list(Ps, 3);
light_result([{{?TAG,type},hemilight}|_]=Ps) ->
    split_list(Ps, 10);
light_result([{{?TAG,type},pathlight}|_]=Ps) ->
    split_list(Ps, 10);
light_result(Ps) ->
%    erlang:display({?MODULE,?LINE,Ps}),
    {[],Ps}.



pref_edit(St) ->
    Dialogs = get_pref(dialogs, ?DEF_DIALOGS),
    Renderer = get_pref(renderer, ?DEF_RENDERER),
    BrowseProps = [{dialog_type,open_dialog},{directory,"/"},
		   case os:type() of
		       {win32,_} -> 
			   {extensions,[{".exe","Windows Executable"}]};
		       _-> {extensions,[]}
		   end],
    Dialog =
	[{vframe,[{menu,[{"Disabled Dialogs",disabled},
			 {"Automatic Dialogs",auto},
			 {"Enabled Dialogs",enabled}],
		   Dialogs,[{key,dialogs}]},
		  {label,"Rendering Command:"},
		  {button,{text,Renderer,
			   [{key,renderer},{props,BrowseProps}]}}]}],
    wpa:dialog("YafRay Options", Dialog, 
	       fun (Attr) -> pref_result(Attr,St) end).

pref_result(Attr, St) ->
    set_pref(Attr),
    init_pref(),
    St.




export_dialog(Operation) ->
    SubDiv = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
    SaveAlpha = get_pref(save_alpha, ?DEF_SAVE_ALPHA),
    Gamma = get_pref(gamma, ?DEF_GAMMA),
    Exposure = get_pref(exposure, ?DEF_EXPOSURE),
    AA_passes = get_pref(aa_passes, ?DEF_AA_PASSES),
    AA_minsamples = get_pref(aa_minsamples, ?DEF_AA_MINSAMPLES),
    AA_pixelwidth = get_pref(aa_pixelwidth, ?DEF_AA_PIXELWIDTH),
    AA_threshold = get_pref(aa_threshold, ?DEF_AA_THRESHOLD),
    Raydepth = get_pref(raydepth, ?DEF_RAYDEPTH),
    Bias = get_pref(bias, ?DEF_BIAS),
    Width = get_pref(width, ?DEF_WIDTH),
    Height = get_pref(height, ?DEF_HEIGHT),
    BgColor = get_pref(background_color, ?DEF_BACKGROUND_COLOR),
    LoadImage = get_pref(load_image, ?DEF_LOAD_IMAGE),
    Options = get_pref(options, ?DEF_OPTIONS),
    DofFilter = get_pref(dof_filter, ?DEF_DOF_FILTER),
    NearBlur = get_pref(near_blur, ?DEF_NEAR_BLUR),
    FarBlur = get_pref(far_blur, ?DEF_FAR_BLUR),
    DofScale = get_pref(dof_scale, ?DEF_DOF_SCALE),
    AntinoiseFilter = get_pref(antinoise_filter, ?DEF_ANTINOISE_FILTER),
    AntinoiseRadius = get_pref(antinoise_radius, ?DEF_ANTINOISE_RADIUS),
    AntinoiseMaxDelta = get_pref(antinoise_radius, ?DEF_ANTINOISE_MAX_DELTA),
    FogDensity = get_pref(fog_density, ?DEF_FOG_DENSITY),
    FogColor = get_pref(fog_color, ?DEF_FOG_COLOR),
    AA_pixelwidthFlags = [{range,{1.0,2.0}},{key,aa_pixelwidth}],
    [{hframe,[{label,"Sub-division Steps"},
	      {text,SubDiv,[{key,subdivisions},{range,0,4}]}],
      [{title,"Pre-rendering"}]},
     {hframe,
      [{vframe,[{label,"AA_passes"},
		{label,"AA_threshold"},
		{label,"Raydepth"},
		{label,"Gamma"}]},
       {vframe,[{text,AA_passes,[{range,{0,1000}},{key,aa_passes}]},
		{text,AA_threshold,[{range,{0.0,100.0}},
				    {key,aa_threshold}]},
		{text,Raydepth,[{range,{1,1000}},{key,raydepth}]},
		{text,Gamma,[{range,{0.0,10.0}},{key,gamma}]}]},
       {vframe,[{label,"AA_minsamples"},
		{label,"AA_pixelwidth"},
		{label,"Bias"},
		{label,"Exposure"}]},
       {vframe,[{text,AA_minsamples,[{range,{1,1000}},{key,aa_minsamples}]},
		{text,AA_pixelwidth,AA_pixelwidthFlags},
		{text,Bias,[{range,{0.0,1.0}},{key,bias}]},
		{text,Exposure,[{range,{0.0,32.0}},{key,exposure}]}]},
       {vframe,[{"Alpha Channel",SaveAlpha,[{key,save_alpha}]},
		{slider,AA_pixelwidthFlags}]}],
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
      [{title,"Camera"}]},
     {hframe,
      [{vframe,[{label,"Fog Density"},
		{"Antinoise",AntinoiseFilter,[{key,antinoise_filter}]},
		{"DOF",DofFilter,[{key,dof_filter}]}]},
       {vframe,
	[{slider,{text,FogDensity,[{range,{0.0,1.0}},
				   {key,fog_density}]}},
	 {hframe,
	  [{vframe,[{label,"Radius"},
		    {label,"Near Blur"},
		    {label,"Scale"}]},
	   {vframe,[{text,AntinoiseRadius,[{range,{0.0,100.0}},
					   {key,antinoise_radius},
					   bhook(enabled, antinoise_filter)]},
		    {text,NearBlur,[{range,{0.0,100.0}},{key,near_blur},
				    bhook(enabled, dof_filter)]},
		    {text,DofScale,[{range,{0.0,100.0}},{key,dof_scale},
				    bhook(enabled, dof_filter)]}]}]}]},
       {vframe,[{label,"Fog Color"},
		{label,"Max Delta"},
		{label,"Far Blur"}]},
       {vframe,[{color,FogColor,[{key,fog_color}]},
		{text,AntinoiseMaxDelta,[{range,{0.0,100.0}},
					 {key,antinoise_max_delta},
					 bhook(enabled, antinoise_filter)]},
		{text,FarBlur,[{range,{0.0,100.0}},{key,far_blur},
			       bhook(enabled, dof_filter)]}]}],
      [{title,"Filters"}]}
     |
     case Operation of 
	 render ->
	     [{hframe,[{label,"Options"},
		       {text,Options,[{key,options}]},
		       {"Load Image",LoadImage,[{key,load_image}]}],
	       [{title,"Rendering Job"}]}];
	 export ->
	     [];
	 export_selected ->
	     []
     end].

%% Boolean hook
%%
%% Used to enable/disable or minimize/maximize a field depending
%% on a boolean control field.
%%
bhook(Type, Tag) ->
    {hook,fun (is_disabled, {_Var,_I,Sto}) ->
		  case Type of
		      enabled -> not gb_trees:get(Tag, Sto);
		      disabled -> gb_trees:get(Tag, Sto);
		      _ -> void
		  end;
	      (is_minimized, {_Var,_I,Sto}) ->
		  case Type of
		      maximized -> not gb_trees:get(Tag, Sto);
		      minimized -> gb_trees:get(Tag, Sto);
		      _ -> void
		  end;
	      (_, _) -> void end}.



%%% Export and rendering functions
%%%

export(Attr, Filename, #e3d_file{objs=Objs,mat=Mats,creator=Creator}) ->
    ExportTS = erlang:now(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    ExportDir = filename:dirname(Filename),
    {ExportFile,RenderFile} =
	case Render of
	    true ->
		{filename:join(ExportDir, 
			       ?MODULE_STRING++"-"++uniqstr()++".xml"),
		 Filename};
	    false ->
		{Filename,
		 filename:rootname(Filename)++".tga"}
	end,
    F = open(ExportFile, export),
    io:format("Exporting to ~s~n", [ExportFile]),
    CameraName = "x_Camera",
    ConstBgName = "x_ConstBackground",
    Lights = proplists:get_value(lights, Attr, []),
    %%
    println(F, "<!-- ~s: Exported from ~s -->~n"++
	    "~n"++
	    "<scene>", [filename:basename(ExportFile), Creator]),
    %%
    section(F, "Shaders"),
    MatsGb =
	foldl(fun ({Name,Mat}, Gb) -> 
		      export_shader(F, "w_"++format(Name), Mat, ExportDir),
		      println(F),
		      gb_trees:insert(Name, Mat, Gb)
	      end, gb_trees:empty(), Mats),
    %%
    section(F, "Objects"),
    foreach(fun (#e3d_object{name=Name,obj=Mesh}) ->
		    export_object(F, "w_"++format(Name), Mesh, MatsGb),
		    println(F)
	    end, Objs),
    %%
    section(F, "Lights"),
    BgLights = 
	reverse(
	  foldl(fun ({Name,Ps}=Light, Bgs) -> 
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
    case proplists:get_value(dof_filter, Attr, ?DEF_DOF_FILTER) of
	true ->
	    export_filter(F, "x_Dof", dof, Attr),
	    println(F);
	false ->
	    ok
    end,
    case proplists:get_value(antinoise_filter, Attr, ?DEF_ANTINOISE_FILTER) of
	true ->
	    export_filter(F, "x_Antinoise", antinoise, Attr),
	    println(F);
	false ->
	    ok
    end,
    export_render(F, CameraName, BgName, filename:basename(RenderFile), Attr),
    %%
    println(F),
    println(F, "</scene>"),
    close(F),
    %%
    RenderTS = erlang:now(),
    Renderer = get_var(renderer),
    Options = proplists:get_value(options,Attr,?DEF_OPTIONS),
    LoadImage = proplists:get_value(load_image,Attr,?DEF_LOAD_IMAGE),
    case {Renderer,Render} of
	{_,false} ->
	    io:format("Export time:     ~.3f s~n", 
		      [now_diff(RenderTS, ExportTS)]),
	    ok;
	{false,_} ->
	    %% Should not happen since the file->render dialog
	    %% must have been disabled
	    file:delete(ExportFile),
	    no_renderer;
	_ ->
	    Parent = self(),
	    spawn_link(
	      fun () -> 
		      set_var(export_ts, ExportTS),
		      set_var(render_ts, RenderTS),
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
    ExportTS = get_var(export_ts),
    RenderTS = get_var(render_ts),
    FinishTS = erlang:now(),
    io:format("Export time:     ~.3f s~n"++
	      "Render time:     ~.3f s~n"++
	      "Total time:      ~.3f s~n",
	      [now_diff(RenderTS, ExportTS),
	       now_diff(FinishTS, RenderTS),
	       now_diff(FinishTS, ExportTS)]),
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
    Maps = proplists:get_value(maps, Mat, []),
    YafRay = proplists:get_value(?TAG, Mat, []),
    Modulators = proplists:get_value(modulators, YafRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when list(Ps) ->
		  case export_texture(F, [Name,$_,format(N)], 
				      Maps, ExportDir, M) of
		      off -> N+1;
		      ok ->
			  println(F),
			  N+1
		  end;
	      (_, N) ->
		  N % Ignore old modulators
	  end, 1, Modulators),
    println(F, "<shader type=\"generic\" name=\"~s\">~n"++ 
	    "    <attributes>", [Name]),
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    Color = alpha(DiffuseA),
    Specular = alpha(proplists:get_value(specular, OpenGL)),
    Ambient = rgba2rgb(proplists:get_value(ambient, OpenGL)),
    DefTransmitted = def_transmitted(DiffuseA),
    export_rgb(F, color, Color),
    export_rgb(F, specular, Specular),
    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "        <hard value=\"~.10f\"/>", 
		   [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, reflected, 
	       proplists:get_value(reflected, YafRay, Ambient)),
    export_rgb(F, transmitted, 
	       proplists:get_value(transmitted, YafRay, DefTransmitted)),
    case proplists:get_value(fresnel2, YafRay, false) of
	true ->
	    export_rgb(F, reflected2, 
		       proplists:get_value(reflected2, YafRay, Ambient)),
	    export_rgb(F, transmitted2, 
		       proplists:get_value(transmitted2, YafRay, 
					   DefTransmitted));
	false -> ok
    end,
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    TIR = proplists:get_value(tir, YafRay, ?DEF_TIR),
    FastFresnel = proplists:get_value(fast_fresnel, YafRay, ?DEF_FAST_FRESNEL),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    println(F, "        <IOR value=\"~.10f\"/>~n"
	    "        <tir value=\"~s\"/>~n"
	    "        <fast_fresnel value=\"~s\"/>~n"
	    "        <min_refle value=\"~.10f\"/>~n"
	    "    </attributes>", 
	    [IOR,format(TIR),format(FastFresnel),MinRefle]),
    foldl(fun ({modulator,Ps}=M, N) when list(Ps) ->
		  case export_modulator(F, [Name,$_,format(N)], 
					Maps, M, Opacity) of
		      off -> N+1;
		      ok ->
			  println(F),
			  N+1
		  end;
	      (_, N) ->
		  N % Ignore old modulators
	  end, 1, Modulators),
    println(F, "</shader>").

alpha({R,G,B,A}) -> {R*A,G*A,B*A}.

export_texture(F, Name, Maps, ExportDir, {modulator,Ps}) when list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
	{false,_,_} ->
	    off;
	{true,_,image} ->
	    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
	    export_texture(F, Name, image, Filename);
	{true,_,jpeg} -> %% Old tag
	    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
	    export_texture(F, Name, image, Filename);
	{true,_,{map,Map}} ->
	    case proplists:get_value(Map, Maps, undefined) of
		undefined ->
		    exit({unknown_texture_map,{?MODULE,?LINE,[Name,Map]}});
		#e3d_image{name=ImageName}=Image ->
		    MapFile = ImageName++".tga",
		    ok = e3d_image:save(Image, 
					filename:join(ExportDir, MapFile)),
		    export_texture(F, Name, image, MapFile)
	    end;
	{true,_,Type} ->
	    export_texture(F, Name, Type, Ps)
    end.

export_texture(F, Name, image, Filename) ->
    println(F, "<texture type=\"image\" name=\"~s\">~n"++
	    "    <filename value=\"~s\"/>~n"++
	    "</texture>", [Name,Filename]);
export_texture(F, Name, Type, Ps) ->
    println(F, "<texture type=\"~s\" name=\"~s\">", [format(Type),Name]),
    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    export_rgb(F, color1, Color1),
    export_rgb(F, color2, Color2),
    println(F, "    <depth value=\"~w\"/>", [Depth]),
    if Type =/= clouds ->
	    Turbulence = proplists:get_value(turbulence, Ps, 
					     ?DEF_MOD_TURBULENCE),
	    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
	    println(F, "    <turbulence value=\"~.6f\"/>~n"++
		    "    <hard value=\"~s\"/>", [Turbulence,format(Hard)]);
       true ->
	    ok
    end,
    case Type of
	marble ->
	    Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
	    println(F, "    <sharpness value=\"~.6f\"/>", [Sharpness]);
	wood ->
	    RingscaleX = proplists:get_value(ringscale_x, Ps, 
					     ?DEF_MOD_RINGSCALE_X),
	    RingscaleZ = proplists:get_value(ringscale_z, Ps, 
					     ?DEF_MOD_RINGSCALE_Z),
	    %% Coordinate rotation, see export_pos/2.
	    println(F, "    <ringscale_x value=\"~.6f\"/>~n"++
		    "    <ringscale_y value=\"~.6f\"/>",
		    [RingscaleX,RingscaleZ]);
	clouds ->
	    ok
    end,
    println(F, "</texture>").

export_modulator(F, Texname, Maps, {modulator,Ps}, Opacity) when list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
	{false,_,_} ->
	    off;
	{true,Mode,Type} ->
	    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
	    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
	    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
	    Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
	    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
	    Ambient = proplists:get_value(ambient, Ps, ?DEF_MOD_AMBIENT),
	    Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
	    Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
	    Color = Diffuse * Opacity,
	    HardValue = Shininess,
	    Transmission = Diffuse * (1.0 - Opacity),
	    Reflection = Ambient,
	    TexCo = 
		case Type of
		    image -> "texco=\"uv\" ";
		    jpeg -> "texco=\"uv\" ";
		    {map,_} -> "texco=\"uv\" ";
		    marble -> "texco=\"global\" ";
		    wood -> "texco=\"global\" ";
		    clouds -> "texco=\"global\" ";
		    _ -> ""
		end,
	    println(F, "        <modulator texname=\"~s\" mode=\"~s\"~n"++
		    "         "++TexCo++"clipping=\"repeat\"~n"++
		    "         sizex=\"~.3f\" sizey=\"~.3f\" sizez=\"~.3f\">~n"++
		    "            <color value=\"~.3f\"/>~n"++
		    "            <specular value=\"~.3f\"/>~n"++
		    "            <hard value=\"~.3f\"/>~n"++
		    "            <transmission value=\"~.3f\"/>~n"++
		    "            <reflection value=\"~.3f\"/>~n"++
		    "            <normal value=\"~.3f\"/>~n"++
		    "        </modulator>", 
		    [Texname,format(Mode),SizeX,SizeY,SizeZ,
		     Color,Specular,HardValue,Transmission,Reflection,Normal])
    end.



export_rgb(F, Type, {R,G,B,_}) ->
    export_rgb(F, Type, {R,G,B});
export_rgb(F, Type, {R,G,B}) ->
    println(F, ["        <",format(Type)," r=\"",format(R),
		"\" g=\"",format(G),"\" b=\"",format(B),"\"/>"]).



%% Return object with arealight faces only
%%
export_object(F, NameStr, Mesh=#e3d_mesh{fs=Fs}, MatsGb) ->
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    case reverse(sort(count_equal(MM))) of
	[] -> ok;
	[{_Count,DefaultMaterial}|_] ->
	    MatPs = gb_trees:get(DefaultMaterial, MatsGb),
	    export_object_1(F, NameStr, Mesh, DefaultMaterial, MatPs)
    end.

%% Count the number of subsequent equal elements in the list.
%% Returns list of {Count,Element}.
%%
count_equal([H|T]) ->
    count_equal(T, 1, H, []).
%%
count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).

export_object_1(F, NameStr, Mesh0=#e3d_mesh{he=He0}, DefaultMaterial, MatPs) ->
    OpenGL = proplists:get_value(opengl, MatPs),
    YafRay = proplists:get_value(?TAG, MatPs, []),
    UseHardness = proplists:get_value(use_hardness, YafRay, ?DEF_USE_HARDNESS),
    Caus = proplists:get_value(caus, YafRay, ?DEF_CAUS),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    AutosmoothAngle = 
	proplists:get_value(autosmooth_angle, YafRay, ?DEF_AUTOSMOOTH_ANGLE),
    Autosmooth = proplists:get_value(autosmooth, YafRay, 
				     if AutosmoothAngle == 0.0 -> false;
					true -> ?DEF_AUTOSMOOTH end),
    Shadow = proplists:get_value(shadow, YafRay, ?DEF_SHADOW),
    EmitRad = proplists:get_value(emit_rad, YafRay, ?DEF_EMIT_RAD),
    RecvRad = proplists:get_value(recv_rad, YafRay, ?DEF_RECV_RAD),
    %% Pre-process mesh
    Mesh1 = #e3d_mesh{} = 
	case {He0,UseHardness} of
	    {[_|_],true} ->
		io:format("Mesh ~s: slitting hard edges...", [NameStr]),
		M1 = e3d_mesh:slit_hard_edges(Mesh0, [slit_end_vertices]),
		io:format("done~n"),
		M1;
	    _ -> Mesh0
	end,
    io:format("Mesh ~s: triangulating...", [NameStr]),
    #e3d_mesh{fs=Fs,vs=Vs,vc=Vc,tx=Tx} = e3d_mesh:triangulate(Mesh1),
    io:format("done~n"),
    io:format("Mesh ~s: exporting...", [NameStr]),
    %%
    println(F, "<object name=\"~s\" shader_name=\"~s\" shadow=\"~s\"~n"++
	    "        "++
	    case Caus of true -> "caus_IOR=\"~.10f\" ";
		false -> ""
	    end++"emit_rad=\"~s\" recv_rad=\"~s\">",
	    [NameStr,"w_"++format(DefaultMaterial),format(Shadow)|
	     case Caus of true -> [IOR];
		 false -> []
	     end]++[format(EmitRad),format(RecvRad)]),
    println(F, "    <attributes>"),
    case Caus of true ->
	    Ambient = rgba2rgb(proplists:get_value(ambient, OpenGL)),
	    Reflected = proplists:get_value(reflected, YafRay, Ambient),
	    DefTransmitted = 
		def_transmitted(proplists:get_value(diffuse, OpenGL)),
	    Transmitted = 
		proplists:get_value(transmitted, YafRay, DefTransmitted),
	    export_rgb(F, caus_rcolor, Reflected),
	    export_rgb(F, caus_tcolor, Transmitted);
	false -> ok
    end,
    println(F, "    </attributes>"),
    case Autosmooth of
	false ->
	    println(F, "    <mesh>");
	true ->
	    println(F, "    <mesh autosmooth=\"~.3f\">", [AutosmoothAngle])
    end,
    println(F, "        <points>"),
    export_vertices(F, Vs),
    println(F, "        </points>~n"++
	    "        <faces>", []),
    export_faces(F, Fs, DefaultMaterial, list_to_tuple(Tx), list_to_tuple(Vc)),
    println(F, "        </faces>~n"++
	    "    </mesh>~n"++
	    "</object>", []),
    io:format("done~n").



export_vertices(_F, []) ->
    ok;
export_vertices(F, [Pos|T]) ->
    export_pos(F, p, Pos),
    export_vertices(F, T).



%% The coordinate system is rotated to make sunsky background
%% and environment images work as expected. 
%% It assumes X=South Y=East Z=Up in YafRay coordinates.
%% Hence Z=South, X=East, Y=Up in Wings coordinates.
%%
export_pos(F, Type, {X,Y,Z}) ->
    println(F, ["        <",format(Type)," x=\"",format(Z),
		"\" y=\"",format(X),"\" z=\"",format(Y),"\"/>"]).



export_faces(_F, [], _DefMat, _TxT, _VColT) ->
    ok;
export_faces(F, [#e3d_face{vs=[A,B,C],vc=VCols,tx=Tx,mat=[Mat|_]}|T], 
	     DefaultMaterial, TxT, VColT) ->
    Shader =
	case Mat of
	    DefaultMaterial -> "";
	    _ -> [" shader_name=\"w_",format(Mat),"\""]
	end,
    UV = case {TxT,Tx} of
	     {{},[]} -> "";
	     {{},_} ->
		 io:format("WARNING! Face refers to non-existing "
			   "texture coordinates~n"),
		 "";
	     {_,[]} ->
		 %%io:format("WARNING! Face missing texture coordinates~n"),
		 "";
	     {_,[Ta,Tb,Tc]} ->
		 {Ua,Va} = element(1+Ta, TxT),
		 {Ub,Vb} = element(1+Tb, TxT),
		 {Uc,Vc} = element(1+Tc, TxT),
		 [io_lib:nl(),"           u_a=\"",format(Ua),
		  "\" v_a=\"",format(-Va),"\"",
		  io_lib:nl(),"           u_b=\"",format(Ub),
		  "\" v_b=\"",format(-Vb),"\"",
		  io_lib:nl(),"           u_c=\"",format(Uc),
		  "\" v_c=\"",format(-Vc),"\""];
	     _ ->
		 io:format("WARNING! Face has ~w =/= 3 texture coordinates~n",
			    [length(Tx)]),
		 ""
	 end,
    VCol = case {VColT,VCols} of
	       {{},[]} -> "";
	       {{},_} ->
		   io:format("WARNING! Face refers to non-existing "
			     "vertex colors~n"),
		   "";
	       {_,[]} ->
		   %%io:format("WARNING! Face missing vertex colors~n"),
		   "";
	       {_,[VcA,VcB,VcC]} ->
		   {VcAr,VcAg,VcAb} = element(1+VcA, VColT),
		   {VcBr,VcBg,VcBb} = element(1+VcB, VColT),
		   {VcCr,VcCg,VcCb} = element(1+VcC, VColT),
		   [io_lib:nl(),"           vcol_a_r=\"",format(VcAr),
		    "\" vcol_a_g=\"",format(VcAg),
		    "\" vcol_a_b=\"",format(VcAb),"\"",
		    io_lib:nl(),"           vcol_b_r=\"",format(VcBr),
		    "\" vcol_b_g=\"",format(VcBg),
		    "\" vcol_b_b=\"",format(VcBb),"\"",
		    io_lib:nl(),"           vcol_c_r=\"",format(VcCr),
		    "\" vcol_c_g=\"",format(VcCg),
		    "\" vcol_c_b=\"",format(VcCb),"\""];
	       _ ->
		   io:format("WARNING! Face has ~w =/= 3 vertex colors~n",
			     [length(VCols)]),
		   ""
	   end,
    println(F, ["        <f a=\"",format(A),
		"\" b=\"",format(B),"\" c=\"",format(C),"\"",
		Shader,UV,VCol,"/>"]),
    export_faces(F, T, DefaultMaterial, TxT, VColT).



export_light(F, Name, Ps) ->
    case proplists:get_value(visible, Ps, true) of
	true ->
	    OpenGL = proplists:get_value(opengl, Ps, []),
	    YafRay = proplists:get_value(?TAG, Ps, []),
	    Type = proplists:get_value(type, OpenGL, []),
	    export_light(F, Name, Type, OpenGL, YafRay);
	_ ->
	    undefined
    end.

export_light(F, Name, point, OpenGL, YafRay) ->
    Power = proplists:get_value(power, YafRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
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
    case Bg of 
	sunsky ->
	    Bg;
	_ ->
	    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
	    CastShadows = 
		proplists:get_value(cast_shadows, YafRay, ?DEF_CAST_SHADOWS),
	    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
	    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
	    println(F,"<light type=\"sunlight\" name=\"~s\" "++
		    "power=\"~.3f\" cast_shadows=\"~s\">", 
		    [Name, Power,format(CastShadows)]),
	    export_pos(F, from, Position),
	    export_rgb(F, color, Diffuse),
	    println(F, "</light>"),
	    Bg
    end;
export_light(F, Name, spot, OpenGL, YafRay) ->
    Power = proplists:get_value(power, YafRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, ?DEF_CONE_ANGLE),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    Type = proplists:get_value(type, YafRay, ?DEF_SPOT_TYPE),
    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\" ", 
	    [Type,Name,Power]),
    case Type of
	spotlight ->
	    CastShadows = 
		proplists:get_value(cast_shadows, YafRay, ?DEF_CAST_SHADOWS),
	    SpotExponent = 
		proplists:get_value(spot_exponent, OpenGL, ?DEF_SPOT_EXPONENT),
	    Blend = proplists:get_value(blend, YafRay, ?DEF_BLEND),
	    println(F, "       cast_shadows=\"~s\" size=\"~.3f\"~n"++
		    "       beam_falloff=\"~.10f\" blend=\"~.3f\">", 
		    [format(CastShadows), ConeAngle, SpotExponent, Blend]);
	photonlight ->
	    Mode = proplists:get_value(mode, YafRay, ?DEF_MODE),
	    Photons = proplists:get_value(photons, YafRay, ?DEF_PHOTONS),
	    Depth = proplists:get_value(depth, YafRay, ?DEF_DEPTH),
	    Fixedradius = 
		proplists:get_value(fixedradius, YafRay, ?DEF_FIXEDRADIUS),
	    Search = proplists:get_value(search, YafRay, ?DEF_SEARCH),
	    Mindepth = proplists:get_value(mindepth, YafRay, ?DEF_MINDEPTH),
	    Cluster = proplists:get_value(cluster, YafRay, ?DEF_CLUSTER),
	    UseQMC = proplists:get_value(use_QMC, YafRay, ?DEF_USE_QMC),
	    case Mode of
		diffuse ->
		    println(F, "       mode=\"diffuse\"");
		_ ->
		    ok
	    end,
	    println(F, "       angle=\"~.3f\" photons=\"~w\" depth=\"\~w\"~n"++
		    "       fixedradius=\"~.10f\" search=\"~w\"~n"++
		    "       mindepth=\"~w\" cluster=\"~.10f\" use_QMC=\"~s\">",
		    [ConeAngle,Photons,Depth,
		     Fixedradius,Search,Mindepth,Cluster,format(UseQMC)])
    end,
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    println(F, "</light>"),
    undefined;
export_light(F, Name, ambient, OpenGL, YafRay) ->
    Bg = proplists:get_value(background, YafRay, ?DEF_BACKGROUND),
    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
    Type = proplists:get_value(type, YafRay, ?DEF_AMBIENT_TYPE),
    Samples = proplists:get_value(samples, YafRay, ?DEF_SAMPLES),
    UseQMC = proplists:get_value(use_QMC, YafRay, ?DEF_USE_QMC),
    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\" use_QMC=\"~s\"", 
	    [Type,Name,Power,format(UseQMC)]),
    case Type of
	hemilight ->
	    Ambient = proplists:get_value(ambient, OpenGL, 
					  ?DEF_BACKGROUND_COLOR),
	    println(F,"       samples=\"~w\">", [Samples]),
	    export_rgb(F, color, Ambient);
	pathlight ->
	    Depth = proplists:get_value(depth, YafRay, ?DEF_DEPTH),
	    println(F,"       samples=\"~w\" depth=\"~w\">", [Samples,Depth])
    end,
    println(F, "</light>"),
    Bg;
export_light(F, Name, area, OpenGL, YafRay) ->
    Color = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    #e3d_mesh{vs=Vs,fs=Fs} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs),
    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
    ArealightSamples = proplists:get_value(arealight_samples, YafRay, 
					   ?DEF_AREALIGHT_SAMPLES),
    ArealightPsamples = proplists:get_value(arealight_psamples, YafRay, 
					    ?DEF_AREALIGHT_PSAMPLES),
    Dummy = proplists:get_value(dummy, YafRay, ?DEF_DUMMY),
    foldl(
      fun(Face, I) ->
	      foldl(
		fun(#e3d_face{vs=V}, J) ->
			[A,B,C,D] = quadrangle_vertices(V, VsT),
			NameJ = Name++"_"++integer_to_list(J),
			println(F, "<light type=\"arealight\" "
				"name=\"~s\" power=\"~.3f\"~n"
				"       samples=\"~w\" psamples=\"~w\" "
				"dummy=\"~s\">", 
				[NameJ,Power,
				 ArealightSamples,ArealightPsamples,
				 format(Dummy)]),
			export_rgb(F, color, Color),
			export_pos(F, a, A),
			export_pos(F, b, B),
			export_pos(F, c, C),
			export_pos(F, d, D),
			println(F, "</light>"),
			J+1
		end,
		I,
		e3d_mesh:quadrangulate_face(Face, Vs))
      end,
      1,
      Fs),
    undefined;
export_light(_F, Name, Type, _OpenGL, _YafRay) ->
    io:format("WARNING: Ignoring unknown light \"~s\" type: ~p~n", 
	      [Name, format(Type)]),
    undefined.

%% Cut the longest edge of a triangle in half to make it a quad.
%% Lookup vertex positions.
%%
quadrangle_vertices([V1,V2,V3], VsT) -> 
    P1 = element(V1+1, VsT),
    P2 = element(V2+1, VsT),
    P3 = element(V3+1, VsT),
    [L12,L23,L31] = 
	[e3d_vec:dot(L, L) || 
	    L <- [e3d_vec:sub(P1, P2),e3d_vec:sub(P2, P3),
		  e3d_vec:sub(P3, P1)]],
    if L23 > L31 ->
	    if L12 > L23 -> [P1,e3d_vec:average([P1,P2]),P2,P3];
	       true -> [P1,P2,e3d_vec:average([P2,P3]),P3]
	    end;
       true -> [P1,P2,P3,e3d_vec:average([P3,P1])]
    end;
quadrangle_vertices([V1,V2,V3,V4], VsT) -> 
    [element(V1+1, VsT),element(V2+1, VsT),
     element(V3+1, VsT),element(V4+1, VsT)].



export_camera(F, Name, Attr) ->
    #camera_info{aim=Aim,distance_to_aim=Distance,elevation=El,
		 azimuth=Az,tracking={TrackX,TrackY},fov=Fov} =
	proplists:lookup(camera_info, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    Ro = math:pi()/180.0,
    Dist = limit_dist(Distance),
    %% Fov is horizontal angle from left to right border.
    %% YafRay focal plane is 1 unit high.
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
	    BgColor = proplists:get_value(background_color, YafRay, 
					  ?DEF_BACKGROUND_COLOR),
	    export_rgb(F, color, BgColor);
	sunsky ->
	    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
	    AddSun = (Power > 0.0),
	    Turbidity = proplists:get_value(turbidity, YafRay, ?DEF_TURBIDITY),
	    Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),
	    println(F, "~n            turbidity=\"~.3f\" add_sun=\"~s\">", 
		    [Turbidity,format(AddSun)]),
	    export_pos(F, from, Position);
	'HDRI' ->
	    BgFname = proplists:get_value(background_filename_HDRI, YafRay, 
					  ?DEF_BACKGROUND_FILENAME),
	    BgExpAdj = proplists:get_value(background_exposure_adjust, YafRay, 
					   ?DEF_BACKGROUND_EXPOSURE_ADJUST),
	    BgMapping = proplists:get_value(background_mapping, YafRay, 
					    ?DEF_BACKGROUND_MAPPING),
	    println(F, "~n            exposure_adjust=\"~w\" mapping=\"~s\">", 
		    [BgExpAdj,format(BgMapping)]),
	    println(F, "    <filename value=\"~s\" />", [BgFname]);
	image ->
	    BgFname = proplists:get_value(background_filename_image, YafRay,
					  ?DEF_BACKGROUND_FILENAME),
	    BgPower = proplists:get_value(background_power, YafRay, 
					   ?DEF_BACKGROUND_POWER),
	    println(F, " power=\"~.3f\">", [BgPower]),
	    println(F, "    <filename value=\"~s\" />", [BgFname])
    end,
    println(F, "</background>").


export_filter(F, Name, dof, Attr) ->
    #camera_info{distance_to_aim=Dist} = proplists:lookup(camera_info, Attr),
    Focus = limit_dist(Dist),
    NearBlur = proplists:get_value(near_blur, Attr),
    FarBlur = proplists:get_value(far_blur, Attr),
    Scale = proplists:get_value(dof_scale, Attr),
    println(F, "<filter name=\"~s\" type=\"dof\" focus=\"~s\"", 
	    [Name,format(Focus)]),
    println(F, "        near_blur=\"~.3f\" far_blur=\"~.3f\" scale=\"~.3f\">",
	    [NearBlur,FarBlur,Scale]),
    println(F, "</filter>");
export_filter(F, Name, antinoise, Attr) ->
    Radius = proplists:get_value(antinoise_radius, Attr),
    MaxDelta = proplists:get_value(antinoise_max_delta, Attr),
    println(F, "<filter name=\"~s\" type=\"antinoise\"", [Name]),
    println(F, "        radius=\"~.3f\" max_delta=\"~.3f\">",
	    [Radius,MaxDelta]),
    println(F, "</filter>");
export_filter(_F, Name, Type, _Attr) ->
    io:format("WARNING: Ignoring unknown filter \"~s\" type: ~w~n", 
	      [Name, Type]),
    ok.

export_render(F, CameraName, BackgroundName, Outfile, Attr) ->
    AA_passes = proplists:get_value(aa_passes, Attr),
    AA_minsamples = proplists:get_value(aa_minsamples, Attr),
    AA_pixelwidth = proplists:get_value(aa_pixelwidth, Attr),
    AA_threshold = proplists:get_value(aa_threshold, Attr),
    Raydepth = proplists:get_value(raydepth, Attr),
    Bias = proplists:get_value(bias, Attr),
    SaveAlpha = proplists:get_value(save_alpha, Attr),
    Gamma = proplists:get_value(gamma, Attr),
    Exposure = proplists:get_value(exposure, Attr),
    FogColor = proplists:get_value(fog_color, Attr),
    FogDensity = proplists:get_value(fog_density, Attr),
    println(F, "<render camera_name=\"~s\" "
	    "AA_passes=\"~w\" raydepth=\"~w\"~n"
	    "        bias=\"~.10f\" AA_threshold=\"~.10f\""
	    "AA_minsamples=\"~w\" AA_pixelwidth=\"~.10f\">~n"
	    "    <background_name value=\"~s\"/>~n"
	    "    <outfile value=\"~s\"/>~n"
	    "    <indirect_samples value=\"0\"/>~n"
	    "    <indirect_power value=\"1.0\"/>~n"
	    "    <exposure value=\"~.10f\"/>~n"
	    "    <save_alpha value=\"~s\"/>~n"
	    "    <gamma value=\"~.10f\"/>~n"
	    "    <fog_density value=\"~.10f\"/>",
	    [CameraName,AA_passes,Raydepth,Bias,AA_threshold,
	     AA_minsamples,AA_pixelwidth,BackgroundName,Outfile,Exposure,
	     format(SaveAlpha),Gamma,FogDensity]),
    export_rgb(F, fog_color, FogColor),
    println(F, "</render>").



%%% Noisy file output functions. Fail if anything goes wrong.
%%%

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



%% Convert certain terms to printable strings in a
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
%%
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
%%
skip_walpha1([$ |T]) ->
    skip_walpha(T);
skip_walpha1([$\t|T]) ->
    skip_walpha(T);
skip_walpha1([$\n|T]) ->
    skip_walpha(T);
skip_walpha1(L) ->
    L.

%% Skip whitespace and '%d' .bat file arguments from end of line
%%
rskip_warg(L) ->
    rskip_warg1(lists:reverse(L)).
%%
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
%%
lowercase([]) ->
    [];
lowercase([C|T]) when C >= $A, C =< $Z ->
    [(C + $a - $A)|lowercase(T)];
lowercase([C|T]) ->
    [C|lowercase(T)].

%% Convert lonely CR to NL, and CRLF to NL
%%
cr_to_nl([$\r,$\n|T]) ->
    [$\n|cr_to_nl(T)];
cr_to_nl([$\r|T]) ->
    [$\n|cr_to_nl(T)];
cr_to_nl([C|T]) ->
    [C|cr_to_nl(T)];
cr_to_nl([]) ->
    [].

%% Split a list into a list of length Pos, and the tail
%%
split_list(List, Pos) when list(List), integer(Pos), Pos >= 0 ->
    case split_list1(List, Pos, []) of
	{_,_}=Result ->
	    Result;
	Error ->
	    erlang:fault(Error, [List, Pos])
    end.
%%
split_list1(List, 0, Head) ->
    {lists:reverse(Head),List};
split_list1([], _Pos, _) ->
    badarg;
split_list1([H|T], Pos, Head) ->
    split_list1(T, Pos-1, [H|Head]).

%%% %% {lists:filter(Pred, List),lists:filter(fun(X) -> not Pred(X) end, List)}
%%% filter2(Pred, List) -> filter2_1(Pred, List, [], []).
%%% %%
%%% filter2_1(_Pred, [], True, False) ->
%%%     {reverse(True),reverse(False)};
%%% filter2_1(Pred, [H|T], True, False) ->
%%%     case Pred(H) of
%%% 	true -> filter2_1(Pred, T, [H|True], False);
%%% 	false -> filter2_1(Pred, T, True, [H|False])
%%%     end.

now_diff({A1,B1,C1}, {A2,B2,C2}) ->
    (A1-A2)*1000000.0 + float(B1-B2) + (C1-C2)*0.000001.

%% Create a string unique for the OS process. It consists 
%% of the OS process id, a dash, and seconds since Jan 1 1970
%% encoded in approx 8 characters. It should be unique even in 
%% the event of an OS restart.
uniqstr() ->
    {Ms,S,_} = now(),
    os:getpid()++"-"++uniqstr(Ms*1000000 + S).
%%
-define(UNIQBASE, (10+$Z-$A+1)).
uniqstr(0) -> [];
uniqstr(N) ->
    case N rem ?UNIQBASE of
	M when M < 10 ->
	    [$0+M|uniqstr(N div ?UNIQBASE)];
	M ->
	    [$A+M-10|uniqstr(N div ?UNIQBASE)]
    end.

-ifdef(print_mesh_1).
print_mesh(#e3d_mesh{type=T,vs=Vs,vc=Vc,tx=Tx,ns=Ns,fs=Fs,he=He,matrix=M}) ->
    io:format("#e3d_mesh{type=~p,~nvs=~p,~nvc=~p,~ntx=~p,~nns=~p,~nfs=~p,~n"
	      "he=~p,~nmatrix=~p}.~n",
	      [T,Vs,Vc,Tx,Ns,Fs,He,M]).
-endif.

help_button(Subject) ->
    Title = help(title, Subject),
    TextFun = fun () -> help(text, Subject) end,
    {help,Title,TextFun}.

help(title, material_dialog) ->
    "YafRay Material Properties";
help(text, material_dialog) ->
    [<<"Each Material creates a YafRay shader. "
      "The OpenGL properties that map to YafRay shader parameters are:">>,
     <<"Diffuse * Opacity -> 'color'.">>,
     <<"Specular -> 'specular'.">>,
     <<"Shininess * 128-> 'hard'.">>];
help(title, {material_dialog,object}) ->
    "YafRay Material Properties: Object Parameters";
help(text, {material_dialog,object}) ->
    [<<"Object Parameters are applied to whole objects, namely those "
      "that have this material on a majority of their faces.">>,
     <<"Mapping to YafRay object parameters:">>,
     <<"Cast Shadow -> 'shadow'.">>,
     <<"Emit Rad -> 'emit_rad'.">>,
     <<"Recv Rad -> 'recv_rad'.">>,
     <<"Use Edge Hardness -> Emulate hard edges by "
      "slitting the object mesh along hard edges.">>,
     <<"Caustic -> Make the object caustic, i.e refract and "
      "reflect photons but not get hit by them. This is done by "
      "setting options 'caus_IOR', 'caus_rcolor' and 'caus_tcolor' "
      "from the corresponding Fresnel Parameters.">>,
     <<"Autosmooth Angle -> 'autosmooth'.">>];
help(title, {material_dialog,fresnel}) ->
    "YafRay Material Properties: Fresnel Parameters";
help(text, {material_dialog,fresnel}) ->
    [<<"Fresnel Parameters affect how rays reflect off and refract in "
      "glass-like materials. This is a different light model than the "
      "OpenGL (Diffuse,Specular,Shininess) model and they do not often "
      "go well together.">>,
     <<"Mapping to YafRay shader parameters:">>,
     <<"Index Of Refraction -> 'ior'.">>,
     <<"Total Internal Reflection -> 'tir'.">>,
     <<"Minimum Reflection -> 'min_refle'.">>,
     <<"Reflected -> 'reflected'.">>,
     <<"Transmitted -> 'transmitted'.">>,
     <<"Set Default -> Sets 'transmitted' to Diffuse * (1 - Opacity). "
      "This makes a semi-transparent object in OpenGL look the same in "
      "YafRay provided that Index Of Refraction is 1.0.">>,
     <<"Grazing Angle Colors -> Use the secondary Reflected and Transmitted "
      "colors following that show from grazing angles of the material. "
      "For a glass with green edges set Transmitted to white and "
      "Grazing Angle Transmitted to green.">>];
%%
help(title, light_dialog) ->
    "YafRay Light Properties";
help(text, light_dialog) ->
    [<<"OpenGL properties that map to YafRay light parameters are:">>,
     <<"Diffuse -> 'color'">>,
     <<"All other OpenGl properties are ignored, particulary the "
      "Attenuation properties">>,
     <<"YafRay parameters mapping is pretty straightforward - "
      "the dialog field names should be self-explanatory">>].
