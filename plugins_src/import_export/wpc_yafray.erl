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
%%     $Id: wpc_yafray.erl,v 1.91 2004/06/22 10:21:33 raimo_niskanen Exp $
%%

-module(wpc_yafray).

-export([init/0,menu/2,dialog/2,command/2]).

%% Debug export
-export([now_diff_1/1]).


-include_lib("kernel/include/file.hrl").

-include("e3d.hrl").
-include("e3d_image.hrl").
-include("wings.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,keydelete/3,
		foreach/2,foldl/3,foldr/3]).

-define(TAG, yafray).
-define(KEY(K), {?TAG,(K)}).
-define(TAG_RENDER, yafray_render).

key(Key) -> {key,?KEY(Key)}.

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
-define(DEF_HALO, false).
-define(DEF_HALO_RES, 512).
-define(DEF_HALO_BLUR, 0.0).
-define(DEF_HALO_SHADOW_BLUR, 0.0).
-define(DEF_HALO_FOG_DENSITY, 0.0).
-define(DEF_HALO_FOG_COLOR, {0.0,0.0,0.0}).

%% Photonlight
-define(DEF_MODE,diffuse).
-define(DEF_PHOTONS,5000).
-define(DEF_SEARCH,50).
-define(DEF_DEPTH,3).
-define(DEF_CAUS_DEPTH,4).
-define(DEF_DIRECT,false).
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
-define(DEF_SUNSKY_VAR, 1.0).
%% Hemilight and Pathlight
-define(DEF_AMBIENT_TYPE, hemilight).
-define(DEF_BACKGROUND_FILENAME, "").
-define(DEF_BACKGROUND_EXPOSURE_ADJUST, 0).
-define(DEF_BACKGROUND_MAPPING, probe).
-define(DEF_BACKGROUND_POWER, 1.0).
-define(DEF_SAMPLES, 256).
%% Pathlight
-define(DEF_CACHE, false).
-define(DEF_CACHE_SIZE, 0.01).
-define(DEF_ANGLE_THRESHOLD, 0.2).
-define(DEF_SHADOW_THRESHOLD, 0.3).
-define(DEF_GRADIENT, false).
-define(DEF_SHOW_SAMPLES, false).
%% Global Photonlight
-define(DEF_GLOBALPHOTONLIGHT_PHOTONS, 50000).
-define(DEF_GLOBALPHOTONLIGHT_RADIUS, 1.0).
-define(DEF_GLOBALPHOTONLIGHT_DEPTH, 2).
-define(DEF_GLOBALPHOTONLIGHT_SEARCH, 200).

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

range(T) -> {range,range_1(T)}.

%% Material ranges
range_1(autosmooth_angle)	-> {0.0,180.0};
range_1(ior)			-> {0.0,infinity};
range_1(min_refle)		-> {0.0,1.0};
range_1(size)			-> {0.0,infinity};
range_1(modulation)		-> {0.0,1.0};
range_1(turbulence)		-> {1.0e-6,infinity};
range_1(scale)			-> {1.0e-6,infinity};
range_1(sharpness)		-> {1.0,infinity};
range_1(noise_depth)		-> {1,infinity};
%% Light ranges
range_1(power)			-> {0.0,infinity};
range_1(bias)			-> {0.0,1.0};
range_1(res)			-> {0,infinity};
range_1(radius)			-> {0,infinity};
range_1(blur)			-> {0.0,1.0};
range_1(samples)		-> {1,infinity};
range_1(halo_fog_density)	-> {0.0,infinity};
range_1(blend)			-> {0.0,infinity};
range_1(photons)		-> {0,infinity};
range_1(depth)			-> {0,infinity};
range_1(fixedradius)		-> {1.0,infinity};
range_1(search)			-> {0,infinity};
range_1(cluster)		-> {0.0,infinity};
range_1(turbidity)		-> {0.0,infinity};
range_1(angle_threshold)	-> {0.0,1.0};
range_1(raydepth)		-> {1,infinity};
range_1(cache_size)		-> {0.0,infinity};
range_1(shadow_threshold)	-> {0.0,infinity};
range_1(cache_search)		-> {3,infinity};
range_1(exposure_adjust)	-> {-128,127};
range_1(psamples)		-> {0,infinity};
%% Render ranges
range_1(subdivisions)		-> {0,infinity};
range_1(aa_pixelwidth)		-> {1.0,2.0};
range_1(aa_passes)		-> {0,infinity};
range_1(aa_threshold)		-> {0.0,1.0};
range_1(aa_minsamples)		-> {1,infinity};
range_1(gamma)			-> {0.0,infinity};
range_1(exposure)		-> {0.0,infinity};
range_1(pixels)			-> {1,infinity};
range_1(fog_density)		-> {0.0,infinity};
range_1(antinoise_radius)	-> {0.0,infinity};
range_1(antinoise_max_delta)	-> {0.0,infinity};
range_1(dof_blur)		-> {0.0,infinity}.



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
	    do_export(export, props(render), [{?TAG_RENDER,true}|Attr], St);
       _RenderFile ->
	    wpa:error("Already rendering.")
    end;
command_file(render, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "YafRay Render Options", export_dialog(render),
	       fun(Attr) -> {file,{render,{?TAG,Attr}}} end);
command_file(?TAG_RENDER, Result, _St) ->
    Rendering = set_var(rendering, false),
    case Rendering of
	false ->
	    keep;
	RenderFile ->
	    case Result of
		load_image ->
		    io:format("Loading rendered image~n~n"),
		    load_image(RenderFile);
		ok ->
		    io:format("Rendering Job ready~n~n"),
		    keep;
		{error,Error} ->
		    io:format("Rendering error: ~p~n~n", [Error]),
		    wpa:error("Rendering error")
	    end
    end;
command_file(Op, Attr, St) when is_list(Attr) ->
    %% when Op =:= export; Op =:= export_selected
    set_pref(Attr),
    do_export(Op, props(Op), Attr, St);
command_file(Op, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "YafRay Export Options", export_dialog(Op),
	       fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).

-record(camera_info, {pos,dir,up,fov}).

do_export(Op, Props0, Attr0, St0) ->
    SubDiv = proplists:get_value(subdivisions, Attr0, ?DEF_SUBDIVISIONS),
    Props = [{subdivisions,SubDiv}|Props0],
    [{Pos,Dir,Up},Fov] = wpa:camera_info([pos_dir_up,fov]),
    CameraInfo = #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov},
    Attr = [CameraInfo,{lights,wpa:lights(St0)}|Attr0],
    ExportFun = 
	fun (Filename, Contents) ->
		case catch export(Attr, Filename, Contents) of
		    ok ->
			ok;
		    Error ->
			io:format("ERROR: Failed to export:~n~p~n", [Error]),
			{error,"Failed to export"}
		end
	end,
    %% Freeze virtual mirrors.
    Shapes0 = gb_trees:to_list(St0#st.shapes),
    Shapes = [{Id,wpa:vm_freeze(We)} || {Id,We} <- Shapes0],
    St = St0#st{shapes=gb_trees:from_orddict(Shapes)},
    wpa:Op(Props, ExportFun, St).

props(render) ->
    [{title,"Render"},{ext,".tga"},{ext_desc,"Targa File"}];
props(export) ->
    [{title,"Export"},{ext,".xml"},{ext_desc,"YafRay File"}];
props(export_selected) ->
    [{title,"Export Selected"},{ext,".xml"},{ext_desc,"YafRay File"}].

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
	 [{hframe,[{"Cast Shadow",Shadow,[key(shadow)]},
		   {"Emit Rad",EmitRad,[key(emit_rad)]},
		   {"Recv Rad",RecvRad,[key(recv_rad)]},
		   panel,
		   help_button({material_dialog,object})]},
	  {hframe,[{"Use Edge Hardness",UseHardness,
		    [key(use_hardness)]},
		   {"Caustic",Caus,[key(caus)]}]},
	  {hframe,[{"Autosmooth",Autosmooth,[key(autosmooth)]},
		   {label,"Angle"},
		   {slider,{text,AutosmoothAngle,
			    [range(autosmooth_angle),{width,5},
			     key(autosmooth_angle),
			     hook(enable, ?KEY(autosmooth))]}}]}],
	 [{title,"Object Parameters"},{minimized,ObjectMinimized},
	  key(object_minimized)]},
    FresnelVframe =
	{vframe,
	 [{hframe,[{label,"Index Of Refraction"},
		   {text,IOR,[range(ior),key(ior)]},
		   panel,
		   help_button({material_dialog,fresnel})]},
	  {hframe,[{"Fast Fresnel",FastFresnel,[key(fast_fresnel)]},
		   {"Total Internal Reflection",TIR,[key(tir)]}]},
	  {hframe,[{label,"Minimum Reflection"},
		   {slider,{text,MinRefle,[range(min_refle),{width,5},
					   key(min_refle)]}}]},
	  {hframe,[{vframe,[{label,"Reflected"},
			    {label,"Transmitted"}]},
		   {vframe,[{slider,{color,Reflected,
				     [key(reflected)]}},
			    {slider,{color,Transmitted,
				     [key(transmitted)]}}]},
		   {vframe,[panel,
			    {button,"Set Default",keep,
			     [transmitted_hook(?KEY(transmitted))]}]}]},
	  {"Grazing Angle Colors",Fresnel2,[key(fresnel2),
					    layout]},
	  {hframe,[{vframe,[{label,"Reflected"},
			    {label,"Transmitted"}]},
		   {vframe,[{slider,{color,Reflected2,
				     [key(reflected2)]}},
			    {slider,{color,Transmitted2,
				     [key(transmitted2)]}}]},
		   {vframe,[panel,
			    {button,"Set Default",keep,
			     [transmitted_hook(?KEY(transmitted2))]}]}],
	   [hook(open, ?KEY(fresnel2))]}],
	 [{title,"Fresnel Parameters"},{minimized,FresnelMinimized},
	  key(fresnel_minimized)]},
    %%
    [{vframe,
      [ObjectVframe,
       FresnelVframe
       |modulator_dialogs(Modulators, Maps)],
      [{title,"YafRay Options"},{minimized,Minimized},key(minimized)]}].

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

material_result(_Name, Mat0, [{?KEY(minimized),_}|_]=Res0) ->
    {Ps1,Res1} = split_list(Res0, 19),
    Ps2 = [{Key,Val} || {?KEY(Key),Val} <- Ps1],
    {Ps,Res} = modulator_result(Ps2, Res1),
    Mat = [?KEY(Ps)|keydelete(?TAG, 1, Mat0)],
    {Mat,Res};
material_result(Name, Mat, Res) ->
    exit({invalid_tag,{?MODULE,?LINE,[Name,Mat,Res]}}).

modulator_dialogs(Modulators, Maps) ->
    modulator_dialogs(Modulators, Maps, 1).

modulator_dialogs([], _Maps, M) ->
    [{hframe,
      [{button,"New Modulator",done,[key(new_modulator)]},
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
		  Type,[{key,TypeTag},layout]}],
    [{vframe,
      [{hframe,[{"Enabled",Enabled,[{key,{?TAG,enabled,M}}]},
		{menu,[{"Mix",mix},{"Mul",mul},{"Add",add}],Mode,
		 [hook(enable, {?TAG,enabled,M})]},
		{button,"Delete",done}]},
       {vframe, % hook(enable, {?TAG,enabled,M})
	[{hframe,[{label,"SizeX"},{text,SizeX,[range(size)]},
		  {label,"SizeY"},{text,SizeY,[range(size)]},
		  {label,"SizeZ"},{text,SizeZ,[range(size)]}]},
	 {hframe,[{vframe,[{label,"Diffuse "},
			   {label,"Specular"},
			   {label,"Ambient"},
			   {label,"Shininess"},
			   {label,"Normal"}]},
		  {vframe,[{slider,{text,Diffuse,[range(modulation)]}},
			   {slider,{text,Specular,[range(modulation)]}},
			   {slider,{text,Ambient,[range(modulation)]}},
			   {slider,{text,Shininess,[range(modulation)]}},
			   {slider,{text,Normal,[range(modulation)]}}]}]}]
	++MapsFrame++
	[{hradio,[{"Image",image},{"Clouds",clouds},
		  {"Marble",marble},{"Wood",wood}],
	  Type,[{key,TypeTag},layout]},
	 {vframe,
	  [{hframe,
	    [{hframe,
	      [{label,"Filename"},
	       {button,{text,Filename,[{props,BrowseProps}]}}],
	      [hook(open, [member,{?TAG,type,M},image])]},
	     {hframe,
	      [{label,"Color 1"},{color,Color1},
	       {label,"Color 2"},{color,Color2},
	       {label,"Depth"},{text,Depth,[range(noise_depth)]},
	       {"Hard Noise",Hard,
		[hook(open, [member,{?TAG,type,M},marble,wood])]}],
	      [hook(open, ['not',[member,{?TAG,type,M},image]])]}]},
	   {hframe,
	    [{label,"Turbulence"},{text,Turbulence,[range(turbulence)]},
	     {hframe,
	      [{label,"Sharpness"},{text,Sharpness,[range(sharpness)]}],
	      [hook(open, [member,{?TAG,type,M},marble])]}],
	    [hook(open, [member,{?TAG,type,M},marble,wood])]},
	   {hframe,
	    [{label,"Ringscale X"},{text,RingscaleX,[range(scale)]},
	     {label,"Ringscale Z"},{text,RingscaleZ,[range(scale)]}],
	    [hook(open, [member,{?TAG,type,M},wood])]}]}],
	[hook(enable, {?TAG,enabled,M})]}],
      [{title,
	"Modulator "++integer_to_list(M)++mod_legend(Enabled, Mode, Type)},
       {minimized,Minimized}]}];
modulator_dialog(_Modulator, _Maps, _) ->
    []. % Discard old modulators that anyone may have

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
modulator_result(Ps, [{?KEY(new_modulator),false},false|Res], 1, []) ->
    {[{modulators,[]}|Ps],Res};
modulator_result(Ps, [{?KEY(new_modulator),false},true|Res], 1, []) ->
    %% Default Modulators
    {Ps,Res};
modulator_result(Ps, [{?KEY(new_modulator),true},_|Res], 1, []) ->
    {[{modulators,[{modulator,[]}]}|Ps],Res};
modulator_result(Ps, [{?KEY(new_modulator),false}|Res], _, Modulators) ->
    {[{modulators,reverse(Modulators)}|Ps],Res};
modulator_result(Ps, [{?KEY(new_modulator),true}|Res], _, Modulators) ->
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
     Color1,Color2,Depth,Hard,
     Turbulence,Sharpness,
     RingscaleX,RingscaleZ] %% 17 values = 18-1
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
      [{hframe,[{vframe, [{label,"Power"}]},
		{vframe,[{text,Power,[range(power),key(power)]}]},
		panel,
		help_button(light_dialog)]}|
       light_dialog(Name, Type, YafRay)],
      [{title,"YafRay Options"},key(minimized),{minimized,Minimized}]}].

light_dialog(_Name, point, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_POINT_TYPE),
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Bias = proplists:get_value(bias, Ps, ?DEF_BIAS),
    Res = proplists:get_value(res, Ps, ?DEF_RES),
    Radius = proplists:get_value(radius, Ps, ?DEF_RADIUS),
    [{hframe,
      [{vradio,[{"Pointlight",pointlight},{"Softlight",softlight}],
	Type,[key(type),layout]},
       {"Cast Shadows",CastShadows,
	[key(cast_shadows),
	 hook(open, [member,?KEY(type),pointlight])]},
       {hframe,
	[{vframe,[{label,"Bias"},
		  {label,"Res"},
		  {label,"Radius"}]},
	 {vframe,[{text,Bias,[range(bias),key(bias)]},
		  {text,Res,[range(res),key(res)]},
		  {text,Radius,[range(radius),key(radius)]}]}],
	[hook(open, [member,?KEY(type),softlight])]}]}];
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
    MinimizedHalo = proplists:get_value(minimized_halo, Ps, true),
    Halo = proplists:get_value(halo, Ps, ?DEF_HALO),
    HaloRes = proplists:get_value(halo_res, Ps, ?DEF_HALO_RES),
    HaloSamples = proplists:get_value(halo_samples, Ps, HaloRes),
    HaloShadowSamples = 
	proplists:get_value(halo_shadow_samples, Ps, HaloRes),
    HaloBlur = proplists:get_value(halo_blur, Ps, ?DEF_HALO_BLUR),
    HaloShadowBlur = 
	proplists:get_value(halo_shadow_blur, Ps, ?DEF_HALO_SHADOW_BLUR),
    HaloFogDensity = 
	proplists:get_value(halo_fog_density, Ps, ?DEF_HALO_FOG_DENSITY),
    HaloFogColor = 
	proplists:get_value(halo_fog_color, Ps, ?DEF_HALO_FOG_COLOR),
    BlurRange = range(blur),
    HaloFrame =
	{hframe,
	 [{vframe,[{label,"Res"},
		   {label,"Samples"},
		   {label,"Shadow Samples"},
		   {label,"Blur"},
		   {label,"Shadow Blur"},
		   {label,"Fog Density"},
		   {label,"Fog Color"}]},
	  {vframe,[{text,HaloRes,
		    [range(res),key(halo_res),
		     {hook,
		      fun (update, {Key,_I,Val,Sto0}) ->
			      Sto1 = gb_trees:update
				       (?KEY(halo_samples), Val, Sto0),
			      Sto = gb_trees:update
				      (?KEY(halo_shadow_samples), Val, Sto1),
			      {store,gb_trees:update(Key, Val, Sto)};
			  (_, _) -> void
		      end}]},
		   {text,HaloSamples,
		    [range(samples),key(halo_samples)]},
		   {text,HaloShadowSamples,
		    [range(samples),key(halo_shadow_samples)]},
		   {text,HaloBlur,[BlurRange,key(halo_blur)]},
		   {text,HaloShadowBlur,
		    [BlurRange,key(halo_shadow_blur)]},
		   {text,HaloFogDensity,
		    [range(halo_fog_density),key(halo_fog_density)]},
		   {color,HaloFogColor,[key(halo_fog_color)]}]},
	  {vframe,[panel,
		   panel,
		   panel,
		   {slider,[BlurRange,key(halo_blur)]},
		   {slider,[BlurRange,key(halo_shadow_blur)]},
		   panel]}],
	 [{title,"Halo"},
	  key(minimized_halo),{minimized,MinimizedHalo},
	  hook(enable, ?KEY(halo))]},
    %%
    [{hframe,
      [{hradio,[{"Spotlight",spotlight},
		{"Photonlight",photonlight}],Type,[layout,key(type)]},
       {menu,[{"Diffuse",diffuse},{"Caustic",caustic}],Mode,
	[key(mode),hook(open, [member,?KEY(type),photonlight])]},
       {"Use QMC",UseQMC,[key(use_QMC),
			  hook(open, [member,?KEY(type), photonlight])]}]},
     {vframe,
      [{hframe,[{"Cast Shadows",CastShadows,[key(cast_shadows)]},
		{label,"Blend"},
		{text,Blend,[range(blend),key(blend)]}]},
       {hframe,[{"",Halo,[key(halo)]},
		HaloFrame]}],
      [hook(open, [member,?KEY(type), spotlight])]},
     {hframe,[{vframe,[{label,"Photons"},
		       {label,"Depth"},
		       {label,"Fixedradius"}]},
	      {vframe,[{text,Photons,[range(photons),key(photons)]},
		       {text,Depth,[range(depth),key(depth)]},
		       {text,Fixedradius,[range(fixedradius),
					  key(fixedradius)]}]},
	      {vframe,[{label,"Search"},
		       {label,"Mindepth"},
		       {label,"Cluster"}]},
	      {vframe,[{text,Search,[range(search),key(search)]},
		       {text,Mindepth,[range(depth),key(mindepth)]},
		       {text,Cluster,[range(cluster),key(cluster)]}]}],
      [hook(open, [member,?KEY(type), photonlight])]}];
light_dialog(_Name, infinite, Ps) ->
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND),
    %%
    BgColor = proplists:get_value(background_color, Ps, ?DEF_BACKGROUND_COLOR),
    %%
    Turbidity = proplists:get_value(turbidity, Ps, ?DEF_TURBIDITY),
    A_var = proplists:get_value(a_var, Ps, ?DEF_SUNSKY_VAR),
    B_var = proplists:get_value(b_var, Ps, ?DEF_SUNSKY_VAR),
    C_var = proplists:get_value(c_var, Ps, ?DEF_SUNSKY_VAR),
    D_var = proplists:get_value(d_var, Ps, ?DEF_SUNSKY_VAR),
    E_var = proplists:get_value(e_var, Ps, ?DEF_SUNSKY_VAR),
    %%
    [{"Cast Shadows",CastShadows,[key(cast_shadows)]},
     {vframe,
      [{hradio,[{"Constant",constant},
		{"Sunsky",sunsky},
		{"None", undefined}],Bg,[layout,key(background)]},
       {hframe,[{label,"Color"},
		{color,BgColor,[key(background_color)]}],
	[hook(open, [member,?KEY(background),constant])]},
       {vframe,
	[{hframe,[]},
	 {hframe,
	  [{vframe,[{label,"Turbidity"},
		    {label,"a: Horizon Brightness"},
		    {label,"b: Horizon Spread"},
		    {label,"c: Sun Brightness"},
		    {label,"d: Sun Contraction"},
		    {label,"e: Sun Backscatter"}]},
	   {vframe,[{text,Turbidity,[range(turbidity),key(turbidity)]},
		    {text,A_var,[key(a_var)]},
		    {text,B_var,[key(b_var)]},
		    {text,C_var,[key(c_var)]},
		    {text,D_var,[key(d_var)]},
		    {text,E_var,[key(e_var)]}]}]}],
	[hook(open, [member,?KEY(background),sunsky])]}],
      [{title,"Background"}]}];
light_dialog(_Name, ambient, Ps) ->
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND),
    BgColor = proplists:get_value(background_color, Ps, ?DEF_BACKGROUND_COLOR),
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
    %%
    Type = proplists:get_value(type, Ps, ?DEF_AMBIENT_TYPE),
    Samples = proplists:get_value(samples, Ps, ?DEF_SAMPLES),
    Depth = proplists:get_value(depth, Ps, ?DEF_DEPTH),
    CausDepth = proplists:get_value(caus_depth, Ps, ?DEF_CAUS_DEPTH),
    Direct = proplists:get_value(direct, Ps, ?DEF_DIRECT),
    UseQMC = proplists:get_value(use_QMC, Ps, ?DEF_USE_QMC),
    %%
    CacheMinimized = proplists:get_value(cache_minimized, Ps, true),
    Cache = proplists:get_value(cache, Ps, ?DEF_CACHE),
    CacheSize = proplists:get_value(cache_size, Ps, ?DEF_CACHE_SIZE),
    AngleThreshold = proplists:get_value(angle_threshold, Ps, 
					 ?DEF_ANGLE_THRESHOLD),
    AngleKey = ?KEY(angle_threshold),
    AngleRange = range(angle_threshold),
    ShadowThreshold = proplists:get_value(shadow_threshold, Ps, 
					  ?DEF_SHADOW_THRESHOLD),
    Gradient = proplists:get_value(gradient, Ps, ?DEF_GRADIENT),
    ShowSamples = proplists:get_value(show_samples, Ps, ?DEF_SHOW_SAMPLES),
    Search = proplists:get_value(search, Ps, ?DEF_SEARCH),
    %%
    GplPhotons = proplists:get_value(globalphotonlight_photons, Ps,
				     ?DEF_GLOBALPHOTONLIGHT_PHOTONS),
    GplRadius = proplists:get_value(globalphotonlight_radius, Ps,
				    ?DEF_GLOBALPHOTONLIGHT_RADIUS),
    GplDepth = proplists:get_value(globalphotonlight_depth, Ps,
				   ?DEF_GLOBALPHOTONLIGHT_DEPTH),
    GplSearch = proplists:get_value(globalphotonlight_search, Ps,
				    ?DEF_GLOBALPHOTONLIGHT_SEARCH),
    [{hradio,[{"Hemilight",hemilight},
	      {"Pathlight",pathlight},
	      {"Global Photonlight",globalphotonlight}],
      Type,[layout,key(type)]},
     %% Hemilight and Pathlight
     {hframe,
      [{vframe,[{"Use QMC",UseQMC,[key(use_QMC)]},
		{"Direct",Direct,[key(direct),
				  hook(open, [member,?KEY(type),
					      pathlight])]}]},
       {vframe,[{label,"Samples"},
		{label,"Depth",[hook(open, [member,?KEY(type),pathlight])]}]},
       {vframe,[{text,Samples,[range(samples),key(samples)]},
		{text,Depth,[range(raydepth),key(depth),
			     hook(open, [member,?KEY(type),pathlight])]}]},
       {vframe,[panel,
		{label,"Caus Depth"}],
	[hook(open, [member,?KEY(type),pathlight])]},
       {vframe,[panel,
		{text,CausDepth,[range(raydepth),key(caus_depth)]}],
	[hook(open, [member,?KEY(type),pathlight])]}],
      [hook(open, [member,?KEY(type),hemilight,pathlight])]},
     %% Pathlight
     {vframe,
      [{hframe,
	[{"",Cache,[key(cache)]},
	 {hframe,
	  [{vframe,
	    [{label,"Size"},
	     {label,"Angle Threshold"},
	     panel,
	     {label,"Shadow Threshold"},
	     {"Gradient",Gradient,[key(gradient)]},
	     {label,"Search"}]},
	   {vframe,
	    [{text,CacheSize,[key(cache_size),range(cache_size)]},
	     {text,AngleThreshold,[{key,AngleKey},AngleRange]},
	     {slider,[{key,AngleKey},AngleRange]},
	     {text,ShadowThreshold,[key(shadow_threshold),
				    range(shadow_threshold)]},
	     {"Show Samples",ShowSamples,[key(show_samples)]},
	     {text,Search,[key(search),range(cache_search)]}]}],
	  [{title,"Irradiance Cache"},
	   {minimized,CacheMinimized},key(cache_minimized),
	   hook(enable, ?KEY(cache))]}],
	[hook(enable, ['not',?KEY(direct)])]}],
      [hook(open, [member,?KEY(type),pathlight])]},
     %% Global Photonlight
     {hframe,[{vframe,[{label,"Photons"},
		       {label,"Depth"}]},
	      {vframe,[{text,GplPhotons,
			[range(photons),key(globalphotonlight_photons)]},
		       {text,GplDepth,
			[range(raydepth),
			 key(globalphotonlight_depth)]}]},
	      {vframe,[{label,"Radius"},
		       {label,"Search"}]},
	      {vframe,[{text,GplRadius,
			[range(fixedradius),key(globalphotonlight_radius)]},
		       {text,GplSearch,
			[range(search),key(globalphotonlight_search)]}]}],
      [hook(open, [member,?KEY(type),globalphotonlight])]},
     %% Backgrounds
     {vframe,
      [{hradio,[{"HDRI",'HDRI'},
		{"Image",image},
		{"Constant",constant},
		{"None", undefined}],Bg,[layout,key(background)]},
       {hframe,[{label,"Filename"},
		{button,{text,BgFnameHDRI,
			 [key(background_filename_HDRI),
			  {props,BrowsePropsHDRI}]}}],
	[hook(open, [member,?KEY(background),'HDRI'])]},
       {hframe,[{label,"Filename"},
		{button,{text,BgFnameImage,
			 [key(background_filename_image),
			  {props,BrowsePropsImage}]}}],
	[hook(open, [member,?KEY(background),image])]},
       {hframe,[{label,"Exposure Adjust"},
		{text,BgExpAdj,[key(background_exposure_adjust),
				range(exposure_adjust)]},
		{menu,[{"Angular Map",probe},{"Spherical Map",spherical}],
		 BgMapping,[key(background_mapping)]}],
	[hook(open, [member,?KEY(background),'HDRI'])]},
       {hframe,[{label,"Power"},
		{text,BgPower,[key(background_power),range(power)]}],
	[hook(open, [member,?KEY(background),image])]},
       {hframe,[{label,"Color"},
		{color,BgColor,[key(background_color)]}],
	[hook(open, [member,?KEY(background),constant])]}],
      [{title,"Background"}]}];
light_dialog(_Name, area, Ps) ->
    ArealightSamples = proplists:get_value(arealight_samples, Ps, 
					   ?DEF_AREALIGHT_SAMPLES),
    ArealightPsamples = proplists:get_value(arealight_psamples, Ps, 
					    ?DEF_AREALIGHT_PSAMPLES),
    Dummy = proplists:get_value(dummy, Ps, ?DEF_DUMMY),
    [{"Global Photonlight Dummy",Dummy,[key(dummy)]},
     {hframe,[{label,"Samples"},
	      {text,ArealightSamples,[range(samples),key(arealight_samples)]},
	      {label,"Penumbra Samples"},
	      {text,ArealightPsamples,[range(psamples),
				       key(arealight_psamples)]}],
      [hook(enable, ['not',?KEY(dummy)])]}];
light_dialog(_Name, _Type, _Ps) ->
%%%    erlang:display({?MODULE,?LINE,{_Name,_Type,_Ps}}),
    [].

light_result(_Name, Ps0, 
	     [{?KEY(minimized),Minimized},{?KEY(power),Power}|Res0]) ->
    {LightPs0,Res1} = light_result(Res0),
    LightPs = [{Key,Val} || {?KEY(Key),Val} <- LightPs0],
    Ps = [{?TAG,[{minimized,Minimized},{power,Power}|LightPs]}
	  |keydelete(?TAG, 1, Ps0)],
%    erlang:display({?MODULE,?LINE,[Ps,Res1]}),
    {Ps,Res1}.

%% Point
light_result([{?KEY(type),pointlight}|_]=Ps) ->
    split_list(Ps, 5);
light_result([{?KEY(type),softlight}|_]=Ps) ->
    split_list(Ps, 5);
%% Spot
light_result([{?KEY(type),spotlight}|_]=Ps) ->
    split_list(Ps, 20);
light_result([{?KEY(type),photonlight}|_]=Ps) ->
    split_list(Ps, 20);
%% Infinite
light_result([_,{?KEY(background),_}|_]=Ps) ->
    split_list(Ps, 9);
%% Area
light_result([_,{?KEY(arealight_samples),_}|_]=Ps) ->
    split_list(Ps, 3);
%% Ambient
light_result([{?KEY(type),hemilight}|_]=Ps) ->
    split_list(Ps, 25);
light_result([{?KEY(type),pathlight}|_]=Ps) ->
    split_list(Ps, 25);
light_result([{?KEY(type),globalphotonlight}|_]=Ps) ->
    split_list(Ps, 25);
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
    AA_thresholdFlags = [range(aa_threshold),{key,aa_threshold}],
    AA_pixelwidthFlags = [range(aa_pixelwidth),{key,aa_pixelwidth}],
    BiasFlags = [range(bias),{key,bias}],
    [{hframe,[{label,"Sub-division Steps"},
	      {text,SubDiv,[{key,subdivisions},range(subdivisions)]}],
      [{title,"Pre-rendering"}]},
     {hframe,
      [{vframe,[{label,"AA_passes"},
		{label,"AA_minsamples"},
		{label,"Raydepth"},
		{label,"Gamma"}]},
       {vframe,[{text,AA_passes,[range(aa_passes),{key,aa_passes}]},
		{text,AA_minsamples,[range(aa_minsamples),
				     {key,aa_minsamples}]},
		{text,Raydepth,[range(raydepth),{key,raydepth}]},
		{text,Gamma,[range(gamma),{key,gamma}]}]},
       {vframe,[{label,"AA_threshold"},
		{label,"AA_pixelwidth"},
		{label,"Bias"},
		{label,"Exposure"}]},
       {vframe,[{text,AA_threshold,AA_thresholdFlags},
		{text,AA_pixelwidth,AA_pixelwidthFlags},
		{text,Bias,BiasFlags},
		{text,Exposure,[range(exposure),{key,exposure}]}]},
       {vframe,[{slider,AA_thresholdFlags},
		{slider,AA_pixelwidthFlags},
		{slider,BiasFlags},
		{"Alpha Channel",SaveAlpha,[{key,save_alpha}]}]}],
      [{title,"Render"}]},
     {hframe,
      [{vframe,[{label,"Default Color"}]},
       {vframe,[{color,BgColor,[{key,background_color}]}]}],
      [{title,"Background"}]},
     {hframe,
      [{vframe,[{label,"Width"}]},
       {vframe,[{text,Width,[range(pixels),{key,width}]}]},
       {vframe,[{label,"Height"}]},
       {vframe,[{text,Height,[range(pixels),{key,height}]}]}],
      [{title,"Camera"}]},
     {hframe,
      [{vframe,[panel,
		{"Antinoise",AntinoiseFilter,[{key,antinoise_filter}]},
		{"DOF",DofFilter,[{key,dof_filter}]}]},
       {vframe,
	[{hframe,
	  [{vframe,[{label,"Fog Density"},
		    {label,"Radius"},
		    {label,"Near Blur"},
		    {label,"Scale"}]},
	   {vframe,[{text,FogDensity,[range(fog_density),{key,fog_density}]},
		    {text,AntinoiseRadius,[range(antinoise_radius),
					   {key,antinoise_radius},
					   hook(enable, antinoise_filter)]},
		    {text,NearBlur,[range(dof_blur),{key,near_blur},
				    hook(enable, dof_filter)]},
		    {text,DofScale,[range(scale),{key,dof_scale},
				    hook(enable, dof_filter)]}]}]}]},
       {vframe,[{label,"Fog Color"},
		{label,"Max Delta"},
		{label,"Far Blur"}]},
       {vframe,[{color,FogColor,[{key,fog_color}]},
		{text,AntinoiseMaxDelta,[range(antinoise_max_delta),
					 {key,antinoise_max_delta},
					 hook(enable, antinoise_filter)]},
		{text,FarBlur,[range(dof_blur),{key,far_blur},
			       hook(enable, dof_filter)]}]}],
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



%% General purpose hook handling is_minimized and is_disabled.
%% Does lookup in Store for combinations of values.
%%
hook(Props) when is_list(Props) ->
    {hook,
     fun (is_minimized, {_Var,I,Store}) ->
	     case proplists:lookup(open, Props) of
		 {_,Expr} ->
		     not hook_eval(Expr, I, Store);
		 none -> void
	     end;
	 (is_disabled, {_Var,I,Store}) ->
	     case proplists:lookup(enable, Props) of
		 {_,Expr} ->
		     not hook_eval(Expr, I, Store);
		 none -> void
	     end;
	 (_, _) -> void
     end};
hook(Prop) -> hook([Prop]).

hook(Op, Expr) -> hook([{Op,Expr}]).
    
hook_eval(['not',Expr], I, Store) ->
    not hook_eval(Expr, I, Store);
hook_eval(['and'|Exprs], I, Store) ->
    hook_and(Exprs, I, Store);
hook_eval(['or'|Exprs], I, Store) ->
    hook_or(Exprs, I, Store);
hook_eval([member,Expr|Keys], I, Store) ->
    lists:member(hook_eval(Expr, I, Store), Keys);
hook_eval([key,Key], I, Store) ->
    hook_key(Key, I, Store);
hook_eval(Key, I, Store) when not is_list(Key) ->
    hook_key(Key, I, Store).

hook_key(Key, I, Store) when is_integer(Key) ->
    gb_trees:get(I+Key, Store);
hook_key(Key, _I, Store) ->
    gb_trees:get(Key, Store).

hook_and([Expr], I, Store) -> 
    hook_eval(Expr, I, Store);
hook_and([Expr|Exprs], I, Store) ->
    hook_eval(Expr, I, Store) andalso hook_and(Exprs, I, Store).

hook_or([Expr], I, Store) -> 
    hook_eval(Expr, I, Store);
hook_or([Expr|Exprs], I, Store) ->
    hook_eval(Expr, I, Store) orelse hook_or(Exprs, I, Store).



%%% Export and rendering functions
%%%

export(Attr, Filename, #e3d_file{objs=Objs,mat=Mats,creator=Creator}) ->
    wpa:popup_console(),
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
	    io:format("Export time:     ~s~n", 
		      [now_diff(RenderTS, ExportTS)]),
	    ok;
	{false,_} ->
	    %% Should not happen since the file->render dialog
	    %% must have been disabled
	    file:delete(ExportFile),
	    no_renderer;
	_ ->
	    spawn_link(
	      fun () -> 
		      set_var(export_ts, ExportTS),
		      set_var(render_ts, RenderTS),
		      file:delete(RenderFile),
		      render(Renderer, Options, ExportFile, LoadImage) 
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



render(Renderer, Options, Filename, LoadImage) ->
    process_flag(trap_exit, true),
    Dirname = filename:dirname(Filename),
    Basename = filename:basename(Filename),
    %% Filename is auto-generated so Basename should not need quoting
    Cmd = uquote(Renderer)++" "++Options++" "++Basename,
    PortOpts = [{cd,Dirname},eof,exit_status,stderr_to_stdout],
%%%     PortOpts = [{line,1},{cd,Dirname},eof,exit_status,stderr_to_stdout],
    io:format("Rendering Job started ~p:~n>~s~n", [self(),Cmd]),
    case catch open_port({spawn,Cmd}, PortOpts) of
	Port when port(Port) ->
	    Result = render_job(Port),
	    render_done(Filename, Result, LoadImage);
	{'EXIT',Reason} ->
	    render_done(Filename, {error,Reason}, LoadImage)
    end.

render_done(Filename, ExitStatus, LoadImage) ->
    io:format("~nRendering Job returned: ~p~n", [ExitStatus]),
    ExportTS = get_var(export_ts),
    RenderTS = get_var(render_ts),
    FinishTS = erlang:now(),
    io:format("Export time:     ~s~n"++
	      "Render time:     ~s~n"++
	      "Total time:      ~s~n",
	      [now_diff(RenderTS, ExportTS),
	       now_diff(FinishTS, RenderTS),
	       now_diff(FinishTS, ExportTS)]),
    file:delete(Filename),
    Status =
	case ExitStatus of
	    0         -> ok;
	    undefined -> ok;
	    {error,_} -> ExitStatus;
	    _         -> {error,ExitStatus}
	end,
    Result =
	case {Status,LoadImage} of
	    {ok,true}  -> load_image;
	    {ok,false} -> ok;
	    _          -> Status
	end,
    wpa:send_command({file,{?TAG_RENDER,Result}}),
    ok.

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
	    io:put_chars(Data),
	    case Tag of	eol -> io:nl(); noeol -> ok end,
	    render_job(Port);
	{Port,{data,Data}} ->
	    io:put_chars(Data),
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
%%%     case Bg of 
%%% 	sunsky ->
%%% 	    Bg;
%%% 	_ ->
    begin
	    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
	    if Power > 0.0 ->
		    CastShadows = proplists:get_value(cast_shadows, YafRay, 
						      ?DEF_CAST_SHADOWS),
		    Position = proplists:get_value(position, OpenGL, 
						   {0.0,0.0,0.0}),
		    Diffuse = proplists:get_value(diffuse, OpenGL, 
						  {1.0,1.0,1.0,1.0}),
		    println(F,"<light type=\"sunlight\" name=\"~s\" "++
			    "power=\"~.3f\" cast_shadows=\"~s\">", 
			    [Name, Power,format(CastShadows)]),
		    export_pos(F, from, Position),
		    export_rgb(F, color, Diffuse),
		    println(F, "</light>");
	       true -> ok
	    end,
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
	    print(F, "       cast_shadows=\"~s\" size=\"~.3f\"~n"++
		  "       beam_falloff=\"~.10f\" blend=\"~.3f\"", 
		  [format(CastShadows), ConeAngle, SpotExponent, Blend]),
	    case proplists:get_value(halo, YafRay, ?DEF_HALO) of
		false -> 
		    println(F, ">");
		true ->
		    HaloRes = 
			proplists:get_value(halo_res, YafRay, ?DEF_HALO_RES),
		    HaloSamples = 
			proplists:get_value(halo_samples, YafRay, HaloRes),
		    HaloShadowSamples = 
			proplists:get_value(halo_shadow_samples, YafRay, 
					    HaloRes),
		    HaloBlur = 
			proplists:get_value(halo_blur, YafRay, ?DEF_HALO_BLUR),
		    HaloShadowBlur = 
			proplists:get_value(halo_shadow_blur, YafRay, 
					    ?DEF_HALO_SHADOW_BLUR),
		    HaloFogDensity = 
			proplists:get_value(halo_fog_density, YafRay, 
					    ?DEF_HALO_FOG_DENSITY),
		    HaloFogColor = 
			proplists:get_value(halo_fog_color, YafRay, 
					    ?DEF_HALO_FOG_COLOR),
		    println(F, "~n       halo=\"~s\" res=\"~w\" "
			    "samples=\"~w\" shadow_samples=\"~w\"~n"
			    "       halo_blur=\"~.10f\" "
			    "shadow_blur=\"~.10f\"~n"
			    "       fog_density=\"~.10f\">",
			  [format(true),HaloRes,HaloSamples,HaloShadowSamples,
			   HaloBlur,HaloShadowBlur,HaloFogDensity]),
		    export_rgb(F, fog, HaloFogColor)
	    end;
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
    Type = proplists:get_value(type, YafRay, ?DEF_AMBIENT_TYPE),
    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
    case Type of
	hemilight when Power > 0.0 ->
	    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\"", 
		    [Type,Name,Power]),
	    UseQMC = proplists:get_value(use_QMC, YafRay, 
					 ?DEF_USE_QMC),
	    Ambient = proplists:get_value(ambient, OpenGL, 
					  ?DEF_BACKGROUND_COLOR),
	    Samples = proplists:get_value(samples, YafRay, 
					  ?DEF_SAMPLES),
	    println(F,"       use_QMC=\"~s\" samples=\"~w\">", 
		    [format(UseQMC),Samples]),
	    export_rgb(F, color, Ambient),
	    println(F, "</light>");
	hemilight -> ok;
	pathlight when Power > 0.0 ->
	    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\"", 
		    [Type,Name,Power]),
	    UseQMC = proplists:get_value(use_QMC, YafRay, 
					 ?DEF_USE_QMC),
	    Depth = proplists:get_value(depth, YafRay, ?DEF_DEPTH),
	    CausDepth = proplists:get_value(caus_depth, YafRay, ?DEF_CAUS_DEPTH),
	    Direct = proplists:get_value(direct, YafRay, ?DEF_DIRECT),
	    Samples = proplists:get_value(samples, YafRay, 
					  ?DEF_SAMPLES),
	    print(F, "       use_QMC=\"~s\" samples=\"~w\" "
		  "depth=\"~w\" caus_depth=\"~w\"", 
		  [format(UseQMC),Samples,Depth,CausDepth]),
	    case Direct of
		true ->
		    print(F, " direct=\"on\"", []);
		false ->
		    case proplists:get_value(cache, YafRay, ?DEF_CACHE) of
			true ->
			    CacheSize = 
				proplists:get_value(cache_size, YafRay, 
						    ?DEF_CACHE_SIZE),
			    AngleThreshold = 
				proplists:get_value(angle_threshold, YafRay, 
						    ?DEF_ANGLE_THRESHOLD),
			    ShadowThreshold = 
				proplists:get_value(shadow_threshold, YafRay, 
						    ?DEF_SHADOW_THRESHOLD),
			    Gradient = 
				proplists:get_value(gradient, YafRay, 
						    ?DEF_GRADIENT),
			    ShowSamples = 
				proplists:get_value(show_samples, YafRay, 
						    ?DEF_SHOW_SAMPLES),
			    Search = 
				proplists:get_value(search, YafRay, ?DEF_SEARCH),
			    print(F, " cache=\"on\"~n"
				  "       cache_size=\"~.10f\" "
				  "angle_threshold=\"~.10f\"~n"
				  "       shadow_threshold=\"~.10f\" "
				  "gradient=\"~s\"~n"
				  "       show_samples=\"~s\" search=\"~w\"",
				  [CacheSize,AngleThreshold,
				   ShadowThreshold,format(Gradient),
				   format(ShowSamples),Search]);
			false -> ok
		    end
	    end,
	    println(F, ">~n</light>", []);
	pathlight -> ok;
	globalphotonlight ->
	    println(F,"<light type=\"~w\" name=\"~s\"", [Type,Name]),
	    GplPhotons = proplists:get_value(
			   globalphotonlight_photons, YafRay,
			   ?DEF_GLOBALPHOTONLIGHT_PHOTONS),
	    GplRadius = proplists:get_value(
			  globalphotonlight_radius, YafRay,
			  ?DEF_GLOBALPHOTONLIGHT_RADIUS),
	    GplDepth = proplists:get_value(
			 globalphotonlight_depth, YafRay,
			 ?DEF_GLOBALPHOTONLIGHT_DEPTH),
	    GplSearch = proplists:get_value(
			  globalphotonlight_search, YafRay,
			  ?DEF_GLOBALPHOTONLIGHT_SEARCH),
	    println(F,"       photons=\"~w\" radius=\"~.3f\" "
		    "depth=\"~w\" search=\"~w\">", 
		    [GplPhotons,GplRadius,GplDepth,GplSearch]),
	    println(F, "</light>")
    end,
    proplists:get_value(background, YafRay, ?DEF_BACKGROUND);
export_light(F, Name, area, OpenGL, YafRay) ->
    Color = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    #e3d_mesh{vs=Vs,fs=Fs0} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs),
    Power = proplists:get_value(power, YafRay, ?DEF_ATTN_POWER),
    Samples = proplists:get_value(arealight_samples, YafRay, 
					   ?DEF_AREALIGHT_SAMPLES),
    Psamples = proplists:get_value(arealight_psamples, YafRay, 
					    ?DEF_AREALIGHT_PSAMPLES),
    Dummy = proplists:get_value(dummy, YafRay, ?DEF_DUMMY),
    Fs = foldr(fun (Face, Acc) -> 
			e3d_mesh:quadrangulate_face(Face, Vs)++Acc
		end, [], Fs0),
    As = e3d_mesh:face_areas(Fs, Vs),
    Area = foldl(fun (A, Acc) -> A+Acc end, 0.0, As),
    AFs = zip(As, Fs),
    foldl(
      fun ({Af,#e3d_face{vs=VsF}}, I) ->
	      case catch Power*Af/Area of
		  {'EXIT',{badarith,_}} -> I;
		  Pwr ->
		      NameI = Name++"_"++integer_to_list(I),
		      [A,B,C,D] = quadrangle_vertices(VsF, VsT),
		      println(F, "<light type=\"arealight\" "
			      "name=\"~s\" power=\"~.3f\"~n"
			      "       samples=\"~w\" "
			      "psamples=\"~w\" dummy=\"~s\">", 
			      [NameI,Pwr,Samples,Psamples,format(Dummy)]),
		      export_rgb(F, color, Color),
		      export_pos(F, a, A),
		      export_pos(F, b, B),
		      export_pos(F, c, C),
		      export_pos(F, d, D),
		      println(F, "</light>"),
		      I+1
	      end
      end, 1, AFs),
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
    #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov} = 
	proplists:lookup(camera_info, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    Ro = math:pi()/180.0,
    %% Fov is vertical angle from lower to upper border.
    %% YafRay focal plane is 1 unit wide.
    FocalDist = 0.5 / ((Width/Height) * math:tan(limit_fov(Fov)*0.5*Ro)),
    println(F, "<camera name=\"~s\" "++
	    "resx=\"~w\" resy=\"~w\" focal=\"~.10f\">",
	    [Name,Width,Height,FocalDist]),
    export_pos(F, from, Pos),
    export_pos(F, to, e3d_vec:add(Pos, Dir)),
    export_pos(F, up, e3d_vec:add(Pos, Up)),
    println(F, "</camera>").

limit_fov(Fov) when Fov < 1.0 -> 1.0;
limit_fov(Fov) when Fov > 179.0 -> 179.0;
limit_fov(Fov) -> Fov.



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
%%% 	    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
	    Turbidity = proplists:get_value(turbidity, YafRay, ?DEF_TURBIDITY),
	    A_var = proplists:get_value(a_var, YafRay, ?DEF_SUNSKY_VAR),
	    B_var = proplists:get_value(b_var, YafRay, ?DEF_SUNSKY_VAR),
	    C_var = proplists:get_value(c_var, YafRay, ?DEF_SUNSKY_VAR),
	    D_var = proplists:get_value(d_var, YafRay, ?DEF_SUNSKY_VAR),
	    E_var = proplists:get_value(e_var, YafRay, ?DEF_SUNSKY_VAR),
	    Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),
	    println(F, "~n            turbidity=\"~.3f\" a_var=\"~.3f\"~n"
		    "            b_var=\"~.3f\" c_var=\"~.3f\"~n"
		    "            d_var=\"~.3f\" e_var=\"~.3f\" "
		    "add_sun=\"off\">",
		    [Turbidity,A_var,B_var,C_var,D_var,E_var]),
%%% 	    if Power > 0.0 ->
%%% 		    println(F, "~n            add_sun=\"on\" "
%%% 			    "sun_power=\"~.3f\">",
%%% 			    [Power]);
%%% 	       true ->
%%% 		    println(F, ">")
%%% 	    end,
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
    #camera_info{dir=Dir} = proplists:lookup(camera_info, Attr),
    Focus = e3d_vec:len(Dir),
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
	    "        bias=\"~.10f\" AA_threshold=\"~.10f\"~n"
	    "        AA_minsamples=\"~w\" AA_pixelwidth=\"~.10f\">~n"
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



%% A bit like os:find_executable, but if the found executable is a .bat file
%% on windows; scan the .bat file for a real executable file, and return the
%% looked up executable name instead of the full path (except for .bat files).
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
			    Name
		    end;
		_ ->
		    Name
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
	    File = [C || C <- Line, C =/= $"], % Remove doublequotes
	    Filename = filename:nativename(File),
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

%% Universal quoting
%%
%% If the string contains singlequote, doublequote or whitespace
%% - doublequote the string and singlequote embedded doublequotes.
%% God may forbid doublequotes. They do not work in Windows filenames, 
%% nor in YafRay result .tga filenames. They might only work in 
%% Unix executable pathname being very weird even there.
%%
uquote(Cs) ->
    case uquote_needed(Cs) of
	true -> [$"|uquote_1(Cs)];
	false -> Cs
    end.

uquote_1([]) -> [$"];
uquote_1([$"]) -> [$",$',$",$'];
uquote_1([$"|Cs]) -> [$",$',$",$',$"|uquote_1(Cs)];
uquote_1([C|Cs]) -> [C|uquote_1(Cs)].

uquote_needed([]) -> false;
uquote_needed([$"|_]) -> true;
uquote_needed([$'|_]) -> true;
uquote_needed([$\s|_]) -> true;
uquote_needed([$\t|_]) -> true;
uquote_needed([$\r|_]) -> true;
uquote_needed([$\n|_]) -> true;
uquote_needed([_|Cs]) -> uquote_needed(Cs).


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

%% Zip lists together into a list of tuples
%%
zip([], []) -> [];
zip([H1|T1], [H2|T2]) -> [{H1,H2}|zip(T1, T2)].

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


%% Returns the time difference as a deep string in 
%% S.sss, M:S.sss, or H:M:S.sss deep string format, 
%% with trailing format description.
now_diff({A1,B1,C1}, {A2,B2,C2}) ->
    now_diff_1(((A1-A2)*1000000 + B1 - B2)*1000000 + C1 - C2).

now_diff_1(T) when T < 0 -> 
    [$-|now_diff_2(-T)];
now_diff_1(T) -> now_diff_2(T).

now_diff_2(T0) ->
    Us = T0 rem 60000000,
    T1 = T0 div 60000000,
    M = T1 rem 60,
    H = T1 div 60,
    case {H,M} of
	{0,0} -> now_diff_us(Us);
	{0,_} -> now_diff_m(M, Us);
	{_,_} -> now_diff_h(H, M, Us)
    end.

now_diff_h(H, M, Us) ->
    [integer_to_list(H),$:|now_diff_m(M, Us)].

now_diff_m(M, Us) ->
    [integer_to_list(M),$'|now_diff_us(Us)].

now_diff_us(Us) ->
    io_lib:format("~.3f\"", [Us/1000000.0]).

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
      "the dialog field names should be self-explanatory">>,
    <<"Note: For a YafRay Global Photon Light (one of the Ambient lights) - "
     "the Power parameter is ignored">>].
