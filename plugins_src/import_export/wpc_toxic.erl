%%
%%  wpc_toxic.erl
%%
%%     Toxic Plugin User Interface and Exporter.
%%
%%  Copyright (c) 2004 Dan Gudmundsson Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_toxic.erl,v 1.8 2004/06/12 05:38:44 dgud Exp $
%%

-module(wpc_toxic).

-export([init/0,menu/2,dialog/2,command/2]).

-include_lib("kernel/include/file.hrl").

-include("e3d.hrl").
-include("e3d_image.hrl").
-include("wings.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,keydelete/3,
		foreach/2,foldl/3,foldr/3]).

-define(TAG, toxic).
-define(TAG_RENDER, toxic_render).

%%% Default values

-define(DEF_DIALOGS, auto).
-define(DEF_RENDERER, "toxic").
-define(DEF_OPTIONS, "").
-define(DEF_LOAD_IMAGE, true).
-define(DEF_EXPORT_MESH, true).

-define(DEF_SUBDIVISIONS, 0).
-define(DEF_GAMMA, 2.0).

%% Render Opts
-define(DEF_DIRECT_LIGHT, true).
-define(DEF_DIRECT_LIGHT_MIN, true).
-define(DEF_INDIRECT_LIGHT, false).
-define(DEF_INDIRECT_LIGHT_MIN, true).
-define(DEF_SPECULAR_REFL, false).
-define(DEF_SPECULAR_REFL_MIN, true).
-define(DEF_CAUSTICS, false).
-define(DEF_CAUSTICS_MIN, true).
-define(DEF_WIDTH, 100).
-define(DEF_HEIGHT, 100).

-define(DEF_SAMPLING, whitted).
-define(DEF_SSTYPE,   stratified).
-define(DEF_SSRANDOM, 8).
-define(DEF_SSWIDTH,  8).
-define(DEF_SSHEIGHT, 8).
-define(DEF_WACONTRAST,  0.025).
-define(DEF_WAMAX, 3).

-define(DEF_IDLNOPHOTONS, 1000000).
-define(DEF_IDLMAXPHOTONS,500).
-define(DEF_IDLMAXDIST,   1000000).

-define(DEF_RADPRE_COMP, true).
-define(DEF_RADPRE_SPACING, 4).
-define(DEF_RADPRE_SEARCH,  0.1).

-define(DEF_RADPRIMARY, false).
-define(DEF_RADSECONDARY, false).
-define(DEF_RADSECDIST, 0.01).

-define(DEF_SPECDEPTH,  3).

%% Shader
-define(DEF_BDF, lambertian).
-define(DEF_EDF, false).
-define(DEF_EDF_RADIANCE, 5).

%% Arealight
-define(DEF_AREALIGHT, false).
-define(DEF_AREALIGHT_SAMPLES, 50).
-define(DEF_AREALIGHT_PSAMPLES, 0).
-define(DEF_DUMMY, false).

%% Light
-define(DEF_POWER, 10.0).
-define(DEF_ATTN_POWER, 10.0).

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
    %%    erlang:display({?MODULE,?LINE,Spec}),
    next.

dialog({material_editor_setup,Name,Mat}, Dialog) ->
    maybe_append(edit, Dialog, material_dialog(Name, Mat));
dialog({material_editor_result,Name,Mat}, Res) ->
    case is_plugin_active(edit) of
	false ->    {Mat,Res};
	_ ->     material_result(Name, Mat, Res)
    end;
dialog({light_editor_setup,Name,Ps}, Dialog) ->
    maybe_append(edit, Dialog, light_dialog(Name, Ps));
dialog({light_editor_result,Name,Ps0}, Res) ->
    case is_plugin_active(edit) of
	false ->   {Ps0,Res};
	_ ->     light_result(Name, Ps0, Res)
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
	    absolute -> Renderer;
	    _ ->
		case find_executable(Renderer) of
		    false -> false;
		    Path ->  Path
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
	false ->    Menu;
	_ ->     Menu++PluginMenu
    end.

is_plugin_active(Condition) ->
    case Condition of
	export ->   get_var(dialogs);
	edit ->     get_var(dialogs);
	render ->   get_var(renderer);
	_ ->     false
    end.

menu_entry(render) ->    [{"Toxic (.tga)",?TAG,[option]}];
menu_entry(export) ->    [{"Toxic (.xml)",?TAG,[option]}];
menu_entry(pref) ->      [{"Toxic",?TAG}].

command_file(render, Attr, St) when is_list(Attr) ->
    set_pref(Attr),
    case get_var(rendering) of
	false ->
	    do_export(export, props(render), [{?TAG_RENDER,true}|Attr], St);
	_RenderFile ->
	    wpa:error("Already rendering.")
    end;
command_file(render, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "Toxic Render Options", export_dialog(render),
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
    set_pref(Attr),
    do_export(Op, props(Op), Attr, St);
command_file(Op, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "Toxic Export Options", export_dialog(Op),
	       fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).

props(render) ->
    [{title,"Render"},{ext,".tga"},{ext_desc,"Targa File"}];
props(export) ->
    [{title,"Export"},{ext,".xml"},{ext_desc,"Toxic File"}];
props(export_selected) ->
    [{title,"Export Selected"},{ext,".xml"},{ext_desc,"Toxic File"}].

-record(camera_info, {pos,dir,up,fov,origin,distance,azimuth,
		      elevation,pan_x,pan_y}).

attr(St, Attr) ->
    [{Pos,Dir,Up},Fov,Origin,Dist,Az,El,{PanX,PanY}] = 
	wpa:camera_info([pos_dir_up,fov,aim,distance_to_aim,
			 azimuth,elevation,tracking]),
    
    CameraInfo = #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov,
			      origin=Origin,distance=Dist,azimuth=Az,
			      elevation=El,pan_x=PanX,pan_y=PanY},
    [CameraInfo,{lights,wpa:lights(St)}|Attr].

do_export(Op, Props0, Attr, St0) ->    
    case pget(export_mesh,Attr) of
	true -> 
	    SubDiv = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
	    Props = [{subdivisions,SubDiv}|Props0],
	    %% Freeze virtual mirrors.
	    Shapes0 = gb_trees:to_list(St0#st.shapes),
	    Shapes = [{Id, wpa:vm_freeze(We)} || {Id, We} <- Shapes0],
	    St = St0#st{shapes = gb_trees:from_orddict(Shapes)},
	    wpa:Op(Props, fun_export_2(attr(St, Attr)), St);
	false ->
	    wpa:Op(Props0, fun_export_2(attr(St0, Attr)), St0)
    end.

fun_export_2(Attr) ->
    fun(Filename, Contents) ->
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
    Toxic   = proplists:get_value(?TAG, Mat, []),
    Min     = pget(shader_minimized, Toxic, true),
    BDF     = pget(bdf, Toxic, ?DEF_BDF),
    EDF     = pget(edf, Toxic, ?DEF_EDF),
    Radiant = pget(radiant, Toxic, if EDF == false -> 0;
				      true -> ?DEF_EDF_RADIANCE end),
    [{vframe,
      [{hframe,
	[{label, "BRDF"},
	 {hradio,[{"Lambertian",lambertian},
		  {"Perfect Specular",perfectspecular}],
	  BDF,[layout,{key,{?TAG,bdf}}]},
	 panel,
	 help_button({material_dialog,shaders})]},
       {hframe,
	[{"Enable Emission", EDF, [{key, {?TAG,edf}}]},
	 {label, "Radiant Exitance"},
	 {text, Radiant, [{range, {0, 500000}}, {key, {?TAG, radiant}},
			  enable_hook({?TAG, edf})]}]}],
      [{title,"Toxic Options"},{minimized,Min},
       {key,{?TAG,shader_minimized}}]}].

rgba2rgb({R,G,B,_}) -> {R,G,B}.

material_result(_Name, Mat0, [{{?TAG,shader_minimized},_}|_]=Res0) ->
    {Ps,Res} = split_list(Res0, 4),
    Mat = [{?TAG,Ps}|keydelete(?TAG, 1, Mat0)],
    {Mat,Res};
material_result(Name, Mat, Res) ->
    exit({invalid_tag,{?MODULE,?LINE,[Name,Mat,Res]}}).

light_dialog(_Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    Toxic = proplists:get_value(?TAG, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    DefPower = case Type of
		   point -> ?DEF_ATTN_POWER;
		   spot -> ?DEF_ATTN_POWER;
		   area -> ?DEF_ATTN_POWER;
		   _ -> ?DEF_POWER
	       end,
    Minimized = pget(minimized, Toxic, true),
    Power = pget(power, Toxic, DefPower),
    CastShadows = pget(cast_shadows, Toxic, true),
    case Type of
	area -> [{label, "Not supported for toxic use emission in material setting instead"}];
	ambient -> [];
	_ ->
	    [{vframe,
	      [{hframe,[{label,"Power"},
			{text,Power,[{range,{0.0,10000.0}},{key,{?TAG,power}}]},
			panel,
			help_button(light_dialog)]},
	       {"Cast Shadows",CastShadows,[{key,{?TAG,cast_shadows}}]}],
	      [{title,"Toxic Options"},{key,{?TAG,minimized}},{minimized,Minimized}]}]
    end.

max_hook(Key, Values) when list(Values) ->
    {hook,
     fun (is_minimized, {_Var,I,Sto}) when is_integer(Key) ->
	     not lists:member(gb_trees:get(I+Key, Sto), Values);
	 (is_minimized, {_Var,_I,Sto}) ->
%%	     io:format("~p ~p~n", [Key, gb_trees:get(Key, Sto)]),
	     not lists:member(gb_trees:get(Key, Sto), Values);
	 (_, _) -> void
     end};
max_hook(Key, Value) -> max_hook(Key, [Value]).

enable_hook(Key) ->
    {hook,
     fun (is_disabled, {_Var,_I,Store}) ->
	     not enable_hook_eval(Key, Store);
	 (_, _) -> void
     end}.

enable_hook_eval(['not',Key], Store) ->
    not enable_hook_eval(Key, Store);
enable_hook_eval(['and'|Keys], Store) ->
    enable_hook_and(Keys, Store);
enable_hook_eval(['or'|Keys], Store) ->
    enable_hook_or(Keys, Store);
enable_hook_eval(Key, Store) when not is_list(Key) ->
    gb_trees:get(Key, Store).

enable_hook_and([Key], Store) ->
    enable_hook_eval(Key, Store);
enable_hook_and([Key|Keys], Store) ->
    enable_hook_eval(Key, Store) andalso enable_hook_and(Keys, Store).

enable_hook_or([Key], Store) ->
    enable_hook_eval(Key, Store);
enable_hook_or([Key|Keys], Store) ->
    enable_hook_eval(Key, Store) orelse enable_hook_or(Keys, Store).

light_result(_Name, Ps0, [{{?TAG,minimized},_}|_]=Res0) ->
    {LightPs,Res} = split_list(Res0,3),
    Ps = [{?TAG,LightPs} | keydelete(?TAG, 1, Ps0)],
    %%    erlang:display({?MODULE,?LINE,[Ps,Res1]}),
    {Ps,Res};
light_result(_Name, Ps, Res) ->
    {Ps,Res}.

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
    wpa:dialog("Toxic Options", Dialog,
	       fun (Attr) -> pref_result(Attr,St) end).

pref_result(Attr, St) ->
    set_pref(Attr),
    init_pref(),
    St.

export_dialog(Operation) ->
    SubDiv     = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
    GScale     = get_pref(globalscale,  1.0),
    
    Camera     = get_pref(camera,  pinhole),
    Fstop      = get_pref(fstop,        1.4),   %??
    Focallen   = get_pref(focallen,     0.125), %??
    FocalDist  = get_pref(focaldist,    2.0),   %??
    AutoFocus  = get_pref(autofocus,    true),
    AutoPosX   = get_pref(autoposx,     0.0),
    AutoPosY   = get_pref(autoposy,     0.0),

    Gamma      = get_pref(gamma, ?DEF_GAMMA),
    Width      = get_pref(width, ?DEF_WIDTH),
    Height     = get_pref(height, ?DEF_HEIGHT),
    BG         = get_pref(background, {0.0,0.0,0.0}),
    LoadImage  = get_pref(load_image, ?DEF_LOAD_IMAGE),

    ExportMesh = get_pref(export_mesh, ?DEF_EXPORT_MESH),
    %% Lighting
    DirectLight      = get_pref(direct_light, ?DEF_DIRECT_LIGHT),
    DirectLightMin   = get_pref(direct_light_min, ?DEF_DIRECT_LIGHT_MIN),
    InDirectLight    = get_pref(indirect_light, ?DEF_INDIRECT_LIGHT),
    InDirectLightMin = get_pref(indirect_light_min,?DEF_INDIRECT_LIGHT_MIN),
    SpecularRefl     = get_pref(specular_refl, ?DEF_SPECULAR_REFL),
    SpecularReflMin  = get_pref(specular_refl_min, ?DEF_SPECULAR_REFL_MIN),
    Caustics         = get_pref(caustics, ?DEF_CAUSTICS),
    CausticsMin      = get_pref(caustics_min, ?DEF_CAUSTICS_MIN),

    %% Sampling
    Sampling  = get_pref(sampling,  ?DEF_SAMPLING),
    SStype    = get_pref({pixel,sstype},    ?DEF_SSTYPE),
    SSrandom  = get_pref({pixel,ssrandom},  ?DEF_SSRANDOM),
    SSWidth   = get_pref({pixel,sswidth},   ?DEF_SSWIDTH),
    SSHeight  = get_pref({pixel,ssheight},  ?DEF_SSHEIGHT),
    WAContrast= get_pref(wacontrast,?DEF_WACONTRAST),
    WAMax     = get_pref(wamax,     ?DEF_WAMAX),
    %% Direct Light (Area parameters)
    DLtype    = get_pref({direct_light,sstype},    ?DEF_SSTYPE),
    DLrandom  = get_pref({direct_light,ssrandom},  ?DEF_SSRANDOM),
    DLWidth   = get_pref({direct_light,sswidth},   ?DEF_SSWIDTH),
    DLHeight  = get_pref({direct_light,ssheight},  ?DEF_SSHEIGHT),
    %% InDirect Light
    IDLNoPhotons = get_pref(idlnoPhotons,  ?DEF_IDLNOPHOTONS),
    IDLMaxPhotons= get_pref(idlmaxPhotons, ?DEF_IDLMAXPHOTONS),
    IDLMaxDist   = get_pref(idlmaxDist,    ?DEF_IDLMAXDIST),

    RadPreComp    = get_pref(radprecomp,     ?DEF_RADPRE_COMP),
    RadPreSpacing = get_pref(radprespacing,  ?DEF_RADPRE_SPACING),
    RadPreSearch  = get_pref(radpresearch,   ?DEF_RADPRE_SEARCH),

    RadPrimary = get_pref(radprimary,    ?DEF_RADPRIMARY),
    IDLtype    = get_pref({indirect_light,sstype},    ?DEF_SSTYPE),
    IDLrandom  = get_pref({indirect_light,ssrandom},  ?DEF_SSRANDOM),
    IDLWidth   = get_pref({indirect_light,sswidth},   ?DEF_SSWIDTH*2),
    IDLHeight  = get_pref({indirect_light,ssheight},  ?DEF_SSHEIGHT div 2),

    RadSecondary = get_pref(radsecondary,    ?DEF_RADSECONDARY),
    RadSecDist   = get_pref(radsecdist,      ?DEF_RADSECDIST),

    %% Specular
    SpecDepth    = get_pref(specdepth,       ?DEF_SPECDEPTH),

    %% Caustics
    CAUNoPhotons = get_pref(caunoPhotons,  ?DEF_IDLNOPHOTONS),
    CAUMaxPhotons= get_pref(caumaxPhotons, ?DEF_IDLMAXPHOTONS),
    CAUMaxDist   = get_pref(caumaxDist,    ?DEF_IDLMAXDIST),

    Options      = get_pref(options,   []),

    [{vframe,
      [{hframe,
	[{hframe,[{"Export Mesh(es)", ExportMesh, [{key, {?TAG,export_mesh}}]},
		  {label,"Sub-division Steps"},
		  {text,SubDiv,[{key,{?TAG,subdivisions}},{range,{0,10}},
				enable_hook({?TAG, export_mesh})]}],
	  [{title,"Pre-rendering"}]},
	 panel,
	 help_button(export_dialog)]},
       {hframe, [{label, "Scale"},
		 {text, GScale, [{range,{0.0,1.0E200}},
				 {key, {?TAG,globalscale}}]}],
	[{title,"Global Scale"}]},
       {vframe, [{hframe, [{hradio,[{"PinHole Camera",pinhole},
				    {"Thin Lens Camera",thinlens}],
			    Camera,[layout,{key,{?TAG,camera}}]}]},
		 {hframe, [{label, "Fstop"},
			   {text, Fstop, [{key, {?TAG,fstop}}]},
			   {label, "Focal length"},
			   {text, Focallen, [{key, {?TAG,focallen}}]}
			  ], [max_hook({?TAG,camera},[thinlens])]},
		 {vframe, [{hframe, [{"AutoFocus", AutoFocus,
				      [{key, {?TAG,autofocus}},layout]},
				     {hframe, [{label, "X"},
					       {text,AutoPosX,
						[{key,{?TAG,autoposx}},
						 {range,{-0.5,0.5}}]},
					       {label, "Y"},
					       {text,AutoPosY,
						[{key,{?TAG,autoposy}},
						 {range,{-0.5,0.5}}]}],
				      [max_hook({?TAG, autofocus},[true])]},
				     {hframe, [{label, "Focal Distance"},
					       {text, FocalDist, 
						[{key, {?TAG,focaldist}}]}],
				      [max_hook({?TAG, autofocus},[false])]}]
			   }],[max_hook({?TAG,camera},[thinlens])]}
		], [{title, "Camera"}]},
       {vframe,
	[{hradio,[{"Super Sampling",super},
		  {"Whitted Addaptive",whitted}],
	  Sampling,[layout,{key,{?TAG,sampling}}]},
	 {vframe,
	  pixelsampling(pixel,[SStype, SSrandom,SSWidth,SSHeight]),
	  [max_hook({?TAG, sampling},[super])]},
	 {hframe,
	  [{vframe, [{label, "Contrast Threshold"},
		     {label, "Max Depth"}]},
	   {vframe, [{text, WAContrast, [{key, {?TAG,wacontrast}}]},
		     {text, WAMax, [{range, {0, 500}}, {key, {?TAG, wamax}}]}]}],
	  [max_hook({?TAG, sampling},[whitted])]}],
	[{title,"Pixel Sampling"}]},
       {vframe,
	[{hframe,
	  [{"", DirectLight, [{key, {?TAG,direct_light}}]},
	   {vframe,
	    pixelsampling(direct_light,[DLtype, DLrandom, DLWidth,DLHeight]),
	    [{title,"Enable Direct Lighting"},
	     {minimized,DirectLightMin},{key,{?TAG,direct_light_min}},
	     enable_hook({?TAG,direct_light})]}]},
	 {hframe,
	  [{"", InDirectLight, [{key, {?TAG,indirect_light}}]},
	   {vframe,
	    [{hframe,
	      [{label, "Emit Photons"},
	       {text,
		IDLNoPhotons,[{range,{10000,50000000}},{key,{?TAG,idlnoPhotons}}]},
	       {label, "Max Photons"},
	       {text,
		IDLMaxPhotons,[{range,{10,50000}},{key,{?TAG,idlmaxPhotons}}]},
	       {label, "Max Distance"},
	       {text, IDLMaxDist, [{key, {?TAG, idlmaxDist}}]}]},
	     {hframe,
	      [{"", RadPreComp, [{key, {?TAG, radprecomp}}]},
	       {label, "Radiance Precomputation"},
	       {hframe,
		[
		 {label, "Spacing"},
		 {text, RadPreSpacing,[{range,{1,10000}},
				       {key,{?TAG,{radprecomp,spacing}}}]},
		 {label, "Search Dist"},
		 {text, RadPreSearch, [{range,{0.1,100.0}},
				       {key, {?TAG, {radprecomp, search}}}]}],
		[enable_hook({?TAG, radprecomp})]}]},
	     {hframe,
	      [{"", RadPrimary, [{key, {?TAG, radprimary}}]},
	       {vframe,
		[{label, "Primary Final Gathering"} |
		 pixelsampling(radprimary, [IDLtype, IDLrandom,
					    IDLWidth,IDLHeight])],
		[enable_hook({?TAG, radprimary})]}]},
	     {hframe,
	      [{"", RadSecondary, [{key, {?TAG, radsecondary}}]},
	       {label, "Secondary Final Gathering"},
	       {text, RadSecDist, [{range, {0.0000001, 1000.0}},
				   {key, {?TAG, {radsecondary, dist}}},
				   enable_hook({?TAG,radsecondary})]}]}],
	    [{title,"Enable InDirect Lighting"},
	     {minimized,InDirectLightMin},
	     {key,{?TAG,indirect_light_min}},
	     enable_hook({?TAG,indirect_light})]}]},
	 {hframe,
	  [{"", SpecularRefl, [{key, {?TAG,specular_refl}}]},
	   {hframe,
	    [{label, "Reflection Depth"},
	     {text, SpecDepth, [{range, {0, 1000000}},
				{key, {?TAG, specdepth}}]}],
	    [{title,"Enable Specular Reflections"},
	     {minimized,SpecularReflMin},{key,{?TAG,specular_refl_min}},
	     enable_hook({?TAG,specular_refl})]}]},
	 {hframe,
	  [{"", Caustics, [{key, {?TAG,caustics}}]},
	   {hframe,
	    [{label, "Emit Photons"},
	     {text, CAUNoPhotons,
	      [{range,{10000,50000000}},{key,{?TAG,caunoPhotons}}]},
	     {label, "Max Photons"},
	     {text, CAUMaxPhotons,
	      [{range,{10,50000}},{key,{?TAG,caumaxPhotons}}]},
	     {label, "Max Distance"},
	     {text, CAUMaxDist, [{key, {?TAG, caumaxDist}}]}],
	    [{title,"Enable Caustics"},
	     {minimized,CausticsMin},{key,{?TAG,caustics_min}},
	     enable_hook({?TAG,caustics})]}]}],
	[{title,"Render"}]},
       {hframe,
	[{vframe,[{label,"Width"}]},
	 {vframe,[{text,Width,[{range,{1,10000}},{key,{?TAG,width}}]}]},
	 {vframe,[{label,"Height"}]},
	 {vframe,[{text,Height,[{range,{1,10000}},{key,{?TAG,height}}]}]},
	 {vframe,[{label,"Gamma"}]},
	 {vframe,[{text,Gamma,[{range,{0.0,10.0}},{key,{?TAG,gamma}}]}]},
	 {vframe,[{label,"BackGround"}]},
	 {vframe,[{color,BG,[{key,{?TAG,background}}]}]}],
	[{title,"Output"}]}]}
     |
     case Operation of
	 render ->
	     [{hframe,[{label,"Options"},
		       {text,Options,[{key,{?TAG,options}}]},
		       {"Load Image",LoadImage,[{key,load_image}]}],
	       [{title,"Rendering Job"}]}];
	 export ->
	     [];
	 export_selected ->
	     []
     end].

pixelsampling(PType, [Sample, Random, Width,Height]) ->
    [{hframe, [{hradio, [{"Random", random},
			 {"Regular", regular},
			 {"Stratified", stratified}],
		Sample, [layout, {key,{?TAG,{PType,sstype}}}]},
	       {hframe,
		[{label, "Samples"},
		 {text, Random, [{range,{1,128}},
				 {key,{?TAG,{PType,samples}}}]}],
		[max_hook({?TAG,{PType,sstype}},[random])]},
	       {hframe,
		[{label, "Width"},
		 {text, Width, [{range,{1,128}},{key,{?TAG,{PType,sswidth}}}]},
		 {label, "Height"},
		 {text, Height,[{range,{1,128}},{key,{?TAG,{PType,ssheight}}}]}
		],[max_hook({?TAG,{PType,sstype}},[regular, stratified])]}
	      ]}].

%% Boolean hook
%%
%% Used to enable/disable or minimize/maximize a field depending
%% on a boolean control field.
%%
% bhook(Type, Tag) ->
%     {hook,fun (is_disabled, {_Var,_I,Sto}) ->
% 		  case Type of
% 		      enabled -> not gb_trees:get(Tag, Sto);
% 		      disabled -> gb_trees:get(Tag, Sto);
% 		      _ -> void
% 		  end;
% 	      (is_minimized, {_Var,_I,Sto}) ->
% 		  case Type of
% 		      maximized -> not gb_trees:get(Tag, Sto);
% 		      minimized -> gb_trees:get(Tag, Sto);
% 		      _ -> void
% 		  end;
% 	      (_, _) -> void end}.

%%% Export and rendering functions
%%%

-record(files, {dir, image, scene, settings, objects}).

files(Render, Filename) ->
    RootName  = filename:rootname(Filename),
    SceneName = filename:rootname(filename:basename(Filename)),
    ExportDir0 = filename:dirname(Filename),
    ExportDir  = case Render of
		     false -> ExportDir0;
		     true ->
			 Dir = RootName ++ "_toxic",
			 case file:make_dir(Dir) of
			     ok -> Dir;
			     {error, eexist} -> Dir;
			     Err  -> exit(Err)
			 end
		 end,
    Image = case Render of
		true ->  Filename;
		false -> RootName ++".tga"
	    end,
    Scene    = filename:join(ExportDir, SceneName ++ ".xml"),
    Settings = filename:join(ExportDir, SceneName ++ ".settings.xml"),
    Objects  = filename:join(ExportDir, SceneName ++ ".objects.obj"),
    #files{dir=ExportDir, image=Image, scene=Scene,
	   settings=Settings, objects=Objects}.

export(Attr, Filename, E3DExport0) ->
    wpa:popup_console(),
    ExportTS = erlang:now(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    Files = files(Render, Filename),
%    E3DExport = obj_per_mat(E3DExport0),
    E3DExport = obj_per_file(E3DExport0),
    case pget(export_mesh, Attr) of
	true ->
	    export_objs(Files, E3DExport);
	false ->
	    ok
    end,

    export_settings(Files, Attr),
    export_scene(Files, E3DExport,Attr),

    RenderTS = erlang:now(),
    Renderer = get_var(renderer),
    case {Renderer,Render} of
	{_,false} ->
	    io:format("Export time:     ~s~n",
		      [now_diff(RenderTS, ExportTS)]),
	    ok;
	{false,_} ->
	    %% Should not happen since the file->render dialog
	    %% must have been disabled
	    no_renderer;
	_ ->
	    spawn_link(
	      fun () ->
		      set_var(export_ts, ExportTS),
		      set_var(render_ts, RenderTS),
		      render(Renderer, Attr, Files)
	      end),
	    set_var(rendering, Files#files.image),
	    ok
    end.

export_objs(#files{}, []) -> ok;
export_objs(F=#files{dir=Dir}, [E3F|R]) ->
    #e3d_file{objs=[#e3d_object{name=File}]} = E3F,
    e3d_obj:export(filename:join(Dir,File++".obj"), E3F),
    export_objs(F,R);
export_objs(#files{objects=File}, E3DExport) ->
    e3d_obj:export(File, E3DExport).

obj_per_mat(EF = #e3d_file{objs=Objs0}) ->
    Objs = split_objs(Objs0, []),
    EF#e3d_file{objs=Objs}.

obj_per_file(EF = #e3d_file{objs=Objs0, mat=Mtab}) ->
    Objs = split_objs(Objs0, []),
    [EF#e3d_file{objs=[Obj],
 		 mat=[element(2,lists:keysearch(hd(Obj#e3d_object.mat),1,Mtab))]}
     || Obj <- Objs].

split_objs([Ok=#e3d_object{mat=[_OneMat]}|R], Acc) ->
    split_objs(R, [Ok|Acc]);
split_objs([Obj0=#e3d_object{obj=Mesh0=#e3d_mesh{fs=FaceL0},name=Name}|R],Acc) ->
    case lists:keysort(#e3d_face.mat, FaceL0) of
	[] -> % hmm no faces..
	    split_objs(R, Acc);
	[First|SortedFL] ->
	    Objs = [Obj0#e3d_object{obj=Mesh0#e3d_mesh{fs=FaceL},
				    mat=Mat,name=mkName(Name,Mat)}
		    ||{Mat, FaceL} <- split_fs(SortedFL, First#e3d_face.mat, [[First]])],
	    split_objs(R, Objs ++ Acc)
    end;
split_objs([], Acc) -> Acc.

mkName(Name, [Mat0]) when list(Name) ->
    Mat = to_list(Mat0),
    Name ++ "_" ++ Mat;
mkName(Name, Mat) ->
    erlang:fault({strange_names, Name,Mat}).

to_list(Mat) when atom(Mat) ->
    atom_to_list(Mat);
to_list(Mat) when list(Mat) ->
    Mat;
to_list(Mat) ->
    exit({strange_matname, Mat}).

split_fs([F=#e3d_face{mat=Mat}|R], Mat, [Curr|Acc]) ->
    split_fs(R, Mat, [[F|Curr]|Acc]);
split_fs([F=#e3d_face{mat=Mat}|R], PrevMat, [Curr|Acc]) ->
    split_fs(R, Mat, [[F],{PrevMat,Curr}|Acc]);
split_fs([], Mat, [Curr|Acc]) ->
    [{Mat,Curr}|Acc].

export_settings(#files{settings=File}, Attr) ->
    F = open(File, export),
    io:format("Exporting to ~s~n", [File]),
    println(F, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
    println(F, "<!-- ~s: Exported from Wings3d -->",
	    [filename:basename(File)]),
    println(F, "<!-- Exported to toxic see: http://toxicengine.sourceforge.net -->"),
    println(F, "<ToxicSceneSettings xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
	    "xsi:noNamespaceSchemaLocation=\"toxicscene.settings.xsd\">"),
    println(F, " <Rendering>"),
    println(F, " <PixelSampling>"),
    case pget(sampling, Attr) of
	whitted ->
	    println(F,"  <WhittedAdaptiveSampling contrastthreshold=\"~s\" "
		    "maxdepth=\"~s\" />",
		    [format(pget(wacontrast, Attr)),
		     format(pget(wamax, Attr))]);
	super ->
	    println(F,"  <Supersampling>"),
	    export_sampling(F,pixel, Attr),
	    println(F,"  </Supersampling>")
    end,

    println(F, " </PixelSampling>"),
    println(F, " <Components>"),
    case pget(direct_light, Attr) of
	true ->
	    println(F,"  <DirectLighting>~n   <ArealightSampling>", []),
	    export_sampling(F,direct_light, Attr),
	    println(F,"   </ArealightSampling>~n  </DirectLighting>",[]);
	false -> ok
    end,
    case pget(indirect_light, Attr) of
	true ->
	    println(F, " <IndirectLighting>"),
	    println(F, "  <PhotonTracing photons=\"~s\" />",
		    [format(pget(idlnoPhotons,Attr))]),
	    println(F, "  <RadianceEstimate maxphotons=\"~s\" maxdistance=\"~s\"/>",
		    [format(pget(idlmaxPhotons,Attr)),
		     format(pget(idlmaxDist,Attr))]),
	    case pget(radprecomp,Attr) of
		true ->
		    println(F, "<RadiancePrecomputation spacing=\"~s\" "
			    "maxsearchdistance=\"~s\" />",
			    [format(pget({radprecomp,spacing},Attr)),
			     format(pget({radprecomp,search},Attr))]);
		false -> ok
	    end,
	    case pget(radprimary, Attr) of
		true ->
		    println(F, " <PrimaryFinalGathering>"),
		    export_sampling(F,radprimary, Attr),
		    println(F, " </PrimaryFinalGathering>");
		false -> ok
	    end,
	    case pget(radsecondary, Attr) of
		true ->
		    println(F, " <SecondaryFinalGathering distancethreshold=\"~s\">",
			    [format(pget({radsecondary,dist},Attr))]),
		    println(F, " </SecondaryFinalGathering>");
		false -> ok
	    end,
	    println(F," </IndirectLighting>");
	false -> ok
    end,
    case pget(specular_refl, Attr) of
	true ->
	    println(F, " <SpecularReflections maxdepth=\"~s\" />",
		    [format(pget(specdepth,Attr))]);
	false -> ok
    end,
    case pget(caustics, Attr) of
	true ->
	    println(F, " <Caustics>"),
	    println(F, "  <PhotonTracing photons=\"~s\" />",
		    [format(pget(caunoPhotons,Attr))]),
	    println(F, "  <RadianceEstimate maxphotons=\"~s\" maxdistance=\"~s\"/>",
		    [format(pget(caumaxPhotons,Attr)),
		     format(pget(caumaxDist,Attr))]),
	    println(F, " </Caustics>");
	false -> ok
    end,
    println(F, " </Components>"),
    println(F, " </Rendering>"),
    println(F, " <Output>"),
    println(F, "  <GammaCorrection targetgamma=\"~s\" />",
	    [format(pget(gamma, Attr))]),
    println(F, " </Output>"),
    println(F, "</ToxicSceneSettings>"),
    close(F),
    ok.

export_sampling(F,Type,Attr) ->
    Samples = pget({Type,samples},Attr),
    W = pget({Type,sswidth},Attr),
    H = pget({Type,ssheight},Attr),
    case pget({Type,sstype},Attr) of
	random ->
	    println(F, "<RandomSampling samples=\"~s\" />",
		    [format(Samples)]);
	stratified ->
	    println(F, "<StratifiedSampling width=\"~s\" height=\"~s\" />",
		    [format(W),format(H)]);
	regular ->
	    println(F, "<RegularSampling width=\"~s\" height=\"~s\" />",
		    [format(W),format(H)])
    end.

export_scene(#files{scene=File,dir=Dir, objects=Wavefront}, Objs, Attrs) ->
    F = open(File, export),
    io:format("Exporting to ~s~n", [File]),
    println(F, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
    println(F, "<!-- ~s: Exported from Wings3d -->",
	    [filename:basename(File)]),
    println(F, "<!-- Exported to toxic see: http://toxicengine.sourceforge.net -->"),
    println(F, "<ToxicScene xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
	    "xsi:noNamespaceSchemaLocation=\"../../schemas/toxicscene.xsd\">"),
    println(F, " <Frame>"),
    println(F, "  <Parameter name=\"backgroundcolor\" value=\"~s ~s ~s\"/>",
	    vector_to_list(pget(background,Attrs))),

    Mat = case Objs of  %% Hack
	      #e3d_file{mat=Mtab} -> 
		  Mtab;
	      _ -> 
		  lists:usort([OMat || #e3d_file{mat=[OMat]} <- Objs])
	  end,

    export_shaders(F, Mat, Dir),
    Scale = pget(globalscale, Attrs),
    export_obj_refs(F, Objs, Scale, Wavefront),
    Lights = proplists:get_value(lights, Attrs, []),
    export_lights(F, Lights, Scale),
    export_camera(F, Scale, Attrs),
    println(F, " </Frame>"),
    println(F, "</ToxicScene>"),
    close(F),
    ok.

export_shaders(F, [{Name,Mat}|Ms], ExportDir) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Toxic = proplists:get_value(?TAG, Mat, []),
    println(F, "  <SurfaceShader name=\"~s\">", [to_list(Name)]),
    println(F, "   <BDF type=\"~s\"/>", [to_list(pget(bdf, Toxic, lambertian))]),
    println(F, "   <Reflectance>"),
    case proplists:get_value(diffuse, Maps, none) of
	none -> 
	    {DR,DG,DB} = rgba2rgb(proplists:get_value(diffuse, OpenGL)),
	    println(F,"     <ConstantTexture value=\"~s ~s ~s\"/>",
		    [format(DR),format(DG),format(DB)]);
	#e3d_image{name=ImageName}=Image ->
	    MapFile = ImageName++".tga",
	    ok = e3d_image:save(Image, filename:join(ExportDir, MapFile)),
	    println(F,"     <ImageTexture href=\"~s\"/>",[MapFile])
    end,
    println(F, "   </Reflectance>"),
    case pget(edf, Toxic) of
	true -> 
	    {ER,EG,EB} = rgba2rgb(proplists:get_value(emission, OpenGL)),
	    Rad = pget(radiant, Toxic),
	    println(F, "   <EDF type=\"lambertian\"/>"),
	    println(F, "   <RadiantExitance value=\"~s ~s ~s\"/>",
		    [format(ER*Rad), format(EB*Rad), format(EG*Rad)]),
	    ok;
	_ -> ok
    end,
    println(F, "  </SurfaceShader>"),
    export_shaders(F, Ms, ExportDir);
export_shaders(_, [], _) -> ok.
export_obj_refs(F, [Obj|Os], Scale, File) ->
    {Name, MatName} = 
	case Obj of 
	    #e3d_object{name=N,mat=[M]} -> {N,M};
	    #e3d_file{objs=[#e3d_object{name=N,mat=[M]}]} -> {N,M}
	end,

    println(F,"  <Object type=\"mesh\">"),
%     println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
% 	    ["href", filename:basename(File)]),
    println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
	    ["href", filename:basename(Name++".obj")]),
%     println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
% 	    ["include", Name]),
    println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
	    ["surfaceshader", MatName]),
    println(F,"     <Transform> <Scale value=\"~s\"/> </Transform>", [format(Scale)]),
    println(F,"  </Object>"),
    export_obj_refs(F, Os, Scale, File);
export_obj_refs(_, [],_,_) ->
    ok.

export_lights(F, [{_Name,L}|Ls], Scale) ->
    case proplists:get_value(visible,L,true) of
	true ->
	    OpenGL = proplists:get_value(opengl, L, []),
	    Toxic  = proplists:get_value(?TAG, L, []),
	    Type   = proplists:get_value(type, OpenGL, []),
	    export_light(F, Type, Scale, OpenGL, Toxic);
	_ ->
	    undefined
    end,
    export_lights(F, Ls,Scale);
export_lights(_, [],_) -> ok.

export_light(_, area, _, _, _) -> ignore;     %% BUGBUG Arealights.
export_light(F, ambient, _, OpenGL, _) -> 
    {DR,DG,DB} = rgba2rgb(proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0})),
    println(F,"  <Parameter name=\"ambientillum\" value=\"~s ~s ~s\"/>", 
	    [format(DR),format(DG),format(DB)]); 
export_light(F, _, Scale, OpenGL, Toxic) ->
    {X,Y,Z} = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    {DR,DG,DB} = rgba2rgb(proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0})),
    Power = pget(power, Toxic, ?DEF_ATTN_POWER),
    CastShadows = pget(cast_shadows, Toxic, true),
    println(F,"  <Object type=\"pointlight\">"),
    println(F,"  <Parameter name=\"power\" value=\"~s ~s ~s\"/>", 
	    [format(DR*Power),format(DG*Power),format(DB*Power)]),
    println(F,"  <Parameter name=\"castsshadow\" value=\"~s\"/>", 
	    [to_list(CastShadows)]),
    println(F,"    <Transform>"),
    println(F,"      <Translation value=\"~s ~s ~s\"/>", 
	    [format(X),format(Y),format(Z)]),
    println(F,"      <Scale value=\"~s\"/>", [format(Scale)]),
    println(F,"    </Transform>"),
    println(F,"  </Object>").

export_camera(F, Scale, Attr) ->
    #camera_info{ %pos=_Pos,dir=Dir,up=Up,
	       fov=Fov, origin=Origin,distance=Dist,azimuth=Az,
	       elevation=El,pan_x=PanX,pan_y=PanY} = 
	proplists:lookup(camera_info, Attr),
    case pget(camera, Attr) of
	pinhole ->
	    println(F,"  <Object type=\"pinholecamera\">");
	thinlens ->
	    println(F,"  <Object type=\"thinlenscamera\">"),
	    println(F,"    <Parameter name=\"fstop\" value=\"~s\"/>", 
		    [format(pget(fstop,Attr))]),
	    println(F,"    <Parameter name=\"focallength\" value=\"~s\"/>", 
		    [format(pget(focallen,Attr))]),
	    case pget(autofocus, Attr) of
		true ->
		    println(F,"    <Parameter name=\"autofocus\" value=\"~s ~s\"/>", 
			    [format(pget(autoposx,Attr)),format(pget(autoposy,Attr))]);
		false ->
		    println(F,"    <Parameter name=\"focaldistance\" value=\"~s\"/>", 
			    [format(pget(focaldist,Attr))])
	    end
    end,
%    println(F,"  <Parameter name=\"hfov\" value=\"~s\"/>", [format(Fov)]),
    println(F,"    <Transform>"),

%%%%%%%%%%% Almost Same as wings %%%%%%%%%%%%
%%% After trial and error, This works..
    println(F,"      <Translation value=\"~s ~s ~s\"/>", 
	    [format(-PanX*Scale),format(-PanY*Scale),format(Dist*Scale)]),
    println(F,"      <Rotation angle=\"~s\" axis=\"1.0 0.0 0.0\"/>", 
	    [format(-El)]),
    println(F,"      <Rotation angle=\"~s\" axis=\"0.0 1.0 0.0\"/>", 
	    [format(-Az)]),
    println(F,"      <Translation value=\"~s ~s ~s\"/>", 
	    vector_to_list(Origin,-Scale)),
    println(F,"    </Transform>"),
    println(F,"  </Object>"),
    ok.

% rotate_vec(V1,V2) ->
%     ACos = e3d_vec:dot(V1,V2),
%     Dir = math:acos(ACos)*180/math:pi(),
%     case e3d_vec:norm(e3d_vec:cross(V1,V2)) of
% 	{0.0,0.0,0.0} ->
% 	    {Dir, {0.0,1.0,0.0}};
% 	Cross ->
% 	    {Dir, Cross}
%     end.

vector_to_list({X,Y,Z}) ->
    [format(X),format(Y),format(Z)].
vector_to_list({X,Y,Z},Scale) ->
    [format(X*Scale),format(Y*Scale),format(Z*Scale)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Render

render(Renderer, Attr, #files{dir=Dir, image=Image, scene=Scene}) ->
    process_flag(trap_exit, true),
    Options = pget(options,Attr,?DEF_OPTIONS),
    LoadImage = pget(load_image,Attr,?DEF_LOAD_IMAGE),
    Width = pget(width,Attr),
    Height = pget(height,Attr),

    Cmd = uquote(Renderer)++" "++ uquote(filename:basename(Scene)) ++ 
	" -o " ++ uquote(Image) ++ " -w " ++ format(Width) ++ 
	" -h " ++ format(Height) ++ " " ++ Options,
    PortOpts = [{cd,Dir},eof,exit_status,stderr_to_stdout],
%%%     PortOpts = [{line,1},{cd,Dirname},eof,exit_status,stderr_to_stdout],
    io:format("Rendering Job started ~p:~n>~s~n", [self(),Cmd]),
    case catch open_port({spawn,Cmd}, PortOpts) of
	Port when port(Port) ->
	    Result = render_job(Port),
	    render_done(Result, LoadImage);
	{'EXIT',Reason} ->
	    render_done({error,Reason}, LoadImage)
    end.

render_done(ExitStatus, LoadImage) ->
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
	    case Tag of eol -> io:nl(); noeol -> ok end,
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

%%% Noisy file output functions. Fail if anything goes wrong.
%%%

open(Filename, export) ->
    case file:open(Filename, [write,raw,delayed_write]) of
	{ok, F} ->
	    F;
	Error ->
	    erlang:fault(Error, [Filename, export])
    end.

%% println(F) ->
%%     println(F, "").

%% print(F, DeepString) ->
%%     case file:write(F, DeepString) of
%%  ok ->
%%      ok;
%%  Error ->
%%      erlang:fault(Error, [F,DeepString])
%%     end.

println(F, DeepString) ->
    case file:write(F, [DeepString,io_lib:nl()]) of
	ok ->    ok;
	Error -> erlang:fault(Error, [F,DeepString])
    end.

%% print(F, Format, Args) ->
%%     case file:write(F, io_lib:format(Format, Args)) of
%%  	ok ->    ok;
%%  	Error -> erlang:fault(Error, [F,Format,Args])
%%     end.

println(F, Format, Args) ->
    case file:write(F, [io_lib:format(Format, Args),io_lib:nl()]) of
	ok ->    ok;
	Error -> erlang:fault(Error, [F,Format,Args])
    end.

close(F) ->
    case file:close(F) of
	ok ->    ok;
	Error -> erlang:fault(Error, [F])
    end.

%% Convert certain terms to printable strings in a
%% hopefully efficient way.

format(F) when is_float(F) ->
    I = abs(trunc(F)),
    D = abs(F) - float(I),
    if F < 0 ->   [$-,integer_to_list(I)|format_decimals(D)];
       true ->    [integer_to_list(I)|format_decimals(D)]
    end;
format(I) when is_integer(I) ->    integer_to_list(I);
format(true) ->     "true";
format(false) ->    "false";
format(A) when is_atom(A) ->    atom_to_list(A);
format(L) when is_list(L) ->    L.

format_decimals(F) when float(F), F >= 0.0 ->
    format_decimals_1(F).

format_decimals_1(0.0) ->    ".0";
format_decimals_1(F) when is_float(F) ->
    G = 10.0 * F,
    I = trunc(G),
    D = G - float(I),
    [$.,(I+$0)|format_decimals_2(D)].

format_decimals_2(0.0) ->    [];
format_decimals_2(F) when is_float(F) ->
    G = 100.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->   [$0,(I+$0)|format_decimals_3(D)];
       true ->     [integer_to_list(I)|format_decimals_3(D)]
    end.

format_decimals_3(0.0) ->    [];
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

format_decimals_4(0.0) ->    [];
format_decimals_4(F) when is_float(F) ->
    G = 10000.0 * F,
    I = trunc(G),
    if I < 100 ->
	    if I < 10 ->   [$0,$0,$0,(I+$0)];
	       true ->     [$0,$0|integer_to_list(I)]
	    end;
       true ->
	    if I < 1000 -> [$0|integer_to_list(I)];
	       true ->     integer_to_list(I)
	    end
    end.

%% Set and get preference variables saved in the .wings file for this module

set_pref(Attr) ->
    wpa:pref_set(?MODULE, Attr).
get_pref(Key, Def) ->
    case wpa:pref_get(?MODULE, {?TAG,Key}) of
	undefined ->
	    wpa:pref_get(?MODULE, Key, Def);
	Else ->
	    Else
    end.
pget(Key,Attr) ->
    proplists:get_value({?TAG,Key}, Attr).
pget(Key,Attr,Def) ->
    proplists:get_value({?TAG,Key}, Attr, Def).

%% Set and get global variables (in the process dictionary)
%% per wings session for this module.

set_var(Name, undefined) ->
    erase_var(Name);
set_var(Name, Value) ->
    put({?MODULE,{?TAG,Name}}, Value).
get_var(Name) ->
    get({?MODULE,{?TAG,Name}}).
erase_var(Name) ->
    erase({?MODULE,{?TAG,Name}}).

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
%% nor in Toxic result .tga filenames. They might only work in
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

%%% %% {lists:filter(Pred, List),lists:filter(fun(X) -> not Pred(X) end,List)}
%%% filter2(Pred, List) -> filter2_1(Pred, List, [], []).
%%% %%
%%% filter2_1(_Pred, [], True, False) ->
%%%     {reverse(True),reverse(False)};
%%% filter2_1(Pred, [H|T], True, False) ->
%%%     case Pred(H) of
%%%  true -> filter2_1(Pred, T, [H|True], False);
%%%  false -> filter2_1(Pred, T, True, [H|False])
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

help(title, export_dialog) ->
    "Toxic Export Properties: ";
help(text, export_dialog) ->
    [<<"Toxic: see http://toxicengine.sourceforge.net">>,
     <<"Toxic export uses several files, YourScene.xml consists of "
      "the shaders (e.g. materials) and objects (with references to the "
      "actual meshes). Lights and the camera is also defined as scene objects. "
      "Render settings are defined in YourScene.setting.xml, here are all the " 
      "render specific information defined, i.e. most of the settings in this "
      "dialog are defined here. The actual meshes is stored in YourScene.objects.obj "
      "file, that means that you don't have to export the the meshes for every "
      "rendering, i.e. if you only change something in the materials or in this "
      "dialog you don't have to export the meshes again." >>,
     <<"Global Scale lets you specify a scale parameter from wings units to meters. "
      "This is necessary get correct lighting in your scene. So if one wings unit "
      "represents one centimeter on your model set Global Scale to 0.01, or if "
      "one wings unit is two meters, set Global Scale to 2">>,
     <<"Camera lets you choose between two different type of cameras."
      "The pinhole camera is the simplest one. Since it has infinite "
      "depth of field, objects can't be out of focus and thus never appear blurred. "
      "The thin lens camera is slightly more complex (and much more plausible) "
      "than the pinhole camera. It will produce depth of field according to the "
      "fstop, focaldistance and focallength  parameters. Out of focus objects "
      "will appear blurred.\n"
      "Fstop: this is the aperture number or f-stop number.\n"
      "     Lens diameter is computed as follow: \n"
      "     lensdiameter = focallength / fstop.\n "      
      "Focal length: focal length of the camera, expressed in meters.\n"

      "Additionally, one of the two following parameters must be defined: "
      "focaldistance or autofocus. Focal distance of the camera, expressed in meters. "
      "The focal distance is the distance along the view direction at which "
      "objects will be in focus." 
      "Autofocus is a point, in the image plane, which will be in focus. "
      "Bottom left corner of the image plane is at (-0.5, -0.5), and "
      "upper right corner is at (0.5, 0.5), regardless of the image resolution. ">>,
     <<"Pixel Sampling specifies how antialising is done">>,
     <<"Rendering specifies which kind of lighting method should be used "
      "There are four types lighting methods: direct lighting, indirect lighting, "
      "specular reflections and caustics. These methods form the global " 
      "illumination (GI) and each method can be individually enabled or disabled. ">>,
     <<"Output specifies the rendered image width, height and gamma">>,
     <<"Rendering Job lets you specify additional command line arguments "
      "toxic">>
    ];

help(title, {material_dialog,shaders}) ->
    "Toxic Material Properties: ";
help(text, {material_dialog,shaders}) ->
    [<<"The BRDF characterizes how light is reflected by the surface.">>,
     <<" Lambertian is a perfectly diffuse surface and currently the only "
      "other option is a perfectly specular surface">>,
     <<" The diffuse color is used in toxic as the reflectance color"
      " for all different types of BRDF."
      " If the material has an diffuse texture it will replace the reflectance color">>,
     <<"If emission is enabled, EDF will be set to lambertian, "
      "Radiant Exitance will be multiplied with the emission color, and"
      " it is expressed in W.m^-2">>];
%%
help(title, light_dialog) ->
    "Toxic Light Properties";
help(text, light_dialog) ->
    [<<"Toxic only has one light type, pointlight, it produces true hard shadows."
      " The diffuse color is multiplied with the power value, "
      "to get the emission power in Watts.">>,
     <<"All Wings3D lights (except area lights) uses the Toxic point light.",
      " Area lights are currently not supported (or exported). Instead you should use"
      " the emission setting in material dialog to create area lights">>].


