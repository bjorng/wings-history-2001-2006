%%
%%  wings_material.erl --
%%
%%     This module manages the face materials (i.e. colors and textures).
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_material.erl,v 1.73 2003/01/21 06:43:56 bjorng Exp $
%%

-module(wings_material).
-export([new/1,sub_menu/2,command/2,
	 color/4,default/0,add_materials/2,update_image/4,
	 used_materials/1,apply_material/2,
	 is_transparent/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").

-import(lists, [map/2,foreach/2,sort/1,foldl/3,reverse/1,
		keyreplace/4,keydelete/3,keysearch/3,flatten/1]).

new(_) ->
    wings_ask:ask("New Material",
		  [{"Material Name","New Material"}],
		  fun([Name]) ->
			  Action = {action,{material,{new,Name}}},
			  wings_wm:send_after_redraw(geom, Action),
			  ignore
		  end).

sub_menu(select, St) ->
    {"Material",{material,material_list(St)}}.

command({material,{new,Name0}}, #st{mat=Mtab}=St) ->
    Name1 = list_to_atom(Name0),
    case gb_trees:is_defined(Name1, Mtab) of
	true ->
	    Names = [atom_to_list(N) || N <- gb_trees:keys(Mtab)],
	    Name = list_to_atom(wings_util:unique_name(Name0, Names)),
	    new_material(Name, St);
	false ->
	    new_material(Name1, St)
    end;
command({material,{edit,Mat}}, St) ->
    edit(Mat, St);
command({material,{assign,Mat}}, St) ->
    set_material(Mat, St);
command({select,{material,Mat}}, St) ->
    wings_sel:make(fun(Face, #we{fs=Ftab}) ->
			   #face{mat=M} = gb_trees:get(Face, Ftab),
			   M =:= Mat
		   end, face, St).

new_material(Name, St0) ->
    Mat = make_default({1.0,1.0,1.0}, 1.0),
    St = add(Name, Mat, St0),
    edit(Name, St).

material_list(#st{mat=Mat0}) ->
    map(fun({Id,Mp}) ->
		OpenGL = prop_get(opengl, Mp),
		Color = prop_get(diffuse, OpenGL),
		{atom_to_list(Id),Id,[],[{color,Color}]}
	end, gb_trees:to_list(Mat0)).

set_material(Mat, #st{selmode=face}=St) ->
    wings_sel:map(fun(Faces, We) ->
			  set_material_1(Mat, gb_sets:to_list(Faces), We)
		  end, St);
set_material(Mat, #st{selmode=body}=St) ->
    wings_sel:map(fun(_, #we{fs=Ftab}=We) ->
			  set_material_1(Mat, gb_trees:keys(Ftab), We)
		  end, St);
set_material(_, St) -> St.

set_material_1(Mat, Faces, #we{fs=Ftab0}=We) ->
    Ftab = foldl(fun(Face, Ft) ->
			 Rec = gb_trees:get(Face, Ft),
			 gb_trees:update(Face, Rec#face{mat=Mat}, Ft)
		 end, Ftab0, Faces),
    We#we{fs=Ftab}.

default() ->
    M = [{default,make_default({1.0,1.0,1.0}, 1.0)},
	 {'_hole_',make_default({0.0,0.0,0.9}, 0.50)}],
    gb_trees:from_orddict(sort(M)).

make_default({R,G,B}, Opacity) ->
    Color = {R,G,B,Opacity},
    White = {1.0,1.0,1.0,1.0},
    Mat = [{opengl,[{diffuse,Color},{ambient,Color},{specular,White},
		    {emission,{0.0,0.0,0.0,0.0}},{shininess,1.0}]},
	   {maps,[]}],
    sort([{K,sort(L)} || {K,L} <- Mat]).

update_image(MatName, MapType, Image, #st{mat=Mtab}) ->
    Mat = gb_trees:get(MatName, Mtab),
    Maps = prop_get(maps, Mat, []),
    {value,{MapType,ImageId}} = keysearch(MapType, 1, Maps),
    wings_image:update(ImageId, Image).

add_materials(Ms, St) ->
    add_materials(Ms, St, []).

add_materials([{Name,Mat0}|Ms], St0, NewNames) ->
    Mat1 = add_defaults(Mat0),
    Maps = load_maps(prop_get(maps, Mat1, [])),
    Mat = keyreplace(maps, 1, Mat1, {maps,Maps}),
    case add(Name, Mat, St0) of
	#st{}=St ->
	    add_materials(Ms, St, NewNames);
	{#st{}=St,NewName} ->
	    add_materials(Ms, St, [{Name,NewName}|NewNames])
    end;
add_materials([], St, NewNames) -> {St,NewNames}.

add_defaults([]) ->
    add_defaults([{opengl,[]},{maps,[]}]);
add_defaults(Props0) ->
    OpenGL0 = prop_get(opengl, Props0, []),
    OpenGL = add_defaults_1(OpenGL0),
    Props = [{opengl,OpenGL}|lists:keydelete(opengl, 1, Props0)],
    case prop_get(maps, Props) of
	undefined -> [{maps,[]}|Props];
	_ -> Props
    end.

add_defaults_1(P) ->
    Def = {1.0,1.0,1.0,1.0},
    [{diffuse,norm(prop_get(diffuse, P, Def))},
     {ambient,norm(prop_get(ambient, P, Def))},
     {specular,norm(prop_get(specular, P, Def))},
     {emission,norm(prop_get(emission, P, {0.0,0.0,0.0,0.0}))},
     {shininess,prop_get(shininess, P, 1.0)}].

norm({_,_,_,_}=Color) -> Color;
norm({R,G,B}) -> {R,G,B,1.0}.
    
load_maps([{Key,Filename}|T]) when is_list(Filename) ->
    [{Key,load_map(Filename)}|load_maps(T)];
load_maps([{Key,{W,H,Bits}}|T]) ->
    E3dImage = #e3d_image{type=r8g8b8,order=lower_left,
			  width=W,height=H,image=Bits},
    Id = wings_image:new(atom_to_list(Key)++" texture", E3dImage),
    [{Key,Id}|load_maps(T)];
load_maps([]) -> [].
    
load_map(MapName) ->
    case catch load_map_1(MapName) of
	none -> none;
	{'EXIT',R} ->
	    io:format("~P\n", [R,20]),
	    none;
	Im when is_integer(Im)-> Im
    end.

load_map_1(none) -> none;
load_map_1(File0) ->
    File = filename:absname(File0, wings_pref:get_value(current_directory)),
    Ps = [{filename,File},{type,r8g8b8},{order,lower_left},{alignment,1}],
    case wpa:image_read(Ps) of
	#e3d_image{}=Im ->
	    wings_image:new("ImportedImage", Im);
	{error,Error} ->
	    io:format("Failed to load \"~s\": ~s\n",
		      [File,file:format_error(Error)]),
	    none
    end.
    
add('_hole_'=Name, Mat, #st{mat=MatTab}=St) ->
    case gb_trees:is_defined(Name, MatTab) of
	true -> St;
	false -> St#st{mat=gb_trees:insert(Name, Mat, MatTab)}
    end;
add(Name, Mat0, #st{mat=MatTab}=St) ->
    Mat = sort([{K,sort(L)} || {K,L} <- Mat0]),
    case gb_trees:lookup(Name, MatTab) of
	none ->
	    St#st{mat=gb_trees:insert(Name, Mat, MatTab)};
	{value,Mat} -> St;
	{value,_} ->
	    NewName = new_name(atom_to_list(Name), MatTab, 0),
	    {add(NewName, Mat, St),NewName}
    end.

new_name(Name0, Tab, I) ->
    Name = list_to_atom(Name0 ++ "_" ++ integer_to_list(I)),
    case gb_trees:is_defined(Name, Tab) of
	false -> Name;
	true -> new_name(Name0, Tab, I+1)
    end.

apply_material(Name, Mtab) when is_atom(Name) ->
    Mat = gb_trees:get(Name, Mtab),
    OpenGL = prop_get(opengl, Mat),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_DIFFUSE, prop_get(diffuse, OpenGL)), 
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_AMBIENT, prop_get(ambient, OpenGL)),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_SPECULAR, prop_get(specular, OpenGL)),
    Shine = prop_get(shininess, OpenGL)*128,
    gl:materialf(?GL_FRONT_AND_BACK, ?GL_SHININESS, Shine),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, prop_get(emission, OpenGL)),
    Maps = prop_get(maps, Mat, []),
    case prop_get(diffuse, Maps, none) of
	none -> gl:disable(?GL_TEXTURE_2D);
	DiffuseImage -> apply_texture(DiffuseImage)
    end.

apply_texture(Image) ->
    case wings_image:txid(Image) of
	none -> ok;
	TxId ->
	    gl:enable(?GL_TEXTURE_2D),
	    gl:texEnvi(?GL_TEXTURE_ENV,
		       ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
	    gl:bindTexture(?GL_TEXTURE_2D, TxId),
	    gl:texParameteri(?GL_TEXTURE_2D,
			     ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
	    gl:texParameteri(?GL_TEXTURE_2D,
			     ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
	    gl:texParameteri(?GL_TEXTURE_2D,
			     ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
	    gl:texParameteri(?GL_TEXTURE_2D,
			     ?GL_TEXTURE_WRAP_T, ?GL_REPEAT)
    end.

%%% Returns the materials used.

used_materials(#st{shapes=Shs,mat=Mat0}) ->
    Used0 = foldl(fun(#we{fs=Ftab}, A) ->
			  used_materials_1(Ftab, A)
		  end, gb_sets:empty(), gb_trees:values(Shs)),
    Used1 = sofs:from_external(gb_sets:to_list(Used0), [name]),
    Mat = sofs:relation(gb_trees:to_list(Mat0), [{name,data}]),
    Used = sofs:restriction(Mat, Used1),
    sofs:to_external(Used).

used_materials_1(Ftab, Acc) ->
    foldl(fun(#face{mat=[_|_]=Mat}, A) ->
		  gb_sets:union(A, gb_sets:from_list(Mat));
	     (#face{mat=Mat}, A) ->
		  gb_sets:add(Mat, A)
	  end, Acc, gb_trees:values(Ftab)).

is_transparent(Name, Mtab) ->
    Mat = gb_trees:get(Name, Mtab),
    OpenGL = prop_get(opengl, Mat),
    foldl(fun(_, true) -> true;
	     ({emission,_}, _) -> false;
	     ({_,{_,_,_,1.0}}, _) -> false;
	     ({_,{_,_,_,_}}, _) -> true;
	     (_, _) -> false
	  end, false, OpenGL).

%%% The material editor.

-define(PREVIEW_SIZE, 100).

edit(Name, #st{mat=Mtab0}=St) ->
    Mat0 = gb_trees:get(Name, Mtab0),
    OpenGL0 = prop_get(opengl, Mat0),
    {Diff0,Opacity0} = ask_prop_get(diffuse, OpenGL0),
    {Amb0,_} = ask_prop_get(ambient, OpenGL0),
    {Spec0,_} = ask_prop_get(specular, OpenGL0),
    Shine0 = prop_get(shininess, OpenGL0),
    {Emiss0,_} = ask_prop_get(emission, OpenGL0),
    Maps0 = show_maps(Mat0),
    Qs1 = [{vframe,
	    [
	     {hframe, 
	      [{custom,?PREVIEW_SIZE,?PREVIEW_SIZE+5,fun mat_preview/5},
	       {vframe,
		[{label,"Diffuse"},
		 {label,"Ambient"},
		 {label,"Specular"},
		 {label,"Emission"}]
	       },
	       {vframe,
		[{color,Diff0,[{key,diffuse}]},
		 {color,Amb0,[{key,ambient}]},
		 {color,Spec0,[{key,specular}]},
		 {color,Emiss0,[{key,emission}]}
		]}]},
	     {hframe, [{vframe, [{label,"Shininess"},
				 {label,"Opacity"}]},
		       {vframe, [{slider,{text,Shine0,
					  [{range,{0.0,1.0}},
					   {key,shininess}]}},
				 {slider,{text,Opacity0,
					  [{range,{0.0,1.0}},
					   {key,opacity}]}}]}]
	     }]
	   }],
    Qs = wings_plugin:dialog({material_editor_setup,Name,Mat0}, Qs1),
    Ask = fun([{diffuse,Diff},{ambient,Amb},{specular,Spec},
	       {emission,Emiss},{shininess,Shine},{opacity,Opacity}|More]) ->
		  OpenGL = [ask_prop_put(diffuse, Diff, Opacity),
			    ask_prop_put(ambient, Amb, Opacity),
			    ask_prop_put(specular, Spec, Opacity),
			    ask_prop_put(emission, Emiss, Opacity),
			    {shininess,Shine}],
		  Mat1 = keyreplace(opengl, 1, Mat0, {opengl,OpenGL}),
		  Mat = plugin_results(Name, More, Mat1),
		  Mtab = gb_trees:update(Name, Mat, Mtab0),
		  wings_draw_util:map(fun invalidate_dlists/2, Name),
		  St#st{mat=Mtab}
	  end,
    wings_ask:dialog("Material Properties: "++atom_to_list(Name), Qs, Ask).

plugin_results(_, [], Mat) -> Mat;
plugin_results(Name, Res0, Mat0) ->
    {Mat,Res} = wings_plugin:dialog({material_editor_result,Name,Mat0}, Res0),
    plugin_results(Name, Res, Mat).

show_maps(Mat) ->
    case prop_get(maps, Mat) of
	[] -> [];
	Maps -> [separator|[show_map(M) || M <- sort(Maps)]]
    end.

show_map({Type,Image}) ->
    #e3d_image{width=W,height=H} = wings_image:info(Image),
    Label = flatten(io_lib:format("~p ~px~p", [Type,W,H])),
    {hframe,
     [{label,Label}]}.

invalidate_dlists(#dlo{src_we=#we{fs=Ftab}}=D, Name) ->
    case material_used(gb_trees:values(Ftab), Name) of
	false -> {D,Name};
	true -> {D#dlo{work=none,vs=none,smooth=none,smoothed=none},Name}
    end.

material_used([#face{mat=Name}|_], Name) -> true;
material_used([_|T], Name) -> material_used(T, Name);
material_used([], _) -> false.

ask_prop_get(Key, Props) ->
    {R,G,B,Alpha} = prop_get(Key, Props),
    {{R,G,B},Alpha}.

ask_prop_put(specular=Key, {R,G,B}, _) ->
    {Key,{R,G,B,1.0}};
ask_prop_put(Key, {R,G,B}, Opacity) ->
    {Key,{R,G,B,Opacity}}.
    
mat_preview(X, Y, _W, _H, Common) ->
    wings_io:border(X, Y, ?PREVIEW_SIZE, ?PREVIEW_SIZE, ?PANE_COLOR),
    MM = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    PM = gl:getDoublev(?GL_PROJECTION_MATRIX),
    ViewPort = wings_wm:viewport(),
    {true,Ox,Oy,_} = glu:project(X, Y+?PREVIEW_SIZE, 0, MM, PM, ViewPort),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:viewport(trunc(Ox), trunc(Oy), ?PREVIEW_SIZE, ?PREVIEW_SIZE),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:perspective(60.0, 1, 0.01, 256.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:translatef(0.0, 0.0, -2.0),
    wings_light:modeling_lights(camera,mat_preview),
    gl:shadeModel(?GL_SMOOTH),
    Alpha = gb_trees:get(opacity, Common),
    Amb = preview_mat(ambient, Common, Alpha),
    Diff = preview_mat(diffuse, Common, Alpha),
    Spec = preview_mat(specular, Common, Alpha),
    Shine = gb_trees:get(shininess, Common),
    gl:materialf(?GL_FRONT, ?GL_SHININESS, Shine*128.0),
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT, Amb),
    gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, Diff),
    gl:materialfv(?GL_FRONT, ?GL_SPECULAR, Spec),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_BLEND),
    gl:enable(?GL_CULL_FACE),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, 0.9, 50, 50),
    glu:deleteQuadric(Obj),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_BLEND),
    gl:shadeModel(?GL_FLAT),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:popAttrib().

preview_mat(Key, Colors, Alpha) ->
    {R,G,B} = gb_trees:get(Key, Colors),
    {R,G,B,Alpha}.
    
%%% Return color in texture for the given UV coordinates.

color(Face, {U,V}, #we{fs=Ftab}, #st{mat=Mtab}) ->
    #face{mat=Name} = gb_trees:get(Face, Ftab),
    Props = gb_trees:get(Name, Mtab),
    Maps = prop_get(maps, Props),
    case prop_get(diffuse, Maps, none) of
	none ->
	    OpenGL = prop_get(opengl, Props),
	    {R,G,B,_} = prop_get(diffuse, OpenGL),
	    wings_color:share({R,G,B});
	DiffMap ->
	    color_1(U, V, wings_image:info(DiffMap))
    end;
color(_Face, {_,_,_}=RGB, _We, _St) -> RGB.

color_1(U0, V0, #e3d_image{width=W,height=H,image=Bits}) ->
    U = (((round(U0*W) rem W) + W) rem W),
    V = ((round(V0*H) rem H) + H) rem H,
    Pos = V*W*3 + U*3,
    <<_:Pos/binary,R:8,G:8,B:8,_/binary>> = Bits,
    wings_util:share(R/255, G/255, B/255).
    
prop_get(Key, Props) ->
    proplists:get_value(Key, Props).

prop_get(Key, Props, Def) ->
    proplists:get_value(Key, Props, Def).
