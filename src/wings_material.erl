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
%%     $Id: wings_material.erl,v 1.90 2003/04/21 10:16:58 bjorng Exp $
%%

-module(wings_material).
-export([material_menu/1,command/2,new/1,color/4,default/0,
	 mat_faces/2,add_materials/2,update_image/4,used_images/1,
	 get/2,get_all/1,delete_face/2,delete_faces/2,cleanup/1,
	 assign/3,assign_materials/2,
	 used_materials/1,used_materials_we/1,
	 apply_material/2,is_transparent/2,
	 renumber/2,merge/1]).

-ifdef(DEBUG).
-export([validate/1]).
-endif.

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").

-import(lists, [map/2,foreach/2,sort/1,foldl/3,reverse/1,reverse/2,
		keyreplace/4,keydelete/3,keysearch/3,flatten/1]).

material_menu(St) ->
    [{basic,{"Material",
	     {material,
	      [{"New...",new,
		"Create a new material and assign to selected faces"},
	       separator|mat_list(St)]}}},
     {advanced,{"Material",{material,material_fun(St)}}}].

material_fun(St) ->
    fun(help, _Ns) ->
	    {"Assign existing material to selection",[],
	     "Create and assign new material"};
       (1, _Ns) ->
	    mat_list(St);
       (3, _) ->
	    {material,new};
       (_, _) -> ignore
    end.

mat_list(#st{mat=Mtab}) ->
    mat_list_1(gb_trees:to_list(Mtab), []).

mat_list_1([{Name,Ps}|Ms], Acc) ->
    OpenGL = prop_get(opengl, Ps, []),
    Diff = prop_get(diffuse, OpenGL),
    Menu = {atom_to_list(Name),{'VALUE',{assign,Name}},[],[{color,Diff}]},
    mat_list_1(Ms, [Menu|Acc]);
mat_list_1([], Acc) -> reverse(Acc).

new(_) ->
    new_1(new).

new_1(Act) ->
    wings_ask:ask("New Material",
		  [{"Material Name","New Material"}],
		  fun([Name]) ->
			  Action = {action,{material,{Act,Name}}},
			  wings_wm:send_after_redraw(geom, Action),
			  ignore
		  end).

command(new, _) ->
    new_1(assign_new);
command({assign_new,Name}, St) ->
    new_material(Name, true, St);
command({new,Name}, St) ->
    new_material(Name, false, St);
command({edit,Mat}, St) ->
    edit(list_to_atom(Mat), false, St);
command({assign,Mat}, St) when is_atom(Mat) ->
    set_material(Mat, St);
command({assign,Mat}, St) ->
    set_material(list_to_atom(Mat), St);
command({select,[Mat]}, St) ->
    select_material(list_to_atom(Mat), St);
command({duplicate,MatList}, St) ->
    duplicate_material(MatList, St);
command({delete,MatList}, St) ->
    delete_material(MatList, St);
command({rename,MatList0}, St) ->
    case MatList0 -- ["default","_hole_"] of
	[] -> St;
	MatList -> rename(MatList, St)
    end.

new_material(Name0, Assign, #st{mat=Mtab}=St) ->
    Name1 = list_to_atom(Name0),
    case gb_trees:is_defined(Name1, Mtab) of
	true ->
	    Names = [atom_to_list(N) || N <- gb_trees:keys(Mtab)],
	    Name = list_to_atom(wings_util:unique_name(Name0, Names)),
	    new_material_1(Name, Assign, St);
	false ->
	    new_material_1(Name1, Assign, St)
    end.

new_material_1(Name, Assign, St0) ->
    Mat = make_default({1.0,1.0,1.0}, 1.0),
    St = add(Name, Mat, St0),
    edit(Name, Assign, St).

duplicate_material([M0|Ms], #st{mat=Mat}=St0) ->
    M1 = list_to_atom(M0),
    MatPs = gb_trees:get(M1, Mat),
    M = new_name(M0, Mat),
    St = add(M, MatPs, St0),
    duplicate_material(Ms, St);
duplicate_material([], St) -> St.

delete_material(["default"|Ms], St) ->
    delete_material(Ms, St);
delete_material(["_hole_"|Ms], St) ->
    delete_material(Ms, St);
delete_material([M0|Ms], #st{mat=Mat0}=St0) ->
    M = list_to_atom(M0),
    Mat = gb_trees:delete(M, Mat0),
    St = reassign_material(M, default, St0),
    delete_material(Ms, St#st{mat=Mat});
delete_material([], St) ->
    {save_state,St}.

rename(Mats, St) ->
    Qs = rename_qs(Mats),
    wings_ask:dialog("Rename", Qs,
		     fun(NewNames) ->
			     rename_1(NewNames, St, [])
		     end).

rename_1([{Old,New}|Ms], #st{mat=Mat0}=St, Acc) ->
    MatPs = gb_trees:get(Old, Mat0),
    Mat = gb_trees:delete(Old, Mat0),
    rename_1(Ms, St#st{mat=Mat}, [{Old,list_to_atom(New),MatPs}|Acc]);
rename_1([], St, Acc) -> rename_2(Acc, St).

rename_2([{Old,New0,MatPs}|Ms], St0) ->
    case add(New0, MatPs, St0) of
	{St1,New} -> ok;
	#st{}=St1 -> New = New0
    end,
    St = reassign_material(Old, New, St1),
    rename_2(Ms, St);
rename_2([], St) -> St.

rename_qs(Ms) ->
    OldNames = [{label,M} || M <- Ms],
    TextFields = [{text,M,[{key,list_to_atom(M)}]} || M <- Ms],
    [{hframe,
      [{vframe,OldNames},
       {vframe,TextFields}]}].

reassign_material(Old, New, St0) ->
    %% It would be tempting to call select_material/2 here instead of
    %% make_fake_selection/2, but we must make sure that even invisible
    %% and locked objects gets selected.
    case make_fake_selection(Old, St0) of
	#st{sel=[]} -> St0;
	St1 ->
	    #st{shapes=Shs,mat=Mat} = set_material(New, St1),
	    St0#st{shapes=Shs,mat=Mat}
    end.

make_fake_selection(OldMat, #st{shapes=Shapes}=St) ->
    Sel0 = gb_trees:values(Shapes),
    Sel = make_fake_selection_1(Sel0, OldMat),
    St#st{selmode=face,sel=Sel}.

make_fake_selection_1([#we{id=Id,fs=Ftab,mat=OldMat}|Shs], OldMat) ->
    [{Id,gb_trees:keys(Ftab)}|make_fake_selection_1(Shs, OldMat)];
make_fake_selection_1([#we{mat=Atom}|Shs], OldMat) when is_atom(Atom) ->
    make_fake_selection_1(Shs, OldMat);
make_fake_selection_1([#we{id=Id,mat=MatTab}|Shs], OldMat) ->
    case [Face || {Face,Mat} <- MatTab, Mat =:= OldMat] of
	[] -> make_fake_selection_1(Shs, OldMat);
	Sel -> [{Id,gb_sets:from_ordset(Sel)}|make_fake_selection_1(Shs, OldMat)]
    end;
make_fake_selection_1([], _) -> [].

select_material(Mat, St) ->
    %% XXX Works but is slow.
    wings_sel:make(fun(_, #we{mat=AtomMat}) when is_atom(AtomMat) ->
			   Mat =:= AtomMat;
		      (Face, #we{mat=MatTab}) ->
			   case keysearch(Face, 1, MatTab) of
			       false -> false;
			       {value,{_,Mat}} -> true;
			       {value,_} -> false
			   end
		   end, face, St).

set_material(Mat, #st{selmode=face}=St) ->
    wings_sel:map(fun(Faces, We) ->
			  assign(Mat, Faces, We)
		  end, St);
set_material(Mat, #st{selmode=body}=St) ->
    wings_sel:map(fun(_, #we{fs=Ftab}=We) ->
			  assign(Mat, gb_trees:keys(Ftab), We)
		  end, St);
set_material(_, St) -> St.

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
    case load_map(Filename) of
	none -> load_maps(T);
	Map -> [{Key,Map}|load_maps(T)]
    end;
load_maps([{Key,{W,H,Bits}}|T]) ->
    E3dImage = #e3d_image{type=r8g8b8,order=lower_left,
			  width=W,height=H,image=Bits},
    Id = wings_image:new(atom_to_list(Key), E3dImage),
    [{Key,Id}|load_maps(T)];
load_maps([{Key,#e3d_image{}=E3dImage}|T]) ->
    Id = wings_image:new(atom_to_list(Key), E3dImage),
    [{Key,Id}|load_maps(T)];
load_maps([{_,none}|T]) ->
    load_maps(T);
load_maps([{_,Id}=Map|T]) when is_integer(Id) ->
    [Map|load_maps(T)];
load_maps([]) -> [].
    
load_map(MapName) ->
    case catch load_map_1(MapName) of
	none -> none;
	{'EXIT',R} ->
	    io:format("~P\n", [R,20]),
	    none;
	Im when is_integer(Im) -> Im
    end.

load_map_1(none) -> none;
load_map_1(File0) ->
    File = filename:absname(File0, wings_pref:get_value(current_directory)),
    Ps = [{filename,File},{order,lower_left},{alignment,1}],
    case wpa:image_read(Ps) of
	#e3d_image{}=Im ->
	    Name = filename:rootname(filename:basename(File)),
	    wings_image:new(Name, Im#e3d_image{filename=File});
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
	    NewName = new_name(atom_to_list(Name), MatTab),
	    {add(NewName, Mat, St),NewName}
    end.

new_name(Name0, Tab) ->
    Names = [atom_to_list(N) || N <- gb_trees:keys(Tab)],
    Name = wings_util:unique_name(Name0, Names),
    list_to_atom(Name).

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
    apply_texture(prop_get(diffuse, Maps, none)).

apply_texture(none) -> no_texture();
apply_texture(Image) ->
    case wings_image:txid(Image) of
	none ->
	    %% Image was deleted.
	    no_texture();
	TxId ->
	    gl:enable(?GL_TEXTURE_2D),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
	    gl:bindTexture(?GL_TEXTURE_2D, TxId),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
	    case wings_image:info(Image) of
		#e3d_image{bytes_pp=4} ->
		    gl:enable(?GL_ALPHA_TEST),
		    gl:alphaFunc(?GL_GREATER, 0.5);
		#e3d_image{type=a8} ->
		    gl:enable(?GL_ALPHA_TEST),
		    gl:alphaFunc(?GL_GREATER, 0.5);
		_ -> 
		    gl:disable(?GL_ALPHA_TEST)
	    end
    end.

no_texture() ->
    gl:disable(?GL_TEXTURE_2D),
    gl:disable(?GL_ALPHA_TEST).

%% Return the materials used by the objects in the scene.

used_materials(#st{shapes=Shs,mat=Mat0}) ->
    Used0 = foldl(fun(We, A) ->
			  [used_materials_we(We)|A]
		  end, [], gb_trees:values(Shs)),
    Used1 = gb_sets:union(Used0),
    Used2 = sofs:from_external(gb_sets:to_list(Used1), [name]),
    Mat = sofs:relation(gb_trees:to_list(Mat0), [{name,data}]),
    Used = sofs:restriction(Mat, Used2),
    sofs:to_external(Used).

used_materials_we(#we{mat=Mat}) when is_atom(Mat) ->
    gb_sets:singleton(Mat);
used_materials_we(#we{mat=MatTab}) ->
    Used0 = sofs:from_external(MatTab, [{face,material}]),
    Used = sofs:range(Used0),
    gb_sets:from_list(sofs:to_external(Used)).

%% Return all image ids used by materials.

used_images(#st{mat=Mat}) ->
    used_images_1(gb_trees:values(Mat), []).

used_images_1([M|Ms], Acc0) ->
    Maps = prop_get(maps, M, []),
    Acc = [Id || {_,Id} <- Maps, is_integer(Id)] ++ Acc0,
    used_images_1(Ms, Acc);
used_images_1([], Acc) -> gb_sets:from_list(Acc).

is_transparent(Name, Mtab) ->
    Mat = gb_trees:get(Name, Mtab),
    is_mat_transparent(Mat).

is_mat_transparent(Mat) ->
    OpenGL = prop_get(opengl, Mat),
    foldl(fun(_, true) -> true;
	     ({emission,_}, _) -> false;
	     ({_,{_,_,_,1.0}}, _) -> false;
	     ({_,{_,_,_,_}}, _) -> true;
	     (_, _) -> false
	  end, false, OpenGL).

%%% The material editor.

-define(PREVIEW_SIZE, 100).

edit(Name, Assign, #st{mat=Mtab0}=St) ->
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
	     }|Maps0]
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
		  maybe_assign(Assign, Name, St#st{mat=Mtab})
	  end,
    wings_ask:dialog("Material Properties: "++atom_to_list(Name), Qs, Ask).

maybe_assign(false, _, St) -> St;
maybe_assign(true, Name, St) -> set_material(Name, St).
    
plugin_results(_, [], Mat) -> Mat;
plugin_results(Name, Res0, Mat0) ->
    {Mat,Res} = wings_plugin:dialog({material_editor_result,Name,Mat0}, Res0),
    plugin_results(Name, Res, Mat).

show_maps(Mat) ->
    case prop_get(maps, Mat) of
	[] -> [];
	Maps ->
	    MapDisp = [show_map(M) || M <- sort(Maps)],
	    [{vframe,MapDisp,[{title,"Textures"}]}]
    end.

show_map({Type,Image}) ->
    Label = case wings_image:info(Image) of
		none ->
		    flatten(io_lib:format("~p: <image deleted>", [Type]));
		#e3d_image{name=Name,width=W,height=H,bytes_pp=PP} ->
		    flatten(io_lib:format("~p: ~p [~px~px~p]",
					  [Type,Name,W,H,PP*8]))
	    end,
    {hframe,[{label,Label}]}.

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

color(Face, {U,V}, We, #st{mat=Mtab}) ->
    Name = get(Face, We),
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

color_1(_, _, none) -> wings_color:white();
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

%%%
%%% New low-level interface introduced when the #face record was
%%% removed. Ideally, the functions below should be the only ones
%%% that exactly know how materials are implemented.
%%%

%% mat_faces([{Face,Info}], We) -> [{Mat,[{Face,Info}]}]
%%  Group face tab into groups based on material.
%%  Used for displaying objects.
mat_faces(Ftab, #we{mat=AtomMat}) when is_atom(AtomMat) ->
    [{AtomMat,Ftab}];
mat_faces(Ftab0, #we{mat=MatTab}) ->
    Ftab1 = mat_join(Ftab0, MatTab, []),
    Ftab2 = sofs:from_external(Ftab1, [{material,info}]),
    Ftab = sofs:relation_to_family(Ftab2),
    sofs:to_external(Ftab).

mat_join([{F1,_}|_]=Fs, [{F2,_}|Ms], Acc) when F2 < F1 ->
    mat_join(Fs, Ms, Acc);
mat_join([{F,Info}|Fs], [{F,Mat}|Ms], Acc) ->
    mat_join(Fs, Ms, [{Mat,{F,Info}}|Acc]);
mat_join([], _, Acc) -> reverse(Acc).

get_all(#we{mat=Mat,fs=Ftab}) ->
    force_list(Mat, Ftab).

get(_, #we{mat=Atom}) when is_atom(Atom) -> Atom;
get(Face, #we{mat=Tab}) ->
    {value,{_,Mat}} = keysearch(Face, 1, Tab),
    Mat.

delete_face(_, #we{mat=AtomMat}=We) when is_atom(AtomMat) -> We;
delete_face(Face, #we{mat=MatTab0}=We) ->
    MatTab = orddict:erase(Face, MatTab0),
    We#we{mat=MatTab}.

delete_faces(_, #we{mat=AtomMat}=We) when is_atom(AtomMat) -> We;
delete_faces(Faces0, #we{mat=MatTab0}=We) when is_list(Faces0) ->
    Faces = sofs:from_external(Faces0, [face]),
    MatTab1 = sofs:from_external(MatTab0, [{face,mat}]),
    MatTab2 = sofs:drestriction(MatTab1, Faces),
    MatTab = sofs:to_external(MatTab2),
    We#we{mat=MatTab};
delete_faces(Faces, We) ->
    delete_faces(gb_sets:to_list(Faces), We).

cleanup(#we{mat=Mat}=We) when is_atom(Mat) -> We;
cleanup(#we{mat=Mat0,fs=Ftab}=We) ->
    Fs = sofs:from_external(gb_trees:keys(Ftab), [face]),
    Mat1 = sofs:from_external(Mat0, [{face,material}]),
    Mat2 = sofs:restriction(Mat1, Fs),
    Mat = sofs:to_external(Mat2),
    We#we{mat=Mat}.
    
assign_materials([{M,F}|_]=MatFace, We) when is_atom(M), is_integer(F) ->
    foldl(fun({Mat,Faces}, W) ->
		  assign(Mat, Faces, W)
	  end, We, wings_util:rel2fam(MatFace));
assign_materials([{F,M}|_]=MatFace0, We) when is_integer(F), is_atom(M) ->
    MatFace1 = sofs:relation(MatFace0),
    MatFace2 = sofs:converse(MatFace1),
    MatFace = sofs:to_external(MatFace2),
    assign_materials(MatFace, We).

assign(Mat, _, #we{mat=Mat}=We) -> We;
assign(Mat, Faces, #we{mat=Tab0,fs=Ftab}=We) when is_list(Faces) ->
    case length(Faces) =:= gb_trees:size(Ftab) of
	true ->
	    We#we{mat=Mat};
	false ->
	    Tab = force_list(Tab0, Ftab),
	    NewTab = sort(make_tab(Mat, Faces)),
	    MatTab = mat_merge(NewTab, Tab, []),
	    We#we{mat=MatTab}
    end;
assign(Mat, Faces, We) ->
    assign(Mat, gb_sets:to_list(Faces), We).

force_list(L, _) when is_list(L) -> L;
force_list(M, Ftab) when is_atom(M) ->
    reverse(make_tab(M, gb_trees:keys(Ftab))).

make_tab(M, List) ->
    foldl(fun(F, A) -> [{F,M}|A] end, [], List).

mat_merge([{Fn,_}|_]=Fns, [{Fo,_}=Fold|Fos], Acc) when Fo < Fn ->
    mat_merge(Fns, Fos, [Fold|Acc]);
mat_merge([{Fn,_}=Fnew|Fns], [{Fo,_}|_]=Fos, Acc) when Fo > Fn ->
    mat_merge(Fns, Fos, [Fnew|Acc]);
mat_merge([Fnew|Fns], [_|Fos], Acc) -> % Equality
    mat_merge(Fns, Fos, [Fnew|Acc]);
mat_merge([], Fos, Acc) ->
    reverse(Acc, Fos);
mat_merge(Fns, [], Acc) ->
    reverse(Acc, Fns).

renumber(Mat, _) when is_atom(Mat) -> Mat;
renumber(L, Fmap) -> renumber_1(L, Fmap, []).

renumber_1([{F,M}|T], Fmap, Acc) ->
    renumber_1(T, Fmap, [{gb_trees:get(F, Fmap),M}|Acc]);
renumber_1([], _, Acc) -> reverse(Acc).

merge([{M,_}|T]=L) ->
    case all_same(T, M) of
	true -> M;
	false -> merge_1(L, [])
    end.

merge_1([{M,#we{fs=Ftab}}|T], Acc) ->
    merge_1(T, [force_list(M, Ftab)|Acc]);
merge_1([], Acc) -> lists:merge(Acc).

all_same([{M,_}|T], M) ->
    all_same(T, M);
all_same([], _) -> true;
all_same([_|_], _) -> false.

-ifdef(DEBUG).
validate(#we{mat=Mat}) when is_atom(Mat) -> ok;
validate(#we{mat=Mat,fs=Ftab}) ->
    Faces = gb_trees:keys(Ftab),
    Faces = [F || {F,_} <- Mat].
-endif.
