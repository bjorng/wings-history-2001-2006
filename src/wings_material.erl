%%
%%  wings_material.erl --
%%
%%     This module manages the face materials (i.e. colors and textures).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_material.erl,v 1.14 2001/11/29 13:04:16 bjorng Exp $
%%

-module(wings_material).
-export([sub_menu/2,command/2,default/0,add_materials/2,used_materials/1,
	 apply_material/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [map/2,sort/1,foldl/3,reverse/1]).

%% Material record.
-record(mat,
	{ambient={0.0,0.0,0.0},			%Ambient color
	 diffuse={0.0,0.0,0.0},			%Diffuse color
	 specular={0.0,0.0,0.0},		%Specular color
	 shininess=0.0,				%Sinininess (0..1)
	 opacity=1.0,				%Opacity (0..1)
	 twosided=false,			%Twosided material.
	 diffuse_map=none,			%Diffuse map.
	 diffuse_map_dl=none,			%Diffuse map.
	 attr=[],				%Uinterpreted attributes
	 setup					%Fun for OpenGL drawing
	 }).

sub_menu(face, St) ->
    Mlist = material_list(St),
    Materials = list_to_tuple([{"New",new},separator|Mlist]),
    {"Set Material",{material,Materials}};
sub_menu(edit, St) ->
    {"Material [ALPHA]",{material,materials(St)}};
sub_menu(select, St) ->
    {"Material",{material,materials(St)}}.

command({face,{material,new}}, St0) ->
    case wings_getline:string("Material Name: ") of
	aborted -> St0;
	Name0 ->
	    Name = list_to_atom(Name0),
	    Mat = make_default({1.0,1.0,1.0}),
	    St1 = add(Name, Mat, St0),
	    St = edit(Name, St1),
	    set_material(Name, St)
    end;
command({face,{material,Mat}}, St) ->
    set_material(Mat, St);
command({select,{material,Mat}}, St) ->
    wings_sel:make(fun(Face, #we{fs=Ftab}) ->
			   #face{mat=M} = gb_trees:get(Face, Ftab),
			   M =:= Mat
		   end, face, St);
command({edit,{material,Mat}}, St) ->
    edit(Mat, St).

materials(St) ->
    list_to_tuple(material_list(St)).

material_list(#st{mat=Mat0}) ->
    map(fun(Id) ->
		Name = case atom_to_list(Id) of
			   [H|T] when $a =< H, H =< $z ->
			       [H-$\s|T];
			   Name0 -> Name0
		       end,
		{Name,Id}
	end, gb_trees:keys(Mat0)).

set_material(Mat, St) ->
    wings_sel:map(
      fun(Face, #we{fs=Ftab0}=We) ->
	      Rec = gb_trees:get(Face, Ftab0),
	      Ftab = gb_trees:update(Face, Rec#face{mat=Mat}, Ftab0),
	      We#we{mode=material,fs=Ftab}
      end, St).

default() ->
    M0 = [{default,wings_util:share(1.0, 1.0, 1.0)},
	  {hole,wings_util:share(0.5, 0.5, 0.0)}],
    M = [{Key,make_default(Color)} || {Key,Color} <- M0],
    gb_trees:from_orddict(sort(M)).

make_default({R,G,B}) ->
    Opacity = 1.0,
    Color = {R,G,B},
    White = {1.0,1.0,1.0},
    setup_fun(#mat{ambient=Color,diffuse=Color,specular=White,
		   shininess=0.0,opacity=Opacity}).

add_materials([{Name,Prop}|Ms], St0) ->
    Mat0 = translate_mat(Prop, #mat{}),
    {Mat,St1} = init_texture(Mat0, St0),
    St = add(Name, Mat, St1),
    add_materials(Ms, St);
add_materials([], St) -> St.

translate_mat([{ambient,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{ambient=RGB});
translate_mat([{diffuse,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{diffuse=RGB});
translate_mat([{specular,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{specular=RGB});
translate_mat([{shininess,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{shininess=RGB});
translate_mat([{opacity,RGB}|T], Mat) ->
    translate_mat(T, Mat#mat{opacity=RGB});
translate_mat([{twosided,Boolean}|T], Mat) ->
    translate_mat(T, Mat#mat{twosided=Boolean});
translate_mat([{diffuse_map,{W,H,Bits}=Tx}|T], Mat) when binary(Bits) ->
    translate_mat(T, Mat#mat{diffuse_map=Tx});
translate_mat([{diffuse_map,Name}|T], Mat) ->
    case catch loadTexture(Name) of
	{'EXIT',R} -> translate_mat(T, Mat);
	{W,H,Bits}=Tx -> translate_mat(T, Mat#mat{diffuse_map=Tx})
    end;
translate_mat([Other|T], #mat{attr=Attr}=Mat) ->
    translate_mat(T, Mat#mat{attr=[Other|Attr]});
translate_mat([], Mat) -> Mat.

add(Name, Mat0, #st{mat=MatTab}=St) ->
    Mat = setup_fun(Mat0),
    St#st{mat=gb_trees:enter(Name, Mat, MatTab)}.

setup_fun(Mat) ->
    #mat{ambient=Amb0,diffuse=Diff0,specular=Spec0,
	 shininess=Shine,opacity=Opac,diffuse_map_dl=Dmap} = Mat,
    Amb = erlang:append_element(Amb0, Opac),
    Diff = erlang:append_element(Diff0, Opac),
    Spec = erlang:append_element(Spec0, Opac),
    F = fun() ->
		if
		    Dmap =:= none ->
			gl:enable(?GL_TEXTURE_2D);
		    true ->
			gl:enable(?GL_TEXTURE_2D),
			gl:texEnvi(?GL_TEXTURE_ENV,
				   ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
			gl:bindTexture(?GL_TEXTURE_2D, Dmap),
			gl:texParameteri(?GL_TEXTURE_2D,
					 ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
			gl:texParameteri(?GL_TEXTURE_2D,
					 ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
			gl:texParameteri(?GL_TEXTURE_2D,
					 ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
			gl:texParameteri(?GL_TEXTURE_2D,
					 ?GL_TEXTURE_WRAP_T, ?GL_CLAMP)
		end,
		gl:materialfv(?GL_FRONT, ?GL_AMBIENT, Amb),
		gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, Diff),
		gl:materialfv(?GL_FRONT, ?GL_SPECULAR, Spec),
		gl:materialfv(?GL_FRONT, ?GL_SHININESS, (1.0-Shine)*128.0)
	end,
    Mat#mat{setup=F}.

apply_material(Mat, Mtab) when atom(Mat) ->
    #mat{setup=Setup} = gb_trees:get(Mat, Mtab),
    Setup();
apply_material([Mat|_], Mtab) ->
    apply_material(Mat, Mtab).

%%% Returns the materials used.

used_materials(#st{mat=Mat0}=St) ->
    Used0 = wings_util:fold_shape(
	      fun(#shape{sh=#we{fs=Ftab}}, A) ->
		      used_materials_1(Ftab, A)
	      end, gb_sets:empty(), St),
    Used1 = sofs:from_external(gb_sets:to_list(Used0), [name]),
    Mat = sofs:relation(gb_trees:to_list(Mat0), [{name,data}]),
    Used = sofs:restriction(Mat, Used1),
    [to_external(M) || M <- sofs:to_external(Used)].

used_materials_1(Ftab, Acc) ->
    foldl(fun(#face{mat=[_|_]=Mat}, A) ->
		  gb_sets:union(A, gb_sets:from_list(Mat));
	     (#face{mat=Mat}, A) ->
		  gb_sets:add(Mat, A)
	  end, Acc, gb_trees:values(Ftab)).

to_external({Name,#mat{ambient=Amb,diffuse=Diff,specular=Spec,
		       shininess=Shine,opacity=Opacity,twosided=TwoSided,
		       diffuse_map=Map,
		       attr=Attr}}) ->
    {Name,[{ambient,Amb},{diffuse,Diff},{specular,Spec},
	   {shininess,Shine},{opacity,Opacity},
	   {diffuse_map,Map},
	   {twosided,TwoSided}|Attr]}.

%%% The material editor.

edit(Name, #st{mat=Mtab0}=St) ->
    Mat0 = gb_trees:get(Name, Mtab0),
    Mat = setup_fun(wings_matedit:edit(Mat0)),
    Mtab = gb_trees:update(Name, Mat, Mtab0),
    wings_draw:model_changed(St#st{mat=Mtab}).

%%% Texture support.

init_texture(#mat{diffuse_map={W,H,Bits},diffuse_map_dl=none}=Mat,
	     #st{next_tx=TxId}=St) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
		  W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Bits),
    gl:popAttrib(),
    {Mat#mat{diffuse_map_dl=TxId},St#st{next_tx=TxId+1}};
init_texture(Mat, St) -> {Mat,St}.

loadTexture(none) -> exit(no_texture);
loadTexture(File) ->
    io:format("Loading ~s\n", [File]),
    {ok,Bin0} = file:read_file(File),
    <<$B:8,$M:8,_:8/binary,Offset:32/little,Bin/binary>> = Bin0,
    <<_:32/little,W:32/little,H:32/little,
     _:16,BitCount:16/little,Compression:16/little,_/binary>> = Bin,
    BitCount = 24,
    Compression = 0,
    PixelsLen = H*W*3,
    <<_:Offset/binary,Pixels0:PixelsLen/binary,_/binary>> = Bin0,
    Pixels = shuffle_colors(Pixels0, []),
    {W,H,Pixels}.

shuffle_colors(<<B:8,G:8,R:8,T/binary>>, Acc) ->
    shuffle_colors(T, [[R,G,B]|Acc]);
shuffle_colors(<<>>, Acc) -> list_to_binary(reverse(Acc)).
