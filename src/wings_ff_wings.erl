%%
%%  wings_ff_wings.erl --
%%
%%     This module contain the functions for reading and writing .wings files.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_ff_wings.erl,v 1.34 2003/03/03 21:44:13 bjorng Exp $
%%

-module(wings_ff_wings).
-export([import/2,export/2]).

-include("wings.hrl").
-include("e3d_image.hrl").
-import(lists, [sort/1,reverse/1,foldl/3]).

-define(WINGS_HEADER, "#!WINGS-1.0\r\n\032\04").

%% Load a wings file.

import(Name, St0) ->
    case file:read_file(Name) of
	{ok,<<?WINGS_HEADER,Sz:32,Data/binary>>} when size(Data) =:= Sz ->
	    case catch binary_to_term(Data) of
		{wings,0,_Shapes} ->
                    {error,"Pre-0.80 Wings format no longer supported."};
		{wings,1,_,_,_} ->
                     %% Pre-0.92. No longer supported.
                    {error,"Pre-0.92 Wings format no longer supported."};
		{wings,2,{Shapes,Materials,Props}} ->
                    import_vsn2(Shapes, Materials, Props, St0);
		{wings,_,_} ->
		    {error,"unknown wings format"};
		Other ->
		    io:format("~p\n", [Other]),
                    {error,"corrupt Wings file"}
	    end;
	{ok,_Bin} ->
	    {error,"not a Wings file (or old Wings format)"};
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_vsn2(Shapes, Materials0, Props, St0) ->
    Images = import_images(Props),
    Materials1 = translate_materials(Materials0),
    Materials = translate_map_images(Materials1, Images),
    {St1,NameMap0} = wings_material:add_materials(Materials, St0),
    St = import_props(Props, St1),
    NameMap = gb_trees:from_orddict(sort(NameMap0)),
    import_objects(Shapes, NameMap, St).

import_objects(Shapes, NameMap, #st{selmode=Mode,shapes=Shs0,onext=Oid0}=St) ->
    {Objs,Oid} = import_objects(Shapes, Mode, NameMap, Oid0, []),
    Shs = gb_trees:from_orddict(gb_trees:to_list(Shs0) ++ reverse(Objs)),
    St#st{shapes=Shs,onext=Oid}.

import_objects([Sh0|Shs], Mode, NameMap, Oid, ShAcc) ->
    {object,Name,{winged,Es,Fs,Vs,He},Props} = Sh0,
    ObjMode = import_object_mode(Props),
    Etab0 = import_edges(Es, #edge{}, 0, []),
    Etab = gb_trees:from_orddict(Etab0),
    Ftab0 = import_faces(Fs, #face{}, NameMap, 0, []),
    Ftab = face_add_incident(Ftab0, Etab0),
    Vtab = import_vs(Vs, 0, []),
    Htab = gb_sets:from_list(He),
    Perm = import_perm(Props),
    We0 = #we{es=Etab,fs=Ftab,vp=Vtab,he=Htab,perm=Perm,
	      id=Oid,name=Name,mode=ObjMode},
    We = wings_we:rebuild(We0),
    import_objects(Shs, Mode, NameMap, Oid+1, [{Oid,We}|ShAcc]);
import_objects([], _Mode, _NameMap, Oid, ShAcc) -> {ShAcc,Oid}.
    
import_edges([E|Es], Template, Edge, Acc) ->
    Rec = import_edge(E, Template),
    import_edges(Es, Template, Edge+1, [{Edge,Rec}|Acc]);
import_edges([], _Template, _Edge, Acc) -> reverse(Acc).

import_edge([{edge,Va,Vb,Lf,Rf,Ltpr,Ltsu,Rtpr,Rtsu}|T], Rec0) ->
    Rec = Rec0#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
		    ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu},
    import_edge(T, Rec);
import_edge([{color,Bin}|T], Rec) ->
    <<R1/float,G1/float,B1/float,R2/float,G2/float,B2/float>> = Bin,
    case {wings_color:share({R1,G1,B1}),wings_color:share({R2,G2,B2})} of
	{Same,Same} -> import_edge(T, Rec#edge{a=Same,b=Same});
	{A,B} -> import_edge(T, Rec#edge{a=A,b=B})
    end;
import_edge([{uv,Bin}|T], Rec) ->
    <<U1/float,V1/float,U2/float,V2/float>> = Bin,
    import_edge(T, Rec#edge{a={U1,V1},b={U2,V2}});
import_edge([_|T], Rec) ->
    import_edge(T, Rec);
import_edge([], Rec) -> Rec.

import_faces([F|Fs], Template, NameMap, Face, Acc) ->
    Rec = import_face(F, NameMap, Template),
    import_faces(Fs, Template, NameMap, Face+1, [{Face,Rec}|Acc]);
import_faces([], _Template, _NameMap, _Face, Acc) -> reverse(Acc).

import_face([{material,Mat0}|T], NameMap, Rec) ->
    case gb_trees:lookup(Mat0, NameMap) of
	none -> import_face(T, NameMap, Rec#face{mat=Mat0});
	{value,NewName} -> import_face(T, NameMap, Rec#face{mat=NewName})
    end;
import_face([_|T], NameMap, Rec) ->
    import_face(T, NameMap, Rec);
import_face([], _NameMap, Rec) -> Rec.

import_vs([Vtx|Vs], V, Acc) -> 
    Rec = import_vertex(Vtx, []),
    import_vs(Vs, V+1, [{V,Rec}|Acc]);
import_vs([], _V, Acc) ->
    gb_trees:from_orddict(reverse(Acc)).

import_vertex([<<X/float,Y/float,Z/float>>|T], _) ->
    import_vertex(T, wings_util:share(X, Y, Z));
import_vertex([_|T], Rec) ->
    import_vertex(T, Rec);
import_vertex([], Rec) -> Rec.

import_perm(Props) ->
    case proplists:get_value(state, Props) of
	undefined -> 0;
	locked -> 1;
	hidden -> [];
	{hidden,Mode,Set} -> {Mode,gb_sets:from_list(Set)}
    end.

face_add_incident(Ftab0, Es) ->
    FtoE0 = foldl(fun({Edge,#edge{lf=Lf,rf=Rf}}, A) ->
			  [{Lf,Edge},{Rf,Edge}|A]
		  end, [], Es),
    FtoE1 = sofs:relation(FtoE0, [{face,edge}]),
    FtoE = sofs:relation_to_family(FtoE1),
    Ftab1 = sofs:relation(Ftab0, [{face,data}]),
    Ftab2 = sofs:relative_product({Ftab1,FtoE}),
    Ftab = foldl(fun({Face,{Rec,[Edge|_]}}, A) ->
			 [{Face,Rec#face{edge=Edge}}|A]
		 end, [], sofs:to_external(Ftab2)),
    gb_trees:from_orddict(reverse(Ftab)).

import_object_mode(Ps) ->
    case proplists:get_value(mode, Ps, material) of
	undefined ->
	    io:format("Changed undefined mode to material\n"),
	    material;
	Other -> Other
    end.

import_props([{selection,{Mode,Sel0}}|Ps], St) ->
    Sel = import_sel(Sel0, St),
    import_props(Ps, St#st{selmode=Mode,sel=Sel});
import_props([{saved_selection,{Mode,Sel0}}|Ps], St0) ->
    Sel = import_sel(Sel0, St0),
    St = new_sel_group("<Stored Selection>", Mode, Sel, St0),
    import_props(Ps, St);
import_props([{{selection_group,Name},{Mode,Sel0}}|Ps], St0) ->
    Sel = import_sel(Sel0, St0),
    St = new_sel_group(Name, Mode, Sel, St0),
    import_props(Ps, St);
import_props([{lights,Lights}|Ps], St0) ->
    St = wings_light:import(Lights, St0),
    import_props(Ps, St);
import_props([_|Ps], St) ->
    import_props(Ps, St);
import_props([], St) -> St.

import_sel(Sel, #st{onext=IdBase}) ->
    [{IdBase+Id,gb_sets:from_list(Elems)} || {Id,Elems} <- Sel].

new_sel_group(Name, Mode, Sel, #st{ssels=Ssels0}=St) ->
    Key = {Mode,Name},
    case gb_trees:is_defined(Key, Ssels0) of
	true -> St;
	false ->
	    Ssels = gb_trees:insert(Key, Sel, Ssels0),
	    St#st{ssels=Ssels}
    end.

import_images(Props) ->
    Empty = gb_trees:empty(),
    case proplists:get_value(images, Props) of
	undefined -> Empty;
	Images -> import_images_1(Images, Empty)
    end.
	    
import_images_1([{Id0,Im}|T], Map) ->
    Name = proplists:get_value(name, Im, "unnamed image"),
    W = proplists:get_value(width, Im, 0),
    H = proplists:get_value(height, Im, 0),
    PP = proplists:get_value(samples_per_pixel, Im, 0),
    Pixels = proplists:get_value(pixels, Im),
    if
	W*H*PP =:= size(Pixels) -> ok;
	true -> wings_util:error("Bad image: ~p\n", [Name])
    end,
    Type = case PP of
	       1 -> g8;
	       2 -> g8a8;
	       3 -> r8g8b8;
	       4 -> r8g8b8a8
	   end,
    E3D = #e3d_image{width=W,height=H,type=Type,order=lower_left,
		     alignment=1,bytes_pp=PP,image=Pixels},
    Id = wings_image:new(Name, E3D),
    import_images_1(T, gb_trees:insert(Id0, Id, Map));
import_images_1([], Map) -> Map.

translate_map_images(Mats, ImMap) ->
    [translate_map_images_1(M, ImMap) || M <- Mats].

translate_map_images_1({Name,Props0}=Mat, ImMap) ->
    case proplists:get_value(maps, Props0, []) of
	[] -> Mat;
	Maps ->
	    Props = lists:keydelete(maps, 1, Props0),
	    {Name,[{maps,translate_map_images_2(Maps, ImMap)}|Props]}
    end.

translate_map_images_2([{Type,Im0}|T], ImMap) when is_integer(Im0) ->
    Im = gb_trees:get(Im0, ImMap),
    [{Type,Im}|translate_map_images_2(T, ImMap)];
translate_map_images_2([H|T], ImMap) ->
    [H|translate_map_images_2(T, ImMap)];
translate_map_images_2([], _) -> [].

%%%
%%% Import of old materials format (up to and including wings-0.94.02).
%%%

translate_materials(Mats) ->
    [translate_material(M) || M <- Mats].
    
translate_material({Name,Props}=Mat) ->
    case proplists:is_defined(opengl, Props) of
	true -> Mat;
	false ->
	    Opac = proplists:get_value(opacity, Props),
	    {Name,translate_material(Props, Opac, [], [])}
    end.

translate_material([Mat|Mats], Opac, OpenGL, Maps) ->
    case Mat of
	{diffuse_map,Map} ->
	    translate_material(Mats, Opac, OpenGL, [{diffuse,Map}|Maps]);
	{diffuse,_}=Diff ->
	    translate_material(Mats, Opac, [trans(Diff, Opac)|OpenGL], Maps);
	{ambient,_}=Amb ->
	    translate_material(Mats, Opac, [trans(Amb, Opac)|OpenGL], Maps);
	{specular,_}=Spec ->
	    translate_material(Mats, Opac, [trans(Spec, Opac)|OpenGL], Maps);
	{shininess,Sh} ->
	    translate_material(Mats, Opac, [{shininess,1.0-Sh}|OpenGL], Maps);
	_ ->
	    translate_material(Mats, OpenGL, Opac, Maps)
    end;
translate_material([], _, OpenGL, Maps) ->
    [{opengl,OpenGL},{maps,Maps}].

trans({Key,{R,G,B}}, Opac) -> {Key,{R,G,B,Opac}}.
    
%%%
%%% Save a Wings file (in version 2).
%%%

export(Name, St0) ->
    Lights = wings_light:export(St0),
    #st{shapes=Shs0} = St = remove_lights(St0),
    Sel0 = collect_sel(St),
    {Shs1,Sel} = renumber(gb_trees:to_list(Shs0), Sel0, 0, [], []),
    Shs = foldl(fun shape/2, [], Shs1),
    Materials = wings_material:used_materials(St),
    Props0 = export_props(Sel),
    Props1 = case Lights of
		 [] -> Props0;
		 [_|_] -> [{lights,Lights}|Props0]
	     end,
    Props = case export_images() of
		[] -> Props1;
		Images -> [{images,Images}|Props1]
	    end,
    Wings = {wings,2,{Shs,Materials,Props}},
    write_file(Name, term_to_binary(Wings, [compressed])).

remove_lights(#st{sel=Sel0,shapes=Shs0}=St) ->
    Shs1 = foldl(fun(We, A) when ?IS_LIGHT(We) -> A;
		    (#we{id=Id}=We, A) -> [{Id,We}|A]
		 end, [], gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(reverse(Shs1)),
    Sel = [S || {Id,_}=S <- Sel0, gb_trees:is_defined(Id, Shs)],
    St#st{sel=Sel,shapes=Shs}.

collect_sel(#st{selmode=Mode,sel=Sel0,ssels=Ssels}=St) ->
    Sel1 = [{Id,{Mode,gb_sets:to_list(Elems),selection}} ||
	       {Id,Elems} <- Sel0],
    Sel2 = collect_sel_groups(gb_trees:to_list(Ssels), St, Sel1),
    Sel3 = sofs:relation(Sel2, [{id,data}]),
    Sel = sofs:relation_to_family(Sel3),
    sofs:to_external(Sel).

collect_sel_groups([{{Mode,Name},Sel}|Gs], St, Acc0) ->
    Acc = [{Id,{Mode,gb_sets:to_list(Elems),{selection_group,Name}}} ||
	      {Id,Elems} <- wings_sel:valid_sel(Sel, Mode, St)] ++ Acc0,
    collect_sel_groups(Gs, St, Acc);
collect_sel_groups([], _, Acc) -> Acc.

renumber([{Id,We0}|Shs], [{Id,Root0}|Sel], NewId, WeAcc, RootAcc) ->
    {We,Root} = wings_we:renumber(We0, 0, Root0),
    renumber(Shs, Sel, NewId+1, [We|WeAcc], [{NewId,Root}|RootAcc]);
renumber([{_,We0}|Shs], Sel, NewId, WeAcc, RootAcc) ->
    We = wings_we:renumber(We0, 0),
    renumber(Shs, Sel, NewId+1, [We|WeAcc], RootAcc);
renumber([], [], _NewId, WeAcc, RootAcc) ->
    {WeAcc,RootAcc}.

export_props(Sel0) ->
    Sel1 = sofs:family(Sel0, [{id,[{mode,list,key}]}]),
    Sel2 = sofs:family_to_relation(Sel1),
    Sel3 = sofs:projection(
	     {external,fun({Id,{Mode,Elems,Key}}) ->
			       {{Key,Mode},{Id,Elems}}
		       end}, Sel2),
    Sel = sofs:relation_to_family(Sel3),
    export_props_1(sofs:to_external(Sel), []).

export_props_1([{{What,Mode},Sel}|T], Acc) ->
    export_props_1(T, [{What,{Mode,Sel}}|Acc]);
export_props_1([], Acc) -> Acc.

write_file(Name, Bin) ->
    Data = <<?WINGS_HEADER,(size(Bin)):32,Bin/binary>>,
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

shape(#we{mode=ObjMode,name=Name,fs=Fs0,vp=Vs0,es=Es0,he=Htab}=We, Acc) ->
    Vs1 = foldl(fun export_vertex/2, [], gb_trees:values(Vs0)),
    Vs = reverse(Vs1),
    Es1 = foldl(fun(E, A) ->
			export_edge(E, ObjMode, A)
		end, [], gb_trees:values(Es0)),
    Es = reverse(Es1),
    Fs1 = foldl(fun export_face/2, [], gb_trees:values(Fs0)),
    Fs = reverse(Fs1),
    He = gb_sets:to_list(Htab),
    Props = [{mode,ObjMode}|export_perm(We)],
    [{object,Name,{winged,Es,Fs,Vs,He},Props}|Acc].

export_edge(Rec, Mode, Acc) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
	  ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu} = Rec,
    Data0 = [{edge,Va,Vb,Lf,Rf,Ltpr,Ltsu,Rtpr,Rtsu}],
    Data = edge_data(Mode, Rec, Data0),
    [Data|Acc].

export_perm(#we{perm=0}) -> [];
export_perm(#we{perm=1}) -> [{state,locked}];
export_perm(#we{perm=[]}) -> [{state,hidden}];
export_perm(#we{perm={Mode,Elems}}) ->
    [{state,{hidden,Mode,gb_sets:to_list(Elems)}}].
    
edge_data(uv, #edge{a={U1,V1},b={U2,V2}}, Acc) ->
    [{uv,<<U1/float,V1/float,U2/float,V2/float>>}|Acc];
edge_data(_, #edge{a={1.0,1.0,1.0},b={1.0,1.0,1.0}}, Acc) -> Acc;
edge_data(_, #edge{a={R1,G1,B1},b={R2,G2,B2}}, Acc) ->
    [{color,<<R1/float,G1/float,B1/float,
      R2/float,G2/float,B2/float>>}|Acc];
edge_data(_, _, Acc) -> Acc.

export_face(#face{mat=default}, Acc) ->
    [[]|Acc];
export_face(#face{mat=Mat}, Acc) ->
    [[{material,Mat}]|Acc].

export_vertex({X,Y,Z}, Acc) ->
    [[<<X/float,Y/float,Z/float>>]|Acc].

export_images() ->
    export_images_1(wings_image:images()).

export_images_1([{Id,Im}|T]) ->
    [{Id,export_image(Im)}|export_images_1(T)];
export_images_1([]) -> [].

export_image(#e3d_image{type=Type0,order=Order}=Im0) ->
    Im = case {export_img_type(Type0),Order} of
	     {Type0,lower_left} -> Im0;
	     {Type,_} -> e3d_image:convert(Im0, Type, 1, lower_left)
	 end,
    #e3d_image{width=W,height=H,bytes_pp=PP,image=Pixels,name=Name} = Im,
    [{name,Name},{width,W},{height,H},{samples_per_pixel,PP},{pixels,Pixels}].

export_img_type(b8g8r8) -> r8g8b8;
export_img_type(b8g8r8a8) -> r8g8b8a8;
export_img_type(Type) -> Type.
