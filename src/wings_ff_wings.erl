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
%%     $Id: wings_ff_wings.erl,v 1.31 2003/01/20 07:36:55 bjorng Exp $
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
    Materials = translate_materials(Materials0),
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
    Ssels = gb_trees:insert({Mode,Name}, Sel, Ssels0),
    St#st{ssels=Ssels}.

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
    Materials0 = wings_material:used_materials(St),
    Materials = mat_images(Materials0),
    Props0 = export_props(Sel),
    Props = case Lights of
		[] -> Props0;
		[_|_] -> [{lights,Lights}|Props0]
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

mat_images(Mats) ->
    mat_images(Mats, []).

mat_images([{Name,Mat0}|T], Acc) ->
    Mat = mat_images_1(Mat0, []),
    mat_images(T, [{Name,Mat}|Acc]);
mat_images([], Acc) -> Acc.

mat_images_1([{maps,Maps0}|T], Acc) ->
    Maps = mat_images_2(Maps0, []),
    mat_images_1(T, [{maps,Maps}|Acc]);
mat_images_1([H|T], Acc) ->
    mat_images_1(T, [H|Acc]);
mat_images_1([], Acc) -> Acc.

mat_images_2([{Type,Image}|T], Acc) ->
    #e3d_image{width=W,height=H,image=Bits} = wings_image:info(Image),
    mat_images_2(T, [{Type,{W,H,Bits}}|Acc]);
mat_images_2([], Acc) -> Acc.
