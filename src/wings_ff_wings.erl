%%
%%  wings_ff_wings.erl --
%%
%%     This module contain the functions for reading and writing .wings files.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_ff_wings.erl,v 1.19 2002/02/25 08:49:19 bjorng Exp $
%%

-module(wings_ff_wings).
-export([import/2,export/2]).
-include("wings.hrl").
-import(lists, [sort/1,reverse/1,foldl/3]).

-define(WINGS_HEADER, "#!WINGS-1.0\r\n\032\04").

%% Load a wings file.

import(Name, St0) ->
    case file:read_file(Name) of
	{ok,<<?WINGS_HEADER,Sz:32,Data/binary>>} when size(Data) =:= Sz ->
	    case catch binary_to_term(Data) of
		{wings,0,Shapes} ->
                    {error,"Pre-0.80 Wings format no longer supported."};
		{wings,1,Shapes,Materials,Props} ->
                    St1 = wings_material:add_materials(Materials, St0),
                    St2 = St1#st{selmode=import_selmode(Props, St1)},
		    case import_old_objects(Shapes, St2) of
			#st{}=St -> St;
			Other -> {error,"bad objects in Wings file"}
		    end;
		{wings,2,{Shapes,Materials,Props}} ->
                    St1 = wings_material:add_materials(Materials, St0),
                    St2 = import_props(Props, St1),
		    St = import_objects(Shapes, St2);
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

import_objects(Shapes, #st{selmode=Mode,shapes=Shs0,onext=Oid0}=St) ->
    {Objs,Oid} = import_objects(Shapes, Mode, Oid0, []),
    Shs = gb_trees:from_orddict(gb_trees:to_list(Shs0) ++ reverse(Objs)),
    St#st{shapes=Shs,onext=Oid}.

import_objects([Sh0|Shs], Mode, Oid, ShAcc) ->
    {object,Name,{winged,Es,Fs,Vs,He},Props} = Sh0,
    ObjMode = import_object_mode(Props),
    Etab0 = import_edges(Es, #edge{}, 0, []),
    Etab = gb_trees:from_orddict(Etab0),
    Ftab0 = import_faces(Fs, #face{}, 0, []),
    Ftab = face_add_incident(Ftab0, Etab0),
    Vtab0 = import_vs(Vs, #vtx{}, 0, []),
    Vtab = vertex_add_incident(Vtab0, Etab0),
    Htab = gb_sets:from_list(He),
    Perm = import_perm(Props),
    NextId = 1+lists:max([gb_trees:size(Etab),
			  gb_trees:size(Ftab),
			  gb_trees:size(Vtab)]),
    We = #we{es=Etab,fs=Ftab,vs=Vtab,he=Htab,perm=Perm,
	     id=Oid,first_id=0,next_id=NextId,name=Name,mode=ObjMode},
    import_objects(Shs, Mode, Oid+1, [{Oid,We}|ShAcc]);
import_objects([], _Mode, Oid, ShAcc) -> {ShAcc,Oid}.
    
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

import_faces([F|Fs], Template, Face, Acc) ->
    Rec = import_face(F, Template),
    import_faces(Fs, Template, Face+1, [{Face,Rec}|Acc]);
import_faces([], _Template, _Face, Acc) -> reverse(Acc).

import_face([{material,Mat}|T], Rec) ->
    import_face(T, Rec#face{mat=Mat});
import_face([_|T], Rec) ->
    import_face(T, Rec);
import_face([], Rec) -> Rec.

import_vs([Vtx|Vs], Template, V, Acc) -> 
    Rec = import_vertex(Vtx, Template),
    import_vs(Vs, Template, V+1, [{V,Rec}|Acc]);
import_vs([], _Template, _V, Acc) -> reverse(Acc).

import_vertex([<<X/float,Y/float,Z/float>>|T], Rec) ->
    import_vertex(T, Rec#vtx{pos=wings_util:share(X, Y, Z)});
import_vertex([_|T], Rec) ->
    import_vertex(T, Rec);
import_vertex([], Rec) -> Rec.

import_perm(Props) ->
    case property_lists:get_value(state, Props) of
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

vertex_add_incident(Vtab0, Es) ->
    VtoE0 = foldl(fun({Edge,#edge{vs=Va,ve=Vb}}, A) ->
			  [{Va,Edge},{Vb,Edge}|A]
		  end, [], Es),
    VtoE1 = sofs:relation(VtoE0, [{vertex,edge}]),
    VtoE = sofs:relation_to_family(VtoE1),
    Vtab1 = sofs:relation(Vtab0, [{vertex,data}]),
    Vtab2 = sofs:relative_product({Vtab1,VtoE}),
    Vtab = foldl(fun({V,{Rec,[Edge|_]}}, A) ->
			 [{V,Rec#vtx{edge=Edge}}|A]
		 end, [], sofs:to_external(Vtab2)),
    gb_trees:from_orddict(reverse(Vtab)).

import_object_mode(Ps) ->
    property_lists:get_value(mode, Ps, material).

import_props([{selection,{Mode,Sel0}}|Ps], St) ->
    Sel = import_sel(Sel0, St),
    import_props(Ps, St#st{selmode=Mode,sel=Sel});
import_props([{saved_selection,{Mode,Sel0}}|Ps], St) ->
    Sel = import_sel(Sel0, St),
    import_props(Ps, St#st{ssel={Mode,Sel}});
import_props([{vectors,Svec0}|Ps], St) ->
    Svec = import_vectors(Svec0, St),
    import_props(Ps, St#st{svec=Svec});
import_props([_|Ps], St) ->
    import_props(Ps, St);
import_props([], St) -> St.

import_vectors(Svec, St) ->
    [{Name,{Vec,{Mode,import_sel(Sel, St)}}} || {Name,Vec,Mode,Sel} <- Svec].
    
import_sel(Sel, #st{onext=IdBase}) ->
    [{IdBase+Id,gb_sets:from_list(Elems)} || {Id,Elems} <- Sel].

%%%
%%% Import of old Wings file in format 1.
%%%

import_old_objects(Shapes, #st{selmode=Mode,shapes=Shs0,onext=Oid0}=St) ->
    {Objs,Sel0,Oid} = import_old_objects(Shapes, Mode, Oid0, {[],[]}),
    Shs = gb_trees:from_orddict(gb_trees:to_list(Shs0) ++ reverse(Objs)),
    Sel = reverse(Sel0),
    St#st{shapes=Shs,sel=Sel,onext=Oid}.

import_old_objects([Sh0|Shs], Mode, Oid, {ShAcc,SelAcc0}) ->
    {object,Name,{winged,Fs,Vs0,He},Props} = Sh0,
    Vs = decode_vs(Vs0),
    Es = wings_we:build_edges_only(Fs),
    We0 = wings_we:build_rest(material, Es, Fs, Vs, He),
    We = We0#we{id=Oid,name=Name},
    SelAcc = import_old_sel(Mode, Oid, Props, Es, SelAcc0),
    import_old_objects(Shs, Mode, Oid+1, {[{Oid,We}|ShAcc],SelAcc});
import_old_objects([], _Mode, Oid, {ShAcc,SelAcc}) -> {ShAcc,SelAcc,Oid}.

import_old_sel(Mode, Id, Prop, Es, Acc) ->
    case property_lists:get_value(selected, Prop) of
	undefined -> Acc;
	Items -> [{Id,import_old_sel_1(Mode, Items, Es)}|Acc]
    end.

import_selmode(Prop, #st{selmode=Mode}) ->
    property_lists:get_value(selmode, Prop, Mode).

import_old_sel_1(vertex, Vs, _Es) ->
    gb_sets:from_list(Vs);
import_old_sel_1(edge, Vpairs, Es) ->
    wings_we:vpairs_to_edges(Vpairs, Es);
import_old_sel_1(face, Faces, _Es) ->
    gb_sets:from_list(Faces);
import_old_sel_1(body, _Items, _Es) ->
    gb_sets:singleton(0).

decode_vs(Bin) ->
    decode_vs(Bin, []).

decode_vs(<<X/float,Y/float,Z/float,T/binary>>, Acc) ->
    decode_vs(T, [wings_util:share(X, Y, Z)|Acc]);
decode_vs(<<>>, Acc) -> reverse(Acc).

%%%
%%% Save a Wings file (in version 2).
%%%

export(Name, #st{shapes=Shs0}=St) ->
    Sel0 = collect_sel(St),
    {Shs1,Sel} = renumber(gb_trees:to_list(Shs0), Sel0, 0, [], []),
    Shs = foldl(fun shape/2, [], Shs1),
    Materials = wings_material:used_materials(St),
    Props = export_props(Sel),
    Wings = {wings,2,{Shs,Materials,Props}},
    write_file(Name, term_to_binary(Wings, [compressed])).

collect_sel(#st{selmode=Mode,sel=Sel0,ssel={SMode,SSel}}=St) ->
    Sel1 = [{Id,{Mode,gb_sets:to_list(Elems),selection}} ||
	       {Id,Elems} <- Sel0],
    Sel2 = [{Id,{SMode,gb_sets:to_list(Elems),saved_selection}} ||
	       {Id,Elems} <- wings_sel:valid_sel(SSel, SMode, St)] ++ Sel1,
    Sel3 = collect_vectors(St, Sel2),
    Sel4 = sofs:relation(Sel3, [{id,data}]),
    Sel = sofs:relation_to_family(Sel4),
    sofs:to_external(Sel).

collect_vectors(#st{svec=Svec}=St, Acc) ->
    collect_vectors_1(Svec, St, 0, Acc).

collect_vectors_1([{Name,{Vec,{Mode,Sel0}}}|Vecs], St, I, Acc0) ->
    Sel = wings_sel:valid_sel(Sel0, Mode, St),
    NameVec = {vector,I,Name,Vec},
    Acc = [{Id,{Mode,gb_sets:to_list(Elems),NameVec}} ||
	      {Id,Elems} <- Sel] ++ Acc0,
    collect_vectors_1(Vecs, St, I+1, Acc);
collect_vectors_1([], _St, _I, Acc) -> Acc.
    
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

export_props_1([{{{vector,_,Name,Vec},Mode},Sel}|_]=T, Acc) ->
    export_vectors(T, Acc, []);
export_props_1([{{What,Mode},Sel}|T], Acc) ->
    export_props_1(T, [{What,{Mode,Sel}}|Acc]);
export_props_1([], Acc) -> Acc.

export_vectors([{{{vector,_,Name,Vec},Mode},Sel}|T], Props, Acc) ->
    export_vectors(T, Props, [{Name,Vec,Mode,Sel}|Acc]);
export_vectors([], Props, Acc) ->
    [{vectors,reverse(Acc)}|Props].

write_file(Name, Bin) ->
    Data = <<?WINGS_HEADER,(size(Bin)):32,Bin/binary>>,
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

shape(#we{mode=ObjMode,name=Name,fs=Fs0,vs=Vs0,es=Es0,he=Htab}=We, Acc) ->
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
    
edge_data(vertex, #edge{a={R1,G1,B1},b={R2,G2,B2}}, Acc) ->
    [{color,<<R1/float,G1/float,B1/float,
	     R2/float,G2/float,B2/float>>}|Acc];
edge_data(uv, #edge{a={U1,V1},b={U2,V2}}, Acc) ->
    [{uv,<<U1/float,V1/float,U2/float,V2/float>>}|Acc];
edge_data(_, _, Acc) -> Acc.

export_face(#face{mat=default}, Acc) ->
    [[]|Acc];
export_face(#face{mat=Mat}, Acc) ->
    [[{material,Mat}]|Acc].

export_vertex(#vtx{pos={X,Y,Z}}, Acc) ->
    [[<<X/float,Y/float,Z/float>>]|Acc].
