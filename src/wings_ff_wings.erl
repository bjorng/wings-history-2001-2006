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
%%     $Id: wings_ff_wings.erl,v 1.17 2002/01/06 14:47:09 bjorng Exp $
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
		    case old_import_objects(Shapes, St0) of
			#st{}=St -> St;
			Other -> {error,"bad objects in Wings file"}
		    end;
		{wings,1,Shapes,Materials,Props} ->
                    St1 = wings_material:add_materials(Materials, St0),
                    St2 = St1#st{selmode=import_selmode(Props, St1)},
		    case import_objects(Shapes, St2) of
			#st{}=St -> St;
			Other -> {error,"bad objects in Wings file"}
		    end;
		{wings,_,_} ->
		    {error,"unknown wings format"};
		Other ->
		    io:format("~p\n", [Other]),
                    {error,"corrupt Wings file"}
	    end;
	{ok,Bin} ->
	    {error,"not a Wings file (or old Wings format)"};
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_objects(Shapes, #st{selmode=Mode,shapes=Shs0,onext=Oid0}=St) ->
    {Objs,Sel0,Oid} = import_objects(Shapes, Mode, Oid0, {[],[]}),
    Shs = gb_trees:from_orddict(gb_trees:to_list(Shs0) ++ reverse(Objs)),
    Sel = reverse(Sel0),
    St#st{shapes=Shs,sel=Sel,onext=Oid}.

import_objects([Sh0|Shs], Mode, Oid, {ShAcc,SelAcc0}) ->
    {object,Name,{winged,Fs,Vs0,He},Props} = Sh0,
    Vs = decode_vs(Vs0),
    Es = wings_we:build_edges_only(Fs),
    We0 = wings_we:build_rest(material, Es, Fs, Vs, He),
    We = We0#we{id=Oid,name=Name},
    SelAcc = import_sel(Mode, Oid, Props, Es, SelAcc0),
    import_objects(Shs, Mode, Oid+1, {[{Oid,We}|ShAcc],SelAcc});
import_objects([], Mode, Oid, {ShAcc,SelAcc}) -> {ShAcc,SelAcc,Oid}.

decode_vs(Bin) ->
    decode_vs(Bin, []).

decode_vs(<<X/float,Y/float,Z/float,T/binary>>, Acc) ->
    decode_vs(T, [wings_util:share(X, Y, Z)|Acc]);
decode_vs(<<>>, Acc) -> reverse(Acc).

import_selmode(Prop, #st{selmode=Mode}) ->
    property_lists:get_value(selmode, Prop, Mode).

import_sel(Mode, Id, Prop, Es, Acc) ->
    case property_lists:get_value(selected, Prop) of
	undefined -> Acc;
	Items -> [{Id,import_sel_1(Mode, Items, Es)}|Acc]
    end.

import_sel_1(vertex, Vs, Es) ->
    gb_sets:from_list(Vs);
import_sel_1(edge, Vpairs, Es) ->
    wings_we:vpairs_to_edges(Vpairs, Es);
import_sel_1(face, Faces, Es) ->
    gb_sets:from_list(Faces);
import_sel_1(body, Items, Es) ->
    gb_sets:singleton(0).

%% Reading of version 0 wings files.
old_import_objects(Shapes, St0) ->		% Version 0.
    foldl(fun ({object,Name,Matrix0,{winged,Fs,Vs0},Prop}, St) ->
		  HardEdges = property_lists:get_value(hard_edges, Prop, []),
		  Matrix1 = e3d_mat:compress(Matrix0),
		  Matrix = e3d_mat:mul(Matrix1, e3d_mat:scale(0.1, 0.1, 0.1)),
		  Vs = [e3d_mat:mul_point(Matrix, P) || P <- Vs0],
		  We = wings_we:build(Fs, Vs, HardEdges),
		  wings_shape:new(Name, We, St)
	  end, St0, Shapes).

%% Save a Wings file (in version 1).

export(Name, #st{selmode=Mode,sel=Sel0,shapes=Shapes0}=St) ->
    Shapes1 = gb_trees:to_list(Shapes0),
    ShapeSel = shape_sel(Shapes1, Sel0),
    Shapes2 = foldl(fun({Sh,Sel}, A) ->
			    shape(Sh, Mode, Sel, A)
		    end, [], ShapeSel),
    Shapes = reverse(Shapes2),
    Materials = wings_material:used_materials(St),
    Prop = export_selmode(St),
    Wings = {wings,1,Shapes,Materials,Prop},
    write_file(Name, term_to_binary(Wings, [compressed])).

shape_sel(Shapes0, Sel0) ->
    Shapes1 = sofs:from_term(Shapes0, [{id,shape}]),
    Sel1 = sofs:from_external(Sel0, [{id,items}]),
    EmptySel = sofs:from_term(gb_sets:empty(), items),
    NotSelected0 = sofs:difference(sofs:domain(Shapes1), sofs:domain(Sel1)),
    NotSelected = sofs:constant_function(NotSelected0, EmptySel),
    Sel = sofs:union(Sel1, NotSelected),
    sofs:to_external(sofs:relative_product1(Shapes1, Sel)).
    
write_file(Name, Bin) ->
    Data = <<?WINGS_HEADER,(size(Bin)):32,Bin/binary>>,
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

shape(#we{mode=ObjMode,name=Name}=We0, Mode, Sel0, Acc) ->
    Sel1 = gb_sets:to_list(Sel0),
    {We,[{Mode,Sel}]} = wings_we:renumber(We0, 0, [{Mode,Sel1}]),
    #we{fs=Ftab,vs=Vs0,es=Etab,he=Htab} = We,
    Vs1 = gb_trees:values(Vs0),
    Vs2 = [<<X/float,Y/float,Z/float>> || #vtx{pos={X,Y,Z}} <- Vs1],
    Vs = list_to_binary(Vs2),
    Fs1 = foldl(fun({Face,#face{mat=Mat}}, A) ->
			[{Mat,wings_face:surrounding_vertices(Face, We)}|A]
		end, [], gb_trees:to_list(Ftab)),
    Fs = reverse(Fs1),
    He = sort(edges2vertices(gb_sets:to_list(Htab), Etab)),
    Prop = export_sel(Mode, Sel, Etab),
    [{object,Name,{winged,Fs,Vs,He},Prop}|Acc].

export_selmode(#st{sel=[]}) -> [];
export_selmode(#st{selmode=Mode}) -> [{selmode,Mode}].

export_sel(_, [], Etab) -> [];
export_sel(vertex, Sel, Etab) -> [{selected,Sel}];
export_sel(edge, Sel, Etab) -> [{selected,edges2vertices(Sel, Etab)}];
export_sel(face, Sel, Etab) -> [{selected,Sel}];
export_sel(body, Sel, Etab) -> [{selected,[]}].

edges2vertices(Edges, Etab) ->
    foldl(fun(Edge, A) ->
		  #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		  [edge(Va,Vb)|A]
	  end, [], Edges).
    
edge(Va, Vb) when Va < Vb -> {Va,Vb};
edge(Va, Vb) -> {Vb,Va}.
