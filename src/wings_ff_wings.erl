%%
%%  wings_ff_wings.erl --
%%
%%     This module contain the functions for reading and writing .wings files.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_ff_wings.erl,v 1.7 2001/09/17 07:19:18 bjorng Exp $
%%

-module(wings_ff_wings).
-export([import/2,export/2]).
-include("wings.hrl").
-import(lists, [sort/1,reverse/1,foldl/3,keysearch/3]).

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
		    case import_objects(Shapes, St1) of
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

import_objects(Shapes, St0) ->			% Version 1.
    foldl(fun ({object,Name,{winged,Fs,Vs0,He},Props}, St) ->
		  Vs = decode_vs(Vs0),
		  We = wings_we:build(identity, Fs, Vs, He),
		  wings_shape:new(Name, We, St)
	  end, St0, Shapes).

decode_vs(Bin) ->
    decode_vs(Bin, []).

decode_vs(<<X/float,Y/float,Z/float,T/binary>>, Acc) ->
    decode_vs(T, [wings_util:share(X, Y, Z)|Acc]);
decode_vs(<<>>, Acc) -> reverse(Acc).

%% Reading of version 0 wings files.
old_import_objects(Shapes, St0) ->		% Version 0.
    foldl(fun ({object,Name,Matrix0,{winged,Fs,Vs},Props}, St) ->
		  HardEdges = case keysearch(hard_edges, 1, Props) of
				  {value,{_,HE}} -> HE;
				  false -> []
			      end,
		  Matrix1 = e3d_mat:compress(Matrix0),
		  Matrix = e3d_mat:mul(Matrix1, e3d_mat:scale(0.1, 0.1, 0.1)),
		  We = wings_we:build(Matrix, Fs, Vs, HardEdges),
		  wings_shape:new(Name, We, St)
	  end, St0, Shapes).

%% Save a Wings file (in version 1).

export(Name, St) ->
    Shapes = wings_util:fold_shape_all(fun shape/2, [], St),
    Materials = wings_material:used_materials(St),
    Wings = {wings,1,Shapes,Materials,[]},
    write_file(Name, term_to_binary(Wings, [compressed])).

write_file(Name, Bin) ->
    Data = <<?WINGS_HEADER,(size(Bin)):32,Bin/binary>>,
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

shape(#shape{name=Name,sh=We0}, Acc) ->
    #we{vs=Vs0,es=Etab,he=Htab} = We = wings_we:renumber(We0, 0),
    Vs1 = gb_trees:values(Vs0),
    Vs2 = [<<X/float,Y/float,Z/float>> || #vtx{pos={X,Y,Z}} <- Vs1],
    Vs = list_to_binary(Vs2),
    Fs1 = wings_util:fold_face(
	    fun(Face, #face{mat=Mat}, A) ->
		    [{Mat,wings_face:surrounding_vertices(Face, We)}|A]
	    end, [], We),
    Fs = reverse(Fs1),
    He0 = gb_sets:fold(fun(Edge, A) ->
			       #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			       [edge(Va,Vb)|A]
		       end, [], Htab),
    He = sort(He0),
    [{object,Name,{winged,Fs,Vs,He},[]}|Acc].

edge(Va, Vb) when Va < Vb -> {Va,Vb};
edge(Va, Vb) -> {Vb,Va}.

