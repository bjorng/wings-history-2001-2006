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
%%     $Id: wings_ff_wings.erl,v 1.1.1.1 2001/08/14 18:16:37 bjorng Exp $
%%

-module(wings_ff_wings).
-export([import/2,export/2]).
-include("wings.hrl").
-import(lists, [map/2,reverse/1,foldl/3,keysearch/3]).

-define(WINGS_HEADER, "#!WINGS-1.0\r\n\032\04").

%% Load a wings file.

import(Name, St0) ->
    case file:read_file(Name) of
	{ok,<<?WINGS_HEADER,Sz:32,Data/binary>>} when size(Data) =:= Sz ->
	    case catch binary_to_term(Data) of
		{wings,0,Shapes} ->
		    case build_shapes(Shapes, St0) of
			#st{}=St -> St;
			Other -> {error,"bad objects in Wings file"}
		    end;
		{wings,_,_} ->
		    {error,"unknown version number in Wings file"};
		Other ->
		    io:format("~p\n", [Other]),
                    {error,"corrupt Wings file"}
	    end;
	{ok,Bin} ->
	    {error,"not a Wings file (or old Wings format)"};
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

build_shapes(Shapes, St0) ->
    foldl(fun ({object,Name,Matrix,{winged,Fs,Vs},Props}, St) ->
		  HardEdges = case keysearch(hard_edges, 1, Props) of
				  {value,{_,HE}} -> HE;
				  false -> []
			      end,
		  wings_we:build(Name, Matrix, Fs, Vs, HardEdges, St)
	  end, St0, Shapes).

%% Save a Wings file.

export(Name, St) ->
    Shapes = wings_util:fold_shape_all(fun shape/2, [], St),
    Wings = {wings,0,Shapes},
    write_file(Name, term_to_binary(Wings, [compressed])).

write_file(Name, Bin) ->
    Data = <<?WINGS_HEADER,(size(Bin)):32,Bin/binary>>,
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

shape(#shape{name=Name,sh=We0}, Acc) ->
    #we{vs=Vs0,es=Etab,he=Htab} = We = wings_we:renumber(We0, 0),
    Vs = [P || {V,#vtx{pos=P}} <- gb_trees:to_list(Vs0)],
    Fs1 = wings_util:fold_face(
	    fun(Face, #face{mat=Mat}, A) ->
		    [{Mat,wings_face:surrounding_vertices(Face, We)}|A]
	    end, [], We),
    Fs = reverse(Fs1),
    HardEdges = hard_edges(Etab, Htab),
    Identity = e3d_mat:identity(),
    [{object,Name,Identity,{winged,Fs,Vs},HardEdges}|Acc].

hard_edges(Etab, Htab) ->
    Hard = foldl(fun(Edge, Acc) ->
			 #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			 [{Va,Vb}|Acc]
		 end, [], gb_sets:to_list(Htab)),
    case Hard of
	[] -> [];
	List -> [{hard_edges,List}]
    end.
