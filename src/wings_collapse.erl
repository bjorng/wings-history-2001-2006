%%
%%  wings_collapse.erl --
%%
%%     This module contains the Collapse commands (for vertices,
%%     edges, and faces).
%%
%%  Copyright (c) 2001 Jakob Cederlund, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_collapse.erl,v 1.2 2001/08/30 08:49:20 bjorng Exp $
%%

-module(wings_collapse).
-export([collapse/1,collapse_vertex/2,collapse_edge/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keymember/3,member/2]).
-import(wings_we, [new_id/1]).
-import(wings_mat, [norm_cross_product/2]).

collapse(St0) ->
    case St0#st.selmode of
	face ->
	    {St,Sel} = wings_sel:mapfold_shape(fun collapse_faces/4, [], St0),
	    St#st{selmode=vertex,sel=Sel};
	edge ->
	    {St,Sel} = wings_sel:mapfold_shape(fun collapse_edges/4, [], St0),
	    wings_sel:valid_sel(St#st{selmode=vertex,sel=Sel});
	vertex ->
	    {St,Sel} = wings_sel:mapfold_shape(fun collapse_vertices/4,
					       [], St0),
	    wings_sel:valid_sel(St#st{selmode=face,sel=Sel})
    end.

collapse_faces(Id, Faces, We0, SelAcc)->
    We = foldl(fun collapse_face/2, We0, gb_sets:to_list(Faces)),
    Sel = wings_we:new_items(vertex, We0, We),
    {We,[{Id,Sel}|SelAcc]}.

collapse_face(Face, #we{fs=Ftab}=We) ->
    %% This face could have have been removed earlier because it
    %% had only two edges left.
    case gb_trees:is_defined(Face, Ftab) of
	true -> collapse_face_1(Face, We);
	false -> We
    end.

collapse_face_1(Face, We0) ->
    Vertices = wings_face:surrounding_vertices(Face, We0),

    %% Allocate an Id for the new center vertex.
    {NewV,We1}= new_id(We0),
    #we{es=Es0,he=He0,fs=Fs0,vs=Vs0}= We1,

    %% Delete edges and vertices.
    {Es1,Vs1,Fs1,He1} =
	wings_face:fold(
	  fun(V, Edge, _, A) ->
		  delete_edges(V, Edge, Face, A)
	  end, {Es0,Vs0,Fs0,He0}, Face, We1),

    %% Delete face.
    Fs2 = gb_trees:delete(Face, Fs1),

    %% Patch vertices references in edges surronding the deleted vertices.
    {AnEdge,Es2} = foldl(fun(V, A) ->
				 patch_vtx_refs(V, NewV, We0, A)
			 end, {none,Es1}, Vertices),

    %% Insert the new vertex, if there are any edges left
    %% to connect it to.
    if
	AnEdge =:= none -> We0;
	true ->
	    Pos = wings_vertex:center(Vertices, We1),
	    Vs = gb_trees:insert(NewV, #vtx{edge=AnEdge,pos=Pos}, Vs1),
	    We2 = We1#we{vs=Vs,es=Es2,fs=Fs2,he=He1},
	    We = wings_vertex:fold(
		   fun(_, F, _, W) ->
			   delete_degenerated(F, W)
		   end, We2, NewV, We2),

	    %% If no edges left, return the original object.
	    case gb_trees:is_empty(We#we.es) of
		true -> We0;
		false -> We
	    end
    end.

delete_edges(V, Edge, Face, {Etab0,Vtab0,Ftab0,Htab0}) ->
    Rec = gb_trees:get(Edge, Etab0),

    %% Patch all predecessors and successor of
    %% the edge we will remove.
    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
    Etab1 = wings_edge:patch_edge(LP, LS, Edge, Etab0),
    Etab2 = wings_edge:patch_edge(LS, LP, Edge, Etab1),
    Etab3 = wings_edge:patch_edge(RP, RS, Edge, Etab2),
    Etab4 = wings_edge:patch_edge(RS, RP, Edge, Etab3),

    %% Delete edge and vertex.
    Etab = gb_trees:delete(Edge, Etab4),
    Vtab = gb_trees:delete(V, Vtab0),

    %% Patch the face entry for the remaining face.
    Ftab = case Rec of
	       #edge{lf=Face,rf=AFace,rtpr=AnEdge} ->
		   wings_face:patch_face(AFace, Edge, AnEdge, Ftab0);
	       #edge{rf=Face,lf=AFace,ltpr=AnEdge} ->
		   wings_face:patch_face(AFace, Edge, AnEdge, Ftab0)
	   end,
    Htab = wings_edge:hardness(Edge, soft, Htab0),
    {Etab,Vtab,Ftab,Htab}.

collapse_edges(Id, Edges0, #we{es=Etab}=We0, SelAcc)->
    Edges = gb_sets:to_list(Edges0),
    We = foldl(fun collapse_edge/2, We0, Edges),
    Sel = foldl(fun(Edge, A) ->
			#edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			gb_sets:add(Va, gb_sets:add(Vb, A))
		end, gb_sets:empty(), Edges),
    {We,[{Id,Sel}|SelAcc]}.

collapse_edge(Edge, #we{es=Etab0}=We0)->
    collapse_edge_1(gb_trees:lookup(Edge, Etab0), Edge, We0).

collapse_edge_1(none, Edge, We)-> We;
collapse_edge_1({value,Erec}, Edge,
		#we{es=Etab0, he=Htab0, fs=Ftab0, vs=Vtab0}=We0)->
    %% Get the edge to remove
    #edge{vs=Vkeep,ve=Vremove,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS,
	  lf=LF,rf=RF}= Erec,

    %% Check that we don't create a waist
    Edges = get_vertex_edges(Vkeep, We0) -- [Edge],
    case check_waist(Erec, Edges, Etab0) of
	true ->
	    We0;
	false ->

	    %% move kept vertex
	    Pt = wings_vertex:center([Vremove, Vkeep], Vtab0),
	    VkeepRec= gb_trees:get(Vkeep, Vtab0),
	    Vtab1= gb_trees:update(Vkeep, VkeepRec#vtx{pos= Pt}, Vtab0),
	    
	    %% Patch all predecessors and successor of
	    %% the edge we will remove.
	    Etab1 = wings_edge:patch_edge(LP, LS, Edge, Etab0),
	    Etab2 = wings_edge:patch_edge(LS, LP, Edge, Etab1),
	    Etab3 = wings_edge:patch_edge(RP, RS, Edge, Etab2),
	    Etab4 = wings_edge:patch_edge(RS, RP, Edge, Etab3),
	    
	    %% Patch the vertices referring to edge.
	    Vtab2 = wings_vertex:patch_vertex(Vkeep, RP, Vtab1),
	    
	    %% Patch the face entries for the surrounding faces.
	    Ftab1= wings_face:patch_face(LF, Edge, LP, Ftab0),
	    Ftab = wings_face:patch_face(RF, Edge, RP, Ftab1),
	    
	    %% now we can safely remove the edge
	    Etab5 = gb_trees:delete(Edge, Etab4),
	    Htab = wings_edge:hardness(Edge, soft, Htab0),
	    
	    %% Remove vertex...
	    Vtab = gb_trees:delete(Vremove, Vtab2),
	    
	    %% ... change all references to the kept vertex.
	    %% We iterate on the original data structure, and operates
	    %% on our updated edge table.
	    {_,Etab} = patch_vtx_refs(Vremove, Vkeep, We0, {none,Etab5}),

	    We1 = We0#we{vs = Vtab, he = Htab, fs = Ftab, es= Etab},
	    Lredges = get_edges(RF, We1)++get_edges(LF, We1),
	    We2 = remove_1vertex_edges(Lredges, We1),
	    case remove_2side_faces([RF, LF], We2) of
		bad_edge -> We0;
		We3 -> We3
	    end
    end.

%%
%% The Collapse command on vertices.
%%
collapse_vertices(Id, Vs, We0, SelAcc) ->
    {We,Sel} = gb_sets:fold(fun(V, {W,S}) ->
				    do_collapse_vertex(V, W, S)
			    end, {We0,gb_sets:empty()}, Vs),
    {We,[{Id,Sel}|SelAcc]}.

%% collapse_vertex(V, We) -> We'
%%  Remove a vertex, replacing it with a face.
collapse_vertex(V, We0) ->
    {We,_} = do_collapse_vertex(V, We0, gb_sets:empty()),
    We.

do_collapse_vertex(V, We0, Sel0) ->
    %% Handle winged vertices (i.e. vertices with two edges) specially.
    case wings_vertex:dissolve(V, We0) of
	error -> collapse_vertex_1(V, We0, Sel0);
	We -> {We,Sel0}
    end.
	    
collapse_vertex_1(Vremove, #we{es=Es0,vs=Vs0,fs=Fs0,he=He0}=We0, Sel0)->
    %% start removing faces and edges
    Edges = get_vertex_edges(Vremove, We0),
    Vlist = [wings_vertex:other(Vremove, gb_trees:get(E, Es0)) || E <- Edges],

    %% simple test to see if we need to connect vertices
    %% (we're just looking at closest from removed vertex,
    %% which probably isn't good enough)
    #vtx{pos=Pt} = gb_trees:get(Vremove, Vs0),
    Vclosest = get_closest(Pt, Vlist, Vs0), % get vertices sorted by closeness
    Onsameplane = get_on_same_plane(Vclosest, Vlist, Vs0),
    We = case Onsameplane of % check points outside that plane
	     Vlist -> We0;% we're done (all points on same plane)
	     _ ->	% yes, we need to connect them
		 Pairs = make_pairs(Onsameplane),
		 foldl(fun(Pair, We00) ->
			       wings_vertex_cmd:connect(Pair, We00)
		       end, We0, Pairs)
	 end,
    Sel = wings_vertex:fold(
	    fun(Edge, Face, Rec, A) ->
		    gb_sets:add(Face, A)
	    end, Sel0, Vremove, We),
    {wings_edge:dissolve_edges(Edges, We),Sel}.

make_pairs(L) when list(L) ->
    make_pairs(L, hd(L), []).

make_pairs([A], F, Acc) ->
    [[A,F] | Acc];
make_pairs([A,B | Rest], F, Acc) ->
    make_pairs([B | Rest], F, [[A,B] | Acc]).


%% remove all edges with same start and end vertex
remove_1vertex_edges([], We) ->
    We;
remove_1vertex_edges([Edge|Erest], #we{es=Etab0}=We0) ->
    We1 = case gb_trees:lookup(Edge, Etab0) of
	      {value,#edge{vs=V,ve=V}} ->
		  wings_edge:dissolve_edge(Edge, We0);
	      _ ->
		  We0
	  end,
    remove_1vertex_edges(Erest, We1).

%% Delete a degenerate face (a face consisting of only two edges).
delete_degenerated(Face, #we{fs=Ftab,es=Etab}=We) ->
    %% Note: The face could have been deleted by a previous
    %% wings_edge:dissolve_edge/2.
    case gb_trees:lookup(Face, Ftab) of
	{value,#face{edge=Edge}} ->
	    case gb_trees:get(Edge, Etab) of
		#edge{ltpr=Same,ltsu=Same} ->
		    wings_edge:dissolve_edge(Edge, We);
		#edge{rtpr=Same,rtsu=Same} ->
		    wings_edge:dissolve_edge(Edge, We);
		Other -> We
	    end;
	none -> We
    end.

%% Remove all faces with just two edges (in a given list of faces).
remove_2side_faces([], We) ->
    We;
remove_2side_faces([Face|Frest], #we{fs=Ftab0,es=Etab0}=We0) ->
    case gb_trees:lookup(Face, Ftab0) of
	{value,Frec} ->
	    Edge = Frec#face.edge,
	    case gb_trees:get(Edge, Etab0) of
		#edge{ltpr=E0,ltsu=E0,rtpr=E0,rtsu=E0} ->
		    bad_edge;
		#edge{ltpr=E1,ltsu=E1} = Erec ->
		    remove_2side_faces(Frest, wings_edge:dissolve_edge(Edge, We0));
		#edge{rtpr=E2,rtsu=E2} = Erec ->
		    remove_2side_faces(Frest, wings_edge:dissolve_edge(Edge, We0));
		_ ->
		    remove_2side_faces(Frest, We0)
	    end;
	none ->
	    remove_2side_faces(Frest, We0)
    end.

get_closest(Pt, Vlist, Vtab) ->
    DistV = [{wings_mat:distance(Pt, wings_vertex:pos(V, Vtab)), V} ||
		V <- Vlist],
    [V || {_,V} <- lists:sort(DistV)].

get_on_same_plane([V1,V2,V3 | _], Vlist, Vtab) ->
    Pt1 = wings_vertex:pos(V1, Vtab),
    P12 = wings_mat:subtract(Pt1, wings_vertex:pos(V2, Vtab)),
    P13 = wings_mat:subtract(Pt1, wings_vertex:pos(V3, Vtab)),
    Normcross = norm_cross_product(P12, P13),
    foldl(fun(V, Planelist0) ->
		  case V of
		      V1 -> [V|Planelist0];
		      V2 -> [V|Planelist0];
		      V3 -> [V|Planelist0];
		      _ ->
			  P14 = wings_mat:subtract(Pt1, wings_vertex:pos(V, Vtab)),
			  case same_or_neg_pt(Normcross, norm_cross_product(P12, P14)) of
			      true -> [V | Planelist0];
			      false -> Planelist0
			  end
		  end
	  end, [], Vlist).

same_or_neg_pt(Pt1, Pt2) ->
    (wings_mat:distance(Pt1, Pt2) < 1.0E-5) or
	(wings_mat:distance(Pt1, wings_mat:negate(Pt2)) < 1.0E-5).

check_waist(Erec, [], Etab0) ->
    false;
check_waist(#edge{vs=Vs0,ve=Ve0}=Erec, [Edge1|Erest], Etab0) ->
    #edge{vs=Vs,ve=Ve}= gb_trees:get(Edge1, Etab0),
    case {Vs0,Ve0} of
	{Vs,Ve} ->
	    true;
	{Ve,Vs} ->
	    true;
	_ ->
	    check_waist(Erec, Erest, Etab0)
    end.

patch_vtx_refs(OldV, NewV, We, {_,_}=Acc) ->
    wings_vertex:fold(
      fun(Edge, _, _, {_,Tab}=A) ->
	      case gb_trees:lookup(Edge, Tab) of
		  {value,#edge{vs=OldV}=Rec} ->
		      {Edge,gb_trees:update(Edge, Rec#edge{vs=NewV}, Tab)};
		  {value,#edge{ve=OldV}=Rec} ->
		      {Edge,gb_trees:update(Edge, Rec#edge{ve=NewV}, Tab)};
		  none -> A		%An deleted edge.
	      end
      end, Acc, OldV, We).

get_edges(Face, We) ->
    wings_face:fold(fun(_, Edge, Rec, Acc0) ->
			    [{Edge,Rec}|Acc0]
		    end, [], Face, We).

get_vertex_edges(V, We) ->
    wings_vertex:fold(
      fun(E, _, _, Acc0) ->
	      [E|Acc0]
      end, [], V, We).
