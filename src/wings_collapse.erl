%%
%%  wings_collapse.erl --
%%
%%     This module contains the Collapse commands (for vertices,
%%     edges, and faces).
%%
%%  Copyright (c) 2001-2002 Jakob Cederlund, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_collapse.erl,v 1.17 2002/01/30 18:41:13 bjorng Exp $
%%

-module(wings_collapse).
-export([collapse/1,collapse_vertex/2,collapse_edge/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keymember/3,member/2]).

collapse(#st{selmode=face}=St0) ->
    {St,Sel} = wings_sel:mapfold(fun collapse_faces/3, [], St0),
    St#st{selmode=vertex,sel=reverse(Sel)};
collapse(#st{selmode=edge}=St0) ->
    {St,Sel} = wings_sel:mapfold(fun collapse_edges/3, [], St0),
    wings_sel:valid_sel(St#st{selmode=vertex,sel=reverse(Sel)});
collapse(#st{selmode=vertex}=St0) ->
    {St,Sel} = wings_sel:mapfold(fun collapse_vertices/3, [], St0),
    wings_sel:valid_sel(St#st{selmode=face,sel=reverse(Sel)}).

collapse_faces(Faces, #we{id=Id}=We0, SelAcc)->
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
    {NewV,We1}= wings_we:new_id(We0),
    #we{es=Es0,he=He0,fs=Fs0,vs=Vs0}= We1,

    %% Delete edges and vertices.
    {Es1,Vs1,Fs1,He1} =
	wings_face:fold(
	  fun(V, Edge, OldRec, A) ->
		  delete_edges(V, Edge, Face, A)
	  end, {Es0,Vs0,Fs0,He0}, Face, We1),

    %% Delete face.
    Fs2 = gb_trees:delete(Face, Fs1),

    %% Patch vertices references in edges surrounding the deleted vertices.
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
    Etab2 = case Rec of
		#edge{lf=Face,rf=Rf,rtpr=RP,rtsu=RS} ->
		    Etab1 = wings_edge:patch_edge(RP, RS, Rf, Edge, Etab0),
		    wings_edge:patch_edge(RS, RP, Rf, Edge, Etab1);
		#edge{rf=Face,lf=Lf,ltpr=LP,ltsu=LS} ->
		    Etab1 = wings_edge:patch_edge(LP, LS, Lf, Edge, Etab0),
		    wings_edge:patch_edge(LS, LP, Lf, Edge, Etab1)
	    end,

    %% Delete edge and vertex.
    Etab = gb_trees:delete(Edge, Etab2),
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
	    
collapse_edges(Edges0, #we{id=Id,es=Etab}=We0, SelAcc)->
    Edges = gb_sets:to_list(Edges0),
    We = foldl(fun collapse_edge/2, We0, Edges),
    Sel = foldl(fun(Edge, A) ->
			#edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			gb_sets:add(Va, gb_sets:add(Vb, A))
		end, gb_sets:empty(), Edges),
    {We,[{Id,Sel}|SelAcc]}.

collapse_edge(Edge, #we{es=Etab}=We)->
    case gb_trees:lookup(Edge, Etab) of
	{value,Rec} -> collapse_edge_1(Edge, Rec, We);
	none -> We
    end.

collapse_edge_1(Edge, Rec, #we{es=Etab0,he=Htab0,fs=Ftab0,vs=Vtab0}=We0)->
    #edge{vs=Vkeep,ve=Vremove} = Rec,
    case is_waist(Vkeep, Vremove, We0) of
	true -> We0;
	false ->
	    #edge{lf=LF,rf=RF,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,

	    %% Move kept vertex. Delete the other one.
	    Pos = wings_vertex:center([Vremove,Vkeep], Vtab0),
	    VkeepRec0 = gb_trees:get(Vkeep, Vtab0),
	    VkeepRec = VkeepRec0#vtx{edge=RP,pos=Pos},
	    Vtab1 = gb_trees:update(Vkeep, VkeepRec, Vtab0),
	    
	    %% Patch all predecessors and successor of
	    %% the edge we will remove.
	    Etab1 = wings_edge:patch_edge(LP, LS, LF, Edge, Etab0),
	    Etab2 = wings_edge:patch_edge(LS, LP, LF, Edge, Etab1),
	    Etab3 = wings_edge:patch_edge(RP, RS, RF, Edge, Etab2),
	    Etab4 = wings_edge:patch_edge(RS, RP, RF, Edge, Etab3),
	    
	    %% Patch the face entries for the surrounding faces.
	    Ftab1= wings_face:patch_face(LF, Edge, LP, Ftab0),
	    Ftab = wings_face:patch_face(RF, Edge, RP, Ftab1),
	    
	    %% Now we can safely remove the edge and vertex.
	    Etab5 = gb_trees:delete(Edge, Etab4),
	    Htab = wings_edge:hardness(Edge, soft, Htab0),
	    Vtab = gb_trees:delete(Vremove, Vtab1),
	    
	    %% ... change all references to the kept vertex.
	    %% We iterate on the original data structure, and operates
	    %% on our updated edge table.
	    {_,Etab} = patch_vtx_refs(Vremove, Vkeep, We0, {none,Etab5}),

	    We1 = We0#we{vs=Vtab,he=Htab,fs=Ftab,es=Etab},
	    We = foldl(fun(Face, bad_edge) -> bad_edge;
			  (Face, W) -> delete_degenerated(Face, W)
		       end, We1, [LF,RF]),
	    case We of
		bad_edge -> We0;
		_ -> We
	    end
    end.

%%
%% The Collapse command on vertices.
%%
collapse_vertices(Vs, #we{id=Id}=We0, SelAcc) ->
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
    VsEs = wings_vertex:fold(
	     fun(E, _, Rec, Acc0) ->
		     OtherV = wings_vertex:other(Vremove, Rec),
		     [{OtherV,E}|Acc0]
	     end, [], Vremove, We0),
    Edges = [E || {_,E} <- VsEs],
    Vlist = reverse([V || {V,_} <- VsEs]),
    check_vertices(Vlist),
    
    %% simple test to see if we need to connect vertices
    %% (we're just looking at closest from removed vertex,
    %% which probably isn't good enough)
    #vtx{pos=Pt} = gb_trees:get(Vremove, Vs0),
    Pairs = make_pairs(Vlist),
    We = foldl(fun(Pair, WeI) ->
		       wings_vertex_cmd:connect(Pair, WeI)
	       end, We0, Pairs),
    Sel = wings_vertex:fold(
	    fun(Edge, Face, Rec, A) ->
		    gb_sets:add(Face, A)
	    end, Sel0, Vremove, We),
    {wings_edge:dissolve_edges(Edges, We),Sel}.

make_pairs([H|_]=L) ->
    make_pairs(L, H, []).

make_pairs([A], F, Acc) -> [[A,F]|Acc];
make_pairs([A|[B|_]=T], F, Acc) -> make_pairs(T, F, [[A,B]|Acc]).

check_vertices(Vs0) ->
    check_vertices_1(sort(Vs0)).

check_vertices_1([V,V|_]) ->
    throw({command_error,"Non-collapsible vertex - would leave waist."});
check_vertices_1([V|Vs]) ->
    check_vertices(Vs);
check_vertices_1([]) -> ok.

%% Delete a degenerate face (a face consisting of only two edges).
delete_degenerated(Face, #we{fs=Ftab,es=Etab}=We) ->
    %% Note: The face could have been deleted by a previous
    %% wings_edge:dissolve_edge/2.
    case gb_trees:lookup(Face, Ftab) of
	{value,#face{edge=Edge}} ->
	    case gb_trees:get(Edge, Etab) of
		#edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same} ->
		    bad_edge;
		#edge{ltpr=Same,ltsu=Same} ->
		    wings_edge:dissolve_edge(Edge, We);
		#edge{rtpr=Same,rtsu=Same} ->
		    wings_edge:dissolve_edge(Edge, We);
		Other -> We
	    end;
	none -> We
    end.

is_waist(Va, Vb, We) ->
    N = wings_vertex:fold(
	  fun(_, _, Rec, N) ->
		  case wings_vertex:other(Va, Rec) of
		      Vb -> N+1;
		      Other -> N
		  end
	  end, 0, Va, We),
    N =/= 1.

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
