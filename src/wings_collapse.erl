%%
%%  wings_collapse.erl --
%%
%%     This module contains the Collapse commands
%%     (for vertices, edges, and faces).
%%
%%  Copyright (c) 2001 Jakob Cederlund
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_collapse.erl,v 1.35 2003/09/26 07:30:23 bjorng Exp $
%%

-module(wings_collapse).
-export([collapse/1,collapse_vertices/2,collapse_edge/2,collapse_edge/3]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keymember/3,member/2]).

collapse(#st{selmode=face}=St0) ->
    {St,Sel} = wings_sel:mapfold(fun collapse_faces/3, [], St0),
    wings_sel:set(vertex, Sel, St);
collapse(#st{selmode=edge}=St0) ->
    {St,Sel} = wings_sel:mapfold(fun collapse_edges/3, [], St0),
    wings_sel:valid_sel(wings_sel:set(vertex, Sel, St));
collapse(#st{selmode=vertex}=St0) ->
    {St1,Sel} = wings_sel:mapfold(fun collapse_vertices_cmd/3, [], St0),
    case wings_sel:valid_sel(wings_sel:set(face, Sel, St1)) of
	#st{sel=[]}=St -> St#st{selmode=vertex};
	St -> St
    end.

collapse_faces(Faces, #we{id=Id}=We0, SelAcc)->
    We1 = foldl(fun collapse_face/2, We0, gb_sets:to_list(Faces)),
    We = wings_material:cleanup(We1),
    check_consistency(We),
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
    Vertices = wings_face:vertices_ccw(Face, We0),
    check_face_vertices(Vertices, We0),

    %% Allocate an Id for the new center vertex.
    {NewV,We1}= wings_we:new_id(We0),
    #we{es=Es0,he=He0,fs=Fs0,vc=Vct0,vp=Vs0}= We1,

    %% Delete edges and vertices.
    {Es1,Vct1,Vs1,Fs1,He1} =
	wings_face:fold(
	  fun(V, Edge, _OldRec, A) ->
		  delete_edges(V, Edge, Face, A)
	  end, {Es0,Vct0,Vs0,Fs0,He0}, Face, We1),

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
	    Vct = gb_trees:insert(NewV, AnEdge, Vct1),
	    Vs = gb_trees:insert(NewV, Pos, Vs1),
	    We2 = We1#we{vc=Vct,vp=Vs,es=Es2,fs=Fs2,he=He1},
	    We = wings_vertex:fold(
		   fun(_, _, _, bad_edge) -> bad_edge;
		      (_, F, _, W) -> wings_face:delete_if_bad(F, W)
		   end, We2, NewV, We2),

	    %% If no edges left, return the original object.
	    case We == bad_edge orelse gb_trees:is_empty(We#we.es) of
		true -> We0;
		false -> We
	    end
    end.

check_face_vertices([V|Vs], We) ->
    Vlist = wings_vertex:fold(
	      fun(_, _, Rec, Acc0) ->
		      OtherV = wings_vertex:other(V, Rec),
		      [OtherV|Acc0]
	      end, [], V, We),
    check_vertices(Vlist),
    check_face_vertices(Vs, We);
check_face_vertices([], _) -> ok.

delete_edges(V, Edge, Face, {Etab0,Vct0,Vtab0,Ftab0,Htab0}) ->
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
    Vct = gb_trees:delete(V, Vct0),
    Vtab = gb_trees:delete(V, Vtab0),

    %% Patch the face entry for the remaining face.
    Ftab = case Rec of
	       #edge{lf=Face,rf=AFace,rtpr=AnEdge} ->
		   wings_face:patch_face(AFace, Edge, AnEdge, Ftab0);
	       #edge{rf=Face,lf=AFace,ltpr=AnEdge} ->
		   wings_face:patch_face(AFace, Edge, AnEdge, Ftab0)
	   end,
    Htab = wings_edge:hardness(Edge, soft, Htab0),
    {Etab,Vct,Vtab,Ftab,Htab}.
	    
collapse_edges(Edges0, #we{id=Id,es=Etab}=We0, SelAcc)->
    Edges = gb_sets:to_list(Edges0),
    We = foldl(fun collapse_edge/2, We0, Edges),
    check_consistency(We),
    Sel = foldl(fun(Edge, A) ->
			#edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			gb_sets:add(Va, gb_sets:add(Vb, A))
		end, gb_sets:empty(), Edges),
    {We,[{Id,Sel}|SelAcc]}.

collapse_edge(Edge, #we{es=Etab}=We)->
    case gb_trees:lookup(Edge, Etab) of
	{value,#edge{vs=Vkeep}=Rec} -> 
	    collapse_edge_1(Edge, Vkeep, Rec, We);
	none -> We
    end.

collapse_edge(Edge, Vkeep, #we{es=Etab}=We)->
    case gb_trees:lookup(Edge, Etab) of
	{value,Rec} -> 
	    collapse_edge_1(Edge, Vkeep, Rec, We);
	none -> We
    end.

collapse_edge_1(Edge, Vkeep, Rec,
		#we{es=Etab0,he=Htab0,fs=Ftab0,vc=Vct0,vp=Vtab0}=We0)->
    case Rec of
	#edge{vs=Vkeep,ve=Vremove} -> ok;
	#edge{ve=Vkeep,vs=Vremove} -> ok
    end,
    case is_waist(Vkeep, Vremove, We0) of
	true -> We0;
	false ->
	    #edge{lf=LF,rf=RF,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,

	    %% Move kept vertex. Delete the other one.
	    Pos = wings_vertex:center([Vremove,Vkeep], Vtab0),
	    Vct1 = gb_trees:update(Vkeep, RP, Vct0),
	    Vtab1 = gb_trees:update(Vkeep, Pos, Vtab0),
	    
	    %% Patch all predecessors and successors of
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
	    Vct = gb_trees:delete(Vremove, Vct1),
	    Vtab = gb_trees:delete(Vremove, Vtab1),
	    
	    %% ... change all references to the kept vertex.
	    %% We iterate on the original data structure, and operates
	    %% on our updated edge table.
	    {_,Etab} = patch_vtx_refs(Vremove, Vkeep, We0, {none,Etab5}),

	    We1 = We0#we{vc=Vct,vp=Vtab,he=Htab,fs=Ftab,es=Etab},
	    We = foldl(fun(_Face, bad_edge) -> bad_edge;
			  (Face, W) -> wings_face:delete_if_bad(Face, W)
		       end, We1, [LF,RF]),
	    case We of
		bad_edge -> We0;
		_ -> We
	    end
    end.

%%
%% The Collapse command on vertices.
%%
collapse_vertices_cmd(Vs, #we{id=Id}=We0, SelAcc) ->
    {We,Sel} = do_collapse_vertices(gb_sets:to_list(Vs), We0),
    check_consistency(We),
    {We,[{Id,Sel}|SelAcc]}.

%% collapse_vertices(Vs, We) -> We'
%%  Remove vertices, replacing them with faces.
collapse_vertices(Vs, We0) ->
    {We,_} = do_collapse_vertices(Vs, We0),
    We.

do_collapse_vertices(Vs, We) ->
    do_collapse_vertices(Vs, We, gb_sets:empty(), []).

do_collapse_vertices([V|Vs], #we{vp=Vtab}=We0, Sel0, Acc) ->
    case gb_trees:is_defined(V, Vtab) of
	false ->
	    do_collapse_vertices(Vs, We0, Sel0, Acc);
	true ->
	    {We,Sel} = collapse_vertex_1(V, We0, Sel0),
	    do_collapse_vertices(Vs, We, Sel, [V|Acc])
    end;
do_collapse_vertices([], We, Sel, []) ->
    {We,Sel};
do_collapse_vertices([], We, Sel, Vs) ->
    %% Note that a vertex may be connected to two faces that
    %% have no edge in common. In that case, the vertex will
    %% still be there.
    do_collapse_vertices(Vs, We, Sel, []).

collapse_vertex_1(Vremove, We0, Sel0) ->
    VsEs = wings_vertex:fold(
	     fun(E, _, Rec, Acc0) ->
		     OtherV = wings_vertex:other(Vremove, Rec),
		     [{OtherV,E}|Acc0]
	     end, [], Vremove, We0),
    case VsEs of
	[_,_] ->
	    {wings_vertex:dissolve(Vremove, We0),Sel0};
	_ ->
	    Vlist = reverse([V || {V,_} <- VsEs]),
	    check_vertices(Vlist),

	    %% Connect vertices.
	    Pairs = make_pairs(Vlist),
	    We1 = foldl(fun(Pair, W) ->
				collapse_connect(Pair, W)
			end, We0, Pairs),

	    %% Remove all original edges.
	    Edges = [E || {_,E} <- VsEs],
	    We = wings_edge:dissolve_edges(Edges, We1),

	    Faces = collapse_vtx_faces(Vlist, We, []),
	    Sel = collapse_vtx_sel(Faces, ordsets:from_list(Vlist), We, Sel0),
	    {We,Sel}
    end.

collapse_connect(Pair, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Pair, We),
    foldl(fun({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
	     ({Face,Vs}, Acc) -> collapse_connect_1(Face, Vs, Acc)
	  end, We, FaceVs).

collapse_connect_1(Face, [Va,Vb], We0) ->
    case wings_vertex:edge_through(Va, Vb, Face, We0) of
	none ->
	    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
	    We;
	_ -> We0
    end;
collapse_connect_1(_, _, We) -> We.

collapse_vtx_faces([V|Vs], We, Acc0) ->
    Acc = wings_vertex:fold(
	    fun(_, Face, _, A) ->
		    [Face|A]
	    end, Acc0, V, We),
    collapse_vtx_faces(Vs, We, Acc);
collapse_vtx_faces([], _, Acc) ->
    ordsets:from_list(Acc).

collapse_vtx_sel([Face|Fs], NewVs, We, Sel) ->
    case ordsets:from_list(wings_face:vertices_ccw(Face, We)) of
	NewVs -> gb_sets:add(Face, Sel);
	_ -> collapse_vtx_sel(Fs, NewVs, We, Sel)
    end;
collapse_vtx_sel([], _, _, Sel) -> Sel.

make_pairs([H|_]=L) ->
    make_pairs(L, H, []).

make_pairs([A], F, Acc) -> [[A,F]|Acc];
make_pairs([A|[B|_]=T], F, Acc) -> make_pairs(T, F, [[A,B]|Acc]).

check_vertices(Vs0) ->
    check_vertices_1(sort(Vs0)).

check_vertices_1([V,V|_]) ->
    wings_util:error("Non-collapsible vertex (" ++ integer_to_list(V) ++
		     ") - would leave waist.\n");
check_vertices_1([_|Vs]) ->
    check_vertices(Vs);
check_vertices_1([]) -> ok.

is_waist(Va, Vb, We) ->
    N = wings_vertex:fold(
	  fun(_, _, Rec, N) ->
		  case wings_vertex:other(Va, Rec) of
		      Vb -> N+1;
		      _Other -> N
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

check_consistency(We) ->
    case wings_we:is_consistent(We) of
	true -> ok;
	false ->
	    Msg = "Collapsing would cause an inconsistent object structure.",
	    wings_util:error(Msg)
    end.

	    
