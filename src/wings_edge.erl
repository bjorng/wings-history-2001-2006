%%
%%  wings_edge.erl --
%%
%%     This module contains most edge command and edge utility functions.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_edge.erl,v 1.12 2001/09/24 07:24:53 bjorng Exp $
%%

-module(wings_edge).
-export([convert_selection/1,select_more/1,select_less/1,
	 to_vertices/2,
	 select_loop/1,cut/2,cut/3,fast_cut/3,connect/1,dissolve/1,
	 dissolve_edges/2,dissolve_edge/2,patch_edge/4,patch_edge/5,
	 hardness/2,hardness/3,loop_cut/1]).

-include("wings.hrl").
-import(lists, [foldl/3,last/1,member/2,reverse/1,reverse/2,seq/2,sort/1]).

%%
%% Convert the current selection to an edge selection.
%%
convert_selection(#st{selmode=body}=St) ->
    wings_sel:convert_shape(
      fun(_, #we{es=Etab}) ->
	      gb_sets:from_list(gb_trees:keys(Etab))
      end, edge, St);
convert_selection(#st{selmode=face}=St) ->
    wings_sel:convert(
      fun(Face, We, Sel0) ->
	      wings_face:fold(
		fun(_, Edge, _, Sel) ->
			gb_sets:add(Edge, Sel)
		end, Sel0, Face, We)
      end, edge, St);
convert_selection(#st{selmode=edge}=St) ->
    wings_sel:convert_shape(
      fun(Edges, #we{es=Etab}) ->
	      gb_sets:fold(
		fun(Edge, S0) ->
			Rec = gb_trees:get(Edge, Etab),
			#edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
			gb_sets:union(S0, gb_sets:from_list([LP,LS,RP,RS]))
		end, Edges, Edges)
      end, edge, St);
convert_selection(#st{selmode=vertex}=St) ->
    wings_sel:convert(
      fun(V, We, Sel0) ->
	      wings_vertex:fold(
		fun(Edge, _, _, Sel) ->
			gb_sets:add(Edge, Sel)
		end, Sel0, V, We)
      end, edge, St).

%%% Select more or less.

select_more(St) ->
    wings_sel:convert_shape(
      fun(Edges, We) ->
	      Vs = to_vertices(Edges, We),
	      adjacent_edges(Vs, We, Edges)
      end, edge, St).

adjacent_edges(Vs, We, Acc) ->
    foldl(fun(V, A) ->
		  wings_vertex:fold(
		    fun(Edge, _, _, AA) ->
			    gb_sets:add(Edge, AA)
		    end, A, V, We)
	  end, Acc, Vs).


select_less(St) ->
    wings_sel:convert_shape(
      fun(Edges, #we{es=Etab}) ->
	      gb_sets:fold(
		fun(Edge, A) ->
			Rec = gb_trees:get(Edge, Etab),
			#edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
			Set = gb_sets:from_list([LP,LS,RP,RS]),
			case gb_sets:is_subset(Set, Edges) of
			    true -> A;
			    false -> gb_sets:delete(Edge, A)
			end
		end, Edges, Edges)
      end, edge, St).

%% to_vertices(EdgeGbSet, We) -> VertexGbSet
%%  Convert a set of edges to a set of vertices.
to_vertices(Edges, #we{es=Etab}) ->
    to_vertices(gb_sets:to_list(Edges), Etab, []).

to_vertices([], Etab, Acc) -> ordsets:from_list(Acc);
to_vertices([E|Es], Etab, Acc) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
    to_vertices(Es, Etab, [Va,Vb|Acc]).

%%% The Select Edge Loop command.

select_loop(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold_shape(fun select_loop/3, [], St),
    St#st{sel=reverse(Sel)};
select_loop(St) -> St.

select_loop(#shape{id=Id,sh=#we{es=Etab}=We}, Edges0, Acc) ->
    Edges = select_loop_1(Edges0, Etab, gb_sets:empty()),
    [{Id,Edges}|Acc].

select_loop_1(Edges0, Etab, Sel0) ->
    case gb_sets:is_empty(Edges0) of
	true -> Sel0;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    Sel = gb_sets:insert(Edge, Sel0),
	    Edges = select_loop_edges(Edge, Etab, Sel, Edges1),
	    select_loop_1(Edges, Etab, Sel)
    end.

select_loop_edges(Edge, Etab, Sel, Edges0) ->
    #edge{vs=Va,ve=Vb} = Erec = gb_trees:get(Edge, Etab),
    Edges = try_edge_from(Va, Edge, Erec, Etab, Sel, Edges0),
    try_edge_from(Vb, Edge, Erec, Etab, Sel, Edges).

try_edge_from(V, FromEdge, Erec, Etab, Sel, Edges) ->
    case try_edge_from_1(V, FromEdge, Erec, Etab) of
	none -> Edges;
	Edge ->
	    case gb_sets:is_member(Edge, Sel) of
		true -> Edges;
		false -> gb_sets:add(Edge, Edges)
	    end
    end.

try_edge_from_1(V, From, Erec, Etab) ->
    case Erec of
	#edge{vs=V,lf=FL,rf=FR,ltsu=EL,rtpr=ER} -> ok;
	#edge{ve=V,lf=FL,rf=FR,ltpr=EL,rtsu=ER} -> ok
    end,
    if
	EL =:= ER -> EL;
	true ->
	    case {next_edge(From, V, FL, EL, Etab),
		  next_edge(From, V, FR, ER, Etab)} of
		{Edge,Edge} -> Edge;
		{_,_} -> none
	    end
    end.

next_edge(From, V, Face, Edge, Etab) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,ve=Ov,rf=Face,rtpr=From,ltsu=To} -> To;
	#edge{vs=V,ve=Ov,lf=Face,ltsu=From,rtpr=To} -> To;
	#edge{ve=V,vs=Ov,rf=Face,rtsu=From,ltpr=To} -> To;
	#edge{ve=V,vs=Ov,lf=Face,ltpr=From,rtsu=To} -> To
    end.

%%%
%%% The Cut command.
%%%

cut(N, #st{selmode=edge}=St0) when N > 1 ->
    {St,Sel} = wings_sel:mapfold_shape(
		 fun(Id, Edges, We0, Acc) ->
			 We = cut_edges(Edges, N, We0),
			 S = wings_we:new_items(vertex, We0, We),
			 {We,[{Id,S}|Acc]}
		 end, [], St0),
    St#st{selmode=vertex,sel=reverse(Sel)};
cut(N, St) -> St.

cut_edges(Edges, N, We0) ->
    gb_sets:fold(fun(Edge, W0) ->
			 {We,_} = cut(Edge, N, W0),
			 We
		 end, We0, Edges).

%% cut(Edge, Parts, We0) -> {We,NewVertex,NewEdge}
%%  Cut an edge into Parts parts.
cut(Edge, 2, We) ->
    fast_cut(Edge, default, We);
cut(Edge, N, We0) ->
    NumIds = (N-1),
    {BaseId,We} = wings_we:new_ids(NumIds, We0),
    #we{es=Etab0,vs=Vtab0,he=Htab0} = We,
    #edge{vs=Vstart,ve=Vend} = Template = gb_trees:get(Edge, Etab0),
    Vtab1 = make_vertices(N, BaseId, Vstart, Vend, Vtab0),
    {Etab1,EdgeA,EdgeB} = make_edges(N, BaseId, Template, Edge, Etab0),

    LastEdge = BaseId+NumIds-1,
    VendRec = gb_trees:get(Vend, Vtab1),
    Vtab = gb_trees:update(Vend, VendRec#vtx{edge=LastEdge}, Vtab1),
    Etab2 = patch_edge(EdgeA, LastEdge, Edge, Etab1),
    Etab3 = patch_edge(EdgeB, LastEdge, Edge, Etab2),
    
    NewEdge = Template#edge{ve=BaseId,rtsu=BaseId,ltpr=BaseId},
    Etab = gb_trees:update(Edge, NewEdge, Etab3),

    Htab = case gb_sets:is_member(Edge, Htab0) of
	       false -> Htab0;
	       true ->
		   Hard = gb_sets:from_list(seq(BaseId, BaseId+NumIds)),
		   gb_sets:union(Hard, Htab0)
	   end,
    {We#we{es=Etab,vs=Vtab,he=Htab},BaseId}.
		    
make_edges(2, Id, #edge{ltpr=EdgeA,rtsu=EdgeB}=Template, Prev, Etab) ->
    ThisEdge = Id,
    New = Template#edge{vs=Id,ltsu=Prev,rtpr=Prev},
    {gb_trees:insert(ThisEdge, New, Etab),EdgeA,EdgeB};
make_edges(N, Id, Template, Prev, Etab0) ->
    ThisEdge = Id,
    New = Template#edge{vs=Id,ve=Id+1,ltsu=Prev,rtpr=Prev,
			ltpr=ThisEdge+1,rtsu=ThisEdge+1},
    Etab = gb_trees:insert(ThisEdge, New, Etab0),
    make_edges(N-1, Id+1, Template, ThisEdge, Etab).

make_vertices(N, Id, Vstart, Vend, Vtab) ->
    Va = wings_vertex:pos(Vstart, Vtab),
    Vb = wings_vertex:pos(Vend, Vtab),
    Dir = e3d_vec:divide(e3d_vec:sub(Vb, Va), float(N)),
    make_vertices_1(N, Id, Va, Dir, Vtab).
    
make_vertices_1(1, Id, Va, Dir, Vtab) -> Vtab;
make_vertices_1(N, Id, Va, Dir, Vtab0) ->
    NextPos = wings_util:share(e3d_vec:add(Va, Dir)),
    Vtx = #vtx{pos=NextPos,edge=Id},
    Vtab = gb_trees:insert(Id, Vtx, Vtab0),
    make_vertices_1(N-1, Id+1, NextPos, Dir, Vtab).

%% fast_cut(Edge, Position, We0) -> {We,NewVertex,NewEdge}
%%  Cut an edge in two parts. Position can be given as
%%  the atom `default', in which case the position will
%%  be set to the midpoint of the edge.
fast_cut(Edge, Pos, We0) ->
    {NewV,We} = wings_we:new_ids(1, We0),
    NewEdge = NewV,
    #we{es=Etab0,vs=Vtab0,he=Htab0} = We,
    Template = gb_trees:get(Edge, Etab0),
    #edge{vs=Vstart,ve=Vend,ltpr=EdgeA,rtsu=EdgeB} = Template,

    #vtx{pos=VendPos,edge=VendEdge}= VendRec = gb_trees:get(Vend, Vtab0),
    Vtab1 = if
		VendEdge =:= Edge ->
		    gb_trees:update(Vend, VendRec#vtx{edge=NewEdge}, Vtab0);
		true -> Vtab0
	    end,
    NewVPos0 = if
		   Pos =:= default ->
		       VstartPos = wings_vertex:pos(Vstart, Vtab0),
		       e3d_vec:average([VstartPos,VendPos]);
		   true -> Pos
	       end,
    NewVPos = wings_util:share(NewVPos0),
    Vtx = #vtx{pos=NewVPos,edge=NewEdge},
    Vtab = gb_trees:insert(NewV, Vtx, Vtab1),

    NewEdgeRec = Template#edge{vs=NewV,ltsu=Edge,rtpr=Edge},
    Etab1 = gb_trees:insert(NewEdge, NewEdgeRec, Etab0),
    Etab2 = patch_edge(EdgeA, NewEdge, Edge, Etab1),
    Etab3 = patch_edge(EdgeB, NewEdge, Edge, Etab2),
    EdgeRec = Template#edge{ve=NewV,rtsu=NewEdge,ltpr=NewEdge},
    Etab = gb_trees:update(Edge, EdgeRec, Etab3),

    Htab = case gb_sets:is_member(Edge, Htab0) of
	       false -> Htab0;
	       true -> gb_sets:insert(NewEdge, Htab0)
	   end,
    {We#we{es=Etab,vs=Vtab,he=Htab},NewV}.

%%%
%%% The Connect command.
%%%

connect(St0) ->
    {St,Sel} = wings_sel:mapfold_shape(fun connect/4, [], St0),
    St#st{sel=Sel}.

connect(Id, Es, We0, Acc) ->
    Iter = gb_sets:iterator(Es),
    {We1,Vs} = cut_edges(Es, We0),
    We = wings_vertex_cmd:connect(Vs, We1),
    Sel = wings_we:new_items(edge, We1, We),
    {We,[{Id,Sel}|Acc]}.

cut_edges(Es, We) ->
    gb_sets:fold(fun(Edge, {W0,Vs0}) ->
			 {W,V} = cut(Edge, 2, W0),
			 {W,[V|Vs0]}
		 end, {We,[]}, Es).

%%%
%%% The Dissolve command.
%%%

dissolve(St0) ->
    St = wings_sel:map_shape(fun dissolve_edges/2, St0),
    St#st{sel=[]}.

dissolve_edges(Edges0, We0) when list(Edges0) ->
    #we{es=Etab} = We = foldl(fun dissolve_edge/2, We0, Edges0),
    case [E || E <- Edges0, gb_trees:is_defined(E, Etab)] of
	Edges0 -> We;
	Edges -> dissolve_edges(Edges, We)
    end;
dissolve_edges(Edges, We) -> 
    dissolve_edges(gb_sets:to_list(Edges), We).

dissolve_edge(Edge, #we{es=Etab}=We0) ->
    case gb_trees:lookup(Edge, Etab) of
	none -> We0;
	{value,#edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same}} ->
	    Empty = gb_trees:empty(),
	    #we{vs=Empty,es=Empty,fs=Empty,he=gb_sets:empty()};
	{value,#edge{rtpr=Back,ltsu=Back}=Rec} ->
	    merge_edges(backward, Edge, Rec, We0);
	{value,#edge{rtsu=Forward,ltpr=Forward}=Rec} ->
	    merge_edges(forward, Edge, Rec, We0);
	{value,Rec} -> 
	    case catch dissolve_edge(Edge, Rec, We0) of
		{'EXIT',Reason} -> exit(Reason);
		hole -> We0;
		We -> We
	    end
    end.

%% dissolve_edge(Edge, EdgeRecord, We) -> We
%%  Remove an edge and a face. If one of the faces is degenerated
%%  (only consists of two edges), remove that one. Otherwise, it
%%  doesn't matter which face we remove.
dissolve_edge(Edge, #edge{lf=Remove,rf=Keep,ltpr=Same,ltsu=Same}=Rec, We) ->
    dissolve_edge(Edge, Remove, Keep, Rec, We);
dissolve_edge(Edge, #edge{lf=Keep,rf=Remove}=Rec, We) ->
    dissolve_edge(Edge, Remove, Keep, Rec, We).

dissolve_edge(Edge, FaceRemove, FaceKeep, Rec,
	      #we{fs=Ftab0,es=Etab0,vs=Vtab0,he=Htab0}=We0) ->
    #edge{vs=Vstart,ve=Vend,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,

    %% First change face for all edges surrounding the face we will remove.
    Etab1 =
	wings_face:fold(
	  fun (V, E, R, IntEtab) when E =:= Edge -> IntEtab;
	      (V, E, R, IntEtab) ->
		  case R of
 		      #edge{lf=FaceRemove,rf=FaceKeep} ->
 			  throw(hole);
 		      #edge{rf=FaceRemove,lf=FaceKeep} ->
 			  throw(hole);
		      #edge{lf=FaceRemove} ->
			  gb_trees:update(E, R#edge{lf=FaceKeep}, IntEtab);
		      #edge{rf=FaceRemove} ->
			  gb_trees:update(E, R#edge{rf=FaceKeep}, IntEtab)
		  end
	  end, Etab0, FaceRemove, We0),

    %% Patch all predecessors and successor of the edge we will remove.
    Etab2 = patch_edge(LP, RS, Edge, Etab1),
    Etab3 = patch_edge(LS, RP, Edge, Etab2),
    Etab4 = patch_edge(RP, LS, Edge, Etab3),
    Etab5 = patch_edge(RS, LP, Edge, Etab4),

    %% Remove the edge.
    Etab = gb_trees:delete(Edge, Etab5),
    Htab = hardness(Edge, soft, Htab0),

    %% Remove the face. Patch the face entry for the remaining face.
    Ftab1 = gb_trees:delete(FaceRemove, Ftab0),
    FaceRec = gb_trees:get(FaceKeep, Ftab1),
    Ftab = gb_trees:update(FaceKeep, FaceRec#face{edge=LP}, Ftab1),

    %% Patch the vertices referenced by the removed edge.
    Vtab1 = wings_vertex:patch_vertex(Vstart, RP, Vtab0),
    Vtab = wings_vertex:patch_vertex(Vend, RS, Vtab1),

    %% Return result.
    We = We0#we{es=Etab,fs=Ftab,vs=Vtab,he=Htab},
    #face{edge=AnEdge} = gb_trees:get(FaceKeep, Ftab),
    case gb_trees:get(AnEdge, Etab) of
	#edge{lf=FaceKeep,ltpr=Same,ltsu=Same} ->
	    dissolve_edge(AnEdge, We);
	#edge{rf=FaceKeep,rtpr=Same,rtsu=Same} ->
	    dissolve_edge(AnEdge, We);
	Other -> We
    end.

%%
%% We like winged edges, but not winged vertices (a vertex with
%% only two edges connected to it). We will remove the winged vertex
%% by joining the two edges connected to it.
%%

merge_edges(Dir, Edge, Rec, #we{es=Etab}=We) ->
    {Va,Vb,_,_,To,To} = half_edge(Dir, Rec),
    case gb_trees:get(To, Etab) of
	#edge{vs=Va,ve=Vb} ->
	    del_2edge_face(Dir, Edge, Rec, To, We);
	#edge{vs=Vb,ve=Va} ->
	    del_2edge_face(Dir, Edge, Rec, To, We);
	Other ->
	    merge(Dir, Edge, Rec, To, We)
    end.

merge(Dir, Edge, Rec, To, #we{es=Etab0,vs=Vtab0,fs=Ftab0,he=Htab0}=We) ->
    OtherDir = reverse_dir(Dir),
    {Vkeep,Vdelete,Lf,Rf,L,R} = half_edge(OtherDir, Rec),
    Etab1 = patch_edge(L, To, Edge, Etab0),
    Etab2 = patch_edge(R, To, Edge, Etab1),
    Etab3 = patch_half_edge(To, Vkeep, Lf, L, Rf, R, Vdelete, Etab2),
    Htab = hardness(Edge, soft, Htab0),
    Etab = gb_trees:delete(Edge, Etab3),
    Vtab1 = gb_trees:delete(Vdelete, Vtab0),
    Vtab = wings_vertex:patch_vertex(Vkeep, To, Vtab1),
    #edge{lf=Lf,rf=Rf} = Rec,
    Ftab1 = update_face(Lf, To, Edge, Ftab0),
    Ftab = update_face(Rf, To, Edge, Ftab1),
    check_edge(To, We#we{es=Etab,vs=Vtab,fs=Ftab,he=Htab}).

check_edge(Edge, #we{es=Etab}=We) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ltpr=Same,ltsu=Same} ->
	    dissolve_edge(Edge, We);
	#edge{rtpr=Same,rtsu=Same} ->
	    dissolve_edge(Edge, We);
	Other -> We
    end.

update_face(Face, Edge, OldEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	#face{edge=OldEdge}=Frec ->
	    gb_trees:update(Face, Frec#face{edge=Edge}, Ftab);
	Other -> Ftab
    end.

del_2edge_face(Dir, EdgeA, RecA, EdgeB,
	       #we{vs=Vtab0,es=Etab0,fs=Ftab0,he=Htab0}=We) ->
    {Vkeep,Vdelete,Lf,Rf,_,_} = half_edge(reverse_dir(Dir), RecA),
    RecB = gb_trees:get(EdgeB, Etab0),
    Del = gb_sets:from_list([EdgeA,EdgeB]),
    EdgeANear = stabile_neighbor(RecA, Del),
    EdgeBNear = stabile_neighbor(RecB, Del),
    Etab1 = patch_edge(EdgeANear, EdgeBNear, EdgeA, Etab0),
    Etab2 = patch_edge(EdgeBNear, EdgeANear, EdgeB, Etab1),
    Etab3 = gb_trees:delete(EdgeA, Etab2),
    Etab = gb_trees:delete(EdgeB, Etab3),

    %% Patch hardness table.
    Htab1 = hardness(EdgeA, soft, Htab0),
    Htab = hardness(EdgeB, soft, Htab1),

    %% Patch vertex table.
    Vtab1 = gb_trees:delete(Vdelete, Vtab0),
    Vtab = wings_vertex:patch_vertex(Vkeep, EdgeANear, Vtab1),

    %% Patch the face table.
    #edge{lf=Klf,rf=Krf} = RecANear = gb_trees:get(EdgeANear, Etab),
    KeepFaces = ordsets:from_list([Klf,Krf]),
    EdgeAFaces = ordsets:from_list([Lf,Rf]),
    [DelFace] = ordsets:subtract(EdgeAFaces, KeepFaces),
    Ftab1 = gb_trees:delete(DelFace, Ftab0),
    [KeepFace] = ordsets:intersection(KeepFaces, EdgeAFaces),
    Ftab2 = update_face(KeepFace, EdgeANear, EdgeA, Ftab1),
    Ftab = update_face(KeepFace, EdgeBNear, EdgeB, Ftab2),

    %% Return result.
    We#we{vs=Vtab,es=Etab,fs=Ftab,he=Htab}.

stabile_neighbor(#edge{ltpr=Ea,ltsu=Eb,rtpr=Ec,rtsu=Ed}, Del) ->
    [Edge] = foldl(fun(E, A) ->
			   case gb_sets:is_member(E, Del) of
			       true -> A;
			       false -> [E|A]
			   end
		   end, [], [Ea,Eb,Ec,Ed]),
    Edge.

%%%
%%% The Hardness command
%%%

hardness(Hardness, St) ->
    wings_sel:map(fun(Edge, #we{he=Htab0}=We) ->
			  Htab = hardness(Edge, Hardness, Htab0),
			  We#we{he=Htab}
		  end, St).

hardness(Edge, Hardness, Htab) ->
    case {Hardness,gb_sets:is_member(Edge, Htab)} of
	{soft,false} -> Htab;
	{hard,true} -> Htab;
	{soft,true} -> gb_sets:delete(Edge, Htab);
	{hard,false} -> gb_sets:insert(Edge, Htab)
    end.

%%%
%%% The Loop Cut command.
%%%

loop_cut(#st{sel=OldSel}=St0) ->
    {Sel0,St1} = wings_sel:fold_shape(fun loop_cut/3, {[],St0}, St0),
    Sel = sort(Sel0),
    St2 = St1#st{selmode=face,sel=Sel},
    St = wings_face_cmd:dissolve(St2),
    wings_body:convert_selection(St#st{selmode=body,sel=OldSel}).

loop_cut(#shape{id=Id,sh=We0}=Sh, Edges, {Sel0,#st{onext=NewId}=St0}=Acc) ->
    #we{es=Etab,fs=Ftab} = We0,
    {AnEdge,_} = gb_sets:take_smallest(Edges),
    #edge{lf=Lf,rf=Rf} = gb_trees:get(AnEdge, Etab),
    LeftFaces = collect_faces(Lf, Edges, We0),
    RightFaces = collect_faces(Rf, Edges, We0),
    case proper_division(LeftFaces, RightFaces, We0) of
	false -> Acc;				%improper division
	true ->
	    St = wings_shape:insert(Sh, "cut", St0),
	    Sel = [{Id,LeftFaces},{NewId,RightFaces}|Sel0],
	    {Sel,St}
    end.

proper_division(Left, Right, #we{fs=Ftab}) ->
    NumFaces = gb_trees:size(Ftab),
    case gb_sets:size(Left) + gb_sets:size(Right) of
	NumFaces ->
	    gb_sets:is_empty(gb_sets:intersection(Left, Right));
	Other ->
	    false
    end.

collect_faces(Face, Edges, We) ->
    collect_faces(gb_sets:singleton(Face), We, Edges, gb_sets:empty()).

collect_faces(Work0, We, Edges, Acc0) ->
    case gb_sets:is_empty(Work0) of
	true -> Acc0;
	false ->
	    {Face,Work1} = gb_sets:take_smallest(Work0),
	    Acc = gb_sets:insert(Face, Acc0),
	    Work = collect_maybe_add(Work1, Face, Edges, We, Acc),
	    collect_faces(Work, We, Edges, Acc)
    end.

collect_maybe_add(Work, Face, Edges, We, Res) ->
    wings_face:fold(
      fun(V, Edge, Rec, A) ->
	      case gb_sets:is_member(Edge, Edges) of
		  true -> A;
		  false ->
		      Of = wings_face:other(Face, Rec),
		      case gb_sets:is_member(Of, Res) of
			  true -> A;
			  false -> gb_sets:add(Of, A)
		      end
	      end
      end, Work, Face, We).
    
%%%
%%% Utilities.
%%%

reverse_dir(forward) -> backward;
reverse_dir(backward) -> forward.

half_edge(backward, #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,ltsu=L,rtpr=R}) ->
    {Va,Vb,Lf,Rf,L,R};
half_edge(forward, #edge{ve=Va,vs=Vb,lf=Lf,rf=Rf,ltpr=L,rtsu=R}) ->
    {Va,Vb,Lf,Rf,L,R}.

patch_half_edge(Edge, V, FaceA, Ea, FaceB, Eb, OrigV, Etab) ->
    New = case gb_trees:get(Edge, Etab) of
	      #edge{vs=OrigV,lf=FaceA,rf=FaceB}=Rec ->
		  Rec#edge{vs=V,ltsu=Ea,rtpr=Eb};
	      #edge{vs=OrigV,lf=FaceB,rf=FaceA}=Rec ->
		  Rec#edge{vs=V,ltsu=Eb,rtpr=Ea};
	      #edge{ve=OrigV,lf=FaceA,rf=FaceB}=Rec ->
		  Rec#edge{ve=V,ltpr=Ea,rtsu=Eb};
	      #edge{ve=OrigV,lf=FaceB,rf=FaceA}=Rec ->
		  Rec#edge{ve=V,ltpr=Eb,rtsu=Ea}
	  end,
    gb_trees:update(Edge, New, Etab).
    
patch_edge(Edge, ToEdge, OrigEdge, Etab) ->
    New = case gb_trees:get(Edge, Etab) of
	      #edge{ltsu=OrigEdge}=R ->
		  R#edge{ltsu=ToEdge};
	      #edge{ltpr=OrigEdge}=R ->
		  R#edge{ltpr=ToEdge};
	      #edge{rtsu=OrigEdge}=R ->
		  R#edge{rtsu=ToEdge};
	      #edge{rtpr=OrigEdge}=R ->
		  R#edge{rtpr=ToEdge}
	  end,
    gb_trees:update(Edge, New, Etab).

patch_edge(Edge, ToEdge, Face, OrigEdge, Etab) ->
    New = case gb_trees:get(Edge, Etab) of
	      #edge{lf=Face,ltsu=OrigEdge}=R ->
		  R#edge{ltsu=ToEdge};
	      #edge{lf=Face,ltpr=OrigEdge}=R ->
		  R#edge{ltpr=ToEdge};
	      #edge{rf=Face,rtsu=OrigEdge}=R ->
		  R#edge{rtsu=ToEdge};
	      #edge{rf=Face,rtpr=OrigEdge}=R ->
		  R#edge{rtpr=ToEdge}
	  end,
    gb_trees:update(Edge, New, Etab).
