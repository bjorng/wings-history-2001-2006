%%
%%  wings_edge.erl --
%%
%%     This module contains most edge command and edge utility functions.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_edge.erl,v 1.50 2002/09/25 16:32:42 bjorng Exp $
%%

-module(wings_edge).

%% Commands.
-export([menu/3,command/2,
 	 select_region/1,select_edge_ring/1,
 	 cut/2,cut/3,fast_cut/3,connect/1,
 	 dissolve/1,dissolve_edges/2,dissolve_edge/2,
 	 hardness/2,hardness/3]).

%% Utilities.
-export([convert_selection/1,
	 select_more/1,select_more/2,
	 select_less/1,adjacent_edges/2,
	 to_vertices/2,from_faces/2,extend_sel/2,
	 connect/2,
	 patch_edge/4,patch_edge/5]).

-export([dissolve_vertex/2]).

-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,last/1,member/2,reverse/1,reverse/2,
		seq/2,sort/1]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Menu = [{basic,{"Edge operations",ignore}},
	    {basic,separator},
	    {"Move",{move,Dir}},
	    wings_menu_util:rotate(),
	    wings_menu_util:scale(),
	    separator,
	    {"Extrude",{extrude,Dir}},
	    separator,
	    {"Cut",{cut,cut_fun()}},
	    {"Connect",connect,"Create a new edge to connect selected edges"},
	    {"Bevel",bevel,"Round off selected edges"},
	    separator,
	    {"Dissolve",dissolve,"Eliminate selected edges"},
	    {"Collapse",collapse,"Delete edges, replacing them with vertices"},
	    separator,
	    {"Hardness",{hardness,[{"Soft",soft},
				   {"Hard",hard}]}},
	    separator,
	    {"Loop Cut",loop_cut,"Cut into two objects along edge loop"}],
    wings_menu:popup_menu(X, Y, edge, Menu).

cut_fun() ->
    fun(help, _Ns) ->
	    {"Cut into edges of equal length",[],"Pick cut position"};
       (1, _Ns) ->
	    [cut_entry(2),
	     cut_entry(3),
	     cut_entry(4),
	     cut_entry(5),
	     separator,
	     cut_entry(10)];
       (2, _) -> ignore;
       (3, _) ->
	    {edge,cut_pick}
    end.

cut_entry(N) ->
    Str = integer_to_list(N),
    {Str,N,"Cut into " ++ Str ++ " edges of equal length"}.
    
%% Edge commands.
command(bevel, St) ->
    ?SLOW(wings_extrude_edge:bevel(St));
command({extrude,Type}, St) ->
    ?SLOW(wings_extrude_edge:extrude(Type, St));
command(cut_pick, St) ->
    cut_pick(St);
command({cut,Num}, St) ->
    {save_state,cut(Num, St)};
command(connect, St) ->
    {save_state,connect(St)};
command(dissolve, St) ->
    {save_state,dissolve(St)};
command(collapse, St) ->
    {save_state,wings_collapse:collapse(St)};
command({hardness,Type}, St) ->
    {save_state,hardness(Type, St)};
command(loop_cut, St) ->
    ?SLOW({save_state,loop_cut(St)});
command(auto_smooth, St) ->
    wings_body:auto_smooth(St);
command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St).

%%
%% Convert the current selection to an edge selection.
%%
convert_selection(#st{selmode=body}=St) ->
    wings_sel:convert_shape(
      fun(_, #we{es=Etab}) ->
	      gb_sets:from_list(gb_trees:keys(Etab))
      end, edge, St);
convert_selection(#st{selmode=face}=St) ->
    wings_sel:convert_shape(
      fun(Faces, We) ->
	      from_faces(Faces, We)
      end, edge, St);
convert_selection(#st{selmode=edge}=St) ->
    wings_sel:convert_shape(
      fun(Edges, We) ->
	      extend_sel(Edges, We)
      end, edge, St);
convert_selection(#st{selmode=vertex}=St) ->
    wings_sel:convert_shape(fun(Vs, We) -> from_vs(Vs, We) end, edge, St).

from_vs(Vs, We) ->
    from_vs(gb_sets:to_list(Vs), We, []).

from_vs([V|Vs], We, Acc0) ->
    Acc = wings_vertex:fold(fun(E, _, _, A) -> [E|A] end, Acc0, V, We),
    from_vs(Vs, We, Acc);
from_vs([], We, Acc) -> gb_sets:from_list(Acc).

%%% Select more or less.

select_more(St) ->
    wings_sel:convert_shape(fun select_more/2, edge, St).

select_more(Edges, We) ->
    Vs = to_vertices(Edges, We),
    adjacent_edges(Vs, We, Edges).

select_less(St) ->
    wings_sel:convert_shape(
      fun(Edges, #we{es=Etab}=We) ->
	      Vs0 = gb_sets:fold(
		      fun(Edge, A) ->
			      Rec = gb_trees:get(Edge, Etab),
			      #edge{vs=Va,ve=Vb,
				    ltpr=LP,ltsu=LS,
				    rtpr=RP,rtsu=RS} = Rec,
			      Set = gb_sets:from_list([LP,LS,RP,RS]),
			      case gb_sets:is_subset(Set, Edges) of
				  true -> A;
				  false -> [Va,Vb|A]
			      end
		      end, [], Edges),
	      Vs = ordsets:from_list(Vs0),
	      AdjEdges = adjacent_edges(Vs, We, gb_sets:empty()),
	      gb_sets:subtract(Edges, AdjEdges)
      end, edge, St).

adjacent_edges(Vs, We) ->
    adjacent_edges(Vs, We, gb_sets:empty()).
adjacent_edges(Vs, We, Acc) ->
    foldl(fun(V, A) ->
		  wings_vertex:fold(
		    fun(Edge, _, _, AA) ->
			    gb_sets:add(Edge, AA)
		    end, A, V, We)
	  end, Acc, Vs).

%% to_vertices(EdgeGbSet, We) -> VertexGbSet
%%  Convert a set of edges to a set of vertices.

to_vertices(Edges, #we{es=Etab}) when is_list(Edges) ->
    to_vertices(Edges, Etab, []);
to_vertices(Edges, #we{es=Etab}) ->
    to_vertices(gb_sets:to_list(Edges), Etab, []).

to_vertices([E|Es], Etab, Acc) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
    to_vertices(Es, Etab, [Va,Vb|Acc]);
to_vertices([], _Etab, Acc) -> ordsets:from_list(Acc).

%% from_faces(FaceSet, We) -> EdgeSet
%%  Convert faces to edges.
from_faces(Faces, We) ->
    Edges = wings_face:fold_faces(
	      fun(_, _, Edge, _, A) ->
		      [Edge|A]
	      end, [], Faces, We),
    gb_sets:from_list(Edges).

%% from_faces(Edges, We) -> EdgeSet
%%  Extend Edges with all neighboring edges.
extend_sel(Edges, We) when is_list(Edges) ->
    extend_sel(Edges, gb_sets:from_list(Edges), We);
extend_sel(EdgeSet, We) ->
    extend_sel(gb_sets:to_list(EdgeSet), EdgeSet, We).

extend_sel(Edges, EdgeSet, #we{es=Etab}) ->
    foldl(fun(Edge, S0) ->
		  Rec = gb_trees:get(Edge, Etab),
		  #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
		  gb_sets:union(S0, gb_sets:from_list([LP,LS,RP,RS]))
	  end, EdgeSet, Edges).

%%%
%%% The Cut command.
%%%

cut(N, #st{selmode=edge}=St0) when N > 1 ->
    {St,Sel} = wings_sel:mapfold(
		 fun(Edges, #we{id=Id}=We0, Acc) ->
			 We = cut_edges(Edges, N, We0),
			 S = wings_we:new_items(vertex, We0, We),
			 {We,[{Id,S}|Acc]}
		 end, [], St0),
    wings_sel:set(vertex, Sel, St);
cut(_, St) -> St.

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
		   Hard = gb_sets:from_list(seq(BaseId, BaseId+NumIds-1)),
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
    
make_vertices_1(1, _Id, _Va, _Dir, Vtab) -> Vtab;
make_vertices_1(N, Id, Va, Dir, Vtab0) ->
    NextPos = wings_util:share(e3d_vec:add(Va, Dir)),
    Vtx = #vtx{pos=NextPos,edge=Id},
    Vtab = gb_trees:insert(Id, Vtx, Vtab0),
    make_vertices_1(N-1, Id+1, NextPos, Dir, Vtab).

%% fast_cut(Edge, Position, We0) -> {We,NewVertex,NewEdge}
%%  Cut an edge in two parts. Position can be given as
%%  the atom `default', in which case the position will
%%  be set to the midpoint of the edge.

fast_cut(Edge, Pos0, We0) ->
    {NewV,We} = wings_we:new_ids(1, We0),
    NewEdge = NewV,
    #we{es=Etab0,vs=Vtab0,he=Htab0} = We,
    Template = gb_trees:get(Edge, Etab0),
    #edge{vs=Vstart,ve=Vend,a=ACol,b=BCol,lf=Lf,rf=Rf,
	  ltpr=EdgeA,rtsu=EdgeB,rtpr=NextBCol} = Template,
    #vtx{pos=VendPos,edge=VendEdge}= VendRec = gb_trees:get(Vend, Vtab0),
    Vtab1 = if
		VendEdge =:= Edge ->
		    gb_trees:update(Vend, VendRec#vtx{edge=NewEdge}, Vtab0);
		true -> Vtab0
	    end,
    VstartPos = wings_vertex:pos(Vstart, Vtab0),
    if
	Pos0 =:= default ->
	    NewVPos0 = e3d_vec:average([VstartPos,VendPos]);
	true ->
	    NewVPos0 = Pos0
    end,
    NewVPos = wings_util:share(NewVPos0),
    Vtx = #vtx{pos=NewVPos,edge=NewEdge},
    Vtab = gb_trees:insert(NewV, Vtx, Vtab1),

    %% Here we handle vertex colors/UV coordinates.
    Weight = if
		 Pos0 == default -> 0.5;
		 true ->
		     ADist = e3d_vec:dist(Pos0, VstartPos),
		     BDist = e3d_vec:dist(Pos0, VendPos),
		     case catch ADist/(ADist+BDist) of
			 {'EXIT',_} -> 0.5;
			 Weight0 -> Weight0
		     end
	     end,
    AColOther = get_vtx_color(EdgeA, Lf, Etab0),
    NewColA = wings_color:mix(Weight, ACol, AColOther),
    BColOther = get_vtx_color(NextBCol, Rf, Etab0),
    NewColB = wings_color:mix(Weight, BCol, BColOther),

    NewEdgeRec = Template#edge{vs=NewV,a=NewColA,ltsu=Edge,rtpr=Edge},
    Etab1 = gb_trees:insert(NewEdge, NewEdgeRec, Etab0),
    Etab2 = patch_edge(EdgeA, NewEdge, Edge, Etab1),
    Etab3 = patch_edge(EdgeB, NewEdge, Edge, Etab2),
    EdgeRec = Template#edge{ve=NewV,b=NewColB,rtsu=NewEdge,ltpr=NewEdge},
    Etab = gb_trees:update(Edge, EdgeRec, Etab3),

    Htab = case gb_sets:is_member(Edge, Htab0) of
	       false -> Htab0;
	       true -> gb_sets:insert(NewEdge, Htab0)
	   end,
    {We#we{es=Etab,vs=Vtab,he=Htab},NewV}.

get_vtx_color(Edge, Face, Etab) ->
    case gb_trees:get(Edge, Etab) of
	#edge{lf=Face,a=Col} -> Col;
	#edge{rf=Face,b=Col} -> Col
    end.
    
%%%
%%% Cut and then interactively adjust the vertex positions.
%%%

cut_pick(St0) ->
    St = cut(2, St0),
    Tvs = wings_sel:fold(
	    fun(Vs, #we{id=Id}=We, Acc) ->
		    [{Id,cut_pick_1(gb_sets:to_list(Vs), We, [])}|Acc]
	    end, [], St),
    wings_drag:setup(Tvs, [{percent,{-1.0,1.0}}], [], St).

cut_pick_1([V|Vs], #we{vs=Vtab,es=Etab}=We, Acc) ->		    
    #vtx{pos=Pa,edge=Edge} = gb_trees:get(V, Vtab),
    Pb = wings_vertex:other_pos(V, gb_trees:get(Edge, Etab), Vtab),
    cut_pick_1(Vs, We, [{e3d_vec:sub(Pa, Pb),[V]}|Acc]);
cut_pick_1([], _, Acc) -> Acc.

%%%
%%% The Connect command.
%%%

connect(St0) ->
    {St,Sel} = wings_sel:mapfold(fun connect/3, [], St0),
    wings_sel:set(Sel, St).

connect(Es, We0) ->
    {We,_} = connect(Es, We0, []),
    We.

connect(Es, #we{id=Id}=We0, Acc) ->
    {We1,Vs} = cut_edges(Es, We0),
    We2 = wings_vertex_cmd:connect(Vs, We1),
    Sel = wings_we:new_items(edge, We1, We2),
    We = remove_winged_vs(Vs, We2),
    {We,[{Id,Sel}|Acc]}.

cut_edges(Es, We) ->
    gb_sets:fold(fun(Edge, {W0,Vs0}) ->
			 {W,V} = cut(Edge, 2, W0),
			 {W,[V|Vs0]}
		 end, {We,[]}, Es).

remove_winged_vs(Vs, We) ->
    foldl(fun(V, W0) ->
		  case dissolve_vertex(V, W0) of
		      error -> W0;
		      W -> W
		  end
	  end, We, Vs).

%%%
%%% The Dissolve command.
%%%

dissolve(St0) ->
    St = wings_sel:map(fun dissolve_edges/2, St0),
    wings_sel:clear(St).

dissolve_edges(Edges0, We0) when is_list(Edges0) ->
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
	    We0#we{vs=Empty,es=Empty,fs=Empty,he=gb_sets:empty()};
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
	  fun (_, E, _, IntEtab) when E =:= Edge -> IntEtab;
	      (_, E, R, IntEtab) ->
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
	_Other -> We
    end.

%% dissolve(Vertex, We) -> We|error
%%  Remove a "winged vertex" - a vertex with exactly two edges.
dissolve_vertex(V, #we{es=Etab,vs=Vtab}=We0) ->
    #vtx{edge=Edge} = gb_trees:get(V, Vtab),
    case gb_trees:lookup(Edge, Etab) of
	{value,#edge{vs=V,ltsu=AnEdge,rtpr=AnEdge}=Rec} ->
	    merge_edges(backward, Edge, Rec, We0);
	{value,#edge{ve=V,rtsu=AnEdge,ltpr=AnEdge}=Rec} ->
	    merge_edges(forward, Edge, Rec, We0);
	_Other -> error
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
	_Other ->
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
	_Other -> We
    end.

update_face(Face, Edge, OldEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	#face{edge=OldEdge}=Frec ->
	    gb_trees:update(Face, Frec#face{edge=Edge}, Ftab);
	_Other -> Ftab
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
    #edge{lf=Klf,rf=Krf} = gb_trees:get(EdgeANear, Etab),
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
%%% The Hardness command.
%%%

hardness(soft, St) ->
    wings_sel:map(fun(Edges, #we{he=Htab0}=We) ->
			  Htab = gb_sets:difference(Htab0, Edges),
			  We#we{he=Htab}
		  end, St);
hardness(hard, St) ->
    wings_sel:map(fun(Edges, #we{he=Htab0}=We) ->
			  Htab = gb_sets:union(Htab0, Edges),
			  We#we{he=Htab}
		  end, St).

hardness(Edge, soft, Htab) ->
    case gb_sets:is_member(Edge, Htab) of
	true -> gb_sets:delete(Edge, Htab);
	false -> Htab
    end;
hardness(Edge, hard, Htab) -> gb_sets:add(Edge, Htab).

%%%
%%% Select one side of an edge loop.
%%%

select_region(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun select_region/3, [], St),
    wings_sel:set(face, Sel, St);
select_region(St) -> St.

select_region(Edges, #we{id=Id}=We, Acc) ->
    Part = wings_edge_loop:partition_edges(Edges, We),
    FaceSel0 = select_region_1(Part, Edges, We, []),
    FaceSel = wings_sel:subtract_mirror_face(FaceSel0, We),
    [{Id,FaceSel}|Acc].

select_region_1([[AnEdge|_]|Ps], Edges, #we{es=Etab}=We, Acc) ->
    #edge{lf=Lf,rf=Rf} = gb_trees:get(AnEdge, Etab),
    Left = collect_faces(Lf, Edges, We),
    Right = collect_faces(Rf, Edges, We),
    select_region_1(Ps, Edges, We, [Left,Right|Acc]);
select_region_1([], _Edges, _We, [A,B]) ->
    case {gb_sets:size(A),gb_sets:size(B)} of
	{Sa,Sb} when Sa < Sb  -> A;
	{_,_} -> B
    end;
select_region_1([], _Edges, _We, Acc0) ->
    Acc = sort([gb_sets:to_list(P) || P <- Acc0]),
    select_region_2(Acc, []).

select_region_2([H,H|T], Acc) ->
    select_region_2(strip_prefix(T, H), Acc);
select_region_2([H|T], Acc) ->
    select_region_2(T, [H|Acc]);
select_region_2([], Acc) ->
    gb_sets:from_ordset(lists:merge(Acc)).

strip_prefix([Prefix|T], Prefix) -> strip_prefix(T, Prefix);
strip_prefix(L, _Prefix) -> L.

%%%
%%% The Loop Cut command.
%%%

loop_cut(#st{onext=NextId}=St0) ->
    St1 = wings_sel:map(fun(_, #we{mirror=none}=We) -> We;
			   (_, We) ->We#we{mirror=none}
			end, St0),
    {Sel0,St2} = wings_sel:fold(fun loop_cut/3, {[],St1}, St1),
    St3 = wings_sel:set(face, Sel0, St2),
    #st{sel=Sel1} = St = wings_face_cmd:dissolve(St3),
    Sel = [S || {Id,_}=S <- Sel1, Id >= NextId],
    wings_body:convert_selection(wings_sel:set(body, Sel, St)).

loop_cut(Edges, #we{name=Name}=We, Acc) ->
    case wings_edge_loop:edge_loop_vertices(Edges, We) of
	none ->
	    Error = "Selected edges in \"" ++
		Name ++ "\" does not form one or more loops.",
	    throw({command_error,Error});
	_Other -> loop_cut_1(Edges, We, Acc)
    end.

loop_cut_1(Edges, #we{id=Id,es=Etab,name=Name}=We0,
	   {Sel0,#st{onext=NewId}=St0}) ->
    {AnEdge,_} = gb_sets:take_smallest(Edges),
    #edge{lf=Lf,rf=Rf} = gb_trees:get(AnEdge, Etab),
    LeftFaces = collect_faces(Lf, Edges, We0),
    RightFaces = collect_faces(Rf, Edges, We0),
    case gb_sets:is_subset(LeftFaces, RightFaces) orelse
	gb_sets:is_subset(RightFaces, LeftFaces) of
	true ->
	    Error = "Edge loop doesn't divide \"" ++ Name ++
		"\" into two (or more) parts.",
	    wings_util:error(Error);
	false ->
	    WeCopy = wings_we:get_sub_object(AnEdge, We0),
	    St = wings_shape:insert(WeCopy, "cut", St0),
	    Sel = case gb_sets:size(LeftFaces) < gb_sets:size(RightFaces) of
		      true ->
			  [{Id,LeftFaces},{NewId,RightFaces}|Sel0];
		      false ->
			  [{Id,RightFaces},{NewId,LeftFaces}|Sel0]
		  end,
	    {Sel,St}
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
      fun(_, Edge, Rec, A) ->
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
%%% Edge Ring. (Based on Anders Conradi's plug-in.)
%%%

select_edge_ring(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun build_selection/3, [], St),
    wings_sel:set(Sel, St);
select_edge_ring(St) -> St.

build_selection(Edges, #we{id=Id} = We, ObjAcc) ->
    [{Id,foldl(fun(Edge, EdgeAcc) -> 
		       grow_from_edge(Edge, We, EdgeAcc) 
	       end, gb_sets:empty(), gb_sets:to_list(Edges))}|ObjAcc].

grow_from_edge(unknown, _We, Selected) -> Selected;
grow_from_edge(Edge, We, Selected0) ->
    Selected = gb_sets:add(Edge, Selected0),
    case gb_sets:is_member(Edge, Selected0) of
        true -> Selected;
        false ->
	    LeftSet = grow_from_edge(opposing_edge(Edge, We, left), We, Selected),
	    grow_from_edge(opposing_edge(Edge, We, right), We, LeftSet)
    end.

opposing_edge(Edge, #we{es=Es}=We, Side) ->
    #edge{lf=Left,rf=Right} = gb_trees:get(Edge, Es),
    Face = case Side of
               left -> Left;
               right -> Right
           end,
    %% Get opposing edge or fail.
    case wings_face:vertices(Face, We) of
        4 -> next_edge(next_edge(Edge, Face, We), Face, We);
        _ -> unknown
    end.

next_edge(Edge, Face, #we{es=Etab})->
    case gb_trees:get(Edge, Etab) of
        #edge{lf=Face,ltsu=NextEdge} -> NextEdge;
        #edge{rf=Face,rtsu=NextEdge} -> NextEdge
    end.

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
