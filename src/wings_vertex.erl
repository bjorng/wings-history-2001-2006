%%
%%  wings_vertex.erl --
%%
%%     This module contains utility functions for vertices.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vertex.erl,v 1.13 2001/10/03 09:24:11 bjorng Exp $
%%

-module(wings_vertex).
-export([convert_selection/1,select_more/1,select_less/1,
	 fold/4,other/2,other_pos/3,
	 until/4,until/5,
	 center/1,center/2,
	 bounding_box/1,bounding_box/2,bounding_box/3,
	 normal/2,per_face/2,
	 dissolve/2,
	 connect/3,force_connect/4,
	 patch_vertex/3,pos/2]).

-include("wings.hrl").
-import(lists, [member/2,keymember/3,foldl/3,reverse/1,last/1]).

%%
%% Convert the current selection to a vertex selection.
%%
convert_selection(#st{selmode=body}=St) ->
    wings_sel:convert_shape(
      fun(_, #we{vs=Vtab}) ->
	      gb_sets:from_list(gb_trees:keys(Vtab))
      end, vertex, St);
convert_selection(#st{selmode=face}=St) ->
    wings_sel:convert(
      fun(Face, We, Sel0) ->
	      wings_face:fold(
		fun(V, _, _, Sel) ->
			gb_sets:add(V, Sel)
		end, Sel0, Face, We)
      end, vertex, St);
convert_selection(#st{selmode=edge}=St) ->
    wings_sel:convert(
      fun(Edge, #we{es=Etab}, Sel0) ->
	      #edge{vs=Vs,ve=Ve} = gb_trees:get(Edge, Etab),
	      gb_sets:add(Vs, gb_sets:add(Ve, Sel0))
      end, vertex, St);
convert_selection(#st{selmode=vertex}=St) ->
    select_more(St).

%%% Select more or less.

select_more(St) ->
    wings_sel:convert_shape(
      fun(Vs, We) ->
	      gb_sets:fold(
		fun(V, S0) ->
			fold(
			  fun(_, _, Rec, S1) ->
				  Other = other(V, Rec),
				  gb_sets:add(Other, S1)
			  end, S0, V, We)
		end, Vs, Vs)
      end, vertex, St).

select_less(St) ->
    wings_sel:convert_shape(
      fun(Vs, We) ->
	      gb_sets:fold(
		fun(V, A) ->
			Set = fold(
				fun(_, _, Rec, S) ->
					Other = other(V, Rec),
					gb_sets:add(Other, S)
				end, gb_sets:empty(), V, We),
			case gb_sets:is_subset(Set, Vs) of
			    true -> gb_sets:add(V, A);
			    false -> A
			end
		end, gb_sets:empty(), Vs)
      end, vertex, St).

%%
%% fold over all edges/faces surrounding a vertex.
%%

fold(F, Acc, V, #we{es=Etab,vs=Vtab}) ->
    #vtx{edge=Edge} = gb_trees:get(V, Vtab),
    #edge{lf=Face} = gb_trees:get(Edge, Etab),
    fold(F, Acc, V, Face, Edge, Edge, Etab, not_done).

fold(F, Acc, V, Face, Last, Last, Etab, done) -> Acc;
fold(F, Acc0, V, Face, Edge, LastEdge, Etab, _) ->
    Acc = case gb_trees:get(Edge, Etab) of
	      #edge{vs=V,lf=Face,rf=Other,rtpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,lf=Face,rf=Other,rtsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{vs=V,rf=Face,lf=Other,ltsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,rf=Face,lf=Other,ltpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0)
	  end,
    fold(F, Acc, V, Other, NextEdge, LastEdge, Etab, done).
%%
%% Fold over all edges/faces surrounding a vertex until the
%% accumulator changes.
%%

until(F, Acc, V, #we{vs=Vtab}=We) ->
    #vtx{edge=Edge} = gb_trees:get(V, Vtab),
    until(F, Acc, V, Edge, We).

until(F, Acc, V, Edge, #we{es=Etab,vs=Vtab}) ->
    #edge{lf=Face} = gb_trees:get(Edge, Etab),
    until(F, Acc, V, Face, Edge, Edge, Etab, not_done).

until(F, Acc, V, Face, Last, Last, Etab, done) -> Acc;
until(F, Acc0, V, Face, Edge, LastEdge, Etab, _) ->
    Acc = case gb_trees:get(Edge, Etab) of
	      #edge{vs=V,lf=Face,rf=Other,rtpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,lf=Face,rf=Other,rtsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{vs=V,rf=Face,lf=Other,ltsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,rf=Face,lf=Other,ltpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0)
	  end,
    if
	Acc =:= Acc0 ->
	    until(F, Acc, V, Other, NextEdge, LastEdge, Etab, done);
	true -> Acc
    end.

%% other(Vertex, EdgeRecord) -> OtherVertex
%%  Pick up the "other vertex" from an edge record.
other(V, #edge{vs=V,ve=Other}) -> Other;
other(V, #edge{ve=V,vs=Other}) -> Other.

%% pos(Vertex, VtabOrWe) -> {X,Y,Z}
%%  Return the three co-ordinates for a vertex.
pos(V, #we{vs=Vtab}) ->
    pos(V, Vtab);
pos(V, Vtab) ->
    #vtx{pos=Pos} = gb_trees:get(V, Vtab),
    Pos.

%% other_pos(Vertex, EdgeRecord, VtabOrWe) -> {X,Y,Z}
%%  Pick up the position for the "other vertex" from an edge record.
other_pos(V, #edge{vs=V,ve=Other}, Tab) -> pos(Other, Tab);
other_pos(V, #edge{ve=V,vs=Other}, Tab) -> pos(Other, Tab).

%% center(We) -> {CenterX,CenterY,CenterZ}
%%  Find the geometric center of a body.
center(#we{vs=Vtab}) ->
    Positions = foldl(fun(#vtx{pos=Pos}, A) ->
			      [Pos|A]
		      end, [], gb_trees:values(Vtab)),
    e3d_vec:average(Positions).

%% center(VertexGbSet, We) -> {CenterX,CenterY,CenterZ}
%%  Find the geometric center of all vertices.
center(Vs0, #we{vs=Vtab}) ->
    Vs = if
	     list(Vs0) -> Vs0;
	     true -> gb_sets:to_list(Vs0)
	 end,
    center(Vs, Vtab);
center(Vlist, Vtab) ->
    Positions = foldl(fun(V, A) ->
			      [pos(V, Vtab)|A]
		      end, [], Vlist),
    e3d_vec:average(Positions).

bounding_box(We) ->
    bounding_box(We, none).

bounding_box(#we{vs=Vtab}=We, BB) ->
    do_bounding_box(gb_trees:values(Vtab), BB);
bounding_box(Vs, We) ->
    bounding_box(Vs, We, none).
    
bounding_box(Vs, We, BB) when list(Vs) ->
    bounding_box_1(ordsets:from_list(Vs), We, BB);
bounding_box(Vs, We, BB) ->
    bounding_box(gb_sets:to_list(Vs), We, BB).

bounding_box_1(Vs0, #we{vs=Vtab}, BB) ->
    Vs1 = sofs:from_external(Vs0, [vertex]),
    R = sofs:from_external(gb_trees:to_list(Vtab), [{vertex,data}]),
    I = sofs:image(R, Vs1),
    Vs = sofs:to_external(I),
    do_bounding_box(Vs, BB).

do_bounding_box([#vtx{pos={X,Y,Z}}|Vs], none) ->
    do_bounding_box(Vs, X, X, Y, Y, Z, Z);
do_bounding_box(Vs, [{X0,Y0,Z0},{X1,Y1,Z1}]) ->
    do_bounding_box(Vs, X0, X1, Y0, Y1, Z0, Z1).

do_bounding_box([#vtx{pos={X,_,_}}|_]=Vs, X0, X1, Y0, Y1, Z0, Z1)
  when X < X0 ->
    do_bounding_box(Vs, X, X1, Y0, Y1, Z0, Z1);
do_bounding_box([#vtx{pos={X,_,_}}|_]=Vs, X0, X1, Y0, Y1, Z0, Z1)
  when X > X1 ->
    do_bounding_box(Vs, X0, X, Y0, Y1, Z0, Z1);
do_bounding_box([#vtx{pos={_,Y,_}}|_]=Vs, X0, X1, Y0, Y1, Z0, Z1)
  when Y < Y0 ->
    do_bounding_box(Vs, X0, X1, Y, Y1, Z0, Z1);
do_bounding_box([#vtx{pos={_,Y,_}}|_]=Vs, X0, X1, Y0, Y1, Z0, Z1)
  when Y > Y1 ->
    do_bounding_box(Vs, X0, X1, Y0, Y, Z0, Z1);
do_bounding_box([#vtx{pos={_,_,Z}}|_]=Vs, X0, X1, Y0, Y1, Z0, Z1)
  when Z < Z0 ->
    do_bounding_box(Vs, X0, X1, Y0, Y1, Z, Z1);
do_bounding_box([#vtx{pos={_,_,Z}}|_]=Vs, X0, X1, Y0, Y1, Z0, Z1)
  when Z > Z1 ->
    do_bounding_box(Vs, X0, X1, Y0, Y1, Z0, Z);
do_bounding_box([_|Vs], X0, X1, Y0, Y1, Z0, Z1) ->
    do_bounding_box(Vs, X0, X1, Y0, Y1, Z0, Z1);
do_bounding_box([], X0, X1, Y0, Y1, Z0, Z1) ->
    [{X0,Y0,Z0},{X1,Y1,Z1}].

%% normal(Vertex, We) -> Normal
%%  Calculate the normal for a vertex (based on the normals for all
%%  surrounding faces).
normal(V, We) ->
    Ns = fold(fun(_, Face, _, A) ->
		      [wings_face:normal(Face, We)|A]
	      end, [], V, We),
    e3d_vec:norm(e3d_vec:add(Ns)).

%% per_face(Vs, We) -> [{Face,[V]}]
%%  Group vertices according to face.
per_face(Vs, We) when list(Vs) ->
    per_face(Vs, We, []);
per_face(Vs, We) ->
    per_face(gb_sets:to_list(Vs), We, []).

per_face([V|Vs], We, Acc0) ->
    Acc = fold(fun(_, Face, _, A) ->
		       [{Face,V}|A]
	       end, Acc0, V, We),
    per_face(Vs, We, Acc);
per_face([], We, Acc) ->
    R = sofs:relation(Acc),
    F = sofs:relation_to_family(R),
    sofs:to_external(F).

%% dissolve(Vertex, We) -> We|error
%%  Remove a "winged vertex" - a vertex with exactly two edges.
dissolve(V, #we{es=Etab,vs=Vtab}=We) ->
    #vtx{edge=Edge} = gb_trees:get(V, Vtab),
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,ltsu=AnEdge,rtpr=AnEdge}=Rec ->
	    wings_edge:dissolve_edge(Edge, We);
	#edge{ve=V,rtsu=AnEdge,ltpr=AnEdge}=Rec ->
	    wings_edge:dissolve_edge(Edge, We);
	Other -> error
    end.

%% Connect vertices (which must share a face).

connect(Face, [_], We) -> We;
connect(Face, Vs, #we{}=We0) ->
    case polygon_pairs(Face, Vs, We0) of
	no ->
	    min_distance_pairs(gb_sets:singleton(Face), Vs, We0);
	#we{}=We -> We
    end.

%% Create pairs by walking the edge of the face. If we can connect
%% all selected vertices for the face we are done. The result will
%% be a polygon.
%%
%% +----*----+
%% |   / \   |
%% |  /	  \  |	     * = Selected vertices
%% | /     \ |	     + = Unselected vertices
%% |/  	    \|
%% *         *
%% |\  	    /|
%% | \ 	   / |
%% |  \	  /  |
%% |   \ /   |
%% +----*----+

polygon_pairs(Face, Vs, #we{}=We0) ->
    ?ASSERT(length(Vs) > 1),
    Iter = wings_face:iterator(Face, We0),
    case catch pp_start(Iter, Vs) of
	{'EXIT',Reason} -> exit(Reason);
	no -> no;
	Pairs when list(Pairs) ->
	    foldl(fun(Vpair, Acc0) ->
			  case try_connect(Vpair, Face, Acc0) of
			      no -> Acc0;
			      {Acc,_} -> Acc
			  end
		  end, We0, Pairs)
    end.

pp_start(Iter0, Vs) ->
    {V,_,_,Iter1} = wings_face:next_cw(Iter0),
    case member(V, Vs) of
	false -> pp_start(Iter1, Vs);
	true ->
	    Iter = pp_skip_one(Iter1, Vs),
	    pp_next(Iter, Vs, V, [])
    end.

pp_next(Iter0, Vs, Start, Acc0) ->
    {V,_,_,Iter1} = wings_face:next_cw(Iter0),
    case member(V, Vs) of
	false -> pp_next(Iter1, Vs, Start, Acc0);
	true ->
	    Acc = [{Start,V}|Acc0],
	    case keymember(V, 1, Acc0) of
		true ->
		    case Acc0 of
			[{V,Start}] -> Acc0;
			Other -> Acc
		    end;
		false ->
		    Iter = pp_skip_one(Iter1, Vs),
		    pp_next(Iter, Vs, V, Acc)
	    end
    end.

pp_skip_one(Iter0, Vs) ->
    {V,_,_,Iter} = wings_face:next_cw(Iter0),
    case member(V, Vs) of
	true -> throw(no);
	false -> Iter
    end.

%% If polygon_pairs/3 failed, we search for the two
%% vertices which are nearest each other. We try to connect,
%% then repeat the search in both the original face and the
%% the newly created face. We continue until no more connections
%% are possible. Two vertices that have been connected cannot be
%% connected again (in the same face).
%%
%% +-----+
%% |	 |   * = Selected vertices
%% *-----*   + = Non-selected vertices
%% |	 |
%% |	 |
%% *-----*
%% |	 |
%% |	 |
%% *-----*
%% |	 |
%% +-----+
%%

min_distance_pairs(Faces0, Vs0, We0) ->
    case gb_sets:is_empty(Faces0) of
	true -> We0;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    case nearest_pair(Face, Vs0, We0) of
		none ->
		    min_distance_pairs(Faces1, Vs0, We0);
		{Va,Vb}=Pair ->
		    case try_connect(Pair, Face, We0) of
			no ->
			    min_distance_pairs(Faces1, Vs0, We0);
			{We,FaceSet} ->
			    Faces = gb_sets:union(Faces1, FaceSet),
	       	       	    Vs1 = lists:delete(Va, Vs0),
			    Vs = lists:delete(Vb, Vs1),
			    min_distance_pairs(Faces, Vs, We)
		    end
	    end
    end.

nearest_pair(Face, Vs, We) ->
    NumV = wings_face:vertices(Face, We),
    Iter = wings_face:iterator(Face, We),
    nearest_pair_1(NumV, Iter, NumV, Vs, Face, We, {none,1.0e200}).

nearest_pair_1(0, Iter0, NumV, Vs, Face, We, {Pair,Dist}) -> Pair;
nearest_pair_1(N, Iter0, NumV, Vs, Face, We, PairDist0) ->
    #we{vs=Vtab} = We,
    {V,_,_,Iter1} = wings_face:next_cw(Iter0),
    case member(V, Vs) of
	false ->
	    nearest_pair_1(N-1, Iter1, NumV, Vs, Face, We, PairDist0);
	true ->
	    Pos = pos(V, Vtab),
	    {_,_,_,Iter} = wings_face:next_cw(Iter1),
	    PairDist = nearest_pair_2(NumV-3, Iter, Vs, Face, We,
				      V, Pos, PairDist0),
	    nearest_pair_1(N-1, Iter1, NumV, Vs, Face, We, PairDist)
    end.

nearest_pair_2(N, Iter0, Vs, Face, We, Start, Pos, PairDist0) when N > 0 ->
    #we{vs=Vtab} = We,
    {V,_,_,Iter} = wings_face:next_cw(Iter0),
    case member(V, Vs) of
	false ->
	    nearest_pair_2(N-1, Iter, Vs, Face, We, Start, Pos, PairDist0);
	true ->
	    {_,Dist} = PairDist0,
	    Vpos = pos(V, Vtab),
	    PairDist =
		case e3d_vec:dist(Vpos, Pos) of
		    D when D < Dist ->
			Pair = {Start,V},
			case try_connect(Pair, Face, We) of
			    no -> PairDist0;
			    {_,_} -> {Pair,D}
			end;
		    D -> PairDist0
		end,
	    nearest_pair_2(N-1, Iter, Vs, Face, We, Start, Pos, PairDist)
    end;
nearest_pair_2(N, Iter, Vs, Face, We, Start, Pos, PairDist) -> PairDist.

try_connect({Start,End}, Face, We0) ->
    {We,NewFace} = force_connect(Start, End, Face, We0),

    %% Reject the connection if any of the face normals are undefined.
    case {wings_face:good_normal(Face, We),wings_face:good_normal(NewFace, We)} of
	{true,true} -> {We,gb_sets:from_list([Face,NewFace])};
	{_,_} -> no
    end.

force_connect(Vstart, Vend, Face, #we{es=Etab0,fs=Ftab0}=We0) ->
    {NewFace,We} = wings_we:new_ids(1, We0),
    NewEdge = NewFace,
    NeRec0 = #edge{vs=Vstart,ve=Vend,lf=NewFace,rf=Face},

    Iter0 = wings_face:iterator(Face, We),
    Iter1 = wings_face:skip_to_cw(Vstart, Iter0),
    {_,_,_,Iter2} = wings_face:next_ccw(Iter1),
    {Etab1,NeRec1,Iter3} = connect_1(Iter2, Vstart, NewEdge, NeRec0, Etab0),
    {Etab2,NeRec2} = connect_2(Iter3, Vstart, NewEdge, NeRec1, Etab1),
    {Etab3,Iter} = connect_3(Iter3, Face, Vend, NewFace, Etab2),
    Etab = connect_4(Iter, Vend, NewEdge, NeRec2, Etab3),

    FaceRec = gb_trees:get(Face, Ftab0),
    Ftab1 = gb_trees:insert(NewFace, FaceRec#face{edge=NewEdge}, Ftab0),
    Ftab = gb_trees:update(Face, FaceRec#face{edge=NewEdge}, Ftab1),
    {We#we{es=Etab,fs=Ftab},NewFace}.

%% connect_1(Iter0, Vstart, NewEdge, NeRec0, Etab0) -> {Etab,NeRec,Iter}
%%  Connect the edge immediately before Vstart.
connect_1(Iter0, Vstart, NewEdge, NeRec0, Etab) ->
    case wings_face:next_cw(Iter0) of
	{_,Edge,#edge{ve=Vstart}=Rec0,Iter} ->
	    NeRec = NeRec0#edge{rtpr=Edge},
	    Rec = Rec0#edge{rtsu=NewEdge};
	{_,Edge,#edge{vs=Vstart}=Rec0,Iter} ->
	    NeRec = NeRec0#edge{rtpr=Edge},
	    Rec = Rec0#edge{ltsu=NewEdge}
    end,
    {gb_trees:update(Edge, Rec, Etab),NeRec,Iter}.

%% connect_2(Iter0, Vstart, NewEdge, NeRec0, Etab) -> {Etab,NeRec}
%%  Connect the edge immediately after Vstart.
connect_2(Iter, Vstart, NewEdge, NeRec0, Etab) ->
    case wings_face:next_cw(Iter) of
	{_,Edge,#edge{vs=Vstart}=Rec0,_} ->
	    NeRec = NeRec0#edge{ltsu=Edge},
	    Rec = Rec0#edge{rtpr=NewEdge};
	{_,Edge,#edge{ve=Vstart}=Rec0,_} ->
	    NeRec = NeRec0#edge{ltsu=Edge},
	    Rec = Rec0#edge{ltpr=NewEdge}
    end,
    {gb_trees:update(Edge, Rec, Etab),NeRec}.

%% connect_3(Iter, Face, Vend, NewFace, Etab0) -> {Etab,Iter}
%%  Replace the face for all edges between Vstart and Vend.
%%  The returned iterator points to the edge immediately before Vend.
connect_3(Iter0, Face, Vend, NewFace, Etab0) ->
    {_,Edge,_,Iter} = wings_face:next_cw(Iter0),

    %% Ignore the record returned by the iterator, because it
    %% is stale for the edge that was updated by connect_2/5.
    Rec = case gb_trees:get(Edge, Etab0) of
	      #edge{lf=Face}=Rec0 -> Rec0#edge{lf=NewFace};
	      #edge{rf=Face}=Rec0 -> Rec0#edge{rf=NewFace}
	  end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    case Rec of
	#edge{vs=Vend} -> {Etab,Iter0};
	#edge{ve=Vend} -> {Etab,Iter0};
	Other -> connect_3(Iter, Face, Vend, NewFace, Etab)
    end.

%% connect_4(Iter, Vend, NewEdge, NeRec, Etab0) -> Etab
%%  Patches the final two edges.
connect_4(Iter0, Vend, NewEdge, NeRec0, Etab0) ->
    {_,Edge,_,Iter} = wings_face:next_cw(Iter0),
    Rec = case gb_trees:get(Edge, Etab0) of
	      #edge{ve=Vend}=Rec0 ->
		  NeRec1 = NeRec0#edge{ltpr=Edge},
		  Rec0#edge{rtsu=NewEdge};
	      #edge{vs=Vend}=Rec0 ->
		  NeRec1 = NeRec0#edge{ltpr=Edge},
		  Rec0#edge{ltsu=NewEdge}
	  end,
    Etab1 = gb_trees:update(Edge, Rec, Etab0),

    %% Now for the final edge.
    FinalRec = case wings_face:next_cw(Iter) of
		   {_,Final,#edge{vs=Vend}=FinalRec0,_} ->
		       NeRec = NeRec1#edge{rtsu=Final},
		       FinalRec0#edge{rtpr=NewEdge};
		   {_,Final,#edge{ve=Vend}=FinalRec0,_} ->
		       NeRec = NeRec1#edge{rtsu=Final},
		       FinalRec0#edge{ltpr=NewEdge}
	       end,
    Etab = gb_trees:update(Final, FinalRec, Etab1),
    gb_trees:insert(NewEdge, NeRec, Etab).

%% Patch the incident edge for a vertex.

patch_vertex(V, Edge, Vtab) ->
    case gb_trees:get(V, Vtab) of
	#vtx{edge=Edge} -> Vtab;
	Vrec -> gb_trees:update(V, Vrec#vtx{edge=Edge}, Vtab)
    end.
