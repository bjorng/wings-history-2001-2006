%%%-------------------------------------------------------------------
%%% File    : auv_segment.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Different segmentation algorithms.
%%%               Segments Model into set of charts containg faces.
%%% Created :  3 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002 Dan Gudmundsson 
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv_segment.erl,v 1.2 2002/10/08 11:32:24 dgud Exp $



-module(auv_segment).

-export([create/2, segment_by_material/1]).
-export([degrees/0]). %% Debug
-include("wings.hrl").
-include("auv.hrl").

-import(lists, [map/2,sort/1]).

%% Returns segments=[Charts=[Faces]] and Bounds=[Edges]
create(Mode, We0) ->
    case Mode of 
	feature ->
	    {_Distances,Charts0,Bounds0} = segment_by_feature(We0),
	    {Charts0, Bounds0};
	autouvmap ->
	    Charts0 = segment_by_direction(We0),
	    {Charts0, []};
	mat_uvmap ->
	    Charts0 = segment_by_material(We0),
	    {Charts0, []}
    end.

%%%%%% Feature detection Algorithm %%%%%%%%%%%%
%% Algorithms based on the paper, now probably totally ruined by me..
%% 'Least Square Conformal Maps for Automatic Texture Generation Atlas'
%% by Bruno Levy, Sylvain Petitjean, Nicolas Ray, Jerome Mailot
%% Presented on Siggraph 2002

-define(MIN_FEATURE_LENGTH, 12).
-define(MAX_STRING_LENGTH, 5).
-define(MAX_DIRECTION, 0.866025). %% See degrees
-define(MIN_SHARPNESS, (1 - ?MAX_DIRECTION) * (?MAX_STRING_LENGTH -1)).

%% Debug func %%
degrees() ->
    Test = fun(D) ->
		   X = (math:cos(D * math:pi() / 180) + 1) / 2, 
		   Y = math:sin(D *  math:pi() / 180) /2,    
		   Dir = math:sqrt(X*X+Y*Y),
		   ?DBG("~.3w deg -> ~w ~w~n", [D, Dir, 1 - Dir])
	   end,
    ?DBG("MaxDir ~p MaxSharpness ~p~n", 
	      [?MAX_DIRECTION, ?MIN_SHARPNESS]),
    lists:foreach(Test, lists:seq(0,360,15)).

segment_by_feature(We0) ->
    {Features,VEG,EWs} = find_features(We0),
    build_charts(Features, VEG, EWs, We0).
    
find_features(We0) ->
    {Sorted, N, _FNormals} = sort_edges_by_weight(We0),
    %% split list normals may diff with +-60 deg and 20% of the edges
    {Best, _Rest} = pick_features(Sorted, 0, ?MAX_DIRECTION, N, []),
    %%    ?DBG("Best ~p ~n", [Best]),
    EVGraph = build_vertex_graph(Sorted, We0, gb_trees:empty()),
    EWs    = gb_trees:from_orddict(lists:sort(Sorted)),

    Features0 = expand_features(Best, EVGraph, EWs, We0),
    Features1 = 
	if Features0 == [] -> find_extremities(We0);
	   true -> Features0
	end,
    {Features1, EVGraph, EWs}.

sort_edges_by_weight(We0 = #we{fs = Ftab, es = Etab}) ->
    Faces = gb_trees:keys(Ftab),
    FaceNormals0 = lists:map(fun(Face) ->
				     {Face,wings_face:normal(Face, We0)}
			     end, Faces),
    FaceNormals = gb_trees:from_orddict(FaceNormals0),
    Edges  = gb_trees:keys(Etab),
    WEdges = lists:map(fun(Edge) ->
			       Val = calc_normal_value(Edge, FaceNormals, We0),
			       {Edge, Val}
		       end, Edges),
    {lists:keysort(2,WEdges), length(WEdges), FaceNormals}.

calc_normal_value(Edge, FaceData, We) ->
    #edge{lf = LF, rf = RF} = gb_trees:get(Edge, We#we.es),
    calc_dir_vector(gb_trees:get(LF,FaceData), gb_trees:get(RF,FaceData)).

pick_features([Edge = {_,Value}|Rest], N, Constraint, Max, Acc) 
  when Value < Constraint, N < Max -> 
    pick_features(Rest, N+1, Constraint, Max, [Edge|Acc]);
pick_features(Rest, _,_,_, Acc) ->
    {lists:reverse(Acc), Rest}.

-record(fd, {graph, vals, neigh, feat}).
-define(fdcreate(VerG,Vals), #fd{graph=VerG, vals=Vals, neigh=gb_sets:empty(), feat=[]}).
-define(fdmember(Edge,Fd), gb_sets:is_member(Edge, Fd#fd.neigh)).
-define(fdisneigh(Edge,Fd),  gb_sets:is_member(Edge, Fd#fd.neigh)).
-define(fdgetfeat(Fd), Fd#fd.feat).
-define(fdgetsurrneigh(EdgVer, Fd), gb_trees:get(EdgVer,Fd#fd.graph)).
-define(fdgetvalue(Edge, Fd), gb_trees:get(Edge,Fd#fd.vals)).
-define(fdmarkused(Edge, Fd), Fd#fd{neigh=gb_sets:add(Edge, Fd#fd.neigh)}).
fdaddneighandfeat(Feat, We, FdXX) ->
    FindXX = fun(EdgeXX, AccXX) ->			 
		     #edge{vs=Vs,ve=Ve} = gb_trees:get(EdgeXX, We#we.es),
		     Neigh1 = ?fdgetsurrneigh({EdgeXX,Vs}, FdXX),
		     Neigh2 = ?fdgetsurrneigh({EdgeXX,Ve}, FdXX),
		     (Neigh1 ++ Neigh2) ++ AccXX
	     end,		    
    PossNeighXX = gb_sets:from_list(lists:foldl(FindXX,[], Feat)),
    NewNBXX = gb_sets:union(PossNeighXX, FdXX#fd.neigh),
    FdXX#fd{neigh = NewNBXX, feat = Feat ++ FdXX#fd.feat}.

expand_features(Possible, EVGr, EdgeCost, We) ->
    expand_features(Possible, ?fdcreate(EVGr, EdgeCost), We).

expand_features([First = {Edge, _}|Rest], Fd = #fd{}, We) ->
    case ?fdmember(Edge,Fd) of
	true -> %% Already used ignore
	    expand_features(Rest, Fd, We);
	false ->
	    {Feature, Fd1} = expand_feature_curve(First, Fd, We),
%	    ?DBG("Expand ~p ~p ~p~n", [First,length(Feature), Feature]),
	    if 
		length(Feature) < ?MIN_FEATURE_LENGTH ->     
		    expand_features(Rest, Fd, We);
		true -> %% Accepted feature
		    %% Add neighbors
		    Fd2 = fdaddneighandfeat(Feature,We,Fd1),
		    expand_features(Rest, Fd2, We)
	    end
    end;
expand_features([], Fd, _We) ->
%    ?DBG("Expand done~n"),
    ?fdgetfeat(Fd). 

expand_feature_curve({Edge, _Sharpness}, Fd0, We) ->
    #edge{vs = Vs, ve = Ve} = gb_trees:get(Edge, We#we.es),    
    Dir1 = get_vector(Vs,Ve,We),
    {Edges1,_} = get_edges(Vs, Edge, Dir1, We, Fd0),
    {Feat0, Fd2} = depth_traverse_tree([Edges1],0,0,Dir1,Fd0,We,[Edge]), 
    Dir2 = get_vector(Ve,Vs,We),
    {Edges2,_} = get_edges(Ve, Edge, Dir2, We, Fd2),
    depth_traverse_tree([Edges2],0,0,Dir2,Fd2,We,Feat0).

depth_traverse_tree(Tree=[[{Val,_,_}|_]|_],Sharp,Depth,_Dir1,Fd0, We, Feat) 
  when Sharp + Val > ?MIN_SHARPNESS ->
    %% Found suiteable edge -> add to feat
    [[{Val0,{Edge,#edge{vs=VaN,ve=VbN}},V0}|_]|Found] = lists:reverse(Tree),
%    ?DBG("Found suiteable edge ~p ~p ~p ~n", [Edge,Sharp,Depth]),
    Fd1 = ?fdmarkused(Edge, Fd0),
    case Found of 
	[] -> %% Oops first level hit, restart (special case)
	    ?DBG("Special case ~p ~n", [Tree]),
	    NextV = if V0 == VaN -> VbN; V0 == VbN -> VaN end,
	    Dir2 = get_vector(VaN,VbN,V0,We),
	    {Edges1,_} = get_edges(NextV, Edge, Dir2, We, Fd1),
	    depth_traverse_tree([Edges1], 0, 0, Dir2, Fd1, We, [Edge|Feat]);
	[[{_,{_,#edge{vs=Va,ve=Vb}},V}|_]|_] ->
	    Dir2 = get_vector(Va,Vb,V,We),
	    depth_traverse_tree(lists:reverse(Found), Sharp - Val0, Depth -1, 
				Dir2, Fd1, We, [Edge|Feat])
    end;
 
depth_traverse_tree([[]|[[{Value,_,_}|Alt]|Tree]],Sharp,Depth,Dir,Fd0,We,Feat) ->
    %% Last tested in that branch -> search other branches
%    ?DBG("Last tested in that branch -> search other branches ~p~n", [Depth]),
    depth_traverse_tree([Alt|Tree], Sharp -Value, Depth -1,Dir,Fd0, We, Feat);
 
depth_traverse_tree([_Miss|[[Root|Alt]|Tree]], Sharp, Depth, Dir,Fd0, We, Feat) 
  when Depth >= ?MAX_STRING_LENGTH ->
    %% To deep -> look in other branch
%    ?DBG("To deep -> look in other branch ~n",[]),
    {Value, _, _} = Root,
    depth_traverse_tree([Alt|Tree], Sharp - Value, Depth -1, Dir,Fd0, We, Feat);
depth_traverse_tree([[]], _Sharp, _Depth, _Dir,Fd0,_We, Feat) ->
    %% done -> tree search complete
%    ?DBG("done[[]]~n",[]),
    {Feat, Fd0};
depth_traverse_tree(Tree=[[{Val,Leaf,Vertex}|_]|_],Sharp,Depth,Dir,Fd0,We,Feat) ->
    %% No success yet -> search deeper
%    ?DBG("No success yet -> search deeper ~p (~p) ~p ~n",[Sharp+Val, ?MIN_SHARPNESS, Depth]),
    Next = case Leaf of 
	       {Id, #edge{vs = Vertex, ve = NextV}} ->
		   NextV;
	       {Id, #edge{ve = Vertex, vs = NextV}} ->
		   NextV
	   end,
    case get_edges(Next, Id, Dir, We, Fd0) of
	{[],Poss} -> %% Fixing last edge in edgeloop
	    {NewSharp,NewTree} = patch_tree(Poss, Sharp+Val, Tree, Feat),
	    depth_traverse_tree(NewTree,NewSharp,Depth+1,Dir,Fd0,We,Feat);
        {Edges,_} ->
	    depth_traverse_tree([Edges|Tree],Sharp+Val,Depth+1,Dir,Fd0,We,Feat)
    end.

patch_tree([F={Val,{Edge,_ER},_Vs}|_],Sharp,Tree,Feat) ->
    case lists:member(Edge, Feat) of
	true when (Val + Sharp) > ?MIN_SHARPNESS ->
	    ?DBG("Patched OK ~p ~n",[Edge]),
	    {Val + Sharp, [[F]|lists:map(fun([Element|_]) -> [Element] end, Tree)]};
	_Mem ->
	    ?DBG("patch miss ~p ~p ~p ~p ~n",[Edge, _Mem, Val, Sharp]),
	    {Sharp, [[]|Tree]}
    end;
patch_tree([],Sharp,Tree,_) ->
    {Sharp, [[]|Tree]}.

get_edges(V, Current, CurrVect, We, Fd) ->
    Surr = ?fdgetsurrneigh({Current,V}, Fd),
    Add = fun(Edge, Acc = {Acc1,Acc2}) ->
		  case ?fdisneigh(Edge, Fd) of
		      true ->
			  ER = #edge{vs = Va,ve = Vb} = gb_trees:get(Edge, We#we.es),
			  Val = ?fdgetvalue(Edge,Fd),
			  {Acc1,[{1-Val,{Edge,ER},V}|Acc2]};
		      false ->			  
			  ER = #edge{vs = Va,ve = Vb} = gb_trees:get(Edge, We#we.es),
			  ThisVect = get_vector(Va,Vb,V,We),
			  Dir = calc_dir_vector(CurrVect, ThisVect),
			  if Dir < 0.707106  -> %% 90deg
				  Acc;
			     true ->
				  Val = ?fdgetvalue(Edge,Fd),
				  {[{1 - Val, {Edge, ER}, V}|Acc1],Acc2}
			  end
		  end
	  end,
    {New,Bup} = lists:foldl(Add, {[],[]}, Surr),
    {lists:reverse(lists:sort(New)), lists:reverse(lists:sort(Bup))}.


calc_dir_vector(Vec1, Vec2) ->
    Vec = e3d_vec:add(Vec1,Vec2),
    e3d_vec:len(e3d_vec:divide(Vec, 2.0)).

get_vector(A,B,A,We) ->
    get_vector(B,A,We);
get_vector(A,B,B,We) ->
    get_vector(A,B,We).

get_vector(A,B,#we{vs = Vs}) ->
    e3d_vec:norm(e3d_vec:sub((gb_trees:get(A,Vs))#vtx.pos,(gb_trees:get(B,Vs))#vtx.pos)).

%% This could be improved V2 should be in opposite direction of V1
%% Find is used when pick features failed, i.e. a backup function
find_extremities(#we{vs= Vs}) ->
    Vs1 = gb_trees:to_list(Vs),
    Vs2 = [Pos || {_, #vtx{pos = Pos}} <- Vs1],
    Center = e3d_vec:average(Vs2),
    All = lists:map(fun({Id, #vtx{pos = Pos}}) ->
			    Dist = e3d_vec:dist(Pos, Center),
			    {Dist, Id}
		    end, Vs1),
    [{_,V1},{_,V2}|_] = lists:reverse(lists:sort(All)),
    E1 = (gb_trees:get(V1, Vs))#vtx.edge,
    E2 = (gb_trees:get(V2, Vs))#vtx.edge,
    [E1,E2].


build_charts(Features0, VEG, EWs, We0) ->
    FaceGraph = build_face_graph(gb_trees:keys(We0#we.fs), We0, gb_trees:empty()), 
    Distances = [{Max, _}|_] = calc_distance(Features0, FaceGraph, We0),
    {DistTree,LocalMaxs} = find_local_max(Distances, Features0, FaceGraph, We0),
    ?DBG("local max ~p ~p ~n", [length(LocalMaxs), LocalMaxs]),
    {Charts0,Bounds} = expand_charts(LocalMaxs, Max + 1, DistTree, VEG,EWs, We0),
    Charts1 = fix_charts(Charts0, undefined, -1, []),
%%    ?DBG("Charts ~p ~p ~n", [Charts1,Bounds]),
    {Distances, Charts1, Bounds}.

fix_charts([{Face,Chart}|Rest], Chart, G, [{G,Old}|Acc]) -> 
    fix_charts(Rest, Chart, G, [{G,[Face|Old]}|Acc]);
fix_charts([{Face,Chart}|Rest], _Old, G, Acc) ->
    fix_charts(Rest, Chart, G+1, [{G+1,[Face]}|Acc]);
fix_charts([],_,_,Acc) ->
    Acc.
    
add_face_edges_to_heap(Face, FaceDist, ChartBds, Heap, EWs, We) ->
    Find = fun(_V, Edge, #edge{lf=LF,rf=RF}, Heap0) ->		   
		   case gb_sets:is_member(Edge, ChartBds) of
		       true ->
			   Fopp = if LF == Face -> RF; RF == Face ->LF end,
			   EdgeW = 1 - gb_trees:get(Edge, EWs),
			   gb_trees:enter({FaceDist,EdgeW,Edge},{Face,Fopp}, Heap0);
		       false ->
			   Heap0
		   end
	   end,
    wings_face:fold(Find, Heap, Face, We).

expand_charts(LocalMaxs, Max, Dt, VEG,EWs, We0) ->
    ChartsE = gb_trees:empty(),
    HeapE = gb_trees:empty(),
    ChartBds = gb_sets:from_ordset(gb_trees:keys(We0#we.es)),
    Init = fun({LMax, Face}, {Chart0, Heap0}) ->
		   Chart1 = gb_trees:insert(Face, Face, Chart0),
		   Heap1  = add_face_edges_to_heap(Face,Max-LMax,ChartBds,Heap0,EWs,We0),
		   {Chart1, Heap1}
	   end,
    {ChartI, HeapI} = lists:foldl(Init, {ChartsE, HeapE}, LocalMaxs),
    expand_charts(HeapI, ChartI, ChartBds, Max, Dt, VEG,EWs, We0).
    
expand_charts(Heap0, Charts0, ChartBds0, Max, Dt, VEG,EWs, We) ->
    case gb_trees:is_empty(Heap0) of
	true ->  
	    {lists:keysort(2,gb_trees:to_list(Charts0)), gb_sets:to_list(ChartBds0)};
	false ->	    
	    {{_Dist,_,Edge},{Face,Fopp},Heap1} = gb_trees:take_smallest(Heap0),
	    Chart0 = gb_trees:get(Face, Charts0),
	    case gb_trees:lookup(Fopp, Charts0) of
		none ->
		    Charts1 = gb_trees:insert(Fopp, Chart0, Charts0),
		    ChartBds1 = gb_sets:delete(Edge, ChartBds0),
		    DistFopp = gb_trees:get(Fopp, Dt),
		    Heap2 = add_face_edges_to_heap(Fopp,Max-DistFopp,
						   ChartBds1,Heap1,EWs,We),
		    expand_charts(Heap2, Charts1, ChartBds1, Max, Dt, VEG,EWs, We);
		{value, Chart0} ->  %% Fopp and Face is in same chart
		    ChartBds1 = case is_extremity(Edge, ChartBds0, VEG,We) of
				    true -> %% delete_any missed
					Temp = gb_sets:singleton(Edge),
					gb_sets:difference(ChartBds0, Temp);
				    false -> ChartBds0
				end,
		    expand_charts(Heap1, Charts0, ChartBds1, Max, Dt, VEG,EWs, We);
		{value, Chart2} ->
		    MaxDistChartFace = gb_trees:get(Chart0, Dt),
		    MaxDistChartFopp = gb_trees:get(Chart2, Dt),
		    DistFace = gb_trees:get(Face, Dt),
		    DistFopp = gb_trees:get(Fopp, Dt),
		    Const = Max/4,
		    if ((MaxDistChartFace - DistFace) < Const) and
		       ((MaxDistChartFopp - DistFopp) < Const) ->
			    {Charts1,ChartBds1} = 
				merge_charts(Chart0,Chart2,Charts0,Dt,ChartBds0,We),
			    expand_charts(Heap1, Charts1, ChartBds1, Max, Dt, VEG,EWs, We);
		       true ->
			    expand_charts(Heap1, Charts0, ChartBds0, Max, Dt, VEG,EWs, We)
		    end
	    end
    end.

is_extremity(Edge,ChartBds0,VEG,We) ->
    #edge{vs=Vs,ve=Ve} = gb_trees:get(Edge, We#we.es),
    Next = fun() -> gb_trees:get({Edge,Ve},VEG) end,
    is_extremity(gb_trees:get({Edge,Vs},VEG),Next,ChartBds0).
is_extremity([Edge|Rest], Next, ChartBds0) ->
    case gb_sets:is_member(Edge, ChartBds0) of
	true ->
	    if Next == nomore -> 
		    false;
	       true ->
		    is_extremity(Next(), nomore, ChartBds0)
	    end;
	false ->
	    is_extremity(Rest, Next, ChartBds0)
    end;
is_extremity([], _, _) ->
    true.

merge_charts(Ch1,Ch2, Charts0, Dt, ChartBds0,We) ->    
    {C1,C2} =  
	case gb_trees:get(Ch1, Dt) > gb_trees:get(Ch2, Dt) of
	    true ->
		{Ch1, Ch2};
	    false->
		{Ch2, Ch1}
	end,
    List = gb_trees:to_list(Charts0),
    {Merged, ChartBds1} = 
	lists:mapfoldl(fun({Face, Chart}, ChB) when Chart == C2 ->	      
			       %% Removed Merged Charts borders from
			       %% BorderEdges.
			       DelCommonEdge = 
				   fun(_V,Edge,#edge{lf=LF,rf=RF},Acc) ->
					   Test = if LF==Face -> RF; true -> LF end,
					   case gb_trees:lookup(Test,Charts0) of
					       {value, C1} ->
						   gb_sets:delete(Edge,Acc);
					       _ ->
						   Acc
					   end
				   end,
			       {{Face, C1}, wings_face:fold(DelCommonEdge, ChB, Face, We)};
			  (Else,ChB) -> 
			       {Else,ChB} end, 
		       ChartBds0, List),
    {gb_trees:from_orddict(Merged), ChartBds1}.   

find_local_max(Distances, Features, FaceGraph, #we{es = Es}) ->
    DTree = lists:foldl(fun({Dist, Face}, Tree) ->
				gb_trees:insert(Face, Dist, Tree)
			end, gb_trees:empty(), Distances),
    %% Remove the features from FaceGraph, edges which are a feature shouldn't
    %% connect to opposite faces 
    FG1 = lists:foldl(fun(Edge, Tree0) -> 
			      #edge{lf=LF,rf=RF} = gb_trees:get(Edge, Es),
			      LFL = gb_trees:get(LF, Tree0),
			      LFL1 = lists:delete(RF, LFL),
			      Tree1 = gb_trees:update(LF, LFL1, Tree0),
			      RFL = gb_trees:get(RF, Tree1),
			      RFL1 = lists:delete(LF, RFL),
			      gb_trees:update(RF, RFL1, Tree1)
		      end, FaceGraph, Features),    
    {DTree, find_local_maximum(Distances, DTree, FG1, [])}.

find_local_maximum([This = {Dist, Face}|Dists], Dtree0, FG, Maxs) ->
    case gb_trees:delete_any(Face, Dtree0) of
	Dtree0 ->
	    find_local_maximum(Dists, Dtree0, FG, Maxs);
	Dtree1 ->
	    Dtree2 = delete_less([{gb_trees:get(Face, FG), Dist}], Dtree1, FG),
	    find_local_maximum(Dists, Dtree2, FG, [This|Maxs])
    end;
find_local_maximum([], _, _FG, Maxs) ->
    lists:reverse(Maxs).

delete_less([{[Face|Rest1], Dist}|Rest2], Dtree0, FG) ->
    case gb_trees:lookup(Face, Dtree0) of
	none ->
	    delete_less([{Rest1, Dist}|Rest2], Dtree0, FG);
	{value, Val} when Val > Dist ->
	    delete_less([{Rest1, Dist}|Rest2], Dtree0, FG);
	{value, Val} ->
	    Dtree1 = gb_trees:delete(Face, Dtree0),
	    New = {gb_trees:get(Face, FG), Val},
	    delete_less([{Rest1,Dist},New|Rest2], Dtree1, FG)
    end;
delete_less([{[],_}|Rest], Dt, Fg) ->
    delete_less(Rest, Dt,Fg);
delete_less([],Dt,_Fg) ->
    Dt.

build_face_graph([Face|Faces], We, Tree0) ->
    Find = fun(_V, _Edge, #edge{lf=LF,rf=RF}, Acc) ->
		   New = if LF == Face -> RF; RF == Face ->LF end,
		   [New|Acc]
	   end,
    Surround = wings_face:fold(Find, [], Face, We),
    build_face_graph(Faces, We, gb_trees:insert(Face, Surround, Tree0));
build_face_graph([],_, Tree) ->
    Tree.

build_vertex_graph([{Edge, _Value}|R], We, Tree0) ->
    #edge{vs = Vs, ve = Ve} = gb_trees:get(Edge,We#we.es),
    Find = fun(Id, _Face, _, Acc) when Id == Edge ->
		   Acc;
	      (Id, _Face, _, Acc) ->
		   [Id|Acc]
	   end,
    Surround0 = wings_vertex:fold(Find, [], Vs, We),
    Tree1 = gb_trees:insert({Edge,Vs}, Surround0, Tree0),
    Surround1 = wings_vertex:fold(Find, [], Ve, We),
    build_vertex_graph(R, We, gb_trees:insert({Edge,Ve}, Surround1, Tree1));
build_vertex_graph([],_, Tree) ->
    Tree.

calc_distance(Features, FG, We = #we{fs = FsOrig}) ->
    Faces = fun(Edge, {Dist0,Next0,Fs0}) ->
		    #edge{lf = LF, rf = RF} = gb_trees:get(Edge, We#we.es),
		    {Dist1,Next1,Fs1} = find_delete(LF,0,Dist0,Next0,Fs0,FG),
		    find_delete(RF,0,Dist1,Next1,Fs1,FG)
	    end,   
    {Dist2,Next2,Fs2} = lists:foldl(Faces, {[],[],FsOrig}, Features),
    calc_distance(Next2, 1, Dist2, [], Fs2,FG).

calc_distance([Head|Rest], Level, Dist0, Next0, Fs0,Fg) ->
    {Dist2, Next2, Fs2} = find_delete(Head, Level, Dist0, Next0, Fs0,Fg),
    calc_distance(Rest, Level, Dist2, Next2, Fs2,Fg);
calc_distance([], _Level, Dist, [], _Fs, _) ->
    lists:reverse(lists:sort(Dist));
calc_distance([], Level, Dist, Next, Fs, Fg) ->
    calc_distance(Next, Level+1, Dist, [], Fs, Fg).

find_delete(Face,Level,Dist0,Next0,Fs0,Fg) ->
    case gb_trees:delete_any(Face, Fs0) of
	Fs0 -> {Dist0,Next0,Fs0};
	Fs1 -> 
	    Next = gb_trees:get(Face,Fg),
	    {[{Level,Face}|Dist0],Next ++ Next0,Fs1}
    end.
%%%%%%%%%%% End Feature detection
%% Original autouv algorithm...
segment_by_direction(We0) ->
    Faces = gb_trees:keys(We0#we.fs),
    FaceNormals = map(fun(Face) ->
			      {Face,wings_face:normal(Face, We0)}
		      end, Faces),
    Clustered   = sort(map(fun(FN) ->
				   determine_dir(FN) end,
			   FaceNormals)),
    segment_by_cluster(Clustered, We0).

determine_dir({Face, {X0,Y0,Z0}}) ->
    Max = lists:max([abs(X0), abs(Y0), abs(Z0)]),
    Dir = if
	      X0 =:= Max -> {1.0, 0.0, 0.0};
	      Y0 =:= Max -> {0.0, 1.0, 0.0};
	      Z0 =:= Max -> {0.0, 0.0, 1.0};
	      -X0 =:= Max -> {-1.0, 0.0, 0.0};
	      -Y0 =:= Max -> {0.0, -1.0, 0.0};
	      -Z0 =:= Max -> {0.0, 0.0, -1.0}
	  end,
    {Dir, Face}.
%% By Color 
segment_by_material(We0) ->
    Ftab = We0#we.fs,
    Faces = gb_trees:keys(We0#we.fs),
    Clustered  = sort(map(fun(Face) ->
				  #face{mat=MatName} =
				      gb_trees:get(Face, Ftab),
				  {MatName, Face}
			  end, Faces)),
    segment_by_cluster(Clustered, We0).
%% Common segmentation algo   
segment_by_cluster(Sorted, We) ->
    ClusterFunc = fun({CType, Face}, [{CType, List}|R]) ->
			  [{CType, [Face|List]}|R];
		     ({CType, Face}, Acc) ->
			  [{CType, [Face]}|Acc]
		  end,
    Clustered = lists:foldl(ClusterFunc, [], Sorted),
    Neigh = [cluster_neighbors(Cluster, We) || 
		{_, Cluster} <- Clustered],
    Mod = fun(List, {N,Acc}) ->
		  New = [{N, L} || L <- List],
%		  New = {N, List},
		  {N+1, New ++ Acc}
	  end,
    {_, Charts0} = lists:foldl(Mod, {0,[]}, Neigh),
    Charts0.

cluster_neighbors(List, We) ->
    Fs = gb_sets:from_list(List),
    Surr = [{Face, get_surrounding_faces(Face,Fs,We)} ||  Face <- List],
    Temp = digraph:new(),
    Res = cluster_faces(sort(Surr), Temp),
    digraph:delete(Temp),
    Res.

cluster_faces([{Face, Fcs}|Rest], DG) ->
    digraph:add_vertex(DG, Face),
    lists:foreach(fun(Connected) -> 
			  digraph:add_vertex(DG, Connected),
			  digraph:add_edge(DG, Face, Connected)
		  end, Fcs),
    cluster_faces(Rest, DG);
cluster_faces([], DG) ->
    digraph_utils:components(DG).

get_surrounding_faces(Face, Fs, We) ->
    Accept = fun(_V, _E, EdgeRec, Acc) ->
		     Other = wings_face:other(Face, EdgeRec),
		     case gb_sets:is_member(Other, Fs) of
			 true -> [Other|Acc];
			 false -> Acc
		     end
	     end,
    sort(wings_face:fold(Accept, [], Face, We)).
