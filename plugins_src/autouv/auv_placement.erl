%%%-------------------------------------------------------------------
%%% File    : auv_placement.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Algotrihms for placing charts on texture.
%%%
%%% Created :  7 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002 Dan Gudmundsson 
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv_placement.erl,v 1.20 2003/08/16 17:50:34 bjorng Exp $

-module(auv_placement).

-include("wings.hrl").
-include("auv.hrl").

-export([place_areas/1,group_edge_loops/2]).

-import(lists, [max/1, sort/1, map/2, reverse/1]).

%% Returns a gb_tree with areas...
place_areas(Areas0) ->
    Rotate = fun(#we{name=#ch{fs=Fs}=Ch0}=We0, {C, BBs}) ->
		     {{Dx,Dy}=Size, Vs} = center_rotate(Fs, We0),
		     Ch = Ch0#ch{size=Size},
		     We = We0#we{id=C,vp=gb_trees:from_orddict(Vs),name=Ch},
		     {We,{C+1, [{Dx,Dy,C}|BBs]}}
	     end,
    {Areas1, {_,Sizes0}} = lists:mapfoldl(Rotate, {1,[]}, Areas0),
%    ?DBG("~p~n",[Sizes0]),    
    {Positions0, Max} = fill(Sizes0, [0,0]),
%    ?DBG("~p~n",[Positions0]),
    Scale  = 1 / max(Max),
    move_and_scale_charts(Areas1, lists:sort(Positions0), Scale, []).

center_rotate(Fs, We) ->
    VL = rotate_area(Fs, We),
    {{_,Xmin},{_,Xmax},{_,Ymin},{_,Ymax}} = auv_util:maxmin(VL),
    Dx = Xmax - Xmin,
    Dy = Ymax - Ymin,
    CX = Xmin + Dx / 2,
    CY = Ymin + Dy / 2,
    Vs = auv_util:moveAndScale(VL, -CX, -CY, 1, []),
    {{Dx,Dy}, Vs}.

fill(Areas, [0,0]) ->  %% First time
    Map = fun({W,H,Id}) when W > H ->
		     {{W,width}, H, Id};
	     ({W,H,Id}) ->
		  {{H,height}, W, Id}
	  end,
    SL0 = sort(map(Map, Areas)),
    [First|SL] = reverse(SL0),
    {Res,PX,PY} = insert(First, 0,0),
    fill(SL, PX,PY, [Res]);
fill(Areas, [PX,PY]) ->
    Map = fun({W,H,Id}) when W > H ->
		     {{W,width}, H, Id};
		({W,H,Id}) ->
		     {{H,height}, W, Id}
	     end,
    SL0 = sort(map(Map, Areas)),
    SL = reverse(SL0),
    fill(SL, PX,PY, []).

fill([], MaxX, MaxY, Res) ->
    {Res, [MaxX,MaxY]};
fill([Biggest|SL], MX0, MY0, Res0) ->
    {W0,H0} = case Biggest of {{W,width},H,_} -> {W,H}; {{H,height},W,_} -> {W,H} end,
    %% Calc possible places
    A1 = max([(W0 + MX0), max([H0,MY0])]),  %% Build Element to the right
    A2 = max([max([W0, MX0]), (MY0 + H0)]), %% Build Element on the top 
    if 
	A1 < A2 ->
	    {New, MX1, MY1} = insert(Biggest, MX0, 0),
	    if MY0 >= MY1 ->
		    {SL2, Res1} = fill_area(SL, W0,MY0-H0, MX0,MY1, [], [New|Res0]), 
		    fill(SL2, MX1, MY0, Res1);
	       MY1 > MY0 ->
		    {SL2, Res1} = fill_area(SL, MX0,MY1-MY0, 0,MY0, [], [New|Res0]), 
		    fill(SL2, MX1, MY1, Res1)
	    end;
	true ->	    
	    {New, MX1, MY1} = insert(Biggest, 0, MY0),
	    if MX0 >= MX1 ->
		    {SL2, Res1} = fill_area(SL, MX0-W0,H0, MX1,MY0, [], [New|Res0]), 
		    fill(SL2, MX0, MY1, Res1);
	       MX1 > MX0 ->
		    {SL2, Res1} = fill_area(SL, MX1-MX0,MY0, MX0,0, [], [New|Res0]), 
		    fill(SL2, MX1, MY1, Res1)
	       end
    end.

insert({{X,width}, Y, I},XP,YP) ->
    New = {I, {XP+X/2, YP+Y/2}},
    {New, XP+X, YP+Y};
insert({{Y,height}, X, I},XP,YP) ->
    New = {I, {XP+X/2, YP+Y/2}},
    {New, XP+X, YP+Y}.

fill_area([Sel = {{X0,width},Y0,_}|SL], MX, MY, XP,YP, UU, Res0) 
  when X0 =< MX, Y0 =< MY ->
    {New,_,_} = insert(Sel,XP,YP),
    {SL2, Res1}= fill_area(SL, MX-X0, MY, XP+X0, YP, [], Res0),
    fill_area(SL2, X0, MY-Y0, XP, YP+Y0, UU, [New|Res1]);
fill_area([Sel={{Y0,height},X0,_}|SL], MX, MY, XP,YP,UU, Res0) 
  when X0 =< MX, Y0 =< MY ->
    {New,_,_} = insert(Sel,XP,YP),
    {SL2, Res1} = fill_area(SL, MX,MY-Y0, XP, YP+Y0,[], Res0),
    fill_area(SL2, MX-X0, Y0, XP+X0, YP, UU, [New|Res1]);
fill_area([Nouse|SL], MaxX, MaxY, XP,YP, Unused, Res) ->
    fill_area(SL,MaxX,MaxY, XP,YP, [Nouse|Unused], Res);
fill_area([], _,_, _,_, Unused,Res) ->
    {reverse(Unused), Res}.

%%%%%%%%%%%%%%%%
move_and_scale_charts([We0|RA], [{C,{Cx,Cy}}|RP], S, Acc) ->
    Transform0 = e3d_mat:scale(S, S, 0.0),
    Transform = e3d_mat:mul(e3d_mat:translate(S*Cx, S*Cy, 0.0), Transform0),
    We = wings_we:transform_vs(Transform, We0),
    move_and_scale_charts(RA, RP, S, [{C,We}|Acc]);
move_and_scale_charts([], [], _, Acc) -> Acc.

rotate_area(Fs, #we{vp=VTab}=We) ->
    Vs = gb_trees:to_list(VTab),
    [{_,Eds3}|_] = group_edge_loops(Fs,We),
    Eds4 = make_convex(reverse(Eds3), [], VTab),

    [{LV1,LV2,_,_Dist}|_] = lists:reverse(lists:keysort(4, Eds4)),
    LV1P = gb_trees:get(LV1, VTab),
    LV2P = gb_trees:get(LV2, VTab),
    Angle = math:atan2(element(2,LV2P)-element(2,LV1P),
		       element(1,LV2P)-element(1,LV1P)),
%    ?DBG("Angle ~p ~p P1 ~p P2 ~p~n", 
%	[Angle, _Dist, {LV1, LV1P}, {LV2,LV2P}]),
    if true ->
	    Rot = e3d_mat:rotate(-(Angle*180/math:pi()), {0.0,0.0,1.0}),
	    Res = [{Id,e3d_mat:mul_point(Rot, Vtx)} || {Id,Vtx} <- Vs],
%	    ?DBG("Rot angle ~p ~p~n", [Angle*180/math:pi(), Res]),
	    Res;
       true ->
	    Vs
    end.

-define(X(Vtx), element(1,element(2,Vtx))).
-define(Y(Vtx), element(2,element(2,Vtx))).
-define(Z(Vtx), element(3,element(2,Vtx))).
-define(Id(Vtx), element(1, Vtx)).
-define(Pos(Vtx), element(2,Vtx)). 

%angle_to_min_area(VsList, Vpos) ->
%    ?DBG("List ~p ~n", [VsList]),
%    {{P1,Xmin},{P2,Xmax},{P3,Ymin},{P4,Ymax}} = 
%	wpc_autouv:maxmin(VsList),
%    First = {FirstKey,_} = hd(VsList),
%    {LastKey,T0} = 
%	lists:foldl(fun(Next = {NId,_}, {Current,Tree}) ->
%			    {NId, gb_trees:insert(Current,Next,Tree)}
%		    end, 
%		    {FirstKey, gb_trees:empty()}, tl(VsList)),
%    T1 = gb_trees:insert(LastKey, First,T0),
%    Area = (Xmax-Xmin)*(Ymax-Ymin),
%    P1p = {P1,(gb_trees:get(P1, Vpos))#vtx.pos},   
%    P2p = {P2,(gb_trees:get(P2, Vpos))#vtx.pos},
%    P3p = {P3,(gb_trees:get(P3, Vpos))#vtx.pos},   
%    P4p = {P4,(gb_trees:get(P4, Vpos))#vtx.pos},
%    Ax = math:atan2(?Y(P2p)-?Y(P1p), ?X(P2p)-?X(P1p)),
%    Ay = math:atan2(?X(P4p)-?X(P3p), ?Y(P4p)-?Y(P3p)),
%    angle_to_min_area(0.0,Ax,P1p,P2p,Ay,P3p,P4p,{0.0,Area},T1).

%angle_to_min_area(Ta,_Ax,_P1,_P2,_Ay,_P3,_P4, {A,_},_)
%  when Ta > (3.14159/4) ->
%    A;
%angle_to_min_area(Ta,Ax0,P1,P2,Ay0,P3,P4,Old={_,Area0},Next) ->
%    NP1 = gb_trees:get(?Id(P1),Next),NP2 = gb_trees:get(?Id(P2),Next),
%    NP3 = gb_trees:get(?Id(P3),Next),NP4 = gb_trees:get(?Id(P4),Next),
%    AP1 = math:atan2(abs(?X(NP1)-?X(P1)),abs(?Y(NP1)-?Y(P1))),
%    AP2 = math:atan2(abs(?X(NP2)-?X(P2)),abs(?Y(NP2)-?Y(P2))),
%    AP3 = math:atan2(abs(?Y(NP3)-?Y(P3)),abs(?X(NP3)-?X(P3))),
%    AP4 = math:atan2(abs(?Y(NP4)-?Y(P4)),abs(?X(NP4)-?X(P4))),
%    Min = lists:min([AP1,AP2,AP3,AP4]),
%    NewA = Min-Ta,
%    Ax1 = Ax0+NewA,
%    Ay1 = Ay0+NewA,
%    L1 = e3d_vec:dist(?Pos(P1),?Pos(P2)) * math:cos(Ax1),
%    L2 = e3d_vec:dist(?Pos(P3),?Pos(P4)) * math:cos(Ay1),    
%    NewArea = L1*L2,
%    Area1 = if 
%		NewArea<0 -> %% Assert
%		    Old;
%		NewArea<Area0 ->{Min,NewArea};
%		true -> Old
%	    end,    
%    Deg45 = math:pi()/4,
%    if Min > Deg45 ->
%%	    ?DBG("Rot quit ~p ~p ~n", [Min,Old]),
%	    angle_to_min_area(Min,Ax0,P1,P2,Ay0,P3,P4,Old,Next);
%       true ->
%%	    ?DBG("Rot cont ~p ~p was ~p~n", [Min,Area1,Old]),
%	    case Min of
%		AP1 ->
%		    angle_to_min_area(Min,Ax1,NP1,P2,Ay1,P3,P4,Area1,Next);
%		AP2 ->
%		    angle_to_min_area(Min,Ax1,P1,NP2,Ay1,P3,P4,Area1,Next);
%		AP3 -> 
%		    angle_to_min_area(Min,Ax1,P1,P2,Ay1,NP3,P4,Area1,Next);
%		AP4 -> 
%		    angle_to_min_area(Min,Ax1,P1,P2,Ay1,P3,NP4,Area1,Next)
%	    end
%    end.
			          
%% Group edgeloops and return a list sorted by total dist.
%% [{TotDist, [{V1,V2,Edge,Dist},...]}, ...]
group_edge_loops(Fs, We) ->
    case auv_util:outer_edges(Fs, We, false) of
	[] -> [];
	Eds1 ->
	    Map = fun({Edge,Face}) ->
			  case gb_trees:get(Edge, We#we.es) of
			      #edge{vs=V1,ve=V2,lf=Face} ->
				  Dist = dist(V1,V2,We#we.vp),
				  {V1,V2,Edge,Dist};
			      #edge{vs=V2,ve=V1,rf=Face} ->
				  Dist = dist(V1,V2,We#we.vp),
				  {V1,V2,Edge,Dist}
			  end
		  end,
	    Eds2  = lists:map(Map, Eds1),
	    Loops = sort_edges(Eds2),
	    Add = fun({_,_,_,Dist}, Acc) ->  Acc + Dist end,
	    SumLoops = [{lists:foldl(Add, 0, Loop), Loop} 
			|| Loop <- Loops],
	    lists:reverse(lists:sort(SumLoops))
    end.

-define(PI, 3.141592).
-define(ALMOSTPI, (?PI-(0.5/180*?PI))). %% cluster together straight lines

make_convex([This, Next|Rest], Acc, Vs) ->
    case calc_dir(This,Next,Vs) >= ?ALMOSTPI of
	true ->
	    New = {element(1,This), element(2,Next), 
		   [element(3,This),element(3,Next)],
		   dist(element(1,This),element(2,Next),Vs)}, 
	    if Acc == [] ->
		    make_convex([New|Rest], Acc, Vs);
	       true ->
		    make_convex([hd(Acc),New|Rest], tl(Acc), Vs)
	    end;
	false ->
	    make_convex([Next|Rest], [This|Acc], Vs)
    end;
make_convex([This],Acc, Vs) ->
    [Next|Acc2] = lists:reverse(Acc),
    case calc_dir(This,Next,Vs) >= ?ALMOSTPI of
	true ->
	    New = {element(1,This), element(2,Next), 
		   [element(3,This),element(3,Next)],
		   dist(element(1,This),element(2,Next),Vs)},
	    Acc3 = reverse(Acc2),
	    make_convex([hd(Acc3),New], tl(Acc3), Vs);
	false ->
	    [This|Acc]
    end.

calc_dir({V11,V12,_E1,_},{V12,V22,_E2,_}, Vs) ->    
    C  = gb_trees:get(V12, Vs),
    V1 = gb_trees:get(V11, Vs),
    V2 = gb_trees:get(V22, Vs),
    {X1,Y1,_} = e3d_vec:sub(V1, C),
    {X2,Y2,_} = e3d_vec:sub(V2, C),
    Angle = case (math:atan2(Y1,X1) - math:atan2(Y2,X2)) of 
		A when A >= 0.0 ->
		    A;
		A -> 
		    2 * math:pi() + A
	    end,
%    ?DBG("Angle Vertex ~p Edges ~w : ~p-~p = ~p ~n",
%	[V12,{_E1,_E2},math:atan2(Y1,X1), math:atan2(Y2,X2),Angle]),
    Angle.

dist(V1, V2, Vs) ->
    e3d_vec:dist(gb_trees:get(V1, Vs), gb_trees:get(V2, Vs)).

%% Returns a list of loops 
sort_edges(Eds) ->
    EdsT = lists:foldl(fun({V1,V2,Edge,Dist}, Tree) ->
			       gb_trees:insert(V1,{V2,Edge,Dist}, Tree)
		       end, gb_trees:empty(), Eds),    
    {V1, {V2, Edge,Dist}, EdsT0} = gb_trees:take_smallest(EdsT),
    sort_edges(V2, EdsT0, [[{V1,V2,Edge,Dist}]]).

sort_edges(V21, EdsT0, All = [Current|Acc]) ->
    case gb_trees:lookup(V21, EdsT0) of
	{value, {V22,Edge2,Dist}} -> 
	    sort_edges(V22, gb_trees:delete(V21,EdsT0), 
		       [[{V21,V22,Edge2,Dist}|Current]|Acc]);
	none ->	    
	    case catch gb_trees:take_smallest(EdsT0) of
		{V1, {V2, Edge1,Dist}, EdsT1} ->
		    sort_edges(V2, EdsT1, [[{V1,V2,Edge1,Dist}]|All]);
		{'EXIT', _} -> %% Stop
		    All
	    end
    end.
