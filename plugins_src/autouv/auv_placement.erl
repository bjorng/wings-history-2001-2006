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
%%     $Id: auv_placement.erl,v 1.2 2002/10/08 11:32:24 dgud Exp $


-module(auv_placement).

-include("auv.hrl").
-include("wings.hrl").

-export([place_areas/1]).
-import(lists, [max/1, sort/1, map/2, reverse/1]).

%% Returns a gb_tree with areas..
place_areas(Areas) ->    
    {Sizes0,_} = lists:mapfoldl(fun(#a{size = {X,Y}}, C) ->
					{{X, Y, C}, C+1}
				end, 1, Areas),
    
    {Positions0, Max} = fill(Sizes0, [0,0]),
    %%    io:format("~p ~p ~p ~n", 
    %%              [length(Areas), length(Sizes0), length(Positions0)]),
    Scale = 1 / max(Max),
    Areas1 = move_and_scale_areas(Areas, 
				  lists:sort(Positions0), 
				  Scale, []),
    {Areas2, _} = lists:mapfoldl(fun(Q, C) -> {{C,Q}, C+1} end,
				 1, Areas1),
    gb_trees:from_orddict(Areas2).

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

revapp([H|R], List) ->
    revapp(R, [H|List]);
revapp([], List) ->
    List.

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
move_and_scale_areas([Area|RA], [{_,{Cx,Cy}}|RP], S, Acc) ->
    {SX, SY} = Area#a.size,
    New = Area#a{center = {Cx*S,Cy*S}, size={SX*S,SY*S}, scale=S},
    move_and_scale_areas(RA, RP, S, [New|Acc]);
move_and_scale_areas([],[],_,Acc) ->
    Acc.

rotate_area(Fs, Vs0, We) ->
    NewVs = 
	lists:foldl(fun({No, Pos}, Tree) ->
			    Vtx = gb_trees:get(No,Tree),
			    gb_trees:update(No, Vtx#vtx{pos=Pos}, Tree)
		    end, We#we.vs, Vs0),
    
    Eds1 = wpc_autouv:outer_edges(Fs, We#we{vs = NewVs}),
    Map = fun({Edge,Face}) ->
		  case gb_trees:get(Edge, We#we.es) of
		      #edge{vs=V1,ve=V2,lf=Face} ->
			  {V1,V2,Edge};
		      #edge{vs=V2,ve=V1,rf=Face} ->
			  {V1,V2,Edge}
		  end
	  end,
    Eds2 = lists:map(Map, Eds1),
    [Eds3|_] = sort_edges(Eds2),  %% BUGBUG pick largest i.e not a hole
    Eds4 = make_convex(reverse(Eds3), [], NewVs).

make_convex([This, Next|Rest], Acc, Vs) ->
    case calc_dir(This,Next,Vs) > math:pi() of
	true ->
	    New = {element(1,This), element(2,Next), 
		   [element(3,This),element(3,Next)]}, 
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
    case calc_dir(This,Next,Vs) > math:pi() of
	true ->
	    New = {element(1,This), element(2,Next), 
		   [element(3,This),element(3,Next)]},
	    Acc3 = reverse(Acc2),
	    make_convex([hd(Acc3),New], tl(Acc3), Vs);
	false ->
	    [This|Acc]
    end.

calc_dir({V11,V12,E1},{V12,V22,E2}, Vs) ->    
    C = gb_trees:get(V12, Vs),
    V1 = gb_trees:get(V11, Vs),
    V2 = gb_trees:get(V22, Vs),
    {X1,Y1,_} = e3d_vec:sub(V1#vtx.pos, C#vtx.pos),
    {X2,Y2,_} = e3d_vec:sub(V2#vtx.pos, C#vtx.pos),
    Angle = case (math:atan2(Y1,X1) - math:atan2(Y2,X2)) of 
		A when A >= 0.0 ->
		    A;
		A -> 
		    2 * math:pi() + A
	    end,
%    ?DBG("Angle Vertex ~p Edges ~p : ~p-~p = ~p ~n",
%	      [V12,{E1,E2},math:atan2(Y1,X1), math:atan2(Y2,X2),Angle]),
    Angle.

%% Returns a list of loops 
sort_edges(Eds) ->
    EdsT = lists:foldl(fun({V1,V2,Edge}, Tree) ->
			       gb_trees:insert(V1,{V2,Edge}, Tree)
		       end, gb_trees:empty(), Eds),    
    {V1, {V2, Edge}, EdsT0} = gb_trees:take_smallest(EdsT),
    sort_edges(V2, EdsT0, [[{V1,V2,Edge}]]).

sort_edges(V21, EdsT0, All = [Current|Acc]) ->
    case gb_trees:lookup(V21, EdsT0) of
	{value, {V22,Edge2}} -> 
	    sort_edges(V22, gb_trees:delete(V21,EdsT0), 
		       [[{V21,V22,Edge2}|Current]|Acc]);
	none ->	    
	    case catch gb_trees:take_smallest(EdsT0) of
		{V1, {V2, Edge1}, EdsT1} ->
		    sort_edges(V2, EdsT1, [[{V1,V2,Edge1}]|All]);
		{'EXIT', _} -> %% Stop
		    All
	    end
    end.
