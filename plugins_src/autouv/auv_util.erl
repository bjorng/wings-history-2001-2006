%%%-------------------------------------------------------------------
%%% File    : auv_util.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Algotrihms for placing charts on texture.
%%%
%%% Created :  7 Nov 2002 by Bjorn Gustavsson
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv_util.erl,v 1.1 2002/11/08 17:37:17 bjorng Exp $

-module(auv_util).

-export([maxmin/1]).
-export([moveAndScale/5]).
-export([outer_edges/2,outer_edges/3]).

-include("wings.hrl").
-include("auv.hrl").

-import(lists, [foldl/3,reverse/1,sort/1]).

moveAndScale([{Id, {X0, Y0,_}}|R], XD, YD, Scale, Acc) ->
    moveAndScale(R, XD,YD, Scale, 
		 [{Id, {X0*Scale+XD,Y0*Scale+YD,0.0}}|Acc]);
moveAndScale([],_,_,_,Acc) ->
    Acc.

maxmin([{Id, {X,Y,_}}|Rest]) ->
    maxmin(Rest, {Id, X},{Id, X},{Id, Y},{Id, Y});
maxmin([{Id, {X,Y}}|Rest]) ->
    maxmin(Rest, {Id, X},{Id, X},{Id, Y},{Id, Y}).

maxmin([],Xmin,Xmax,Ymin,Ymax) ->
    {Xmin,Xmax,Ymin,Ymax};
maxmin([{Id, {X,Y,_}}|Rest], 
       XMin={_IdX0,X0}, XMax={_IdX1,X1}, 
       YMin={_IdY0,Y0}, YMax={_IdY1,Y1}) ->
    if 	X > X1 ->
	    if Y > Y1 -> maxmin(Rest, XMin, {Id,X}, YMin, {Id,Y});
	       Y < Y0 -> maxmin(Rest, XMin, {Id,X}, {Id,Y}, YMax);
	       true ->   maxmin(Rest, XMin, {Id,X}, YMin, YMax)
	    end;
	X < X0 ->
	    if Y > Y1 -> maxmin(Rest,{Id,X}, XMax, YMin, {Id,Y});
	       Y < Y0 -> maxmin(Rest,{Id,X}, XMax, {Id,Y}, YMax);
	       true ->   maxmin(Rest,{Id,X}, XMax, YMin, YMax)
	    end;
	Y > Y1 ->
	    maxmin(Rest,XMin, XMax, YMin, {Id,Y});
	Y < Y0 ->
	    maxmin(Rest,XMin, XMax, {Id,Y}, YMax);
	true ->
	    maxmin(Rest,XMin, XMax, YMin, YMax)
    end;
maxmin([{Id, {X,Y}}|Rest], 
       XMin={_IdX0,X0}, XMax={_IdX1,X1}, 
       YMin={_IdY0,Y0}, YMax={_IdY1,Y1}) ->
    if 	X > X1 ->
	    if Y > Y1 -> maxmin(Rest, XMin, {Id,X}, YMin, {Id,Y});
	       Y < Y0 -> maxmin(Rest, XMin, {Id,X}, {Id,Y}, YMax);
	       true ->   maxmin(Rest, XMin, {Id,X}, YMin, YMax)
	    end;
	X < X0 ->
	    if Y > Y1 -> maxmin(Rest,{Id,X}, XMax, YMin, {Id,Y});
	       Y < Y0 -> maxmin(Rest,{Id,X}, XMax, {Id,Y}, YMax);
	       true ->   maxmin(Rest,{Id,X}, XMax, YMin, YMax)
	    end;
	Y > Y1 ->
	    maxmin(Rest,XMin, XMax, YMin, {Id,Y});
	Y < Y0 ->
	    maxmin(Rest,XMin, XMax, {Id,Y}, YMax);
	true ->
	    maxmin(Rest,XMin, XMax, YMin, YMax)
    end.

outer_edges(Faces0, We) ->
    outer_edges(Faces0, We, true).
outer_edges(Faces0, We, VisibleOnly) ->    
    %% I use normals here to detect direction of face and remove 
    %% faces with wrong direction.
    Faces1 = case VisibleOnly of 
		 true ->
		     foldl(fun(Face, Acc)-> 
				   Zval = e3d_vec:dot(wings_face:normal(Face, We), 
						      {0.0,0.0,1.0}),
				   case Zval >= 0.0 of
				       true -> [Face|Acc];
				       _ -> Acc
				   end
			   end, [], Faces0);
		 false ->
		     Faces0
	     end,
    S = wings_face:fold_faces(fun(Face, _, E, _, A) -> [{E,Face}|A] end, [], Faces1, We),
    outer_edges_1(sort(S), []).
outer_edges_1([{E,_},{E,_}|T], Out) ->
    outer_edges_1(T, Out);
outer_edges_1([E|T], Out) ->
    outer_edges_1(T, [E|Out]);
outer_edges_1([], Out) -> reverse(Out).
