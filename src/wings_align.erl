%%
%%  wings_align.erl --
%%
%%     This module contains the Align and Center commands.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_align.erl,v 1.1.1.1 2001/08/14 18:16:40 bjorng Exp $
%%

-module(wings_align).
-export([align/2,center/2]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,reverse/1]).

align(Axis, St) ->
    Cs = wings_sel:centers(St),
    Center = wings_mat:average(Cs),
    move_to(Center, Cs, Axis, St).

center(Axis, St) ->
    Cs0 = wings_sel:centers(St),
    CommonCenter = wings_mat:average(Cs0),
    Cs = lists:duplicate(length(Cs0), CommonCenter),
    Center = wings_mat:zero(),
    move_to(Center, Cs, Axis, St).

move_to(Center, Cs, Axis, #st{selmode=body}=St0) ->
    {St,_} = wings_sel:mapfold(
	       fun(#shape{sh=#we{vs=Vtab0}=We}=Sh, [MyCenter|Centers]) ->
		       Offset0 = wings_mat:subtract(Center, MyCenter),
		       case filter_coord(Axis, Offset0) of
			   {0.0,0.0,0.0} -> {Sh,Centers};
			   Offset ->
			       Vtab = offset(Offset, Vtab0),
			       {Sh#shape{sh=We#we{vs=Vtab}},Centers}
		       end
	       end, Cs, St0),
    St;
move_to(Center, Cs, Axis, St0) ->
    {St,_} = wings_sel:mapfold_shape(
	       fun(Id, Vs, #we{vs=Vtab0}=We, [MyCenter|Centers]) ->
		       Offset0 = wings_mat:subtract(Center, MyCenter),
		       case filter_coord(Axis, Offset0) of
			   {0.0,0.0,0.0} -> {We,Centers};
			   Offset ->
			       Vtab = offset(Offset, Vtab0),
			       {We#we{vs=Vtab},Centers}
		       end
	       end, Cs, St0),
    St.
    
filter_coord(x, {X,_,_}) -> {X,0.0,0.0};
filter_coord(y, {_,Y,_}) -> {0.0,Y,0.0};
filter_coord(z, {_,_,Z}) -> {0.0,0.0,Z};
filter_coord(radial_x, {_,Y,Z}) -> {0.0,Y,Z};
filter_coord(radial_y, {X,_,Z}) -> {X,0.0,Z};
filter_coord(radial_z, {X,Y,_}) -> {X,Y,0.0};
filter_coord(all, All) -> All.

offset(Offset, Vtab0) ->
    Vtab = foldl(fun({V,#vtx{pos=Pos}=Vtx}, A) ->
			 [{V,Vtx#vtx{pos=wings_mat:add(Pos, Offset)}}|A]
		 end, [], gb_trees:to_list(Vtab0)),
    gb_trees:from_orddict(reverse(Vtab)).
		 
			      
