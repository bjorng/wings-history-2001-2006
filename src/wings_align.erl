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
%%     $Id: wings_align.erl,v 1.11 2001/10/26 12:40:52 bjorng Exp $
%%

-module(wings_align).
-export([align/2,center/2,copy_bb/1,
	 scale_to_bb/2,scale_to_bb_prop/2,move_to_bb/2]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,reverse/1]).

align(Axis, #st{sel=[]}=St) -> St;
align(Axis, St) ->
    Cs = wings_sel:centers(St),
    Center = e3d_vec:average(Cs),
    move_to(Center, Cs, Axis, St).

center(Axis, #st{sel=[]}=St) -> St;
center(Axis, St) ->
    Cs0 = wings_sel:centers(St),
    CommonCenter = e3d_vec:average(Cs0),
    Cs = lists:duplicate(length(Cs0), CommonCenter),
    Center = e3d_vec:zero(),
    move_to(Center, Cs, Axis, St).

copy_bb(St) ->
    BB = wings_sel:bounding_box(St),
    St#st{bb=BB}.

scale_to_bb(Dir, #st{bb=none}=St) -> St;
scale_to_bb(Dir, #st{bb=Dest}=St) ->
    case wings_sel:bounding_box(St) of
	none -> St;
	Src ->
	    Matrix = make_scale(Dir, Src, Dest),
	    transform(Matrix, St)
    end.

scale_to_bb_prop(Dir, #st{bb=none}=St) -> St;
scale_to_bb_prop(Dir, #st{bb=Dest}=St) ->
    case wings_sel:bounding_box(St) of
	none -> St;
	Src ->
	    Matrix = make_prop_scale(Dir, Src, Dest),
	    transform(Matrix, St)
    end.

move_to_bb(Dir, #st{bb=none}=St) -> St;
move_to_bb(Dir, #st{bb=Dest}=St) ->
    case wings_sel:bounding_box(St) of
	none -> St;
	Src ->
	    Matrix = make_move(Dir, Src, Dest),
	    transform(Matrix, St)
    end.

transform(Matrix, #st{selmode=body}=St) ->
    wings_sel:map(
      fun(#shape{sh=We0}=Sh) ->
	      We = wings_we:transform_vs(Matrix, We0),
	      Sh#shape{sh=We}
      end, St);
transform(Matrix, St) ->
    wings_sel:map_shape(
      fun(Items, We0) ->
	      wings_we:transform_vs(Matrix, We0)
      end, St).

make_move(Dir, Src, Dest0) ->
    SrcMid = e3d_vec:average(Src),
    DestMid = e3d_vec:average(Dest0),
    Tvec = filter_coord(Dir, e3d_vec:sub(DestMid, SrcMid)),
    e3d_mat:translate(Tvec).

make_scale(Dir, Src0, Dest0) ->
    Src1 = e3d_vec:sub(Src0),
    Dest1 = e3d_vec:sub(Dest0),
    Src = filter_coord(Dir, Src1),
    Dest = filter_coord(Dir, Dest1),
    Sc0 = make_scales(Dest, Src),
    [ScX,ScY,ScZ] = map(fun(none) -> 1.0;
			   (Sc) -> Sc end, Sc0),
    e3d_mat:scale(ScX, ScY, ScZ).

make_prop_scale(Dir, Src0, Dest0) ->
    Src1 = e3d_vec:sub(Src0),
    Dest1 = e3d_vec:sub(Dest0),
    Src = filter_coord(Dir, Src1),
    Dest = filter_coord(Dir, Dest1),
    Sc0 = make_scales(Dest, Src),
    Max = max_scale(Sc0),
    e3d_mat:scale(Max, Max, Max).

make_scales(Ta, Tb) ->
    make_scales(1, Ta, Tb).

make_scales(I, Ta, Tb) when I > size(Ta); I > size(Tb) -> [];
make_scales(I, Ta, Tb) ->
    S = case {element(I, Ta),element(I, Tb)} of
	    {A,0.0} -> none;
	    {A,B} ->
		case catch A / B of		%catch if B is very small
		    {'EXIT',_} -> none;
		    Q -> Q
		end
	end,
    [S|make_scales(I+1, Ta, Tb)].

max_scale([none|Ss]) -> max_scale(Ss);
max_scale([S|Ss]) -> max_scale(Ss, S).

max_scale([none|Ss], Max) ->
    max_scale(Ss, Max);
max_scale([S|Ss], Max) when Max < S ->
    max_scale(Ss, S);
max_scale([_|Ss], Max) ->
    max_scale(Ss, Max);
max_scale([], Max) -> Max.

move_to(Center, Cs, Axis, #st{selmode=body}=St0) ->
    {St,_} = wings_sel:mapfold(
	       fun(#shape{sh=#we{vs=Vtab0}=We}=Sh, [MyCenter|Centers]) ->
		       Offset0 = e3d_vec:sub(Center, MyCenter),
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
		       Offset0 = e3d_vec:sub(Center, MyCenter),
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
			 [{V,Vtx#vtx{pos=e3d_vec:add(Pos, Offset)}}|A]
		 end, [], gb_trees:to_list(Vtab0)),
    gb_trees:from_orddict(reverse(Vtab)).
