%%
%%  wings_magnet.erl --
%%
%%     This module implements the Magnet command.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_magnet.erl,v 1.4 2001/09/06 12:02:58 bjorng Exp $
%%

-module(wings_magnet).
-export([setup/3]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3]).

setup(Type, Dir, #st{selmode=vertex}=St) ->
    Vec = make_vector(Dir, St),
    Tvs = wings_sel:fold_shape(fun(Sh, Items, Acc) ->
				       setup_1(Sh, Items, Vec, Type, Acc)
			       end, [], St),
    wings_drag:init_drag(Tvs, constraint(Type), St#st{inf_r=?GROUND_GRID_SIZE}).

setup_1(#shape{id=Id,sh=We}=Sh, Items, Vec, Type, Acc) ->
    Tv = vertices_to_vertices(gb_sets:to_list(Items), We, Type, Vec),
    [{Id,Tv}|Acc].

constraint(free) -> view_dependent;
constraint(Other) -> none.
    
make_vector(x, St) -> {?GROUND_GRID_SIZE,0.0,0.0};
make_vector(y, St) -> {0.0,?GROUND_GRID_SIZE,0.0};
make_vector(z, St) -> {0.0,0.0,?GROUND_GRID_SIZE};
make_vector(free, St) -> free;
make_vector(normal, St) -> normal.

%%
%% Conversion of vertice selections to vertices. :-)
%% Not entirely pointless, as we'll need to add vectors for
%% the points (vertices).
%%

vertices_to_vertices(Vs, We, Type, normal) ->
    make_tvs(Type, vertex_normals(We, Vs));
vertices_to_vertices(Vs, We, Type, Vec) -> make_tvs(Type, Vs, Vec).

vertex_normals(We, Vs) ->
    foldl(fun(V, Acc) ->
		  Vec = e3d_vec:mul(wings_vertex:normal(V, We),
				    float(?GROUND_GRID_SIZE)),
		  [{Vec,[V]}|Acc]
	  end, [], Vs).

make_tvs(Type, Vs, Vec) ->
    make_tvs(Type, [{Vec,Vs}]).

make_tvs(Type, Tvs) ->
    DF = distance_fun(Type),
    fun(#shape{sh=#we{vs=Vtab0}=We0}=Sh, Dx, Dy, #st{inf_r=IR}=St) ->
	    Vtab = magnet_move(Tvs, Dx, Dy, {DF,IR}, St, Vtab0, Vtab0),
	    We = We0#we{vs=Vtab},
	    {shape,Sh#shape{sh=We}}
    end.

distance_fun(linear) ->
    fun(Dist) when float(Dist) ->
	    case 1-Dist of
		Negative when Negative =< 0 -> 0.0;
		Positive -> Positive
	    end
    end;
distance_fun(gaussian) ->
    fun(Dist) when float(Dist) ->
	    math:exp(-(Dist*Dist)/2)
    end.

magnet_move([{free,Vs}|Tvs], Dx, Dy, IR, St, OVtab, Vtab0) ->
    #st{azimuth=Az,elevation=El} = St,
    G = ?GROUND_GRID_SIZE,
    M0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    M = e3d_mat:mul(M1, e3d_mat:scale(G, G, G)),
    {Xt,Yt,Zt} = e3d_mat:mul_point(M, {Dx,Dy,0.0}),
    Vtab = magnet_move_1({Xt,Yt,Zt}, Vs, IR, OVtab, Vtab0),
    magnet_move(Tvs, Dx, Dy, IR, St, OVtab, Vtab);
magnet_move([{{Xt0,Yt0,Zt0},Vs}|Tvs], Dx, Dy, IR, St, OVtab, Vtab0) ->
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    Vtab = magnet_move_1({Xt,Yt,Zt}, Vs, IR, OVtab, Vtab0),
    magnet_move(Tvs, Dx, Dy, IR, St, OVtab, Vtab);
magnet_move([], Dx, Dy, IR, St, OVtab, Vtab) -> Vtab.

magnet_move_1(VtVec, Vs, IR, OVtab, Vtab) ->
    foldl(fun(V, Tab) -> 
		  Center = wings_vertex:pos(V, OVtab),
		  magnet_move_2(VtVec, Center, IR, OVtab, Tab)
	  end, Vtab, Vs).

magnet_move_2(TrVec, Center, {DF,IR}, OVtab, Vtab) ->
    wings_util:fold_vertex(
      fun (V, #vtx{pos=Pos0}=Vtx, Tab) ->
	      Dist0 = e3d_vec:dist(Pos0, Center) / IR,
	      case DF(Dist0) of
		  Dist when Dist < 1.0E-5 -> Tab;
		  Dist ->
		      #vtx{pos=Pos1} = gb_trees:get(V, Vtab),
		      Offset = e3d_vec:mul(TrVec, Dist),
		      Pos = e3d_vec:add(Pos1, Offset),
		      gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab)
	      end
      end, Vtab, OVtab).
