%%
%%  wings_vertex_cmd.erl --
%%
%%     This module contains most of the commands for vertices.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vertex_cmd.erl,v 1.1.1.1 2001/08/14 18:16:39 bjorng Exp $
%%

-module(wings_vertex_cmd).
-export([flatten/2,extrude/2,bevel/1,connect/1,connect/2,
	 tighten/1,tighten/4]).

-include("wings.hrl").

-import(lists, [member/2,keymember/3,foldl/3,mapfoldl/3,
		reverse/1,last/1,sort/1]).

-define(EXTRUDE_DIST, 2.0).

%%%
%%% The Flatten command.
%%%

flatten(Plane0, St) ->
    Plane = plane_normal(Plane0),
    wings_sel:map_shape(
      fun(Vs, We) ->
	      case gb_sets:size(Vs) of
		  Sz when Sz < 2 -> We;
		  Sz -> flatten(Vs, Plane, We)
	      end
      end, St).

plane_normal(x) -> {1.0,0.0,0.0};
plane_normal(y) -> {0.0,1.0,0.0};
plane_normal(z) -> {0.0,0.0,1.0}.

flatten(Vs, PlaneNormal, #we{vs=Vtab0}=We) ->
    Center = wings_vertex:center(Vs, We),
    Vtab = foldl(
	     fun(V, Tab0) ->
		     flatten_move(V, PlaneNormal, Center, Tab0)
	     end, Vtab0, gb_sets:to_list(Vs)),
    We#we{vs=Vtab}.

flatten_move(V, PlaneNormal, Center, Tab0) ->
    #vtx{pos=Pos0} = Vtx = gb_trees:get(V, Tab0),
    ToCenter = wings_mat:subtract(Center, Pos0),
    Dot = wings_mat:dot_product(ToCenter, PlaneNormal),
    ToPlane = wings_mat:mul(PlaneNormal, Dot),
    Pos = wings_mat:add(Pos0, ToPlane),
    gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab0).

%%%
%%% The Extrude command.
%%%

extrude(Type, St0) ->
    St = wings_sel:map_shape(fun extrude_vertices/2, St0),
    wings_move:setup(Type, St).

extrude_vertices(Vs0, We0) ->
    Vs = gb_sets:to_list(Vs0),
    We = foldl(fun(V, A) ->
		       ex_new_vertices(V, A)
	       end, We0, Vs).

ex_new_vertices(V, #we{vs=Vtab}=We0) ->
    Center = wings_vertex:pos(V, We0),
    {We,VsFaces} =
	wings_vertex:fold(
	  fun(Edge, Face, Rec, {W0,Vs}) ->
		  OtherV = wings_vertex:other(V, Rec),
		  Pos0 = wings_vertex:pos(OtherV, Vtab),
		  Dir0 = wings_mat:subtract(Pos0, Center),
		  Dist = case wings_mat:len(Dir0) of
			     D when D < ?EXTRUDE_DIST+0.25 -> D/2;
			     Other ->?EXTRUDE_DIST
			 end,
		  Dir = wings_mat:norm(Dir0),
		  Pos = wings_mat:add(Center, wings_mat:mul(Dir, Dist)),
		  {W,NewV,_} = wings_edge:fast_cut(Edge, Pos, W0),
		  {W,[NewV,Face|Vs]}
	  end, {We0,[]}, V, We0),
    ex_connect(VsFaces, VsFaces, We).

ex_connect([Va,Face|[Vb|_]=T], More, We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    ex_connect(T, More, We);
ex_connect([Va,Face], [Vb|_], We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    We.

%%%
%%% The Bevel command.
%%%

bevel(#st{sel=Vsel}=St0) ->
    {St,{Tvs0,FaceSel0}} =
	wings_sel:mapfold_shape(
	  fun(Id, Vs, We0, {Tvs,Fa}) ->
		  Iter = gb_sets:iterator(Vs),
		  {We,Tv,Fs0} = bevel_vertices(Iter, Vs, We0, We0, [], []),
		  Fs = gb_sets:from_list(Fs0),
		  {We,{[{Id,Tv}|Tvs],[{Id,Fs}|Fa]}}
	  end, {[],[]}, St0),
    FaceSel = sort(FaceSel0),
    {Min,Tvs} = bevel_normalize(Tvs0, Vsel),
    wings_drag:init_drag(Tvs, {0,Min}, St#st{selmode=face,sel=FaceSel}).

bevel_vertices(Iter0, Vs, WeOrig, We0, Acc0, Facc) ->
    case gb_sets:next(Iter0) of
	none -> {We0,Acc0,Facc};
	{V,Iter} ->
	    AdjFlag = adjacent(V, Vs, WeOrig),
	    case bevel_vertex(V, AdjFlag, We0, Acc0) of
		winged_vertex ->
		    bevel_vertices(Iter, Vs, WeOrig, We0, Acc0, Facc);
		{We,Acc,Face} ->
		    bevel_vertices(Iter, Vs, WeOrig, We, Acc, [Face|Facc])
	    end
    end.

bevel_vertex(V, AdjFlag, We, Vec0) ->
    Es = wings_vertex:fold(
	   fun(Edge, Face, Rec, Acc) ->
		   [{Edge,Face,Rec}|Acc]
	   end, [], V, We),
    case length(Es) of
	2 -> winged_vertex;
	NumEdges -> bevel_vertex_1(V, Es, NumEdges, AdjFlag, We, Vec0)
    end.

bevel_vertex_1(V, Es, NumEdges, AdjFlag, We0, Vec0) ->
    {InnerFace,We1} = wings_we:new_id(We0),
    {Ids,We} = wings_we:new_wrap_range(NumEdges, 2, We1),
    #we{es=Etab0,vs=Vtab0,fs=Ftab0}= We,
    Vtab = bevel_vertices_1(V, Ids, NumEdges, Vtab0),
    {_,Etab,Vec} = foldl(
		     fun(E, {Ids0,Etab1,Vs0}) ->
			     {Etab,Vec} = bevel(V, E, InnerFace, Ids0,
						AdjFlag, Vtab0, Etab1),
			     {wings_we:bump_id(Ids0),Etab,[Vec|Vs0]}
		     end, {Ids,Etab0,Vec0}, Es),
    Ftab = gb_trees:insert(InnerFace, #face{edge=wings_we:id(1, Ids)}, Ftab0),
    {We#we{es=Etab,fs=Ftab,vs=Vtab},Vec,InnerFace}.

bevel(V, {Edge,Face,Rec0}, InnerFace, Ids, AdjFlag, Vtab, Etab0) ->
    Vprev = wings_we:id(0, Ids),
    Eprev = wings_we:id(1, Ids),
    Va = wings_we:id(2, Ids),
    Ecurr = wings_we:id(3, Ids),
    Vb = wings_we:id(4, Ids),
    Enext = wings_we:id(5, Ids),
    Vpos = wings_vertex:pos(V, Vtab),
    {Rec,Curr,Vec0} =
	case Rec0 of
	    #edge{vs=V,ve=Vother,rf=Face} ->
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Eprev},
		 Rec0#edge{vs=Vb,ve=Va,lf=InnerFace,
			   rtsu=Edge,ltpr=Eprev,ltsu=Enext},
		 wings_mat:subtract(wings_vertex:pos(Vother, Vtab), Vpos)};
	    #edge{vs=V,ve=Vother} ->
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Enext},
		 Rec0#edge{vs=Vprev,ve=Va,lf=InnerFace,
			   rtsu=Edge,ltpr=Enext,ltsu=Eprev},
		 wings_mat:subtract(wings_vertex:pos(Vother, Vtab), Vpos)};
	    #edge{ve=V,vs=Vother,lf=Face} ->
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Eprev},
		 Rec0#edge{vs=Va,ve=Vb,rf=InnerFace,
			   ltsu=Edge,rtpr=Eprev,rtsu=Enext},
		 wings_mat:subtract(wings_vertex:pos(Vother, Vtab), Vpos)};
	    #edge{ve=V,vs=Vother} ->
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Enext},
		 Rec0#edge{vs=Va,ve=Vprev,rf=InnerFace,
			   ltsu=Edge,rtpr=Enext,rtsu=Eprev},
		 wings_mat:subtract(wings_vertex:pos(Vother, Vtab), Vpos)}

	end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    Vec = case AdjFlag of
	      true -> wings_mat:divide(Vec0, 2.0);
	      false -> Vec0
	  end,
    {gb_trees:insert(Ecurr, Curr, Etab),{Vec,Va}}.

bevel_vertices_1(V, Ids, N, Vtab0) ->
    Vtx = gb_trees:get(V, Vtab0),
    Vtab = gb_trees:delete(V, Vtab0),
    bevel_new_vertices(Ids, N, Vtx, Vtab).

bevel_new_vertices(Ids, N, Vtx, Vtab0) when N > 0 ->
    Id = wings_we:id(0, Ids),
    Vtab = gb_trees:insert(Id, Vtx#vtx{edge=Id+1}, Vtab0),
    bevel_new_vertices(wings_we:bump_id(Ids), N-1, Vtx, Vtab);
bevel_new_vertices(Ids, N, Vtx, Vtab) -> Vtab.

bevel_normalize(Tvs, Sel) ->
    bevel_normalize(Tvs, 1.0E200, []).

bevel_normalize([{Id,VecVs0}|Tvs], Min0, Acc) ->
    {VecVs,Min} = bevel_normalize_1(VecVs0, Min0),
    bevel_normalize(Tvs, Min, [{Id,VecVs}|Acc]);
bevel_normalize([], Min, Tvs) -> {Min,Tvs}.

bevel_normalize_1(VecVs, Min0) ->
    mapfoldl(fun({Vec,V}, Min0) ->
		     Min = case wings_mat:len(Vec) of
			       Len when Len < Min0 -> Len;
			       Len -> Min0
			   end,
		     {{wings_mat:norm(Vec),[V]},Min}
	     end, Min0, VecVs).

adjacent(V, Vs, We) ->
    wings_vertex:fold(
      fun(_, _, _, true) -> true;
	 (_, _, Rec, false) ->
	      gb_sets:is_member(wings_vertex:other(V, Rec), Vs)
      end, false, V, We).
    
%%%
%%% The Connect command.
%%%

connect(St) ->
    wings_sel:map_shape(fun connect/2, St).

connect(Vs, #we{}=We) ->
    FaceVs = wings_vertex:per_face(Vs, We),
    foldl(fun({Face,Vs}, Acc) ->
		 wings_vertex:connect(Face, Vs, Acc)
	 end, We, FaceVs).

%%%
%%% The Tighten command.
%%%

tighten(St0) ->
    {St,Tvs} = wings_sel:mapfold_shape(fun tighten/4, [], St0),
    wings_drag:init_drag(Tvs, none, St).

tighten(Id, Vs, #we{vs=Vtab}=We, Acc) when list(Vs) ->
    Tv = foldl(
	   fun(V, A) ->
		   Vec = tighten_vec(V, Vs, We),
		   [{Vec,[V]}|A]
	   end, [], Vs),
    {We,[{Id,Tv}|Acc]};
tighten(Id, Vs, We, Acc) ->
    tighten(Id, gb_sets:to_list(Vs), We, Acc).

tighten_vec(V, Vs, #we{vs=Vtab}=We) ->
    Nbs = wings_vertex:fold(
	    fun(_, _, Rec, A) ->
		    [wings_vertex:other(V, Rec)|A]
	    end, [], V, We),
    Center = wings_vertex:center(Nbs, Vtab),
    wings_mat:subtract(Center, wings_vertex:pos(V, Vtab)).
    
