%%
%%  wings_vertex_cmd.erl --
%%
%%     This module contains most of the commands for vertices.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vertex_cmd.erl,v 1.24 2002/03/13 11:57:39 bjorng Exp $
%%

-module(wings_vertex_cmd).
-export([menu/3,command/2,tighten/3,connect/2]).

-include("wings.hrl").
-import(lists, [member/2,keymember/3,foldl/3,mapfoldl/3,
		reverse/1,last/1,sort/1]).
-import(wings_draw, [model_changed/1]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Menu = [{"Vertex operations",ignore},
	    separator,
	    {"Move",{move,Dir}},
	    wings_menu_util:rotate(),
	    wings_menu_util:scale(),
	    separator,
	    {"Extrude",{extrude,Dir}},
	    separator,
	    wings_menu_util:flatten(),
	    separator,
	    {"Connect",connect,
	     "Create a new edge to connect selected vertices"},
	    {"Tighten",tighten},
	    {"Bevel",bevel,"Create faces of selected vertices"},
	    {"Collapse",collapse,"Delete selected vertices"},
	    {"Dissolve",dissolve,"Delete selected vertices"},
	    separator,
	    {"Deform",wings_deform:sub_menu(St)}|wings_vec:menu(St)],
    wings_menu:popup_menu(X, Y, vertex, Menu, St).

%% Vertex menu.
command({flatten,Plane}, St) ->
    {save_state,model_changed(flatten(Plane, St))};
command(connect, St) ->
    {save_state,model_changed(connect(St))};
command(tighten, St) ->
    tighten(St);
command(bevel, St) ->
    ?SLOW(bevel(St));
command({extrude,Type}, St) ->
    ?SLOW(extrude(Type, St));
command({deform,Deform}, St0) ->
    ?SLOW(wings_deform:command(Deform, St0));
command(auto_smooth, St) ->
    wings_body:auto_smooth(St);
command(dissolve, St) ->
    {save_state,model_changed(dissolve(St))};
command(collapse, St) ->
    {save_state,model_changed(wings_collapse:collapse(St))};
command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St).

%%%
%%% The Flatten command.
%%%

flatten({Plane,Center}, St) ->
    flatten(Plane, Center, St);
flatten(Plane, St) ->
    flatten(Plane, average, St).

flatten(Plane0, average, St) ->
    Plane = wings_util:make_vector(Plane0),
    wings_sel:map(
      fun(Vs, We) ->
	      wings_vertex:flatten(Vs, Plane, We)
      end, St);
flatten(Plane0, Center, St) ->
    Plane = wings_util:make_vector(Plane0),
    wings_sel:map(
      fun(Vs, We) ->
	      wings_vertex:flatten(Vs, Plane, Center, We)
      end, St).
    
%%%
%%% The Extrude command.
%%%

extrude(Type, St0) ->
    {St,Tvs} = wings_sel:mapfold(
		 fun(Vs, We0, Acc) ->
			 extrude_vertices(Vs, We0, Acc)
		 end, [], St0),
    wings_move:plus_minus(Type, Tvs, St).

extrude_vertices(Vs, We0, Acc) ->
    We = foldl(fun(V, A) ->
		       ex_new_vertices(V, We0, A)
	       end, We0, gb_sets:to_list(Vs)),
    NewVs = gb_sets:to_list(wings_we:new_items(vertex, We0, We)),
    {We,[{Vs,NewVs,We}|Acc]}.

ex_new_vertices(V, OrigWe, #we{vs=Vtab}=We0) ->
    Center = wings_vertex:pos(V, We0),
    {We,VsFaces} =
	wings_vertex:fold(
	  fun(Edge, Face, Rec, {W0,Vs}) ->
		  OtherV = wings_vertex:other(V, Rec),
		  R = edge_ratio(OtherV, OrigWe),
		  Pos0 = wings_vertex:pos(OtherV, Vtab),
		  Dir = e3d_vec:sub(Pos0, Center),
		  Pos = e3d_vec:add(Center, e3d_vec:mul(Dir, R)),
		  {W,NewV} = wings_edge:fast_cut(Edge, Pos, W0),
		  {W,[NewV,Face|Vs]}
	  end, {We0,[]}, V, We0),
    ex_connect(VsFaces, VsFaces, We).

edge_ratio(V, #we{vs=Vtab}) ->
    case gb_trees:is_defined(V, Vtab) of
	false -> 1/3;
	true -> 0.25
    end.

ex_connect([Va,Face|[Vb|_]=T], More, We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    ex_connect(T, More, We);
ex_connect([Va,Face], [Vb|_], We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    We.

%%%
%%% The Bevel command.
%%%

bevel(St0) ->
    {St,{Tvs0,FaceSel}} =
	wings_sel:mapfold(
	  fun(Vs, #we{id=Id}=We0, {Tvs,Fa}) ->
		  Iter = gb_sets:iterator(Vs),
		  {We,Tv,Fs0} = bevel_vertices(Iter, Vs, We0, We0, [], []),
		  Fs = gb_sets:from_list(Fs0),
		  {We,{[{Id,Tv}|Tvs],[{Id,Fs}|Fa]}}
	  end, {[],[]}, St0),
    {Min,Tvs} = bevel_normalize(Tvs0),
    wings_drag:setup(Tvs, [{distance,{0.0,Min}}],
			  wings_sel:set(face, FaceSel, St)).

bevel_vertices(Iter0, Vs, WeOrig, We0, Acc0, Facc) ->
    case gb_sets:next(Iter0) of
	none -> {We0,Acc0,Facc};
	{V,Iter} ->
	    Adj = adjacent(V, Vs, WeOrig),
	    case bevel_vertex(V, Adj, We0, Acc0) of
		winged_vertex ->
		    bevel_vertices(Iter, Vs, WeOrig, We0, Acc0, Facc);
		{We,Acc,Face} ->
		    bevel_vertices(Iter, Vs, WeOrig, We, Acc, [Face|Facc])
	    end
    end.

bevel_vertex(V, Adj, We, Vec0) ->
    Es = wings_vertex:fold(
	   fun(Edge, Face, Rec, Acc) ->
		   [{Edge,Face,Rec}|Acc]
	   end, [], V, We),
    case length(Es) of
	2 -> winged_vertex;
	NumEdges -> bevel_vertex_1(V, Es, NumEdges, Adj, We, Vec0)
    end.

bevel_vertex_1(V, Es, NumEdges, Adj, We0, Vec0) ->
    {InnerFace,We1} = wings_we:new_id(We0),
    {Ids,We} = wings_we:new_wrap_range(NumEdges, 2, We1),
    #we{es=Etab0,vs=Vtab0,fs=Ftab0}= We,
    Vtab = bevel_vertices_1(V, Ids, NumEdges, Vtab0),
    {_,Etab,Vec} = foldl(
		     fun(E, {Ids0,Etab1,Vs0}) ->
			     {Etab,Vec} = bevel(V, E, InnerFace, Ids0,
						Adj, Vtab0, Etab1),
			     {wings_we:bump_id(Ids0),Etab,[Vec|Vs0]}
		     end, {Ids,Etab0,Vec0}, Es),
    Ftab = gb_trees:insert(InnerFace, #face{edge=wings_we:id(1, Ids)}, Ftab0),
    {We#we{es=Etab,fs=Ftab,vs=Vtab},Vec,InnerFace}.

bevel(V, {Edge,Face,Rec0}, InnerFace, Ids, Adj, Vtab, Etab0) ->
    Vprev = wings_we:id(0, Ids),
    Eprev = wings_we:id(1, Ids),
    Va = wings_we:id(2, Ids),
    Ecurr = wings_we:id(3, Ids),
    Vb = wings_we:id(4, Ids),
    Enext = wings_we:id(5, Ids),
    Vpos = wings_vertex:pos(V, Vtab),
    {Rec,Curr,Vec} =
	case Rec0 of
	    #edge{vs=V,ve=Vother,rf=Face} ->
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Eprev},
		 Rec0#edge{vs=Vb,ve=Va,lf=InnerFace,
			   rtsu=Edge,ltpr=Eprev,ltsu=Enext},
		 bevel_vec(Adj, Vother, Vpos, Vtab)};
	    #edge{vs=V,ve=Vother} ->
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Enext},
		 Rec0#edge{vs=Vprev,ve=Va,lf=InnerFace,
			   rtsu=Edge,ltpr=Enext,ltsu=Eprev},
		 bevel_vec(Adj, Vother, Vpos, Vtab)};
	    #edge{ve=V,vs=Vother,lf=Face} ->
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Eprev},
		 Rec0#edge{vs=Va,ve=Vb,rf=InnerFace,
			   ltsu=Edge,rtpr=Eprev,rtsu=Enext},
		 bevel_vec(Adj, Vother, Vpos, Vtab)};
	    #edge{ve=V,vs=Vother} ->
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Enext},
		 Rec0#edge{vs=Va,ve=Vprev,rf=InnerFace,
			   ltsu=Edge,rtpr=Enext,rtsu=Eprev},
		 bevel_vec(Adj, Vother, Vpos, Vtab)}

	end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    {gb_trees:insert(Ecurr, Curr, Etab),{Vec,Va}}.

bevel_vec(Adj, Vother, Vpos, Vtab) ->
    Opos = wings_vertex:pos(Vother, Vtab),
    case member(Vother, Adj) of
	true ->
	    e3d_vec:sub(e3d_vec:average([Opos,Vpos]), Vpos);
	false ->
	    e3d_vec:sub(Opos, Vpos)
    end.

bevel_vertices_1(V, Ids, N, Vtab0) ->
    Vtx = gb_trees:get(V, Vtab0),
    Vtab = gb_trees:delete(V, Vtab0),
    bevel_new_vertices(Ids, N, Vtx, Vtab).

bevel_new_vertices(Ids, N, Vtx, Vtab0) when N > 0 ->
    Id = wings_we:id(0, Ids),
    Vtab = gb_trees:insert(Id, Vtx#vtx{edge=Id+1}, Vtab0),
    bevel_new_vertices(wings_we:bump_id(Ids), N-1, Vtx, Vtab);
bevel_new_vertices(_Ids, _N, _Vtx, Vtab) -> Vtab.

bevel_normalize(Tvs) ->
    bevel_normalize(Tvs, 1.0E200, []).

bevel_normalize([{Id,VecVs0}|Tvs], Min0, Acc) ->
    {VecVs,Min} = bevel_normalize_1(VecVs0, Min0),
    bevel_normalize(Tvs, Min, [{Id,VecVs}|Acc]);
bevel_normalize([], Min, Tvs) -> {Min,Tvs}.

bevel_normalize_1(VecVs, Min0) ->
    mapfoldl(fun({Vec,V}, M0) ->
		     Min = case e3d_vec:len(Vec) of
			       Len when Len < M0 -> Len;
			       Len -> M0
			   end,
		     {{e3d_vec:norm(Vec),[V]},Min}
	     end, Min0, VecVs).

adjacent(V, Vs, We) ->
    wings_vertex:fold(
      fun(_, _, Rec, A) ->
	      OtherV = wings_vertex:other(V, Rec),
	      case gb_sets:is_member(OtherV, Vs) of
		  true -> [OtherV|A];
		  false -> A
	      end
      end, [], V, We).
    
%%%
%%% The Connect command.
%%%

connect(St) ->
    wings_sel:map(fun connect/2, St).

connect(Vs0, #we{}=We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun({Face,Vs}, Acc) ->
		  wings_vertex:connect(Face, Vs, Acc)
	  end, We, FaceVs).

%%%
%%% The Tighten command.
%%%

tighten(St) ->
    Tvs = wings_sel:fold(fun tighten/3, [], St),
    wings_drag:setup(Tvs, [percent], St).

tighten(Vs, #we{id=Id}=We, Acc) when is_list(Vs) ->
    Tv = foldl(
	   fun(V, A) ->
		   Vec = tighten_vec(V, We),
		   [{Vec,[V]}|A]
	   end, [], Vs),
    [{Id,Tv}|Acc];
tighten(Vs, We, Acc) -> 
    tighten(gb_sets:to_list(Vs), We, Acc).

tighten_vec(V, #we{vs=Vtab}=We) ->
    Nbs = wings_vertex:fold(
	    fun(_, _, Rec, A) ->
		    [wings_vertex:other(V, Rec)|A]
	    end, [], V, We),
    Center = wings_vertex:center(Nbs, Vtab),
    e3d_vec:sub(Center, wings_vertex:pos(V, Vtab)).
    
%%%
%%% The Dissolve command. Like Collapse, but stays in vertex mode
%%% (without any selection).
%%%

dissolve(St0) ->
    St = wings_collapse:collapse(St0),
    St#st{selmode=vertex,sel=[]}.
