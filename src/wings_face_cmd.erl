%%
%%  wings_face_cmd.erl --
%%
%%     This module contains most of the face commands.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_face_cmd.erl,v 1.31 2002/02/01 22:39:43 bjorng Exp $
%f%

-module(wings_face_cmd).
-export([extrude/2,extrude_region/2,extract_region/2,
	 inset/1,dissolve/1,smooth/1,bridge/1,
	 intrude/1,mirror/1,flatten/2,flatten_move/2,
	 lift_selection/1,lift/2]).
-export([outer_edge_partition/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keysort/2,
		keymember/3,keysearch/3,keydelete/3,
		member/2,seq/2,last/1]).

%%%
%%% Extrude, Extrude Region, and Inset commands.
%%%

extrude(Type, St) ->
    wings_move:setup(Type, extrude_faces(St)).

inset(St) ->
    wings_scale:inset(extrude_faces(St)).

extrude_faces(St) ->
    wings_sel:map(fun(Faces, We) ->
			  wings_extrude_face:faces(Faces, We)
		  end, St).

%%% Extrude the selected regions.

extrude_region(Type, St0) ->
    {St1,Sel0} = wings_sel:mapfold(fun extrude_region/3, [], St0),
    St = St1#st{sel=reverse(Sel0)},
    wings_move:setup(Type, St).

extrude_region(Faces0, #we{id=Id}=We0, Acc) ->
    %% We KNOW that a gb_set with fewer elements sorts before
    %% a gb_set with more elements.
    Rs = sort(wings_sel:face_regions(Faces0, We0)),
    {We,Sel} = extrude_region_1(Rs, We0, []),
    {We,[{Id,Sel}|Acc]}.

extrude_region_1([Faces0|Rs0]=Rs, We0, Acc) ->
    case gb_sets:size(Faces0) of
	1 ->
	    [Face] = gb_sets:to_list(Faces0),
	    extrude_region_1(Rs0, We0, [Face|Acc]);
	Other ->
	    We = wings_extrude_face:faces(Acc, We0),
	    Sel = [gb_sets:from_list(Acc)],
	    extrude_region_2(Rs, We, Sel)
    end;
extrude_region_1([], We0, Faces) ->
    We = wings_extrude_face:faces(Faces, We0),
    {We,gb_sets:from_list(Faces)}.

extrude_region_2([Faces|Rs], We0, Sel0) ->
    {We,Sel} = wings_extrude_face:region(Faces, We0),
    extrude_region_2(Rs, We, [Sel|Sel0]);
extrude_region_2([], We, Sel) ->
    {We,gb_sets:union(Sel)}.

%%%
%%% The Extract Region command.
%%%

extract_region(Type, #st{onext=Id0,shapes=Shapes0}=St0) ->
    St1 = wings_sel:fold(
	    fun(Faces, We, #st{sel=Sel0,onext=Oid}=S0) ->
		    S = wings_shape:insert(We, "extract", S0),
		    Sel = [{Oid,Faces}|Sel0],
		    S#st{sel=Sel}
	    end, St0#st{sel=[]}, St0),
    Sel = reverse(St1#st.sel),
    St2 = St1#st{sel=Sel},
    St3 = extract_inverse(St2),
    St4 = dissolve(St3),
    St = St4#st{sel=Sel},
    wings_move:setup(Type, St).

extract_inverse(St) ->
    Sel = wings_sel:fold(
	    fun(Faces, #we{id=Id}=We, A) ->
		    Diff = wings_sel:inverse_items(face, Faces, We),
		    case gb_sets:is_empty(Diff) of
			true -> A;
			false -> [{Id,Diff}|A]
		    end
	    end, [], St),
    St#st{sel=reverse(Sel)}.
    
%%%
%%% The Dissolve command.
%%%

dissolve(St0) ->
    {St,Sel} = wings_sel:mapfold(fun dissolve/3, [], St0),
    St#st{sel=reverse(Sel)}.

dissolve(Faces, #we{id=Id}=We0, Acc) ->
    We = dissolve_1(Faces, We0, We0),
    case wings_we:is_consistent(We) of
	true ->
	    Sel = wings_we:new_items(face, We0, We),
	    {We,[{Id,Sel}|Acc]};
	false ->
	    throw({command_error,
		   "Dissolving would cause an inconsistent object structure."})
    end.
		  
dissolve_1(Faces, WeOrig, #we{fs=Ftab}=We0) ->
    {Face,_} = gb_sets:take_smallest(Faces),
    #face{mat=Mat} = gb_trees:get(Face, Ftab),
    Parts = outer_edge_partition(Faces, We0),
    do_dissolve(Faces, Parts, Mat, WeOrig, We0).

do_dissolve(Faces, Ess, Mat, WeOrig, We0) ->
    We1 = do_dissolve_faces(Faces, We0),
    Inner = wings_face:inner_edges(Faces, WeOrig),
    {DelVs0,We2} = delete_inner(Inner, We1),
    {KeepVs,We} = do_dissolve_1(Ess, Mat, WeOrig, gb_sets:empty(), We2),
    #we{es=Etab,vs=Vtab0,he=Htab0} = We,
    DelVs = gb_sets:difference(DelVs0, KeepVs),
    Vtab1 = gb_sets:fold(fun(V, A) -> gb_trees:delete(V, A) end, Vtab0, DelVs),
    Vtab = update_vtab(KeepVs, Etab, WeOrig, Vtab1),
    Htab = gb_sets:difference(Htab0, gb_sets:from_list(Inner)),
    We#we{vs=Vtab,he=Htab}.

do_dissolve_1([EdgeList|Ess], Mat, WeOrig,
	      KeepVs0, #we{es=Etab0,fs=Ftab0}=We0) ->
    {Face,We} = wings_we:new_id(We0),
    FaceRec = #face{edge=hd(EdgeList),mat=Mat},
    Ftab = gb_trees:insert(Face, FaceRec, Ftab0),
    Last = last(EdgeList),
    {KeepVs,Etab} = update_outer([Last|EdgeList], EdgeList, Face, WeOrig,
				 Ftab, KeepVs0, Etab0),
    do_dissolve_1(Ess, Mat, WeOrig, KeepVs, We#we{es=Etab,fs=Ftab});
do_dissolve_1([], Mat, WeOrig, KeepVs, We) -> {KeepVs,We}.

do_dissolve_faces(Faces, #we{fs=Ftab0}=We) ->
    Ftab = gb_sets:fold(fun(Face, Ft) ->
				gb_trees:delete(Face, Ft)
			end, Ftab0, Faces),
    We#we{fs=Ftab}.

delete_inner(Inner, #we{es=Etab0}=We) ->
    {Vs,Etab} = foldl(
		  fun(Edge, {Vs0,Et0}) ->
			  #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Et0),
			  Vs = gb_sets:add(Va, gb_sets:add(Vb, Vs0)),
			  Et = gb_trees:delete(Edge, Et0),
			  {Vs,Et}
		  end, {gb_sets:empty(),Etab0}, Inner),
    {Vs,We#we{es=Etab}}.

update_outer([Pred|[Edge|Succ]=T], More, Face, WeOrig, Ftab, KeepVs0, Etab0) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = R0 = gb_trees:get(Edge, Etab0),
    Rec = case gb_trees:is_defined(Rf, Ftab) of
	      true ->
		  ?ASSERT(false == gb_trees:is_defined(Lf, Ftab)),
		  LS = succ(Succ, More),
		  R0#edge{lf=Face,ltpr=Pred,ltsu=LS};
	      false ->
		  ?ASSERT(true == gb_trees:is_defined(Lf, Ftab)),
		  RS = succ(Succ, More),
		  R0#edge{rf=Face,rtpr=Pred,rtsu=RS}
	  end,
    KeepVs = gb_sets:add(Va, gb_sets:add(Vb, KeepVs0)),
    Etab = gb_trees:update(Edge, Rec, Etab0),
    update_outer(T, More, Face, WeOrig, Ftab, KeepVs, Etab);
update_outer([_], More, Face, WeOrig, Ftab, KeepVs, Etab) ->
    {KeepVs,Etab}.

succ([Succ|_], More) -> Succ;
succ([], [Succ|_]) -> Succ.

update_vtab(Vs, Etab, We, Vtab) ->
    gb_sets:fold(
      fun(V, Vt0) ->
	      #vtx{edge=AnEdge} = Vtx0 = gb_trees:get(V, Vt0),
	      case gb_trees:is_defined(AnEdge, Etab) of
		  true -> Vt0;
		  false ->
		      Edge = find_edge(V, Etab, We),
		      Vtx = Vtx0#vtx{edge=Edge},
		      gb_trees:update(V, Vtx, Vt0)
	      end
      end, Vtab, Vs).

find_edge(V, Etab, We) ->
    wings_vertex:until(
      fun(Edge, _, _, _) ->
	      case gb_trees:is_defined(Edge, Etab) of
		  true -> Edge;
		  false -> not_found
	      end
      end, not_found, V, We).

%%%
%%% The Intrude command.
%%% 

intrude(St0) ->
    St1 = dissolve(St0),
    {St,Sel} = wings_sel:mapfold(fun intrude/3, [], St1),
    wings_move:setup(intrude, St#st{sel=reverse(Sel)}).

intrude(Faces0, #we{id=Id,es=Etab,fs=Ftab,next_id=Wid}=We0, SelAcc) ->
    Faces = gb_sets:to_list(Faces0),
    RootSet0 = foldl(
		 fun(F, A) ->
			 #face{edge=Edge} = gb_trees:get(F, Ftab),
			 #edge{vs=V} = gb_trees:get(Edge, Etab),
			 [{face,F},{vertex,V}|A]
		 end, [], Faces),
    {We1,RootSet} = wings_we:renumber(We0, Wid, RootSet0),
    We2 = wings_we:invert_normals(We1),
    We3 = wings_we:merge(We0, We2),
    Sel0 = wings_we:new_items(face, We0, We3),
    BridgeFaces = [F || {face,F} <- RootSet0 ++ RootSet],
    Sel = gb_sets:difference(Sel0, gb_sets:from_list(BridgeFaces)),
    We = intrude_bridge(RootSet0, RootSet, We3),
    {We,[{Id,Sel}|SelAcc]}.

intrude_bridge([{face,FaceA},{vertex,Va}|FsA],
	       [{face,FaceB},{vertex,Vb}|FsB], We0) ->
    We = force_bridge(FaceA, Va, FaceB, Vb, We0),
    intrude_bridge(FsA, FsB, We);
intrude_bridge([], [], We) -> We.

%%%
%%% The Mirror command.
%%%

mirror(St0) ->
    St = wings_sel:map(fun mirror_faces/2, St0),
    St#st{sel=[]}.

mirror_faces(Faces, We0) ->
    gb_sets:fold(fun(Face, WeAcc) ->
			 mirror_face(Face, We0, WeAcc)
		 end, We0, Faces).

mirror_face(Face, #we{fs=Ftab}=OrigWe, #we{next_id=Id}=We0) ->
    #face{edge=AnEdge} = gb_trees:get(Face, Ftab),
    RootSet0 = [{face,Face},{edge,AnEdge}],
    {WeNew0,RootSet} = wings_we:renumber(OrigWe, Id, RootSet0),
    [{face,FaceNew},{edge,ANewEdge}] = RootSet,
    WeNew = mirror_vs(FaceNew, WeNew0),
    We = wings_we:merge(We0, WeNew),

    %% Now weld the old face with new (mirrored) face.
    IterA0 = wings_face:iterator(Face, We),
    IterA = wings_face:skip_to_edge(AnEdge, IterA0),
    IterB0 = wings_face:iterator(FaceNew, We),
    IterB = wings_face:skip_to_edge(ANewEdge, IterB0),
    N = wings_face:vertices(Face, We),
    mirror_weld(N, IterA, Face, IterB, FaceNew, We, We).

mirror_vs(Face, #we{vs=Vtab0}=We0) ->
    Mirrorout = 2.0,
    Normal = wings_face:normal(Face, We0),
    Vs = wings_face:surrounding_vertices(Face, We0),
    Center = wings_vertex:center(Vs, We0),
    Vtab = foldl(fun(V, A) ->
 			 flatten_move(V, Normal, Center, Mirrorout, A)
 		 end, Vtab0, gb_trees:keys(Vtab0)),
    We = We0#we{vs=Vtab},
    wings_we:invert_normals(We).

mirror_weld(0, IterA0, FaceA, IterB0, FaceB, WeOrig, #we{fs=Ftab0}=We) ->
    Ftab1 = gb_trees:delete(FaceA, Ftab0),
    Ftab = gb_trees:delete(FaceB, Ftab1),
    We#we{fs=Ftab};
mirror_weld(N, IterA0, FaceA, IterB0, FaceB, WeOrig, We0) ->
    %% We will remove FaceA and FaceB, as well as all edges and vertices
    %% surrounding FaceB.
    {_,EdgeA,RecA0,IterA} = wings_face:next_cw(IterA0),
    {_,EdgeB,RecB0,IterB} = wings_face:next_ccw(IterB0),
    RecB = turn_edge(RecB0),
    {RecA,Pred,Succ} =
	case RecA0 of
	    #edge{lf=FaceA} ->
		update_edge(RecA0, RecB,
			    #edge.lf, #edge.ltpr, #edge.ltsu,
			    #edge.rtpr, #edge.rtsu);
	    #edge{rf=FaceA} ->
		update_edge(RecA0, RecB,
			    #edge.rf, #edge.rtpr, #edge.rtsu,
			    #edge.ltpr, #edge.ltsu)
	    end,
    #we{es=Etab0,vs=Vtab0,fs=Ftab0,he=Htab0} = We0,

    %% Update vertex table.
    #edge{vs=VstartB,ve=VendB} = RecB,
    Vtab1 = foldl(fun(V, Vt0) ->
			  case gb_trees:is_defined(V, Vt0) of
			      true -> gb_trees:delete(V, Vt0);
			      false -> Vt0
			  end
		  end, Vtab0, [VstartB,VendB]),
    #edge{vs=VstartA,ve=VendA} = RecA,
    Vtab2 = wings_vertex:patch_vertex(VstartA, EdgeA, Vtab1),
    Vtab = wings_vertex:patch_vertex(VendA, EdgeA, Vtab2),

    %% Update edge table.
    DelEdges = case RecB of
		   #edge{lf=FaceB,ltpr=D0,ltsu=D1} -> [D0,D1];
		   #edge{rf=FaceB,rtpr=D0,rtsu=D1} -> [D0,D1]
	      end,
    {Etab1,Htab} = delete_edges(DelEdges, Etab0, Htab0),
    Etab2 = gb_trees:update(EdgeA, RecA, Etab1),
    Etab3 = cond_patch_edge(Pred, EdgeA, EdgeB, Etab2),
    Etab4 = cond_patch_edge(Succ, EdgeA, EdgeB, Etab3),

    %% Patch references to the vertices that we have removed.
    Etab5 = replace_vertex(VstartB, VstartA, WeOrig, Etab4),
    Etab = replace_vertex(VendB, VendA, WeOrig, Etab5),

    %% Update face table
    Ftab1 = wings_face:patch_face(wings_face:other(FaceA, RecA0),
				  EdgeA, Ftab0),
    Ftab = wings_face:patch_face(wings_face:other(FaceB, RecB), EdgeA, Ftab1),

    %% Next edge.
    We = We0#we{es=Etab,fs=Ftab,vs=Vtab,he=Htab},
    mirror_weld(N-1, IterA, FaceA, IterB, FaceB, WeOrig, We).

update_edge(New0, Old, FaceP, PrP, SuP, OPrP, OSuP) ->
    New1 = case {element(PrP, Old),element(OSuP, Old)} of
	       {Pred,Pred} -> New0;
	       {Pred,OPred} -> setelement(PrP, New0, Pred)
	   end,
    New2 = case {element(SuP, Old),element(OPrP, Old)} of
	       {Succ,Succ} -> New1;
	       {Succ,OSucc} -> setelement(SuP, New1, Succ)
	   end,
    New = setelement(FaceP, New2, element(FaceP, Old)),
    {New,Pred,Succ}.

cond_patch_edge(Edge, New, Orig, Etab) ->
    case gb_trees:is_defined(Edge, Etab) of
	true -> wings_edge:patch_edge(Edge, New, Orig, Etab);
	false -> Etab
    end.

delete_edges(Edges, Etab0, Htab0) ->
    foldl(fun(Edge, {Et0,Ht0}=Acc) ->
		  case gb_trees:is_defined(Edge, Et0) of
		      true ->
			  Et = gb_trees:delete(Edge, Et0),
			  Ht = wings_edge:hardness(Edge, soft, Ht0),
			  {Et,Ht};
		      false -> Acc
		  end
	  end, {Etab0,Htab0}, Edges).

turn_edge(Rec) ->
    #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
    #edge{vs=Ve,ve=Vs,lf=Rf,rf=Lf,ltpr=RP,ltsu=RS,rtpr=LP,rtsu=LS}.

replace_vertex(Old, New, We, Etab0) ->
    wings_vertex:fold(
      fun(Edge, _, _, Et0) ->
	      case gb_trees:lookup(Edge, Et0) of
		  {value,#edge{vs=Old}=Rec} ->
		      gb_trees:update(Edge, Rec#edge{vs=New}, Et0);
		  {value,#edge{ve=Old}=Rec} ->
		      gb_trees:update(Edge, Rec#edge{ve=New}, Et0);
		  Other -> Et0			%Deleted or already modified.
	      end
      end, Etab0, Old, We).

%%%
%%% The Flatten command.
%%%

flatten(Type, St) ->
    Plane = flatten_vector(Type),
    wings_sel:map(
      fun(Faces, We) ->
	      Rs = wings_sel:face_regions(Faces, We),
	      foldl(fun(Fs, W) -> flatten(Fs, Plane, W) end, We, Rs)
      end, St).

flatten(Faces, normal, We) ->
    N = gb_sets:fold(fun(Face, Normal) ->
			     e3d_vec:add(Normal, wings_face:normal(Face, We))
		     end, e3d_vec:zero(), Faces),
    flatten(Faces, e3d_vec:norm(N), We);
flatten(Faces, PlaneNormal, #we{vs=Vtab0}=We) ->
    Vs = foldl(fun(Face, Vs0) ->
		       wings_face:fold(fun(V, _, _, A) ->
					       gb_sets:add(V, A)
				       end, Vs0, Face, We)
	       end, gb_sets:empty(), gb_sets:to_list(Faces)),
    Center = wings_vertex:center(Vs, We),
    Vtab = foldl(
	     fun(V, Tab0) ->
		     flatten_move(V, PlaneNormal, Center, 1.0, Tab0)
	     end, Vtab0, gb_sets:to_list(Vs)),
    We#we{vs=Vtab}.

flatten_move(V, PlaneNormal, Center, Dist, Tab0) ->
    #vtx{pos=Pos0} = Vtx = gb_trees:get(V, Tab0),
    ToCenter = e3d_vec:sub(Center, Pos0),
    Dot = e3d_vec:dot(ToCenter, PlaneNormal),
    ToPlane = e3d_vec:mul(PlaneNormal, Dot),
    Pos = wings_util:share(e3d_vec:add(Pos0, e3d_vec:mul(ToPlane, Dist))),
    gb_trees:update(V, Vtx#vtx{pos=Pos}, Tab0).

flatten_vector({_,{_,_,_}=Plane}) -> Plane;
flatten_vector(Plane) -> wings_util:make_vector(Plane).

%%%
%%% The Flatten Move command.
%%%

flatten_move(Type, St) ->
    {Center,Plane} = flatten_move_vector(Type),
    wings_sel:map(
      fun(Faces, We) ->
	      Vs = wings_face:to_vertices(Faces, We),
	      wings_vertex:flatten(Vs, Plane, Center, We)
      end, St).

flatten_move_vector({{_,_,_},{_,_,_}}=CenterPlane) ->
    CenterPlane;
flatten_move_vector(Plane) ->
    {{0.0,0.0,0.0},wings_util:make_vector(Plane)}.

%%%
%%% The Smooth command.
%%%

smooth(St0) ->
    {St,Sel} = wings_sel:mapfold(fun smooth/3, [], St0),
    St#st{sel=Sel}.

smooth(Faces0, #we{id=Id,name=Name}=We0, Acc) ->
    wings_io:progress("Smoothing \"" ++ Name ++ "\""),
    Rs = wings_sel:face_regions(Faces0, We0),
    We1 = smooth_regions(Rs, We0),
    NewFaces = wings_we:new_items(face, We0, We1),
    NewVs = wings_we:new_items(vertex, We0, We1),
    We = smooth_connect(NewVs, NewFaces, We1),
    {We,[{Id,NewFaces}|Acc]}.

smooth_regions([Faces0|Rs], #we{es=Etab,he=Htab}=We0) ->
    HardEdges0 = wings_face:outer_edges(Faces0, We0),
    HardEdges = gb_sets:union(gb_sets:from_list(HardEdges0), Htab),
    Faces = gb_sets:to_list(Faces0),
    {Vs,Es} = all_edges(Faces0, We0),
    We = wings_subdiv:smooth(Faces, Faces, Vs, Es, HardEdges, We0),
    smooth_regions(Rs, We);
smooth_regions([], We) -> We.

all_edges(Faces, We) ->
    {Vs,Es} = wings_face:fold_faces(
		fun(_, _, Edge, #edge{vs=Va,ve=Vb}, {Vs,Es}) ->
			{[Va,Vb|Vs],[Edge|Es]} end,
		{[],[]}, Faces, We),
    {ordsets:from_list(Vs),ordsets:from_list(Es)}.

smooth_connect(Vs, Faces0, We0) ->
    Faces = sofs:from_external(gb_sets:to_list(Faces0), [face]),
    FaceVs0 = wings_vertex:per_face(Vs, We0),
    FaceVs1 = sofs:from_external(FaceVs0, [{face,[vertex]}]),
    FaceVs2 = sofs:drestriction(FaceVs1, Faces),
    FaceVs = sofs:to_external(FaceVs2),
    smooth_connect_1(FaceVs, We0).

smooth_connect_1([{Face,[V]}|Fvs], We0) ->
    Iter0 = wings_face:iterator(Face, We0),
    IterCw = wings_face:skip_to_cw(V, Iter0),
    IterCcw = wings_face:skip_to_ccw(V, Iter0),
    We = smooth_connect_2(IterCw, IterCcw, V, Face, We0),
    smooth_connect_1(Fvs, We);
smooth_connect_1([{Face,Vs}|Fvs], We0) ->
    We = wings_vertex:connect(Face, Vs, We0),
    smooth_connect_1(Fvs, We);
smooth_connect_1([], We) -> We.

smooth_connect_2(IterCw0, IterCcw0, V, Face, We0) ->
    case {wings_face:next_cw(IterCw0),wings_face:next_ccw(IterCcw0)} of
	{{_,Edge,_,_},{_,Edge,_,_}} ->
	    {We1,NewV} = wings_edge:cut(Edge, 2, We0),
	    {We,_} = wings_vertex:force_connect(V, NewV, Face, We1),
	    We;
	{{Va,_,_,IterCw},{Vb,_,Rec,IterCcw}} ->
	    case wings_vertex:other(Vb, Rec) of
		Va when Va =/= V ->
		    {We,_} = wings_vertex:force_connect(V, Va, Face, We0),
		    We;
		Other ->
		    smooth_connect_2(IterCw, IterCcw, V, Face, We0)
	    end
    end.
    
%%%
%%% The Bridge command.
%%%

bridge(#st{shapes=Shapes0,sel=[{IdA,FacesA},{IdB,FacesB}]}=St0) ->
    case {gb_sets:to_list(FacesA),gb_sets:to_list(FacesB)} of
	{[FA],[FB0]} ->
	    #we{next_id=Id}=WeA = gb_trees:get(IdA, Shapes0),
	    #we{}=WeB0 = gb_trees:get(IdB, Shapes0),
	    {WeB,[{face,FB}]} = wings_we:renumber(WeB0, Id, [{face,FB0}]),
	    We = wings_we:merge(WeA, WeB),
	    Shapes1 = gb_trees:delete(IdB, Shapes0),
	    Shapes = gb_trees:update(IdA, We, Shapes1),
	    Sel = [{IdA,gb_sets:from_list([FA,FB])}],
	    St = St0#st{shapes=Shapes,sel=Sel},
	    bridge(St);
	Other ->
	    bridge_error()
    end;
bridge(#st{shapes=Shapes0,sel=[{Id,Faces}]}=St) ->
    case gb_sets:to_list(Faces) of
	[FA,FB] ->
	    We0 = gb_trees:get(Id, Shapes0),
	    We = bridge(FA, FB, We0),
	    Shapes = gb_trees:update(Id, We, Shapes0),
	    St#st{shapes=Shapes,sel=[]};
	Other ->
	    bridge_error()
    end;
bridge(St) ->
    bridge_error().

bridge(FaceA, FaceB, #we{vs=Vtab}=We) ->
    VsA = wings_face:surrounding_vertices(FaceA, We),
    VsB = wings_face:surrounding_vertices(FaceB, We),
    if
	length(VsA) =/= length(VsB) ->
	    bridge_error("Faces must have the same number of vertices.");
	true ->
	    An = wings_face:face_normal(VsA, Vtab),
	    Bn = wings_face:face_normal(VsB, Vtab),
	    case e3d_vec:dot(An, Bn) of
		Dot when Dot > 0.99 ->
		    bridge_error("Faces must not point in the same direction.");
		Dot ->
		    case are_neighbors(FaceA, FaceB, We) of
			true ->
			    bridge_error("Faces must not be neighbors.");
			false ->
			    bridge(FaceA, VsA, FaceB, VsB, We)
		    end
	    end
    end.

bridge(FaceA, VsA0, FaceB, VsB0, #we{vs=Vtab}=We0) ->
    Len = wings_face:vertices(FaceA, We0),
    [Va|_] = VsA0,
    [Vb|_] = VsB0,
    {Ids,We} = wings_we:new_wrap_range(Len, 2, We0),
    IterA = wings_face:skip_to_cw(Va, wings_face:iterator(FaceA, We)),
    IterB = wings_face:skip_to_ccw(Vb, wings_face:iterator(FaceB, We)),
    try_bridge(Len, Len, Va, FaceA, IterA,
	       Vb, FaceB, IterB, Ids, We, {1.0E250,We}).

try_bridge(0, Len, Va, FaceA, IterA, Vb, FaceB, IterB, Ids, _, {EdgeSum,We}) ->
    We;
try_bridge(N, Len, Va0, FaceA, IterA0, Vb, FaceB, IterB, Ids, We0,
	   {EdgeSum0,_}=Best0) ->
    We = do_bridge(Len, Va0, FaceA, IterA0, Vb, FaceB, IterB, Ids, We0),
    Best = case sum_edge_lens(Len, Ids, We, 0) of
		  Min when Min < EdgeSum0 -> {Min,We};
	       _ -> Best0
	   end,
    {_,_,_,IterA} = wings_face:next_cw(IterA0),
    {Va,_,_,_} = wings_face:next_cw(IterA),
    try_bridge(N-1, Len, Va, FaceA, IterA,
	       Vb, FaceB, IterB, Ids, We0, Best).

sum_edge_lens(0, Ids, We, Sum) -> Sum;
sum_edge_lens(N, Ids0, #we{es=Etab,vs=Vtab}=We, Sum) ->
    Edge = wings_we:id(0, Ids0),
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    VaPos = wings_vertex:pos(Va, Vtab),
    VbPos = wings_vertex:pos(Vb, Vtab),
    Ids = wings_we:bump_id(Ids0),
    Dist = e3d_vec:dist(VaPos, VbPos),
    sum_edge_lens(N-1, Ids, We, Sum + Dist).

force_bridge(FaceA, Va, FaceB, Vb, We0) ->
    Len = wings_face:vertices(FaceA, We0),
    {Ids,We} = wings_we:new_wrap_range(Len, 2, We0),
    IterA = wings_face:skip_to_cw(Va, wings_face:iterator(FaceA, We)),
    IterB = wings_face:skip_to_ccw(Vb, wings_face:iterator(FaceB, We)),
    do_bridge(Len, Va, FaceA, IterA, Vb, FaceB, IterB, Ids, We).

do_bridge(0, Va, FaceA, IterA, Vb, FaceB, IterB, Ids0, #we{fs=Ftab0}=We) ->
    Ftab1 = gb_trees:delete(FaceA, Ftab0),
    Ftab = gb_trees:delete(FaceB, Ftab1),
    We#we{fs=Ftab};
do_bridge(N, Va0, FaceA, IterA0, Vb0, FaceB, IterB0, Ids0, We0) ->
    #we{es=Etab0,fs=Ftab0} = We0,
    LeftEdge = wings_we:id(0, Ids0),
    LeftFace = wings_we:id(1, Ids0),
    NewEdge = wings_we:id(2, Ids0),
    RightFace = wings_we:id(3, Ids0),
    RightEdge = wings_we:id(4, Ids0),
    
    {_,EdgeA,RecA0,IterA} = wings_face:next_cw(IterA0),
    RecA = case RecA0 of
	       #edge{lf=FaceA} ->
		   RecA0#edge{lf=RightFace,ltpr=NewEdge,ltsu=RightEdge};
	       #edge{rf=FaceA} ->
		   RecA0#edge{rf=RightFace,rtpr=NewEdge,rtsu=RightEdge}
	   end,
    Etab1 = gb_trees:update(EdgeA, RecA, Etab0),

    {_,EdgeB,RecB0,IterB} = wings_face:next_ccw(IterB0),
    RecB = case RecB0 of
	       #edge{lf=FaceB} ->
		   RecB0#edge{lf=RightFace,ltpr=RightEdge,ltsu=NewEdge};
	       #edge{rf=FaceB} ->
		   RecB0#edge{rf=RightFace,rtpr=RightEdge,rtsu=NewEdge}
	   end,
    Etab2 = gb_trees:update(EdgeB, RecB, Etab1),

    RightRec0 = get_edge(RightEdge, Etab0),
    RightRec = RightRec0#edge{lf=RightFace,ltpr=EdgeA,ltsu=EdgeB},
    Etab3 = gb_trees:enter(RightEdge, RightRec, Etab2),
    
    NewRec0 = get_edge(NewEdge, Etab0),
    NewRec = NewRec0#edge{ve=Va0,vs=Vb0,rf=RightFace,rtpr=EdgeB,rtsu=EdgeA},
    Etab = gb_trees:enter(NewEdge, NewRec, Etab3),

    FaceRec = #face{edge=NewEdge},
    Ftab = gb_trees:insert(RightFace, FaceRec, Ftab0),
    
    We = We0#we{es=Etab,fs=Ftab},
    Ids = wings_we:bump_id(Ids0),
    Va = wings_vertex:other(Va0, RecA0),
    Vb = wings_vertex:other(Vb0, RecB0),
    do_bridge(N-1, Va, FaceA, IterA, Vb, FaceB, IterB, Ids, We).

get_edge(Edge, Etab) ->
    case gb_trees:lookup(Edge, Etab) of
	{value,Erec} -> Erec;
	none -> #edge{}
    end.

bridge_error() ->
    bridge_error("Exactly two faces must be selected.").

bridge_error(Error) ->
    throw({command_error,Error}).

%% Test whether two faces are neighbors or not. (In the sense that
%% they share at least one vertex.)
are_neighbors(FaceA, FaceB, We) ->
    VsA = wings_face:surrounding_vertices(FaceA, We),
    VsB = wings_face:surrounding_vertices(FaceB, We),
    ordsets:intersection(ordsets:from_list(VsA),
			 ordsets:from_list(VsB)) =/= [].

%%%
%%% The Lift command.
%%%

lift_selection(OrigSt) ->
    {fun(St) ->
	     wings_io:message("Select the edge to keep fixed."),
	     St#st{selmode=edge,sel=[]}
     end,
     fun(_) -> {none,""} end,
     fun lift_exit/3}.

lift_exit(X, Y, #st{selmode=Mode,sel=Sel}=St) ->
    Lift = fun(_, _) -> {face,{lift,{Mode,Sel}}} end,
    Menu = [{"Vector Selection",ignore},
	    separator,
	    {"Lift",Lift},
	    {"Abort Command",aborted}],
    {seq,pop,wings_menu:popup_menu(X, Y, secondary_selection, Menu, St)}.

lift({edge,EdgeSel}, St) ->
    lift_from_edge(EdgeSel, St);
lift(_, St) -> St.

lift_from_edge(EdgeSel, St0) ->
    Res = wings_sel:mapfold(
	    fun(Faces, #we{id=Id}=We0, {[{Id,Edges}|ES],Tv0}) ->
		    {We,Tv} = lift_from_edge(Faces, Edges, We0, Tv0),
		    {We,{ES,Tv}};
	       (_, _, _) -> lift_sel_mismatch()
	    end, {EdgeSel,[]}, St0),
    case Res of
	{St,{[],Tvs}} ->
	    wings_drag:init_drag(Tvs, none, angle, St);
	{_,_} -> lift_sel_mismatch()
    end.

lift_sel_mismatch() ->
    throw({command_error, "Face and edge selections don't match."}).
	
lift_from_edge(Faces, Edges, We0, Tv) ->
    EsFs0 = wings_face:fold_faces(
	      fun(Face, _, Edge, _, A) -> [{Edge,Face}|A] end,
	      [], Faces, We0),
    EsFs1 = sofs:relation(EsFs0, [{edge,face}]),
    EsFs = sofs:restriction(EsFs1, sofs:set(gb_sets:to_list(Edges), [edge])),
    FaceToEdge0 = sofs:converse(EsFs),
    case sofs:is_a_function(FaceToEdge0)  of
	false ->
	    lift_sel_mismatch();
	true ->
	    FaceToEdge = sofs:to_external(FaceToEdge0),
	    We = wings_extrude_face:faces(Faces, We0),
	    lift_from_edge_1(FaceToEdge, We0, We, Tv)
    end.

lift_from_edge_1([{Face,Edge}|T], #we{es=Etab}=OrigWe, We0, Tv0) ->
    Side = case gb_trees:get(Edge, Etab) of
	       #edge{lf=Face} -> left;
	       #edge{rf=Face} -> right
	   end,
    {We,Tv} = lift_from_edge_2(Face, Edge, Side, We0, Tv0),
    lift_from_edge_1(T, OrigWe, We, Tv);
lift_from_edge_1([], OrigWe, We, Tv) -> {We,Tv}.

lift_from_edge_2(Face, Edge, Side, #we{id=Id,es=Etab}=We0, Tv) ->
    FaceVs0 = ordsets:from_list(wings_face:surrounding_vertices(Face, We0)),
    #edge{vs=Va0,ve=Vb0} = gb_trees:get(Edge, Etab),
    {Va,Ea} = lift_edge_vs(Va0, FaceVs0, We0),
    {Vb,Eb} = lift_edge_vs(Vb0, FaceVs0, We0),
    We1 = wings_collapse:collapse_edge(Ea, We0),
    We = wings_collapse:collapse_edge(Eb, We1),
    FaceVs = ordsets:subtract(FaceVs0, ordsets:from_list([Va,Vb])),
    VaPos = wings_vertex:pos(Va, We0),
    VbPos = wings_vertex:pos(Vb, We0),
    Axis0 = case Side of
		left -> e3d_vec:sub(VbPos, VaPos);
		right -> e3d_vec:sub(VaPos, VbPos)
	    end,
    Axis = e3d_vec:norm(Axis0),
    Rot = wings_rotate:rotate(FaceVs, We, {VaPos,Axis}),
    {We,[{Id,Rot}|Tv]}.

lift_edge_vs(V, FaceVs, We) ->
    wings_vertex:fold(
      fun(Edge, _, Rec, none) ->
	      OtherV = wings_vertex:other(V, Rec),
	      case member(OtherV, FaceVs) of
		  true -> {OtherV,Edge};
		  false -> none
	      end;
	 (_, _, _, A) -> A
      end, none, V, We).

%% outer_edge_partition(FaceSet, WingedEdge) -> [[Edge]].
%%  Partition all outer edges. Outer edges are all edges
%%  between one face in the set and one outside.

outer_edge_partition(Faces, We) when is_list(Faces) ->
    collect_outer_edges(Faces, gb_sets:from_list(Faces), We, []);
outer_edge_partition(Faces, We) ->
    collect_outer_edges(gb_sets:to_list(Faces), Faces, We, []).

collect_outer_edges([Face|Fs], Faces, We, Acc0) ->
    Acc = wings_face:fold(
	    fun(_, E, Erec, A) ->
		    outer_edge(E, Erec, Face, Faces, A)
	    end, Acc0, Face, We),
    collect_outer_edges(Fs, Faces, We, Acc);
collect_outer_edges([], Faces, We, Acc) ->
    R = sofs:relation(Acc),
    F0 = sofs:relation_to_family(R),
    F = gb_trees:from_orddict(sofs:to_external(F0)),
    partition_edges(F, Faces, We, []).

outer_edge(Edge, Erec, Face, Faces, Acc) ->
    {V,OtherV,OtherFace} =
	case Erec of
	    #edge{vs=Vs,ve=Ve,lf=Face,rf=Other0,ltpr=Next0} ->
		{Vs,Ve,Other0};
	    #edge{vs=Vs,ve=Ve,rf=Face,lf=Other0,rtpr=Next0} ->
		{Ve,Vs,Other0}
	end,
    case gb_sets:is_member(OtherFace, Faces) of
	true -> Acc;
	false -> [{V,{Edge,V,OtherV}}|Acc]
    end.

partition_edges(Es0, Faces, We, Acc) ->
    case gb_sets:is_empty(Es0) of
	true -> Acc;
	false ->
	    {Key,Val,Es1} = gb_trees:take_smallest(Es0),
	    {Part,Es} = partition_edges(Key, Val, Faces, Es1, We, []),
	    partition_edges(Es, Faces, We, [Part|Acc])
    end.

partition_edges(Va, [{Edge,Va,Vb}], Faces, Es0, We, Acc0) ->
    %%io:format("line:~w, ~w\n", [?LINE,Va]),
    Acc = [Edge|Acc0],
    case gb_trees:lookup(Vb, Es0) of
	none ->
	    %%io:format("line:~w, ~w\n", [?LINE,Acc]),
	    {Acc,Es0};
	{value,Val} ->
	    %%io:format("line:~w, ~w\n", [?LINE,{Va,Val}]),
	    Es = gb_trees:delete(Vb, Es0),
	    partition_edges(Vb, Val, Faces, Es, We, Acc)
    end;
partition_edges(Va, [Val|More], Faces, Es0, We, []) ->
    Es = gb_trees:insert(Va, More, Es0),
    partition_edges(Va, [Val], Faces, Es, We, []);
partition_edges(Va, Edges, Faces, Es, We, Acc) ->
    %%io:format("line:~w, ~w\n", [?LINE,{Va,Edges}]),
    part_try_all_edges(Va, Edges, Faces, Es, We, Acc, [], none).
    
%% Here we have multiple choices of edges. Use the shortest path
%% that don't return us to the Va vertex.
%% (We want edge loops without repeated vertices.)
part_try_all_edges(Va, [Val|More], Faces, Es0, We, Acc, Alt0, Path0) ->
    Es1 = gb_trees:insert(Va, [{repeated,Va,fake}], Es0),
    case partition_edges(Va, [Val], Faces, Es1, We, Acc) of
	none ->
	    Alt = [Val|Alt0],
	    part_try_all_edges(Va, More, Faces, Es0, We, Acc, Alt, Path0);
	{[repeated|_],_} ->
	    Alt = [Val|Alt0],
	    part_try_all_edges(Va, More, Faces, Es0, We, Acc, Alt, Path0);
	{_,_}=Found ->
	    Path = part_shortest_path(Path0, Found),
	    part_try_all_edges(Va, More, Faces, Es0, We, Acc, Alt0, Path)
    end;
part_try_all_edges(Va, [], _, _, _, _, [], {Path,Es}) ->
    %%io:format("line:~w, ~w\n", [?LINE,Va]),
    {Path,gb_trees:delete(Va, Es)};
part_try_all_edges(Va, [], _, _, _, _, Alt, {Path,Es}) ->
    %%io:format("line:~w, ~w\n", [?LINE,Va]),
    {Path,gb_trees:enter(Va, Alt, Es)};
part_try_all_edges(Va, [], _, _, _, _, Alt, none) ->
    %%io:format("line:~w, ~w\n", [?LINE,Va]),
    none.

part_shortest_path(none, Path) -> Path;
part_shortest_path({AccA,_}=A, {AccB,_}) when length(AccA) < length(AccB) -> A;
part_shortest_path({_,_}, {_,_}=B) -> B.
