%%
%%  wings_face_cmd.erl --
%%
%%     This module contains most of the face commands.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_face_cmd.erl,v 1.11 2001/09/18 12:02:54 bjorng Exp $
%%

-module(wings_face_cmd).
-export([select_material/2,set_material/2,bevel_faces/1,
	 extrude/2,extrude_region/2,extract_region/2,
	 inset/1,dissolve/1,smooth/1,bridge/1,
	 intrude/1,mirror/1,flatten/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keysort/2,
		keymember/3,keysearch/3,keydelete/3,
		member/2,seq/2,last/1]).

%%%
%%% Select Material.
%%%

select_material(Mat, St) ->
    wings_sel:make(fun(Face, #we{fs=Ftab}) ->
			   #face{mat=M} = gb_trees:get(Face, Ftab),
			   M =:= Mat
		   end, face, St).

%%%
%%% The Material command.
%%%

set_material(Mat, St) ->
    wings_sel:map(
      fun(Face, #we{fs=Ftab0}=We) ->
	      Rec = gb_trees:get(Face, Ftab0),
	      Ftab = gb_trees:update(Face, Rec#face{mat=Mat}, Ftab0),
	      We#we{fs=Ftab}
      end, St).

%%%
%%% Extrude, Extrude Region, and Inset commands.
%%%

extrude(Type, St) ->
    wings_move:setup(Type, extrude_faces(St)).

inset(St) ->
    wings_scale:inset(extrude_faces(St)).

extrude_faces(St) ->
    wings_sel:map_shape(
      fun(Faces, We) ->
	      wings_extrude_face:faces(Faces, We)
      end, St).

%%% Extrude the selected regions.

extrude_region(Type, St0) ->
    {St1,Sel0} = wings_sel:mapfold_shape(fun extrude_region/4, [], St0),
    St = St1#st{sel=reverse(Sel0)},
    wings_move:setup(Type, St).

extrude_region(Id, Faces0, We0, Acc) ->
    %% We KNOW that a gb_set with fewer elements sorts before
    %% a gb_set with more elements.
    Rs = sort(wings_sel:find_face_regions(Faces0, We0)),
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
%%% The Bevel command.
%%%

bevel_faces(St0) ->
    {St,OrigVs} = wings_sel:mapfold_region(fun bevel_faces/4, [], St0),
    wings_scale:bevel_face(sort(OrigVs), St).

bevel_faces(ShId, Faces, We0, Acc) ->
    DisEdges = wings_face:inner_edges(Faces, We0),
    OrigVs = wings_face:to_vertices(Faces, We0),
    MoveEdges = bevel_move_edges(Faces, We0),
    We1 = wings_extrude_face:faces(Faces, We0),
    NewVs = wings_we:new_items(vertex, We0, We1),
    We2 = dissolve_edges(DisEdges, We1),
    We3 = bevel_connect(OrigVs, NewVs, We2),
    We = dissolve_more_edges(OrigVs, NewVs, We3),
    {We,[{ShId,MoveEdges}|Acc]}.

bevel_connect(Vs, NewVs, We) ->
    gb_sets:fold(fun(V, A) -> bevel_connect_1(V, NewVs, A) end, We, Vs).

bevel_connect_1(V, NewVs, We) ->
    Vs = wings_vertex:fold(
	  fun(Edge, _, Rec, A) ->
		  OtherV = wings_vertex:other(V, Rec),
		  case gb_sets:is_member(OtherV, NewVs) of
		      false -> A;
		      true ->
			  wings_vertex:fold(
			    fun(_, Face, _, A1) ->
				    [{Face,OtherV}|A1]
			    end, A, OtherV, We)
		  end
	  end, [], V, We),
    R = sofs:relation(Vs),
    Family = sofs:relation_to_family(R),
    foldl(fun ({Face,[_]}, A) -> A;
	      ({Face,[Vstart,Vend]}, A0) ->
		  {A,_} = wings_vertex:force_connect(Vstart, Vend, Face, A0),
		  A
	  end, We, sofs:to_external(Family)).

dissolve_edges(Edges, We0) ->
    foldl(fun(E, W) -> wings_edge:dissolve_edge(E, W) end, We0, Edges).

dissolve_more_edges(Vs, NewVs, We) ->
    gb_sets:fold(fun(V, A) ->
			 dissolve_more_edges_1(V, NewVs, A)
		 end, We, Vs).

dissolve_more_edges_1(V, NewVs, We) ->
    Dis = wings_vertex:fold(
	    fun (Edge, Face, #edge{lf=Lf,rf=Rf}=Rec, A) ->
		    OtherV = wings_vertex:other(V, Rec),
		    case gb_sets:is_member(OtherV, NewVs) of
			false -> A;
			true -> [{Lf,Edge},{Rf,Edge}|A]
		    end
	    end, [], V, We),
    R = sofs:relation(Dis),
    F0 = sofs:relation_to_family(R),
    F = sofs:family_specification(fun(L) ->
					   sofs:no_elements(L) =:= 2
				   end, F0),
    Fs = sofs:to_external(sofs:domain(F)),
    Inner = wings_face:inner_edges(Fs, We),
    dissolve_edges(Inner, We).

bevel_move_edges(Faces, We) ->
    R0 = wings_face:fold_faces(
	   fun(F, _, E, _, A) ->
		   [{F,E}|A]
	   end, [], Faces, We),
    R = sofs:relation(R0),
    P = sofs:partition(2, R),
    M = sofs:specification(fun(L) -> sofs:no_elements(L) =:= 1 end, P),
    U = sofs:union(M),
    sofs:to_external(sofs:relation_to_family(U)).

%%%
%%% The Extract Region command.
%%%

extract_region(Type, #st{onext=Id0,shapes=Shapes0}=St0) ->
    St1 = wings_sel:fold_shape(
	    fun(Sh, Faces, #st{sel=Sel0,onext=Oid}=St0) ->
		    St = wings_shape:insert(Sh, "extract", St0),
		    Sel = [{Oid,Faces}|Sel0],
		    St#st{sel=Sel}
	    end, St0#st{sel=[]}, St0),
    St2 = wings_sel:inverse(St1),
    St3 = dissolve(St2),
    St = wings_sel:inverse(St3),
    wings_move:setup(Type, St).
    
%%%
%%% The Dissolve command.
%%%

dissolve(St0) ->
    {St,Sel} = wings_sel:mapfold_shape(fun dissolve/4, [], St0),
    St#st{sel=reverse(Sel)}.

dissolve(Id, Faces0, We0, Acc) ->
    Rs = wings_sel:find_face_regions(Faces0, We0),
    {We,Sel0} = dissolve_1(Rs, We0, We0, []),
    Sel = gb_sets:union(wings_we:new_items(face, We0, We),
			gb_sets:from_list(Sel0)),
    {We,[{Id,Sel}|Acc]}.

dissolve_1([Faces|Rs], WeOrig, We0, Sel) ->
    case gb_trees:size(Faces) of
	1 ->
	    [Face] = gb_sets:to_list(Faces),
	    dissolve_1(Rs, WeOrig, We0, [Face|Sel]);
	Other ->
 	    Parts = outer_edge_partition(Faces, We0),
 	    We = do_dissolve(Faces, Parts, WeOrig, We0),
	    dissolve_1(Rs, WeOrig, We, Sel)
    end;
dissolve_1([], WeOrig, We, Sel) -> {We,Sel}.

do_dissolve(Faces, Ess, WeOrig, We0) ->
    We1 = do_dissolve_faces(Faces, We0),
    Inner = wings_face:inner_edges(Faces, WeOrig),
    {DelVs0,We2} = delete_inner(Inner, We1),
    {KeepVs,We} = do_dissolve_1(Ess, WeOrig, gb_sets:empty(), We2),
    #we{es=Etab,vs=Vtab0,he=Htab0} = We,
    DelVs = gb_sets:difference(DelVs0, KeepVs),
    Vtab1 = gb_sets:fold(fun(V, A) -> gb_trees:delete(V, A) end, Vtab0, DelVs),
    Vtab = update_vtab(KeepVs, Etab, WeOrig, Vtab1),
    Htab = gb_sets:difference(Htab0, gb_sets:from_list(Inner)),
    We#we{vs=Vtab,he=Htab}.

do_dissolve_1([EdgeList|Ess], WeOrig, KeepVs0, #we{es=Etab0,fs=Ftab0}=We0) ->
    {Face,We} = wings_we:new_id(We0),
    FaceRec = #face{edge=hd(EdgeList),mat=hole},
    Ftab = gb_trees:insert(Face, FaceRec, Ftab0),
    Last = last(EdgeList),
    {KeepVs,Etab} = update_outer([Last|EdgeList], EdgeList, Face, WeOrig,
				 Ftab, KeepVs0, Etab0),
    do_dissolve_1(Ess, WeOrig, KeepVs, We#we{es=Etab,fs=Ftab});
do_dissolve_1([], WeOrig, KeepVs, We) -> {KeepVs,We}.

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
    {St,Sel} = wings_sel:mapfold_shape(fun intrude/4, [], St1),
    wings_move:setup(intrude, St#st{sel=Sel}).

intrude(Id, Faces0, #we{es=Etab,fs=Ftab,next_id=Wid}=We0, SelAcc) ->
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
    mirror(St0, 2.0).

mirror(St0, Mirrorout) ->
    St = wings_sel:map_shape(
	   fun(Faces, We) ->
		   mirror_faces(Faces, We, Mirrorout)
	   end, St0),
    St#st{sel=[]}.

mirror_faces(Faces, We0, Mirrorout) ->
    gb_sets:fold(fun(Face, WeAcc) ->
			 mirror_face(Face, We0, WeAcc, Mirrorout)
		 end, We0, Faces).

mirror_face(Face, #we{fs=Ftab}=OrigWe, #we{next_id=Id}=We0, Mirrorout) ->
    #face{edge=AnEdge} = gb_trees:get(Face, Ftab),
    RootSet0 = [{face,Face},{edge,AnEdge}],
    {WeNew0,RootSet} = wings_we:renumber(OrigWe, Id, RootSet0),
    [{face,FaceNew},{edge,ANewEdge}] = RootSet,
    WeNew = mirror_vs(FaceNew, WeNew0, Mirrorout),
    We = wings_we:merge(We0, WeNew),

    %% Now weld the old face with new (mirrored) face.
    IterA0 = wings_face:iterator(Face, We),
    IterA = wings_face:skip_to_edge(AnEdge, IterA0),
    IterB0 = wings_face:iterator(FaceNew, We),
    IterB = wings_face:skip_to_edge(ANewEdge, IterB0),
    N = wings_face:vertices(Face, We),
    mirror_weld(N, IterA, Face, IterB, FaceNew, We, We).

mirror_vs(Face, #we{vs=Vtab0}=We0, Mirrorout) ->
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

flatten(Plane0, St) ->
    Plane = plane_normal(Plane0),
    wings_sel:map_region(
      fun(Faces, We) ->
	      flatten(Faces, Plane, We)
      end, St).

plane_normal(x) -> {1.0,0.0,0.0};
plane_normal(y) -> {0.0,1.0,0.0};
plane_normal(z) -> {0.0,0.0,1.0};
plane_normal(normal) -> normal.

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

%%%
%%% The Smooth command.
%%%

smooth(St0) ->
    {St,Sel} = wings_sel:mapfold_region(fun smooth/4, [], St0),
    St#st{sel=Sel}.

smooth(Id, Faces0, #we{es=Etab,he=Htab}=We0, Acc) ->
    HardEdges0 = wings_face:outer_edges(Faces0, We0),
    HardEdges = gb_sets:union(gb_sets:from_list(HardEdges0), Htab),
    Faces = gb_sets:to_list(Faces0),
    {Vs,Es} = all_edges(Faces0, We0),
    We = wings_subdiv:smooth(Faces, Faces, Vs, Es, HardEdges, We0),
    Sel = wings_we:new_items(face, We0, We),
    {We,[{Id,Sel}|Acc]}.

all_edges(Faces, We) ->
    {Vs,Es} = wings_face:fold_faces(
		fun(_, _, Edge, #edge{vs=Va,ve=Vb}, {Vs,Es}) ->
			{[Va,Vb|Vs],[Edge|Es]} end,
		{[],[]}, Faces, We),
    {ordsets:from_list(Vs),ordsets:from_list(Es)}.

%%%
%%% The Bridge command.
%%%

bridge(#st{shapes=Shapes0,sel=[{IdA,FacesA},{IdB,FacesB}]}=St0) ->
    case {gb_sets:to_list(FacesA),gb_sets:to_list(FacesB)} of
	{[FA],[FB0]} ->
	    #shape{sh=#we{next_id=Id}=WeA}=ShA0 = gb_trees:get(IdA, Shapes0),
	    #shape{sh=#we{}=WeB0} = gb_trees:get(IdB, Shapes0),
	    {WeB,[{face,FB}]} = wings_we:renumber(WeB0, Id, [{face,FB0}]),
	    We = wings_we:merge(WeA, WeB),
	    ShA = ShA0#shape{sh=We},
	    Shapes1 = gb_trees:delete(IdB, Shapes0),
	    Shapes = gb_trees:update(IdA, ShA, Shapes1),
	    Sel = [{IdA,gb_sets:from_list([FA,FB])}],
	    St = St0#st{shapes=Shapes,sel=Sel},
	    bridge(St);
	Other ->
	    bridge_error()
    end;
bridge(#st{shapes=Shapes0,sel=[{Id,Faces}]}=St) ->
    case gb_sets:to_list(Faces) of
	[FA,FB] ->
	    #shape{sh=#we{}=We0}=Sh = gb_trees:get(Id, Shapes0),
	    case bridge(FA, FB, We0) of
		{error,_}=Error ->
		    Error;
		We ->
		    Shapes = gb_trees:update(Id, Sh#shape{sh=We}, Shapes0),
		    St#st{shapes=Shapes,sel=[]}
	    end;
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
	    {error,"Faces must have the same number of vertices."};
	true ->
	    An = wings_face:face_normal(VsA, Vtab),
	    Bn = wings_face:face_normal(VsB, Vtab),
	    case e3d_vec:dot(An, Bn) of
		Dot when Dot > 0.99 ->
		    {error,"Faces must not point in the same direction."};
		Dot ->
		    case are_neighbors(FaceA, FaceB, We) of
			true ->
			    {error,"Faces must not be neighbors."};
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
    {error,"Exactly two faces must be selected."}.

%% Test if two faces are neighbors.
are_neighbors(FaceA, FaceB, We) ->
    wings_face:fold(
      fun (_, _, Rec, true) -> true;
	  (_, _, Rec, false) ->
	      case Rec of
		  #edge{lf=FaceB} -> true;
		  #edge{rf=FaceB} -> true;
		  Other -> false
	      end
      end, false, FaceA, We).

%% outer_edge_partition(FaceSet, WingedEdge) -> [[Edge]].
%%  Partition all outer edges. Outer edges are all edges
%%  between one face in the set and one outside.

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
    F = sofs:relation_to_family(R),
    partition_edges(gb_trees:from_orddict(sofs:to_external(F)), []).

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
	false -> [{V,{Edge,V,OtherV,Face}}|Acc]
    end.

partition_edges(Es0, Acc) ->
    case gb_sets:is_empty(Es0) of
	true -> Acc;
	false ->
	    {Key,Val,Es1} = gb_trees:take_smallest(Es0),
	    {Part,Es} = partition_edges(Key, unknown, Val, Es1, []),
	    partition_edges(Es, [Part|Acc])
    end.

partition_edges(Va, _, [{Edge,Va,Vb,Face}], Es0, Acc0) ->
    Acc = [Edge|Acc0],
    case gb_trees:lookup(Vb, Es0) of
	none -> {Acc,Es0};
	{value,Val} ->
	    Es = gb_trees:delete(Vb, Es0),
	    partition_edges(Vb, Face, Val, Es, Acc)
    end;
partition_edges(Va, unknown, [{_,Va,_,Face}|_]=Edges, Es, Acc) ->
    partition_edges(Va, Face, Edges, Es, Acc);
partition_edges(Va, Face, Edges0, Es0, Acc) ->
    [Val] = [E || {_,_,_,AFace}=E <- Edges0, AFace =:= Face],
    Edges = [E || {_,_,_,AFace}=E <- Edges0, AFace =/= Face],
    Es = gb_trees:insert(Va, Edges, Es0),
    partition_edges(Va, Face, [Val], Es, Acc).
