%%
%%  wings_face_cmd.erl --
%%
%%     This module contains most of the face commands.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_face_cmd.erl,v 1.111 2004/12/16 20:05:10 bjorng Exp $
%%

-module(wings_face_cmd).
-export([menu/3,command/2]).
-export([dissolve/1,dissolve/2,outer_edge_partition/2,mirror_faces/2,
	 set_color/2, force_bridge/5]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keysort/2,
		keymember/3,keysearch/3,keydelete/3,
		member/2,seq/2,last/1]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Menu = [{basic,{?__(1,"Face operations"),ignore}},
	    {basic,separator},
	    {?__(2,"Move"),{move,Dir},[],[magnet]},
	    wings_menu_util:rotate(St),
	    wings_menu_util:scale(St),
	    separator,
	    {?__(3,"Extrude"),{extrude,Dir}},
	    {?__(4,"Extrude Region"),{extrude_region,Dir}},
	    {?__(5,"Extract Region"),{extract_region,Dir}},
	    separator,
	    wings_menu_util:flatten(),
	    separator,
	    {?__(6,"Inset"),inset,
	     ?__(7,"Inset a face inside the selected face")},
	    {?__(8,"Intrude"),intrude,
	     ?__(9,"Carve out interior of object, making selected faces holes")},
	    {?__(10,"Bevel"),bevel,
	     ?__(11,"Round off edges of selected faces")},
	    {?__(12,"Bridge"),bridge,
	     ?__(13,"Create a bridge or tunnel between two faces")},
	    {advanced,separator},
	    {?__(14,"Bump"),bump,
	     ?__(15,"Create bump of selected faces")},
	    {advanced,{?__(16,"Lift"),{lift,lift_fun(St)}}},
	    {advanced,{?__(17,"Put On"),put_on_fun(),
		       {?__(18,"Move and rotate object, aligning the selected face to another element"),[],
		        ?__(19,"Clone object on to one or more elements")},[]}},
	    separator,
	    {?__(20,"Mirror"),mirror_fun(),
	     {?__(21,"Mirror object around selected faces and merge to object"),[],
	      ?__(22,"Mirror and create separate objects")},[]},
    	    {?__(23,"Dissolve"),dissolve,
	     ?__(24,"Eliminate all edges between selected faces")},
	    {?__(25,"Collapse"),collapse,
	     ?__(26,"Delete faces, replacing them with vertices")},
	    {?__(27,"Smooth"),smooth,
	     ?__(28,"Subdivide selected faces to smooth them (Catmull-Clark)")},
	    {?__(29,"Tesselate"),{subdivide,wings_tesselation:submenu()}},
	    separator] ++ wings_material:material_menu(St) ++
	[{?__(30,"Vertex Color"),vertex_color,
	  ?__(31,"Apply vertex colors to selected faces")}],
    wings_menu:popup_menu(X, Y, face, Menu).

lift_fun(St) ->
    fun(help, _Ns) ->
	    {?__(1,"Lift, rotating face around edge or vertex"),[],
	     ?__(2,"Lift in std. directions")};
       (1, Ns) ->
	    Funs = lift_selection(rotate, St),
	    wings_menu:build_command({'ASK',Funs}, Ns);
       (3, Ns) ->
	    wings_menu_util:directions([normal,free,x,y,z], Ns);
       (_, _) -> ignore
    end.

put_on_fun() ->
    fun(1, _Ns) ->
	    {face,put_on};
       (3, _Ns) ->
	    {face,clone_on};
       (_, _) -> ignore
    end.

mirror_fun() ->
    fun(1, _Ns) ->
	    {face,mirror};
       (3, _Ns) ->
	    {face,mirror_separate};
       (_, _) -> ignore
    end.

command({extrude,Type}, St) ->
    ?SLOW(extrude(Type, St));
command({extrude_region,Type}, St) ->
    ?SLOW(extrude_region(Type, St));
command({extract_region,Type}, St) ->
    extract_region(Type, St);
command(bump, St) ->
    ?SLOW(wings_extrude_edge:bump(St));
command({flatten,Plane}, St) ->
    flatten(Plane, St);
command(bevel, St) ->
    ?SLOW(wings_extrude_edge:bevel_faces(St));
command(inset, St) ->
    ?SLOW(inset(St));
command(mirror, St) ->
    ?SLOW({save_state,mirror(St)});
command(mirror_separate, St) ->
    ?SLOW({save_state,mirror_separate(St)});
command(intrude, St) ->
    ?SLOW(intrude(St));
command(dissolve, St) ->
    {save_state,dissolve(St)};
command(bridge, St) ->
    {save_state,bridge(St)};
command(smooth, St) ->
    ?SLOW({save_state,smooth(St)});
command(auto_smooth, St) ->
    wings_body:auto_smooth(St);
command({lift,Lift}, St) ->
    lift(Lift, St);
command(put_on, St) ->
    put_on(St);
command(clone_on, St) ->
    clone_on(St);
command(collapse, St) ->
    {save_state,wings_collapse:collapse(St)};
command({material,Cmd}, St) ->
    wings_material:command(Cmd, St);
command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St);
command({subdivide,Subdivide}, St) ->
    wings_tesselation:command(Subdivide, St);
command(vertex_color, St) ->
    wings_color:choose(fun(Color) ->
			       set_color(Color, St)
		       end).

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
    St = wings_sel:map(fun extrude_region_0/2, St0),
    wings_move:setup(Type, St).

extrude_region_0(Faces0, We0) ->
    %% We KNOW that a gb_set with fewer elements sorts before
    %% a gb_set with more elements.
    Rs = sort(wings_sel:face_regions(Faces0, We0)),
    We = extrude_region_1(Rs, We0, []),
    extrude_region_vmirror(We0, We).

extrude_region_1([Faces0|Rs0]=Rs, We0, Acc) ->
    case gb_sets:size(Faces0) of
	1 ->
	    [Face] = gb_sets:to_list(Faces0),
	    extrude_region_1(Rs0, We0, [Face|Acc]);
	_Other ->
	    We = wings_extrude_face:faces(Acc, We0),
	    extrude_region_2(Rs, We)
    end;
extrude_region_1([], We, Faces) ->
    wings_extrude_face:faces(Faces, We).

extrude_region_2([Faces|Rs], We0) ->
    We = wings_extrude_face:region(Faces, We0),
    extrude_region_2(Rs, We);
extrude_region_2([], We) -> We.

extrude_region_vmirror(_, #we{mirror=none}=We) -> We;
extrude_region_vmirror(OldWe, #we{mirror=Face0}=We0) ->
    %% Merge the mirror face and any newly created faces to one new mirror face
    %% and flatten it.
    FaceSet = gb_sets:singleton(Face0),
    Bordering = wings_face:extend_border(FaceSet, We0),
    NewFaces = wings_we:new_items(face, OldWe, We0),
    BorderingNew = gb_sets:intersection(Bordering, NewFaces),
    case gb_sets:is_empty(BorderingNew) of
	true -> We0;
	false ->
	    Dissolve = gb_sets:union(FaceSet, BorderingNew),
	    We1 = dissolve(Dissolve, We0),
	    [Face] = NewFace = gb_sets:to_list(wings_we:new_items(face, We0, We1)),
	    We = wings_material:assign('_hole_', NewFace, We1),
	    wings_we:mirror_flatten(OldWe, We#we{mirror=Face})
    end.

%%%
%%% The Extract Region command.
%%%

extract_region(Type, St0) ->
    St1 = wings_sel:fold(
	    fun(Faces, We0, #st{sel=Sel0,onext=Oid}=S0) ->
		    We = We0#we{mirror=none},
		    S = wings_shape:insert(We, extract, S0),
		    Sel = [{Oid,Faces}|Sel0],
		    S#st{sel=Sel}
	    end, St0#st{sel=[]}, St0),
    Sel = St1#st.sel,
    St2 = wings_sel:set(Sel, St1),
    St3 = extract_inverse(St2),
    St4 = dissolve(St3),
    St = wings_sel:set(Sel, St4),
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
    wings_sel:set(Sel, St).
    
%%%
%%% The Dissolve command.
%%%

dissolve(St0) ->
    {St,Sel} = wings_sel:mapfold(fun dissolve/3, [], St0),
    wings_sel:set(Sel, St).

dissolve(Faces, We0) ->
    {We,_} = dissolve(Faces, We0, []),
    We.

dissolve(Faces, #we{id=Id}=We0, Acc) ->
    We = dissolve_1(Faces, We0),
    case wings_we:is_consistent(We) of
	true ->
	    Sel = wings_we:new_items(face, We0, We),
	    {We,[{Id,Sel}|Acc]};
	false ->
	    wings_util:error(?__(1,"Dissolving would cause an inconsistent object structure."))
    end.
		  
dissolve_1(Faces, We) ->
    case gb_sets:is_empty(Faces) of
	true -> We;
	false -> wings_we:vertex_gc(dissolve_2(Faces, We#we{vc=undefined}))
    end.

dissolve_2(Faces, We) ->
    Parts = wings_sel:face_regions(Faces, We),
    dissolve_3(Parts, We).

dissolve_3([Faces|T], We0) ->
    Face = gb_sets:smallest(Faces),
    Mat = wings_material:get(Face, We0),
    We1 = wings_material:delete_faces(Faces, We0),
    Parts = outer_edge_partition(Faces, We1),
    We2 = do_dissolve(Faces, Parts, Mat, We0, We1),
    We = foldl(fun(_, bad_edge) -> bad_edge;
		  (F, W) -> wings_face:delete_if_bad(F, W)
	       end, We2, gb_sets:to_list(wings_we:new_items(face, We0, We2))),
    dissolve_3(T, We);
dissolve_3([], We) -> We.

do_dissolve(Faces, Ess, Mat, WeOrig, We0) ->
    We1 = do_dissolve_faces(Faces, We0),
    Inner = wings_face:inner_edges(Faces, WeOrig),
    We2 = delete_inner(Inner, We1),
    #we{he=Htab0} = We = do_dissolve_1(Ess, Mat, WeOrig, We2),
    Htab = gb_sets:difference(Htab0, gb_sets:from_list(Inner)),
    We#we{he=Htab}.

do_dissolve_1([EdgeList|Ess], Mat, WeOrig, #we{es=Etab0,fs=Ftab0}=We0) ->
    {Face,We1} = wings_we:new_id(We0),
    Ftab = gb_trees:insert(Face, hd(EdgeList), Ftab0),
    Last = last(EdgeList),
    Etab = update_outer([Last|EdgeList], EdgeList, Face, WeOrig,
			Ftab, Etab0),
    We2 = We1#we{es=Etab,fs=Ftab},
    We = wings_material:assign(Mat, [Face], We2),
    do_dissolve_1(Ess, Mat, WeOrig, We);
do_dissolve_1([], _Mat, _WeOrig, We) -> We.

do_dissolve_faces(Faces, #we{fs=Ftab0}=We) ->
    Ftab = foldl(fun(Face, Ft) ->
			 gb_trees:delete(Face, Ft)
		 end, Ftab0, gb_sets:to_list(Faces)),
    We#we{fs=Ftab}.

delete_inner(Inner, #we{es=Etab0}=We) ->
    Etab = foldl(fun(Edge, Et) ->
			 gb_trees:delete(Edge, Et)
		 end, Etab0, Inner),
    We#we{es=Etab}.

update_outer([Pred|[Edge|Succ]=T], More, Face, WeOrig, Ftab, Etab0) ->
    #edge{rf=Rf} = R0 = gb_trees:get(Edge, Etab0),
    Rec = case gb_trees:is_defined(Rf, Ftab) of
	      true ->
		  ?ASSERT(false == gb_trees:is_defined(R0#edge.lf, Ftab)),
		  LS = succ(Succ, More),
		  R0#edge{lf=Face,ltpr=Pred,ltsu=LS};
	      false ->
		  ?ASSERT(true == gb_trees:is_defined(R0#edge.lf, Ftab)),
		  RS = succ(Succ, More),
		  R0#edge{rf=Face,rtpr=Pred,rtsu=RS}
	  end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    update_outer(T, More, Face, WeOrig, Ftab, Etab);
update_outer([_], _More, _Face, _WeOrig, _Ftab, Etab) -> Etab.

succ([Succ|_], _More) -> Succ;
succ([], [Succ|_]) -> Succ.

%%%
%%% The Intrude command.
%%% 

intrude(St0) ->
    St1 = dissolve(St0),
    {St,Sel} = wings_sel:mapfold(fun intrude/3, [], St1),
    wings_move:setup(intrude, wings_sel:set(Sel, St)).

intrude(Faces0, #we{id=Id,es=Etab,fs=Ftab,next_id=Wid}=We0, SelAcc) ->
    Faces = gb_sets:to_list(Faces0),
    RootSet0 = foldl(
		 fun(F, A) ->
			 Edge = gb_trees:get(F, Ftab),
			 #edge{vs=V} = gb_trees:get(Edge, Etab),
			 [{face,F},{vertex,V}|A]
		 end, [], Faces),
    {We1,RootSet} = wings_we:renumber(We0, Wid, RootSet0),
    We2 = wings_we:invert_normals(We1),
    We3 = wings_we:merge(We0, We2),
    Sel0 = wings_we:new_items(face, We0, We3),
    BridgeFaces = [F || {face,F} <- RootSet0 ++ RootSet],
    Sel = gb_sets:difference(Sel0, gb_sets:from_list(BridgeFaces)),
    We4 = intrude_bridge(RootSet0, RootSet, We3),
    We = restore_mirror(We4, We0),
    {We#we{mode=We0#we.mode},[{Id,Sel}|SelAcc]}.

restore_mirror(We, #we{mirror=none}) -> We;
restore_mirror(We, #we{mirror=Face}) -> We#we{mirror=Face}.

intrude_bridge([{face,FaceA},{vertex,Va}|FsA],
	       [{face,FaceB},{vertex,Vb}|FsB], We0) ->
    We = force_bridge(FaceA, Va, FaceB, Vb, We0),
    intrude_bridge(FsA, FsB, We);
intrude_bridge([], [], We) -> We.

%%%
%%% The Mirror command.
%%%

mirror_separate(St0) ->
    St = wings_sel:fold(fun mirror_sep_faces/3, St0, St0),
    wings_sel:clear(St).
    
mirror_sep_faces(Faces, We0, Acc) when is_list(Faces) ->
    Template = wings_we:invert_normals(We0),
    foldl(fun(Face, A) ->
		  We = mirror_vs(Face, Template),
		  wings_shape:insert(We, mirror, A)
	  end, Acc, Faces);
mirror_sep_faces(Faces, We, Acc) ->
    mirror_sep_faces(gb_sets:to_list(Faces), We, Acc).

mirror(St0) ->
    St = wings_sel:map(fun mirror_faces/2, St0),
    wings_sel:clear(St).

mirror_faces(Faces, #we{mode=Mode}=We0) when is_list(Faces) ->
    OrigWe = wings_we:invert_normals(We0),
    We = foldl(fun(Face, WeAcc) ->
		       mirror_face(Face, OrigWe, WeAcc)
	       end, We0, Faces),
    We#we{mode=Mode};
mirror_faces(Faces, We) ->
    mirror_faces(gb_sets:to_list(Faces), We).

mirror_face(Face, #we{fs=Ftab}=OrigWe, #we{next_id=Id}=We0) ->
    AnEdge = gb_trees:get(Face, Ftab),
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

mirror_vs(Face, #we{vp=Vtab0}=We) ->
    Normal = wings_face:normal(Face, We),
    Center = wings_face:center(Face, We),
    Vtab1 = foldl(fun(Vtx, A) ->
			  mirror_move_vs(Vtx, Normal, Center, A)
		  end, [], gb_trees:to_list(Vtab0)),
    Vtab = gb_trees:from_orddict(reverse(Vtab1)),
    We#we{vp=Vtab}.

mirror_move_vs({V,Pos0}, PlaneNormal, Center, A) ->
    ToCenter = e3d_vec:sub(Center, Pos0),
    Dot = e3d_vec:dot(ToCenter, PlaneNormal),
    Pos = wings_util:share(e3d_vec:add_prod(Pos0, PlaneNormal, 2.0*Dot)),
    [{V,Pos}|A].

mirror_weld(0, _IterA0, FaceA, _IterB0, FaceB, _WeOrig, #we{fs=Ftab0}=We0) ->
    Ftab1 = gb_trees:delete(FaceA, Ftab0),
    Ftab = gb_trees:delete(FaceB, Ftab1),
    We = wings_material:delete_faces([FaceA,FaceB], We0#we{fs=Ftab}),
    wings_we:vertex_gc(We);
mirror_weld(N, IterA0, FaceA, IterB0, FaceB, WeOrig, We0) ->
    %% We will remove FaceA and FaceB, as well as all edges and vertices
    %% surrounding FaceB.
    {_,EdgeA,RecA0,IterA} = wings_face:next_cw(IterA0),
    {_,EdgeB,RecB0,IterB} = wings_face:next_ccw(IterB0),
    Col = case RecB0 of
	      #edge{lf=FaceB,b=Col0} -> Col0;
	      #edge{rf=FaceB,a=Col0} -> Col0
	  end,
    RecA1 = case RecA0 of
		#edge{lf=FaceA}=R -> R#edge{a=Col};
		#edge{rf=FaceA}=R -> R#edge{b=Col}
	    end,
    RecB = turn_edge(RecB0),
    {RecA,Pred,Succ} =
	case RecA1 of
	    #edge{lf=FaceA} ->
		update_edge(RecA1, RecB,
			    #edge.lf, #edge.ltpr, #edge.ltsu,
			    #edge.rtpr, #edge.rtsu);
	    #edge{rf=FaceA} ->
		update_edge(RecA1, RecB,
			    #edge.rf, #edge.rtpr, #edge.rtsu,
			    #edge.ltpr, #edge.ltsu)
	    end,
    #we{es=Etab0,fs=Ftab0,he=Htab0} = We0,

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
    #edge{vs=VstartB,ve=VendB} = RecB,
    #edge{vs=VstartA,ve=VendA} = RecA,
    Etab5 = replace_vertex(VstartB, VstartA, WeOrig, Etab4),
    Etab = replace_vertex(VendB, VendA, WeOrig, Etab5),

    %% Update face table
    Ftab1 = wings_face:patch_face(wings_face:other(FaceA, RecA1),
				  EdgeA, Ftab0),
    Ftab = wings_face:patch_face(wings_face:other(FaceB, RecB), EdgeA, Ftab1),

    %% Next edge.
    We = We0#we{es=Etab,fs=Ftab,vc=undefined,he=Htab},
    mirror_weld(N-1, IterA, FaceA, IterB, FaceB, WeOrig, We).

update_edge(New0, Old, FaceP, PrP, SuP, OPrP, OSuP) ->
    New1 = case {element(PrP, Old),element(OSuP, Old)} of
	       {Pred,Pred} -> New0;
	       {Pred,_} -> setelement(PrP, New0, Pred)
	   end,
    New2 = case {element(SuP, Old),element(OPrP, Old)} of
	       {Succ,Succ} -> New1;
	       {Succ,_} -> setelement(SuP, New1, Succ)
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
		  _Other -> Et0			%Deleted or already modified.
	      end
      end, Etab0, Old, We).

%%%
%%% The Flatten command.
%%%

flatten({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun flatten/2);
flatten({Plane,Point}, St) ->
    {save_state,flatten(Plane, Point, St)};
flatten(Plane, St) ->
    {save_state,flatten(Plane, average, St)}.

flatten(Plane0, average, St) ->
    Plane = wings_util:make_vector(Plane0),
    wings_sel:map(
      fun(Faces, We) ->
	      Rs = wings_sel:face_regions(Faces, We),
	      foldl(fun(Fs, W) -> do_flatten(Fs, Plane, W) end, We, Rs)
      end, St);
flatten(normal, Center, St) ->
    wings_sel:map(
      fun(Faces, We) ->
	      Rs = wings_sel:face_regions(Faces, We),
	      foldl(fun(Fs, W) -> do_flatten_normal(Fs, Center, W) end, We, Rs)
      end, St);
flatten(Plane0, Center, St) ->
    Plane = wings_util:make_vector(Plane0),
    wings_sel:map(
      fun(Faces, We) ->
	      Vs = wings_face:to_vertices(Faces, We),
	      wings_vertex:flatten(Vs, Plane, Center, We)
      end, St).

do_flatten(Faces, normal, We) ->
    N = gb_sets:fold(fun(Face, Normal) ->
			     e3d_vec:add(Normal, wings_face:normal(Face, We))
		     end, e3d_vec:zero(), Faces),
    do_flatten(Faces, e3d_vec:norm(N), We);
do_flatten(Faces, PlaneNormal, We) ->
    Vs = wings_face:to_vertices(Faces, We),
    Center = wings_vertex:center(Vs, We),
    wings_vertex:flatten(Vs, PlaneNormal, Center, We).

do_flatten_normal(Faces, Center, We) ->
    N0 = foldl(fun(Face, A) ->
		       [wings_face:normal(Face, We)|A]
	       end, [], gb_sets:to_list(Faces)),
    N = e3d_vec:norm(e3d_vec:add(N0)),
    Vs = wings_face:to_vertices(Faces, We),
    wings_vertex:flatten(Vs, N, Center, We).

%%%
%%% The Smooth command.
%%%

smooth(St0) ->
    {St,Sel} = wings_sel:mapfold(fun smooth/3, [], St0),
    wings_sel:set(Sel, St).

smooth(Faces0, #we{id=Id}=We0, Acc) ->
    Rs = wings_sel:face_regions(Faces0, We0),
    wings_pb:start(?__(1,"smoothing")),
    We1 = wings_pb:done(smooth_regions(Rs, 1, length(Rs), We0)),
    NewFaces = wings_we:new_items(face, We0, We1),
    NewVs = wings_we:new_items(vertex, We0, We1),
    We2 = smooth_connect(NewVs, NewFaces, We1),
    We = wings_we:mirror_flatten(We0, We2),
    {We,[{Id,NewFaces}|Acc]}.

smooth_regions([Faces0|Rs], I, N, #we{he=Htab}=We0) ->
    wings_pb:update(I/N, io_lib:format("~p/~p\n", [I,N])),
    HardEdges0 = wings_face:outer_edges(Faces0, We0),
    HardEdges = gb_sets:union(gb_sets:from_list(HardEdges0), Htab),
    Faces = gb_sets:to_list(Faces0),
    {Vs,Es} = all_edges(Faces0, We0),
    We = wings_subdiv:smooth(Faces, Vs, Es, HardEdges, We0),
    smooth_regions(Rs, I+1, N, We);
smooth_regions([], _, _, We) -> We.

all_edges(Faces, We) ->
    {Vs,Es} = wings_face:fold_faces(
		fun(_, _, Edge, #edge{vs=Va,ve=Vb}, {Vs,Es}) ->
			{[Va,Vb|Vs],[Edge|Es]} end,
		{[],[]}, Faces, We),
    {ordsets:from_list(Vs),ordsets:from_list(Es)}.

smooth_connect(Vs, Faces0, #we{mirror=Mirror}=We0) ->
    Faces1 = gb_sets:add(Mirror, Faces0),
    Faces = sofs:from_external(gb_sets:to_list(Faces1), [face]),
    FaceVs0 = wings_vertex:per_face(Vs, We0),
    FaceVs1 = sofs:from_external(FaceVs0, [{face,[vertex]}]),
    FaceVs2 = sofs:drestriction(FaceVs1, Faces),
    FaceVs = sofs:to_external(FaceVs2),
    smooth_connect_0(FaceVs, We0).

smooth_connect_0([{Face,Vs}|Fvs], We0) ->
    case wings_material:get(Face, We0) of
	'_hole_' ->
	    smooth_connect_0(Fvs, We0);
	_ ->
	    We = smooth_connect_1(Face, Vs, We0),
	    smooth_connect_0(Fvs, We)
    end;
smooth_connect_0([], We) -> We.

smooth_connect_1(Face, [V], We) ->
    Iter0 = wings_face:iterator(Face, We),
    IterCw = wings_face:skip_to_cw(V, Iter0),
    IterCcw = wings_face:skip_to_ccw(V, Iter0),
    smooth_connect_2(IterCw, IterCcw, V, Face, We);
smooth_connect_1(Face, Vs, We) ->
    wings_vertex:connect(Face, Vs, We).

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
		_Other ->
		    smooth_connect_2(IterCw, IterCcw, V, Face, We0)
	    end
    end.

%%%
%%% The Bridge command.
%%%

bridge(#st{shapes=Shapes0,sel=[{IdA,FacesA},{IdB,FacesB}]}=St0) ->
    case {gb_sets:to_list(FacesA),gb_sets:to_list(FacesB)} of
	{[FA],[FB0]} ->
	    #we{next_id=Id}=WeA0 = gb_trees:get(IdA, Shapes0),
	    #we{}=WeB0 = gb_trees:get(IdB, Shapes0),
	    {WeB1,[{face,FB}]} = wings_we:renumber(WeB0, Id, [{face,FB0}]),
	    Mode = unify_modes(WeA0, WeB1),
	    WeA = bridge_null_uvs(Mode, WeA0),
	    WeB = bridge_null_uvs(Mode, WeB1),
	    We = (wings_we:merge(WeA, WeB))#we{mode=Mode},
	    Shapes1 = gb_trees:delete(IdB, Shapes0),
	    Shapes = gb_trees:update(IdA, We, Shapes1),
	    Sel = [{IdA,gb_sets:from_list([FA,FB])}],
	    St1 = wings_sel:set(Sel, St0),
	    St = St1#st{shapes=Shapes},
	    bridge(St);
	_Other ->
	    bridge_error()
    end;
bridge(#st{shapes=Shapes0,sel=[{Id,Faces}]}=St) ->
    case gb_sets:to_list(Faces) of
	[FA,FB] ->
	    We0 = gb_trees:get(Id, Shapes0),
	    We = bridge(FA, FB, We0),
	    Shapes = gb_trees:update(Id, We, Shapes0),
	    St#st{shapes=Shapes,sel=[]};
	_Other ->
	    bridge_error()
    end;
bridge(_St) ->
    bridge_error().

unify_modes(#we{mode=Mode}, #we{mode=Mode}) -> Mode;
unify_modes(_, _) ->
    wings_util:error(?__(1,
			 "An object with vertex colors cannot be bridged with an object with materials.")).

bridge_null_uvs(Mode, #we{mode=Mode}=We) -> We;
bridge_null_uvs(uv, #we{es=Etab0}=We) ->
    Etab = bridge_null_uvs_1(gb_trees:to_list(Etab0), {0.0,0.0}, []),
    We#we{es=Etab}.

bridge_null_uvs_1([{E,Rec}|Es], UV, Acc) ->
    bridge_null_uvs_1(Es, UV, [{E,Rec#edge{a=UV,b=UV}}|Acc]);
bridge_null_uvs_1([], _, Acc) ->
    gb_trees:from_orddict(reverse(Acc)).

bridge(FaceA, FaceB, #we{vp=Vtab}=We) ->
    VsA = wings_face:vertices_ccw(FaceA, We),
    VsB = wings_face:vertices_ccw(FaceB, We),
    if
	length(VsA) =/= length(VsB) ->
	    bridge_error(?__(1,"Faces must have the same number of vertices."));
	true ->
	    An = wings_face:face_normal_cw(VsA, Vtab),
	    Bn = wings_face:face_normal_cw(VsB, Vtab),
	    case e3d_vec:dot(An, Bn) of
		Dot when Dot > 0.99 ->
		    bridge_error(?__(2,"Faces must not point in the same direction."));
		_Dot ->
		    case wings_face:are_neighbors(FaceA, FaceB, We) of
			true ->
			    bridge_error(?__(3,"Faces must not be neighbors."));
			false ->
			    bridge(FaceA, VsA, FaceB, VsB, We)
		    end
	    end
    end.

bridge(FaceA, VsA0, FaceB, VsB0, We0) ->
    Len = wings_face:vertices(FaceA, We0),
    [Va|_] = VsA0,
    [Vb|_] = VsB0,
    {Ids,We} = wings_we:new_wrap_range(Len, 2, We0),
    IterA = wings_face:skip_to_cw(Va, wings_face:iterator(FaceA, We)),
    IterB = wings_face:skip_to_ccw(Vb, wings_face:iterator(FaceB, We)),
    try_bridge(Len, Len, Va, FaceA, IterA,
	       Vb, FaceB, IterB, Ids, We, {9.9E307,We}).

try_bridge(0, _Len, _Va, _FaceA, _IterA, _Vb, _FaceB, _IterB, _, _, {_,We}) ->
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

sum_edge_lens(0, _Ids, _We, Sum) -> Sum;
sum_edge_lens(N, Ids0, #we{es=Etab,vp=Vtab}=We, Sum) ->
    Edge = wings_we:id(0, Ids0),
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    VaPos = gb_trees:get(Va, Vtab),
    VbPos = gb_trees:get(Vb, Vtab),
    Dist = e3d_vec:dist(VaPos, VbPos),
    Ids = wings_we:bump_id(Ids0),
    sum_edge_lens(N-1, Ids, We, Sum + Dist).

force_bridge(FaceA, Va, FaceB, Vb, We0) ->
    Len = wings_face:vertices(FaceA, We0),
    {Ids,We} = wings_we:new_wrap_range(Len, 2, We0),
    IterA = wings_face:skip_to_cw(Va, wings_face:iterator(FaceA, We)),
    IterB = wings_face:skip_to_ccw(Vb, wings_face:iterator(FaceB, We)),
    do_bridge(Len, Va, FaceA, IterA, Vb, FaceB, IterB, Ids, We).

do_bridge(0, _Va, FaceA, _IterA, _Vb, FaceB, _IterB, _, #we{fs=Ftab0}=We) ->
    Ftab1 = gb_trees:delete(FaceA, Ftab0),
    Ftab = gb_trees:delete(FaceB, Ftab1),
    wings_material:delete_faces([FaceA,FaceB], We#we{fs=Ftab});
do_bridge(N, Va0, FaceA, IterA0, Vb0, FaceB, IterB0, Ids0, We0) ->
    #we{es=Etab0,fs=Ftab0} = We0,
    NewEdge = wings_we:id(2, Ids0),
    RightFace = wings_we:id(3, Ids0),
    RightEdge = wings_we:id(4, Ids0),
    
    {_,EdgeA,RecA0,IterA} = wings_face:next_cw(IterA0),
    RecA = case RecA0 of
	       #edge{b=ColA,lf=FaceA,rf=OfA,rtpr=ColEdgeA} ->
		   ColA0 = bridge_color(ColEdgeA, OfA, IterA),
		   RecA0#edge{a=ColA0,lf=RightFace,
			      ltpr=NewEdge,ltsu=RightEdge};
	       #edge{a=ColA,rf=FaceA,lf=OfA,ltpr=ColEdgeA} ->
		   ColA0 = bridge_color(ColEdgeA, OfA, IterA),
		   RecA0#edge{b=ColA0,rf=RightFace,
			      rtpr=NewEdge,rtsu=RightEdge}
	   end,
    Etab1 = gb_trees:update(EdgeA, RecA, Etab0),

    {_,EdgeB,RecB0,IterB} = wings_face:next_ccw(IterB0),
    RecB = case RecB0 of
	       #edge{b=ColB,lf=FaceB,rf=OfB,rtpr=ColEdgeB} ->
		   ColB0 = bridge_color(ColEdgeB, OfB, IterA),
		   RecB0#edge{a=ColB0,lf=RightFace,
			      ltpr=RightEdge,ltsu=NewEdge};
	       #edge{a=ColB,rf=FaceB,lf=OfB,ltpr=ColEdgeB} ->
		   ColB0 = bridge_color(ColEdgeB, OfB, IterA),
		   RecB0#edge{b=ColB0,rf=RightFace,
			      rtpr=RightEdge,rtsu=NewEdge}
	   end,
    Etab2 = gb_trees:update(EdgeB, RecB, Etab1),

    RightRec0 = get_edge(RightEdge, Etab0),
    RightRec = RightRec0#edge{a=ColB,lf=RightFace,ltpr=EdgeA,ltsu=EdgeB},
    Etab3 = gb_trees:enter(RightEdge, RightRec, Etab2),
    
    NewRec0 = get_edge(NewEdge, Etab0),
    NewRec = NewRec0#edge{ve=Va0,vs=Vb0,b=ColA,
			  rf=RightFace,rtpr=EdgeB,rtsu=EdgeA},
    Etab = gb_trees:enter(NewEdge, NewRec, Etab3),

    Mat = wings_material:get(FaceA, We0),
    We1 = wings_material:assign(Mat, [RightFace], We0),
    Ftab = gb_trees:insert(RightFace, NewEdge, Ftab0),
    
    We = We1#we{es=Etab,fs=Ftab},
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
    bridge_error(?__(1,"Exactly two faces must be selected.")).

bridge_error(Error) ->
    wings_util:error(Error).

bridge_color(Edge, Face, Iter) ->
    Etab = wings_face:iter2etab(Iter),
    case gb_trees:get(Edge, Etab) of
	#edge{lf=Face,a=Col} -> Col;
	#edge{rf=Face,b=Col} -> Col
    end.

%%%
%%% The Lift command.
%%%

lift_selection(Dir, OrigSt) ->
    Desc = ?__(1,"Select edge or vertex to act as hinge"),
    Fun = fun(check, St) ->
		  lift_check_selection(St, OrigSt);
	     (exit, {_,_,#st{selmode=Mode,sel=Sel}=St}) ->
		  case lift_check_selection(St, OrigSt) of
		      {_,[]} -> {[],[{Dir,Mode,Sel}]};
		      {_,_} -> error
		  end
	  end,
    {[{Fun,Desc}],[],[],[vertex,edge]}.

lift_check_selection(#st{selmode=edge,sel=EdgeSel}, OrigSt) ->
    Res = wings_sel:fold(
	    fun(_, _, error) -> error;
	       (Faces, #we{id=Id}=We, [{Id,Edges}|More]) ->
		    case lift_face_edge_pairs(Faces, Edges, We) of
			error -> error;
			_ -> More
		    end;
	       (_, _, _) -> error
	    end, EdgeSel, OrigSt),
    case Res of
	[] -> {none,""};
	_ -> {none,?__(1,"Face and edge selections don't match.")}
    end;
lift_check_selection(#st{selmode=vertex,sel=VsSel}, OrigSt) ->
    Res = wings_sel:fold(
	    fun(_, _, error) -> error;
	       (Faces, #we{id=Id}=We, [{Id,Vs}|More]) ->
		    case lift_face_vertex_pairs(Faces, Vs, We) of
			error -> error;
			_ -> More
		    end;
	       (_, _, _) -> error
	    end, VsSel, OrigSt),
    case Res of
	[] -> {none,""};
	_ -> {none,?__(2,"Face and vertex selections don't match.")}
    end.

lift({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun lift/2);
lift({Dir,edge,EdgeSel}, St) ->
    lift_from_edge(Dir, EdgeSel, St);
lift({Dir,vertex,VertexSel}, St) ->
    lift_from_vertex(Dir, VertexSel, St);
lift(Dir, St) ->
    wings:ask(lift_selection(Dir, St), St, fun lift/2).

lift_setup_drag(Tvs, rotate, St) ->
    wings_drag:setup(Tvs, [angle], St);
lift_setup_drag(Tvs, free, St) ->
    wings_drag:setup(Tvs, [dx,dy,dz], [screen_relative], St);
lift_setup_drag(Tvs, _, St) ->
    wings_drag:setup(Tvs, [distance], St).

%%%
%%% Lift from edge.
%%%

lift_from_edge(Dir, EdgeSel, St0) ->
    Res = wings_sel:mapfold(
	    fun(Faces, #we{id=Id}=We0, {[{Id,Edges}|ES],Tv0}) ->
		    {We,Tv} = lift_from_edge(Dir, Faces, Edges, We0, Tv0),
		    {We,{ES,Tv}};
	       (_, _, _) -> lift_sel_mismatch()
	    end, {EdgeSel,[]}, St0),
    case Res of
	{St,{[],Tvs}} -> lift_setup_drag(Tvs, Dir, St);
	{_,_} -> lift_sel_mismatch()
    end.

lift_sel_mismatch() ->
    wings_util:error(?__(1,"Face and edge selections don't match.")).
	
lift_from_edge(Dir, Faces, Edges, We0, Tv) ->
    case lift_face_edge_pairs(Faces, Edges, We0) of
	error -> lift_sel_mismatch();		%Can happen if repeated.
	FaceEdgeRel ->
	    We = wings_extrude_face:faces(Faces, We0),
	    lift_from_edge_1(Dir, FaceEdgeRel, We0, We, Tv)
    end.

lift_from_edge_1(Dir, [{Face,Edge}|T], #we{es=Etab}=OrigWe, We0, Tv0) ->
    Side = case gb_trees:get(Edge, Etab) of
	       #edge{lf=Face} -> left;
	       #edge{rf=Face} -> right
	   end,
    {We,Tv} = lift_from_edge_2(Dir, Face, Edge, Side, We0, Tv0),
    lift_from_edge_1(Dir, T, OrigWe, We, Tv);
lift_from_edge_1(_Dir, [], _OrigWe, We, Tv) -> {We,Tv}.

lift_from_edge_2(Dir, Face, Edge, Side, #we{id=Id,es=Etab}=We0, Tv) ->
    FaceVs0 = ordsets:from_list(wings_face:vertices_ccw(Face, We0)),
    #edge{vs=Va0,ve=Vb0} = gb_trees:get(Edge, Etab),
    {Va,Ea} = lift_edge_vs(Va0, FaceVs0, We0),
    {Vb,Eb} = lift_edge_vs(Vb0, FaceVs0, We0),
    We1 = wings_collapse:collapse_edge(Ea, We0),
    We = wings_collapse:collapse_edge(Eb, We1),
    FaceVs = ordsets:subtract(FaceVs0, ordsets:from_list([Va,Vb])),
    VaPos = wings_vertex:pos(Va, We0),
    VbPos = wings_vertex:pos(Vb, We0),
    case Dir of
	rotate ->
	    Axis = case Side of
			left -> e3d_vec:norm_sub(VbPos, VaPos);
			right -> e3d_vec:norm_sub(VaPos, VbPos)
		   end,
	    Rot = wings_rotate:rotate(Axis, VaPos, FaceVs, We, Tv),
	    {We,Rot};
	_Other ->
	    Vec = wings_util:make_vector(Dir),
	    Move = wings_move:setup_we(vertex, Vec,
				       gb_sets:from_list(FaceVs), We),
	    {We,[{Id,Move}|Tv]}
    end.

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

%% Pair the face selection with the edge selection (if possible).
%%  Returns: [{Face,Edge}] | error
lift_face_edge_pairs(Faces, Edges, We) ->
    EsFs0 = wings_face:fold_faces(
	      fun(Face, _, Edge, _, A) -> [{Edge,Face}|A] end,
	      [], Faces, We),
    EsFs1 = sofs:relation(EsFs0, [{edge,face}]),
    EsFs = sofs:restriction(EsFs1, sofs:set(gb_sets:to_list(Edges), [edge])),
    FaceEdgeRel0 = sofs:converse(EsFs),
    case sofs:is_a_function(FaceEdgeRel0) of
	false -> error;
	true ->
	    FaceEdgeRel = sofs:to_external(FaceEdgeRel0),
	    case gb_sets:size(Faces) of
		Size when Size =:= length(FaceEdgeRel) -> FaceEdgeRel;
		_Size -> error
	    end
    end.

%%%
%%% Lift from vertex.
%%%

lift_from_vertex(Dir, VsSel, St0) ->
    Res = wings_sel:mapfold(
	    fun(Faces, #we{id=Id}=We0, {[{Id,Vs}|MoreVs],Tv0}) ->
		    {We,Tv} = lift_from_vertex(Dir, Faces, Vs, We0, Tv0),
		    {We,{MoreVs,Tv}};
	       (_, _, _) -> lift_vtx_sel_mismatch()
	    end, {VsSel,[]}, St0),
    case Res of
	{St,{[],Tvs}} -> lift_setup_drag(Tvs, Dir, St);
	{_,_} -> lift_vtx_sel_mismatch()
    end.

lift_vtx_sel_mismatch() ->
    wings_util:error(?__(1,"Face and vertex selections don't match.")).

lift_from_vertex(Dir, Faces, Vs, We, Tv) ->
    case lift_face_vertex_pairs(Faces, Vs, We) of
	error -> lift_vtx_sel_mismatch();	%Can happen if repeated.
	FaceVtxRel ->
	    lift_from_vertex_1(Dir, FaceVtxRel, We, Tv)
    end.

lift_from_vertex_1(Dir, [{Face,V}|T], We0, Tv0) ->
    {We,Tv} = lift_from_vertex_2(Dir, Face, V, We0, Tv0),
    lift_from_vertex_1(Dir, T, We, Tv);
lift_from_vertex_1(_Dir, [], We, Tv) -> {We,Tv}.

lift_from_vertex_2(Dir, Face, V, #we{id=Id,next_id=Next}=We0, Tv) ->
    We1 = wings_extrude_face:faces([Face], We0),
    We = wings_vertex:fold(
	   fun(Edge, _, _, W) when Edge >= Next ->
		   wings_collapse:collapse_edge(Edge, V, W);
	      (_, _, _, W) -> W
	   end, We1, V, We1),
    FaceVs = wings_we:new_items(vertex, We0, We),
    case Dir of
	rotate ->
	    Vpos = wings_vertex:pos(V, We),
	    Vecs = wings_vertex:fold(
		     fun(_, _, #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}, A)
			when Lf =:= Face;
			     Rf =:= Face ->
			     Pos = case V of
				       Va -> wings_vertex:pos(Vb, We);
				       Vb -> wings_vertex:pos(Va, We)
				   end,
			     [e3d_vec:norm_sub(Pos, Vpos)|A];
			(_, _, _, A) -> A
		     end, [], V, We),
	    M = e3d_vec:norm(e3d_vec:add(Vecs)),
	    N = wings_face:normal(Face, We),
	    Axis = e3d_vec:cross(M, N),
 	    Rot = wings_rotate:rotate(Axis, Vpos, gb_sets:to_list(FaceVs),
				      We, Tv),
	    {We,Rot};
	_Other ->
	    Vec = wings_util:make_vector(Dir),
	    Move = wings_move:setup_we(vertex, Vec, FaceVs, We),
	    {We,[{Id,Move}|Tv]}
    end.

%% Pair the face selection with the vertex selection (if possible).
%%  Returns: [{Face,Vertex}] | error
lift_face_vertex_pairs(Faces, Vs, We) ->
    VsFs0 = wings_face:fold_faces(
	      fun(Face, V, _, _, A) ->
		      [{V,Face}|A]
	      end, [], Faces, We),
    VsFs1 = sofs:relation(VsFs0, [{vertex,face}]),
    VsFs = sofs:restriction(VsFs1, sofs:set(gb_sets:to_list(Vs), [vertex])),
    FaceVtxRel0 = sofs:converse(VsFs),
    case sofs:is_a_function(FaceVtxRel0) of
	false -> error;
	true ->
	    FaceVtxRel = sofs:to_external(FaceVtxRel0),
	    case gb_sets:size(Faces) of
		Size when Size =:= length(FaceVtxRel) -> FaceVtxRel;
		_Size -> error
	    end
    end.

%%%
%%% The Put On command.
%%%

put_on(#st{sel=[{_,Faces}]}=St) ->
    case gb_trees:size(Faces) of
	1 ->
	    wings:ask(put_on_selection(St), St, fun put_on/2);
	_ ->
	    wings_util:error(?__(1,"There must only be one face selected."))
    end;
put_on(_) ->
    wings_util:error(?__(1,"There must only be one face selected.")).

put_on_selection(OrigSt) ->
    Desc = ?__(1,"Select target element on which to put source object"),
    Fun = fun(check, St) -> put_on_check_selection(St, OrigSt);
	     (exit, {_,_,#st{selmode=Mode,sel=Sel}=St}) ->
		  case put_on_check_selection(St, OrigSt) of
		      {_,[]} -> {[],[{Mode,Sel}]};
		      {_,_} -> error
		  end
	  end,
    {[{Fun,Desc}],[],[],[face,edge,vertex]}.

put_on_check_selection(#st{sel=[{Id,_}]}, #st{sel=[{Id,_}]}) ->
    {none,?__(1,"Selection must not be in the same object.")};
put_on_check_selection(#st{sel=[{_,Elems}]}, _) ->
    case gb_trees:size(Elems) of
	1 -> {none,""};
	_ -> {none,?__(2,"Select only one element.")}
    end;
put_on_check_selection(_, _) ->
    {none,?__(2,"Select only one element.")}.

put_on({Mode,[{Id,Els}]}, #st{shapes=Shs}=St0) ->
    We0 = gb_trees:get(Id, Shs),
    [El] = gb_sets:to_list(Els),
    {Axis,Target} = on_target(Mode, El, We0),
    St = wings_sel:map(fun(Faces, We) ->
			       [Face] = gb_sets:to_list(Faces),
			       put_on_1(Face, Axis, Target, We)
		       end, St0),
    {save_state,St}.
    
put_on_1(Face, Axis, Target, We) ->
    Vs = wings_face:vertices_ccw(Face, We),
    Center = wings_vertex:center(Vs, We),
    N = wings_face:face_normal_cw(Vs, We),
    RotAxis = e3d_mat:rotate_s_to_t(N, Axis),
    M0 = e3d_mat:translate(Target),
    M1 = e3d_mat:mul(M0, RotAxis),
    M = e3d_mat:mul(M1, e3d_mat:translate(e3d_vec:neg(Center))),
    wings_we:transform_vs(M, We).

%%%
%%% The "Clone On" command (RMB click on Put On).
%%%

clone_on(#st{sel=[{_,Faces}]}=St) ->
    case gb_trees:size(Faces) of
	1 ->
	    wings:ask(clone_on_selection(), St, fun clone_on/2);
	_ ->
	    wings_util:error(?__(1,"There must only be one face selected."))
    end;
clone_on(_) ->
    wings_util:error(?__(1,"There must only be one face selected.")).

clone_on_selection() ->
    Desc = ?__(1,"Select target elements on which to put clones"),
    Fun = fun(check, _) ->
		  {none,""};
	     (exit, {_,_,#st{selmode=Mode,sel=Sel}}) ->
		  {[],[{Mode,Sel}]}
	  end,
    {[{Fun,Desc}],[],[],[face,edge,vertex]}.

clone_on({Mode,Sel}, #st{sel=[{Id,Faces}],shapes=Shs0}=St) ->
    We = gb_trees:get(Id, Shs0),
    [Face] = gb_sets:to_list(Faces),
    Vs = wings_face:vertices_ccw(Face, We),
    Center = wings_vertex:center(Vs, We),
    Translate = e3d_mat:translate(e3d_vec:neg(Center)),
    N = wings_face:face_normal_cw(Vs, We),
    #st{shapes=Shs,onext=Onext} =
	clone_on_1(Translate, N, We, St#st{selmode=Mode,sel=Sel}),
    {save_state,St#st{shapes=Shs,onext=Onext}}.
    
clone_on_1(Tr, N, Clone, St) ->
    wings_sel:fold(
      fun(Els, We, S) ->
	      clone_2(gb_sets:to_list(Els), We, Tr, N, Clone, S)
      end, St, St).

clone_2([E|Els], We, Tr, N, Clone, St0) ->
    St = clone_3(E, We, Tr, N, Clone, St0),
    clone_2(Els, We, Tr, N, Clone, St);
clone_2([], _, _, _, _, St) -> St.

clone_3(El, We, Tr, N, Clone, #st{selmode=Mode}=St) ->
    {Axis,Target} = on_target(Mode, El, We),
    RotAxis = e3d_mat:rotate_s_to_t(N, Axis),
    M0 = e3d_mat:translate(Target),
    M1 = e3d_mat:mul(M0, RotAxis),
    M = e3d_mat:mul(M1, Tr),
    NewWe = wings_we:transform_vs(M, Clone),
    wings_shape:insert(NewWe, clone, St).

%%
%% Common help function.
%%

on_target(face, Face, We) ->
    Vs = wings_face:vertices_ccw(Face, We),
    N = wings_face:face_normal_ccw(Vs, We),
    Center = wings_vertex:center(Vs, We),
    {N,Center};
on_target(edge, Edge, #we{es=Etab}=We) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
    N = e3d_vec:norm(e3d_vec:add([wings_face:normal(Lf, We),
				  wings_face:normal(Rf, We)])),
    Center = wings_vertex:center([Va,Vb], We),
    {N,Center};
on_target(vertex, V, We) ->
    N = wings_vertex:normal(V, We),
    Center = wings_vertex:pos(V, We),
    {N,Center}.

%%%
%%% Set vertex color for selected faces.
%%%

set_color(Color, St) ->
    wings_sel:map(fun(Fs, We) ->
			  set_color_1(gb_sets:to_list(Fs), Color,
				      We#we{mode=vertex})
		  end, St).

set_color_1([F|Fs], Color, #we{es=Etab0}=We) ->
    Etab = wings_face:fold(
	     fun(_V, Edge, Rec0, Es) ->
		     Rec = case Rec0 of
			       #edge{lf=F} -> Rec0#edge{a=Color};
			       #edge{rf=F} -> Rec0#edge{b=Color}
			   end,
		     gb_trees:update(Edge, Rec, Es)
	     end, Etab0, F, We),
    set_color_1(Fs, Color, We#we{es=Etab});
set_color_1([], _, We) -> We.

    
%% outer_edge_partition(FaceSet, WingedEdge) -> [[Edge]].
%%  Partition all outer edges. Outer edges are all edges
%%  between one face in the set and one outside.

outer_edge_partition(Faces, We) when is_list(Faces) ->
    collect_outer_edges(Faces, gb_sets:from_list(Faces), We);
outer_edge_partition(Faces, We) ->
    collect_outer_edges(gb_sets:to_list(Faces), Faces, We).

collect_outer_edges(Fs, Faces, We) ->
    F0 = wings_face:fold_faces(
	   fun(Face, _, Edge, #edge{vs=V,ve=OtherV,lf=Face,rf=Other}, Acc) ->
		   case gb_sets:is_member(Other, Faces) of
		       false -> [{V,{Edge,V,OtherV}}|Acc];
		       true -> Acc
		   end;
	      (Face, _, Edge, #edge{vs=OtherV,ve=V,rf=Face,lf=Other}, Acc) ->
		   case gb_sets:is_member(Other, Faces) of
		       false -> [{V,{Edge,V,OtherV}}|Acc];
		       true -> Acc
		   end
	   end, [], Fs, We),
    F = gb_trees:from_orddict(wings_util:rel2fam(F0)),
    partition_edges(F, Faces, We, []).

partition_edges(Es0, Faces, We, Acc) ->
    case gb_trees:is_empty(Es0) of
	true -> Acc;
	false ->
	    {Key,Val,Es1} = gb_trees:take_smallest(Es0),
	    {Part,Es} = partition_edges(Key, Val, Faces, Es1, We, []),
	    partition_edges(Es, Faces, We, [Part|Acc])
    end.

partition_edges(Va, [{Edge,Va,Vb}], Faces, Es0, We, Acc0) ->
    Acc = [Edge|Acc0],
    case gb_trees:lookup(Vb, Es0) of
	none ->
	    {Acc,Es0};
	{value,Val} ->
	    Es = gb_trees:delete(Vb, Es0),
	    partition_edges(Vb, Val, Faces, Es, We, Acc)
    end;
partition_edges(Va, [Val|More], Faces, Es0, We, []) ->
    Es = gb_trees:insert(Va, More, Es0),
    partition_edges(Va, [Val], Faces, Es, We, []);
partition_edges(Va, Edges, Faces, Es, We, Acc) ->
    part_try_all_edges(Va, Edges, Faces, Es, We, Acc, [], none).
    
%% Here we have multiple choices of edges. Use the shortest path
%% that don't return us to the Va vertex.
%% (We want edge loops without repeated vertices.)
part_try_all_edges(Va, [Val|More], Faces, Es0, We, Acc, Done0, Path0) ->
    Es1 = gb_trees:insert(Va, [{repeated,Va,fake}], Es0),
    Done = [Val|Done0],
    case partition_edges(Va, [Val], Faces, Es1, We, Acc) of
	none ->
	    part_try_all_edges(Va, More, Faces, Es0, We, Acc, Done, Path0);
	{[repeated|_],_} ->
	    part_try_all_edges(Va, More, Faces, Es0, We, Acc, Done, Path0);
	{_,_}=Found ->
	    Path1 = part_shortest_path(Path0, Found),
	    Path = {Path1,Done0,More},
	    part_try_all_edges(Va, More, Faces, Es0, We, Acc, Done, Path)
    end;
part_try_all_edges(Va, [], _, _, _, _, _, {{Path,Es},Alt0,Alt1}) ->
    case Alt0++Alt1 of
	[] -> {Path,gb_trees:delete(Va, Es)};
	Alt -> {Path,gb_trees:enter(Va, Alt, Es)}
    end;
part_try_all_edges(_Va, [], _, _, _, _, _Alt, none) -> none.

part_shortest_path(none, Path) -> Path;
part_shortest_path({{AccA,_},_,_}=A, {{AccB,_},_,_})
  when length(AccA) < length(AccB) -> A;
part_shortest_path(_, B) -> B.
