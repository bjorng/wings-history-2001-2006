%%
%%  wings_subdiv.erl --
%%
%%     This module implements the Smooth command for objects and faces.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_subdiv.erl,v 1.69 2004/03/09 21:57:15 raimo_niskanen Exp $
%%

-module(wings_subdiv).
-export([smooth/1,smooth/5]).
-export([setup/1,quick_preview/1,update/2,draw/2,draw_smooth_edges/1,
	 clean/1,smooth_we/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,merge/1,foreach/2]).

%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.

smooth(We) ->
    {Faces,Htab} = smooth_faces_htab(We),
    smooth(Faces, Htab, We).
    
smooth(Fs, Htab, #we{vp=Vtab,es=Etab}=We) ->
    Vs = gb_trees:keys(Vtab),
    Es = gb_trees:keys(Etab),
    smooth(Fs, Vs, Es, Htab, We).

smooth(Fs, Vs, Es, Htab, #we{vp=Vp,next_id=Id}=We0) ->
    wings_pb:start("smoothing"),
    wings_pb:update(0.05, "calculating face centers"),
    FacePos0 = face_centers(Fs, We0),

    %% First do all topological changes to the edge table.
    wings_pb:update(0.20, "cutting edges"),
    We1 = cut_edges(Es, Htab, We0#we{vc=undefined}),
    wings_pb:update(0.25, "updating materials"),
    We2 = smooth_materials(Fs, FacePos0, We1),
    wings_pb:update(0.47, "creating new faces"),
    We = smooth_faces(FacePos0, Id, We2),
    wings_pb:update(0.60, "moving vertices"),

    %% Now calculate all vertex positions.
    FacePos = gb_trees:from_orddict(FacePos0),
    {UpdatedVs,Mid} = update_edge_vs(Es, We0, FacePos, Htab, Vp, Id),
    NewVs = smooth_new_vs(FacePos0, Mid),
    Vtab = smooth_move_orig(Vs, FacePos, Htab, We0, UpdatedVs ++ NewVs),
    wings_pb:update(1.0, "finishing"),
    Res = wings_util:validate_mirror(wings_we:rebuild(We#we{vp=Vtab})),
    wings_pb:done(),
    Res.

smooth_faces_htab(#we{mirror=none,fs=Ftab,he=Htab}) ->
    Faces = gb_trees:keys(Ftab),
    {Faces,Htab};
smooth_faces_htab(#we{mirror=Face,fs=Ftab,he=Htab}=We) ->
    Faces = gb_trees:keys(gb_trees:delete(Face, Ftab)),
    He0 = wings_face:outer_edges([Face], We),
    He = gb_sets:union(gb_sets:from_list(He0), Htab),
    {Faces,He}.

%%%
%%% Calculation of face centers.
%%%

face_centers(Faces, We) ->
    face_centers(Faces, We, []).

face_centers([Face|Fs], We, Acc) ->
    {Vs,Cols} = wings_face:fold(
		  fun(V, _, #edge{ve=V,a=C}, {Vs0,Col0}) ->
			  {[V|Vs0],[C|Col0]};
		     (V, _, #edge{vs=V,b=C}, {Vs0,Col0}) ->
			  {[V|Vs0],[C|Col0]}
		  end, {[],[]}, Face, We),
    case Vs of
	[_,_] ->
	    wings_util:error("Face " ++ integer_to_list(Face) ++
			     " has only two edges.");
	_ ->
	    Center0 = wings_vertex:center(Vs, We),
	    Center = wings_util:share(Center0),
	    Col = wings_color:average(Cols),
	    face_centers(Fs, We, [{Face,{Center,Col,length(Vs)}}|Acc])
    end;
face_centers([], _We, Acc) -> reverse(Acc).

%%%
%%% Updating of the topology (edge and hard edge tables).
%%%

cut_edges(Es, Hard, #we{he=Htab0,next_id=Id0}=We) ->
    Etab0 = prepare_etab(Es, We),
    {Id,Etab,Htab} = cut_edges_1(Es, Hard, Id0, Etab0, Htab0),
    We#we{es=Etab,he=Htab,next_id=Id}.

prepare_etab(Es, #we{es=Etab0,next_id=Id}) ->
    Etab = prepare_etab_1(Id+length(Es)-1, Id, []),
    gb_trees:from_orddict(gb_trees:to_list(Etab0) ++ Etab).

prepare_etab_1(Id, Lim, Acc) when Id >= Lim ->
    prepare_etab_1(Id-1, Lim, [{Id,dummy}|Acc]);
prepare_etab_1(_, _, Acc) -> Acc.

cut_edges_1([Edge|Es], Hard, NewEdge, Etab0, Htab0) ->
    Rec = gb_trees:get(Edge, Etab0),
    Etab = fast_cut(Edge, Rec, NewEdge, Etab0),
    case gb_sets:is_member(Edge, Hard) of
	true ->
	    Htab = case gb_sets:is_member(Edge, Htab0) of
		       true -> gb_sets:insert(NewEdge, Htab0);
		       false -> Htab0
		   end,
	    cut_edges_1(Es, Hard, NewEdge+1, Etab, Htab);
	false ->
	    cut_edges_1(Es, Hard, NewEdge+1, Etab, Htab0)
    end;
cut_edges_1([], _Hard, Id, Etab, Htab) ->
    {Id,Etab,Htab}.

fast_cut(Edge, Template, NewV=NewEdge, Etab0) ->
    #edge{a=ACol,b=BCol,lf=Lf,rf=Rf,
	  ltpr=EdgeA,rtsu=EdgeB,rtpr=NextBCol} = Template,
    AColOther = get_vtx_color(EdgeA, Lf, Etab0),
    NewColA = wings_color:average(AColOther, ACol),
    BColOther = get_vtx_color(NextBCol, Rf, Etab0),
    NewColB = wings_color:average(BColOther, BCol),

    NewEdgeRec = Template#edge{vs=NewV,a=NewColA,ltsu=Edge,rtpr=Edge},
    Etab1 = gb_trees:update(NewEdge, NewEdgeRec, Etab0),
    EdgeRec = Template#edge{ve=NewV,b=NewColB,rtsu=NewEdge,ltpr=NewEdge},
    Etab2 = gb_trees:update(Edge, EdgeRec, Etab1),
    Etab = wings_edge:patch_edge(EdgeA, NewEdge, Edge, Etab2),
    wings_edge:patch_edge(EdgeB, NewEdge, Edge, Etab).

get_vtx_color(Edge, Face, Etab) ->
    case gb_trees:get(Edge, Etab) of
	#edge{lf=Face,a=Col} -> Col;
	#edge{rf=Face,b=Col} -> Col
    end.

smooth_faces(FacePos, Id, We) ->
    smooth_faces_1(FacePos, Id, [], We).

smooth_faces_1([{Face,{_,Color,NumIds}}|Fs], Id, EsAcc0, #we{es=Etab0}=We0) ->
    {Ids,We} = wings_we:new_wrap_range(NumIds, 1, We0),
    NewV = wings_we:id(0, Ids),
    {Etab,EsAcc,_} =
	wings_face:fold(
	  fun(_, E, Rec, A) ->
		  smooth_edge(Face, E, Rec, NewV, Color, Id, A)
	  end, {Etab0,EsAcc0,Ids}, Face, We),
    smooth_faces_1(Fs, Id, EsAcc, We#we{es=Etab});
smooth_faces_1([], _, Es, #we{es=Etab0}=We) ->
    Etab1 = gb_trees:to_list(Etab0) ++ reverse(Es),
    Etab = gb_trees:from_orddict(Etab1),
    We#we{es=Etab,fs=undefined}.

smooth_edge(Face, Edge, Rec0, NewV, Color, Id, {Etab0,Es0,Ids0}) ->
    LeftEdge = RFace = wings_we:id(0, Ids0),
    NewEdge = LFace = wings_we:id(1, Ids0),
    RightEdge = wings_we:id(2, Ids0),
    case Rec0 of
	#edge{ve=Vtx,b=OldCol,rf=Face} when Vtx >= Id ->
	    Ids = Ids0,
	    Rec = Rec0#edge{rf=RFace,rtsu=NewEdge},
	    NewErec0 = get_edge(NewEdge, Es0),
	    NewErec = NewErec0#edge{vs=Vtx,a=OldCol,ve=NewV,b=Color,
				    rf=RFace,lf=LFace,
				    rtpr=Edge,rtsu=LeftEdge};
	#edge{vs=Vtx,a=OldCol,lf=Face} when Vtx >= Id ->
	    Ids = Ids0,
	    Rec = Rec0#edge{lf=RFace,ltsu=NewEdge},
	    NewErec0 = get_edge(NewEdge, Es0),
	    NewErec = NewErec0#edge{vs=Vtx,a=OldCol,ve=NewV,b=Color,
				    rf=RFace,lf=LFace,
				    rtpr=Edge,rtsu=LeftEdge};
 	#edge{vs=Vtx,rf=Face} when Vtx >= Id ->
	    Rec = Rec0#edge{rf=LFace,rtpr=NewEdge},
	    NewErec0 = get_edge(NewEdge, Es0),
 	    NewErec = NewErec0#edge{ltpr=RightEdge,ltsu=Edge},
	    Ids = wings_we:bump_id(Ids0);
 	#edge{ve=Vtx,lf=Face} when Vtx >= Id ->
	    Rec = Rec0#edge{lf=LFace,ltpr=NewEdge},
	    NewErec0 = get_edge(NewEdge, Es0),
 	    NewErec = NewErec0#edge{ltpr=RightEdge,ltsu=Edge},
	    Ids = wings_we:bump_id(Ids0)
    end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    Es = store(NewEdge, NewErec, Es0),
    {Etab,Es,Ids}.

get_edge(Key, [{K,_Value}|_]) when Key > K -> #edge{};
get_edge(Key, [{K,_Value}|D]) when Key < K -> get_edge(Key, D);
get_edge(_Key, [{_K,Value}|_]) -> Value;	%Key == K
get_edge(_Key, []) -> #edge{}.

%% Store in reverse order.
store(Key, New, [{K,_Old}=E|Dict]) when Key > K ->
    [{Key,New},E|Dict];
store(Key, New, [{K,_Old}=E|Dict]) when Key < K ->
    [E|store(Key, New, Dict)];
store(Key, New, [{_K,_Old}|Dict]) ->		%Key == K
    [{Key,New}|Dict];
store(Key, New, []) -> [{Key,New}].

smooth_materials(_, _, #we{mat=Mat}=We) when is_atom(Mat) -> We;
smooth_materials(Fs, FacePos, #we{fs=Ftab}=We) ->
    Mat0 = wings_material:get_all(We),
    case length(Fs) =:= gb_trees:size(Ftab) of
	true ->					%We are smoothing all faces.
	    smooth_materials_1(Mat0, FacePos, We, []);
	false ->				%Must pick up the faces not smoothed.
	    Mat1 = sofs:from_external(Mat0, [{face,mat}]),
	    Changed = sofs:from_external(Fs, [face]),
	    {Mat2,Keep0} = sofs:partition(1, Mat1, Changed),
	    Mat = sofs:to_external(Mat2),
	    Keep = sofs:to_external(Keep0),
	    smooth_materials_1(Mat, FacePos, We, Keep)
    end.

smooth_materials_1(Fmat, Fpos, #we{next_id=Id}=We, Keep) ->
    Mat = smooth_materials_2(Fmat, Fpos, Id, Keep),
    wings_material:replace_materials(Mat, We).

smooth_materials_2([{F,Mat}|Fs], [{F,{_,_,N}}|Fpos], Face, Acc0) ->
    NextFace = Face+N,
    Acc = smooth_materials_3(Mat, NextFace, Face, Acc0),
    smooth_materials_2(Fs, Fpos, NextFace, Acc);
smooth_materials_2([], [], _, Acc) -> Acc.

smooth_materials_3(_, Face, Face, Acc) -> Acc;
smooth_materials_3(Mat, NextFace, Face, Acc) ->
    smooth_materials_3(Mat, NextFace, Face+1, [{Face,Mat}|Acc]).

%%%
%%% Moving of vertices.
%%%

smooth_move_orig(Vs, FacePos, Htab, #we{vp=Vtab}=We, VtabTail) ->
    MoveFun = smooth_move_orig_fun(Vtab, FacePos, Htab),
    RevVtab = case gb_trees:size(Vtab) of
		  N when N =:= length(Vs) ->
		      smooth_move_orig_all(gb_trees:to_list(Vtab), MoveFun, We, []);
		  _ ->
		      smooth_move_orig_some(Vs, gb_trees:to_list(Vtab), MoveFun, We, [])
	      end,
    gb_trees:from_orddict(reverse(RevVtab, VtabTail)).

smooth_move_orig_all([{V,Pos0}|Vs], MoveFun, We, Acc) ->
    Pos = smooth_move_orig_1(V, Pos0, MoveFun, We),
    smooth_move_orig_all(Vs, MoveFun, We, [{V,Pos}|Acc]);
smooth_move_orig_all([], _FacePos, _MoveFun, Acc) -> Acc.

smooth_move_orig_some([V|Vs], [{V,Pos0}|Vs2], MoveFun, We, Acc) ->
    Pos = smooth_move_orig_1(V, Pos0, MoveFun, We),
    smooth_move_orig_some(Vs, Vs2, MoveFun, We, [{V,Pos}|Acc]);
smooth_move_orig_some(Vs, [Pair|Vs2], MoveFun, We, Acc) ->
    smooth_move_orig_some(Vs, Vs2, MoveFun, We, [Pair|Acc]);
smooth_move_orig_some([], [], _, _, Acc) -> Acc;
smooth_move_orig_some([], Vs2, _, _, Acc) -> reverse(Vs2, Acc).

smooth_move_orig_1(V, S, MoveFun, We) ->
    {_,Ps0,Hard} = wings_vertex:fold(MoveFun, {V,[],[]}, V, We),
    case length(Hard) of
	NumHard when NumHard < 2 ->
	    Ps = e3d_vec:add(Ps0),
	    {A,B} = case length(Ps0) of
			2*3 -> {1/9,1/3};
			2*4 -> {1/16,2/4};
			2*5 -> {1/25,3/5};
			N0 -> 
			    N = N0 bsr 1,
			    {1.0/(N*N),(N-2.0)/N}
		    end,
	    Pos = e3d_vec:add_prod(e3d_vec:mul(Ps, A), S, B),
	    wings_util:share(Pos);
	NumHard when NumHard =:= 2 ->
	    Pos0 = e3d_vec:add([e3d_vec:mul(S, 6.0)|Hard]),
	    Pos = e3d_vec:mul(Pos0, 1/8),
	    wings_util:share(Pos);
	_ThreeOrMore -> S
    end.

smooth_move_orig_fun(Vtab, FacePos, Htab) ->
    case gb_sets:is_empty(Htab) of
	true ->
	    %% No hard eges imply that all faces can be found
	    %% in the FacePos table. Therefore gb_trees:get/2 is safe.
	    fun(_Edge, Face, Erec, {V,Ps,_}) ->
		    OPos = wings_vertex:other_pos(V, Erec, Vtab),
		    {FPos,_,_} = gb_trees:get(Face, FacePos),
		    {V,[OPos,FPos|Ps],[]}
	    end;
	false ->
	    fun(Edge, Face, Erec, {V,Ps0,Hard0}) ->
		    OPos = wings_vertex:other_pos(V, Erec, Vtab),
		    FPos = case gb_trees:lookup(Face, FacePos) of
			       none -> none;
			       {value,{Fp,_,_}} -> Fp
			   end,
		    Es = case gb_sets:is_member(Edge, Htab) of
			     true -> [OPos|Hard0];
			     false -> Hard0
			 end,
		    Ps = [FPos,OPos|Ps0],
		    {V,Ps,Es}
	    end
    end.

%% Update the position for the vertex that was created in the middle
%% of each original edge.
update_edge_vs(#we{es=Etab}, FacePos, Hard, Vtab, V) ->
    update_edge_vs_all(gb_trees:to_list(Etab), FacePos, Hard, Vtab, V, []).

update_edge_vs(Es, #we{es=Etab}, FacePos, Hard, Vtab, V) ->
    case gb_trees:size(Etab) of
	N when N =:= length(Es) ->
	    update_edge_vs_all(gb_trees:to_list(Etab), FacePos, Hard, Vtab, V, []);
	_ ->
	    update_edge_vs_some(Es, Etab, FacePos, Hard, Vtab, V, [])
    end.

update_edge_vs_all([{Edge,Rec}|Es], FacePos, Hard, Vtab, V, Acc) ->
    Pos = update_edge_vs_1(Edge, Hard, Rec, FacePos, Vtab),
    update_edge_vs_all(Es, FacePos, Hard, Vtab, V+1, [{V,Pos}|Acc]);
update_edge_vs_all([], _, _, _, V, Acc) ->
    {reverse(Acc),V}.

update_edge_vs_some([E|Es], Etab, FacePos, Hard, Vtab, V, Acc) ->
    Rec = gb_trees:get(E, Etab),
    Pos = update_edge_vs_1(E, Hard, Rec, FacePos, Vtab),
    update_edge_vs_some(Es, Etab, FacePos, Hard, Vtab, V+1, [{V,Pos}|Acc]);
update_edge_vs_some([], _, _, _, _, V, Acc) ->
    {reverse(Acc),V}.

update_edge_vs_1(Edge, Hard, Rec, FacePos, Vtab) ->
    case gb_sets:is_member(Edge, Hard) of
	true ->
	    #edge{vs=Va,ve=Vb} = Rec,
	    e3d_vec:average(gb_trees:get(Va, Vtab), gb_trees:get(Vb, Vtab));
	false ->
	    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = Rec,
	    {LfPos,_,_} = gb_trees:get(Lf, FacePos),
	    {RfPos,_,_} = gb_trees:get(Rf, FacePos),
	    Pos0 = e3d_vec:average(gb_trees:get(Va, Vtab),
				   gb_trees:get(Vb, Vtab),
				   LfPos, RfPos),
	    wings_util:share(Pos0)
    end.

smooth_new_vs(FacePos, V) ->
    smooth_new_vs(FacePos, V, []).
    
smooth_new_vs([{_,{Center,_,NumIds}}|Fs], V, Acc) ->
    smooth_new_vs(Fs, V+NumIds, [{V,Center}|Acc]);
smooth_new_vs([], _, Acc) -> reverse(Acc).

%%%
%%% The Smooth Proxy implementation.
%%%

-record(sp,
	{src_we=#we{},				%Previous source we.
	 we=none,				%Previous smoothed we.
	 plan=none
	}).

quick_preview(_St) ->
    case any_proxy() of
	false ->
	    setup_all(true),
	    wings_wm:set_prop(workmode, false);
	true  ->
	    setup_all(false),
	    wings_wm:set_prop(workmode, true)
    end.

setup(#st{sel=OrigSel}=St) ->
    wings_draw_util:map(fun(D, Sel) -> setup_1(D, Sel) end, OrigSel),
    {save_state,wings_sel:reset(St)}.

setup_1(#dlo{src_we=#we{id=Id},proxy_data=Pd}=D, [{Id,_}|Sel]) ->
    case Pd of
	none ->
	    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
	    Wire = gb_sets:add(Id, Wire0),
	    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
	    {D#dlo{smooth=none,proxy_data=#sp{}},Sel};
	_ ->
	    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
	    Wire = gb_sets:delete_any(Id, Wire0),
	    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
	    {D#dlo{smooth=none,proxy_faces=none,proxy_data=none},Sel}
    end;
setup_1(D, Sel) -> {D,Sel}.

setup_all(Activate) ->
    wings_draw_util:map(fun(D, _) -> setup_all(D, Activate) end, []).

setup_all(#dlo{src_we=#we{perm=P},proxy_data=none}=D, _) when ?IS_NOT_SELECTABLE(P) ->
    D;
setup_all(#dlo{src_we=#we{id=Id},proxy_data=none}=D, true) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:add(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    D#dlo{smooth=none,proxy_data=#sp{}};
setup_all(#dlo{proxy_data=none}=D, false) -> D;
setup_all(#dlo{src_we=#we{id=Id}}=D, false) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:delete_any(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    D#dlo{smooth=none,proxy_faces=none,proxy_data=none};
setup_all(D, _) -> D.

update(#dlo{proxy_data=none}=D, _) -> D;
update(#dlo{proxy_faces=none,src_we=We0,proxy_data=Pd0}=D, St) ->
    Pd1 = clean(Pd0),
    Pd = proxy_smooth(We0, Pd1, St),
    #sp{we=We,plan=Plan} = Pd,
    {Faces,Edges} = draw_faces(Plan, We),
    ProxyEdges = update_edges(D, Pd),
    D#dlo{edges=Edges,proxy_faces=Faces,proxy_edges=ProxyEdges,proxy_data=[Faces,Pd]};
update(#dlo{proxy_edges=none,proxy_data=Pd0}=D, _) ->
    Pd = clean(Pd0),
    ProxyEdges = update_edges(D, Pd),
    D#dlo{proxy_edges=ProxyEdges};
update(D, _) -> D.

update_edges(D, #sp{we=We}) ->
    update_edges_1(D, We, wings_pref:get_value(proxy_shaded_edge_style)).

update_edges_1(_, _, cage) -> none;
update_edges_1(#dlo{src_we=#we{vp=OldVtab}}, #we{vp=Vtab,es=Etab}=We, some) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    Edges = wings_edge:from_vs(gb_trees:keys(OldVtab), We),
    foreach(fun(E) ->
		    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
		    gl:vertex3dv(gb_trees:get(Va, Vtab)),
		    gl:vertex3dv(gb_trees:get(Vb, Vtab))
	    end, Edges),
    gl:'end'(),
    gl:endList(),
    Dl;
update_edges_1(_, #we{es=Etab,vp=Vtab}, all) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    foreach(fun(#edge{vs=Va,ve=Vb}) ->
		    gl:vertex3dv(gb_trees:get(Va, Vtab)),
		    gl:vertex3dv(gb_trees:get(Vb, Vtab))
	    end, gb_trees:values(Etab)),
    gl:'end'(),
    gl:endList(),
    Dl.

smooth_we(#dlo{proxy_data=none,src_we=We}) -> We;
smooth_we(#dlo{src_we=We}) when ?IS_ANY_LIGHT(We) -> We;
smooth_we(#dlo{proxy_data=Pd0,src_we=We0}) ->
    case clean(Pd0) of
	#sp{we=none} -> We0;
	#sp{we=We} -> We
    end.

any_proxy() ->
    wings_draw_util:fold(fun(#dlo{proxy_data=none}, A) -> A;
			    (#dlo{}, _) -> true end, false).

draw(#dlo{proxy_faces=none,proxy_data=[Dl|_]}=D, Wire) ->
    draw_1(D, Dl, Wire, proxy_moving_opacity, cage);
draw(#dlo{proxy_faces=none}, _Wire) -> ok;
draw(#dlo{proxy_faces=Dl}=D, Wire) ->
    draw_1(D, Dl, Wire, proxy_static_opacity, cage).

draw_1(D, Dl, Wire, Key, EdgeStyleKey) ->
    draw_edges(D, Wire, EdgeStyleKey),
    gl:shadeModel(?GL_SMOOTH),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(2, 2),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    case wings_util:is_gl_ext('GL_ARB_imaging') of
	false -> ok;
	true ->
	    case wings_pref:get_value(Key) of
		1.0 -> ok;
		Opacity ->
		    gl:enable(?GL_BLEND),
		    gl:blendFunc(?GL_CONSTANT_ALPHA, ?GL_ONE_MINUS_CONSTANT_ALPHA),
		    gl:blendColor(0, 0, 0, Opacity)
	    end
    end,
    wings_draw_util:call(Dl),
    gl:disable(?GL_BLEND),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT).

draw_smooth_edges(D) ->
    draw_edges(D, true, wings_pref:get_value(proxy_shaded_edge_style)).

draw_edges(_, false, _) -> ok;
draw_edges(D, true, EdgeStyle) -> draw_edges_1(D, EdgeStyle).

draw_edges_1(#dlo{edges=Edges}, cage) ->
    gl:color3fv(wings_pref:get_value(edge_color)),
    gl:lineWidth(1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:enable(?GL_POLYGON_OFFSET_LINE),
    gl:polygonOffset(1, 1),
    gl:disable(?GL_CULL_FACE),
    wings_draw_util:call(Edges),
    gl:enable(?GL_CULL_FACE);
draw_edges_1(#dlo{proxy_edges=ProxyEdges}, _) ->
    gl:color3fv(wings_pref:get_value(edge_color)),
    gl:lineWidth(1),
    wings_draw_util:call(ProxyEdges).

clean([_,#sp{}=Pd]) -> Pd;
clean(Other) -> Other.

proxy_smooth(#we{es=Etab,he=Hard,mat=M,next_id=Next,mirror=Mirror}=We0,
	     #sp{src_we=#we{es=Etab,he=Hard,mat=M,next_id=Next,mirror=Mirror}}=Pd, _St) ->
    We = inc_smooth(We0, Pd),
    Pd#sp{src_we=We0,we=We};
proxy_smooth(We0, Pd, St) ->
    #we{fs=Ftab} = We = if ?IS_ANY_LIGHT(We0) -> We0;
			   true -> smooth(We0) 
			end,
    Plan = wings_draw_util:prepare(gb_trees:to_list(Ftab), We, St),
    Pd#sp{src_we=We0,we=We,plan=Plan}.

inc_smooth(#we{vp=Vp,next_id=Next}=We0, #sp{we=OldWe}) ->
    {Faces,Htab} = smooth_faces_htab(We0),
    FacePos0 = face_centers(Faces, We0),
    FacePos = gb_trees:from_orddict(FacePos0),
    {UpdatedVs,Mid} = update_edge_vs(We0, FacePos, Htab, Vp, Next),
    NewVs = smooth_new_vs(FacePos0, Mid),
    Vtab = smooth_move_orig(gb_trees:keys(Vp), FacePos, Htab, We0, UpdatedVs ++ NewVs),
    OldWe#we{vp=Vtab}.

%%%
%%% Specialized drawing routines that exploits the fact that
%%% a sub-divided surface only can contain quads.
%%%

draw_faces({material,MatFaces,St}, We) ->
    Faces = gl:genLists(1),
    gl:newList(Faces, ?GL_COMPILE),
    mat_faces(MatFaces, We, St),
    gl:endList(),
    {Faces,none};
draw_faces({color,Colors,#st{mat=Mtab}}, We) ->
    BasicFaces = gl:genLists(2),
    Dl = BasicFaces+1,
    gl:newList(BasicFaces, ?GL_COMPILE),
    draw_vtx_faces(Colors, We),
    gl:endList(),
    
    gl:newList(Dl, ?GL_COMPILE),
    wings_material:apply_material(default, Mtab),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    gl:callList(BasicFaces),
    gl:disable(?GL_COLOR_MATERIAL),
    gl:endList(),
    
    Edges = wings_draw_util:force_flat_color(BasicFaces,
					     wings_pref:get_value(edge_color)),
    {{call,Dl,BasicFaces},Edges}.


draw_vtx_faces({Same,Diff}, We) ->
    Draw = fun() ->
		   draw_vtx_faces_1(Same, We),
		   draw_vtx_faces_3(Diff, We)
	   end,
    wings_draw_util:begin_end(?GL_QUADS, Draw).

draw_vtx_faces_1([{none,Faces}|Fs], We) ->
    gl:color3f(1.0, 1.0, 1.0),
    draw_vtx_faces_2(Faces, We),
    draw_vtx_faces_1(Fs, We);
draw_vtx_faces_1([{Col,Faces}|Fs], We) ->
    gl:color3fv(Col),
    draw_vtx_faces_2(Faces, We),
    draw_vtx_faces_1(Fs, We);
draw_vtx_faces_1([], _) -> ok.

draw_vtx_faces_2([F|Fs], We) ->
    mat_face(F, We),
    draw_vtx_faces_2(Fs, We);
draw_vtx_faces_2([], _) -> ok.

draw_vtx_faces_3([[F|Cols]|Fs], We) ->
    vcol_face(F, We, Cols),
    draw_vtx_faces_3(Fs, We);
draw_vtx_faces_3([], _) -> ok.

mat_faces(MatFaces, We, #st{mat=Mtab}) ->
    mat_faces_1(MatFaces, We, Mtab).

mat_faces_1([{Mat,Faces}|T], We, Mtab) ->
    gl:pushAttrib(?GL_TEXTURE_BIT),
    case wings_material:apply_material(Mat, Mtab) of
	false ->
	    Tess = wings_draw_util:tess(),
	    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
	    gl:'begin'(?GL_QUADS),
	    draw_mat_faces(Faces, We),
	    gl:'end'(),
	    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA);
	true ->
	    gl:'begin'(?GL_QUADS),
	    draw_uv_faces(Faces, We),
	    gl:'end'()
    end,
    gl:popAttrib(),
    mat_faces_1(T, We, Mtab);
mat_faces_1([], _, _) -> ok.

draw_mat_faces([{Face,Edge}|Fs], We) ->
    mat_face(Face, Edge, We),
    draw_mat_faces(Fs, We);
draw_mat_faces([], _) -> ok.

draw_uv_faces([{Face,Edge}|Fs], We) ->
    uv_face(Face, Edge, We),
    draw_uv_faces(Fs, We);
draw_uv_faces([], _) -> ok.

mat_face(Face, #we{fs=Ftab}=We) ->
    mat_face(Face, gb_trees:get(Face, Ftab), We).
    
mat_face(Face, Edge, #we{vp=Vtab}=We) ->
    Vs = wings_face:vertices_cw(Face, Edge, We),
    mat_face_1(Vs, Vtab, []).

mat_face_1([V|Vs], Vtab, Acc) ->
    mat_face_1(Vs, Vtab, [gb_trees:get(V, Vtab)|Acc]);
mat_face_1([], _, VsPos) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    case VsPos of
	[A,B,C,D] ->
	    gl:vertex3dv(A),
	    gl:vertex3dv(B),
	    gl:vertex3dv(C),
	    gl:vertex3dv(D);
	_ ->					%Could only be the virtual mirror face.
	    ok
    end.

uv_face(Face, Edge, #we{vp=Vtab}=We) ->
    Vs0 = wings_face:vinfo_cw(Face, Edge, We),
    uv_face_1(Vs0, Vtab, [], []).

uv_face_1([[V|Col]|Vs], Vtab, Nacc, VsAcc) ->
    Pos = gb_trees:get(V, Vtab),
    uv_face_1(Vs, Vtab, [Pos|Nacc], [[Pos|Col]|VsAcc]);
uv_face_1([], _, Nacc, Vs) ->
    N = e3d_vec:normal(Nacc),
    gl:normal3fv(N),
    uv_face_2(Vs).

uv_face_2([[Pos|Attr]|T]) ->
    case Attr of
	{_,_}=UV -> gl:texCoord2fv(UV);
	_ -> gl:texCoord2f(0.0, 0.0)
    end,
    gl:vertex3dv(Pos),
    uv_face_2(T);
uv_face_2([]) -> ok.

vcol_face(Face, We, Cols) ->
    VsPos = wings_face:vertex_positions(Face, We),
    gl:normal3fv(e3d_vec:normal(VsPos)),
    vcol_face_1(VsPos, Cols).

vcol_face_1([P|Ps], [{_,_,_}=Col|Cols]) ->
    gl:color3fv(Col),    
    gl:vertex3dv(P),
    vcol_face_1(Ps, Cols);
vcol_face_1([P|Ps], [_|Cols]) ->
    gl:color3f(1.0, 1.0, 1.0),
    gl:vertex3dv(P),
    vcol_face_1(Ps, Cols);
vcol_face_1([], []) -> ok.
