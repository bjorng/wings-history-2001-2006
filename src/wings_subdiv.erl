%%
%%  wings_subdiv.erl --
%%
%%     This module implements the Smooth command for objects and faces.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_subdiv.erl,v 1.38 2003/05/31 15:29:22 bjorng Exp $
%%

-module(wings_subdiv).
-export([smooth/1,smooth/5]).
-export([setup/1,update/2,draw/1,clean/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,merge/1]).

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
    FacePos0 = face_centers(Fs, We0),
    FacePos = gb_trees:from_orddict(FacePos0),

    %% First do all topological changes to the edge table.
    We1 = cut_edges(Es, FacePos, Htab, We0#we{vc=undefined}),
    We = smooth_faces(Fs, FacePos0, Id, We1),

    %% Now calculate all vertex positions.
    {UpdatedVs,Mid} = update_edge_vs(Es, We0, FacePos, Htab, Vp, Id),
    NewVs = smooth_new_vs(FacePos0, Mid),
    Vtab = smooth_move_orig(Vs, FacePos, Htab, We0, UpdatedVs ++ NewVs),
    wings_util:validate_mirror(wings_we:rebuild(We#we{vp=Vtab})).

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

cut_edges(Es, FacePos, Hard, #we{es=Etab0,he=Htab0,next_id=Id0}=We) ->
    Etab1 = {Id0,Etab0,gb_trees:empty()},
    {Id,{_,Etab2,Etab3},Htab} =
	cut_edges_1(Es, FacePos, Hard, Id0, Etab1, Htab0),
    Etab = gb_trees:from_orddict(gb_trees:to_list(Etab2) ++
				 gb_trees:to_list(Etab3)),
    We#we{es=Etab,he=Htab,next_id=Id}.

cut_edges_1([Edge|Es], FacePos, Hard, NewEdge, Etab0, Htab0) ->
    Rec = edge_get(Edge, Etab0),
    Etab = fast_cut(Edge, Rec, NewEdge, Etab0),
    case gb_sets:is_member(Edge, Hard) of
	true ->
	    Htab = case gb_sets:is_member(Edge, Htab0) of
		       true -> gb_sets:insert(NewEdge, Htab0);
		       false -> Htab0
		   end,
	    cut_edges_1(Es, FacePos, Hard, NewEdge+1, Etab, Htab);
	false ->
	    cut_edges_1(Es, FacePos, Hard, NewEdge+1, Etab, Htab0)
    end;
cut_edges_1([], _FacePos, _Hard, Id, Etab, Htab) ->
    {Id,Etab,Htab}.

fast_cut(Edge, Template, NewV=NewEdge, Etab0) ->
    #edge{a=ACol,b=BCol,lf=Lf,rf=Rf,
	  ltpr=EdgeA,rtsu=EdgeB,rtpr=NextBCol} = Template,
    AColOther = get_vtx_color(EdgeA, Lf, Etab0),
    NewColA = wings_color:mix(0.5, ACol, AColOther),
    BColOther = get_vtx_color(NextBCol, Rf, Etab0),
    NewColB = wings_color:mix(0.5, BCol, BColOther),

    {Id,EtabA0,EtabB0} = Etab0,
    NewEdgeRec = Template#edge{vs=NewV,a=NewColA,ltsu=Edge,rtpr=Edge},
    EtabB = gb_trees:insert(NewEdge, NewEdgeRec, EtabB0),
    EdgeRec = Template#edge{ve=NewV,b=NewColB,rtsu=NewEdge,ltpr=NewEdge},
    EtabA = gb_trees:update(Edge, EdgeRec, EtabA0),
    Etab = patch_edge(EdgeA, NewEdge, Edge, {Id,EtabA,EtabB}),
    patch_edge(EdgeB, NewEdge, Edge, Etab).

get_vtx_color(Edge, Face, Etab) ->
    case edge_get(Edge, Etab) of
	#edge{lf=Face,a=Col} -> Col;
	#edge{rf=Face,b=Col} -> Col
    end.

patch_edge(Edge, ToEdge, OrigEdge, {Id,EtabA,EtabB}) when Edge < Id ->
    New = case gb_trees:get(Edge, EtabA) of
	      #edge{ltsu=OrigEdge}=R ->
		  R#edge{ltsu=ToEdge};
	      #edge{ltpr=OrigEdge}=R ->
		  R#edge{ltpr=ToEdge};
	      #edge{rtsu=OrigEdge}=R ->
		  R#edge{rtsu=ToEdge};
	      #edge{rtpr=OrigEdge}=R ->
		  R#edge{rtpr=ToEdge}
	  end,
    {Id,gb_trees:update(Edge, New, EtabA),EtabB};
patch_edge(Edge, ToEdge, OrigEdge, {Id,EtabA,EtabB}) ->
    New = case gb_trees:get(Edge, EtabB) of
	      #edge{ltsu=OrigEdge}=R ->
		  R#edge{ltsu=ToEdge};
	      #edge{ltpr=OrigEdge}=R ->
		  R#edge{ltpr=ToEdge};
	      #edge{rtsu=OrigEdge}=R ->
		  R#edge{rtsu=ToEdge};
	      #edge{rtpr=OrigEdge}=R ->
		  R#edge{rtpr=ToEdge}
	  end,
    {Id,EtabA,gb_trees:update(Edge, New, EtabB)}.

edge_get(Edge, {Id,Etab,_}) when Edge < Id ->
    gb_trees:get(Edge, Etab);
edge_get(Edge, {_,_,Etab}) ->
    gb_trees:get(Edge, Etab).

smooth_faces(Fs, FacePos, Id, We0) ->
    We = smooth_materials(Fs, FacePos, We0),
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
	    Pos = e3d_vec:add(e3d_vec:mul(Ps, A), e3d_vec:mul(S, B)),
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
	 we=none				%Previous smoothed we.
	}).

setup(#st{sel=OrigSel}=St) ->
    wings_draw_util:map(fun(D, Sel) -> setup_1(D, Sel) end, OrigSel),
    {save_state,wings_sel:reset(St)}.

setup_1(#dlo{src_we=#we{id=Id}}=D, [{Id,_}|Sel]) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:add(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    {D#dlo{proxy_data=#sp{}},Sel};
setup_1(D, Sel) -> {D,Sel}.

update(#dlo{proxy_data=none}=D, _) -> D;
update(#dlo{smooth_proxy=none,src_we=We0,proxy_data=Pd0}=D, St) ->
    Pd1 = clean(Pd0),
    Pd = proxy_smooth(We0, Pd1),
    #sp{we=#we{fs=Ftab}=We} = Pd,
    Faces = gl:genLists(1),
    gl:newList(Faces, ?GL_COMPILE),
    wings_draw:draw_faces(gb_trees:to_list(Ftab), We, St),
    gl:endList(),
    D#dlo{smooth_proxy=Faces,proxy_data=[Faces,Pd]};
update(D, _) -> D.

draw(#dlo{smooth_proxy=Dl}) when is_integer(Dl) ->
    draw_1(Dl);
draw(#dlo{proxy_data=[Dl|_]}) when is_integer(Dl) ->
    draw_1(Dl);
draw(_) -> ok.

draw_1(Dl) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(2.0, 2.0),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_draw_util:call(Dl),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    gl:disable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT).

clean([_,#sp{}=Pd]) -> Pd;
clean(Other) -> Other.

proxy_smooth(#we{es=Etab,he=Hard}=We0, #sp{src_we=#we{es=Etab,he=Hard}}=Pd) ->
    We = inc_smooth(We0, Pd),
    Pd#sp{src_we=We0,we=We};
proxy_smooth(We0, Pd) ->
    We = smooth(We0),
    Pd#sp{src_we=We0,we=We}.
    
inc_smooth(#we{vp=Vp,next_id=Next}=We0, #sp{we=OldWe}) ->
    {Faces,Htab} = smooth_faces_htab(We0),
    FacePos0 = face_centers(Faces, We0),
    FacePos = gb_trees:from_orddict(FacePos0),
    {UpdatedVs,Mid} = update_edge_vs(We0, FacePos, Htab, Vp, Next),
    NewVs = smooth_new_vs(FacePos0, Mid),
    Vtab = smooth_move_orig(gb_trees:keys(Vp), FacePos, Htab, We0, UpdatedVs ++ NewVs),
    OldWe#we{vp=Vtab}.
