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
%%     $Id: wings_subdiv.erl,v 1.32 2003/05/30 08:36:10 bjorng Exp $
%%

-module(wings_subdiv).
-export([smooth/1,smooth/5]).
-export([setup/1,update/2,draw/1,clean/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,merge/1]).

%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.

smooth(#we{mirror=none,vp=Vtab,es=Etab,fs=Ftab,he=Htab}=We) ->
    Faces = gb_trees:keys(Ftab),
    Vs = gb_trees:keys(Vtab),
    Es = gb_trees:keys(Etab),
    smooth(Faces, Vs, Es, Htab, We);
smooth(#we{mirror=Face,vp=Vtab,es=Etab,fs=Ftab,he=Htab}=We) ->
    Faces = gb_trees:keys(gb_trees:delete(Face, Ftab)),
    Vs = gb_trees:keys(Vtab),
    Es = gb_trees:keys(Etab),
    He0 = wings_face:outer_edges([Face], We),
    He = gb_sets:union(gb_sets:from_list(He0), Htab),
    smooth(Faces, Vs, Es, He, We).

smooth(Fs, Vs, Es, Htab, #we{next_id=Id}=We0) ->
    FacePos0 = reverse(face_centers(Fs, We0)),
    FacePos = gb_trees:from_orddict(FacePos0),
    We1 = cut_edges(Es, FacePos, Htab, We0#we{vc=undefined}),
    {We,NewVs} = smooth_faces(Fs, FacePos0, Id, We1),
    #we{vp=Vtab2} = We,
    Vtab3 = smooth_move_orig(Vs, FacePos, Htab, We0, Vtab2),
    Vtab = gb_trees:from_orddict(gb_trees:to_list(Vtab3) ++ NewVs),
    wings_util:validate_mirror(wings_we:rebuild(We#we{vp=Vtab})).

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
face_centers([], _We, Acc) -> Acc.

smooth_move_orig([V|Vs], FacePos, Htab, We, Vtab0) ->
    Vtab = smooth_move_orig_1(V, FacePos, Htab, We, Vtab0),
    smooth_move_orig(Vs, FacePos, Htab, We, Vtab);
smooth_move_orig([], _FacePos, _Htab, _We, Vtab) -> Vtab.

smooth_move_orig_1(V, FacePosTab, Htab, #we{vp=OVtab}=We, Vtab) ->
    {Ps0,Hard} =
	wings_vertex:fold(
	  fun (Edge, Face, Erec, {Ps0,Hard0}) ->
		  OPos = wings_vertex:other_pos(V, Erec, OVtab),
		  FPos = case gb_trees:lookup(Face, FacePosTab) of
			     none -> none;
			     {value,{Fp,_,_}} -> Fp
			 end,
		  Ps = [FPos,OPos|Ps0],
		  Es = case gb_sets:is_member(Edge, Htab) of
			   true -> [OPos|Hard0];
			   false -> Hard0
		       end,
		  {Ps,Es}
	  end, {[],[]}, V, We),

    S = gb_trees:get(V, Vtab),
    case length(Hard) of
	NumHard when NumHard < 2 ->
	    Ps = e3d_vec:add(Ps0),
	    N = length(Ps0) bsr 1,
	    Pos0 = e3d_vec:add(e3d_vec:mul(Ps, 1/(N*N)),
			       e3d_vec:mul(S, (N-2.0)/N)),
	    Pos = wings_util:share(Pos0),
	    gb_trees:update(V, Pos, Vtab);
	NumHard when NumHard =:= 2 ->
	    Pos0 = e3d_vec:add([e3d_vec:mul(S, 6.0)|Hard]),
	    Pos1 = e3d_vec:mul(Pos0, 1/8),
	    Pos = wings_util:share(Pos1),
	    gb_trees:update(V, Pos, Vtab);
	_ThreeOrMore -> Vtab
    end.

smooth_faces(Fs, FacePos, Id, #we{next_id=NextId}=We0) ->
    We1 = smooth_materials(Fs, FacePos, We0),
    NewVs = smooth_new_vs(FacePos, NextId, []),
    We = smooth_faces_1(FacePos, Id, [], We1),
    {We,NewVs}.

smooth_new_vs([{_,{Center,_,NumIds}}|Fs], V, Acc) ->
    smooth_new_vs(Fs, V+NumIds, [{V,Center}|Acc]);
smooth_new_vs([], _, Acc) -> reverse(Acc).

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
%%% Cut edges.
%%%

cut_edges(Es, FacePos, Hard, #we{es=Etab0,vp=Vtab0,
				 he=Htab0,next_id=Id0}=We) ->
    Etab1 = {Id0,Etab0,gb_trees:empty()},
    {Id,Vtab,{_,Etab2,Etab3},Htab} =
	cut_edges_1(Es, FacePos, Hard, Id0, Etab1, Vtab0, Htab0, []),
    Etab = gb_trees:from_orddict(gb_trees:to_list(Etab2) ++
				 gb_trees:to_list(Etab3)),
    We#we{vp=Vtab,es=Etab,he=Htab,next_id=Id}.

cut_edges_1([Edge|Es], FacePos, Hard, 
	    NewEdge, Etab0, Vtab, Htab0, VsAcc0) ->
    Rec = edge_get(Edge, Etab0),
    Etab = fast_cut(Edge, Rec, NewEdge, Etab0),
    case gb_sets:is_member(Edge, Hard) of
	true ->
	    Htab = case gb_sets:is_member(Edge, Htab0) of
		       true -> gb_sets:insert(NewEdge, Htab0);
		       false -> Htab0
		   end,
	    #edge{vs=Va,ve=Vb} = Rec,
	    Pos = e3d_vec:average(gb_trees:get(Va, Vtab), gb_trees:get(Vb, Vtab)),
	    VsAcc = [{NewEdge,Pos}|VsAcc0],
	    cut_edges_1(Es, FacePos, Hard, NewEdge+1, Etab, Vtab, Htab, VsAcc);
	false ->
	    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = Rec,
	    {LfPos,_,_} = gb_trees:get(Lf, FacePos),
	    {RfPos,_,_} = gb_trees:get(Rf, FacePos),
	    Pos0 = e3d_vec:average(gb_trees:get(Va, Vtab),
				   gb_trees:get(Vb, Vtab),
				   LfPos, RfPos),
	    Pos = wings_util:share(Pos0),
	    VsAcc = [{NewEdge,Pos}|VsAcc0],
	    cut_edges_1(Es, FacePos, Hard, NewEdge+1, Etab, Vtab, Htab0, VsAcc)
    end;
cut_edges_1([], _FacePos, _Hard, Id, Etab, Vtab0, Htab, VsAcc) ->
    Vtab = gb_trees:from_orddict(gb_trees:to_list(Vtab0) ++
				 reverse(VsAcc)),
    {Id,Vtab,Etab,Htab}.

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

%%%
%%% The Smooth Proxy implmentation.
%%%

-record(sp,
	{
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

update(#dlo{src_we=We0}=D, St) ->
    #we{fs=Ftab} = We = wings_subdiv:smooth(We0),
    Faces = gl:genLists(1),
    gl:newList(Faces, ?GL_COMPILE),
    wings_draw:draw_faces(gb_trees:to_list(Ftab), We, St),
    gl:endList(),
    D#dlo{smooth_proxy=Faces,proxy_data=[Faces,#sp{}]}.

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
