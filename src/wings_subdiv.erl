%%
%%  wings_subdiv.erl --
%%
%%     This module implements the Smooth command for objects and faces.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_subdiv.erl,v 1.20 2002/04/23 05:31:06 bjorng Exp $
%%

-module(wings_subdiv).
-export([smooth/1,smooth/6]).
-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,merge/1]).

%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.

smooth(#we{vs=Vtab,es=Etab,fs=Ftab,he=Htab}=We) ->
    Faces = gb_trees:keys(Ftab),
    Vs = gb_trees:keys(Vtab),
    Es = gb_trees:keys(Etab),
    smooth(Faces, Faces, Vs, Es, Htab, We).

smooth(AllFs, Fs, Vs, Es, Htab, #we{next_id=Id}=We0) ->
    wings_io:progress_tick(),
    FacePos0 = face_centers(AllFs, We0),
    FacePos = gb_trees:from_orddict(reverse(FacePos0)),
    wings_io:progress_tick(),
    We1 = cut_edges(Es, FacePos, Htab, We0),
    wings_io:progress_tick(),
    {We,NewVs} = smooth_faces(Fs, Id, FacePos, We1),
    wings_io:progress_tick(),
    #we{vs=Vtab2} = We,
    Vtab3 = smooth_move_orig(Vs, FacePos, Htab, We0, Vtab2),
    Vtab = gb_trees:from_orddict(gb_trees:to_list(Vtab3) ++ reverse(NewVs)),
    wings_io:progress_tick(),
    We#we{vs=Vtab}.

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
	    throw({command_error,"Face " ++ integer_to_list(Face) ++
		   " has only two edges."});
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

smooth_move_orig_1(V, FacePosTab, Htab, #we{vs=OVtab}=We, Vtab) ->
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

    #vtx{pos=S} = Vrec = gb_trees:get(V, Vtab),
    case length(Hard) of
	NumHard when NumHard < 2 ->
	    Ps = e3d_vec:add(Ps0),
	    N = length(Ps0) bsr 1,
	    Pos0 = e3d_vec:add(e3d_vec:mul(Ps, 1/(N*N)),
			       e3d_vec:mul(S, (N-2.0)/N)),
	    Pos = wings_util:share(Pos0),
	    gb_trees:update(V, Vrec#vtx{pos=Pos}, Vtab);
	NumHard when NumHard =:= 2 ->
	    Pos0 = e3d_vec:add([e3d_vec:mul(S, 6.0)|Hard]),
	    Pos1 = e3d_vec:mul(Pos0, 1/8),
	    Pos = wings_util:share(Pos1),
	    gb_trees:update(V, Vrec#vtx{pos=Pos}, Vtab);
	_ThreeOrMore -> Vtab
    end.

smooth_faces(Faces0, Id, FacePos, #we{fs=Ftab0}=We) ->
    {Faces,FaceAcc} =
	case {length(Faces0),gb_trees:size(Ftab0)} of
	    {Same,Same} ->
		{gb_trees:to_list(Ftab0),[]};
	    {_,_} ->
		Ftab1 = gb_trees:to_list(Ftab0),
		Ftab2 = sofs:relation(Ftab1, [{face,data}]),
		FaceSet = sofs:set(Faces0, [face]),
		Ftab = sofs:drestriction(Ftab2, FaceSet),
		Faces1 = sofs:restriction(Ftab2, FaceSet),
		{sofs:to_external(Faces1),sofs:to_external(Ftab)}
	end,
    smooth_faces(Faces, Id, FacePos, [], FaceAcc, [], We).
    
smooth_faces([{Face,#face{mat=Mat}}|Faces], Id, FacePos,
	     EsAcc0, Ftab0, Vtab0, #we{es=Etab0}=We0) ->
    {NewV,We1} = wings_we:new_id(We0),
    {Center,Color,NumIds} = gb_trees:get(Face, FacePos),
    {Ids,We} = wings_we:new_wrap_range(NumIds, 2, We1),
    AnEdge = wings_we:id(0, Ids),
    Vtab = [{NewV,#vtx{pos=Center,edge=AnEdge}}|Vtab0],
    {Etab,EsAcc,Ftab,_} =
	wings_face:fold(
	  fun(_, E, Rec, A) ->
		  smooth_edge(Face, E, Rec, NewV,
			      Color, Id, Mat, A)
	  end, {Etab0,EsAcc0,Ftab0,Ids}, Face, We),
    smooth_faces(Faces, Id, FacePos, EsAcc, Ftab, Vtab, We#we{es=Etab});
smooth_faces([], _, _, Es, Ftab0, NewVs, #we{es=Etab0}=We) ->
    Etab1 = gb_trees:to_list(Etab0) ++ reverse(Es),
    Etab = gb_trees:from_orddict(Etab1),
    Ftab = gb_trees:from_orddict(sort(Ftab0)),
    {We#we{es=Etab,fs=Ftab},NewVs}.

smooth_edge(Face, Edge, Rec0, NewV, Color, Id, Mat,
	    {Etab0,Es0,Ftab0,Ids0}) ->
    LeftEdge = wings_we:id(0, Ids0),
    RFace = wings_we:id(1, Ids0),
    NewEdge = wings_we:id(2, Ids0),
    LFace = wings_we:id(3, Ids0),
    RightEdge = wings_we:id(4, Ids0),
    case Rec0 of
	#edge{ve=Vtx,b=OldCol,rf=Face} when Vtx >= Id ->
	    Ids = Ids0,
	    Ftab = [{RFace,#face{edge=NewEdge,mat=Mat}}|Ftab0],
	    Rec = Rec0#edge{rf=RFace,rtsu=NewEdge},
	    NewErec0 = get_edge(NewEdge, Es0),
	    NewErec = NewErec0#edge{vs=Vtx,a=OldCol,ve=NewV,b=Color,
				    rf=RFace,lf=LFace,
				    rtpr=Edge,rtsu=LeftEdge};
	#edge{vs=Vtx,a=OldCol,lf=Face} when Vtx >= Id ->
	    Ids = Ids0,
	    Ftab = [{RFace,#face{edge=NewEdge,mat=Mat}}|Ftab0],
	    Rec = Rec0#edge{lf=RFace,ltsu=NewEdge},
	    NewErec0 = get_edge(NewEdge, Es0),
	    NewErec = NewErec0#edge{vs=Vtx,a=OldCol,ve=NewV,b=Color,
				    rf=RFace,lf=LFace,
				    rtpr=Edge,rtsu=LeftEdge};
 	#edge{vs=Vtx,rf=Face} when Vtx >= Id ->
	    Ids = wings_we:bump_id(Ids0),
	    Ftab = Ftab0,
	    Rec = Rec0#edge{rf=LFace,rtpr=NewEdge},
	    NewErec0 = get_edge(NewEdge, Es0),
 	    NewErec = NewErec0#edge{ltpr=RightEdge,ltsu=Edge};
 	#edge{ve=Vtx,lf=Face} when Vtx >= Id ->
	    Ids = wings_we:bump_id(Ids0),
	    Ftab = Ftab0,
	    Rec = Rec0#edge{lf=LFace,ltpr=NewEdge},
	    NewErec0 = get_edge(NewEdge, Es0),
 	    NewErec = NewErec0#edge{ltpr=RightEdge,ltsu=Edge}
    end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    Es = store(NewEdge, NewErec, Es0),
    {Etab,Es,Ftab,Ids}.

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

%%
%% Cut edges.
%%

cut_edges(Es, FacePos, Hard,
	  #we{mode=Mode,es=Etab0,vs=Vtab0,he=Htab0,next_id=Id0}=We) ->
    Etab1 = {Id0,Etab0,gb_trees:empty()},
    {Id,Vtab,{_,Etab2,Etab3},Htab} =
	cut_edges_1(Es, FacePos, Mode, Hard, Id0, Etab1, Vtab0, Htab0, []),
    Etab = gb_trees:from_orddict(gb_trees:to_list(Etab2) ++
				 gb_trees:to_list(Etab3)),
    We#we{vs=Vtab,es=Etab,he=Htab,next_id=Id}.

cut_edges_1([Edge|Es], FacePos, Mode, Hard, 
	    NewEdge, Etab0, Vtab0, Htab0, VsAcc0) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = Rec = edge_get(Edge, Etab0),
    Pos0 = [wings_vertex:pos(Va, Vtab0),wings_vertex:pos(Vb, Vtab0)],
    {Pos1,Htab} =
	case gb_sets:is_member(Edge, Hard) of
	    true ->
		{Pos0,case gb_sets:is_member(Edge, Htab0) of
			  true -> gb_sets:insert(NewEdge, Htab0);
			  false -> Htab0
		      end};
	    false ->
		{LfPos,_,_} = gb_trees:get(Lf, FacePos),
		{RfPos,_,_} = gb_trees:get(Rf, FacePos),
		{[LfPos,RfPos|Pos0],Htab0}
	end,
    Pos = wings_util:share(e3d_vec:average(Pos1)),
    Vtx = #vtx{pos=Pos,edge=NewEdge},
    VsAcc = [{NewEdge,Vtx}|VsAcc0],
    Vtab = case gb_trees:get(Vb, Vtab0) of
	       #vtx{edge=Edge}=VbRec ->
		   gb_trees:update(Vb, VbRec#vtx{edge=NewEdge}, Vtab0);
	       _ ->
		   Vtab0
	   end,
    Etab = fast_cut(Edge, Rec, Mode, NewEdge, Etab0),
    cut_edges_1(Es, FacePos, Mode, Hard, NewEdge+1, Etab, Vtab, Htab, VsAcc);
cut_edges_1([], _FacePos, _Mode, _Hard, Id, Etab, Vtab0, Htab, VsAcc) ->
    Vtab = gb_trees:from_orddict(gb_trees:to_list(Vtab0) ++
				 reverse(VsAcc)),
    {Id,Vtab,Etab,Htab}.

fast_cut(Edge, Template, Mode, NewV=NewEdge, Etab0) ->
    #edge{ltpr=EdgeA,rtsu=EdgeB} = Template,

    %% Here we handle vertex colors.
    case Mode of
	material ->
	    NewColA = NewColB = wings_color:white();
	_Other ->
	    #edge{a=ACol,b=BCol,lf=Lf,rf=Rf,rtpr=NextBCol} = Template,
	    AColOther = get_vtx_color(EdgeA, Lf, Etab0),
	    NewColA = wings_color:mix(0.5, ACol, AColOther),
	    BColOther = get_vtx_color(NextBCol, Rf, Etab0),
	    NewColB = wings_color:mix(0.5, BCol, BColOther)
    end,

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
