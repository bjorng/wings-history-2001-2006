%%
%%  wings_subdiv.erl --
%%
%%     This module implements the Smooth command for faces.
%%     (Currently, there is similar code in the wings_body.erl
%%     for smoothing an entire object.)
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_subdiv.erl,v 1.2 2001/09/03 11:01:39 bjorng Exp $
%%

-module(wings_subdiv).
-export([smooth/1,smooth/6]).
-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1]).

%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.

smooth(#we{vs=Vtab,es=Etab,fs=Ftab,he=Htab}=We) ->
    Faces = gb_trees:keys(Ftab),
    Vs = gb_trees:keys(Vtab),
    Es = gb_trees:keys(Etab),
    wings_subdiv:smooth(Faces, Faces, Vs, Es, Htab, We).

smooth(AllFs, Fs, Vs, Es, Htab,
       #we{es=Etab,fs=Ftab0,vs=Vtab0,next_id=Id}=We0) ->
    FacePos0 = face_centers(AllFs, We0),
    FacePos = gb_trees:from_orddict(reverse(FacePos0)),
    We1 = cut_edges(Es, FacePos, Htab, We0),
    We = foldl(fun(Face, Acc) ->
		       smooth_face(Face, Id, FacePos, Acc)
	       end, We1, Fs),
    #we{vs=Vtab2} = We,
    Vtab = smooth_move_orig(Vs, FacePos, Htab, We0, Vtab2),
    We#we{vs=Vtab}.

face_centers(Faces, #we{fs=Ftab}=We) ->
    face_centers(Faces, We, []).

face_centers([Face|Fs], We, Acc) ->
    Vs = wings_face:surrounding_vertices(Face, We),
    Center = wings_util:share(wings_vertex:center(Vs, We)),
    face_centers(Fs, We, [{Face,[Center|length(Vs)]}|Acc]);
face_centers([], We, Acc) -> Acc.

smooth_move_orig([V|Vs], FacePos, Htab, We, Vtab0) ->
    Vtab = smooth_move_orig_1(V, FacePos, Htab, We, Vtab0),
    smooth_move_orig(Vs, FacePos, Htab, We, Vtab);
smooth_move_orig([], FacePos, Htab, We, Vtab) -> Vtab.

smooth_move_orig_1(V, FacePosTab, Htab, #we{es=Etab,vs=OVtab}=We, Vtab) ->
    {Ps0,Hard} =
	wings_vertex:fold(
	  fun (Edge, Face, Erec, {Ps0,Hard0}) ->
		  OPos = wings_vertex:other_pos(V, Erec, OVtab),
		  FPos = case gb_trees:lookup(Face, FacePosTab) of
			     none -> none;
			     {value,[Fp|_]} -> Fp
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
	    N = float(length(Ps0) bsr 1),
	    Pos0 = e3d_vec:add(e3d_vec:divide(Ps, (N*N)),
				 e3d_vec:mul(S, (N-2.0)/N)),
	    Pos = wings_util:share(Pos0),
	    gb_trees:update(V, Vrec#vtx{pos=Pos}, Vtab);
	NumHard when NumHard =:= 2 ->
	    Pos0 = e3d_vec:add([e3d_vec:mul(S, 6.0)|Hard]),
	    Pos1 = e3d_vec:mul(Pos0, 1/8),
	    Pos = wings_util:share(Pos1),
	    gb_trees:update(V, Vrec#vtx{pos=Pos}, Vtab);
	ThreeOrMore -> Vtab
    end.

smooth_face(Face, Id, FacePos, #we{es=Etab0,fs=Ftab0,vs=Vtab0}=We0) ->
    [Center|NumIds] = gb_trees:get(Face, FacePos),
    {NewV,We1} = wings_we:new_id(We0),
    {Ids,We} = wings_we:new_wrap_range(NumIds, 2, We0),
    #face{mat=Mat} = gb_trees:get(Face, Ftab0),
    {Etab,Ftab1,_} = wings_face:fold(
		       fun(_, E, Rec, A) ->
			       smooth_edge(Face, E, Rec, NewV, Id, Mat, A)
		       end, {Etab0,Ftab0,Ids}, Face, We),
    Ftab = gb_trees:delete(Face, Ftab1),
    AnEdge = wings_we:id(0, Ids),
    Vtab = gb_trees:insert(NewV, #vtx{pos=Center,edge=AnEdge}, Vtab0),
    We#we{es=Etab,fs=Ftab,vs=Vtab}.

smooth_edge(Face, Edge, Rec0, NewV, Id, Mat, {Etab0,Ftab0,Ids0}) ->
    LeftEdge = wings_we:id(0, Ids0),
    RFace = wings_we:id(1, Ids0),
    NewEdge = wings_we:id(2, Ids0),
    LFace = wings_we:id(3, Ids0),
    RightEdge = wings_we:id(4, Ids0),
    case Rec0 of
	#edge{ve=Vtx,rf=Face} when Vtx >= Id ->
	    Ids = Ids0,
	    Ftab = gb_trees:insert(RFace, #face{edge=NewEdge,mat=Mat}, Ftab0),
	    Rec = Rec0#edge{rf=RFace,rtsu=NewEdge},
	    NewErec0 = get_edge(NewEdge, Etab0),
	    NewErec = NewErec0#edge{vs=Vtx,ve=NewV,rf=RFace,lf=LFace,
				    rtpr=Edge,rtsu=LeftEdge};
	#edge{vs=Vtx,lf=Face} when Vtx >= Id ->
	    Ids = Ids0,
	    Ftab = gb_trees:insert(RFace, #face{edge=NewEdge,mat=Mat}, Ftab0),
	    Rec = Rec0#edge{lf=RFace,ltsu=NewEdge},
	    NewErec0 = get_edge(NewEdge, Etab0),
	    NewErec = NewErec0#edge{vs=Vtx,ve=NewV,rf=RFace,lf=LFace,
				    rtpr=Edge,rtsu=LeftEdge};
 	#edge{vs=Vtx,rf=Face} when Vtx >= Id ->
	    Ids = wings_we:bump_id(Ids0),
	    Ftab = Ftab0,
	    Rec = Rec0#edge{rf=LFace,rtpr=NewEdge},
	    NewErec0 = get_edge(NewEdge, Etab0),
 	    NewErec = NewErec0#edge{ltpr=RightEdge,ltsu=Edge};
 	#edge{ve=Vtx,lf=Face} when Vtx >= Id ->
	    Ids = wings_we:bump_id(Ids0),
	    Ftab = Ftab0,
	    Rec = Rec0#edge{lf=LFace,ltpr=NewEdge},
	    NewErec0 = get_edge(NewEdge, Etab0),
 	    NewErec = NewErec0#edge{ltpr=RightEdge,ltsu=Edge}
    end,
    Etab1 = gb_trees:update(Edge, Rec, Etab0),
    Etab = gb_trees:enter(NewEdge, NewErec, Etab1),
    {Etab,Ftab,Ids}.

get_edge(Edge, Etab) ->
    case gb_trees:lookup(Edge, Etab) of
	{value,Erec} -> Erec;
	none -> #edge{}
    end.

cut_edges(Es0, FacePos, Htab, #we{es=Etab}=We) ->
    Es = [{Edge,gb_trees:get(Edge, Etab)} || Edge <- Es0],
    cut_edges_1(Es, FacePos, Htab, We).

cut_edges_1([{Edge,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}}|Es], FacePos, Htab,
	    #we{vs=Vtab}=We0) ->
    case gb_sets:is_member(Edge, Htab) of
	true ->
	    {We,_,_} = wings_edge:fast_cut(Edge, default, We0),
	    cut_edges_1(Es, FacePos, Htab, We);
	false ->
	    [LfPos|_] = gb_trees:get(Lf, FacePos),
	    [RfPos|_] = gb_trees:get(Rf, FacePos),
	    VaPos = wings_vertex:pos(Va, Vtab),
	    VbPos = wings_vertex:pos(Vb, Vtab),
	    Pos = e3d_vec:average([LfPos,RfPos,VaPos,VbPos]),
	    {We,_,_} = wings_edge:fast_cut(Edge, Pos, We0),
	    cut_edges_1(Es, FacePos, Htab, We)
    end;
cut_edges_1([], FacePos, Htab, We) -> We.
