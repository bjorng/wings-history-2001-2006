%%
%%  wings_body.erl --
%%
%%     This module contains most of the command for entire Wings objects.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_body.erl,v 1.1 2001/08/14 18:16:35 bjorng Exp $
%%

-module(wings_body).
-export([convert_selection/1,cleanup/1,
	 invert_normals/1,flip/2,duplicate/2,delete/1,
	 tighten/1,smooth/1,combine/1,separate/1,auto_smooth/1]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1]).

%%
%% Convert the current selection to a body selection.
%%
convert_selection(St) ->
    St#st{selmode=body,sel=[]}.

%%%
%%% The Cleanup command.
%%%

cleanup(St) ->
    wings_sel:map(fun cleanup_1/1, St).

cleanup_1(#shape{sh=#we{}=We0}=Sh) ->
    We1 = clean_winged_vertices(We0),
    We = clean_short_edges(We1),
    Sh#shape{sh=We};
cleanup_1(Other) -> Other.

clean_winged_vertices(We) ->
    wings_util:fold_vertex(
      fun(V, _, W) ->
	      case wings_vertex:dissolve(V, W) of
		  error -> W;
		  Other -> Other
	      end
      end, We, We).

clean_short_edges(#we{es=Etab,vs=Vtab}=We) ->
    Short = foldl(
	      fun({Edge,#edge{vs=Va,ve=Vb}}, A) ->
		      VaPos = wings_vertex:pos(Va, Vtab),
		      VbPos = wings_vertex:pos(Vb, Vtab),
		      case abs(e3d_vec:dist(VaPos, VbPos)) of
			  Dist when Dist < 1.0E-3 -> [Edge|A];
			  Dist -> A
		      end
	      end, [], gb_trees:to_list(Etab)),
    foldl(fun(Edge, #we{es=Et}=W) ->
		  case gb_trees:is_defined(Edge, Et) of
		      true -> wings_collapse:collapse_edge(Edge, W);
		      false -> W
		  end
	  end, We, Short).

%%%
%%% The Invert command.
%%%

invert_normals(St0) ->
    wings_sel:map(fun invert_normals_1/1, St0).

invert_normals_1(#shape{sh=#we{}=We0}=Sh0) ->
    We = wings_we:invert_normals(We0),
    Sh0#shape{sh=We}.

%%%
%%% The Duplicate command.
%%%

duplicate(Dir, #st{onext=Id0,shapes=Shapes0}=St) ->
    {Shapes,Id} = wings_sel:fold(
		    fun(#shape{name=Name0}=Sh0, {Shs,I}) ->
			    Name = new_name(reverse(Name0), I),
			    Sh = Sh0#shape{id=I,name=Name},
			    {gb_trees:insert(I, Sh, Shs),I+1}
		    end, {Shapes0,Id0}, St),
    wings_move:setup(Dir, St#st{shapes=Shapes,onext=Id}).

new_name([H|T], Id) when $0 =< H, H =< $9 ->    
    new_name(T, Id);
new_name("ypoc"++_=Name, Id) ->
    reverse(Name, integer_to_list(Id));
new_name(Name, Id) ->
    reverse(Name, "_copy" ++ integer_to_list(Id)).

%%%
%%% The Delete command.
%%%

delete(#st{shapes=Shapes0}=St) ->
    Shapes = wings_sel:fold(fun(#shape{id=Id}, Shs) ->
				    gb_trees:delete(Id, Shs)
			    end, Shapes0, St),
    St#st{shapes=Shapes,sel=[]}.

%%%
%%% The Flip command
%%%

flip(Plane0, St) ->
    Plane = flip_scale(Plane0),
    wings_sel:map(fun(Sh) -> flip_body(Plane, Sh) end, St).

flip_body(Plane, #shape{sh=#we{}=We0}=Sh) ->
    {Cx,Cy,Cz} = wings_vertex:center(We0),
    M0 = wings_mat:translate(Cx, Cy, Cz),
    M1 = wings_mat:mult(M0, Plane),
    M = wings_mat:mult(M1, wings_mat:translate(-Cx, -Cy, -Cz)),
    Vtab = wings_util:fold_vertex(
	     fun(V, #vtx{pos={X0,Y0,Z0}}=Vrec, Acc) ->
		     {X,Y,Z,_} = wings_mat:mult(M, {X0,Y0,Z0,1}),
		     Pos = wings_util:share(X, Y, Z),
		     gb_trees:insert(V, Vrec#vtx{pos=Pos}, Acc)
	     end, gb_trees:empty(), We0),
    We = wings_we:invert_normals(We0#we{vs=Vtab}),
    Sh#shape{sh=We};
flip_body(Plane, Sh) -> Sh.

flip_scale(x) -> wings_mat:scale(-1, 1, 1);
flip_scale(y) -> wings_mat:scale(1, -1, 1);
flip_scale(z) -> wings_mat:scale(1, 1, -1).


%%%
%%% The Tighten command.
%%%

tighten(St0) ->
    {St,Tvs} = wings_sel:mapfold(fun tighten/2, [], St0),
    wings_drag:init_drag(Tvs, none, St).

tighten(#shape{id=Id,sh=#we{vs=Vtab}=We0}=Sh, A) ->
    Vs = gb_trees:keys(Vtab),
    {We,Tvs} = wings_vertex_cmd:tighten(Id, Vs, We0, A),
    {Sh#shape{sh=We},Tvs}.
    
%%%
%%% The Smooth command.
%%%
%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.
%%% 

smooth(St) ->
    wings_io:progress("Smoothing: ", 0),
    wings_sel:map(
      fun(#shape{sh=#we{fs=Ftab,he=Htab}=We0}=Sh0) ->
	      We = smooth_1(We0),
	      Sh = Sh0#shape{sh=We};
	 (Sh) -> Sh
      end, St).

smooth_1(#we{es=Etab,fs=Ftab0,next_id=Id,vs=Vtab0}=We0) ->
    FacePos0 = face_centers(We0),
    FacePos = gb_trees:from_orddict(reverse(FacePos0)),
    wings_io:progress("Smoothing: ", 25),
    We1 = cut_edges(gb_trees:to_list(Etab), FacePos, We0),
    wings_io:progress("Smoothing: ", 50),
    We = wings_util:fold_face(fun(Face, Rec, Acc) ->
				      smooth_face(Face, Id, FacePos, Acc)
			      end, We1, We1),
    wings_io:progress("Smoothing: ", 75),
    #we{vs=Vtab2} = We,
    Vtab = smooth_move_orig(gb_trees:keys(Vtab0), FacePos, We0, Vtab2),
    We#we{vs=Vtab}.

face_centers(#we{fs=Ftab}=We) ->
    face_centers(gb_trees:keys(Ftab), We, []).

face_centers([Face|Fs], We, Acc) ->
    Vs = wings_face:surrounding_vertices(Face, We),
    Center = wings_util:share(wings_vertex:center(Vs, We)),
    face_centers(Fs, We, [{Face,[Center|length(Vs)]}|Acc]);
face_centers([], We, Acc) -> Acc.

smooth_move_orig([V|Vs], FacePos, We, Vtab0) ->
    Vtab = smooth_move_orig_1(V, FacePos, We, Vtab0),
    smooth_move_orig(Vs, FacePos, We, Vtab);
smooth_move_orig([], FacePos, We, Vtab) -> Vtab.

smooth_move_orig_1(V, FacePosTab, #we{es=Etab,he=Htab,vs=OVtab}=We, Vtab) ->
    {Ps0,Hard} =
	wings_vertex:fold(
	  fun (Edge, Face, Erec, {Ps0,Hard0}) ->
		  OPos = wings_vertex:other_pos(V, Erec, OVtab),
		  Es = case gb_sets:is_member(Edge, Htab) of
			   true -> [OPos|Hard0];
			   false -> Hard0
		       end,
		  [FPos|_] = gb_trees:get(Face, FacePosTab),
		  Ps = [FPos,OPos|Ps0],
		  {Ps,Es}
	  end, {[],[]}, V, We),

    #vtx{pos=S} = Vrec = gb_trees:get(V, Vtab),
    case length(Hard) of
	NumHard when NumHard < 2 ->
	    Ps = wings_mat:add(Ps0),
	    N = length(Ps0) / 2,
	    Pos0 = wings_mat:add(wings_mat:divide(Ps, (N*N)),
				 wings_mat:mul(S, (N-2.0)/N)),
	    Pos = wings_util:share(Pos0),
	    gb_trees:update(V, Vrec#vtx{pos=Pos}, Vtab);
	NumHard when NumHard =:= 2 ->
	    Pos0 = wings_mat:add([wings_mat:mul(S, 6.0)|Hard]),
	    Pos1 = wings_mat:mul(Pos0, 1/8),
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

cut_edges([{Edge,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}}|Es], FacePos,
	  #we{he=Htab,vs=Vtab}=We0) ->
    case gb_sets:is_member(Edge, Htab) of
	true ->
	    {We,_,_} = wings_edge:fast_cut(Edge, default, We0),
	    cut_edges(Es, FacePos, We);
	false ->
	    [LfPos|_] = gb_trees:get(Lf, FacePos),
	    [RfPos|_] = gb_trees:get(Rf, FacePos),
	    VaPos = wings_vertex:pos(Va, Vtab),
	    VbPos = wings_vertex:pos(Vb, Vtab),
	    Pos = wings_mat:average([LfPos,RfPos,VaPos,VbPos]),
	    {We,_,_} = wings_edge:fast_cut(Edge, Pos, We0),
	    cut_edges(Es, FacePos, We)
    end;
cut_edges([], FacePos, We) -> We.

%%%
%%% The Combine command.
%%%

combine(#st{sel=[]}=St) -> St;
combine(#st{shapes=Shapes0,sel=[{Id,_}=Sel|T]}=St) ->
    case gb_trees:get(Id, Shapes0) of
	#shape{sh=#we{}=We0}=Sh ->
	    {We,Shapes1} = combine(T, Shapes0, We0),
	    Shapes = gb_trees:update(Id, Sh#shape{sh=We}, Shapes1),
	    St#st{shapes=Shapes,sel=[Sel]};
	Other ->
	    combine(St#st{sel=T})
    end.

combine([{Id,_}|T], Shapes0, We0) ->
    case gb_trees:get(Id, Shapes0) of
	#shape{sh=#we{}=WeOther} ->
	    We = wings_we:merge(We0, WeOther),
	    Shapes = gb_trees:delete(Id, Shapes0),
	    combine(T, Shapes, We);
	Other -> combine(T, Shapes0, We0)
    end;
combine([], Shapes, We) -> {We,Shapes}.

%%%
%%% The Separate command.
%%%

separate(St0) ->
    {St,New} =
	wings_sel:mapfold(
	  fun(#shape{sh=#we{}=We0}=Sh0, A) ->
		  case wings_we:separate(We0) of
		      [_] -> {Sh0,A};
		      [We|New] -> {Sh0#shape{sh=We},New++A}
		  end;
	     (Sh, A) -> {Sh,A}
	  end, [], St0),
    #st{onext=Id0,shapes=Shapes0} = St,
    {Shapes,Id} = foldl(fun(We, {A,I}) ->
				Name = "separate"++integer_to_list(I),
				Sh = #shape{id=I,name=Name,sh=We},
				{gb_trees:insert(I, Sh, A),I+1}
			end, {Shapes0,Id0}, New),
    St#st{onext=Id,shapes=Shapes,sel=[]}.

%%%
%%% The Auto-Smooth command.
%%%

auto_smooth(St) ->
    wings_sel:map(
      fun(#shape{sh=#we{}=We0}=Sh0) ->
	      We = auto_smooth_1(We0),
	      Sh = Sh0#shape{sh=We};
	 (Sh) -> Sh
      end, St).

auto_smooth_1(#we{he=Htab0}=We) ->
    Htab = wings_util:fold_edge(fun(E, R, A) ->
					auto_smooth(E, R, A, We)
				end, Htab0, We),
    We#we{he=Htab}.

auto_smooth(Edge, #edge{lf=Lf,rf=Rf}, H0, We) ->
    Ln = wings_face:normal(Lf, We),
    Lr = wings_face:normal(Rf, We),
    case wings_mat:is_non_zero(Ln, Lr) of
	false -> H0;				%Ignore this edge.
	true ->
	    case wings_mat:unit_dot_product(Ln, Lr) of
		P when P < 0.5 ->		%cos(60), i.e. angle > 60
		    wings_edge:hardness(Edge, hard, H0);
		P ->				%angle =< 60
		    wings_edge:hardness(Edge, soft, H0)
	    end
    end.
