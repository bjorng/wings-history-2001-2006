%%
%%  wings_extrude.erl --
%%
%%     This module extrude faces or face regions. Used by commands
%%     such as Extrude, Extrude Region, Bevel, and Inset.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_extrude.erl,v 1.1.1.1 2001/08/14 18:16:35 bjorng Exp $
%%

-module(wings_extrude).
-export([extrude_face/3]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1]).

extrude_face(Faces, Edges, We0) ->
    {Ids,We} = wings_we:new_wrap_range(length(Edges), 4, We0),
    extrude_edges(Edges, Faces, Ids, We, We).

extrude_edges([E|Es], Face, Ids, We, Acc0) ->
    Acc = extrude_edge(E, Face, Ids, We, Acc0),
    extrude_edges(Es, Face, wings_we:bump_id(Ids), We, Acc);
extrude_edges([], _, _, _, Acc) -> Acc.

extrude_edge(Eupper, Faces, Ids, #we{es=Etab,fs=Ftab}=We, Acc0) ->
    Elower = wings_we:id(6, Ids),
    Eleft = wings_we:id(4, Ids),
    Vleft = wings_we:id(5, Ids),
    Vright = wings_we:id(9, Ids),
    Eright = wings_we:id(8, Ids),
    NewFace = wings_we:id(7, Ids),

    Erec = gb_trees:get(Eupper, Etab),
    Face = get_face(Erec, Faces),
    FaceRec = gb_trees:get(Face, Ftab),
    {UppermostLeft,UppermostRight,EleftLowest,ErightLowest,
     VlowerLeft,VlowerRight,OtherFace,Acc1} =
	split_top_edge(Eupper, Erec, Faces, Face, Ids, Etab, Acc0),
    Acc2 = patch_lowest_left(EleftLowest, Eupper, Faces, OtherFace, Ids, Acc1),
    Acc3 = patch_lowest_right(ErightLowest, Eupper, Faces,
			      OtherFace, Ids, Acc2),
    Acc4 = new_left_edge(Eupper, VlowerLeft, Ids, Acc3),
    Acc5 = new_right_edge(Eupper, Ids, Acc4),
    Acc6 = store(insert, face, NewFace, FaceRec#face{edge=Eleft}, Acc5),
    OtherFaceRec = gb_trees:get(OtherFace, Ftab),
    Acc7 = store(update, face, OtherFace, OtherFaceRec#face{edge=Elower}, Acc6),
    Acc8 = patch_uppermost_left(VlowerLeft, UppermostLeft,
				Faces, Face, Ids, Acc7),
    Acc = patch_uppermost_right(VlowerRight, UppermostRight,
				Faces, Face, Ids, Acc8),
    new_vertex(VlowerLeft, Vleft, Eleft, We, Acc).

get_face(#edge{lf=Lf,rf=Rf}, Faces) ->
    case gb_sets:is_member(Lf, Faces) of
	true -> Lf;
	false ->
	    ?ASSERT(gb_sets:is_member(Rf, Faces)),
	    Rf
    end.

split_top_edge(Eupper, Erec, Faces, Face, Ids, Etab, Acc0) ->
    ElowerLeft = wings_we:id(2, Ids),
    Elower = wings_we:id(6, Ids),
    ElowerRight = wings_we:id(10, Ids),
    NewFace = wings_we:id(7, Ids),
    Vleft = wings_we:id(5, Ids),
    Vright = wings_we:id(9, Ids),
    Eleft = wings_we:id(4, Ids),
    Eright = wings_we:id(8, Ids),
    case Erec of
	#edge{vs=VlowerLeft,ve=VlowerRight,lf=Face,rf=OtherFace,
	      ltsu=UppermostLeft,ltpr=UppermostRight,
	      rtpr=EleftLowest,rtsu=ErightLowest} ->
	    Upper = Erec#edge{vs=Vleft,ve=Vright,
			      rf=NewFace,rtpr=Eleft,rtsu=Eright},
	    Lower0 = Erec#edge{lf=NewFace,ltpr=Eright,ltsu=Eleft},
	    Lower1 = patch_lower(Lower0, #edge.rtpr, ElowerLeft, 
				 Faces, OtherFace, Etab),
	    Lower = patch_lower(Lower1, #edge.rtsu, ElowerRight,
				Faces, OtherFace, Etab);
	#edge{ve=VlowerLeft,vs=VlowerRight,rf=Face,lf=OtherFace,
	      rtsu=UppermostLeft,rtpr=UppermostRight,
	      ltpr=EleftLowest,ltsu=ErightLowest} ->
	    Upper = Erec#edge{vs=Vright,ve=Vleft,
			      lf=NewFace,ltpr=Eleft,ltsu=Eright},
	    Lower0 = Erec#edge{rf=NewFace,rtpr=Eright,rtsu=Eleft},
	    Lower1 = patch_lower(Lower0, #edge.ltpr, ElowerLeft,
				 Faces, OtherFace, Etab),
	    Lower = patch_lower(Lower1, #edge.ltsu, ElowerRight,
				Faces, OtherFace, Etab)
    end,
    Acc1 = store(insert, edge, Elower, Lower, Acc0),
    Acc = store(update, edge, Eupper, Upper, Acc1),
    {UppermostLeft,UppermostRight,EleftLowest,ErightLowest,
     VlowerLeft,VlowerRight,OtherFace,Acc}.

patch_lower(Lower, Field, NewEdge, Faces, Face, Etab) ->
    Edge = element(Field, Lower),
    case gb_trees:get(Edge, Etab) of
	#edge{lf=Face,rf=AFace} ->
	    maybe_patch(AFace, Faces, Field, Lower, NewEdge);
	#edge{rf=Face,lf=AFace} ->
	    maybe_patch(AFace, Faces, Field, Lower, NewEdge);
	Other -> Lower
    end.

maybe_patch(Face, Faces, Field, Lower, NewEdge) ->
    case gb_sets:is_member(Face, Faces) of
	true -> setelement(Field, Lower, NewEdge);
	false -> Lower
    end.

new_left_edge(Eupper, VlowerLeft, Ids, Acc) ->
    Elower = wings_we:id(6, Ids),
    NewFace = wings_we:id(7, Ids),
    Eleft = wings_we:id(4, Ids),
    Vleft = wings_we:id(5, Ids),
    Left0 = new_edge(Eleft, Acc),
    Left = Left0#edge{vs=VlowerLeft,ve=Vleft,
		      lf=wings_we:id(3, Ids),rf=NewFace,
		      ltsu=wings_we:id(2, Ids),
		      rtpr=Elower,rtsu=Eupper},
    store(enter, edge, Eleft, Left, Acc).

new_right_edge(Eupper, Ids, Acc) ->
    Eright = wings_we:id(8, Ids),
    Right0 = new_edge(Eright, Acc),
    Right = Right0#edge{ltpr=Eupper},
    store(enter, edge, Eright, Right, Acc).

patch_lowest_left(EleftLowest, Eupper, Faces, OtherFace, Ids, Acc) ->
    Eleft = wings_we:id(4, Ids),
    Elower = wings_we:id(6, Ids),
    LeftNewFace = wings_we:id(3, Ids),
    case get_edge(EleftLowest, Acc) of
	#edge{lf=OtherFace,rf=AFace}=LL0 ->
	    ?ASSERT(LL0#edge.ltsu == Eupper),
	    case gb_sets:is_member(AFace, Faces) of
		true -> Acc;
		false ->
		    LeftLowest = LL0#edge{ltsu=Elower},
		    store(update, edge, EleftLowest, LeftLowest, Acc)
	    end;
	#edge{rf=OtherFace,lf=AFace}=LL0 ->
	    ?ASSERT(LL0#edge.rtsu == Eupper),
	    case gb_sets:is_member(AFace, Faces) of
		true -> Acc;
		false ->
		    LeftLowest = LL0#edge{rtsu=Elower},
		    store(update, edge, EleftLowest, LeftLowest, Acc)
	    end;
	Other -> Acc
    end.

patch_lowest_right(ErightLowest, Eupper, Faces, OtherFace, Ids, Acc) ->
    Elower = wings_we:id(6, Ids),
    case get_edge(ErightLowest, Acc) of
	#edge{lf=OtherFace,rf=AFace}=RL0 ->
	    ?ASSERT(RL0#edge.ltpr == Eupper),
	    case gb_sets:is_member(AFace, Faces) of
		true -> Acc;
		false ->
		    RightLowest = RL0#edge{ltpr=Elower},
		    store(update, edge, ErightLowest, RightLowest, Acc)
		end;
	#edge{rf=OtherFace,lf=AFace}=RL0 ->
	    ?ASSERT(RL0#edge.rtpr == Eupper),
	    case gb_sets:is_member(AFace, Faces) of
		true -> Acc;
		false ->
		    RightLowest = RL0#edge{rtpr=Elower},
		    store(update, edge, ErightLowest, RightLowest, Acc)
	    end;
	Other -> Acc
    end.

patch_uppermost_left(V, UppermostLeft, Faces, Face, Ids, Acc0) ->
    Vleft = wings_we:id(5, Ids),
    case lookup_edge(UppermostLeft, Acc0) of
	#edge{ve=V,lf=Face,rf=OtherFace,rtsu=Next}=Erec ->
	    Left = Erec#edge{ve=Vleft},
	    Acc = store(enter, edge, UppermostLeft, Left, Acc0),
	    case gb_sets:is_member(OtherFace, Faces) of
		true ->
		    patch_uppermost_left(V, Next, Faces, 
					 OtherFace, Ids, Acc);
		false -> Acc
	    end;
	#edge{vs=V,rf=Face,lf=OtherFace,ltsu=Next}=Erec ->
	    Left = Erec#edge{vs=Vleft},
	    Acc = store(enter, edge, UppermostLeft, Left, Acc0),
	    case gb_sets:is_member(OtherFace, Faces) of
		true ->
		    patch_uppermost_left(V, Next, Faces, 
					 OtherFace, Ids, Acc);
		false -> Acc
	    end;
	Other -> Acc0
    end.

patch_uppermost_right(V, UppermostRight, Faces, Face, Ids, Acc0) ->
    Vright = wings_we:id(9, Ids),
    case lookup_edge(UppermostRight, Acc0) of
	#edge{vs=V,lf=Face,rf=OtherFace,rtpr=Next}=Erec ->
	    Right = Erec#edge{vs=Vright},
	    Acc = store(enter, edge, UppermostRight, Right, Acc0),
	    case gb_sets:is_member(OtherFace, Faces) of
		true ->
		    patch_uppermost_right(V, Next, Faces, OtherFace,
					  Ids, Acc);
		false -> Acc
	    end;
	#edge{ve=V,rf=Face,lf=OtherFace,ltpr=Next}=Erec ->
	    Right = Erec#edge{ve=Vright},
	    Acc = store(enter, edge, UppermostRight, Right, Acc0),
	    case gb_sets:is_member(OtherFace, Faces) of
		true ->
		    patch_uppermost_right(V, Next, Faces, OtherFace,
					  Ids, Acc);
		false -> Acc
	    end;
	Other -> Acc0
    end.

new_vertex(Old, New, Edge, #we{vs=VsTab0}=We, Acc0) ->
    VtxRec0 = gb_trees:get(Old, VsTab0),
    VtxRec = VtxRec0#vtx{edge=Edge},
    Acc = store(insert, vertex, New, VtxRec, Acc0),
    store(update, vertex, Old, VtxRec, Acc).

get_edge(Edge, #we{es=Tab}) ->
    gb_trees:get(Edge, Tab).

lookup_edge(Edge, #we{es=Tab}) ->
    case gb_trees:lookup(Edge, Tab) of
	{value,Data} -> Data;
 	none -> none
    end.

new_edge(Edge, #we{es=Etab}) ->
    case gb_trees:lookup(Edge, Etab) of
	{value,Data} -> Data;
	none -> #edge{}
    end.

store(Op, vertex, Key, Data, #we{vs=Vtab0}=We) ->
    Vtab = update_op(Op, Key, Data, Vtab0),
    We#we{vs=Vtab};
store(Op, edge, Key, Data, #we{es=Etab0}=We) ->
    Etab = update_op(Op, Key, Data, Etab0),
    We#we{es=Etab};
store(Op, face, Key, Data, #we{fs=Ftab0}=We) ->
    Ftab = update_op(Op, Key, Data, Ftab0),
    We#we{fs=Ftab}.

update_op(insert, Key, Data, Tab) ->
    gb_trees:insert(Key, Data, Tab);
update_op(update, Key, Data, Tab) ->
    gb_trees:update(Key, Data, Tab);
update_op(enter, Key, Data, Tab) ->
    gb_trees:enter(Key, Data, Tab).
