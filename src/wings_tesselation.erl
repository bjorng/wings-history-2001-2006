%%
%%  wings_tesselation.erl --
%%
%%     Tesselation/subdivision commands.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_tesselation.erl,v 1.1 2003/08/21 06:02:51 bjorng Exp $
%%

-module(wings_tesselation).
-export([submenu/0,command/2]).
-export([triangulate/1,triangulate/2,quadrangulate/1,quadrangulate/2]).

-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-import(lists, [map/2,reverse/1]).

submenu() ->
    [{"Triangulate",triangulate},
     {"Quadrangulate",quadrangulate}].

command(triangulate, St) ->
    Action = fun triangulate/2,
    {St1,Sel} = wings_sel:mapfold(fun(Fs, We, A) ->
					  do_faces(Action, Fs, We, A) end,
				  [], St),
    {save_state,St1#st{sel=reverse(Sel)}};
command(quadrangulate, St) ->
    Action = fun quadrangulate/2,
    {St1,Sel} = wings_sel:mapfold(fun(Fs, We, A) ->
					  do_faces(Action, Fs, We, A) end,
				  [], St),
    {save_state,St1#st{sel=reverse(Sel)}}.

triangulate(#we{fs=Ftab}=We) ->
    triangulate(gb_trees:keys(Ftab), We).

triangulate(Faces, We) when is_list(Faces) ->
    tess_faces(Faces, We, false);
triangulate(Faces, We) ->
    triangulate(gb_sets:to_list(Faces), We).

quadrangulate(#we{fs=Ftab}=We) ->
    quadrangulate(gb_trees:keys(Ftab), We).

quadrangulate(Faces, We) when is_list(Faces) ->
    tess_faces(Faces, We, true);
quadrangulate(Faces, We) ->
    quadrangulate(gb_sets:to_list(Faces), We).

%%%
%%% Internal functions.
%%%

do_faces(Action, Faces, #we{id=Id}=We0, Acc) ->
    We = Action(Faces, We0),
    Sel = gb_sets:union(wings_we:new_items(face, We0, We), Faces),
    {We,[{Id,Sel}|Acc]}.

tess_faces([], We, _Q) -> We;
tess_faces([F|T], We, Q) -> tess_faces(T, doface(F, We, Q), Q).

doface(Face, #we{vp=Vtab}=We, Q) ->
    Mat = wings_material:get(Face, We),
    Vs = wpa:face_vertices(Face, We),
    Len = length(Vs),
    case (Q and (Len < 5)) orelse (not(Q) and (Len < 4)) of
	true -> We;
	_ ->
	    Vcoords = coord_list(gb_trees:to_list(Vtab)),
	    E3dface = #e3d_face{vs=Vs,mat=Mat},
	    T3dfaces = case Q of
			   true -> e3d_mesh:quadrangulate_face(E3dface, Vcoords);
			   _ -> e3d_mesh:triangulate_face(E3dface, Vcoords)
		       end,
	    Tfaces = [ X#e3d_face.vs || X <- T3dfaces],
	    Bord = bedges(Vs),
	    Diags = diags(Tfaces, Bord),
	    connect_diags(Diags, We)
    end.

%% This simple code only works because we assume that each
%% vertex can appear only once in a face.
connect_diags([], We) -> We;
connect_diags([{A,B}|T], We) ->
    Vs = gb_sets:from_ordset([A,B]),
    We1 = wings_vertex_cmd:connect(Vs, We),
    connect_diags(T, We1).


%% Return GbSet of {A,B} where AB is an edge in face F
bedges(F) -> bedges(F,F,gb_sets:empty()).

bedges([], _, S) -> S;
bedges([A],[B|_],S) -> gb_sets:add({A,B}, S);
bedges([A,B|T],F,S) -> bedges([B|T], F, gb_sets:add({A,B}, S)).

diags(Fl, Bord) -> diags(Fl, Bord, []).

diags([], _, S) -> reverse(S);
diags([Vs|T], Bord, S) ->
    diags(T, Bord, diagsf(Vs, Vs, Bord, S)).

diagsf([A], [First|_], Bord, S) -> trydiag({A,First}, Bord, S);
diagsf([A|[B|_]=T], Vs, Bord, S) ->
    diagsf(T, Vs, Bord, trydiag({A,B}, Bord, S)).

trydiag(E={A,B}, Bord, S) ->
    if
	A > B -> S;	% only want one representative of diag
	true ->
	    case gb_sets:is_member(E, Bord) of
		true -> S;
		_ -> [E | S]
	    end
    end.

%% L is ordered list of {index,pos} pairs.
%% Return list of pos, with implicit indexing from 0 to max index
%% (fill in missing coords with {0,0,0}).
coord_list(L) -> coord_list(L, 0, []).

coord_list([], _, Acc) -> reverse(Acc);
coord_list(L=[{K,P}|Rest], I, Acc) ->
    if
	I < K ->
	    coord_list(L, I+1, [e3d_vec:zero() | Acc]);
	I == K ->
	    coord_list(Rest, I+1, [P | Acc])
    end.
