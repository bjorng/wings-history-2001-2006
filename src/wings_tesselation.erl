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
%%     $Id: wings_tesselation.erl,v 1.3 2003/08/22 10:52:26 bjorng Exp $
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

doface(Face, We, Q) ->
    Vs = wings_face:vertices_ccw(Face, We),
    case length(Vs) of
	4 when not Q -> triangulate_quad(Vs, We);
	Len when not Q, Len =< 3 -> We;
	Len when Q, Len =< 4 -> We;
	Len -> doface_1(Len, Vs, We, Q)
    end.

%% triangulate_quad([VertexIndex], We) -> We'
%%  Quickly triangulates a quad by connecting along the shortest diagonal,
%%  and then checking that normals for the triangles are consistent with
%%  the normal for the quad. Falls back to the general triangulator if
%%  normals are inconsistent (= concave or otherwise strange quad).
triangulate_quad(Vs, #we{vp=Vtab}=We0) ->
    case triangulate_quad_1(Vs, Vs, Vtab, We0, []) of
	error -> doface_1(4, Vs, We0, false);
	We -> We
    end.

triangulate_quad_1([V|Vs], Vs0, Vtab, We, Acc) ->
    triangulate_quad_1(Vs, Vs0, Vtab, We, [gb_trees:get(V, Vtab)|Acc]);
triangulate_quad_1([], [Ai,Bi,Ci,Di], _, We, VsPos) ->
    N = e3d_vec:normal(VsPos),
    [D,C,B,A] = VsPos,
    case e3d_vec:dist(A, C) < e3d_vec:dist(B, D) of
	true ->
	    case wings_draw_util:consistent_normal(C, B, A, N) andalso
		wings_draw_util:consistent_normal(D, C, A, N) of
		false -> error;
		true -> wings_vertex_cmd:connect([Ai,Ci], We)
	    end;
	false ->
	    case wings_draw_util:consistent_normal(B, A, D, N) andalso
		wings_draw_util:consistent_normal(B, D, C, N) of
		false -> error;
		true -> wings_vertex_cmd:connect([Bi,Di], We)
	    end
    end.

doface_1(Len, Vs, #we{vp=Vtab}=We, Q) ->
    FaceVs = lists:seq(0, Len-1),
    Vcoords = [gb_trees:get(V, Vtab) || V <- Vs],
    E3dface = #e3d_face{vs=FaceVs},
    T3dfaces = case Q of
		   true -> e3d_mesh:quadrangulate_face(E3dface, Vcoords);
		   false -> e3d_mesh:triangulate_face(E3dface, Vcoords)
	       end,
    VsTuple = list_to_tuple(Vs),
    Tfaces = [renumber(FVs, VsTuple) || #e3d_face{vs=FVs} <- T3dfaces],
    Bord = bedges(Vs),
    Diags = diags(Tfaces, Bord),
    connect_diags(Diags, We).

renumber(L, Vtab) ->
    renumber(L, Vtab, []).
renumber([V|Vs], Vtab, Acc) ->
    renumber(Vs, Vtab, [element(V+1, Vtab)|Acc]);
renumber([], _, Acc) -> reverse(Acc).

%% This simple code only works because we assume that each
%% vertex can appear only once in a face.
connect_diags([], We) -> We;
connect_diags([{A,B}|T], We0) ->
    We = wings_vertex_cmd:connect([A,B], We0),
    connect_diags(T, We).

%% Return GbSet of {A,B} where AB is an edge in face F
bedges(F) -> bedges(F, F, []).

bedges([A], [B|_], S) -> gb_sets:from_list([{A,B}|S]);
bedges([A|[B|_]=T], F, S) -> bedges(T, F, [{A,B}|S]).

diags(Fl, Bord) -> diags(Fl, Bord, []).

diags([], _, S) -> reverse(S);
diags([Vs|T], Bord, S) -> diags(T, Bord, diagsf(Vs, Vs, Bord, S)).

diagsf([A], [First|_], Bord, S) ->
    trydiag(A, First, Bord, S);
diagsf([A|[B|_]=T], Vs, Bord, S) ->
    diagsf(T, Vs, Bord, trydiag(A, B, Bord, S)).

trydiag(A, B, _, S) when A > B ->
    %% only want one representative of diag
    S;
trydiag(A, B, Bord, S) ->
    E = {A,B},
    case gb_sets:is_member(E, Bord) of
	true -> S;
	false -> [E|S]
    end.