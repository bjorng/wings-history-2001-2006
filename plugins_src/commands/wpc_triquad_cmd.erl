%%
%%  wpc_triquad_cmd.erl --
%%
%%     Triangulate, Quadrangulate faces plugin.
%%
%%  Copyright (c) 2001 Howard Trickey
%%		  2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(wpc_triquad_cmd).

-export([init/0,menu/2,command/2,tess_faces/3]).

-import(lists, [map/2,reverse/1]).

-include_lib("wings.hrl").
-include_lib("e3d.hrl").

init() -> true.

menu({face}, Menu0) ->
    Menu0 ++ [separator,
	      {"Tesselate",
	       {tesselate, [{"Triangulate",triangulate},
			    {"Quadrangulate",quadrangulate}]}}
	     ];
menu(_, Menu) -> Menu.

command({face, {tesselate,triangulate}}, St) ->
    {St1,Sel} = wings_sel:mapfold(fun (Fs,We,A) ->
					  dofaces(Fs,We,false,A) end,
				  [], St),
    St1#st{sel=reverse(Sel)};
command({face, {tesselate,quadrangulate}}, St) ->
    {St1,Sel} = wings_sel:mapfold(fun (Fs,We,A) ->
					  dofaces(Fs,We,true,A) end,
				  [], St),
    St1#st{sel=reverse(Sel)};
command(_, _) -> next.

%% First arg is set of Face ids to operate on in We.
%% Q says whether to quadrangulate (true) or triangulate (false).
%% Returns modifed We.
dofaces(Faces, #we{id=Id}=We0, Q, Acc) ->
    We = tess_faces(gb_sets:to_list(Faces), We0, Q),
    Sel = gb_sets:union(wings_we:new_items(face, We0, We), Faces),
    {We,[{Id,Sel}|Acc]}.

tess_faces([], We, _Q) -> We;
tess_faces([F|T], We, Q) -> tess_faces(T, doface(F, We, Q), Q).

doface(Face, #we{vp=Vtab}=We, Q) ->
    Mat = wings_material:get(Face, We),
    %%#face{mat=Mat} = gb_trees:get(Face, Ftab),
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
