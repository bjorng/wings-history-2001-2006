%%%-------------------------------------------------------------------
%%% File    : auv_mapping.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Provides different uv-parametrisation algorihms
%%%
%%% Created :  4 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002 Dan Gudmundsson, Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv_mapping.erl,v 1.5 2002/10/09 21:46:34 dgud Exp $

-module(auv_mapping).
-include("wings.hrl").
-include("auv.hrl").
-include("e3d.hrl").
-export([projectFromChartNormal/2, project2d/3, lsqcm/2]).

%%% From camera would look something like this!! 
%%% It actually worked once :-) 
%project -> %% Only one Area
%    Clustered = gb_trees:keys(Ftab0),
%    _CI = [_Aim, Azi, Elev, _Track] = 
%	wpa:camera_info([aim,azimuth, elevation, tracking]),
%    N0 = {0.0, 0.0, 1.0},
%    AziRot = e3d_mat:rotate(Azi, {0.0,1.0,0.0}),
%		N1 = e3d_mat:mul_vector(AziRot, N0),
%    ElevRot = e3d_mat:rotate(Elev, {1.0,0.0,0.0}),
%    N2 = e3d_mat:mul_vector(ElevRot, N1),
%    %%		?DBG("Projected by ~p using camera ~p ~n", [N2, _CI]),
%    [create_area(Clustered, N2, We0)];

projectFromChartNormal({_Id,Chart},We) ->
    CalcNormal = fun(Face, Sum) ->
			 Normal = wings_face:normal(Face, We),
			 Vs0 = wpa:face_vertices(Face, We),
			 Area = calc_area(Vs0,Normal, We),
			 %%         ?DBG("Area was ~p ~n", [Area]),
			 e3d_vec:add(Sum, e3d_vec:mul(Normal, Area))
		 end,
    N0 = lists:foldl(CalcNormal, e3d_vec:zero(), Chart),
    Normal = e3d_vec:norm(N0),
    Vs0 = wings_face:to_vertices(Chart, We),
    Vs2 = project2d(Vs0, Normal, We),
    Vs2.

project2d(Vs, Normal, We) ->
    Rot = e3d_mat:rotate_s_to_t(Normal,{0.0,0.0,1.0}),
    Res = [{V,e3d_mat:mul_point(Rot, wings_vertex:pos(V, We))} || 
	      V <- Vs],
    lists:reverse(Res).

%% Alg. found in comp.graphics.algorithms faq
%% To be correct it needs the polygons to flat but we
%% don't need to be 100% correct.
calc_area(Vs0, Normal, We) ->
    [V|Vs] = [wings_vertex:pos(V, We) || V <- Vs0],
    Sum = sum_crossp([V|Vs] ++ [V], e3d_vec:zero()),
    0.5 * abs(e3d_vec:dot(Normal, Sum)).

sum_crossp([V1,V2|Vs], Acc) ->
    Cross = e3d_vec:cross(V1,V2),
    sum_crossp([V2|Vs], e3d_vec:add(Acc, Cross));
sum_crossp([_Last], Acc) ->
    Acc.

project_and_triangulate([Face|Fs], We, I, Acc) ->
    Normal = wings_face:normal(Face, We),
    Vs0 = wings_face:to_vertices([Face], We),
    Vs2 = project2d(Vs0, Normal, We),
    if length(Vs2) > 3 ->
	    {Ids,Cds,All} = setup_tri_vs(Vs2,0,[],[],[]),
	    NewFs = e3d_mesh:triangulate_face(#e3d_face{vs=Ids}, Cds),
	    VT = gb_trees:from_orddict(All),
	    {Add, I1} = get_verts(NewFs, I, VT, []),
	    project_and_triangulate(Fs,We,I1,Add ++ Acc);
       true ->
	    Vs3 = [{Vid, {Vx, Vy}} || {Vid,{Vx,Vy,Vz}} <- Vs2],
	    project_and_triangulate(Fs,We,I,[{Face, Vs3}|Acc])
    end;
project_and_triangulate([],_,_,Acc) -> 
    Acc.

setup_tri_vs([{Old,Coord}|Vs],Id,Ids,Cds,All) ->
    setup_tri_vs(Vs,Id+1,[Id|Ids],[Coord|Cds],[{Id,{Old,Coord}}|All]);
setup_tri_vs([],_,Ids,Cds,All) ->
    {lists:reverse(Ids),lists:reverse(Cds),lists:reverse(All)}.

get_verts([#e3d_face{vs = Vs}|Fs],I,Coords,Acc) ->
    Get = fun(Id) ->
		  {RealId,{X,Y,_}} = gb_trees:get(Id, Coords),
		  {RealId,{X,Y}}
	  end,
    VsC = lists:map(Get, Vs),
    get_verts(Fs,I-1,Coords,[{I, VsC}|Acc]);
get_verts([],I,_,Acc) ->
    {Acc, I}.

lsqcm(C = {Id, Fs}, We) ->
    Vs1 = project_and_triangulate(Fs,We,-1,[]),
    {V1, V2} = get_2uvs(C, We),
    case lsq(Vs1,V1,V2) of
	{error, What} ->
	    ?DBG("TXMAP error ~p~n", [What]),
	    exit({txmap_error, What});
	{ok,Vs2} ->
	    Patch = fun({Idt, {Ut,Vt}}) -> {Idt, {Ut,Vt, 0.0}} end,
	    lists:map(Patch, Vs2)
    end.

get_2uvs(C = {Id, Faces}, We) ->
    %% Hack to test Raimo's algo 
    VsProjected = projectFromChartNormal(C, We),
    %% Use wings_face: outer_edges because I don't want to exclude
    %% the faces whose normal points in the neg Z..
    BorderEdges = 
	case wings_face:outer_edges(Faces, We) of
	    [] -> 
		exit({invalid_chart, 
		      {C, is_closed_surface}});
	    Else ->
		Else
	end,
    Lookup = gb_trees:from_orddict(lists:sort(VsProjected)),
    Pick = fun(Edge, Acc) ->
		   #edge{vs=Vs,ve=Ve}=gb_trees:get(Edge, We#we.es),
		   V1 = gb_trees:get(Vs, Lookup),
		   V2 = gb_trees:get(Ve, Lookup),
		   [{Vs,V1},{Ve,V2}|Acc]
	   end,
    Vs0 = lists:foldl(Pick, [], BorderEdges),

    [First |RVs1] = Vs0,
    {BX0={_,{X1,_,_}}, BX1={_,{X2,_,_}}, 
     BY0={_,{_,Y1,_}}, BY1={_,{_,Y2,_}}} =
	lists:foldl(fun(Pos, Ac) -> maxmin(Pos, Ac) end, 
		    {First,First,First,First}, RVs1),
    if 
	(X2-X1) > (Y2-Y1) ->
	    ?DBG("Points choosen ~p ~p~n", [BX0,BX1]),
	    choose(BX1,BX0);
	true ->
	    ?DBG("Points choosen ~p ~p~n", [BY0,BY1]),
	    choose(BY1,BY0)
    end.

choose({Id1,{X1,Y1,_}}, {Id2,{X2,Y2,_}}) ->
    {{Id1, {X1,Y1}},{Id2,{X2,Y2}}}.

maxmin(New = {_,{X,Y,_}}, {X1={_,{XMin,_,_}},X2={_,{XMax,_,_}},
			   Y1={_,{_,YMin,_}},Y2={_,{_,YMax,_}}}) ->
    if 	X > XMax ->
	    if Y > YMax -> {X1, New, Y1, New};
	       Y < YMin -> {X1, New, New, Y2};
	       true -> {X1, New, Y1, Y2}
	    end;
	X < XMin ->
	    if Y > YMax -> {New, X2, Y1, New};
	       Y < YMin -> {New, X2, New, Y2};
	       true -> {New, X2, Y1, Y2}
	    end;
	Y > YMax ->
	    {X1, X2, Y1, New};
	Y < YMin ->
	    {X1, X2, New, Y2};
	true ->
	    {X1, X2, Y1, Y2}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%

lsq([C | _] = Name, P1, P2)
  when integer(C),
       0 =< C, C=< 255 ->
    case file:consult(Name) of
	{ok, L} ->
	    lsq(L, P1, P2);
	Error ->
	    Error
    end;
lsq(_, {P,_} = PUV1, {P,_} = PUV2) ->
    {error, {invalid_arguments, [PUV1, PUV2]}};
lsq(_, {_,{U1,V1}} = PUV1, {_,{U2,V2}} = PUV2)
  when U1 == U2, V1 == V2 -> % Coarse comparision, not match
    {error, {invalid_arguments, [PUV1, PUV2]}};
lsq(L, {P1,{U1,V1}} = PUV1, {P2,{U2,V2}} = PUV2)
  when list(L), number(U1), number(V1), number(U2), number(V2) ->
    case catch lsq_int(L, PUV1, PUV2) of
	{'EXIT', Reason} ->
	    exit(Reason);
	badarg ->
	    erlang:fault(badarg, [L, PUV1, PUV2]);
	Result ->
	    Result
    end;
lsq(L, P1, P2) ->
    erlang:fault(badarg, [L, P1, P2]).

lsq_int(L, {P1,{U1,V1}} = PUV1, {P2,{U2,V2}} = PUV2) ->
    %% Create a dictionary and a reverse dictionary for all points
    %% in the indata. Translate the point identities into a
    %% continous integer sequence.
    {M, Dict, Rdict} = lsq_points(L),
    {N, L1, L2} = lsq_triangles(L, Dict),
    Q1 = case dict:find(P1, Dict) of 
	     {ok, Q_1} -> Q_1;
	     error -> throw({error, {invalid_arguments, [PUV1]}})
	 end,
    Q2 = case dict:find(P2, Dict) of 
	     {ok, Q_2} -> Q_2;
	     error -> throw({error, {invalid_arguments, [PUV2]}})
	 end,
    
    %% Build the basic submatrixes 
    %% M1 = Re(M), M2 = Im(M), M2n = -M2
    M1 = auv_matrix:rows(N, M, L1),
    M2 = auv_matrix:rows(N, M, L2),
    L2n = [[{J,-V} || {J,V} <- R] || R <- L2],
    M2n = auv_matrix:rows(N, M, L2n),
    
    %% Build column lists of the M matrixes
    M1c = auv_matrix:cols(M1),
    M2c = auv_matrix:cols(M2),
    M2nc = auv_matrix:cols(M2n),
    %% Split the column lists into free (Mf) and pinned (Mp)
    [{Qa,{Ua,Va}}, {Qb,{Ub,Vb}}] = lists:sort([{Q1,{U1,V1}}, {Q2,{U2,V2}}]),
    QaQb = [Qa, Qb],
    {Mf1c,Mp1c} = pick(M1c, QaQb),
    {Mf2c,Mp2c} = pick(M2c, QaQb),
    {Mf2nc,Mp2nc} = pick(M2nc, QaQb),
    
    %% Build the matrixes Af and Ap, and vector B
    %% A = [ M1 -M2 ],  B = Ap U, U is vector of pinned points
    %%     [ M2  M1 ]
    MM = 2*(M-2),
    Afu = auv_matrix:cols(N, MM, Mf1c++Mf2nc),
    Afl = auv_matrix:cols(N, MM, Mf2c++Mf1c),
    Af = auv_matrix:cat_rows(Afu, Afl),
    Apu = auv_matrix:cols(N, 4, Mp1c++Mp2nc),
    Apl = auv_matrix:cols(N, 4, Mp2c++Mp1c),
    Ap = auv_matrix:cat_rows(Apu, Apl),
    U = auv_matrix:rows(4, 1, [[{1,Ua}], [{1,Ub}], [{1,Va}], [{1,Vb}]]),
    B = auv_matrix:mult(-1, auv_matrix:mult(Ap, U)),
    
    %% Solve A x = B in a Least SQares sense
    %% X becomes [ I x ] where x is the vector of unknown points
    %% I.e reduce and backsubstitute
    %%     [ (trans(Af) Af) (trans(Af) B) ]
    AfT = auv_matrix:trans(Af),
    AfTAf = auv_matrix:mult_trans(AfT, AfT),
    AfTB = auv_matrix:mult(AfT, B),
    AA = auv_matrix:cat_cols(AfTAf, AfTB),
    AAA = auv_matrix:reduce(AA),
    X = auv_matrix:backsubst(AAA),
    
    %% Extract the vector of previously unknown points,
    %% and insert the pinned points. Re-translate the
    %% original point identities.
    lsq_result(X, {Q1,{U1,V1}}, {Q2,{U2,V2}}, Rdict).



%% Extract all point identities from indata, and create
%% forwards and backwards dictionaries for translation
%% between point identity and point number.
%%
lsq_points(L) ->
    PL1 = 
	lists:foldl(
	  fun ({_,[{P1,_},{P2,_},{P3,_}]}, P) ->
		  [P1, P2, P3 | P];
	      (Invalid, _) ->
		  throw({error, {invalid_triangle, Invalid}})
	  end, [], L),
    PL2 = lists:usort(PL1),
    lists:foldl(
      fun (P, {N, D, DR}) ->
	      N1 = N+1,
	      {N1, dict:store(P, N1, D), dict:store(N1, P, DR)}
      end, {0, dict:new(), dict:new()}, PL2).



%% Create matrix M rows for all triangles in indata.
%% Return number of triangles, and lists for Re(M) and Im(M).
%%
lsq_triangles(L, Dict) ->
    {N, L1, L2} =
	lists:foldl(
	  fun ({_,[{P1,{X1,Y1}},{P2,{X2,Y2}},{P3,{X3,Y3}}]}, {N,Re,Im})
	      when number(X1), number(X2), number(X3),
		   number(Y1), number(Y2), number(Y3) ->
		  SqrtDT = math:sqrt(abs((X2-X1)*(Y3-Y1) - 
					 (Y2-Y1)*(X3-X1))),
		  W1re = auv_matrix:sub(X3, X2), 
		  W1im = auv_matrix:sub(Y3, Y2),
		  W2re = auv_matrix:sub(X1, X3), 
		  W2im = auv_matrix:sub(Y1, Y3),
		  W3re = auv_matrix:sub(X2, X1), 
		  W3im = auv_matrix:sub(Y2, Y1),
		  Q1 = dict:fetch(P1, Dict),
		  Q2 = dict:fetch(P2, Dict),
		  Q3 = dict:fetch(P3, Dict),
		      {N+1,
		       [[{Q1,W1re/SqrtDT}, 
			 {Q2,W2re/SqrtDT}, 
			 {Q3,W3re/SqrtDT}]
			| Re],
		       [[{Q1,W1im/SqrtDT}, 
			 {Q2,W2im/SqrtDT}, 
			 {Q3,W3im/SqrtDT}]
			| Im]};
		   (Invalid, _) ->
		  throw({error, {invalid_triangle, Invalid}})
	  end, {0, [],[]}, L),
    {N, lists:reverse(L1), lists:reverse(L2)}.



%% Extract the result from matrix X and combine it with the 
%% pinned points. Re-translate the point identities.
%%
lsq_result(X, QUV1, QUV2, Rdict) ->
    {_,MM} = auv_matrix:size(X),
    {_,UlistVlistR} =
	lists:foldl(
	  fun ([{J,1}, {M,H}], {J,R}) when M == MM ->
		  {J+1, [H | R]};
	      ([{J,1}], {J,R}) ->
		  {J+1, [0 | R]};
	      (Other, State) ->
		  throw({error, {?FILE, ?LINE, [Other, State, X]}})
	  end, {1, []}, auv_matrix:rows(X)),
    {Ulist, Vlist} = split(lists:reverse(UlistVlistR), MM div 2),
    {[],UVlistR} = 
	lists:foldl(
	  fun (U, {[V | L], R}) ->
		  {L, [{U,V} | R]};
	      (Other, State) ->
		  throw({error, {?FILE, ?LINE, [Other, State, X]}})
	  end, {Vlist, []}, Ulist),
    UVlist = insert(lists:reverse(UVlistR), lists:sort([QUV1, QUV2])),
    {_, TxMapR} =
	lists:foldl(
	  fun (UV, {Q,R}) ->
		  {Q+1,[{dict:fetch(Q, Rdict),UV} | R]}
	  end, {1,[]}, UVlist),
    {ok, lists:reverse(TxMapR)}.



%% Picks terms with specified indexes from a list.
%%
%% L: list of indexes in ascending order
%% P: list of terms
%%
%% Return: {P_remaining, P_picked}
%%
pick(L, P) 
  when list(L), list(P) ->
    case pick(1, L, P, [], []) of
	{_, _} = Ok ->
	    Ok;
	Fault ->
	    erlang:fault(Fault, [L, P])
    end;
pick(L, P) ->
    erlang:fault(badarg, [L, P]).

pick(_, L, [], R, Q) ->
    {lists:reverse(R, L), lists:reverse(Q)};
pick(_, [], _, _, _) ->
    badarg;
pick(_, _, [I, J | _], _, _) when I >= J ->
    badarg;
pick(I, [V | L], [I | P], R, Q) ->
    pick(I+1, L, P, R, [V | Q]);
pick(I, [V | L], P, R, Q) ->
    pick(I+1, L, P, [V | R], Q);
pick(_, _, _, _, _) ->
    badarg.



%% Insert terms with specified indexes in a list
%%
insert(L, S)
  when list(L), list(S) ->
    case insert(1, L, S, []) of
	R when list(R) ->
	    R;
	Fault ->
	    erlang:fault(Fault, [L, S])
    end;
insert(L, S) ->
    erlang:fault(badarg, [L, S]).

insert(_, L, [], R) ->
    lists:reverse(R, L);
insert(_, _, [{I,_}, {J,_} | _], _) when I >= J ->
    badarg;
insert(I, L, [{I,E} | S], R) ->
    insert(I+1, L, S, [E | R]);
insert(_, [], _, _) ->
    badarg;
insert(I, [E | L], S, R) ->
    insert(I+1, L, S, [E | R]).



%% Split a list into two after N terms
%%
split(L, N) ->
    split(L, N, []).

split([], _, R) ->
    {lists:reverse(R), []};
split(L, 0, R) ->
    {lists:reverse(R), L};
split([E | L], N, R) ->
    split(L, N-1, [E | R]).
