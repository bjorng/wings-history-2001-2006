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
%%     $Id: auv_mapping.erl,v 1.13 2002/10/18 21:57:29 dgud Exp $

%%%%%% Least Square Conformal Maps %%%%%%%%%%%%
%% Algorithms based on the paper, 
%% (now probably totally ruined by me or Raimo)
%% 'Least Square Conformal Maps for Automatic Texture Generation Atlas'
%% by Bruno Levy, Sylvain Petitjean, Nicolas Ray, Jerome Mailot
%% Presented on Siggraph 2002
%%
%%%% The Conjugate Gradient Method (trad)
%% Algorithms based on the paper:
%%       An Introduction to 
%%  the Conjugate Gradient Method
%%    Without the Agonizing Pain
%% by
%%  Jonathan Richard Shewchuk, March 7, 1994

%% All credits about the LSQCM implementation goes to Raimo, who
%% implemented the lot.

-module(auv_mapping).

-ifdef(lsq_standalone).
-export([lsq/2, lsq/4]).
-define(DBG(Fmt,Args), io:format(?MODULE_STRING++":~p: "++(Fmt), 
				       [?LINE | (Args)])).
-define(TC(Cmd), tc(?MODULE, ?LINE, fun () -> Cmd end)).
tc(Module, Line, Fun) ->
    {A1,A2,A3} = erlang:now(),
    Result = Fun(),
    {B1,B2,B3} = erlang:now(),
    Time = ((B1-A1)*1000000 + (B2-A2))*1000000 + B3-A3,
    io:format("~p:~p: ~.3f ms:~n", [Module, Line, Time/1000]),
    Result.
-else.

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
    Res.


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

project_and_triangulate([Face|Fs], We, I, Tris,Area) ->
    Normal = wings_face:normal(Face, We),
    Vs0 = wpa:face_vertices(Face, We),
    Vs2 = project2d(Vs0, Normal, We),
    NewArea = calc_area(Vs0, Normal, We) + Area,
    if length(Vs2) > 3 ->
	    {Ids,Cds,All} = setup_tri_vs(Vs2,0,[],[],[]),
	    NewFs = e3d_mesh:triangulate_face(#e3d_face{vs=Ids}, Cds),
	    VT = gb_trees:from_orddict(All),
	    {Add, I1} = get_verts(NewFs, I, VT, []),
	    project_and_triangulate(Fs,We,I1,Add ++ Tris,NewArea);
       true ->
	    Vs3 = [{Vid, {Vx, Vy}} || {Vid,{Vx,Vy,_}} <- Vs2],
	    project_and_triangulate(Fs,We,I,[{Face, Vs3}|Tris],NewArea)
    end;
project_and_triangulate([],_,_,Tris,Area) -> 
    {Tris,Area}.

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
    ?DBG("Project and tri ~n", []),
    {Vs1,Area} = ?TC(project_and_triangulate(Fs,We,-1,[],0.0)),    
    {V1, V2} = ?TC(find_pinned(C, We)),
    ?DBG("LSQ ~p ~p~n", [V1,V2]),
%    Idstr = lists:flatten(io_lib:format("~p", [Id])),
%    {ok, Fd} = file:open("raimo_" ++ Idstr, [write]),
%    io:format(Fd, "{~w, ~w, ~w}.~n", [Vs1,V1,V2]),
%    file:close(Fd),
    case ?TC(lsq(Vs1,V1,V2)) of
	{error, What} ->
	    ?DBG("TXMAP error ~p~n", [What]),
	    exit({txmap_error, What});
	{ok,Vs2} ->
%	    ?DBG("LSQ res ~p~n", [Vs2]),
	    Patch = fun({Idt, {Ut,Vt}}) -> {Idt,#vtx{pos={Ut,Vt,0.0}}} end,
	    Vs3 = lists:sort(lists:map(Patch, Vs2)),
	    TempVs = gb_trees:from_orddict(Vs3),
	    MappedArea = calc_2dface_area(Fs, We#we{vs=TempVs}, 0.0),	    
	    Scale = Area/MappedArea,
	    ?DBG("Scale Chart ~p ~n",[Scale]),
	    scaleVs(Vs3,Scale,[])
    end.

scaleVs([{Id, #vtx{pos={X,Y,_}}}|Rest],Scale,Acc) 
  when float(X), float(Y), float(Scale) ->
    scaleVs(Rest, Scale, [{Id, {X*Scale,Y*Scale,0.0}}|Acc]);
scaleVs([],_,Acc) ->
    Acc.

calc_2dface_area([Face|Rest],We,Area) ->
    Vs0 = wpa:face_vertices(Face, We),
    NewArea = calc_area(Vs0, {0.0,0.0,1.0}, We) + Area,
    calc_2dface_area(Rest,We,NewArea);
calc_2dface_area([],_,Area) ->
    Area.

find_pinned(C = {_Id, Faces}, We) ->
    {Circumference, BorderEdges} = 
	case auv_placement:group_edge_loops(Faces, We, false) of
	    [] -> 
		exit({invalid_chart, {C, is_closed_surface}});
	    [Best|_] ->
		Best
	end,
    Vs = [{(gb_trees:get(V1, We#we.vs))#vtx.pos,V1} || 
	     {V1,_,_,_} <-BorderEdges],
    [{_V1pos,V1}|_] = lists:sort(Vs),
    BE1 = reorder_edge_loop(V1, BorderEdges, []),
    {V2,Dx,Dy} = find_furthest_away(BE1,0.0,0.0,0.0,Circumference/2,We#we.vs),
    {{V1,{0.0,0.0}},{V2,{Dx,Dy}}}.
    
find_furthest_away([{V1,V2,_,_}|_], DX,DY, Dist, Max,_) 
  when float(Dist), float(Max), Dist >= Max ->
    {V1, math:sqrt(DX),math:sqrt(DY)};
find_furthest_away([{V1,V2,_,Delta}|Rest], DX0,DY0,Dist, Max,Vs) 
  when float(DX0),float(DY0),float(Delta), float(Dist) ->
    V1p = (gb_trees:get(V1,Vs))#vtx.pos,
    V2p = (gb_trees:get(V2,Vs))#vtx.pos,
    {Dx,Dy,Dz} = e3d_vec:sub(V2p, V1p),    
    find_furthest_away(Rest,DX0+Dx*Dx+Dz*Dz,DY0+Dy*Dy,Delta+Dist,Max,Vs).

reorder_edge_loop(V1, [{V1,_,_,_}|_] = Ordered, Acc) ->
    Ordered ++ lists:reverse(Acc);
reorder_edge_loop(V1, [H|Tail], Acc) ->
    reorder_edge_loop(V1, Tail, [H|Acc]).

-endif. % -ifdef(lsq_standalone). -else.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Least Square Conformal Maps %%%%%%%%%%%%

-ifdef(lsq_standalone).

lsq(Name, Method) ->
    {ok, [{L, P1, P2}]} = file:consult(Name),
    lsq(L, P1, P2, Method).

-else.

lsq(L, P1, P2) ->
    lsq(L, P1, P2, cg).

-endif. % -ifdef(lsq_standalone).

lsq(_, {P,_} = PUV1, {P,_} = PUV2, _) ->
    {error, {invalid_arguments, [PUV1, PUV2]}};
lsq(_, {_,{U1,V1}} = PUV1, {_,{U2,V2}} = PUV2, _)
  when U1 == U2, V1 == V2 -> % Coarse comparision, not match
    {error, {invalid_arguments, [PUV1, PUV2]}};
lsq(L, {_P1,{U1,V1}} = PUV1, {_P2,{U2,V2}} = PUV2, Method)
  when list(L), number(U1), number(V1), number(U2), number(V2) ->
    case catch lsq_int(L, PUV1, PUV2, Method) of
	{'EXIT', Reason} ->
	    exit(Reason);
	badarg ->
	    erlang:fault(badarg, [L, PUV1, PUV2]);
	Result ->
	    Result
    end;
lsq(L, P1, P2, Method) ->
    erlang:fault(badarg, [L, P1, P2, Method]).

lsq_int(L, {P1,{U1,V1}} = PUV1, {P2,{U2,V2}} = PUV2, Method) ->
    %% Create a dictionary and a reverse dictionary for all points
    %% in the indata. Translate the point identities into a
    %% continous integer sequence.
    {M, Dict, Rdict} = lsq_points(L),
    {N, L1, L2} = lsq_triangles(L, Dict, M),
    Q1 = case dict:find(P1, Dict) of 
	     {ok, Q_1} -> Q_1;
	     error -> throw({error, {invalid_arguments, [PUV1]}})
	 end,
    Q2 = case dict:find(P2, Dict) of 
	     {ok, Q_2} -> Q_2;
	     error -> throw({error, {invalid_arguments, [PUV2]}})
	 end,
    Q1uv = {Q1,{U1,V1}},
    Q2uv = {Q2,{U2,V2}},
    %% Build the basic submatrixes 
    %% M1 = Re(M), M2 = Im(M), M2n = -M2
    {M1,M2,M2n} = 
	?TC(build_basic(M,L1,L2)),
    %% Compile the basic submatrixes into the ones related to 
    %% free points (Mf*) i.e unknown, 
    %% and pinned points (Mp*).
    {{Mf1c,Mp1c},{Mf2c,Mp2c},{Mf2nc,Mp2nc},UVa,UVb} =
	?TC(build_cols(M1,M2,M2n,Q1uv,Q2uv)),
    %% Compose the matrix and vector to solve
    %% for a Least SQares solution.
    {Af,B} = 
	?TC(build_matrixes(N,M,Mf1c,Mp1c,Mf2c,Mp2c,Mf2nc,Mp2nc,UVa,UVb)),
    ?DBG("Solving matrixes~n", []),
    X = case Method of
	    cg ->
		X0 = auv_matrix:vector(lists:duplicate(M-2, (U1+U2)/2)++
				       lists:duplicate(M-2, (V1+V2)/2)),
		{_,X1} = ?TC(minimize_cg(Af, X0, B)),
		X1;
	    _ ->
		?TC(minimize(Af,B))
	end,
%%    ?DBG("X=~p~n", [X]),
    %% Extract the vector of previously unknown points,
    %% and insert the pinned points. Re-translate the
    %% original point identities.
    ?TC(lsq_result(X, Q1uv,Q2uv, Rdict)).

    
    
build_basic(M,L1,L2) ->
    M1 = auv_matrix:rows(M, L1),
    M2 = auv_matrix:rows(M, L2),
    M2n = auv_matrix:rows(M, [auv_matrix:mult(-1, X) || X <- L2]),
    {M1,M2,M2n}.

build_cols(M1,M2,M2n,Q1uv,Q2uv) ->
    %% Build column lists of the M matrixes
    M1c = auv_matrix:cols(M1),
    M2c = auv_matrix:cols(M2),
    M2nc = auv_matrix:cols(M2n),
    %% Split the column lists into free (Mf) and pinned (Mp)
    [{Qa,UVa}, {Qb,UVb}] = lists:sort([Q1uv,Q2uv]),
    QaQb = [Qa, Qb],
    {Mf1c,Mp1c} = pick(M1c, QaQb),
    {Mf2c,Mp2c} = pick(M2c, QaQb),
    {Mf2nc,Mp2nc} = pick(M2nc, QaQb),
    {{Mf1c,Mp1c},{Mf2c,Mp2c},{Mf2nc,Mp2nc},UVa,UVb}.

build_matrixes(N,_M,Mf1c,Mp1c,Mf2c,Mp2c,Mf2nc,Mp2nc,{Ua,Va},{Ub,Vb}) ->
    %% Build the matrixes Af and Ap, and vector B
    %% A = [ M1 -M2 ],  B = Ap U, U is vector of pinned points
    %%     [ M2  M1 ]
%    MM = 2*(M-2),
    Afu = auv_matrix:cols(N, Mf1c++Mf2nc),
    Afl = auv_matrix:cols(N, Mf2c++Mf1c),
    Af = auv_matrix:cat_rows(Afu, Afl),
    Apu = auv_matrix:cols(N, Mp1c++Mp2nc),
    Apl = auv_matrix:cols(N, Mp2c++Mp1c),
    Ap = auv_matrix:cat_rows(Apu, Apl),
    U = auv_matrix:vector(4, [{1,-Ua}, {2,-Ub}, {3,-Va}, {4,-Vb}]),
    B = auv_matrix:mult(Ap, U),
    {Af, B}.

%%               _   _    2
%% Minimize || A x - b ||  
%%
%%              t   _       t _
%% by solving A   A x  =  A   b
%%
%% using Gaussian Elimination and Back Substitution.
%%
minimize(A,B) ->
    AA = ?TC(mk_solve_matrix(A, B)),
    AAA = ?TC(auv_matrix:reduce(AA)),
    X   = ?TC(auv_matrix:backsubst(AAA)),
    ?DBG("Solved~n",[]),
    X.    

mk_solve_matrix(Af,B) ->
    AfT = auv_matrix:trans(Af),
    AfTAf = auv_matrix:mult_trans(AfT, AfT),
    AfTB = auv_matrix:mult(AfT, B),
    auv_matrix:cat_cols(AfTAf, AfTB).

%%               _   _    2
%% Minimize || A x - b ||  
%%
%%              t   _       t _
%% by solving A   A x  =  A   b
%%                                          __
%% using the Coujugate Gradient Method with x0 as 
%% iteration start vector.
%%
minimize_cg(A, X0, B) ->
    {N,M} = auv_matrix:size(A),
    {M,1} = auv_matrix:size(X0),
    {N,1} = auv_matrix:size(B),
    I = M,
    Epsilon = 1.0e-3,
    At = auv_matrix:trans(A),
    AtB = auv_matrix:mult(At, B),
    D = R = auv_matrix:sub(AtB, 
			   auv_matrix:mult(At, auv_matrix:mult(A, X0))),
    Delta = auv_matrix:mult(auv_matrix:trans(R), R),
    Delta_max = Epsilon*Epsilon*Delta,
    minimize_cg(At, A, AtB, Delta_max, 
		Delta, I, D, R, X0).

minimize_cg(At, A, _, _, 
	    _, 0, D, _, X) ->
    ?DBG("minimize_cg() sizes were ~p ~p ~p~n", 
	 [auv_matrix:size(At), auv_matrix:size(A), auv_matrix:size(D)]),
    {stopped, X};
minimize_cg(At, A, _, Delta_max, 
	    Delta, _, D, _, X) when Delta < Delta_max ->
    ?DBG("minimize_cg() sizes were ~p ~p ~p~n", 
	 [auv_matrix:size(At), auv_matrix:size(A), auv_matrix:size(D)]),
    {ok, X};
minimize_cg(At, A, AtB, Delta_max, 
	    Delta, I, D, R, X) ->
%%    ?DBG("minimize_cg() step ~p Delta=~p~n", [I, Delta]),
    AD = auv_matrix:mult(A, D),
    Q = auv_matrix:mult(At, AD),
    Alpha = Delta / auv_matrix:mult(auv_matrix:trans(AD), AD),
    X_next = auv_matrix:add(X, auv_matrix:mult(Alpha, D)),
    R_new = 
	if (I + 5) rem 10 == 0 ->
		?DBG("minimize_cg() recalculating residual ~p~n", [Delta]),
		auv_matrix:sub
		  (AtB, auv_matrix:mult(At, auv_matrix:mult(A, X_next)));
	   true ->
		auv_matrix:sub(R, auv_matrix:mult(Alpha, Q))
	end,
    Delta_new = auv_matrix:mult(auv_matrix:trans(R_new), R_new),
    {R_next, Delta_next} = 
	if Delta_new < Delta_max ->
		?DBG("minimize_cg() verifying residual ~p~n", [Delta]),
		R_n = 
		    auv_matrix:sub
		      (AtB, auv_matrix:mult(At, auv_matrix:mult(A, X_next))),
		{R_n, auv_matrix:mult(auv_matrix:trans(R_new), R_new)};
	   true ->
		{R_new, Delta_new}
	end,
    Beta = Delta_next / Delta,
    D_next = auv_matrix:add(R_next, auv_matrix:mult(Beta, D)),
    minimize_cg(At, A, AtB, Delta_max, 
		Delta_next, I-1, D_next, R_next, X_next).

    

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
lsq_triangles(L, Dict, M) ->
    {N, L1, L2} =
	lists:foldl(
	  fun ({_,[{P1,{X1,Y1}},{P2,{X2,Y2}},{P3,{X3,Y3}}]}, {N,Re,Im})
	      when number(X1), number(X2), number(X3),
		   number(Y1), number(Y2), number(Y3) ->
		  SqrtDT = math:sqrt(abs((X2-X1)*(Y3-Y1) - 
					 (Y2-Y1)*(X3-X1))),
		  W1re = X3-X2, W1im = Y3-Y2, 
		  W2re = X1-X3, W2im = Y1-Y3, 
		  W3re = X2-X1, W3im = Y2-Y1,
		  Q1 = dict:fetch(P1, Dict),
		  Q2 = dict:fetch(P2, Dict),
		  Q3 = dict:fetch(P3, Dict),
		  {N+1,
		   [auv_matrix:vector(M, [{Q1,W1re/SqrtDT}, 
					  {Q2,W2re/SqrtDT}, 
					  {Q3,W3re/SqrtDT}])
		    | Re],
		   [auv_matrix:vector(M, [{Q1,W1im/SqrtDT}, 
					  {Q2,W2im/SqrtDT}, 
					  {Q3,W3im/SqrtDT}])
		    | Im]};
	      (Invalid, _) ->
		  throw({error, {invalid_triangle, Invalid}})
	  end, {0, [],[]}, L),
    {N, lists:reverse(L1), lists:reverse(L2)}.



%% Extract the result from vector X and combine it with the 
%% pinned points. Re-translate the point identities.
%%
lsq_result(X, QUV1, QUV2, Rdict) ->
    {MM,1} = auv_matrix:size(X),
%     {_,UlistVlistR} =
% 	lists:foldl(
% 	  fun ({J,UV}, {J,R}) ->
% 		  {J+1, [UV | R]};
% 	      ({J2,_}, {J1,R}) ->
% 		  {J2+1, lists:duplicate(J2-J1, 0.0) ++ R};
% 	      (Other, State) ->
% 		  throw({error, {?FILE, ?LINE, [Other, State, X]}})
% 	  end, {1, []}, auv_matrix:vector(X)),
%     {Ulist, Vlist} = ?TC(split(lists:reverse(UlistVlistR), MM div 2)),
    {Ulist, Vlist} = ?TC(split(auv_matrix:vector(X), MM div 2)),
    {[],UVlistR} = 
	lists:foldl(
	  fun (U, {[], R}) ->
		  {[], [{U,0.0} | R]};
	      (U, {[V | L], R}) ->
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
