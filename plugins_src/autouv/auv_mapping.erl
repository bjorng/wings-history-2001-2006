%%
%%  auv_mapping.erl --
%%
%%     The UV parametrisation algorithms.
%%
%%  Copyright (c) 2002-2004 Dan Gudmundsson, Raimo Niskanen,
%%                     Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: auv_mapping.erl,v 1.65 2005/03/12 10:23:25 bjorng Exp $
%%

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
%%
%% The Column Norm Preconditioning was stumbled upon, just briefly
%% mentioned, in the paper:
%%      Incomplete Factorization Preconditioning
%%        for Linear Least Squares Problems
%% by
%%      Xiaoge Wang, 1994

%% All credits about the LSQCM implementation goes to Raimo, who
%% implemented the lot. 

-module(auv_mapping).

-export([lsq/2, lsq/3, find_pinned/2]). % Debug entry points
-export([stretch_opt/2, area2d2/3,area3d/3]).
-export([map_chart/3, project2d/3]).

%% Internal exports.
-export([model_l2/5]).

-include("wings.hrl").
-include("auv.hrl").
-include("e3d.hrl").

-import(lists, [foldl/3,reverse/1]).

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

map_chart(Type, We, Pinned) ->
    Faces = wings_we:visible(We),
    case wpa:face_outer_edges(Faces, We) of
	[] ->
	    {error,"A closed surface cannot be mapped. "
	     "(Either divide it into into two or more charts, "
	     "or cut it along some edges.)"};
	[[_,_]] ->
	    {error,"A cut in a closed surface must consist of at least two edges."};
	_ when is_list(Pinned), length(Pinned) < 2 ->
	    {error,"At least 2 vertices (per chart) must be selected"};
	[_] ->
	    map_chart_1(Type, Faces, Pinned, We);
	[_,_|_] ->
	    map_chart_1(Type, Faces, Pinned, We)
	    %% For the moment at least, allow holes.
            %%{error,"A chart is not allowed to have holes."}
    end.

map_chart_1(Type, Chart, Pinned, We) ->
    case catch map_chart_2(Type, Chart, Pinned, We) of
	{'EXIT',{badarith,_}} when Type == project ->
	    {error,"Numeric problem. (Probably impossible to calculate chart normal.)"};
	{'EXIT',{badarith,_}} ->
	    {error,"Numeric problem."};
	{'EXIT',Reason} ->
	    Msg = io_lib:format("Internal error: ~P", [Reason,10]),
	    {error,lists:flatten(Msg)};
	Other -> Other
    end.

map_chart_2(project, C, _, We) -> projectFromChartNormal(C, We);
map_chart_2(lsqcm, C, Pinned, We) -> lsqcm(C, Pinned, We).

projectFromChartNormal(Chart, We) ->
    CalcNormal = fun(Face, Sum) ->
			 Normal = wings_face:normal(Face, We),
			 Vs0 = wpa:face_vertices(Face, We),
			 Area = calc_area(Vs0,Normal, We),
			 %%         ?DBG("Area was ~p ~n", [Area]),
			 e3d_vec:add(Sum, e3d_vec:mul(Normal, Area))
		 end,
    N0 = foldl(CalcNormal, e3d_vec:zero(), Chart),
    Normal = e3d_vec:norm(N0),
    Vs0 = wings_face:to_vertices(Chart, We),
    project2d(Vs0, Normal, We).

project2d(Vs, Normal, We) ->
    Rot = e3d_mat:rotate_s_to_t(Normal,{0.0,0.0,1.0}),
    [{V,e3d_mat:mul_point(Rot, wings_vertex:pos(V, We))} || V <- Vs].

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

project_faces([Face|Fs], We, I, Tris,Area) ->
    Normal = wings_face:normal(Face, We),
    Vs0 = wpa:face_vertices(Face, We),
    Vs2 = project2d(Vs0, Normal, We),
    NewArea = calc_area(Vs0, Normal, We) + Area,
    case length(Vs2) of 
	3 ->
	    Vs3 = [{Vid, {Vx, Vy}} || {Vid,{Vx,Vy,_}} <- Vs2],
	    project_faces(Fs,We,I,[{Face, Vs3}|Tris],NewArea);
	_ ->
	    io:format("Error: Face isn't triangulated ~p with ~p vertices~n",
		      [Face, Vs0]),
	    erlang:error({triangulation_bug, [Face, Vs2]})
    end;
project_faces([],_,_,Tris,Area) -> 
    {Tris,Area}.

lsqcm(Fs, Pinned0, We) ->
    ?DBG("Project and tri ~n", []),
    TriWe = wings_tesselation:triangulate(Fs, We),
    TriFs = Fs ++ wings_we:new_items_as_ordset(face, We, TriWe),
    {Vs1,Area} = project_faces(TriFs,TriWe,-1,[],0.0),
    Pinned = case Pinned0 of
		 none -> 
		     {V1, V2} = find_pinned(Fs, We),
		     [V1,V2];
		 _ -> 
		     Pinned0
	     end,
    ?DBG("LSQ ~p~n", [Pinned]),
    case lsq(Vs1, Pinned) of
	{error, What} ->
	    ?DBG("TXMAP error ~p~n", [What]),
	    exit({txmap_error, What});
	{ok,Vs2} ->
	    %%?DBG("LSQ res ~p~n", [Vs2]),
	    Patch = fun({Idt, {Ut,Vt}}) -> {Idt,{Ut,Vt,0.0}} end,
	    Vs3 = lists:sort(lists:map(Patch, Vs2)),
	    TempVs = gb_trees:from_orddict(Vs3),
	    MappedArea = calc_2dface_area(TriFs, TriWe#we{vp=TempVs}, 0.0),
	    Scale = Area/MappedArea,
	    scaleVs(Vs3,math:sqrt(Scale),[])
    end.

scaleVs([{Id, {X,Y,_}}|Rest],Scale,Acc) 
  when is_float(X), is_float(Y), is_float(Scale) ->
    scaleVs(Rest, Scale, [{Id, {X*Scale,Y*Scale,0.0}}|Acc]);
scaleVs([],_,Acc) ->
    lists:reverse(Acc).

calc_2dface_area([Face|Rest],We,Area) ->
    Vs0 = wpa:face_vertices(Face, We),
    NewArea = calc_area(Vs0, {0.0,0.0,1.0}, We) + Area,
    calc_2dface_area(Rest,We,NewArea);
calc_2dface_area([],_,Area) ->
    Area.

find_border_edges(Faces, We) ->
    case auv_placement:group_edge_loops(Faces, We) of
	[] -> 
	    exit({invalid_chart, {Faces, is_closed_surface}});
	[Best|_] ->
	    Best
    end.
find_pinned(Faces, We) ->
    {Circumference, BorderEdges} = find_border_edges(Faces,We),
    find_pinned_from_edges(BorderEdges, Circumference, We).
find_pinned_from_edges(BorderEdges, Circumference, We) ->
    Vs = [gb_trees:get(V1, We#we.vp) || {V1,_,_,_} <-BorderEdges],
    Center = e3d_vec:average(Vs),
    AllC = lists:map(fun({Id,_,_,_}) ->
			     Pos = gb_trees:get(Id, We#we.vp),
			     Dist = e3d_vec:dist(Pos, Center),
			     {Dist, Id, Pos}
		     end, BorderEdges),
    [{_,V0,_V1Pos}|_] = lists:reverse(lists:sort(AllC)),
    BE1 = reorder_edge_loop(V0, BorderEdges, []),
    HalfCC = Circumference/2, %% - Circumference/100,
    {V1, V2} = find_pinned(BE1, BE1, 0.0, HalfCC, HalfCC, undefined), 
    {{V1,{0.0,0.0}},{V2,{1.0,1.0}}}.
    
find_pinned(Curr=[{C1,_,_,Clen}|CR],Start=[{_,S2,_,Slen}|SR],Len,HCC,Best,BVs) ->    
    Dlen = HCC-(Clen+Len),
    ADlen = abs(Dlen),
%    ?DBG("Testing ~p ~p ~p ~p ~p~n", [{S2,C1},Dlen,{Len+Clen,HCC}, Best, BVs]),    
    if 
	Dlen >= 0.0 ->
	    if ADlen < Best ->
		    find_pinned(CR,Start,Clen+Len,HCC,ADlen,{S2,C1});
	       true ->
		    find_pinned(CR,Start,Clen+Len,HCC,Best,BVs)
	    end;
	Dlen < 0.0 ->
	    if ADlen < Best ->
		    find_pinned(Curr,SR,Len-Slen,HCC, ADlen,{S2,C1});
	       true ->
		    find_pinned(Curr,SR,Len-Slen,HCC,Best,BVs)
	    end
    end;
find_pinned([], _, _, _, _Best, Bvs) ->
%    ?DBG("Found ~p ~p~n", [_Best, Bvs]),
    Bvs.

reorder_edge_loop(V1, [Rec={V1,_,_,_}|Ordered], Acc) ->
    Ordered ++ lists:reverse([Rec|Acc]);
reorder_edge_loop(V1, [H|Tail], Acc) ->
    reorder_edge_loop(V1, Tail, [H|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Least Square Conformal Maps %%%%%%%%%%%%

lsq(L, Lpuv) when list(Lpuv) ->
    lsq(L, Lpuv, env);
lsq(Name, Method) when atom(Method) ->
    {ok, [{L, Lpuv}]} = file:consult(Name),
    lsq(L, Lpuv, Method).


lsq(L, Lpuv, Method0) when is_list(L), is_list(Lpuv), is_atom(Method0) ->
    Method = case Method0 of 
		 env ->
		     case os:getenv("WINGS_AUTOUV_SOLVER") of
			 "ge" -> ge;
			 "cg" -> cg;
			 "cg_jacobian" -> cg_jacobian;
			 "cg_colnorm" -> cg_colnorm;
			 _ -> cg_colnorm
		     end;
		 M -> M
	     end,
    try lsq_int(L, Lpuv, Method)
    catch
	error:badarg ->
	    erlang:error(badarg, [L,Lpuv,Method])
    end;
lsq(L, Lpuv, Method) ->
    erlang:error(badarg, [L, Lpuv, Method]).

lsq_int(L, Lpuv, Method) ->
    %% Create a dictionary and a reverse dictionary for all points
    %% in the indata. Translate the point identities into a
    %% continous integer sequence.
    ?DBG("lsq_int - Lpuv = ~p~n", [Lpuv]),
    {M,Dict,Rdict} = lsq_points(L),
    {N,L1,L2} = lsq_triangles(L, Dict, M),
    Lquv = 
	lists:sort( % Must be sorted for pick() and insert() to work.
	  lists:map(
	    fun ({P,{U,V} = UV} = PUV) when number(U), number(V) ->
		    case dict:find(P, Dict) of
			{ok,Q} -> {Q,UV};
			error -> throw({error,{invalid_arguments,[PUV]}})
		    end;
		(PUV) ->
		    throw({error,{invalid_arguments,[PUV]}})
	    end,
	    Lpuv)),
    ?DBG("lsq_int - Lquv = ~p~n", [Lquv]),
    {Np,Usum,Vsum} =
	lists:foldl(
	  fun ({_,{U,V}}, {Np1,Usum1,Vsum1}) ->
		  {Np1+1,Usum1+U,Vsum1+V}
	  end,
	  {0,0,0}, Lquv),
    %% Build the basic submatrixes 
    %% M1 = Re(M), M2 = Im(M), M2n = -M2
    {M1,M2,M2n} = build_basic(M,L1,L2),
    %% Compile the basic submatrixes into the ones related to 
    %% free points (Mf*) i.e unknown, 
    %% and pinned points (Mp*).
    {Mfp1c,Mfp2c,Mfp2nc,LuLv} = build_cols(M1,M2,M2n,Lquv),
    ?DBG("lsq_int - LuLv = ~p~n", [LuLv]),
    %% Compose the matrix and vector to solve
    %% for a Least SQares solution.
    {Af,B} = build_matrixes(N,Mfp1c,Mfp2c,Mfp2nc,LuLv),
    ?DBG("Solving matrices~n", []),
    X = case Method of
	    ge -> minimize_ge(Af, B);
	    _ ->
		X0 = auv_matrix:vector(lists:duplicate(M-Np, Usum/Np)++
				       lists:duplicate(M-Np, Vsum/Np)),
		{_,X1} = minimize_cg(Af, X0, B),
		X1
	end,
%%    ?DBG("X=~p~n", [X]),
    %% Extract the vector of previously unknown points,
    %% and insert the pinned points. Re-translate the
    %% original point identities.
    lsq_result(X, Lquv, Rdict).


build_basic(M,L1,L2) ->
    M1 = auv_matrix:rows(M, L1),
    M2 = auv_matrix:rows(M, L2),
    M2n = auv_matrix:rows(M, [auv_matrix:mult(-1, X) || X <- L2]),
    {M1,M2,M2n}.

build_cols(M1,M2,M2n,Lquv) ->
    %% Build column lists of the M matrixes
    M1c = auv_matrix:cols(M1),
    M2c = auv_matrix:cols(M2),
    M2nc = auv_matrix:cols(M2n),
    %% Split the column lists into free (Mf) and pinned (Mp)
    {Lq,Lu,Lv} = split_quv(Lquv), % Lquv is sorted
    {pick(M1c, Lq),pick(M2c, Lq),pick(M2nc, Lq), Lu++Lv}.

split_quv(Lquv) ->
    split_quv(Lquv, [], [], []).

split_quv([], Lq, Lu, Lv) ->
    {lists:reverse(Lq),lists:reverse(Lu),lists:reverse(Lv)};
split_quv([{Q,{U,V}} | Lquv], Lq, Lu, Lv) ->
    split_quv(Lquv, [Q | Lq], [U | Lu], [V | Lv]).

build_matrixes(N,{Mf1c,Mp1c},{Mf2c,Mp2c},{Mf2nc,Mp2nc},LuLv) ->
    %% Build the matrixes Af and Ap, and vector B
    %% A = [ M1 -M2 ],  B = Ap U, U is vector of pinned points
    %%     [ M2  M1 ]
    Afu = auv_matrix:cols(N, Mf1c++Mf2nc),
    Afl = auv_matrix:cols(N, Mf2c++Mf1c),
    Af = auv_matrix:cat_rows(Afu, Afl),
    Apu = auv_matrix:cols(N, Mp1c++Mp2nc),
    Apl = auv_matrix:cols(N, Mp2c++Mp1c),
    Ap = auv_matrix:cat_rows(Apu, Apl),
    {Np,K_LuLv} = keyseq_neg(LuLv),
    U = auv_matrix:vector(Np, K_LuLv),
    ?DBG("build_matrixes - U = ~p~n", [U]),
    B = auv_matrix:mult(Ap, U),
    {Af, B}.

keyseq_neg(L) ->
    keyseq(1, L, []).

keyseq(N, [], R) ->
    {N-1,lists:reverse(R)};
keyseq(N, [X | L], R) ->
    keyseq(N+1, L, [{N,-X} | R]).

%%               _   _    2
%% Minimize || A x - b ||  
%%
%%              t   _       t _
%% by solving A   A x  =  A   b
%%
%% using Gaussian Elimination and back substitution.
%%
minimize_ge(A, B) ->
    AA = mk_solve_matrix(A, B),
    AAA = auv_matrix:reduce(AA),
%%    ?DBG("Reduced: ~p~n", [AAA]),
    X = auv_matrix:backsubst(AAA),
    ?DBG("Solved~n",[]),
    X.    

mk_solve_matrix(Af,B) ->
    AfT = auv_matrix:trans(Af),
    AfTAf = auv_matrix:mult_trans(AfT, AfT),
    AfTB = auv_matrix:mult(-1, auv_matrix:mult(AfT, B)),
    auv_matrix:cat_cols(AfTAf, AfTB).

%%               _   _    2
%% Minimize || A x - b ||  
%%
%%             -1  t    _      -1  t  _
%% by solving M   A   A x  =  M   A   b
%%                                                         __
%% using the Preconditioned Coujugate Gradient method with x0 as 
%% iteration start vector.
%%
minimize_cg(A, X0, B) ->
    ?DBG("minimize_cg - dim A=~p X0=~p B=~p~n",
	 [auv_matrix:dim(A), auv_matrix:dim(X0), auv_matrix:dim(B)]),
    {N,M} = auv_matrix:dim(A),
    {M,1} = auv_matrix:dim(X0),
    {N,1} = auv_matrix:dim(B),
    I = M,
    Epsilon = 1.0e-3,
    At = auv_matrix:trans(A),
    AtB = auv_matrix:mult(At, B),

    %% A very cheap preconditioning. The column norm
    %% takes no time to calculate compared to 
    %% AtA above. The iteration time impact is also 
    %% very low since it is a matrix multiplication
    %% with a diagonal (i.e. very sparse) matrix.
    %% The preconditioning effect (on the required
    %% number of iterations) is modest, but 
    %% cost effective.
    Diag = auv_matrix:row_norm(At),
    M_inv = try [1/V || V <- Diag] of
		Diag_inv ->
		    M_i = auv_matrix:diag(Diag_inv),
		    fun (R_new) ->
			    auv_matrix:mult(M_i, R_new)
		    end
	    catch
		error:badarith ->
		    fun (R_new) ->
			    auv_matrix:mult(1, R_new)
		    end
	    end,
    R = auv_matrix:sub(AtB, auv_matrix:mult(At, auv_matrix:mult(A, X0))),
    D = M_inv(R),
    Delta = auv_matrix:mult(auv_matrix:trans(R), D),
    Delta_max = Epsilon*Epsilon*Delta,
    minimize_cg(M_inv, At, A, AtB, Delta_max, 
		Delta, I, D, R, X0).

minimize_cg(_, _At, _A, _, _, 
	    _, 0, _D, _, X) ->
    ?DBG("minimize_cg() sizes were ~p ~p ~p~n", 
	 [auv_matrix:dim(_At), auv_matrix:dim(_A), auv_matrix:dim(_D)]),
    {stopped, X};
minimize_cg(_, _At, _A, _, Delta_max, 
	    Delta, _, _D, _, X) when Delta < Delta_max ->
    ?DBG("minimize_cg() sizes were ~p ~p ~p~n", 
	 [auv_matrix:dim(_At), auv_matrix:dim(_A), auv_matrix:dim(_D)]),
    {ok, X};
minimize_cg(M_inv, At, A, AtB, Delta_max, 
	    Delta, I, D, R, X) ->
%%    ?DBG("minimize_cg() step ~p Delta=~p~n", [I, Delta]),
    P = auv_matrix:mult(A, D),
    Alpha = Delta / auv_matrix:mult(auv_matrix:trans(P), P),
    X_new = auv_matrix:add(X, auv_matrix:mult(Alpha, D)),
    if (I + 5) rem 10 == 0 ->
	    minimize_cg_3(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, X_new);
       true ->
	    minimize_cg_2(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, R, X_new, Alpha, P)
    end.

minimize_cg_2(M_inv, At, A, AtB, Delta_max,
	      Delta, I, D, R, X_new, Alpha, P) ->
    R_new = auv_matrix:sub(R, auv_matrix:mult(Alpha, auv_matrix:mult(At, P))),
    S = M_inv(R_new),
    Delta_new = auv_matrix:mult(auv_matrix:trans(R_new), S),
    if Delta_new < Delta_max ->
	    minimize_cg_3(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, X_new);
       true ->
	    D_new = auv_matrix:add(S, auv_matrix:mult(Delta_new/Delta, D)),
	    minimize_cg(M_inv, At, A, AtB, Delta_max,
			Delta_new, I-1, D_new, R_new, X_new)
    end.

minimize_cg_3(M_inv, At, A, AtB, Delta_max,
	      Delta, I, D, X_new) ->
    ?DBG("minimize_cg() recalculating residual ~p~n", [Delta]),
    R_new = auv_matrix:sub
	      (AtB, auv_matrix:mult(At, auv_matrix:mult(A, X_new))),
    S = M_inv(R_new),
    Delta_new = auv_matrix:mult(auv_matrix:trans(R_new), S),
    D_new = auv_matrix:add(S, auv_matrix:mult(Delta_new/Delta, D)),
    minimize_cg(M_inv, At, A, AtB, Delta_max,
		Delta_new, I-1, D_new, R_new, X_new).



%% Extract all point identities from indata, and create
%% forwards and backwards dictionaries for translation
%% between point identity and point number.
%%
lsq_points(L) ->
    PL1 = 
	foldl(
	  fun ({_,[{P1,_},{P2,_},{P3,_}]}, P) ->
		  [P1, P2, P3 | P];
	      (Invalid, _) ->
		  throw({error, {invalid_triangle, Invalid}})
	  end, [], L),
    PL2 = lists:usort(PL1),
    foldl(
      fun (P, {N, D, DR}) ->
	      N1 = N+1,
	      {N1, dict:store(P, N1, D), dict:store(N1, P, DR)}
      end, {0, dict:new(), dict:new()}, PL2).



%% Create matrix M rows for all triangles in indata.
%% Return number of triangles, and lists for Re(M) and Im(M).
%%
lsq_triangles(L, Dict, M) ->
    {N, L1, L2} =
	foldl(
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
lsq_result(X, Lquv, Rdict) ->
    {MM,1} = auv_matrix:dim(X),
    {Ulist, Vlist} = split(auv_matrix:vector(X), MM div 2),
    {[],UVlistR} = 
	foldl(
	  fun (U, {[], R}) ->
		  {[], [{U,0.0} | R]};
	      (U, {[V | L], R}) ->
		  {L, [{U,V} | R]};
	      (Other, State) ->
		  throw({error, {?FILE, ?LINE, [Other, State, X]}})
	  end, {Vlist, []}, Ulist),
    UVlist = insert(lists:reverse(UVlistR), Lquv),
    {_, TxMapR} =
	foldl(
	  fun (UV, {Q,R}) ->
		  {Q+1,[{dict:fetch(Q, Rdict),UV} | R]}
	  end, {1,[]}, UVlist),
    TxMap = lists:reverse(TxMapR),
    ?DBG("lsq_result - TxMap = ~p~n", [TxMap]),
    {ok, TxMap}.



%% Picks terms with specified indexes from a list.
%%
%% L: list of terms
%% P: list of indexes in ascending order
%%
%% Return: {L_remaining, L_picked}
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
%% L: List of terms
%% S: List of {Pos,Term} tuples with Term to be 
%%    inserted at position Pos in L
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

area2d2({S1,T1},{S2,T2},{S3,T3})
  when is_float(S1),is_float(S2),is_float(S3),
       is_float(T1),is_float(T2),is_float(T3) ->
    ((S2-S1)*(T3-T1)-(S3-S1)*(T2-T1)).

area3d(V1, V2, V3) ->
    e3d_vec:area(V1, V2, V3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Texture metric stretch 
%% From 'Texture Mapping Progressive Meshes' by
%% Pedro V. Sander, John Snyder Steven J. Gortler, Hugues Hoppe

-record(s,{f2v,   % Face 2 vertex id
	   v2f,   % Vertex 2 faces
	   f2a,   % Face(s) 3d area
	   f2ov,  % Face original vertex 3d position
	   bv     % Border vertices
	  }).

-define(MIN_STRETCH, 1.01).
-define(MAX_ITER, 100).
-define(MAX_LEVELS, 6).
-define(VERTEX_STEP, 0.001).

stretch_opt(We0, OVs) ->
    Fs = wings_we:visible(We0),
    wings_pb:start("optimizing"),
    wings_pb:update(0.01, "initializing"),

    {_,R1,R2} = now(),
    random:seed(R2, R1, 128731),

    %% {FaceToStretchMean, FaceToStretchWorst,FaceToVerts,VertToFaces,VertToUvs}
    {F2S2,_F2S8,Uvs,State,Scale} = stretch_setup(Fs,We0,OVs),

    V2S = stretch_per_vertex(gb_trees:to_list(State#s.v2f),F2S2,State,gb_trees:empty()),
    S2V = lists:reverse(lists:keysort(2,gb_trees:to_list(V2S))),
    {SUvs0,_F2S2} = wings_pb:done(stretch_iter(S2V,1,V2S,F2S2,Uvs,State)),
    %% Verify
    _Mean2  = model_l2(gb_trees:keys(_F2S2), _F2S2, State#s.f2a,0.0, 0.0),
    io:format("After Stretch sum (mean) ~p ~n",  [_Mean2]),

    SUvs1 = gb_trees:to_list(SUvs0),
    
    Suvs = [{Id,{S0/Scale,T0/Scale,0.0}} || {Id,{S0,T0}} <- SUvs1],
    We0#we{vp=gb_trees:from_orddict(Suvs)}.

stretch_setup(Fs, We0, OVs) ->
    Be = wings_face:outer_edges(Fs, We0),
    Bv0 = foldl(fun(Edge, Acc) ->
			#edge{vs=Vs,ve=Ve} = gb_trees:get(Edge, We0#we.es),
			[Vs,Ve|Acc]
		end, [], Be),
    Bv = gb_sets:from_list(Bv0),
    Tris0 = triangulate(Fs,We0),
    {S,F2A,F2OV} = calc_scale(Tris0, OVs, 0.0, 0.0, [], []),
    Tris = [{Face,[{Id1,{S1*S,T1*S}},{Id2,{S2*S,T2*S}},{Id3,{S3*S,T3*S}}]} ||
	       {Face,[{Id1,{S1,T1}},{Id2,{S2,T2}},{Id3,{S3,T3}}]} <- Tris0],
    {F2S2,F2S8,Uvs,State0} = init_stretch(Tris,F2OV, [], [], [], [], []),
    Worst = model_l8(gb_trees:keys(F2S8), F2S8, 0.0), 
    Mean  = model_l2(gb_trees:keys(F2S2), F2S2, F2A,0.0, 0.0),
    io:format("Stretch sum (worst) ~p ~n", [Worst]),
    io:format("Stretch sum (mean) ~p ~n",  [Mean]),
    {F2S2,F2S8,Uvs,State0#s{f2a=F2A,f2ov=F2OV,bv=Bv},S}.

stretch_iter(S2V0=[{_,First}|_],I,V2S0,F2S20,Uvs0,State) 
  when First > ?MIN_STRETCH, I < ?MAX_ITER ->
    if
	I rem 4 =:= 0 ->
	    wings_pb:update(I/?MAX_ITER, "iteration "++integer_to_list(I));
	true ->
	    ok
    end,
    {V2S,F2S2,Uvs} = stretch_iter2(S2V0,V2S0,F2S20,Uvs0,State),
    S2V = lists:reverse(lists:keysort(2, gb_trees:to_list(V2S))),
    stretch_iter(S2V,I+1,V2S,F2S2,Uvs,State);
stretch_iter(_,_,_,F2S2,Uvs,_) ->
    {Uvs,F2S2}.

stretch_iter2([{V,OldVal}|R],V2S0,F2S20,Uvs0,State)
  when OldVal > ?MIN_STRETCH ->
    Line = random_line(),
    #s{f2v=F2Vs,v2f=V2Fs} = State,
    Fs   = gb_trees:get(V,V2Fs),
    Val  = gb_trees:get(V,V2S0),
    %%	    ?DBG("~p ~.4f:",[V,Val]), 
    {PVal,Uvs,F2S2} = opt_v(Val,0,?VERTEX_STEP,V,Line,Fs,F2S20,Uvs0,State),
    case PVal == Val of 
	true -> 
	    stretch_iter2(R,V2S0,F2S20,Uvs0,State);
	false ->
	    Vs0  = lists:usort(lists:append([gb_trees:get(F,F2Vs)|| F<-Fs])),
	    Upd0 = foldl(fun(Vtx, New) ->
				 [{Vtx,gb_trees:get(Vtx, V2Fs)}|New]
			 end, [], Vs0),
	    V2S = stretch_per_vertex(Upd0,F2S2,State,V2S0),
	    stretch_iter2(R,V2S,F2S2,Uvs,State)
    end;
stretch_iter2(_,V2S,F2S2,Uvs,_) ->
    {V2S, F2S2, Uvs}.

random_line() ->
    X   = random:uniform()-0.5,
    Y   = random:uniform()-0.5,
    Len = math:sqrt(X*X+Y*Y),
    {X/Len,Y/Len}.

opt_v(PVal,I,Step,V,L,Fs,F2S0,Uvs0,_State=#s{f2v=F2Vs,f2ov=F2OV,f2a=F2A}) ->    
    UV = gb_trees:get(V, Uvs0),
    {Data,F2S1} = 
	foldl(fun(Face, {Acc,Fs0}) ->
		      Vs = [V1,V2,V3] = gb_trees:get(Face, F2Vs),
		      {[{Vs,
			 {gb_trees:get(V1,Uvs0),
			  gb_trees:get(V2,Uvs0),
			  gb_trees:get(V3,Uvs0)},
			 gb_trees:get(Face, F2OV),
			 Face,
			 gb_trees:get(Face, F2A)}|Acc],
		       [{Face,gb_trees:get(Face,F2S0)}|Fs0]}
	      end, {[],[]}, Fs),
    {Stretch,St,F2S2} = opt_v2(PVal,I,Step,V,UV,L,Data,F2S1),
    case Stretch < PVal of
	true ->
	    F2S = update_fs(F2S2,F2S0),
	    {Stretch,gb_trees:update(V,St,Uvs0),F2S};
	false ->
	    {PVal,Uvs0,F2S0}
    end.

update_fs([{Face,S}|Ss],F2S) ->
    update_fs(Ss,gb_trees:update(Face,S,F2S));
update_fs([],F2S) -> F2S.

opt_v2(PVal,I,Step,V,UV={S0,T0},L={X,Y},Data,FS0) 
  when I < ?MAX_LEVELS ->
    St = {S0+X*Step,T0+Y*Step},
    {Stretch,FS} = calc_stretch(V,Data,St,0.0,0.0,[]),
    if 
	Stretch < PVal ->
%	    io:format(">"),
	    opt_v2(Stretch,I,Step,V,St,L,Data,FS);
	(I rem 2) == 0 ->
%	    io:format("S"),
	    opt_v2(PVal,I+1,-Step*0.9,V,UV,L,Data,FS0);
	true ->
%	    io:format("<"),
	    NewStep = Step/10,
	    opt_v2(PVal,I+1,NewStep,V,UV,L,Data,FS0)
    end;
opt_v2(PVal,_I,_Step,_V,St,_L,_,FS) ->
%    io:format("~n"),
    {PVal,St,FS}.
       
calc_stretch(V,[{[V,_,_],{_,UV2,UV3},{Q1,Q2,Q3},Face,FA}|R],UV1,Mean,Area,FS) ->
    S = l2(UV1,UV2,UV3,Q1,Q2,Q3),
    calc_stretch(V,R,UV1,S*S*FA+Mean,FA+Area,[{Face,S}|FS]);
calc_stretch(V,[{[_,V,_],{UV1,_,UV3},{Q1,Q2,Q3},Face,FA}|R],UV2,Mean,Area,FS) ->
    S = l2(UV1,UV2,UV3,Q1,Q2,Q3),
    calc_stretch(V,R,UV2,S*S*FA+Mean,FA+Area,[{Face,S}|FS]);
calc_stretch(V,[{[_,_,V],{UV1,UV2,_},{Q1,Q2,Q3},Face,FA}|R],UV3,Mean,Area,FS) ->
    S = l2(UV1,UV2,UV3,Q1,Q2,Q3),
    calc_stretch(V,R,UV3,S*S*FA+Mean,FA+Area,[{Face,S}|FS]);
calc_stretch(_,[],_,Mean,Area,FS) ->
    {math:sqrt(Mean/Area),reverse(FS)}.

stretch_per_vertex([{V,Fs}|R],F2S,State=#s{bv=Bv,f2a=F2A},Tree) ->
    case gb_sets:is_member(V,Bv) of
	false ->
	    Res = model_l2(Fs,F2S,F2A,0.0,0.0),
	    stretch_per_vertex(R,F2S,State,gb_trees:enter(V,Res,Tree));
	true ->
	    stretch_per_vertex(R,F2S,State,Tree)
    end;
stretch_per_vertex([], _, _,Acc) ->
    Acc.

init_stretch([{Face,FUvs=[{Id1,P1},{Id2,P2},{Id3,P3}]}|R],
	     Ovs,F2S2,F2S8,F2Vs,V2Fs,UVs) ->
    {Q1,Q2,Q3} = gb_trees:get(Face,Ovs),
    S2 = l2(P1,P2,P3,Q1,Q2,Q3),
    S8 = l8(P1,P2,P3,Q1,Q2,Q3),
    init_stretch(R,Ovs, [{Face,S2}|F2S2],[{Face,S8}|F2S8],
		 [{Face, [Id1,Id2,Id3]}|F2Vs],
		 [{Id1,Face},{Id2,Face},{Id3,Face}|V2Fs],
		 FUvs ++ UVs);
init_stretch([],_,F2S2,F2S8,F2Vs,V2Fs0,Uvs) ->
    V2Fs1 = sofs:relation(V2Fs0),
    V2Fs2 = sofs:relation_to_family(V2Fs1),
    V2Fs = sofs:to_external(V2Fs2),
    {gb_trees:from_orddict(lists:sort(F2S2)),
     gb_trees:from_orddict(lists:sort(F2S8)),
     gb_trees:from_orddict(lists:usort(Uvs)),
     #s{f2v = gb_trees:from_orddict(lists:sort(F2Vs)),
	v2f = gb_trees:from_orddict(V2Fs)}}.

calc_scale([{Face,[{Id1,P1},{Id2,P2},{Id3,P3}]}|R], Ovs, A2D, A3D,F2A,F2OVs) ->
    A2 = abs(area2d2(P1,P2,P3)/2),
    Q1 = gb_trees:get(Id1,Ovs),
    Q2 = gb_trees:get(Id2,Ovs),
    Q3 = gb_trees:get(Id3,Ovs),    
    A3 = area3d(Q1,Q2,Q3),
    calc_scale(R,Ovs,A2+A2D,A3+A3D,[{Face,A3}|F2A],[{Face,{Q1,Q2,Q3}}|F2OVs]);
calc_scale([],_Ovs,A2D,A3D,F2A,F2OVs) ->
    {math:sqrt(A3D/A2D), 
     gb_trees:from_orddict(lists:sort(F2A)), 
     gb_trees:from_orddict(lists:sort(F2OVs))}.

model_l8([Face|R], F2S8, Worst) ->
    FVal = gb_trees:get(Face,F2S8),
    New  = if FVal > Worst -> 
%		   ?DBG("Face ~p has worst ~p~n", [Face,FVal]),
		   FVal;
	      true -> 
		   Worst
	   end,
    model_l8(R,F2S8,New);
model_l8([], _, Worst) -> Worst.

model_l2([Face|R], F2S2, F2A, Mean, Area)  ->
    TriM = gb_trees:get(Face,F2S2),
    case gb_trees:get(Face,F2A) of
	A when is_float(TriM), is_float(A) ->
	    model_l2(R,F2S2,F2A,TriM*TriM*A+Mean,Area+A)
    end;
model_l2([],_,_,Mean,Area) ->
    math:sqrt(Mean/Area).

l2({S1,T1}, {S2,T2}, {S3,T3},
   {Q1x,Q1y,Q1z}, {Q2x,Q2y,Q2z}, {Q3x,Q3y,Q3z})
  when is_float(S1), is_float(S2), is_float(S3),
       is_float(T1), is_float(T2), is_float(T3),
       is_float(Q1x), is_float(Q1y), is_float(Q1z),
       is_float(Q2x), is_float(Q2y), is_float(Q2z),
       is_float(Q3x), is_float(Q3y), is_float(Q3z) ->
    T23 = T2-T3,    T31 = T3-T1,    T12 = T1-T2,
    S32 = S3-S2,    S13 = S1-S3,    S21 = S2-S1,
    case S21*T31-S13*T12 of
	DoubleArea when DoubleArea > 0.00000001 ->
	    SX = Q1x*T23+Q2x*T31+Q3x*T12,
	    SY = Q1y*T23+Q2y*T31+Q3y*T12,
	    SZ = Q1z*T23+Q2z*T31+Q3z*T12,
	    A = SX*SX+SY*SY+SZ*SZ,

	    TX = Q1x*S32+Q2x*S13+Q3x*S21,
	    TY = Q1y*S32+Q2y*S13+Q3y*S21,
	    TZ = Q1z*S32+Q2z*S13+Q3z*S21,
	    C = TX*TX+TY*TY+TZ*TZ,

	    math:sqrt((A+C)/(2.0*DoubleArea*DoubleArea));
	_ -> 
	    9999999999.9
    end.

l8(P1,P2,P3,Q1,Q2,Q3) ->  %% Worst stretch value
    A2 = area2d2(P1,P2,P3),
    if A2 > 0.00000001 ->
	    SS = ss(P1,P2,P3,Q1,Q2,Q3,A2),
	    ST = st(P1,P2,P3,Q1,Q2,Q3,A2),
	    A = e3d_vec:dot(SS,SS),
	    B = e3d_vec:dot(SS,ST),
	    C = e3d_vec:dot(ST,ST),
	    math:sqrt(0.5*((A+C)+math:sqrt((A-C)*(A-C)+4*B*B)));
       true ->
	    9999999999.9
    end.
    
ss({_,T1},{_,T2},{_,T3},{Q1x,Q1y,Q1z},{Q2x,Q2y,Q2z},{Q3x,Q3y,Q3z},A) 
  when is_float(T1),is_float(T2),is_float(T3),
       is_float(Q1x),is_float(Q1y),is_float(Q1z),
       is_float(Q2x),is_float(Q2y),is_float(Q2z),
       is_float(Q3x),is_float(Q3y),is_float(Q3z) ->
    T23 = T2-T3,    T31 = T3-T1,    T12 = T1-T2,
    {(Q1x*T23+Q2x*T31+Q3x*T12)/A,
     (Q1y*T23+Q2y*T31+Q3y*T12)/A,
     (Q1z*T23+Q2z*T31+Q3z*T12)/A}.
    
st({S1,_},{S2,_},{S3,_},{Q1x,Q1y,Q1z},{Q2x,Q2y,Q2z},{Q3x,Q3y,Q3z},A) 
  when is_float(S1),is_float(S2),is_float(S3),
       is_float(Q1x),is_float(Q1y),is_float(Q1z),
       is_float(Q2x),is_float(Q2y),is_float(Q2z),
       is_float(Q3x),is_float(Q3y),is_float(Q3z) ->
    S32 = S3-S2,    S13 = S1-S3,    S21 = S2-S1,
    {(Q1x*S32+Q2x*S13+Q3x*S21)/A,
     (Q1y*S32+Q2y*S13+Q3y*S21)/A,
     (Q1z*S32+Q2z*S13+Q3z*S21)/A}.

triangulate(Fs,We) ->
    TriWe = wings_tesselation:triangulate(Fs, We),
    TriFs = Fs ++ wings_we:new_items_as_ordset(face, We, TriWe),
    get_face_vspos(TriFs,TriWe, []).

get_face_vspos([Face|Fs], We, Tris) ->
    Vs0 = wpa:face_vertices(Face, We),
    Vs1 = [{V,wings_vertex:pos(V,We)} || V <- Vs0],
    if length(Vs0) == 3 ->
	    Vs2 = [{Vid, {Vx, Vy}} || {Vid,{Vx,Vy,_}} <- Vs1],
	    get_face_vspos(Fs,We,[{Face, Vs2}|Tris]);
       true ->
	    io:format("Error: Face isn't triangulated ~p with ~p vertices~n",
		      [Face, Vs1]),
	    erlang:error({triangulation_bug, [Face, Vs1]})    
    end;
get_face_vspos([], _, Tris) ->
    Tris.
