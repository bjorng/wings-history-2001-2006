%%%-------------------------------------------------------------------
%%% File    : auv_matrix.erl
%%% Author  : Raimo Niskanen
%%% Description : Provides Matrix ops for sparse populated matrixes
%%%
%%% Created :  4 Oct 2002 by Raimo Niskanen
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002, Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: auv_matrix.erl,v 1.8 2002/10/19 19:06:11 raimo_niskanen Exp $

-module(auv_matrix).

-export([dim/1]).
-export([vector/1, vector/2]).
-export([rows/1, rows/2, cols/1, cols/2]).
-export([cat_cols/2, cat_rows/2]).
-export([trans/1, mult/2, mult_trans/2]).
-export([add/2, sub/2]).
-export([reduce/1, backsubst/1]).

-export([float_perf/2]).

-define(TAG, ?MODULE).

-compile(inline).
-compile({inline_size, 100}).


%% Exported
%%
dim({?TAG,N,M,_}) ->
    {N,M};
dim({?TAG,N,_}) ->
    {N,1};
dim(V) when number(V) ->
    {1,1};
dim(A) ->
    erlang:fault(badarg, [A]).



%% Exported
%%
vector({?TAG,_N,A}) ->
    vector_to_list(A, []);
vector(V) when number(V) ->
    [V];
vector(L) when list(L) ->
    case vector_from_list(0, L, []) of
	{[], 0} ->
	    erlang:fault(badarg, [L]);
	{A, N} when list(A) ->
	    fix({?TAG,N,A});
	Fault ->
	    erlang:fault(Fault, [L])
    end;
vector(L) ->
    erlang:fault(badarg, [L]).

vector_to_list([], C) ->
    lists:reverse(C);
vector_to_list([V | A], C) when float(V) ->
    vector_to_list(A, [V | C]);
vector_to_list([1 | A], C) ->
    vector_to_list(A, [0.0 | C]);
vector_to_list([Z | A], C) ->
    vector_to_list([Z-1 | A], [0.0 | C]).

vector_from_list(N, [], C) ->
    {lists:reverse(C), N};
vector_from_list(N, [V | L], C) when number(V) ->
    F = float(V),
    vector_from_list(N+1, L, push_v(F, C));
vector_from_list(_, _, _) ->
    badarg.



%% Exported
%%
vector(N, D)
  when integer(N), list(D),
       1 =< N ->
    case vector_from_tuplist(1, N, lists:sort(D), []) of
	L when list(L) ->
	    fix({?TAG,N,L});
	Fault ->
	    erlang:fault(Fault, [N, D])
    end;
vector(N, D) ->
    erlang:fault(badarg, [N, D]).

vector_from_tuplist(I, N, [], C) when I =< N ->
    vector_from_tuplist(N+1, N, [], push_v(N+1-I, C));
vector_from_tuplist(_, _, [], C) ->
    lists:reverse(C);
vector_from_tuplist(I1, N, [{I2,V} | D], C)
  when integer(I2), number(V), 
       I1 =< I2, I2 =< N ->
    F = float(V),
    vector_from_tuplist(I2+1, N, D, push_v(F, push_v(I2-I1, C)));
vector_from_tuplist(_, _, _, _) ->
    badarg.



%% Exported
%%
rows({?TAG,1,M,[A]}) ->
    fix({?TAG,M,A});
rows({?TAG,_,M,A}) ->
    rows_to_list(M, A, []);
rows({?TAG,_,_} = A) ->
    vector(A);
rows(V) when number(V) ->
    [V];
rows(A) ->
    erlang:fault(badarg, [A]).

rows_to_list(_, [], C) ->
    lists:reverse(C);
rows_to_list(M, [Row | A], C) ->
    rows_to_list(M, A, [fix({?TAG,M,Row}) | C]).



%% Exported
%%
rows(M, L)
  when integer(M), list(L), M >= 1 ->
    case vecs(M, L) of
	{A, N} when list(A) ->
	    fix({?TAG,N,M,A});
	Fault ->
	    erlang:fault(Fault, [M, L])
    end;
rows(M, L) ->
    erlang:fault(badarg, [M, L]).



%% Exported
%%
cols({?TAG,_,_,_} = A) ->
    rows(trans(A));
cols({?TAG,_,_} = A) ->
    [fix(A)];
cols(V) when number(V) ->
    [V];
cols(A) ->
    erlang:fault(badarg, A).



%% Exported
%%
cols(N, L)
  when integer(N), list(L), N >= 1 ->
    case vecs(N, L) of
	{A, M} when list(A) ->
	    trans({?TAG,M,N,A});
	Fault ->
	    erlang:fault(Fault, [N, L])
    end;
cols(N, L) ->
    erlang:fault(badarg, [N, L]).



%% Exported
%%
cat_cols({?TAG,N,_,_} = A, {?TAG,N,_,_} = B) ->
    cols(N, cols(A)++cols(B));
cat_cols({?TAG,N,_,_} = A, {?TAG,N,_} = B) ->
    cols(N, cols(A)++cols(B));
cat_cols({?TAG,N,_} = A, {?TAG,N,_,_} = B) ->
    cols(N, cols(A)++cols(B));
cat_cols({?TAG,N,_} = A, {?TAG,N,_} = B) ->
    cols(N, cols(A)++cols(B));
cat_cols(A, B) ->
    erlang:fault(badarg, [A, B]).



%% Exported
%%
cat_rows({?TAG,Na,M,A}, {?TAG,Nb,M,B}) ->
    {?TAG,Na+Nb,M,A++B};
cat_rows({?TAG,_} = A, {?TAG,_,1} = B) ->
    rows(1, rows(A)++rows(B));
cat_rows({?TAG,_,1} = A, {?TAG,_} = B) ->
    rows(1, rows(A)++rows(B));
cat_rows({?TAG,_} = A, {?TAG,_} = B) ->
    rows(1, rows(A)++rows(B));
cat_rows(A, B) ->
    erlang:fault(badarg, [A, B]).



%% Exported
%%
trans({?TAG,1,M,[A]}) ->
    fix({?TAG,M,A});
trans({?TAG,N,M,A}) ->
    fix({?TAG,M,N,trans_cols_forw(1, M, A, [])});
trans({?TAG,N,A}) ->
    fix({?TAG,1,N,[A]});
trans(A) when number(A) ->
    float(A);
trans(A) ->
    erlang:fault(badarg, [A]).

trans_cols_forw(J, M, _, C_r) when J == M+1 ->
    lists:reverse(C_r);
trans_cols_forw(J, M, A, C_r) ->
    {Col_r, A_r} = trans_mk_col_r(A),
    trans_cols_rev(J+1, M, A_r, [lists:reverse(Col_r) | C_r]).

trans_cols_rev(J, M, _, C_r) when J == M+1 ->
    lists:reverse(C_r);
trans_cols_rev(J, M, A_r, C_r) ->
    {Col, A} = trans_mk_col_r(A_r),
    trans_cols_forw(J+1, M, A, [Col | C_r]).

trans_mk_col_r(A) ->
    trans_mk_col_r(A, [], []).

trans_mk_col_r([], B, C) ->
    {C, B};
trans_mk_col_r([[V | Row] | A], B, C) when float(V) ->
    trans_mk_col_r(A, [Row | B], [V | C]);
trans_mk_col_r([Row | A], B, C) ->
    trans_mk_col_r(A, [pop_z(Row) | B], push_v(1, C));
trans_mk_col_r([[] | A], B, C) ->
    trans_mk_col_r(A, [[] | B], push_v(1, C)).



%% Exported
%%
mult({?TAG,K,_M} = A, {?TAG,1,K,[B]}) ->
    mult_trans(A, {?TAG,K,B});
mult({?TAG,_,K,_} = A, {?TAG,K,_,_} = B) ->
    mult_trans(A, trans(B));
mult({?TAG,1,M,[A]}, {?TAG,M,B}) ->
    vec_mult(A, B);
mult({?TAG,N,M,A}, {?TAG,M,B}) ->
    fix({?TAG,N,mult_vec(B, A)});
mult(A, {?TAG,N,1,[B]}) when number(A) ->
    fix({?TAG,N,1,[vec_mult(float(A), B)]});
mult(A, {?TAG,N,M,B}) when number(A) ->
    fix({?TAG,N,M,mult_const(float(A), B, [])});
mult(A, {?TAG,N,B}) when number(A) ->
    fix({?TAG,N,vec_mult(float(A), B)});
mult({?TAG,N,1,[A]}, B) when number(B) ->
    fix({?TAG,N,1,[vec_mult(float(B), A)]});
mult({?TAG,N,M,A}, B) when number(B) ->
    fix({?TAG,N,M,mult_const(float(B), A, [])});
mult({?TAG,N,A}, B) when number(B) ->
    fix({?TAG,N,vec_mult(float(B), A)});
mult(A, B) when number(A), number(B) ->
    float(A*B);
mult(A, B) ->
    erlang:fault(badarg, [A, B]).

mult_vec(VecA, B) ->
    mult_vec(list_to_tuple(vector_to_list(VecA, [])), B, []).

mult_vec(_, [], C) ->
    lists:reverse(C);
mult_vec(VecA, [VecB | B], C) ->
    mult_vec(VecA, B, push_v(vec_mult(VecA, VecB), C)).



%% Exported
%%
mult_trans({?TAG,1,M,[A]}, {?TAG,1,M,[B]}) ->
    vec_mult(A, B);
mult_trans({?TAG,_N,M,_} = A, {?TAG,1,M,_} = B) ->
    mult(A, trans(B));
mult_trans({?TAG,Na,M,A}, {?TAG,Nb,M,B}) ->
    fix({?TAG,Na,Nb,mult_row(A, B, [])});
%%
mult_trans(A, {?TAG,N,B}) ->
    mult(A, {?TAG,1,N,[B]});
mult_trans({?TAG,N,A}, B) ->
    mult_trans(trans({?TAG,1,N,[A]}), B);
%%
mult_trans({?TAG,_,_,_} = A, B) when number(B) ->
    mult(A, B);
mult_trans({?TAG,_,_} = A, B) when number(B) ->
    mult(A, B);
mult_trans(A, {?TAG,_,_,_} = B) when number(A) ->
    trans(mult(A, B));
mult_trans(A, {?TAG,_,_} = B) when number(A) ->
    trans(mult(A, B));
mult_trans(A, B) when number(A), number(B) ->
    float(A * B);
mult_trans(A, B) ->
    erlang:fault(badarg, [A, B]).

mult_row([], _, C) ->
    lists:reverse(C);
mult_row([RowA | A], B, C) ->
    mult_row(A, B, [mult_vec(RowA, B) | C]).

mult_const(_, [], C) ->
    lists:reverse(C);
mult_const(F, [Row | A], C) ->
    mult_const(F, A, [vec_mult(F, Row) | C]).



%% Exported
%% 
add({?TAG,N,M,A}, {?TAG,N,M,B}) ->
    fix({?TAG,N,M,add_row(A, B, [])});
add({?TAG,N,A}, {?TAG,N,B}) ->
    fix({?TAG,N,vec_add(A, B)});
add({?TAG,N,1,_} = A, {?TAG,N,B}) ->
    {?TAG,1,N,[C]} = trans(A),
    fix({?TAG,N,vec_add(C, B)});
add({?TAG,N,A}, {?TAG,N,1,_} = B) ->
    {?TAG,1,N,[C]} = trans(B),
    fix({?TAG,N,vec_add(A, C)});
add(Va, Vb) when number(Va), number(Vb) ->
    float(Va + Vb);
%%
add({?TAG,1,1,_} = A, Vb) when number(Vb) ->
    add(A, {?TAG,1,1,[push_v(Vb, [])]});
add({?TAG,1,_} = A, Vb) when number(Vb) ->
    add(A, {?TAG,1,push_v(Vb, [])});
add(Va, B) when number(Va) ->
    add(B, Va);
%%
add(A, B) ->
    erlang:fault(badarg, [A, B]).

add_row([], [], C) ->
    lists:reverse(C);
add_row([RowA | A], [RowB | B], C) ->
    add_row(A, B, [vec_add(RowA, RowB) | C]).



%% Exported
%%
sub(A, B) ->
    add(A, mult(-1, B)).



%% Exported
%%
reduce({?TAG,N,M,A}) ->
    case catch reduce_sort(A, []) of
	{'EXIT',{badarith,_}} ->
	    illconditioned;
	B ->
	    {?TAG,N,M,B}
    end;
reduce(A) ->
    erlang:fault(badarg, [A]).

reduce_sort([], C) ->
    reduce_row(lists:sort(C), []);
reduce_sort([[V | _] = Row | A], C) when float(V) ->
    reduce_sort(A, [[0, 1.0/abs(V) | Row] | C]);
reduce_sort([[Z | [V | _] = Row] | A], C) when float(V) ->
    reduce_sort(A, [[Z, 1.0/abs(V) | Row] | C]);
reduce_sort([[Z] | A], C) ->
    reduce_sort(A, [[Z, infinity] | C]).

reduce_row([], C) ->
    lists:reverse(C);
reduce_row([Row | A], C) ->
    reduce_row(reduce_zap(Row, A, []), 
	       [case Row of
		    [0, _ | R] ->
			R;
		    [Z, _ | R] ->
			[Z | R]
		end | C]).

reduce_zap([Z, _, V | Row] = R, [[Z, _, Va | RowA] | A], C) 
  when float(V), float(Va) ->
    reduce_zap(R, A,
	       [case vec_add(RowA, -Va/V, Row) of
		    [Vc | _] = RowC when float(Vc) ->
			[Z+1, 1.0/abs(Vc) | RowC];
		    [Zc | [Vc | _] = RowC] when float(Vc) ->
			[Z+1+Zc, 1.0/abs(Vc) | RowC];
		    [Zc] ->
			[Z+1+Zc, infinity];
		    [] ->
			[Z+1, infinity]
	       end | C]);
reduce_zap(_, [], C) ->
    lists:sort(C);
reduce_zap(_, A, C) ->
    lists:merge(A, lists:sort(C)).



%% Exported
%%
backsubst({?TAG, N, M, A} = AA) when M == N+1 ->
    case catch backsubst_rev(A, [], []) of
	B when list(B) ->
	    {?TAG,N,B};
	{error, Reason} ->
	    Reason;
	{'EXIT', {badarith, []}} ->
	    illconditioned;
	{'EXIT', Reason} ->
	    exit(Reason);
	Fault ->
	    erlang:fault(Fault, [AA])
    end;
backsubst(A) ->
    erlang:fault(badarg, [A]).

backsubst_rev([], B, C) ->
    backsubst_row(B, C, []);
backsubst_rev([[V | Row] | A], B, C) when float(V) ->
    backsubst_rev([1.0 | vec_mult(1/V, Row)], A, B, C);
backsubst_rev([[Z, V | Row] | A], B, C) when float(V) ->
    backsubst_rev([Z, 1.0 | vec_mult(1/V, Row)], A, B, C);
backsubst_rev([[_] | _], _, _) ->
    {error, underdeterminded};
backsubst_rev(_, _, _) ->
    badarg.

backsubst_rev(Row, A, B, C) ->
    case lists:reverse(Row) of
	[Vc | R] when float(Vc) ->
	    backsubst_rev(A, [R | B], [Vc | C]);
	[1 | R] ->
	    backsubst_rev(A, [R | B], [0.0 | C]);
	[Zc | R] ->
	    backsubst_rev(A, [[Zc-1 | R] | B], [0.0 | C])
    end.

backsubst_row([], [], X) ->
    backsubst_vec(X, []);
backsubst_row([Row | A], [Vc | C], X) ->
    backsubst_row(A, C, X++[backsubst_col(Row, Vc, X)]).

backsubst_col([1.0], Vc, []) ->
    Vc;
backsubst_col([1.0, Z], Vc, [])
  when integer(Z) ->
    Vc;
backsubst_col([V | Row], Vc, [Vx | X]) 
  when float(V), float(Vc), float(Vx) ->
    backsubst_col(Row, Vc-Vx*V, X);
backsubst_col([1 | Row], Vc, [_ | X]) ->
    backsubst_col(Row, Vc, X);
backsubst_col([Z | Row], Vc, [_ | X])
  when integer(Z) ->
    backsubst_col([Z-1 | Row], Vc, X).

backsubst_vec([], C) ->
    C;
backsubst_vec([V | A], C) ->
    backsubst_vec(A, push_v(V, C)).



vecs(N, L) ->
    vecs(0, N, L, []).

vecs(I, _, [], C) ->
    {lists:reverse(C), I};
vecs(I, N, [{?TAG,N,D} | L], C) ->
    vecs(I+1, N, L, [D | C]);
vecs(I, 1, [V | L], C) when number(V) ->
    vecs(I+1, 1, L, [push_v(float(V), []) | C]);
vecs(_, _, _, _) ->
    badarg.



vec_add(A, B) ->
    vec_add(A, B, []).

vec_add(A, F, B) when float(F) ->
    vec_add(A, F, B, []);

vec_add([Va | A] = AA, [Vb | B], C) when integer(Va) ->
    if integer(Vb) ->
	    if Va == Vb ->
		    vec_add(A, B, push_v(Va, C));
	       Va < Vb ->
		    vec_add(A, [Vb-Va | B], push_v(Va, C));
	       true ->
		    vec_add([Va-Vb | A], B, push_v(Vb, C))
	    end;
       true -> % float(Vb)
	    vec_add(pop_z(AA), B, push_v(Vb, C))
    end;
vec_add([Va | A], [Vb | B] = BB, C) -> % when float(Va)
    if integer(Vb) ->
	    vec_add(A, pop_z(BB), push_v(Va, C));
       float(Vb), float(Va) ->
	    vec_add(A, B, push_v(Va + Vb, C))
    end;
vec_add([], _, C) ->
    lists:reverse(C);
vec_add(_, [], C) ->
    lists:reverse(C).

vec_add([Va | A] = AA, F, [Vb | B], C) when integer(Va) ->
    if integer(Vb) ->
	    if Va == Vb ->
		    vec_add(A, F, B, push_v(Va, C));
	       Va < Vb ->
		    vec_add(A, F, [Vb-Va | B], push_v(Va, C));
	       true ->
		    vec_add([Va-Vb | A], F, B, push_v(Vb, C))
	    end;
       float(F), float(Vb) ->
	    vec_add(pop_z(AA), F, B, push_v(F*Vb, C))
    end;
vec_add([Va | A], F, [Vb | B] = BB, C) -> % when float(Va)
    if integer(Vb) ->
	    vec_add(A, F, pop_z(BB), push_v(Va, C));
       float(Vb), float(F), float(Va) ->
	    vec_add(A, F, B, push_v(Va + F*Vb, C))
    end;
vec_add([], _, _, C) ->
    lists:reverse(C);
vec_add(_, _, [], C) ->
    lists:reverse(C).

vec_mult(F, B) when float(F) ->
    vec_mult_const(F, B, []);
vec_mult(T, B) when tuple(T) ->
    vec_mult_tuple(T, 1, B, 0.0);
vec_mult(A, B) ->
    vec_mult(A, B, 0.0).

vec_mult_const(F, [V | B], C)
  when float(F), float(V) ->
    vec_mult_const(F, B, push_v(F*V, C));
vec_mult_const(F, [Z | B], C) ->
    vec_mult_const(F, B, push_v(Z, C));
vec_mult_const(_, [], C) ->
    lists:reverse(C).

vec_mult_tuple(T, I, [Zb | B], S) when integer(Zb) ->
    vec_mult_tuple(T, I+Zb, B, S);
vec_mult_tuple(T, I, [Vb | B], S) when float(Vb), float(S) ->
    case element(I, T) of
	Va when float(Va) ->
	    vec_mult_tuple(T, I+1, B, Va*Vb + S)
    end;
vec_mult_tuple(_, _, [], S) ->
    S.

vec_mult([Za | A], B, S) when integer(Za) ->
    vec_mult_pop(Za, A, B, S);
vec_mult(A, [Zb | B], S) when integer(Zb) ->
    vec_mult_pop(Zb, B, A, S);
vec_mult([Va | A], [Vb | B], S) when float(Va), float(Vb), float(S) ->
    vec_mult(A, B, Va*Vb + S);
vec_mult([], _, S) ->
    S;
vec_mult(_, [], S) ->
    S.

vec_mult_pop(_, [], _, S) ->
    S;
vec_mult_pop(0, A, B, S) ->
    vec_mult(A, B, S);
vec_mult_pop(Za, A, [Vb | B], S) when float(Vb) ->
    vec_mult_pop(Za-1, A, B, S);
vec_mult_pop(_, _, [_], S) -> % when integer(Zb)
    S;
vec_mult_pop(Za, A, [Zb | B], S) when Za < Zb ->
    vec_mult_pop(Zb-Za, B, A, S);
vec_mult_pop(Za, A, [Zb | B], S) when Zb < Za ->
    vec_mult_pop(Za-Zb, A, B, S);
vec_mult_pop(_, A, [_ | B], S) -> % when Za == Zb
    vec_mult(A, B, S);
vec_mult_pop(_, _, [], S) ->
    S.



%% Push value; zeros or float
push_v(0.0, C) ->
    case C of
	[Z | R] when integer(Z) ->
	    [Z+1 | R];
	R ->
	    [1 | R]
    end;
push_v(V, C) when float(V) ->
    [V | C];
push_v(0, C) ->
    C;
push_v(Z1, C) when integer(Z1) ->
    case C of
	[Z2 | R] when integer(Z2) ->
	    [Z1+Z2 | R];
	R ->
	    [Z1 | R]
    end.

%% Pop zero
pop_z([]) ->
    [];
pop_z([1]) ->
    [];
pop_z([1 | C]) ->
    C;
pop_z([Z | C]) when integer(Z) ->
    [Z-1 | C].


%% Fix 1x1 matrixes to become scalars
%%
fix({?TAG,1,1,[[1]]}) ->
    0.0;
fix({?TAG,1,1,[[V]]}) ->
    V;
fix({?TAG,1,[1]}) ->
    0.0;
fix({?TAG,1,[V]}) ->
    V;
fix(M) ->
    M.



float_perf(A, L) ->
    float_perf(A, L, []).

float_perf(_, [], C) ->
    lists:reverse(C);
float_perf(A, [B | T], C) ->
    float_perf(A, T, [float_perf_int(A, B, 0.0), C]).

float_perf_int([], [], S) ->
    S;
float_perf_int([Va | A], [Vb | B], S) when float(Va), float(Vb), float(S) ->
    float_perf_int(A, B, Va*Vb + S).
