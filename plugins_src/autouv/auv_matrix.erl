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
%%     $Id: auv_matrix.erl,v 1.3 2002/10/16 08:22:19 dgud Exp $

-module(auv_matrix).

-export([size/1]).
-export([vector/1, vector/2]).
-export([rows/1, rows/3, cols/1, cols/3]).
-export([cat_cols/2, cat_rows/2]).
-export([trans/1, mult/2, mult_trans/2]).
-export([reduce/1, backsubst/1]).

-define(TAG, ?MODULE).



%% Exported
%%
size({?TAG,N,M,_}) ->
    {N,M};
size({?TAG,N,_}) ->
    {N};
size(A) ->
    erlang:fault(badarg, [A]).



%% Exported
%%
vector({?TAG,N,A}) ->
    vector_to_list(A, []);
vector(L) when list(L) ->
    case vector_from_list(0, L, []) of
	{[], 0} ->
	    erlang:fault(badarg, [L]);
	{A, N} when list(A) ->
	    {?TAG,N,A};
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
	    {?TAG,N,L};
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
rows({?TAG,_,M,A}) ->
    rows_to_list(M, A, []);
rows({?TAG,N,_} = A) ->
    rows(cols(N, 1, [A]));
rows(A) ->
    erlang:fault(badarg, [A]).

rows_to_list(_, [], C) ->
    lists:reverse(C);
rows_to_list(M, [Row | A], C) ->
    rows_to_list(M, A, [{?TAG,M,Row} | C]).



%% Exported
%%
rows(N, M, L)
  when integer(N), integer(M), list(L),
       N >= 1, M >= 1 ->
    case vecs(N, M, L) of
	A when list(A) ->
	    {?TAG,N,M,A};
	Fault ->
	    erlang:fault(Fault, [N, M, L])
    end.



%% Exported
%%
cols({?TAG,_,_,_} = A) ->
    rows(trans(A));
cols({?TAG,_,_} = A) ->
    [A];
cols(A) ->
    erlang:fault(badarg, A).



%% Exported
%%
cols(N, M, L)
  when integer(N), integer(M), list(L),
       N >= 1, M >= 1 ->
    case vecs(M, N, L) of
	A when list(A) ->
	    trans({?TAG,M,N,A});
	Fault ->
	    erlang:fault(Fault, [N, M, L])
    end;
cols(N, M, L) ->
    erlang:fault(badarg, [N, M, L]).



%% Exported
%%
cat_cols({?TAG,N,Ma,_} = A, {?TAG,N,Mb,_} = B) ->
    cols(N,Ma+Mb,cols(A)++cols(B));
cat_cols(A, B) ->
    erlang:fault(badarg, [A, B]).



%% Exported
%%
cat_rows({?TAG,Na,M,A}, {?TAG,Nb,M,B}) ->
    {?TAG,Na+Nb,M,A++B};
cat_rows(A, B) ->
    erlang:fault(badarg, [A, B]).



%% Exported
%%
trans({?TAG,N,M,A}) ->
    {?TAG,M,N,trans_cols_forw(1, M, A, [])};
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
mult({?TAG,_,K,_} = A, {?TAG,K,_,_} = B) ->
    mult_trans(A, trans(B));
mult({?TAG,N,M,A}, {?TAG,M,B}) ->
    {?TAG,N,1,mult_row(A,[B], [])};
mult(V, {?TAG,N,B}) when number(V) ->
    {?TAG,N,vec_mult(float(V), B)};
mult(A, B) ->
    erlang:fault(badarg, [A, B]).



%% Exported
%%
mult_trans({?TAG,Na,M,A}, {?TAG,Nb,M,B}) ->
    {?TAG,Na,Nb,mult_row(A, B, [])};
mult_trans(A, B) ->
    erlang:fault(badarg, [A, B]).

mult_row([], _, C) ->
    lists:reverse(C);
mult_row([RowA | A], B, C) ->
    mult_row(A, B, [mult_col(RowA, B, []) | C]).

mult_col(_, [], C) ->
    lists:reverse(C);
mult_col(RowA, [ColB | B], C) ->
    mult_col(RowA, B, push_v(vec_mult(RowA, ColB), C)).



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
			[Z+1+Zc, infinity]
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



vecs(N, M, L) ->
    vecs(1, N, M, L, []).

vecs(I, N, _, [], C) when I == N+1 ->
    lists:reverse(C);
vecs(I, N, M, [{?TAG,M,D} | L], C) ->
    vecs(I+1, N, M, L, [D | C]);
vecs(_, _, _, _, _) ->
    badarg.



vec_add(A, F, B) ->
    vec_add(A, F, B, []).

vec_add([], _, _, C) ->
    lists:reverse(C);
vec_add(_, _, [], C) ->
    lists:reverse(C);
vec_add([Va | A], F, [Vb | B], C)
  when float(Va), float(F), float(Vb) ->
    Vc = Va + F*Vb,
    vec_add(A, F, B, push_v(Va + F*Vb, C));
vec_add(A, F, [Vb | B], C)
  when float(F), float(Vb) ->
    vec_add(pop_z(A), F, B, push_v(F*Vb, C));
vec_add([Va | A], F, B, C)
  when float(Va) ->
    vec_add(A, F, pop_z(B), push_v(Va, C));
vec_add([Z | A], F, [Z | B], C) ->
    vec_add(A, F, B, [Z | C]);
vec_add([Za | A], F, [Zb | B], C) when Za < Zb ->
    vec_add(A, F, [Zb-Za | B], [Za | C]);
vec_add([Za | A], F, [Zb | B], C) ->
    vec_add([Za-Zb | A], F, B, [Zb | C]).



vec_mult(F, B) when float(F) ->
    vec_mult_const(F, B, []);
vec_mult(A, B) ->
    vec_mult(A, B, 0.0).

vec_mult_const(_, [], C) ->
    lists:reverse(C);
vec_mult_const(F, [V | B], C)
  when float(F), float(V) ->
    vec_mult_const(F, B, push_v(F*V, C));
vec_mult_const(F, [Z | B], C) ->
    vec_mult_const(F, B, push_v(Z, C)).

vec_mult([], _, S) ->
    S;
vec_mult(_, [], S) ->
    S;
vec_mult([Va | A], [Vb | B], S)
  when float(Va), float(Vb), float(S) ->
    vec_mult(A, B, Va*Vb+S);
vec_mult(A, [Vb | B], S)
  when float(Vb) ->
    vec_mult(pop_z(A), B, S);
vec_mult([Va | A], B, S)
  when float(Va) ->
    vec_mult(A, pop_z(B), S);
vec_mult([Z | A], [Z | B], S) ->
    vec_mult(A, B, S);
vec_mult([Za | A], [Zb | B], S) when Za < Zb ->
    vec_mult(A, [Zb-Za | B], S);
vec_mult([Za | A], [Zb | B], S) -> % Zb < Za
    vec_mult([Za-Zb | A], B, S).


%% Push value; zeros or float   XXX How do I get this one inlined?
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

%% Pop zero   XXX How do I get this one inlined?
pop_z([]) ->
    [];
pop_z([1]) ->
    [];
pop_z([1 | C]) ->
    C;
pop_z([Z | C]) when integer(Z) ->
    [Z-1 | C].
