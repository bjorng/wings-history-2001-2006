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
%%     $Id: auv_matrix.erl,v 1.2 2002/10/10 18:28:04 bjorng Exp $

-module(auv_matrix).

-export([const/3, diag/2]).
-export([size/1]).
-export([new/3, rows/1, rows/3, cols/1, cols/3]).
-export([set_element/4, get_element/3, set_row/3]).
-export([cat_cols/2, cat_rows/2]).
-export([sub_cols/3]).
-export([trans/1, mult/2, mult_trans/2, reduce/1, backsubst/1]).
-export([sub/2]).

-define(TAG, ?MODULE).

%% Exported
%%
const(N, M, V)
  when integer(N), integer(M), number(V), N > 0, M > 0 ->
    if V == 0 ->
	    {?TAG,N,M,[]};
       true ->
	    {?TAG,N,M,
	     const_int(N, M, V, [])}
    end;
const(N, M, V) ->
    erlang:fault(badarg, [N, M, V]).

const_int(0, _, _, A) ->
    A;
const_int(N, M, V, A) ->
    const_int(N-1, M, V, [{N,const_row(M, V, [])} | A]).

const_row(0, _, Row) ->
    Row;
const_row(J, V, Row) ->
    const_row(J-1, V, [{J,V} | Row]).



%% Exported
%%
diag(N, V)
  when integer(N), N > 0, number(V) ->
    {?TAG,N,N,diag_int(N, V, [])};
diag(N, L)
  when integer(N), N > 0 ->
    case mk_row(N, L) of
	Row when list(Row) ->
	    {?TAG,N,N,[{I,[IV]} || {I,_}=IV <- Row]};
	Fault ->
	    erlang:fault(Fault, [N, L])
    end;
diag(N, V) ->
    erlang:fault(badarg, [N, V]).

diag_int(0, _, A) ->
    A;
diag_int(N, V, A) ->
    diag_int(N-1, V, [{N, [{N, V}]} | A]).



%% Exported
%%
size({?TAG,N,M,_}) ->
    {N,M};
size(A) ->
    erlang:fault(badarg, [A]).



%% Exported
%%
new(N, M, Data)
  when integer(N), integer(M), number(Data),
       0 < N, 0 < M ->
    const(N, M, Data);
new(N, M, Data)
  when integer(N), integer(M), list(Data),
       0 < N, 0 < M ->
    case new_int(N, M, Data, 1, 1, []) of
	C when list(C) ->
	    {?TAG,N,M,C};
	Fault ->
	    erlang:fault(Fault, [N, M, Data])
    end;
new(N, M, Data) ->
    erlang:fault(badarg, [N, M, Data]).

new_int(_, _, [], _, _, C) ->
    unflatten(lists:sort(C), []);
new_int(N, _, _, I, _, _) when N < I ->
    badarg;
new_int(N, M, Data, I, J, C) when M < J ->
    new_int(N, M, Data, I+1, 1, C);
new_int(N, M, [{Ia,Ja,V} = Ea | Data], _, _, C) 
  when integer(Ia), integer(Ja), number(V),
       0 < Ia, Ia =< N, 0 < Ja, Ja =< M ->
    new_int(N, M, Data, Ia, Ja+1, 
	    if V == 0 -> C;
	       true -> [Ea | C]
	    end);
new_int(N, M, [{Ia,RowA} | Data], _, J, C)
  when integer(Ia), list(RowA),
       0 < Ia, Ia =< N ->
    new_row(N, M, Data, RowA, Ia, J, C);
new_int(N, M, [RowA | Data], I, J, C)
  when list(RowA) ->
    new_row(N, M, Data, RowA, I, J, C);
new_int(N, M, [V | Data], I, J, C)
  when number(V) ->
    new_int(N, M, Data, I, J+1, 
	    if V == 0 -> C;
	       true -> [{I,J,V} | C]
	    end);
new_int(_, _, _, _, _, _) ->
    badarg.

new_row(N, M, Data, [], Ia, _, C) ->
    new_int(N, M, Data, Ia+1, 1, C);
new_row(_, M, _, _, _, J, _) when M < J ->
    badarg;
new_row(N, M, Data, [{Ja,V} | RowA], Ia, _, C) 
  when integer(Ja), number(V),
       0 < Ja, Ja =< M ->
    new_row(N, M, Data, RowA, Ia, Ja+1, 
	    if V == 0 -> C;
	       true -> [{Ia,Ja,V} | C]
	    end);
new_row(N, M, Data, [V | RowA], Ia, J, C) 
  when number(V) ->
    new_row(N, M, Data, RowA, Ia, J+1, 
	    if V == 0 -> C;
	       true -> [{Ia,J,V} | C]
	    end);
new_row(_, _, _, _, _, _, _) ->
    badarg.



%% Exported
%%
rows({?TAG,N,_,A}) ->
    rows_int(1, N, A, []);
rows(A) ->
    erlang:fault(badarg, [A]).

rows_int(I, N, [], C) when I == N+1 ->
    lists:reverse(C);
rows_int(I, N, [{I, Row} | A], C) when I =< N ->
    rows_int(I+1, N, A, [Row | C]);
rows_int(I, N, A, C) when I =< N ->
    rows_int(I+1, N, A, [[] | C]).



%% Exported
%%
rows(N, M, L)
  when integer(N), integer(M), list(L),
       N >= 1, M >= 1 ->
    case rows_rows(1, N, M, L, []) of
	A when list(A) ->
	    {?TAG,N,M,A};
	Fault ->
	    erlang:fault(Fault, [N, M, L])
    end.

rows_rows(I, N, _, [], C) when I == N+1 ->
    lists:reverse(C);
rows_rows(I, N, M, [[] | L], C) ->
    rows_rows(I+1, N, M, L, C);
rows_rows(I, N, M, [R | L], C) ->
    case mk_row(M, R) of
	Row when list(Row) ->
	    rows_rows(I+1, N, M, L, [{I,Row} | C]);
	Fault ->
	    Fault
    end;
rows_rows(_, _, _, _, _) ->
    badarg.



%% Exported
%%
cols({?TAG,_,_,_} = A) ->
    rows(trans(A));
cols(A) ->
    erlang:fault(badarg, A).



%% Exported
%%
cols(N, M, L)
  when integer(N), integer(M), list(L),
       N >= 1, M >= 1 ->
    case rows_rows(1, M, N, L, []) of
	A when list(A) ->
	    trans({?TAG,M,N,A});
	Fault ->
	    erlang:fault(Fault, [N, M, L])
    end;
cols(N, M, L) ->
    erlang:fault(badarg, [N, M, L]).

%% Exported
%%
set_element({?TAG,N,M,A}, I, J, V)
  when integer(I), integer(J), number(V),
       0 < I, I =< N, 0 < J, J =< M ->
    {?TAG,N,M,
     set_element_row(I, J, V, A, [])}.

set_element_row(I, J, V, [{I,Row} | A], C) ->
    lists:reverse(C, [{I,set_element_col(J, V, Row, [])} | A]);
set_element_row(I, J, V, [{Ia,_} = IaRowA | A], C) when Ia < I ->
    set_element_row(I, J, V, A, [IaRowA | C]);
set_element_row(I, J, V, A, C) ->
    lists:reverse(C, [{I,[{J,V}]} | A]).

set_element_col(J, V, [{J,_} | Row], R) ->
    lists:reverse(R, [{J,V} | Row]);
set_element_col(J, V, [{Ja,_} = JaV | Row], R) when Ja < J ->
    set_element_col(J, V, Row, [JaV | R]);
set_element_col(J, V, Row, R) ->
    lists:reverse(R, [{J,V} | Row]).



%% Exported
%%
get_element({?TAG,N,M,A}, I, J)
  when integer(I), integer(J),
       0 < I, I =< N, 0 < J, J =< M ->
    case lists:keysearch(I, 1, A) of
	{value,{I,Rai}} ->
	    case lists:keysearch(J, 1, Rai) of
		{value,{J,V}} ->
		    V;
		false ->
		    0
	    end;
	false ->
	    0
    end.



%% Exported
%%
set_row({?TAG,N,M,A} = AA, I, R)
  when integer(I), 
       0 < I, I =< N ->
    case mk_row(M, R) of
	Row when list(Row) ->
	    {?TAG,N,M,set_row_int(I, Row, A, [])};
	Fault ->
	    erlang:fault(Fault, [AA, I, R])
    end;
set_row(A, I, R) ->
    erlang:fault(badarg, [A, I, R]).

set_row_int(_, [], A, _) ->
    A;
set_row_int(I, Row, [{I,_} | A], C) ->
    lists:reverse(C, [{I,Row} | A]);
set_row_int(I, Row, [{Ia,_} = IaRowA | A], C) when Ia < I ->
    set_row_int(I, Row, A, [IaRowA | C]);
set_row_int(I, Row, A, C) ->
    lists:reverse(C, [{I,Row} | A]).



%% Exported
%%
cat_cols({?TAG,N,Ma,_} = A, {?TAG,N,Mb,_} = B) ->
    cols(N,Ma+Mb,cols(A)++cols(B));
cat_cols(A, B) ->
    erlang:fault(badarg, [A, B]).

%% Exported
%%
cat_rows({?TAG,Na,M,A}, {?TAG,Nb,M,B}) ->
    {?TAG,Na+Nb,M,A++[{Nb+N,RowB} || {N,RowB} <- B]};
cat_rows(A, B) ->
    erlang:fault(badarg, [A, B]).



%% Exported
%%
sub_cols({?TAG,N,M,A}, J1, J2)
  when integer(J1), integer(J2),
       1 =< J1, J1 =< M, J1 =< J2, J2 =< M ->
    {?TAG,N,J2-J1+1,sub_cols_row(A, J1, J2, [])};
sub_cols(A, J1, J2) ->
    erlang:fault(badarg, [A, J1, J2]).

sub_cols_row([], _, _, C) ->
    lists:reverse(C);
sub_cols_row([{I,Row} | A], J1, J2, C) ->
    case sub_cols_col(Row, J1, J2, []) of
	[] ->
	    sub_cols_row(A, J1, J2, C);
	R ->
	    sub_cols_row(A, J1, J2, [{I,R} | C])
    end.

sub_cols_col([], _, _, []) ->
    [];
sub_cols_col([{J,_} | Row], J1, J2, C) when J < J1 ->
    sub_cols_col(Row, J1, J2, C);
sub_cols_col(Row, J1, J2, C) ->
    sub_cols_col2(Row, J1, J2, C).

sub_cols_col2([], _, _, C) ->
    lists:reverse(C);
sub_cols_col2([{J,V} | Row], J1, J2, C) when J =< J2 ->
    sub_cols_col(Row, J1, J2, [{J-J1+1,V} | C]);
sub_cols_col2(_, _, _, C) ->
    lists:reverse(C).



%% Exported
%%
trans({?TAG,N,M,A} = AA) ->
    case unflatten(trans_flatten(A, []), []) of
	C when list(C) ->
	    {?TAG,M,N,C};
	Fault ->
	    erlang:fault(Fault, [AA])
    end;
trans(A) ->
    erlang:fault(badarg, [A]).

trans_flatten([], C) ->
    lists:sort(C);
trans_flatten([{I,Row} | A], C) ->
    trans_flatten(A, trans_flatten_row(I, Row, C)).

trans_flatten_row(_, [], C) ->
    C;
trans_flatten_row(I, [{J,V} | Row], C) ->
    trans_flatten_row(I, Row, [{J,I,V} | C]).



%% Exported
%%
mult({?TAG,_,K,_} = A, {?TAG,K,_,_} = B) ->
    mult_trans(A, trans(B));
mult({?TAG,N,M,A}, B) when number(B) ->
    {?TAG,N,M,mult_const(B, A, [])};
mult(A, {?TAG,N,M,B}) when number(A) ->
    {?TAG,N,M,mult_const(A, B, [])};
mult(A, B) ->
    erlang:fault(badarg, [A, B]).

mult_trans({?TAG,Na,M,A}, {?TAG,Nb,M,B}) ->
    {?TAG,Na,Nb,mult_trans_int(A, B, [])};
mult_trans(A, B) ->
    erlang:fault(badarg, [A, B]).

mult_trans_int([], _, C) ->
    lists:reverse(C);
mult_trans_int([{Ia,RowA} | A], B, C) ->
    mult_trans_int(A, B, mult_trans_row(Ia, RowA, B, [], C)).

mult_trans_row(_, _, [], [], C) ->
    C;
mult_trans_row(Ia, _, [], RowC, C) ->
    [{Ia, lists:reverse(RowC)} | C];
mult_trans_row(Ia, RowA, [{Ib,RowB} | B], RowC, C) ->
    V = mult_trans_col(RowA, RowB, 0),
    if V == 0 ->
	    mult_trans_row(Ia, RowA, B, RowC, C);
       true ->
	    mult_trans_row(Ia, RowA, B, [{Ib,V} | RowC], C)
    end.

mult_trans_col([], _, V) ->
    V;
mult_trans_col(_, [], V) ->
    V;
mult_trans_col([{Ja,_} | RowA], [{Jb,_} | _] = RowB, V) when Ja < Jb ->
    mult_trans_col(RowA, RowB, V);
mult_trans_col([{J,Va} | RowA], [{J,Vb} | RowB], V) ->
    mult_trans_col(RowA, RowB, V + Va*Vb);
mult_trans_col(RowA, [_ | RowB], V) ->
    mult_trans_col(RowA, RowB, V).

mult_const(_, [], C) ->
    lists:reverse(C);
mult_const(K, [{I,Row} | A], C) ->
    case mult_const_row(K, Row, []) of
	[] ->
	    mult_const(K, A, C);
	RowK ->
	    mult_const(K, A, [{I,RowK} | C])
    end.

mult_const_row(_, [], C) ->
    lists:reverse(C);
mult_const_row(K, [{J,V} | Row], C) ->
    VK = V*K,
    mult_const_row(K, Row,
		   if VK == 0 ->
			   C;
		      true ->
			   [{J,VK} | C]
		   end).



%% Exported
%%
reduce({?TAG, N, M, A}) ->
    {?TAG,N,M,reduce_unrow(A, [])};
reduce(A) ->
    erlang:fault(badarg, [A]).

reduce_unrow([], C) ->
    reduce_zap(1, lists:sort(C));
reduce_unrow([{_,Row} | A], C) ->
    reduce_unrow(A, [Row | C]).

reduce_zap(_, []) ->
    [];
reduce_zap(I, [[{J,1} | RowA] = RA, [{J,Vb} | RowB] | A]) ->
    case sub_rows(RowB, Vb, RowA, []) of
	[] ->
	    reduce_zap(I, [RA | A]);
	RB ->
	    reduce_zap(I, [RA | insert_one(RB, A, [])])
    end;
reduce_zap(I, [[{_,1} | _] = RA | A]) ->
    %% rerow
    [{I,RA} | reduce_zap(I+1, A)];
reduce_zap(I, [[{J,Va} | RowA] | A]) ->
    reduce_zap(I, [[{J,1} | [{Ja,V/Va} || {Ja,V} <- RowA]] | A]).

insert_one(E, [H|T], Acc) when E > H ->
    insert_one(E, T, [H|Acc]);
insert_one(E, L, Acc) ->
    lists:reverse(Acc, [E|L]).

%% Exported
%%
backsubst({?TAG, N, M, A}) ->
    {?TAG,N,M,backsubst_norm(A, [])};
backsubst(A) ->
    erlang:fault(badarg, [A]).

backsubst_norm([], C) ->
    backsubst_zap(lists:reverse(C), []);
backsubst_norm([{_,[{_,1} | _]} = R | A], C) ->
    backsubst_norm(A, [R | C]);
backsubst_norm([{I,[{J,V} | Row]} | A], C) ->
    backsubst_norm(A, [{I,[{J,1} | [{Ja,Va/V} || {Ja,Va} <- Row]]} | C]).

backsubst_zap([], C) ->
    lists:reverse(C);
backsubst_zap([{I,[{J,1} = J_1 | Row]} | A], C) ->
    backsubst_zap(A, [{I,[J_1 | backsubst_row(J, Row, A)]} | C]).

backsubst_row(_, Row1, []) ->
    Row1;
backsubst_row(J1, Row1, [{_,[{J2,1} | Row2]} | A]) ->
    backsubst_row(J1, backsubst_col(Row1, J2, Row2), A).

backsubst_col([{J,V1} | Row1], J, Row2) ->
    sub_rows(Row1, V1, Row2, []);
backsubst_col([{J1,_} = J1_V1 | Row1], J2, Row2) when J1 < J2 ->
    [J1_V1 | backsubst_col(Row1, J2, Row2)];
backsubst_col(Row1, _, _) ->    
    Row1.



%% Exported
%%
sub(A, B)
  when number(A), number(B) ->
    C = A - B,
    if abs(C) < 1.0E-10*abs(A) ->
	    0;
       true ->
	    C
    end;
sub(A, B) ->
    erlang:fault(badarg, [A, B]).



mk_row(M, L) when list(L) ->
    mk_row(M, lists:sort(L), []);
mk_row(_, _) ->
    badarg.

mk_row(_, [], R) ->
    lists:reverse(R);
mk_row(M, [{J,_} | _], _) when J > M ->
    badarg;
mk_row(_, [{J,_}, {J,_} | _], _) ->
    badarg;
mk_row(M, [{J,V} = JV | L], R) when integer(J), number(V), J > 0 ->
    mk_row(M, L, if V == 0 -> R;
		    true -> [JV | R]
		 end);
mk_row(_, _, _) ->
    badarg.



unflatten([], D) ->
    lists:reverse(D);
unflatten([{J,_,_} | _] = C, D) ->
    unflatten_row(J, C, D, []).

unflatten_row(J, [{J,I,_}, {J,I,_} | _], _, _) ->
    badarg;
unflatten_row(J, [{J,I,V} | C], D, Row) ->
    unflatten_row(J, C, D, [{I,V} | Row]);
unflatten_row(J, C, D, Row) ->
    unflatten(C, [{J, lists:reverse(Row)} | D]).



sub_rows([], _, [], C) ->
    lists:reverse(C);
sub_rows([], F, [{Jb,Vb} | RowB], C) ->
    sub_rows([], F, RowB, [{Jb,-F*Vb} | C]);
sub_rows([{Ja,Va} | RowA], F, [], C) ->
    sub_rows(RowA, F, [], [{Ja,Va} | C]);
sub_rows([{J,Va} | RowA], F, [{J,Vb} | RowB], C) ->
    sub_rows(RowA, F, RowB, 
	     case sub(Va, F*Vb) of
		 0 -> C;
		 V -> [{J,V} | C]
	     end);
sub_rows([{Ja,_} = Ja_Va | RowA], F, [{Jb,_} | _] = RowB, C) when Ja < Jb ->
    sub_rows(RowA, F, RowB, [Ja_Va | C]);
sub_rows(RowA, F, [{Jb,Vb} | RowB], C) -> % Ja > Jb
    sub_rows(RowA, F, RowB, [{Jb,-F*Vb} | C]).
