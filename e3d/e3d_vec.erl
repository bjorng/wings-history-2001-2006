%%
%%  e3d_vec.erl --
%%
%%     Arithmetic on vectors and points (represented as three-tuples).
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_vec.erl,v 1.6 2001/09/06 12:02:58 bjorng Exp $
%%

-module(e3d_vec).

-export([zero/0,is_zero/1,add/1,add/2,sub/1,sub/2,mul/2,divide/2,neg/1,
	 dot/2,cross/2,norm_cross/2,len/1,dist/2,norm/1,normal/3,
	 average/1]).
-compile({inline,[{norm,3}]}).

zero() ->
    {0.0,0.0,0.0}.

is_zero({0.0,0.0,0.0}) -> true;
is_zero(_) -> false.

add({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
				       float(V20), float(V21), float(V22) ->
    {V10+V20,V11+V21,V12+V22}.

add([{V10,V11,V12}|T]) ->
    add(T, V10, V11, V12).

add([{V10,V11,V12},{V20,V21,V22},{V30,V31,V32}|T], A0, A1, A2)
  when float(V10), float(V11), float(V12),
       float(V20), float(V21), float(V22),
       float(V30), float(V31), float(V32),
       float(A0), float(A1), float(A2) ->
    add(T, A0+V10+V20+V30, A1+V11+V21+V31, A2+V12+V22+V32);
add([{V10,V11,V12},{V20,V21,V22}|T], A0, A1, A2)
  when float(V10), float(V11), float(V12),
       float(V20), float(V21), float(V22),
       float(A0), float(A1), float(A2) ->
    add(T, A0+V10+V20, A1+V11+V21, A2+V12+V22);
add([{V10,V11,V12}|T], A0, A1, A2) when float(V10), float(V11), float(V12),
					float(A0), float(A1), float(A2) ->
    add(T, A0+V10, A1+V11, A2+V12);
add([], A0, A1, A2) -> {A0,A1,A2}.

sub({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
				       float(V20), float(V21), float(V22) ->
    {V10-V20,V11-V21,V12-V22}.

sub([{V10,V11,V12}|T]) ->
    sub(V10, V11, V12, T).

sub(A0, A1, A2, [{V10,V11,V12}|T]) ->
    sub(A0-V10, A1-V11, A2-V12, T);
sub(A0, A1, A2, []) -> {A0,A1,A2}.

mul({V10,V11,V12}, S) when float(V10), float(V11), float(V12), float(S) ->
    {V10*S,V11*S,V12*S}.

divide({V10,V11,V12}, S) when float(V10), float(V11), float(V12), float(S) ->
    {V10/S,V11/S,V12/S}.

neg({X,Y,Z}) -> {-X,-Y,-Z}.

dot({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
				       float(V20), float(V21), float(V22) ->
    V10*V20 + V11*V21 + V12*V22.

cross({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
					 float(V20), float(V21), float(V22) ->
    {V11*V22-V12*V21,V12*V20-V10*V22,V10*V21-V11*V20}.

norm_cross({V10,V11,V12}, {V20,V21,V22})
  when float(V10), float(V11), float(V12),
       float(V20), float(V21), float(V22) ->
    norm(V11*V22-V12*V21, V12*V20-V10*V22, V10*V21-V11*V20).

len({X,Y,Z}) when float(X), float(Y), float(Z) ->
    math:sqrt(X*X+Y*Y+Z*Z).

dist({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
					float(V20), float(V21), float(V22) ->
    X = V10-V20,
    Y = V11-V21,
    Z = V12-V22,
    math:sqrt(X*X+Y*Y+Z*Z).

norm({V1,V2,V3}) ->
    norm(V1, V2, V3).

norm(V1, V2, V3) when float(V1), float(V2), float(V3) ->
    D = math:sqrt(V1*V1+V2*V2+V3*V3),
    case catch {V1/D,V2/D,V3/D} of
	{'EXIT',_} -> {0.0,0.0,0.0};
	R -> R
    end.

normal({V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32})
  when float(V10), float(V11), float(V12),
       float(V20), float(V21), float(V22),
       float(V30), float(V31), float(V32) ->
    D10 = V10-V20,
    D11 = V11-V21,
    D12 = V12-V22,
    D20 = V20-V30,
    D21 = V21-V31,
    D22 = V22-V32,
    N0 = D11*D22-D12*D21,
    N1 = D12*D20-D10*D22,
    N2 = D10*D21-D11*D20,
    D = math:sqrt(N0*N0+N1*N1+N2*N2),
    case catch {N0/D,N1/D,N2/D} of
	{'EXIT',_} -> {0.0,0.0,0.0};
	R -> R
    end.

%% average([{X,Y,Z}]) -> {Ax,Ay,Az}
%%  Average the given list of points.
average([{V10,V11,V12}|T]=All) ->
    average(T, V10, V11, V12, length(All)).

average([{V10,V11,V12},{V20,V21,V22},{V30,V31,V32}|T], A0, A1, A2, L)
  when float(V10), float(V11), float(V12),
       float(V20), float(V21), float(V22),
       float(V30), float(V31), float(V32),
       float(A0), float(A1), float(A2) ->
    average(T, A0+V10+V20+V30, A1+V11+V21+V31, A2+V12+V22+V32, L);
average([{V10,V11,V12},{V20,V21,V22}|T], A0, A1, A2, L)
  when float(V10), float(V11), float(V12),
       float(V20), float(V21), float(V22),
       float(A0), float(A1), float(A2) ->
    average(T, A0+V10+V20, A1+V11+V21, A2+V12+V22, L);
average([{V10,V11,V12}|T], A0, A1, A2, L)
  when float(V10), float(V11), float(V12),
       float(A0), float(A1), float(A2) ->
    average(T, A0+V10, A1+V11, A2+V12, L);
average([], A0, A1, A2, L0) ->
    L = float(L0),
    {A0/L,A1/L,A2/L}.
