%%
%%  wings_mat.erl --
%%
%%     This module is being phased out. The replacement modules are
%%     e3d_vec and e3d_mat. (This module contains vector and matrix
%%     operations.)
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_mat.erl,v 1.1 2001/08/14 18:16:35 bjorng Exp $
%%

-module(wings_mat).
-export([zero/0,add/1,add/2,subtract/2,distance/2,mul/2,divide/2,
	 is_non_zero/1,is_non_zero/2,
	 dot_product/2,unit_dot_product/2,
	 cross_product/2,norm_cross_product/2,
	 norm/1,norm/3,len/1,
	 negate/1,mult/2,identity/0,translate/3,scale/3,transpose/1,
	 rotate/2,rotate_x/1,rotate_y/1,rotate_z/1,
	 average/1,average_normals/1]).

%% Debugging.
-export([print_matrix/1]).

-include("wings.hrl").
-import(lists, [foldl/3]).

zero() ->
    Z = 0.0,
    {Z,Z,Z}.

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

subtract({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
					    float(V20), float(V21), float(V22) ->
    {V10-V20,V11-V21,V12-V22}.

distance({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
					    float(V20), float(V21), float(V22) ->
    X = V10-V20,
    Y = V11-V21,
    Z = V12-V22,
    math:sqrt(X*X+Y*Y+Z*Z).

mul({V10,V11,V12}, S) when float(S) ->
    {V10*S,V11*S,V12*S}.

divide({V10,V11,V12}, S) when float(S) ->
    {V10/S,V11/S,V12/S}.

negate({X,Y,Z}) -> {-X,-Y,-Z}.

is_non_zero({0.0,0.0,0.0}) -> false;
is_non_zero(_) -> true.

is_non_zero(A, B) ->
    case is_non_zero(A) of
	false -> false;
	true -> is_non_zero(B)
    end.

%% Calculate the dot product for two vectors of arbitrary length.

dot_product({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
					       float(V20), float(V21), float(V22) ->
    V10*V20 + V11*V21 + V12*V22.

%% Calculate the dot product for two unit length vectors.

unit_dot_product({V10,V11,V12}=Va, {V20,V21,V22}=Vb)
  when float(V10), float(V11), float(V12),
       float(V20), float(V21), float(V22) ->
    ?ASSERT(abs(len(Va) - 1) < 1.0E-6),
    ?ASSERT(abs(len(Vb) - 1) < 1.0E-6),
    V10*V20 + V11*V21 + V12*V22.
	      
cross_product({V10,V11,V12}, {V20,V21,V22}) when float(V10), float(V11), float(V12),
						float(V20), float(V21), float(V22) ->
    {V11*V22-V12*V21,V12*V20-V10*V22,V10*V21-V11*V20}.

norm_cross_product({V10,V11,V12}, {V20,V21,V22})
  when float(V10), float(V11), float(V12),
       float(V20), float(V21), float(V22) ->
    norm(V11*V22-V12*V21, V12*V20-V10*V22, V10*V21-V11*V20).

norm({V1,V2,V3}) ->
    norm(V1, V2, V3).

norm(V1, V2, V3) when float(V1), float(V2), float(V3) ->
    D = math:sqrt(V1*V1+V2*V2+V3*V3),
    case catch {V1/D,V2/D,V3/D} of
	{'EXIT',_} -> {0.0,0.0,0.0};
	R -> R
    end.

len({X,Y,Z}) when float(X), float(Y), float(Z) ->
    math:sqrt(X*X+Y*Y+Z*Z).

print_matrix(M) when size(M) == 16 ->
    print_row(1, M),
    print_row(5, M),
    print_row(9, M),
    print_row(13, M).

print_row(I, M) ->
    io:format("~10p ~10p ~10p ~10p\n",
	      [element(I, M),element(I+1, M),
	       element(I+2, M),element(I+3, M)]).

mult(M, N) when size(M) == 16, size(N) == 16 ->
    {m(1, M, 1, N),m(1, M, 2, N),m(1, M, 3, N),m(1, M, 4, N),
     m(5, M, 1, N),m(5, M, 2, N),m(5, M, 3, N),m(5, M, 4, N),
     m(9, M, 1, N),m(9, M, 2, N),m(9, M, 3, N),m(9, M, 4, N),
     m(13, M, 1, N),m(13, M, 2, N),m(13, M, 3, N),m(13, M, 4, N)};
mult(M, N) when size(M) == 16, size(N) == 4 ->
    {m2(1, M, 1, N),m2(5, M, 1, N),m2(9, M, 1, N),m2(13, M, 1, N)}.

m(IM, M, IN, N) ->     
    element(IM, M)*element(IN, N) +
	element(IM+1, M)*element(IN+1*4, N) +
	element(IM+2, M)*element(IN+2*4, N) +
	element(IM+3, M)*element(IN+3*4, N).

m2(IM, M, IN, N) ->     
    element(IM, M)*element(IN, N) +
	element(IM+1, M)*element(IN+1, N) +
	element(IM+2, M)*element(IN+2, N) +
	element(IM+3, M)*element(IN+3, N).

transpose({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
    {M1,M5,M9,M13,M2,M6,M10,M14,M3,M7,M11,M15,M4,M8,M12,M16}.

identity() ->
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,Zero,
     Zero,One,Zero,Zero,
     Zero,Zero,One,Zero,
     Zero,Zero,Zero,One}.

translate(X, Y, Z) ->
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,X,
     Zero,One,Zero,Y,
     Zero,Zero,One,Z,
     Zero,Zero,Zero,One}.

scale(X, Y, Z) ->
    Zero = 0.0,
    One = 1.0,
    {X,Zero,Zero,Zero,
     Zero,Y,Zero,Zero,
     Zero,Zero,Z,Zero,
     Zero,Zero,Zero,One}.

% rotate(A, {1.0,0.0,0.0}) -> rotate_x(A);
% rotate(A, {0.0,1.0,0.0}) -> rotate_y(A);
% rotate(A, {0.0,0.0,1.0}) -> rotate_z(A);
rotate(A0, {X,Y,Z}=Vec) when float(X), float(Y), float(Z) ->
    ?ASSERT(abs(len(Vec)-1) < 1.0E-8),
    A = A0*3.1416/180,
    CosA = math:cos(A),
    SinA = math:sin(A),
    S = [0,-Z,Y,
	 Z,0,-X,
	 -Y,X,0],
    Uut = [X*X,X*Y,X*Z,
	   Y*X,Y*Y,Y*Z,
	   Z*X,Z*Y,Z*Z],
    I = [1,0,0,
	 0,1,0,
	 0,0,1],
    M0 = rot_add(Uut, rot_mul(rot_sub(I, Uut), CosA)),
    M = rot_add(M0, rot_mul(S, SinA)),
    [M1,M2,M3,
     M4,M5,M6,
     M7,M8,M9] = M,
    Zero = 0.0,
    {M1,M2,M3,Zero,
     M4,M5,M6,Zero,
     M7,M8,M9,Zero,
     Zero,Zero,Zero,1.0}.

rot_add([H1|T1], [H2|T2]) -> [H1+H2|rot_add(T1, T2)];
rot_add([], []) -> [].
 
rot_sub([H1|T1], [H2|T2]) -> [H1-H2|rot_sub(T1, T2)];
rot_sub([], []) -> [].

rot_mul([H|T], C) -> [H*C|rot_mul(T, C)];
rot_mul([], C) -> [].
    
rotate_x(A0) ->
    A = A0*math:pi()/180,
    Cos = math:cos(A),
    Sin = math:sin(A),
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,Zero,
     Zero,Cos,-Sin,Zero,
     Zero,Sin,Cos,Zero,
     Zero,Zero,Zero,One}.

rotate_y(A0) ->
    A = A0*math:pi()/180,
    Cos = math:cos(A),
    Sin = math:sin(A),
    Zero = 0.0,
    One = 1.0,
    {Cos,Zero,Sin,Zero,
     Zero,One,Zero,Zero,
     -Sin,Zero,Cos,Zero,
     Zero,Zero,Zero,One}.

rotate_z(A0) ->
    A = A0*math:pi()/180,
    Cos = math:cos(A),
    Sin = math:sin(A),
    Zero = 0.0,
    One = 1.0,
    {Cos,-Sin,Zero,Zero,
     Sin,Cos,Zero,Zero,
     Zero,Zero,One,Zero,
     Zero,Zero,Zero,One}.

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

%% average_normals(Normals) -> Normal
%%  Average normals taking the angle between them into account.
average_normals([N|Ns]) ->
    average_normals(Ns, N).

average_normals([N0|Ns], Sum0) ->
    Sum1 = add(N0, norm(Sum0)),
    Dot = dot_product(N0, Sum1),
    Sum = add(divide(N0, Dot), Sum0),
    average_normals(Ns, Sum);
average_normals([], Sum) -> Sum.


