%%
%%  e3d_mat.erl --
%%
%%     Operations on matrices.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_mat.erl,v 1.7 2001/09/17 07:19:18 bjorng Exp $
%%

-module(e3d_mat).

-export([identity/0,compress/1,expand/1,
	 translate/1,translate/3,scale/1,scale/3,rotate/2,
	 mul/2,mul_point/2]).

identity() ->
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,
     Zero,One,Zero,
     Zero,Zero,One,
     Zero,Zero,Zero}.

compress({A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0}) ->
    {A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}.

expand({A,B,C,_,D,E,F,_,G,H,I,_,Tx,Ty,Tz,_}=Mat) -> Mat;
expand({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}) ->
    {A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0}.

translate({X,Y,Z}) -> translate(X, Y, Z).

translate(Tx, Ty, Tz) ->
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,
     Zero,One,Zero,
     Zero,Zero,One,
     Tx,Ty,Tz}.

scale({X,Y,Z}) -> scale(X, Y, Z).

scale(Sx, Sy, Sz) ->
    Zero = 0.0,
    One = 1.0,
    {Sx,Zero,Zero,
     Zero,Sy,Zero,
     Zero,Zero,Sz,
     Zero,Zero,Zero}.

rotate(A0, {X,Y,Z}=Vec) when float(X), float(Y), float(Z) ->
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
    {M1,M4,M7,
     M2,M5,M8,
     M3,M6,M9,
     0.0,0.0,0.0}.

rot_add([H1|T1], [H2|T2]) -> [H1+H2|rot_add(T1, T2)];
rot_add([], []) -> [].
 
rot_sub([H1|T1], [H2|T2]) -> [H1-H2|rot_sub(T1, T2)];
rot_sub([], []) -> [].

rot_mul([H|T], C) -> [H*C|rot_mul(T, C)];
rot_mul([], C) -> [].

mul({B_a,B_b,B_c,B_d,B_e,B_f,B_g,B_h,B_i,B_tx,B_ty,B_tz},
    {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_tx,A_ty,A_tz})
  when float(A_a), float(A_b), float(A_c), float(A_d), float(A_e),
       float(A_f), float(A_g), float(A_h), float(A_i), float(A_tx),
       float(A_ty), float(A_tz),
       float(B_a), float(B_b), float(B_c), float(B_d), float(B_e),
       float(B_f), float(B_g), float(B_h), float(B_i), float(B_tx),
       float(B_ty), float(B_tz) ->
    {A_a*B_a + A_b*B_d + A_c*B_g,
     A_a*B_b + A_b*B_e + A_c*B_h,
     A_a*B_c + A_b*B_f + A_c*B_i,
     A_d*B_a + A_e*B_d + A_f*B_g,
     A_d*B_b + A_e*B_e + A_f*B_h,
     A_d*B_c + A_e*B_f + A_f*B_i,
     A_g*B_a + A_h*B_d + A_i*B_g,
     A_g*B_b + A_h*B_e + A_i*B_h,
     A_g*B_c + A_h*B_f + A_i*B_i,
     A_tx*B_a + A_ty*B_d + A_tz*B_g + B_tx,
     A_tx*B_b + A_ty*B_e + A_tz*B_h + B_ty,
     A_tx*B_c + A_ty*B_f + A_tz*B_i + B_tz}.

mul_point({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}, {X,Y,Z})
  when float(A), float(B), float(C), float(D), float(E),
       float(F), float(G), float(H), float(I), 
       float(Tx), float(Ty), float(Tz), float(X), float(Y), float(Z) ->
    share(X*A + Y*D + Z*G + Tx,
	  X*B + Y*E + Z*H + Ty,
	  X*C + Y*F + Z*I + Tz);
mul_point({A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0}, {X,Y,Z})
  when float(A), float(B), float(C), float(D), float(E),
       float(F), float(G), float(H), float(I), 
       float(Tx), float(Ty), float(Tz), float(X), float(Y), float(Z) ->
    share(X*A + Y*D + Z*G + Tx,
	  X*B + Y*E + Z*H + Ty,
	  X*C + Y*F + Z*I + Tz).

share(X, X, X) -> {X,X,X};
share(X, X, Z) -> {X,X,Z};
share(X, Y, Y) -> {X,Y,Y};
share(X, Y, X) -> {X,Y,X};
share(X, Y, Z) -> {X,Y,Z}.
