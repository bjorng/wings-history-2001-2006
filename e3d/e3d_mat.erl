%%
%%  e3d_mat.erl --
%%
%%     Operations on matrices.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_mat.erl,v 1.16 2002/06/19 09:35:13 bjorng Exp $
%%

-module(e3d_mat).

-export([identity/0,is_identity/1,compress/1,expand/1,
	 translate/1,translate/3,scale/1,scale/3,
	 rotate/2,rotate_to_z/1,rotate_s_to_t/2,
	 transpose/1,mul/2,mul_point/2,mul_vector/2]).

identity() ->
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,
     Zero,One,Zero,
     Zero,Zero,One,
     Zero,Zero,Zero}.

is_identity({1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0}) -> true;
is_identity({1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,
	     0.0,0.0,0.0,0.1}) -> true;
is_identity({_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}) -> false;
is_identity({_,_,_,_,_,_,_,_,_,_,_,_}) -> false.

compress({A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0}) ->
    {A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}.

expand({_A,_B,_C,_,_D,_E,_F,_,_G,_H,_I,_,_Tx,_Ty,_Tz,_}=Mat) -> Mat;
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

scale({X,Y,Z}) -> scale(X, Y, Z);
scale(Sc) when is_float(Sc) -> scale(Sc, Sc, Sc).

scale(Sx, Sy, Sz) ->
    Zero = 0.0,
    {Sx,Zero,Zero,
     Zero,Sy,Zero,
     Zero,Zero,Sz,
     Zero,Zero,Zero}.

rotate(A0, {X,Y,Z}) when is_float(X), is_float(Y), is_float(Z) ->
    A = A0*3.1416/180,
    CosA = math:cos(A),
    SinA = math:sin(A),
    SSinA = {0.0,-Z*SinA,Y*SinA,
	     Z*SinA,0.0,-X*SinA,
	     -Y*SinA,X*SinA,0.0},
    Uut = {X*X,X*Y,X*Z,
	   Y*X,Y*Y,Y*Z,
	   Z*X,Z*Y,Z*Z},
    rot_add(Uut, rot_ineg_mul(Uut, CosA), SSinA).

rot_add({A1,A2,A3,A4,A5,A6,A7,A8,A9},
	{B1,B2,B3,B4,B5,B6,B7,B8,B9},
	{C1,C2,C3,C4,C5,C6,C7,C8,C9})
  when is_float(A1), is_float(A2), is_float(A3),
       is_float(A4), is_float(A5), is_float(A6),
       is_float(A7), is_float(A8), is_float(A9) ->
    {A1+B1+C1,A4+B4+C4,A7+B7+C7,
     A2+B2+C2,A5+B5+C5,A8+B8+C8,
     A3+B3+C3,A6+B6+C6,A9+B9+C9,
     0.0,0.0,0.0}.

rot_ineg_mul({M1,M2,M3,M4,M5,M6,M7,M8,M9}, S) when is_float(S) ->
    {S*(1.0-M1),-S*M2,-S*M3,
     -S*M4,S*(1.0-M5),-S*M6,
     -S*M7,-S*M8,S*(1.0-M9)}.

rotate_to_z(Vec) ->
    {Vx,Vy,Vz} = V =
	case e3d_vec:norm(Vec) of
	    {Wx,Wy,Wz}=W when abs(Wx) < abs(Wy), abs(Wx) < abs(Wz) ->
		e3d_vec:norm(0.0, Wz, -Wy);
	    {Wx,Wy,Wz}=W when abs(Wy) < abs(Wz) ->
		e3d_vec:norm(Wz, 0.0, -Wx);
	    {Wx,Wy,Wz}=W ->
		e3d_vec:norm(Wy, -Wx, 0.0)
	end,
    {Ux,Uy,Uz} = e3d_vec:cross(V, W),
    {Ux,Vx,Wx,
     Uy,Vy,Wy,
     Uz,Vz,Wz,
     0.0,0.0,0.0}.

rotate_s_to_t(S, T) ->
    V = e3d_vec:cross(S, T),
    case e3d_vec:dot(S, T) of
	E when E > 0.999 ->
	    identity();
	E when E < -0.999 ->
	    rotate_180(S);
	E ->
	    rotate_so_to_t_1(V, E)
    end.

rotate_180(S) ->
    X = {1.0,0.0,0.0},
    V = case e3d_vec:dot(S, X) of
	    Dot when abs(Dot) > 0.999 ->
		Y = {0.0,1.0,0.0},
		e3d_vec:cross(Y, S);
	    _ ->
		e3d_vec:cross(X, S)
	end,
    rotate(180, V).

rotate_so_to_t_1({Vx,Vy,Vz}=V, E) when is_float(Vx), is_float(Vy), is_float(Vz) ->
    H = (1.0 - E)/e3d_vec:dot(V, V),
    {E+H*Vx*Vx,H*Vx*Vy+Vz,H*Vx*Vz-Vy,
     H*Vx*Vy-Vz,E+H*Vy*Vy,H*Vy*Vz+Vx,
     H*Vx*Vz+Vy,H*Vy*Vz-Vx,E+H*Vz*Vz,
     0.0,0.0,0.0}.

transpose({M1,M2,M3,M4,M5,M6,M7,M8,M9,0.0=Z,0.0,0.0}) ->
    {M1,M4,M7,
     M2,M5,M8,
     M3,M6,M9,
     Z,Z,Z}.

mul({B_a,B_b,B_c,B_d,B_e,B_f,B_g,B_h,B_i,B_tx,B_ty,B_tz},
    {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_tx,A_ty,A_tz})
  when is_float(A_a), is_float(A_b), is_float(A_c), is_float(A_d), is_float(A_e),
       is_float(A_f), is_float(A_g), is_float(A_h), is_float(A_i), is_float(A_tx),
       is_float(A_ty), is_float(A_tz),
       is_float(B_a), is_float(B_b), is_float(B_c), is_float(B_d), is_float(B_e),
       is_float(B_f), is_float(B_g), is_float(B_h), is_float(B_i), is_float(B_tx),
       is_float(B_ty), is_float(B_tz) ->
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
     A_tx*B_c + A_ty*B_f + A_tz*B_i + B_tz};
mul({A,B,C,Q0,D,E,F,Q1,G,H,I,Q2,Tx,Ty,Tz,Q3}, {X,Y,Z,W})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz),
       is_float(Q0), is_float(Q1), is_float(Q2), is_float(Q3),
       is_float(X), is_float(Y), is_float(Z) ->
    {X*A + Y*D + Z*G + W*Tx,
     X*B + Y*E + Z*H + W*Ty,
     X*C + Y*F + Z*I + W*Tz,
     X*Q0 + Y*Q1 + Z*Q2 + W*Q3}.

mul_point({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G + Tx,
	  X*B + Y*E + Z*H + Ty,
	  X*C + Y*F + Z*I + Tz);
mul_point({A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G + Tx,
	  X*B + Y*E + Z*H + Ty,
	  X*C + Y*F + Z*I + Tz).

mul_vector({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G,
	  X*B + Y*E + Z*H,
	  X*C + Y*F + Z*I);
mul_vector({A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G,
	  X*B + Y*E + Z*H,
	  X*C + Y*F + Z*I).

share(X, X, X) -> {X,X,X};
share(X, X, Z) -> {X,X,Z};
share(X, Y, Y) -> {X,Y,Y};
share(X, Y, X) -> {X,Y,X};
share(X, Y, Z) -> {X,Y,Z}.
