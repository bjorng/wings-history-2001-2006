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
%%     $Id: e3d_mat.erl,v 1.25 2003/05/13 14:38:11 bjorng Exp $
%%

-module(e3d_mat).

-export([identity/0,is_identity/1,compress/1,expand/1,
	 translate/1,translate/3,scale/1,scale/3,
	 rotate/2,rotate_to_z/1,rotate_s_to_t/2,
	 project_to_plane/1,
	 transpose/1,mul/2,mul_point/2,mul_vector/2]).
-compile(inline).

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
    A = A0*3.14159/180,
    CosA = math:cos(A),
    SinA = math:sin(A),
    XSinA = X*SinA,
    YSinA = Y*SinA,
    ZSinA = Z*SinA,
    {C2,C3, C4,C6, C7,C8} =
	{-ZSinA,YSinA,
	 ZSinA,-XSinA,
	 -YSinA,XSinA},
    {U1,U2,U3, U5,U6, U9} =
	{X*X,X*Y,X*Z,
	     Y*Y,Y*Z,
	         Z*Z},
    U4 = U2,
    U7 = U3,
    U8 = U6,
    S = CosA,
    NegS = -S,
    {U1+S*(1.0-U1), U4+NegS*U4+C4, U7+NegS*U7+C7,
     U2+NegS*U2+C2, U5+S*(1.0-U5), U8+NegS*U8+C8,
     U3+NegS*U3+C3, U6+NegS*U6+C6, U9+S*(1.0-U9),
     0.0,0.0,0.0}.

%% Project to plane perpendicular to vector Vec.
project_to_plane(Vec) ->
    %%       T
    %% P = QQ
    %% (Strang: Linear Algebra and its Applications, 3rd edition, p 170.)
    {Ux,Vx,_,
     Uy,Vy,_,
     Uz,Vz,_,
     _,_,_} = rotate_to_z(Vec),
    if
	is_float(Ux), is_float(Uy), is_float(Uz) ->
	    {Ux*Ux+Vx*Vx,Uy*Ux+Vy*Vx,Uz*Ux+Vz*Vx,
	     Ux*Uy+Vx*Vy,Uy*Uy+Vy*Vy,Uz*Uy+Vz*Vy,
	     Ux*Uz+Vx*Vz,Uy*Uz+Vy*Vz,Uz*Uz+Vz*Vz,
	     0.0,0.0,0.0}
    end.

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
    %% Tomas Moller/Eric Haines: Real-Time Rendering (ISBN 1-56881-101-2).
    %%  3.3. Quaternions; Rotating one vector to another
    case e3d_vec:dot(S, T) of
 	E when abs(E) > 0.999999 ->
 	    almost_parallel(S, T);
	E ->
	    V = e3d_vec:cross(S, T),
	    rotate_s_to_t_1(V, E)
    end.

almost_parallel(S, T) ->
    %% Parallel case as in Moller/Hughes:
    %%  http://www.acm.org/jgt/papers/MollerHughes99
    Axis = closest_axis(S),
    U = e3d_vec:sub(Axis, S),
    V = e3d_vec:sub(Axis, T),

    C1 = 2.0 / e3d_vec:dot(U, U),
    C2 = 2.0 / e3d_vec:dot(V, V),
    C3 = C1 * C2 * e3d_vec:dot(U, V),
    C = {C1,C2,C3,U,V},

    {1.0+ael(C, 1, 1),ael(C, 2, 1),ael(C, 3, 1),
     ael(C, 1, 2),1.0+ael(C, 2, 2),ael(C, 3, 2),
     ael(C, 1, 3),ael(C, 2, 3),1.0+ael(C, 3, 3),
     0.0,0.0,0.0}.

ael({C1,C2,C3,U,V}, I, J) ->
    -C1 * element(I, U) * element(J, U) -
	C2 * element(I, V) * element(J, V) +
	C3 * element(I, V) * element(J, U).

closest_axis({X0,Y0,Z0}) ->
    X = abs(X0),
    Y = abs(Y0),
    Z = abs(Z0),
    if
	X < Y ->
	    if
		X < Z -> {1.0,0.0,0.0};
		true -> {0.0,0.0,1.0}
	    end;
	true ->
	    if
		Y < Z -> {0.0,1.0,0.0};
		true -> {0.0,0.0,1.0}
	    end
    end.
    
rotate_s_to_t_1({Vx,Vy,Vz}, E) when is_float(Vx), is_float(Vy), is_float(Vz) ->
    H = (1.0 - E)/(Vx*Vx+Vy*Vy+Vz*Vz),
    HVx = H*Vx,
    HVz = H*Vz,
    HVxy = HVx*Vy,
    HVxz = HVx*Vz,
    HVyz = HVz*Vy,
    {E+HVx*Vx,HVxy+Vz,HVxz-Vy,
     HVxy-Vz,E+H*Vy*Vy,HVyz+Vx,
     HVxz+Vy,HVyz-Vx,E+HVz*Vz,
     0.0,0.0,0.0}.

transpose({M1,M2,M3,M4,M5,M6,M7,M8,M9,0.0=Z,0.0,0.0}) ->
    {M1,M4,M7,
     M2,M5,M8,
     M3,M6,M9,
     Z,Z,Z}.

mul({1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,B_tx,B_ty,B_tz},
    {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_tx,A_ty,A_tz})
  when is_float(A_tx), is_float(A_ty), is_float(A_tz),
       is_float(B_tx), is_float(B_ty), is_float(B_tz) ->
    {A_a, A_b, A_c,
     A_d, A_e, A_f,
     A_g, A_h, A_i,
     A_tx + B_tx,
     A_ty + B_ty,
     A_tz + B_tz};
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
mul({B_a,B_b,B_c,B_d,B_e,B_f,B_g,B_h,B_i,B_j,B_k,B_l,B_tx,B_ty,B_tz,B_w},
    {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_j,A_k,A_l,A_tx,A_ty,A_tz,A_w})
  when is_float(A_a), is_float(A_b), is_float(A_c), is_float(A_d),
       is_float(A_e), is_float(A_f), is_float(A_g), is_float(A_h),
       is_float(A_i), is_float(A_j), is_float(A_k), is_float(A_l),
       is_float(A_tx),is_float(A_ty), is_float(A_tz), is_float(A_w) ->
    {A_a*B_a + A_b*B_e + A_c*B_i + A_d*B_tx,
     A_a*B_b + A_b*B_f + A_c*B_j + A_d*B_ty,
     A_a*B_c + A_b*B_g + A_c*B_k + A_d*B_tz,
     A_a*B_d + A_b*B_h + A_c*B_l + A_d*B_w,

     A_e*B_a + A_f*B_e + A_g*B_i + A_h*B_tx,
     A_e*B_b + A_f*B_f + A_g*B_j + A_h*B_ty,
     A_e*B_c + A_f*B_g + A_g*B_k + A_h*B_tz,
     A_e*B_d + A_f*B_h + A_g*B_l + A_h*B_w,

     A_i*B_a + A_j*B_e + A_k*B_i + A_l*B_tx,
     A_i*B_b + A_j*B_f + A_k*B_j + A_l*B_ty,
     A_i*B_c + A_j*B_g + A_k*B_k + A_l*B_tz,
     A_i*B_d + A_j*B_h + A_k*B_l + A_l*B_w,

     A_tx*B_a + A_ty*B_e + A_tz*B_i + A_w*B_tx,
     A_tx*B_b + A_ty*B_f + A_tz*B_j + A_w*B_ty,
     A_tx*B_c + A_ty*B_g + A_tz*B_k + A_w*B_tz,
     A_tx*B_d + A_ty*B_h + A_tz*B_l + A_w*B_w};
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
