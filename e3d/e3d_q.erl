%%
%%  e3d_q.erl --
%%
%%     Operations on quaternions.
%%
%%  Copyright (c) 2003 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_q.erl,v 1.1 2003/03/19 13:55:54 dgud Exp $
%%

%% Quaternions are represented as a {{Qx,Qy,Qz},Qw}
%% to differ them from Vectors.
%% The following is implemented from
%% "The Matrix and Quaternions FAQ"

-module(e3d_q).

-export([identity/0,inverse/1,norm/1,mult/1,mult/2,
	 magnitude/1, conjugate/1,
	 to_rotation_matrix/1, from_rotation_matrix/1,
	 from_angle_axis/2, to_angle_axis/1,
	 vec_rotate/2]).

-compile(inline).
-compile({inline_size,24}).

identity() ->
    {{0.0,0.0,0.0},1.0}.

magnitude({{Qx,Qy,Qz},Qw})
  when is_float(Qx),is_float(Qy),is_float(Qz),is_float(Qw) ->
    math:sqrt(Qx*Qx+Qy*Qy+Qz*Qz+Qw*Qw).

conjugate({{Qx,Qy,Qz},Qw})
  when is_float(Qx),is_float(Qy),is_float(Qz),is_float(Qw) ->
    {{-Qx,-Qy,-Qz},Qw}.

inverse(Q) ->
    conjugate(Q).

norm(Q = {{Qx,Qy,Qz},Qw})
  when is_float(Qx),is_float(Qy),is_float(Qz),is_float(Qw) ->
    M = magnitude(Q),
    case catch {{Qx/M,Qy/M,Qz/M},Qw/M} of
	{'EXIT', _} -> {{0.0,0.0,0.0},0.0};
	R -> R
    end.

mult([H|R]) ->
    mmult(R,H).
mmult([H|R],A) ->
    mmult(R, mult(A,H));
mmult([],A) ->
    A.

mult({{X1,Y1,Z1},W1}, {{X2,Y2,Z2},W2})
  when is_float(X1),is_float(Y1),is_float(Z1),is_float(W1),
       is_float(X2),is_float(Y2),is_float(Z2),is_float(W2) ->
    {{W1*X2+X1*W2+Y1*Z2-Z1*Y2,
      W1*Y2+Y1*W2+Z1*X2-X1*Z2,
      W1*Z2+Z1*W2+X1*Y2-Y1*X2},
     W1*W2-X1*X2-Y1*Y2-Z1*Z2}.

%% We use opengl matrices instead of the mathematical representation
%% {M0,M1,M2,M3,                    {M0 ,M4 ,M8 ,M12,
%%  M4,M5,M6,M7,                     M1 ,M5 ,M9 ,M13,
%%  M8,M9,M10,M11,     instead of    M2 ,M6 ,M10,M14,
%%  M12,M13,M14,M15}                 M3 ,M7 ,M11,M15}
%%

to_rotation_matrix({{Qx,Qy,Qz},Qw})
  when is_float(Qx),is_float(Qy),is_float(Qz),is_float(Qw) ->
    Zero = 0.0,    One  = 1.0,    Two = 2.0,
    XX = Qx*Qx,    XY = Qx*Qy,    XZ = Qx*Qz,    XW = Qx*Qw,
    YY = Qy*Qy,    YZ = Qy*Qz,    YW = Qy*Qw,
    ZZ = Qz*Qz,    ZW = Qz*Qw,

    M0=One-Two*(YY+ZZ), M1=Two*(XY-ZW),     M2=Two*(XZ+YW),
    M4=Two*(XY+ZW),     M5=One-Two*(XX+ZZ), M6=Two*(YZ-XW),
    M8=Two*(XZ-YW),     M9=Two*(YZ+XW),     M10=One-Two*(XX+YY),

    M3=M7=M11=M12=M13=M14=Zero,
    M15 = One,
    {M0, M4, M8, M12,
     M1, M5, M9, M13,
     M2, M6, M10,M14,
     M3, M7, M11,M15}.

from_rotation_matrix(M) when size(M) /= 16 ->
    from_rotation_matrix(e3d_mat:expand(M));
from_rotation_matrix({M0, M4, M8, M12,
		      M1, M5, M9, M13,
		      M2, M6, M10,M14,
		      M3, M7, M11,M15})
  when is_float(M0),is_float(M1),is_float(M2),is_float(M3),
       is_float(M4),is_float(M5),is_float(M6),is_float(M7),
       is_float(M8),is_float(M9),is_float(M10),is_float(M11),
       is_float(M12),is_float(M13),is_float(M14),is_float(M15) ->
    One = 1.0,  Two = 2.0, Eps = 0.000000001,
    T = One + M0 + M5 + M10,
    if T > Eps ->
	    S = math:sqrt(T) * Two,
	    {{(M9-M6)/S, (M2-M8)/S,(M4-M1)/S},0.25*S};
       (M0>M5) and (M0>M10) ->
	    S = math:sqrt(One+M0-M5-M10) * Two,
	    {{0.25*S,(M4+M1)/S,(M2+M8)/S},(M9-M6)/S};
       (M5>M10) ->
	    S = math:sqrt(One+M5-M0-M10),
	    {{(M4+M1)/S,0.25*S,(M9+M6)/S},(M2-M8)/S};
       true ->
	    S = math:sqrt(One+M10-M0-M5),
	    {{(M2+M8)/S,(M9+M6)/S,0.25*S},(M4-M1)/S}
    end.

from_angle_axis(Angle0,Axis0) ->
    HalfAngle = Angle0*3.14159/180/2,
    {X,Y,Z} = e3d_vec:norm(Axis0),
    Sin = math:sin(HalfAngle),
    Cos = math:cos(HalfAngle),
    {{X*Sin,Y*Sin,Z*Sin},Cos}.

to_angle_axis(Q) ->
    {{Qx,Qy,Qz},Qw} = norm(Q),
    Cos = Qw,
    Angle = math:acos(Cos) *2*180/3.14159,
    Sin0  = math:sqrt(1.0 - Cos*Cos),
    Sin   = if
		abs(Sin0) < 0.000005 -> 1.0;
		true -> Sin0
	    end,    
    {Angle,{Qx/Sin,Qy/Sin,Qz/Sin}}.

vec_rotate(V = {_,_,_},Q) ->
    IQ = inverse(Q),
    {Vec0,Scale} = mult([Q,{V,1.0},IQ]),
    if Scale =:= 1.0 -> 
	    Vec0;
       true ->
	    e3d_vec:mul(Vec0, 1.0/Scale)       
    end.
