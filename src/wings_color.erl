%%
%%  wings_color.erl --
%%
%%     Color utilites.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_color.erl,v 1.7 2002/12/05 21:08:21 dgud Exp $
%%

-module(wings_color).
-export([init/0,default/0,share/1,store/1,average/1,mix/3,white/0,
	 rgb_to_hsi/1,rgb_to_hsi/3,hsi_to_rgb/1,hsi_to_rgb/3,
	 rgb_to_hsv/1,rgb_to_hsv/3,hsv_to_rgb/1,hsv_to_rgb/3]).

-include("wings.hrl").
-import(lists, [foreach/2]).

-define(BLACK, {0.0,0.0,0.0}).
-define(WHITE, {1.0,1.0,1.0}).

init() ->
    Black = ?BLACK,
    White = ?WHITE,
    Standard = [Black,White,average([Black,White])],
    foreach(fun(C0) ->
		    C = wings_util:share(C0),
		    put(C, C)
	    end, Standard),
    put(wings_default_color, get(White)).

default() ->
    get(wings_default_color).

share({Same,Same}) -> {Same,Same};
share({_,_}=UV) -> UV;
share({_,_,_}=RGB) ->
    case get(RGB) of
	undefined -> wings_util:share(RGB);
	Other -> Other
    end.

store({_,_,_}=RGB) ->
    case get(RGB) of
	undefined ->
	    C = wings_util:share(RGB),
	    put(C, C),
	    C;
	Other -> Other
    end.

average([{V10,V11}|T]=All) ->
    average(T, V10, V11, length(All));
average(Colors) ->
    share(e3d_vec:average(Colors)).

mix(_W, Same, Same) -> Same;
mix(Wa, {Ua,Va}, {Ub,Vb}) when is_float(Wa) ->
    Wb = 1.0 - Wa,
    share({Wa*Ua+Wb*Ub,Wa*Va+Wb*Vb});
mix(Wa, {Ra,Ga,Ba}, {Rb,Gb,Bb}) when is_float(Wa) ->
    Wb = 1.0 - Wa,
    share({Wa*Ra+Wb*Rb,Wa*Ga+Wb*Gb,Wa*Ba+Wb*Bb}).

white() ->
    get(?WHITE).

average([{V10,V11}|T], A0, A1, L)
  when is_float(V10), is_float(V11), is_float(A0), is_float(A1) ->
    average(T, A0+V10, A1+V11, L);
average([], A0, A1, L0) ->
    L = float(L0),
    {A0/L,A1/L}.


rgb_to_hsv({R,G,B}) ->
    rgb_to_hsv(R,G,B).
rgb_to_hsv(R0,G0,B0) ->
%%    {R,G,B} = e3d_vec:norm(float(R0),float(G0),float(B0)),
    {R,G,B} = {R0,G0,B0},
    Max = lists:max([R,G,B]),
    Min = lists:min([R,G,B]),
    V = Max,   
    if Max == Min, V > 0.5 -> %% Hue is unknown/undefined 
	    {60.0, 0.0, V};   %% Set it to yellow which is the ligtest 
       Max == Min ->          %% Hue is unknown/undefined 
	    {300.0, 0.0, V};  %% Set it to magenta which is the darkest
       Min == B ->
	    Sat = (Max-Min)/Max,
	    Hue = 120.0*(G-Min)/(R+G-2.0*Min),
	    {Hue,Sat,V};
       Min == R ->
	    Sat = (Max-Min)/Max,
	    Hue = 120.0*(1.0+(B-Min)/(B+G-2.0*Min)),
	    {Hue,Sat,V};
       Min == G ->
	    Sat = (Max-Min)/Max,
	    Hue = 120.0*(2.0+(R-Min)/(B+R-2.0*Min)),
	    {Hue,Sat,V}
    end.

hsv_to_rgb({H,S,V}) ->
    hsv_to_rgb(H,S,V).
hsv_to_rgb(H,S,V) ->
    Min = V*(1-S),
    if 
	V < 1.0E-5 ->
	    {0.0,0.0,0.0};
	H =< 120.0 ->
	    convert_hsv(H,V,Min);
	H =< 240.0 ->
	    {G,B,R} = convert_hsv(H-120.0,V,Min),
	    {R,G,B};
	true ->
	    {B,R,G} = convert_hsv(H-240.0,V,Min),
	    {R,G,B}
    end.

convert_hsv(H,V,Min) when H =< 60.0 ->
    Mean = Min + H*(V-Min)/(120.0-H),
    {V, Mean, Min};
convert_hsv(H,V,Min) ->
    Mean = Min+(120-H)*(V-Min)/H,
    {Mean,V,Min}.

rgb_to_hsi({R,G,B}) ->
    rgb_to_hsi(R, G, B).

rgb_to_hsi(R, G, B) when is_float(R), is_float(G), is_float(B) ->
    RMinusB = R-B,
    RMinusG = R-G,
    A1 = case math:sqrt(RMinusG*RMinusG+RMinusB*(G-B)) of
	     A0 when A0 < 1.0E-6 -> 0.0;
	     A0 -> 0.5*(RMinusB+RMinusG)/A0
	 end,
    A = 180*math:acos(A1)/math:pi(),
    H = if
	    B =< G -> A;
	    true -> 360 - A
	end,
    S = case R+G+B of
	    Sum when Sum < 1.0E-4 -> 0.0;
	    Sum ->
		MinRGB = lists:min([R,G,B]),
		1.0 - 3*MinRGB/Sum
	end,
    I = (R+G+B)/3,
    {H,S,I}.

hsi_to_rgb({H,S,I}) ->
    hsi_to_rgb(H, S, I).

hsi_to_rgb(H, S, I) when 0 =< H, H < 120 ->
    {B,R,G} = rgb_components(H, S, I),
    {R,G,B};
hsi_to_rgb(H, S, I) when H < 240 ->
    rgb_components(H-120, S, I);
hsi_to_rgb(H, S, I) when H < 360 ->
    {G,B,R} = rgb_components(H-240, S, I),
    {R,G,B}.

rgb_components(H0, S, I) ->
    H = to_radians(H0),
    A = I*(1-S),
    B = clamp(I*(1 + S*math:cos(H)/math:cos(to_radians(60)-H))),
    C = 3*I-(A+B),
    {A,B,C}.
    
to_radians(A) ->
    A*math:pi()/180.

clamp(V) when V < 0.0 -> 0.0;
clamp(V) when V > 1.0 -> 1.0;
clamp(V) -> V.
