%%
%%  wings_deform.erl --
%%
%%     This module contains the Deform commands for vertices.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_deform.erl,v 1.11 2001/10/23 17:09:12 bjorng Exp $
%%

-module(wings_deform).
-export([crumple/1,inflate/1,taper/3,twist/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1]).
-define(HUGE, 1.0E200).
-define(PI, 3.1416).
-compile({inline,[{mix,2}]}).

%%
%% The Crumple deformer.
%%

crumple(St) ->
    Tvs = wings_sel:fold_shape(fun crumple/3, [], St),
    wings_drag:init_drag(Tvs, {0.0,10.0}, St).

crumple(#shape{id=Id,sh=#we{vs=Vtab}=We}, Vs0, Acc) ->
    {Sa,Sb,Sc} = now(),
    Vs = gb_sets:to_list(Vs0),
    Fun = fun(#shape{sh=#we{vs=Vtab0}=W}=Sh, Dx, Dy, St) ->
		  random:seed(Sa, Sb, Sc),
		  Vt = foldl(
			 fun(V, Vt) ->
				 {R1,R2,R3} = rnd(Dx/4),
				 Rec = gb_trees:get(V, Vt),
				 #vtx{pos={X0,Y0,Z0}} = Rec,
				 X = X0 + R1,
				 Y = Y0 + R2,
				 Z = Z0 + R3,
				 Pos = {X,Y,Z},
				 gb_trees:update(V, Rec#vtx{pos=Pos}, Vt)
			 end, Vtab0, Vs),
		  {shape,Sh#shape{sh=W#we{vs=Vt}}}
	  end,
    [{Id,Fun}|Acc].

rnd(Sc) when float(Sc) ->
    %% Use Box-Muller's method for generation of normally-distributed
    %% random numbers.
    X1 = random:uniform(),
    X2 = random:uniform(),
    A1 = 2*?PI*X2,
    R1 = math:sqrt(-2.0*math:log(X1)),
    Y1 = R1*math:cos(A1),
    Y2 = R1*math:sin(A1),
    
    X3 = random:uniform(),
    X4 = random:uniform(),
    Y3 = math:sqrt(-2.0*math:log(X3))*math:cos(2*?PI*X4),
    
    {Sc*(Y1-0.5),Sc*(Y2-0.5),Sc*(Y3-0.5)}.

%%
%% The Inflate deformer.
%%

inflate(St) ->
    Tvs = wings_sel:fold_shape(fun inflate/3, [], St),
    wings_drag:init_drag(Tvs, {-1.0,1.0}, St).

inflate(#shape{id=Id,sh=#we{vs=Vtab}=We}, Vs0, Acc) ->
    Vs = gb_sets:to_list(Vs0),
    Center = wings_vertex:center(Vs, We),
    Radius = foldl(
	       fun(V, R0) ->
		       VPos = wings_vertex:pos(V, Vtab),
		       case e3d_vec:dist(Center, VPos) of
			   R when R > R0 -> R;
			   Smaller -> R0
		       end
	       end, 0.0, Vs),
    [{Id,foldl(
	  fun(V, A) ->
		  VPos = wings_vertex:pos(V, Vtab),
		  D = e3d_vec:dist(Center, VPos),
		  Dir = e3d_vec:norm(e3d_vec:sub(VPos, Center)),
		  Vec = e3d_vec:mul(Dir, Radius-D),
		  [{Vec,[V]}|A]
	  end, [], Vs)}|Acc].
			       

%%
%% The Taper deformer.
%%

taper(Primary, Effect, St) ->
    Tvs = wings_sel:fold_shape(fun(Sh, Vs, Acc) ->
				       taper(Sh, Vs, Primary, Effect, Acc)
			       end, [], St),
    wings_drag:init_drag(Tvs, {-1.0,?HUGE}, St).

taper(#shape{id=Id,sh=We}, Vs0, Primary, Effect, Acc) ->
    [MinR,MaxR] = wings_vertex:bounding_box(Vs0, We),
    Key = key(Primary),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    Tf = taper_fun(Key, Effect, MinR, MaxR),
    Vs = gb_sets:to_list(Vs0),
    Fun = fun(#shape{sh=#we{vs=Vtab0}=W}=Sh, Dx, Dy, St) ->
		  U = Dx + 1.0,
		  Vt = foldl(
			 fun(V, Vt) ->
				 Rec = gb_trees:get(V, Vt),
				 #vtx{pos=Pos0} = Rec,
				 Pos = Tf(U, Pos0),
				 gb_trees:update(V, Rec#vtx{pos=Pos}, Vt)
			 end, Vtab0, Vs),
		  {shape,Sh#shape{sh=W#we{vs=Vt}}}
	  end,
    [{Id,Fun}|Acc].

taper_fun(Key, Effect, {IX,IY,IZ}=MinR, {AX,AY,AZ}=MaxR) ->
    Min = element(Key, MinR),
    Range = element(Key, MaxR) - element(Key, MinR),
    {Ekey1,Ekey2} = effect(Effect),
    io:format("~w\n", [{Ekey1,Ekey2}]),
    Eoffset1 = (element(Ekey1, MinR)+element(Ekey1, MaxR))/2,
    case Ekey2 of
	none ->
	    fun(U, Pos) when float(U), float(Min), float(Range) ->
		    S0 = (element(Key, Pos)-Min)/Range,
		    S = mix(S0, U),
		    E = S * (element(Ekey1, Pos)-Eoffset1) + Eoffset1,
		    setelement(Ekey1, Pos, E)
	    end;
	Other ->
	    Eoffset2 = (element(Ekey2, MinR)+element(Ekey2, MaxR))/2,
	    fun(U, Pos0) when float(U), float(Min), float(Range) ->
		    S0 = (element(Key, Pos0)-Min)/Range,
		    S = mix(S0, U),
		    E0 = S * (element(Ekey1, Pos0)-Eoffset1) + Eoffset1,
		    E =  S * (element(Ekey2, Pos0)-Eoffset2) + Eoffset2,
		    Pos = setelement(Ekey1, Pos0, E0),
		    setelement(Ekey2, Pos, E)
	    end
    end.

effect(yz) -> {2,3};
effect(xy) -> {1,2};
effect(xz) -> {1,3};
effect(Single) -> {key(Single),none}.

mix(A, F) ->
    F + (1-F)*A.
    
%%%
%%% The Twist deformer.
%%%

twist(Axis, St) ->
    Tvs = wings_sel:fold_shape(fun(Sh, Vs, Acc) ->
				       twist(Sh, Vs, Axis, Acc)
			       end, [], St),
    wings_drag:init_drag(Tvs, none, St).

twist(#shape{id=Id,sh=We}, Vs0, Axis, Acc) ->
    Tf = twist_fun(Axis),
    Key = key(Axis),
    {Min,Max} = range(Key, We),
    Range = Max-Min,
    Vs = gb_sets:to_list(Vs0),
    Fun = fun(#shape{sh=#we{vs=Vtab0}=W}=Sh, Dx, Dy, St) ->
		  Angle = Dx * 15,
		  U = (Angle / 180.0 * ?PI)/Range,
		  wings_io:message(lists:flatten(
				     io_lib:format("A:~10p",
						   [Angle]))),
		  Vt = foldl(
			 fun(V, Vt) ->
				 Rec = gb_trees:get(V, Vt),
				 #vtx{pos=Pos0} = Rec,
				 Pos = Tf(U, Min, Pos0),
				 gb_trees:update(V, Rec#vtx{pos=Pos}, Vt)
			 end, Vtab0, Vs),
		  {shape,Sh#shape{sh=W#we{vs=Vt}}}
	  end,
    [{Id,Fun}|Acc].

twist_fun(x) ->
    fun(U, Min, {X,Y,Z})
       when float(U), float(Min), float(X), float(Y), float(Z) ->
	    Angle = U*(X-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X,Y*Cos-Z*Sin,Y*Sin+Z*Cos}
    end;
twist_fun(y) ->
    fun(U, Min, {X,Y,Z})
       when float(U), float(Min), float(X), float(Y), float(Z) ->
	    Angle = U*(Y-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X*Cos+Z*Sin,Y,Z*Cos-X*Sin}
    end;
twist_fun(z) ->
    fun(U, Min, {X,Y,Z})
       when float(U), float(Min), float(X), float(Y), float(Z) ->
	    Angle = U*(Z-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X*Cos-Y*Sin,X*Sin+Y*Cos,Z}
    end.

%%%
%%% Utilities.
%%%

key(x) -> 1;
key(y) -> 2;
key(z) -> 3.

range(Key, #we{vs=Vtab}) ->
    [#vtx{pos=Pos0}|Vrecs] = gb_trees:values(Vtab),
    M = element(Key, Pos0),
    foldl(fun(#vtx{pos=Pos}, {Min0,Max0}=A) ->
		  case element(Key, Pos) of
		      Low when Low < Min0 -> {Low,Max0};
		      High when Max0 < High -> {Min0,High};
		      _ -> A
		  end
	  end, {M,M}, Vrecs).
