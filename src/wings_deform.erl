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
%%     $Id: wings_deform.erl,v 1.1.1.1 2001/08/14 18:16:40 bjorng Exp $
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
    wings_drag:init_drag(Tvs, {0.0,1.0}, St).

crumple(#shape{id=Id,sh=#we{vs=Vtab}=We}, Vs0, Acc) ->
    {Sa,Sb,Sc} = now(),
    Vs = gb_sets:to_list(Vs0),
    Fun = fun(#shape{sh=#we{vs=Vtab0}=W}=Sh, Dx, Dy, St) ->
		  random:seed(Sa, Sb, Sc),
		  Vt = foldl(
			 fun(V, Vt) ->
				 {R1,R2,R3} = rnd(Dx*2),
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
		       case wings_mat:distance(Center, VPos) of
			   R when R > R0 -> R;
			   Smaller -> R0
		       end
	       end, 0.0, Vs),
    [{Id,foldl(
	  fun(V, A) ->
		  VPos = wings_vertex:pos(V, Vtab),
		  D = wings_mat:distance(Center, VPos),
		  Dir = wings_mat:norm(wings_mat:subtract(VPos, Center)),
		  Vec = wings_mat:mul(Dir, Radius-D),
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
    Key = key(Primary),
    {Min,Max} = range(Key, We),
    Range = Max-Min,
    Tf = taper_fun(Primary, Effect),
    Vs = gb_sets:to_list(Vs0),
    Fun = fun(#shape{sh=#we{vs=Vtab0}=W}=Sh, Dx, Dy, St) ->
		  U = Dx + 1.0,
		  Vt = foldl(
			 fun(V, Vt) ->
				 Rec = gb_trees:get(V, Vt),
				 #vtx{pos=Pos0} = Rec,
				 Pos = Tf(U, Min, Range, Pos0),
				 gb_trees:update(V, Rec#vtx{pos=Pos}, Vt)
			 end, Vtab0, Vs),
		  {shape,Sh#shape{sh=W#we{vs=Vt}}}
	  end,
    [{Id,Fun}|Acc].

taper_fun(x, yz) ->
    fun(U, Min, Range, {X,Y,Z})
       when float(U), float(Min), float(Range), float(X), float(Y), float(Z) ->
	    S0 = (X-Min)/Range,
	    S = mix(S0, U),
	    {X,Y*S,Z*S}
    end;
taper_fun(y, xz) ->
    fun(U, Min, Range, {X,Y,Z})
       when float(U), float(Min), float(Range), float(X), float(Y), float(Z) ->
	    S0 = (Y-Min)/Range,
	    S = mix(S0, U),
	    {X*S,Y,Z*S}
    end;
taper_fun(z, xy) ->
    fun(U, Min, Range, {X,Y,Z})
       when float(U), float(Min), float(Range), float(X), float(Y), float(Z) ->
	    S0 = (Z-Min)/Range,
	    S = mix(S0, U),
	    {X*S,Y*S,Z}
    end;
taper_fun(Primary, Effect0) ->
    Key = key(Primary),
    Effect = key(Effect0),
    fun(U, Min, Range, Pos) when float(U), float(Min), float(Range) ->
	    S0 = (element(Key, Pos)-Min)/Range,
	    S = mix(S0, U),
	    setelement(Effect, Pos, element(Effect, Pos)*S)
    end.
    
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
		  U = (Dx * 2.0 * ?PI)/Range,
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

range(Key, We) ->
    wings_util:fold_vertex(
      fun(V, #vtx{pos=Pos}, {Min0,Max0}=A) ->
	      case element(Key, Pos) of
		  Low when Low < Min0 -> {Low,Max0};
		  High when Max0 < High -> {Min0,High};
		  _ -> A
	      end
      end, {?HUGE,-?HUGE}, We).
