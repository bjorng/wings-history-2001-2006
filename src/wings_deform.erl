%%
%%  wings_deform.erl --
%%
%%     This module contains the Deform commands for vertices.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_deform.erl,v 1.27 2002/03/25 09:51:04 bjorng Exp $
%%

-module(wings_deform).
-export([sub_menu/1,command/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1]).
-define(PI, 3.1416).

sub_menu(_St) ->
    InflateHelp = {"Inflate elements",[],"Pick center and radius"},
    {deform,fun(help, _Ns) -> "";
	       (1, _Ns) ->
		    XYZ = [{"X",x},
			   {"Y",y},
			   {"Z",z}],
		    [{"Crumple",crumple},
		     {"Inflate",inflate_fun(),InflateHelp,[]},
		     {"Twist",{twist,XYZ}},
		     {"Twisty Twist",{twisty_twist,XYZ}}];
	       (_, _Ns) -> ignore
	    end}.

inflate_fun() ->
    fun(help, _) -> {"Inflate elements",[],"Pick center and radius"};
       (1, _Ns) -> {vertex,{deform,inflate}};
       (2, _Ns) -> ignore;
       (3, Ns) -> {vector,{pick,[point,point],[],[inflate|Ns]}}
    end.

command(crumple, St) -> crumple(St);
command(inflate, St) -> inflate(St);
command({inflate,What}, St) -> inflate(What, St);
command({twist,Axis}, St) -> twist(Axis, St);
command({twisty_twist,Axis}, St) -> twisty_twist(Axis, St).

%%
%% The Crumple deformer.
%%

crumple(St) ->
    Tvs = wings_sel:fold(fun crumple/3, [], St),
    wings_drag:setup(Tvs, [{distance,{0.0,10.0}}], St).

crumple(Vs0, #we{id=Id}=We, Acc) ->
    {Sa,Sb,Sc} = now(),
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun([Dx], A) ->
		  random:seed(Sa, Sb, Sc),
		  foldl(fun({V,#vtx{pos={X0,Y0,Z0}}=Rec}, VsAcc) ->
				{R1,R2,R3} = rnd(Dx/4),
				X = X0 + R1,
				Y = Y0 + R2,
				Z = Z0 + R3,
				Pos = {X,Y,Z},
				[{V,Rec#vtx{pos=Pos}}|VsAcc]
			end, A, VsPos)
	  end,
    [{Id,{Vs,Fun}}|Acc].

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
    Tvs = wings_sel:fold(fun inflate/3, [], St),
    wings_drag:setup(Tvs, [percent], St).

inflate(Vs0, #we{vs=Vtab}=We, Acc) ->
    Vs = gb_sets:to_list(Vs0),
    Center = wings_vertex:center(Vs, We),
    Radius = foldl(
	       fun(V, R0) ->
		       VPos = wings_vertex:pos(V, Vtab),
		       case e3d_vec:dist(Center, VPos) of
			   R when R > R0 -> R;
			   _Smaller -> R0
		       end
	       end, 0.0, Vs),
    inflate(Center, Radius, Vs, We, Acc).

inflate({Center,Outer}, St) ->
    Radius = e3d_vec:dist(Center, Outer),
    Tvs = wings_sel:fold(fun(Vs, We, _) ->
				 inflate(Center, Radius, gb_sets:to_list(Vs), We, [])
			 end, [], St),
    wings_drag:setup(Tvs, [percent], St).

inflate(Center, Radius, Vs, #we{id=Id,vs=Vtab}, Acc) ->
      [{Id,foldl(
	  fun(V, A) ->
		  VPos = wings_vertex:pos(V, Vtab),
		  D = e3d_vec:dist(Center, VPos),
		  Dir = e3d_vec:norm(e3d_vec:sub(VPos, Center)),
		  Vec = e3d_vec:mul(Dir, Radius-D),
		  [{Vec,[V]}|A]
	  end, [], Vs)}|Acc].
    
%%%
%%% The Twist deformer.
%%%

twist(Axis, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 twist(Vs, We, Axis, Acc)
			 end, [], St),
    wings_drag:setup(Tvs, [angle], St).

twist(Vs0, #we{id=Id}=We, Axis, Acc) ->
    Key = key(Axis),
    [MinR,MaxR] = wings_vertex:bounding_box(Vs0, We),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    check_range(Range, Axis),
    Tf = twist_fun(Axis, e3d_vec:average([MinR,MaxR])),
    Vs = gb_sets:to_list(Vs0),
    Fun = twister_fun(Vs, Tf, Min, Range, We),
    [{Id,{Vs,Fun}}|Acc].

twist_fun(x, {_,Cy,Cz}) ->
    fun(U, Min, {X,Y0,Z0})
       when float(U), float(Min), float(X), float(Y0), float(Z0) ->
	    Angle = U*(X-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    Y = Y0 - Cy,
	    Z = Z0 - Cz,
	    {X,Y*Cos-Z*Sin+Cy,Y*Sin+Z*Cos+Cz}
    end;
twist_fun(y, {Cx,_,Cz}) ->
    fun(U, Min, {X0,Y,Z0})
       when float(U), float(Min), float(X0), float(Y), float(Z0) ->
	    Angle = U*(Y-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    X = X0 - Cx,
	    Z = Z0 - Cz,
	    {X*Cos+Z*Sin+Cx,Y,Z*Cos-X*Sin+Cz}
    end;
twist_fun(z, {Cx,Cy,_}) ->
    fun(U, Min, {X0,Y0,Z})
       when float(U), float(Min), float(X0), float(Y0), float(Z) ->
	    Angle = U*(Z-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    X = X0 - Cx,
	    Y = Y0 - Cy,
	    {X*Cos-Y*Sin+Cx,X*Sin+Y*Cos+Cy,Z}
    end.

%%%
%%% The Twisty Twist deformer.
%%%

twisty_twist(Axis, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 twisty_twist(Vs, We, Axis, Acc)
			 end, [], St),
    wings_drag:setup(Tvs, [angle], St).

twisty_twist(Vs0, #we{id=Id}=We, Axis, Acc) ->
    Tf = twisty_twist_fun(Axis),
    Key = key(Axis),
    [MinR,MaxR] = wings_vertex:bounding_box(Vs0, We),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    check_range(Range, Axis),
    Vs = gb_sets:to_list(Vs0),
    Fun = twister_fun(Vs, Tf, Min, Range, We),
    [{Id,{Vs,Fun}}|Acc].

twisty_twist_fun(x) ->
    fun(U, Min, {X,Y,Z})
       when float(U), float(Min), float(X), float(Y), float(Z) ->
	    Angle = U*(X-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X,Y*Cos-Z*Sin,Y*Sin+Z*Cos}
    end;
twisty_twist_fun(y) ->
    fun(U, Min, {X,Y,Z})
       when float(U), float(Min), float(X), float(Y), float(Z) ->
	    Angle = U*(Y-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X*Cos+Z*Sin,Y,Z*Cos-X*Sin}
    end;
twisty_twist_fun(z) ->
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

twister_fun(Vs, Tf, Min, Range, We) ->
    VsPos = wings_util:add_vpos(Vs, We),
    fun([Angle], A) ->
	    U = (Angle / 180.0 * ?PI)/Range,
	    foldl(fun({V,#vtx{pos=Pos0}=Rec}, VsAcc) ->
			  Pos = Tf(U, Min, Pos0),
			  [{V,Rec#vtx{pos=Pos}}|VsAcc]
		  end, A, VsPos)
    end.

check_range(Range, Axis0) when Range < 0.01 ->
    Axis = wings_util:upper(atom_to_list(Axis0)),
    Error = lists:concat(["Extent along ",Axis," axis is too short."]),
    wings_util:error(Error);
check_range(_Range, _Axis) -> ok.
