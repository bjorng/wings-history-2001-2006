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
%%     $Id: wings_deform.erl,v 1.29 2002/04/13 07:22:31 bjorng Exp $
%%

-module(wings_deform).
-export([sub_menu/1,command/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1]).
-define(PI, 3.1416).
-compile({inline,[{mix,2}]}).

sub_menu(_St) ->
    InflateHelp = {"Inflate elements",[],"Pick center and radius"},
    {deform,fun(help, _Ns) -> "";
	       (1, _Ns) ->
		    [{"Crumple",{crumple,crumple_dirs()},
		      "Randomly move vertices"},
		     {"Inflate",inflate_fun(),InflateHelp,[]},
		     {"Taper",{taper,
			       [taper_item(x),
				taper_item(y),
				taper_item(z)]}},
		     {"Twist",{twist,dirs(twist)}},
		     {"Torque",{torque,dirs(torque)}}];
	       (_, _Ns) -> ignore
	    end}.

crumple_dirs() ->
    [{"Random",random,"Move each vertex a random amount "
      "in a random direction"},
     {"Normal",normal,"Move each vertex a random amount along its normal"},
     {"X",x,"Move each vertex a random amount along the X axis"},
     {"Y",y,"Move each vertex a random amount along the Y axis"},
     {"Z",z,"Move each vertex a random amount along the Z axis"}].

dirs(Cmd) ->
    [dir(x, Cmd),
     dir(y, Cmd),
     dir(z, Cmd)].

dir(Axis, Cmd) ->
    AxisStr = wings_util:upper(atom_to_list(Axis)),
    Help = "Twist selected vertices around the " ++ AxisStr ++
	case Cmd of
	    twist -> " passing through the center of the selection";
	    torque -> " passing through the origin"
	end,
    {AxisStr,Axis,Help,[]}.

taper_item(Axis) ->
    Effects = effect_menu(Axis),
    AxisStr = wings_util:upper(Axis),
    case wings_pref:get_value(advanced_menus) of
	false ->
	    F = fun(1, Ns) ->
			[Effect|_] = Effects,
			wings_menu:build_command({Axis,Effect}, Ns)
		end,
	    Help = "Taper along " ++ AxisStr,
	    {AxisStr,F,Help};
	true ->
	    F = fun(help, _Ns) ->
			[Effect|_] = Effects,
			TaperAlong = "Taper along " ++ AxisStr,
			{TaperAlong ++ " (with effect on " ++
			 wings_util:upper(Effect) ++ ")",
			 "Choose effect axis",
			 "Pick axis center location"};
		   (1, Ns) ->
			[Effect|_] = Effects,
			wings_menu:build_command(Effect, Ns);
		   (2, _Ns) ->
			expand_effects(Effects, []);
		   (3, Ns) ->
			[Effect|_] = Effects,
			{vector,{pick,[point],[Effect],Ns}}
		end,
	    {AxisStr,{Axis,F},[]}
    end.

effect_menu(x) -> [yz,y,z];
effect_menu(y) -> [xz,x,z];
effect_menu(z) -> [xy,x,y].

expand_effects([H|T], Acc) ->
    Effect = wings_util:upper(H),
    Help = {"Effect on "++Effect,[],"Pick axis center location"},
    Item = {Effect,effect_fun(H),Help,[]},
    expand_effects(T, [Item|Acc]);
expand_effects([], Acc) -> reverse(Acc).

effect_fun(Effect) ->
    fun(1, Ns) -> wings_menu:build_command(Effect, Ns);
       (2, _Ns) -> ignore;
       (3, Ns) -> {vector,{pick,[point],[Effect],Ns}}
    end.
    
inflate_fun() ->
    fun(help, _) -> {"Inflate elements",[],"Pick center and radius"};
       (1, _Ns) -> {vertex,{deform,inflate}};
       (2, _Ns) -> ignore;
       (3, Ns) -> {vector,{pick,[point,point],[],[inflate|Ns]}}
    end.

command({crumple,Dir}, St) -> crumple(Dir, St);
command(inflate, St) -> inflate(St);
command({inflate,What}, St) -> inflate(What, St);
command({taper,{Primary,{Effect,Center}}}, St) ->
    taper(Primary, Effect, Center, St);
command({taper,{Primary,Effect}}, St) ->
    taper(Primary, Effect, center, St);
command({twist,Axis}, St) -> twist(Axis, St);
command({torque,Axis}, St) -> torque(Axis, St).

%%
%% The Crumple deformer.
%%

crumple(Dir, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 crumple(Dir, Vs, We, Acc) end,
			 [], St),
    wings_drag:setup(Tvs, [{percent,{-20.0,20.0}}], St).

crumple(normal, Vs0, #we{id=Id}=We, Acc) ->
    {Sa,Sb,Sc} = now(),
    Vs = gb_sets:to_list(Vs0),
    VsPos0 = wings_util:add_vpos(Vs, We),
    VsPos = [{V,Vtx,wings_vertex:normal(V, We)} || {V,Vtx} <- VsPos0],
    Fun = fun([Dx], A) ->
		  random:seed(Sa, Sb, Sc),
		  foldl(fun({V,#vtx{pos=Pos0}=Rec,N}, VsAcc) ->
				{R1,_,_} = rnd(Dx/10),
				Dis = e3d_vec:mul(N, R1),
				Pos = e3d_vec:add(Pos0, Dis),
				[{V,Rec#vtx{pos=Pos}}|VsAcc]
			end, A, VsPos)
	  end,
    [{Id,{Vs,Fun}}|Acc];
crumple(Dir, Vs0, #we{id=Id}=We, Acc) ->
    {Xmask,Ymask,Zmask} = crumple_mask(Dir),
    {Sa,Sb,Sc} = now(),
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun([Dx], A) ->
		  random:seed(Sa, Sb, Sc),
		  foldl(fun({V,#vtx{pos={X0,Y0,Z0}}=Rec}, VsAcc) ->
				{R1,R2,R3} = rnd(Dx/4),
				X = X0 + R1*Xmask,
				Y = Y0 + R2*Ymask,
				Z = Z0 + R3*Zmask,
				Pos = {X,Y,Z},
				[{V,Rec#vtx{pos=Pos}}|VsAcc]
			end, A, VsPos)
	  end,
    [{Id,{Vs,Fun}}|Acc].

crumple_mask(x) -> {1,0,0};
crumple_mask(y) -> {0,1,0};
crumple_mask(z) -> {0,0,1};
crumple_mask(random) -> {1,1,1}.

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
				 inflate(Center, Radius,
					 gb_sets:to_list(Vs), We, [])
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

%%
%% The Taper deformer.
%%

taper(Primary, Effect, Center, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 taper_2(Vs, We, Primary, Effect, Center, Acc)
			 end, [], St),
    wings_drag:setup(Tvs, [percent], St).

taper_2(Vs, #we{id=Id}=We, Primary, Effect, Center, Acc) ->
    [MinR,MaxR] = wings_vertex:bounding_box(Vs, We),
    Key = key(Primary),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    check_range(Range, Primary),
    taper_3(Id, Vs, We, Key, Effect, MinR, MaxR, Center, Acc).

taper_3(Id, Vs0, We, Key, Effect, MinR, MaxR, Center, Acc) ->
    Tf = taper_fun(Key, Effect, Center, MinR, MaxR),
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun([Dx], A) ->
		  U = Dx + 1.0,
		  foldl(fun({V,#vtx{pos=Pos0}=Rec}, VsAcc) ->
				Pos = Tf(U, Pos0),
				[{V,Rec#vtx{pos=Pos}}|VsAcc]
			end, A, VsPos)
	  end,
    [{Id,{Vs,Fun}}|Acc].

taper_fun(Key, Effect, center, MinR, MaxR) ->
    Center = e3d_vec:average([MinR,MaxR]),
    taper_fun(Key, Effect, Center, MinR, MaxR);
taper_fun(Key, Effect, Center, {_,_,_}=MinR, {_,_,_}=MaxR) ->
    Origin = element(Key, Center),
    Range = element(Key, MaxR) - element(Key, MinR),
    {Ekey1,Ekey2} = effect(Effect),
    Eoffset1 = (element(Ekey1, MinR)+element(Ekey1, MaxR))/2,
    case Ekey2 of
	none ->
	    fun(U, Pos) when is_float(U), is_float(Origin), is_float(Range) ->
		    S0 = 1.0+(element(Key, Pos)-Origin)/Range,
		    S = mix(S0, U),
		    E = S * (element(Ekey1, Pos)-Eoffset1) + Eoffset1,
		    setelement(Ekey1, Pos, E)
	    end;
	_Other ->
	    Eoffset2 = (element(Ekey2, MinR)+element(Ekey2, MaxR))/2,
	    fun(U, Pos0) when is_float(U), is_float(Origin), is_float(Range) ->
		    S0 = 1.0+(element(Key, Pos0)-Origin)/Range,
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

torque(Axis, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 torque(Vs, We, Axis, Acc)
			 end, [], St),
    wings_drag:setup(Tvs, [angle], St).

torque(Vs0, #we{id=Id}=We, Axis, Acc) ->
    Tf = torque_fun(Axis),
    Key = key(Axis),
    [MinR,MaxR] = wings_vertex:bounding_box(Vs0, We),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    check_range(Range, Axis),
    Vs = gb_sets:to_list(Vs0),
    Fun = twister_fun(Vs, Tf, Min, Range, We),
    [{Id,{Vs,Fun}}|Acc].

torque_fun(x) ->
    fun(U, Min, {X,Y,Z})
       when float(U), float(Min), float(X), float(Y), float(Z) ->
	    Angle = U*(X-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X,Y*Cos-Z*Sin,Y*Sin+Z*Cos}
    end;
torque_fun(y) ->
    fun(U, Min, {X,Y,Z})
       when float(U), float(Min), float(X), float(Y), float(Z) ->
	    Angle = U*(Y-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X*Cos+Z*Sin,Y,Z*Cos-X*Sin}
    end;
torque_fun(z) ->
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
