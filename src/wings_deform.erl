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
%%     $Id: wings_deform.erl,v 1.18 2001/12/23 11:32:46 bjorng Exp $
%%

-module(wings_deform).
-export([sub_menu/1,command/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1]).
-define(HUGE, 1.0E200).
-define(PI, 3.1416).
-compile({inline,[{mix,2}]}).

sub_menu(St) ->
    XYZ = {{"X",x},
	   {"Y",y},
	   {"Z",z}},
    {deform,{{"Crumple",crumple},
	     {"Inflate",inflate},
	     {"Taper",{taper,
		       {{"Along",ignore},
			separator,
			{"X",taper_item(x)},
			{"Y",taper_item(y)},
			{"Z",taper_item(z)}}}},
	     {"Twist",{twist,XYZ}},
	     {"Twisty Twist",{twisty_twist,XYZ}}}}.

taper_item(x) -> taper_item_1([yz,y,z], x, []);
taper_item(y) -> taper_item_1([xz,x,z], y, []);
taper_item(z) -> taper_item_1([xy,x,y], z, []).

taper_item_1([H|T], Label, Acc) ->		    
    taper_item_1(T, Label, [{wings_util:upper(atom_to_list(H)),H}|Acc]);
taper_item_1([], Label, Acc) ->
    {Label,list_to_tuple([{"Effect",ignore},
			  separator|reverse(Acc)])}.

command(crumple, St) -> crumple(St);
command(inflate, St) -> inflate(St);
command({taper,{Primary,Effect}}, St) -> taper(Primary, Effect, St);
command({twist,Axis}, St) -> twist(Axis, St);
command({twisty_twist,Axis}, St) -> twisty_twist(Axis, St).

%%
%% The Crumple deformer.
%%

crumple(St) ->
    Tvs = wings_sel:fold_shape(fun crumple/3, [], St),
    wings_drag:init_drag(Tvs, {0.0,10.0}, St).

crumple(#shape{id=Id,sh=We}, Vs0, Acc) ->
    {Sa,Sb,Sc} = now(),
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun(Dx, Dy, A) ->
		  wings_drag:message([Dx], percent),
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
    Tvs = wings_sel:fold_shape(fun inflate/3, [], St),
    wings_drag:init_drag(Tvs, none, percent, St).

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
				       taper_2(Sh, Vs, Primary, Effect, Acc)
			       end, [], St),
    wings_drag:init_drag(Tvs, {-1.0,?HUGE}, St).

taper_2(#shape{id=Id,sh=We}, Vs, Primary, Effect, Acc) ->
    [MinR,MaxR] = wings_vertex:bounding_box(Vs, We),
    Key = key(Primary),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    case Max - Min of
	Range when Range < 0.01 ->
	    Axis = wings_util:upper(atom_to_list(Primary)),
	    Error = lists:concat(["Extent along ",Axis," axis is too short."]),
	    throw({command_error,Error});
	Range -> taper_3(Id, Vs, We, Range, Key, Effect, MinR, MaxR, Acc)
    end.

taper_3(Id, Vs0, We, Range, Key, Effect, MinR, MaxR, Acc) ->
    Tf = taper_fun(Key, Effect, MinR, MaxR),
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun(Dx, Dy, A) ->
		  wings_drag:message([Dx], percent),
		  U = Dx + 1.0,
		  foldl(fun({V,#vtx{pos=Pos0}=Rec}, VsAcc) ->
				Pos = Tf(U, Pos0),
				[{V,Rec#vtx{pos=Pos}}|VsAcc]
			end, A, VsPos)
	  end,
    [{Id,{Vs,Fun}}|Acc].

taper_fun(Key, Effect, {IX,IY,IZ}=MinR, {AX,AY,AZ}=MaxR) ->
    Min = element(Key, MinR),
    Range = element(Key, MaxR) - element(Key, MinR),
    {Ekey1,Ekey2} = effect(Effect),
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
    wings_drag:init_drag(Tvs, none, angle, St).

twist(#shape{id=Id,sh=We}, Vs0, Axis, Acc) ->
    Key = key(Axis),
    [MinR,MaxR] = wings_vertex:bounding_box(Vs0, We),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
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
    Tvs = wings_sel:fold_shape(fun(Sh, Vs, Acc) ->
				       twisty_twist(Sh, Vs, Axis, Acc)
			       end, [], St),
    wings_drag:init_drag(Tvs, none, angle, St).

twisty_twist(#shape{id=Id,sh=We}, Vs0, Axis, Acc) ->
    Tf = twisty_twist_fun(Axis),
    Key = key(Axis),
    [MinR,MaxR] = wings_vertex:bounding_box(Vs0, We),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
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
    fun(Dx, Dy, A) ->
	    Angle = Dx * 15,
	    U = (Angle / 180.0 * ?PI)/Range,
	    wings_drag:message([Angle], angle),
	    foldl(fun({V,#vtx{pos=Pos0}=Rec}, VsAcc) ->
			  Pos = Tf(U, Min, Pos0),
			  [{V,Rec#vtx{pos=Pos}}|VsAcc]
		  end, A, VsPos)
    end.
