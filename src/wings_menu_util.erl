%%
%%  wings_menu_util.erl --
%%
%%     Menu utilities and helpers.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_menu_util.erl,v 1.9 2002/03/20 07:36:49 bjorng Exp $
%%

-module(wings_menu_util).
-export([directions/1,directions/2,scale/0,rotate/0,flatten/0,
	 magnet_props/2,xyz/0,all_xyz/0]).

-include("wings.hrl").

directions(#st{selmode=Mode}) ->
    fun(B, Ns) ->
	    dirs(B, Mode, Ns)
    end.

dirs(1, Mode, Ns) -> dirs_1(Mode, Ns);
dirs(2, _Mode, _Ns) -> ignore;
dirs(3, _Mode, [move|_]=Ns) -> {vector,{pick,[axis],[],Ns}};
dirs(3, _Mode, Ns) -> {vector,{pick,[axis],[],Ns}};
dirs(help, _Mode, Ns) -> dirs_help(Ns).

dirs_help([move|_]) ->
    {"Move along std. axis",[],"Select axis to move along"};
dirs_help([rotate|_]) ->
    {"Rotate around std. axis",[],"Select axis to rotate around"};
dirs_help([scale|_]) -> "Scale selected elements";
dirs_help([extrude|_]) ->
    {"Extrude along std. axis",[],"Select axis to extrude along"};
dirs_help([extrude_region|_]) ->
    {"Extrude along std. axis",[],"Select axis to extrude along"};
dirs_help(_) -> "".

dirs_1(body, Ns) -> directions([free,x,y,z], Ns);
dirs_1(vertex, [rotate|_]=Ns) -> directions([free,x,y,z], Ns);
dirs_1(_, Ns) -> directions([normal,free,x,y,z], Ns).

xyz() ->
    [{"X",x},
     {"Y",y},
     {"Z",z}].

all_xyz() ->
    [{"All",all},
     {"X",x},
     {"Y",y},
     {"Z",z}].

%%%
%%% Scale sub-menu.
%%%

scale() ->
    {"Scale",{scale,fun scale/2}}.

scale(help, _) ->
     {"Scale along std. axis","Pick axis for radial scale",
      "Pick axis to scale along"};
scale(1, Ns) ->
    [scale_fun(uniform, Ns),
     scale_fun(x, Ns),
     scale_fun(y, Ns),
     scale_fun(z, Ns),
     scale_fun({radial,x}, Ns),
     scale_fun({radial,y}, Ns),
     scale_fun({radial,z}, Ns),
     {advanced,separator},
     scale_axis_fun(last_axis, Ns),
     scale_axis_fun(default_axis, Ns),
     {advanced,separator},
     scale_axis_fun({radial,last_axis}, Ns),
     scale_axis_fun({radial,default_axis}, Ns)
    ];
scale(2, Ns) -> {vector,{pick,[axis,point],[radial],Ns}};
scale(3, Ns) -> {vector,{pick,[axis,point],[],Ns}}.

scale_fun(Dir, Names) ->
    DirString = stringify_dir(Dir),
    F = fun(1, Ns) -> wings_menu:build_command({Dir,center}, Ns);
	   (2, _Ns) -> ignore;
	   (3, Ns) -> {vector,{pick,[point],[Dir],Ns}};
	   ({magnet,_}, Ns) -> {vector,{pick,[magnet],[center,Dir],Ns}}
	end,
    Help0 = dir_help(Dir, Names),
    Help = {Help0,[],"Pick point to scale to"},
    {DirString,F,Help,magnet_props(Dir, Names)}.

scale_axis_fun(Axis, Names) ->
    Vec = case Axis of
	      {radial,Ax} ->
		  {_,Vec0} = wings_pref:get_value(Ax),
		  {radial,Vec0};
	      Ax ->
		  {_,Vec0} = wings_pref:get_value(Ax),
		  Vec0
	  end,
    DirString = stringify_dir(Axis),
    F = fun(1, Ns) -> wings_menu:build_command({Vec,center}, Ns);
	   (2, _Ns) -> ignore;
	   (3, Ns) ->
		{vector,{pick,[point],[Vec],Ns}};
	   ({magnet,_}, Ns) -> {vector,{pick,[magnet],[center,Vec],Ns}}
	end,
    Help0 = dir_help(Axis, Names),
    Help = {Help0,[],"Pick point to scale to"},
    {advanced,{DirString,F,Help,[magnet]}}.

stringify_dir({radial,Axis}) -> "Radial " ++ wings_util:stringify(Axis);
stringify_dir(Dir) -> wings_util:stringify(Dir).

%%%
%%% Rotate sub-menu.
%%%

rotate() ->
    {"Rotate",{rotate,fun rotate/2}}.

rotate(help, _) ->
    {"Rotate along std. axis",[],"Pick axis to rotate around"};
rotate(1, [rotate,Mode]=Ns) when Mode == vertex; Mode == body ->
    [rotate_fun(free, Ns),
     rotate_fun(x, Ns),
     rotate_fun(y, Ns),
     rotate_fun(z, Ns),
     {advanced,separator},
     rotate_axis_fun(last_axis, Ns),
     rotate_axis_fun(default_axis, Ns)];
rotate(1, Ns) ->
    [rotate_fun(normal, Ns),
     rotate_fun(free, Ns),
     rotate_fun(x, Ns),
     rotate_fun(y, Ns),
     rotate_fun(z, Ns),
     {advanced,separator},
     rotate_axis_fun(last_axis, Ns),
     rotate_axis_fun(default_axis, Ns)];
rotate(2, _) -> ignore;
rotate(3, Ns) -> {vector,{pick,[axis,point],[],Ns}}.

rotate_fun(Dir, Names) ->
    DirString = wings_util:stringify(Dir),
    F = fun(1, Ns) -> wings_menu:build_command({Dir,center}, Ns);
	   (2, _Ns) -> ignore;
	   (3, Ns) -> {vector,{pick,[point],[Dir],Ns}};
	   ({magnet,_}, Ns) -> {vector,{pick,[magnet],[center,Dir],Ns}}
	end,
    Help0 = dir_help(Dir, Names),
    Help = {Help0,[],"Pick point to rotate through"},
    Ps = magnet_props(Dir, Names),
    {DirString,F,Help,Ps}.

rotate_axis_fun(Axis, Names) ->
    {_,Vec} = wings_pref:get_value(Axis),
    DirString = stringify_dir(Axis),
    F = fun(1, Ns) -> wings_menu:build_command({Vec,center}, Ns);
	   (2, _Ns) -> ignore;
	   (3, Ns) ->
		{vector,{pick,[point],[Vec],Ns}};
	   ({magnet,_}, Ns) -> {vector,{pick,[magnet],[center,Vec],Ns}}
	end,
    Help0 = dir_help(Axis, Names),
    Help = {Help0,[],"Pick point to rotate through"},
    {advanced,{DirString,F,Help,magnet_props(Axis, Names)}}.

%%%
%%% Flatten submenu.
%%%

flatten() ->
    {"Flatten",{flatten,fun flatten/2}}.

flatten(help, _) ->
    {"Flatten to std. planes",[],"Pick plane"};
flatten(1, [flatten,vertex]) ->
    [flatten_fun(x),
     flatten_fun(y),
     flatten_fun(z),
     {advanced,separator},
     flatten_axis_fun(last_axis),
     flatten_axis_fun(default_axis)];
flatten(1, _) ->
    [flatten_fun(normal),
     flatten_fun(x),
     flatten_fun(y),
     flatten_fun(z),
     {advanced,separator},
     flatten_axis_fun(last_axis),
     flatten_axis_fun(default_axis)];
flatten(3, Ns) -> {vector,{pick,[axis,point],[],Ns}}.

flatten_fun(Dir) ->
    DirString = wings_util:stringify(Dir),
    F = fun(1, Ns) -> wings_menu:build_command(Dir, Ns);
	   (2, _Ns) -> ignore;
	   (3, Ns) -> {vector,{pick,[point],[Dir],Ns}}
	end,
    Help0 = dir_help(Dir, [flatten]),
    Help = {Help0,[],"Pick point on plane"},
    {DirString,F,Help,[]}.

flatten_axis_fun(Axis) ->
    {_,Vec} = wings_pref:get_value(Axis),
    AxisString = wings_util:stringify(Axis),
    F = fun(1, Ns) -> wings_menu:build_command(Vec, Ns);
	   (2, _Ns) -> ignore;
	   (3, Ns) -> {vector,{pick,[point],[Vec],Ns}}
	end,
    Help0 = dir_help(Axis, [flatten]),
    Help = {Help0,[],"Pick point on plane"},
    {AxisString,F,Help,[]}.

%%%
%%% General directions.
%%%

directions([D|Dirs], Ns) ->
    [direction(D, Ns)|directions(Dirs, Ns)];
directions([], Ns) ->
    [{advanced,separator},
     {advanced,move_axis_fun(last_axis, Ns)},
     {advanced,move_axis_fun(default_axis, Ns)}].

direction(Dir, Ns) ->
    Help = dir_help(Dir, Ns),
    {wings_util:stringify(Dir),Dir,Help,magnet_props(Dir, Ns)}.

move_axis_fun(Axis, Ns) ->
    {_,Vec} = wings_pref:get_value(Axis),
    Help = dir_help(Axis, Ns),
    F = fun(1, _) -> wings_menu:build_command(Vec, Ns);
	   (2, _) -> ignore;
	   (3, _) -> ignore;
	   ({magnet,_}, _) -> {vector,{pick,[magnet],[Vec],Ns}}
	end,
    {wings_util:stringify(Axis),F,Help,magnet_props(Axis, Ns)}.

magnet_props(normal, _) -> [];
magnet_props(_, [_,body]) -> [];
magnet_props(_, [move|_]) -> [magnet];
magnet_props(_, [scale|_]) -> [magnet];
magnet_props(_, [rotate|_]) -> [magnet];
magnet_props(_, _) -> [].

dir_help(Axis, Ns) when Axis == x; Axis == y; Axis == z ->
    dir_help_1(Ns, "the " ++ wings_util:stringify(Axis) ++ " axis");
dir_help(last_axis, Ns) ->
    dir_help_1(Ns, "the last axis");
dir_help(default_axis, Ns) ->
    dir_help_1(Ns, "the default axis");
dir_help({radial,Axis}, Ns) ->
    dir_help_1(Ns, [around|"the " ++ wings_util:stringify(Axis) ++ " axis"]);
dir_help(radial_x, Ns) ->
    dir_help_1(Ns, [around|"around the X axis"]);
dir_help(radial_y, Ns) ->
    dir_help_1(Ns, [around|"around the Y axis"]);
dir_help(radial_z, Ns) ->
    dir_help_1(Ns, [around|"around the Z axis"]);
dir_help(normal, Ns) ->
    dir_help_1(Ns, [normal|"along its normal"]);
dir_help(free, Ns) ->
    dir_help_1(Ns, [free|"freely in all directions"]);
dir_help(uniform, [scale|_]) ->
    "Scale equally in all directions".

%% Normal/Free.
dir_help_1([move|_], [NF|Text]) when NF == normal; NF == free ->
    "Move each element " ++ Text;
dir_help_1([rotate|_], [free|_Text]) ->
    "Rotate freely";
dir_help_1([rotate|_], [normal|_Text]) ->
    "Rotate around each element's normal";
dir_help_1([extrude|_], [NF|Text]) when NF == normal; NF == free ->
    "Extrude each element, then move it " ++ Text;
dir_help_1([extrude_region|_], [normal|_]) ->
    "Extrude faces as region, then move faces along the region's normal";
dir_help_1([extrude_region|_], [free|Text]) ->
    "Extrude faces as region, then move faces " ++ Text;
dir_help_1([extract_region|_], [normal|_]) ->
    "Extract faces, then move faces along the region's normal";
dir_help_1([extract_region|_], [free|Text]) ->
    "Extract faces, then move faces " ++ Text;
dir_help_1([flatten|_], [normal|_Text]) ->
    "Flatten elements to normal plane";
dir_help_1([lift|_], [normal|_]) ->
    "Lift face along its normal";
dir_help_1([lift|_], [free|Text]) ->
    "Lift face and move it " ++ Text;

%% Axis
dir_help_1([move|_], Text) ->
    "Move each element along " ++ Text;
dir_help_1([extrude|_], Text) ->
    "Extrude elements, then move along " ++ Text;
dir_help_1([extrude_region|_], Text) ->
    "Extrude faces as region, then move along " ++ Text;
dir_help_1([extract_region|_], Text) ->
    "Extract faces, then move along " ++ Text;
dir_help_1([rotate|_], Text) ->
    "Rotate around " ++ Text;
dir_help_1([scale|_], [around|Text]) ->
    "Scale " ++ Text;
dir_help_1([scale|_], Text) ->
    "Scale along " ++ Text;
dir_help_1([flatten|_], Text) ->
    "Flatten to " ++ Text;
dir_help_1([flatten_move|_], Text) ->
    "Flatten and move to " ++ Text;
dir_help_1([lift|_], Text) ->
    "Lift face along " ++ Text;
dir_help_1(_, _) -> "".
