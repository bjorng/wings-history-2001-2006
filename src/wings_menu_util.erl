%%
%%  wings_menu_util.erl --
%%
%%     Menu utilities and helpers.
%%
%%  Copyright (c) 2002-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_menu_util.erl,v 1.31 2003/10/11 13:42:37 bjorng Exp $
%%

-module(wings_menu_util).
-export([directions/1,directions/2,scale/1,rotate/1,flatten/0,all_xyz/0]).

-include("wings.hrl").

directions(#st{selmode=Mode}) ->
    fun(B, Ns) ->
	    dirs(B, Mode, Ns)
    end.

dirs(1, Mode, Ns) -> dirs_1(Mode, Ns);
dirs(2, _Mode, [duplicate|_]) -> {body,duplicate};
dirs(2, _Mode, Ns) -> {vector,{pick,[],[normal],Ns}};
dirs(3, _Mode, Ns) -> {vector,{pick,[axis],[],Ns}};
dirs(help, _Mode, Ns) -> dirs_help(Ns).

dirs_help([move|_]) ->
    {"Move along std. axis","Move along selection's normal",
     "Pick axis to move along"};
dirs_help([rotate|_]) ->
    {"Rotate around std. axis","Pick axis to rotate around",
     "Pick axis and point to rotate through"};
dirs_help([scale|_]) -> "Scale selected elements";
dirs_help([extrude|_]) ->
    {"Extrude along std. axis","Extrude along selection's normal",
     "Pick axis to extrude along"};
dirs_help([extrude_region|_]) ->
    {"Extrude along std. axis","Extrude along selection's normal",
     "Pick axis to extrude along"};
dirs_help([extract_region|_]) ->
    {"Extract along std. axis","Extract along selection's normal",
     "Pick axis to extract along"};
dirs_help([duplicate|_]) ->
    {"Duplicate; move along std. axis","Duplicate; don't move",
     "Duplicate; pick axis to move along"};
dirs_help(_) -> "".

dirs_1(body, Ns) -> directions([free,x,y,z], Ns);
dirs_1(vertex, [rotate|_]=Ns) -> directions([free,x,y,z], Ns);
dirs_1(_, Ns) -> directions([normal,free,x,y,z], Ns).

all_xyz() ->
    [{"All",all},
     {"X",x},
     {"Y",y},
     {"Z",z}].

%%%
%%% Scale sub-menu.
%%%
scale(St) ->
    case wings_pref:get_value(advanced_menus) of
	false -> basic_scale();
	true -> adv_scale(St)
    end.

%% Basic menu Scale command.

basic_scale() ->
    Names = [scale],
    Dirs = [uniform,x,y,z,{radial,x},{radial,y},{radial,z}],
    {"Scale",{scale,basic_scale_1(Dirs, Names)}}.

basic_scale_1([Dir|Dirs], Names) ->
    DirString = stringify_dir(Dir),
    Help = dir_help(Dir, Names),
    [{DirString,{'VALUE',{Dir,center}},Help}|basic_scale_1(Dirs, Names)];
basic_scale_1([], _) -> [].

%% Advanced menu Scale commands.

adv_scale(#st{selmode=body}) ->
    adv_scale_1([], body);
adv_scale(#st{selmode=Mode}) ->
    adv_scale_1([magnet], Mode).

adv_scale_1(Flags, Mode) ->
    [{"Scale Uniform",{scale,fun(B, Ns) -> uniform_scale(B, Ns, Mode) end},[],Flags},
     {"Scale Axis",{scale,fun(B, Ns) -> scale(B, Ns, []) end},[],Flags},
     {"Scale Radial",{scale,fun(B, Ns) -> scale(B, Ns, [radial]) end},[],Flags}].

uniform_scale(help, _, _) ->
    ChoosePoint = "Choose point to scale from",
    {"Scale uniformly from midpoint of selection",ChoosePoint,ChoosePoint};
uniform_scale(1, _Ns, Mode) ->
    {vector,{pick,[],[center,uniform],[scale,Mode]}};
uniform_scale(2, Ns, _) -> {vector,{pick,[point],[],Ns}};
uniform_scale(3, Ns, _) -> {vector,{pick,[point],[],Ns}}.

scale(help, _, []) ->
    {"Scale along std. axis","Pick axis to scale along",
     "Pick axis and point to scale from"};
scale(help, _, [radial]) ->
    {"Scale outward from std. axis","Pick axis to scale out from",
     "Pick axis and point to scale from"};
scale(1, Ns, Flags) ->
    [scale_fun(x, Ns, Flags),
     scale_fun(y, Ns, Flags),
     scale_fun(z, Ns, Flags),
     {advanced,separator},
     scale_axis_fun(last_axis, Ns, Flags),
     scale_axis_fun(default_axis, Ns, Flags)];
scale(2, Ns, Flags) -> {vector,{pick,[axis_point],Flags,Ns}};
scale(3, Ns, Flags) -> {vector,{pick,[axis,point],Flags,Ns}}.

scale_fun(Dir, Names, [radial]) ->
    scale_fun({radial,Dir}, Names, []);
scale_fun(Dir, Names, _Flags) ->
    DirString = stringify_dir(Dir),
    F = magnet_scale_rot_fun(Dir, center),
    Help0 = dir_help(Dir, Names),
    Help = {Help0,[],"Pick point to scale from"},
    {DirString,F,Help,magnet_props(Dir, Names)}.

scale_axis_fun(Axis0, Names, Flags) ->
    {Point,Vec0} = wings_pref:get_value(Axis0),
    {Vec,Axis} = case Flags of
		     [] -> {Vec0,Axis0};
		     [radial] -> {{radial,Vec0},{radial,Axis0}}
		 end,
    DirString = stringify_dir(Axis),
    F = magnet_scale_rot_fun(Vec, Point),
    Help0 = dir_help(Axis, Names),
    Help = {Help0,[],"Pick point to scale to"},
    {advanced,{DirString,F,Help,magnet_props(Axis, Names)}}.

stringify_dir({radial,Axis}) -> "Radial " ++ wings_util:stringify(Axis);
stringify_dir(Dir) -> wings_util:stringify(Dir).

%%%
%%% Rotate sub-menu.
%%%

rotate(#st{selmode=body}) ->
    {"Rotate",{rotate,fun rotate/2}};
rotate(_) ->
    {"Rotate",{rotate,fun rotate/2},[],[magnet]}.

rotate(help, _) ->
    {"Rotate around std. axis","Pick axis to rotate around",
     "Pick axis and point to rotate through"};
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
rotate(2, Ns) -> {vector,{pick,[axis_point],[],Ns}};
rotate(3, Ns) -> {vector,{pick,[axis,point],[],Ns}}.

rotate_fun(Dir, Names) ->
    DirString = wings_util:stringify(Dir),
    F = magnet_scale_rot_fun(Dir, center),
    Help0 = dir_help(Dir, Names),
    Help = {Help0,[],"Pick point to rotate through"},
    Ps = magnet_props(Dir, Names),
    {DirString,F,Help,Ps}.

rotate_axis_fun(Axis, Names) ->
    {Point,Vec} = wings_pref:get_value(Axis),
    DirString = stringify_dir(Axis),
    F = magnet_scale_rot_fun(Vec, Point),
    Help0 = dir_help(Axis, Names),
    Help = {Help0,[],"Pick point to rotate through"},
    {advanced,{DirString,F,Help,magnet_props(Axis, Names)}}.

magnet_scale_rot_fun(Vec, Point) ->
    fun(1, Ns) -> {vector,{pick,[],[Point,Vec],Ns}};
       (2, _Ns) -> ignore;
       (3, Ns) -> {vector,{pick,[point],[Vec],Ns}}
    end.

%%%
%%% Flatten submenu.
%%%

flatten() ->
    {"Flatten",{flatten,fun flatten/2}}.

flatten(help, _) ->
    {"Flatten to std. planes","Pick plane",
     "Pick plane and ref point on plane"};
flatten(1, [flatten,vertex]) ->
    %% Vertex mode flatten.
    [flatten_fun(x),
     flatten_fun(y),
     flatten_fun(z),
     {advanced,separator},
     {advanced,flatten_axis_fun(last_axis)},
     {advanced,flatten_axis_fun(default_axis)}];
flatten(1, _) ->
    %% Face mode flatten.
    [flatten_fun(normal),
     flatten_fun(x),
     flatten_fun(y),
     flatten_fun(z),
     {advanced,separator},
     {advanced,flatten_axis_fun(last_axis)},
     {advanced,flatten_axis_fun(default_axis)}];
flatten(2, Ns) -> {vector,{pick,[axis],[],Ns}};
flatten(3, Ns) -> {vector,{pick,[axis,point],[],Ns}}.

flatten_axis_fun(Axis) ->
    {_,Vec} = wings_pref:get_value(Axis),
    flatten_fun_1(Vec, Axis, wings_util:stringify(Axis)).

flatten_fun(Vec) ->
    flatten_fun_1(Vec, Vec, wings_util:stringify(Vec)).

flatten_fun_1(Vec, Axis, String) ->
    F = fun(1, Ns) -> wings_menu:build_command(Vec, Ns);
	   (2, _Ns) -> ignore;
	   (3, Ns) -> {vector,{pick,[point],[Vec],Ns}}
	end,
    Help0 = dir_help(Axis, [flatten]),
    Help = {Help0,[],"Pick point on plane"},
    {String,F,Help,[]}.

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
    Str = wings_util:stringify(Dir),
    Help = dir_help(Dir, Ns),
    Ps = magnet_props(Dir, Ns),
    {Str,Dir,Help,Ps}.

move_axis_fun(Axis, Ns) ->
    {_,Vec} = wings_pref:get_value(Axis),
    Help = dir_help(Axis, Ns),
    Str = wings_util:stringify(Axis),
    Ps =  magnet_props(Axis, Ns),
    {Str,{'VALUE',Vec},Help,Ps}.

magnet_props(normal, [rotate|_]) -> [];
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
dir_help_1([duplicate|_], [free|Text]) ->
    "Duplicate and move freely " ++ Text;

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
dir_help_1([duplicate|_], Text) ->
    "Duplicate, then move along " ++ Text;
dir_help_1(_, _) -> "".
