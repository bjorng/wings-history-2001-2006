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
%%     $Id: wings_menu_util.erl,v 1.1 2002/03/03 17:02:53 bjorng Exp $
%%

-module(wings_menu_util).
-export([directions/1,scale/0,xyz/0,all_xyz/0,flatten_dir/1]).

-include("wings.hrl").

directions(#st{selmode=Mode}) ->
    fun(B, Ns) ->
	    dirs(B, Mode, Ns)
    end.

dirs(1, Mode, Ns) -> dirs_1(Mode, Ns);
dirs(2, _Mode, Ns) -> {vector,{pick_named,Ns}};
dirs(3, _Mode, Ns) -> {vector,{pick_new,Ns}};
dirs(help, _Mode, Ns) -> dirs_help(Ns).

dirs_help([move|_]) ->
    {"Move along std. axis","Move along named axis",
     "Select vector to move along"};
dirs_help([rotate|_]) ->
    {"Rotate around std. axis","Rotate around named axis",
     "Select vector to rotate around"};
dirs_help([scale|_]) -> "Scale selected elements";
dirs_help([extrude|_]) ->
    {"Extrude along std. axis","Extrude along named axis",
     "Select vector to extrude along"};
dirs_help([extrude_region|_]) ->
    {"Extrude along std. axis","Extrude along named axis",
     "Select vector to extrude along"};
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

scale() ->
    {"Scale",{scale,fun scale/2},[]}.

scale(help, _) -> "";
scale(_, _) ->
    [scale_fun(uniform),
     scale_fun(x),
     scale_fun(y),
     scale_fun(z),
     scale_fun(radial_x),
     scale_fun(radial_y),
     scale_fun(radial_z)].

scale_fun(Dir) ->
    DirString = wings_util:stringify(Dir),
    F = fun(1, Ns) -> wings_menu:build_command(Dir, Ns);
	   (2, Ns) -> {vector,{pick_named,[Dir|Ns]}};
	   (3, Ns) -> {vector,{pick_new,[Dir|Ns]}}
	end,
    Help0 = dir_help(Dir, [scale]),
    Help = {Help0,"Scale to named vector","Pick vector to scale to"},
    {DirString,F,Help,[]}.

%%%
%%% Flatten submenu.
%%%

flatten_dir(#st{selmode=Mode}) ->
    fun(B, Ns) -> flatten_dir_1(B, Mode, Ns) end.

flatten_dir_1(help, _, _) ->
    {"Flatten to std. planes","Flatten to named plane","Pick plane"};
flatten_dir_1(1, _, [flatten_move|_]=Ns) ->
    directions([x,y,z], Ns);
flatten_dir_1(1, vertex, Ns) ->
    directions([x,y,z], Ns);
flatten_dir_1(1, face, Ns) ->
    directions([normal,x,y,z], Ns);
flatten_dir_1(2, _Mode, Ns) ->
    {vector,{pick_named,Ns}};
flatten_dir_1(3, _Mode, Ns) ->
    {vector,{pick_new,Ns}};
flatten_dir_1(_, _, _) -> ignore.

%%%
%%% General directions.
%%%

directions([D|Dirs], Ns) ->
    [direction(D, Ns)|directions(Dirs, Ns)];
directions([], _) -> [].

direction(Dir, Ns) ->
    Help = dir_help(Dir, Ns),
    {wings_util:stringify(Dir),Dir,Help}.

dir_help(Axis, Ns) when Axis == x; Axis == y; Axis == z ->
    dir_help_1(Ns, "the " ++ wings_util:stringify(Axis) ++ " axis");
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
dir_help(uniform, [scale]) ->
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
