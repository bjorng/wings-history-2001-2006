%%
%%  wings_shapes.erl --
%%
%%     This module contains definition of all primitive
%%     shapes that can be created, such as Cube, Sphere,
%%     and Grid.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_shapes.erl,v 1.27 2002/12/15 18:19:41 bjorng Exp $
%%

-module(wings_shapes).
-export([menu/3,command/2]).
-include("wings.hrl").

-import(lists, [map/2,foreach/2,foldl/3,sort/1,last/1,seq/2,seq/3]).
-import(math, [sqrt/1,cos/1,sin/1,pi/0]).

menu(X, Y, _) ->
    Menu0 = [{"Tetrahedron",tetrahedron},
	     {"Octahedron",octahedron},
	     {"Octotoad",octotoad},
	     {"Dodecahedron",dodecahedron},
	     {"Icosahedron",icosahedron},
	     separator,
	     {"Cube",cube},
	     separator,
	     {"Cylinder",cylinder,[],[option]},
	     {"Cone",cone,[],[option]},
	     {"Sphere",sphere,[],[option]},
	     {"Torus",torus,[],[option]},
	     separator,
	     {"Grid",grid,[],[option]},
	     separator,
	     {"Light",{light,wings_light:light_types()}},
	     {"More",{more,[]},"More primitives"}],
    Menu = [prim_help(Item) || Item <- Menu0],
    wings_menu:popup_menu(X, Y, shape, Menu).

prim_help({S,K}) ->
    {S,K,create_help(S)};
prim_help({S,K,[],Opts}) when is_atom(K) ->
    {S,K,create_help(S),Opts};
prim_help(Other) -> Other.

create_help([Vowel|T]) when Vowel == $O; Vowel == $I ->
    "Create an "++[Vowel+($a-$A)|T];
create_help([H|T]) ->
    "Create a "++[H+($a-$A)|T].

command(tetrahedron, St) -> tetrahedron(St);
command(octahedron, St) -> octahedron(St);
command(octotoad, St) -> octotoad(St);
command(dodecahedron, St) -> dodecahedron(St);
command(icosahedron, St) -> icosahedron(St);
command(cube, St) -> cube(St);
command({cylinder,Ask}, St) -> cylinder(Ask, St);
command({cone,Ask}, St) -> cone(Ask, St);
command({sphere,Ask}, St) -> sphere(Ask, St);
command({torus,Ask}, St) -> torus(Ask, St);
command({grid,Ask}, St) -> grid(Ask, St);
command({light,Type}, St) -> wings_light:create(Type, St).

build_shape(Prefix, Fs, Vs, #st{onext=Oid}=St) ->
    We = wings_we:build(Fs, Vs),
    Name = Prefix++integer_to_list(Oid),
    wings_shape:new(Name, We, St).

tetrahedron(St) ->
    Fs = [[2,1,0],[1,2,3],[1,3,0],[3,2,0]],
    Vs = [{0.0,1.0*2,0.0},
	  {0.0,-0.33333*2,0.942809*2},
	  {-0.816497*2,-0.333333*2,-0.471405*2},
	  {0.816497*2,-0.333333*2,-0.471405*2}],
    build_shape("tetrahedron", Fs, Vs, St).

octahedron(St) ->
    Fs = [[2,4,0],[4,2,1],[4,3,0],[3,4,1],[5,2,0],[2,5,1],[3,5,0],[5,3,1]],
    Vs = [{2.0,0.0,0.0},{-2.0,0.0,0.0},{0.0,2.0,0.0},
	  {0.0,-2.0,0.0},{0.0,0.0,2.0},{0.0,0.0,-2.0}],
    build_shape("octahedron", Fs, Vs, St).

octotoad(St) ->
    Fs = [[2,3,1,0],[7,6,4,5],[9,8,0,1],[10,11,3,2],
	  [12,0,8],[12,14,2,0],[13,9,1],[14,10,2],
	  [15,3,11],[15,13,1,3],[16,17,5,4],[16,20,12,8],
	  [17,16,8,9],[18,19,11,10],[19,18,6,7],[19,23,15,11],
	  [20,16,4],[20,22,14,12],[21,5,17],[21,17,9,13],
	  [21,23,7,5],[22,6,18],[22,18,10,14],[22,20,4,6],
	  [23,19,7],[23,21,13,15]],
    Vs = [{1.668,0.556,0.556},{1.668,0.556,-0.556},
	  {1.668,-0.556,0.556},{1.668,-0.556,-0.556},
	  {-1.668,0.556,0.556},{-1.668,0.556,-0.556},
	  {-1.668,-0.556,0.556},{-1.668,-0.556,-0.556},
	  {0.556,1.668,0.556},{0.556,1.668,-0.556},
	  {0.556,-1.668,0.556},{0.556,-1.668,-0.556},
	  {0.556,0.556,1.668},{0.556,0.556,-1.668},
	  {0.556,-0.556,1.668},{0.556,-0.556,-1.668},
	  {-0.556,1.668,0.556},{-0.556,1.668,-0.556},
	  {-0.556,-1.668,0.556},{-0.556,-1.668,-0.556},
	  {-0.556,0.556,1.668},{-0.556,0.556,-1.668},
	  {-0.556,-0.556,1.668},{-0.556,-0.556,-1.668}],
    build_shape("octotoad", Fs, Vs, St).

dodecahedron(St) ->
    Alpha = sqrt(2.0 / (3.0 + sqrt(5.0))),
    Beta = 1.0 + sqrt(6.0 / (3.0 + sqrt(5.0)) -
		      2.0 + 2.0 * sqrt(2.0 / (3.0 + sqrt(5.0)))),
    Fs = [[0,1,9,16,5],[1,0,3,18,7],[1,7,11,10,9],[11,7,18,19,6],
	  [8,17,16,9,10],[2,14,15,6,19],[2,13,12,4,14],[2,19,18,3,13],
	  [3,0,5,12,13],[6,15,8,10,11],[4,17,8,15,14],[4,12,5,16,17]],
    Vs = [{-Alpha,0.0,Beta},{Alpha,0.0,Beta},{-1.0,-1.0,-1.0},{-1.0,-1.0,1.0},
	  {-1.0,1.0,-1.0},{-1.0,1.0,1.0},{1.0,-1.0,-1.0},
	  {1.0,-1.0,1.0},{1.0,1.0,-1.0},
	  {1.0,1.0,1.0},{Beta,Alpha,0.0},{Beta,-Alpha,0.0},{-Beta,Alpha,0.0},
	  {-Beta,-Alpha,0.0},{-Alpha,0.0,-Beta},{Alpha,0.0,-Beta},
	  {0.0,Beta,Alpha},{0.0,Beta,-Alpha},{0.0,-Beta,Alpha},
	  {0.0,-Beta,-Alpha}],
    build_shape("dodecahedron", Fs, Vs, St).

icosahedron(St) ->
    X = 1.05146,
    Z = 1.70130,
    Fs = [[1,4,0],[4,9,0],[4,5,9],[8,5,4],[1,8,4],[1,10,8],
	  [10,3,8],[8,3,5],[3,2,5],[3,7,2],[3,10,7],[10,6,7],
	  [6,11,7],[6,0,11],[6,1,0],[10,1,6],[11,0,9],
	  [2,11,9],[5,2,9],[11,2,7]],
    Vs = [{-X,0.0,Z},{X,0.0,Z},{-X,0.0,-Z},{X,0.0,-Z},{0.0,Z,X},
	  {0.0,Z,-X},{0.0,-Z,X},{0.0,-Z,-X},{Z,X,0.0},{-Z,X,0.0},
	  {Z,-X,0.0},{-Z,-X,0.0}],
    build_shape("icosahedron", Fs, Vs, St).

cube(St) ->
    Fs = [[0,3,2,1],[2,3,7,6],[0,4,7,3],[1,2,6,5],[4,5,6,7],[0,1,5,4]],
    Vs = [{-1.0,-1.0,1.0},{-1.0,1.0,1.0},{1.0,1.0,1.0},{1.0,-1.0,1.0},
	  {-1.0,-1.0,-1.0},{-1.0,1.0,-1.0},{1.0,1.0,-1.0},{1.0,-1.0,-1.0}],
    build_shape("cube", Fs, Vs, St).

circle(N, Y) ->
    circle(N, Y, 1.0).

circle(N, Y, R) ->
    Delta = pi()*2 / N,
    [{R*cos(I*Delta), Y, R*sin(I*Delta)} || I <- lists:seq(0, N-1)].

cylinder(Ask, _St) when is_atom(Ask) ->
    ask(cylinder, Ask, [{"Sections",16,[{range,{3,1024}}]}]);
cylinder([Sections], St) ->
    Fs = cylinder_faces(Sections),
    Vs = cylinder_vertices(Sections),
    build_shape("cylinder", Fs, Vs, St).

cylinder_faces(N) ->
    Ns =lists:reverse(lists:seq(0, N-1)),
    Upper= Ns,
    Lower= lists:seq(N, N+N-1),
    Sides= [[I, (I+1) rem N, N + (I+1) rem N, N + I] || I <- Ns],
    [Upper, Lower | Sides].

cylinder_vertices(N) ->
    circle(N, 1.0) ++ circle(N, -1.0).

cone(Ask, _St) when is_atom(Ask) ->
    ask(cone, Ask, [{"Sections",16,[{range,{3,1024}}]}]);
cone([N], St) ->
    Ns = lists:seq(0, N-1),
    Lower = lists:seq(0, N-1),
    C = circle(N, -1.0),
    Vs = C ++ [{0.0,1.0,0.0}],
    Sides = [[N, (I+1) rem N, I] || I <- Ns],
    Fs = [Lower | Sides],
    build_shape("cone", Fs, Vs, St).
    
sphere_circles(Ns, Nl) ->
    Delta = pi() / Nl,
    PosAndRads= [{cos(I*Delta), sin(I*Delta)} || I <- lists:seq(1, Nl-1)],
    Circles = [circle(Ns, Pos, Rad) || {Pos, Rad} <- PosAndRads],
    lists:flatten(Circles).

sphere_faces(Ns, Nl) ->
    Lasti= (Nl-1)*Ns,
    Topi= Lasti,
    Boti= Topi+1,
    Topf= [[Topi, (I+1) rem Ns, I]
	   || I <- lists:seq(0, Ns-1)],
    Botf= [[Boti, Lasti - Ns + I, Lasti - Ns + (I+1) rem Ns]
	   || I <- lists:seq(0, Ns-1)],
    Slices= [ [ [(I+1) rem Ns  +J*Ns,
		 (I+1) rem Ns  +J*Ns +Ns,
		 I             +J*Ns +Ns,
		 I             +J*Ns]
		|| I <- lists:seq(0, Ns-1)]
	      || J <- lists:seq(0, Nl-3)],
    Topf ++ Botf ++ lists:append(Slices).

sphere(Ask, _St) when is_atom(Ask) ->
    ask(sphere, Ask, [{"Sections",16,[{range,{3,128}}]},
		      {"Slices",8,[{range,{3,128}}]}]);
sphere([Ns,Nl], St) ->
    Fs = sphere_faces(Ns, Nl),
    Vs = sphere_circles(Ns, Nl) ++ [{0.0, 1.0, 0.0}, {0.0, -1.0, 0.0}],
    build_shape("sphere", Fs, Vs, St).
    
torus(Ask, _St) when is_atom(Ask) ->
    ask(torus, Ask, [{"Sections",16,[{range,{3,128}}]},
		     {"Slices/2",4,[{range,{1,128}}]},
		     {"Major Radius",math:sqrt(2)},
		     {"Minor Radius",0.25}]);
torus([Ns,Nl,Major,Minor], St) ->
    Vs = torus_vertices(Ns, Nl, Major, Minor),
    Fs = torus_faces(Ns, Nl),
    build_shape("torus", Fs, Vs, St).

torus_faces(Ns, Nl) ->
    Nl2 = Nl*2,
    Slices = [ [ [(I+1) rem Ns + J*Ns, I + J*Ns,
		  I+ ((J+1) rem Nl2) *Ns, (I+1) rem Ns + ((J+1) rem Nl2)*Ns]
		 || I <- lists:seq(0, Ns - 1)]
	       || J <- lists:seq(0, Nl2 - 1)],
    lists:append(Slices).

torus_vertices(Ns, Nl0, Hs, Minor) ->
    Nl = 2*Nl0,
    Delta = 2*pi() / Nl,
    Circles = map(fun(I) ->
			  A = I*Delta,
			  Pos = Minor*sin(A),
			  Rad = Hs + Minor*cos(A),
			  circle(Ns, Pos, Rad)
		  end, lists:seq(0, Nl - 1)),
    lists:append(Circles).

grid(Ask, _) when is_atom(Ask) ->
    ask(grid, Ask, [{"Rows/cols",10,[{range,{1,128}}]}]);
grid([Size], St) ->
    Vs = grid_vertices(Size),
    Fs = grid_faces(Size),
    build_shape("grid", Fs, Vs, St).

grid_vertices(Size) ->
    {Low,High} = case Size rem 2 of
		     0 -> {-Size,Size};
		     1 -> {-Size,Size}
		 end,
    Sz = 0.5,
    H = 0.1,
    TopSeq = seq(Low, High, 2),
    BotSeq = [Low,High],
    VsBot = [{I*Sz/2,-H,J*Sz/2} || J <- BotSeq, I <- BotSeq],
    [{I*Sz/2,H,J*Sz/2} || J <- TopSeq, I <- TopSeq] ++ VsBot.

grid_faces(Size) ->
    TopSeq = seq(0, Size-1),
    Rsz = Size+1,
    TopFs = [grid_face(I, J, Rsz) || I <- TopSeq, J <- TopSeq],
    ULv = Rsz*Rsz,
    Fs0 = [[ULv,ULv+1,ULv+3,ULv+2]|TopFs],	%Add bottom.
    Fs1 = [[ULv+1,ULv|seq(0, Rsz-1)]|Fs0],	%North
    Fs2 = [[ULv,ULv+2|seq(Rsz*Rsz-Rsz, 0, -Rsz)]|Fs1], %West
    Fs = [[ULv+2,ULv+3|seq(Rsz*Rsz-1, Rsz*Rsz-Rsz, -1)]|Fs2], % South.
    [[ULv+3,ULv+1|seq(Rsz-1, Rsz*Rsz-1, Rsz)]|Fs]. %East

grid_face(I, J, Rsz) ->
    [Rsz*J+I+1,   Rsz*J+I,
     Rsz*(J+1)+I, Rsz*(J+1)+I+1].

ask(Shape, Bool, Qs) ->
    wings_ask:ask(Bool, Qs, fun(Res) -> {shape,{Shape,Res}} end).
