%%
%%  wings_shapes.erl --
%%
%%     This module contains definition of all primitive
%%     shapes that can be created, such as Cube, Sphere,
%%     and Grid.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_shapes.erl,v 1.2 2001/08/16 09:52:58 bjorng Exp $
%%

-module(wings_shapes).
-export([tetrahedron/1,octahedron/1,dodecahedron/1,icosahedron/1,cube/1,
	 cylinder/1,cylinder/2,
	 cone/1,cone/2,
	 sphere/1,sphere/2,
	 torus/1,torus/2,
	 grid/1,grid/2]).
-include("wings.hrl").

-import(lists, [foreach/2,foldl/3,sort/1,last/1,seq/2,seq/3]).
-import(math, [sqrt/1,cos/1,sin/1,pi/0]).

build_shape(Name0, Fs, Vs, #st{onext=Id}=St) ->
    Name = Name0 ++ integer_to_list(Id),
    Sc = ?GROUND_GRID_SIZE,
    Matrix = wings_mat:scale(Sc, Sc, Sc),
    wings_we:build(Name, Matrix, Fs, Vs, St).

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

dodecahedron(St) ->
    Alpha = sqrt(2.0 / (3.0 + sqrt(5.0))),
    Beta = 1.0 + sqrt(6.0 / (3.0 + sqrt(5.0)) -
		      2.0 + 2.0 * sqrt(2.0 / (3.0 + sqrt(5.0)))),
    Fs = [[0,1,9,16,5],[1,0,3,18,7],[1,7,11,10,9],[11,7,18,19,6],
	  [8,17,16,9,10],[2,14,15,6,19],[2,13,12,4,14],[2,19,18,3,13],
	  [3,0,5,12,13],[6,15,8,10,11],[4,17,8,15,14],[4,12,5,16,17]],
    Vs = [{-Alpha,0,Beta},{Alpha,0,Beta},{-1,-1,-1},{-1,-1,1},
	  {-1,1,-1},{-1,1,1},{1,-1,-1},{1,-1,1},{1,1,-1},
	  {1,1,1},{Beta,Alpha,0},{Beta,-Alpha,0},{-Beta,Alpha,0},
	  {-Beta,-Alpha,0},{-Alpha,0,-Beta},{Alpha,0,-Beta},
	  {0,Beta,Alpha},{0,Beta,-Alpha},{0,-Beta,Alpha},
	  {0,-Beta,-Alpha}],
    build_shape("dodecahedron", Fs, Vs, St).

icosahedron(St) ->
    X = 1.05146,
    Z = 1.70130,
    Fs = [[1,4,0],[4,9,0],[4,5,9],[8,5,4],[1,8,4],[1,10,8],
	  [10,3,8],[8,3,5],[3,2,5],[3,7,2],[3,10,7],[10,6,7],
	  [6,11,7],[6,0,11],[6,1,0],[10,1,6],[11,0,9],
	  [2,11,9],[5,2,9],[11,2,7]],
    Vs = [{-X,0,Z},{X,0,Z},{-X,0,-Z},{X,0,-Z},{0,Z,X},
	  {0,Z,-X},{0,-Z,X},{0,-Z,-X},{Z,X,0},{-Z,X,0},
	  {Z,-X,0},{-Z,-X,0}],
    build_shape("icosahedron", Fs, Vs, St).

cube(St) ->
    Fs = [[0,3,2,1],[2,3,7,6],[0,4,7,3],[1,2,6,5],[4,5,6,7],[0,1,5,4]],
    Vs = [{-1.0,-1.0,1.0},{-1.0,1.0,1.0},{1.0,1.0,1.0},{1.0,-1.0,1.0},
	  {-1.0,-1.0,-1.0},{-1.0,1.0,-1.0},{1.0,1.0,-1.0},{1.0,-1.0,-1.0}],
    build_shape("cube", Fs, Vs, St).

circle(N, Y) ->
    circle(N, Y, 1.0).

circle(N, Y, R) ->
    Delta= pi()*2 / N,
    [{R*cos(I*Delta), Y, R*sin(I*Delta)} || I <- lists:seq(0, N-1)].

cylinder(_, St) ->
    make_cylinder(true, St).

cylinder(St) ->
    make_cylinder(false, St).

make_cylinder(Ask, St0) ->
    ask(Ask, [{"Sections",16,3,unlimited}],
	fun([Sections], St) ->
		Fs = cylinder_faces(Sections),
		Vs = cylinder_vertices(Sections),
		build_shape("cylinder", Fs, Vs, St)
	end, St0).

cylinder_faces(N) ->
    Ns= lists:reverse(lists:seq(0, N-1)),
    Upper= Ns,
    Lower= lists:seq(N, N+N-1),
    Sides= [[I, (I+1) rem N, N + (I+1) rem N, N + I] || I <- Ns],
    [Upper, Lower | Sides].

cylinder_vertices(N) ->
    circle(N, 1.0) ++ circle(N, -1.0).

cone(_, St) ->
    make_cone(true, St).

cone(St) ->
    make_cone(false, St).

make_cone(Ask, St0) ->
    ask(Ask, [{"Sections",16,3,unlimited}],
	fun([N], St) ->
		Ns = lists:seq(0, N-1),
		Lower = lists:seq(0, N-1),
		C = circle(N, -1.0),
		Vs = C ++ [{0.0, 1.0, 0.0}],
		Sides = [[N, (I+1) rem N, I] || I <- Ns],
		Fs = [Lower | Sides],
		build_shape("cone", Fs, Vs, St)
	end, St0).
    
sphere_circles(Ns, Nl) ->
    Delta= pi() / Nl,
    PosAndRads= [{cos(I*Delta), sin(I*Delta)} || I <- lists:seq(1, Nl-1)],
    Circles= [circle(Ns, Pos, Rad) || {Pos, Rad} <- PosAndRads],
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

sphere(_, St) ->
    make_sphere(true, St).

sphere(St) ->
    make_sphere(false, St).

make_sphere(Ask, St0) ->
    ask(Ask, [{"Sections",16,0,unlimited},
	      {"Slices",8,0,unlimited}],
	fun([Ns,Nl], St) ->
		Vs = sphere_circles(Ns, Nl) ++
		    [{0.0, 1.0, 0.0}, {0.0, -1.0, 0.0}],
		Fs = sphere_faces(Ns, Nl),
		build_shape("sphere", Fs, Vs, St)
	end, St0).
    
torus_faces(Ns, Nl) ->
    Nl2= Nl*2,
    Slices= [ [ [(I+1) rem Ns + J*Ns, I + J*Ns,
		 I+ ((J+1) rem Nl2) *Ns, (I+1) rem Ns + ((J+1) rem Nl2)*Ns]
		|| I <- lists:seq(0, Ns - 1)]
	      || J <- lists:seq(0, Nl2 - 1)],
    lists:append(Slices).

torus_vertices(Ns, Nl) ->
    Nl2 = Nl*2,
    Delta= 2*pi() / Nl2,
    PosAndRads= [{0.25*sin(I*Delta), 0.75 + 0.25*cos(I*Delta)}
		 || I <- lists:seq(0, Nl2 - 1)],
    Circles= [circle(Ns, Pos, Rad) || {Pos, Rad} <- PosAndRads],
    lists:flatten(Circles).

torus(_, St) ->
    make_torus(true, St).

torus(St) ->
    make_torus(false, St).

make_torus(Ask, St0) ->
    ask(Ask, [{"Sections",16,0,unlimited},
	      {"Slices",8,0,unlimited}],
	fun([Ns,Nl], St) ->
		Vs = torus_vertices(Ns, Nl),
		Fs = torus_faces(Ns, Nl),
		build_shape("torus", Fs, Vs, St)
	end, St0).

grid(_, St) ->
    make_grid(true, St).

grid(St) ->
    make_grid(false, St).

make_grid(Ask, St0) ->
    ask(Ask, [{"Rows/cols",10,2,unlimited}],
	fun([Size], St) ->
		Vs = grid_vertices(Size),
		Fs = grid_faces(Size),
		build_shape("grid", Fs, Vs, St)
	end, St0).

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

ask(false, Qs, Fun, St) ->
    Ns = [Def || {_,Def,_,_} <- Qs],
    Fun(Ns, St);
ask(true, Qs, Fun, St) ->
    case ask(Qs) of
	aborted -> aborted;
	Ns -> Fun(Ns, St)
    end.

ask([{Prompt,Default,Min,Max}|T]=T0) ->
    case wings_getline:number(Prompt ++ ": ", Default) of
	aborted -> aborted;
	N ->
	    case ask(T) of
		aborted -> ask(T0);
		Ns -> [N|Ns]
	    end
    end;
ask([]) -> [].
