%% UV Torus Plugin for Wings 0.90 or higher.
%% Copyright (c) 2002 Anthony D'Agostino (scorpius@compuserve.com)
%%
%% Allows one to specify the U/V resolution and the major/minor radius.
%% The U resolution is the number of faces that will be generated
%% along the path of the major radius; the V resolution, along the path
%% of the minor radius.
%%
%% To generate square faces, rather than long and thin rectangles,
%% maintain the following ratio:
%%
%%          U Resolution   Major Radius
%%          ------------ = ------------
%%          V Resolution   Minor Radius
%%
%% I've added the two other types of tori (lumpy and spiral) from LightFlow
%% and plan to replace my minimal-torus-knot plugin with a real generator.
%%
%% Check my page for updates and an interactive torus generator plugin
%% for Blender.  http://ourworld.compuserve.com/homepages/scorpius

-module(wpc_torus).
-export([init/0,menu/2,command/2]).

-import(math, [sqrt/1,cos/1,sin/1,pi/0]).
-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,foreach/2,
		map/2,foldl/3]).

init() -> true.

menu({shape,more}, []) ->
    torus_menu();
menu({shape,more}, Menu) ->
    Menu ++ [separator|torus_menu()];
menu(_, Menu) -> Menu.

torus_menu() ->
    [{"UV Torus"     ,uvtorus,[option]},
     {"Lumpy Torus"  ,lutorus,[option]},
     {"Spiral Torus" ,sptorus,[option]}].

command({shape,{more,{uvtorus,Ask}}}, _St) -> make_uvtorus(Ask);
command({shape,{more,{lutorus,Ask}}}, _St) -> make_lutorus(Ask);
command({shape,{more,{sptorus,Ask}}}, _St) -> make_sptorus(Ask);
command(_, _) -> next.

%%% The rest are local functions.

% ======= Regular Torus =======
make_uvtorus(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, "Create UV Torus",
	    [{"U Resolution",80},
	     {"V Resolution",16},
	     {"Major Radius",1.0},
	     {"Minor Radius",0.2}],
	    fun(Res) -> {shape,{more,{uvtorus,Res}}} end);
make_uvtorus([Ures, Vres, MajR, MinR]) ->
    Vs = uvtorus_verts(Ures, Vres, MajR, MinR),
    Fs = uvtorus_faces(Ures, Vres),
    {new_shape,"UV Torus",Fs,Vs}.

% ======= Lumpy Torus =======
make_lutorus(Ask) when is_atom(Ask) ->
	wpa:ask(Ask, "Create Lumpy Torus",
		[{"U Resolution",125},
		 {"V Resolution",25},
		 {"Major Radius",1.0},
		 {"Minor Radius",0.2},
		 {"Lumps",8},
		 {"Lump Amplitude",0.5}],
		fun(Res) -> {shape,{more,{lutorus,Res}}} end);
make_lutorus([Ures, Vres, MajR, MinR, Loops, LoopRad]) ->
	Vs = lutorus_verts(Ures, Vres, MajR, MinR, Loops, LoopRad),
	Fs = uvtorus_faces(Ures, Vres),
	{new_shape,"Lumpy Torus",Fs,Vs}.

% ======= Spiral Torus =======
make_sptorus(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, "Make Spiral Torus",
	    [{"U Resolution",200},
	     {"V Resolution",20},
	     {"Major Radius",1.0},
	     {"Minor Radius",0.2},
	     {"Loops       ",8},
	     {"Loop Radius ",0.2}],
	    fun(Res) -> {shape,{more,{sptorus,Res}}} end);
make_sptorus([Ures, Vres, MajR, MinR, Loops, LoopRad]) ->
    Vs = sptorus_verts(Ures, Vres, MajR, MinR, Loops, LoopRad),
    Fs = uvtorus_faces(Ures, Vres),
    {new_shape,"Spiral Torus",Fs,Vs}.


% theta = (I*2*pi()/Ures)
% phi	= (J*2*pi()/Vres)
% I need to find a way to get these into the 'for loops' to avoid long formulas.

% ======= Regular Torus =======
uvtorus_verts(Ures, Vres, MajR, MinR) ->
	Verts = [[
			{
			(MajR + MinR*cos(J*2*pi()/Vres)) * cos(I*2*pi()/Ures),
				  -(MinR*sin(J*2*pi()/Vres)),
			(MajR + MinR*cos(J*2*pi()/Vres)) * sin(I*2*pi()/Ures)
			} % this tuple contains the x,y,z coordinates of this vertex
			|| J <- lists:seq(0, Vres-1)]
			|| I <- lists:seq(0, Ures-1)],
	lists:append(Verts). % add it to the list of verts

% ======= Lumpy Torus =======
lutorus_verts(Ures, Vres, MajR, MinR, Loops, LoopRad) ->
	Verts = [[
			{
			(MajR + MinR*cos((J*2*pi()/Vres))*(1.0+cos((I*2*pi()/Ures)*Loops)*LoopRad)) * cos((I*2*pi()/Ures)),
				  -(MinR*sin((J*2*pi()/Vres))*(1.0+cos((I*2*pi()/Ures)*Loops)*LoopRad)),
			(MajR + MinR*cos((J*2*pi()/Vres))*(1.0+cos((I*2*pi()/Ures)*Loops)*LoopRad)) * sin((I*2*pi()/Ures))
			} % this tuple contains the x,y,z coordinates of this vertex
			|| J <- lists:seq(0, Vres-1)]
			|| I <- lists:seq(0, Ures-1)],
	lists:append(Verts). % add it to the list of verts

% ======= Spiral Torus =======
sptorus_verts(Ures, Vres, MajR, MinR, Loops, LoopRad) ->
	Verts = [[
			{
			(MajR + MinR*cos((J*2*pi()/Vres)) + sin((I*2*pi()/Ures)*Loops)*LoopRad) * cos((I*2*pi()/Ures)),
				  -(MinR*sin((J*2*pi()/Vres)) + cos((I*2*pi()/Ures)*Loops)*LoopRad),
			(MajR + MinR*cos((J*2*pi()/Vres)) + sin((I*2*pi()/Ures)*Loops)*LoopRad) * sin((I*2*pi()/Ures))
			} % this tuple contains the x,y,z coordinates of this vertex
			|| J <- lists:seq(0, Vres-1)]
			|| I <- lists:seq(0, Ures-1)],
	lists:append(Verts). % add it to the list of verts

% ======= Generate faces for all three types of tori =======
uvtorus_faces(Ures, Vres) ->
	Faces = [[
			[
			J + I*Vres,
			J + ((I+1) rem Ures) * Vres,
			(J+1) rem Vres + ((I+1) rem Ures)*Vres,
			(J+1) rem Vres + I*Vres
			] % this list contains the list of vertex indices in this face (a quad)
			|| J <- lists:seq(0, Vres-1)]
			|| I <- lists:seq(0, Ures-1)],
	lists:append(Faces). % add it to the list of faces

	% OBJ output for debugging.
	% io:format("UV Torus: ~p x ~p = ~p\n", [Ures, Vres, Ures*Vres]),
	% io:format("# Numverts: ~p\n", [length(Vs)]),
	% foreach(fun({X,Y,Z}) -> io:format("v ~10f ~10f ~10f\n", [X,Y,Z]) end, Vs),
	% io:format("# Numfaces: ~p\n", [length(Fs)]),
	% foreach(fun([A,B,C,D]) -> io:format("f ~2w ~2w ~2w ~2w\n", [A+1,B+1,C+1,D+1]) end, Fs),
