%% Plane Plugin for Wings 0.90 or higher.
%% Copyright (c) 2002 Anthony D'Agostino (scorpius@compuserve.com)
%%
%% For updates and other plugins:
%% http://ourworld.compuserve.com/homepages/scorpius

-module(wpc_plane).
-export([init/0,menu/2,command/2]).
-import(math, [sqrt/1,cos/1,sin/1,pi/0,pow/2,exp/1]).

init() -> true.

menu({shape}, Menu0) ->
	Menu0 ++ [separator,
		{"Plane",plane,[option]},
		{"Lumpy Plane",lumpyplane,[option]},
		{"Wavy Plane",wavyplane,[option]},
		{"Sombrero Plane",sombreroplane,[option]}
		];
menu(_, Menu) -> Menu.

command({shape,{plane,Ask}}, St) -> make_plane(Ask, St);
command({shape,{lumpyplane,Ask}}, St) -> make_lumpyplane(Ask, St);
command({shape,{wavyplane,Ask}}, St) -> make_wavyplane(Ask, St);
command({shape,{sombreroplane,Ask}}, St) -> make_sombreroplane(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

% ======= Regular Plane =======
make_plane(Ask, St) when is_atom(Ask) ->
	wpa:ask(Ask, [ {"Resolution",5},
				   {"Size",2}
				 ],
	St, fun(Res) -> {shape,{plane,Res}} end);
make_plane([Nres, Size], St) ->
	Vs = plane_verts(Nres, Size),
	Fs = plane_faces_top(Nres, Nres)++plane_faces_bot(Nres, Nres),
	{new_shape,"Plane",Fs,Vs}.

% ======= Lumpy Plane =======
make_lumpyplane(Ask, St) when is_atom(Ask) ->
	wpa:ask(Ask, [ {"Resolution",30},
				   {"Size",2},
				   {"Lumps",2}
				 ],
	St, fun(Res) -> {shape,{lumpyplane,Res}} end);
make_lumpyplane([Nres, Size, Lumps], St) ->
	Vs = lumpyplane_verts(Nres, Size, Lumps),
	Fs = plane_faces_top(Nres, Nres)++plane_faces_bot(Nres, Nres),
	{new_shape,"Lumpy Plane",Fs,Vs}.

% ======= Wavy Plane =======
make_wavyplane(Ask, St) when is_atom(Ask) ->
	wpa:ask(Ask, [ {"Resolution",60},
				   {"Size",2},
				   {"Waves",4},
				   {"Height", 0.2}
				 ],
	St, fun(Res) -> {shape,{wavyplane,Res}} end);
make_wavyplane([Nres, Size, Waves, Height], St) ->
	Vs = wavyplane_verts(Nres, Size, Waves, Height),
	Fs = plane_faces_top(Nres, Nres)++plane_faces_bot(Nres, Nres),
	{new_shape,"Wavy Plane",Fs,Vs}.

% ======= Sombrero Plane =======
make_sombreroplane(Ask, St) when is_atom(Ask) ->
	wpa:ask(Ask, [ {"Resolution",60},
				   {"Size",2},
				   {"Waves",4},
				   {"Falloff",1},
				   {"Height", 1}
				 ],
	St, fun(Res) -> {shape,{sombreroplane,Res}} end);
make_sombreroplane([Nres, Size, Waves, Falloff, Height], St) ->
        Vs = sombreroplane_verts(Nres, Size, Waves, Falloff, Height),
        Fs = plane_faces_top(Nres, Nres)++plane_faces_bot(Nres, Nres),
	{new_shape,"Sombrero Plane",Fs,Vs}.

% ======= Regular Plane: Generate Verts =======
plane_verts(Nres, Size) ->
	Verts = [[
			{
			Size*(I/(Nres-1.0)*2-1),
			Size*(0.0),
			Size*(J/(Nres-1.0)*2-1)
			} % this tuple contains the x,y,z coordinates of this vertex
			|| J <- lists:seq(0, Nres-1)]
			|| I <- lists:seq(0, Nres-1)],
	lists:append(Verts). % add it to the list of verts

% ======= Lumpy Plane: Generate Verts =======
lumpyplane_verts(Nres, Size, Lumps) ->
	Verts = [[
			{
			Size*(I/(Nres-1.0)*2-1),
			Size*(cos(Lumps*(I/(Nres-1.0)*2-1)*pi())*
				  cos(Lumps*(J/(Nres-1.0)*2-1)*pi())/(Lumps*3)),
			Size*(J/(Nres-1.0)*2-1)
			} % this tuple contains the x,y,z coordinates of this vertex
			|| J <- lists:seq(0, Nres-1)]
			|| I <- lists:seq(0, Nres-1)],
	lists:append(Verts). % add it to the list of verts

% ======= Wavy Plane: Generate Verts =======
wavyplane_verts(Nres, Size, Waves, Height) ->
	Verts = [[
			{
			Size*(I/(Nres-1.0)*2-1),
			Size*(1.0/Waves *
				  cos(2*pi()*sqrt( pow((Waves*(I/(Nres-1.0)*2-1)), 2)
								 + pow((Waves*(J/(Nres-1.0)*2-1)), 2))))*Height,
			Size*(J/(Nres-1.0)*2-1)
			} % this tuple contains the x,y,z coordinates of this vertex
			|| J <- lists:seq(0, Nres-1)]
			|| I <- lists:seq(0, Nres-1)],
	lists:append(Verts). % add it to the list of verts

% ======= Sombrero Plane: Generate Verts =======
sombreroplane_verts(Nres, Size, Waves, Falloff, Height) ->
	Verts = [[
			{
			Size*(I/(Nres-1.0)*2-1),
			Size*(1.0/Waves *
				  cos(2*pi()*sqrt( pow((Waves*(I/(Nres-1.0)*2-1)), 2)
								  +pow((Waves*(J/(Nres-1.0)*2-1)), 2)))
					  * exp(-sqrt( pow((Waves*(I/(Nres-1.0)*2-1)), 2)
								  +pow((Waves*(J/(Nres-1.0)*2-1)), 2))*Falloff))*Height,
			Size*(J/(Nres-1.0)*2-1)
			} % this tuple contains the x,y,z coordinates of this vertex
			|| J <- lists:seq(0, Nres-1)]
			|| I <- lists:seq(0, Nres-1)],
	lists:append(Verts). % add it to the list of verts


% ======= Generate Faces For Plane =======
plane_faces_top(Ures, Vres) ->
	Faces = [[
			[
			I*Vres+J, I*Vres+J+1, (I+1)*Vres+J+1, (I+1)*Vres+J
			] % this list contains the list of vertex indices
			  % in this face (a quad)
			|| J <- lists:seq(0, Vres-2)]
			|| I <- lists:seq(0, Ures-2)],
	lists:append(Faces). % add it to the list of faces
plane_faces_bot(Ures, Vres) ->
	Faces = [[
			[
			(I+1)*Vres+J, (I+1)*Vres+J+1, I*Vres+J+1, I*Vres+J
			] % this list contains the list of vertex indices
			  % in this face (a quad)
			|| J <- lists:seq(0, Vres-2)]
			|| I <- lists:seq(0, Ures-2)],
	lists:append(Faces). % add it to the list of faces

	% Is there a faster way to reverse a list of lists?

