%%
%%  e3d.hrl --
%%
%%     Record definition for generic in-memory 3D file format.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d.hrl,v 1.1 2001/08/14 18:16:30 bjorng Exp $
%%

-record(e3d_file,
	{objs=[],				%List of objects.
	 mat=[],				%List of materials.
	 creator=""}).				%Creator string.

-record(e3d_object,
	{name,					%Name of object (string).
	 obj,					%Object implementation.
	 attr=[]}).				%List of attributes.

%% Polygon mesh.
-record(e3d_mesh,
 	{type=triangle,				%'triangle' or 'polygon'.
	 vs=[],					%Vertex table (list).
 	 fs=[],					%Face table (list of e3d_face).
	 he=[],					%List of chains of hard edges.
	 matrix=none				%Local coordinate system.
 	}).

-record(e3d_face,
	{vs=[],					%List of vertex indices.
	 mat=[],				%Materials for face.
	 vis=-1}).				%Visible edges


