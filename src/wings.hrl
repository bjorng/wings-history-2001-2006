%%
%%  wings.hrl --
%%
%%     Global record definition and defines.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings.hrl,v 1.25 2001/11/23 17:25:48 bjorng Exp $
%%

-ifdef(NEED_ESDL).
-include_lib("esdl/include/sdl.hrl").
-include_lib("esdl/include/sdl_events.hrl").
-include_lib("esdl/include/sdl_video.hrl").
-include_lib("esdl/include/sdl_keyboard.hrl").
-endif.

-ifdef(NEED_OPENGL).
-include_lib("esdl/include/gl.hrl").
-include_lib("esdl/include/glu.hrl").
-endif.

-define(WINGS_VERSION, ?wings_version).

-define(CHAR_HEIGHT, 14).
-define(CHAR_WIDTH, 7).
-define(LINE_HEIGHT, (?CHAR_HEIGHT+3)).
-define(GROUND_GRID_SIZE, 1.0).
-define(CAMERA_DIST, (8*?GROUND_GRID_SIZE)).
-define(NORMAL_LINEWIDTH, 0.1).

-define(MOUSE_DIVIDER, 500).

-define(UNDO_LEVELS, 32).
-define(HIT_BUF_SIZE, (1024*1024)).

-ifdef(DEBUG).
-define(ASSERT(E), case E of
		       true -> ok;
		       _ ->
			   erlang:fault({assertion_failed,?MODULE,?LINE})
		   end).
-define(VALIDATE_MODEL(St), wings_util:validate(St)).
-define(CHECK_ERROR(), wings_util:check_error(?MODULE, ?LINE)).
-else.
-define(ASSERT(E),ok).
-define(VALIDATE_MODEL(St),ok).
-define(CHECK_ERROR(), ok).
-endif.

-define(DL_PICK, 99).
-define(DL_DRAW_BASE, 100).

%% Display lists.
-record(dl,
	{faces=none,				%Faces.
	 edges=none,				%Edges.
	 pick=none,				%For picking.
	 old_sel,				%Actual selection.
	 sel=none				%For selected faces.
	}).

%% Main state record containing all objects and other important state.
-record(st,
	{shapes,				%All visible shapes
	 hidden,				%Hidden shapes
	 selmode,				%Selection mode:
						% vertex, edge, face, body
	 sel=[],				%Current sel: [{Id,GbSet}]
	 hsel,					%Hidden sel: GbSet
	 ssel,					%Saved selection.
	 mat,					%Defined materials (GbTree).
	 next_tx=100,				%Next OpenGL texture ID.
	 drag=none,				%Current drag information or
						% 'none' if no drag
						% in progress.
	 dl=none,				%Cached display lists.
	 file,					%Current filename.
	 saved,					%True if model has been saved.
	 onext,					%Next object id to use.
	 hit_buf,				%Hit buffer for hit testing.
	 inf_r,					%Radius of influence (for magnet).
	 bb=none,				%Saved bounding box.
	 edge_loop=none,			%Previous edge loop.

	 %% Previous commands.
	 repeatable,				%Last repeatable command.

	 %% Undo information.
	 top,					%Top of stack.
	 bottom,				%Bottom of stack.
	 next_is_undo,				%State of undo/redo toggle.
	 undone					%States that were undone.
	 }).

%% Shape (or object) which can be implemented in different ways.
-record(shape,
	{id,					%Shape id
	 name,					%Shape name
	 matrix=e3d_mat:identity(),		%Transformation matrix
	 sh					%The shape itself:
						% An 'we' record
	}).

%% The Winged-Edge data structure.
%% See http://www.cs.mtu.edu/~shene/COURSES/cs3621/NOTES/model/winged-e.html
-record(we,
	{es,					%gb_tree containing edges
	 vs,					%gb_tree containing vertices
	 fs,					%gb_tree containing faces
	 he,					%gb_sets containing hard edges
	 first_id,				%First ID used
	 next_id,				%Next free ID for vertices,
						% edges, and faces
	 mode					%'vertex'/'material'
	 }).

%% Edge in a winged-edge shape.
-record(edge,
	{vs,					%Start vertex for edge
	 ve,					%End vertex for edge
	 a=wings_color:default(),		%Color or UV coordinate.
	 b=wings_color:default(),		%Color or UV coordinate.
	 lf,					%Left face
	 rf,					%Right face
	 ltpr,					%Left traversal predecessor
	 ltsu,					%Left traversal successor
	 rtpr,					%Right traversal predecessor
	 rtsu					%Right traversal successor
	}).

%% A face in a winged-edge shape.
-record(face,
	{edge,					%Incident edge
	 mat=default				%Material for face
	}).

%% A vertex in a winged-edge shape.
-record(vtx,
	{edge,					%Incident edge
	 pos					%Position ({X,Y,Z})
	}).

%% The current view.
-record(view,
	{origo,
	 distance,				% From origo
	 azimuth,
	 elevation,
	 pan_x,					%Panning in X direction.
	 pan_y}).				%Panning in Y direction
