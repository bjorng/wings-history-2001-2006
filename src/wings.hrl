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
%%     $Id: wings.hrl,v 1.16 2001/11/07 20:55:55 bjorng Exp $
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

-define(MOUSE_DIVIDER, 500).

-define(UNDO_LEVELS, 32).
-define(HIT_BUF_SIZE, 2048).

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

-record(dl,
	{we=none,				%Winged edge objects.
	 dragging=none,				%WE faces being dragged.
	 drag_faces=none,			%GbSet containing faces.
	 pick=none,				%For picking.
	 pick_mode,				%Mode pick list was created in.
	 old_sel,				%Actual selection.
	 sel=none,				%For selected faces.
	 matrix=e3d_mat:identity()}).

-record(st,
	{shapes,				%All visible shapes
	 hidden,				%Hidden shapes
	 selmode,				%Selection mode:
						% vertex, edge, face, body
	 sel,					%Current sel: [{Id,GbSet}]
	 hsel,					%Hidden sel: GbSet
	 ssel,					%Saved selection.
	 mat,					%Defined materials (GbTree).
	 next_tx=100,				%Next OpenGL texture ID.
	 drag,					%Current drag information or
						% 'undefined' if no drag
						% in progress.
	 camera,				%Camera information or
						% 'undefined'.
	 dl=none,				%Cached display lists.
	 file,					%Current filename.
	 saved,					%True if model has been saved.
	 onext,					%Next object id to use.
	 hit_buf,				%Hit buffer for hit testing.
	 inf_r,					%Radius of influence (for magnet).
	 last_command,				%Last command.
	 bb=none,				%Saved bounding box.

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
	 next_id				%Next free ID for vertices,
						% edges, and faces
	 }).

%% Edge in a winged-edge shape.
-record(edge,
	{vs,					%Start vertex for edge
	 ve,					%End vertex for edge
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
