%%%-------------------------------------------------------------------
%%% File    : auv.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Records for texturing
%%%
%%% Created :  3 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002 Dan Gudmundsson 
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv.hrl,v 1.26 2004/02/17 17:12:45 dgud Exp $

%% Chart record (one for each chart).
%% Stored in the 'name' field in the #we{} record.
-record(ch,
	{size,
	 fs,					%Faces in chart (list).
	 vmap					%Map back to original vertex numbers.
	}).

-record(setng, {texsz = {512, 512},   %% Texture size
		texbg = false,        %% Texture background
		color = true,         %% Texture drawing options
		edges = all_edges,    %% Draw edges ??
		edge_color = false,   %% Use vertex/face color on edges
		edge_width = 2.0      %% Edge Thickness (overdraw help)
	       }).

-record(uvstate,
	{mode = body,      %% body, face, edge, vertex
	 geom,             %% Window geom
	 dl,               %% Display list for non selected areas

	 sel=[],           %% Selection: [{Id,GbSet}]
	 areas,            %% The charts 

	 matname,
	 orig_we,	    % Original We.
	 edges,		    % Edge numbers.
%%
	 st,               %% My maybe modified st
	 origst            %% Orignal st
	}).             

-define(HOLE, 'Ignore Chart').

-ifdef(DEBUG).
-define(DBG(S,A), io:format("~p:~p " ++ S, [?MODULE,?LINE|A])).
-else.
-define(DBG(S,A), ok).
-endif.


-ifndef(DEBUG).
-undef(TC).
-define(TC(Cmd), Cmd).
-endif.
