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
%%     $Id: auv.hrl,v 1.13 2003/01/27 13:57:49 dgud Exp $

%% Chart record (one for each chart).
-record(ch,
	{center = {0,0},
	 scale = 1.0,
	 rotate = 0.0,
	 size,
	 be,				%Boundary edges.
	 bf = [],                       % Backface created from cut.
	 we                             % We per chart
	}).

-record(setng, {texsz = {512, 512},   %% Texture size
		texbg = false,        %% Texture background
		color = true,         %% Texture drawing options
		edges = all_edges,    %% Draw edges ??
		edge_color = false,   %% Use vertex/face color on edges
		edge_width = 2.0      %% Edge Thickness (overdraw help)
	       }).
-record(uvstate,
	{ op,               %% Current op i.e. move rotate, scale..
	  mode = faceg,     %% faceg, face, edge, vertex
	  size = [0,0],     %% Max Size currently (unscaled)
	  option = #setng{},%% Settings
	  geom,             %% Window geom
	  last_file = "",   %% Export/Imported texture filename
	  dl,               %% Display list for non selected areas
%% Data
	  sel = [],         %% Selected areas
	  areas,            %% The charts 
%%
	  id,               %% We Id of orig_we.
	  matname,
	  orig_we,	    % Original We.
	  edges,	    % Edge numbers.
	  vmap,	            % Map for going back to original vertex numbers.
%%
	  st,               %% My maybe modified st
	  origst            %% Orignal st
	 }).             

-ifdef(DEBUG).
-define(DBG(S,A), io:format("~p:~p " ++ S, [?MODULE,?LINE|A])).
-else.
-define(DBG(S,A), ok).
-endif.


-ifndef(DEBUG).
-undef(TC).
-define(TC(Cmd), Cmd).
-endif.
