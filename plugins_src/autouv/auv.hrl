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
%%     $Id: auv.hrl,v 1.3 2002/10/13 21:25:42 dgud Exp $

-record(a, {center = {0,0}, scale = 1.0, rotate = 0.0, size, 
	    fs, vpos,
	    %% temporary stuff
	    twe,tbe}).

-record(areas, {as, matname, we, chbycut}).

-define(add_as(AsAA,TreeAA), 
	lists:foldl(fun({K,Area}, TreeBB) -> 
			    gb_trees:insert(hd(K),Area,TreeBB) 
		    end,TreeAA,AsAA)).

-record(setng, {texsz = {512, 512},   %% Texture size
		texbg = false,        %% Texture background
		color = true,         %% Texture drawing options
		edges = border_edges, %% Draw edges ??
		edge_color = false,   %% Use vertex/face color on edges
		edge_width = 2.0      %% Edge Thickness  overdraw help..
	       }).
-record(uvstate,
	{ op,               %% Current op i.e. move rotate, scale..
	  command,          %% Edit or create
	  mode = faceg,     %% faceg, face, edge, vertex
	  size = [0,0],     %% Max Size currently (unscaled)
	  option = #setng{},%% Settting
	  geom,             %% Window geom
	  last_file = "",   %% Export/Imported texture filename
	  dl,               %% Display list for non selected areas
	  %% Data
	  sel = [],         %% Selected areas
	  areas,            %% The areas
	  rest_objects,     %% The remaing objects 
	  st,               %% My maybe modified st
	  origst            %% Orignal st
	 }).             

-define(DBG(S,A), io:format("~p:~p " ++ S, [?MODULE,?LINE|A])).
