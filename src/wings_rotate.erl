%%
%%  wings_rotate.erl --
%%
%%     This module implements the Rotate command.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_rotate.erl,v 1.10 2001/10/03 09:24:11 bjorng Exp $
%%

-module(wings_rotate).
-export([setup/2]).
-include("wings.hrl").

-import(lists, [foldl/3]).

setup(Type, #st{selmode=vertex}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs = wings_sel:fold_shape(
	    fun(#shape{id=Id,sh=We}, Vs, Acc) ->
		    [{Id,vertices_to_vertices(Vs, We, Vec)}|Acc]
	    end, [], St),
    wings_drag:init_drag(Tvs, none, St);
setup(Type, #st{selmode=edge}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs = wings_sel:fold_region(
	    fun(Id, Edges, We, Acc) ->
		    [{Id,edges_to_vertices(Edges, We, Vec)}|Acc]
	    end, [], St),
    wings_drag:init_drag(Tvs, none, St);
setup(Type, #st{selmode=face}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs = wings_sel:fold_region(
	    fun(Id, Faces, We, Acc) ->
		    [{Id,faces_to_vertices(Faces, We, Vec)}|Acc]
	    end, [], St),
    wings_drag:init_drag(Tvs, none, St);
setup(Type, #st{selmode=body}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs = wings_sel:fold(
	    fun(#shape{id=Id,sh=#we{}=We}=Sh, Acc) ->
		    [{Id,body_to_vertices(We, Vec)}|Acc]
	    end, [], St),
    wings_drag:init_drag(Tvs, none, St).

%%
%% Conversion of vertex selection to vertices. :-)
%% Not entirely pointless, as we'll need to add vectors for
%% the points (vertices).
%%

vertices_to_vertices(Vs, We, Vec) ->
    rotate(gb_sets:to_list(Vs), We, Vec).

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Es, #we{es=Etab}=We, normal) ->
    Ns = foldl(fun(Edge, Acc) ->
		       #edge{lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
		       [wings_face:normal(Lf, We),
			wings_face:normal(Rf, We)|Acc]
	       end, [], gb_sets:to_list(Es)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    edges_to_vertices(Es, We, Vec);
edges_to_vertices(Es, #we{es=Etab}=We, Vec) ->
    Vs = wings_edge:to_vertices(Es, We),
    rotate(Vs, We, Vec).
 
%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Faces, We, normal) ->
    #we{fs=Ftab,es=Etab,vs=Vtab} = We,
    Ns = foldl(fun(Face, N0) ->
		       [wings_face:normal(Face, We)|N0]
	       end, [], gb_sets:to_list(Faces)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    Vs = wings_face:to_vertices(Faces, We),
    rotate(Vs, We, Vec);
faces_to_vertices(Faces, We, Vec) ->
    rotate(wings_face:to_vertices(Faces, We), We, Vec).
   
%%
%% Conversion of body selections (entire objects) to vertices.
%%

body_to_vertices(#we{vs=Vtab}=We, Vec) ->
    rotate(gb_trees:keys(Vtab), We, Vec).

%% Setup rotation.

rotate(Vs, We, Vec) when list(Vs) ->
    Center = wings_vertex:center(Vs, We),
    [{{rot,Center,Vec},Vs}].
