%%
%%  wpa.erl --
%%
%%     Wings Plugin API.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpa.erl,v 1.5 2002/01/25 09:04:38 bjorng Exp $
%%
-module(wpa).
-export([ask/3,error/1,message/1,yes_no/1,
	 pref_get/2,pref_get/3,pref_set/3,pref_delete/2,
	 sel_map/2,sel_fold/3,sel_convert/3,
	 sel_edge_regions/2,sel_face_regions/2,
	 vertices/1,vertex_pos/2,vertex_flatten/3,vertex_center/2,
	 faces/1,face_vertices/2,face_outer_vertices/2,face_outer_edges/2,
	 edge_loop_vertices/2,
	 obj_name/1,obj_id/1
	]).

-include("wings.hrl").
-import(lists, [reverse/1]).

ask(Ask, Qs, Fun) ->
    wings_util:ask(Ask, Qs, Fun).

%% Show String in a dialog box.
error(String) ->
    throw({command_error,String}).

%% Show message and wait for OK.
message(Message) ->
    wings_util:message(Message).

%% Ask yes/no question. Returns yes|no|aborted.
yes_no(Question) ->
    wings_util:yes_no(Question).

%%%
%%% Preferences.
%%%
%%% As Mod, pass in ?MODULE.
%%%

pref_get(Mod, Key) ->
    wings_pref:get_value({Mod,Key}).

pref_get(Mod, Key, Default) ->
    wings_pref:get_value({Mod,Key}, Default).

pref_set(Mod, Key, Value) ->
    wings_pref:set_value({Mod,Key}, Value).

pref_delete(Mod, Key) ->
    wings_pref:delete_value({Mod,Key}).
    
%%% Selection utilities.

sel_map(F, St) ->
    wings_sel:map(
      fun(Items, We) ->
	      F(gb_sets:to_list(Items), We)
      end, St).

sel_fold(F, Acc, St) ->
    wings_sel:fold(
      fun(Items, We, A) ->
	      F(gb_sets:to_list(Items), We, A)
      end, Acc, St).

sel_convert(F, Mode, St) ->
    Sel = wings_sel:fold(
	    fun(Items0, #we{id=Id}=We, A) ->
		    case F(gb_sets:to_list(Items0), We) of
			[] -> A;
			Items -> [{Id,gb_sets:from_list(Items)}|A]
		    end
	    end, [], St),
    St#st{selmode=Mode,sel=reverse(Sel)}.

sel_edge_regions(Edges, We) ->
    wings_sel:edge_regions(Edges, We).

sel_face_regions(Faces, We) ->
    wings_sel:face_regions(Faces, We).

%%% Vertices.

vertices(#we{vs=Vtab}) -> gb_trees:keys(Vtab).

vertex_pos(V, #we{vs=Vtab}) ->
    #vtx{pos=Pos} = gb_trees:get(V, Vtab),
    Pos.

vertex_flatten(Vs, PlaneNormal, We) ->
    wings_vertex:flatten(Vs, PlaneNormal, We).

vertex_center(Vs, We) ->
    wings_vertex:center(Vs, We).

%%% Edges.

edge_loop_vertices(Edges, We) ->
    wings_edge_loop:edge_loop_vertices(Edges, We).

%%% Faces

faces(#we{fs=Ftab}) -> gb_trees:keys(Ftab).

face_vertices(Face, We) ->
    wings_face:surrounding_vertices(Face, We).

face_outer_vertices(Faces, We) ->
    wings_vertex:outer_partition(Faces, We).

face_outer_edges(Faces, We) ->
    wings_face_cmd:outer_edge_partition(Faces, We).

%%% Objects.

obj_name(#we{name=Name}) -> Name.
obj_id(#we{id=Id}) -> Id.
