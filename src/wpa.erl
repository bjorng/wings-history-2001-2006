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
%%     $Id: wpa.erl,v 1.9 2002/02/06 16:57:48 bjorng Exp $
%%
-module(wpa).
-export([ask/3,error/1,message/1,yes_no/1,
	 bind_unicode/2,bind_virtual/3,
	 import/3,export/3,export_selected/3,
	 pref_get/2,pref_get/3,pref_set/3,pref_delete/2,
	 sel_get/1,sel_set/2,sel_set/3,sel_map/2,sel_fold/3,sel_convert/3,
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

bind_unicode(Key, Command) ->
    wings_hotkey:bind_unicode(Key, Command, plugin).

bind_virtual(Key, Mods, Command) ->
    wings_hotkey:bind_virtual(Key, Mods, Command, plugin).

%%%
%%% Import/export support.
%%%

import(Props, Importer, St) ->
    wings_file:import(Props, Importer, St).

export(Props, Exporter, St) ->
    wings_file:export(Props, Exporter, St),
    St.

export_selected(Props, Exporter, St) ->
    Shs0 = wings_sel:fold(fun(_, #we{id=Id}=We, A) ->
				  [{Id,We}|A]
			  end, [], St),
    Shs = gb_trees:from_orddict(reverse(Shs0)),
    wings_file:export(Props, Exporter, St#st{shapes=Shs}),
    St.

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

%%%    
%%% Selection utilities.
%%%

sel_set(Sel, St) ->
    wings_sel:set(Sel, St).

sel_set(Mode, Sel, St) ->
    wings_sel:set(Mode, Sel, St).

sel_get(#st{sel=Sel}) ->
    Sel.

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
