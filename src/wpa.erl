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
%%     $Id: wpa.erl,v 1.16 2002/05/05 07:41:53 bjorng Exp $
%%
-module(wpa).
-export([ask/3,ask/4,dialog/3,dialog/4,error/1,yes_no/1,
	 bind_unicode/2,bind_virtual/3,
	 import/3,export/3,export_selected/3,export_filename/2,
	 pref_get/2,pref_get/3,pref_set/2,pref_set/3,pref_delete/2,
	 sel_get/1,sel_set/2,sel_set/3,sel_map/2,sel_fold/3,sel_convert/3,
	 sel_edge_regions/2,sel_face_regions/2,
	 drag/3,drag/4,
	 pick/3,
	 vertices/1,vertex_pos/2,vertex_flatten/3,vertex_center/2,
	 faces/1,face_vertices/2,face_outer_vertices/2,face_outer_edges/2,
	 edge_loop_vertices/2,
	 obj_name/1,obj_id/1,
	 camera_info/1
	]).

-include("wings.hrl").
-import(lists, [reverse/1,foldl/3,foreach/2]).

%%%
%%% ask/3,4 is simpler to use, but only supports a single list of fields.
%%% dialog/3,4 is more powerful but is slightly more involved.
%%%

ask(Qs, St, Fun) ->
    wings_ask:ask(Qs, St, Fun).

ask(Ask, Qs, St, Fun) ->
    wings_ask:ask(Ask, Qs, St, Fun).

dialog(Qs, St, Fun) ->
    wings_ask:dialog(Qs, St, Fun).

dialog(Ask, Qs, St, Fun) ->
    wings_ask:dialog(Ask, Qs, St, Fun).

%% Show String in a dialog box.
error(String) ->
    wings_util:error(String).

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

export_selected(Props, Exporter, #st{selmode=Mode}=St)
  when Mode == body; Mode == face ->
    Shs0 = wings_sel:fold(
	     fun(Elems, #we{id=Id}=We, A) ->
		     [{Id,export_sel_set_holes(Mode, Elems, We)}|A]
	     end, [], St),
    Shs = gb_trees:from_orddict(reverse(Shs0)),
    wings_file:export(Props, Exporter, St#st{shapes=Shs}),
    St;
export_selected(_, _, _) -> error("Select objects or faces.").

export_sel_set_holes(body, _, We) -> We;
export_sel_set_holes(face, Faces, #we{fs=Ftab0}=We) ->
    Ftab = foldl(fun({Face,Rec}=Keep, A) ->
			 case gb_sets:is_member(Face, Faces) of
			     true -> [Keep|A];
			     false -> [{Face,Rec#face{mat='_hole_'}}|A]
			 end
		 end, [], gb_trees:to_list(Ftab0)),
    We#we{fs=gb_trees:from_orddict(reverse(Ftab))}.

export_filename(Prop, St) ->
    wings_file:export_filename(Prop, St).

%%%
%%% Preferences.
%%%
%%% As Mod, pass in ?MODULE.
%%%

pref_get(Mod, Key) ->
    wings_pref:get_value({Mod,Key}).

pref_get(Mod, Key, Default) ->
    wings_pref:get_value({Mod,Key}, Default).

pref_set(Mod, KeyVals) when is_list(KeyVals) ->
    foreach(fun({Key,Val}) ->
		    wings_pref:set_value({Mod,Key}, Val)
	    end, KeyVals).

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
    wings_sel:map(F, St).

sel_fold(F, Acc, St) ->
    wings_sel:fold(F, Acc, St).

sel_convert(F, Mode, St) ->
    Sel = wings_sel:fold(
	    fun(Items0, #we{id=Id}=We, A) ->
		    case F(Items0, We) of
			[] -> A;
			[_|_]=Items ->
			    [{Id,gb_sets:from_list(Items)}|A];
			Items ->
			    case gb_sets:is_empty(Items) of
				true -> A;
				false -> [{Id,Items}|A]
			    end
		    end
	    end, [], St),
    wings_sel:set(Mode, Sel).

sel_edge_regions(Edges, We) ->
    wings_sel:edge_regions(Edges, We).

sel_face_regions(Faces, We) ->
    wings_sel:face_regions(Faces, We).

%%%
%%% Picking.
%%%

pick(X, Y, St) ->
    wings_pick:do_pick(X, Y, St).

%%%
%%% Dragging support
%%%

drag(Tvs, Units, St) ->
    wings_drag:setup(Tvs, Units, [], St).

drag(Tvs, Units, Flags, St) ->
    wings_drag:setup(Tvs, Units, Flags, St).

%%%
%%% Vertex functions.
%%%

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

%%%
%%% Camera info.
%%%

camera_info(As) ->
    camera_info(As, wings_view:current()).

camera_info([aim|As], #view{origo=Aim}=View) ->
    [Aim|camera_info(As, View)];
camera_info([distance_to_aim|As], #view{distance=Dist}=View) ->
    [Dist|camera_info(As, View)];
camera_info([azimuth|As], #view{azimuth=Az}=View) ->
    [Az|camera_info(As, View)];
camera_info([elevation|As], #view{elevation=El}=View) ->
    [El|camera_info(As, View)];
camera_info([tracking|As], #view{pan_x=X,pan_y=Y}=View) ->
    [{X,Y}|camera_info(As, View)];
camera_info([fov|As], View) ->
    %% Field of view. XXX Should not be hard-coded.
    [45|camera_info(As, View)];
camera_info([], _) -> [].
