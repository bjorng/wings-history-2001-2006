%% File    : auv_seg_ui.erl
%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%% Description : The segmentation interface.
%%
%% Created : 24 Jan 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%-------------------------------------------------------------------
%%  Copyright (c) 2002-2003 Bjorn Gustavsson, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv_seg_ui.erl,v 1.5 2003/07/11 10:26:28 bjorng Exp $

-module(auv_seg_ui).
-export([start/3]).

-define(NEED_ESDL, 1).
-include("wings.hrl").
-include("auv.hrl").
-import(lists, [sort/1,map/2,member/2,foldl/3]).

%%%
%%% Segmentation interface.
%%%

-record(seg, {st,				%Current St.
	      selmodes,				%Legal selection modes.
	      we,				%Original We.
	      msg				%Message.
	     }).

start(#we{id=Id}=We0, OrigWe, St0) ->
    Modes = [vertex,edge,face],
    wings:mode_restriction(Modes),
    We = We0#we{mode=material},
    check_for_defects(We),
    St1 = seg_create_materials(St0),
    St = St1#st{sel=[],selmode=face,shapes=gb_trees:from_orddict([{Id,We}])},
    Ss = seg_init_message(#seg{selmodes=Modes,st=St,we=OrigWe}),
    {seq,push,get_seg_event(Ss)}.
    
%     Active = wings_wm:this(),
%     wings_wm:callback(fun() ->
% 			      wings_util:menu_restriction(Active, [view,select,window])
% 		      end),
    

seg_init_message(Ss) ->
    Msg = ["[L] Select  [R] Show menu  "|wings_camera:help()],
    Ss#seg{msg=Msg}.

get_seg_event(#seg{st=St}=Ss) ->
    wings_wm:current_state(St),
    wings_draw:update_dlists(St),
    wings_wm:dirty(),
    get_seg_event_noredraw(Ss).

get_seg_event_noredraw(Ss) ->
    {replace,fun(Ev) -> seg_event(Ev, Ss) end}.

seg_event(init_opengl, #seg{st=St}=Ss) ->
    wings:init_opengl(St),
    get_seg_event(Ss);
seg_event(redraw, #seg{st=St,msg=Msg}) ->
    wings_wm:message(Msg, "Segmenting"),
    wings:redraw(St),
    keep;
seg_event(close, _) ->
    seg_cancel();
seg_event(Ev, #seg{st=St}=Ss) ->
    case wings_camera:event(Ev, St) of
	next -> seg_event_2(Ev, Ss);
	Other -> Other
    end.

seg_event_2(Ev, #seg{st=St}=Ss) ->
    case wings_pick:event(Ev, St, fun() -> wings:redraw(St) end) of
	next -> seg_event_3(Ev, Ss);
	Other -> Other
    end.

seg_event_3(Ev, #seg{st=#st{selmode=Mode}}=Ss) ->
    case wings_menu:is_popup_event(Ev) of
	no -> seg_event_4(Ev, Ss);
	{yes,X,Y,_} -> 
	    Mappers = mappers(), 
	    Menu = [{"Continue",{continue, Mappers}},
		    separator,
		    {"Segment by",
		     {segment,
		      [{"Projection",autouvmap},
		       {"Feature Detection",feature}]}}|
		    seg_mode_menu(Mode, Ss, seg_debug([]))],
	    wings_menu:popup_menu(X, Y, auv_segmentation, Menu)
    end.

-ifndef(DEBUG).
seg_debug(Tail) -> Tail.
mappers() ->
    [{"Unfolding",lsqcm},				  
     {"Projection",project}].
-else.
seg_debug(Tail) ->
    [separator,
     {"Debugging",
      {debug,
       [{"Select features",select_features},
	{"Select seeds",select_seeds},
        {"Select Pinned vertices", select_pinned}]}}|Tail].
mappers() ->
    [{"Unfolding",lsqcm}, 
     {"Two pass Unfolding",lsqcm2},
     {"Projection",project}].
-endif.

seg_mode_menu(vertex, _, Tail) -> Tail;
seg_mode_menu(edge, _, Tail) ->
    [separator,
     {"Mark Edges for Cut",cut_edges},
     {"Unmark Edges",no_cut_edges},
     separator,
     {"Select Marked Edges",select_hard_edges}|Tail];
seg_mode_menu(face, _, Tail) ->
    Menu0 = map(fun({Name,Color}) ->
			{atom_to_list(Name),Name,[],[{color,Color}]};
		   (Other) -> Other
		end, auv_util:seg_materials()),
    Menu = Menu0 ++
	[separator,
	 {"Select",{select,Menu0}}|Tail],
    [separator|Menu].

seg_event_4(Ev, Ss) ->
    case translate_key(Ev) of
	next -> seg_event_5(Ev, Ss);
	Other -> Other
    end.

seg_event_5(Ev, #seg{st=St0}=Ss) ->
    case wings_hotkey:event(Ev, St0) of
	next -> seg_event_6(Ev, Ss);
	Action ->
	    wings_wm:later({action,Action}),
	    keep
    end.

seg_event_6({new_state,St}, Ss) ->
    get_seg_event(Ss#seg{st=St});
seg_event_6({action,{view,Cmd}}, #seg{st=St0}=Ss) ->
    case wings_view:command(Cmd, St0) of
	#st{}=St -> get_seg_event(Ss#seg{st=St});
	Other -> Other
    end;
seg_event_6({action,{select,Cmd}}, #seg{st=St0}=Ss) ->
    case wings_sel_cmd:command(Cmd, St0) of
	St0 ->     keep;
	{save_state,St} ->  filter_sel_command(Ss, St);
	#st{}=St ->  	    filter_sel_command(Ss, St);
	Other -> 	    Other
    end;
seg_event_6({action,{window,geom_viewer}}, _) ->
    keep;
seg_event_6({action,{window,Cmd}}, #seg{st=St0}=Ss) ->
    case wings:command({window,Cmd}, St0) of
	St0 -> keep;
	#st{}=St -> get_seg_event(Ss#seg{st=St});
	Other -> Other
    end;
seg_event_6({action,{material,Cmd}}, #seg{st=St0}=Ss) ->
    case wings_material:command(Cmd, St0) of
	St0 -> keep;
	{save_state,St} -> seg_event({new_state,St}, Ss);
	#st{}=St -> seg_event({new_state,St}, Ss);
	Other -> Other
    end;
seg_event_6({action,{auv_segmentation,Cmd}}, Ss) ->
    seg_command(Cmd, Ss);
seg_event_6({callback, Fun}, _) when function(Fun) ->
    Fun();
seg_event_6({message,Message}, _) ->
    wings_util:message(Message);
seg_event_6(#mousemotion{}, _) -> keep;
seg_event_6(#mousebutton{}, _) -> keep;
seg_event_6(#keyboard{}, _) -> keep;
seg_event_6(_Ev, _) ->
%%    ?DBG("~w\n", [_Ev]),
    keep.

translate_key(#keyboard{sym=27}) ->
    seg_cancel();
translate_key(_) -> next.

filter_sel_command(#seg{selmodes=Modes}=Ss, #st{selmode=Mode}=St) ->
    case member(Mode, Modes) of
	false -> keep;
	true -> seg_event({new_state,St}, Ss)
    end.

seg_command({continue,Method}, Ss) ->
    seg_map_charts(Method, Ss);
seg_command(cut_edges, #seg{st=St0}=Ss) ->
    St = wings_edge:hardness(hard, St0),
    get_seg_event(Ss#seg{st=St});
seg_command(no_cut_edges, #seg{st=St0}=Ss) ->
    St = wings_edge:hardness(soft, St0),
    get_seg_event(Ss#seg{st=St});
seg_command(select_hard_edges, _) ->
    wings_wm:send(geom, {action,{select,{by,hard_edges}}}),
    keep;
seg_command({select,Mat}, _) ->
    wings_wm:send(geom, {action,{material,{select,[atom_to_list(Mat)]}}}),
    keep;
seg_command({segment,Type}, #seg{st=St0}=Ss) ->
    St = segment(Type, St0),
    get_seg_event(Ss#seg{st=St});
seg_command({debug,select_features}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    Tot = gb_trees:size(We#we.es),
    {Es,_,_} = auv_segment:find_features(We, 60, Tot div 50),
    Sel = [{Id,gb_sets:from_list(Es)}],
    get_seg_event(Ss#seg{st=St#st{selmode=edge,sel=Sel}});
seg_command({debug,select_seeds}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    Tot = gb_trees:size(We#we.es),
    {Features,_,_} = auv_segment:find_features(We, 60, Tot div 50),
    {Seeds0,_} = auv_segment:build_seeds(Features, We),
    Seeds = [S || {_,S} <- Seeds0],
    Sel = [{Id,gb_sets:from_list(Seeds)}],
    get_seg_event(Ss#seg{st=St#st{selmode=face,sel=Sel}});
seg_command({debug,select_pinned}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    [{Id,SetOfFaces}] = St#st.sel,
    case {St#st.selmode == face, gb_sets:to_list(SetOfFaces)} of
	{true,Fs} when Fs /= [] ->
	    {{V1,_UV1},{V2,_UV2}} = auv_mapping:find_pinned(Fs, We),
	    ?DBG("Pinned ~p ~n", [{{V1,_UV1},{V2,_UV2}}]),
	    Sel = [{Id,gb_sets:from_list([V1,V2])}],
	    get_seg_event(Ss#seg{st=St#st{selmode=vertex,sel=Sel}});
	_ -> 
	    ?DBG("Not in face mode~n", []),
	    keep
    end;

seg_command(Cmd, #seg{st=#st{mat=Mat}=St0}=Ss) ->
    case gb_trees:is_defined(Cmd, Mat) of
	false ->
	    io:format("Cmd: ~w\n", [Cmd]),
	    keep;
	true ->
	    St = wings_material:command({assign,atom_to_list(Cmd)}, St0),
	    get_seg_event(Ss#seg{st=St})
    end.

seg_cancel() ->
    wings_draw_util:delete_dlists(),
    delete.

seg_create_materials(St0) ->
    M0 = auv_util:seg_materials(),
    M = [{Name,auv_util:make_mat(Diff)} || {Name,Diff} <- M0],
    {St,[]} = wings_material:add_materials(M, St0),
    St.

seg_map_charts(Method, #seg{st=#st{shapes=Shs},we=OrigWe}=Ss) ->
    [{_,#we{he=Cuts0}=We0}] = gb_trees:to_list(Shs),
    Charts0 = auv_segment:segment_by_material(We0),
    {Charts1,Cuts} = auv_segment:normalize_charts(Charts0, Cuts0, We0),
    Charts = auv_segment:cut_model(Charts1, Cuts, OrigWe),
    N = length(Charts),
    seg_map_charts_1(Charts, Method, 1, N, [], Ss).

seg_map_charts_1(Cs, Type, I, N, Acc, Ss) when I =< N ->
    MapChart = fun() -> seg_map_chart(Cs, Type, I, N, Acc, Ss) end,
    wings_wm:later({callback,MapChart}),
    Msg = io_lib:format("Mapping chart ~w of ~w", [I,N]),
    get_seg_event(Ss#seg{msg=Msg});
seg_map_charts_1(_, _, _, _, MappedCharts, #seg{we=#we{id=Id}}) ->
    wings_wm:later({init_show_maps,Id,MappedCharts}),
    pop.

seg_map_chart([{Fs,Vmap,We0}|Cs], Type, I, N, Acc0, Ss) ->
    case auv_mapping:map_chart(Type, Fs, We0) of
	{error,Message} ->
	    seg_error(Message, Ss);
	Vs ->
	    We = We0#we{vp=gb_trees:from_orddict(sort(Vs))},
	    Acc = [#ch{we=We,fs=Fs,vmap=Vmap}|Acc0],
	    seg_map_charts_1(Cs, Type, I+1, N, Acc, Ss)
    end.

seg_error(Message, Ss) ->
    wings_wm:send(geom, {message,Message}),
    get_seg_event(seg_init_message(Ss)).

check_for_defects(We) ->
    case wings_vertex:isolated(We) of
	[] ->
	    ok;
	[_|_] ->
	    wpa:error("The model has isolated vertices. (Use the Cleanup command.)")
    end.

segment(Mode, #st{shapes=Shs}=St) ->
    [{_,We}] = gb_trees:to_list(Shs),
    {Charts,Cuts} = auv_segment:create(Mode, We),
    auv_util:mark_segments(Charts, Cuts, We, St).
