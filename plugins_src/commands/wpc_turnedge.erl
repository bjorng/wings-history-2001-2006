%%
%%  wpc_turnedge.erl --
%%
%%     Plug-in for turning edges
%%
%%  Copyright (c) 2002 Chris Osgood
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_turnedge.erl,v 1.2 2002/12/26 09:47:07 bjorng Exp $
%%

-module(wpc_turnedge).

-export([init/0,menu/2,command/2]).

-include_lib("wings.hrl").

init() ->
    wpa:bind_unicode($t, {edge,turn}),
    wpa:bind_unicode($T, {edge,turn_opt}),
    true.

menu({edge}, Menu0) ->
    case wings_pref:get_value(advanced_menus) of
	true ->
	    TurnMenu = turns(),
	    Menu0 ++ [separator,
		      {"Turn",{turn, TurnMenu}}];
	false ->
	    Menu0 ++ [separator,
		      {"Turn",turn,"Turn edge"},
		      {"Optimized Turn", turn_opt,
		       "Turns edge only if new edge length would be shorter"}]
    end;
menu(_, Menu) -> Menu.

command({edge,turn}, St0) ->
    {Sel, St} = wpa:sel_fold(
		  fun(Edges, We, {Sel,StAcc}) ->
			  {Sel1, StAcc0} = turn_edges(gb_sets:to_list(Edges), We, false, StAcc),
			  {[Sel1|Sel], StAcc0}
		  end, {[],St0}, St0),
    St#st{sel=Sel};
command({edge,turn_opt}, St0) ->
    {Sel, St} = wpa:sel_fold(
		  fun(Edges, We, {Sel, StAcc}) ->
			  {Sel1, StAcc0} = turn_edges(gb_sets:to_list(Edges), We, true, StAcc),
			  {[Sel1|Sel], StAcc0}
		  end, {[],St0}, St0),
    St#st{sel=Sel};
command(_Cmd, _) -> next.

turns() ->
    fun(B, _Ns) ->
	    turn_menu(B)
    end.

turn_menu(1) -> {edge,turn};
turn_menu(3) -> {edge,turn_opt};
turn_menu(help) -> turn_help().

turn_help() ->
    {"Turn edge", "", "Turns edge only if new edge length would be shorter"}.

%%
%% Edge turning
%%

turn_edges(Edges, #we{id=Id}=We0, Opt, St) ->
    {Sel0, We} = lists:foldl(
		   fun(Edge, {Sel0, WeAcc}) ->
			   {NewEdge, We1} = try_turn(Edge, WeAcc, Opt),
			   {[NewEdge|Sel0], We1}
		   end, {[],We0}, Edges),
    Shapes = gb_trees:enter(Id, We, St#st.shapes),
    Sel = {Id,gb_sets:from_list(Sel0)},
    {Sel, St#st{shapes=Shapes}}.

try_turn(Edge, #we{es=Etab,vp=Vtab}=We0, Opt) ->
    #edge{vs=Vstart,ve=Vend,rf=RightFace,lf=LeftFace} =
	gb_trees:get(Edge, Etab),
    LeftVs0 = wings_face:to_vertices([LeftFace], We0),
    RightVs0 = wings_face:to_vertices([RightFace], We0),
    LeftVs = lists:delete(Vstart, lists:delete(Vend, LeftVs0)),
    RightVs = lists:delete(Vstart, lists:delete(Vend, RightVs0)),
    case optimize(Vstart, Vend, LeftVs, RightVs, Opt, Vtab) of
	{Vert1, Vert2} ->
	    We1 = wings_edge:dissolve_edge(Edge, We0),
	    case gb_trees:is_defined(Edge, We1#we.es) of
                true -> {Edge, We0};
                false ->
                    Vs0 = gb_sets:from_list([Vert1, Vert2]),
                    We = wings_vertex_cmd:connect(Vs0, We1),
                    if 
                        We1#we.next_id =/= We#we.next_id ->
                            {We1#we.next_id, We};
                        true -> {Edge, We0}
                    end
	    end;
	_ ->
	    {Edge, We0}
    end.

optimize(Evs1, Evs2, VsList1, VsList2, Opt, Vtab) ->
    Dist1 = e3d_vec:dist(wings_vertex:pos(Evs1, Vtab), wings_vertex:pos(Evs2, Vtab)),
    V1 = hd(VsList1),
    V2 = hd(VsList2),
    Dist2 = e3d_vec:dist(wings_vertex:pos(V1, Vtab), wings_vertex:pos(V2, Vtab)),
    case Opt of 
        true when Dist2 < Dist1 -> {V1, V2};
        true -> none;
        false -> {V1, V2}
    end.
