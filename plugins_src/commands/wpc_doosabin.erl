%%
%%  wpc_DooSabin.erl --
%%
%%     Plug-in for Doo-Sabin subdivision according to Wasamonkey.
%%
%%  Copyright (c) 2005 Dan Gudmundsson, Wasamonkey
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_doosabin.erl,v 1.1 2005/06/03 14:56:53 dgud Exp $
%%

-module(wpc_doosabin).

-export([init/0,menu/2,command/2]).

-include_lib("wings.hrl").

init() ->
    init_pref(),
    true.

menu({body}, Menu) ->
    case is_enabled() of
	true -> 
	    add_menu(Menu);
	false -> Menu
    end;
menu({edit,plugin_preferences}, Menu) ->
    Menu ++ [{"Doo Sabin subd",doo_sabin}];
menu(_, Menu) -> Menu.

add_menu([Smooth = {_,smooth,_}|Rest]) ->
    [Smooth, 
     {"DS subdivision", doo_sabin, 
      "Makes a Doo-Sabin subdivision according to WasaMonkey"}|Rest];
add_menu([Other|Rest]) ->
    [Other| add_menu(Rest)];
add_menu([]) -> %% Just in case we end up here..
    [{"DS subdivision", doo_sabin, 
      "Makes a Doo-Sabin subdivision according to WasaMonkey"}].

command({body,doo_sabin}, St0) ->
    %% Do For each selected object 
    wpa:sel_fold(fun(_,We,St) -> doo_sabin(We,St) end, 
		 St0#st{sel=[],selmode=edge}, St0);
command({edit,{plugin_preferences,doo_sabin}}, St) ->
    pref_edit(St);
command(_Cmd, _) -> next.

doo_sabin(We0 = #we{es=Etab0, id=Id}, St0 = #st{shapes=Sh0,sel=OrigSel}) ->
    OrigEdges = gb_trees:keys(Etab0),
    %% Set all edges hard. 
    We1 = We0#we{he=gb_sets:from_ordset(OrigEdges)},
    We2 = #we{es=Etab1} = wings_subdiv:smooth(We1),
    SubdEdges = gb_trees:keys(Etab1),
    We3 = wings_subdiv:smooth(We2),
    %% Set selection to the subd-edges and update the object we are subd'ing
    %%
    %% I do the dissolving (not like Wasamonkey) in two steps to be able to
    %% select the correct edges to return which can be scalade..
    
    %% First find the Original Edge loops
    St1 = St0#st{sel=[{Id,gb_sets:from_ordset(OrigEdges)}],
		 shapes=gb_trees:update(Id,We3,Sh0)},
    #st{sel=[{Id,DissolveEds1}]} = wings_edge_loop:select_loop(St1),
    %% Dissolve Edges..
    We4 = wings_edge:dissolve_edges(DissolveEds1, We3),
    %% Find and dissolve isolated vertices.
    IsolatedVs1 = wings_vertex:isolated(We4),
    We5 = wings_edge:dissolve_isolated_vs(IsolatedVs1, We4),
    %% Step 2
    %% Find the SubdEdges loops
    CurrEds = gb_sets:from_ordset(gb_trees:keys(We5#we.es)),
    SubdEdges2 = gb_sets:intersection(gb_sets:from_ordset(SubdEdges),CurrEds),
     St2 = St0#st{sel=[{Id,SubdEdges2}],
		 shapes=gb_trees:update(Id,We5,Sh0)},
    #st{sel=[{Id,DissolveEds2}]} = wings_edge_loop:select_loop(St2),
    %% Find the faces of these edges.
    CurrFaces = wings_face:from_edges(DissolveEds2,We5),
    ScaleFaces = wings_sel:inverse_items(face, CurrFaces, We5),
    %% Dissolve Edges..
    We6 = wings_edge:dissolve_edges(DissolveEds2, We5),
    %% Find and dissolve isolated vertices.
    IsolatedVs2 = wings_vertex:isolated(We6),
    We7 =#we{fs=Ftab}= wings_edge:dissolve_isolated_vs(IsolatedVs2, We6),
    %% Fix the selection
    Fs = gb_sets:from_ordset(gb_trees:keys(Ftab)),
    CornerEds = wings_edge:from_faces(gb_sets:intersection(ScaleFaces,Fs),We7),
    %% Return the result
    St1#st{sel=[{Id,CornerEds}|OrigSel],
	   shapes=gb_trees:update(Id,We7,Sh0)}.

%%%
%%% Preference support.
%%%

pref_edit(St) ->
    Enabled = get_pref(enabled, false),
    wpa:dialog("Doo Sabin Preferences",
	       [{hframe,[{"Enabled",Enabled,[{key,enabled}]}]}],
	       fun(Attr) -> pref_result(Attr, St) end).

pref_result(Attr, St) ->
    set_pref(Attr),
    init_pref(),
    St.

set_pref(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

init_pref() ->
    Enabled = get_pref(enabled, false),
    put({?MODULE,enabled}, Enabled),
    ok.

is_enabled() -> get({?MODULE,enabled}).