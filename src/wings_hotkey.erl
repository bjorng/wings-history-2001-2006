%%
%%  wings_hotkey.erl --
%%
%%     This modules translates hotkeys.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_hotkey.erl,v 1.42 2003/07/28 17:38:48 bjorng Exp $
%%

-module(wings_hotkey).
-export([event/1,event/2,matching/1,bind_unicode/3,bind_virtual/4,
	 bind_from_event/2,delete_by_command/1,set_default/0,
	 listing/0]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,sort/1,foreach/2,map/2,last/1,reverse/1]).

-compile({parse_transform,ms_transform}).

-define(KL, wings_state).

%%%
%%% Hotkey lookup and translation.
%%%

event(Event) ->
    event_1(Event, none).

event(Event, #st{sel=[]}) ->
    event_1(Event, none);
event(Event, #st{selmode=Mode}=St) ->
    case wings_light:is_any_light_selected(St) of
	true -> event_1(Event, light);
	false -> event_1(Event, Mode)
    end.

event_1(#keyboard{sym=Sym,mod=Mod,unicode=C}, SelMode) ->
    Mods = modifiers(Mod),
    Key = case Mods of
	      _ when Sym == ?SDLK_TAB -> {Sym,Mods};
 	      [] when C =/= 0 -> fix_bksp_and_del(Sym, C);
	      [shift] when C =/= 0 -> C;
	      _Other -> {Sym,Mods}
	  end,
    case lookup(Key, SelMode) of
	next -> lookup(Key, none);
	Action -> Action
    end;
event_1(_, _) -> next.

lookup(Key, none) ->
    case ets:lookup(?KL, {bindkey,Key}) of
	[{_,Action,_}] -> Action;
	[] -> next
    end;
lookup(Key, SelMode) ->
    case ets:lookup(?KL, {bindkey,SelMode,Key}) of
	[{_,Action,_}] -> Action;
	[] -> next
    end.

%%%
%%% Binding and unbinding of keys.
%%%

bind_from_event(#keyboard{sym=Sym}, _Cmd) when Sym >= ?SDLK_NUMLOCK ->
    error;
bind_from_event(#keyboard{sym=?SDLK_TAB,mod=Mod}, Cmd) ->
    keyname(bind_virtual(?SDLK_TAB, modifiers(Mod), Cmd, user));
bind_from_event(#keyboard{sym=Sym,mod=Mod,unicode=C}, Cmd) ->
    Bkey = case modifiers(Mod) of
 	       [] when C =/= 0 ->
		   bind_unicode(fix_bksp_and_del(Sym, C), Cmd, user);
	       [shift] when C =/= 0 ->
		   bind_unicode(C, Cmd, user);
	       Mods ->
		   bind_virtual(Sym, Mods, Cmd, user)
	   end,
    keyname(Bkey);
bind_from_event(_, _) -> error.

delete_by_command(Cmd) ->
    case sort(ets:match_object(?KL, {'_',Cmd,'_'})) of
	[{Key,_,_}] ->
	    ets:delete(?KL, Key),
	    [];
	[{Key,_,_},{Next,_,_}|_] ->
	    ets:delete(?KL, Key),
	    keyname(Next);
	[] -> []
    end.
    
bind_unicode(Key, Cmd, Source) ->
    Bkey = bkey(Key, Cmd),
    ets:insert(?KL, {Bkey,Cmd,Source}),
    Bkey.

bind_virtual(Key, Mods, Cmd, Source) ->
    Bkey = bkey({Key,sort(Mods)}, Cmd),
    ets:insert(?KL, {Bkey,Cmd,Source}),
    Bkey.

bkey(Key, {Mode,_}) when Mode == shapes; Mode == vertex; Mode == edge;
			 Mode == face; Mode == body ->
    {bindkey,Mode,Key};
bkey(Key, _Cmd) ->
    {bindkey,Key}.
    
matching(Names) ->
    M0 = matching_global(Names) ++ matching_mode(Names),
    M = wings_util:rel2fam(M0),
    [{Name,Key} || {Name,[{_,Key}|_]} <- M].

matching_global(Names) ->
    Spec0 = foldl(fun(N, A) -> {N,A} end, '$1', Names),
    Spec = [{{{bindkey,'$2'},Spec0,'$3'},
	     [],
	     [{{'$1',{{'$3','$2'}}}}]}],
    [{Name,mkeyname(Key)} || {Name,Key} <- ets:select(?KL, Spec)].

matching_mode(Names) ->
    Mode = lists:last(Names),
    case suitable_mode(Mode) of
	false -> [];
	true ->
	    Spec0 = foldl(fun(N, A) -> {N,A} end, '$1', Names),
	    Spec = [{{{bindkey,Mode,'$2'},Spec0,'$3'},
		     [],
		     [{{'$1',{{'$3','$2'}}}}]}],
	    [{Name,mkeyname(Key)} || {Name,Key} <- ets:select(?KL, Spec)]
    end.

mkeyname({user,K}) -> {1,keyname(K)};
mkeyname({default,K}) -> {2,keyname(K)};
mkeyname({plugin,K}) -> {3,keyname(K)}.

suitable_mode(shapes) -> true;
suitable_mode(vertex) -> true;
suitable_mode(edge) -> true;
suitable_mode(face) -> true;
suitable_mode(body) -> true;
suitable_mode(_) -> false.

%%%
%%% Make a listing of all hotkeys.
%%%

listing() ->
    MatchSpec = ets:fun2ms(fun({{bindkey,K},Cmd,Src}) ->
				   {all,{{bindkey,K},Cmd,Src}};
			      ({{bindkey,Mode,K},Cmd,Src}) ->
				   {Mode,{{bindkey,K},Cmd,Src}}
			   end),
    Keys = wings_util:rel2fam(ets:select(?KL, MatchSpec)),
    listing_1(Keys, []).

listing_1([{Mode,Keys}|T], Acc0) ->
    Acc = [list_keys(Keys),list_header(Mode)|Acc0],
    listing_1(T, Acc);
listing_1([], Acc) -> reverse(Acc).

list_header(all) -> "Hotkeys in all modes";
list_header(body) -> "Hotkeys in object mode";
list_header(edge) -> "Hotkeys in edge mode";
list_header(face) -> "Hotkeys in face mode";
list_header(light) -> "Hotkeys for lights";
list_header(vertex) -> "Hotkeys for vertices";
list_header(A) -> atom_to_list(A).

list_keys([{Key,Cmd,Src}|T]) ->
    KeyStr = keyname(Key),
    SrcStr = case Src of
		 default -> "";
		 user -> " (user-defined)";
		 plugin -> " (plug-in-defined)"
	     end,
    KeyStr ++ ": " ++ wings_util:stringify(Cmd) ++ SrcStr ++ 
	"\n" ++ list_keys(T);
list_keys([]) -> [].

%%%
%%% Local functions.
%%%

%% For the benefit of Mac OS X, but does no harm on other platforms.
fix_bksp_and_del(?SDLK_DELETE, _) -> ?SDLK_DELETE;
fix_bksp_and_del(?SDLK_BACKSPACE, _) -> ?SDLK_BACKSPACE;
fix_bksp_and_del(_, C) -> C.

modifiers(Mod) ->
    modifiers(Mod, []).
modifiers(Mod, Acc) when Mod band ?CTRL_BITS =/= 0 ->
    Pressed = Mod band ?CTRL_BITS,
    modifiers(Mod bxor Pressed, [ctrl|Acc]);
modifiers(Mod, Acc) when Mod band ?ALT_BITS =/= 0 ->
    Pressed = Mod band ?ALT_BITS,
    modifiers(Mod bxor Pressed, [alt|Acc]);
modifiers(Mod, Acc) when Mod band ?SHIFT_BITS =/= 0 ->
    Pressed = Mod band ?SHIFT_BITS,
    modifiers(Mod bxor Pressed, [shift|Acc]);
modifiers(Mod, Acc) when Mod band ?KMOD_META =/= 0 ->
    Pressed = Mod band ?KMOD_META,
    modifiers(Mod bxor Pressed, [command|Acc]);
modifiers(_, Acc) -> lists:sort(Acc).

keyname({bindkey,Key}) ->
    keyname(Key);
keyname({bindkey,_Mode,Key}) ->
    keyname(Key);
keyname({C,Mods}) ->
    modname(Mods) ++ vkeyname(C);
keyname($\b) -> "Bksp";
keyname($\t) -> "Tab";
keyname($\s) -> "Space";
keyname(C) when $a =< C, C =< $z -> [C-32];
keyname(C) when $A =< C, C =< $Z ->
    case get(wings_os_type) of
	{unix,darwin} -> [shift,C];
	_ -> "Shift+" ++ [C]
    end;
keyname(C) when is_integer(C), C < 256 -> [C];
keyname(C) when is_integer(C), 63236 =< C, C =< 63247 ->
    [$F|integer_to_list(C-63235)];
keyname(C) -> [$<|integer_to_list(C)++">"].

modname(Mods) ->
    case get(wings_os_type) of
	{unix,darwin} -> mac_modname(Mods, []);
	_ -> modname_1(Mods)
    end.

modname_1([ctrl|T]) -> "Ctrl+"++modname_1(T);
modname_1([shift|T]) -> "Shift+"++modname_1(T);
modname_1([alt|T]) -> "Alt+"++modname_1(T);
modname_1([]) -> [].

mac_modname([ctrl|T], Acc) ->
    [$^|mac_modname(T, Acc)];
mac_modname([shift|T], Acc) -> mac_modname(T, [shift|Acc]);
mac_modname([alt|T], Acc) -> mac_modname(T, [option|Acc]);
mac_modname([command|T], Acc) -> mac_modname(T, Acc++[command]);
mac_modname([], Acc) -> Acc.

vkeyname(?SDLK_BACKSPACE) -> "Bksp";
vkeyname(?SDLK_TAB) -> "Tab";
vkeyname(?SDLK_RETURN) -> "Enter";
vkeyname(?SDLK_PAUSE) -> "Pause";
vkeyname(?SDLK_ESCAPE) -> "Esc";
vkeyname(?SDLK_SPACE) -> "Space";
vkeyname(?SDLK_DELETE) -> "Delete";
vkeyname(C) when $a =< C, C =< $z-> [C-32];
vkeyname(C) when $\s < C, C < 256 -> [C];
vkeyname(C) when ?SDLK_KP0 < C, C < ?SDLK_KP9 -> [C-?SDLK_KP0+$0];
vkeyname(C) when ?SDLK_F1 =< C, C =< ?SDLK_F15 ->
    [$F|integer_to_list(C-?SDLK_F1+1)];
vkeyname(?SDLK_KP_PERIOD) -> "Del";
vkeyname(?SDLK_KP_DIVIDE) -> "Div";
vkeyname(?SDLK_KP_MULTIPLY) -> "Mul";
vkeyname(?SDLK_KP_MINUS) -> "-";
vkeyname(?SDLK_KP_PLUS) -> "+";
vkeyname(?SDLK_KP_ENTER) -> "Enter";
vkeyname(?SDLK_KP_EQUALS) -> "=";
vkeyname(?SDLK_UP) -> "Up";
vkeyname(?SDLK_DOWN) -> "Down";
vkeyname(?SDLK_RIGHT) -> "Right";
vkeyname(?SDLK_LEFT) -> "Left";
vkeyname(?SDLK_INSERT) -> "Insert";
vkeyname(?SDLK_HOME) -> "Home";
vkeyname(?SDLK_END) -> "End";
vkeyname(?SDLK_PAGEUP) -> "Page Up";
vkeyname(?SDLK_PAGEDOWN) -> "Page Down";
vkeyname(_) -> "UKEY".

%%%
%%% Default keybindings.
%%%

set_default() ->
    foreach(
      fun({{Key,List0},Action}) when is_integer(Key) ->
	      List = convert_modifiers(List0),
	      ets:insert(wings_state, {{bindkey,{Key,sort(List)}},
				       Action,default});
	 ({Key,Action}) when is_integer(Key) ->
	      ets:insert(wings_state, {{bindkey,Key},
				       Action,default});
	 ({Mode,{Key,List0},Action}) when is_integer(Key) ->
	      List = convert_modifiers(List0),
	      ets:insert(wings_state, {{bindkey,Mode,{Key,sort(List)}},
				       Action,default});
	 ({Mode,Key,Action}) when is_integer(Key) ->
	      ets:insert(wings_state, {{bindkey,Mode,Key},
				       Action,default})
      end, default_keybindings()).

convert_modifiers(Mod) ->
    case get(wings_os_type) of
	{unix,darwin} ->
	    map(fun(ctrl) -> command;
		   (Other) -> Other end, Mod);
	_ -> Mod
    end.

default_keybindings() ->
    [{{$a,[ctrl]},          {select,all}},
     {{$i,[ctrl,shift]},    {select,inverse}},
     {{$l,[ctrl]},          {file,merge}},
     {{$n,[ctrl]},          {file,new}},
     {{$o,[ctrl]},          {file,open}},
     {{$q,[ctrl]},          {file,quit}},
     {{$s,[ctrl,shift]},    {file,save_as}},
     {{$s,[ctrl]},          {file,save}},
     {{$z,[alt,ctrl]},      {edit,undo}},
     {{$z,[ctrl,shift]},    {edit,redo}},
     {{$z,[ctrl]},          {edit,undo_toggle}},
     {{?SDLK_KP_PLUS,[]},   {select,more}},
     {{?SDLK_KP_MINUS,[]},  {select,less}},
     {{?SDLK_F3,[]},        {select,{edge_loop,prev_edge_loop}}},
     {{?SDLK_F4,[]},        {select,{edge_loop,next_edge_loop}}},
     {{?SDLK_F5,[]},        {select,{by,{faces_with,5}}}},
     {{?SDLK_TAB,[]},       {view,workmode}},
     {{?SDLK_TAB,[shift]},  {view,quick_preview}},
     {$\s,              {select,deselect}},
     {$a,               {view,aim}},
     {$A,               {view,frame}},
     {$b,               {select,body}},
     {$d,               {edit,repeat}},
     {$D,               {edit,repeat_drag}},
     {$e,               {select,edge}},

     {$f,               {select,face}},
     {$g,               {select,{edge_loop,edge_ring}}},
     {{$g,[alt]},       {select,{edge_loop,edge_ring_incr}}},
     {{$g,[alt,ctrl]},  {select,{edge_loop,edge_ring_decr}}},
     {$i,               {select,similar}},
     {$l,               {select,{edge_loop,edge_loop}}},
     {$L,		{select,{edge_loop,edge_loop_to_region}}},
     {{$l,[alt]},       {select,{edge_loop,edge_link_incr}}},
     {{$l,[alt,ctrl]},  {select,{edge_loop,edge_link_decr}}},
     {$o,               {view,orthogonal_view}},
     {$r,               {view,reset}},
     {$u,               {view,auto_rotate}},
     {$v,               {select,vertex}},
     {$w,               {view,toggle_wireframe}},
     {$x,               {view,{along,x}}},
     {$y,               {view,{along,y}}},
     {$z,               {view,{along,z}}},
     {$X,               {view,{along,neg_x}}},
     {$Y,               {view,{along,neg_y}}},
     {$Z,               {view,{along,neg_z}}},
     {$+,               {select,more}},
     {$=,               {select,more}},
     {$-,               {select,less}},

     %% Mode-specific bindings.
     {edge,$2,		{edge,{cut,2}}},
     {edge,$3,		{edge,{cut,3}}},
     {edge,$4,		{edge,{cut,4}}},
     {edge,$5,		{edge,{cut,5}}},
     {edge,$6,		{edge,{cut,6}}},
     {edge,$7,		{edge,{cut,7}}},
     {edge,$8,		{edge,{cut,8}}},
     {edge,$9,		{edge,{cut,9}}},
     {edge,$0,		{edge,{cut,10}}},

     {vertex,$c,	{vertex,connect}},
     {edge,$c,		{edge,connect}},
      
     {vertex,$\b,	{vertex,collapse}},
     {edge,$\b,		{edge,dissolve}},
     {face,$\b,		{face,dissolve}},
     {body,$\b,		{body,delete}},

     {vertex,{?SDLK_DELETE,[]},	{vertex,dissolve}},
     {edge,{?SDLK_DELETE,[]},	{edge,dissolve}},
     {face,{?SDLK_DELETE,[]},	{face,dissolve}},
     {body,{?SDLK_DELETE,[]},	{body,delete}},

     {vertex,{?SDLK_KP_PERIOD,[]},{vertex,dissolve}},
     {edge,{?SDLK_KP_PERIOD,[]},  {edge,dissolve}},
     {face,{?SDLK_KP_PERIOD,[]},  {face,dissolve}},
     {body,{?SDLK_KP_PERIOD,[]},  {body,delete}},

     {light,$\b,	{light,delete}},
     {light,{?SDLK_DELETE,[]},	{light,delete}},
     {light,{?SDLK_KP_PERIOD,[]},  {light,delete}},

     {face,$s,		{face,smooth}},
     {body,$s,		{body,smooth}}
    ].
