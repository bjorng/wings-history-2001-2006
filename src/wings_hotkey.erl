%%
%%  wings_hotkey.erl --
%%
%%     This modules translates hotkeys.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_hotkey.erl,v 1.25 2002/05/16 07:10:51 bjorng Exp $
%%

-module(wings_hotkey).
-export([event/1,event/2,matching/1,bind_unicode/3,bind_virtual/4,
	 bind_from_event/2,delete_by_command/1,set_default/0]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,sort/1,foreach/2]).

-define(KL, wings_state).

%%%
%%% Hotkey lookup and translation.
%%%

event(Event) ->
    event_1(Event, none).

event(Event, #st{selmode=Mode}) ->
    event_1(Event, Mode).

event_1(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=C}}, SelMode) ->
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

bind_from_event(#keyboard{keysym=#keysym{sym=Sym}}, _Cmd)
  when Sym >= ?SDLK_NUMLOCK ->
    error;
bind_from_event(#keyboard{keysym=#keysym{sym=?SDLK_TAB,mod=Mod}}, Cmd) ->
    keyname(bind_virtual(?SDLK_TAB, modifiers(Mod), Cmd, user));
bind_from_event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=C}}, Cmd) ->
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
	[{Key,_,_}] -> ets:delete(?KL, Key);
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
    Spec0 = foldl(fun(N, A) -> {N,A} end, '$1', Names),
    Spec = [{{{bindkey,'$2'},Spec0,'_'},
	     [],
	     [{{'$1','$2'}}]}],
    [{Name,keyname(Key)} || {Name,Key} <- ets:select(?KL, Spec)] ++
	matching_mode(Spec0).

matching_mode({Mode,_}=Spec0) when Mode == shapes; Mode == vertex;
				   Mode == edge; Mode == face;
				   Mode == body ->
    Spec = [{{{bindkey,Mode,'$2'},Spec0,'_'},
	     [],
	     [{{'$1','$2'}}]}],
    [{Name,keyname(Key)} || {Name,Key} <- ets:select(?KL, Spec)];
matching_mode(_Other) -> [].

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
keyname(C) when $A =< C, C =< $Z -> "Shift+" ++ [C];
keyname(C) when is_integer(C) -> [C].

modname([ctrl|T]) -> "Ctrl+"++modname(T);
modname([shift|T]) -> "Shift+"++modname(T);
modname([alt|T]) -> "Alt+"++modname(T);
modname([]) -> [].

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
vkeyname(?SDLK_KP_PERIOD) -> ".";
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
      fun({{Key,List},Action}) when is_integer(Key) ->
	      ets:insert(wings_state, {{bindkey,{Key,sort(List)}},
				       Action,default});
	 ({Key,Action}) when is_integer(Key) ->
	      ets:insert(wings_state, {{bindkey,Key},
				       Action,default});
	 ({Mode,{Key,List},Action}) when is_integer(Key) ->
	      ets:insert(wings_state, {{bindkey,Mode,{Key,sort(List)}},
				       Action,default});
	 ({Mode,Key,Action}) when is_integer(Key) ->
	      ets:insert(wings_state, {{bindkey,Mode,Key},
				       Action,default})
      end, default_keybindings()).

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
     {{?SDLK_F3,[]},        {select,prev_edge_loop}},
     {{?SDLK_F4,[]},        {select,next_edge_loop}},
     {{?SDLK_F5,[]},        {select,{by,{faces_with,5}}}},
     {{?SDLK_TAB,[]},       {view,workmode}},
     {{?SDLK_TAB,[shift]},  {view,smoothed_preview}},
     {$\s,              {select,deselect}},
     {$a,               {view,aim}},
     {$A,               {view,frame}},
     {$b,               {select,body}},
     {$d,               {edit,repeat}},
     {$D,               {edit,repeat_drag}},
     {$e,               {select,edge}},

     {$f,               {select,face}},
     {$g,               {select,edge_ring}},
     {$i,               {select,similar}},
     {$l,               {select,edge_loop}},
     {$L,		{select,select_region}},
     {$o,               {view,orthogonal_view}},
     {$r,               {view,reset}},
     {$R,               {wings,reset}},
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

     {vertex,{?SDLK_DELETE,[]},	{vertex,collapse}},
     {edge,{?SDLK_DELETE,[]},	{edge,dissolve}},
     {face,{?SDLK_DELETE,[]},	{face,dissolve}},
     {body,{?SDLK_DELETE,[]},	{body,delete}},

     {face,$s,		{face,smooth}},
     {body,$s,		{body,smooth}}
    ].
