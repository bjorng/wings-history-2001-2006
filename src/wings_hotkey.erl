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
%%     $Id: wings_hotkey.erl,v 1.13 2002/01/26 11:24:51 bjorng Exp $
%%

-module(wings_hotkey).
-export([event/1,matching/1,bind_unicode/3,bind_virtual/4,
	 bind_from_event/2,delete_by_command/1,set_default/0]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,sort/1,foreach/2]).

-define(KL, wings_state).

%%%
%%% Hotkey lookup and translation.
%%%

event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=C}}=Key) ->
    Mods = modifiers(Mod, []),
    translate_key(Sym, Mods, C);
event(_) -> next.

translate_key(Sym, Mods, C) ->
    Key = case Mods of
	      [] when C =/= 0 -> {bindkey,C};
	      [shift] when C =/= 0 -> {bindkey,C};
	      Other -> {bindkey,{Sym,Mods}}
	  end,
    case ets:lookup(?KL, Key) of
	[{_,Action,_}] -> Action;
	[] -> next
    end.

%%%
%%% Binding and unbinding of keys.
%%%

bind_from_event(#keyboard{keysym=#keysym{sym=Sym}}, Cmd)
  when Sym >= ?SDLK_NUMLOCK ->
    error;
bind_from_event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=C}}, Cmd) ->
    Bkey = case modifiers(Mod, []) of
	       [] when C =/= 0 -> bind_unicode(C, Cmd, user);
	       [shift] when C =/= 0 -> bind_unicode(C, Cmd, user);
	       Mods -> bind_virtual(Sym, Mods, Cmd, user)
	   end,
    keyname(Bkey);
bind_from_event(_, Cmd) -> error.

delete_by_command(Cmd) ->
    case sort(ets:match_object(?KL, {{bindkey,'_'},Cmd,'_'})) of
	[{Key,_,_}] -> ets:delete(?KL, Key);
	[{Key,_,_},{Next,_,_}|_] ->
	    ets:delete(?KL, Key),
	    keyname(Next);
	[] -> []
    end.
    
bind_unicode(Key, Cmd, Source) ->
    Bkey = {bindkey,Key},
    ets:insert(?KL, {Bkey,Cmd,Source}),
    Bkey.

bind_virtual(Key, Mods, Cmd, Source) ->
    Bkey = {bindkey,{Key,sort(Mods)}},
    ets:insert(?KL, {Bkey,Cmd,Source}),
    Bkey.

matching(Names) ->
    Spec0 = foldl(fun(N, A) -> {N,A} end, '$1', Names),
    Spec = [{{{bindkey,'$2'},Spec0,'_'},
	     [{'not',{is_tuple,'$1'}}],
	     [{{'$1','$2'}}]}],
    [{Name,keyname(Key)} || {Name,Key} <- ets:select(?KL, Spec)].

%%%
%%% Local functions.
%%%

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
keyname({C,Mods}) ->
    modname(Mods) ++ vkeyname(C);
keyname($\t) -> "Tab";
keyname($\s) -> "Space";
keyname(C) when is_integer(C) -> [C].

modname([ctrl|T]) -> "Ctrl-"++modname(T);
modname([shift|T]) -> "Shift-"++modname(T);
modname([alt|T]) -> "Shift-"++modname(T);
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
vkeyname(C) when ?SDLK_F1 < C, C < ?SDLK_F15 ->
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
      fun({{Key,List},Action}) ->
	      ets:insert(wings_state, {{bindkey,{Key,sort(List)}},
				       Action,default});
	 ({Key,Action}) ->
	      ets:insert(wings_state, {{bindkey,Key},
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
     {$\t,                 {view,smooth_preview}},
     {$\s,                 {select,deselect}},
     {$a,                  {view,aim}},
     {$b,                  {select,body}},
     {$d,                  {edit,repeat}},
     {$e,                  {select,edge}},
     {$f,                  {select,face}},
     {$i,                  {select,similar}},
     {$l,                  {select,edge_loop}},
     {$o,                  {view,orthogonal_view}},
     {$r,                  {view,reset}},
     {$R,                  {select,edge_ring}},
     {$s,                  {body,auto_smooth}},
     {$u,                  {view,auto_rotate}},
     {$v,                  {select,vertex}},
     {$w,                  {view,wire_mode}},
     {$x,                  {view,{along,x}}},
     {$y,                  {view,{along,y}}},
     {$z,                  {view,{along,z}}},
     {$X,                  {view,{along,neg_x}}},
     {$Y,                  {view,{along,neg_y}}},
     {$Z,                  {view,{along,neg_z}}},
     {$2,                  {edge,{cut,2}}},
     {$3,                  {edge,{cut,3}}},
     {$4,                  {edge,{cut,4}}},
     {$5,                  {edge,{cut,5}}},
     {$6,                  {edge,{cut,6}}},
     {$7,                  {edge,{cut,7}}},
     {$8,                  {edge,{cut,8}}},
     {$9,                  {edge,{cut,9}}},
     {$0,                  {edge,{cut,10}}},
     {$+,                  {select,more}},
     {$=,                  {select,more}},
     {$-,                  {select,less}}
    ].
