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
%%     $Id: wings_hotkey.erl,v 1.12 2002/01/25 09:04:36 bjorng Exp $
%%

-module(wings_hotkey).
-export([event/1,matching/1]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3]).

-define(KL, wings_state).

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
	[{_,Action}] -> Action;  	
	[] -> next
    end.
    
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

matching(Names) ->
    Spec0 = foldl(fun(N, A) -> {N,A} end, '$1', Names),
    Spec = [{{{bindkey,'$2'},Spec0},
	     [{'not',{is_tuple,'$1'}}],
	     [{{'$1','$2'}}]}],
    [{Name,keyname(Key)} || {Name,Key} <- ets:select(?KL, Spec)].

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

