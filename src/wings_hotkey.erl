%%
%%  wings_hotkey..erl --
%%
%%     This modules translates hotkeys.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_hotkey.erl,v 1.11 2002/01/22 09:57:23 bjorng Exp $
%%

-module(wings_hotkey).
-export([event/1]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(KL, wings_state).

event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=C}}=Key) ->
    Mods = modifiers(Mod, []),
    translate_key(Sym, Mods, C);
event(_) -> next.

translate_key(Sym, Mods, C) ->
    Key = case Mods of
	      [] when C =/= 0 -> {bindkey,C};
	      [shift] when C =/= 0 -> {bindkey,C};
	      Other -> {bindkey,Sym,Mods}
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

