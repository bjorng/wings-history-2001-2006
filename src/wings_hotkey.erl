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
%%     $Id: wings_hotkey.erl,v 1.9 2002/01/18 16:40:59 dgud Exp $
%%

-module(wings_hotkey).
-export([event/1]).
-compile(export_all).


-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(KL, wings_state).

event(Key = #keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=C}}) ->
    Mods = modifiers(Mod, []),
%%    io:format("Pressed Key ~p with ~p~n", [{Sym, C, Mod}, Mods]),
    Res = translate_key(Sym, Mods, C),
%%    io:format("Action ~p ~n", [Res]),
    Res;

event(_) -> next.

translate_key(Sym, [], C) ->
    case ets:lookup(?KL, {bindkey, Sym}) of
	[{_, Action}] ->
	    Action;  	
	[] ->
	    case ets:lookup(?KL, {bindkey, C}) of
		[{_, Action}] ->
		    Action;
		[] ->
		    next;
		Else -> 
		    erlang:fault({?MODULE, ?LINE, Else})
	    end;
	Else -> 
	    erlang:fault({?MODULE, ?LINE, Else})
    end;

translate_key(Sym, Mods, C) ->
    case ets:lookup(?KL, {bindkey, Sym, Mods}) of
	[] ->
	    next;
	[{_, Action}] ->
	    Action;
	Else -> 
	    erlang:fault({?MODULE, ?LINE, Else})
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
modifiers(_, Acc) ->
    lists:sort(Acc).

