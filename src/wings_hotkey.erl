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
%%     $Id: wings_hotkey.erl,v 1.5 2001/12/29 20:32:28 bjorng Exp $
%%

-module(wings_hotkey).
-export([event/1]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(INTERESTING_BITS, (?CTRL_BITS bor ?ALT_BITS)).

event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=C}}) ->
    translate_key(Sym, Mod, C);
event(_) -> next.
    
translate_key($c, Mod, C) when Mod band ?ALT_BITS =/= 0 -> {edit,copy_bb};
translate_key($v, Mod, C) when Mod band ?ALT_BITS =/= 0 -> {edit,paste_bb};
translate_key($a, Mod, C) when Mod band ?CTRL_BITS =/= 0 -> {select,all};
translate_key($i, Mod, C) when Mod band ?CTRL_BITS =/= 0,
				Mod band ?SHIFT_BITS =/= 0 -> {select,inverse};
translate_key($l, Mod, C) when Mod band ?CTRL_BITS =/= 0 -> {file,merge};
translate_key($n, Mod, C) when Mod band ?CTRL_BITS =/= 0 -> {file,new};
translate_key($o, Mod, C) when Mod band ?CTRL_BITS =/= 0 -> {file,open};
translate_key($q, Mod, C) when Mod band ?CTRL_BITS =/= 0 -> {file,quit};
translate_key($s, Mod, C) when Mod band ?SHIFT_BITS =/= 0,
			       Mod band ?CTRL_BITS =/= 0 ->
    {file,save_as};
translate_key($s, Mod, C) when Mod band ?CTRL_BITS =/= 0 -> {file,save};
translate_key($z, Mod, C) when Mod band ?ALT_BITS =/= 0,
			    Mod band ?CTRL_BITS =/= 0 -> {edit,undo};
translate_key($z, Mod, C) when Mod band ?SHIFT_BITS =/= 0,
			    Mod band ?CTRL_BITS =/= 0 -> {edit,redo};
translate_key($z, Mod, C) when Mod band ?CTRL_BITS =/= 0 -> {edit,undo_toggle};
translate_key(Sym, Mod, C) when Mod band ?INTERESTING_BITS == 0 ->
    case Sym of
	?SDLK_KP_PLUS -> {select,more};
	?SDLK_KP_MINUS -> {select,less};
	?SDLK_F3 -> {select,prev_edge_loop};
	?SDLK_F4 -> {select,next_edge_loop};
	?SDLK_F5 -> {select,{faces_with,5}};
	_ -> translate_key(C)
    end;
translate_key(_, _, _) -> next.

translate_key($\s) -> {select,deselect};
translate_key($a) -> {view,aim};
translate_key($b) -> {select,body};
translate_key($d) -> {edit,repeat};
translate_key($e) -> {select,edge};
translate_key($f) -> {select,face};
translate_key($i) -> {select,similar};
translate_key($l) -> {select,edge_loop};
translate_key($o) -> {view,orthogonal_view};
translate_key($r) -> {view,reset};
translate_key($s) -> {body,auto_smooth};
translate_key($u) -> {view,auto_rotate};
translate_key($v) -> {select,vertex};
translate_key($w) -> {view,wire_mode};
translate_key($x) -> {view,{along,x}};
translate_key($y) -> {view,{along,y}};
translate_key($z) -> {view,{along,z}};
translate_key($X) -> {view,{along,neg_x}};
translate_key($Y) -> {view,{along,neg_y}};
translate_key($Z) -> {view,{along,neg_z}};
translate_key($2) -> {edge,{cut,2}};
translate_key($3) -> {edge,{cut,3}};
translate_key($4) -> {edge,{cut,4}};
translate_key($5) -> {edge,{cut,5}};
translate_key($6) -> {edge,{cut,6}};
translate_key($7) -> {edge,{cut,7}};
translate_key($8) -> {edge,{cut,8}};
translate_key($9) -> {edge,{cut,9}};
translate_key($0) -> {edge,{cut,10}};
translate_key(?SDLK_TAB)  -> {view,smooth_preview};
translate_key($+)  -> {select,more};
translate_key($=)  -> {select,more};
translate_key($-)  -> {select,less};
translate_key(_) -> next.
