%%
%%  wings_text.erl --
%%
%%     This module contains an fixed-width font. It will not
%%     be needed when Wings is migrated to GTK.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_text.erl,v 1.16 2002/12/29 07:19:17 bjorng Exp $
%%

-module(wings_text).
-export([init/0,width/0,width/1,height/0,draw/1,char/1]).
-export([font_module/1,choose_font/0]).
-export([sub_menu/1,command/2]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-compile({parse_transform,ms_transform}).

init() ->
    ets:new(wings_fonts, [named_table,ordered_set]),
    wings_pref:set_default(system_font, wpf_7x14).

font_module(Mod) ->
    Desc = Mod:desc(),
    ets:insert(wings_fonts, {Mod,Desc}).

choose_font() ->
    Font0 = wings_pref:get_value(system_font),
    Font = case ets:member(wings_fonts, Font0) of
	       true -> Font0;
	       false -> ets:first(wings_fonts)
	   end,
    put(?MODULE, Font).

width(S) -> (get(?MODULE)):width(S).

width() -> (get(?MODULE)):width().

height() -> (get(?MODULE)):height().

draw(S) ->
    (get(?MODULE)):draw(S).

char(C) ->
    (get(?MODULE)):char(C).

sub_menu(_St) ->
    [{"Font...",font}].

command(font, _St) ->
    Def = {font,get(?MODULE)},
    Qs = [{vframe,{alt,fonts(),Def}}],
    wings_ask:dialog("Choose Font", Qs,
		     fun([Font]) ->
			     wings_pref:set_value(system_font, Font),
			     {W,H} = wings_pref:get_value(window_size),
			     wings_io:putback_event(#resize{w=W,h=H}),
			     put(?MODULE, Font),
			     ignore
		     end).

fonts() ->
    MatchSpec = ets:fun2ms(fun({Font,Desc}) -> {Desc,Font} end),
    ets:select(wings_fonts, MatchSpec).
