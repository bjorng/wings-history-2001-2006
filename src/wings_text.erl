%%
%%  wings_text.erl --
%%
%%     Text and font support.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_text.erl,v 1.21 2004/02/29 17:56:36 bjorng Exp $
%%

-module(wings_text).
-export([init/0,width/0,width/1,height/0,draw/1,char/1]).
-export([break_lines/2]).
-export([font_module/1,choose_font/0,fonts/0]).

-define(NEED_ESDL, 1).
-include("wings.hrl").
-compile({parse_transform,ms_transform}).

-import(lists, [reverse/1]).

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

fonts() ->
    MatchSpec = ets:fun2ms(fun({Font,Desc}) -> {Desc,Font} end),
    ets:select(wings_fonts, MatchSpec).

break_lines(Lines, Limit) ->
    break_lines(Lines, Limit, 0, []).

break_lines([S|T], Limit, Rows, Acc) ->
    break_line(S, T, Limit, Rows, Acc);
break_lines([], _, Rows, Lines) ->
    {Rows,reverse(Lines)}.

break_line(S, T, Limit, Rows, Acc) ->
    case break_line_1(S, Limit) of
	done when T =/= [] ->
	    break_lines(T, Limit, Rows+1, [[]|Acc]);
	done ->
	    break_lines(T, Limit, Rows, Acc);
	{Line,More} ->
	    break_line(More, T, Limit, Rows+1, [Line|Acc])
    end.

break_line_1([$\n|T], Limit) -> break_line_1(T, Limit);
break_line_1([$\s|T], Limit) -> break_line_1(T, Limit);
break_line_1([], _) -> done;
break_line_1(T, Limit) -> break_line_2(T, 0, Limit, [], []).

break_line_2(_, N, Limit, _Acc, {Bef,More}) when N > Limit ->
    {reverse(Bef),More};
break_line_2([$\n|T], _N, _Limit, Acc, _Break) ->
    {reverse(Acc),T};
break_line_2([$\s|T0], N, Limit, Acc, _Break) ->
    T = skip_blanks(T0),
    break_line_2(T, N+1, Limit, [$\s|Acc], {Acc,T});
break_line_2([{_,Str}=C|T], N, Limit, Acc, Break) ->
    break_line_2(T, N+length(Str), Limit, [C|Acc], Break);
break_line_2([C|T], N, Limit, Acc, Break) ->
    break_line_2(T, N+1, Limit, [C|Acc], Break);
break_line_2([], _, _Limit, Acc, _Break) -> {reverse(Acc),[]}.

skip_blanks([$\n|T]) -> skip_blanks(T);
skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks(T) -> T.
