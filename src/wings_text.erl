%%
%%  wings_text.erl --
%%
%%     Text and font support.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_text.erl,v 1.24 2004/04/07 17:39:35 bjorng Exp $
%%

-module(wings_text).
-export([init/0,resize/0,width/0,width/1,height/0,draw/1,char/1,bold/1]).
-export([break_lines/2]).
-export([font_module/1,choose_font/0,fonts/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-compile({parse_transform,ms_transform}).

-import(lists, [reverse/1,foldl/3]).

init() ->
    ets:new(wings_fonts, [named_table,ordered_set]),
    wings_pref:set_default(system_font, wpf_7x14).

resize() ->
    Base = gl:genLists(256),
    make_font_dlists(0, Base),
    gl:listBase(Base).

make_font_dlists(256, _) -> ok;
make_font_dlists(C, Base) ->
    gl:newList(Base+C, ?GL_COMPILE),
    catch char(C),
    gl:endList(),
    make_font_dlists(C+1, Base).

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

width(S) ->
    Mod = get(?MODULE),
    CwFun = case width() of
		W when W < 7 -> fun cw_small/1;
		_ -> fun cw_large/1
	    end,
    WF0 = Mod:width_fun(),
    WF = fun(C, W) when is_atom(C) ->
		 W+CwFun(C);
	    (C, W) ->
		 W+WF0(C)
	 end,
    foldl(WF, 0, S).

width() -> (get(?MODULE)):width().

height() -> (get(?MODULE)):height().

draw(S) ->
    case wings_pref:get_value(text_display_lists, false) of
	true -> gl:callLists(length(S), ?GL_UNSIGNED_BYTE, S);
	false -> (get(?MODULE)):draw(S)
    end.

char(C) when is_atom(C) ->
    special(C);
char(C) ->
    (get(?MODULE)):char(C).

bold(S) ->
    (get(?MODULE)):bold(S).

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

%%%
%%% Special characters.
%%%

cw_small(option_box) -> 6;
cw_small(command) -> 8;
cw_small(folder) -> 12;
cw_small(option) -> 12;
cw_small(shift) -> 13;
cw_small(crossmark) -> 8.

cw_large(option_box) -> 7;
cw_large(command) -> 8;
cw_large(folder) -> 14;
cw_large(option) -> 14;
cw_large(shift) -> 14;
cw_large(crossmark) -> 8.

special(C) ->
    case width() of
	W when W < 7 ->
	    special_small(C);
	_ ->
	    special_large(C)
    end.

special_small(option_box) ->
    B = <<
	 2#11111100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#11111100,
	 2#11111100
	 >>,
    gl:bitmap(6, 8, 0, 3, 6, 0, B);

special_small(command) ->
    B = <<
       	 2#01000100,
       	 2#10101010,
       	 2#10101010,
	 2#01111100,
	 2#00101000,
	 2#01111100,
       	 2#10101010,
       	 2#10101010,
       	 2#01000100>>,
    gl:bitmap(7, 9, 0, 0, 8, 0, B);

special_small(option) ->
    B = <<
	 2#00000000111000000:16,
	 2#00000001000000000:16,
	 2#00000010000000000:16,
       	 2#00000100000000000:16,
       	 2#00001000000000000:16,
       	 2#11110001111000000:16>>,
    gl:bitmap(11, 6, 0, 0, 12, 0, B);

special_small(shift) ->
    B = <<
	 2#0000111110000000:16,
	 2#0000100010000000:16,
	 2#0000100010000000:16,
	 2#0011100011100000:16,
	 2#0001100011000000:16,
       	 2#0000110110000000:16,
       	 2#0000011100000000:16,
       	 2#0000001000000000:16>>,
    gl:bitmap(12, 8, 0, 0, 13, 0, B);

special_small(caret) ->
    B = <<
       	 2#11011000,
       	 2#00100000,
       	 2#00100000,
	 2#00100000,
       	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#11011000
	 >>,
    gl:bitmap(5, 11, 0, 3, 2, 0, B);

special_small(crossmark) ->
    B = <<
	 2#00100000,
	 2#01110000,
	 2#11111000,
	 2#11011100,
	 2#10001110,
	 2#00000110,
	 2#00000010
	 >>,
    gl:bitmap(7, 7, 0, 0, 8, 0, B);

special_small(axisx) ->
    B = <<16#63,16#63,16#36,16#3e,16#1c,16#3e,16#36,16#63,16#63>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_small(axisy) ->
    B = <<16#18,16#18,16#18,16#18,16#3c,16#3c,16#66,16#e7,16#c3>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_small(axisz) ->
    B = <<16#7f,16#60,16#70,16#38,16#1c,16#0e,16#07,16#03,16#7f>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_small(folder) ->
    B = <<
       	 2#0111111111000000:16,
	 2#0100000001000000:16,
	 2#0100000001000000:16,
       	 2#0100000001000000:16,
	 2#0100000001000000:16,
	 2#0111111111000000:16,
       	 2#0010001000000000:16,
       	 2#0001110000000000:16,
       	 2#0000000000000000:16>>,
    gl:bitmap(11, 9, 0, 0, 12, 0, B).

special_large(option_box) ->
    B = <<
	 2#11111100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#11111100,
	 2#11111100
	 >>,
    gl:bitmap(6, 10, 0, 4, 7, 0, B);

special_large(command) ->
    B = <<
       	 2#01000100,
       	 2#10101010,
       	 2#10101010,
	 2#01111100,
	 2#00101000,
	 2#01111100,
       	 2#10101010,
       	 2#10101010,
       	 2#01000100>>,
    gl:bitmap(7, 9, 0, 0, 8, 0, B);

special_large(option) ->
    B = <<
	 2#0000000001111000:16,
	 2#0000000010000000:16,
	 2#0000000100000000:16,
	 2#0000001000000000:16,
       	 2#0000010000000000:16,
       	 2#0000100000000000:16,
       	 2#1111000111111000:16>>,
    gl:bitmap(13, 7, 0, 0, 14, 0, B);

special_large(shift) ->
    B = <<
	 2#0000111110000000:16,
	 2#0000100010000000:16,
	 2#0000100010000000:16,
	 2#0000100010000000:16,
       	 2#0111100011110000:16,
	 2#0011000001100000:16,
	 2#0001100011000000:16,
       	 2#0000110110000000:16,
       	 2#0000011100000000:16,
       	 2#0000001000000000:16>>,
    gl:bitmap(13, 10, 0, 0, 14, 0, B);

special_large(caret) ->
    B = <<
       	 2#11011000,
       	 2#00100000,
       	 2#00100000,
	 2#00100000,
       	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#00100000,
	 2#11011000
	 >>,
    gl:bitmap(5, 14, 0, 3, 2, 0, B);

special_large(crossmark) ->
    B = <<
	 2#00100000,
	 2#01110000,
	 2#11111000,
	 2#11011100,
	 2#10001110,
	 2#00000110,
	 2#00000010
	 >>,
    gl:bitmap(7, 7, 0, 0, 8, 0, B);

special_large(axisx) ->
    B = <<16#63,16#63,16#36,16#3e,16#1c,16#3e,16#36,16#63,16#63>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_large(axisy) ->
    B = <<16#18,16#18,16#18,16#18,16#3c,16#3c,16#66,16#e7,16#c3>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_large(axisz) ->
    B = <<16#7f,16#60,16#70,16#38,16#1c,16#0e,16#07,16#03,16#7f>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_large(folder) ->
    B = <<
       	 2#0111111111110000:16,
	 2#0100000000010000:16,
	 2#0100000000010000:16,
	 2#0100000000010000:16,
       	 2#0100000000010000:16,
	 2#0100000000010000:16,
	 2#0111111111110000:16,
       	 2#0010000100000000:16,
       	 2#0001111000000000:16,
       	 2#0000000000000000:16>>,
    gl:bitmap(13, 10, 0, 0, 14, 0, B).
