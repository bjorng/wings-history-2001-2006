%%
%%  wings_getline.erl --
%%
%%     This module provides a read-line interface with editing.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_getline.erl,v 1.16 2003/04/27 05:09:59 bjorng Exp $
%%

-module(wings_getline).
-export([filename/2,yes_no/1]).

-import(lists, [reverse/1,reverse/2,prefix/2,nthtail/2,member/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-record(text,
	{x,
	 y,
	 bef,
	 aft,
	 max,
	 ext
	}).

filename(Prompt, Ext) ->
    Ts = init_text(cwd()),
    readline(Prompt, Ts#text{ext=Ext}).

yes_no(Prompt0) ->
    Prompt = Prompt0 ++ " (yes/no) ",
    Ts = init_text(""),
    case readline(Prompt, Ts) of
	"yes" -> yes;
	"no" -> no;
	aborted -> aborted;
	_ -> yes_no(Prompt0)
    end.

cwd() ->
    slashify(wings_pref:get_value(current_directory)).

slashify(Cwd0) ->
    Cwd = filename:join([Cwd0]),
    case lists:last(Cwd) of
	$/ -> Cwd;
	_Other -> Cwd ++ "/"
    end.
	    
readline(Prompt, #text{bef=Bef}=Ts0) ->
    wings_wm:draw_message(
      fun() ->
	      wings_io:text_at(0, Prompt),
	      X = ?CHAR_WIDTH * length(Prompt),
	      Ts = Ts0#text{x=X,y=0},
	      wings_io:text_at(X, reverse(Bef)),
	      toggle_cursor(Ts),
	      read_loop(Ts)
      end).

init_text(String) ->
    {_,_,_,H} = wings_wm:viewport(message),
    X = 0,
    Y = H-?LINE_HEIGHT,
    #text{max=40,bef=lists:reverse(String),aft=[],x=X,y=Y}.

read_loop(Ts0) ->
    case get_event() of
	{?SDLK_KP_ENTER,_} ->
	    get_text(Ts0);
	{_,$\r} ->
	    get_text(Ts0);
	{_,27} ->				%Escape
	    aborted;
	{Sym,Unicode} ->
	    Ts = key(Sym, Unicode, Ts0),
	    toggle_cursor(Ts0),
	    update(Ts, Ts0),
	    gl:flush(),
	    toggle_cursor(Ts),
	    read_loop(Ts)
    end.

get_text(#text{bef=Bef,aft=Aft}) ->
    reverse(Bef, Aft).

key(?SDLK_HOME, _, Ts) -> key(1, Ts);
key(?SDLK_END, _, Ts) -> key(5, Ts);
key(?SDLK_LEFT, _, Ts) -> key(2, Ts);
key(?SDLK_RIGHT, _, Ts) -> key(6, Ts);
key(?SDLK_DELETE, _, Ts) -> key(4, Ts);
key(?SDLK_BACKSPACE, _, Ts) -> key(?SDLK_BACKSPACE, Ts);
key(?SDLK_KP_PERIOD, _, Ts) ->
    key($., Ts);
key(C, _, Ts) when ?SDLK_KP0 =< C, C =< ?SDLK_KP9 ->
    key(C-?SDLK_KP0+$0, Ts);
key(_, Unicode, Ts) ->
    key(Unicode, Ts).

key($\b, #text{bef=[_|Bef]}=Ts) ->
    Ts#text{bef=Bef};
key($\t, Ts) ->
    complete(Ts);
key(2, #text{bef=[C|Bef],aft=Aft}=Ts) ->	%Ctrl-B
    Ts#text{bef=Bef,aft=[C|Aft]};
key(6, #text{bef=Bef,aft=[C|Aft]}=Ts) ->	%Ctrl-F
    Ts#text{bef=[C|Bef],aft=Aft};
key(1, #text{bef=Bef,aft=Aft}=Ts) ->		%Ctrl-A
    Ts#text{bef=[],aft=reverse(Bef, Aft)};
key(5, #text{bef=Bef,aft=Aft}=Ts) ->		%Ctrl-E
    Ts#text{bef=reverse(Aft, Bef),aft=[]};
key(11, #text{}=Ts) ->				%Ctrl-K
    Ts#text{aft=[]};
key(4, #text{aft=[_|Aft]}=Ts) ->		%Ctrl-D
    Ts#text{aft=Aft};
key(C, #text{bef=Bef0}=Ts0) when $\s =< C, C < 256 ->
    Ts0#text{bef=[C|Bef0]};
key(_, Ts) -> Ts.

complete(#text{ext=undefined}=Ts) -> Ts;
complete(#text{ext=Exts,bef=Bef0,aft=[]}=Ts) ->
    Wc = reverse(Bef0, "*"),
    Alts0 = filelib:wildcard(Wc),
    Alts = transform_names(Alts0, Exts, []),
    case match(reverse(Bef0), Alts) of
	no -> Ts;
	{yes,Chars} ->
	    Ts#text{bef=reverse(Chars, Bef0)}
    end;
complete(Ts) -> Ts.

transform_names([N|Ns], Exts, Acc) ->
    case filelib:is_dir(N) of
	true -> transform_names(Ns, Exts, [N++"/"|Acc]);
	false ->
	    case is_good_extension(filename:extension(N), Exts) of
		true -> transform_names(Ns, Exts, [N|Acc]);
		false -> transform_names(Ns, Exts, Acc)
	    end
    end;
transform_names([], _, Acc) -> Acc.

is_good_extension(Ext, Exts) ->
    member(Ext, Exts).

update(#text{bef=BefC,aft=AftC,x=X,y=Y}, #text{bef=BefP,aft=AftP}) ->
    update(reverse(BefC, AftC), reverse(BefP, AftP), X, Y).

update([C|Curr], [C|Prev], X, Y) ->
    update(Curr, Prev, X+?CHAR_WIDTH, Y);
update([C|Curr], [_|Prev], X, Y) ->
    wings_io:space_at(X, 0),
    wings_io:text_at(X, [C]),
    update(Curr, Prev, X+?CHAR_WIDTH, Y);
update([_|_]=Curr, [], X, _) ->
    wings_io:text_at(X, Curr);
update([], [_|Prev], X, Y) ->
    wings_io:space_at(X, Y),
    update([], Prev, X+?CHAR_WIDTH, Y);
update([], [], _, _) -> ok.
    
toggle_cursor(#text{bef=Bef,x=X0,y=Y}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    X = X0 + length(Bef) * ?CHAR_WIDTH,
    gl:color3f(0.75, 0.75, 0.75),
    gl:enable(?GL_COLOR_LOGIC_OP),
    gl:logicOp(?GL_XOR),
    gl:recti(X, Y-?LINE_HEIGHT+3, X+?CHAR_WIDTH, Y+3),
    gl:flush(),
    gl:popAttrib().

get_event() ->
    case wings_io:get_event() of
	no_event ->
	    erlang:yield(),
	    get_event();
	#keyboard{keysym=#keysym{sym=Sym,unicode=Unicode}} ->
	    {Sym,Unicode};
	_Other ->
	    erlang:yield(),
	    get_event()
    end.

match(Prefix, Alts) ->
    Matches = match1(Prefix, Alts, []),
    case longest_common_head(Matches) of
	{partial, []} ->
	    print_matches(Matches),
	    no;
	{partial, Str} ->
	    case nthtail(length(Prefix), Str) of
		[] ->
		    print_matches(Matches),
		    {yes, []};
		Remain ->
		    {yes, Remain}
	    end;
	{complete, Str} ->
	    {yes,nthtail(length(Prefix), Str)};
	no -> no
    end.

%% Print the list of names L in multiple columns.
print_matches(L) ->
    col_print(lists:sort(L)).

col_print([]) -> ok;
col_print(L)  ->
    wings_wm:draw_completions(
      fun() ->
	      wings_io:text_at(0, "Completions: "),
	      col_print(L, field_width(L), 0, 0, 2*?LINE_HEIGHT)
      end).

col_print(Any, Width, Len, X, Y) when Width + Len > 79 ->
    col_print(Any, Width, 0, X, Y+?LINE_HEIGHT);
col_print([H|T], Width, Len, X, Y) ->
    wings_io:text_at(X+Len*?CHAR_WIDTH, Y, filename:basename(H)),
    col_print(T, Width, Len+Width, X, Y);
col_print([], _, _, _, _) -> ok.

field_width([H|T]) -> field_width(T, length(filename:basename(H))).

field_width([H|T], W) ->
    case length(filename:basename(H)) of
	L when L > W -> field_width(T, L);
	_ -> field_width(T, W)
    end;
field_width([], W) when W < 40 ->
    W + 4;
field_width([], _) ->
    40.

match1(Prefix, [H|T], L) ->
    case prefix(Prefix, Str = H) of
	true ->  match1(Prefix, T, [Str|L]);
	false -> match1(Prefix, T, L)
    end;
match1(_, [], L) -> L.

longest_common_head([]) -> no;
longest_common_head(LL) -> longest_common_head(LL, []).

longest_common_head([[]|_], L) ->
    {partial, reverse(L)};
longest_common_head(LL, L) ->
    case same_head(LL) of
	true ->
	    [[H|_]|_] = LL,
	    LL1 = all_tails(LL),
	    case all_nil(LL1) of
		false ->
		    longest_common_head(LL1, [H|L]);
		true ->
		    {complete, reverse([H|L])}
	    end;
	false ->
	    {partial, reverse(L)}
    end.

same_head([[H|_]|T1]) -> same_head(H, T1).

same_head(H, [[H|_]|T]) -> same_head(H, T);
same_head(_, [])        -> true;
same_head(_, _)         -> false.

all_tails(LL) -> all_tails(LL, []).

all_tails([[_|T]|T1], L) -> all_tails(T1, [T|L]);
all_tails([], L)         -> L.

all_nil([]) -> true;
all_nil([[] | Rest]) -> all_nil(Rest);
all_nil(_) -> false.
