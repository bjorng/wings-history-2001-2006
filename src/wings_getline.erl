%%
%%  wings_getline.erl --
%%
%%     This module provides a read-line interface with editing.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_getline.erl,v 1.2 2001/09/18 12:02:54 bjorng Exp $
%%

-module(wings_getline).
-export([filename/2,string/1,string/2,yes_no/1,number/2,set_cwd/1]).

-import(lists, [reverse/1,reverse/2,prefix/2,nthtail/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-record(text,
	{x,
	 y,
	 bef,
	 aft,
	 max,
	 ext,
	 number=false
	}).

filename(Prompt, Ext) ->
    Ts = init_text(cwd()),
    readline(Prompt, Ts#text{ext=Ext}).

number(Prompt, Default) ->
    Ts0 = init_text(integer_to_list(Default)),
    Ts = Ts0#text{number=true},
    case readline(Prompt, Ts) of
	aborted -> aborted;
	Str ->
	    case catch list_to_integer(Str) of
		{'EXIT',_} ->
		    please_enter_a_number(),
		    number(Prompt, Default);
		N -> N
	    end
    end.

please_enter_a_number() ->
    wings_io:display(
      fun(_, _) ->
	      wings_io:draw_message(
		fun() ->
			wings_io:text_at(0, "Please enter a number.")
		end)
      end),
    receive after 1000 -> ok end.

string(Prompt) ->
    Ts = init_text(""),
    readline(Prompt, Ts).

string(Prompt, Default) ->
    Ts = init_text(Default),
    readline(Prompt, Ts).

yes_no(Prompt0) ->
    Prompt = Prompt0 ++ " (yes/no) ",
    Ts = init_text(""),
    case readline(Prompt, Ts) of
	"yes" -> yes;
	"no" -> no;
	aborted -> aborted;
	Other ->
	    please_answer_yes_or_no(),
	    yes_no(Prompt0)
    end.

please_answer_yes_or_no() ->
    wings_io:display(
      fun(_, _) ->
	      wings_io:draw_message(
		fun() ->
			wings_io:text_at(0, "Please answer yes or no.")
		end)
      end),
    receive after 1000 -> ok end.

set_cwd(Dir) ->
    put(wings_cwd, slashify(Dir)).

cwd() ->
    case get(wings_cwd) of
	undefined ->
	    {ok,Cwd0} = file:get_cwd(),
	    Cwd = slashify(Cwd0),
	    put(wings_cwd, Cwd),
	    Cwd;
	Cwd -> Cwd
    end.

slashify(Cwd0) ->
    Cwd = filename:join([Cwd0]),
    case lists:last(Cwd) of
	$/ -> Cwd;
	Other -> Cwd ++ "/"
    end.
	    
readline(Prompt, Ts) ->
    wings_io:display(fun(W, H) -> readline(W, H, Prompt, Ts) end).
			     
readline(W, H, Prompt, #text{x=X0,y=Y,bef=Bef,ext=Ext}=Ts0) ->
    wings_io:draw_message(
      fun() ->
	      wings_io:text_at(0, Prompt),
	      X = ?CHAR_WIDTH * length(Prompt),
	      Ts = Ts0#text{x=X,y=0},
	      wings_io:text_at(X, reverse(Bef)),
	      toggle_cursor(Ts),
	      read_loop(Ts)
      end).

init_text(String) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    X = 0,
    Y = H-?LINE_HEIGHT,
    #text{max=40,bef=lists:reverse(String),aft=[],x=X,y=Y}.

read_loop(Ts0) ->
    case get_event() of
	{_,27} ->				%Escape
	    aborted;
	{_,$\r} ->
	    get_text(Ts0);
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

key(?SDLK_KP_PLUS, _, #text{number=true}=Ts) ->
    increment(Ts, 1);
key(?SDLK_KP_MINUS, _, #text{number=true}=Ts) ->
    increment(Ts, -1);
key(_, Unicode, Ts) ->
    key(Unicode, Ts).

key($+, #text{number=true}=Ts) ->
    increment(Ts, 1);
key($-, #text{number=true}=Ts) ->
    increment(Ts, -1);
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
key(C, Ts) ->
%%    erlang:display({C,Ts}),
    Ts.

increment(Ts, Incr) ->
    Str0 = get_text(Ts),
    case catch list_to_integer(Str0) of
	{'EXIT',_} -> Ts;
	N ->
	    Str = integer_to_list(N+Incr),
	    Ts#text{bef=reverse(Str),aft=[]}
    end.
	    
complete(#text{ext=undefined}=Ts) -> Ts;
complete(#text{ext=Ext,bef=Bef0,aft=[]}=Ts) ->
    Wc = reverse(Bef0, "*"),
    Alts0 = filelib:wildcard(Wc),
    Alts = transform_names(Alts0, Ext, []),
    case match(reverse(Bef0), Alts, Ts) of
	no -> Ts;
	{yes,Chars} ->
	    Ts#text{bef=reverse(Chars, Bef0)}
    end;
complete(Ts) -> Ts.

transform_names([N|Ns], Ext, Acc) ->
    case filelib:is_dir(N) of
	true -> transform_names(Ns, Ext, [N++"/"|Acc]);
	false ->
	    case filename:extension(N) of
		Ext -> transform_names(Ns, Ext, [N|Acc]);
		Other -> transform_names(Ns, Ext, Acc)
	    end
    end;
transform_names([], Ext, Acc) -> Acc.

update(#text{bef=BefC,aft=AftC,x=X,y=Y}, #text{bef=BefP,aft=AftP}) ->
    update(reverse(BefC, AftC), reverse(BefP, AftP), X, Y).

update([C|Curr], [C|Prev], X, Y) ->
    update(Curr, Prev, X+?CHAR_WIDTH, Y);
update([C|Curr], [_|Prev], X, Y) ->
    wings_io:space_at(X, 0),
    wings_io:text_at(X, [C]),
    update(Curr, Prev, X+?CHAR_WIDTH, Y);
update([_|_]=Curr, [], X, Y) ->
    wings_io:text_at(X, Curr);
update([], [_|Prev], X, Y) ->
    wings_io:space_at(X, Y),
    update([], Prev, X+?CHAR_WIDTH, Y);
update([], [], X, Y) -> ok.
    
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
	Other ->
	    erlang:yield(),
	    get_event()
    end.

match(Prefix, Alts, Ts) ->
    Matches = match1(Prefix, Alts, []),
    case longest_common_head(Matches) of
	{partial, []} ->
	    print_matches(Matches, Ts),
	    no;
	{partial, Str} ->
	    case nthtail(length(Prefix), Str) of
		[] ->
		    print_matches(Matches, Ts),
		    {yes, []};
		Remain ->
		    {yes, Remain}
	    end;
	{complete, Str} ->
	    {yes,nthtail(length(Prefix), Str)};
	no -> no
    end.

%% Print the list of names L in multiple columns.
print_matches(L, Ts) ->
    col_print(lists:sort(L), Ts).

col_print([], Ts) -> ok;
col_print(L, #text{}=Ts)  ->
    wings_io:draw_completions(
      fun() ->
	      wings_io:text_at(0, "Completions: "),
	      col_print(L, field_width(L), 0, 0, 2*?LINE_HEIGHT)
      end).

col_print(Any, Width, Len, X, Y) when Width + Len > 79 ->
    col_print(Any, Width, 0, X, Y+?LINE_HEIGHT);
col_print([H|T], Width, Len, X, Y) ->
    wings_io:text_at(X+Len*?CHAR_WIDTH, Y, filename:basename(H)),
    col_print(T, Width, Len+Width, X, Y);
col_print([], _, _, X, Y) -> ok.

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

same_head([[H|T]|T1]) -> same_head(H, T1).

same_head(H, [[H|_]|T]) -> same_head(H, T);
same_head(H, [])        -> true;
same_head(H, _)         -> false.

all_tails(LL) -> all_tails(LL, []).

all_tails([[_|T]|T1], L) -> all_tails(T1, [T|L]);
all_tails([], L)         -> L.

all_nil([]) -> true;
all_nil([[] | Rest]) -> all_nil(Rest);
all_nil(_) -> false.
