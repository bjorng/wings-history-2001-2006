%%
%%  wings_ask.erl --
%%
%%     Dialog boxes.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_ask.erl,v 1.1 2002/02/10 18:17:11 bjorng Exp $
%%

-module(wings_ask).
-export([ask/3,ask/4]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,reverse/2]).
-record(s,
	{w,
	 h,
	 call,
	 focus,
	 qs,
	 redraw
	}).

ask(false, Qs, St, Fun) ->
    Res = [element(2, Q) || Q <- Qs],
    wings_io:putback_event({action,Fun(Res)}),
    keep;
ask(true, Qs, St, Fun) -> ask(Qs, St, Fun).

ask(Qs0, Redraw, Fun) ->
    {Qs,W,H} = normalize(Qs0),
    S = #s{w=W,h=H,call=Fun,qs=Qs,focus=1,redraw=Redraw},
    {seq,{push,dummy},get_event(S)}.

get_event(S) ->
    redraw(S),
    {replace,fun(Ev) -> event(Ev, S) end}.

event(#keyboard{keysym=#keysym{sym=Sym,unicode=Unicode}}=Ev, S) ->
    event_key(Sym, Unicode, Ev, S);
event(Ev, S) ->
    field_event(Ev, S).

event_key(?SDLK_ESCAPE, _, Ev, S) ->
    wings_io:putback_event(redraw),
    pop;
event_key(_, $\t, Ev, #s{focus=I,qs=Qs}=S) when I =:= size(Qs) ->
    get_event(S#s{focus=1});
event_key(_, $\t, Ev, #s{focus=I}=S) ->
    get_event(S#s{focus=I+1});
event_key(?SDLK_KP_ENTER, _, Ev, S) ->
    return_result(S);
event_key(_, $\r, Ev, S) ->
    return_result(S);
event_key(_, _, Ev, S) ->
    field_event(Ev, S).

field_event(Ev, #s{focus=I,call=EndFun,qs=Qs0}=S) ->
    {FieldFun,Fst0} = element(I, Qs0),
    Fst = FieldFun({event,Ev}, Fst0),
    Qs = setelement(I, Qs0, {FieldFun,Fst}),
    get_event(S#s{qs=Qs}).

return_result(#s{qs=Qs}=S) ->
    return_result(1, Qs, S, []).

return_result(I, Qs, S, Acc) when I =< size(Qs) ->
    {Fun,Fst} = element(I, Qs),
    case catch Fun(value, Fst) of
	{'EXIT',Reason} ->
	    exit(Reason);
	{command_error,Error} ->
	    wings_util:message(Error),
	    get_event(S#s{focus=I});
	Res ->
	    return_result(I+1, Qs, S, [Res|Acc])
    end;
return_result(_, _, #s{call=EndFun}=S, Res) ->
     case catch EndFun(reverse(Res)) of
	 {command_error,Error} ->
	     wings_util:message(Error),
	     get_event(S);
	 {'EXIT',Reason} ->
	     exit(Reason);
	 ignore ->
	     wings_io:putback_event(redraw),
	     pop;
	 Action when is_tuple(Action); is_atom(Action) ->
	     wings_io:putback_event({action,Action}),
	     pop
     end.
    
redraw(#s{w=Xs,h=Ys,redraw=Redraw,focus=Focus,qs=Qs}) ->
    case Redraw of
	#st{}=St ->
	    wings_draw:render(St),
	    wings_io:draw_ui(St);
	Other ->
	    Redraw()
    end,
    wings_io:ortho_setup(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    gl:translated((W-Xs)/2, (H-Ys)/2, 0.0),
    B = 16,
    wings_io:raised_rect(-B, -B, Xs+B, Ys+B, ?MENU_COLOR),
    draw_fields(1, Qs, Focus),
    wings_io:swap_buffers().

draw_fields(I, Qs, Focus) when I =< size(Qs) ->
    {Fun,Fst} = element(I, Qs),
    Fun({redraw,I =:= Focus}, Fst),
    draw_fields(I+1, Qs, Focus);
draw_fields(_, _, _) -> ok.

%%%
%%% Conversion of queries to internal format and dimension calculation.
%%%

normalize(Qs) when is_list(Qs) ->
    normalize({vframe,Qs}, 0, 0, 0, 0, []);
normalize(Qs) ->
    normalize(Qs, 0, 0, 0, 0, []).

normalize({vframe,Qs}, X, Y, W, H, Acc) ->
    vframe(Qs, X, Y, W, H, Acc).

vframe([Q|Qs], X, Y, W0, H0, Acc) ->
    {F,W,H} = normalize_field(Q, X, Y),
    vframe(Qs, X, Y+H, max(W0, W), H0+H, [F|Acc]);
vframe([], X, Y, W, H, Acc) ->
    {list_to_tuple(reverse(Acc)),W,H}.

normalize_field({Prompt,Def}, X, Y) when is_float(Def) ->
    Fun = float_fun(),
    DefStr = simplify_float(lists:flatten(io_lib:format("~f", [Def]))),
    Fst = init_text(X, Y, Prompt++": ", DefStr, 20),
    {{Fun,Fst},(20+length(Prompt)+2)*?CHAR_WIDTH,?LINE_HEIGHT};
normalize_field({Prompt,Def}, X, Y) when is_integer(Def) ->
    Fun = integer_fun(),
    Fst = init_text(X, Y, Prompt++": ", integer_to_list(Def), 10),
    {{Fun,Fst},(10+length(Prompt)+2)*?CHAR_WIDTH,?LINE_HEIGHT};
normalize_field({Prompt,Def,Min,Max}, X, Y) when is_integer(Def) ->
    Fun = integer_fun(),
    Fst = init_text(X, Y, Prompt++": ", integer_to_list(Def), 10),
    {{Fun,Fst},(10+length(Prompt)+2)*?CHAR_WIDTH,?LINE_HEIGHT};
normalize_field({Prompt,Def}, X, Y) when is_list(Def) ->
    Fun = string_fun(),
    Fst = init_text(X, Y, Prompt++": ", Def, 40),
    {{Fun,Fst},(40+length(Prompt)+2)*?CHAR_WIDTH,?LINE_HEIGHT};
normalize_field({Prompt,Def}, X, Y) ->
    Fun = term_fun(),
    TermStr = print_term(Def),
    Len = max(length(TermStr)+10, 40),
    Fst = init_text(X, Y, Prompt++": ", TermStr, Len),
    {{Fun,Fst},(Len+length(Prompt)+2)*?CHAR_WIDTH,?LINE_HEIGHT}.

max(A, B) when A > B -> A;
max(A, B) -> B.

simplify_float(F) ->
    reverse(simplify_float_1(reverse(F))).

simplify_float_1("0."++_=F) -> F;
simplify_float_1("0"++F) -> simplify_float_1(F);
simplify_float_1(F) -> F.

print_term(Term) ->
    lists:flatten(print_term_1(Term)).

print_term_1(Tuple) when is_tuple(Tuple) ->
    ["{",print_tuple(1, size(Tuple), Tuple),"}"];
print_term_1(Float) when is_float(Float) ->
    S0 = io_lib:format("~f", [Float]),
    S = simplify_float(lists:flatten(S0));
print_term_1(Term) ->
    io_lib:format("~p", [Term]).

print_tuple(I, I, T) ->
    print_term_1(element(I, T));
print_tuple(I, Sz, T) ->
    [print_term_1(element(I, T)),$,|print_tuple(I+1, Sz, T)].
    
%%%
%%% Readline stuff.
%%%

-record(text,
	{x,
	 y,
	 bef,
	 aft,
	 max,
	 ext,
	 prompt,
	 number=false
	}).

string_fun() ->
    fun({redraw,Active}, Ts) ->
	    draw_text(Ts, Active);
       ({event,Ev}, Ts) ->
	    read_event(Ev, Ts);
       (value, Ts) ->
	    get_text(Ts)
    end.

integer_fun() ->
    fun({redraw,Active}, Ts) ->
	    draw_text(Ts, Active);
       ({event,Ev}, Ts) ->
	    read_event(Ev, Ts);
       (value, Ts) ->
	    Text = get_text(Ts),
	    case catch list_to_integer(Text) of
		Int when is_integer(Int) -> Int;
		Crash -> wings_util:error("Bad integer ("++Text++")")
	    end
    end.

float_fun() ->
    fun({redraw,Active}, Ts) ->
	    draw_text(Ts, Active);
       ({event,Ev}, Ts) ->
	    read_event(Ev, Ts);
       (value, Ts) ->
	    Text = get_text(Ts),
	    case catch list_to_float(Text) of
		Float when is_float(Float) -> Float;
		Other ->
		    case catch list_to_integer(Text) of
			Int when is_integer(Int) -> float(Int);
			Crash ->
			    wings_util:error("Bad number ("++Text++")")
		    end
	    end
    end.

term_fun() ->
    fun({redraw,Active}, Ts) ->
	    draw_text(Ts, Active);
       ({event,Ev}, Ts) ->
	    read_event(Ev, Ts);
       (value, Ts) ->
	    Text = get_text(Ts),
	    make_term(get_text(Ts))
    end.

make_term(Str) ->
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
		{ok, Term} -> Term;
		{error,{_,_,Reason}} ->
		    io:format("~s: ~s\n", [Reason,Str]),
		    wings_util:error("Bad entry ("++Str++")")
	    end;
	{error,{_,_,Reason},_} ->
	    io:format("~s: ~s~n", [Reason, Str]),
	    wings_util:error("Bad entry ("++Str++")")
    end.

init_text(X, Y0, Prompt, String, Max) ->
    Y = Y0 + ?CHAR_HEIGHT div 2,
    #text{prompt=Prompt,max=Max,bef=lists:reverse(String),aft=[],x=X,y=Y}.

draw_text(#text{prompt=Prompt,bef=BefC,aft=AftC,x=X0,y=Y}, false) ->
    wings_io:text_at(X0, Y, Prompt),
    wings_io:text(reverse(BefC)),
    wings_io:text(AftC);
draw_text(#text{prompt=Prompt,bef=BefC,aft=AftC,x=X0,y=Y}, true) ->
    wings_io:text_at(X0, Y, Prompt),
    wings_io:text(reverse(BefC)),
    X1 = (length(Prompt) + length(BefC))*?CHAR_WIDTH,
    gl:color3f(0, 0, 0),
    gl:recti(X1, Y-?CHAR_HEIGHT, X1+?CHAR_WIDTH, Y+3),
    gl:color3f(1, 1, 1),			%White text
    T = case AftC of
	    [C|T0] ->
		wings_io:text_at(X1, Y, [C]),
		T0;
	    [] ->
		wings_io:text_at(X1, Y, [$\s]),
		[]
	end,
    gl:color3f(0, 0, 0),
    X = X1 + ?CHAR_WIDTH,
    wings_io:text_at(X, Y, T).

read_event(#keyboard{keysym=#keysym{sym=Sym,unicode=Unicode}}, Ts0) ->
    key(Sym, Unicode, Ts0);
read_event(_Ev, Ts) -> Ts.

get_text(#text{bef=Bef,aft=Aft}) ->
    reverse(Bef, Aft).

key(?SDLK_KP_PLUS, _, #text{number=true}=Ts) ->
    increment(Ts, 1);
key(?SDLK_KP_MINUS, _, #text{number=true}=Ts) ->
    increment(Ts, -1);
key(?SDLK_HOME, _, Ts) -> key(1, Ts);
key(?SDLK_END, _, Ts) -> key(5, Ts);
key(?SDLK_LEFT, _, Ts) -> key(2, Ts);
key(?SDLK_RIGHT, _, Ts) -> key(6, Ts);
key(?SDLK_DELETE, _, Ts) -> key(4, Ts);
key(?SDLK_KP_PERIOD, _, Ts) ->
    key($., Ts);
key(C, _, Ts) when ?SDLK_KP0 =< C, C =< ?SDLK_KP9 ->
    key(C-?SDLK_KP0+$0, Ts);
key(Other, Unicode, Ts) ->
    key(Unicode, Ts).

key($+, #text{number=true}=Ts) ->
    increment(Ts, 1);
key($=, #text{number=true}=Ts) ->		%Same key as plus on American keybd.
    increment(Ts, 1);
key($-, #text{number=true}=Ts) ->
    increment(Ts, -1);
key($\b, #text{bef=[_|Bef]}=Ts) ->
    Ts#text{bef=Bef};
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
    %%erlang:display({C,Ts}),
    Ts.

increment(Ts, Incr) ->
    Str0 = get_text(Ts),
    case catch list_to_integer(Str0) of
	{'EXIT',_} -> Ts;
	N ->
	    Str = integer_to_list(N+Incr),
	    Ts#text{bef=reverse(Str),aft=[]}
    end.
