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
%%     $Id: wings_ask.erl,v 1.3 2002/02/11 20:07:07 bjorng Exp $
%%

-module(wings_ask).
-export([ask/3,ask/4]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,reverse/2,duplicate/2]).
-record(s,
	{w,
	 h,
	 call,
	 focus,
	 fi,					%Static data for all fields.
	 priv,					%States for all fields.
	 redraw
	}).

%% Static data for each field.
-record(fi,
	{handler,				%Handler fun.
	 flags,					%Flags field.
	 x,y,					%Upper left position.
	 lw,					%Label width.
	 w,h					%Width, height.
	}).

ask(false, Qs, St, Fun) ->
    Res = [element(2, Q) || Q <- Qs],
    wings_io:putback_event({action,Fun(Res)}),
    keep;
ask(true, Qs, St, Fun) -> ask(Qs, St, Fun).

ask(Qs0, Redraw, Fun) ->
    Qs1 = normalize(Qs0),
    Qs = propagate_sizes(Qs1),
    {Fis0,Priv0} = flatten_fields(Qs),
    Fis = list_to_tuple(Fis0),
    Priv = list_to_tuple(Priv0),
    {#fi{lw=Lw,w=W0,h=H},_} = Qs,
    W = Lw+W0,
    S0 = #s{w=W,h=H,call=Fun,fi=Fis,priv=Priv,focus=size(Fis),redraw=Redraw},
    S = next_focus(S0, 1),
    {seq,{push,dummy},get_event(S)}.

get_event(S) ->
    redraw(S),
    {replace,fun(Ev) -> event(Ev, S) end}.

event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=Unicode}}, S) ->
    event_key({key,Sym,Mod,Unicode}, S);
event(#resize{}=Ev, S) ->
    wings_io:putback_event(Ev),
    pop;
event(Ev, S) ->
    field_event(Ev, S).

event_key({key,?SDLK_ESCAPE,_,_}, S) ->
    wings_io:putback_event(redraw),
    pop;
event_key({key,?SDLK_TAB,Mod,_}, S) when Mod band ?SHIFT_BITS =/= 0 ->
    get_event(next_focus(S, -1));
event_key({key,?SDLK_TAB,Mod,_}, S) ->
    get_event(next_focus(S, 1));
event_key({key,_,Mod,$\t}, S) ->
    get_event(next_focus(S, 1));
event_key({key,?SDLK_KP_ENTER,_,_}, S) ->
    return_result(S);
event_key({key,_,_,$\r}, S) ->
    return_result(S);
event_key(Ev, S) ->
    field_event(Ev, S).

next_focus(#s{focus=I}=S, Dir) ->
    next_focus_1(I, Dir, S).

next_focus_1(I0, Dir, #s{fi=Fis,priv=Priv}=S) ->
    I = case I0+Dir of
	    I1 when 0 < I1, I1 =< size(Fis) -> I1;
	    0 -> size(Fis);
	    _ -> 1
	end,
    #fi{handler=Handler} = Fi = element(I, Fis),
    case Handler(is_inert, Fi, element(I, Priv)) of
	true -> next_focus_1(I, Dir, S);
	false -> S#s{focus=I}
    end.
    
field_event(Ev, #s{focus=I,call=EndFun,fi=Fis,priv=Priv0}=S) ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv0),
    Fst = Handler({event,Ev}, Fi, Fst0),
    Priv = setelement(I, Priv0, Fst),
    get_event(S#s{priv=Priv}).

return_result(#s{fi=Fis,priv=Priv}=S) ->
    return_result(1, Fis, Priv, S, []).

return_result(I, Fis, Priv, S, Acc) when I =< size(Fis) ->
    #fi{handler=Handler,flags=Flags} = Fi = element(I, Fis),
    Fst = element(I, Priv),
    case Handler(is_inert, Fi, Fst) of
	true -> return_result(I+1, Fis, Priv, S, Acc);
	false ->
	    case catch Handler(value, Fi, Fst) of
		{'EXIT',Reason} ->
		    exit(Reason);
		{command_error,Error} ->
		    wings_util:message(Error),
		    get_event(S#s{focus=I});
		Res0 ->
		    Res = case property_lists:get_value(key, Flags) of
			      undefined -> Res0;
			      Key -> {Key,Res0}
			  end,
		    return_result(I+1, Fis, Priv, S, [Res|Acc])
	    end
    end;
return_result(_, _, _, #s{call=EndFun}=S, Res) ->
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
    
redraw(#s{w=Xs,h=Ys,redraw=Redraw,focus=Focus,fi=Fi,priv=Priv}) ->
    case Redraw of
	#st{}=St ->
	    wings_draw:render(St),
	    wings_io:draw_ui(St);
	Other ->
	    Redraw()
    end,
    wings_io:ortho_setup(),
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Tx = case (W-Xs)/2 of
	     SmallX when SmallX < 10 -> 10;
	     Tx0 -> Tx0
	 end,
    Ty = case (H-Ys)/2 of
	     SmallY when SmallY < 36 -> 36;
	     Ty0 -> Ty0
	 end,
    gl:translated(Tx, Ty, 0.0),
    wings_io:raised_rect(-16, -8, Xs+32, Ys+16, ?MENU_COLOR),
    draw_fields(1, Fi, Priv, Focus),
    wings_io:swap_buffers().

draw_fields(I, Fis, Priv, Focus) when I =< size(Fis) ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst = element(I, Priv),
    Handler({redraw,I =:= Focus}, Fi, Fst),
    draw_fields(I+1, Fis, Priv, Focus);
draw_fields(_, _, _, _) -> ok.

%%%
%%% Conversion of queries to internal format and dimension calculation.
%%%

normalize(Qs) when is_list(Qs) ->
    normalize({vframe,Qs,[]});
normalize(Qs) ->
    normalize(Qs, #fi{x=0,y=0}).

normalize({vframe,Qs,Flags}, Fi) ->
    vframe(Qs, Fi, Flags);
normalize({hframe,Qs,Flags}, Fi) ->
    hframe(Qs, Fi, Flags, []);
normalize(separator, Fi) ->
    normalize_field(separator(), [], Fi);
normalize({Prompt,Def}, Fi) when Def == false; Def == true ->
    normalize_field(checkbox(Prompt, Def), [], Fi);
normalize({Prompt,Def,Flags}, Fi) when Def == false; Def == true ->
    normalize_field(checkbox(Prompt, Def), Flags, Fi);
normalize({Prompt,Def}, Fi) ->
    normalize_field(text_field(Prompt, Def), [], Fi);
normalize({Prompt,Def,Flags}, Fi) ->
    normalize_field(text_field(Prompt, Def), Flags, Fi).

vframe(Qs, Fi, Flags) ->
    vframe(Qs, Fi, Flags, []).

vframe([Q|Qs], #fi{y=Y}=Fi0, Flags, Acc) ->
    {#fi{h=H}=Fi,Priv} = normalize(Q, Fi0),
    vframe(Qs, Fi#fi{y=Y+H}, Flags, [{Fi,Priv}|Acc]);
vframe([], _, Flags, Fields0) ->
    [{Fi,_}|_] = Fields = reverse(Fields0),
    {Lw0,W0,H0} = frame_init_size(Flags),
    {Lw,W,H} = vframe_size(Fields, Lw0, W0, H0),
    Fun = frame_fun(),
    {Fi#fi{handler=Fun,lw=Lw,w=W,h=H,flags=Flags},{vframe,Fields}}.

hframe([Q|Qs], #fi{x=X}=Fi0, Flags, Acc) ->
    {#fi{lw=Lw,w=W}=Fi,Priv} = normalize(Q, Fi0),
    hframe(Qs, Fi#fi{x=X+Lw+W}, Flags, [{Fi,Priv}|Acc]);
hframe([], _, Flags, Fields0) ->
    [{Fi,_}|_] = Fields = reverse(Fields0),
    {Lw0,W0,H0} = frame_init_size(Flags),
    {Lw,W,H} = hframe_size(Fields, Lw0, W0, H0),
    Fun = frame_fun(),
    {Fi#fi{handler=Fun,lw=Lw,w=W,h=H,flags=Flags},{vframe,Fields}}.

normalize_field({Handler,Priv,Lw,W,H}, Flags, Fi) ->
    {Fi#fi{handler=Handler,flags=Flags,w=W,h=H,lw=Lw},Priv}.

frame_init_size(Flags) ->
    case property_lists:is_defined(border, Flags) of
	true -> {0,2*10,2*10};
	false -> {0,0,0}
    end.
    
max(A, B) when A > B -> A;
max(A, B) -> B.

vframe_size([{#fi{lw=Lw,w=W,h=H},_}|Fis], Lw0, W0, H0) ->
    vframe_size(Fis, max(Lw0, Lw), max(W0, W), H0+H);
vframe_size([], Lw, W, H) -> {Lw,W,H}.

hframe_size([{#fi{lw=Lw,w=W,h=H},_}|Fis], Lw0, W0, H0) ->
    hframe_size(Fis, Lw+Lw0, W+W0, max(H0, H));
hframe_size([], Lw, W, H) -> {Lw,W,H}.

propagate_sizes({Fi,{vframe,Fields0}}) ->
    Fields = vframe_propagate(Fields0, Fi, []),
    {Fi,{vframe,Fields}};
propagate_sizes({Fi,{hframe,Fields0}}) ->
    Fields = hframe_propagate(Fields0, Fi, []),
    {Fi,{hframe,Fields}};
propagate_sizes(Other) -> Other.

vframe_propagate([{#fi{lw=0}=Fi0,Priv}|Fis], #fi{lw=Lw,w=W}=Sz, Acc) ->
    Fi = Fi0#fi{w=Lw+W},
    vframe_propagate(Fis, Sz, [propagate_sizes({Fi,Priv})|Acc]);
vframe_propagate([{Fi0,Priv}|Fis], #fi{lw=Lw,w=W}=Sz, Acc) ->
    Fi = Fi0#fi{lw=Lw,w=W},
    vframe_propagate(Fis, Sz, [propagate_sizes({Fi,Priv})|Acc]);
vframe_propagate([], _, Acc) -> reverse(Acc).

hframe_propagate([{Fi0,Priv}|Fis], #fi{h=H}=Sz, Acc) ->
    Fi = Fi0#fi{h=H},
    hframe_propagate(Fis, Sz, [propagate_sizes({Fi,Priv})|Acc]);
hframe_propagate([], _, Acc) -> reverse(Acc).

flatten_fields({Fi,P}) ->
    {Fis,Priv} = flatten_fields(Fi, P, [], []),
    {reverse(Fis),reverse(Priv)}.

flatten_fields(Fi, {vframe,Fields}, FisAcc, PrivAcc) ->
    frame_flatten(Fields, [Fi|FisAcc], [{vframe,[]}|PrivAcc]);
flatten_fields(Fi, {hframe,Fields}, FisAcc, PrivAcc) ->
    frame_flatten(Fields, [Fi|FisAcc], [{hframe,[]}|PrivAcc]);
flatten_fields(Fi, Priv, FisAcc, PrivAcc) ->
    {[Fi|FisAcc],[Priv|PrivAcc]}.

frame_flatten([{Fi,Priv}|T], FisAcc0, PrivAcc0) ->
    {FisAcc,PrivAcc} = flatten_fields(Fi, Priv, FisAcc0, PrivAcc0),
    frame_flatten(T, FisAcc, PrivAcc);
frame_flatten([], FisAcc, PrivAcc) -> {FisAcc,PrivAcc}.

frame_fun() ->
    fun({redraw,Active}, Fi, _Dummy) -> ok;
       ({event,Ev}, Fi, Frame) -> Frame;
       (is_inert, Fi, Frame) -> true
    end.

%%%
%%% Separator.
%%%

separator() ->
    Fun = separator_fun(),
    {Fun,no_state,0,4*?CHAR_WIDTH,10}.

separator_fun() ->
    fun({redraw,Active}, Fi, _Dummy) ->
	    separator_draw(Active, Fi);
       ({event,Ev}, Fi, Sep) ->
	    Sep;
       (is_inert, Fi, Sep) -> true
    end.

separator_draw(Active, #fi{x=X,y=Y,w=W}) ->
    ?CHECK_ERROR(),
    LeftX = X,
    RightX = X+W,
    UpperY = Y + 5,
    LowerY = UpperY + 1,
    gl:lineWidth(1.0),
    gl:'begin'(?GL_LINES),
    gl:color3f(0.10, 0.10, 0.10),
    gl:vertex2f(LeftX+0.5, UpperY+0.5),
    gl:vertex2f(RightX+0.5, UpperY+0.5),
    gl:color3f(0.90, 0.90, 0.90),
    gl:vertex2f(LeftX+1.5, LowerY+0.5),
    gl:vertex2f(RightX+0.5, LowerY+0.5),
    gl:'end'(),
    gl:color3f(0, 0, 0),
    ?CHECK_ERROR().

%%%
%%% Checkboxes.
%%%

-record(cb,
	{label,
	 state}).
	 
checkbox(Label, Def) ->
    Cb = #cb{label=Label,state=Def},
    Fun = checkbox_fun(),
    {Fun,Cb,0,(length(Label)+5)*?CHAR_WIDTH,?LINE_HEIGHT}.

checkbox_fun() ->
    fun({redraw,Active}, Fi, Cb) ->
	    cb_draw(Active, Fi, Cb);
       ({event,Ev}, Fi, Cb) ->
	    cb_event(Ev, Cb);
       (value, Fi, #cb{state=State}) ->
	    State;
       (is_inert, Fi, Sep) ->
	    false
    end.

cb_draw(Active, #fi{x=X,y=Y0}, #cb{label=Label,state=State}) ->
    Y = Y0+?CHAR_HEIGHT,
    case State of
	false -> wings_io:text_at(X, Y, "[ ] ");
	true -> wings_io:text_at(X, Y, "[" ++ [crossmark]++ "] ")
    end,
    wings_io:text_at(X+4*?CHAR_WIDTH, Y, Label),
    if
	Active == true ->
	    wings_io:text_at(X+4*?CHAR_WIDTH, Y,
			     duplicate(length(Label), $_));
	true -> ok
    end.

cb_event({key,_,_,$\s}, #cb{state=State}=Cb) ->
    Cb#cb{state=not State};
cb_event(_, Cb) -> Cb.

%%%
%%% Text and number input fields.
%%%

-record(text,
	{bef,
	 aft,
	 max,
	 ext,
	 label,
	 integer=false
	}).

text_field(Label, Def) when is_float(Def) ->
    Fun = float_fun(),
    DefStr = simplify_float(lists:flatten(io_lib:format("~f", [Def]))),
    init_text(Fun, Label, DefStr, 15, false);
text_field(Label, Def) when is_integer(Def) ->
    init_text(integer_fun(), Label, integer_to_list(Def), 10, true);
text_field(Label, Def) when is_list(Def) ->
    init_text(string_fun(), Label, Def, 30, false);
text_field(Label, Def) ->
    Fun = term_fun(),
    TermStr = print_term(Def),
    init_text(term_fun(), Label, TermStr, 30, false).

init_text(Fun, Label, String, Max0, IsInteger) ->
    Max = max(Max0, length(String)+5),
    Ts = #text{label=Label,max=Max,bef=[],aft=String,integer=IsInteger},
    {Fun,Ts,(length(Label)+1)*?CHAR_WIDTH,Max*?CHAR_WIDTH,?LINE_HEIGHT}.

string_fun() ->
    fun(value, Fi, Ts) -> get_text(Ts);
       (Other, Fi, Ts) -> gen_text_handler(Other, Fi, Ts)
    end.

integer_fun() ->
    fun(value, Fi, Ts) ->
	    Text = get_text(Ts),
	    case catch list_to_integer(Text) of
		Int when is_integer(Int) -> Int;
		Crash -> wings_util:error("Bad integer ("++Text++")")
	    end;
       (Other, Fi, Ts) -> gen_text_handler(Other, Fi, Ts)
    end.

float_fun() ->
    fun(value, Fi, Ts) ->
	    Text = get_text(Ts),
	    case catch list_to_float(Text) of
		Float when is_float(Float) -> Float;
		Other ->
		    case catch list_to_integer(Text) of
			Int when is_integer(Int) -> float(Int);
			Crash ->
			    wings_util:error("Bad number ("++Text++")")
		    end
	    end;
       (Other, Fi, Ts) -> gen_text_handler(Other, Fi, Ts)
    end.


term_fun() ->
    fun(value, Fi, Ts) ->
	    Text = get_text(Ts),
	    make_term(get_text(Ts));
       (Other, Fi, Ts) -> gen_text_handler(Other, Fi, Ts)
    end.

gen_text_handler({redraw,Active}, Fi, Ts) ->
    draw_text(Fi, Ts, Active);
gen_text_handler({event,Ev}, Fi, Ts) ->
    text_event(Ev, Ts);
gen_text_handler(is_inert, Fi, Ts) ->
    false.

text_event({key,Sym,_,Unicode}, Ts) ->
    key(Sym, Unicode, Ts);
text_event(_Ev, Ts) -> Ts.

draw_text(#fi{x=X,y=Y0,lw=Lw}, #text{label=Label,bef=BefC,aft=AftC}, false) ->
    Y = Y0 + ?CHAR_HEIGHT,
    wings_io:text_at(X, Y, Label),
    wings_io:text_at(X+Lw, Y, reverse(BefC)),
    wings_io:text(AftC);
draw_text(#fi{x=X0,y=Y0,lw=Lw}, #text{label=Label,bef=BefC,aft=AftC}, true) ->
    Y = Y0 + ?CHAR_HEIGHT,
    wings_io:text_at(X0, Y, Label),
    wings_io:text_at(X0+Lw, Y, reverse(BefC)),
    X1 = X0+Lw+length(BefC)*?CHAR_WIDTH,
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

get_text(#text{bef=Bef,aft=Aft}) ->
    reverse(Bef, Aft).

key(?SDLK_KP_PLUS, _, #text{integer=true}=Ts) ->
    increment(Ts, 1);
key(?SDLK_KP_MINUS, _, #text{integer=true}=Ts) ->
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

key($+, #text{integer=true}=Ts) ->
    increment(Ts, 1);
key($=, #text{integer=true}=Ts) ->		%Same key as plus on American keybd.
    increment(Ts, 1);
key($-, #text{integer=true}=Ts) ->
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
