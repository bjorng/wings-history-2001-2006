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
%%     $Id: wings_ask.erl,v 1.5 2002/02/16 15:15:49 bjorng Exp $
%%

-module(wings_ask).
-export([ask/3,ask/4]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(HMARGIN, 16).
-define(VMARGIN, 8).

-define(IS_SHIFTED(Mod), ((Mod) band ?SHIFT_BITS =/= 0)).
-import(lists, [reverse/1,reverse/2,duplicate/2]).

-record(s,
	{w,
	 h,
	 ox,
	 oy,
	 call,
	 focus,
	 fi,					%Static data for all fields.
	 priv,					%States for all fields.
	 coords,				%Coordinates for hit testing.
	 redraw
	}).

%% Static data for each field.
-record(fi,
	{handler,				%Handler fun.
	 inert=true,				%Inert field.
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
    S1 = init_origin(S0),
    S = next_focus(S1, 1),
    {seq,{push,dummy},get_event(S)}.

init_origin(#s{w=Xs,h=Ys}=S) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    Tx = case (W-Xs)/2 of
	     SmallX when SmallX < 10 -> 10;
	     Tx0 -> Tx0
	 end,
    Ty = case (H-Ys)/2 of
	     SmallY when SmallY < 36 -> 36;
	     Ty0 -> Ty0
	 end,
    S#s{ox=Tx,oy=Ty}.

get_event(S) ->
    redraw(S),
    {replace,fun(Ev) -> event(Ev, S) end}.

event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=Unicode}}, S) ->
    event_key({key,Sym,Mod,Unicode}, S);
event(#mousebutton{button=1,x=X,y=Y}=Ev, S) ->
    mouse_event(X, Y, Ev, S);
event(#mousemotion{state=Bst}=Ev, S) when Bst band ?SDL_BUTTON_LMASK == 0 ->
    keep;
event(#mousemotion{x=X,y=Y}=Ev, S) ->
    mouse_event(X, Y, Ev, S);
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

mouse_event(X0, Y0, Ev, #s{fi=Fis,ox=Ox,oy=Oy,w=W,h=H}=S0) ->
    X = X0-Ox,
    Y = Y0-Oy,
    case mouse_to_field(1, Fis, X, Y) of
	none -> keep;
	I ->
	    case Ev of
		#mousebutton{state=?SDL_PRESSED} ->
		    S = set_focus(I, S0),
		    field_event(Ev#mousebutton{x=X,y=Y}, S);
		#mousebutton{} ->
		    field_event(Ev#mousebutton{x=X,y=Y}, I, S0);
		#mousemotion{} ->
		    field_event(Ev#mousemotion{x=X,y=Y}, I, S0)
	    end
    end.

mouse_to_field(I, Fis, X, Y) when I > size(Fis) -> none;
mouse_to_field(I, Fis, X, Y) ->
    case element(I, Fis) of
	#fi{inert=false,x=Lx,y=Uy,lw=Lw,w=W,h=H}
	when Lx =< X, X < Lx+Lw+W,
	     Uy =< Y, Y < Uy+H -> I;
	_Other -> mouse_to_field(I+1, Fis, X, Y)
    end.

next_focus(#s{focus=I}=S, Dir) ->
    next_focus_1(I, Dir, S).

next_focus_1(I0, Dir, #s{fi=Fis,priv=Priv}=S) ->
    I = case I0+Dir of
	    I1 when 0 < I1, I1 =< size(Fis) -> I1;
	    0 -> size(Fis);
	    _ -> 1
	end,
    case element(I, Fis) of
	#fi{inert=true} -> next_focus_1(I, Dir, S);
	_ -> set_focus(I, S)
    end.

set_focus(I, #s{focus=OldFocus,fi=Fis,priv=Priv0}=S) ->
    #fi{handler=OldHandler} = OldFi = element(OldFocus, Fis),
    OldFst0 = element(I, Priv0),
    OldFst = OldHandler({event,{focus,false}}, OldFi, OldFst0),
    Priv1 = setelement(I, Priv0, OldFst),

    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv1),
    Fst = Handler({event,{focus,true}}, Fi, Fst0),
    Priv = setelement(I, Priv1, Fst),
    
    S#s{focus=I,priv=Priv}.
    
field_event(Ev, #s{focus=I}=S) ->
    field_event(Ev, I, S).

field_event(Ev, I, #s{call=EndFun,fi=Fis,priv=Priv0}=S) ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv0),
    Fst = Handler({event,Ev}, Fi, Fst0),
    Priv = setelement(I, Priv0, Fst),
    get_event(S#s{priv=Priv}).

return_result(#s{fi=Fis,priv=Priv}=S) ->
    return_result(1, Fis, Priv, S, []).

return_result(I, Fis, Priv, S, Acc) when I =< size(Fis) ->
    case element(I, Fis) of
	#fi{inert=true} ->
	    return_result(I+1, Fis, Priv, S, Acc);
	#fi{handler=Handler,flags=Flags}=Fi ->
	    Fst = element(I, Priv),
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
    
redraw(#s{w=W,h=H,ox=Ox,oy=Oy,redraw=Redraw,focus=Focus,fi=Fi,priv=Priv}) ->
    case Redraw of
	#st{}=St ->
	    wings_draw:render(St),
	    wings_io:draw_ui(St);
	Other ->
	    Redraw()
    end,
    wings_io:ortho_setup(),
    gl:translated(Ox, Oy, 0),
    wings_io:raised_rect(-?HMARGIN, -?VMARGIN,
			 W+2*?HMARGIN, H+2*?VMARGIN,
			 ?MENU_COLOR),
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
    {Fi#fi{handler=Fun,inert=true,lw=Lw,w=W,h=H,flags=Flags},{vframe,Fields}}.

hframe([Q|Qs], #fi{x=X}=Fi0, Flags, Acc) ->
    {#fi{lw=Lw,w=W}=Fi,Priv} = normalize(Q, Fi0),
    hframe(Qs, Fi#fi{x=X+Lw+W}, Flags, [{Fi,Priv}|Acc]);
hframe([], _, Flags, Fields0) ->
    [{Fi,_}|_] = Fields = reverse(Fields0),
    {Lw0,W0,H0} = frame_init_size(Flags),
    {Lw,W,H} = hframe_size(Fields, Lw0, W0, H0),
    Fun = frame_fun(),
    {Fi#fi{handler=Fun,inert=true,lw=Lw,w=W,h=H,flags=Flags},{vframe,Fields}}.

normalize_field({Handler,Inert,Priv,Lw,W,H}, Flags, Fi) ->
    {Fi#fi{handler=Handler,inert=Inert,flags=Flags,w=W,h=H,lw=Lw},Priv}.

frame_init_size(Flags) ->
    case property_lists:is_defined(border, Flags) of
	true -> {0,2*10,2*10};
	false -> {0,0,0}
    end.
    
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
       ({event,Ev}, Fi, Frame) -> Frame
    end.

%%%
%%% Separator.
%%%

separator() ->
    Fun = separator_fun(),
    {Fun,true,no_state,0,4*?CHAR_WIDTH,10}.

separator_fun() ->
    fun({redraw,Active}, Fi, _Dummy) ->
	    separator_draw(Active, Fi);
       ({event,Ev}, Fi, Sep) ->
	    Sep
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
    {Fun,false,Cb,0,(length(Label)+5)*?CHAR_WIDTH,?LINE_HEIGHT}.

checkbox_fun() ->
    fun({redraw,Active}, Fi, Cb) ->
	    cb_draw(Active, Fi, Cb);
       ({event,Ev}, Fi, Cb) ->
	    cb_event(Ev, Cb);
       (value, Fi, #cb{state=State}) ->
	    State
    end.

cb_draw(Active, #fi{x=X,y=Y0}, #cb{label=Label,state=State}) ->
    wings_io:sunken_rect(X, Y0+6, ?CHAR_WIDTH+1, ?CHAR_WIDTH+1, {1,1,1}),
    Y = Y0+?CHAR_HEIGHT,
    case State of
	false -> ok;
	true -> wings_io:text_at(X+1, Y, [crossmark])
    end,
    wings_io:text_at(X+2*?CHAR_WIDTH, Y, Label),
    if
	Active == true ->
	    wings_io:text_at(X+2*?CHAR_WIDTH, Y,
			     duplicate(length(Label), $_));
	true -> ok
    end.

cb_event({key,_,_,$\s}, #cb{state=State}=Cb) ->
    Cb#cb{state=not State};
cb_event(#mousebutton{x=X,y=Y,state=?SDL_RELEASED},
	 #cb{label=Label,state=State}=Cb) ->
    if 
	X < (4+length(Label))*?CHAR_WIDTH ->
	    Cb#cb{state=not State};
	true -> Cb
    end;
cb_event(Ev, Cb) ->
    Cb.

%%%
%%% Text and number input fields.
%%%

-record(text,
	{bef,
	 aft,
	 sel=0,
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
    Ts = #text{label=Label,bef=[],aft=String,integer=IsInteger},
    {Fun,false,Ts,(length(Label)+1)*?CHAR_WIDTH,
     Max*?CHAR_WIDTH,?LINE_HEIGHT+3}.

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
    text_event(Ev, Fi, Ts).

text_event({key,Sym,Mod,Unicode}, Fi, Ts) ->
    key(Sym, Mod, Unicode, Ts);
text_event({focus,true}, Fi, #text{bef=Bef,aft=Aft}=Ts) ->
    Str = reverse(Bef, Aft),
    Ts#text{bef=[],sel=length(Str),aft=Str};
text_event(#mousebutton{x=X,y=Y,state=?SDL_PRESSED}, Fi, Ts) ->
    text_pos(X, Fi, Ts);
text_event(#mousebutton{x=X,y=Y,state=?SDL_RELEASED}, Fi, Ts) ->
    text_sel(X, Fi, Ts);
text_event(#mousemotion{x=X,y=Y}, Fi, Ts) ->
    text_sel(X, Fi, Ts);
text_event(_Ev, _Fi, Ts) -> Ts.

text_pos(Mx0, #fi{x=X,y=Y,lw=Lw}, Ts) ->
    text_pos_1(round((Mx0-X-Lw)/?CHAR_WIDTH), Ts).

text_pos_1(Mx, #text{bef=[C|Bef],aft=Aft}=Ts) when Mx < length(Bef) ->
    text_pos_1(Mx, Ts#text{bef=Bef,aft=[C|Aft]});
text_pos_1(Mx, #text{bef=Bef,aft=[C|Aft]}=Ts) when Mx > length(Bef) ->
    text_pos_1(Mx, Ts#text{bef=[C|Bef],aft=Aft});
text_pos_1(Mx, #text{bef=Bef}=Ts) ->
    Ts#text{sel=0}.

text_sel(Mx0, #fi{x=X,y=Y,lw=Lw}, Ts) ->
    text_sel_1(round((Mx0-X-Lw)/?CHAR_WIDTH), Ts).

text_sel_1(Mx, #text{bef=Bef,aft=Aft}=Ts) when Mx < length(Bef) ->
    Ts#text{sel=max(Mx, 0)-length(Bef)};
text_sel_1(Mx, #text{bef=Bef,aft=Aft}=Ts) ->
    Ts#text{sel=min(Mx-length(Bef),length(Aft))};
text_sel_1(Mx, Ts) -> Ts.

key(?SDLK_KP_PLUS, _, _, #text{integer=true}=Ts) ->
    increment(Ts, 1);
key(?SDLK_KP_MINUS, _, _, #text{integer=true}=Ts) ->
    increment(Ts, -1);
key(?SDLK_HOME, Mod, _, Ts) -> key(1, Mod, Ts);
key(?SDLK_END, Mod, _, Ts) -> key(5, Mod, Ts);
key(?SDLK_LEFT, Mod, _, Ts) -> key(2, Mod, Ts);
key(?SDLK_RIGHT, Mod, _, Ts) -> key(6, Mod, Ts);
key(?SDLK_DELETE, Mod, _, Ts) -> key(4, Mod, Ts);
key(?SDLK_KP_PERIOD, Mod, _, Ts) ->
    key($., Mod, Ts);
key(C, Mod, _, Ts) when ?SDLK_KP0 =< C, C =< ?SDLK_KP9 ->
    key(C-?SDLK_KP0+$0, Mod, Ts);
key(Other, Mod, Unicode, Ts) ->
    key(Unicode, Mod, Ts).

key($+, Mod, #text{integer=true}=Ts) ->
    increment(Ts, 1);
key($=, Mod, #text{integer=true}=Ts) ->		%Same key as plus on American keybd.
    increment(Ts, 1);
key($-, Mod, #text{integer=true}=Ts) ->
    increment(Ts, -1);
key($\b, Mod, #text{sel=0,bef=[_|Bef]}=Ts) ->	%Bksp (no selection).
    Ts#text{bef=Bef};
key($\b, Mod, Ts) ->				%Bksp (selection).
    del_sel(Ts);
key(2, Mod, #text{sel=Sel,bef=Bef}=Ts) when ?IS_SHIFTED(Mod) ->
    if
	-length(Bef) < Sel -> Ts#text{sel=Sel-1};
	true -> Ts
    end;
key(2, Mod, #text{bef=[C|Bef],aft=Aft}=Ts) ->	%Ctrl-B
    Ts#text{sel=0,bef=Bef,aft=[C|Aft]};
key(2, Mod, Ts) ->				%Ctrl-B
    Ts#text{sel=0};
key(6, Mod, #text{sel=Sel,aft=Aft}=Ts) when ?IS_SHIFTED(Mod) ->
    if
	Sel < length(Aft) -> Ts#text{sel=Sel+1};
	true -> Ts
    end;
key(6, Mod, #text{bef=Bef,aft=[C|Aft]}=Ts) ->	%Ctrl-F
    Ts#text{sel=0,bef=[C|Bef],aft=Aft};
key(1, Mod, #text{sel=Sel,bef=Bef}=Ts) when ?IS_SHIFTED(Mod) ->
    Ts#text{sel=-length(Bef)};
key(1, Mod, #text{bef=Bef,aft=Aft}=Ts) ->	%Ctrl-A
    Ts#text{sel=0,bef=[],aft=reverse(Bef, Aft)};
key(5, Mod, #text{sel=Sel,aft=Aft}=Ts) when ?IS_SHIFTED(Mod) ->
    Ts#text{sel=length(Aft)};
key(5, Mod, #text{bef=Bef,aft=Aft}=Ts) ->	%Ctrl-E
    Ts#text{sel=0,bef=reverse(Aft, Bef),aft=[]};
key(11, Mod, #text{}=Ts) ->			%Ctrl-K
    Ts#text{aft=[]};
key(4, Mod, #text{sel=0,aft=[_|Aft]}=Ts) ->	%Ctrl-D
    Ts#text{aft=Aft};
key(4, Mod, Ts) ->				%Ctrl-D
    del_sel(Ts);
key(C, Mod, #text{bef=Bef0}=Ts0) when $\s =< C, C < 256 ->
    del_sel(Ts0#text{bef=[C|Bef0]});
key(C, Mod, Ts) -> Ts.

del_sel(#text{sel=Sel,bef=Bef}=Ts) when Sel < 0 ->
    Ts#text{sel=0,bef=lists:nthtail(-Sel, Bef)};
del_sel(#text{sel=Sel,aft=Aft}=Ts) when Sel > 0 ->
    Ts#text{sel=0,aft=lists:nthtail(Sel, Aft)};
del_sel(Ts) -> Ts.

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

draw_text(#fi{x=X0,y=Y0,w=W,lw=Lw,h=H},
	  #text{label=Label,sel=Sel,bef=Bef0,aft=Aft}, Active) ->
    wings_io:sunken_rect(X0+Lw-4, Y0+2, W+8, ?CHAR_HEIGHT+2, {1,1,1}),
    Y = Y0 + ?CHAR_HEIGHT,
    wings_io:text_at(X0, Y, Label),
    Bef = reverse(Bef0),
    wings_io:text_at(X0+Lw, Y, Bef),
    wings_io:text(Aft),
    if
	not Active -> ok;
	Sel < 0 ->
	    gl:color3f(0, 0, 0.5),
	    Skip = length(Bef)+Sel,
	    X1 = X0+Lw+Skip*?CHAR_WIDTH,
 	    gl:recti(X1, Y-?CHAR_HEIGHT+3, X1-Sel*?CHAR_WIDTH, Y+2),
 	    gl:color3f(1, 1, 1),
	    wings_io:text_at(X1, Y, lists:nthtail(Skip, Bef)),
	    gl:color3f(0, 0, 0);
	Sel > 0 ->
	    gl:color3f(0, 0, 0.5),
	    X1 = X0+Lw+length(Bef)*?CHAR_WIDTH,
 	    gl:recti(X1, Y-?CHAR_HEIGHT+3, X1+Sel*?CHAR_WIDTH, Y+2),
 	    gl:color3f(1, 1, 1),
	    draw_text_1(X1, Y, Aft, Sel);
	true ->
	    gl:color3f(1, 0, 0),
	    X1 = X0+Lw+length(Bef)*?CHAR_WIDTH,
	    wings_io:text_at(X1, Y, [caret]),
	    gl:color3f(0, 0, 0)
    end.

draw_text_1(X, Y, T, 0) ->
    gl:color3f(0, 0, 0);
draw_text_1(X, Y, [C|T], N) ->
    wings_io:text_at(X, Y, [C]),
    draw_text_1(X+?CHAR_WIDTH, Y, T, N-1).

get_text(#text{bef=Bef,aft=Aft}) ->
    reverse(Bef, Aft).

max(A, B) when A > B -> A;
max(A, B) -> B.

min(A, B) when A < B -> A;
min(A, B) -> B.
