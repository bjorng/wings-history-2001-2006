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
%%     $Id: wings_ask.erl,v 1.15 2002/04/11 08:20:39 bjorng Exp $
%%

-module(wings_ask).
-export([ask/3,ask/4,dialog/3,dialog/4]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(HMARGIN, 16).
-define(VMARGIN, 8).

-define(HFRAME_SPACING, (3*?CHAR_WIDTH div 2)).

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
	 common=gb_trees:empty(),		%Data common for all fields.
	 redraw
	}).

%% Static data for each field.
-record(fi,
	{handler,				%Handler fun.
	 key,					%Field key.
	 inert=true,				%Inert field.
	 flags,					%Flags field.
	 x,y,					%Upper left position.
	 w,h,					%Width, height.
	 ipadx=0,ipady=0			%Internal padding.
	}).

ask(Qs, St, Fun) ->
    ask(true, Qs, St, Fun).

ask(Bool, Qs0, St, Fun) ->
    {Labels,Vals} = ask_unzip(Qs0),
    Qs = [{hframe,
	   [{vframe,Labels},
	    {vframe,Vals}]}],
    dialog(Bool, Qs, St, Fun).

ask_unzip(Qs) ->
    ask_unzip(Qs, [], []).
ask_unzip([{Label,Def}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def}|AccB]);
ask_unzip([{Label,Def,Flags}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def,Flags}|AccB]);
ask_unzip([], Labels, Vals) ->
    {reverse(Labels),reverse(Vals)}.

dialog(false, Qs, St, Fun) ->
    S = setup_ask(Qs, St, Fun),
    return_result(S),
    keep;
dialog(true, Qs, St, Fun) -> dialog(Qs, St, Fun).

dialog(Qs, Redraw, Fun) ->
    S0 = setup_ask(Qs, Redraw, Fun),
    S1 = init_origin(S0),
    S = next_focus(S1, 1),
    {seq,{push,dummy},get_event(S)}.

setup_ask(Qs0, Redraw, Fun) ->
    Qs1 = normalize(Qs0),
    Qs = propagate_sizes(Qs1),
    {Fis0,Priv0} = flatten_fields(Qs),
    Fis1 = insert_keys(Fis0, 1),
    Fis = list_to_tuple(Fis1),
    Priv = list_to_tuple(Priv0),
    {#fi{w=W,h=H},_} = Qs,
    S = #s{w=W,h=H,call=Fun,fi=Fis,priv=Priv,focus=size(Fis),redraw=Redraw},
    init_fields(1, size(Priv), S).

insert_keys([#fi{flags=Flags}=Fi|T], I) ->
    Key = case property_lists:get_value(key, Flags) of
	      undefined -> I;
	      Key0 -> Key0
	  end,
    [Fi#fi{key=Key}|insert_keys(T, I+1)];
insert_keys([], _) -> [].

init_fields(I, N, #s{fi=Fis,priv=Priv0,common=Common0}=S) when I =< N ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv0),
    case Handler({event,init}, Fi, Fst0, Common0) of
	{Fst,Common} when is_atom(element(1, Fst)), is_tuple(Common) ->
	    Priv = setelement(I, Priv0, Fst),
	    init_fields(I+1, N, S#s{priv=Priv,common=Common});
	Fst when is_atom(element(1, Fst)) ->
	    Priv = setelement(I, Priv0, Fst),
	    init_fields(I+1, N, S#s{priv=Priv})
    end;
init_fields(_, _, S) -> S.

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
    wings_io:swap_buffers(),
    {replace,fun(Ev) -> event(Ev, S) end}.

event(#keyboard{keysym=#keysym{sym=Sym,mod=Mod,unicode=Unicode}}, S) ->
    event_key({key,Sym,Mod,Unicode}, S);
event(#mousebutton{button=1,x=X,y=Y}=Ev, S) ->
    mouse_event(X, Y, Ev, S);
event(#mousemotion{state=Bst}, _S) when Bst band ?SDL_BUTTON_LMASK == 0 ->
    keep;
event(#mousemotion{x=X,y=Y}=Ev, S) ->
    mouse_event(X, Y, Ev, S);
event(#resize{}=Ev, _S) ->
    wings_io:putback_event(Ev),
    pop;
event({action,{update,I,{Fst,Common}}}, #s{priv=Priv0}=S)
  when is_atom(element(1, Fst)), is_tuple(Common) ->
    Priv = setelement(I, Priv0, Fst),
    get_event(S#s{priv=Priv,common=Common});
event({action,{update,I,Fst}}, #s{priv=Priv0}=S) ->
    Priv = setelement(I, Priv0, Fst),
    get_event(S#s{priv=Priv});
event(Ev, S) -> field_event(Ev, S).

event_key({key,?SDLK_ESCAPE,_,_}, _S) ->
    wings_io:putback_event(redraw),
    pop;
event_key({key,?SDLK_TAB,Mod,_}, S) when Mod band ?SHIFT_BITS =/= 0 ->
    get_event(next_focus(S, -1));
event_key({key,?SDLK_TAB,_,_}, S) ->
    get_event(next_focus(S, 1));
event_key({key,_,_,$\t}, S) ->
    get_event(next_focus(S, 1));
event_key({key,?SDLK_KP_ENTER,_,_}, S) ->
    return_result(S);
event_key({key,_,_,$\r}, S) ->
    return_result(S);
event_key(Ev, S) ->
    field_event(Ev, S).

mouse_event(X0, Y0, #mousemotion{}=Ev, #s{focus=I,ox=Ox,oy=Oy}=S) ->
    X = X0-Ox,
    Y = Y0-Oy,
    field_event(Ev#mousemotion{x=X,y=Y}, I, S);
mouse_event(X0, Y0, Ev, #s{fi=Fis,ox=Ox,oy=Oy}=S0) ->
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

mouse_to_field(I, Fis, _X, _Y) when I > size(Fis) -> none;
mouse_to_field(I, Fis, X, Y) ->
    case element(I, Fis) of
	#fi{inert=false,x=Lx,y=Uy,w=W,h=H}
	when Lx =< X, X < Lx+W,
	     Uy =< Y, Y < Uy+H -> I;
	_Other -> mouse_to_field(I+1, Fis, X, Y)
    end.

next_focus(#s{focus=I}=S, Dir) ->
    next_focus_1(I, Dir, S).

next_focus_1(I0, Dir, #s{fi=Fis}=S) ->
    I = case I0+Dir of
	    I1 when 0 < I1, I1 =< size(Fis) -> I1;
	    0 -> size(Fis);
	    _ -> 1
	end,
    case element(I, Fis) of
	#fi{inert=true} -> next_focus_1(I, Dir, S);
	_ -> set_focus(I, S)
    end.

set_focus(I, #s{focus=OldFocus,fi=Fis,priv=Priv0,common=Common0}=S) ->
    #fi{handler=OldHandler} = OldFi = element(OldFocus, Fis),
    OldFst0 = element(OldFocus, Priv0),
    
    {OldFst,Common2} = 
	case OldHandler({event,{focus,false}}, OldFi, OldFst0, Common0) of
	    {Ofst,Common1} when is_atom(element(1, Ofst)), is_tuple(Common1) ->
		{Ofst,Common1};
	    Ofst when is_atom(element(1, Ofst)) ->
		{Ofst,Common0}
	end,
    Priv1 = setelement(OldFocus, Priv0, OldFst),

    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv1),
    {Fst,Common} =
	case Handler({event,{focus,true}}, Fi, Fst0, Common2) of
	    {Fst1,Common3} when is_atom(element(1, Fst1)), is_tuple(Common3) ->
		{Fst1,Common3};
	    Fst1 when is_atom(element(1, Fst1)) ->
		{Fst1,Common2}
	end,
    Priv = setelement(I, Priv1, Fst),

    S#s{focus=I,priv=Priv,common=Common}.

field_event(Ev, #s{focus=I}=S) ->
    field_event(Ev, I, S).

field_event(Ev, I, #s{fi=Fis,priv=Priv0,common=Common0}=S) ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv0),
    case Handler({event,Ev}, Fi, Fst0, Common0) of
	ok ->
	    return_result(S);
	cancel ->
	    wings_io:putback_event(redraw),
	    pop;
	{dialog,Qs,Fun} ->
	    recursive_dialog(I, Qs, Fun, S);
	{Fst,Common} when is_atom(element(1, Fst)), is_tuple(Common) ->
	    Priv = setelement(I, Priv0, Fst),
	    get_event(S#s{priv=Priv,common=Common});
	Fst when is_atom(element(1, Fst)) ->
	    Priv = setelement(I, Priv0, Fst),
	    get_event(S#s{priv=Priv})
    end.

recursive_dialog(I, Qs, Fun, S0) ->
    S = S0#s{focus=none},
    Redraw = fun() ->
		     redraw(S)
	     end,
    dialog(Qs, Redraw, fun(Vs) -> {update,I,Fun(Vs)} end).

return_result(#s{fi=Fis,priv=Priv}=S) ->
    return_result(1, Fis, Priv, S, []).

return_result(I, Fis, Priv, #s{common=Common}=S, Acc) when I =< size(Fis) ->
    case element(I, Fis) of
	#fi{inert=true} ->
	    return_result(I+1, Fis, Priv, S, Acc);
	#fi{handler=Handler,flags=Flags}=Fi ->
	    Fst = element(I, Priv),
	    case catch Handler(value, Fi, Fst, Common) of
		{'EXIT',Reason} ->
		    exit(Reason);
		{command_error,Error} ->
		    wings_util:message(Error),
		    get_event(S#s{focus=I});
		none ->
		    return_result(I+1, Fis, Priv, S, Acc);
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
	#st{}=St ->
	    wings_io:putback_event({new_state,St}),
	    pop;
	Action when is_tuple(Action); is_atom(Action) ->
	    wings_io:putback_event({action,Action}),
	    pop
    end.

redraw(#s{w=W,h=H,ox=Ox,oy=Oy,redraw=Redraw,focus=Focus,
	  fi=Fi,priv=Priv,common=Common}) ->
    case Redraw of
	#st{}=St ->
	    wings_draw:render(St),
	    wings_io:draw_ui(St);
	_Other ->
	    Redraw()
    end,
    wings_io:ortho_setup(),
    gl:translated(Ox, Oy, 0),
    wings_io:raised_rect(-?HMARGIN, -?VMARGIN,
			 W+2*?HMARGIN, H+2*?VMARGIN,
			 ?MENU_COLOR),
    draw_fields(1, Fi, Priv, Focus, Common).

draw_fields(I, Fis, Priv, Focus, Common) when I =< size(Fis) ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst = element(I, Priv),
    Handler({redraw,I =:= Focus}, Fi, Fst, Common),
    draw_fields(I+1, Fis, Priv, Focus, Common);
draw_fields(_, _, _, _, _) -> ok.

%%%
%%% Conversion of queries to internal format and dimension calculation.
%%%

normalize(Qs) when is_list(Qs) ->
    normalize({hframe,[{vframe,Qs},
		       {vframe,[{button,ok},
				{button,cancel}]}]});
normalize(Qs) ->
    normalize(Qs, #fi{x=0,y=0}).

normalize({label_column,Qs0}, Fi) ->
    {Labels,Fields} = dialog_unzip(Qs0),
    Qs = {hframe,
	  [{vframe,Labels},
	   {vframe,Fields}]},
    normalize(Qs, Fi);
normalize({vframe,Qs}, Fi) ->
    vframe(Qs, Fi, []);
normalize({vframe,Qs,Flags}, Fi) ->
    vframe(Qs, Fi, Flags);
normalize({hframe,Qs}, Fi) ->
    hframe(Qs, Fi, []);
normalize({hframe,Qs,Flags}, Fi) ->
    hframe(Qs, Fi, Flags);
normalize({label,Label}, Fi) ->
    normalize_field(label(Label), [], Fi);
normalize({color,Def}, Fi) ->
    normalize_field(color(Def), [], Fi);
normalize({color,Def,Flags}, Fi) ->
    normalize_field(color(Def), Flags, Fi);
normalize({alt,VarDef,Prompt,Val}, Fi) ->
    normalize_field(radiobutton(VarDef, Prompt, Val), [], Fi);
normalize({button,Action}, Fi) ->
    Label = button_label(Action),
    normalize_field(button(Label, Action), [], Fi);
normalize({button,Label,Action}, Fi) ->
    normalize_field(button(Label, Action), [], Fi);
normalize({custom,W,H,Custom}, Fi) ->
    normalize_field(custom(W, H, Custom), [], Fi);
normalize({slider,Field}, Fi) ->
    normalize({hframe,[Field,{internal_slider,Field}]}, Fi);
normalize({internal_slider,Field}, Fi) ->
    normalize_field(slider(Field), [], Fi);
normalize(separator, Fi) ->
    normalize_field(separator(), [], Fi);
normalize({text,Def}, Fi) ->
    normalize_field(text_field(Def, []), [], Fi);
normalize({text,Def,Flags}, Fi) ->
    normalize_field(text_field(Def, Flags), Flags, Fi);
normalize({Prompt,Def}, Fi) when Def == false; Def == true ->
    normalize_field(checkbox(Prompt, Def), [], Fi);
normalize({Prompt,Def,Flags}, Fi) when Def == false; Def == true ->
    normalize_field(checkbox(Prompt, Def), Flags, Fi).

vframe(Qs, #fi{x=X,y=Y0}=Fi0, Flags) ->
    {Dx,Dy} = case have_border(Flags) of
		  true -> {10,?LINE_HEIGHT};
		  false -> {0,0}
	      end,
    {Fields,Y,W0} = vframe_1(Qs, Fi0#fi{x=X+Dx,y=Y0+Dy}, 0, []),
    H0 = Y-Y0,
    {Ipadx,Ipady} = case have_border(Flags) of
			true -> {2*10,10};
			false -> {0,0}
		    end,
    W = W0 + Ipadx,
    H = H0 + Ipady,
    Fun = frame_fun(),
    Fi = Fi0#fi{handler=Fun,inert=true,flags=Flags,
		w=W,h=H,ipadx=Ipadx,ipady=Ipady},
    {Fi,{vframe,Fields}}.

vframe_1([Q|Qs], #fi{y=Y}=Fi0, W0, Acc) ->
    {#fi{w=W,h=H}=Fi,Priv} = normalize(Q, Fi0),
    vframe_1(Qs, Fi#fi{y=Y+H}, max(W0, W), [{Fi,Priv}|Acc]);
vframe_1([], #fi{y=Y}, W, Fields) ->
    {reverse(Fields),Y,W}.

hframe(Qs, #fi{x=X0,y=Y}=Fi, Flags) ->
    {Dx0,Dy0} = case have_border(Flags) of
		    true -> {10,?LINE_HEIGHT};
		    false -> {0,0}
		end,
    {Fields,X,H0} = hframe_1(Qs, Fi#fi{x=X0+Dx0,y=Y+Dy0}, 0, []),
    W0 = X-X0-Dx0,
    {Ipadx,Ipady} = case have_border(Flags) of
			true -> {2*10,?LINE_HEIGHT+10};
			false -> {0,0}
		    end,
    W = W0 + Ipadx,
    H = H0 + Ipady,
    Fun = frame_fun(),
    {Fi#fi{handler=Fun,inert=true,flags=Flags,
	   w=W,h=H,ipadx=Ipadx,ipady=Ipady},
     {hframe,Fields}}.

hframe_1([Q|Qs], #fi{x=X}=Fi0, H0, Acc) ->
    {#fi{w=W,h=H}=Fi,Priv} = normalize(Q, Fi0),
    hframe_1(Qs, Fi#fi{x=X+W+?HFRAME_SPACING},
	     max(H0, H), [{Fi,Priv}|Acc]);
hframe_1([], #fi{x=X}, H, Fields0) ->
    {reverse(Fields0),X-?HFRAME_SPACING,H}.

normalize_field({Handler,Inert,Priv,W,H}, Flags, Fi) ->
    {Fi#fi{handler=Handler,inert=Inert,flags=Flags,w=W,h=H},Priv}.

%%%
%%% Propagate changed sizes to all fields.
%%%

propagate_sizes({Fi,{vframe,Fields}}) ->
    vframe_propagate(Fields, Fi, []);
propagate_sizes({Fi,{hframe,Fields}}) ->
    hframe_propagate(Fields, Fi, []);
propagate_sizes(Other) -> Other.

vframe_propagate([{Fi,Priv}|Fis], #fi{w=SzW,ipadx=Ipadx}=Sz, Acc) ->
    {#fi{w=W},_} = New = propagate_sizes({Fi#fi{w=SzW-Ipadx},Priv}),
    vframe_propagate(Fis, Sz#fi{w=max(W+Ipadx, SzW)}, [New|Acc]);
vframe_propagate([], Sz, Acc) -> {Sz,{vframe,reverse(Acc)}}.

hframe_propagate([{Fi,Priv}|Fis], Sz, Acc) ->
    #fi{w=SzW,h=H,ipady=Ipady} = Sz,
    New = propagate_sizes({Fi#fi{h=H-Ipady},Priv}),
    hframe_propagate(Fis, Sz#fi{w=SzW}, [New|Acc]);
hframe_propagate([], Fi, Acc) -> {Fi,{hframe,reverse(Acc)}}.

%%%
%%% Calculate flattened fields array.
%%%

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
    fun({redraw,_Active}, Fi, _, _) ->
	    frame_redraw(Fi);
       ({event,_Ev}, _Fi, Frame, _) -> Frame
    end.

frame_redraw(#fi{flags=[]}) -> ok;
frame_redraw(#fi{x=X,y=Y0,w=W,h=H0,flags=Flags}) ->
    case property_lists:get_value(title, Flags) of
	undefined -> ok;
	Title ->
	    Y = Y0 + ?CHAR_HEIGHT div 2 + 3,
	    H = H0 - (Y-Y0) - 4,
	    gl:'begin'(?GL_LINES),
	    vline(X+W-2, Y, H),
	    hline(X, Y, W),
	    hline(X, Y+H-1, W-1),
	    vline(X, Y+1, H-2),
	    gl:'end'(),
	    gl:color3fv(?MENU_COLOR),
	    TextPos = X + 3*?CHAR_WIDTH,
	    gl:rectf(TextPos-?CHAR_WIDTH, Y-1,
		     TextPos+(length(Title)+1)*?CHAR_WIDTH, Y+2),
	    gl:color3f(0, 0, 0),
	    wings_io:text_at(TextPos, Y0+?CHAR_HEIGHT, Title)
    end.

hline(X0, Y0, W) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    gl:color3f(0.5, 0.5, 0.5),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+W, Y),
    gl:color3f(1, 1, 1),
    gl:vertex2f(X, Y+1),
    gl:vertex2f(X+W, Y+1).

vline(X0, Y0, H) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    gl:color3f(0.5, 0.5, 0.5),
    gl:vertex2f(X, Y),
    gl:vertex2f(X, Y+H),
    gl:color3f(1, 1, 1),
    gl:vertex2f(X+1, Y),
    gl:vertex2f(X+1, Y+H).

have_border(Flags) ->
    property_lists:is_defined(title, Flags).

%%%
%%% Separator.
%%%

separator() ->
    Fun = separator_fun(),
    {Fun,true,{},4*?CHAR_WIDTH,10}.

separator_fun() ->
    fun({redraw,_Active}, Fi, _Dummy, _) ->
	    separator_draw(Fi);
       ({event,_Ev}, _Fi, Sep, _) ->
	    Sep
    end.

separator_draw(#fi{x=X,y=Y,w=W}) ->
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
    {Fun,false,Cb,(length(Label)+2)*?CHAR_WIDTH,?LINE_HEIGHT+2}.

checkbox_fun() ->
    fun({redraw,Active}, Fi, Cb, _) ->
	    cb_draw(Active, Fi, Cb);
       ({event,Ev}, Fi, Cb, _) ->
	    cb_event(Ev, Fi, Cb);
       (value, _Fi, #cb{state=State}, _) ->
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

cb_event({key,_,_,$\s}, _, #cb{state=State}=Cb) ->
    Cb#cb{state=not State};
cb_event(#mousebutton{x=Xb,state=?SDL_RELEASED},
	 #fi{x=X},
	 #cb{label=Label,state=State}=Cb) ->
    if
	Xb-X < (4+length(Label))*?CHAR_WIDTH ->
	    Cb#cb{state=not State};
	true -> Cb
    end;
cb_event(_Ev, _Fi, Cb) -> Cb.

%%%
%%% Radio buttons.
%%%

-record(rb,
	{var,
	 val,
	 label}).

radiobutton({Var,Def}, Label, Val) ->
    Rb = #rb{var=Var,val=Val,label=Label},
    Fun = radiobutton_fun(Def),
    {Fun,false,Rb,(length(Label)+2)*?CHAR_WIDTH,?LINE_HEIGHT+2}.

radiobutton_fun(Def) ->
    fun({event,init}, _Fi, #rb{var=Var,val=Val}=Rb, Common) ->
	    case Val of
		Def -> {Rb,gb_trees:insert(Var, Val, Common)};
		_ -> Rb
	    end;
       ({event,Ev}, Fi, Rb, Common) ->
 	    rb_event(Ev, Fi, Rb, Common);
       ({redraw,Active}, Fi, Rb, Common) ->
 	    rb_draw(Active, Fi, Rb, Common);
       (value, _Fi, #rb{var=Var,val=Val}, Common) ->
	    case gb_trees:get(Var, Common) of
		Val -> Val;
		_ -> none
	    end
    end.

rb_draw(Active, #fi{x=X,y=Y0}, #rb{label=Label,var=Var,val=Val}, Common) ->
    Y = Y0+?CHAR_HEIGHT,
    gl:color3f(1, 1, 1),
    White = <<
	     2#00111000,
	     2#01111100,
	     2#11111110,
	     2#11111110,
	     2#11111110,
	     2#01111100,
	     2#00111000>>,
    gl:rasterPos2i(X, Y),
    gl:bitmap(7, 7, -1, 0, 7, 0, White),
    gl:color3f(0, 0, 0),
    B = case gb_trees:get(Var, Common) of
	    Val ->
	       	<<
	       	 2#00111000,
	       	 2#01000100,
	       	 2#10111010,
	       	 2#10111010,
	       	 2#10111010,
	       	 2#01000100,
	       	 2#00111000>>;
	_ ->
	       	<<
	       	 2#00111000,
	       	 2#01000100,
	       	 2#10000010,
	       	 2#10000010,
	       	 2#10000010,
	       	 2#01000100,
	       	 2#00111000>>
    end,
    gl:rasterPos2i(X, Y),
    gl:bitmap(7, 7, -1, 0, 7, 0, B),
    wings_io:text_at(X+2*?CHAR_WIDTH, Y, Label),
    if
	Active == true ->
	    wings_io:text_at(X+2*?CHAR_WIDTH, Y,
			     duplicate(length(Label), $_));
	true -> ok
    end.

rb_event({key,_,_,$\s}, _, Rb, Common) ->
    rb_set(Rb, Common);
rb_event(#mousebutton{x=Xb,state=?SDL_RELEASED},
	 #fi{x=X}, #rb{label=Label}=Rb, Common) ->
    if
	Xb-X < (4+length(Label))*?CHAR_WIDTH ->
	    rb_set(Rb, Common);
	true -> Rb
    end;
rb_event(_Ev, _Fi, Rb, _Common) -> Rb.

rb_set(#rb{var=Var,val=Val}=Rb, Common0) ->
    Common = gb_trees:update(Var, Val, Common0),
    {Rb,Common}.

%%%
%%% Buttons.
%%%

-record(but,
	{label,
	 action}).

button(Label, Action) ->
    But = #but{label=Label,action=Action},
    Fun = button_fun(),
    {Fun,false,But,(length(Label)+2)*?CHAR_WIDTH,?LINE_HEIGHT+6}.

button_label(ok) -> "OK";
button_label(Act) ->
    wings_util:cap(atom_to_list(Act)).

button_fun() ->
    fun({redraw,Active}, Fi, But, _) ->
	    button_draw(Active, Fi, But);
       ({event,Ev}, Fi, But, _) ->
	    button_event(Ev, Fi, But);
       (value, _, _, _) -> none
    end.

button_draw(Active, #fi{x=X,y=Y0,w=W,h=H}, #but{label=Label}) ->
    Y = Y0+?CHAR_HEIGHT,
    wings_io:raised_rect(X, Y-H+9, W, H-5, ?MENU_COLOR),
    TextX = X + (W-length(Label)*?CHAR_WIDTH) div 2,
    wings_io:text_at(TextX, Y, Label),
    if
	Active == true ->
	    L = length(Label),
	    wings_io:text_at(TextX, Y, duplicate(L, $_));
	true -> ok
    end.

button_event(#mousebutton{state=?SDL_RELEASED}, _, #but{action=Action}) ->
    Action;
button_event(_Ev, _Fi, But) -> But.

%%%
%%% Color box.
%%%

-record(col,
	{val}).

color(Def) ->
    Col = #col{val=Def},
    Fun = color_fun(),
    {Fun,false,Col,3*?CHAR_WIDTH,?LINE_HEIGHT+2}.

color_fun() ->
    fun({redraw,Active}, Fi, Col, Common) ->
	    col_draw(Active, Fi, Col, Common);
       ({event,init}, #fi{key=Key}, #col{val=Val}=Col, Common) ->
	    {Col,gb_trees:insert(Key, Val, Common)};
       ({event,Ev}, Fi, Col, Common) ->
	    col_event(Ev, Fi, Col, Common);
       (value, #fi{key=Key}, _Col, Common) ->
	    gb_trees:get(Key, Common)
    end.

col_draw(Active, #fi{key=Key,x=X,y=Y0}, _, Common) ->
    Color = gb_trees:get(Key, Common),
    wings_io:sunken_rect(X, Y0+3,
			 3*?CHAR_WIDTH, ?CHAR_HEIGHT, Color),
    Y = Y0+?CHAR_HEIGHT,
    if
	Active == true ->
	    wings_io:text_at(X, Y, "___");
	true -> ok
    end.

col_event({key,_,_,$\s}, Fi, Col, Common) ->
    pick_color(Fi, Col, Common);
col_event(#mousebutton{x=Xb,state=?SDL_RELEASED}, #fi{x=X}=Fi,
	  Col, Common) ->
    if
	X =< Xb, Xb < X+3*?CHAR_WIDTH ->
	    pick_color(Fi, Col, Common);
	true -> {Col,Common}
    end;
col_event(_Ev, _Fi, Col, Common) -> {Col,Common}.

pick_color(#fi{key=Key}, Col, Common0) ->
    {R0,G0,B0} = gb_trees:get(Key, Common0),
    Range = [{range,{0,255}}],
    Draw = fun(X, Y, _W, _H, Common) ->
		   Color = {gb_trees:get(r, Common)/255,
			    gb_trees:get(g, Common)/255,
			    gb_trees:get(b, Common)/255},
		   wings_io:sunken_rect(X, Y, 50, 50-4, Color)
	   end,
    {dialog,[{custom,50,50,Draw},
	     {label_column,
	      [{"R",{slider,{text,trunc(R0*255),[{key,r}|Range]}}},
	       {"G",{slider,{text,trunc(G0*255),[{key,g}|Range]}}},
	       {"B",{slider,{text,trunc(B0*255),[{key,b}|Range]}}}]}],
     fun([{r,R},{g,G},{b,B}]) ->
	     Val = {R/255,G/255,B/255},
	     {Col#col{val=Val},gb_trees:update(Key, Val, Common0)}
     end}.

%%%
%%% Custom field.
%%%

-record(custom,
	{handler}
       ).

custom(W, H, Handler) ->
    Custom = #custom{handler=Handler},
    Fun = custom_fun(),
    {Fun,true,Custom,W,H}.

custom_fun() ->
    fun({redraw,_Active}, #fi{x=X,y=Y,w=W,h=H},
	#custom{handler=Handler}, Common) ->
	    Handler(X, Y, W, H, Common);
       ({event,_Ev}, _Fi, Custom, _) -> Custom
    end.

%%%
%%% Slider.
%%%

-define(SL_LENGTH, 100).

-record(sl,
	{min,
	 range,
	 peer
	}).

slider(Field) ->
    Flags = element(size(Field), Field),
    {Min,Max} = property_lists:get_value(range, Flags),
    Key = property_lists:get_value(key, Flags),
    Sl = #sl{min=Min,range=(Max-Min)/?SL_LENGTH,peer=Key},
    Fun = slider_fun(),
    {Fun,false,Sl,?SL_LENGTH+4,?LINE_HEIGHT+2}.

slider_fun() ->
    fun({redraw,_Active}, Fi, Sl, Common) ->
	    slider_redraw(Fi, Sl, Common);
       ({event,init}, #fi{key=Key}, #sl{peer=undefined}=Sl, _) ->
	    Sl#sl{peer=Key-1};
       ({event,Ev}, Fi, Sl, Common) ->
	    slider_event(Ev, Fi, Sl, Common);
       (value, _, _, _) -> none
    end.

slider_redraw(#fi{x=X,y=Y0,w=W},
	      #sl{min=Min,range=Range,peer=Peer}, Common) ->
    Y = Y0+?LINE_HEIGHT div 2 + 4,
    wings_io:sunken_rect(X, Y, W, 1, {0,0,0}),
    Val = gb_trees:get(Peer, Common),
    Pos = trunc((Val-Min) / Range),
    wings_io:raised_rect(X+Pos, Y-5, 4, 10, ?MENU_COLOR).

slider_event(#mousebutton{x=Xb,state=?SDL_RELEASED}, Fi, Sl, Common) ->
    slider_move(Xb, Fi, Sl, Common);
slider_event(#mousemotion{x=Xb}, Fi, Sl, Common) ->
    slider_move(Xb, Fi, Sl, Common);
slider_event(_, _, Sl, _) -> Sl.

slider_move(Xb, #fi{x=X}, #sl{min=Min,range=Range,peer=Peer}=Sl, Common) ->
    Pos = max(0, min(Xb-X, ?SL_LENGTH)),
    Val0 = Min + Pos*Range,
    Val = if
	      is_integer(Min) -> round(Val0);
	      true -> Val0
	  end,
    {Sl,gb_trees:update(Peer, Val, Common)}.

%%%
%%% Label.
%%%

-record(label, {label}).

label(Text) ->
    Lbl = #label{label=Text},
    Fun = label_fun(),
    {Fun,true,Lbl,length(Text)*?CHAR_WIDTH,?LINE_HEIGHT+2}.

label_fun() ->
    fun({redraw,_Active}, #fi{x=X,y=Y}, #label{label=Text}, _Common) ->
	    wings_io:text_at(X, Y+?CHAR_HEIGHT, Text);
       ({event,_Ev}, _Fi, Label, _) ->
	    Label
    end.
    
%%%
%%% Text and number input fields.
%%%

-record(text,
	{bef,
	 aft,
	 sel=0,
	 max,
	 integer=false,
	 charset,				%Character set validator.
	 last_val,
	 validator
	}).

text_field(Def, Flags) when is_float(Def) ->
    DefStr = text_val_to_str(Def),
    {Max,Validator} = float_validator(Flags),
    init_text(Def, DefStr, Max, false,
	      fun float_chars/1, Validator);
text_field(Def, Flags) when is_integer(Def) ->
    {Max,Validator} = integer_validator(Flags),
    init_text(Def, integer_to_list(Def), Max, true,
	      fun integer_chars/1, Validator);
text_field(Def, _Flags) when is_list(Def) ->
    init_text(Def, Def, 30, false,
	      fun all_chars/1, fun(_) -> ok end).

init_text(Val, String, Max, IsInteger, Charset, Validator) ->
    Ts = #text{last_val=Val,bef=[],aft=String,max=Max,
	       integer=IsInteger,charset=Charset,validator=Validator},
    Fun = fun gen_text_handler/4,
    {Fun,false,Ts,(1+Max)*?CHAR_WIDTH,?LINE_HEIGHT+2}.

text_val_to_str(Val) when is_float(Val) ->
    simplify_float(lists:flatten(io_lib:format("~f", [Val])));
text_val_to_str(Val) when is_integer(Val) ->
    integer_to_list(Val);
text_val_to_str(Val) when is_list(Val) ->
    Val.

integer_chars($-) -> true;
integer_chars(C) when $0 =< C, C =< $9 -> true;
integer_chars(_) -> false.

float_chars($-) -> true;
float_chars($.) -> true;
float_chars(C) when $0 =< C, C =< $9 -> true;
float_chars(_) -> false.

all_chars(_) -> true.

integer_validator(Flags) ->
    case property_lists:get_value(range, Flags) of
	undefined -> {8,fun accept_all/1};
	{Min,Max} when is_integer(Min), is_integer(Max), Min =< Max ->
	    Digits = trunc(math:log(Max-Min+1)/math:log(10))+2,
	    {Digits,integer_range(Min, Max)}
    end.

float_validator(Flags) ->
    case property_lists:get_value(range, Flags) of
	undefined -> {12,fun accept_all/1};
	{Min,Max} when is_float(Min), is_float(Max), Min =< Max ->
	    Digits = trunc(math:log(Max-Min+1)/math:log(10))+8,
	    {Digits,float_range(Min, Max)}
    end.

accept_all(_) -> ok.

integer_range(Min, Max) ->
    fun(Str) ->
	    case catch list_to_integer(Str) of
		{'EXIT',_} -> integer_to_list(Min);
		Int when Int < Min -> integer_to_list(Min);
		Int when Int > Max -> integer_to_list(Max);
		Int when is_integer(Int) -> ok
	    end
    end.

float_range(Min, Max) ->
    fun(Str) ->
	    case catch list_to_float(Str) of
		{'EXIT',_} -> float_to_list(Min);
		Float when Float < Min -> float_to_list(Min);
		Float when Float > Max -> float_to_list(Max);
		Float when is_float(Float) -> ok
	    end
    end.

text_get_val(#text{last_val=OldVal}=Ts) when is_integer(OldVal) ->
    case catch list_to_integer(get_text(validate_string(Ts))) of
	{'EXIT',_} -> OldVal;
	Val -> Val
    end;
text_get_val(#text{last_val=Val}=Ts) when is_float(Val) ->
    Text = case get_text(Ts) of
	       [$.|_]=T -> [$0|T];
	       T -> T
	   end,
    case catch list_to_float(Text) of
	Float when is_float(Float) -> Float;
	_Other ->
	    case catch list_to_integer(Text) of
		Int when is_integer(Int) -> float(Int);
		_Crash -> 0.0
	    end
    end;
text_get_val(#text{last_val=Val}=Ts) when is_list(Val) ->
    get_text(Ts).

gen_text_handler({redraw,Active}, Fi, Ts, Common) ->
    draw_text(Active, Fi, Ts, Common);
gen_text_handler({event,init}, #fi{key=Key},
		 #text{last_val=Val}=Ts, Common0) ->
    Common = gb_trees:insert(Key, Val, Common0),
    {Ts#text{last_val=Val},Common};
gen_text_handler({event,Ev}, #fi{key=Key}=Fi,
		 #text{last_val=Val0}=Ts0, Common0) ->
    Ts1 = case gb_trees:get(Key, Common0) of
	      Val0 -> Ts0;
	      Val1 ->
		  ValStr = text_val_to_str(Val1),
		  Ts0#text{bef=[],aft=ValStr}
	  end,
    Ts = text_event(Ev, Fi, Ts1),
    Val = text_get_val(Ts),
    Common = gb_trees:update(Key, Val, Common0),
    {Ts#text{last_val=Val},Common};
gen_text_handler(value, #fi{key=Key}, _, Common) ->
    gb_trees:get(Key, Common).

draw_text(false, #fi{key=Key,x=X0,y=Y0}, #text{max=Max}, Common) ->
    Val = gb_trees:get(Key, Common),
    Str = text_val_to_str(Val),
    gl:color3f(0, 0, 0),
    wings_io:sunken_rect(X0, Y0+2,
			 (Max+1)*?CHAR_WIDTH, ?CHAR_HEIGHT+1,
			 {1,1,1}),
    Y = Y0 + ?CHAR_HEIGHT,
    X = X0 + 4,
    wings_io:text_at(X, Y, Str);
draw_text(true, #fi{x=X0,y=Y0}, #text{sel=Sel,bef=Bef0,aft=Aft,max=Max}, _) ->
    gl:color3f(0, 0, 0),
    wings_io:sunken_rect(X0, Y0+2,
			 (Max+1)*?CHAR_WIDTH, ?CHAR_HEIGHT+1,
			 {1,1,1}),
    Y = Y0 + ?CHAR_HEIGHT,
    X = X0 + 4,
    Bef = reverse(Bef0),
    wings_io:text_at(X, Y, Bef),
    wings_io:text(Aft),
    if
	Sel < 0 ->
	    gl:color3f(0, 0, 0.5),
	    Skip = length(Bef)+Sel,
	    X1 = X+Skip*?CHAR_WIDTH,
 	    gl:recti(X1, Y-?CHAR_HEIGHT+3, X1-Sel*?CHAR_WIDTH, Y+2),
 	    gl:color3f(1, 1, 1),
	    wings_io:text_at(X1, Y, lists:nthtail(Skip, Bef)),
	    gl:color3f(0, 0, 0);
	Sel > 0 ->
	    gl:color3f(0, 0, 0.5),
	    X1 = X+length(Bef)*?CHAR_WIDTH,
 	    gl:recti(X1, Y-?CHAR_HEIGHT+3, X1+Sel*?CHAR_WIDTH, Y+2),
 	    gl:color3f(1, 1, 1),
	    draw_text_1(X1, Y, Aft, Sel);
	true ->
	    gl:color3f(1, 0, 0),
	    X1 = X+length(Bef)*?CHAR_WIDTH,
	    wings_io:text_at(X1, Y, [caret]),
	    gl:color3f(0, 0, 0)
    end.

draw_text_1(_, _, _, 0) ->
    gl:color3f(0, 0, 0);
draw_text_1(X, Y, [C|T], N) ->
    wings_io:text_at(X, Y, [C]),
    draw_text_1(X+?CHAR_WIDTH, Y, T, N-1).

validate_string(#text{validator=Validator}=Ts) ->
    case Validator(get_text(Ts)) of
	ok -> Ts;
	Str when is_list(Str) -> Ts#text{bef=[],aft=Str,sel=0}
    end.

get_text(#text{bef=Bef,aft=Aft}) ->
    reverse(Bef, Aft).

text_event({key,Sym,Mod,Unicode}, _Fi, Ts) ->
    key(Sym, Mod, Unicode, Ts);
text_event({focus,false}, _Fi, Ts) ->
    validate_string(Ts);
text_event({focus,true}, _Fi, #text{bef=Bef,aft=Aft}=Ts) ->
    Str = reverse(Bef, Aft),
    Ts#text{bef=[],sel=length(Str),aft=Str};
text_event(#mousebutton{x=X,state=?SDL_PRESSED}, Fi, Ts) ->
    text_pos(X, Fi, Ts);
text_event(#mousebutton{x=X,state=?SDL_RELEASED}, Fi, Ts) ->
    text_sel(X, Fi, Ts);
text_event(#mousemotion{x=X}, Fi, Ts) ->
    text_sel(X, Fi, Ts);
text_event(_Ev, _Fi, Ts) -> Ts.

text_pos(Mx0, #fi{x=X}, Ts) ->
    text_pos_1(round((Mx0-X)/?CHAR_WIDTH), Ts).

text_pos_1(Mx, #text{bef=[C|Bef],aft=Aft}=Ts) when Mx < length(Bef) ->
    text_pos_1(Mx, Ts#text{bef=Bef,aft=[C|Aft]});
text_pos_1(Mx, #text{bef=Bef,aft=[C|Aft]}=Ts) when Mx > length(Bef) ->
    text_pos_1(Mx, Ts#text{bef=[C|Bef],aft=Aft});
text_pos_1(_Mx, Ts) -> Ts#text{sel=0}.

text_sel(Mx0, #fi{x=X}, Ts) ->
    text_sel_1(round((Mx0-X)/?CHAR_WIDTH), Ts).

text_sel_1(Mx, #text{bef=Bef}=Ts) when Mx < length(Bef) ->
    Ts#text{sel=max(Mx, 0)-length(Bef)};
text_sel_1(Mx, #text{bef=Bef,aft=Aft}=Ts) ->
    Ts#text{sel=min(Mx-length(Bef),length(Aft))};
text_sel_1(_Mx, Ts) -> Ts.

key(?SDLK_KP_PLUS, _, _, #text{integer=true}=Ts) ->
    increment(Ts, 1);
key(?SDLK_KP_MINUS, _, _, #text{integer=true}=Ts) ->
    increment(Ts, -1);
key(?SDLK_HOME, Mod, _, Ts) -> key(1, Mod, Ts);
key(?SDLK_END, Mod, _, Ts) -> key(5, Mod, Ts);
key(?SDLK_LEFT, Mod, _, Ts) -> key(2, Mod, Ts);
key(?SDLK_RIGHT, Mod, _, Ts) -> key(6, Mod, Ts);
key(?SDLK_DELETE, Mod, _, Ts) -> key(4, Mod, Ts);
key(?SDLK_BACKSPACE, Mod, _, Ts) -> key(?SDLK_BACKSPACE, Mod, Ts);
key(?SDLK_KP_PERIOD, Mod, _, Ts) ->
    key($., Mod, Ts);
key(C, Mod, _, Ts) when ?SDLK_KP0 =< C, C =< ?SDLK_KP9 ->
    key(C-?SDLK_KP0+$0, Mod, Ts);
key(_C, Mod, Unicode, Ts) ->
    key(Unicode, Mod, Ts).

key($+, _, #text{integer=true}=Ts) ->
    increment(Ts, 1);
key($=, _, #text{integer=true}=Ts) ->		%Same key as plus on American keybd.
    increment(Ts, 1);
key($-, _, #text{integer=true}=Ts) ->
    increment(Ts, -1);
key($\b, _, #text{sel=0,bef=[_|Bef]}=Ts) ->	%Bksp (no selection).
    Ts#text{bef=Bef};
key($\b, _, Ts) ->				%Bksp (selection).
    del_sel(Ts);
key(2, Mod, #text{sel=Sel,bef=Bef}=Ts) when ?IS_SHIFTED(Mod) ->
    if
	-length(Bef) < Sel -> Ts#text{sel=Sel-1};
	true -> Ts
    end;
key(2, _, #text{bef=[C|Bef],aft=Aft}=Ts) ->	%Ctrl-B
    Ts#text{sel=0,bef=Bef,aft=[C|Aft]};
key(2, _, Ts) ->				%Ctrl-B
    Ts#text{sel=0};
key(6, Mod, #text{sel=Sel,aft=Aft}=Ts) when ?IS_SHIFTED(Mod) ->
    if
	Sel < length(Aft) -> Ts#text{sel=Sel+1};
	true -> Ts
    end;
key(6, _, #text{bef=Bef,aft=[C|Aft]}=Ts) ->	%Ctrl-F
    Ts#text{sel=0,bef=[C|Bef],aft=Aft};
key(1, Mod, #text{bef=Bef}=Ts) when ?IS_SHIFTED(Mod) ->
    Ts#text{sel=-length(Bef)};
key(1, _, #text{bef=Bef,aft=Aft}=Ts) ->		%Ctrl-A
    Ts#text{sel=0,bef=[],aft=reverse(Bef, Aft)};
key(5, Mod, #text{aft=Aft}=Ts) when ?IS_SHIFTED(Mod) ->
    Ts#text{sel=length(Aft)};
key(5, _, #text{bef=Bef,aft=Aft}=Ts) ->		%Ctrl-E
    Ts#text{sel=0,bef=reverse(Aft, Bef),aft=[]};
key(11, _, #text{}=Ts) ->			%Ctrl-K
    Ts#text{aft=[]};
key(4, _, #text{sel=0,aft=[_|Aft]}=Ts) ->	%Ctrl-D
    Ts#text{aft=Aft};
key(4, _, Ts) ->				%Ctrl-D
    del_sel(Ts);
key(C, _, #text{max=Max,charset=Charset}=Ts0)
  when $\s =< C, C < 256 ->
    case Charset(C) of
	true ->
	    case del_sel(Ts0) of
		#text{bef=Bef,aft=Aft}=Ts when length(Bef)+length(Aft) < Max ->
		    Ts#text{bef=[C|Bef]};
		_Other -> Ts0
	    end;
	false -> Ts0
    end;
key(_C, _Mod, Ts) -> Ts.

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

simplify_float(F) ->
    reverse(simplify_float_1(reverse(F))).

simplify_float_1("0."++_=F) -> F;
simplify_float_1("0"++F) -> simplify_float_1(F);
simplify_float_1(F) -> F.

max(A, B) when A > B -> A;
max(_A, B) -> B.

min(A, B) when A < B -> A;
min(_A, B) -> B.

dialog_unzip(L) ->
    dialog_unzip(L, [], []).
dialog_unzip([{Lbl,F}|T], AccA, AccB) ->
    dialog_unzip(T, [{label,Lbl}|AccA], [F|AccB]);
dialog_unzip([], AccA, AccB) ->
    {reverse(AccA),reverse(AccB)}.
