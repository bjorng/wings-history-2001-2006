%%
%%  wings_ask.erl --
%%
%%     Dialog boxes.
%%
%%  Copyright (c) 2002-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_ask.erl,v 1.90 2003/08/06 09:06:27 bjorng Exp $
%%

-module(wings_ask).
-export([ask/3,ask/4,dialog/3,dialog/4]).

-import(wings_util, [min/2,max/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(HMARGIN, 16).
-define(VMARGIN, 8).

-define(HFRAME_SPACING, (3*?CHAR_WIDTH div 2)).

-define(IS_SHIFTED(Mod), ((Mod) band ?SHIFT_BITS =/= 0)).

-import(lists, [reverse/1,reverse/2,duplicate/2,keysearch/3,member/2]).

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
	 level,					%Levels of nesting.
	 owner=Owner,				%Where to send result.
	 grab_win				%Previous grabbed focus window.
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

ask(Title, Qs, Fun) ->
    ask(true, Title, Qs, Fun).

ask(Bool, Title, Qs0, Fun) ->
    {Labels,Vals} = ask_unzip(Qs0),
    Qs = [{hframe,
	   [{vframe,Labels},
	    {vframe,Vals}]}],
    dialog(Bool, Title, Qs, Fun).

ask_unzip(Qs) ->
    ask_unzip(Qs, [], []).
ask_unzip([{Label,{menu,_,_}=Menu}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [Menu|AccB]);
ask_unzip([{Label,Def}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def}|AccB]);
ask_unzip([{Label,Def,Flags}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def,Flags}|AccB]);
ask_unzip([], Labels, Vals) ->
    {reverse(Labels),reverse(Vals)}.

%%
%% Syntax of Qs.
%% 
%% {hframe,Qs [,Flags]}				-- Horizontal frame
%% {vframe,Qs [,Flags]}				-- Vertical frame
%%     Flags = [Flag]
%%     Flag = {title,Title,String}
%% 
%% {label,LabelString}				-- Textual label
%% 
%% {vradio,Alts,VarName,DefaultValue,Flags}	-- Radio buttons vertically
%% {hradio,Alts,VarName,DefaultValue,Flags}     -- Radio buttons horizontally
%%     Alts = [{PromptString,Value}]
%%     VarName = atom()
%%     Flags = [Flag]
%%     Flag = key|{title,TitleString}
%% (Example: see wpc_am.erl.)
%%

dialog(false, _Title, Qs, Fun) ->
    S = setup_ask(Qs, Fun),
    return_result(S),
    keep;
dialog(true, Title, Qs, Fun) -> dialog(Title, Qs, Fun).

dialog(Title, Qs, Fun) ->
    do_dialog(Title, Qs, [make_ref()], Fun).

do_dialog(Title, Qs, Level, Fun) ->
    GrabWin = wings_wm:release_focus(),
    S0 = setup_ask(Qs, Fun),
    S1 = next_focus(S0, 1),
    #s{w=W0,h=H0} = S1,
    W = W0 + 2*?HMARGIN,
    H = H0 + 2*?VMARGIN,
    S = S1#s{ox=?HMARGIN,oy=?VMARGIN,level=Level,grab_win=GrabWin},
    Name = {dialog,hd(Level)},
    setup_blanket(Name),
    Op = {seq,push,get_event(S)},
    {_,X,Y} = sdl_mouse:getMouseState(),
    wings_wm:toplevel(Name, Title, {X,Y-?LINE_HEIGHT,highest}, {W,H},
		      [{anchor,n}], Op),
    wings_wm:set_prop(Name, drag_filter, fun(_) -> yes end),
    keep.

setup_ask(Qs0, Fun) ->
    Qs1 = normalize(Qs0),
    Qs = propagate_sizes(Qs1),
    {Fis0,Priv0} = flatten_fields(Qs),
    Fis1 = insert_keys(Fis0, 1),
    Fis = list_to_tuple(Fis1),
    Priv = list_to_tuple(Priv0),
    {#fi{w=W,h=H},_} = Qs,
    Owner = wings_wm:this(),
    S = #s{w=W,h=H,call=Fun,fi=Fis,priv=Priv,focus=size(Fis),owner=Owner},
    init_fields(1, size(Priv), S).

insert_keys([#fi{flags=Flags}=Fi|T], I) ->
    Key = case proplists:get_value(key, Flags) of
	      undefined -> I;
	      Key0 -> Key0
	  end,
    [Fi#fi{key=Key}|insert_keys(T, I+1)];
insert_keys([], _) -> [].

init_fields(I, N, #s{fi=Fis,priv=Priv0,common=Common0}=S) when I =< N ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv0),
    case Handler(init, Fi, Fst0, Common0) of
	{Fst,Common} when is_atom(element(1, Fst)), is_tuple(Common) ->
	    Priv = setelement(I, Priv0, Fst),
	    init_fields(I+1, N, S#s{priv=Priv,common=Common});
	Fst when is_atom(element(1, Fst)) ->
	    Priv = setelement(I, Priv0, Fst),
	    init_fields(I+1, N, S#s{priv=Priv})
    end;
init_fields(_, _, S) -> S.

setup_blanket(Dialog) ->
    %% The menu blanket window lies below the dialog, covering the entire
    %% screen and ignoring most events. Keyboard events will be forwarded
    %% to the active dialog window.
    Op = {push,fun(Ev) -> blanket(Ev, Dialog) end},
    {TopW,TopH} = wings_wm:top_size(),
    wings_wm:new({blanket,Dialog}, {0,0,highest}, {TopW,TopH}, Op).

delete_blanket(#s{level=[Level|_]}) ->
    wings_wm:delete({blanket,{dialog,Level}});
delete_blanket(#s{level=undefined}) -> ok.

blanket(#keyboard{}=Ev, Dialog) ->
    wings_wm:send(Dialog, Ev);
blanket(_, _) -> keep.

get_event(S) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> event(Ev, S) end}.

event(redraw, S) ->
    redraw(S),
    keep;
event({current_state,_}, _) ->
    keep;
event(#keyboard{sym=Sym,mod=Mod,unicode=Unicode}, S) ->
    event_key({key,Sym,Mod,Unicode}, S);
event(#mousebutton{button=1,x=X,y=Y}=Ev, S) ->
    mouse_event(X, Y, Ev, S);
event(#mousebutton{}, _) ->
    keep;
event(#mousemotion{x=X,y=Y}=Ev, S) ->
    mouse_event(X, Y, Ev, S);
event({drop,{X,Y},DropData}, S) ->
    drop_event(X, Y, DropData, S);
event({action,{update,I,{Fst,Common}}}, #s{priv=Priv0}=S)
  when is_atom(element(1, Fst)), is_tuple(Common) ->
    Priv = setelement(I, Priv0, Fst),
    get_event(S#s{priv=Priv,common=Common});
event({action,{update,I,Fst}}, #s{priv=Priv0}=S) ->
    Priv = setelement(I, Priv0, Fst),
    get_event(S#s{priv=Priv});
event(Ev, S) -> field_event(Ev, S).

event_key({key,?SDLK_ESCAPE,_,_}, S) ->
    delete(S);
event_key({key,?SDLK_TAB,Mod,_}, S) when Mod band ?SHIFT_BITS =/= 0 ->
    get_event(next_focus(S, -1));
event_key({key,?SDLK_TAB,_,_}, S) ->
    get_event(next_focus(S, 1));
event_key({key,_,_,$\t}, S) ->
    get_event(next_focus(S, 1));
event_key({key,?SDLK_KP_ENTER,_,_}, S) ->
    enter_pressed({key,$\r,$\r,$\r}, S);
event_key({key,_,_,$\r}=Ev, S) ->
    enter_pressed(Ev, S);
event_key(Ev, S) ->
    field_event(Ev, S).

enter_pressed(Ev, #s{focus=I}=S) ->
    case field_type(I, S) of
	but -> field_event(Ev, S);
	_ -> return_result(S)
    end.

field_type(I, #s{focus=I,priv=Priv}) ->
    element(1, element(I, Priv)).

delete(#s{level=[_],grab_win=GrabWin}=S) ->
    delete_blanket(S),
    wings_wm:grab_focus(GrabWin),
    delete;
delete(S) ->
    delete_blanket(S),
    delete.

mouse_event(X0, Y0, #mousemotion{}=Ev, #s{focus=I,ox=Ox,oy=Oy}=S) ->
    X = X0-Ox,
    Y = Y0-Oy,
    field_event(Ev#mousemotion{x=X,y=Y}, I, S);
mouse_event(X0, Y0, #mousebutton{state=State}=Ev, #s{focus=I0,ox=Ox,oy=Oy,fi=Fis}=S0) ->
    X = X0-Ox,
    Y = Y0-Oy,
    case State of
	?SDL_RELEASED ->
	    field_event(Ev#mousebutton{x=X,y=Y}, I0, S0);
	?SDL_PRESSED ->
	    case mouse_to_field(1, Fis, X, Y) of
		none -> keep;
		I ->
		    S = set_focus(I, S0),
		    field_event(Ev#mousebutton{x=X,y=Y}, S)
	    end
    end.

drop_event(X, Y, DropData, #s{ox=Ox,oy=Oy,fi=Fis}=S0) ->
    case mouse_to_field(1, Fis, X-Ox, Y-Oy) of
	none -> keep;
	I ->
	    S = set_focus(I, S0),
	    field_event({drop,DropData}, S)
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
	case OldHandler({focus,false}, OldFi, OldFst0, Common0) of
	    {Ofst,Common1} when is_atom(element(1, Ofst)), is_tuple(Common1) ->
		{Ofst,Common1};
	    Ofst when is_atom(element(1, Ofst)) ->
		{Ofst,Common0}
	end,
    Priv1 = setelement(OldFocus, Priv0, OldFst),

    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv1),
    {Fst,Common} =
	case Handler({focus,true}, Fi, Fst0, Common2) of
	    {Fst1,Common3} when is_atom(element(1, Fst1)), is_tuple(Common3) ->
		{Fst1,Common3};
	    Fst1 when is_atom(element(1, Fst1)) ->
		{Fst1,Common2}
	end,
    Priv = setelement(I, Priv1, Fst),

    S#s{focus=I,priv=Priv,common=Common}.

field_event(Ev, #s{focus=I}=S) ->
    field_event(Ev, I, S).

field_event(#mousemotion{x=X,y=Y,state=Bst}, _, #s{fi=Fis})
  when Bst band ?SDL_BUTTON_LMASK == 0 ->
    case mouse_to_field(1, Fis, X, Y) of
	none ->
	    wings_wm:allow_drag(false);
	I ->
	    #fi{flags=Flags} = element(I, Fis),
	    wings_wm:allow_drag(member(drag, Flags))
    end,
    keep;
field_event(Ev, I, #s{fi=Fis,priv=Priv0,common=Common0}=S) ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    Fst0 = element(I, Priv0),
    case Handler(Ev, Fi, Fst0, Common0) of
	ok ->
	    return_result(S);
	cancel ->
	    delete(S);
	{recursive,Return} ->
	    Return;
	{dialog,Title,Qs,Fun} ->
	    recursive_dialog(Title, I, Qs, Fun, S);
	{drag,WH,DropData} ->
	    drag(Ev, WH, Fi, Fst0, Common0, DropData);
	{Fst,Common} when is_atom(element(1, Fst)), is_tuple(Common) ->
	    Priv = setelement(I, Priv0, Fst),
	    get_event(S#s{priv=Priv,common=Common});
	Action when is_function(Action) ->
	    Res = collect_result(S),
	    Action(Res),
	    delete(S);
	Fst when is_atom(element(1, Fst)) ->
	    Priv = setelement(I, Priv0, Fst),
	    get_event(S#s{priv=Priv})
    end.

recursive_dialog(Title, I, Qs, Fun, #s{level=Level}=S) ->
    do_dialog(Title, Qs, [make_ref()|Level],
	      fun(Vs) -> {update,I,Fun(Vs)} end),
    get_event(S).

drag(Ev, {_,_}=Pos, _, _, _, DropData) ->
    wings_wm:drag(Ev, Pos, DropData).

return_result(#s{call=EndFun,owner=Owner}=S) ->
    Res = collect_result(S),
    case catch EndFun(Res) of
	{'EXIT',Reason} ->
	    exit(Reason);
	{command_error,Message} ->
	    wings_util:message(Message),
	    get_event(S);
	ignore ->
	    delete(S);
	#st{}=St ->
	    wings_wm:send(Owner, {new_state,St}),
	    delete(S);
	Action when is_tuple(Action); is_atom(Action) ->
	    wings_wm:send(Owner, {action,Action}),
	    delete(S)
    end.

collect_result(#s{fi=Fis,priv=Priv}=S) ->
    collect_result(1, Fis, Priv, S, []).

collect_result(I, Fis, Priv, #s{common=Common}=S, Acc) when I =< size(Fis) ->
    case element(I, Fis) of
	#fi{inert=true} ->
	    collect_result(I+1, Fis, Priv, S, Acc);
	#fi{handler=Handler,flags=Flags}=Fi ->
	    Fst = element(I, Priv),
	    case catch Handler(value, Fi, Fst, Common) of
		{'EXIT',Reason} ->
		    exit(Reason);
		none ->
		    collect_result(I+1, Fis, Priv, S, Acc);
		Res0 ->
		    Res = case proplists:get_value(key, Flags) of
			      undefined -> Res0;
			      Key -> {Key,Res0}
			  end,
		    collect_result(I+1, Fis, Priv, S, [Res|Acc])
	    end
    end;
collect_result(_, _, _, _, Res) -> reverse(Res).

redraw(#s{w=W,h=H,ox=Ox,oy=Oy,focus=Focus,fi=Fi,priv=Priv,common=Common}) ->
    wings_io:ortho_setup(),
    gl:translated(Ox, Oy, 0),
    blend(fun(Color) ->
		  wings_io:border(-?HMARGIN, -?VMARGIN,
				  W+2*?HMARGIN-1, H+2*?VMARGIN-1,
				  Color)
	  end),
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
normalize({vradio,Qs,Var,Def}, Fi) ->
    normalize(radio(vframe, Qs, Var, Def, []), Fi);
normalize({vradio,Qs,Var,Def,Flags}, Fi) ->
    normalize(radio(vframe, Qs, Var, Def, Flags), Fi);
normalize({hradio,Qs,Var,Def}, Fi) ->
    normalize(radio(hframe, Qs, Var, Def, []), Fi);
normalize({hradio,Qs,Var,Def,Flags}, Fi) ->
    normalize(radio(hframe, Qs, Var, Def, Flags), Fi);
normalize({vframe,Qs}, Fi) ->
    vframe(Qs, Fi, []);
normalize({vframe,Qs,Flags}, Fi) ->
    vframe(Qs, Fi, Flags);
normalize({hframe,Qs}, Fi) ->
    hframe(Qs, Fi, []);
normalize({hframe,Qs,Flags}, Fi) ->
    hframe(Qs, Fi, Flags);
normalize({label,Label}, Fi) ->
    normalize_field(label(Label, []), [], Fi);
normalize({label,Label,Flags}, Fi) ->
    normalize_field(label(Label, Flags), Flags, Fi);
normalize({color,Def}, Fi) ->
    normalize_field(color(Def), [drag], Fi);
normalize({color,Def,Flags}, Fi) ->
    normalize_field(color(Def), [drag|Flags], Fi);
normalize({alt,VarDef,Prompt,Val}, Fi) ->
    normalize_field(radiobutton(VarDef, Prompt, Val), [], Fi);
normalize({key_alt,{Key,_}=VarDef,Prompt,Val}, Fi) ->
    normalize_field(radiobutton(VarDef, Prompt, Val), [{key,Key}], Fi);
normalize({menu,Alt,{Val,VarDef}}, Fi) ->
    normalize_field(menu(Val, VarDef, Alt), [], Fi);
normalize({menu,Alt,VarDef}, Fi) ->
    normalize_field(menu(none, VarDef, Alt), [], Fi);
normalize({menu,Alt,{Val,VarDef},Flags}, Fi) ->
    normalize_field(menu(Val, VarDef, Alt), Flags, Fi);
normalize({menu,Alt,VarDef,Flags}, Fi) ->
    normalize_field(menu(none, VarDef, Alt), Flags, Fi);    
normalize({button,Action}, Fi) when is_atom(Action) ->
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

radio(FrameType, Qs0, Var, Def, Flags) ->
    AltTag = case proplists:get_bool(key, Flags) of
		 false -> alt;
		 true -> key_alt
	     end,
    VarDef = {Var,Def},
    Qs = [{AltTag,VarDef,Prompt,Val} || {Prompt,Val} <- Qs0],
    {FrameType,Qs,Flags}.

vframe(Qs, #fi{x=X,y=Y0}=Fi0, Flags) ->
    {Dx,Dy} = case have_border(Flags) of
		  true -> {10,?LINE_HEIGHT};
		  false -> {0,0}
	      end,
    {Fields,Y,W0} = vframe_1(Qs, Fi0#fi{x=X+Dx,y=Y0+Dy}, 0, []),
    W1 = frame_fit_title(W0, Flags),
    H0 = Y-Y0,
    {Ipadx,Ipady} = case have_border(Flags) of
			true -> {2*10,10};
			false -> {0,0}
		    end,
    W = W1 + Ipadx,
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
    W0 = frame_fit_title(X-X0-Dx0, Flags),
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

frame_fit_title(W, Flags) ->
    case proplists:get_value(title, Flags) of
	undefined -> W;
	Title -> max(3*?CHAR_WIDTH+wings_text:width(Title), W)
    end.
	     
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
       (_, _, Frame, _) -> Frame
    end.

frame_redraw(#fi{flags=[]}) -> ok;
frame_redraw(#fi{x=X,y=Y0,w=W,h=H0,flags=Flags}) ->
    case proplists:get_value(title, Flags) of
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
	    TextPos = X + 3*?CHAR_WIDTH,
	    blend(fun(Color) ->
			  wings_io:set_color(Color),
			  gl:rectf(TextPos-?CHAR_WIDTH, Y-1,
				   TextPos+(length(Title)+1)*?CHAR_WIDTH, Y+2)
		  end),
	    gl:color3b(0, 0, 0),
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
    proplists:is_defined(title, Flags).

%%%
%%% Separator.
%%%

separator() ->
    Fun = separator_fun(),
    {Fun,true,{separator},4*?CHAR_WIDTH,10}.

separator_fun() ->
    fun({redraw,_Active}, Fi, _Dummy, _) ->
	    separator_draw(Fi);
       (_, _, Sep, _) -> Sep
    end.

separator_draw(#fi{x=X,y=Y,w=W}) ->
    ?CHECK_ERROR(),
    LeftX = X + 0.5,
    RightX = X + W + 0.5,
    UpperY = Y + 5.5,
    gl:lineWidth(1),
    gl:'begin'(?GL_LINES),
    gl:color3f(0.10, 0.10, 0.10),
    gl:vertex2f(LeftX, UpperY),
    gl:vertex2f(RightX, UpperY),
    gl:'end'(),
    gl:color3b(0, 0, 0),
    ?CHECK_ERROR().

%%%
%%% Checkboxes.
%%%
-define(CB_SIZE, 8).

-record(cb,
	{label,
	 labelw,				%Width of label in pixels.
	 spacew,				%Width of a space character.
	 state
	}).

checkbox(Label, Def) ->
    LabelWidth = wings_text:width(Label),
    SpaceWidth = wings_text:width(" "),
    Cb = #cb{label=Label,state=Def,labelw=LabelWidth,spacew=SpaceWidth},
    Fun = fun(Ev, Fi, C, _) -> cb_event(Ev, Fi, C) end,
    {Fun,false,Cb,LabelWidth+SpaceWidth+?CB_SIZE,?LINE_HEIGHT+2}.

cb_event(value, _, #cb{state=Val}) ->
    Val;
cb_event({redraw,Active}, Fi, Cb) ->
    cb_draw(Active, Fi, Cb);
cb_event({key,_,_,$\s}, _, #cb{state=State}=Cb) ->
    Cb#cb{state=not State};
cb_event(#mousebutton{x=Xb,state=?SDL_PRESSED},
	 #fi{x=X},
	 #cb{state=State,labelw=LblW,spacew=SpaceW}=Cb) ->
    if
	Xb-X < LblW+4*SpaceW ->
	    Cb#cb{state=not State};
	true -> Cb
    end;
cb_event(_Ev, _Fi, Cb) -> Cb.

cb_draw(Active, #fi{x=X,y=Y0}, #cb{label=Label,state=State}) ->
    wings_io:sunken_rect(X, Y0+?CHAR_HEIGHT-9, 8, 8, {1,1,1}),
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

%%%
%%% Radio buttons.
%%%

-record(rb,
	{var,
	 def,					%Default value.
	 val,
	 label,
	 labelw,			    %Width of label in pixels.
	 spacew				  %Width of a space character.
	}).

radiobutton({Var,Def}, Label, Val) ->
    LabelWidth = wings_text:width(Label),
    SpaceWidth = wings_text:width(" "),
    Rb = #rb{var=Var,def=Def,val=Val,label=Label,labelw=LabelWidth,spacew=SpaceWidth},
    Fun = fun rb_event/4,
    {Fun,false,Rb,LabelWidth+2*SpaceWidth,?LINE_HEIGHT+2}.

rb_event(init, _Fi, #rb{var=Var,def=Def,val=Val}=Rb, Common) ->
    case Val of
	Def -> {Rb,gb_trees:insert(Var, Val, Common)};
	_ -> Rb
    end;
rb_event({redraw,Active}, Fi, Rb, Common) ->
    rb_draw(Active, Fi, Rb, Common);
rb_event(value, _Fi, #rb{var=Var,val=Val}, Common) ->
    case gb_trees:get(Var, Common) of
	Val -> Val;
	_ -> none
    end;
rb_event({key,_,_,$\s}, _, Rb, Common) ->
    rb_set(Rb, Common);
rb_event(#mousebutton{x=Xb,state=?SDL_RELEASED},
	 #fi{x=X}, #rb{labelw=LblW,spacew=SpaceW}=Rb, Common) ->
    if
	Xb-X < LblW+4*SpaceW ->
	    rb_set(Rb, Common);
	true -> Rb
    end;
rb_event(_Ev, _Fi, Rb, _Common) -> Rb.

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
    gl:color3b(0, 0, 0),
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

rb_set(#rb{var=Var,val=Val}=Rb, Common0) ->
    Common = gb_trees:update(Var, Val, Common0),
    {Rb,Common}.

%%%
%%% Menu.
%%%

-record(menu,
	{var,
	 key,
	 menu
	}).

menu(Var, Key, Alt) ->
    W = menu_width(Alt, 0) + 2*wings_text:width(" ") + 10,
    M = #menu{var=Var,key=Key,menu=Alt},
    Fun = fun(Ev, Fi, State, _) -> menu_event(Ev, Fi, State) end,
    {Fun,false,M,W,?LINE_HEIGHT+4}.

menu_event({redraw,_Active}, Fi, M) ->
    menu_draw(Fi, M);
menu_event(value, _, #menu{var=none,key=Key}) ->
    Key;
menu_event(value, _, #menu{var=Var,key=Key}) ->
    {Var,Key};
menu_event(#mousebutton{button=1,state=?SDL_PRESSED}, Fi, M) ->
    menu_popup(Fi, M),
    M;
menu_event({popup_result,Key}, _, M) ->
    M#menu{key=Key};
menu_event(_, _, M) -> M.

menu_width([{S,_}|T], W0) ->
    case wings_text:width(S) of
	W when W < W0 -> menu_width(T, W0);
	W -> menu_width(T, W)
    end;
menu_width([], W) -> W.

menu_draw(#fi{x=X,y=Y0,w=W,h=H}, #menu{key=Key,menu=Menu}=M) ->
    blend(fun(Col) ->
		  wings_io:raised_rect(X, Y0+1, W-?CHAR_WIDTH+10, H-3, Col)
	  end),
    Y = Y0+?CHAR_HEIGHT,
    Val = [Desc || {Desc,K} <- Menu, K =:= Key],
    wings_io:text_at(X+5, Y, Val),

    Xr = X + W-8,
    wings_io:border(Xr-1, Y-9, 10, 10, ?PANE_COLOR),
    gl:color3b(0, 0, 0),
    Arrows = <<
	      2#00010000,
	      2#00111000,
	      2#01111100,
	      2#00000000,
	      2#01111100,
	      2#00111000,
	      2#00010000>>,
    gl:rasterPos2f(Xr+0.5, Y+0.5),
    gl:bitmap(7, 7, 0, -1, 7, 0, Arrows),
    M.

-record(popup,
	{parent,				%Parent window name.
	 sel,					%Selected element (integer).
	 orig_sel,				%Original selection (integer).
	 menu					%Tuple.
	}).

menu_popup(#fi{x=X0,y=Y0,w=W}, #menu{key=Key,menu=Menu0}) ->
    {X1,Y1} = wings_wm:local2global(X0+?HMARGIN, Y0+?VMARGIN),
    Sel = popup_find_index(Menu0, Key, 1),
    Menu = list_to_tuple(Menu0),
    Mh = size(Menu)*?LINE_HEIGHT,
    Ps = #popup{parent=wings_wm:this(),sel=Sel,orig_sel=Sel,menu=Menu},
    Op = {seq,push,get_popup_event(Ps)},
    X = X1-2*?CHAR_WIDTH,
    Y = Y1-2-(Sel-1)*?LINE_HEIGHT,
    wings_wm:new(menu_popup, {X,Y,highest}, {W+2*?CHAR_WIDTH,Mh+10}, Op),
    wings_wm:grab_focus(menu_popup).

popup_find_index([{_,Key}|_], Key, I) -> I;
popup_find_index([_|T], Key, I) -> popup_find_index(T, Key, I+1).

get_popup_event(Ps) ->
    {replace,fun(Ev) -> popup_event(Ev, Ps) end}.

popup_event(redraw, Ps) ->
    popup_redraw(Ps);
popup_event(#mousemotion{y=Y}, #popup{menu=Menu,sel=Sel0}=Ps) ->
    case ((Y-2) div ?LINE_HEIGHT)+1 of
	Sel0 -> keep;
	Sel when 0 < Sel, Sel =< size(Menu) ->
	    wings_wm:dirty(),
	    get_popup_event(Ps#popup{sel=Sel});
	_ -> keep
    end;
popup_event(#mousebutton{button=1,state=?SDL_RELEASED},
	    #popup{parent=Parent,menu=Menu,sel=Sel}) ->
    {_,Key} = element(Sel, Menu),
    wings_wm:send(Parent, {popup_result,Key}),
    delete;
popup_event(_, _) -> keep.

popup_redraw(#popup{sel=Sel,orig_sel=OrigSel,menu=Menu}) ->
    wings_io:ortho_setup(),
    {_,_,W,H} = wings_wm:viewport(),
    blend(fun(Col) ->
		  wings_io:border(0, 0, W-1, H-1, Col)
	  end),
    gl:color3b(0, 0, 0),
    X = 3*?CHAR_WIDTH-1,
    Y = ?CHAR_HEIGHT+2,
    popup_redraw_1(1, Menu, Sel, W, X, ?CHAR_HEIGHT+2),
    gl:color3b(0, 0, 0),
    wings_io:text_at(X-10, OrigSel*Y, [crossmark]),
    keep.

popup_redraw_1(Sel, Menu, Sel, W, X, Y) ->
    {Desc,_} = element(Sel, Menu),
    gl:color3f(0, 0, 0.5),
    gl:recti(X-2, Y+2, X+W-4*?CHAR_WIDTH, Y-?CHAR_HEIGHT+2),
    gl:color3f(1, 1, 1),
    wings_io:text_at(X, Y, Desc),
    gl:color3b(0, 0, 0),
    popup_redraw_1(Sel+1, Menu, Sel, W, X, Y+?LINE_HEIGHT);
popup_redraw_1(I, Menu, Sel, W, X, Y) when I =< size(Menu) ->
    {Desc,_} = element(I, Menu),
    wings_io:text_at(X, Y, Desc),
    popup_redraw_1(I+1, Menu, Sel, W, X, Y+?LINE_HEIGHT);
popup_redraw_1(_, _, _, _, _, _) -> keep.

%%%
%%% Buttons.
%%%

-record(but,
	{label,					%Textual label.
	 action}).

button(Label, Action) ->
    W = lists:max([wings_text:width([$\s,$\s|Label]),
		   wings_text:width(" cancel ")]),
    But = #but{label=Label,action=Action},
    Fun = fun(Ev, Fi, State, _) -> button_event(Ev, Fi, State) end,
    {Fun,false,But,W,?LINE_HEIGHT+2+2}.

button_label(ok) -> "OK";
button_label(S) when is_list(S) -> S;
button_label(Act) -> wings_util:cap(atom_to_list(Act)).

button_event({redraw,Active}, Fi, But) ->
    button_draw(Active, Fi, But);
button_event(value, _, _) ->
    none;
button_event(#mousebutton{x=X,y=Y,state=?SDL_RELEASED}, #fi{x=Bx,y=By,w=W,h=H},
	     #but{action=Action}) when Bx =< X, X =< Bx+W, By =< Y, Y =< By+H ->
    Action;
button_event({key,_,_,Key}, _, #but{action=Action}) when Key =:= $\r; Key =:= $\s ->
    Action;
button_event(_Ev, _Fi, But) ->
    But.

button_draw(Active, #fi{x=X,y=Y0,w=W,h=H}, #but{label=Label}) ->
    Y = Y0+?CHAR_HEIGHT+2,
    blend(fun(Col) ->
		  wings_io:raised_rect(X, Y0+2, W, H-4, Col)
	  end),
    TextX = X + 2 + (W-wings_text:width(Label)) div 2,
    wings_io:text_at(TextX, Y, Label),
    if
	Active == true ->
	    L = length(Label),
	    wings_io:text_at(TextX, Y, duplicate(L, $_));
	true -> ok
    end.

%%%
%%% Color box.
%%%
-define(COL_PREVIEW_SZ, 60).

-record(col,
	{val}).

color(Def) ->
    Col = #col{val=Def},
    Fun = fun col_event/4,
    {Fun,false,Col,3*?CHAR_WIDTH,?LINE_HEIGHT+2}.

col_event({redraw,Active}, Fi, Col, Common) ->
    col_draw(Active, Fi, Col, Common);
col_event(init, #fi{key=Key}, #col{val=Val}=Col, Common) ->
	    {Col,gb_trees:insert(Key, Val, Common)};
col_event(value, #fi{key=Key}, _Col, Common) ->
    gb_trees:get(Key, Common);
col_event({key,_,_,$\s}, Fi, Col, Common) ->
    pick_color(Fi, Col, Common);
col_event(#mousemotion{x=Xm,y=Ym}, Fi, #col{val=RGB}=Col, Common) ->
    case col_inside(Xm, Ym, Fi) of
	true -> {Col,Common};
	false -> {drag,{3*?CHAR_WIDTH,?CHAR_HEIGHT},{color,RGB}}
    end;
col_event(#mousebutton{x=Xm,y=Ym,state=?SDL_RELEASED}, Fi, Col, Common) ->
    case col_inside(Xm, Ym, Fi) of
	true -> pick_color(Fi, Col, Common);
	false -> {Col,Common}
    end;
col_event({drop,{color,RGB1}}, #fi{key=Key}, Col, Common) ->
    RGB0 = gb_trees:get(Key, Common),
    RGB = replace_rgb(RGB0, RGB1),
    {Col#col{val=RGB},gb_trees:update(Key, RGB, Common)};
col_event(_Ev, _Fi, Col, Common) -> {Col,Common}.

%% replace_rgb(OldRGBA, NewRGBA) -> RGBA
%%  Replace a color (RGB + possibly A) with a new color, 
%%  making sure that the new color has the same number of components.
replace_rgb({_,_,_}, {_,_,_}=RGB) -> RGB;
replace_rgb({_,_,_}, {R,G,B,_}) -> {R,G,B};
replace_rgb({_,_,_,_}, {_,_,_,_}=RGB) -> RGB;
replace_rgb({_,_,_,A}, {R,G,B}) -> {R,G,B,A}.

col_draw(Active, #fi{key=Key,x=X,y=Y0}, _, Common) ->
    Color = gb_trees:get(Key, Common),
    wings_io:border(X, Y0+3,
			 3*?CHAR_WIDTH, ?CHAR_HEIGHT, Color),
    Y = Y0+?CHAR_HEIGHT,
    if
	Active == true ->
	    wings_io:text_at(X, Y, "___");
	true -> ok
    end.

col_inside(Xm, Ym, #fi{x=X,y=Y}) ->
    X =< Xm andalso Xm < X+3*?CHAR_WIDTH andalso
	Y+3 =< Ym andalso Ym < Y+?CHAR_HEIGHT+2.

rgb_to_hsv(R,G,B) ->
    {H,S,V} = wings_color:rgb_to_hsv(R,G,B),
    {round(H),S,V}.

pick_color(#fi{key=Key}, Col, Common0) ->
    {R1,G1,B1,A1} =
	case gb_trees:get(Key, Common0) of
	    {R0,G0,B0} -> {R0,G0,B0,none};
	    {R0,G0,B0,A0} -> {R0,G0,B0,A0}
	end,
    {H1,S1,V1} = rgb_to_hsv(R1, G1, B1),
    RGBRange = [{range,{0.0,1.0}},color],
    HRange   = [{range,{0, 360}},color],
    SIRange  = [{range,{0.0,1.0}},color],
    Draw = fun(X, Y, _W, _H, Common) ->
		   Color = {gb_trees:get(r, Common),
			    gb_trees:get(g, Common),
			    gb_trees:get(b, Common)},
		   wings_io:sunken_rect(X, Y, ?COL_PREVIEW_SZ,
					?COL_PREVIEW_SZ-4, Color)
	   end,
    Aslider = case A1 of
		  none -> [];
		  _ ->
		      [separator,
		       {hframe,
			[{hframe,[{label,"A"},{text,A1,[{key,a}|RGBRange]}]},
			 {internal_slider,{text,A1,[{key,a}|RGBRange]}}]}]
	      end,
    {dialog,
     "Pick Color",
     [{hframe,
       [{custom,?COL_PREVIEW_SZ,?COL_PREVIEW_SZ,Draw},
	{vframe,
	 [{hframe,
	   [{vframe,
	     [{hframe,[{label,"R"},{text,R1,[{key,r}|RGBRange]}]},
	      {hframe,[{label,"G"},{text,G1,[{key,g}|RGBRange]}]},
	      {hframe,[{label,"B"},{text,B1,[{key,b}|RGBRange]}]}]},
	    {vframe,
	     [{internal_slider,{text,R1,[{key,r}|RGBRange]}},
	      {internal_slider,{text,G1,[{key,g}|RGBRange]}},
	      {internal_slider,{text,B1,[{key,b}|RGBRange]}}]}]},
	  separator,
	  {hframe,
	   [{vframe,
	     [{hframe,[{label,"H"},{text,H1,[{key,h}|HRange]}]},
	      {hframe,[{label,"S"},{text,S1,[{key,s}|SIRange]}]},
	      {hframe,[{label,"V"},{text,V1,[{key,v}|SIRange]}]}]},
	    {vframe,
	     [{internal_slider,{text,H1,[{key,h}|HRange]}},
	      {internal_slider,{text,S1,[{key,s}|SIRange]}},
	      {internal_slider,{text,V1,[{key,v}|SIRange]}}]}]}|Aslider]}]}],
     fun([{r,R},{g,G},{b,B}|More]) ->
	     Val = case keysearch(a, 1, More) of
		       false -> {R,G,B};
		       {value,{a,A}} -> {R,G,B,A}
		   end,
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
       (_, _, Custom, _) -> Custom
    end.

%%%
%%% Slider.
%%%

-define(SL_LENGTH, 150).
-define(SL_BAR_H, (?LINE_HEIGHT-3)).

-record(sl,
	{min,
	 range,
	 peer,
	 h					%Slider height.
	}).

slider(Field) ->
    Flags = element(size(Field), Field),
    {Min,Max} = proplists:get_value(range, Flags),
    Key = proplists:get_value(key, Flags),
    H = case proplists:get_bool(color, Flags) of
	    false -> 2;
	    true -> 9
	end,
    Sl = #sl{min=Min,range=(Max-Min)/?SL_LENGTH,peer=Key,h=H},
    Fun = slider_fun(),
    {Fun,false,Sl,?SL_LENGTH+4,?LINE_HEIGHT+2}.

slider_fun() ->
    fun({redraw,_Active}, Fi, Sl, Common) ->
	    slider_redraw(Fi, Sl, Common);
       (init, #fi{key=Key}, #sl{peer=undefined}=Sl, _) ->
	    Sl#sl{peer=Key-1};
       (value, _, _, _) ->
	    none;
       (Ev, Fi, Sl, Common) ->
	    slider_event(Ev, Fi, Sl, Common)
    end.

get_colRange(r,Common) ->
    B = gb_trees:get(b, Common),
    G = gb_trees:get(g, Common),
    [{0,G,B},{1,G,B}];
get_colRange(g, Common) ->
    R = gb_trees:get(r, Common),
    B = gb_trees:get(b, Common),
    [{R,0,B},{R,1,B}];
get_colRange(b, Common) ->
    R = gb_trees:get(r, Common),
    G = gb_trees:get(g, Common),
    [{R,G,0},{R,G,1}];
get_colRange(s, Common) ->
    H = gb_trees:get(h, Common),
    V = gb_trees:get(v, Common),
    S0 = wings_color:hsv_to_rgb(H,0.0,V),
    S1 = wings_color:hsv_to_rgb(H,1.0,V),
    [S0,S1];
get_colRange(v, Common) ->
    H = gb_trees:get(h, Common),
    S = gb_trees:get(s, Common),
    V0 = wings_color:hsv_to_rgb(H,S,0.0),
    V1 = wings_color:hsv_to_rgb(H,S,1.0),
    [V0,V1];
get_colRange(_E, _Common) ->
    [color(), color()].

hue_color_slider(Hue,_,_,_,_,_,_) when Hue > (360-60) ->
    ok;
hue_color_slider(Hue,S,V,X,W,Y,H) ->
    X0 = (X+1)+W*Hue/360.0,
    X1 = (X+1)+W*(Hue+60)/360.0,
    gl:vertex2f(X0,Y+H),
    gl:vertex2f(X0,Y+1),
    gl:color3fv(wings_color:hsv_to_rgb(60+Hue,S,V)),
    gl:vertex2f(X1,Y+1),
    gl:vertex2f(X1,Y+H),
    hue_color_slider(Hue+60,S,V,X,W,Y,H).

color_slider(h,X,W,Y,H,Common) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    S = gb_trees:get(s, Common),
    V = gb_trees:get(v, Common),
    wings_io:set_color(wings_color:hsv_to_rgb(0, S, V)),
    hue_color_slider(0,S,V,X,W,Y,H),
    gl:'end'();
color_slider(Peer,X,W,Y,H,Common) ->
    [SCol,ECol] = get_colRange(Peer, Common),
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    wings_io:set_color(SCol),
    gl:vertex2f(X+1,Y+H),
    gl:vertex2f(X+1,Y+1),
    wings_io:set_color(ECol),
    gl:vertex2f(X+1+W,Y+1),
    gl:vertex2f(X+1+W,Y+H),
    gl:'end'().

slider_redraw(#fi{x=X,y=Y0,w=W},
	      #sl{min=Min,range=Range,peer=Peer,h=H},
	      Common) ->
    Y = Y0+?LINE_HEIGHT div 2 + 2,
    blend(fun(Col) ->
		  wings_io:sunken_rect(X, Y-(H div 2), W, H, Col)
	  end),
    color_slider(Peer, X, W, Y-(H div 2), H, Common),
    Val = gb_trees:get(Peer, Common),
    Pos = trunc((Val-Min) / Range),
    blend(fun(Col) ->
		  wings_io:raised_rect(X+Pos, Y-(?SL_BAR_H div 2),
				       4, ?SL_BAR_H, Col)
	  end).

slider_event(#mousebutton{x=Xb,state=?SDL_RELEASED}, Fi, Sl, Common) ->
    slider_move(Xb, Fi, Sl, Common);
slider_event(#mousemotion{x=Xb}, Fi, Sl, Common) ->
    slider_move(Xb, Fi, Sl, Common);
slider_event(_, _, Sl, _) -> Sl.

slider_move(Xb, #fi{x=X}, #sl{min=Min,range=Range,peer=Peer}=Sl, Common0) ->
    Pos = max(0, min(Xb-X, ?SL_LENGTH)),
    Val0 = Min + Pos*Range,
    Val = if
	      is_integer(Min) -> round(Val0);
	      true -> Val0
	  end,
    Common1 = gb_trees:update(Peer, Val, Common0),
    Common  = update_color(Peer, Common1),
    {Sl,Common}.

update_color(RGB, Common0) when RGB == r; RGB == g; RGB == b ->
    R = gb_trees:get(r, Common0),
    G = gb_trees:get(g, Common0),
    B = gb_trees:get(b, Common0),
    {H,S,V} = rgb_to_hsv(R,G,B),
    Common1 = gb_trees:update(h, H, Common0),
    Common2 = gb_trees:update(s, S, Common1),
    gb_trees:update(v, V, Common2);
update_color(HSV, Common0) when HSV == h; HSV == s; HSV == v ->
    H = gb_trees:get(h, Common0),
    S = gb_trees:get(s, Common0),
    V = gb_trees:get(v, Common0),
    {R,G,B} = wings_color:hsv_to_rgb(H,S,V),
    Common1 = gb_trees:update(r, R, Common0),
    Common2 = gb_trees:update(g, G, Common1),
    gb_trees:update(b, B, Common2);
update_color(_, Common) ->
    Common.

%%%
%%% Label.
%%%

-record(label,
	{lines					%The lines.
	}).

label(Text, Flags) ->
    Limit = proplists:get_value(break, Flags, infinite),
    {_,Lines} = wings_text:break_lines([Text], Limit),
    Lbl = #label{lines=Lines},
    Fun = label_fun(),
    {W,H} = label_dimensions(Lines, 0, 2),
    {Fun,true,Lbl,W,H}.

label_dimensions([L|Lines], W0, H) ->
    case wings_text:width(L) of
	W when W > W0 -> label_dimensions(Lines, W, H+?LINE_HEIGHT);
	_ -> label_dimensions(Lines, W0, H+?LINE_HEIGHT)
    end;
label_dimensions([], W, H) -> {W,H}.

label_fun() ->
    fun({redraw,_Active}, #fi{x=X,y=Y}, #label{lines=Lines}, _Common) ->
	    label_draw(Lines, X, Y+?CHAR_HEIGHT);
       (_, _, Label, _) -> Label
    end.

label_draw([L|Lines], X, Y) ->
    wings_io:text_at(X, Y, L),
    label_draw(Lines, X, Y+?LINE_HEIGHT);
label_draw([], _, _) -> ok.

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
	 validator,
         password=false
	}).

text_field(Def, Flags) when is_float(Def) ->
    DefStr = text_val_to_str(Def),
    {Max,Validator} = float_validator(Flags),
    init_text(Def, DefStr, Max, false,
	      fun float_chars/1, Validator, Flags);
text_field(Def, Flags) when is_integer(Def) ->
    {Max,Validator} = integer_validator(Flags),
    init_text(Def, integer_to_list(Def), Max, true,
	      fun integer_chars/1, Validator, Flags);
text_field(Def, Flags) when is_list(Def) ->
    init_text(Def, Def, 30, false,
	      fun all_chars/1, fun(_) -> ok end, Flags).

init_text(Val, String, Max, IsInteger, Charset, Validator, Flags) ->
    Password = proplists:get_bool(password, Flags),
    Ts = #text{last_val=Val,bef=[],aft=String,max=Max,
	       integer=IsInteger,charset=Charset,
               validator=Validator,password=Password},
    Fun = fun gen_text_handler/4,
    {Fun,false,Ts,(1+Max)*?CHAR_WIDTH,?LINE_HEIGHT+2}.

text_val_to_str(Val) when is_float(Val) ->
    wings_util:nice_float(Val);
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
    case proplists:get_value(range, Flags) of
	undefined -> {8,fun accept_all/1};
	{Min,Max} when is_integer(Min), is_integer(Max), Min =< Max ->
	    Digits = trunc(math:log(Max-Min+1)/math:log(10))+2,
	    {Digits,integer_range(Min, Max)}
    end.

float_validator(Flags) ->
    case proplists:get_value(range, Flags) of
	undefined -> {12,fun accept_all/1};
	{Min,Max} when is_float(Min), is_float(Max), Min =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+8, 20),
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
	    case string_to_float(Str) of
		error -> wings_util:nice_float(Min);
		Float when Float < Min -> wings_util:nice_float(Min);
		Float when Float > Max -> wings_util:nice_float(Max);
		Float when is_float(Float) -> ok
	    end
    end.

text_get_val(#text{last_val=OldVal}=Ts) when is_integer(OldVal) ->
    case catch list_to_integer(get_text(validate_string(Ts))) of
	{'EXIT',_} -> OldVal;
	Val -> Val
    end;
text_get_val(#text{last_val=Val}=Ts) when is_float(Val) ->
    case string_to_float(get_text(Ts)) of
	error -> 0.0;
	F -> F
    end;
text_get_val(#text{last_val=Val}=Ts) when is_list(Val) ->
    get_text(Ts).

string_to_float(Str0) ->
    Str = case Str0 of
	      [$.|_]=T -> [$0|T];
	      [$-|[$.|_]=T] -> [$-,$0|T];
	      T -> T
	  end,
    case catch list_to_float(Str) of
	Float when is_float(Float) -> Float;
	_Other ->
	    case catch list_to_integer(Str) of
		Int when is_integer(Int) -> float(Int);
		_Crash -> error
	    end
    end.

gen_text_handler({redraw,Active}, Fi, Ts, Common) ->
    draw_text(Active, Fi, Ts, Common);
gen_text_handler(value, #fi{key=Key}, _, Common) ->
    gb_trees:get(Key, Common);
gen_text_handler(init, #fi{key=Key},
		 #text{last_val=Val}=Ts, Common0) ->
    Common = gb_trees:insert(Key, Val, Common0),
    {Ts#text{last_val=Val},Common};
gen_text_handler(Ev, #fi{key=Key}=Fi,
		 #text{last_val=Val0}=Ts0, Common0) ->
    Ts1 = case gb_trees:get(Key, Common0) of
	      Val0 -> Ts0;
	      Val1 ->
		  ValStr = text_val_to_str(Val1),
		  Ts0#text{bef=[],aft=ValStr}
	  end,
    Ts = text_event(Ev, Fi, Ts1),
    Val = text_get_val(Ts),
    Common1 = gb_trees:update(Key, Val, Common0),
    Common = update_color(Key, Common1),
    {Ts#text{last_val=Val},Common}.

draw_text(Active, Fi, #text{bef=Bef0,aft=Aft0,password=true}=Ts, Common) ->
    Bef = subst_stars(Bef0),
    Aft = subst_stars(Aft0),
    draw_text_0(Active, Fi, Ts#text{bef=Bef,aft=Aft}, Common);
draw_text(Active, Fi, Ts, Common) ->
    draw_text_0(Active, Fi, Ts, Common).

draw_text_0(false, #fi{key=Key,x=X0,y=Y0}, #text{max=Max}, Common) ->
    Val = gb_trees:get(Key, Common),
    Str = text_val_to_str(Val),
    gl:color3b(0, 0, 0),
    wings_io:sunken_rect(X0, Y0+2,
			 (Max+1)*?CHAR_WIDTH, ?CHAR_HEIGHT+1,
			 {1,1,1}),
    Y = Y0 + ?CHAR_HEIGHT,
    X = X0 + 4,
    wings_io:text_at(X, Y, Str);
draw_text_0(true, #fi{x=X0,y=Y0}, #text{sel=Sel,bef=Bef0,aft=Aft,max=Max}, _) ->
    gl:color3b(0, 0, 0),
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
	    gl:color3b(0, 0, 0);
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
	    gl:color3b(0, 0, 0)
    end.

draw_text_1(_, _, _, 0) ->
    gl:color3b(0, 0, 0);
draw_text_1(X, Y, [C|T], N) ->
    wings_io:text_at(X, Y, [C]),
    draw_text_1(X+?CHAR_WIDTH, Y, T, N-1).

subst_stars(S) ->
    lists:duplicate(length(S), $*).

validate_string(#text{validator=Validator,sel=Sel0}=Ts) ->
    case Validator(get_text(Ts)) of
	ok -> Ts;
	Str when is_list(Str) ->
	    Sel = case Sel0 of
		      0 -> Sel0;
		      _ -> length(Str)
		  end,
	    Ts#text{bef=[],aft=Str,sel=Sel}
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

increment(Ts0, Incr) ->
    Str0 = get_text(Ts0),
    case catch list_to_integer(Str0) of
	{'EXIT',_} -> Ts0;
	N ->
	    Str = integer_to_list(N+Incr),
	    Ts = Ts0#text{bef=reverse(Str),aft=[],sel=-length(Str)},
	    validate_string(Ts)
    end.

dialog_unzip(L) ->
    dialog_unzip(L, [], []).
dialog_unzip([{Lbl,F}|T], AccA, AccB) ->
    dialog_unzip(T, [{label,Lbl}|AccA], [F|AccB]);
dialog_unzip([], AccA, AccB) ->
    {reverse(AccA),reverse(AccB)}.

blend(Draw) ->
    wings_io:blend(color(), Draw).

color() ->
    wings_pref:get_value(dialog_color).
