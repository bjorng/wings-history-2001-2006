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
%%     $Id: wings_ask.erl,v 1.99 2003/10/23 13:31:41 raimo_niskanen Exp $
%%

-module(wings_ask).
-export([ask/3,ask/4,dialog/3,dialog/4,
	 hsv_to_rgb/1,hsv_to_rgb/3,rgb_to_hsv/1,rgb_to_hsv/3]).

-import(wings_util, [min/2,max/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(HMARGIN, 16).
-define(VMARGIN, 8).

-define(HFRAME_SPACING, (3*?CHAR_WIDTH div 2)).

-define(IS_SHIFTED(Mod), ((Mod) band ?SHIFT_BITS =/= 0)).

-import(lists, [reverse/1,reverse/2,duplicate/2,keysearch/3,member/2]).

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(DEBUG_DISPLAY(X), (erlang:display({?MODULE,?LINE,X}))).
-define(DEBUG_FORMAT(Fmt,Args), 
	(io:format(?MODULE_STRING":"++integer_to_list(?LINE)++" "++Fmt, Args))).
-else.
-define(DEBUG_DISPLAY(_X), true).
-define(DEBUG_FORMAT(_Fmt,_Args), ok).
-endif.

-record(s,
	{w,
	 h,
	 ox,
	 oy,
	 call,
	 focus,
	 fi,					%Static data for all fields.
	 coords,				%Coordinates for hit testing.
	 store,					%Data for all fields.
	 level,					%Levels of nesting.
	 owner=Owner,				%Where to send result.
	 grab_win				%Previous grabbed focus window.
	}).

%% Static data for each field.
-record(fi,
	{handler,				%Handler fun.
	 key,					%Field key.
	 inert=true,				%Inert field.
	 disabled=false,			%Disabled field, temporary inert
	 hook,					%Field hook fun/2
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

setup_ask(Qs, Fun) ->
    ?DEBUG_FORMAT("setup_ask~n  (~p,~n   ~p)~n", [Qs,Fun]),
    Qs1 = normalize(Qs),
%%%    ?DEBUG_FORMAT("normalize() ->~n  ~p~n", [Qs1]),
    Qs2 = propagate_sizes(Qs1),
%%%    ?DEBUG_FORMAT(":propagate_sizes() ->~n  ~p~n", [Qs2]),
    {Fis,Store} = flatten_fields(Qs2, gb_trees:empty()),
    ?DEBUG_FORMAT("flatten_fields() ->~n  {~p,~n   ~p}~n", [Fis,Store]),
    FisT = list_to_tuple(Fis),
    {#fi{w=W,h=H},_} = Qs2,
    Owner = wings_wm:this(),
    S = #s{w=W,h=H,call=Fun,fi=FisT,focus=size(FisT),owner=Owner,store=Store},
    init_fields(S).


init_fields(S) ->
    init_fields(1, S).

init_fields(I, #s{fi=Fis,store=Store}=S) ->
    Store1 = init_fields(I, Fis, Store),
    S#s{store=Store1}.

init_fields(I, Fis, Store0) when I =< size(Fis) ->
    #fi{handler=Handler} = Fi = element(I, Fis),
%    ?DEBUG_DISPLAY([init,Fi,I]),
    case Handler(init, Fi, I, Store0) of
	{store,Store} ->
	    init_fields(I+1, Fis, Store);
	keep ->
	    init_fields(I+1, Fis, Store0)
    end;
init_fields(_, _, Store) -> Store.


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
    {replace,
     fun(Ev) -> 
	     case catch event(Ev, S) of
		 {'EXIT',Reason} ->
		     io:format("Dialog ~p~nCrashed for event ~p~n"
			       "With reason ~p", [S,Ev,Reason]),
		     delete(S);
		 Result -> Result
	     end
     end}.

event(redraw, S) ->
    ?DEBUG_DISPLAY(redraw),
    redraw(S);
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
event({action,Action}, S) ->
    field_event(Action, S);
event(Ev, S) -> field_event(Ev, S).

event_key({key,?SDLK_ESCAPE,_,_}, S) ->
    escape_pressed(S);
event_key({key,?SDLK_TAB,Mod,_}, S) when ?IS_SHIFTED(Mod) ->
    get_event(next_focus(S, -1));
event_key({key,?SDLK_TAB,_,_}, S) ->
    get_event(next_focus(S, 1));
event_key({key,_,_,$\t}, S) ->
    get_event(next_focus(S, 1));
event_key({key,?SDLK_KP_ENTER,_,_}, S) ->
    enter_pressed({key,$\r,$\r,$\r}, S);
event_key({key,_,_,$\r}=Ev, S) ->
    enter_pressed(Ev, S);
event_key({key,_,_,$!}=Ev, S) ->
    ?DEBUG_DISPLAY([Ev,S]),
    field_event(Ev, S);
event_key(Ev, S) ->
    field_event(Ev, S).

enter_pressed(Ev, #s{focus=I,store=Store}=S) ->
    case field_type(I, Store) of
	but -> field_event(Ev, S);
	_ -> return_result(S)
    end.

escape_pressed(#s{fi=Fi,store=Store}=S) ->
    escape_pressed_1(1, Fi, Store, S).

escape_pressed_1(I, Fi, Store, S) when I =< size(Fi) ->
    case field_type(I, Store) of
	but ->
	    case member(cancel, field_flags(I, Fi)) of
		false ->
		    escape_pressed_1(I+1, Fi, Store, S);
		true ->
		    field_event({key,$\s,$\s,$\s}, S#s{focus=I})
	    end;
	_ ->
	    escape_pressed_1(I+1, Fi, Store, S)
    end;
escape_pressed_1(_, _, _, _) -> keep.

field_type(I, Store) ->
    case gb_trees:lookup(-I, Store) of
	{value,F} -> element(1, F);
	none -> undefined
    end.

field_flags(I, Fi) ->
    #fi{flags=Flags} = element(I, Fi),
    Flags.

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
mouse_event(X0, Y0, #mousebutton{state=State}=Ev, 
	    #s{focus=I0,ox=Ox,oy=Oy,fi=Fis}=S0) ->
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
	#fi{inert=false,x=Lx,y=Uy,w=W,h=H,disabled=false}
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
	#fi{disabled=true} -> next_focus_1(I, Dir, S);
	_ -> set_focus(I, S)
    end.

set_focus(I, #s{focus=OldFocus,fi=Fis,store=Store0}=S) ->
    ?DEBUG_DISPLAY({set_focus,[OldFocus,I]}),
    #fi{handler=OldHandler} = OldFi = element(OldFocus, Fis),
    Store2 = case OldHandler({focus,false}, OldFi, OldFocus, Store0) of
		 {store,Store1} -> Store1;
		 _ -> Store0
	     end,
    #fi{handler=Handler} = Fi = element(I, Fis),
    Store = case Handler({focus,true}, Fi, I, Store2) of
		{store,Store3} -> Store3;
		_ -> Store2
	    end,
    S#s{focus=I,store=Store}.

		    

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
field_event(Ev, I, #s{fi=Fis,store=Store}=S) ->
    #fi{handler=Handler} = Fi = element(I, Fis),
    ?DEBUG_DISPLAY({field_handler,[I,Ev]}),
    Result = Handler(Ev, Fi, I, Store),
    ?DEBUG_DISPLAY({field_handler,Result}),
    case Result of
	ok ->
	    return_result(S);
	cancel ->
	    delete(S);
	keep ->
	    get_event(S);
	{recursive,Return} ->
	    Return;
	{drag,{_,_}=WH,DropData} ->
	    wings_wm:drag(Ev, WH, DropData);
	{store,Store1} ->
	    get_event(S#s{store=Store1});
	Action when is_function(Action) ->
	    Res = collect_result(S),
	    Action(Res),
	    delete(S)
    end.

return_result(#s{call=EndFun,owner=Owner}=S) ->
    Res = collect_result(S),
    ?DEBUG_DISPLAY({return_result,Res}),
    case EndFun(Res) of
	{command_error,Message} ->
	    wings_util:message(Message),
	    get_event(S);
	ignore ->
	    delete(S);
	#st{}=St ->
	    ?DEBUG_DISPLAY({return_result,[Owner,new_state]}),
	    wings_wm:send(Owner, {new_state,St}),
	    delete(S);
	Action when is_tuple(Action); is_atom(Action) ->
	    ?DEBUG_DISPLAY({return_result,[Owner,{action,Action}]}),
	    wings_wm:send(Owner, {action,Action}),
	    delete(S)
    end.

collect_result(#s{fi=Fis,store=Store}) ->
    collect_result(1, Fis, Store).

collect_result(I, Fis, Store) when I =< size(Fis) ->
    case element(I, Fis) of
	#fi{inert=true} ->
	    collect_result(I+1, Fis, Store);
	#fi{handler=Handler,key=Key}=Fi ->
	    case Handler(value, Fi, I, Store) of
		none ->
		    collect_result(I+1, Fis, Store);
		{value,Res} ->
		    [case Key of 
			 0 -> Res; 
			 _ -> {Key,Res} 
		     end|collect_result(I+1, Fis, Store)]
	    end
    end;
collect_result(_, _, _) -> [].

redraw(#s{w=W,h=H,ox=Ox,oy=Oy,focus=Focus,fi=Fis0,store=Store} = S) ->
    wings_io:ortho_setup(),
    gl:translated(Ox, Oy, 0),
    blend(fun(Color) ->
		  wings_io:border(-?HMARGIN, -?VMARGIN,
				  W+2*?HMARGIN-1, H+2*?VMARGIN-1,
				  Color)
	  end),
    case draw_fields(1, Fis0, Focus, Store, keep) of
	keep -> keep;
	Fis ->
	    case element(Focus, Fis) of
		#fi{disabled=true} -> get_event(next_focus(S#s{fi=Fis}, 1));
		_ -> get_event(S#s{fi=Fis})
	    end
    end.

draw_fields(I, Fis, Focus, Store, Keep) when I =< size(Fis) ->
    #fi{handler=Handler,disabled=Disabled} = Fi = element(I, Fis),
    case Handler({redraw,I =:= Focus}, Fi, I, Store) of
	enable when Disabled == true ->
	    draw_fields(I+1, setelement(I, Fis, Fi#fi{disabled=false}),
			Focus, Store, change);
	disable when Disabled == false -> 
	    draw_fields(I+1, setelement(I, Fis, Fi#fi{disabled=true}),
			Focus, Store, change);
	_ ->
	    draw_fields(I+1, Fis, Focus, Store, Keep)
    end;
draw_fields(_, _, _, _, keep) -> keep;
draw_fields(_, Fis, _, _, change) -> Fis.

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
normalize({alt,{Var,Def},Prompt,Val}, Fi) ->
    normalize_field(radiobutton(Var, Def, Prompt, Val), [], Fi);
normalize({alt,{Var,Def},Prompt,Val,Flags}, Fi) ->
    normalize_field(radiobutton(Var, Def, Prompt, Val), Flags, Fi);
normalize({key_alt,{Var,Def},Prompt,Val}, Fi) ->
    normalize_field(radiobutton(Var, Def, Prompt, Val), [{key,Var}], Fi);
normalize({key_alt,{Var,Def},Prompt,Val,Flags}, Fi) ->
    normalize_field(radiobutton(Var, Def, Prompt, Val), [{key,Var}|Flags], Fi);
normalize({menu,Menu,{Var,Def}}, Fi) ->
    normalize_field(menu(Var, Def, Menu), [{key,Var}], Fi);    
normalize({menu,Menu,VarDef}, Fi) ->
    normalize({menu,Menu,VarDef,[]}, Fi);
normalize({menu,Menu,{Var,Def},Flags}, Fi) ->
    normalize_field(menu(Var, Def, Menu), [{key,Var}|Flags], Fi);    
normalize({menu,Menu,Def,Flags}, Fi) ->
    normalize_field(menu(0, Def, Menu), Flags, Fi);    
normalize({button,Action}, Fi) when is_atom(Action) ->
    Label = button_label(Action),
    Flags = case Action of
		cancel -> [cancel];
		_ -> []
	    end,
    normalize_field(button(Label, Action), Flags, Fi);
normalize({button,Label,Action}, Fi) ->
    normalize_field(button(Label, Action), [], Fi);
normalize({button,Label,Action,Flags}, Fi) ->
    normalize_field(button(Label, Action), Flags, Fi);
normalize({custom,W,H,Custom}, Fi) ->
    normalize_field(custom(W, H, Custom), [], Fi);
normalize({custom,W,H,Custom,Flags}, Fi) ->
    normalize_field(custom(W, H, Custom), Flags, Fi);
normalize({slider,{text,_,Flags}=Field}, Fi) ->
    normalize({hframe,[Field,{slider,Flags}]}, Fi);
normalize({slider,Flags}, Fi) when is_list(Flags) ->
    normalize_field(slider(Flags), Flags, Fi);
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

-record(vframe, {fields=[]}).

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
    Fun = fun frame_event/4,
    Fi = Fi0#fi{handler=Fun,inert=true,flags=Flags,
		w=W,h=H,ipadx=Ipadx,ipady=Ipady},
    {Fi,{vframe,Fields}}.

vframe_1([Q|Qs], #fi{y=Y}=Fi0, W0, Acc) ->
    {#fi{w=W,h=H}=Fi,Priv} = normalize(Q, Fi0),
    vframe_1(Qs, Fi#fi{y=Y+H}, max(W0, W), [{Fi,Priv}|Acc]);
vframe_1([], #fi{y=Y}, W, Fields) ->
    {reverse(Fields),Y,W}.

-record(hframe, {fields=[]}).

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
    Fun = fun frame_event/4,
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
%%% Calculate flattened fields array, set keys and store fields data.
%%%

flatten_fields(FiPriv, Store0) ->
    {Store,_,Fis} = flatten_fields([FiPriv], Store0, 1, []),
    {lists:reverse(Fis),Store}.

flatten_fields([], Store, I, Fis) ->
    {Store,I,Fis};
flatten_fields([{#fi{flags=Flags}=Fi0,Priv}|FiPrivs], Store0, I0, Fis0) ->
    Fi = Fi0#fi{key=proplists:get_value(key, Flags, 0),
		hook=proplists:get_value(hook, Flags)},
    case Priv of
	#vframe{} ->
	    {Store,I,Fis} = 
		flatten_fields(Priv#vframe.fields, Store0, I0+1, [Fi|Fis0]),
	    flatten_fields(FiPrivs, Store, I, Fis);
	#hframe{} ->
	    {Store,I,Fis} = 
		flatten_fields(Priv#hframe.fields, Store0, I0+1, [Fi|Fis0]),
	    flatten_fields(FiPrivs, Store, I, Fis);
	undefined ->
	    flatten_fields(FiPrivs, Store0, I0+1, Fis0);
	_ ->
	    Store = gb_trees:insert(-I0, Priv, Store0),
	    flatten_fields(FiPrivs, Store, I0+1, [Fi|Fis0])
    end.



%% 
%% Hframe and Vframe
%%

frame_event({redraw,_Active}, Fi, _, _) ->
    frame_redraw(Fi);
frame_event(_, _, _, _) -> keep.

frame_redraw(#fi{flags=[]}) -> ok;
frame_redraw(#fi{x=X,y=Y0,w=W,h=H0,flags=Flags}) ->
    case proplists:get_value(title, Flags) of
	undefined -> keep;
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
	    wings_io:text_at(TextPos, Y0+?CHAR_HEIGHT, Title),
	    keep
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
    Fun = fun separator_event/4,
    {Fun,true,{separator},4*?CHAR_WIDTH,10}.

separator_event({redraw,_Active}, Fi, _, _) ->
    separator_draw(Fi);
separator_event(_, _, _, _) -> keep.

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
    ?CHECK_ERROR(),
    keep.





%%%
%%% Checkbox
%%%

-define(CB_SIZE, 8).

-record(cb,
	{label,
	 labelw,				%Width of label in pixels.
	 spacew,				%Width of a space character.
	 val
	}).

checkbox(Label, Val) ->
    LabelWidth = wings_text:width(Label),
    SpaceWidth = wings_text:width(" "),
    Cb = #cb{label=Label,val=Val,labelw=LabelWidth,spacew=SpaceWidth},
    Fun = fun cb_event/4,
    {Fun,false,Cb,LabelWidth+SpaceWidth+?CB_SIZE,?LINE_HEIGHT+2}.

cb_event(init, #fi{key=Key}, I, Store) ->
    #cb{val=Val} = gb_trees:get(-I, Store),
    {store,gb_trees:enter(ck(Key, I), Val, Store)};
cb_event(value, #fi{key=Key}, I, Store) ->
    {value,gb_trees:get(ck(Key, I), Store)};
cb_event({redraw,Active}, #fi{key=Key,hook=Hook}=Fi, I, Store) ->
    Ck = ck(Key, I),
    Cb = gb_trees:get(-I, Store),
    Val = gb_trees:get(Ck, Store),
    DisEnable = hook(Hook, is_disabled, [Ck, I, Store]),
    cb_draw(Active, Fi, Cb, Val, DisEnable);
cb_event({key,_,_,$\s}, #fi{key=Key}, I, Store) ->
    Ck = ck(Key, I),
    Val = gb_trees:get(Ck, Store),
    {store,gb_trees:update(Ck, not Val, Store)};
cb_event(#mousebutton{x=Xb,state=?SDL_PRESSED}, #fi{x=X,key=Key}, I, Store) ->
    #cb{labelw=LblW,spacew=SpaceW} = gb_trees:get(-I, Store),
    if
	Xb-X < LblW+4*SpaceW ->
	    Ck = ck(Key, I),
	    Val = gb_trees:get(Ck, Store),
	    {store,gb_trees:update(Ck, not Val, Store)};
	true -> keep
    end;
cb_event(_Ev, _Fi, _I, _Store) -> keep.

cb_draw(Active, #fi{x=X,y=Y0}, #cb{label=Label}, Val, DisEnable) ->
    wings_io:sunken_rect(X, Y0+?CHAR_HEIGHT-9, 8, 8, 
			 case DisEnable of
			     disable -> color();
			     _ -> {1,1,1}
			 end),
    Y = Y0+?CHAR_HEIGHT,
    case Val of
	false -> ok;
	true -> wings_io:text_at(X+1, Y, [crossmark])
    end,
    wings_io:text_at(X+2*?CHAR_WIDTH, Y, Label),
    if
	Active == true ->
	    wings_io:text_at(X+2*?CHAR_WIDTH, Y,
			     duplicate(length(Label), $_));
	true -> ok
    end,
    DisEnable.





%%%
%%% Radio button
%%%

-record(rb,
	{var,
	 def,					%Default value.
	 val,
	 label,
	 labelw,			    %Width of label in pixels.
	 spacew				  %Width of a space character.
	}).

radiobutton(Var, Def, Label, Val) when not is_integer(Var) ->
    LabelWidth = wings_text:width(Label),
    SpaceWidth = wings_text:width(" "),
    Rb = #rb{var=Var,def=Def,val=Val,label=Label,
	     labelw=LabelWidth,spacew=SpaceWidth},
    Fun = fun rb_event/4,
    {Fun,false,Rb,LabelWidth+2*SpaceWidth,?LINE_HEIGHT+2}.

rb_event(init, _Fi, I, Store) ->
    #rb{var=Var,def=Def,val=Val} = gb_trees:get(-I, Store),
    case Val of
	Def -> {store,gb_trees:enter(Var, Val, Store)};
	_ -> keep
    end;
rb_event({redraw,Active}, #fi{hook=Hook}=Fi, I, Store) ->
    #rb{var=Var} = Rb = gb_trees:get(-I, Store),
    DisEnable = hook(Hook, is_disabled, [Var, I, Store]),
    rb_draw(Active, Fi, Rb, gb_trees:get(Var, Store), DisEnable);
rb_event(value, _Fi, I, Store) ->
    #rb{var=Var,val=Val} = gb_trees:get(-I, Store),
    case gb_trees:get(Var, Store) of
	Val -> {value,Val};
	_ -> none
    end;
rb_event({key,_,_,$\s}, _Fi, I, Store) ->
    rb_set(gb_trees:get(-I, Store), Store);
rb_event(#mousebutton{x=Xb,state=?SDL_RELEASED}, #fi{x=X}, I, Store) ->
    #rb{labelw=LblW,spacew=SpaceW} = Rb = gb_trees:get(-I, Store),
    if
	Xb-X < LblW+4*SpaceW -> rb_set(Rb, Store);
	true -> keep
    end;
rb_event(_Ev, _Fi, _I, _Store) -> keep.

rb_draw(Active, #fi{x=X,y=Y0}, #rb{label=Label,val=Val}, Common, DisEnable) ->
    Y = Y0+?CHAR_HEIGHT,
    gl:color4fv(case DisEnable of
		    disable -> color();
		    _ -> {1,1,1,1}
		end),
    Fg = <<
	     2#00111000,
	     2#01111100,
	     2#11111110,
	     2#11111110,
	     2#11111110,
	     2#01111100,
	     2#00111000>>,
    gl:rasterPos2i(X, Y),
    gl:bitmap(7, 7, -1, 0, 7, 0, Fg),
    gl:color3b(0, 0, 0),
    B = case Common of
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
    end,
    DisEnable.

rb_set(#rb{var=Var,val=Val}, Store) ->
    {store,gb_trees:update(Var, Val, Store)}.


%%%
%%% Menu
%%%

-record(menu,
	{var,
	 def,
	 menu
	}).

menu(Var, Def, Menu) ->
    W = menu_width(Menu, 0) + 2*wings_text:width(" ") + 10,
    M = #menu{var=Var,def=Def,menu=Menu},
    Fun = fun menu_event/4,
    {Fun,false,M,W,?LINE_HEIGHT+4}.

menu_event({redraw,Active}, #fi{hook=Hook}=Fi, I, Store) ->
    #menu{var=Var} = M = gb_trees:get(-I, Store),
    Ck = ck(Var, I),
    DisEnable = hook(Hook, is_disabled, [Ck, I, Store]),
    menu_draw(Active, Fi, M, gb_trees:get(Ck, Store), DisEnable);
menu_event(init, _Fi, I, Store) ->
    #menu{var=Var,def=Def} = gb_trees:get(-I, Store),
    {store,gb_trees:enter(ck(Var, I), Def, Store)};
menu_event(value, _Fi, I, Store) ->
    #menu{var=Var} = gb_trees:get(-I, Store),
    {value,gb_trees:get(ck(Var, I), Store)};
menu_event({key,_,_,$\s}, #fi{hook=Hook}=Fi, I, Store) ->
    #menu{var=Var} = M = gb_trees:get(-I, Store),
    Ck = ck(Var, I),
    Val = gb_trees:get(Ck, Store),
    Disabled = hook(Hook, menu_disabled, [Ck, I, Store]),
    menu_popup(Fi, M, Val, Disabled);
menu_event(#mousebutton{button=1,state=?SDL_PRESSED}, #fi{hook=Hook}=Fi, 
	   I, Store) ->
    #menu{var=Var} = M = gb_trees:get(-I, Store),
    Ck = ck(Var, I),
    Val = gb_trees:get(Ck, Store),
    Disabled = hook(Hook, menu_disabled, [Ck, I, Store]),
    menu_popup(Fi, M, Val, Disabled);
menu_event({popup_result,Val}, _Fi, I, Store) ->
    #menu{var=Var} = gb_trees:get(-I, Store),
    {store,gb_trees:update(ck(Var, I), Val, Store)};
menu_event(_, _, _, _) -> keep.

menu_width([{S,_}|T], W0) ->
    case wings_text:width(S) of
	W when W < W0 -> menu_width(T, W0);
	W -> menu_width(T, W)
    end;
menu_width([], W) -> W.

menu_draw(Active, #fi{x=X,y=Y0,w=W,h=H}, #menu{menu=Menu}, Val, DisEnable) ->
    blend(fun(Col) ->
		  case DisEnable of
		      disable ->
			  wings_io:border(X, Y0+1, W-?CHAR_WIDTH+10, H-3, Col);
		      _ ->
			  wings_io:raised_rect(X, Y0+1, W-?CHAR_WIDTH+10, 
					       H-3, Col)
		  end
	  end),
    Y = Y0+?CHAR_HEIGHT,
    ValStr = [Desc || {Desc,V} <- Menu, V =:= Val],
    wings_io:text_at(X+5, Y, ValStr),
    Xr = X + W-8,
    case Active of
	false -> wings_io:border(Xr-1, Y-9, 10, 10, ?PANE_COLOR);
	true -> wings_io:sunken_rect(Xr-1, Y-9, 10, 10, ?PANE_COLOR)
    end,
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
    DisEnable.

%% Menu popup

-record(popup,
	{parent,				%Parent window name.
	 sel,					%Selected element (integer).
	 orig_sel,				%Original selection (integer).
	 menu					%Tuple.
	}).

menu_popup(#fi{x=X0,y=Y0,w=W}, #menu{menu=Menu0}, Val, Disabled) ->
    {X1,Y1} = wings_wm:local2global(X0+?HMARGIN, Y0+?VMARGIN),
    case [V || {_,X}=V <- Menu0, not lists:member(X, Disabled)] of
	[] -> ok;
	Menu1 ->
	    Sel = popup_find_index(Menu1, Val),
	    Menu = list_to_tuple(Menu1),
	    Mh = size(Menu)*?LINE_HEIGHT,
	    Ps = #popup{parent=wings_wm:this(),sel=Sel,orig_sel=Sel,menu=Menu},
	    Op = {seq,push,get_popup_event(Ps)},
	    X = X1-2*?CHAR_WIDTH,
	    Y = Y1-2-(Sel-1)*?LINE_HEIGHT,
	    wings_wm:new(menu_popup, {X,Y,highest}, 
			 {W+2*?CHAR_WIDTH,Mh+10}, Op),
	    wings_wm:grab_focus(menu_popup)
    end,
    keep.

popup_find_index(Menu, Val) ->
    popup_find_index(Menu, Val, 1).

popup_find_index([], _Val, _I) -> 1;
popup_find_index([{_,Val}|_], Val, I) -> I;
popup_find_index([_|T], Val, I) -> popup_find_index(T, Val, I+1).

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
popup_event(#mousebutton{button=1,state=?SDL_RELEASED}, Ps) ->
    popup_key($ , 0, $ , Ps);
popup_event(#keyboard{sym=Sym,mod=Mod,unicode=Unicode}, Ps) -> 
    popup_key(Sym, Mod, Unicode, Ps);
popup_event(_Event, _Ps) -> 
    ?DEBUG_DISPLAY([_Event,_Ps]),
    keep.

popup_key(?SDLK_TAB, Mod, _Unicode, Ps) when ?IS_SHIFTED(Mod) -> 
    popup_key(16, Ps);
popup_key(?SDLK_TAB, _Mod, _Unicode, Ps) -> 
    popup_key(14, Ps);
popup_key(?SDLK_UP, _Mod, _Unicode, Ps) -> 
    popup_key(16, Ps);
popup_key(?SDLK_DOWN, _Mod, _Unicode, Ps) -> 
    popup_key(14, Ps);
popup_key(?SDLK_KP_ENTER, _Mod, _Unicode, Ps) -> 
    popup_key($ , Ps);
popup_key(?SDLK_ESCAPE, _Mod, _Unicode, 
	  #popup{parent=Parent,menu=Menu,orig_sel=Sel}) -> 
    {_,Val} = element(Sel, Menu),
    wings_wm:send(Parent, {popup_result,Val}),
    delete;
popup_key(_Sym, _Mod, $\r, Ps) -> 
    popup_key($ , Ps);
popup_key(_Sym, _Mod, Unicode, Ps) -> 
    popup_key(Unicode, Ps).

popup_key(16, #popup{sel=Sel}=Ps) %Ctrl-P
  when Sel > 1 ->
    wings_wm:dirty(),
    get_popup_event(Ps#popup{sel=Sel-1});
popup_key(14, #popup{menu=Menu,sel=Sel}=Ps) %Ctrl-N
  when Sel < size(Menu) ->
    wings_wm:dirty(),
    get_popup_event(Ps#popup{sel=Sel+1});
popup_key($ , #popup{parent=Parent,menu=Menu,sel=Sel}) -> %Space
    {_,Val} = element(Sel, Menu),
    wings_wm:send(Parent, {popup_result,Val}),
    delete;
popup_key(_Unicode, _Ps) ->
    ?DEBUG_DISPLAY([_Unicode,_Ps]),
    keep.

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
%%% Buttons
%%%

-record(but,
	{label,					%Textual label.
	 action}).

button(Label, Action) ->
    W = lists:max([wings_text:width([$\s,$\s|Label]),
		   wings_text:width(" cancel ")]),
    But = #but{label=Label,action=Action},
    Fun = fun button_event/4,
    {Fun,false,But,W,?LINE_HEIGHT+2+2}.

button_label(ok) -> "OK";
button_label(S) when is_list(S) -> S;
button_label(Act) -> wings_util:cap(atom_to_list(Act)).

button_event({redraw,Active}, #fi{key=Key,hook=Hook}=Fi, I, Store) ->
    DisEnable = hook(Hook, is_disabled, [ck(Key, I), I, Store]),
    button_draw(Active, Fi, gb_trees:get(-I, Store), DisEnable);
button_event(value, _, _, _) ->
    none;
button_event(#mousebutton{x=X,y=Y,state=?SDL_RELEASED}, 
	     #fi{x=Bx,y=By,w=W,h=H}, I, Store) 
  when Bx =< X, X =< Bx+W, By =< Y, Y =< By+H ->
    #but{action=Action} = gb_trees:get(-I, Store),
    Action;
button_event({key,_,_,K}, _, I, Store) when K =:= $\r; K =:= $\s ->
    #but{action=Action} = gb_trees:get(-I, Store),
    Action;
button_event(_Ev, _Fi, _I, _Store) -> keep.

button_draw(Active, #fi{x=X,y=Y0,w=W,h=H}, #but{label=Label}, DisEnable) ->
    Y = Y0+?CHAR_HEIGHT+2,
    case DisEnable of disable -> gl:enable(?GL_POLYGON_STIPPLE); _ -> ok end,
    blend(fun(Col) ->
		  case DisEnable of
		      disable ->
			  wings_io:border(X, Y0+2, W, H-4, Col);
		      _ ->
			  wings_io:raised_rect(X, Y0+2, W, H-4, Col)
		  end
	  end),
    TextX = X + 2 + (W-wings_text:width(Label)) div 2,
    wings_io:text_at(TextX, Y, Label),
    if
	Active == true ->
	    L = length(Label),
	    wings_io:text_at(TextX, Y, duplicate(L, $_)),
	    keep;
	true -> keep
    end,
    case DisEnable of disable -> gl:disable(?GL_POLYGON_STIPPLE); _ -> ok end,
    DisEnable.



%%%
%%% Color box.
%%%

-define(COL_PREVIEW_SZ, 60).

-record(col,
	{val}).

color(RGB) ->
    Col = #col{val=RGB},
    Fun = fun col_event/4,
    {Fun,false,Col,3*?CHAR_WIDTH,?LINE_HEIGHT+2}.

col_event({redraw,Active}, #fi{key=Key,hook=Hook}=Fi, I, Store) ->
    Var = ck(Key, I),
    DisEnable = hook(Hook, is_disabled, [Var, I, Store]),
    col_draw(Active, Fi, gb_trees:get(Var, Store), DisEnable);
col_event(init, #fi{key=Key}, I, Store) ->
    #col{val=RGB} = gb_trees:get(-I, Store),
    {store,gb_trees:enter(ck(Key, I), RGB, Store)};
col_event(value, #fi{key=Key}, I, Store) ->
    {value,gb_trees:get(ck(Key, I), Store)};
col_event({key,_,_,$\s}, Fi, I, Store) ->
    pick_color(Fi, I, Store);
col_event(#mousemotion{x=Xm,y=Ym}, #fi{key=Key}=Fi, I, Store) ->
    case col_inside(Xm, Ym, Fi) of
	true -> keep;
	false -> 
	    RGB = gb_trees:get(ck(Key, I), Store),
	    {drag,{3*?CHAR_WIDTH,?CHAR_HEIGHT},{color,RGB}}
    end;
col_event(#mousebutton{x=Xm,y=Ym,state=?SDL_RELEASED}, Fi, I, Store) ->
    case col_inside(Xm, Ym, Fi) of
	true -> pick_color(Fi, I, Store);
	false -> keep
    end;
col_event({drop,{color,RGB1}}, #fi{key=Key}, I, Store) ->
    RGB0 = gb_trees:get(ck(Key, I), Store),
    RGB = replace_rgb(RGB0, RGB1),
    {store,gb_trees:update(ck(Key, I), RGB, Store)};
col_event(_Ev, _Fi, _I, _Store) -> keep.

%% replace_rgb(OldRGBA, NewRGBA) -> RGBA
%%  Replace a color (RGB + possibly A) with a new color, 
%%  making sure that the new color has the same number of components.
replace_rgb({_,_,_}, {_,_,_}=RGB) -> RGB;
replace_rgb({_,_,_}, {R,G,B,_}) -> {R,G,B};
replace_rgb({_,_,_,_}, {_,_,_,_}=RGB) -> RGB;
replace_rgb({_,_,_,A}, {R,G,B}) -> {R,G,B,A}.

col_draw(Active, #fi{x=X,y=Y0}, RGB, DisEnable) ->
    case DisEnable of
	disable ->
	    wings_io:border(X, Y0+3, 3*?CHAR_WIDTH, ?CHAR_HEIGHT, RGB);
	_ ->
	    wings_io:sunken_rect(X, Y0+3, 3*?CHAR_WIDTH, ?CHAR_HEIGHT, RGB)
    end,
    Y = Y0+?CHAR_HEIGHT,
    if
	Active == true ->
	    wings_io:text_at(X, Y, "___"),
	    keep;
	true -> keep
    end,
    DisEnable.

col_inside(Xm, Ym, #fi{x=X,y=Y}) ->
    X =< Xm andalso Xm < X+3*?CHAR_WIDTH andalso
	Y+3 =< Ym andalso Ym < Y+?CHAR_HEIGHT+2.

pick_color(#fi{key=Key}, I, Store) ->
    RGB0 = gb_trees:get(ck(Key, I), Store),
    wings_color:choose(RGB0, fun(RGB) -> {drop,{color,RGB}} end).



%%%
%%% Custom
%%%

-record(custom,
	{handler, val}
       ).

custom(W, H, Handler) ->
    Custom = #custom{handler=Handler},
    Fun = fun custom_event/4,
    {Fun,true,Custom,W,H}.

custom_event({redraw,_Active}, #fi{x=X,y=Y,w=W,h=H}, I, Store) ->
    #custom{handler=Handler} = gb_trees:get(-I, Store),
    Handler(X, Y, W, H, Store);
custom_event(_, _, _, _) -> keep.



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
    Fun = fun label_event/4,
    {W,H} = label_dimensions(Lines, 0, 2),
    {Fun,true,Lbl,W,H}.

label_dimensions([L|Lines], W0, H) ->
    case wings_text:width(L) of
	W when W > W0 -> label_dimensions(Lines, W, H+?LINE_HEIGHT);
	_ -> label_dimensions(Lines, W0, H+?LINE_HEIGHT)
    end;
label_dimensions([], W, H) -> {W,H}.

label_event({redraw,_Active}, #fi{x=X,y=Y}, I, Store) ->
	    #label{lines=Lines} = gb_trees:get(-I, Store),
	    label_draw(Lines, X, Y+?CHAR_HEIGHT);
label_event(_, _, _, _) -> keep.

label_draw([L|Lines], X, Y) ->
    wings_io:text_at(X, Y, L),
    label_draw(Lines, X, Y+?LINE_HEIGHT),
    keep;
label_draw([], _, _) -> keep.





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
         password=false,
	 slider_h,
	 slider_range,
	 val
	}).

text_field(Val, Flags) ->
    IsInteger = is_integer(Val),
    ValStr = text_val_to_str(Val),
    {Max,Validator,Charset,Range} = validator(Val, Flags),
    {SliderH,Val} =
	case proplists:get_value(color, Flags) of
	    {_Type,_KeyC}=TK ->
		{9,TK};
	    undefined ->
		{2,Val}
	end,
    Password = proplists:get_bool(password, Flags),
    Ts = #text{last_val=Val,bef=[],aft=ValStr,max=Max,
	       integer=IsInteger,charset=Charset,
               validator=Validator,password=Password,
	       slider_h=SliderH,slider_range=Range,val=Val},
    Fun = fun gen_text_handler/4,
    {Fun,false,Ts,(1+Max)*?CHAR_WIDTH,?LINE_HEIGHT+2}.

text_val_to_str(Val) when is_float(Val) ->
    wings_util:nice_float(Val);
text_val_to_str(Val) when is_integer(Val) ->
    integer_to_list(Val);
text_val_to_str(Val) when is_list(Val) ->
    Val.

%% -> {MaxFieldWidth,FieldValidator,CharsetPredicate}
%%
validator(Val, Flags) when is_integer(Val) ->
    integer_validator(Flags);
validator(Val, Flags) when is_float(Val) ->
    float_validator(Flags);
validator(Val, _Flags) when is_list(Val) ->
    string_validator().

integer_validator(Flags) ->
    case proplists:get_value(range, Flags) of
	undefined -> {8,fun accept_all/1,integer_chars(),undefined};
	{Min,Max}=Range when is_integer(Min), is_integer(Max), Min =< Max ->
	    Digits = trunc(math:log(Max-Min+1)/math:log(10))+2,
	    {Digits,integer_range(Min, Max),integer_chars(Min, Max),Range}
    end.

float_validator(Flags) ->
    case proplists:get_value(range, Flags) of
	undefined -> {12,fun accept_all/1,float_chars(),undefined};
	{Min,Max}=Range when is_float(Min), is_float(Max), Min =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+8, 20),
	    {Digits,float_range(Min, Max),float_chars(Min, Max),Range}
    end.

string_validator() ->
    {30,fun accept_all/1,fun all_chars/1,undefined}.

integer_chars() ->
    integer_chars(-1, 1).

integer_chars(Min, Max) ->
    fun ($-) when Min < 0 -> true;
	($+) when Max > 0 -> true;
	(C) when $0 =< C, C =< $9 -> true;
	(_) -> false
    end.

float_chars() ->
    float_chars(-1.0, +1.0).
	    
float_chars(Min, Max) ->
    fun ($-) when Min < 0.0 -> true;
	($+) when Max > 0.0 -> true;
	(C) when $0 =< C, C =< $9 -> true;
	($.) -> true;
	(_) -> false
    end.
	    
all_chars(_) -> true.

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

accept_all(_) -> ok.



text_get_val(#text{last_val=OldVal}=Ts) when is_integer(OldVal) ->
    case catch list_to_integer(get_text(validate_string(Ts))) of
	{'EXIT',_} -> OldVal;
	Val -> Val
    end;
text_get_val(#text{last_val=OldVal}=Ts) when is_float(OldVal) ->
    case string_to_float(get_text(validate_string(Ts))) of
	error -> OldVal;
	Val -> Val
    end;
text_get_val(#text{last_val=Val}=Ts) when is_list(Val) ->
    get_text(Ts).

string_to_float(Str0) ->
    Str = case Str0 of
	      [$.|_]=T -> [$0|T];
	      [$-|[$.|_]=T] -> [$-,$0|T];
	      [$+|[$.|_]=T] -> [$+,$0|T];
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

gen_text_handler({redraw,true}, #fi{key=Key,hook=Hook}=Fi, I, Store) ->
    DisEnable = hook(Hook, is_disabled, [ck(Key, I), I, Store]),
    draw_text_active(Fi, gb_trees:get(-I, Store), DisEnable);
gen_text_handler({redraw,false}, #fi{key=Key,hook=Hook}=Fi, I, Store) ->
    Var = ck(Key, I),
    DisEnable = hook(Hook, is_disabled, [Var, I, Store]),
    Val = gb_trees:get(Var, Store),
    draw_text_inactive(Fi, gb_trees:get(-I, Store), Val, DisEnable);
gen_text_handler(value, #fi{key=Key}, I, Store) ->
    {value,gb_trees:get(ck(Key, I), Store)};
gen_text_handler(init, #fi{key=Key}, I, Store) ->
    #text{last_val=Val} = gb_trees:get(-I, Store),
    {store,gb_trees:enter(ck(Key, I), Val, Store)};
gen_text_handler(Ev, #fi{key=Key}=Fi, I, Store0) ->
    #text{last_val=Val0} = Ts0 = gb_trees:get(-I, Store0),
    Ts1 = case gb_trees:get(ck(Key, I), Store0) of
	      Val0 -> Ts0;
	      Val1 ->
		  ?DEBUG_DISPLAY([Key,I,Val1]),
		  ValStr = text_val_to_str(Val1),
		  Ts0#text{bef=[],aft=ValStr}
	  end,
    Ts2 = text_event(Ev, Fi, Ts1),
    {Ts,Store} = case text_get_val(Ts2) of
		     Val0 -> {Ts2,Store0};
		     Val  -> {Ts2#text{last_val=Val},
			      gb_trees:update(ck(Key, I), Val, Store0)}
		 end,
    {store,gb_trees:update(-I, Ts, Store)}.

draw_text_inactive(#fi{x=X0,y=Y0}, #text{max=Max,password=Password}, 
		   Val, DisEnable) ->
    Str0 = text_val_to_str(Val),
    Str = case Password of 
	      true -> stars(Str0);
	      false -> Str0 
	  end,
    gl:color3b(0, 0, 0),
    wings_io:sunken_rect(X0, Y0+2,
			 (Max+1)*?CHAR_WIDTH, ?CHAR_HEIGHT+1,
			 case DisEnable of
			     disable -> color();
			     _ -> {1,1,1}
			 end),
    Y = Y0 + ?CHAR_HEIGHT,
    X = X0 + (?CHAR_WIDTH div 2),
    wings_io:text_at(X, Y, Str),
    DisEnable.

draw_text_active(#fi{x=X0,y=Y0}, 
		 #text{sel=Sel,bef=Bef,aft=Aft,max=Max,password=Password},
		 DisEnable) ->
    gl:color3b(0, 0, 0),
    wings_io:sunken_rect(X0, Y0+2,
			 (Max+1)*?CHAR_WIDTH, ?CHAR_HEIGHT+1,
			 case DisEnable of
			     disable -> color();
			     _ -> {1,1,1}
			 end),
    Y = Y0 + ?CHAR_HEIGHT,
    X = X0 + (?CHAR_WIDTH div 2),
    Len = length(Bef),
    Str = case Password of
	      true -> stars(Len+length(Aft));
	      false -> reverse(Bef, Aft)
	  end,
    ?DEBUG_DISPLAY([Sel,Bef,Aft]),
    wings_io:text_at(X, Y, Str),
    case {DisEnable,abs(Sel)} of
	{disable,_} ->
	    ok;
	{_,0} ->
	    gl:color3f(1, 0, 0),
	    X1 = X+Len*?CHAR_WIDTH,
	    wings_io:text_at(X1, Y, [caret]),
	    gl:color3b(0, 0, 0);
	{_,N} ->
	    Skip = min(Len, Len+Sel),
	    SelStr = string:substr(Str, Skip+1, N),
	    gl:color3f(0, 0, 0.5),
	    X1 = X+Skip*?CHAR_WIDTH,
 	    gl:recti(X1, Y-?CHAR_HEIGHT+3, X1+N*?CHAR_WIDTH, Y+2),
 	    gl:color3f(1, 1, 1),
	    wings_io:text_at(X1, Y, SelStr),
	    gl:color3b(0, 0, 0)
    end,
    DisEnable.

stars(N) when integer(N) ->
    lists:duplicate(N, $*);
stars(Str) ->
    stars(length(Str)).

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

get_text_r(#text{bef=Bef,aft=Aft}) ->
    reverse(Aft, Bef).

text_event({key,Sym,Mod,Unicode}, _Fi, Ts) ->
    key(Sym, Mod, Unicode, Ts);
text_event({focus,false}, _Fi, Ts) ->
    validate_string(Ts);
text_event({focus,true}, _Fi, Ts) ->
    Str = get_text(Ts),
    Ts#text{bef=[],sel=length(Str),aft=Str};
text_event(#mousebutton{x=X,state=?SDL_PRESSED}, Fi, Ts0) ->
    Ts = text_pos(X, Fi, Ts0),
    ?DEBUG_DISPLAY(Ts),
    Ts;
text_event(#mousebutton{x=X,state=?SDL_RELEASED}, Fi, Ts0) ->
    Ts = text_sel(X, Fi, Ts0),
    ?DEBUG_DISPLAY(Ts),
    Ts;
text_event(#mousemotion{x=X}, Fi, Ts) ->
    text_sel(X, Fi, Ts);
text_event(_Ev, _Fi, Ts) -> Ts.

text_pos(Mx0, #fi{x=X}, #text{bef=Bef,aft=Aft}=Ts) ->
    D = round((Mx0-X-(?CHAR_WIDTH div 2))/?CHAR_WIDTH) - length(Bef),
    ?DEBUG_DISPLAY([Mx0,X,?CHAR_WIDTH,length(Bef)]),
    text_pos_1(D, Bef, Aft, Ts).

text_pos_1(D, [C|Bef], Aft, Ts) when D < 0 ->
    text_pos_1(D+1, Bef, [C|Aft], Ts);
text_pos_1(D, Bef, [C|Aft], Ts) when D > 0 ->
    text_pos_1(D-1, [C|Bef], Aft, Ts);
text_pos_1(_, Bef, Aft, Ts) ->
    Ts#text{bef=Bef,aft=Aft,sel=0}.

text_sel(Mx0, #fi{x=X}, #text{bef=Bef}=Ts) ->
    Len = length(Bef),
    text_sel_1(round((Mx0-X-(?CHAR_WIDTH div 2))/?CHAR_WIDTH)-Len, Len, Ts).

text_sel_1(D, Len, #text{}=Ts) when D < 0 ->
    Ts#text{sel=max(D, -Len)};
text_sel_1(D, _Len, #text{aft=Aft}=Ts) ->
    Ts#text{sel=min(D, length(Aft))}.

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
key(2, Mod, #text{sel=Sel,bef=Bef}=Ts) when ?IS_SHIFTED(Mod) ->	%Ctrl-B
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
key(1, _, #text{}=Ts) ->			%Ctrl-A
    Ts#text{sel=0,bef=[],aft=get_text(Ts)};
key(5, Mod, #text{aft=Aft}=Ts) when ?IS_SHIFTED(Mod) ->
    Ts#text{sel=length(Aft)};
key(5, _, #text{}=Ts) ->			%Ctrl-E
    Ts#text{sel=0,bef=get_text_r(Ts),aft=[]};
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



%%%
%%% Slider
%%%

-define(SL_LENGTH, 150).
-define(SL_BAR_H, (?LINE_HEIGHT-3)).

-record(sl,
	{min,
	 range,
	 color,
	 h
	}).

slider(Flags) ->
    {Min,Max} = proplists:get_value(range, Flags),
    {Color,H} = 
	case proplists:get_value(color, Flags) of
	    undefined -> {undefined,2};
	    {T,_,_}=C when T==r;T==g;T==b;T==h;T==s;T==v -> {C,9}
	end,
    Sl = #sl{min=Min,range=Max-Min,color=Color,h=H},
    Fun = fun slider_event/4,
    {Fun,false,Sl,?SL_LENGTH+4,?LINE_HEIGHT+2}.

slider_event(init, #fi{key=Key,flags=Flags}, I, Store) ->
    case proplists:get_value(value, Flags) of
	undefined ->
	    keep;
	Val ->
	    {store,gb_trees:enter(sk(Key, I), Val, Store)}
    end;
slider_event({redraw,Active}, #fi{key=Key,hook=Hook}=Fi, I, Store) ->
    Sl = gb_trees:get(-I, Store),
    Val = gb_trees:get(sk(Key, I), Store),
    DisEnable = hook(Hook, is_disabled, [Sl, I, Store]),
    slider_redraw(Active, Fi, Sl, Val, Store, DisEnable);
slider_event(value, #fi{flags=Flags,key=Key}, I, Store) ->
    case proplists:get_value(value, Flags) of
	undefined ->
	    none;
	_ ->
	    {value,gb_trees:get(sk(Key, I), Store)}
    end;
slider_event(#mousebutton{x=Xb,state=?SDL_RELEASED}, Fi, I, Store) ->
    slider_event_move(Xb, Fi, I, Store);
slider_event(#mousemotion{x=Xb}, Fi, I, Store) ->
    slider_event_move(Xb, Fi, I, Store);
slider_event({key,?SDLK_LEFT,_,_}, Fi, I, Store) ->
    slider_move(-1, Fi, I, Store);
slider_event({key,?SDLK_RIGHT,_,_}, Fi, I, Store) ->
    slider_move(1, Fi, I, Store);
slider_event({key,?SDLK_LEFT,_,_}, Fi, I, Store) ->
    slider_move(1, Fi, I, Store);
slider_event({key,?SDLK_UP,_,_}, Fi, I, Store) ->
    slider_move(10, Fi, I, Store);
slider_event({key,?SDLK_DOWN,_,_}, Fi, I, Store) ->
    slider_move(-10, Fi, I, Store);
slider_event({key,?SDLK_HOME,_,_}, #fi{key=Key,hook=Hook}, I, Store) ->
    #sl{min=Min} = gb_trees:get(-I, Store),
    slider_update(Hook, Min, Key, I, Store);
slider_event({key,?SDLK_END,_,_}, #fi{key=Key,hook=Hook}, I, Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    slider_update(Hook, Min+Range, Key, I, Store);
slider_event({key,_,_,6}, Fi, I, Store) -> %Ctrl-F
    slider_move(1, Fi, I, Store);
slider_event({key,_,_,2}, Fi, I, Store) -> %Ctrl-B
    slider_move(-1, Fi, I, Store);
slider_event({key,_,_,16}, Fi, I, Store) -> %Ctrl-P
    slider_move(10, Fi, I, Store);
slider_event({key,_,_,14}, Fi, I, Store) -> %Ctrl-N
    slider_move(-10, Fi, I, Store);
slider_event(_, _, _, _) -> keep.

slider_move(D0, #fi{key=Key,hook=Hook}, I, Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    Val0 = gb_trees:get(sk(Key, I), Store),
    D = if
	     is_integer(Min), D0 > 0 -> max(round(D0*0.01*Range), 1);
	     is_integer(Min), D0 < 0 -> min(round(D0*0.01*Range), -1);
	     is_float(Min) -> D0*0.01*Range
	 end,
    Val = max(Min, min(Min+Range, Val0+D)),
    slider_update(Hook, Val, Key, I, Store).

slider_event_move(Xb, #fi{x=X,key=Key,hook=Hook}, I, Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    Pos = max(0, min(Xb-X, ?SL_LENGTH)),
    V = Min + Pos*Range/?SL_LENGTH,
    Val = if
	      is_integer(Min) -> round(V);
	      true -> V
	  end,
    slider_update(Hook, Val, Key, I, Store).

slider_update(Hook, Val, Key, I, Store) ->
    Sk = sk(Key, I),
    ?DEBUG_DISPLAY([Val,Sk]),
    hook(Hook, update, [Sk, I, Val, Store]).

slider_redraw(Active, Fi, #sl{color={T,K1,K2}}=Sl, Val, Store, DisEnable) ->
    V1 = gb_trees:get(K1, Store),
    V2 = gb_trees:get(K2, Store),
    slider_redraw_1(Active, Fi, Sl, {T, {Val,V1,V2}}, DisEnable);
slider_redraw(Active, Fi, #sl{color=undefined}=Sl, Val, _Store, DisEnable) ->
    slider_redraw_1(Active, Fi, Sl, Val, DisEnable).

slider_redraw_1(Active, #fi{x=X,y=Y0,w=W}, #sl{min=Min,range=Range,h=H}, C,
		DisEnable) ->
    Y = Y0+?LINE_HEIGHT div 2 + 2,
    blend(fun(Col) ->
		  wings_io:sunken_rect(X, Y-(H div 2), W, H, Col)
	  end),
    Val = color_slider(C, X, W, Y-(H div 2), H),
    Pos = round(?SL_LENGTH * (Val-Min) / Range),
    blend(fun(Col) ->
		  XPos = X+Pos,
		  YPos = Y-(?SL_BAR_H div 2),
		  case {DisEnable,Active} of
		      {disable,_} ->
			  wings_io:border(XPos, YPos, 4, ?SL_BAR_H, Col);
		      {_,false} ->
			  wings_io:raised_rect(XPos, YPos, 4, ?SL_BAR_H, Col);
		      {_,true} ->
			  wings_io:sunken_rect(XPos, YPos, 4, ?SL_BAR_H, Col)
		  end
	  end),
    DisEnable.

color_slider({h,{Hue,S,V}},X,W,Y,H) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    hue_color_slider(S,V,X,W,Y,H),
    gl:'end'(),
    Hue;
color_slider(C,X,W,Y,H) ->
    {Val,[SCol,ECol]} = get_col_range(C),
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    wings_io:set_color(SCol),
    gl:vertex2f(X+1,Y+H),
    gl:vertex2f(X+1,Y+1),
    wings_io:set_color(ECol),
    gl:vertex2f(X+1+W,Y+1),
    gl:vertex2f(X+1+W,Y+H),
    gl:'end'(),
    Val.

hue_color_slider(S, V, X, W, Y, H) ->
    wings_io:set_color(hsv_to_rgb(0, S, V)),
    hue_color_slider(S, V, X, W, Y, H, 0).

hue_color_slider(_, _, _, _, _, _, Hue) when Hue > (360-60) ->
    ok;
hue_color_slider(S, V, X, W, Y, H, Hue) ->
    X0 = (X+1)+W*Hue/360.0,
    X1 = (X+1)+W*(Hue+60)/360.0,
    gl:vertex2f(X0,Y+H),
    gl:vertex2f(X0,Y+1),
    gl:color3fv(hsv_to_rgb(60+Hue, S, V)),
    gl:vertex2f(X1,Y+1),
    gl:vertex2f(X1,Y+H),
    hue_color_slider(S, V, X, W, Y, H, Hue+60).

get_col_range({r, {R,G,B}}) ->
    {R,[{0,G,B},{1,G,B}]};
get_col_range({g, {G,R,B}}) ->
    {G,[{R,0,B},{R,1,B}]};
get_col_range({b, {B,R,G}}) ->
    {B,[{R,G,0},{R,G,1}]};
get_col_range({s, {S,H,V}}) ->
    S0 = hsv_to_rgb(H,0.0,V),
    S1 = hsv_to_rgb(H,1.0,V),
    {S,[S0,S1]};
get_col_range({v, {V,H,S}}) ->
    V0 = hsv_to_rgb(H,S,0.0),
    V1 = hsv_to_rgb(H,S,1.0),
    {V,[V0,V1]};
get_col_range(Val) when is_integer(Val); is_float(Val) ->
    {Val,[color(),color()]}.



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

rgb_to_hsv({R,G,B}) ->
    rgb_to_hsv(R, G, B).

rgb_to_hsv(R,G,B) ->
    {H,S,V} = wings_color:rgb_to_hsv(R,G,B),
    {round(H),S,V}.

hsv_to_rgb({H,S,V}) ->
    hsv_to_rgb(H, S, V).

hsv_to_rgb(H, S, V) ->
    wings_color:hsv_to_rgb(H, S, V).

%% Common data storage key
ck(0, I) when is_integer(I), I > 0 ->
    I;
ck(Key, I) when not is_integer(Key), is_integer(I) ->
    Key.

%% Slider data storage key
sk(0, I) when is_integer(I), I > 0 ->
    I-1;
sk(Key, I) when not is_integer(Key), is_integer(I) ->
    Key.

hook(Hook, is_disabled, [Var, I, Store]) ->
    case Hook of
	undefined -> keep;
	_ when is_function(Hook) ->
	    case Hook(is_disabled, {Var,I,Store}) of
		void -> keep;
		true -> disable;
		false -> enable
	    end
    end;
hook(Hook, update, [Var, I, Val, Store]) ->
    case Hook of
	undefined ->
	    {store,gb_trees:update(Var, Val, Store)};
	_ when is_function(Hook) ->
	    case Hook(update, {Var,I,Val,gb_trees:update(Var, Val, Store)}) of
		void -> keep;
		keep -> keep;
		{store,_}=Result -> Result
	    end
    end;
hook(Hook, menu_disabled, [Var, I, Store]) ->
    case Hook of
	undefined -> [];
	_ when is_function(Hook) ->
	    case Hook(menu_disabled, {Var,I,Store}) of
		void -> [];
		Disabled when list(Disabled) -> Disabled
	    end
    end.
