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
%%     $Id: wings_ask.erl,v 1.129 2003/11/27 17:37:22 raimo_niskanen Exp $
%%

-module(wings_ask).
-export([ask/3,ask/4,dialog/3,dialog/4,
	 hsv_to_rgb/1,hsv_to_rgb/3,rgb_to_hsv/1,rgb_to_hsv/3]).

%% Debug exports
-export([binsearch/2]).

-import(wings_util, [min/2,max/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(HMARGIN, 16).
-define(VMARGIN, 8).

-define(HFRAME_SPACING, (3*?CHAR_WIDTH div 2)).

-define(IS_SHIFTED(Mod), ((Mod) band ?SHIFT_BITS =/= 0)).
-define(BUTTON_MASK,
	(?SDL_BUTTON_LMASK bor ?SDL_BUTTON_MMASK bor ?SDL_BUTTON_RMASK)).

-import(lists, [reverse/1,reverse/2,duplicate/2,keysearch/3,member/2]).

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(DEBUG_DISPLAY(X), 
	begin io:format("~p~n", [{?MODULE,?LINE,(X)}]), true end).
-define(DEBUG_FORMAT(Fmt,Args), 
	(io:format(?MODULE_STRING":"++integer_to_list(?LINE)++" "++Fmt, Args))).
-define(DMPTREE(Fi), begin io:format("~p~n", [{?MODULE,?LINE}]), 
			   dmptree(Fi), 
			   io:format("~p~n", [{?MODULE,?LINE}]), 
			   ok end).
-else.
-define(DEBUG_DISPLAY(_X), true).
-define(DEBUG_FORMAT(_Fmt,_Args), ok).
-define(DMPTREE(Fi), ok).
-endif.

-record(s,
	{w,
	 h,
	 ox,
	 oy,
	 call,
	 focus,					%Field index
	 focusable,				%Tuple of field indexes
	 mouse_focus=false,			%Mouse hit the field
	 fi,					%Static data for all fields.
	 n,					%Number of fields.
	 coords,				%Coordinates for hit testing.
	 store,					%Data for all fields.
	 level,					%Levels of nesting.
	 owner=Owner,				%Where to send result.
	 grab_win				%Previous grabbed focus window.
	}).

%% Static data for every field.
-record(fi,
	{handler,				%Handler fun.
	 key=0,					%Field key.
	 index,					%Field index
	 inert=true,				%Inert field.
	 disabled=false,			%Disabled field, temporary inert
	 hook,					%Field hook fun/2
	 flags=[],				%Flags field.
	 x,y,					%Upper left position.
	 w,h,					%Width, height.
	 extra					%Container or leaf data
	}).

%% Extra static data for container fields

-record(container,
	{type,					%vframe|hframe|oframe
	 x,y,					%Container pos
	 w,h,					%Container size
	 fields,				%Contained fields, tuple()
	 minimized,				%true|false|undefined
	 active					%integer()|undefined
	}).

%% Extra static data for leaf fields

-record(leaf,
	{w,h					%Natural (min) size
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
ask_unzip([{Label,{menu,_,_,_}=Menu}|T], AccA, AccB) ->
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

-record(position, {position}).

do_dialog(Title, Qs, Level, Fun) ->
    GrabWin = wings_wm:release_focus(),
    S0 = setup_ask(Qs, Fun),
    #s{w=W0,h=H0} = S0,
    W = W0 + 2*?HMARGIN,
    H = H0 + 2*?VMARGIN,
    S = #s{fi=Fi,store=Store} = 
	S0#s{ox=?HMARGIN,oy=?VMARGIN,level=Level,grab_win=GrabWin},
    Name = {dialog,hd(Level)},
    setup_blanket(Name, S),
    Op = {seq,push,get_event(S)},
    {{X,Y},Anchor} 
	= case find_position(Fi, Store) of
	      [] -> {mouse_pos(),n};
	      [#fi{index=Index}|_] ->
		  case gb_trees:get(-Index, Store) of
		      #position{position=undefined} -> {mouse_pos(),n};
		      #position{position=Pos} -> {Pos,nw}
		  end
	  end,
    wings_wm:toplevel(Name, Title, {X,Y,highest}, {W,H}, 
		      [{anchor,Anchor}], Op),
    wings_wm:set_prop(Name, drag_filter, fun(_) -> yes end),
    ?DEBUG_DISPLAY({W,H}),
    keep.

mouse_pos() ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    {X,Y-?LINE_HEIGHT}.
    

setup_ask(Qs, Fun) ->
    ?DEBUG_FORMAT("setup_ask~n  (~p,~n   ~p)~n", [Qs,Fun]),
    {Fi0,Sto,N} = mktree(Qs, gb_trees:empty()),
%    ?DEBUG_FORMAT("Sto = ~p~nN = ~p~nFi0 = ~p~n", [Sto,N,Fi0]),
    Fi = #fi{w=W,h=H} = layout(Fi0, Sto),
    ?DMPTREE(Fi),
    Focusable = focusable(Fi),
    Owner = wings_wm:this(),
    S = #s{w=W,h=H,call=Fun,focus=N,focusable=Focusable,
	   fi=Fi,n=N,owner=Owner,store=init_fields(Fi, Sto)},
    ?DEBUG_DISPLAY({W,H}),
    next_focus(1, S).


setup_blanket(Dialog, #s{fi=Fi,store=Sto}) ->
    EyePicker = case find_eyepicker(Fi, Sto) of
		    [#fi{}|_] -> true;
		    [] -> false
		end,

    %% The menu blanket window lies below the dialog, covering the entire
    %% screen and ignoring most events. Keyboard events will be forwarded
    %% to the active dialog window.
    Op = {push,fun(Ev) -> blanket(Ev, Dialog, EyePicker) end},
    {TopW,TopH} = wings_wm:top_size(),
    wings_wm:new({blanket,Dialog}, {0,0,highest}, {TopW,TopH}, Op).

delete_blanket(#s{level=[Level|_]}) ->
    wings_wm:delete({blanket,{dialog,Level}});
delete_blanket(#s{level=undefined}) -> ok.

blanket(#keyboard{}=Ev, Dialog, _) ->
    wings_wm:send(Dialog, Ev);
blanket(#mousemotion{}, _, true) ->
    wings_io:eyedropper(),
    keep;
blanket(#mousebutton{button=1,state=?SDL_RELEASED,x=X0,y=Y0}, Dialog, true) ->
    {X,Y0} = wings_wm:local2global(X0, Y0),
    {_,H} = wings_wm:top_size(),
    Y = H - Y0,
    Mem = sdl_util:alloc(3, ?GL_UNSIGNED_BYTE),
    gl:readBuffer(?GL_FRONT),
    gl:readPixels(X, Y, 1, 1, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    gl:readBuffer(?GL_BACK),
    [R,G,B] = sdl_util:read(Mem, 3),
    Col = {R/255,G/255,B/255},
    wings_wm:send(Dialog, {picked_color,Col}),
    keep;
blanket(_, _, _) -> keep.

get_event(S) ->
    wings_wm:dirty(),
    {replace,
     fun(Ev) ->
	     case catch event1(Ev, S) of
		 {'EXIT',Reason} ->
		     %% dmptree(S#s.fi),
		     io:format("Dialog crashed for event ~p~n"
			       "With reason ~p~n", [Ev,Reason]),
		     delete(S);
		 Result -> Result
	     end
     end}.

event1(redraw, S) ->
    ?DEBUG_DISPLAY(redraw),
    redraw(S);
event1(got_focus, _) ->
    wings_wm:message(" "),
    keep;
event1(lost_focus, _) ->
    wings_wm:message("  "),
    keep;
event1(Ev=#mousemotion{}, S) ->
    mouse_event(Ev, S);
event1(Ev, S) -> 
    wings_wm:message(""),
    event(Ev, S).

event({current_state,_}, _) ->
    keep;
event(#keyboard{sym=Sym,mod=Mod,unicode=Unicode}, S) ->
    event_key({key,Sym,Mod,Unicode}, S);
event(Ev=#mousebutton{}, S) ->
      mouse_event(Ev, S);
event({drop,{X,Y},DropData}, S) ->
    drop_event(X, Y, DropData, S);
event({action,Action}, S) ->
    field_event(Action, S);
event(Ev={picked_color,_}, #s{fi=Fi,store=Sto}=S) ->
    field_event(Ev, S, find_eyepicker(Fi, Sto));
event(Ev, S) -> field_event(Ev, S).

event_key({key,?SDLK_ESCAPE,_,_}, S) ->
    escape_pressed(S);
event_key({key,?SDLK_TAB,Mod,_}, S) when ?IS_SHIFTED(Mod) ->
    get_event(next_focus(-1, S));
event_key({key,?SDLK_TAB,_,_}, S) ->
    get_event(next_focus(+1, S));
event_key({key,_,_,$\t}, S) ->
    get_event(next_focus(+1, S));
event_key({key,?SDLK_KP_ENTER,Mod,_}, S) ->
    enter_pressed({key,$\r,Mod,$\r}, S);
event_key({key,_,_,$\r}=Ev, S) ->
    enter_pressed(Ev, S);
event_key(Ev, S) ->
    field_event(Ev, S).

enter_pressed(Ev, S0=#s{fi=TopFi,store=Store}) ->
    case find_ok(TopFi, Store) of
	[] -> return_result(S0);
	Path -> 
	    S = set_focus(Path, S0, false),
	    case field_event(Ev, S, Path) of
		keep -> get_event(S);
		Other -> Other
	    end
    end.

escape_pressed(S0=#s{fi=TopFi,store=Sto}) -> 
    case find_cancel(TopFi, Sto) of
	[] -> keep;
	Path -> 
	    S = set_focus(Path, S0, false),
	    case field_event({key,$\s,0,$\s}, S, Path) of
		keep -> get_event(S);
		Other -> Other
	    end
    end.

delete(#s{level=[_],grab_win=GrabWin}=S) ->
    delete_blanket(S),
    wings_wm:grab_focus(GrabWin),
    delete;
delete(S) ->
    delete_blanket(S),
    delete.

mouse_event(Ev=#mousemotion{x=X0,y=Y0}, #s{ox=Ox,oy=Oy}=S) ->
    X = X0-Ox,
    Y = Y0-Oy,
    field_event(Ev#mousemotion{x=X,y=Y}, S);
mouse_event(Ev=#mousebutton{x=X0,y=Y0,state=State}, 
	    S0=#s{mouse_focus=MouseFocus,ox=Ox,oy=Oy,fi=TopFi}) ->
    X = X0-Ox,
    Y = Y0-Oy,
    case State of
	?SDL_RELEASED ->
	    case MouseFocus of
		true ->field_event(Ev#mousebutton{x=X,y=Y}, S0);
		false -> keep
	    end;
	?SDL_PRESSED ->
	    case mouse_to_field(X, Y, TopFi) of
		Path=[#fi{inert=false,disabled=false}|_] ->
		    S = set_focus(Path, S0, true),
		    case field_event(Ev#mousebutton{x=X,y=Y}, S, Path) of
			keep -> get_event(S);
			Other -> Other
		    end;
		_ -> get_event(S0#s{mouse_focus=false})
	    end
    end.

drop_event(X, Y, DropData, #s{ox=Ox,oy=Oy,fi=TopFi}=S0) ->
    case mouse_to_field(X-Ox, Y-Oy, TopFi) of
	Path=[#fi{inert=false,disabled=false}|_] ->
	    S = set_focus(Path, S0, true),
	    case field_event({drop,DropData}, S, Path) of
		keep -> get_event(S);
		Other -> Other
	    end;
	_ -> get_event(S0#s{mouse_focus=false})
    end.


next_focus(Dir, S=#s{focus=Index,focusable=Focusable,fi=TopFi}) ->
    J = case binsearch(fun (I) when I < Index -> -1;
			   (I) when Index < I -> +1;
			   (_) -> 0 end, 
		       Focusable) of
	    {I,_} when 0 < Dir -> I;
	    {_,I} -> I;
	    I -> I
	end,
    N = size(Focusable),
    case (J+Dir) rem N of
	K when K =< 0 -> 
	    Path = get_fi(element(N+K, Focusable), TopFi),
	    set_focus(Path, S, false);
	K -> 
	    Path = get_fi(element(K, Focusable), TopFi),
	    set_focus(Path, S, false)
    end.

set_focus(NewPath=[#fi{index=NewIndex,handler=NewHandler}|_], 
	  #s{focus=OldIndex,focusable=_Focusable,
	     fi=TopFi,store=Store0}=S, MouseFocus) ->
    ?DEBUG_DISPLAY({set_focus,[OldIndex,NewIndex,_Focusable]}),
    OldPath = [#fi{handler=OldHandler}|_] = get_fi(OldIndex, TopFi),
    Store2 = case OldHandler({focus,false}, OldPath, Store0) of
		 {store,Store1} -> Store1;
		 _ -> Store0
	     end,
    Store = case NewHandler({focus,true}, NewPath, Store2) of
		{store,Store3} -> Store3;
		_ -> Store2
	    end,
    S#s{focus=NewIndex,mouse_focus=MouseFocus,store=Store}.


field_event(Ev=#mousemotion{x=X,y=Y,state=Bst}, S=#s{fi=Fi})
  when (Bst band ?BUTTON_MASK) =:= 0 ->
    case mouse_to_field(X, Y, Fi) of
	Path=[#fi{index=_I,inert=Inert,disabled=Disabled,
		  flags=Flags,extra=Extra}|_] ->
	    ?DEBUG_DISPLAY({field_event,[_I,Ev]}),
	    Enabled = (Inert =:= false) and (not Disabled),
	    wings_wm:allow_drag(Enabled andalso member(drag, Flags)),
	    case Extra of
		#container{} when Enabled -> field_event(Ev, S, Path);
		#container{} -> wings_wm:message("");
		_ -> 
		    case proplists:get_value(info, Flags) of
			undefined -> wings_wm:message("");
			Info -> wings_wm:message(Info)
		    end
	    end;
	_ ->
	    wings_wm:allow_drag(false),
	    wings_wm:message("")
    end, keep;
field_event({key,_,_,_}=Ev, S=#s{focus=I,fi=Fi}) ->
    %% Key events should always be sent, even if there
    %% is no mouse focus. Otherwise you can't TAB to
    %% text fields.
    field_event(Ev, S, get_fi(I, Fi));
field_event(Ev, S=#s{focus=I,mouse_focus=true,fi=Fi}) ->
    field_event(Ev, S, get_fi(I, Fi));
field_event(_Ev, _S) -> keep.

field_event(Ev, S=#s{focus=_I,fi=TopFi=#fi{w=W0,h=H0},store=Store0}, 
	    Path=[#fi{handler=Handler}|_]) ->
    ?DEBUG_DISPLAY({field_handler,[_I,Ev]}),
    Result = Handler(Ev, Path, Store0),
%    ?DEBUG_DISPLAY({field_handler,Result}),
    case Result of
	ok -> return_result(S);
	done -> return_result(S);
	{done,Store} -> return_result(S#s{store=Store});
	cancel -> delete(S);
	keep -> keep;
	{recursive,Return} -> Return;
	{drag,{_,_}=WH,DropData} -> wings_wm:drag(Ev, WH, DropData);
	{store,Store} -> get_event(S#s{store=Store});
	{layout,Store} ->
	    case layout(TopFi, Store) of
		TopFi -> get_event(next_focus(0, S#s{store=Store}));
		Fi = #fi{w=W,h=H} ->
		    ?DMPTREE(Fi),
		    Focusable = focusable(Fi),
		    ?DEBUG_DISPLAY({new_focusable,Focusable}),
		    case {W,H} of
			{W0,H0} -> ok;
			_ -> 
			    Size = {W+2*?HMARGIN,H+2*?VMARGIN},
			    resize_maybe_move(Size)
		    end,
		    ?DEBUG_DISPLAY({W,H}),
		    get_event(next_focus(0, S#s{w=W,h=H,
						focusable=Focusable,
						fi=Fi,store=Store}))
	    end;
	{Action,Store} when is_function(Action) ->
	    Res = collect_result(TopFi, Store),
	    Action(Res),
	    delete(S);
	Action when is_function(Action) ->
	    Res = collect_result(TopFi, Store0),
	    Action(Res),
	    delete(S)
    end.

resize_maybe_move({W,H0}=Size) ->
    This = wings_wm:this(),
    {{X1,Y1},{_,H1}} = wings_wm:win_rect({controller,This}),
    H = H0+H1,
    {{X3,Y3},{W3,H3}} = wings_wm:win_rect(desktop),
    X = if  W > W3 -> X3;
	    X1+W > W3 -> W3-W;
	    true -> X1 end,
    Y = if  H > H3 -> Y3;
	    Y1+H > H3 -> H3-H;
	    true -> Y1 end,
    wings_wm:move(This, {X,Y+H1}, Size).
    
		

return_result(#s{call=EndFun,owner=Owner,fi=Fi,store=Sto}=S) ->
    Res = collect_result(Fi, Sto),
    ?DEBUG_DISPLAY({return_result,Res}),
    case catch EndFun(Res) of
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

redraw(S=#s{w=W,h=H,ox=Ox,oy=Oy,focus=Index,fi=Fi0,store=Sto}) ->
    ?DEBUG_DISPLAY(redraw),
    wings_io:ortho_setup(),
    gl:translated(Ox, Oy, 0),
    blend(fun(Col) ->
		  wings_io:border(-?HMARGIN, -?VMARGIN,
				  W+2*?HMARGIN-1, H+2*?VMARGIN-1,
				  Col)
	  end),
    case draw_fields(Fi0, Index, Sto) of
	keep -> keep;
	Fi ->
	    Focusable = focusable(Fi),
	    ?DEBUG_DISPLAY({new_focusable,Focusable}),
	    get_event(next_focus(0, S#s{focusable=Focusable,fi=Fi}))
    end.

%% Binary search a tuple. Cmp should return integer() > 0 | 0 | 0 < integer().
%% Return {I,I+1} | {I-1,I} if not found, or I if found,
%% where integer(I), 1 =< I, I =< Size,
%% where Size = size(Tuple).

binsearch(Cmp, Tuple) when function(Cmp), tuple(Tuple), size(Tuple) > 0 ->
    binsearch(Cmp, Tuple, 1, size(Tuple)).

binsearch(Cmp, Tuple, L, U) when L =< U ->
    I = (L + U) div 2,
    C = Cmp(element(I, Tuple)),
    case I of
	%% L == U
	U when C < 0 -> {U,U+1};
	U when 0 < C -> {U-1,U};
	%% L == U-1
	L when C < 0 -> binsearch(Cmp, Tuple, U, U);
	L when 0 < C -> {L-1,L};
	%% L < I < U
	_ when 0 < C -> binsearch(Cmp, Tuple, L, I-1);
	_ when C < 0 -> binsearch(Cmp, Tuple, I+1, U);
	_ -> I
    end.
    
%%%
%%% Dialog tree functions
%%%

find_ok(Fi, Sto) ->
    find_field(fun([#fi{index=Index,flags=Flags}|_]) ->
		       (field_type(Index, Sto) =:= but)
			   andalso proplists:get_bool(ok, Flags)
	       end, Fi).

find_cancel(Fi, Sto) ->
    find_field(fun([#fi{index=Index,flags=Flags}|_]) ->
		       (field_type(Index, Sto) =:= but)
			   andalso proplists:get_bool(cancel, Flags)
	       end, Fi).

find_eyepicker(Fi, Sto) ->
    find_field(fun([#fi{index=Index}|_]) ->
		       field_type(Index, Sto) =:= eyepicker
	       end, Fi).

find_position(Fi, Sto) ->
    find_field(fun([#fi{index=Index}|_]) ->
		       field_type(Index, Sto) =:= position
	       end, Fi) .

field_type(I, Store) ->
    case gb_trees:lookup(-I, Store) of
	{value,F} -> element(1, F);
	none -> undefined
    end.

%% Search tree to find field matching a predicate using reverse linear scan.

find_field(Fun, Fi) -> find_field_1(Fun, Fi, []).

find_field_1(Fun, Fi=#fi{extra=#container{fields=Fields}}, Path) ->
    find_field_2(Fun, Fields, [Fi|Path], size(Fields));
find_field_1(Fun, Fi, Path0) ->
    Path = [Fi|Path0],
    case Fun(Path) of
	false -> [];
	true -> Path
    end.

find_field_2(Fun, Fields, Path, I) when I >= 1 ->
    case find_field_1(Fun, element(I, Fields), Path) of
	[] -> find_field_2(Fun, Fields, Path, I-1);
	P -> P
    end;
find_field_2(_Fun, _Fields, _Path, _I) -> [].

%% Collect results from all fields.
%% Return list of result values.

collect_result(Fi, Sto) ->  reverse(collect_result_1(Fi, Sto, [], [])).

collect_result_1(Fi=#fi{inert=true}, Sto, Path, R) ->
    collect_result_2(Fi, Sto, Path, R);
collect_result_1(Fi=#fi{handler=Handler,key=Key}, Sto, Path, R0) ->
    R = case Handler(value, [Fi|Path], Sto) of
	    none -> R0;
	    {value,Res} when integer(Key) -> [Res|R0];
	    {value,Res} -> [{Key,Res}|R0]
	end,
    collect_result_2(Fi, Sto, Path, R).

collect_result_2(Fi=#fi{extra=#container{fields=Fields}}, Sto, Path, R) ->
    collect_result_3(Fields, Sto, [Fi|Path], R, 1);
collect_result_2(_Fi, _Sto, _Path, R) -> R.

collect_result_3(Fields, Sto, Path, R, I) when I =< size(Fields) ->
    collect_result_3(Fields, Sto, Path,
		     collect_result_1(element(I, Fields), Sto, Path, R),
		     I+1);
collect_result_3(_Fields, _Sto, _Path, R, _I) -> R.

%% Draw fields.
%% Return new fields tree.

draw_fields(Fi, Focus, Sto) -> draw_fields_1(Fi, Focus, Sto, []).

draw_fields_1(Fi0=#fi{handler=Handler,
		      index=Index,
		      extra=Container=#container{fields=Fields0,
						 minimized=Minimized,
						 active=Active}},
	      Focus, Sto, Path0) ->
    Path = [Fi0|Path0],
    Handler({redraw,Index =:= Focus}, Path, Sto),
    case {Minimized,Active} of
	{true,_} -> keep;
	{_,undefined} ->
	    case draw_fields_2(Fields0, Focus, Sto, Path, 1, [], false) of
		keep -> keep;
		Fields -> Fi0#fi{extra=Container#container{fields=Fields}}
	    end;
	{_,_} ->
	    case draw_fields_1(element(Active, Fields0), Focus, Sto, Path) of
		keep -> keep;
		Fi -> 
		    Fields = setelement(Active, Fields0, Fi),
		    Fi0#fi{extra=Container#container{fields=Fields}}
	    end
    end;
draw_fields_1(Fi=#fi{handler=Handler,index=Index,disabled=Disabled}, 
	    Focus, Sto, Path) ->
    case Handler({redraw,Index =:= Focus}, [Fi|Path], Sto) of
	enable when Disabled =:= true -> Fi#fi{disabled=false};
	disable when Disabled =:= false -> Fi#fi{disabled=true};
	_ -> keep
    end.

draw_fields_2(Fields, Focus, Sto, TopFi, I, R, Changed)
  when I =< size(Fields) ->
    Fi0 = element(I, Fields),
    case draw_fields_1(Fi0, Focus, Sto, TopFi) of
	keep -> draw_fields_2(Fields, Focus, Sto, TopFi, I+1, [Fi0|R], Changed);
	Fi -> draw_fields_2(Fields, Focus, Sto, TopFi, I+1, [Fi|R], true)
    end;
draw_fields_2(_Fields, _Focus, _Sto, _TopFi, _I, _R, false) ->
    keep;
draw_fields_2(_Fields, _Focus, _Sto, _TopFi, _I, R, true) ->
    list_to_tuple(reverse(R)).

%% Get field index from mouse position.
%% Return path to root.

mouse_to_field(X, Y, Fi) -> mouse_to_field_1(X, Y, Fi, [Fi]).

mouse_to_field_1(X, Y, 
		 Fi0=#fi{index=_Index,
			 extra=#container{x=X0,y=Y0,w=W,h=H,
					  type=Type,fields=Fields,
					  minimized=Minimized,
					  active=Active}},
		 Path0)
  when Minimized =/= true, X0 =< X, X < X0+W, Y0 =< Y, Y < Y0+H ->
    ?DEBUG_DISPLAY([_Index,X,Y,X0,Y0,W,H]),
    case Active of
	undefined -> mouse_to_field_2(X, Y, Fields, Path0, Type);
	_ ->
	    Fi = element(Active, Fields),
	    mouse_to_field_1(X, Y, Fi, [Fi,Fi0|Path0])
    end;
mouse_to_field_1(X, Y, #fi{index=_Index,x=X0,y=Y0,w=W,h=H}, Path)
  when X0 =< X, X < X0+W, Y0 =< Y, Y < Y0+H ->
    ?DEBUG_DISPLAY([_Index,X,Y,X0,Y0,W,H]),
    Path;
mouse_to_field_1(_X, _Y, #fi{}, _Path) ->
    [].

mouse_to_field_2(Xm, Ym, Fields, Path, vframe) ->
    Cmp = fun (#fi{y=Y,h=H}) when Y+H =< Ym -> -1;
	      (#fi{y=Y}) when Ym < Y -> 1;
	      (#fi{}) -> 0 end,
    Index = case binsearch(Cmp, Fields) of
%		{0,1} -> 1;
		{I,_} -> I;
		I -> I
	    end,
    Fi = element(Index, Fields),
    mouse_to_field_1(Xm, Ym, Fi, [Fi|Path]);
mouse_to_field_2(Xm, Ym, Fields, Path, hframe) ->
    Cmp = fun (#fi{x=X,w=W}) when X+W =< Xm -> -1;
	      (#fi{x=X}) when Xm < X -> 1;
	      (#fi{}) -> 0 end,
    Index = case binsearch(Cmp, Fields) of
%		{0,1} -> 1;
		{I,_} -> I;
		I -> I
	    end,
    Fi = element(Index, Fields),
    mouse_to_field_1(Xm, Ym, Fi, [Fi|Path]).
    
%% Get field from field index
%% Return path to root.

get_fi(Index, Fi) -> get_fi_1(Index, Fi, [Fi]).

get_fi_1(Index, #fi{index=Index}, Path) ->
    Path;
get_fi_1(Index, #fi{extra=#container{fields=Fields}}, Path) ->
    Cmp = fun (#fi{index=I}) when I < Index -> -1;
	      (#fi{index=I}) when Index < I -> 1;
	      (_) -> 0 end,
    case binsearch(Cmp, Fields) of
	{0,1} -> [];
	{I,_} -> 
	    Fi = element(I, Fields),
	    get_fi_1(Index, Fi, [Fi|Path]);
	I -> [element(I, Fields)|Path]
    end.% ;
% get_fi_1(_Index, #fi{extra=#leaf{}}) ->
%     [].



%%
%% Conversion of dialog query into internal tree format
%%

mktree(Qs0, Sto0) when is_list(Qs0) ->
    Qs = {hframe,[{vframe,Qs0},
		  {vframe,[{button,ok,[ok]},
			   {button,cancel,[cancel]}]}]},
    {Fis,Sto,I} = mktree(Qs, Sto0, 1),
    {Fis,Sto,I-1};
mktree(Qs, Sto0) ->
    {Fis,Sto,I} = mktree(Qs, Sto0, 1),
    {Fis,Sto,I-1}.

mktree({label_column,Qs0}, Sto, I) ->
    {Labels,Fields} = dialog_unzip(Qs0),
    Qs = {hframe,
	  [{vframe,Labels},
	   {vframe,Fields}]},
    mktree(Qs, Sto, I);
%%
mktree({vradio,Qs,Var,Def}, Sto, I) ->
    mktree(radio(vframe, Qs, Var, Def, []), Sto, I);
mktree({vradio,Qs,Var,Def,Flags}, Sto, I) ->
    mktree(radio(vframe, Qs, Var, Def, Flags), Sto, I);
mktree({hradio,Qs,Var,Def}, Sto, I) ->
    mktree(radio(hframe, Qs, Var, Def, []), Sto, I);
mktree({hradio,Qs,Var,Def,Flags}, Sto, I) ->
    mktree(radio(hframe, Qs, Var,Def, Flags), Sto, I);
%%
mktree({vframe,Qs}, Sto, I) ->
    mktree_container(Qs, Sto, I, [], vframe);
mktree({vframe,Qs,Flags}, Sto, I) ->
    mktree_container(Qs, Sto, I, Flags, vframe);
mktree({hframe,Qs}, Sto, I) ->
    mktree_container(Qs, Sto, I, [], hframe);
mktree({hframe,Qs,Flags}, Sto, I) ->
    mktree_container(Qs, Sto, I, Flags, hframe);
mktree({oframe,Qs,Def}, Sto, I) ->
    mktree_oframe(Qs, Def, Sto, I, []);
mktree({oframe,Qs,Def,Flags}, Sto, I) ->
    mktree_oframe(Qs, Def, Sto, I, Flags);
%%
mktree(panel, Sto, I) ->
    mktree_fi(panel(), Sto, I, []);
%%
mktree({eyepicker,Hook}, Sto, I) ->
    mktree_fi(eyepicker(), Sto, I, [{hook,Hook}]);
%%
mktree({position,Position}, Sto, I) ->
    mktree_fi(position(Position), Sto, I, []);
mktree({position,Position,Flags}, Sto, I) ->
    mktree_fi(position(Position), Sto, I, Flags);
%%
mktree({label,Label}, Sto, I) ->
    mktree_fi(label(Label, []), Sto, I, []);
mktree({label,Label,Flags}, Sto, I) ->
    mktree_fi(label(Label, Flags), Sto, I, Flags);
%%
mktree({color,Def}, Sto, I) ->
    mktree_fi(color(Def), Sto, I, [drag]);
mktree({color,Def,Flags}, Sto, I) ->
    mktree_fi(color(Def), Sto, I, [drag|Flags]);
%%
mktree({alt,{Var,Def},Prompt,Val}, Sto, I) ->
    mktree_fi(radiobutton(Var, Def, Prompt, Val), Sto, I, []);
mktree({alt,Var,Def,Prompt,Val}, Sto, I) ->
    mktree_fi(radiobutton(Var, Def, Prompt, Val), Sto, I, []);
mktree({alt,Var,Def,Prompt,Val,Flags}, Sto, I) ->
    mktree_fi(radiobutton(Var, Def, Prompt, Val), Sto, I, Flags);
mktree({key_alt,{Key,Def},Prompt,Val}, Sto, I) ->
    mktree_fi(radiobutton(Key, Def, Prompt, Val), Sto, I, [{key,Key}]);
mktree({key_alt,{Key,Def},Prompt,Val,Flags}, Sto, I) ->
    mktree_fi(radiobutton(Key, Def, Prompt, Val), Sto, I, [{key,Key}|Flags]);
%%
mktree({menu,Menu,Def}, Sto, I) ->
    mktree_fi(menu(Menu, Def), Sto, I, []);
mktree({menu,Menu,Def,Flags}, Sto, I) when is_list(Flags) ->
    mktree_fi(menu(Menu, Def), Sto, I, Flags);
%%
mktree({button,Action}, Sto, I) ->
    mktree_fi(button(Action), Sto, I, []);
mktree({button,Action,Flags}, Sto, I) when is_list(Flags) ->
    mktree_fi(button(Action), Sto, I, Flags);
mktree({button,Label,Action}, Sto, I) ->
    mktree_fi(button(Label, Action), Sto, I, []);
mktree({button,Label,Action,Flags}, Sto, I) ->
    mktree_fi(button(Label, Action), Sto, I, Flags);
%%
mktree({custom,W,H,Custom}, Sto, I) ->
    mktree_fi(custom(W, H, Custom), Sto, I, []);
mktree({custom,W,H,Custom,Flags}, Sto, I) ->
    mktree_fi(custom(W, H, Custom), Sto, I, Flags);
%%
mktree({slider,Flags}, Sto, I) when is_list(Flags) ->
    mktree_fi(slider(Flags), Sto, I, Flags);
mktree({slider,{text,_,Flags}=Field}, Sto, I) ->
    SliderFlags = case proplists:get_value(key, Flags, 0) of
		      K when integer(K) -> [{key,K-1}|Flags];
		      _ -> Flags
		  end,
    mktree({hframe,[Field,{slider,SliderFlags}]}, Sto, I);
%%
mktree(separator, Sto, I) ->
    mktree_fi(separator(), Sto, I, []);
%%
mktree({text,Def}, Sto, I) ->
    mktree_fi(text(Def, []), Sto, I, []);
mktree({text,Def,Flags}, Sto, I) ->
    mktree_fi(text(Def, Flags), Sto, I, Flags);
%%
mktree({Prompt,Def}, Sto, I) when Def==false; Def == true ->
    mktree_fi(checkbox(Prompt, Def), Sto, I, []);
mktree({Prompt,Def,Flags}, Sto, I) when Def==false; Def == true ->
    mktree_fi(checkbox(Prompt, Def), Sto, I, Flags).

radio(FrameType, Qs0, Var, Def, Flags) ->
    AltTag = case proplists:get_value(key, Flags, 0) of
		 0 -> alt;
		 _ -> key_alt
	     end,
    Qs = [{AltTag,Var,Def,Prompt,Val} || {Prompt,Val} <- Qs0],
    {FrameType,Qs,Flags}.

mktree_fi({Handler,Inert,Priv,W,H}, Sto, I, Flags) ->
    {#fi{handler=Handler,inert=Inert,
	 key=proplists:get_value(key, Flags, 0),
	 index=I,
	 hook=proplists:get_value(hook, Flags),
	 flags=Flags,
	 extra=#leaf{w=W,h=H}},
     gb_trees:insert(-I, Priv, Sto),
     I+1}.

mktree_container(Qs, Sto0, I0, Flags, Type) ->
    {Fields,Sto1,I} = mktree_container_1(Qs, Sto0, I0+1, []),
    Title = proplists:get_value(title, Flags),
    Minimized = proplists:get_value(minimized, Flags),
    Inert = (Title =:= undefined orelse Minimized =:= undefined),
    Key = proplists:get_value(key, Flags, 0),
    Sto2 = gb_trees:insert(var(Key, I0), Minimized, Sto1),
    Sto = gb_trees:insert(-I0, Type, Sto2),
    {#fi{handler=fun frame_event/3,
	 key=Key,
	 inert=Inert,
	 index=I0,
	 hook=proplists:get_value(hook, Flags),
	 flags=Flags,
	 extra=#container{type=Type,fields=Fields}},
     Sto,
     I}.

mktree_container_1([], Sto, I, R) ->
    {list_to_tuple(reverse(R)),Sto,I};
mktree_container_1([Q|Qs], Sto0, I0, R) ->
    {Fi,Sto,I} = mktree(Q, Sto0, I0),
    mktree_container_1(Qs, Sto, I, [Fi|R]).

-record(oframe, {style,				%menu|tabs
		 w,h,				%header size
		 titles}).			%tuple() of list()

mktree_oframe(Qs, Def, Sto0, I0, Flags) when integer(Def), Def >= 1 ->
    {Titles,Fields,Sto1,I} = mktree_oframe_1(Qs, Sto0, I0+1, [], []),
    FieldsTuple = list_to_tuple(Fields),
    if  Def =< size(FieldsTuple) ->
	    Style = proplists:get_value(style, Flags, menu),
	    Key = proplists:get_value(key, Flags, 0),
	    Cw = ?CHAR_WIDTH,
	    {W,H} = 
		case Style of
		    menu -> 
			{10 + 2*Cw +
			 lists:foldl(
			   fun (Title, Width) ->
				   max(wings_text:width(Title), Width)
			   end, 0, Titles),
			 ?LINE_HEIGHT+10};
		    tabs -> 
			{lists:foldl(
			   fun (Title, Width) ->
				   2*Cw+Width+wings_text:width(Title)
			   end, 2*Cw, Titles),
			 ?LINE_HEIGHT+10}
		end,
	    Sto2 = gb_trees:insert(var(Key, I0), Def, Sto1),
	    Sto = gb_trees:insert(-I0, #oframe{style=Style,
					       w=W,h=H,
					       titles=list_to_tuple(Titles)}, 
				  Sto2),
	    {#fi{handler=fun oframe_event/3,
		 key=Key,
		 inert=false,
		 index=I0,
		 hook=proplists:get_value(hook, Flags),
		 flags=Flags,
		 extra=#container{type=oframe,fields=FieldsTuple,active=Def}},
	     Sto,
	     I}
    end.

mktree_oframe_1([], Sto, I, T, R) ->
    {reverse(T),reverse(R),Sto,I};
mktree_oframe_1([{Title,Q}|Qs], Sto0, I0, T, R) when list(Title) ->
    {Fi,Sto,I} = mktree(Q, Sto0, I0),
    mktree_oframe_1(Qs, Sto, I, [Title|T], [Fi|R]).

-ifdef(DEBUG).
%%
%% Dump a dialog tree on stdout
%%

dmptree(Fi) -> 
    io:format("-begin(dmptree/1).~n"),
    case catch dmptree(Fi, "") of
	ok -> 
	    io:format("-end(dmptree/1).~n");
	Other ->
	    io:format("-end(dmptree/1, ~p).~n", [Other])
    end.
    
dmptree(#fi{key=Key,index=Index,inert=Inert,disabled=Disabled,flags=Flags,
	       x=X,y=Y,w=W,h=H,extra=Extra}, Fill) ->
    io:format("~s#fi{key=~p,index=~p,flags=~p,x=~p,y=~p,w=~p,h=~p,~n"
	      "~s    inert=~p,disabled=~p",
	      [Fill,Key,Index,Flags,X,Y,W,H,Fill,Inert,Disabled]),
    dmptree_1(Extra, Fill).

dmptree(Fields, Fill, I) when I =< size(Fields) ->
    dmptree(element(I, Fields), Fill),
    dmptree(Fields, Fill, I+1);
dmptree(_Fields, _Fill, _I) -> ok.

dmptree_1(#container{type=Type,x=X,y=Y,w=W,h=H,
		   fields=Fields,minimized=Minimized,active=Active}, Fill) ->
    io:format("}.~n~s  #container{type=~p,minimized=~p,active=~p,"
	      "x=~p,y=~p,w=~p,h=~p}~n",
	      [Fill,Type,Minimized,Active,X,Y,W,H]),
    dmptree(Fields, "  "++Fill, 1);
dmptree_1(#leaf{w=W,h=H}, _Fill) -> 
    io:format("#leaf{w=~p,h=~p}}.~n", [W,H]).

-endif.

%%
%% Layout the dialog, i.e calculate positions and sizes for all fields.
%% Container frames take their size from their children, sum in one
%% direction and max in other, depending on vframe/hframe/oframe.
%%
%% Note! The #container.fields field is a reversed list after this pass.

layout(Fi, Sto) -> layout_propagate(layout(Fi, Sto, 0, 0)).

layout(Fi=#fi{extra=#leaf{w=W,h=H}}, _Sto, X, Y) ->
    Fi#fi{x=X,y=Y,w=W,h=H};
layout(Fi=#fi{key=Key,index=Index,
	      extra=Container=#container{type=oframe,fields=Fields0}}, 
       Sto, X0, Y0) ->
    #oframe{w=Wt,h=Ht} = gb_trees:get(-Index, Sto),
    Active = gb_trees:get(var(Key, Index), Sto),
    Pad = 10, %case Style of menu -> 10; tabs -> 0 end,
    X1 = X0+Pad,
    Y1 = Y0+Ht,
    {Fields,X2,Y2} = layout_container(oframe, Fields0, Sto, X1, Y1),
    Wi = max(Wt, X2-X1),
    Hi = Y2-Y1,
    Wo = Pad+Wi+Pad,
    Ho = Ht + Hi + Pad,
    Fi#fi{x=X0,y=Y0,w=Wo,h=Ho,
	  extra=Container#container{x=X1,y=Y1,w=Wi,h=Hi,
				    fields=Fields,
				    active=Active}};
layout(Fi=#fi{key=Key,index=Index,flags=Flags,
	      extra=Container=#container{type=Type,fields=Fields0}}, 
       Sto, X0, Y0) when Type =:= hframe; Type =:= vframe ->
    Minimized = gb_trees:get(var(Key, Index), Sto),
    Title = proplists:get_value(title, Flags),
    {X1,Y1} = if Title =:= undefined -> {X0,Y0};
		 true -> {X0+10,Y0+?LINE_HEIGHT} end,
    {Fields,X2,Y2} = layout_container(Type, Fields0, Sto, X1, Y1),
    Wi = if Title =:= undefined -> X2-X1;
	    true -> max(3*?CHAR_WIDTH+wings_text:width(Title), X2-X1) end,
    Hi = Y2-Y1,
    Wo = 2*(X1-X0)+Wi,
    Ho = Y1-Y0 +
	if Minimized =:= true -> 0; true -> Hi end +
	if Title =:= undefined -> 0; true -> 10 end,
    Fi#fi{x=X0,y=Y0,w=Wo,h=Ho,
	  extra=Container#container{x=X1,y=Y1,w=Wi,h=Hi,
				    fields=Fields,
				    minimized=Minimized}}.

layout_container(vframe, Fields, Sto, X, Y) -> 
    layout_vframe(1, Fields, Sto, X, Y, 0, []);
layout_container(hframe, Fields, Sto, X, Y) -> 
    layout_hframe(1, Fields, Sto, X, Y, 0, []);
layout_container(oframe, Fields, Sto, X, Y) -> 
    layout_oframe(1, Fields, Sto, X, Y, 0, 0, []).

layout_vframe(I, Fields, Sto, X0, Y0, W0, R) when I =< size(Fields) ->
    Fi = #fi{x=X0,y=Y0,w=W,h=H} = layout(element(I, Fields), Sto, X0, Y0),
    layout_vframe(I+1, Fields, Sto, X0, Y0+H, max(W, W0), [Fi|R]);
layout_vframe(_I, _Fields, _Sto, X, Y, W, R) ->
    {R,X+W,Y}.

layout_hframe(I, Fields, Sto, X0, Y0, H0, R) when I =< size(Fields) ->
    Fi = #fi{x=X0,y=Y0,w=W,h=H} = layout(element(I, Fields), Sto, X0, Y0),
    layout_hframe(I+1, Fields, Sto, X0+W+?HFRAME_SPACING, Y0, max(H, H0), 
		  [Fi|R]);
layout_hframe(_I, _Fields, _Sto, X, Y, H, R) ->
    {R,X-?HFRAME_SPACING,Y+H}.

layout_oframe(I, Fields, Sto, X, Y, W0, H0, R) when I =< size(Fields) ->
    Fi = #fi{x=X,y=Y,w=W,h=H} = layout(element(I, Fields), Sto, X, Y),
    layout_oframe(I+1, Fields, Sto, X, Y, max(W0, W), max(H0, H), [Fi|R]);
layout_oframe(_I, _Fields, _Sto, X, Y, W, H, R) ->
    {R,X+W,Y+H}.
    


%% Propagate the resulting size of container frames to its children.
%%
%% Convert the #container.fields field back to tuple.

layout_propagate(Fi=#fi{extra=Container=#container{
				type=vframe,w=W,fields=Fields}}) ->
    Fi#fi{extra=Container#container{
		  fields=layout_propagate_vframe(Fields, W, [])}};
layout_propagate(Fi=#fi{extra=Container=#container{
				type=hframe,h=H,fields=Fields}}) ->
    Fi#fi{extra=Container#container{
		  fields=layout_propagate_hframe(Fields, H, [])}};
layout_propagate(Fi=#fi{extra=Container=#container{
				type=oframe,w=W,h=H,fields=Fields}}) ->
    Fi#fi{extra=Container#container{
		  fields=layout_propagate_oframe(Fields, W, H, [])}};
layout_propagate(Fi=#fi{}) ->
    Fi.

layout_propagate_vframe([Fi|Fis], W, R) ->
    layout_propagate_vframe(Fis, W, [layout_propagate(Fi#fi{w=W})|R]);
layout_propagate_vframe([], _W, R) ->
    list_to_tuple(R).

layout_propagate_hframe([Fi|Fis], H, R) ->
    layout_propagate_hframe(Fis, H, [layout_propagate(Fi#fi{h=H})|R]);
layout_propagate_hframe([], _H, R) ->
    list_to_tuple(R).

layout_propagate_oframe([Fi|Fis], W, H, R) ->
    layout_propagate_oframe(Fis, W, H, [layout_propagate(Fi#fi{w=W,h=H})|R]);
layout_propagate_oframe([], _W, _H, R) ->
    list_to_tuple(R).

%%
%% Create a tuple of the focusable field indexes
%%

focusable(Fi) -> list_to_tuple(reverse(focusable_1(Fi, []))).

focusable_1(#fi{index=Index,inert=false,disabled=false,
	      extra=Container}, R0) ->
    R = [Index|R0],
    case Container of
	#container{minimized=true} ->
	    R;
	#container{fields=Fields,active=undefined} ->
	    focusable_2(1, Fields, R);
	#container{fields=Fields,active=Active} ->
	    focusable_1(element(Active, Fields), R);
	_ ->
	    R
    end;
focusable_1(#fi{extra=#container{minimized=true}}, R) ->
    R;
focusable_1(#fi{extra=#container{fields=Fields,active=undefined}}, R) ->
    focusable_2(1, Fields, R);
focusable_1(#fi{extra=#container{fields=Fields,active=Active}}, R) ->
    focusable_1(element(Active, Fields), R);
focusable_1(#fi{}, R) -> R.

focusable_2(I, Fields, R) when I =< size(Fields) ->
    focusable_2(I+1, Fields, focusable_1(element(I, Fields), R));
focusable_2(_I, _Fields, R) -> R.

%% Mark all siblings (that can be minimized) of a container to be minimized
%%

minimize_siblings([#fi{index=Index},
		   #fi{extra=#container{fields=Fields}}|_], Sto) ->
    minimize_siblings(1, Fields, Index, Sto);
minimize_siblings(_Path, Sto) -> Sto.

minimize_siblings(I, Fields, Index, Sto0) when I =< size(Fields) ->
    case element(I, Fields) of
	#fi{key=Key,index=Ix,extra=#container{minimized=false}} 
	when Ix =/= Index -> 
	    Sto = gb_trees:update(var(Key, Ix), true, Sto0),
	    minimize_siblings(I+1, Fields, Index, Sto);
	#fi{} -> minimize_siblings(I+1, Fields, Index, Sto0)
    end;
minimize_siblings(_I, _Fields, _Index, Sto) -> Sto.

%%
%% Traverse the tree and init all fields
%%

init_fields(Fi, Sto) -> init_fields(Fi, Sto, []).

init_fields(Fi=#fi{extra=#container{fields=Fields}}, Sto, Path) ->
    init_fields(Fields, Sto, [Fi|Path], 1);
init_fields(Fi=#fi{handler=Handler}, Sto0, Path) ->
    case Handler(init, [Fi|Path], Sto0) of
	{store,Sto} -> Sto;
	keep -> Sto0
    end.

init_fields(Fields, Sto, Path, I) when I =< size(Fields) ->
    init_fields(Fields, init_fields(element(I, Fields), Sto, Path), 
		Path, I+1);
init_fields(_Fields, Sto, _Path, _I) -> Sto.



%%
%% Hframe and Vframe
%%

frame_event({redraw,Active}, 
	    [Fi=#fi{key=Key,index=I,hook=Hook,
		    extra=#container{minimized=Minimized}}|_],
	    Store) ->
    DisEnable = hook(Hook, is_disabled, [var(Key, I), I, Store]),
    frame_redraw(Active, Fi, DisEnable, Minimized);
frame_event(#mousemotion{x=Xm,y=Ym,state=Bst},
	    [#fi{key=Key,index=I,inert=false,disabled=false,x=X,y=Y}|_],
	    Store) when (Bst band ?BUTTON_MASK) =:= 0 ->
    case inside(Xm, Ym, X, Y+3, 10, ?CHAR_HEIGHT) of
	true ->
	    Minimized = gb_trees:get(var(Key, I), Store),
	    case Minimized of
		true ->
		    wings_util:button_message(
		      "Expand this frame; collapse other frames",
		      "",
		      "Expand this frame");
		false ->
		    wings_util:button_message(
		      "Collapse this frame", "", "Collapse this frame");
		undefined ->
		    wings_wm:message("")
	    end;
	false ->
	    wings_wm:message("")
    end,
    keep;
frame_event(#mousebutton{x=Xb,y=Yb,button=Button,state=?SDL_RELEASED}, 
	    Path=[#fi{x=X0,y=Y0}|_], 
	    Store) ->
    X = X0,
    Y = Y0+3,
    case inside(Xb, Yb, X, Y, 10, ?CHAR_HEIGHT) of
	true -> 
	    case Button of
		1 -> frame_event({key,$\s,0,$\s}, Path, Store);
		2 -> keep;
		3 -> frame_event({key,$\s,?SHIFT_BITS,$\s}, Path, Store)
	    end;
	false -> keep
    end;
frame_event({key,_,Mod,$\s}, 
	    Path=[#fi{key=Key,index=I,
		      extra=#container{minimized=Minimized}}|_], 
	    Store0) ->
    case Minimized of
	undefined -> keep;
	true ->
	    Store = 
		if  ?IS_SHIFTED(Mod) -> Store0;
		    true -> minimize_siblings(Path, Store0)
		end,
	    {layout,gb_trees:update(var(Key, I), false, Store)};
	false -> {layout,gb_trees:update(var(Key, I), true, Store0)}
    end;
frame_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
frame_event(_Ev, _Path, _Store) -> keep.

frame_redraw(Active, #fi{flags=Flags}=Fi, DisEnable, Minimized) ->
    Title = proplists:get_value(title, Flags),
    if  Title =:= undefined -> keep;
	true -> frame_redraw_1(Active, Title, Fi, DisEnable, Minimized) 
    end.

frame_redraw_1(Active, Title, #fi{x=X0,y=Y0,w=W0,h=H0}, 
	       DisEnable, Minimized) ->
    Cw = wings_text:width(),
    Ch = wings_text:height(),
    Y = Y0 + Ch div 2 + 3,
    H = H0 - (Y-Y0) - 4,
    ColLow = color4_lowlight(),
    ColHigh = color4_highlight(),
    TextPos = X0 + 10 + 2*Cw,
    blend(fun(Col) ->
		  gl:'begin'(?GL_LINES),
		  hline(X0, Y, W0, ColLow, ColHigh),
		  if  Minimized =/= true ->
			  hline(X0, Y+H-1, W0-1, ColLow, ColHigh),
			  vline(X0+W0-2, Y, H, ColLow, ColHigh),
			  vline(X0, Y+1, H-2, ColLow, ColHigh);
		      true -> ok
		  end,
		  gl:'end'(),
		  gl:color4fv(Col),
		  gl:rectf(TextPos-Cw, Y-1,
			   TextPos+wings_text:width(Title)+Cw, Y+2)
	  end),
%%%     Col = color3(),
    ColFg = color3_text(),
    gl:color3fv(ColFg),
    wings_io:text_at(TextPos, Y0+Ch, Title),
    if  Minimized =/= undefined ->
	    %% Draw button
	    blend(fun(Col) ->
			  case DisEnable of
			      disable ->
				  wings_io:border(
				    X0, Y0+3, 
				    10, Ch,
				    Col, ColFg);
			      _ ->
				  wings_io:gradient_border(
				    X0, Y0+3,
				    10, Ch,
				    Col, ColFg, Active)
			  end
		  end),
	    %% Draw +/- symbol
	    gl:color3fv(ColFg),
	    gl:rectf(X0+2, Y, X0+9, Y+2),
	    case Minimized of
		true -> gl:rectf(X0+4, Y-2, X0+6, Y+4);
		false -> ok
	    end;
	true -> ok
    end,
    keep.

hline(X0, Y0, W, ColLow, ColHigh) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    gl:color4fv(ColLow),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+W, Y),
    gl:color4fv(ColHigh),
    gl:vertex2f(X, Y+1),
    gl:vertex2f(X+W, Y+1).

vline(X0, Y0, H, ColLow, ColHigh) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    gl:color4fv(ColLow),
    gl:vertex2f(X, Y),
    gl:vertex2f(X, Y+H),
    gl:color4fv(ColHigh),
    gl:vertex2f(X+1, Y),
    gl:vertex2f(X+1, Y+H).

%%
%% Oframe
%%

oframe_event({redraw,Active}, [Fi=#fi{key=Key,index=I,hook=Hook}|_], Store) ->
    Oframe = gb_trees:get(-I, Store),
    DisEnable = hook(Hook, is_disabled, [var(Key, I), I, Store]),
    oframe_redraw(Active, Fi, Oframe, DisEnable);
oframe_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
oframe_event(#mousemotion{state=Bst}, _Path, _Store)
  when (Bst band ?BUTTON_MASK) =:= 0 ->
    wings_wm:message(""),
    keep;
oframe_event(#mousebutton{x=Xb,button=1,state=?SDL_PRESSED}, 
	    [#fi{x=X,y=Y,key=Key,index=I,hook=Hook}|_], 
	    Sto0) ->
    case gb_trees:get(-I, Sto0) of
	#oframe{style=tabs,titles=Titles} ->
	    case oframe_which_tab(X, Xb, Titles) of
		undefined -> keep;
		Val ->
		    case hook(Hook, update, [var(Key, I), I, Val, Sto0]) of
			{store,Sto} -> {layout,Sto};
			Other -> Other
		    end
	    end;
	#oframe{w=W,style=menu,titles=Titles} ->
	    Menu = oframe_menu(Titles),
	    Var = var(Key, I),
	    Val = gb_trees:get(Var, Sto0),
	    Disabled = hook(Hook, menu_disabled, [Var, I, Sto0]),
	    menu_popup(X+10, Y, W, Menu, Val, Disabled)
    end;
oframe_event({popup_result,Val}, [#fi{index=I,key=Key,hook=Hook}|_], Sto0) ->
    case hook(Hook, update, [var(Key, I), I, Val, Sto0]) of
	{store,Sto} -> {layout,Sto};
	Other -> Other
    end;
oframe_event(_Ev, _Path, _Store) -> keep.

oframe_which_tab(X0, Xb, Titles) when Xb >= X0 -> 
    oframe_which_tab(X0, Xb, Titles, ?CHAR_WIDTH, 1);
oframe_which_tab(_X0, _Xb, _Titles) -> undefined.

oframe_which_tab(X0, Xb, Titles, Cw, I) when I =< size(Titles) ->
    W = Cw + wings_text:width(element(I, Titles)) + Cw,
    if  Xb < X0+W -> I;
	true -> oframe_which_tab(X0+W, Xb, Titles, Cw, I+1)
    end.

oframe_menu(Titles) -> oframe_menu(Titles, 1).

oframe_menu(Titles, N) when N =< size(Titles) -> 
    [{element(N, Titles),N,[]}|oframe_menu(Titles, N+1)];
oframe_menu(_Titles, _N) -> [].

oframe_redraw(Active, 
	      #fi{x=X0,y=Y0,w=W0,h=H0,extra=#container{active=I}},
	      #oframe{style=menu,w=Wt,h=Ht,titles=Titles},
	      DisEnable) ->
    Y = Y0 + Ht div 2 + 3,
    H = H0 - (Y-Y0) - 4,
    ColLow = color4_lowlight(),
    ColHigh = color4_highlight(),
    blend(fun(_Col) ->
		  gl:'begin'(?GL_LINES),
		  hline(X0, Y, 10, ColLow, ColHigh),
		  hline(X0+Wt-10, Y, W0-Wt+10, ColLow, ColHigh),
		  hline(X0, Y+H-1, W0-1, ColLow, ColHigh),
		  vline(X0, Y+1, H-2, ColLow, ColHigh),
		  vline(X0+W0-2, Y, H, ColLow, ColHigh),
		  gl:'end'()
	  end),
    Title = element(I, Titles),
    menu_draw(Active, X0+10, Y0+1, Wt, Ht, Title, DisEnable);
oframe_redraw(Active, 
	      #fi{x=X0,y=Y0,w=W0,h=H0,extra=#container{active=I}},
	      #oframe{style=tabs,h=Ht,titles=Titles},
	      DisEnable) ->
    Y = Y0+Ht-5,
    H = H0-Ht,
    ColLow = color4_lowlight(),
    ColHigh = color4_highlight(),
    {X1,X2} = oframe_title_pos(X0, I, Titles),
    blend(fun(_Col) ->
		  gl:'begin'(?GL_LINES),
		  hline(X0, Y, X1-X0, ColLow, ColHigh),
		  hline(X2, Y, W0-(X2-X0), ColLow, ColHigh),
		  hline(X0, Y+H-2, W0, ColLow, ColHigh),
		  vline(X0, Y+1, H-4, ColLow, ColHigh),
		  vline(X0+W0, Y+1, H-4, ColLow, ColHigh),
		  gl:'end'()
	  end),
    oframe_redraw_titles(X0, Y0, Ht-5, Titles, DisEnable),
    case Active of false -> DisEnable;
	true ->
%%% 	    blend(fun(_Col) ->
%%% 			  gl:color3fv(color3_text()),
%%% 			  draw_hat(X1, Y, X2-X1, H, Active)
%%% 		  end),
	    DisEnable
    end.

draw_hat(X, Y, W, H, Double) ->
    gl:'begin'(?GL_LINES),
    gl:vertex2f(X, Y), gl:vertex2f(X+W, Y),
    gl:vertex2f(X, Y), gl:vertex2f(X, Y+H),
    gl:vertex2f(X+W, Y), gl:vertex2f(X+W, Y+H),
    case Double of false -> ok;
	true ->
	    gl:vertex2f(X-1, Y+1), gl:vertex2f(X-1+W, Y+1),
	    gl:vertex2f(X-1, Y+1), gl:vertex2f(X-1, Y+H),
	    gl:vertex2f(X-1+W, Y+1), gl:vertex2f(X+W, Y+H)
    end,
    gl:'end'().

oframe_title_pos(X, Active, Titles) ->
    oframe_title_pos_1(X, Active, Titles, 1, ?CHAR_WIDTH).

oframe_title_pos_1(X, Active, Titles, I, Cw) ->
    W = Cw + wings_text:width(element(I, Titles)) + Cw,
    if  I =:= Active ->
	    {X,X+W};
	true ->
	    oframe_title_pos_1(X+W, Active, Titles, I+1, Cw)
    end.

oframe_redraw_titles(X, Y, H, Titles, DisEnable) -> 
    Cw = ?CHAR_WIDTH,
    ColText = color3_text(),
    ColLow = color4_lowlight(),
    ColHigh = color4_highlight(),
    oframe_redraw_titles(X, Y, H, Titles, DisEnable, 
			 Cw, ColText, ColLow, ColHigh, 1).

oframe_redraw_titles(X, Y, H, Titles, DisEnable, 
		     Cw, ColText, ColLow, ColHigh, I)
  when I =< size(Titles) ->
    Title = element(I, Titles),
    W = wings_text:width(Title),
    gl:color3fv(ColText),
    wings_io:text_at(X+Cw, Y+H-4, Title),
    X2 = X+Cw+W+Cw,
    blend(fun(_Col) ->
%%% 		  if  I =:= 1 -> ok;
%%% 		      true ->
%%% 			  gl:color4fv(ColHigh),
%%% 			  gl:rasterPos2i(X, Y),
%%% 			  gl:bitmap(8, 4, 3, 4, 0, 0, 
%%% 				    << 
%%% 				     2#00001000,
%%% 				     2#00101100,
%%% 				     2#11000111,
%%% 				     2#00000000>>),
%%% 			  gl:color4fv(ColLow),
%%% 			  gl:rasterPos2i(X, Y),
%%% 			  gl:bitmap(8, 4, 3, 4, 0, 0, 
%%% 				    << 
%%% 				     2#00010000,
%%% 				     2#00010000,
%%% 				     2#00010000,
%%% 				     2#11111111>>)
%%% 		  end,
		  gl:'begin'(?GL_LINES),
%%% 		  hline(X+4, Y, X2-X-8, ColLow, ColHigh),
%%% 		  vline(X, Y+3, H-4, ColLow, ColHigh),
		  hline(X, Y, X2-X, ColLow, ColHigh),
		  vline(X, Y+1, H-2, ColLow, ColHigh),
		  gl:'end'()
	  end),
    oframe_redraw_titles(X2, Y, H, Titles, DisEnable, 
			 Cw, ColText, ColLow, ColHigh, I+1);
oframe_redraw_titles(X, Y, H, _Titles, DisEnable, 
		     _Cw, _ColText, ColLow, ColHigh, _I) ->
    blend(fun(_Col) ->
		  gl:'begin'(?GL_LINES),
		  vline(X, Y+1, H-2, ColLow, ColHigh),
		  gl:'end'()
	  end),
    DisEnable.



%%%
%%% Separator.
%%%

separator() ->
    Fun = fun separator_event/3,
    {Fun,true,{separator},4*?CHAR_WIDTH,10}.

separator_event({redraw,_Active}, [Fi|_], _Sto) -> separator_draw(Fi);
separator_event(_Ev, _Path, _Sto) -> keep.

separator_draw(#fi{x=X,y=Y,w=W}) ->
    ?CHECK_ERROR(),
    LeftX = X + 0.5,
    RightX = X + W + 0.5,
    UpperY = Y + 5.5,
    {R,G,B} = color3_disabled(),
    {_,_,_,A} = color4(),
    FgColor = {R,G,B,A},
    wings_io:blend(FgColor,
		   fun(Col) ->
			   gl:lineWidth(1),
			   gl:'begin'(?GL_LINES),
			   gl:color4fv(Col),
			   gl:vertex2f(LeftX, UpperY),
			   gl:vertex2f(RightX, UpperY),
			   gl:'end'()
		   end),
    gl:color3b(0, 0, 0),
    ?CHECK_ERROR(),
    keep.





%%%
%%% Checkbox
%%%

-define(CB_SIZE, 10).

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
    Fun = fun cb_event/3,
    {Fun,false,Cb,LabelWidth+SpaceWidth+?CB_SIZE,?LINE_HEIGHT+2}.

cb_event(init, [#fi{key=Key,index=I}|_], Store) ->
    #cb{val=Val} = gb_trees:get(-I, Store),
    {store,gb_trees:enter(var(Key, I), Val, Store)};
cb_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
cb_event({redraw,Active}, [Fi=#fi{key=Key,index=I,hook=Hook}|_], Store) ->
    Cb = gb_trees:get(-I, Store),
    K = var(Key, I),
    Val = gb_trees:get(K, Store),
    DisEnable = hook(Hook, is_disabled, [K, I, Store]),
    cb_draw(Active, Fi, Cb, Val, DisEnable);
cb_event(#mousebutton{x=Xb,state=?SDL_PRESSED,button=1}, 
	 Path=[#fi{x=X,index=I}|_], Store) ->
    #cb{labelw=LblW,spacew=SpaceW} = gb_trees:get(-I, Store),
    if
	Xb-X < LblW+4*SpaceW ->
	    cb_event({key,$\s,0,$\s}, Path, Store);
	true -> keep
    end;
cb_event({key,_,_,$\s}, [#fi{key=Key,index=I,hook=Hook}|_], Store) ->
    Var = var(Key, I),
    Val = gb_trees:get(Var, Store),
    hook(Hook, update, [Var, I, not Val, Store]);
cb_event(_Ev, _Path, _Store) -> keep.

cb_draw(Active, #fi{x=X,y=Y0}, #cb{label=Label}, Val, DisEnable) ->
    wings_io:sunken_gradient(X, Y0+?CHAR_HEIGHT-?CB_SIZE, ?CB_SIZE, ?CB_SIZE,
			     case DisEnable of
				 disable -> color3();
				 _ -> {0.82,0.82,0.82}
			     end, color4(), Active),
    FgColor = case DisEnable of
		  disable -> color3_disabled();
		  _-> color3_text()
	      end,
    gl:color3fv(FgColor),
    Y = Y0+?CHAR_HEIGHT,
    case Val of
	false -> ok;
	true -> wings_io:text_at(X+2, Y-1, [crossmark])
    end,
    wings_io:text_at(X+round(1.8*?CB_SIZE), Y, Label),
    gl:color3b(0, 0, 0),
    DisEnable.



%%%
%%% Radio button
%%%

-record(rb,
	{var,					%Variable key.
	 def,					%Default value.
	 val,
	 label,
	 labelw,			    %Width of label in pixels.
	 spacew				  %Width of a space character.
	}).

radiobutton(Var, Def, Label, Val) ->
    LabelWidth = wings_text:width(Label),
    SpaceWidth = wings_text:width(" "),
    Rb = #rb{var=Var,def=Def,val=Val,label=Label,
	     labelw=LabelWidth,spacew=SpaceWidth},
    Fun = fun rb_event/3,
    {Fun,false,Rb,LabelWidth+2*SpaceWidth,?LINE_HEIGHT+2}.

rb_event(init, [#fi{index=I}|_], Store) ->
    #rb{var=Var,def=Def,val=Val} = gb_trees:get(-I, Store),
    case Val of
	Def -> {store,gb_trees:enter(var(Var, I), Val, Store)};
	_ -> keep
    end;
rb_event({redraw,Active}, [Fi=#fi{hook=Hook,index=I}|_], Store) ->
    Rb = #rb{var=Var} = gb_trees:get(-I, Store),
    Key = var(Var, I),
    DisEnable = hook(Hook, is_disabled, [Key, I, Store]),
    rb_draw(Active, Fi, Rb, gb_trees:get(Key, Store), DisEnable);
rb_event(value, [#fi{index=I}|_], Store) ->
    #rb{var=Var,val=Val} = gb_trees:get(-I, Store),
    case gb_trees:get(var(Var, I), Store) of
	Val -> {value,Val};
	_ -> none
    end;
rb_event({key,_,_,$\s}, [Fi=#fi{index=I}|_], Store) ->
    rb_set(Fi, gb_trees:get(-I, Store), Store);
rb_event(#mousebutton{x=Xb,state=?SDL_RELEASED,button=1}, 
	 [Fi=#fi{x=X,index=I}|_], Store) ->
    #rb{labelw=LblW,spacew=SpaceW} = Rb = gb_trees:get(-I, Store),
    if
	Xb-X < LblW+4*SpaceW -> rb_set(Fi, Rb, Store);
	true -> keep
    end;
rb_event(_Ev, _Fi, _Store) -> keep.

rb_draw(Active, #fi{x=X,y=Y0}, #rb{label=Label,val=Val}, Common, DisEnable) ->
    FgColor = case DisEnable of
		  disable -> color3_disabled();
		  _-> color3_text()
	      end,
    Y = Y0+?CHAR_HEIGHT,
    gl:color3fv(case DisEnable of
		    disable -> color3();
		    _ -> {1,1,1}
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
    gl:color3fv(FgColor),
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
    gl:color3fv(FgColor),
    wings_io:text_at(X+2*?CHAR_WIDTH, Y, Label),
    if
	Active == true ->
	    Border = <<
		      2#0011111000000000:16,
		      2#0111111100000000:16,
		      2#1100000110000000:16,
		      2#1100000110000000:16,
		      2#1100000110000000:16,
		      2#1100000110000000:16,
		      2#1100000110000000:16,
		      2#0111111100000000:16,
		      2#0011111000000000:16>>,
	    gl:rasterPos2i(X, Y),
	    gl:bitmap(9, 9, 0, 1, 0, 0, Border);
	true -> ok
    end,
    gl:color3b(0, 0, 0),
    DisEnable.

rb_set(#fi{index=I,hook=Hook}, #rb{var=Var,val=Val}, Store) ->
    hook(Hook, update, [var(Var, I), I, Val, Store]).


%%%
%%% Menu
%%%

-record(menu,
	{def,
	 menu
	}).

menu(Menu0, Def) ->
    Menu = [case X of
		{D,V} -> {D,V,[]};
		{_,_,F}=DVF when is_list(F) -> DVF 
	    end || X <- Menu0],
    W = menu_width(Menu, 0) + 2*wings_text:width(" ") + 10,
    M = #menu{def=Def,menu=Menu},
    Fun = fun menu_event/3,
    {Fun,false,M,W,?LINE_HEIGHT+4}.

menu_event({redraw,Active}, 
	   [#fi{hook=Hook,key=Key,index=I,x=X,y=Y,w=W,h=H}|_], Store) ->
    #menu{menu=Menu} = gb_trees:get(-I, Store),
    Var = var(Key, I),
    Val = gb_trees:get(Var, Store),
    DisEnable = hook(Hook, is_disabled, [Var, I, Store]),
    ValStr = [Desc || {Desc,V,_} <- Menu, V =:= Val],
    menu_draw(Active, X, Y, W, H, ValStr, DisEnable);
menu_event(init, [#fi{key=Key,index=I}|_], Store) ->
    #menu{def=Def} = gb_trees:get(-I, Store),
    {store,gb_trees:enter(var(Key, I), Def, Store)};
menu_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
menu_event({key,_,_,$\s}, 
	   [#fi{x=X,y=Y,w=W,hook=Hook,key=Key,index=I}|_], Store) ->
    #menu{menu=Menu} = gb_trees:get(-I, Store),
    Var = var(Key, I),
    Val = gb_trees:get(Var, Store),
    Disabled = hook(Hook, menu_disabled, [Var, I, Store]),
    menu_popup(X, Y, W, Menu, Val, Disabled);
menu_event(#mousebutton{state=?SDL_PRESSED,button=1}, 
	   [#fi{x=X,y=Y,w=W,hook=Hook,key=Key,index=I}|_], Store) ->
    #menu{menu=Menu} = gb_trees:get(-I, Store),
    Var = var(Key, I),
    Val = gb_trees:get(Var, Store),
    Disabled = hook(Hook, menu_disabled, [Var, I, Store]),
    menu_popup(X, Y, W, Menu, Val, Disabled);
menu_event({popup_result,Val}, [#fi{index=I,key=Key,hook=Hook}|_], Store) ->
    hook(Hook, update, [var(Key, I), I, Val, Store]);
menu_event(_Ev, _Path, _Store) -> keep.

menu_width([{D,_,_}|T], W0) ->
    case wings_text:width(D) of
	W when W < W0 -> menu_width(T, W0);
	W -> menu_width(T, W)
    end;
menu_width([], W) -> W.

menu_draw(Active, X, Y0, W, H, Text, DisEnable) ->
    FgColor = case DisEnable of
		  disable -> color3_disabled();
		  _-> color3_text()
	      end,
    blend(fun(Col) ->
		  case DisEnable of
		      disable ->
			  wings_io:border(X, Y0+1, W-?CHAR_WIDTH+10,
					  H-3, Col, FgColor);
		      _ ->
			  wings_io:gradient_border(X, Y0+1, W-?CHAR_WIDTH+10,
						   H-3, Col, FgColor, Active)
		  end
	  end),
    Y = Y0+?CHAR_HEIGHT,
    gl:color3fv(FgColor),
    wings_io:text_at(X+5, Y, Text),
    Xr = X + W-8,
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
    gl:color3b(0, 0, 0),
    DisEnable.

%% Menu popup

-record(popup,
	{parent,				%Parent window name.
	 sel,					%Selected element (integer).
	 orig_sel,				%Original selection (integer).
	 menu					%Tuple.
	}).

menu_popup(X0, Y0, W, Menu0, Val, Disabled) ->
    {X1,Y1} = wings_wm:local2global(X0+?HMARGIN, Y0+?VMARGIN),
    Menu1 = [case proplists:get_value(V, Disabled) of
		 undefined -> {Desc,V,false,Flags};
		 true -> {Desc,V,true,Flags};
		 F when is_list(F) -> {Desc,V,true,F++Flags}
	     end || {Desc,V,Flags} <- Menu0],
    case popup_find_index(Menu1, Val) of
	0 -> ok;
	Sel ->
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
    popup_find_index(Menu, Val, 1, 0).

popup_find_index([], _Val, _I, J) -> J;
popup_find_index([{_,Val,false,_}|_], Val, I, _J) -> I;
popup_find_index([{_,_,false,_}|T], Val, I, 0) ->
    popup_find_index(T, Val, I+1, I);
popup_find_index([_|T], Val, I, J) ->
    popup_find_index(T, Val, I+1, J).


get_popup_event(Ps) ->
    {replace,fun(Ev) -> popup_event(Ev, Ps) end}.

popup_event(redraw, Ps) ->
    popup_redraw(Ps);
popup_event(#mousemotion{y=Y}, #popup{menu=Menu,sel=Sel0}=Ps) ->
    case ((Y-2) div ?LINE_HEIGHT)+1 of
	Sel when 1 =< Sel, Sel =< size(Menu) ->
	    {_,_,Disabled,Flags} = element(Sel, Menu),
	    case proplists:get_value(info, Flags) of
		undefined -> wings_wm:message("");
		Info -> wings_wm:message(Info)
	    end,
	    if  Disabled =:= true -> keep;
		Sel =:= Sel0 -> keep;
		true ->
		    wings_wm:dirty(),
		    get_popup_event(Ps#popup{sel=Sel})
	    end;
	_ ->
	    wings_wm:message(""),
	    keep
    end;
popup_event(#mousebutton{state=?SDL_RELEASED,button=1}, Ps) ->
%%%    wings_wm:message(""),
    popup_key($ , 0, $ , Ps);
popup_event(#keyboard{sym=Sym,mod=Mod,unicode=Unicode}, Ps) ->
    wings_wm:message(""),
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
	  #popup{parent=Parent,menu=Menu,orig_sel=OrigSel}) ->
    {_,Val,_,_} = element(OrigSel, Menu),
    wings_wm:send(Parent, {popup_result,Val}),
    delete;
popup_key(_Sym, _Mod, $\r, Ps) ->
    popup_key($ , Ps);
popup_key(_Sym, _Mod, Unicode, Ps) ->
    popup_key(Unicode, Ps).

popup_key(16, #popup{sel=Sel}=Ps) -> %Ctrl-P
    case popup_sel(-1, Sel, Ps) of
	Sel -> keep;
	NewSel ->
	    wings_wm:dirty(),
	    get_popup_event(Ps#popup{sel=NewSel})
    end;
popup_key(14, #popup{sel=Sel}=Ps) -> %Ctrl-N
    case popup_sel(+1, Sel, Ps) of
	Sel -> keep;
	NewSel ->
	    wings_wm:dirty(),
	    get_popup_event(Ps#popup{sel=NewSel})
    end;
popup_key($ , #popup{parent=Parent,menu=Menu,sel=Sel}) -> %Space
    {_,Val,_,_} = element(Sel, Menu),
    wings_wm:send(Parent, {popup_result,Val}),
    delete;
popup_key(_Unicode, _Ps) ->
    ?DEBUG_DISPLAY([_Unicode,_Ps]),
    keep.

popup_sel(Step, Sel, #popup{sel=PrevSel,menu=Menu}=Ps) ->
    case Sel+Step of
	NewSel when NewSel >= 1, NewSel =< size(Menu) ->
	    case element(NewSel, Menu) of
		{_,_,false,_} -> NewSel; % not disabled
		_ -> popup_sel(Step, NewSel, Ps)
	    end;
	_ -> PrevSel
    end.

popup_redraw(#popup{sel=Sel,orig_sel=OrigSel,menu=Menu}) ->
    FgColor = color3_text(),
    wings_io:ortho_setup(),
    {_,_,W,H} = wings_wm:viewport(),
    blend(fun(Col) ->
		  wings_io:border(0, 0, W-1, H-1, Col, FgColor)
	  end),
    gl:color3fv(FgColor),
    X = 3*?CHAR_WIDTH-1,
    Y = ?CHAR_HEIGHT+2,
    popup_redraw_1(1, Menu, Sel, W, X, ?CHAR_HEIGHT+2),
    gl:color3fv(FgColor),
    wings_io:text_at(X-10, OrigSel*Y, [crossmark]),
    gl:color3b(0, 0, 0),
    keep.

popup_redraw_1(Sel, Menu, Sel, W, X, Y) ->
    {Desc,_,_,_} = element(Sel, Menu),
    gl:color3f(0, 0, 0.5),
    gl:recti(X-2, Y+2, X+W-4*?CHAR_WIDTH, Y-?CHAR_HEIGHT+2),
    gl:color3fv(color3()),
    wings_io:text_at(X, Y, Desc),
    popup_redraw_1(Sel+1, Menu, Sel, W, X, Y+?LINE_HEIGHT);
popup_redraw_1(I, Menu, Sel, W, X, Y) when I =< size(Menu) ->
    {Desc,_,Disabled,_} = element(I, Menu),
    case Disabled of
	true ->
	    gl:color3fv(color3_disabled());
	false ->
	    gl:color3fv(color3_text())
    end,
    wings_io:text_at(X, Y, Desc),
    popup_redraw_1(I+1, Menu, Sel, W, X, Y+?LINE_HEIGHT);
popup_redraw_1(_, _, _, _, _, _) -> keep.



%%%
%%% Buttons
%%%

-record(but,
	{label,					%Textual label.
	 action}).

button(Action) -> button(button_label(Action), Action).

button(Label, Action) ->
    W = lists:max([wings_text:width([$\s,$\s|Label]),
		   wings_text:width(" cancel ")]),
    But = #but{label=Label,action=Action},
    Fun = fun button_event/3,
    {Fun,false,But,W,?LINE_HEIGHT+2+2}.

button_label(ok) -> "OK";
button_label(S) when is_list(S) -> S;
button_label(Act) -> wings_util:cap(atom_to_list(Act)).

button_event({redraw,Active}, [Fi=#fi{key=Key,hook=Hook,index=I}|_], Store) ->
    DisEnable = hook(Hook, is_disabled, [var(Key, I), I, Store]),
    button_draw(Active, Fi, gb_trees:get(-I, Store), DisEnable);
button_event(init, [#fi{key=Key,index=I}|_], Store) ->
    #but{action=Action} = gb_trees:get(-I, Store),
    %% Trick to not have to change store for 'ok' or 'cancel'
    Val = (Action =:= ok) orelse (Action =:= cancel),
    {store,gb_trees:insert(var(Key, I), Val, Store)};
button_event(value, [#fi{key=Key,index=I}|_], Store) ->
    case gb_trees:get(-I, Store) of
	#but{action=ok} -> none;
	#but{action=cancel} -> none;
	#but{} -> {value,gb_trees:get(var(Key, I), Store)}
    end;
button_event(#mousebutton{x=X,y=Y,state=?SDL_RELEASED,button=1},
	     Path=[#fi{x=Bx,y=By,w=W,h=H}|_], Store)
  when Bx =< X, X =< Bx+W, By =< Y, Y =< By+H ->
    button_event({key,$\s,0,$\s}, Path, Store);
button_event({key,_,Mod,$\r}, Path, Store) ->
    button_event({key,$\s,Mod,$\s}, Path, Store);
button_event({key,_,_,$\s}, [#fi{key=Key,index=I,hook=Hook}|_], Store0) ->
    #but{action=Action} = gb_trees:get(-I, Store0),
    case hook(Hook, update, [var(Key, I), I, true, Store0]) of
	keep -> Action;
	{store,Store} -> {Action,Store};
	Result -> Result
    end;
button_event(_Ev, _Path, _Store) -> keep.

button_draw(Active, #fi{x=X,y=Y0,w=W,h=H}, #but{label=Label}, DisEnable) ->
    Y = Y0+?CHAR_HEIGHT+2,
    FgColor = case DisEnable of
		  disable -> color3_disabled();
		  _ -> color3_text()
	      end,
    blend(fun(Col) ->
		  case DisEnable of
		      disable ->
			  wings_io:border(X, Y0+2, W, H-4, Col, FgColor);
		      _ ->
			  wings_io:gradient_border(X, Y0+2, W, H-4,
						   Col, FgColor, Active)
		  end
	  end),
    TextX = X + 2 + (W-wings_text:width(Label)) div 2,
    gl:color3fv(FgColor),
    wings_io:text_at(TextX, Y, Label),
    gl:color3b(0, 0, 0),
    DisEnable.



%%%
%%% Color box.
%%%

-define(COL_PREVIEW_SZ, 60).

-record(col,
	{val}).

color(RGB) ->
    Col = #col{val=RGB},
    Fun = fun col_event/3,
    {Fun,false,Col,3*?CHAR_WIDTH,?LINE_HEIGHT+2}.

col_event({redraw,Active}, [Fi=#fi{key=Key,hook=Hook,index=I}|_], Store) ->
    K = var(Key, I),
    DisEnable = hook(Hook, is_disabled, [K, I, Store]),
    col_draw(Active, Fi, gb_trees:get(K, Store), DisEnable);
col_event(init, [#fi{key=Key,index=I}|_], Store) ->
    #col{val=RGB} = gb_trees:get(-I, Store),
    {store,gb_trees:enter(var(Key, I), RGB, Store)};
col_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
col_event({key,_,_,$\s}, [#fi{key=Key,index=I}|_], Store) ->
    pick_color(var(Key, I), Store);
col_event(#mousemotion{x=Xm,y=Ym}, [Fi=#fi{key=Key,index=I}|_], Store) ->
    case col_inside(Xm, Ym, Fi) of
	true -> keep;
	false -> 
	    RGB = gb_trees:get(var(Key, I), Store),
	    {drag,{3*?CHAR_WIDTH,?CHAR_HEIGHT},{color,RGB}}
    end;
col_event(#mousebutton{x=Xm,y=Ym,state=?SDL_RELEASED,button=1}, 
	  [Fi=#fi{key=Key,index=I}|_], Store) ->
    case col_inside(Xm, Ym, Fi) of
	true -> pick_color(var(Key, I), Store);
	false -> keep
    end;
col_event({drop,{color,RGB1}}, [#fi{key=Key,index=I}|_], Store) ->
    K = var(Key, I),
    RGB0 = gb_trees:get(K, Store),
    RGB = replace_rgb(RGB0, RGB1),
    {store,gb_trees:update(K, RGB, Store)};
col_event(_Ev, _Path, _Store) -> keep.

%% replace_rgb(OldRGBA, NewRGBA) -> RGBA
%%  Replace a color (RGB + possibly A) with a new color,
%%  making sure that the new color has the same number of components.
replace_rgb({_,_,_}, {_,_,_}=RGB) -> RGB;
replace_rgb({_,_,_}, {R,G,B,_}) -> {R,G,B};
replace_rgb({_,_,_,_}, {_,_,_,_}=RGB) -> RGB;
replace_rgb({_,_,_,A}, {R,G,B}) -> {R,G,B,A}.

col_draw(Active, #fi{x=X,y=Y0}, RGB, DisEnable) ->
    FgColor = case DisEnable of
		  disable -> color3_disabled();
		  _-> color3_text()
	      end,
    case DisEnable of
	disable ->
	    wings_io:border(X, Y0+3, 3*?CHAR_WIDTH, ?CHAR_HEIGHT,
			    RGB, FgColor);
	_ ->
	    wings_io:sunken_rect(X, Y0+3, 3*?CHAR_WIDTH, ?CHAR_HEIGHT,
				 RGB, color4(), Active)
    end,
    gl:color3b(0, 0, 0),
    DisEnable.

col_inside(Xm, Ym, #fi{x=X,y=Y}) ->
    inside(Xm, Ym, X, Y, 3*?CHAR_WIDTH, ?CHAR_HEIGHT+2).

pick_color(Key, Store) ->
    RGB0 = gb_trees:get(Key, Store),
    wings_color:choose(RGB0, fun(RGB) -> {drop,{color,RGB}} end).



%%%
%%% Custom
%%%

-record(custom,
	{handler, val}
       ).

custom(W, H, Handler) ->
    Custom = #custom{handler=Handler},
    Fun = fun custom_event/3,
    {Fun,true,Custom,W,H}.

custom_event({redraw,_Active}, [#fi{x=X,y=Y,w=W,h=H,index=I}|_], Store) ->
    #custom{handler=Handler} = gb_trees:get(-I, Store),
    Handler(X, Y, W, H, Store);
custom_event(_Ev, _Path, _Store) -> keep.



%%%
%%% Panel
%%%

-record(panel, {}).

panel() -> {fun (_Ev, _Path, _Store) -> keep end, true, #panel{}, 0, 0}.

%%%
%%% Eyepicker.
%%%

-record(eyepicker, {}).

eyepicker() ->
    {fun eyepicker_event/3, true, #eyepicker{}, 0, 0}.

eyepicker_event(init, [#fi{key=Key,index=I}|_], Store) ->
    Var = var(Key, I),
    case gb_trees:is_defined(Var, Store) of
	true -> keep;
	false -> {store,gb_trees:insert(Var, undefined, Store)}
    end;
eyepicker_event({picked_color,Col}, 
		[#fi{key=Key,index=I,hook=Hook}|_], 
		Store)  ->
    hook(Hook, update, [var(Key, I),I,Col,Store]);
eyepicker_event(_Ev, _Path, _Store) -> keep.

%%%
%%% Position
%%%

position(Position) ->
    {fun position_event/3, value, #position{position=Position}, 0, 0}.

position_event(value, _Path, _Store) ->
    {value,wings_wm:win_ul({controller,wings_wm:this()})};
position_event(_Ev, _Path, _Store) -> keep.

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
    Fun = fun label_event/3,
    {W,H} = label_dimensions(Lines, 0, 2),
    {Fun,true,Lbl,W,H}.

label_dimensions([L|Lines], W0, H) ->
    case wings_text:width(L) of
	W when W > W0 -> label_dimensions(Lines, W, H+?LINE_HEIGHT);
	_ -> label_dimensions(Lines, W0, H+?LINE_HEIGHT)
    end;
label_dimensions([], W, H) -> {W,H}.

label_event({redraw,_Active}, [#fi{x=X,y=Y,index=I}|_], Store) ->
    #label{lines=Lines} = gb_trees:get(-I, Store),
    gl:color3fv(color3_text()),
    label_draw(Lines, X, Y+?CHAR_HEIGHT);
label_event(_Ev, _Path, _Store) -> keep.

label_draw([L|Lines], X, Y) ->
    gl:color3fv(color3_text()),
    wings_io:text_at(X, Y, L),
    label_draw(Lines, X, Y+?LINE_HEIGHT),
    gl:color3b(0, 0, 0),
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

text(Val, Flags) ->
    IsInteger = is_integer(Val),
    ValStr = text_val_to_str(Val),
    {Max0,Validator,Charset,Range} = validator(Val, Flags),
    {SliderH,Val} =
	case proplists:get_value(color, Flags) of
	    {_Type,_KeyC}=TK ->
		{9,TK};
	    undefined ->
		{2,Val}
	end,
    Max = case proplists:get_value(width, Flags) of
	      undefined -> Max0;
	      M when integer(M), M >= 1 -> M
	  end,
    Password = proplists:get_bool(password, Flags),
    Ts = #text{last_val=Val,bef=[],aft=ValStr,max=Max,
	       integer=IsInteger,charset=Charset,
               validator=Validator,password=Password,
	       slider_h=SliderH,slider_range=Range,val=Val},
    Fun = fun gen_text_handler/3,
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

gen_text_handler({redraw,true}, 
		 [Fi=#fi{key=Key,hook=Hook,index=I}|_], 
		 Store) ->
    DisEnable = hook(Hook, is_disabled, [var(Key, I), I, Store]),
    draw_text_active(Fi, gb_trees:get(-I, Store), DisEnable);
gen_text_handler({redraw,false}, 
		 [Fi=#fi{key=Key,hook=Hook,index=I}|_], 
		 Store) ->
    K = var(Key, I),
    DisEnable = hook(Hook, is_disabled, [K, I, Store]),
    Val = gb_trees:get(K, Store),
    draw_text_inactive(Fi, gb_trees:get(-I, Store), Val, DisEnable);
gen_text_handler(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
gen_text_handler(init, [#fi{key=Key,index=I}|_], Store) ->
    #text{last_val=Val} = gb_trees:get(-I, Store),
    {store,gb_trees:enter(var(Key, I), Val, Store)};
gen_text_handler(Ev, [Fi=#fi{key=Key,index=I,hook=Hook}|_], Store) ->
    #text{last_val=Val0} = Ts0 = gb_trees:get(-I, Store),
    K = var(Key, I),
    Ts1 = case gb_trees:get(K, Store) of
	      Val0 -> Ts0;
	      Val1 ->
		  ?DEBUG_DISPLAY([K,I,Val1]),
		  ValStr = text_val_to_str(Val1),
		  Ts0#text{bef=[],aft=ValStr}
	  end,
    Ts = text_event(Ev, Fi, Ts1),
    case text_get_val(Ts) of
	Val0 ->
	    {store,gb_trees:update(-I, Ts, Store)};
	Val ->
	    hook(Hook, update, 
		 [K,I,Val,gb_trees:update(-I, Ts#text{last_val=Val}, Store)])
    end.

draw_text_inactive(#fi{x=X0,y=Y0}, #text{max=Max,password=Password},
		   Val, DisEnable) ->
    Str0 = string:substr(text_val_to_str(Val), 1, Max),
    Str = case Password of
	      true -> stars(Str0);
	      false -> Str0
	  end,
    FgColor =
	case DisEnable of
	    disable ->
		blend(fun(Col) ->
			      wings_io:sunken_rect(X0, Y0+2,
						   (Max+1)*?CHAR_WIDTH,
						   ?CHAR_HEIGHT+1,
						   Col, Col)
		      end),
		color3_disabled();
	    _ ->
		wings_io:sunken_gradient(X0, Y0+2,
					 (Max+1)*?CHAR_WIDTH, ?CHAR_HEIGHT+1,
					 {0.82,0.82,0.82}, color4(), false),
		color3_text()
	end,
    Y = Y0 + ?CHAR_HEIGHT,
    X = X0 + (?CHAR_WIDTH div 2),
    gl:color3fv(FgColor),
    wings_io:text_at(X, Y, Str),
    DisEnable.

draw_text_active(#fi{x=X0,y=Y0},
		 #text{sel=Sel,bef=Bef,aft=Aft,max=Max,password=Password},
		 DisEnable) ->
    wings_io:sunken_gradient(X0, Y0+2, (Max+1)*?CHAR_WIDTH, ?CHAR_HEIGHT+1,
			     {0.82,0.82,0.82}, color4(), true),
    Y = Y0 + ?CHAR_HEIGHT,
    X = X0 + (?CHAR_WIDTH div 2),
    Len = length(Bef),
    Str0 = case Password of
	      true -> stars(Len+length(Aft));
	      false -> reverse(Bef, Aft)
	  end,
    Str = string:substr(Str0, 1, Max),
    ?DEBUG_DISPLAY([Sel,Bef,Aft]),
    gl:color3fv(color3_text()),
    wings_io:text_at(X, Y, Str),
    case {DisEnable,abs(Sel)} of
	{disable,_} ->
	    ok;
	{_,0} ->
	    gl:color3f(1, 0, 0),
	    X1 = X+Len*?CHAR_WIDTH,
	    wings_io:text_at(X1, Y, [caret]);
	{_,N} ->
	    Skip = min(Len, Len+Sel),
	    SelStr = string:substr(Str, Skip+1, N),
	    gl:color3f(0, 0, 0.5),
	    X1 = X+Skip*?CHAR_WIDTH,
 	    gl:recti(X1, Y-?CHAR_HEIGHT+3, X1+N*?CHAR_WIDTH, Y+2),
 	    gl:color3f(1, 1, 1),
	    wings_io:text_at(X1, Y, SelStr)
    end,
    gl:color3b(0, 0, 0),
    DisEnable.

stars(N) when integer(N) ->
    duplicate(N, $*);
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
text_event(#mousebutton{x=X,state=?SDL_PRESSED,button=1}, Fi, Ts0) ->
    Ts = text_pos(X, Fi, Ts0),
    ?DEBUG_DISPLAY(Ts),
    Ts;
text_event(#mousebutton{x=X,state=?SDL_RELEASED,button=1}, Fi, Ts0) ->
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
-define(SL_BAR_W, 10).
-define(SL_BAR_H, 10).

-record(sl,
	{min,
	 range,
	 color,
	 h
	}).

slider(Flags) ->
    {Min,Max} = proplists:get_value(range, Flags),
    Color = case proplists:get_value(color, Flags) of
		undefined -> undefined;
		{T,_,_}=C when T==r;T==g;T==b;T==h;T==s;T==v -> C
	    end,
    Sl = #sl{min=Min,range=Max-Min,color=Color,h=?SL_BAR_H},
    Fun = fun slider_event/3,
    {Fun,false,Sl,?SL_LENGTH+?SL_BAR_W,?LINE_HEIGHT+2}.

slider_event(init, [#fi{key=Key,flags=Flags,index=I}|_], Store) ->
    case proplists:get_value(value, Flags) of
	undefined ->
	    keep;
	Val ->
	    {store,gb_trees:enter(var(Key, I), Val, Store)}
    end;
slider_event({redraw,Active}, [Fi=#fi{key=Key,hook=Hook,index=I}|_], Store) ->
    Sl = gb_trees:get(-I, Store),
    K = var(Key, I),
    Val = gb_trees:get(K, Store),
    DisEnable = hook(Hook, is_disabled, [K, I, Store]),
    slider_redraw(Active, Fi, Sl, Val, Store, DisEnable);
slider_event(value, [#fi{flags=Flags,key=Key,index=I}|_], Store) ->
    case proplists:get_value(value, Flags) of
	undefined ->
	    none;
	_ ->
	    {value,gb_trees:get(var(Key, I), Store)}
    end;
slider_event(#mousebutton{x=Xb,state=?SDL_RELEASED,button=1}, [Fi|_], Store) ->
    slider_event_move(Xb, Fi, Store);
slider_event(#mousemotion{x=Xb,state=Bst}, [Fi|_], Store) 
  when (Bst band ?SDL_BUTTON_LMASK) =/= 0 ->
    slider_event_move(Xb, Fi, Store);
slider_event({key,?SDLK_LEFT,_,_}, [Fi|_], Store) ->
    slider_move(-1, Fi, Store);
slider_event({key,?SDLK_RIGHT,_,_}, [Fi|_], Store) ->
    slider_move(1, Fi, Store);
slider_event({key,?SDLK_LEFT,_,_}, [Fi|_], Store) ->
    slider_move(1, Fi, Store);
slider_event({key,?SDLK_UP,_,_}, [Fi|_], Store) ->
    slider_move(10, Fi, Store);
slider_event({key,?SDLK_DOWN,_,_}, [Fi|_], Store) ->
    slider_move(-10, Fi, Store);
slider_event({key,?SDLK_HOME,_,_}, [#fi{key=Key,hook=Hook,index=I}|_], Store) ->
    #sl{min=Min} = gb_trees:get(-I, Store),
    slider_update(Hook, Min, Key, I, Store);
slider_event({key,?SDLK_END,_,_}, [#fi{key=Key,hook=Hook,index=I}|_], Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    slider_update(Hook, Min+Range, Key, I, Store);
slider_event({key,_,_,6}, [Fi|_], Store) -> %Ctrl-F
    slider_move(1, Fi, Store);
slider_event({key,_,_,2}, [Fi|_], Store) -> %Ctrl-B
    slider_move(-1, Fi, Store);
slider_event({key,_,_,16}, [Fi|_], Store) -> %Ctrl-P
    slider_move(10, Fi, Store);
slider_event({key,_,_,14}, [Fi|_], Store) -> %Ctrl-N
    slider_move(-10, Fi, Store);
slider_event(_Ev, _Path, _Store) -> keep.

slider_move(D0, #fi{key=Key,index=I,hook=Hook}, Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    Val0 = gb_trees:get(var(Key, I), Store),
    D = if
	     is_integer(Min), D0 > 0 -> max(round(D0*0.01*Range), 1);
	     is_integer(Min), D0 < 0 -> min(round(D0*0.01*Range), -1);
	     is_float(Min) -> D0*0.01*Range
	 end,
    Val = max(Min, min(Min+Range, Val0+D)),
    slider_update(Hook, Val, Key, I, Store).

slider_event_move(Xb, #fi{x=X,key=Key,index=I,hook=Hook}, Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    Pos = max(0, min(Xb-X, ?SL_LENGTH)),
    V = Min + Pos*Range/?SL_LENGTH,
    Val = if
	      is_integer(Min) -> round(V);
	      true -> V
	  end,
    slider_update(Hook, Val, Key, I, Store).

slider_update(Hook, Val, Key, I, Store) ->
    K = var(Key, I),
    ?DEBUG_DISPLAY([Val,K]),
    hook(Hook, update, [K, I, Val, Store]).

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
		  wings_io:gradient_border(X, Y-(H div 2), W, H,
					   Col, {0,0,0}, Active)
	  end),
    Pos = color_slider(C, Min, Range, X, W, Y-(H div 2), H),
    XPos = X+Pos,
    YPos = Y-(?SL_BAR_H div 2)+1,
    case DisEnable of
	disable ->
	    blend(fun(Col) ->
			  wings_io:border(XPos, YPos, ?SL_BAR_W, ?SL_BAR_H-2,
					  Col, color3_disabled())
		  end);
	_ ->
	    Col = color4(),
	    wings_io:raised_rect(XPos+1, YPos, ?SL_BAR_W-1, ?SL_BAR_H-2,
				 Col, Col)
    end,
    gl:color3b(0, 0, 0),
    DisEnable.

color_slider({h,{Hue,S,V}}, Min, Range, X, W, Y, H) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    hue_color_slider(S, V, X, W, Y, H),
    gl:'end'(),
    slider_pos(Hue, Min, Range);
color_slider(Val, Min, Range, X, W, Y, H) when is_number(Val) ->
    Pos0 = slider_pos(Val, Min, Range),
    Pos = Pos0 + ?SL_BAR_W div 2,
    {R,G,B,A} = Col = color4(),
    Darker = {R-0.15,G-0.15,B-0.15,A},
    wings_io:gradient_rect(X+1, Y+1, Pos, H-1, Darker),
    wings_io:gradient_rect(X+Pos, Y+1, W-Pos, H-1, Col),
    Pos0;
color_slider(C, Min, Range, X, W, Y, H) ->
    {Val,[SCol,ECol]} = get_col_range(C),
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    wings_io:set_color(SCol),
    gl:vertex2f(X+1,Y+H),
    gl:vertex2f(X+1,Y+1),
    wings_io:set_color(ECol),
    gl:vertex2f(X+W,Y+1),
    gl:vertex2f(X+W,Y+H),
    gl:'end'(),
    slider_pos(Val, Min, Range).

slider_pos(Val, Min, Range) ->
    round(?SL_LENGTH * (Val-Min) / Range).

hue_color_slider(S, V, X, W, Y, H) ->
    wings_io:set_color(hsv_to_rgb(0, S, V)),
    hue_color_slider(S, V, X+1, W-1, Y, H, 0).

hue_color_slider(_, _, _, _, _, _, Hue) when Hue > (360-60) ->
    ok;
hue_color_slider(S, V, X, W, Y, H, Hue) ->
    X0 = X+W*Hue/360.0,
    X1 = X+W*(Hue+60)/360.0,
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
    {V,[V0,V1]}.


%% Common data storage key. Integer key uses relative field index as
%% storage key, non-integer uses itself.
var(0, I) when integer(I) -> I;
var(Key, I) when integer(Key), integer(I) -> I+Key;
var(Key, I) when integer(I) -> Key.


inside(Xm, Ym, X, Y, W, H) ->
    X =< Xm andalso Xm < X+W andalso Y =< Ym andalso Ym < Y+H.


dialog_unzip(L) ->
    dialog_unzip(L, [], []).
dialog_unzip([{Lbl,F}|T], AccA, AccB) ->
    dialog_unzip(T, [{label,Lbl}|AccA], [F|AccB]);
dialog_unzip([], AccA, AccB) ->
    {reverse(AccA),reverse(AccB)}.


blend(Draw) ->
    wings_io:blend(color4(), Draw).


color3_text() ->
    wings_pref:get_value(dialog_text).

color3_disabled() ->
    wings_pref:get_value(dialog_disabled).


color4_highlight() ->
    wings_color:mix(?BEVEL_HIGHLIGHT_MIX, {1,1,1}, color4()).

color4_lowlight() ->
    wings_color:mix(?BEVEL_LOWLIGHT_MIX, {0,0,0}, color4()).

color3() ->
    {R,G,B,_} = color4(),
    {R,G,B}.

color4() ->
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
    case gb_trees:get(Var, Store) of
	Val -> keep;
	_ ->
	    case Hook of
		undefined -> {store,gb_trees:update(Var, Val, Store)};
		_ when is_function(Hook) ->
		    case Hook(update, {Var,I,Val,Store}) of
			void -> {store,gb_trees:update(Var, Val, Store)};
			%% Result -> Result % but more paranoid
			keep -> keep;
			{store,_}=Result -> Result;
			done -> done;
			{done,_}=Result -> Result;
			{layout,_}=Result -> Result
		    end
	    end
    end;
hook(Hook, menu_disabled, [Var, I, Store]) ->
    case Hook of
	undefined -> [];
	_ when is_function(Hook) ->
	    case Hook(menu_disabled, {Var,I,Store}) of
		void -> [];
		Disabled when is_list(Disabled) -> Disabled
	    end
    end.
