%%
%%  wings_drag.erl --
%%
%%     This module handles interactive commands.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_drag.erl,v 1.120 2003/01/01 15:32:24 bjorng Exp $
%%

-module(wings_drag).
-export([setup/3,setup/4,do_drag/1,translate/4]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(CAMMAX, 150).

-import(lists, [foreach/2,map/2,foldl/3,sort/1,keysort/2,
		reverse/1,reverse/2,member/2]).

%% Main drag record. Kept in state.
-record(drag,
	{x,					%Original 2D position
	 y,
	 xs=0,                                  %Summary of mouse movements
	 ys=0,
	 zs=0,
	 xt=0,                                  %Last warp length
	 yt=0,
	 mmb_count=0,
	 offset,				%Offset for each dimension.
	 unit,					%Unit that drag is done in.
	 flags=[],				%Flags.
	 falloff,				%Magnet falloff.
	 magnet,				%Magnet: true|false.
	 info="",				%Information line.
	 st					%Saved st record.
	}).

%% Drag per object.
-record(do,
	{funs					%List of transformation funs.
	}).

setup(Tvs, Unit, St) ->
    setup(Tvs, Unit, [], St).

setup(Tvs, Unit, Flags, St) ->
    wings_io:grab(),
    wings_wm:grab_focus(geom),
    Magnet = proplists:get_value(magnet, Flags, none),
    Offset0 = proplists:get_value(initial, Flags, []),
    Offset = pad_initials(Offset0),
    Drag = #drag{unit=Unit,flags=Flags,offset=Offset,
		 falloff=falloff(Unit),magnet=Magnet,st=St},
    wings_draw:update_dlists(St),
    case Tvs of
	{matrix,TvMatrix} -> insert_matrix(TvMatrix);
	{general,General} -> break_apart_general(General);
	_ -> break_apart(Tvs, St)
    end,
    {drag,Drag}.

falloff([falloff|_]) -> 1.0;
falloff([_|T]) -> falloff(T);
falloff([]) -> none.

%%
%% Here we break apart the objects into two parts - static part
%% (not moved during drag) and dynamic (part of objects actually
%% moved).
%%
break_apart(Tvs0, St) ->
    S = sofs:relation(Tvs0, [{id,info}]),
    F = sofs:relation_to_family(S),
    Tvs = sofs:to_external(F),
    wings_draw_util:map(fun(D, Data) ->
				break_apart(D, Data, St)
			end, Tvs).

break_apart(#dlo{src_we=#we{id=Id}=We}=D0, [{Id,TvList0}|Tvs], St) ->
    TvList = mirror_constrain(TvList0, We),
    {Vs,FunList} = combine_tvs(TvList, We),
    D1 = if
	     ?IS_LIGHT(We) -> D0#dlo{split=none};
	     true -> D0
	 end,
    D = wings_draw:split(D1, Vs, St),
    Do = #do{funs=FunList},
    {D#dlo{drag=Do},Tvs};
break_apart(D, Tvs, _) -> {D,Tvs}.

combine_tvs(TvList, #we{vp=Vtab}) ->
    {FunList,VecVs0} = split_tv(TvList, [], []),
    SS = sofs:from_term(VecVs0, [{vec,[vertex]}]),
    FF = sofs:relation_to_family(SS),
    FU = sofs:family_union(FF),
    VecVs1 = sofs:to_external(FU),
    Affected = foldl(fun({_,Vs}, A) -> Vs++A end, [], VecVs1),
    case insert_vtx_data(VecVs1, Vtab, []) of
	[] -> combine_tv_1(FunList, Affected, []);
	VecVs -> combine_tv_1(FunList, Affected, [translate_fun(VecVs)])
    end.

translate_fun(VecVs) ->
    fun(new_falloff, _Falloff) ->
	    translate_fun(VecVs);
       ([Dx|_], Acc) ->
	    foldl(fun({Vec,VsPos}, A) ->
			  translate(Vec, Dx, VsPos, A)
		  end, Acc, VecVs)
    end.

combine_tv_1([{Aff,Fun}|T], Aff0, FunList) ->
    combine_tv_1(T, Aff++Aff0, [Fun|FunList]);
combine_tv_1([], Aff, FunList) -> {Aff,FunList}.

split_tv([{_,F}=Fun|T], Facc, Vacc) when is_function(F) ->
    split_tv(T, [Fun|Facc], Vacc);
split_tv([L|T], Facc, Vacc) when is_list(L) ->
    split_tv(T, Facc, L++Vacc);
split_tv([], Funs, VecVs) -> {Funs,VecVs}.

insert_vtx_data([{Vec,Vs0}|VecVs], Vtab, Acc) ->
    Vs = insert_vtx_data_1(Vs0, Vtab, []),
    insert_vtx_data(VecVs, Vtab, [{Vec,Vs}|Acc]);
insert_vtx_data([], _Vtab, Acc) -> Acc.

insert_vtx_data_1([V|Vs], Vtab, Acc) ->
    insert_vtx_data_1(Vs, Vtab, [{V,gb_trees:get(V, Vtab)}|Acc]);
insert_vtx_data_1([], _Vtab, Acc) -> Acc.

mirror_constrain(Tvs, #we{mirror=none}) -> Tvs;
mirror_constrain(Tvs, #we{mirror=Face,fs=Ftab}=We) ->
    case gb_trees:is_defined(Face, Ftab) of
	false -> Tvs;
	true ->
	    [V|_] = Vs = wings_face:surrounding_vertices(Face, We),
	    VsSet = ordsets:from_list(Vs),
	    N = wings_face:face_normal(Vs, We),
	    Vpos = wings_vertex:pos(V, We),
	    mirror_constrain_1(Tvs, VsSet, {N,Vpos}, [])
    end.

mirror_constrain_1([{Vs,Tr0}=Fun|Tvs], VsSet, N, Acc) when is_function(Tr0) ->
    case ordsets:intersection(ordsets:from_list(Vs), VsSet) of
	[] ->
	    mirror_constrain_1(Tvs, VsSet, N, [Fun|Acc]);
	[_|_]=Mvs ->
	    Tr = constrain_fun(Tr0, N, Mvs),
	    mirror_constrain_1(Tvs, VsSet, N, [{Vs,Tr}|Acc])
    end;
mirror_constrain_1([VecVs0|Tvs], VsSet, N, Acc) ->
    VecVs1 = sofs:from_term(VecVs0, [{vec,[vertex]}]),
    VecVs2 = sofs:family_to_relation(VecVs1),
    VecVs3 = sofs:to_external(VecVs2),
    VecVs = mirror_constrain_2(VecVs3, VsSet, N, []),
    mirror_constrain_1(Tvs, VsSet, N, [VecVs|Acc]);
mirror_constrain_1([], _, _, Acc) -> Acc.

mirror_constrain_2([{Vec0,V}|T], VsSet, {N,_}=Plane, Acc) ->
    case member(V, VsSet) of
	false ->
	    mirror_constrain_2(T, VsSet, Plane, [{Vec0,[V]}|Acc]);
	true ->
	    Vec = project_vector(Vec0, N),
	    mirror_constrain_2(T, VsSet, Plane, [{Vec,[V]}|Acc])
    end;
mirror_constrain_2([], _, _, Acc) -> Acc.

project_vector(Vec, Plane) ->
    e3d_vec:sub(Vec, e3d_vec:mul(Plane, e3d_vec:dot(Vec, Plane))).

constrain_fun(Tr0, Plane, Vs) ->
    fun(Cmd, Arg) ->
	    case Tr0(Cmd, Arg) of
		Tr when is_function(Tr) ->
		    constrain_fun(Tr, Plane, Vs);
		List ->
		    constrain_vs(List, Vs, Plane, [])
	    end
    end.

constrain_vs([{V,Pos0}=H|T], Vs, {N,Point}=Plane, Acc) ->
    case member(V, Vs) of
	false -> constrain_vs(T, Vs, Plane, [H|Acc]);
	true ->
	    ToPoint = e3d_vec:sub(Point, Pos0),
	    Dot = e3d_vec:dot(ToPoint, N),
	    ToPlane = e3d_vec:mul(N, Dot),
	    Pos = e3d_vec:add(Pos0, ToPlane),
	    constrain_vs(T, Vs, Plane, [{V,Pos}|Acc])
    end;
constrain_vs([], _, _, Acc) -> Acc.

insert_matrix(Tvs) ->
    Id = e3d_mat:identity(),
    wings_draw_util:map(fun(D, Data) ->
				insert_matrix_fun(D, Data, Id)
			end, sort(Tvs)).

insert_matrix_fun(#dlo{work=Work,sel=Sel,wire=W,src_sel=SrcSel,src_we=#we{id=Id}=We,mirror=M},
		  [{Id,Tr}|Tvs], Matrix) ->
    {#dlo{work=Work,sel=Sel,drag={matrix,Tr,Matrix,e3d_mat:expand(Matrix)},
	  wire=W,src_we=We,src_sel=SrcSel,mirror=M},Tvs};
insert_matrix_fun(D, Tvs, _) -> {D,Tvs}.

break_apart_general(Tvs) ->
    wings_draw_util:map(fun break_apart_general/2, Tvs).

break_apart_general(#dlo{src_we=#we{id=Id}}=D, [{Id,Fun}|Tvs]) ->
    {D#dlo{drag={general,Fun}},Tvs};
break_apart_general(D, Tvs) -> {D,Tvs}.
    
%%%
%%% Handling of drag events.
%%%

do_drag(Drag) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    Ev = #mousemotion{x=X,y=Y,state=0},
    {seq,push,handle_drag_event_1(Ev, Drag#drag{x=X,y=Y})}.

help_message(#drag{unit=Unit}=Drag) ->
    Msg = "[L] Accept  [R] Cancel",
    Zmsg = case length(Unit) > 2 of
	       false -> [];
	       true -> ["  Drag ",zmove_help()," Move along Z"]
	   end,
    wings_wm:message([Msg,Zmsg], help_message_right(Drag)).

help_message_right(#drag{magnet=none,falloff=Falloff}) ->
    Help = "[Tab] Numeric entry  [Shift] and/or [Ctrl] Constrain",
    case Falloff of
	none -> Help;
	_ -> ["[+] or [-] Tweak R  "|Help]
    end;
help_message_right(#drag{magnet=Type}) -> wings_magnet:drag_help(Type).

zmove_help() ->
    Buttons = wings_pref:get_value(num_buttons),
    case wings_pref:get_value(camera_mode) of
	nendo when Buttons == 1 -> "[Alt]+[L]";
	nendo when Buttons == 2 -> "[Ctrl]+[R]";
	nendo -> "[M]";
	mirai -> "[M]";
	maya -> "[M]";
	tds -> "[Ctrl]+[R]";
	blender -> "[Ctrl]+[R]"
    end.

pad_initials(Ds) ->
    case length(Ds) of
	L when L >= 3 -> Ds;
	L -> Ds ++ lists:duplicate(3-L, 0.0)
    end.

get_drag_event(Drag) ->
    wings_wm:dirty(),
    get_drag_event_1(Drag).

get_drag_event_1(Drag) ->
    {replace,fun(Ev) -> handle_drag_event(Ev, Drag) end}.

handle_drag_event(#keyboard{keysym=#keysym{sym=9}}, Drag) ->
    numeric_input(Drag);
handle_drag_event(#mousebutton{button=2,state=?SDL_RELEASED},
		  #drag{mmb_count=C}=Drag) when C > 2 ->
    get_drag_event_1(Drag#drag{mmb_count=0});
handle_drag_event(#mousebutton{button=3,state=?SDL_RELEASED}=Ev,
		  #drag{mmb_count=C}=Drag) when C > 2 ->
    case sdl_keyboard:getModState() of
	Mod when Mod band ?CTRL_BITS =/= 0 ->
           get_drag_event_1(Drag#drag{mmb_count=0});
	_ ->
	    handle_drag_event_0(Ev, Drag)
    end;
handle_drag_event(Event, Drag) ->
    case wings_camera:event(Event, fun() -> redraw(Drag) end) of
	next -> handle_drag_event_0(Event, Drag);
	Other -> Other
    end.

handle_drag_event_0(#keyboard{}=Ev, #drag{magnet=none}=Drag) ->
    handle_drag_event_1(Ev, Drag);
handle_drag_event_0(#keyboard{keysym=#keysym{unicode=C}}=Ev, Drag0) ->
    case wings_magnet:hotkey(C) of
	none -> handle_drag_event_1(Ev, Drag0);
	Type ->
	    wings_wm:message_right(wings_magnet:drag_help(Type)),
	    Val = {Type,Drag0#drag.falloff},
	    Drag = parameter_update(new_type, Val, Drag0#drag{magnet=Type}),
	    get_drag_event(Drag)
    end;
handle_drag_event_0(Ev, Drag) -> handle_drag_event_1(Ev, Drag).

handle_drag_event_1(redraw, Drag) ->
    help_message(Drag),
    redraw(Drag),
    get_drag_event_1(Drag);
handle_drag_event_1(#mousemotion{}=Ev, Drag0) ->
    {_,Drag} = motion(Ev, Drag0),
    get_drag_event(Drag);
handle_drag_event_1(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED}, Drag0) ->
    wings_wm:release_focus(),
    wings_io:ungrab(),
    Ev = #mousemotion{x=X,y=Y,state=0},
    {Move,Drag} = ?SLOW(motion(Ev, Drag0)),
    St = normalize(Drag),
    DragEnded = {new_state,St#st{args=Move}},
    wings_wm:send(geom, DragEnded),
    pop;
handle_drag_event_1({drag_arguments,Move}, Drag0) ->
    wings_wm:release_focus(),
    wings_io:ungrab(),
    clear_sel_dlists(),
    Drag = ?SLOW(motion_update(Move, Drag0)),
    St = normalize(Drag),
    DragEnded = {new_state,St#st{args=Move}},
    wings_wm:send(geom, DragEnded),
    pop;
handle_drag_event_1(#mousebutton{button=3,state=?SDL_RELEASED}, _Drag) ->
    wings_draw_util:map(fun invalidate_fun/2, []),
    wings_wm:release_focus(),
    wings_io:ungrab(),
    wings_wm:dirty(),
    pop;
handle_drag_event_1(view_changed, Drag) ->
    get_drag_event(view_changed(Drag));
handle_drag_event_1({action,{numeric_input,Move}}, Drag) ->
    handle_drag_event_1({drag_arguments,Move}, Drag);
handle_drag_event_1(Event, #drag{st=St}=Drag0) ->
    Drag = case wings_hotkey:event(Event) of
	       next -> Drag0;
	       {view,Cmd} ->
		   wings_view:command(Cmd, St),
		   view_changed(Drag0);
	       {select,less} ->
		   magnet_radius(-1, Drag0);
	       {select,more} ->
		   magnet_radius(1, Drag0);
	       _Other -> Drag0
	   end,
    get_drag_event(Drag).

invalidate_fun(#dlo{drag=none}=D, _) -> D;
invalidate_fun(#dlo{src_we=We}=D, _) -> D#dlo{src_we=We#we{es=none}}.
    
numeric_input(Drag0) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    Ev = #mousemotion{x=X,y=Y,state=0},
    wings_wm:set_me_modifiers(0),
    {Move0,Drag} = mouse_translate(Ev, Drag0),
    wings_ask:dialog("Numeric Input", make_query(Move0, Drag),
		     fun(Res) ->
			     {numeric_input,make_move(Res, Drag)}
		     end).

make_query(Move, #drag{unit=Units}) ->
    make_query_1(Units, Move).

make_query_1([U0|Units], [V|Vals]) ->
    case clean_unit(U0) of
	percent ->
	    [{hframe,[{text,V*100.0,qrange(U0)},{label,"%"}]}|
	     make_query_1(Units, Vals)];
	angle ->
	    [{hframe,[{label,"A"},{text,V,qrange(U0)},{label,[?DEGREE]}]}|
	     make_query_1(Units, Vals)];
	U ->
	    [{hframe,[{label,qstr(U)},{text,V,qrange(U0)}]}|
	     make_query_1(Units, Vals)]
    end;
make_query_1([], []) -> [].

qstr(distance) -> "Dx";
qstr(dx) -> "Dx";
qstr(dy) -> "Dy";
qstr(dz) -> "Dz";
qstr(falloff) -> "R";
qstr(angle) -> "A";
qstr(number) -> "N";
qstr(Atom) -> atom_to_list(Atom).

qrange({_,{_,_}=Range}) -> [{range,Range}];
qrange(_) -> [].
    
make_move(Move, #drag{unit=Units}) ->
    make_move_1(Units, Move).

make_move_1([{percent,_}=Unit|Units], [V|Vals]) ->
    [clamp(Unit, V/100)|make_move_1(Units, Vals)];
make_move_1([percent|Units], [V|Vals]) ->
    [V/100|make_move_1(Units, Vals)];
make_move_1([{U,{_Min,_Max}}=Unit|Units], [V|Vals]) ->
    make_move_1([U|Units], [clamp(Unit, V)|Vals]);
make_move_1([_U|Units], [V|Vals]) ->
    [float(V)|make_move_1(Units, Vals)];
make_move_1([], []) -> [].

magnet_radius(_Sign, #drag{falloff=none}=Drag) -> Drag;
magnet_radius(Sign, #drag{falloff=Falloff0}=Drag0) ->
    case Falloff0+Sign*?GROUND_GRID_SIZE/10 of
	Falloff when Falloff > 0 ->
	    Drag = Drag0#drag{falloff=Falloff},
	    parameter_update(new_falloff, Falloff, Drag);
	_Falloff -> Drag0
    end.

view_changed(#drag{flags=Flags}=Drag0) ->
    help_message(Drag0),
    case member(screen_relative, Flags) of
	false -> Drag0;
	true ->
	    wings_draw_util:map(fun view_changed_fun/2, []),
	    {_,X,Y} = sdl_mouse:getMouseState(),
	    Drag0#drag{x=X,y=Y,xs=0,ys=0,zs=0}
    end.

view_changed_fun(#dlo{drag={matrix,Tr,_,_},transparent=#we{}=We}=D, _) ->
    Id = e3d_mat:identity(),
    {D#dlo{src_we=We,drag={matrix,Tr,Id,e3d_mat:expand(Id)}},[]};
view_changed_fun(#dlo{drag={matrix,Tr,_,Mtx}}=D, _) ->
    {D#dlo{drag={matrix,Tr,e3d_mat:compress(Mtx),Mtx}},[]};
view_changed_fun(#dlo{drag=#do{funs=Tv0}=Do,src_we=We}=D, _) ->
    Tv = update_tvs(Tv0, We, []),
    {D#dlo{drag=Do#do{funs=Tv}},[]};
view_changed_fun(D, _) -> {D,[]}.

update_tvs([F0|Fs], NewWe, Acc) ->
    F = F0(view_changed, NewWe),
    update_tvs(Fs, NewWe, [F|Acc]);
update_tvs([], _, Acc) -> reverse(Acc).

motion(Event, Drag0) ->
    {Move,Drag1} = mouse_translate(Event, Drag0),
    Drag = motion_update(Move, Drag1),
    {Move,Drag}.

mouse_translate(Event0, Drag0) ->
    Mode = wings_pref:get_value(camera_mode),
    {Event,Mod} = mouse_pre_translate(Mode, wings_wm:me_modifiers(), Event0),
    {Ds0,Drag} = mouse_range(Event, Drag0),
    Ds = add_offset(Ds0, Drag),
    Move = constrain(Ds, Mod, Drag),
    {Move,Drag}.

add_offset(Ds, #drag{offset=Offset}) ->
    add_offset_1(Ds, Offset).

add_offset_1([D|Ds], [O|Ofs]) ->
    [D+O|add_offset_1(Ds, Ofs)];
add_offset_1([], _) -> [].

mouse_pre_translate(_, Mod, #mousemotion{state=Mask}=Ev) ->
    if
	Mask band ?SDL_BUTTON_RMASK =/= 0,
	Mod band ?CTRL_BITS =/= 0 ->
	    {Ev#mousemotion{state=?SDL_BUTTON_MMASK},Mod band (bnot ?CTRL_BITS)};
	true -> {Ev,Mod}
    end;
mouse_pre_translate(blender, Mod, Ev) -> {Ev,Mod};
mouse_pre_translate(_, Mod, Ev) -> {Ev,Mod}.

mouse_range(#mousemotion{x=X0,y=Y0,state=Mask},
	    #drag{x=OX,y=OY,xs=Xs0,ys=Ys0,zs=Zs0,
		  xt=Xt0,yt=Yt0,mmb_count=Count0}=Drag) ->
    %%io:format("Mouse Range ~p ~p~n", [{X0,Y0}, {OX,OY,Xs0,Ys0}]),
    {X,Y} = wings_wm:local2global(X0, Y0),
    XD0 = (X - OX),
    YD0 = (Y - OY),
    case {XD0,YD0} of
	{0,0} ->
	    {[Xs0/?MOUSE_DIVIDER,-Ys0/?MOUSE_DIVIDER,-Zs0/?MOUSE_DIVIDER],
	     Drag#drag{xt=0,yt=0}};
	_ ->
	    XD = XD0 + Xt0,
	    YD = YD0 + Yt0,
	    if
		Mask band ?SDL_BUTTON_MMASK =/= 0 ->
		    Xs = Xs0,
		    Ys = Ys0,
		    Zs = case wings_pref:get_value(camera_mode) of
			     maya -> Zs0 - XD;	%Horizontal motion
			     _ -> Zs0 + YD	%Vertical motion
			 end,
		    Count = Count0 + 1;
		true ->
		    Xs = Xs0 + XD,
		    Ys = Ys0 + YD,
		    Zs = Zs0,
		    Count = Count0
	    end,
	    wings_io:warp(OX, OY),
	    {[Xs/?MOUSE_DIVIDER,-Ys/?MOUSE_DIVIDER,-Zs/?MOUSE_DIVIDER],
	     Drag#drag{xs=Xs,ys=Ys,zs=Zs,xt=XD0,yt=YD0,mmb_count=Count}}
    end.

constrain(Ds0, Mod, #drag{unit=Unit}=Drag) ->
    Ds = constrain_0(Unit, Ds0, Mod, []),
    constrain_1(Unit, Ds, Drag).

constrain_0([U0|Us], [D0|Ds], Mod, Acc) ->
    U = clean_unit(U0),
    D = case constraint_factor(U, Mod) of
	    none -> D0;
	    {F1,F2} -> trunc(D0*F1)*F2
	end,
    constrain_0(Us, Ds, Mod, [D|Acc]);
constrain_0([_|_], [], _, Acc) -> reverse(Acc);
constrain_0([], Ds, _, Acc) -> reverse(Acc, Ds).

constrain_1([falloff], _, #drag{falloff=Falloff}) ->
    [Falloff];
constrain_1([U|Us], [D|Ds], Drag) ->
    [clamp(U, D)|constrain_1(Us, Ds, Drag)];
constrain_1([], _, _) -> [].

clamp({_,{Min,_Max}}, D) when D < Min -> Min;
clamp({_,{_Min,Max}}, D) when D > Max -> Max;
clamp(_, D) -> D.

constraint_factor(angle, Mod) ->
    if
	Mod band ?SHIFT_BITS =/= 0,
	Mod band ?CTRL_BITS =/= 0 -> {150,1/15};
	Mod band ?CTRL_BITS =/= 0 -> {15,1.0};
	Mod band ?SHIFT_BITS =/= 0 -> {1,15.0};
	true -> {15.0E5,1.0E-5}
    end;
constraint_factor(number, Mod) ->
    if
	Mod band ?SHIFT_BITS =/= 0,
	Mod band ?CTRL_BITS =/= 0 -> {200,2/10};
	Mod band ?CTRL_BITS =/= 0 -> {20,2.0};
	Mod band ?SHIFT_BITS =/= 0 -> {2,20.0};
	true -> {2.0E6,2.0E-6}
    end;
constraint_factor(_, Mod) ->
    if
	Mod band ?SHIFT_BITS =/= 0,
	Mod band ?CTRL_BITS =/= 0 -> {100,1/100};
	Mod band ?CTRL_BITS =/= 0 -> {10,1/10};
	Mod band ?SHIFT_BITS =/= 0 -> {1,1.0};
	true -> none
    end.

%%%
%%% Update selection for new mouse position.
%%%

motion_update(Move, Drag0) ->
    Drag = progress(Move, Drag0),
    wings_draw_util:map(fun(D, _) ->
				   motion_update_fun(D, Move)
			   end, []),
    Drag.

motion_update_fun(#dlo{src_we=We,drag={matrix,Tr,Mtx0,_}}=D, Move) when ?IS_LIGHT(We) ->
    Mtx = Tr(Mtx0, Move),
    wings_light:update_matrix(D, Mtx);
motion_update_fun(#dlo{drag={matrix,Trans,Matrix0,_}}=D, Move) ->
    Matrix = e3d_mat:expand(Trans(Matrix0, Move)),
    D#dlo{drag={matrix,Trans,Matrix0,Matrix}};
motion_update_fun(#dlo{drag={general,Fun}}=D, Move) ->
    Fun(Move, D);
motion_update_fun(#dlo{drag=#do{funs=Tv}}=D, Move) ->
    Vtab = foldl(fun(F, A) -> F(Move, A) end, [], Tv),
    wings_draw:update_dynamic(D, Vtab);
motion_update_fun(D, _) -> D.

parameter_update(Key, Val, Drag0) ->
    wings_draw_util:map(fun(D, _) ->
				parameter_update_fun(D, Key, Val)
			end, []),
    {_,X,Y} = sdl_mouse:getMouseState(),
    Ev = #mousemotion{x=X,y=Y,state=0},
    {_,Drag} = motion(Ev, Drag0),
    Drag.

parameter_update_fun(#dlo{drag=#do{funs=Tv0}=Do}=D, Key, Val) ->
    Tv = foldl(fun(F, A) -> [F(Key, Val)|A] end, [], Tv0),
    D#dlo{drag=Do#do{funs=Tv}};
parameter_update_fun(D, _, _) -> D.

translate({Xt0,Yt0,Zt0}, Dx, VsPos, Acc) ->
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    foldl(fun({V,{X,Y,Z}}, A) -> 
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  [{V,Pos}|A]
	  end, Acc, VsPos).

progress(Move, #drag{unit=Units}=Drag) ->
    Msg0 = progress_units(Units, Move),
    Msg = reverse(trim(reverse(lists:flatten(Msg0)))),
    Drag#drag{info=Msg}.

progress_units([Unit|Units], [N|Ns]) ->
    [unit(clean_unit(Unit), N)|progress_units(Units, Ns)];
progress_units([], []) -> [].

clean_unit({Unit,_}) when is_atom(Unit) -> Unit;
clean_unit(Unit) when is_atom(Unit) -> Unit.
    
unit(angle, A) ->
    trim(io_lib:format("~10.2f~c  ", [A,?DEGREE]));
unit(number, N) ->
    ["N: "|trim(io_lib:format("~10.2f  ", [N]))];
unit(distance, D) ->
    ["D: "|trim(io_lib:format("~10.2f  ", [D]))];
unit(dx, D) ->
    ["DX: "|trim(io_lib:format("~10.2f  ", [D]))];
unit(dy, D) ->
    ["DY: "|trim(io_lib:format("~10.2f  ", [D]))];
unit(dz, D) ->
    ["DZ: "|trim(io_lib:format("~10.2f  ", [D]))];
unit(percent, P) ->
    trim(io_lib:format("~.2f%  ", [P*100.0]));
unit(falloff, R) ->
    ["R: "|trim(io_lib:format("~10.2f", [R]))];
unit(Unit, Move) ->
    io:format("~p\n", [{Unit,Move}]),
    [].

trim([$\s|T]) -> trim(T);
trim([[_|_]=H|T]) ->
    case trim(H) of
	[] -> trim(T);
	S -> [S|T]
    end;
trim(S) -> S.
    
normalize(#drag{magnet=none}=Drag) ->
    normalize_1(Drag);
normalize(#drag{magnet=Type}=Drag) ->
    wings_pref:set_value(magnet_type, Type),
    normalize_1(Drag).

normalize_1(#drag{st=#st{shapes=Shs0}=St}) ->
    Shs = wings_draw_util:map(fun normalize_fun/2, Shs0),
    St#st{shapes=Shs}.

normalize_fun(#dlo{drag=none}=D, Shs) -> {D,Shs};
normalize_fun(#dlo{drag={matrix,_,_,_},transparent=#we{id=Id}=We}=D, Shs0) when ?IS_LIGHT(We) ->
    Shs = gb_trees:update(Id, We, Shs0),
    {D#dlo{work=none,drag=none,src_we=We,transparent=false},Shs};
normalize_fun(#dlo{drag={matrix,_,_,Matrix},
		   src_we=#we{id=Id,mirror=M}=We0}=D, Shs0) ->
    We = wings_we:transform_vs(Matrix, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    {D#dlo{work=none,sel=none,drag=none,src_we=We,mirror=M},Shs};
normalize_fun(#dlo{drag={general,_},src_we=#we{id=Id}=We}=D, Shs) ->
    {D#dlo{drag=none,sel=none,src_we=We},gb_trees:update(Id, We, Shs)};
normalize_fun(#dlo{src_we=#we{id=Id,vp=Vtab0}}=D, Shs) ->
    #we{vp=OldVtab}= We0 = gb_trees:get(Id, Shs),
    %% Heuristic for break-even. (Note that we don't know the exact number
    %% of vertices that will be updated.)
    Break = round(16*math:log(gb_trees:size(OldVtab))/math:log(2)+0.5),
    Vtab = case gb_trees:size(Vtab0) of
	       Sz when Sz =< Break ->
		   %% Update the gb_tree to allow sharing with the undo list.
		   Vt = norm_update(Vtab0, OldVtab),
%  		   io:format("cmp: ~p% \n", [round(100*cmp(Vt, OldVtab)/
%  						   gb_trees:size(OldVtab))]),
		   Vt;
	       _Sz ->
		   %% Too much updated - faster to rebuild the gb_tree.
		   %% (There would not have been much sharing anyway.)
		   norm_rebuild(Vtab0, OldVtab)
	   end,
%     io:format("~p ~p\n", [erts_debug:size([OldVtab,Vtab]),
% 			   erts_debug:flat_size([OldVtab,Vtab])]),
    We = We0#we{vp=Vtab},
    {D#dlo{drag=none,sel=none,src_we=We},gb_trees:update(Id, We, Shs)}.

norm_update(New, Old) ->
    norm_update(gb_trees:to_list(New), gb_trees:to_list(Old), Old).

norm_update([Same|New], [Same|Old], Acc) ->
    norm_update(New, Old, Acc);
norm_update([{V,P0}|New], [{V,OldP}|Old], Acc) ->
    P = tricky_share(P0, OldP),
    norm_update(New, Old, gb_trees:update(V, P, Acc));
norm_update(New, [_|Old], Acc) ->
    norm_update(New, Old, Acc);
norm_update([], _, Acc) -> Acc.

norm_rebuild(New, Old) ->
    norm_rebuild(gb_trees:to_list(New), gb_trees:to_list(Old), []).

norm_rebuild([N|New], [O|Old], Acc) when N =:= O ->
    norm_rebuild(New, Old, [O|Acc]);
norm_rebuild([{V,P0}|New], [{V,OldP}|Old], Acc) ->
    P = tricky_share(P0, OldP),
    norm_rebuild(New, Old, [{V,P}|Acc]);
norm_rebuild(New, [O|Old], Acc) ->
    norm_rebuild(New, Old, [O|Acc]);
norm_rebuild([], Old, Acc) ->
    gb_trees:from_orddict(reverse(Acc, Old)).

%% What do you think about this?
tricky_share({X,Y,Z}=New, {OldX,OldY,OldZ})
  when X =/= OldX, Y =/= OldY, Z =/= OldZ -> New;
tricky_share({X,Y,Z}, {X,Y,_}=Old) ->
    setelement(3, Old, Z);
tricky_share({X,Y,Z}, {X,_,Z}=Old) ->
    setelement(2, Old, Y);
tricky_share({X,Y,Z}, {_,Y,Z}=Old) ->
    setelement(1, Old, X);
tricky_share({X,Y,Z}, {X,_,_}=Old) ->
    {element(1, Old),Y,Z};
tricky_share({X,Y,Z}, {_,Y,_}=Old) ->
    {X,element(2, Old),Z};
tricky_share({X,Y,Z}, {_,_,Z}=Old) ->
    {X,Y,element(3, Old)}.

% cmp({S,New}, {S,Old}) ->
%     cmp(New, Old, 0).

% cmp({_,_,NewSmaller,NewBigger}=New, {_,_,OldSmaller,OldBigger}=Old, N0) ->
%     N1 = case erts_debug:same(New, Old) of
% 	     false -> N0;
% 	     true -> N0+1
% 	 end,
%     N = cmp(NewSmaller, OldSmaller, N1),
%     cmp(NewBigger, OldBigger, N);
% cmp(nil, nil, N) -> N.
    
%%%
%%% Redrawing while dragging.
%%%

redraw(#drag{info=Info,st=St}) ->
    clear_sel_dlists(),
    wings_draw:update_sel_dlist(),
    wings_draw_util:render(St),
    wings_io:info(Info),
    wings_wm:current_state(St).

clear_sel_dlists() ->
    wings_draw_util:map(fun clear_sel_dlists/2, []).

clear_sel_dlists(#dlo{drag=none}=D, _) -> D;
clear_sel_dlists(#dlo{drag={matrix,_,_}}=D, _) -> D;
clear_sel_dlists(D, _) -> D#dlo{sel=none}.
