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
%%     $Id: wings_drag.erl,v 1.77 2002/05/11 08:47:50 bjorng Exp $
%%

-module(wings_drag).
-export([setup/3,setup/4,do_drag/1,translate/4]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,map/2,foldl/3,sort/1,keysort/2,
		reverse/1,reverse/2,concat/1,member/2]).

-record(drag,
	{x,					%Original 2D position
	 y,					
	 xs=0,                                  %Summary of mouse movements
	 ys=0,
	 xt=0,                                  %Last warp length
	 yt=0,
	 unit,					%Unit that drag is done in.
	 flags=[],				%Flags.
	 new=true,				%New objects.
	 falloff,				%Magnet falloff.
	 magnet,				%Magnet: true|false.
	 st					%Saved st record.
	}).

-record(dlist,					%Display list.
	{faces,
	 edges}).

setup(Tvs, Unit, St) ->
    setup(Tvs, Unit, [], St).

setup(Tvs, Unit, Flags, St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    Magnet = property_lists:get_value(magnet, Flags, none),
    Drag = #drag{x=X,y=Y,unit=Unit,flags=Flags,
		 falloff=falloff(Unit),magnet=Magnet},
    init_1(Tvs, Drag, St).

falloff([falloff|_]) -> 1.0;
falloff([_|T]) -> falloff(T);
falloff([]) -> none.
	    
init_1({matrix,Tvs0}, Drag0, St) ->
    wings_draw:update_dlists(St),
    insert_matrix(Tvs0),
    Drag = Drag0#drag{st=St},
    wings_io:grab(),
    {drag,Drag};
init_1(Tvs0, Drag0, St) ->
    wings_draw:update_dlists(St),
    S = sofs:relation(Tvs0, [{id,info}]),
    F = sofs:relation_to_family(S),
    Tvs = sofs:to_external(F),
    break_apart(Tvs),
    Drag = Drag0#drag{st=St},
    wings_io:grab(),
    {drag,Drag}.

%%
%% Here we break apart the objects into two parts - static part
%% (not moved during drag) and dynamic (part of objects actually
%% moved).
%%
break_apart(Tvs) ->
    wings_draw_util:update(fun break_apart/2, Tvs).

break_apart(eol, []) -> eol;
break_apart(#dlo{wire=W,mirror=M,src_sel=Sel,src_we=#we{id=Id}=We0},
	    [{Id,TvList}|Tvs]) ->
    {Vs0,FunList} = combine_tvs(TvList, We0),
    Vs = sofs:set(Vs0, [vertex]),
    #we{es=Etab0,fs=Ftab0,vs=Vtab0} = We0,
    Etab1 = foldl(fun(#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}, A) ->
			 [{Va,Lf},{Va,Rf},{Vb,Lf},{Vb,Rf}|A]
		 end, [], gb_trees:values(Etab0)),
    Etab2 = sofs:relation(Etab1, [{vertex,face}]),
    Faces = sofs:image(Etab2, Vs),
    Ftab1 = sofs:relation(gb_trees:to_list(Ftab0), [{face,data}]),

    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    FtabStatic0 = sofs:drestriction(Ftab1, Faces),
    FtabStatic = sofs:to_external(FtabStatic0),
    WeStatic = We0#we{fs=gb_trees:from_orddict(FtabStatic)},
    draw_faces(WeStatic),
    gl:endList(),

    FtabDyn0 = sofs:restriction(Ftab1, Faces),
    FtabDyn1 = sofs:to_external(FtabDyn0),
    FtabDyn = dyn_faces(FtabDyn1, We0),
    AllVs = sofs:image(sofs:converse(Etab2), Faces),
    WeDyn = We0#we{fs=gb_trees:from_orddict(FtabDyn1),first_id=FtabDyn,
		   vs=undefined},
    StaticVs0 = sofs:to_external(sofs:difference(AllVs, Vs)),
    StaticVs = reverse(insert_vtx_data_1(StaticVs0, Vtab0, [])),
    Dyn = {FunList,StaticVs},
    {#dlo{work=[List],wire=W,mirror=M,
	  src_sel=Sel,src_we=WeDyn,drag=Dyn},Tvs};
break_apart(D, Tvs) -> {D,Tvs}.

dyn_faces(Flist, We) ->
    foldl(fun({Face,#face{edge=Edge}}, A) ->
		  Vs = wings_face:surrounding_vertices(Face, Edge, We),
		  [Vs|A]
	  end, [], Flist).

combine_tvs(TvList, #we{vs=Vtab}) ->
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

insert_matrix(Tvs) ->
    Id = e3d_mat:identity(),
    wings_draw_util:update(fun(D, Data) ->
				   insert_matrix_fun(D, Data, Id)
			   end, sort(Tvs)).

insert_matrix_fun(eol, _, _) -> eol;
insert_matrix_fun(#dlo{work=Work,sel=Sel,wire=W,mirror=M,
		       src_sel=SrcSel,src_we=#we{id=Id}=We},
		  [{Id,Tr}|Tvs], Matrix) ->
    {#dlo{drag={matrix,Tr,Matrix},work={matrix,Matrix,Work},wire=W,mirror=M,
	  sel={matrix,Matrix,Sel},src_we=We,src_sel=SrcSel},Tvs};
insert_matrix_fun(D, Tvs, _) -> {D,Tvs}.

%%%
%%% Handling of drag events.
%%%

do_drag(Drag) ->
    wings_io:message_right(drag_help(Drag)),
    {seq,{push,dummy},get_drag_event_1(Drag)}.

drag_help(#drag{magnet=none,falloff=Falloff}) ->
    Help = "[Tab] Numeric entry  [Shift] and/or [Ctrl] Constrain",
    case Falloff of
	none -> Help;
	_ -> "[+] or [-] Tweak R  " ++ Help
    end;
drag_help(#drag{magnet=Type}) -> wings_magnet:drag_help(Type).

get_drag_event(Drag) ->
    wings_wm:dirty(),
    get_drag_event_1(Drag).

get_drag_event_1(Drag) ->
    {replace,fun(Ev) -> handle_drag_event(Ev, Drag) end}.

handle_drag_event(#keyboard{keysym=#keysym{sym=9}}, Drag) ->
    numeric_input(Drag);
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
	    Help = wings_magnet:drag_help(Type),
	    wings_io:message_right(Help),
	    Val = {Type,Drag0#drag.falloff},
	    Drag = parameter_update(new_type, Val, Drag0#drag{magnet=Type}),
	    get_drag_event(Drag)
    end;
handle_drag_event_0(Ev, Drag) -> handle_drag_event_1(Ev, Drag).

handle_drag_event_1(redraw, Drag) ->
    redraw(Drag),
    get_drag_event_1(Drag);
handle_drag_event_1(#mousemotion{x=X,y=Y}, Drag0) ->
    {Dx0,Dy0,Drag1} = mouse_range(X, Y, Drag0),
    Move = constrain(Dx0, Dy0, Drag1),
    Drag = motion_update(Move, Drag1),
    get_drag_event(Drag);
handle_drag_event_1(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
		    Drag0) ->
    wings_io:ungrab(),
    {Drag,Move} = ?SLOW(motion(X, Y, Drag0)),
    St = normalize(Drag),
    DragEnded = {new_state,St#st{args=Move}},
    wings_io:putback_event(DragEnded),
    pop;
handle_drag_event_1({drag_arguments,Move}, Drag0) ->
    wings_io:ungrab(),
    Drag = ?SLOW(motion_update(Move, Drag0)),
    St = normalize(Drag),
    DragEnded = {new_state,St#st{args=Move}},
    wings_io:putback_event(DragEnded),
    pop;
handle_drag_event_1(#mousebutton{button=3,state=?SDL_RELEASED}, _Drag) ->
    wings_io:ungrab(),
    wings_io:clear_message(),
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

numeric_input(Drag0) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    {Dx0,Dy0,Drag} = mouse_range(X, Y, Drag0),
    Move0 = constrain(Dx0, Dy0, Drag),
    Redraw = fun() -> redraw(Drag) end,
    wings_ask:ask(make_query(Move0, Drag), Redraw,
		  fun(Res) ->
			  {numeric_input,make_move(Res, Drag)}
		  end).

make_query(Move, #drag{unit=Units}) ->
    make_query_1(Units, Move).

make_query_1([{percent,{_Min,_Max}}|Units], [V|Vals]) ->
    [{"P",V*100.0}|make_query_1(Units, Vals)];
make_query_1([percent|Units], [V|Vals]) ->
    [{"P",V*100.0}|make_query_1(Units, Vals)];
make_query_1([{U,{_Min,_Max}}|Units], [V|Vals]) ->
    [{qstr(U),V}|make_query_1(Units, Vals)];
make_query_1([U|Units], [V|Vals]) ->
    [{qstr(U),V}|make_query_1(Units, Vals)];
make_query_1([], []) -> [].

qstr(distance) -> "Dx";
qstr(dx) -> "Dx";
qstr(dy) -> "Dy";
qstr(falloff) -> "R";
qstr(angle) -> "A";
qstr(percent) -> "P";
qstr(Atom) -> atom_to_list(Atom).

make_move(Move, #drag{unit=Units}) ->
    make_move_1(Units, Move).

make_move_1([{percent,_}=Unit|Units], [V|Vals]) ->
    [constrain_range(Unit, V/100)|make_move_1(Units, Vals)];
make_move_1([percent|Units], [V|Vals]) ->
    [V/100|make_move_1(Units, Vals)];
make_move_1([{U,{_Min,_Max}}=Unit|Units], [V|Vals]) ->
    make_move_1([U|Units], [constrain_range(Unit, V)|Vals]);
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

view_changed(#drag{new=true,x=X,y=Y}=Drag0) ->
    {Drag,_} = motion(X, Y, Drag0),
    view_changed(Drag);
view_changed(#drag{flags=Flags}=Drag0) ->
    wings_io:message_right(drag_help(Drag0)),
    case member(screen_relative, Flags) of
	false -> Drag0;
	true ->
	    wings_draw_util:update(fun view_changed_fun/2, []),
	    {_,X,Y} = sdl_mouse:getMouseState(),
	    Drag0#drag{x=X,y=Y,xs=0,ys=0}
    end.

view_changed_fun(eol, _) -> eol;
view_changed_fun(#dlo{work={matrix,Mtx,_},drag={matrix,Tr,_}}=D, _) ->
    {D#dlo{drag={matrix,Tr,e3d_mat:compress(Mtx)}},[]};
view_changed_fun(#dlo{drag={Tv0,StaticVs},src_we=We}=D, _) ->
    Tv = update_tvs(Tv0, We, []),
    {D#dlo{drag={Tv,StaticVs}},[]};
view_changed_fun(D, _) -> {D,[]}.

update_tvs([F0|Fs], NewWe, Acc) ->
    F = F0(view_changed, NewWe),
    update_tvs(Fs, NewWe, [F|Acc]);
update_tvs([], _, Acc) -> reverse(Acc).
    
motion(X, Y, Drag0) ->
    {Dx0,Dy0,Drag} = mouse_range(X, Y, Drag0),
    Move = constrain(Dx0, Dy0, Drag),
    {motion_update(Move, Drag),Move}.

mouse_range(X0, Y0, #drag{x=OX,y=OY,xs=Xs0,ys=Ys0, xt=Xt0, yt=Yt0}=Drag) ->
    %%io:format("Mouse Range ~p ~p~n", [{X0,Y0}, {OX,OY,Xs0,Ys0}]),
    XD0 = (X0 - OX),
    YD0 = (Y0 - OY),
    case {XD0,YD0} of
	{0,0} ->
	    {Xs0/?MOUSE_DIVIDER, -Ys0/?MOUSE_DIVIDER, Drag#drag{xt=0,yt=0}};
	_ ->
	    XD = XD0 + Xt0,
	    YD = YD0 + Yt0,
	    Xs = Xs0 + XD,
	    Ys = Ys0 + YD,
	    wings_io:warp(OX, OY),
	    {Xs/?MOUSE_DIVIDER,-Ys/?MOUSE_DIVIDER,
	     Drag#drag{xs=Xs,ys=Ys,xt=XD0, yt=YD0}}
    end.

constrain(Dx0, _Dy, #drag{unit=[angle|_]}=Drag) ->
    Dx = case sdl_keyboard:getModState() of
	     Mod when Mod band ?SHIFT_BITS =/= 0,
		      Mod band ?CTRL_BITS =/= 0 ->
		 trunc(150*Dx0)/150;
	     Mod when Mod band ?CTRL_BITS =/= 0 ->
		 trunc(15*Dx0)/15;
	     Mod when Mod band ?SHIFT_BITS =/= 0 ->
		 float(trunc(Dx0));
	     _Mod -> Dx0
	 end,
    [15*Dx|maybe_falloff(Drag)];
constrain(Dx0, Dy0, #drag{unit=Unit}=Drag) ->
    {Dx,Dy} = case sdl_keyboard:getModState() of
		  Mod when Mod band ?SHIFT_BITS =/= 0,
			   Mod band ?CTRL_BITS =/= 0 ->
		      D = 100.0,
		      {trunc(D*Dx0)/D,trunc(D*Dy0)/D};
		  Mod when Mod band ?CTRL_BITS =/= 0 ->
		      D = 10.0,
		      {trunc(D*Dx0)/D,trunc(D*Dy0)/D};
		  Mod when Mod band ?SHIFT_BITS =/= 0 ->
		      {float(trunc(Dx0)),float(trunc(Dy0))};
		  _Mod -> {Dx0,Dy0}
	      end,
    constrain_1(Unit, Dx, Dy, Drag).

constrain_1([U], Dx, _Dy, _Drag) ->
    [constrain_range(U, Dx)];
constrain_1([U,falloff], Dx, _Dy, #drag{falloff=Falloff}) ->
    [constrain_range(U, Dx),Falloff];
constrain_1([U1,U2,falloff], Dx, Dy, #drag{falloff=Falloff}) ->
    [constrain_range(U1, Dx),constrain_range(U2, Dy),Falloff];
constrain_1([U1,U2], Dx, Dy, _Drag) ->
    [constrain_range(U1, Dx),constrain_range(U2, Dy)].

constrain_range({_,{Min,_Max}}, D) when D < Min -> Min;
constrain_range({_,{_Min,Max}}, D) when D > Max -> Max;
constrain_range(_, D) -> D.

maybe_falloff(#drag{unit=[_,falloff],falloff=Falloff}) -> [Falloff];
maybe_falloff(_) -> [].

%%%
%%% Update selection for new mouse position.
%%%

motion_update(Move, Drag) ->
    progress(Move, Drag),
    wings_draw_util:update(fun(D, _) ->
				   motion_update_fun(D, Move)
			   end, []),
    Drag#drag{new=false}.

motion_update_fun(eol, _) -> eol;
motion_update_fun(#dlo{work={matrix,_,Work},sel={matrix,_,Sel},
		       drag={matrix,Trans,Matrix0}}=D, Move) ->
    Matrix = e3d_mat:expand(Trans(Matrix0, Move)),
    D#dlo{work={matrix,Matrix,Work},sel={matrix,Matrix,Sel}};
motion_update_fun(#dlo{work=[Work|_],src_we=We0,
		       drag={Tv,StaticVs}}=D, Move) ->
    Vtab0 = foldl(fun(F, A) -> F(Move, A) end, StaticVs, Tv),
    Vtab = gb_trees:from_orddict(sort(Vtab0)),
    We = We0#we{vs=Vtab},
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    draw_flat_faces(We),
    gl:endList(),
    D#dlo{work=[Work,List],src_we=We};
motion_update_fun(D, _) -> D.

parameter_update(Key, Val, Drag0) ->
    wings_draw_util:update(fun(D, _) ->
				   parameter_update_fun(D, Key, Val)
			   end, []),
    {_,X,Y} = sdl_mouse:getMouseState(),
    {Drag,_} = motion(X, Y, Drag0),
    Drag.

parameter_update_fun(eol, _, _) -> eol;
parameter_update_fun(#dlo{drag={Tv0,StaticVs}}=D, Key, Val) ->
    Tv = foldl(fun(F, A) -> [F(Key, Val)|A] end, [], Tv0),
    {D#dlo{drag={Tv,StaticVs}},[]};
parameter_update_fun(D, _, _) -> {D,[]}.

translate({Xt0,Yt0,Zt0}, Dx, VsPos, Acc) ->
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    foldl(fun({V,#vtx{pos={X,Y,Z}}=Vtx}, A) -> 
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  [{V,Vtx#vtx{pos=Pos}}|A]
	  end, Acc, VsPos).

progress(Move, #drag{unit=Units}) ->
    Msg = progress_units(Units, Move),
    progress_1(reverse(lists:flatten(Msg))).

progress_1([$\s|Str]) -> progress_1(Str);
progress_1(Str) -> wings_io:message(reverse(Str)).

progress_units([{Unit,_}|Units], [N|Ns]) ->
    [unit(Unit, N)|progress_units(Units, Ns)];
progress_units([Unit|Units], [N|Ns]) ->
    [unit(Unit, N)|progress_units(Units, Ns)];
progress_units([], []) -> [].
    
unit(angle, A) ->
    io_lib:format("A: ~-10.2f", [A]);
unit(distance, D) ->
    io_lib:format("D: ~-10.2f", [D]);
unit(dx, D) ->
    io_lib:format("DX: ~-10.2f", [D]);
unit(dy, D) ->
    io_lib:format("DY: ~-10.2f", [D]);
unit(percent, P) ->
    ["P: ",io_lib:format("~.2f%  ", [P*100.0])];
unit(falloff, R) ->
    io_lib:format("R: ~-10.2f", [R]);
unit(Unit, Move) ->
    io:format("~p\n", [{Unit,Move}]),
    [].

normalize(#drag{magnet=none}=Drag) ->
    normalize_1(Drag);
normalize(#drag{magnet=Type}=Drag) ->
    wings_pref:set_value(magnet_type, Type),
    normalize_1(Drag).

normalize_1(#drag{st=#st{shapes=Shs0}=St}) ->
    Shs = wings_draw_util:update(fun normalize_fun/2, Shs0),
    St#st{shapes=Shs}.

normalize_fun(eol, _) -> eol;
normalize_fun(#dlo{drag=none}=D, Shs) -> {D,Shs};
normalize_fun(#dlo{work={matrix,Matrix,_},src_we=#we{id=Id}=We0}=D, Shs0) ->
    We = wings_we:transform_vs(Matrix, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    case Matrix of
	{1.0,_,_,_,_,1.0,_,_,_,_,1.0,_,_,_,_,_} ->
	    %% Keep the display list.
	    {D#dlo{drag=none,src_we=We},Shs};
	_NotSafe ->
	    %% Normals could have been scaled. Must reubild
	    %% the display list.
	    {D#dlo{work=none,drag=none,src_we=We},Shs}
    end;
normalize_fun(#dlo{src_we=#we{id=Id,vs=Vtab0}}=D, Shs) ->
    #we{vs=OldVtab0}= We0 = gb_trees:get(Id, Shs),
    Vtab1 = norm_update(gb_trees:to_list(Vtab0), gb_trees:to_list(OldVtab0)),
    Vtab = gb_trees:from_orddict(Vtab1),
    We = We0#we{vs=Vtab},
    {D#dlo{drag=none,src_we=We},gb_trees:update(Id, We, Shs)}.

norm_update(New, Old) ->
    norm_update(New, Old, []).

norm_update([{V,_}=N|New], [{V,_}|Old], Acc) ->
    norm_update(New, Old, [N|Acc]);
norm_update(New, [O|Old], Acc) ->
    norm_update(New, Old, [O|Acc]);
norm_update([], Old, Acc) ->
    reverse(Acc, Old).

%%%
%%% Redrawing while dragging.
%%%

redraw(#drag{st=St}) ->
    wings_draw_util:update(fun clear_sel_dlists/2, []),
    wings_draw:update_sel_dlist(),
    wings_draw_util:render(St),
    wings_io:update(St).

clear_sel_dlists(eol, _) -> eol;
clear_sel_dlists(#dlo{drag=none}=D, _) -> {D,[]};
clear_sel_dlists(#dlo{drag={matrix,_,_}}=D, _) -> {D,[]};
clear_sel_dlists(D, _) -> {D#dlo{sel=none},[]}.

draw_faces(#we{fs=Ftab}=We) ->
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {1.0,1.0,1.0}),
    wings_draw_util:begin_end(
      fun() ->
	      foreach(fun({Face,#face{edge=Edge}}) ->
			      wings_draw_util:face(Face, Edge, We)
		      end, gb_trees:to_list(Ftab))
      end).

draw_flat_faces(#we{first_id=Flist,vs=Vtab}) ->
    wings_draw_util:begin_end(
      fun() ->
	      Tess = wings_draw_util:tess(),
	      draw_flat_faces_1(Tess, Flist, Vtab)
      end).

draw_flat_faces_1(Tess, [Vs|T], Vtab) ->
    VsPos = [pos(V, Vtab) || V <- Vs],
    {X,Y,Z} = N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_flat_face(Tess, VsPos),
    draw_flat_faces_1(Tess, T, Vtab);
draw_flat_faces_1(_, [], _) -> ok.

tess_flat_face(Tess, [P|T]) ->
    glu:tessVertex(Tess, P),
    tess_flat_face(Tess, T);
tess_flat_face(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

pos(Key, Tab) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tab),
    Pos.
