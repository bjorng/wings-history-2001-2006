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
%%     $Id: wings_drag.erl,v 1.61 2002/03/09 22:14:32 bjorng Exp $
%%

-module(wings_drag).
-export([setup/3,setup/4,do_drag/1,translate/4]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(DL_STATIC_FACES, (?DL_DRAW_BASE)).
-define(DL_DYNAMIC_FACES, (?DL_DRAW_BASE+1)).
-define(DL_SEL, (?DL_DRAW_BASE+2)).

-define(DL_DYNAMIC, (?DL_DRAW_BASE+3)).

-import(lists, [foreach/2,map/2,foldl/3,sort/1,keysort/2,
		reverse/1,concat/1,member/2]).

-record(drag,
	{x,					%Original 2D position
	 y,					
	 xs=0,                                  %Summary of mouse movements
	 ys=0,
	 xt=0,                                  %Last warp length
	 yt=0,
	 tvs,					%[{Vertex,Vec}...]
	 unit,					%Unit that drag is done in.
	 flags=[],				%Flags.
	 new,					%New objects.
	 sel,					%Massaged selection.
	 matrices=none,				%Transformation matrices.
	 falloff,				%Magnet falloff.
	 st,					%Saved st record.
	 done=false				%Drag is done.
	}).

-record(dlist,					%Display list.
	{faces,
	 edges}).

setup(Tvs, Unit, St) ->
    setup(Tvs, Unit, [], St).

setup(Tvs, Unit, Flags, St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    Drag = #drag{x=X,y=Y,unit=Unit,flags=Flags,falloff=falloff(Unit, St)},
    init_1(Tvs, Drag, St).

falloff([falloff|_], #st{inf_r=Falloff}) -> Falloff;
falloff([_|T], St) -> falloff(T, St);
falloff([], _St) -> none.
    
init_1(Tvs0, Drag0, #st{selmode=Mode,sel=Sel0}=St0) ->
    St = wings_draw:model_changed(St0),
    wings_draw:make_vec_dlist(St),
    case combine(Tvs0) of
	{matrix,Tv}=Tvs ->
	    Faces = [{Id,matrix} || {Id,Trans,Matrix} <- Tv],
	    Drag = Drag0#drag{sel={Mode,[]},tvs=Tvs,st=St},
	    static_display_list(Faces, St),
	    wings_io:grab(),
	    {drag,Drag};
	Tvs ->
 	    gl:newList(?DL_STATIC_FACES, ?GL_COMPILE),
	    Dyn = break_apart(Tvs, St),
 	    gl:endList(),
	    Sel = {Mode,[{Id,gb_sets:to_list(S)} || {Id,S} <- Sel0]},
	    Drag = Drag0#drag{tvs=Dyn,sel=Sel,st=St},
	    wings_io:grab(),
	    {drag,Drag}
    end.

%% 
%% Mainly an optimisation. We want to combine translatation vectors
%% that happens to be the same. We do some massage of the input.
%%
combine({matrix,Tvs0}) ->
    Ident = e3d_mat:identity(),
    Tvs = [{Id,Trans,Ident} || {Id,Trans} <- Tvs0],
    {matrix,sort(Tvs)};
combine(Tvs) ->
    S = sofs:relation(Tvs),
    F = sofs:relation_to_family(S),
    %% The rest of this function is an optimisation.
    map(fun({Id,[{_,Fun0}|_]=Tv0}) when is_function(Fun0) ->
		Tv = [Fun || {_,Fun} <- Tv0],
		Vs = sort(concat([Vs || {Vs,_} <- Tv0])),
		{Id,{Vs,Tv}};
	   ({Id,L}) ->
		SS = sofs:from_term(L, [[{vec,[vertex]}]]),
		RR = sofs:union(SS),
 		FF = sofs:relation_to_family(RR),
  		FU = sofs:family_union(FF),
		{Id,sofs:to_external(FU)}
	end, sofs:to_external(F)).

%%
%% Here we break apart the objects into two parts - static part
%% (not moved during drag) and dynamic (part of objects actually
%% moved).
%%
break_apart({matrix,_}=Tvs, _St) -> Tvs;	%Specially handled.
break_apart(Tvs, #st{shapes=Shs}) ->
    break_apart(Tvs, gb_trees:to_list(Shs), []).

break_apart([{Id,_}|_]=Tvs, [{ShId,We}|Shs], Dyn) when ShId < Id ->
    draw_faces(We),
    break_apart(Tvs, Shs, Dyn);
break_apart([{Id,Tv}|Tvs], [{Id,We0}|Shs], DynAcc) ->
    #we{es=Etab0,fs=Ftab0,vs=Vtab0} = We0,
    Etab1 = foldl(fun(#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}, A) ->
			 [{Va,Lf},{Va,Rf},{Vb,Lf},{Vb,Rf}|A]
		 end, [], gb_trees:values(Etab0)),
    Etab2 = sofs:relation(Etab1, [{vertex,face}]),
    Vs = tv_vertices(Tv),
    Faces = sofs:image(Etab2, Vs),
    Ftab1 = sofs:relation(gb_trees:to_list(Ftab0), [{face,data}]),
    FtabStatic0 = sofs:drestriction(Ftab1, Faces),
    FtabStatic = sofs:to_external(FtabStatic0),
    WeStatic = We0#we{fs=gb_trees:from_orddict(FtabStatic),
		      first_id=FtabStatic},
    draw_faces(WeStatic),

    FtabDyn0 = sofs:restriction(Ftab1, Faces),
    FtabDyn = sofs:to_external(FtabDyn0),
    AllVs = sofs:image(sofs:converse(Etab2), Faces),
    WeDyn = We0#we{fs=gb_trees:from_orddict(FtabDyn),first_id=FtabDyn,
		   vs=undefined},
    StaticVs0 = sofs:to_external(sofs:difference(AllVs, Vs)),
    StaticVs = reverse(insert_vtx_data_1(StaticVs0, Vtab0, [])),
    Dyn = case Tv of
	      {Trans,FunList} ->
		  {FunList,StaticVs,Id,WeDyn};
	      Trans0 ->
		  Trans1 = keysort(2, Trans0),
		  Trans = insert_vtx_data(Trans1, Vtab0, []),
		  {Trans,StaticVs,Id,WeDyn}
	  end,
    break_apart(Tvs, Shs, [Dyn|DynAcc]);
break_apart([], [{_,We}|Shs], Dyn) ->
    draw_faces(We),
    break_apart([], Shs, Dyn);
break_apart([], [], Dyn) -> Dyn.

tv_vertices({Vs,[Fun|_]}) when is_function(Fun) ->
    sofs:set(Vs, [vertex]);
tv_vertices(Tv) ->
    Vs = foldl(fun({_,Vs}, A) -> [Vs|A] end, [], Tv),
    sofs:union(sofs:from_term(Vs, [[vertex]])).

insert_vtx_data([{Vec,Vs0}|VecVs], Vtab, Acc) ->
    Vs = insert_vtx_data_1(Vs0, Vtab, []),
    insert_vtx_data(VecVs, Vtab, [{Vec,Vs}|Acc]);
insert_vtx_data([], _Vtab, Acc) -> Acc.

insert_vtx_data_1([V|Vs], Vtab, Acc) ->
    insert_vtx_data_1(Vs, Vtab, [{V,gb_trees:get(V, Vtab)}|Acc]);
insert_vtx_data_1([], _Vtab, Acc) -> Acc.

%%%
%%% Handling of drag events.
%%%

do_drag(Drag) ->
    Help = "[Tab] Numeric entry  [Shift],[Ctrl] Constrain",
    wings_io:message_right(Help),
    {seq,{push,dummy},get_drag_event_1(Drag)}.

get_drag_event(Drag) ->
    redraw(Drag),
    wings_io:swap_buffers(),
    get_drag_event_1(Drag).

get_drag_event_1(Drag) ->
    {replace,fun(Ev) -> handle_drag_event(Ev, Drag) end}.

handle_drag_event(#keyboard{keysym=#keysym{sym=9}}, Drag) ->
    numeric_input(Drag);
handle_drag_event(Event, Drag) ->
    case wings_camera:event(Event, fun() -> redraw(Drag) end) of
	next -> handle_drag_event_1(Event, Drag);
	Other -> Other
    end.

handle_drag_event_1(redraw, Drag) ->
    redraw(Drag),
    wings_io:swap_buffers(),
    get_drag_event_1(Drag);
handle_drag_event_1(#mousemotion{x=X,y=Y}, Drag0) ->
    {Dx0,Dy0,Drag1} = mouse_range(X, Y, Drag0),
    Move = constrain(Dx0, Dy0, Drag1),
    Drag = motion_update(Move, Drag1),
    get_drag_event(Drag);
handle_drag_event_1(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
		    Drag0) ->
    wings_io:ungrab(),
    {Drag,Move} = ?SLOW(motion(X, Y, Drag0#drag{done=true})),
    cleanup(Drag),
    St = normalize(Drag),
    DragEnded = {new_state,St#st{args=Move}},
    wings_io:putback_event(DragEnded),
    pop;
handle_drag_event_1({drag_arguments,Move}, Drag0) ->
    wings_io:ungrab(),
    Drag = ?SLOW(motion_update(Move, Drag0#drag{done=true})),
    cleanup(Drag),
    St = normalize(Drag),
    DragEnded = {new_state,St#st{args=Move}},
    wings_io:putback_event(DragEnded),
    pop;
handle_drag_event_1(#mousebutton{button=3,state=?SDL_RELEASED}, Drag) ->
    cleanup(Drag),
    wings_io:ungrab(),
    wings_io:clear_message(),
    wings_draw:model_changed(),
    wings_io:putback_event(redraw),
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

cleanup(#drag{matrices=none}) -> ok;
cleanup(#drag{matrices=Mtxs}) ->
    foreach(fun({Id,_Matrix}) ->
		    gl:deleteLists(?DL_DYNAMIC+Id, 1)
	    end, Mtxs).

magnet_radius(_Sign, #drag{falloff=none}=Drag) -> Drag;
magnet_radius(Sign, #drag{falloff=Falloff0}=Drag0) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    case Falloff0+Sign*?GROUND_GRID_SIZE/5 of
	Falloff when Falloff > 0 ->
	    {Drag,_} = motion(X, Y, Drag0#drag{falloff=Falloff}),
	    Drag;
	_Falloff -> Drag0
    end.

view_changed(#drag{flags=Flags}=Drag0) ->
    case member(screen_relative, Flags) of
	false -> Drag0;
	true ->
	    {_,X,Y} = sdl_mouse:getMouseState(),
	    Drag = Drag0#drag{x=X,y=Y,xs=0,ys=0},
	    view_changed_1(Drag)
    end.

view_changed_1(#drag{matrices=none}=Drag) ->
    #drag{tvs=Tvs0,new=New} = Drag,
    Tvs = update_tvs(Tvs0, reverse(New)),
    Drag#drag{tvs=Tvs};
view_changed_1(#drag{tvs={matrix,Tvs0},matrices=Mtxs}=Drag) ->
    Tvs = view_changed_2(Tvs0, sort(Mtxs)),
    Drag#drag{tvs={matrix,Tvs}}.

view_changed_2([{Id,Trans,_}|Tvs], [{Id,Matrix}|Ms]) ->    
    [{Id,Trans,Matrix}|view_changed_2(Tvs, Ms)];
view_changed_2([], []) -> [].

update_tvs([{Tv,StaticVs,Id,StaticWe}|Tvs], [{Id,NewWe}|New]) ->
    [{update_tvs_1(Tv, NewWe, []),StaticVs,Id,StaticWe}|update_tvs(Tvs, New)];
update_tvs([], []) -> [].

update_tvs_1([F0|Fs], NewWe, Acc) ->
    F = F0(view_changed, NewWe),
    update_tvs_1(Fs, NewWe, [F|Acc]);
update_tvs_1([], _, Acc) -> reverse(Acc).
    
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

constrain(Dx0, _Dy, #drag{unit=[angle]}) ->
    Dx = case sdl_keyboard:getModState() of
	     Mod when Mod band ?SHIFT_BITS =/= 0,
		      Mod band ?CTRL_BITS =/= 0 ->
		 trunc(150*Dx0)/150;
	     Mod when Mod band ?CTRL_BITS =/= 0 ->
		 trunc(15*Dx0)/15;
	     Mod when Mod band ?SHIFT_BITS =/= 0 ->
		 float(trunc(Dx0));
	     Mod -> Dx0
	 end,
    [15*Dx];
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
		  Mod -> {Dx0,Dy0}
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

%%%
%%% Update selection for new mouse position.
%%%

motion_update(Move, #drag{tvs={matrix,Tvs}}=Drag) ->
    progress(Move, Drag),
    gl:newList(?DL_DYNAMIC_FACES, ?GL_COMPILE),
    Mtxs = foldl(fun({Id,Trans,Matrix0}, Acc) when function(Trans) ->
			 Matrix = Trans(Matrix0, Move),
			 gl:pushMatrix(),
			 gl:multMatrixf(e3d_mat:expand(Matrix)),
			 gl:callList(?DL_DYNAMIC+Id),
			 gl:popMatrix(),
			 [{Id,Matrix}|Acc]
		 end, [], Tvs),
    gl:endList(),
    gl:newList(?DL_SEL, ?GL_COMPILE),
    sel_color(),
    gl:callList(?DL_DYNAMIC_FACES),
    gl:endList(),
    Drag#drag{matrices=Mtxs};
motion_update(Move, #drag{tvs=Tvs,sel=Sel,done=Done}=Drag) ->
    progress(Move, Drag),
    gl:newList(?DL_DYNAMIC_FACES, ?GL_COMPILE),
    New = motion_update_1(Tvs, Move, Drag, []),
    gl:endList(),
    if
	Done == true -> ok;
	true -> make_sel_dlist(New, Sel)
    end,
    Drag#drag{new=New}.

motion_update_1([{Tv,StaticVs,Id,We0}|Tvs], Move, #drag{done=Done}=Drag, A) ->
    Vtab0 = transform_vs(Tv, Move, StaticVs),
    Vtab = gb_trees:from_orddict(sort(Vtab0)),
    We = We0#we{vs=Vtab},
    if
	Done == true -> ok;
	true -> draw_flat_faces(We)
    end,
    motion_update_1(Tvs, Move, Drag, [{Id,We}|A]);
motion_update_1([], _Move, _Drag, A) -> A.

transform_vs([F0|_]=Fs, Move, Acc) when is_function(F0) ->
    foldl(fun(F, A) -> F(Move, A) end, Acc, Fs);
transform_vs(Tvs, [Dx], Acc) ->
    foldl(fun({Vec,Vs}, A) ->
		  translate(Vec, Dx, Vs, A)
	  end, Acc, Tvs).

translate({Xt0,Yt0,Zt0}, Dx, VsPos, Acc) ->
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    foldl(fun({V,#vtx{pos={X,Y,Z}}=Vtx}, A) -> 
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  [{V,Vtx#vtx{pos=Pos}}|A]
	  end, Acc, VsPos).

progress(Move, #drag{unit=Units}) ->
    Msg = progress_units(Units, Move),
    wings_io:message(lists:flatten(Msg)).

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
    ["P: ",io_lib:format("~.2f", [P*100.0]),$%];
unit(falloff, R) ->
    io_lib:format("R: ~-10.2f", [R]);
unit(Unit, Move) ->
    io:format("~p\n", [{Unit,Move}]),
    [].

normalize(#drag{new=New,matrices=none,st=#st{shapes=Shs0}=St}) ->
    Shs = foldl(fun({Id,We}, A) ->
			normalize(Id, We, A)
		end, Shs0, New),
    St#st{shapes=Shs};
normalize(#drag{matrices=Mtxs,st=#st{shapes=Shs0}=St}) ->
    Shs = foldl(fun({Id,Matrix}, A) ->
			normalize_matrix(Id, Matrix, A)
		end, Shs0, Mtxs),
    St#st{shapes=Shs}.

normalize_matrix(Id, Matrix, Shapes) ->
    We0 = gb_trees:get(Id, Shapes),
    We = wings_we:transform_vs(Matrix, We0),
    gb_trees:update(Id, We, Shapes).

normalize(Id, #we{vs=Vtab0}, Shapes) ->
    #we{vs=OldVtab0}=We0 = gb_trees:get(Id, Shapes),
    Vtab1 = sofs:relation(gb_trees:to_list(Vtab0), [{vertex,data}]),
    OldVtab1 = sofs:relation(gb_trees:to_list(OldVtab0), [{vertex,data}]),
    OldVtab2 = sofs:drestriction(OldVtab1, sofs:domain(Vtab1)),
    Vtab2 = sofs:union(Vtab1, OldVtab2),
    Vtab = gb_trees:from_orddict(sofs:to_external(Vtab2)),
    We = We0#we{vs=Vtab},
    gb_trees:update(Id, We, Shapes).

%%%
%%% Redrawing while dragging.
%%%

redraw(Drag) ->
    ?CHECK_ERROR(),
    gl:enable(?GL_DEPTH_TEST),
    wings_view:projection(),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    wings_view:model_transformations(),
    wings_draw:ground_and_axes(),
    draw_shapes(Drag),
    gl:callList(?DL_UTIL),
    wings_draw:axis_letters(),
    gl:popAttrib(),
    wings_io:update(Drag#drag.st).

draw_shapes(#drag{sel={SelMode,_}}) ->
    Wire = wings_pref:get_value(wire_mode),
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_BACK),

    %% Draw faces for winged-edge-objects.
    case Wire of
	true -> ok;
	false ->
 	    FaceColor = wings_pref:get_value(face_color),
 	    gl:color3fv(FaceColor),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    gl:polygonOffset(2.0, 2.0),
	    gl:shadeModel(?GL_SMOOTH),
	    gl:enable(?GL_LIGHTING),
	    draw_we(),
	    gl:disable(?GL_LIGHTING),
	    gl:shadeModel(?GL_FLAT)
    end,

    %% Draw edges if they are turned on.
    case Wire orelse wings_pref:get_value(show_edges) of
	false -> ok;
	true ->
	    case {Wire,SelMode} of
		{true,_} -> gl:color3f(1.0, 1.0, 1.0);
		{_,body} -> gl:color3f(0.3, 0.3, 0.3);
		{_,_} -> gl:color3f(0.0, 0.0, 0.0)
	    end,
	    gl:lineWidth(case SelMode of
			     edge -> wings_pref:get_value(edge_width);
			     _ -> ?NORMAL_LINEWIDTH end),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:enable(?GL_POLYGON_OFFSET_LINE),
	    gl:polygonOffset(1.0, 1.0),
	    case wings_pref:get_value(show_wire_backfaces) of
		true ->
		    gl:disable(?GL_CULL_FACE),
		    draw_we(),
		    gl:enable(?GL_CULL_FACE);
		false ->
		    draw_we()
	    end
    end,

    %% Don't draw unselected vertices.
    %% Don't draw hard edges.

    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_POINT),
    gl:disable(?GL_POLYGON_OFFSET_FILL),

    %% Selection.
    draw_sel(SelMode).

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_sel(edge) ->
    sel_color(),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:callList(?DL_SEL);
draw_sel(vertex) ->
    sel_color(),
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:callList(?DL_SEL);
draw_sel(_Mode) ->
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    sel_color(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:callList(?DL_SEL).
    
draw_we() ->
    gl:callList(?DL_STATIC_FACES),
    gl:disable(?GL_LIGHTING),
    gl:callList(?DL_DYNAMIC_FACES).

%% Collect the static display list - faces that will not be moved.
%% Only for the matrix case.
static_display_list(Faces, #st{shapes=Shs0}=St) ->
    make_dlist(?DL_STATIC_FACES, Faces, false, St),
    Shs = gb_trees:to_list(Shs0),
    make_dlist_1(Shs, Faces, true),
    gl:newList(?DL_SEL, ?GL_COMPILE),
    gl:endList().

make_dlist(DlistId, Faces, DrawMembers, #st{shapes=Shapes0}) ->
    gl:newList(DlistId, ?GL_COMPILE),
    make_dlist_1(gb_trees:to_list(Shapes0), Faces, DrawMembers),
    gl:endList().

make_dlist_1([{Id,_}|Shs], [{Id,matrix}|Fs], false) ->
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{Id,We}|Shs], [{Id,matrix}|Fs], true) ->
    gl:newList(?DL_DYNAMIC+Id, ?GL_COMPILE),
    draw_faces(We),
    gl:endList(),
    make_dlist_1(Shs, Fs, true);
make_dlist_1([{_Id,We}|Shs], Fs, false) ->
    draw_faces(We),
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{_,_}|Shs], Fs, true) ->
    make_dlist_1(Shs, Fs, true);
make_dlist_1([], _Fs, _Draw) -> ok.

draw_faces(#we{perm=Perm}) when ?IS_NOT_VISIBLE(Perm) -> ok;
draw_faces(#we{fs=Ftab}=We) ->
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {1.0,1.0,1.0}),
    wings_draw_util:begin_end(
      fun() ->
	      foreach(fun({Face,#face{edge=Edge}}) ->
			      wings_draw_util:face(Face, Edge, We)
		      end, gb_trees:to_list(Ftab))
      end).

draw_flat_faces(#we{first_id=Flist}=We) ->
    wings_draw_util:begin_end(
      fun() ->
	      foreach(fun({Face,#face{edge=Edge}}) ->
			      wings_draw_util:flat_face(Face, Edge, We)
		      end, Flist)
      end).

%%
%% Draw the currently selected items.
%% 

make_sel_dlist(Objs, {Mode,Sel}) ->
    gl:newList(?DL_SEL, ?GL_COMPILE),
    make_sel_dlist_1(Mode, Sel, Objs),
    gl:endList().

make_sel_dlist_1(Mode, [{Id,Items}|Sel], [{Id,We}|Wes]) ->
    draw_sel(Mode, Items, We),
    make_sel_dlist_1(Mode, Sel, Wes);
make_sel_dlist_1(_Mode, [], []) -> ok.

draw_sel(vertex, Vs, #we{vs=Vtab}) ->
    gl:'begin'(?GL_POINTS),
    foreach(fun(V) ->
		    gl:vertex3fv(lookup_pos(V, Vtab))
	    end, Vs),
    gl:'end'();
draw_sel(edge, Edges, #we{es=Etab,vs=Vtab}) ->
    gl:'begin'(?GL_LINES),
    foreach(fun(Edge) ->
		    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		    gl:vertex3fv(lookup_pos(Va, Vtab)),
		    gl:vertex3fv(lookup_pos(Vb, Vtab))
	    end, Edges),
    gl:'end'();
draw_sel(face, Faces, We) ->
    wings_draw_util:begin_end(
      fun() ->
	      foreach(fun(Face) ->
			      wings_draw_util:flat_face(Face, We)
		      end, Faces)
      end);
draw_sel(body, _, #we{fs=Ftab}=We) ->
    draw_sel(face, gb_trees:keys(Ftab), We).

lookup_pos(Key, Tree) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tree),
    Pos.
