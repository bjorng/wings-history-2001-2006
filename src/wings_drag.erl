%%
%%  wings_drag.erl --
%%
%%     This module handles interactive commands.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_drag.erl,v 1.40 2001/12/23 11:32:46 bjorng Exp $
%%

-module(wings_drag).
-export([init_drag/3,init_drag/4,do_drag/1,message/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(DL_STATIC_FACES, (?DL_DRAW_BASE)).
-define(DL_DYNAMIC_FACES, (?DL_DRAW_BASE+1)).
-define(DL_SEL, (?DL_DRAW_BASE+2)).

-define(DL_DYNAMIC, (?DL_DRAW_BASE+3)).

-import(lists, [foreach/2,map/2,foldl/3,sort/1,keysort/2,
		reverse/1,concat/1]).

-record(drag,
	{x,					%Original 2D position
	 y,					
	 xs=0,
	 ys=0,
	 tvs,					%[{Vertex,Vec}...]
	 constraint,				%Constraints for motion
	 unit,					%Unit that drag is done in.
	 new,					%New objects.
	 sel,					%Massaged selection.
	 matrices=none				%Transformation matrices.
	}).

-record(dlist,					%Display list.
	{faces,
	 edges}).

init_drag(Tvs, Constraint, St) ->
    init_drag_1(Tvs, Constraint, none, St).

init_drag(Tvs, Constraint, Unit, St) ->
    init_drag_1(Tvs, Constraint, Unit, St).

init_drag_1(Tvs, Constraint, Unit, St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    Drag = #drag{x=X,y=Y,constraint=Constraint,unit=Unit},
    init_drag_2(Tvs, Drag, St).

init_drag_2(Tvs0, Drag0, #st{selmode=Mode,sel=Sel0}=St) ->
    case combine(Tvs0) of
	{matrix,Tv}=Tvs ->
	    Faces = [{Id,matrix} || {Id,Trans,Matrix} <- Tv],
	    Drag = Drag0#drag{tvs=Tvs},
	    static_display_list(Faces, St),
	    wings_io:grab(),
	    St#st{drag=Drag,dl=none};
	Tvs ->
 	    gl:newList(?DL_STATIC_FACES, ?GL_COMPILE),
	    Dyn = break_apart(Tvs, St),
 	    gl:endList(),
	    Sel = {Mode,[{Id,gb_sets:to_list(S)} || {Id,S} <- Sel0]},
	    Drag = Drag0#drag{tvs=Dyn,sel=Sel},
	    wings_io:grab(),
	    St#st{drag=Drag,dl=none}
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
break_apart({matrix,_}=Tvs, St) -> Tvs;		%Specially handled.
break_apart(Tvs, #st{shapes=Shs}=St) ->
    break_apart(Tvs, gb_trees:to_list(Shs), []).

break_apart([{Id,_}|_]=Tvs, [{ShId,Sh}|Shs], Dyn) when ShId < Id ->
    #shape{sh=We} = Sh,
    draw_faces(We),
    break_apart(Tvs, Shs, Dyn);
break_apart([{Id,Tv}|Tvs], [{Id,#shape{sh=We0}=Sh}|Shs], DynAcc) ->
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
break_apart([], [{_,#shape{sh=We}}|Shs], Dyn) ->
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
insert_vtx_data([], Vtab, Acc) -> Acc.

insert_vtx_data_1([V|Vs], Vtab, Acc) ->
    insert_vtx_data_1(Vs, Vtab, [{V,gb_trees:get(V, Vtab)}|Acc]);
insert_vtx_data_1([], Vtab, Acc) -> Acc.

%%%
%%% Handling of drag events.
%%%

do_drag(St) ->
    {seq,{push,dummy},get_drag_event_1(St)}.

get_drag_event(St) ->
    redraw(St),
    get_drag_event_1(St).

get_drag_event_1(St) ->
    {replace,fun(Ev) -> handle_drag_event(Ev, St) end}.

handle_drag_event(Event, St) ->
    case wings_camera:event(Event, fun() -> redraw(St) end) of
	next -> handle_drag_event_1(Event, St);
	Other -> Other
    end.

handle_drag_event_1(#mousemotion{x=X,y=Y}, #st{drag=Drag0}=St0) ->
    {Dx0,Dy0,Drag1} = mouse_range(X, Y, Drag0),
    {Dx,Dy} = constrain(Dx0, Dy0, Drag1),
    Drag = motion_update(Dx, Dy, Drag1),
    St = St0#st{drag=Drag},
    get_drag_event(St);
handle_drag_event_1(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED}, St0) ->
    wings_io:ungrab(),
    St1 = motion(X, Y, St0),
    St = normalize(St1),
    cleanup(St),
    DragEnded = {drag_ended,St#st{drag=none,dl=none}},
    wings_io:putback_event(DragEnded),
    pop;
handle_drag_event_1(#mousebutton{button=3,x=X,y=Y,state=?SDL_RELEASED}, St) ->
    cleanup(St),
    wings_io:ungrab(),
    wings_io:putback_event(drag_aborted),
    pop;
handle_drag_event_1(view_changed, St) ->
    get_drag_event(view_changed(St));
handle_drag_event_1(Event, St0) ->
    St = case wings_hotkey:event(Event) of
	     next -> St0;
	     {view,Cmd} ->
		 wings_view:command(Cmd, St0),
		 view_changed(St0);
	     {select,less} ->
		 magnet_radius(-1, St0);
	     {select,more} ->
		 magnet_radius(1, St0);
	     Other -> St0
	 end,
    get_drag_event(St).

cleanup(#st{drag=#drag{matrices=none}}) -> ok;
cleanup(#st{drag=#drag{matrices=Mtxs}}) ->
    foreach(fun({Id,Matrix}) ->
		    gl:deleteLists(?DL_DYNAMIC+Id, 1)
	    end, Mtxs).

magnet_radius(Sign, #st{inf_r=InfR0}=St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    case InfR0+Sign*?GROUND_GRID_SIZE/4 of
	InfR when InfR > 0 ->
	    motion(X, Y, St#st{inf_r=InfR});
	Other -> St
    end.

view_changed(#st{drag=#drag{constraint=view_dependent}=Drag0}=St) ->
    {_,X,Y} = sdl_mouse:getMouseState(),
    Drag = Drag0#drag{x=X,y=Y,xs=0,ys=0},
    view_changed_1(St#st{drag=Drag});
view_changed(St) -> St.

view_changed_1(#st{drag=#drag{matrices=none}=Drag0}=St) ->
    #drag{tvs=Tvs0,new=New} = Drag0,
    Tvs = update_tvs(Tvs0, reverse(New)),
    Drag = Drag0#drag{tvs=Tvs},
    St#st{drag=Drag};
view_changed_1(#st{drag=#drag{tvs={matrix,Tvs0},matrices=Mtxs}=Drag}=St) ->
    Tvs = view_changed_2(Tvs0, sort(Mtxs)),
    St#st{drag=Drag#drag{tvs={matrix,Tvs}}}.

view_changed_2([{Id,Trans,_}|Tvs], [{Id,Matrix}|Ms]) ->    
    [{Id,Trans,Matrix}|view_changed_2(Tvs, Ms)];
view_changed_2([], []) -> [].

update_tvs([{Tv,StaticVs,Id,StaticWe}|Tvs], [{Id,NewWe}|New]) ->
    [{update_tvs_1(Tv, NewWe, []),StaticVs,Id,StaticWe}|update_tvs(Tvs, New)];
update_tvs([], []) -> [].

update_tvs_1([F0|Fs], NewWe, Acc) ->
    F = F0(view_changed, NewWe, dummy),
    update_tvs_1(Fs, NewWe, [F|Acc]);
update_tvs_1([], NewWe, Acc) -> reverse(Acc).
    
motion(X, Y, #st{drag=Drag0}=St) ->
    {Dx0,Dy0,Drag1} = mouse_range(X, Y, Drag0),
    {Dx,Dy} = constrain(Dx0, Dy0, Drag1),
    Drag = motion_update(Dx, Dy, Drag1),
    St#st{drag=Drag}.

mouse_range(X0, Y0, #drag{x=OX,y=OY,xs=Xs,ys=Ys}=Drag) ->
    [_,_,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    if
	X0 =:= W-1 ->
	    NewX = W div 2,
	    warp(NewX, Y0, NewX, 0, Drag);
	X0 =:= 0 ->
	    NewX = W div 2,
	    warp(NewX, Y0, -NewX, 0, Drag);
	Y0 =:= H-1 ->
	    NewY = H div 2,
	    warp(X0, NewY, 0, NewY, Drag);
	Y0 =:= 0 ->
	    NewY = H div 2,
	    warp(X0, NewY, 0, -NewY, Drag);
	true ->
	    X = X0 + Xs,
	    Y = Y0 + Ys,
	    Dx = (X-OX) / ?MOUSE_DIVIDER,
	    Dy = (OY-Y) / ?MOUSE_DIVIDER,
	    {Dx,Dy,Drag}
    end.

warp(X, Y, XsInc, YsInc, #drag{xs=Xs,ys=Ys}=Drag) ->
    wings_io:warp(X, Y),
    mouse_range(X, Y, Drag#drag{xs=Xs+XsInc,ys=Ys+YsInc}).

constrain(Dx0, Dy0, #drag{unit=Unit,constraint=Constraint}) ->
    {Dx,Dy} = case sdl_keyboard:getModState() of
  		  Mod when Mod band ?SHIFT_BITS =/= 0,
			   Mod band ?CTRL_BITS =/= 0 ->
		      D = if
			      Unit == angle -> 150.0;
			      true -> 100.0
			  end,
		      {trunc(D*Dx0)/D,trunc(D*Dy0)/D};
		  Mod when Mod band ?CTRL_BITS =/= 0 ->
		      D = if
			      Unit == angle -> 15.0;
			      true -> 10.0
			  end,
		      {trunc(D*Dx0)/D,trunc(D*Dy0)/D};
		  Mod when Mod band ?SHIFT_BITS =/= 0 ->
		      {float(trunc(Dx0)),float(trunc(Dy0))};
		  Mod -> {Dx0,Dy0}
	      end,
    case Constraint of
	none ->
	    {Dx,Dy};
	view_dependent ->
	    {Dx,Dy};
	{Min,Max} when Dx < Min ->
	    {Min,Dy};
	{Min,Max} when Dx > Max ->
	    {Max,Dy};
	{_,_} ->
	    {Dx,Dy}
    end.

%%%
%%% Update selection for new mouse position.
%%%

motion_update(Dx, Dy, #drag{tvs={matrix,Tvs}}=Drag) ->
    gl:newList(?DL_DYNAMIC_FACES, ?GL_COMPILE),
    Mtxs = foldl(fun({Id,Trans,Matrix0}, Acc) when function(Trans) ->
			 Matrix = Trans(Matrix0, Dx, Dy),
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
motion_update(Dx, Dy, #drag{tvs=Tvs,sel=Sel}=Drag) ->
    gl:newList(?DL_DYNAMIC_FACES, ?GL_COMPILE),
    New = motion_update_1(Tvs, Dx, Dy, Drag, []),
    gl:endList(),
    make_sel_dlist(New, Sel),
    Drag#drag{new=New}.

motion_update_1([{Tv,StaticVs,Id,We0}|Tvs], Dx, Dy, Drag, A) ->
    Vtab0 = transform_vs(Tv, Dx, Dy, Drag, StaticVs),
    Vtab = gb_trees:from_orddict(sort(Vtab0)),
    We = We0#we{vs=Vtab},
    draw_flat_faces(We),
    motion_update_1(Tvs, Dx, Dy, Drag, [{Id,We}|A]);
motion_update_1([], Dx, Dy, Drag, A) -> A.

transform_vs([F0|_]=Fs, Dx, Dy, Drag, Acc) when is_function(F0) ->
    foldl(fun(F, A) -> F(Dx, Dy, A) end, Acc, Fs);
transform_vs(Tvs, Dx, Dy, Drag, Acc) ->
    message([Dx], Drag),
    foldl(fun({Vec,Vs}, A) ->
		  translate(Vec, Dx, Vs, A)
	  end, Acc, Tvs).

translate({Xt0,Yt0,Zt0}, Dx, VsPos, Acc) ->
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    foldl(fun({V,#vtx{pos={X,Y,Z}}=Vtx}, A) -> 
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  [{V,Vtx#vtx{pos=Pos}}|A]
	  end, Acc, VsPos).

message(L, #drag{unit=Unit}) ->
    message_0(L, Unit);
message(L, Unit) when is_atom(Unit) ->
    message_0(L, Unit).

message_0([_]=L, none) ->
    message_1("~-10.2f", L);
message_0([U], percent) ->
    message_1("P:~10.2f%", [U*100.0]);
message_0([_]=L, distance) ->
    message_1("D:~-10.2f", L);
message_0([_]=L, angle) ->
    message_1("A:~-10.2f", L);
message_0([_,_]=L, none) ->
    message_1("~-10.2f ~-10.2f", L);
message_0([_,_]=L, distance) ->
    message_1("DX:~-10.2f DY:~-10.2f", L).

message_1(Format, List) ->
    wings_io:message(lists:flatten(io_lib:format(Format, List))).

normalize(#st{shapes=Shapes0,drag=#drag{new=New,matrices=none}}=St) ->
    Shapes = foldl(fun({Id,We}, A) ->
			   normalize(Id, We, A)
		   end, Shapes0, New),
    St#st{shapes=Shapes};
normalize(#st{shapes=Shapes0,drag=#drag{matrices=Mtxs}}=St) ->
    Shapes = foldl(fun({Id,Matrix}, A) ->
			   normalize_matrix(Id, Matrix, A)
		   end, Shapes0, Mtxs),
    St#st{shapes=Shapes}.

normalize_matrix(Id, Matrix, Shapes) ->
    #shape{sh=We0}= Sh0 = gb_trees:get(Id, Shapes),
    We = wings_we:transform_vs(Matrix, We0),
    Sh = Sh0#shape{sh=We},
    gb_trees:update(Id, Sh, Shapes).

normalize(Id, #we{vs=Vtab0}, Shapes) ->
    #shape{sh=#we{vs=OldVtab0}=We0} = Sh0 = gb_trees:get(Id, Shapes),
    Vtab1 = sofs:relation(gb_trees:to_list(Vtab0), [{vertex,data}]),
    OldVtab1 = sofs:relation(gb_trees:to_list(OldVtab0), [{vertex,data}]),
    OldVtab2 = sofs:drestriction(OldVtab1, sofs:domain(Vtab1)),
    Vtab2 = sofs:union(Vtab1, OldVtab2),
    Vtab = gb_trees:from_orddict(sofs:to_external(Vtab2)),
    We = We0#we{vs=Vtab},
    Sh = Sh0#shape{sh=We},
    gb_trees:update(Id, Sh, Shapes).

%%%
%%% Redrawing while dragging.
%%%

redraw(St) ->
    render(St),
    wings_io:update(St).

render(St) ->
    ?CHECK_ERROR(),
    gl:enable(?GL_DEPTH_TEST),
    wings_view:projection(),
    ?CHECK_ERROR(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    ?CHECK_ERROR(),
    wings_view:model_transformations(),
    wings_draw:ground_and_axes(),
    draw_shapes(St),
    gl:popAttrib().

draw_shapes(#st{selmode=SelMode}=St) ->
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
	    draw_we(St),
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
	    draw_we(St)
    end,

    %% Don't draw unselected vertices.

    gl:disable(?GL_POLYGON_OFFSET_LINE),
    gl:disable(?GL_POLYGON_OFFSET_POINT),
    gl:disable(?GL_POLYGON_OFFSET_FILL),

    %% Selection.
    draw_sel(St),

    %% Draw hard edges.
    draw_hard_edges(St).

sel_color() ->
    gl:color3fv(wings_pref:get_value(selected_color)).

draw_sel(#st{selmode=edge}) ->
    sel_color(),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:callList(?DL_SEL);
draw_sel(#st{selmode=vertex}) ->
    sel_color(),
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:callList(?DL_SEL);
draw_sel(St) ->
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonOffset(1.0, 1.0),
    sel_color(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:callList(?DL_SEL).
    
draw_we(St) ->
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

make_dlist(DlistId, Faces, DrawMembers, #st{shapes=Shapes0}=St) ->
    gl:newList(DlistId, ?GL_COMPILE),
    make_dlist_1(gb_trees:to_list(Shapes0), Faces, DrawMembers),
    gl:endList(),
    DlistId.

make_dlist_1([{Id,Shape}|Shs], [{Id,matrix}|Fs], false) ->
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{Id,#shape{sh=We}}|Shs], [{Id,matrix}|Fs], true) ->
    gl:newList(?DL_DYNAMIC+Id, ?GL_COMPILE),
    draw_faces(We),
    gl:endList(),
    make_dlist_1(Shs, Fs, true);
make_dlist_1([{Id,#shape{sh=We}}|Shs], Fs, false) ->
    draw_faces(We),
    make_dlist_1(Shs, Fs, false);
make_dlist_1([{Id,Shape}|Shs], Fs, true) ->
    make_dlist_1(Shs, Fs, true);
make_dlist_1([], Fs, Draw) -> ok.

draw_faces(#we{fs=Ftab}=We) ->
    foreach(fun({Face,#face{edge=Edge}}) ->
		    wings_draw_util:face(Face, Edge, We)
	    end, gb_trees:to_list(Ftab)).

draw_flat_faces(#we{first_id=Flist}=We) ->
    foreach(fun({Face,#face{edge=Edge}}) ->
		    wings_draw_util:flat_face(Face, Edge, We)
	    end, Flist).

draw_hard_edges(#st{shapes=Shapes}) ->
    gl:color3fv(wings_pref:get_value(hard_edge_color)),
    foreach(
      fun(#shape{sh=#we{he=Htab}=We}) ->
	      case gb_sets:is_empty(Htab) of
		  true -> ok;
		  false -> draw_hard_edges_1(We)
	      end;
	 (_) -> ok
      end, gb_trees:values(Shapes)),
    ?CHECK_ERROR().

draw_hard_edges_1(#we{es=Etab,he=Htab,vs=Vtab}) ->
    foreach(fun(Edge) ->
		    #edge{vs=Vstart,ve=Vend} = gb_trees:get(Edge, Etab),
		    gl:'begin'(?GL_LINES),
		    gl:vertex3fv(lookup_pos(Vstart, Vtab)),
		    gl:vertex3fv(lookup_pos(Vend, Vtab)),
		    gl:'end'()
	    end, gb_sets:to_list(Htab)).

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
make_sel_dlist_1(Mode, [], []) -> ok.

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
    foreach(fun(Face) ->
		    wings_draw_util:sel_face(Face, We)
	    end, Faces);
draw_sel(body, Dummy, #we{fs=Ftab}=We) ->
    foreach(fun(Face) ->
		    wings_draw_util:sel_face(Face, We)
	    end, gb_trees:keys(Ftab)).

lookup_pos(Key, Tree) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tree),
    Pos.
