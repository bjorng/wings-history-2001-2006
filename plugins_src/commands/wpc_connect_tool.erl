%%
%%  wpc_connect_tool.erl --
%%
%%     Connect/Cut mode plugin.
%%
%%  Copyright (c) 2004 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_connect_tool.erl,v 1.11 2004/12/18 19:36:02 bjorng Exp $
%%
-module(wpc_connect_tool).

-export([init/0,menu/2,command/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).

-include_lib("wings.hrl").

-import(lists, [foldl/3,last/1,member/2,reverse/1,reverse/2,
		seq/2,sort/1]).
%% State info
-record(cs, {v=[],  %% Connected vertices
	     last,  %% Last vertex selected (for ending loop)
	     we,    %% Current we
	     st,    %% Working St
	     cpos,  %% Cursor Position
	     mode=normal, %% or slide
	     ost}). %% Original St

%% Vertex info
-record(vi, {id,    %% Vertex Id
	     mm,    %% MatrixMode
	     pos}). %% Vertex Pos

init() -> true.

menu({tools}, Menu0) ->
    Menu0 ++ [separator,
	      {"Connect", connect,
	       "Mode for quickly connecting vertices and edges"}
	     ];
menu(_, Menu) -> Menu.

command({tools,connect}, St0) ->
    wings:mode_restriction([vertex,edge]), %% ,face
    Active = wings_wm:this(),
    wings_wm:callback(fun() -> wings_u:menu_restriction(Active, [view]) end),
    St = wings_undo:init(St0#st{selmode=edge,sel=[],sh=true}),
    wings_draw:refresh_dlists(St),
    C = #cs{ost=St0, st=St},
    help(C),
    {seq,push,update_connect_handler(C)};
command(_, _) -> next.

%% Event handler for connect mode

update_connect_handler(#cs{st=St}=C) ->
    wings_wm:current_state(St),
    wings_draw:update_sel_dlist(),
    wings_wm:dirty(),
    {replace,fun(Ev) -> handle_connect_event(Ev, C) end}.

handle_connect_event(redraw, C) ->
    help(C),
    redraw(C),
    keep;
handle_connect_event(Ev, #cs{st=St}=C) ->
    Cam = wings_camera:event(Ev, St),
    case Cam of
	next -> handle_connect_event0(Ev, C);
	Other -> Other
    end.

handle_connect_event0(#keyboard{sym=?SDLK_ESCAPE}, C) ->
    exit_connect(C);
handle_connect_event0(#mousemotion{}=Ev, #cs{st=St, v=VL}=C) ->
    Update = VL /= [],
    Redraw = fun() -> redraw(C) end,
    Options = [{always_dirty,Update}, 
	       {filter, fun(Hit) -> filter_hl(Hit,C) end}],
    case wings_pick:hilite_event(Ev, St, Redraw, Options) of
	next -> handle_connect_event1(Ev, C);
	Other -> Other
    end;
handle_connect_event0(Ev, S) -> handle_connect_event1(Ev, S).

handle_connect_event1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED},
		      #cs{st=St0}=C0) ->
    case wpa:pick(X,Y,St0) of
	{add,MM,St = #st{selmode=edge,sel=[{Shape,Edge0}],shapes=Sh}} ->
	    #we{es=Es} = gb_trees:get(Shape, Sh),
	    [Edge] = gb_sets:to_list(Edge0),
	    #edge{vs=V1,ve=V2} = gb_trees:get(Edge, Es),
	    C = do_connect(X,Y,MM,St,C0),
	    wings:unregister_postdraw_hook(geom,?MODULE),
	    {drag, Drag} = slide(C,V1,V2),
	    wings_wm:later({slide_setup,Drag}),
	    update_connect_handler(C#cs{mode=slide});
	_Other ->
	    keep
    end;
handle_connect_event1(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
		      #cs{st=St0}=C0) ->
    case wpa:pick(X,Y,St0) of
	{add,MM,St} ->
	    C = do_connect(X,Y,MM,St,C0),
	    update_hook(C),
	    wings_draw:refresh_dlists(C#cs.st),
	    update_connect_handler(C);
	_Other ->
	    keep
    end;
handle_connect_event1(#mousebutton{button=3,state=?SDL_RELEASED}, C) ->
    exit_connect(C);
handle_connect_event1(init_opengl, #cs{st=St}) ->
    wings:init_opengl(St),
    wings_draw:refresh_dlists(St),
    keep;
handle_connect_event1(quit=Ev, C) ->
    wings_wm:later(Ev),
    exit_connect(C);
handle_connect_event1(got_focus, C = #cs{st=St}) ->
    update_connect_handler(C#cs{st=St#st{selmode=edge,sel=[],sh=true}});
handle_connect_event1({current_state,St}, #cs{st=St}) ->    
    keep; %% Ignore my own changes.
handle_connect_event1({current_state,St}=Ev, C) ->
    case topological_change(St) of
	false ->
	    wings_draw:refresh_dlists(St),
	    update_connect_handler(C#cs{st=St});
	true ->
	    wings_wm:later(Ev),
	    wings:unregister_postdraw_hook(geom,?MODULE),
	    pop
    end;
handle_connect_event1({slide_setup,Drag},_C) ->
    wings_drag:do_drag(Drag,none);
handle_connect_event1({new_state,St0=#st{shapes=Sh}},C0=#cs{mode=slide,we=Shape,v=[V|VR]}) ->
    #we{vp=Vtab} = gb_trees:get(Shape, Sh),
    Pos = gb_trees:get(V#vi.id, Vtab),
    St1 = St0#st{sel=[],temp_sel=none, sh=true},
    St = wings_undo:save(St0, St1),
    C = C0#cs{mode=normal,v=[V#vi{pos=Pos}|VR],st=St},
    update_hook(C),
    wings_draw:refresh_dlists(C#cs.st),
    update_connect_handler(C);
handle_connect_event1({new_state,St}=Ev, C) ->
    case topological_change(St) of
	false ->
	    wings_draw:refresh_dlists(St),
	    update_connect_handler(C#cs{st=St});
	true ->
	    wings_wm:later(Ev),
	    wings:unregister_postdraw_hook(geom,?MODULE),
	    pop
    end;
handle_connect_event1({action,Action}, #cs{st=St0}=C) ->
    case Action of
	{select,Cmd} -> select_cmd(Cmd, C);
	{view,auto_rotate} -> keep;
	{view,smoothed_preview} -> keep;
	{view,aim} ->
	    St = fake_selection(St0),
	    wings_view:command(aim, St),
	    update_connect_handler(C);
	{view,Cmd} ->
	    case wings_view:command(Cmd, St0) of
		keep ->
		    keep;
		#st{}=St ->
		    refresh_dlists(Cmd, St),
		    update_connect_handler(C#cs{st=St})
	    end;
	{edit,undo_toggle} ->
	    St = wings_undo:undo_toggle(St0),
	    undo_refresh(St,C);
	{edit,undo} ->
	    St = wings_undo:undo(St0),
	    undo_refresh(St,C);
	{edit,redo} ->
	    St = wings_undo:redo(St0),
	    undo_refresh(St,C);
	_Ignore -> keep
    end;
handle_connect_event1(Ev, #cs{st=St}) ->
    case wings_hotkey:event(Ev, St) of
	next -> keep;
	Other -> wings_wm:later({action,Other})
    end.

undo_refresh(St0,C0) ->
    St = St0#st{sel=[],temp_sel=none,sh=true},
    C = C0#cs{v=[],we=undefined,last=undefined,mode=normal,st=St},
    update_hook(C),
    wings_draw:refresh_dlists(St),
    update_connect_handler(C).

exit_connect(#cs{ost=St,st=#st{shapes=Shs,views=Views}}) ->
    wings:unregister_postdraw_hook(geom, ?MODULE),
    wings_wm:later({new_state,St#st{shapes=Shs,views=Views}}),
    pop.

refresh_dlists(wireframe_selected, _) -> ok;
refresh_dlists(shade_selected, _) -> ok;
refresh_dlists(toggle_wireframe, _) -> ok;
refresh_dlists(orthogonal_view, _) -> ok;
refresh_dlists(aim, _) -> ok;
refresh_dlists(frame, _) -> ok;
refresh_dlists(toggle_lights, _) -> ok;
refresh_dlists({along,_}, _) -> ok;
refresh_dlists({toggle_lights,_}, _) -> ok;
refresh_dlists(_, St) -> wings_draw:refresh_dlists(St).

select_cmd(deselect, #cs{st=St0}=C) ->
    St = wings_sel:reset(St0),
    update_connect_handler(C#cs{st=St});
select_cmd(vertex=M, C) -> mode_change(M, C);
select_cmd(edge=M, C) -> mode_change(M, C);
% select_cmd(face=M, C) -> mode_change(M, C);
% select_cmd(body=M, C) -> mode_change(M, C);
% select_cmd({adjacent,M}, C) -> mode_change(M, C);
select_cmd(_, _) -> keep.

mode_change(Mode, #cs{st=St0}=C) ->
    St = St0#st{selmode=Mode,sh=false},
    update_connect_handler(C#cs{st=St}).

topological_change(#st{shapes=Shs}) ->
    R = wings_dl:fold(fun(#dlo{src_we=We}, [We|Wes]) -> Wes;
			 (#dlo{drag=none}, [_|Wes]) -> Wes;
			 (_, _) -> changed
		      end, gb_trees:values(Shs)),
    R =:= changed.

redraw(#cs{st=St}) ->
    wings:redraw("", St),
    keep.

%%filter_hl(Hit,Cstate) -> true|false
filter_hl(_, #cs{v=[]}) -> true;
filter_hl({_,_,{Sh,_}},#cs{we=Prev}) when Sh /= Prev ->
    false;
filter_hl({edge,_,{Shape,Edge}}, #cs{last=Id,st=#st{shapes=Sh}}) ->
    We = #we{es=Es} = gb_trees:get(Shape,Sh),
    Ok = vertex_fs(Id, We),
    #edge{lf=F1,rf=F2} = gb_trees:get(Edge, Es),
    Fs = ordsets:from_list([F1,F2]),
    length(ordsets:intersection(Fs,Ok)) == 1;
filter_hl({vertex,_,{_,Id1}}, #cs{last=Id1}) -> true; %% Allow quitting
filter_hl({vertex,_,{Shape,Id1}}, #cs{last=Id2,st=#st{shapes=Sh}}) ->
    We = gb_trees:get(Shape,Sh),
    Ok = vertex_fs(Id2, We),
    Fs = vertex_fs(Id1, We),
    length(ordsets:intersection(Fs,Ok)) == 1.

do_connect(_X,_Y,MM,St0=#st{selmode=vertex,sel=[{Shape,Sel0}],shapes=Sh},
	   C0=#cs{v=VL,we=Prev}) ->
    [Id1] = gb_sets:to_list(Sel0),
    We0 = gb_trees:get(Shape, Sh),
    Pos = gb_trees:get(Id1, We0#we.vp),
    St1 = St0#st{sel=[],temp_sel=none, sh=true},
    Fs0 = vertex_fs(Id1, We0),
    Fs  = ordsets:from_list(Fs0),
    VI = #vi{id=Id1,mm=MM,pos=Pos},
    case VL of
	[] ->
	    C0#cs{v=[VI],we=Shape,last=Id1,st=St1};
	[#vi{id=Id1}|_] -> 
	    C0#cs{v=[],we=undefined,last=undefined,st=St1};
	[#vi{id=Id2}|_] when Prev == Shape ->
	    Ok = vertex_fs(Id2,We0),
	    case ordsets:intersection(Fs,Ok) of
		[Face] ->
		    We = wings_vertex:connect(Face,[Id1,Id2],We0),
		    St2 = St1#st{shapes=gb_trees:update(Shape,We, Sh)},
		    St = wings_undo:save(St0, St2),
		    C0#cs{v=[VI],we=Shape,last=Id1,st=St};
		_ ->
		    C0
	    end;
	_ ->
	    C0
    end;
do_connect(X,Y,MM,St0=#st{selmode=edge,sel=[{Shape,Sel0}],shapes=Sh},
	   C0=#cs{v=VL,we=Prev}) ->
    [Edge] = gb_sets:to_list(Sel0),
    We0 = gb_trees:get(Shape, Sh),
    if 
	Prev /= undefined, Prev /= Shape ->
	    C0;	    %% Wrong We, Ignore
	true ->
	    St1 = St0#st{sel=[],temp_sel=none,sh=true},
	    {Pos,Fs} = calc_edgepos(X,Y,Edge,MM,We0),
	    {We1,Id1} = wings_edge:fast_cut(Edge, Pos, We0),
	    VI = #vi{id=Id1,mm=MM,pos=Pos},
	    case VL of
		[] ->
		    St = St1#st{shapes=gb_trees:update(Shape,We1,Sh)},
		    C0#cs{v=[VI],we=Shape,last=Id1,st=St};
		[#vi{id=Id2}|_] ->
		    Ok = vertex_fs(Id2,We1),
		    case ordsets:intersection(Fs,Ok) of
			[Face] ->
			    We = wings_vertex:connect(Face,[Id1,Id2],We1),
			    St = St1#st{shapes=gb_trees:update(Shape,We,Sh)},
			    C0#cs{v=[VI],we=Shape,last=Id1,st=St};
			_ ->
			    C0
		    end;
		_ ->
		    C0
	    end
    end.
   
vertex_fs(Id, We) ->
    Fs = wings_vertex:fold(fun(_,Face,_,Acc) -> [Face|Acc] end, [], Id, We),
    ordsets:from_list(Fs).

calc_edgepos(X,Y0,Edge,MM,#we{id=Id,es=Es,vp=Vs}) ->
    {_,H} = wings_wm:win_size(),
    Y = H-Y0,
    #edge{vs=V1,ve=V2,lf=F1,rf=F2} = gb_trees:get(Edge, Es),
    Pos1 = gb_trees:get(V1, Vs),
    Pos2 = gb_trees:get(V2, Vs),
    Matrices = wings_u:get_matrices(Id, MM),
    V1Sp = setelement(3,obj_to_screen(Matrices, Pos1),0.0),
    V2Sp = setelement(3,obj_to_screen(Matrices, Pos2),0.0),
    V1Dist  = e3d_vec:dist(V1Sp,{float(X),float(Y),0.0}),
    V2Dist  = e3d_vec:dist(V2Sp,{float(X),float(Y),0.0}),
    %TotDist = e3d_vec:dist(V1Sp,V2Sp),
    TotDist = V1Dist+V2Dist,
    Dist = V1Dist/TotDist,
    Vec = e3d_vec:mul(e3d_vec:sub(Pos2,Pos1),Dist),
    Pos = e3d_vec:add(Pos1, Vec),
    {Pos, ordsets:from_list([F1,F2])}.

obj_to_screen({MVM,PM,VP}, {X,Y,Z}) ->
    glu:project(X, Y, Z, MVM, PM, VP).

% screen_to_obj({MVM,PM,VP}, {Xs,Ys,Zs}) ->
%     glu:unProject(Xs, Ys, Zs, MVM, PM, VP).

help(#cs{v=[]}) ->
    Msg1 = wings_msg:button_format("Select vertex or cut edge [press button to slide]"),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], "Exit Connect"),
    Msg = wings_msg:join([Msg1,Msg2,Msg3]),
    wings_wm:message(Msg, "");
help(_) ->
    Msg1 = wings_msg:button_format("Connects edge/vertex [reselect last vertex to end]"),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], "Exit Connect"),
    Msg = wings_msg:join([Msg1,Msg2,Msg3]),
    wings_wm:message(Msg, "").

fake_selection(St) ->
    wings_dl:fold(fun(#dlo{src_sel=none}, S) ->
			  %% No selection, try highlighting.
			  fake_sel_1(S);
		     (#dlo{src_we=#we{id=Id},src_sel={Mode,Els}}, S) ->
			  S#st{selmode=Mode,sel=[{Id,Els}]}
		  end, St).

fake_sel_1(St0) ->
    case wings_pref:get_value(use_temp_sel) of
	false -> St0;
	true ->
	    {_,X,Y} = wings_wm:local_mouse_state(),
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} -> St;
		_ -> St0
	    end
    end.

update_hook(#cs{v=[]}) ->
    wings:unregister_postdraw_hook(geom,?MODULE);
update_hook(C) ->
    Hook = fun(_St) -> draw_connect(C) end,
    wings:register_postdraw_hook(geom,?MODULE,Hook).

draw_connect(#cs{v=[#vi{pos=Pos0,mm=MM}],we=Id}) ->
    {W,H} = wings_wm:win_size(),
    {_,X,Y0} = wings_wm:local_mouse_state(),
    Y = H-Y0,
    Matrices = wings_u:get_matrices(Id, MM),
    Pos = setelement(3, obj_to_screen(Matrices, Pos0), 0.0),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_DEPTH_TEST),    
    gl:disable(?GL_ALPHA_TEST),
    gl:color3f(0, 0, 0),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:'begin'(?GL_LINES),
    gl:vertex3fv(Pos),
    gl:vertex3f(X, Y, 0),
    gl:'end'(),
    gl:popAttrib().

slide(#cs{st=St=#st{shapes=Sh},we=Shape,v=[#vi{id=Id1,mm=MM}|_]},S,E) ->
    #we{vp=Vtab} = gb_trees:get(Shape, Sh),
    Start0 = gb_trees:get(S, Vtab),
    End0   = gb_trees:get(E, Vtab),
    Curr  = gb_trees:get(Id1, Vtab),
    Matrices = wings_u:get_matrices(Shape, MM),
    P0 = {P0x,P0y,_} = obj_to_screen(Matrices, Start0),
    P1 = {P1x,P1y,_} = obj_to_screen(Matrices, End0),
    %% Decide what's up and down.
    {Dx,Dy,_} = e3d_vec:sub(P1, P0),
    {Start,End} = 
	if 
	    abs(Dx) > abs(Dy), P0x < P1x ->  {Start0,End0};
	    abs(Dx) > abs(Dy) ->             {End0,Start0};
	    P0y < P1y ->  {Start0,End0};
	    true ->       {End0,Start0}
	end,
    {Tvs,Sel,Init} = slide_make_tvs(Id1,Curr,Start,End,Shape),
    Units = [{percent,{0.0,1.0}}],
    Flags = [{initial,[Init]}],
    wings_drag:setup(Tvs, Units, Flags, wings_sel:set(vertex, Sel, St)).

slide_make_tvs(V,Curr,Start,End,Id) ->
    Dir = e3d_vec:sub(End, Start),
    TotDist = e3d_vec:len(Dir),
    Dist = e3d_vec:dist(Start,Curr),
    CursorPos  = Dist/TotDist,
    
    Fun = fun(I,Acc) -> sliding(I, Acc, V, Start, Dir) end,
    Sel = [{Id,gb_sets:singleton(V)}],
    {[{Id,{[V],Fun}}],Sel,CursorPos}.

sliding([Dx|_],Acc,V,Start,Dir) ->
    Pos = e3d_vec:add_prod(Start, Dir, Dx),
    [{V,Pos}|Acc].
