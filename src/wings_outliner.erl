%%
%%  wings_outliner.erl --
%%
%%     Maintains the outline window.
%%
%%  Copyright (c) 2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_outliner.erl,v 1.1 2003/01/17 21:10:46 bjorng Exp $
%%

-module(wings_outliner).
-export([window/1]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [map/2,reverse/1,reverse/2,keymember/3,keysearch/3,sort/1]).
-compile(inline).

%%%
%%% Outliner window.
%%%
-record(ost,
	{st,					%Current St.
	 n,					%Number of objects.
	 first,					%First object to show.
	 sel,					%Current selection.
	 os,					%All objects.
	 active,				%Number of active object.
	 lh					%Line height.
	}).

window(St) ->
    case wings_wm:is_window(outliner) of
	true ->
	    wings_wm:delete(outliner);
	false ->
	    {{_,DeskY},{DeskW,DeskH}} = wings_wm:win_rect(desktop),
	    W = 25*?CHAR_WIDTH,
	    Ost = #ost{first=0,lh=18,active=-1},
	    Current = {current_state,St},
	    Op = {seq,push,event(Current, Ost)},
	    Pos = {DeskW-5,DeskY+55,?Z_OUTLINER},
	    Size = {W,DeskH div 2},
	    wings_wm:toplevel(outliner, "Outliner", Pos, Size,
			      [resizable,vscroller,{anchor,ne}], Op),
	    wings_wm:send(outliner, Current),
	    keep
    end.

get_event(Ost) ->
    {replace,fun(Ev) -> event(Ev, Ost) end}.

event(resized, Ost) ->
    update_scroller(Ost),
    keep;
event(redraw, Ost) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-0.5, H-1, ?PANE_COLOR),
    draw_objects(Ost),
    keep;
event({current_state,St}, Ost0) ->
    Ost = update_state(St, Ost0),
    update_scroller(Ost),
    get_event(Ost);
event(#mousemotion{y=Y}, #ost{active=Act0}=Ost) ->
    case active_object(Y, Ost) of
	Act0 -> keep;
	Act ->
	    wings_wm:dirty(),
	    get_event(Ost#ost{active=Act})
    end;
event(#mousebutton{y=Y0}=Ev, Ost) ->
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} ->
	    case active_object(Y0, Ost) of
		-1 ->
		    keep;
		Act ->
		    do_menu(Act, X, Y, Ost)
	    end
    end;
event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-1*lines(Ost) div 4, Ost);
event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(lines(Ost) div 4, Ost);
event(scroll_page_up, Ost) ->
    zoom_step(-lines(Ost), Ost);
event(scroll_page_down, Ost) ->
    zoom_step(lines(Ost), Ost);
event({set_knob_pos,Pos}, #ost{first=First0,n=N}=Ost0) ->
    case round(N*Pos) of
	First0 -> keep;
	First when First < N ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{first=First,active=-1},
	    update_scroller(Ost),
	    get_event(Ost);
	_ -> keep
    end;
event({action,{outliner,Cmd}}, Ost) ->
    command(Cmd, Ost);
event(_, _) -> keep.

do_menu(Act, X, Y, #ost{os=Objs}) ->
    Menu = case lists:nth(Act+1, Objs) of
	       {material,Name,_,_} ->
		   [{"Edit Material Properties",
		     fun(_, _) ->
			     {outliner,{edit_material,Name}} end,[]}];
	       {object,Name} ->
		   [{"Delete",
		     fun(_, _) ->
			     {outliner,{delete_object,Name}} end,[]}];
	       {light,Name} ->
		   [{"Delete",
		     fun(_, _) ->
			     {outliner,{delete_light,Name}} end,[]}]
	   end,
    wings_menu:popup_menu(X, Y, outliner, Menu).

command({edit_material,Name0}, _Ost) ->
    Name = list_to_atom(Name0),
    wings_wm:send(geom, {action,{material,{edit,Name}}}),
    keep;
command({delete_object,Name}, _) ->
    keep;
command({delete_light,Name}, _) ->
    keep.

update_state(St, #ost{first=OldFirst}=Ost0) ->
    #ost{first=First0} = Ost = update_state_1(St, Ost0),
    case clamp(First0, Ost) of
	OldFirst -> Ost;
	First ->
	    wings_wm:dirty(),
	    Ost#ost{first=First}
    end.

update_state_1(#st{shapes=Shs,mat=Mat}=St, #ost{st=#st{shapes=Shs,mat=Mat}}=Ost) ->
    Ost#ost{st=St};
update_state_1(#st{mat=Mat,shapes=Shs0}=St, #ost{os=Objs0}=Ost) ->
    Objs = [{object,Name} || #we{name=Name}=We <- gb_trees:values(Shs0),
			    ?IS_NOT_LIGHT(We)] ++
	[{light,Name} || #we{name=Name}=We <- gb_trees:values(Shs0),
			 ?IS_LIGHT(We)] ++
	[make_mat(M) || M <- gb_trees:to_list(Mat)],
    case Objs of
	Objs0 -> ok;
	_ -> wings_wm:dirty()
    end,
    Ost#ost{st=St,os=Objs,n=length(Objs)}.

make_mat({Name,Mp}) ->
    OpenGL = proplists:get_value(opengl, Mp),
    {R,G,B,_} = Color = proplists:get_value(diffuse, OpenGL),
    TextColor = case lists:max([R,G,B]) of
		    V when V < 0.5 -> {1,1,1};
		    _ -> {0,0,0}
		end,
    {material,atom_to_list(Name),Color,TextColor}.

update_scroller(#ost{n=0}) ->
    Name = wings_wm:active_window(),
    wings_wm:set_knob(Name, 0.0, 1.0);
update_scroller(#ost{first=First,n=N}=Ost) ->
    Name = wings_wm:active_window(),
    Lines = lines(Ost),
    wings_wm:set_knob(Name, First/N, Lines/N).

zoom_step(Step, #ost{first=First0}=Ost0) ->
    case clamp(First0+Step, Ost0) of
	First0 -> keep;
	First ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{first=First},
	    update_scroller(Ost),
	    get_event(Ost)
    end.

clamp(F, #ost{n=N}=Ost) ->
    Max = case N-lines(Ost) of
	      Neg when Neg < 0 -> 0;
	      Other -> Other
	  end,
    if
	F < 0 -> 0;
	F > Max -> Max;
	true -> F
    end.
    
active_object(Y0, #ost{lh=Lh,first=First,n=N}) ->
    case Y0 - top_of_first_object() of
	Y when Y < 0 -> -1;
	Y1 ->
	    case Y1 div Lh of
		Y when First+Y < N -> Y;
		_ -> -1
	    end
    end.

draw_objects(#ost{os=Objs0,first=First,lh=Lh,active=Active,n=N0}=Ost) ->
    if
	First < 0 -> erlang:fault({neg_first,First});
	true -> ok
    end,
    Objs = lists:nthtail(First, Objs0),
    R = right_pos(),
    Lines = lines(Ost),
    N = case N0-First of
	    N1 when N1 < Lines -> N1;
	    _ -> Lines
	end,
    draw_icons(N, Objs, Ost, Lh-2),
    draw_objects_1(N, Objs, Ost, R, Active, Lh-2).

draw_objects_1(0, _, _, _, _, _) -> ok;
draw_objects_1(N, [O|Objs], #ost{lh=Lh}=Ost, R, Active, Y) ->
    case O of
	{material,Name,Color,TextColor} ->
	    wings_io:border(3, Y-10, 12, 12, Color),
	    gl:color3fv(TextColor),
	    gl:rasterPos2f(8, Y),
	    wings_io:draw_char(m_bitmap()),
	    gl:color3f(0, 0, 0);
	{_,Name} -> ok
    end,
    if
	Active == 0 ->
	    gl:color3f(0, 0, 0.5),
	    gl:recti(name_pos()-2, Y-?CHAR_HEIGHT, R-2, Y+4),
	    gl:color3f(1, 1, 1);
	true -> ok
    end,
    wings_io:text_at(name_pos(), Y, Name),
    gl:color3f(0, 0, 0),
    draw_objects_1(N-1, Objs, Ost, R, Active-1, Y+Lh).

draw_icons(N, Objs, Ost, Y) ->
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    draw_icons_1(N, Objs, Ost, Y-14),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D).
    
draw_icons_1(0, _, _, _) -> ok;
draw_icons_1(N, [O|Objs], #ost{lh=Lh}=Ost, Y) ->
    X = 2,
    case O of
	{object,_} ->
	    wings_io:draw_icon(X, Y, 16, 16, small_object);
	{light,_} ->
	    wings_io:draw_icon(X, Y, 16, 16, small_light);
	{material,_,_,_} -> ok
    end,
    draw_icons_1(N-1, Objs, Ost, Y+Lh).

m_bitmap() ->
    {5,7,0,0,6,0,
     <<16#88,16#88,16#88,16#a8,16#d8,16#88,16#88>>}.

top_of_first_object() ->
    0.

right_pos() ->
    {W,_} = wings_wm:win_size(),
    W-13.

name_pos() ->
    22.

lines(#ost{lh=Lh}) ->
    {_,_,_,H} = wings_wm:viewport(),
    H div Lh.
