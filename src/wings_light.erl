%%
%%  wings_light.erl --
%%
%%     Implementation of lights.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_light.erl,v 1.4 2002/08/10 17:14:10 bjorng Exp $
%%

-module(wings_light).
-export([is_any_light_selected/1,menu/3,command/2,
	 create/2,update_dynamic/2,update/1,render/1,
	 global_lights/0,camera_lights/0]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-define(DEF_X, 0).
-define(DEF_Y, 2).
-define(DEF_Z, 0).

%% Light record in We.
-record(light,
	{type,
	 diffuse={1.0,1.0,1.0,1.0},
	 ambient={0.0,0.0,0.0,1.0},
	 specular={1.0,1.0,1.0,1.0},
	 aim,					%Aim point for spot/infinite.
	 spot_angle
	}).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St#st{selmode=body}),
    Menu = [{"Light operations",ignore},
	    separator,
	    {"Move",{move,Dir}},
	    separator,
	    {"Position Highlight",position_fun()},
	    separator,
	    {"Spot Angle",spot_angle},
	    separator,
	    {"Edit Properties",edit}],
    wings_menu:popup_menu(X, Y, light, Menu).

command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({position_highlight,{Mode,Sel}}, St) ->
    position_highlight(Mode, Sel, St);
command(spot_angle, St) ->
    spot_angle(St);
command(edit, St) ->
    edit(St);
command(color, St) ->
    St.

position_fun() ->
    fun(help, _) -> "";
       (_, _) -> {vector,{pick_special,position_sel()}}
    end.

position_sel() ->
    {[face,edge,vertex],
     fun(St) ->
	     wings_io:message("Select element to set highlight to."),
	     St#st{selmode=face,sel=[]}
     end,
     fun position_check_selection/1,
     fun(_X, _Y, #st{selmode=Mode,sel=Sel}=St) ->
	     case position_check_selection(St) of
		 {none,""} ->
		     F = fun(_, _) -> {light,{position_highlight,{Mode,Sel}}} end,
		     {"Position Highlight",F};
		 {_,Message} ->
		     wings_io:message(Message),
		     invalid_selection
	     end
     end}.

position_check_selection(#st{sel=[{_,Elems}]}) ->
    case gb_trees:size(Elems) of
	1 -> {none,""};
	_ -> {none,"Select only one element."}
    end;
position_check_selection(_) ->
    {none,"Select only one element."}.

position_highlight(Mode, Sel, St) ->
    Center = e3d_vec:average(wings_sel:bounding_box(St#st{selmode=Mode,sel=Sel})),
    wings_sel:map(fun(_, We) when ?IS_LIGHT(We) ->
			  position_highlight_1(Center, We);
		     (_, We) -> We
		  end, St).

position_highlight_1(Center, #we{light=L0}=We) ->
    case L0 of
	#light{type=point} ->
	    move_light(Center, We);
	_ ->
	    L = L0#light{aim=Center},
	    We#we{light=L}
    end.

spot_angle(St) ->
    Drag = wings_sel:fold(
	     fun(_, #we{id=Id,light=L}=We, none) when ?IS_LIGHT(We) ->
		     case L of
			 #light{type=spot,spot_angle=SpotAngle} ->
			     SpotFun = fun spot_angle/2,
			     Tvs = {general,[{Id,SpotFun}]},
			     Flags = [{initial,[SpotAngle,0,0]}],
			     {Tvs,[{angle,{0.1,89.9}}],Flags};
			 _ ->
			     wings_util:error("Not a spotlight.")
		     end;
		(_, We, _) when ?IS_LIGHT(We) ->
		     wings_util:error("Select only one spotlight.");
		(_, _, A) -> A
	     end, none, St),
    case Drag of
	none -> St;
	{Tvs,Units,Flags} ->
	    wings_drag:setup(Tvs, Units, Flags, St)
    end.

spot_angle([Angle|_], #dlo{src_we=#we{light=L0}=We0}=D) ->
    L = L0#light{spot_angle=Angle},
    We = We0#we{light=L},
    update(D#dlo{work=none,src_we=We}).
    
is_any_light_selected(#st{sel=Sel,shapes=Shs}) ->
    is_any_light_selected(Sel, Shs).
is_any_light_selected([{Id,_}|Sel], Shs) ->
    case gb_trees:get(Id, Shs) of
	#we{light=none} -> is_any_light_selected(Sel, Shs);
	#we{} -> true
    end;
is_any_light_selected([], _) -> false.

%%%
%%% Editing lights.
%%%
edit(#st{sel=[{Id,_}],shapes=Shs}=St) ->
    #we{light=L0} = We0 = gb_trees:get(Id, Shs),
    #light{diffuse=Diff0,ambient=Amb0,specular=Spec0} = L0,
    Qs = [{hframe,
	   [{vframe,
	     [{label,"Diffuse"},
	      {label,"Ambient"},
	      {label,"Specular"}]},
	    {vframe,
	     [{color,Diff0},
	      {color,Amb0},
	      {color,Spec0}]}],
	   [{title,"Colors"}]}|qs_specific(L0)],
    wings_ask:dialog(Qs,
		     fun([Diff,Amb,Spec|More]) ->
			     L1 = L0#light{diffuse=Diff,ambient=Amb,specular=Spec},
			     L = edit_specific(More, L1),
			     We = We0#we{light=L},
			     St#st{shapes=gb_trees:update(Id, We, Shs)}
		     end);
edit(_) -> wings_util:error("Select only one light.").

qs_specific(#light{type=spot,spot_angle=Angle}) ->
    [{label,"Spot Angle"},{slider,{text,Angle,[{range,{0.0,85.0}}]}}];
qs_specific(_) -> [].
    
edit_specific([Angle], #light{type=spot}=L) ->
    L#light{spot_angle=Angle};
edit_specific(_, L) -> L.
    
%%%
%%% Creating lights.
%%%

create(infinite, St) ->
    build("infinite", #light{type=infinite}, St);
create(point, St) ->
    build("point", #light{type=point}, St);
create(spot, St) ->
    build("spot", #light{type=spot,spot_angle=40.0}, St).

build(Prefix, L, #st{onext=Oid}=St) ->
    Fs = [[0,3,2,1],[2,3,7,6],[0,4,7,3],[1,2,6,5],[4,5,6,7],[0,1,5,4]],
    Vs = lists:duplicate(8, {0.0,3.0,0.0}),
    We0 = wings_we:build(Fs, Vs),
    We = We0#we{light=L#light{aim={0.0,0.0,0.0}}},
    Name = Prefix++integer_to_list(Oid),
    wings_shape:new(Name, We, St).

%%%
%%% Updating, drawing and rendering lights.
%%%
update_dynamic(#dlo{work=[Work|_],src_we=#we{light=#light{type=Type}}=We0}=D, Vtab0) ->
    Vtab = gb_trees:from_orddict(lists:sort(Vtab0)),
    We = We0#we{vs=Vtab},
    List = update_1(Type, We, D),
    D#dlo{work=[Work,List],src_we=We}.

update(#dlo{work=none,src_we=#we{light=#light{type=Type}}=We}=D) ->
    List = update_1(Type, We, D),
    D#dlo{work=List,sel=List};
update(#dlo{sel=none,src_we=#we{light=#light{type=Type}}=We}=D) ->
    List = update_1(Type, We, D),
    D#dlo{work=List,sel=List};
update(D) -> D.

update_1(Type, We, #dlo{src_sel=SrcSel}) ->
    Selected = (SrcSel =/= none),
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    update_2(Type, Selected, We),
    gl:endList(),
    List.
    
update_2(infinite, Selected, #we{light=#light{aim=Aim}}=We) ->
    gl:lineWidth(1),
    set_light_col(We),
    gl:pushMatrix(),
    {X,Y,Z} = Pos = light_pos(We),
    gl:translatef(X, Y, Z),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, 0.08, 25, 25),
    glu:deleteQuadric(Obj),
    Vec = e3d_vec:norm(e3d_vec:sub(Aim, Pos)),
    set_sel_color(Selected),
    gl:'begin'(?GL_LINES),
    gl:vertex3fv(e3d_vec:mul(Vec, 0.2)),
    gl:vertex3fv(e3d_vec:mul(Vec, 0.6)),
    gl:'end'(),
    gl:popMatrix();
update_2(point, Selected, We) ->
    gl:lineWidth(1),
    set_light_col(We),
    gl:pushMatrix(),
    {X,Y,Z} = light_pos(We),
    gl:translatef(X, Y, Z),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_FLAT),
    glu:sphere(Obj, 0.08, 25, 25),
    glu:deleteQuadric(Obj),
    set_sel_color(Selected),
    gl:'begin'(?GL_LINES),
    lines({1.0,0.0,0.0}),
    lines({0.0,1.0,0.0}),
    lines({0.0,0.0,1.0}),
    lines({0.71,0.71,0.0}),
    lines({0.71,0.0,0.71}),
    lines({0.0,0.71,0.71}),
    gl:'end'(),
    gl:popMatrix();
update_2(spot, Selected, #we{light=#light{aim=Aim,spot_angle=Angle}}=We) ->
    gl:lineWidth(1),
    set_light_col(We),
    gl:pushMatrix(),
    Obj = glu:newQuadric(),
    {Tx,Ty,Tz} = Top = light_pos(We),
    gl:translatef(Tx, Ty, Tz),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, 0.08, 25, 25),
    set_sel_color(Selected),
    SpotDir = e3d_vec:norm(e3d_vec:sub(Aim, Top)),
    Rad = 3.1416*Angle/180,
    R = math:sin(Rad),
    H = math:cos(Rad),
    {Dx,Dy,Dz} = e3d_vec:mul(SpotDir, H),
    gl:translatef(Dx, Dy, Dz),
    Rot = e3d_mat:expand(e3d_mat:rotate_s_to_t({0.0,0.0,1.0}, e3d_vec:neg(SpotDir))),
    gl:multMatrixd(Rot),
    glu:quadricDrawStyle(Obj, ?GLU_LINE),
    glu:cylinder(Obj, R, 0.08, H, 12, 1),
    glu:deleteQuadric(Obj),
    gl:popMatrix().

lines(Vec) ->
    gl:vertex3fv(e3d_vec:mul(Vec, 0.2)),
    gl:vertex3fv(e3d_vec:mul(Vec, 0.6)),
    gl:vertex3fv(e3d_vec:mul(Vec, -0.2)),
    gl:vertex3fv(e3d_vec:mul(Vec, -0.6)).

set_light_col(#we{light=#light{diffuse=Diff}}) ->
    gl:color4fv(Diff).

set_sel_color(false) -> gl:color3f(0, 0, 1);
set_sel_color(true) -> gl:color3fv(wings_pref:get_value(selected_color)).
    
render(#dlo{work=Light}) ->
    wings_draw_util:call(Light).

%%%
%%% Setting up lights.
%%%

camera_lights() ->
    setup_lights(camera).

global_lights() ->
    setup_lights(global).

setup_lights(CoordType) ->
    case wings_pref:get_value(scene_lights) of
	false -> modeling_lights(CoordType);
	true -> scene_lights(CoordType)
    end.

modeling_lights(global) -> ok;
modeling_lights(camera) ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.1,0.1,0.1,1.0}),
    gl:enable(?GL_LIGHT0),
    gl:disable(?GL_LIGHT2),
    gl:disable(?GL_LIGHT3),
    gl:disable(?GL_LIGHT4),
    gl:disable(?GL_LIGHT5),
    gl:disable(?GL_LIGHT6),
    gl:disable(?GL_LIGHT7),
    case wings_pref:get_value(number_of_lights) of
	1 ->
	    gl:disable(?GL_LIGHT1),
	    gl:lightfv(?GL_LIGHT1, ?GL_DIFFUSE, {1,1,1,1}),
	    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0.0,0.71,0.71,0.0});
	2 ->
	    gl:enable(?GL_LIGHT1),
	    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0.71,0.71,0.0,0.0}),
	    gl:lightfv(?GL_LIGHT1, ?GL_DIFFUSE, {0.5,0.5,0.5,1}),
	    gl:lightfv(?GL_LIGHT1, ?GL_POSITION, {-0.71,-0.71,0.0})
    end.

scene_lights(camera) -> ok;
scene_lights(global) ->
    Lnum = wings_draw_util:fold(fun scene_lights_fun/2, ?GL_LIGHT0),
    disable_from(Lnum).

disable_from(Lnum) when Lnum > ?GL_LIGHT7 -> ok;
disable_from(Lnum) ->
    gl:disable(Lnum),
    disable_from(Lnum+1).

scene_lights_fun(_, Lnum) when Lnum > ?GL_LIGHT7 -> Lnum;
scene_lights_fun(#dlo{src_we=#we{light=none}}, Lnum) -> Lnum;
scene_lights_fun(#dlo{src_we=#we{light=L}=We}, Lnum) ->
    setup_light(Lnum, L, We),
    gl:enable(Lnum),
    Lnum+1.
    
setup_light(Lnum, #light{type=infinite,aim=Aim}=L, We) ->
    {X,Y,Z} = e3d_vec:norm(e3d_vec:sub(light_pos(We), Aim)),
    gl:lightfv(Lnum, ?GL_POSITION, {X,Y,Z,0}),
    setup_color(Lnum, L);
setup_light(Lnum, #light{type=point}=L, We) ->
    {X,Y,Z} = light_pos(We),
    gl:lightfv(Lnum, ?GL_POSITION, {X,Y,Z,1}),
    gl:lightf(Lnum, ?GL_SPOT_CUTOFF, 180.0),
    setup_color(Lnum, L);
setup_light(Lnum, #light{type=spot,spot_angle=SpotAngle,aim=Aim}=L, We) ->
    Pos = {X,Y,Z} = light_pos(We),
    Dir = e3d_vec:norm(e3d_vec:sub(Aim, Pos)),
    gl:lightfv(Lnum, ?GL_POSITION, {X,Y,Z,1}),
    gl:lightf(Lnum, ?GL_SPOT_CUTOFF, SpotAngle),
    gl:lightfv(Lnum, ?GL_SPOT_DIRECTION, Dir),
    setup_color(Lnum, L).

setup_color(Lnum, #light{diffuse=Diff,ambient=Amb,specular=Spec}) ->
    gl:lightfv(Lnum, ?GL_DIFFUSE, Diff),
    gl:lightfv(Lnum, ?GL_AMBIENT, Amb),
    gl:lightfv(Lnum, ?GL_SPECULAR, Spec).

light_pos(#we{vs=Vtab}) ->
    #vtx{pos=Pos} = gb_trees:get(1, Vtab),
    Pos.

move_light(Pos, #we{vs=Vtab0}=We) ->
    Vtab1 = [{V,Vtx#vtx{pos=Pos}} || {V,Vtx} <- gb_trees:to_list(Vtab0)],
    Vtab = gb_trees:from_orddict(Vtab1),
    We#we{vs=Vtab}.
