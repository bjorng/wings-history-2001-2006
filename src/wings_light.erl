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
%%     $Id: wings_light.erl,v 1.19 2002/11/26 20:08:34 dgud Exp $
%%

-module(wings_light).
-export([light_types/0,menu/3,command/2,is_any_light_selected/1,info/1,
	 create/2,update_dynamic/2,update_matrix/2,update/1,render/1,
	 modeling_lights/2,global_lights/0,camera_lights/0,
	 export/1,import/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,foldl/3,member/2]).

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
	 lin_att,				%Linear attenuation.
	 quad_att,				%Quadratic attenuation.
	 spot_angle,
	 spot_exp				%Spot exponent.
	}).

light_types() ->
    [{"Infinite",infinite,"Create a far-away, directional light (like the sun)"},
     {"Point",point,"Create a light that radiates light in every direction"},
     {"Spot",spot,"Create a spotlight"},
     {"Ambient",ambient,"Create an ambient light source"}].

menu(X, Y, St) ->
    SpotOnly = {iff,[spot]},
    NotAmb = {iff,[spot,infinite,point]},
    One = one_light,
    Dir = wings_menu_util:directions(St#st{selmode=body}),
    Menu0 = [{basic,{"Light operations",ignore}},
	     {basic,separator},
	     {"Move",{move_light,Dir}},
	     {NotAmb,separator},
	     {NotAmb,{"Position Highlight",position_fun(),
		      "Position the aim point or location of light"}},
	     {NotAmb,{"Color",color,"Interactively adjust hue, intensity, "
		      "and saturation"}},
	     {NotAmb,
	      {"Attenuation",
	       {attenuation,
		[{"Linear",linear,
		  "Interactively adjust how much light weakens as it travels "
		  "away from its source (linear factor)"},
		 {"Quadratic",quadratic,
		  "Interactively adjust how much light weakens as it travels "
		  "away from its source (quadratic factor)"}]}}},
	     {SpotOnly,separator},
	     {SpotOnly,{"Spot Angle",spot_angle,
			"Interactivly adjust the angle of the spotlight cone"}},
	     {SpotOnly,{"Spot Falloff",spot_falloff,
			"Interactivly adjust how much light weakens farther away "
			"from the center of the spotlight cone"}},
	     {One,separator},
	     {One,{"Edit Properties",edit,"Edit light properties"}},
	     separator,
	     {"Duplicate",{duplicate,Dir},
	      "Duplicate and move selected lights"},
	     {"Delete",delete,"Delete seleced lights"}],
    Menu = filter_menu(Menu0, St),
    wings_menu:popup_menu(X, Y, light, Menu).

filter_menu(Menu0, St) ->
    T = wings_sel:fold(
	  fun(_, #we{light=#light{type=Type}}, none) -> Type;
	     (_, #we{light=#light{}}, _) -> multiple_lights;
	     (_, _, A) -> A
	  end, none, St),
    Menu = foldl(fun({one_light,_}, A) when T == multiple_lights -> A;
		    ({one_light,Entry}, A) -> [Entry|A];
		    ({{iff,Types},Entry}, A) when is_list(Types) ->
			 case member(T, Types) of
			     true -> [Entry|A];
			     false -> A
			 end;
		    (Entry, A) -> [Entry|A]
		 end, [], Menu0),
    reverse(Menu).

command({move_light,Type}, St) ->
    wings_move:setup(Type, St);
command(color, St) ->
    color(St);
command({position_highlight,{Mode,Sel}}, St) ->
    position_highlight(Mode, Sel, St);
command({attenuation,Type}, St) ->
    attenuation(Type, St);
command(spot_angle, St) ->
    spot_angle(St);
command(spot_falloff, St) ->
    spot_falloff(St);
command(edit, St) ->
    edit(St);
command(delete, St) ->
    {save_state,delete(St)};
command({duplicate,Dir}, St) ->
    duplicate(Dir, St);
command(color, St) ->
    St.
    
is_any_light_selected(#st{sel=Sel,shapes=Shs}) ->
    is_any_light_selected(Sel, Shs).
is_any_light_selected([{Id,_}|Sel], Shs) ->
    case gb_trees:get(Id, Shs) of
	#we{light=none} -> is_any_light_selected(Sel, Shs);
	#we{} -> true
    end;
is_any_light_selected([], _) -> false.

info(#we{name=Name,light=#light{type=Type}=L}=We) ->
    Info0 = io_lib:format("Light ~s", [Name]),
    case Type of
	ambient -> Info0;
	_ ->
	    {X,Y,Z} = Pos = light_pos(We),
	    Info = [Info0|io_lib:format(": Pos ~s ~s ~s",
					[wings_util:nice_float(X),
					 wings_util:nice_float(Y),
					 wings_util:nice_float(Z)])],
	    [Info|info1(Type, Pos, L)]
    end.

info1(point, _, _) -> [];
info1(Type, Pos, #light{aim=Aim,spot_angle=A}) ->
    {Ax,Ay,Az} = Aim,
    {Dx,Dy,Dz} = e3d_vec:norm(e3d_vec:sub(Aim, Pos)),
    Info = io_lib:format(". Aim ~s ~s ~s. Dir ~s ~s ~s\n",
			 [wings_util:nice_float(Ax),
			  wings_util:nice_float(Ay),
			  wings_util:nice_float(Az),
			  wings_util:nice_float(Dx),
			  wings_util:nice_float(Dy),
			  wings_util:nice_float(Dz)]),
    [Info|case Type of
	      spot -> io_lib:format(". Angle ~s~c\n",
				    [wings_util:nice_float(A),?DEGREE]);
	      _ -> []
	  end].

%%%
%%% Light Commands.
%%%

color(St) ->
    Drag = wings_sel:fold(
	     fun(_, #we{id=Id,light=L}=We, none) when ?IS_LIGHT(We) ->
		     {R,G,B,A} = get_light_color(L),
		     {H,S,I} = wings_color:rgb_to_hsi(R, G, B),
		     ColorFun = fun(C, D) -> color(C, D, A) end,
		     Tvs = {general,[{Id,ColorFun}]},
		     Units = [{angle,{0.0,359.9999}},
			      {percent,{0.0,1.0}},{percent,{0.0,1.0}}],
		     Flags = [{initial,[H,I,S]}],
		     {Tvs,Units,Flags};
		(_, We, _) when ?IS_LIGHT(We) ->
		     wings_util:error("Select only one light.");
		(_, _, A) -> A
	     end, none, St),
    case Drag of
	none -> St;
	{Tvs,Units,Flags} ->
	    wings_drag:setup(Tvs, Units, Flags, St)
    end.

color([H,I,S], #dlo{src_we=#we{light=L0}=We0}=D, A) ->
    {R,G,B} = wings_color:hsi_to_rgb(H, S, I),
    Col = {R,G,B,A},
    L = update_color(L0, Col),
    We = We0#we{light=L},
    update(D#dlo{work=none,src_we=We}).


get_light_color(#light{type=ambient,ambient=Col}) -> Col;
get_light_color(#light{diffuse=Diff}) -> Diff.

update_color(#light{type=ambient}=L, Col) -> L#light{ambient=Col};
update_color(L, Col) -> L#light{diffuse=Col}.

position_fun() ->
    fun(help, _) -> "";
       (_, _) -> {vector,{pick_special,position_sel()}}
    end.

position_sel() ->
    {[face,edge,vertex],
     fun(St) ->
	     wings_wm:message_right("Select element to set highlight to"),
	     St#st{selmode=face,sel=[]}
     end,
     fun position_check_selection/1,
     fun(_X, _Y, #st{selmode=Mode,sel=Sel}=St) ->
	     case position_check_selection(St) of
		 {none,""} ->
		     F = fun(_, _) -> {light,{position_highlight,{Mode,Sel}}} end,
		     {"Position Highlight",F};
		 {_,_} ->
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
    {save_state,
     wings_sel:map(fun(_, We) when ?IS_LIGHT(We) ->
			   position_highlight_1(Center, We);
		      (_, We) -> We
		   end, St)}.

position_highlight_1(Center, #we{light=L0}=We) ->
    case L0 of
	#light{type=point} ->
	    move_light(Center, We);
	_ ->
	    L = L0#light{aim=Center},
	    We#we{light=L}
    end.

spot_angle(St) ->
    case selected_light(St) of
	{Id,#light{type=spot,spot_angle=SpotAngle}} ->
	    SpotFun0 = fun([Angle|_], L) -> L#light{spot_angle=Angle} end,
	    SpotFun = adjust_fun(SpotFun0),
	    Tvs = {general,[{Id,SpotFun}]},
	    Units = [{angle,{0.1,89.9}}],
	    Flags = [{initial,[SpotAngle]}],
	    wings_drag:setup(Tvs, Units, Flags, St);
	{_,_} -> wings_util:error("Not a spotlight.")
    end.

spot_falloff(St) ->
    case selected_light(St) of
	{Id,#light{type=spot,spot_exp=SpotExp}} ->
	    SpotFun0 = fun([Exp|_], L) -> L#light{spot_exp=Exp} end,
	    SpotFun = adjust_fun(SpotFun0),
	    Tvs = {general,[{Id,SpotFun}]},
	    Units = [{number,{0.0,128.0}}],
	    Flags = [{initial,[SpotExp]}],
	    wings_drag:setup(Tvs, Units, Flags, St);
	{_,_} -> wings_util:error("Not a spotlight.")
    end.

attenuation(Type, St) ->
    case selected_light(St) of
	{Id,#light{type=Ltype}=L} when Ltype == point; Ltype == spot ->
	    Initial = att_initial(Type, L),
	    Fun = adjust_fun(att_fun(Type)),
	    Tvs = {general,[{Id,Fun}]},
	    Units = [{dx,att_range(Type)}],
	    Flags = [{initial,[Initial]}],
	    wings_drag:setup(Tvs, Units, Flags, St);
	{_,_} -> wings_util:error("Not a point light or spotlight.")
    end.

att_initial(linear, #light{lin_att=LinAtt}) -> LinAtt;
att_initial(quadratic, #light{quad_att=QuadAtt}) -> QuadAtt.

att_fun(linear) -> fun([V|_], L) -> L#light{lin_att=V} end;
att_fun(quadratic) -> fun([V|_], L) -> L#light{quad_att=V} end.

att_range(linear) -> {0.0,1.0};
att_range(quadratic) -> {0.0,0.5}.

selected_light(St) ->
    wings_sel:fold(fun(_, #we{id=Id,light=L}=We, none) when ?IS_LIGHT(We) ->
			   {Id,L};
		      (_, We, _) when ?IS_LIGHT(We) ->
			   wings_util:error("Select only one light.");
		      (_, _, A) -> A
		   end, none, St).

adjust_fun(AdjFun) ->
    fun(Ds, #dlo{src_we=#we{light=L0}=We0}=D) ->
	    L = AdjFun(Ds, L0),
	    We = We0#we{light=L},
	    update(D#dlo{work=none,src_we=We})
    end.

%%
%% The Edit Properties command.
%%
edit(#st{sel=[{Id,_}],shapes=Shs}=St) ->
    We = gb_trees:get(Id, Shs),
    edit_1(We, Shs, St);
edit(_) -> wings_util:error("Select only one light.").

edit_1(#we{id=Id,light=#light{type=ambient,ambient=Amb0}=L0}=We0, Shs, St) ->
    Qs = [{hframe,
	   [{vframe,[{label,"Ambient"}]},
	    {vframe,[{color,Amb0}]}],
	   [{title,"Color"}]}|qs_specific(L0)],
    wings_ask:dialog(Qs,
		     fun([Amb]) ->
			     L = L0#light{ambient=Amb},
			     We = We0#we{light=L},
			     St#st{shapes=gb_trees:update(Id, We, Shs)}
		     end);
edit_1(#we{id=Id,light=L0}=We0, Shs, St) ->
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
		     end).

qs_specific(#light{type=spot,spot_angle=Angle}=L) ->
    Spot = [{vframe,[{label,"Angle"},
		     {slider,{text,Angle,[{range,{0.0,89.9}}]}},
		     {label,"Falloff"},
		     {slider,{text,Angle,[{range,{0.0,128.0}}]}}],
	     [{title,"Spot Parameters"}]}],
    qs_att(L, Spot);
qs_specific(#light{type=point}=L) -> qs_att(L, []);
qs_specific(_) -> [].

qs_att(#light{lin_att=Lin,quad_att=Quad}, Tail) ->
    [{vframe,[{label,"Linear"},
	      {slider,{text,Lin,[{range,{0.0,1.0}}]}},
	      {label,"Quadratic"},
	      {slider,{text,Quad,[{range,{0.0,0.5}}]}}],
      [{title,"Attenuation"}]}|Tail].
    
edit_specific([Angle,LinAtt,QuadAtt], #light{type=spot}=L) ->
    L#light{spot_angle=Angle,lin_att=LinAtt,quad_att=QuadAtt};
edit_specific([LinAtt,QuadAtt], #light{type=point}=L) ->
    L#light{lin_att=LinAtt,quad_att=QuadAtt};
edit_specific(_, L) -> L.

%%%
%%% The Delete command.
%%%

delete(#st{shapes=Shs0}=St) ->
    Shs = wings_sel:fold(fun(_, #we{id=Id}, Shs) ->
				 gb_trees:delete(Id, Shs)
			 end, Shs0, St),
    St#st{shapes=Shs,sel=[]}.

%%%
%%% The Duplicate command.
%%%

duplicate(Dir, St0) ->
    Copy = "copy",
    {St,Sel} = wings_sel:fold(
		 fun(Elems, We, {#st{onext=Id}=S0,Sel}) when ?IS_LIGHT(We) ->
			 S = wings_shape:insert(We, Copy, S0),
			 {S,[{Id,Elems}|Sel]};
		    (_, _, A) -> A
		 end, {St0,[]}, St0),
    wings_move:setup(Dir, wings_sel:set(Sel, St)).
    
%%%
%%% Creating lights.
%%%

create(Type, #st{onext=Oid}=St) ->
    Prefix = atom_to_list(Type),
    Name = Prefix++integer_to_list(Oid),
    import([{Name,[{opengl,[{type,Type}]}]}], St).

%%%
%%% Updating, drawing and rendering lights.
%%%
update_dynamic(#dlo{work=[Work|_],src_we=We0}=D, Vtab0) ->
    Vtab = gb_trees:from_orddict(lists:sort(Vtab0)),
    We = We0#we{vs=Vtab},
    List = update_1(We, D),
    D#dlo{work=[Work,List],src_we=We}.

update_matrix(#dlo{src_we=We0}=D, Matrix) ->
    We = wings_we:transform_vs(Matrix, We0),
    List = update_1(We, D),
    D#dlo{work=List,transparent=We}.

update(#dlo{work=none,src_we=#we{light=#light{}}=We}=D) ->
    List = update_1(We, D),
    D#dlo{work=List,sel=List};
update(#dlo{sel=none,src_we=#we{light=#light{}}=We}=D) ->
    List = update_1(We, D),
    D#dlo{work=List,sel=List};
update(D) -> D.

update_1(#we{light=#light{type=Type}}=We, #dlo{src_sel=SrcSel}) ->
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
    gl:popMatrix();
update_2(ambient, Selected, #we{light=#light{ambient=Amb}}=We) ->
    gl:color4fv(Amb),
    gl:pushMatrix(),
    {X,Y,Z} = light_pos(We),
    gl:translatef(X, Y, Z),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, 0.15, 25, 25),
    set_sel_color(Selected),
    glu:quadricDrawStyle(Obj, ?GLU_LINE),
    glu:sphere(Obj, 0.2, 4, 4),
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
%%% Exporting lights.
%%%

export(#st{shapes=Shs}) ->
    L = foldl(fun(We, A) when ?IS_LIGHT(We) ->
		      [get_light(We)|A];
		 (_, A) -> A
	      end, [], gb_trees:values(Shs)),
    reverse(L).

get_light(#we{name=Name,light=#light{type=ambient,ambient=Amb}}=We) ->
    P = light_pos(We),
    OpenGL = [{type,ambient},{ambient,Amb},{position,P}],
    {Name,[{opengl,OpenGL}]};
get_light(#we{name=Name,light=L}=We) ->
    #light{type=Type,diffuse=Diff,ambient=Amb,specular=Spec,
	   aim=Aim,spot_angle=Angle,spot_exp=SpotExp,
	   lin_att=LinAtt,quad_att=QuadAtt} = L,
    P = light_pos(We),
    Common = [{type,Type},{position,P},{aim_point,Aim},
	      {diffuse,Diff},{ambient,Amb},{specular,Spec}],
    OpenGL0 = case Type of
		  spot ->
		      [{cone_angle,Angle},{spot_exponent,SpotExp}|Common];
		  _ ->
		      Common
	     end,
    OpenGL = if
		 Type == point; Type == spot ->
		     [{linear_attenuation,LinAtt},
		      {quadratic_attenuation,QuadAtt}|OpenGL0];
		 true -> OpenGL0
	     end,
    {Name,[{opengl,OpenGL}]}.

%%%
%%% Importing lights.
%%%

import(Lights, St) ->
    foldl(fun import_fun/2, St, Lights).

import_fun({Name,Ps}, St) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    Type = proplists:get_value(type, OpenGL, point),
    Pos = proplists:get_value(position, OpenGL, {0.0,3.0,0.0}),
    Diff = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    Amb = import_ambient(Type, OpenGL),
    Spec = proplists:get_value(specular, OpenGL, {1.0,1.0,1.0,1.0}),
    Aim = proplists:get_value(aim_point, OpenGL, {0.0,0.0,0.0}),
    LinAtt = proplists:get_value(linear_attenuation, OpenGL, 0.0),
    QuadAtt = proplists:get_value(quadratic_attenuation, OpenGL, 0.0),
    Angle = proplists:get_value(cone_angle, OpenGL, 30.0),
    SpotExp = proplists:get_value(spot_exponent, OpenGL, 0.0),
    L = #light{type=Type,diffuse=Diff,ambient=Amb,specular=Spec,
	       aim=Aim,lin_att=LinAtt,quad_att=QuadAtt,
	       spot_angle=Angle,spot_exp=SpotExp},
    Fs = [[0,3,2,1],[2,3,7,6],[0,4,7,3],[1,2,6,5],[4,5,6,7],[0,1,5,4]],
    Vs = lists:duplicate(8, Pos),
    We0 = wings_we:build(Fs, Vs),
    We = We0#we{light=L},
    wings_shape:new(Name, We, St).

import_ambient(ambient, OpenGL) ->
    proplists:get_value(ambient, OpenGL, {0.1,0.1,0.1,1.0});
import_ambient(_, OpenGL) ->
    proplists:get_value(ambient, OpenGL, {0.0,0.0,0.0,1.0}).

%%%
%%% Setting up lights.
%%%

camera_lights() ->
    setup_lights(camera).

global_lights() ->
    setup_lights(global).

setup_lights(CoordType) ->
    case wings_pref:get_value(scene_lights) of
	false -> 
	    NoOfLights = wings_pref:get_value(number_of_lights),
	    modeling_lights(CoordType, NoOfLights);
	true -> scene_lights(CoordType)
    end.

modeling_lights(global, _NoL) -> ok;
modeling_lights(camera, NoL) ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.1,0.1,0.1,1.0}),
    gl:enable(?GL_LIGHT0),
    gl:disable(?GL_LIGHT2),
    gl:disable(?GL_LIGHT3),
    gl:disable(?GL_LIGHT4),
    gl:disable(?GL_LIGHT5),
    gl:disable(?GL_LIGHT6),
    gl:disable(?GL_LIGHT7),
    case NoL of
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
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.0,0.0,0.0,1.0}),
    Lnum = wings_draw_util:fold(fun scene_lights_fun/2, ?GL_LIGHT0),
    disable_from(Lnum).

disable_from(Lnum) when Lnum > ?GL_LIGHT7 -> ok;
disable_from(Lnum) ->
    gl:disable(Lnum),
    disable_from(Lnum+1).

scene_lights_fun(_, Lnum) when Lnum > ?GL_LIGHT7 -> Lnum;
scene_lights_fun(#dlo{src_we=#we{light=none}}, Lnum) -> Lnum;
scene_lights_fun(#dlo{transparent=#we{}=We}, Lnum) ->
    %% This happens when dragging in Body selection mode.
    scene_lights_fun_1(We, Lnum);
scene_lights_fun(#dlo{src_we=We}, Lnum) when ?IS_LIGHT(We) ->
    scene_lights_fun_1(We, Lnum).

scene_lights_fun_1(#we{light=#light{type=ambient,ambient=Amb}}, Lnum) ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, Amb),
    Lnum;
scene_lights_fun_1(#we{light=L}=We, Lnum) ->
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
    setup_color(Lnum, L),
    setup_attenuation(Lnum, L);
setup_light(Lnum, #light{type=spot,aim=Aim,spot_angle=Angle,spot_exp=Exp}=L, We) ->
    Pos = {X,Y,Z} = light_pos(We),
    Dir = e3d_vec:norm(e3d_vec:sub(Aim, Pos)),
    gl:lightfv(Lnum, ?GL_POSITION, {X,Y,Z,1}),
    gl:lightf(Lnum, ?GL_SPOT_CUTOFF, Angle),
    gl:lightf(Lnum, ?GL_SPOT_EXPONENT, Exp),
    gl:lightfv(Lnum, ?GL_SPOT_DIRECTION, Dir),
    setup_color(Lnum, L),
    setup_attenuation(Lnum, L).

setup_color(Lnum, #light{diffuse=Diff,ambient=Amb,specular=Spec}) ->
    gl:lightfv(Lnum, ?GL_DIFFUSE, Diff),
    gl:lightfv(Lnum, ?GL_AMBIENT, Amb),
    gl:lightfv(Lnum, ?GL_SPECULAR, Spec).

setup_attenuation(Lnum, #light{lin_att=Lin,quad_att=Quad}) ->
    gl:lightf(Lnum, ?GL_LINEAR_ATTENUATION, Lin),
    gl:lightf(Lnum, ?GL_QUADRATIC_ATTENUATION, Quad).

light_pos(#we{vs=Vtab}) ->
    #vtx{pos=Pos} = gb_trees:get(1, Vtab),
    Pos.

move_light(Pos, #we{vs=Vtab0}=We) ->
    Vtab1 = [{V,Vtx#vtx{pos=Pos}} || {V,Vtx} <- gb_trees:to_list(Vtab0)],
    Vtab = gb_trees:from_orddict(Vtab1),
    We#we{vs=Vtab}.
