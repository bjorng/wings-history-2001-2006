%%
%%  wings_light.erl --
%%
%%     Implementation of lights.
%%
%%  Copyright (c) 2002-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_light.erl,v 1.41 2004/01/12 18:40:39 bjorng Exp $
%%

-module(wings_light).
-export([light_types/0,menu/3,command/2,is_any_light_selected/1,info/1,
	 create/2,update_dynamic/2,update_matrix/2,update/1,render/1,
	 modeling_lights/2,global_lights/0,camera_lights/0,
	 export/1,export_camera_lights/0,import/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,foldl/3,member/2,keydelete/3]).

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
	 spot_exp,				%Spot exponent.
	 prop=[]				%Extra properties.
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
	     {NotAmb,{"Position Highlight",
		      {'VALUE',{position_highlight,{'ASK',{[point],[]}}}},
		      "Position the aim point or location of light"}},
	     {NotAmb,{"Color",color,"Interactively adjust hue, value, "
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
	     {One,{"Edit Properties...",edit,"Edit light properties"}},
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
command({position_highlight,Data}, St) ->
    position_highlight(Data, St);
command({attenuation,Type}, St) ->
    attenuation(Type, St);
command(spot_angle, St) ->
    spot_angle(St);
command(spot_falloff, St) ->
    spot_falloff(St);
command(edit, St) ->
    edit(St);
command({edit,Id}, St) ->
    edit(Id, St);
command(delete, St) ->
    {save_state,delete(St)};
command({duplicate,Dir}, St) ->
    duplicate(Dir, St).
    
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
    Info = io_lib:format(". Aim ~s ~s ~s. Dir ~s ~s ~s",
			 [wings_util:nice_float(Ax),
			  wings_util:nice_float(Ay),
			  wings_util:nice_float(Az),
			  wings_util:nice_float(Dx),
			  wings_util:nice_float(Dy),
			  wings_util:nice_float(Dz)]),
    [Info|case Type of
	      spot -> io_lib:format(". Angle ~s~c",
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
		     {H,S,V} = wings_color:rgb_to_hsv(R, G, B),
		     ColorFun = fun(C, D) -> color(C, D, A) end,
		     Tvs = {general,[{Id,ColorFun}]},
		     Units = [{angle,{0.0,359.9999}},
			      {percent,{0.0,1.0}},
			      {percent,{0.0,1.0}}
			     ],
		     Flags = [{initial,[H,V,S]}],
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

color([H,V,S], #dlo{src_we=#we{light=L0}=We0}=D, A) ->
    {R,G,B} = wings_color:hsv_to_rgb(H, S, V),
    Col = {R,G,B,A},
    L = update_color(L0, Col),
    We = We0#we{light=L},
    update(D#dlo{work=none,src_we=We}).


get_light_color(#light{type=ambient,ambient=Col}) -> Col;
get_light_color(#light{diffuse=Diff}) -> Diff.

update_color(#light{type=ambient}=L, Col) -> L#light{ambient=Col};
update_color(L, Col) -> L#light{diffuse=Col}.

position_highlight({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun position_highlight/2);
position_highlight(Center, St) ->
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
edit(#st{sel=[{Id,_}]}=St) ->
    edit(Id, St);
edit(_) -> wings_util:error("Select only one light.").

edit(Id, #st{shapes=Shs}=St) ->
    We = #we{light=#light{type=Type}} = gb_trees:get(Id, Shs),
    {Name,Prop} = get_light(We),
    case Type of
	ambient ->
	    {dialog,Qs,Fun} = edit_ambient_dialog(Name, Prop, We, Shs, St),
	    wings_ask:dialog("Ambient Light Properties", Qs, Fun);
	_ ->
	    {dialog,Qs,Fun} = edit_dialog(Name, Prop, We, Shs, St),
	    wings_ask:dialog("Light Properties", Qs, Fun)
    end.

edit_ambient_dialog(Name, Prop0, 
		    We0=#we{id=Id,light=#light{ambient=Amb0}=L0}, Shs, St) ->
    Qs0 = [{hframe,
	    [{vframe,[{label,"Ambient"}]},
	     {vframe,[{color,Amb0}]}],
	    [{title,"Color"}]}|qs_specific(L0)],
    Qs1 = wings_plugin:dialog({light_editor_setup,Name,Prop0}, Qs0),
    Qs = {hframe,[{vframe,Qs1},
		  {vframe,[{button,"OK",done,[ok,{key,light_editor_ok}]},
			   {button,cancel,[cancel]}]}]},
    Fun = fun([Amb|Res]) ->
		  case plugin_results(Name, Prop0, Res) of
		      {ok,Prop} ->
			  L = L0#light{ambient=Amb,prop=Prop},
			  We = We0#we{light=L},
			  St#st{shapes=gb_trees:update(Id, We, Shs)};
		      {again,Prop} -> edit_ambient_dialog(Name, Prop,
							  We0, Shs, St)
		  end
	  end,
    {dialog,Qs,Fun}.

edit_dialog(Name, Prop0, #we{id=Id,light=L0}=We0, Shs, St) ->
    #light{diffuse=Diff0,ambient=Amb0,specular=Spec0} = L0,
    Qs0 = [{hframe,
	    [{vframe,
	      [{label,"Diffuse"},
	       {label,"Ambient"},
	       {label,"Specular"}]},
	     {vframe,
	      [{color,Diff0},
	       {color,Amb0},
	       {color,Spec0}]}],
	    [{title,"Colors"}]}|qs_specific(L0)],
    Qs1 = wings_plugin:dialog({light_editor_setup,Name,Prop0}, Qs0),
    Qs = {hframe,[{vframe,Qs1},
		  {vframe,[{button,"OK",done,[ok,{key,light_editor_ok}]},
			   {button,cancel,[cancel]}]}]},
    Fun = fun([Diff,Amb,Spec|More0]) ->
		  L1 = L0#light{diffuse=Diff,ambient=Amb,specular=Spec},
		  {L,More} = edit_specific(More0, L1),
		  case plugin_results(Name, Prop0, More) of
		      {ok,Prop} ->
			  We = We0#we{light=L#light{prop=Prop}},
			  St#st{shapes=gb_trees:update(Id, We, Shs)};
		      {again,Prop} -> edit_dialog(Name, Prop, We0, Shs, St)
		  end
	  end,
    {dialog,Qs,Fun}.

plugin_results(Name, Prop0, Res0) ->
    case wings_plugin:dialog_result({light_editor_result,Name,Prop0}, Res0) of
	{Prop,[{light_editor_ok,true}]} ->
	    {ok,keydelete(opengl, 1, Prop)};
	{Prop,[{light_editor_ok,false}]} ->
	    {again,Prop};
	{_,Res} ->
	    io:format("Light editor plugin(s) left garbage:~n    ~P~n", 
		      [Res,20]),
	    wings_util:error("Plugin(s) left garbage")
    end.

qs_specific(#light{type=spot,spot_angle=Angle,spot_exp=SpotExp}=L) ->
    Spot = [{vframe,[{label,"Angle"},
		     {slider,{text,Angle,[{range,{0.0,89.9}}]}},
		     {label,"Falloff"},
		     {slider,{text,SpotExp,[{range,{0.0,128.0}}]}}],
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
    
edit_specific([LinAtt,QuadAtt,Angle,SpotExp|More], #light{type=spot}=L) ->
    {L#light{spot_angle=Angle,spot_exp=SpotExp,lin_att=LinAtt,quad_att=QuadAtt},More};
edit_specific([LinAtt,QuadAtt|More], #light{type=point}=L) ->
    {L#light{lin_att=LinAtt,quad_att=QuadAtt},More};
edit_specific(More, L) -> {L,More}.

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
    We = We0#we{vp=Vtab},
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
    Rot = e3d_mat:rotate_s_to_t({0.0,0.0,1.0}, e3d_vec:neg(SpotDir)),
    gl:multMatrixd(Rot),
    glu:quadricDrawStyle(Obj, ?GLU_LINE),
    glu:cylinder(Obj, R, 0.08, H, 12, 1),
    glu:deleteQuadric(Obj),
    gl:popMatrix();
update_2(ambient, _, _) -> ok.

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

export_camera_lights() ->
    Amb = {"Ambient", camera_ambient()},
    Ls = case wings_pref:get_value(number_of_lights) of
	     1 ->
		 [{"Infinite",camera_infinite_1_0()}];
	     2 ->
		 [{"Infinite1",camera_infinite_2_0()},
		  {"Infinite2",camera_infinite_2_1()}]
	 end,
    #view{origin=Aim} = wings_view:current(),
    CameraPos = wings_view:eye_point(),
    GL = fun({Name,Li = #light{aim=Diff}}) ->
		 LPos = e3d_vec:add(CameraPos,Diff),
		 We = #we{name = Name,
			  vp = gb_trees:from_orddict([{1, LPos}]),
			  light = Li#light{aim=Aim}},
		 get_light(We)
	 end,
    [GL(Light) || Light <- [Amb|Ls]].

get_light(#we{name=Name,perm=P}=We) ->
    Ps0 = get_light_1(We),
    Ps = export_perm(P, Ps0),
    {Name,Ps}.

get_light_1(#we{light=#light{type=ambient,ambient=Amb,prop=Prop}}=We) ->
    P = light_pos(We),
    OpenGL = [{type,ambient},{ambient,Amb},{position,P}],
    [{opengl,OpenGL}|Prop];
get_light_1(#we{light=L}=We) ->
    #light{type=Type,diffuse=Diff,ambient=Amb,specular=Spec,
	   aim=Aim,spot_angle=Angle,spot_exp=SpotExp,
	   lin_att=LinAtt,quad_att=QuadAtt,prop=Prop} = L,
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
    [{opengl,OpenGL}|Prop].

export_perm({_,_}, Ps) ->
    [{visible,false},{locked,false}|Ps];
export_perm(P, Ps) when is_integer(P) ->
    [{visible,P < 2},{locked,(P band 1) =/= 0}|Ps].

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
    Prop = lists:keydelete(opengl, 1, Ps),
    L = #light{type=Type,diffuse=Diff,ambient=Amb,specular=Spec,
	       aim=Aim,lin_att=LinAtt,quad_att=QuadAtt,
	       spot_angle=Angle,spot_exp=SpotExp,prop=Prop},
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

camera_ambient() ->
    #light{type = ambient, 
	   aim = {0.0,0.0,0.0},
	   ambient = {0.1,0.1,0.1,1.0}}.
camera_infinite_1_0() ->
    #light{type = infinite, 
	   diffuse  = {0.7,0.7,0.7,1},
	   specular = {0.2,0.2,0.2,1},
	   ambient  = {0,0,0,1.0},
	   aim      = {0.110,0.0,0.994}
	  }.
camera_infinite_2_0() ->
    #light{type = infinite, 
	   diffuse  = {1,1,1,1},
	   specular = {0.3,0.3,0.3,1},
	   ambient  = {0,0,0,1.0},
	   aim      = {0.71,0.71,0.0}
	  }.
camera_infinite_2_1() ->
    #light{type = infinite, 
	   diffuse  = {0.5,0.5,0.5,0.5},
	   specular = {0.3,0.3,0.3,1},
	   ambient  = {0,0,0,1.0},
	   aim      = {-0.71,-0.71,0.0}
	  }.


infinite({X,Y,Z}) ->
    {X,Y,Z,0.0};
infinite(V) -> V.

modeling_lights(global, _Type) -> ok;
modeling_lights(camera, Type) ->
    gl:enable(?GL_LIGHT0),
    gl:disable(?GL_LIGHT2),
    gl:disable(?GL_LIGHT3),
    gl:disable(?GL_LIGHT4),
    gl:disable(?GL_LIGHT5),
    gl:disable(?GL_LIGHT6),
    gl:disable(?GL_LIGHT7),
    Amb = camera_ambient(),
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, Amb#light.ambient),
    case Type of
	1 ->
	    L = camera_infinite_1_0(),
	    gl:disable(?GL_LIGHT1),
	    gl:lightfv(?GL_LIGHT0, ?GL_DIFFUSE,  L#light.diffuse),
	    gl:lightfv(?GL_LIGHT0, ?GL_SPECULAR, L#light.specular),
	    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, infinite(L#light.aim));     
	2 ->
	    L20 = camera_infinite_2_0(),
	    L21 = camera_infinite_2_1(),
	    gl:enable(?GL_LIGHT1),
	    gl:lightfv(?GL_LIGHT0, ?GL_DIFFUSE,  L20#light.diffuse), 
	    gl:lightfv(?GL_LIGHT0, ?GL_SPECULAR, L20#light.specular),
	    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, infinite(L20#light.aim)),
	    gl:lightfv(?GL_LIGHT1, ?GL_DIFFUSE,  L21#light.diffuse), 
	    gl:lightfv(?GL_LIGHT1, ?GL_SPECULAR, L21#light.specular),
	    gl:lightfv(?GL_LIGHT1, ?GL_POSITION, infinite(L21#light.aim));     
	mat_preview ->
	    D = 0.8,
	    S = 0.7,
	    gl:disable(?GL_LIGHT1),
	    gl:lightfv(?GL_LIGHT0, ?GL_DIFFUSE, {D,D,D,1}),
 	    gl:lightfv(?GL_LIGHT0, ?GL_SPECULAR, {S,S,S,1}),
	    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {1,1,2,0})
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

light_pos(#we{vp=Vtab}) ->
    gb_trees:get(1, Vtab).

move_light(Pos, #we{vp=Vtab0}=We) ->
    Vtab1 = [{V,Pos} || V <- gb_trees:keys(Vtab0)],
    Vtab = gb_trees:from_orddict(Vtab1),
    We#we{vp=Vtab}.
