%% File    : wpc_autouv.erl
%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%% Description : A semi-simple semi-automatic UV-mapping semi-plugin
%%
%% Created : 24 Jan 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%-------------------------------------------------------------------
%%  Copyright (c) 2002-2004 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: wpc_autouv.erl,v 1.230 2004/05/07 04:18:45 bjorng Exp $

-module(wpc_autouv).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
 
-export([init/0,menu/2,command/2,redraw/1,window/1]).

-import(lists, [sort/1,map/2,foldl/3,reverse/1,
		append/1,delete/2,usort/1,max/1,min/1,
		member/2,foreach/2,keysearch/3]).

%% Exports to auv_texture
-export([get_material/3,has_texture/2]).

init() ->
    true.

menu({body}, Menu) ->
    case wpc_snap:active() of
	true ->
	    Menu;
	false ->
	    Menu ++ [separator,
		     {"UV Mapping", ?MODULE,
		      "Generate or edit UV mapping or texture"}
		    ]
    end;

menu(_Dbg, Menu) ->
    Menu.

command({body,?MODULE}, St) ->
    start_uvmap(segment, St);
command(_, _) -> next.

window(St) ->
    start_uvmap(edit, St).

start_uvmap(Action, #st{sel=Sel}=St) ->
    start_uvmap_1(Sel, Action, St).

start_uvmap_1([{Id,_}|T], Action, St) ->
    Name = {autouv,Id},
    case wings_wm:is_window(Name) of
	true -> wings_wm:raise(Name);
	false -> start_uvmap_2(Action, Name, Id, St)
    end,
    start_uvmap_1(T, Action, St);
start_uvmap_1([], _, _) -> keep.

start_uvmap_2(Action, Name, Id, #st{shapes=Shs}=St) ->
    #we{name=ObjName} = We = gb_trees:get(Id, Shs),
    Op = {push,fun(Ev) -> auv_event(Ev, St) end},
    Title = "AutoUV: " ++ ObjName,
    {X,Y,W,H} = init_drawarea(),
    Props = [{display_lists,Name}|wings_view:initial_properties()],
    CreateToolbar = fun(N, P, Wi) -> wings:create_toolbar(N, P, Wi) end,
    wings_wm:toplevel(Name, Title, {X,Y,highest}, {W,H},
		      [resizable,closable,menubar,{properties,Props},
		       {toolbar,CreateToolbar}], Op),
    wings_wm:send(Name, {init,{Action,We}}).

auv_event({init,Op}, St) ->
    wings:init_opengl(St),
    case Op of
	{edit,We} ->
	    start_edit(We, St);
	{segment,We} ->
	    case wings_we:uv_mapped_faces(We) of
		[] -> auv_seg_ui:start(We, We, St);
		_Faces -> start_edit(We, St)
	    end
    end;
auv_event({init_show_maps,Id,Map}, #st{shapes=Shs}=St) ->
    We = gb_trees:get(Id, Shs),
    init_show_maps(Map, We, St);
auv_event(redraw, _) ->
    wings_wm:clear_background(),
    keep;
auv_event(_Ev, _) -> keep.

%%%
%%% Start the UV editor.
%%%

start_edit(#we{fs=Ftab}=We, St) ->
    MatNames0 = wings_material:get_all(We),
    MatNames1 = sofs:from_external(MatNames0, [{face,material}]),
    MatNames2 = sofs:converse(MatNames1),
    MatNames3 = sofs:relation_to_family(MatNames2),
    MatNames4 = sofs:to_external(MatNames3),
    MatNames = [Mat || {Name,_}=Mat <- MatNames4, has_texture(Name, St)],
    case MatNames of
	[{MatName,Faces}] ->
	    do_edit(MatName, Faces, We, St);
	_ ->
	    Faces = gb_trees:keys(Ftab),
	    do_edit(none, Faces, We, St)
    end.

do_edit(MatName, Faces, We, St) ->
    Charts = init_edit(Faces, We),
    create_uv_state(Charts, MatName, We, St).

init_show_maps(Map0, We, St) ->
    Map1 = auv_placement:place_areas(Map0),
    Map = gb_trees:from_orddict(sort(Map1)),
    create_uv_state(Map, none, We, St).

create_uv_state(Charts, MatName0, We, GeomSt0) ->
    wings:mode_restriction([vertex,edge,face,body]),
    wings_wm:current_state(#st{selmode=body,sel=[]}),
    {GeomSt1,MatName} = 
	case has_texture(MatName0, GeomSt0) of
	    true ->
		{GeomSt0,MatName0};
	    false ->
		Tx = checkerboard(128, 128),
		add_material(Tx, We#we.name, MatName0, GeomSt0)
	end,
    GeomSt = insert_initial_uvcoords(Charts, We#we.id, MatName, GeomSt1),
    wings_wm:send(geom, {new_state,GeomSt}),
    Uvs = #uvstate{st=wpa:sel_set(face, [], GeomSt),
		   id=We#we.id,
		   matname=MatName},
    St = GeomSt#st{selmode=body,sel=[],shapes=Charts,bb=Uvs},
    Name = wings_wm:this(),

    View = #view{origin={0.0,0.0,0.0},
		 distance=0.65,
		 azimuth=0.0,
		 elevation=0.0,
		 pan_x=-0.5,
		 pan_y=-0.5,
		 fov=90.0,
		 hither=0.1,
		 yon=50.0},
    wings_view:set_current(View),

    wings_wm:set_prop(Name, drag_filter, fun drag_filter/1),
    wings_wm:set_prop(show_wire_backfaces, true),
    wings_wm:set_prop(show_info_text, false),
    wings_wm:set_prop(orthogonal_view, true),
    wings_wm:set_prop(show_axes, false),
    wings_wm:set_prop(show_groundplane, false),
    wings_wm:set_prop(wireframed_objects,
		      gb_sets:from_list(gb_trees:keys(Charts))),
    wings_wm:set_prop(allow_rotation, false),

    wings_wm:later(got_focus),

    Win = wings_wm:this(),
    wings:register_postdraw_hook(Win, ?MODULE,
				 fun draw_background/1),
    wings_wm:menubar(Win, menubar()),
    wings_wm:send({menubar,Win}, {current_state,St}),

    get_event(St).

menubar() ->
    [{"Select",select,fun(St) ->
			      Menu0 = wings_sel_cmd:menu(St),
			      Menu = [I || I <- Menu0,
					   keep_sel_item(I)],
			      redundant_separators(Menu)
		      end}].

keep_sel_item(separator) -> true;
keep_sel_item({_,deselect,_}) -> true;
keep_sel_item({_,hide_selected,_}) -> true;
keep_sel_item({_,hide_unselected,_}) -> true;
keep_sel_item({_,show_all,_}) -> true;
keep_sel_item(_) -> false.

redundant_separators([]) -> [];
redundant_separators([separator]) -> [];
redundant_separators([separator|[separator|_]=T]) ->
    redundant_separators(T);
redundant_separators([H|T]) ->
    [H|redundant_separators(T)].

insert_initial_uvcoords(Charts, Id, MatName, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    We1 = update_uvs(gb_trees:values(Charts), We0),
    We2 = insert_material(Charts, MatName, We1),
    We = We2#we{mode=material},    
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{shapes=Shs}.
   
update_selected_uvcoords(#st{bb=Uvs}=St) ->
    Charts = wpa:sel_fold(fun(_, We, Acc) -> [We|Acc] end, [], St),
    #uvstate{st=#st{shapes=Shs0}=GeomSt0,id=Id} = Uvs,
    We0 = gb_trees:get(Id, Shs0),
    We = update_uvs(Charts, We0),    
    Shs = gb_trees:update(Id, We, Shs0),
    GeomSt = GeomSt0#st{shapes=Shs},
    wings_wm:send(geom, {new_state,GeomSt}),
    St#st{bb=Uvs#uvstate{st=GeomSt}}.

%% update_uvs(Charts, We0) -> We
%%  Update the UV coordinates for the original model.
update_uvs(Cs, #we{es=Etab0}=GeomWe) ->
    update_uvs_1(Cs, GeomWe, Etab0).

update_uvs_1([#we{vp=Vpos0,name=#ch{vmap=Vmap,fm_a2g=A2G0}}=ChartWe|Cs],
	     We, Etab0) ->
    A2G = gb_trees:from_orddict(sofs:to_external(A2G0)),
    VFace0 = wings_face:fold_faces(
	       fun(Face, V, _, _, A) ->
		       [{V,gb_trees:get(Face, A2G)}|A]
	       end, [],
	       wings_we:visible(ChartWe), ChartWe),
    VFace1 = sofs:relation(VFace0),
    VFace2 = sofs:relation_to_family(VFace1),
    VFace = sofs:to_external(VFace2),
    Vpos = gb_trees:to_list(Vpos0),
    Etab = update_uvs_2(Vpos, VFace, Vmap, We, Etab0),
    update_uvs_1(Cs, We, Etab);
update_uvs_1([], We, Etab) -> We#we{es=Etab}.

update_uvs_2([{V0,{X,Y,_}}|Vs], [{V0,Fs}|VFs], Vmap, We, Etab0) ->
    UV = {X,Y},
    V = auv_segment:map_vertex(V0, Vmap),
    Etab = foldl(fun(Face, A) ->
			 update_uvs_3(V, Face, UV, A, We)
		 end, Etab0, Fs),
    update_uvs_2(Vs, VFs, Vmap, We, Etab);
update_uvs_2([{V0,none}|Vs], [{V0,Fs}|VFs], Vmap, We, Etab0) ->
    V = auv_segment:map_vertex(V0, Vmap),
    Etab = foldl(fun(Face, A) ->
			 update_uvs_3(V, Face, none, A, We)
		 end, Etab0, Fs),
    update_uvs_2(Vs, VFs, Vmap, We, Etab);
update_uvs_2([], [], _, _, Etab) -> Etab.

update_uvs_3(V, Face, UV, Etab, We) ->
    wings_vertex:fold(
      fun(Edge, _, Rec0, Et) ->
	      case Rec0 of
		  #edge{vs=V,lf=Face,a=A} when A =/= UV ->
		      Rec = gb_trees:get(Edge, Et),
		      gb_trees:update(Edge, Rec#edge{a=UV}, Et);
		  #edge{ve=V,rf=Face,b=B} when B =/= UV ->
		      Rec = gb_trees:get(Edge, Et),
		      gb_trees:update(Edge, Rec#edge{b=UV}, Et);
		  _ -> Et
	      end
      end, Etab, V, We).

insert_material(Cs, MatName, We) ->
    Faces = lists:append([wings_we:visible(W) || W <- gb_trees:values(Cs)]),
    wings_material:assign(MatName, Faces, We).

init_edit(Faces0, We0) ->
    Faces = [F || F <- Faces0, has_proper_uvs(F, We0)],
    FvUvMap = auv_segment:fv_to_uv_map(Faces, We0),
    {Charts1,Cuts} = auv_segment:uv_to_charts(Faces, FvUvMap, We0),
    Charts2 = auv_segment:cut_model(Charts1, Cuts, We0),
    Charts = build_map(Charts2, FvUvMap, 1, []),
    gb_trees:from_orddict(sort(Charts)).

has_proper_uvs(Face, #we{mirror=Face}) -> false;
has_proper_uvs(Face, We) ->
    foldl(fun({_,_}, F) -> F;
	     (_, _) -> false
	  end, true, wings_face:vertex_info(Face, We)).

build_map([{Fs,Vmap,#we{fs=Ftab}=We0}|T], FvUvMap, No, Acc) ->
    %% XXX Because auv_segment:cut_model/3 distorts the UV coordinates
    %% (bug in wings_vertex_cmd), we must fetch the UV coordinates
    %% from the original object.
    UVs0 = wings_face:fold_faces(
	     fun(F, V, _, _, A) ->
		     OrigV = auv_segment:map_vertex(V, Vmap),
		     UV = gb_trees:get({F,OrigV}, FvUvMap),
		     [{V,UV}|A]
	     end, [], Fs, We0),
    UVs1 = ordsets:from_list(UVs0),
    %% Assertion.
    true = sofs:is_a_function(sofs:relation(UVs1, [{atom,atom}])),
    Z = zero(),
    UVs = [{V,{X,Y,Z}} || {V,{X,Y}} <- UVs1],
    Fs = sort(Fs),
    HiddenFaces = ordsets:subtract(gb_trees:keys(Ftab), Fs),
    We1 = wings_we:hide_faces(HiddenFaces, We0),
    A2G = auv_util:make_face_map(Fs, We1),
    Chart = #ch{size=undefined,vmap=Vmap,fm_a2g=A2G},
    We = We1#we{name=Chart,id=No,vp=gb_trees:from_orddict(UVs)},
    build_map(T, FvUvMap, No+1, [{No,We}|Acc]);
build_map([], _, _, Acc) -> Acc.

zero() ->
    0.0.

%%%%% Material handling

has_texture(MatName, #st{mat=Materials}) ->
    has_texture(MatName, Materials);
has_texture(MatName, Materials) ->
    case gb_trees:lookup(MatName, Materials) of
	none -> false;
	{value,Mat} ->
	    Maps = proplists:get_value(maps, Mat, []),
	    none /= proplists:get_value(diffuse, Maps, none)
    end.

get_material(Face, Materials, We) ->
    MatName = wings_material:get(Face, We),
    Mat = gb_trees:get(MatName, Materials),
    proplists:get_value(diffuse, proplists:get_value(opengl, Mat)).

add_material(Tx = #e3d_image{},Name,none,St0) ->
    MatName0 = list_to_atom(Name++"_auv"),
    Mat = {MatName0,[{opengl,[]},{maps,[{diffuse,Tx}]}]},
    case wings_material:add_materials([Mat], St0) of
	{St,[]} ->
	    {St,MatName0};
	{St,[{MatName0,MatName}]} ->
	    {St,MatName}
    end;
add_material(Im = #e3d_image{}, _, MatName,St) ->
    wings_material:update_image(MatName, diffuse, Im, St),
    {St,MatName}.
    
%%%% Menus.

command_menu(body, X, Y) ->
    Rotate = [{"Free", free, "Rotate freely"},
	      {"90"++[?DEGREE]++" CW",-90,
	       "Rotate selection 90 degrees clockwise"},
	      {"90"++[?DEGREE]++" CCW",90,
	       "Rotate selection 90 degrees counter-clockwise"},
	      {"180"++[?DEGREE],180,"Rotate selection 180 degrees"},
	      separator,
	      {"Flip Horizontal",flip_horizontal,"Flip selection horizontally"},
	      {"Flip Vertical",flip_vertical,"Flip selection vertically"}],
    Scale =  [{"Uniform",    scale_uniform, "Scale in both directions"},
	      {"Horizontal", scale_x, "Scale horizontally (X dir)"},
	      {"Vertical",   scale_y, "Scale vertically (Y dir)"}],

    Menu = [{basic,{"Chart operations",ignore}},
	    {basic,separator},
	    {"Move", move, "Move selected charts"},
	    {"Scale", {scale, Scale}, "Scale selected charts"},
	    {"Rotate", {rotate, Rotate}, "Rotate selected charts"},
	    separator,
	    {"Tighten",tighten,
	     "Move UV coordinates towards average midpoint"},
	    separator,
	    {"Delete",delete,"Remove UV-coordinates for the selected charts"},
	    separator,
	    {"ReMap UV", {remap, [{"Project Normal", project, 
				   "Project UVs from chart normal"},
				  {"Unfold", lsqcm, "Unfold the chart"},
				  separator,
				  {"Stretch optimization", stretch_opt, 
				   "Optimize the chart stretch"}
				 ]}, 
	     "Calculate new UVs with choosen algorithm"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(face, X, Y) ->
    Menu = [{basic,{"Face operations",ignore}},
	    {"Move",move,"Move selected faces"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(edge, X, Y) ->
    Menu = [{basic,{"Edge operations",ignore}},
	    {basic,separator},
	    {"Move",move,"Move selected edges"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(vertex, X, Y) ->
    Scale =  [{"Uniform",    scale_uniform, "Scale in both directions"},
	      {"Horizontal", scale_x, "Scale horizontally (X dir)"},
	      {"Vertical",   scale_y, "Scale vertically (Y dir)"}],
    Menu = [{basic,{"Vertex operations",ignore}},
	    {basic,separator},
	    {"Move",move,"Move selected vertices"},
	    {"Scale",{scale,Scale},"Scale selected vertices"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu).

option_menu() ->
    [separator,
     {"Create Texture",create_texture,"Make and Attach a texture to the model"}].

%%% Event handling

get_event(#st{}=St) ->
    wings_draw:refresh_dlists(St),
    wings_wm:dirty(),
    get_event_nodraw(St).

get_event_nodraw(#st{}=St) ->
    wings_wm:current_state(St),
    {replace,fun(Ev) -> handle_event(Ev, St) end}.

handle_event(redraw, St) ->
    redraw(St),
    get_event_nodraw(St);
handle_event(init_opengl, St) ->
    wings:init_opengl(St),
    get_event(St);
handle_event(resized, St) ->
    get_event(St);
handle_event({new_state,St}, _) ->
    new_state(St);
handle_event(Ev, St) ->
    case wings_camera:event(Ev, fun() -> redraw(St) end) of
	next -> handle_event_0(Ev, St);
	Other -> Other
    end.

handle_event_0(Ev, St) ->
    case wings_pick:event(Ev, St) of
	next -> handle_event_1(Ev, St);
	Other -> Other
    end.

handle_event_1({current_state,geom_display_lists,GeomSt}, AuvSt) ->
    new_geom_state(GeomSt, AuvSt);
handle_event_1(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_RIGHT,x=X0,y=Y0},
	       #st{selmode=Mode}) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    command_menu(Mode, X, Y);
handle_event_1(#keyboard{state=?SDL_PRESSED,sym=?SDLK_SPACE}, _) ->
    wings_wm:later({action,{select,deselect}});
handle_event_1({drop,_,DropData}, St) ->
    handle_drop(DropData, St);
%% Create Texture (see auv_texture)
handle_event_1({action,{auv,create_texture}},_St) ->
    auv_texture:draw_options();
handle_event_1({action,{auv,{draw_options,Opt}}}, #st{bb=Uvs}=St) ->
    #uvstate{st=GeomSt0,matname=MatName0} = Uvs,
    Tx = ?SLOW(auv_texture:get_texture(St, Opt)),
    {GeomSt,MatName} = add_material(Tx, undefined, MatName0, GeomSt0),
    wings_wm:send(geom, {new_state,GeomSt}),
    get_event(St#st{bb=Uvs#uvstate{st=GeomSt,matname=MatName}});
handle_event_1({action,{auv,{remap,Method}}}, St0) ->
    St = remap(Method, St0),
    get_event(St);

%% Others
handle_event_1({action,{auv,quit}}, _St) ->
    restore_wings_window(),
    delete;
handle_event_1(close, _St) ->
    restore_wings_window(),
    delete;
handle_event_1({callback,Fun}, _) when is_function(Fun) ->
    Fun();
handle_event_1({action,{auv,Cmd}}, St) ->
    handle_command(Cmd, St);
handle_event_1({action,{select,Command}}, St0) ->
    case wings_sel_cmd:command(Command, St0) of
	{save_state,St} -> ok;
	#st{}=St -> ok
    end,
    new_state(St);
handle_event_1(got_focus, _) ->
    Msg1 = wings_util:button_format("Select"),
    Msg2 = wings_camera:help(),
    Msg3 = wings_util:button_format([], [], "Show menu"),
    Message = wings_util:join_msg([Msg1,Msg2,Msg3]),
    wings_wm:message(Message),
    wings_wm:dirty();
handle_event_1(_Event, St) ->
    ?DBG("Got unhandled Event ~p ~n", [_Event]),
    get_event(St).

new_state(#st{bb=#uvstate{}=Uvs}=St0) ->
    GeomSt = update_geom_selection(St0),
    St1 = St0#st{bb=Uvs#uvstate{st=GeomSt}},
    St = update_selected_uvcoords(St1),
    get_event(St).
%%    get_event(St#st{selmode=body,sh=false}).

handle_command(move, St) ->
    drag(wings_move:setup(free_2d, St));
handle_command({scale,scale_uniform}, St) ->
    drag(wings_scale:setup({uniform,center}, St));
handle_command({scale,scale_x}, St) ->
    drag(wings_scale:setup({x,center}, St));
handle_command({scale,scale_y}, St) ->
    drag(wings_scale:setup({y,center}, St));
handle_command({rotate,free}, St) ->
    drag(wings_rotate:setup({free,center}, St));
handle_command({rotate,flip_horizontal}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> flip_horizontal(We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command({rotate,flip_vertical}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> flip_vertical(We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command({rotate,Deg}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> rotate_chart(Deg, We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command(tighten, St) ->
    tighten(St);
handle_command(delete, St) ->
    get_event(delete_charts(St));
handle_command(_, #st{sel=[]}) ->
    keep.

drag({drag,Drag}) ->
    wings_wm:set_prop(show_info_text, true),
    wings_drag:do_drag(Drag, none).

tighten(St) ->
    Tvs = wings_sel:fold(fun tighten/3, [], St),
    {drag,Drag} = wings_drag:setup(Tvs, [percent], St),
    wings_drag:do_drag(Drag, none).

tighten(_, #we{vp=Vtab}=We, A) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    Vs = [V || V <- gb_trees:keys(Vtab), not_bordering(V, Vis, We)],
    wings_vertex_cmd:tighten(Vs, We, A).

not_bordering(V, Vis, We) ->
    wings_vertex:fold(fun(_, _, _, false) -> false;
			 (_, F, _, true) -> gb_sets:is_member(F, Vis)
		      end, true, V, We).

delete_charts(#st{shapes=Shs0}=St0) ->
    St1 = wpa:sel_map(fun(_, #we{vp=Vp0}=We) ->
			      Vp1 = gb_trees:to_list(Vp0),
			      Vp = [{V,none} || {V,_} <- Vp1],
			      We#we{vp=gb_trees:from_orddict(Vp)}
		      end, St0),
    St = update_selected_uvcoords(St1),
    Shs = wpa:sel_fold(fun(_, #we{id=Id}, Shs) ->
			       gb_trees:delete(Id, Shs)
		       end, Shs0, St),
    St#st{shapes=Shs,sel=[]}.

drag_filter({image,_,_}) ->
    {yes,"Drop: Change the texture image"};
drag_filter(_) -> no.

handle_drop({image,_,#e3d_image{width=W,height=H}=Im}, #st{bb=Uvs0}=St) ->
    case W =:= H andalso is_power_of_two(W) of
	false -> keep;
	true ->
	    #uvstate{st=GeomSt0,matname=MatName0} = Uvs0,
	    {GeomSt,MatName} = add_material(Im, undefined, MatName0, GeomSt0),
	    wings_wm:send(geom, {new_state,GeomSt}),
	    Uvs = Uvs0#uvstate{st=GeomSt,matname=MatName},
	    get_event(St#st{bb=Uvs})
    end;
handle_drop(_DropData, _) ->
    %%io:format("~P\n", [_DropData,40]),
    keep.

is_power_of_two(X) ->
    (X band -X ) == X.

%% update_selection(GemoSt, AuvSt0) -> AuvSt
%%  Update the selection in the AutoUV window given a selection
%%  from a geometry window.
update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #st{bb=#uvstate{st=#st{selmode=Mode,sel=Sel}}=Uvs}=AuvSt) ->
    get_event_nodraw(AuvSt#st{bb=Uvs#uvstate{st=St}});
update_selection(#st{selmode=Mode,sel=Sel}=St0,
		 #st{selmode=AuvMode,bb=#uvstate{id=Id}=Uvs,
		     shapes=Charts0}=AuvSt0) ->
    Charts = gb_trees:to_list(Charts0),
    case keysearch(Id, 1, Sel) of
	false ->
	    %% No selection in any chart - clear selection.
	    AuvSt = reset_sel(AuvSt0#st{bb=Uvs#uvstate{st=St0}}),
	    get_event(AuvSt);
	{value,{Id,Elems}} when AuvMode == body ->
	    %% Body selection in charts - must be speciallay handled.
	    NewSel = update_body_sel(Mode, Elems, Charts),
	    AuvSt = AuvSt0#st{sel=sort(NewSel),sh=false,
			      bb=Uvs#uvstate{st=St0}},
	    get_event(AuvSt);
	{value,{Id,Elems0}} when AuvMode =:= Mode->
	    %% Same selection mode in Geometry and AutoUV.
	    Elems = gb_sets:to_list(Elems0),
	    NewSel = update_selection_1(AuvMode, Elems, Charts),
	    AuvSt = AuvSt0#st{sel=sort(NewSel),sh=false,
			      bb=Uvs#uvstate{st=St0}},
	    get_event(AuvSt);
	{value,IdElems} ->
	    %% Different selection modes. Convert Geom selection to
	    %% the mode in AutoUV.
	    St = St0#st{sel=[IdElems]},
	    #st{sel=[{Id,Elems0}]} = wings_sel:convert_selection(AuvMode, St),
	    Elems = gb_sets:to_list(Elems0),
	    NewSel = update_selection_1(AuvMode, Elems, Charts),
	    AuvSt = AuvSt0#st{sel=sort(NewSel),sh=false,
			      bb=Uvs#uvstate{st=St0}},
	    get_event(AuvSt)
    end.

update_selection_1(vertex, Vs, Charts) ->
    vertex_sel_to_vertex(Charts, Vs, []);
update_selection_1(edge, Es, Charts) ->
    edge_sel_to_edge(Charts, Es, []);
update_selection_1(face, Faces, Charts) ->
    face_sel_to_face(Charts, Faces, []).

face_sel_to_face([{K,We}|Cs], Faces, Sel) ->
    ChartFaces = auv2geom_faces(wings_we:visible(We), We),
    case ordsets:intersection(ChartFaces, Faces) of
 	[] ->
	    face_sel_to_face(Cs, Faces, Sel);
 	FaceSel0 ->
	    FaceSel1 = geom2auv_faces(FaceSel0, We),
	    FaceSel = gb_sets:from_list(FaceSel1),
	    face_sel_to_face(Cs, Faces, [{K,FaceSel}|Sel])
    end;
face_sel_to_face([], _, Sel) -> Sel.

vertex_sel_to_vertex([{K,#we{vp=Vtab}=We}|Cs], Vs, Sel) ->
    ChartVs = auv2geom_vs(gb_trees:keys(Vtab), We),
    case ordsets:intersection(ChartVs, Vs) of
 	[] ->
	    vertex_sel_to_vertex(Cs, Vs, Sel);
 	VertexSel0 ->
	    VertexSel1 = geom2auv_vs(VertexSel0, We),
	    VertexSel = gb_sets:from_list(VertexSel1),
	    vertex_sel_to_vertex(Cs, Vs, [{K,VertexSel}|Sel])
    end;
vertex_sel_to_vertex([], _, Sel) -> Sel.

edge_sel_to_edge([{K,#we{es=Etab}=We}|Cs], Es, Sel) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    ChartEs0 = [E || {E,#edge{lf=Lf,rf=Rf}} <- gb_trees:to_list(Etab),
		     gb_sets:is_member(Lf, Vis) orelse
			 gb_sets:is_member(Rf, Vis)],
    ChartEs = auv2geom_edges(ChartEs0, We),
    case ordsets:intersection(ChartEs, Es) of
 	[] ->
	    edge_sel_to_edge(Cs, Es, Sel);
	EdgeSel0 ->
	    EdgeSel1 = geom2auv_edges(EdgeSel0, We),
	    EdgeSel = gb_sets:from_list(EdgeSel1),
	    edge_sel_to_edge(Cs, Es, [{K,EdgeSel}|Sel])
    end;
edge_sel_to_edge([], _, Sel) -> Sel.

%% update_body_sel(SelModeInGeom, Elems, Charts) -> Selection
%%  Convert the selection from the geoemetry window to
%%  a body selection in the AutoUV window.

update_body_sel(face, Elems, Charts) ->
    face_sel_to_body(Charts, Elems, []);
update_body_sel(body, _, Charts) ->
    body_sel_to_body(Charts, []);
update_body_sel(Mode, _, _) ->
    io:format("~p: ~p\n", [?LINE,Mode]),
    [].

face_sel_to_body([{K,We}|Cs], Faces, Sel) ->
    Fs = wings_we:visible(We),
    case ordsets:intersection(sort(Fs), Faces) of
 	[] ->
	    face_sel_to_body(Cs, Faces, Sel);
 	_ ->
	    Zero = gb_sets:singleton(0),
	    face_sel_to_body(Cs, Faces, [{K,Zero}|Sel])
    end;
face_sel_to_body([], _, Sel) -> Sel.

body_sel_to_body([{K,_}|Cs], Sel) ->
    body_sel_to_body(Cs, [{K,gb_sets:singleton(0)}|Sel]);
body_sel_to_body([], Sel) -> Sel.
    
%% update_geom_selection(AuvSt)
%%  Given the selection in the AutoUV window, update the selection
%%  in the geometry window.

update_geom_selection(#st{sel=[],bb=#uvstate{st=GeomSt}}) ->
    wpa:sel_set(face, [], GeomSt);
update_geom_selection(#st{selmode=body,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(_, We, A) ->
			       Fs0 = wings_we:visible(We),
			       Fs = auv2geom_faces(Fs0, We),
			       Fs++A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(face, [{Id,Fs}], GeomSt);
update_geom_selection(#st{selmode=face,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(Fs, We, A) ->
			       auv2geom_faces(gb_sets:to_list(Fs), We)++A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(face, [{Id,Fs}], GeomSt);
update_geom_selection(#st{selmode=edge,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Es0 = wpa:sel_fold(fun(Es, We, A) ->
			       auv2geom_edges(gb_sets:to_list(Es), We)++A
		       end, [], St#st{sel=Sel}),
    Es = gb_sets:from_list(Es0),
    wpa:sel_set(edge, [{Id,Es}], GeomSt);
update_geom_selection(#st{selmode=vertex,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(Vs, We, A) ->
			       auv2geom_vs(gb_sets:to_list(Vs), We) ++ A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(vertex, [{Id,Fs}], GeomSt).

reset_sel(St0) ->
    wings_sel:reset(St0).
%     case wings_sel:reset(St0) of
% 	#st{selmode=body,sh=false}=St -> St;
% 	St -> St#st{selmode=body,sh=false}
%    end.

%%%% GUI Operations

rotate_chart(Angle, We) ->
    Center = wings_vertex:center(We),
    Rot0 = e3d_mat:translate(e3d_vec:neg(Center)),
    Rot1 = e3d_mat:mul(e3d_mat:rotate(float(trunc(Angle)), {0.0,0.0,1.0}), Rot0),
    Rot = e3d_mat:mul(e3d_mat:translate(Center), Rot1),
    wings_we:transform_vs(Rot, We).

flip_horizontal(We) ->
    flip(e3d_mat:scale(-1.0, 1.0, 1.0), We).

flip_vertical(We) ->
    flip(e3d_mat:scale(1.0, -1.0, 1.0), We).

flip(Flip, We) ->
    Center = wings_vertex:center(We),
    T0 = e3d_mat:translate(e3d_vec:neg(Center)),
    T1 = e3d_mat:mul(Flip, T0),
    T = e3d_mat:mul(e3d_mat:translate(Center), T1),
    wings_we:transform_vs(T, We).


remap(Method, #st{sel=Sel}=St0) ->
    wings_pb:start("remapping"),
    wings_pb:update(0.001),
    N = length(Sel),
    {St,_} = wings_sel:mapfold(fun(_, We, I) ->
				       Msg = "chart " ++ integer_to_list(I),
				       wings_pb:update(I/N, Msg),
				       {remap(Method, We, St0),I+1}
			       end, 1, St0),
    wings_pb:done(update_selected_uvcoords(St)).

remap(stretch_opt, We, St) ->
    Vs3d = orig_pos(We, St),
    ?SLOW(auv_mapping:stretch_opt(We, Vs3d));
remap(Type, #we{name=Ch}=We0, St) ->
    [Lower,Upper] = wings_vertex:bounding_box(We0),
    {W,H,_} = e3d_vec:sub(Upper, Lower),

    %% Get 3d positions (even for mapped vs).
    Vs3d = orig_pos(We0, St),
    Vs0 = auv_mapping:map_chart(Type, We0#we{vp=Vs3d}),
    We1 = We0#we{vp=gb_trees:from_orddict(sort(Vs0))},    
    Fs = wings_we:visible(We1),
    {{Dx,Dy},Vs1} = auv_placement:center_rotate(Fs, We1),
    Center = wings_vertex:center(We0),
    Scale = if Dx > Dy -> W / Dx;
 	       true -> H / Dy
	    end,
    Smat = e3d_mat:scale(Scale),
    Cmat = e3d_mat:translate(Center),
    Fix  = e3d_mat:mul(Cmat, Smat),
    Vs = foldl(fun({VId, Pos}, Acc) -> 
		       [{VId,e3d_mat:mul_point(Fix, Pos)}|Acc] 
	       end, [], Vs1),
    We0#we{vp=gb_trees:from_orddict(sort(Vs)),
	   name=Ch#ch{size={Dx*Scale, Dy*Scale}}}.

%% gb_tree of every vertex orig 3d position, (including the new cut ones)
orig_pos(We = #we{name=#ch{vmap=Vmap}},St) ->
    #st{bb=#uvstate{id=Id,st=#st{shapes=Sh}}} = St,
    #we{vp=Vs3d0} = gb_trees:get(Id, Sh),
    Vs3d = map(fun({V0,_Pos}) ->
		       case gb_trees:lookup(V0, Vmap) of
			   none -> 
			       {V0, gb_trees:get(V0, Vs3d0)};
			   {value,V} ->
			       {V0, gb_trees:get(V, Vs3d0)}
		       end 
	       end, gb_trees:to_list(We#we.vp)),
    gb_trees:from_orddict(Vs3d).


%%%
%%% Verify that the model in the geometry window hasn't changed its topology.
%%%

new_geom_state(#st{shapes=Shs}=GeomSt, AuvSt0) ->
    AuvSt = new_geom_state_1(Shs, AuvSt0),
    update_selection(GeomSt, AuvSt).

new_geom_state_1(Shs, #st{bb=#uvstate{id=Id,st=#st{shapes=Orig}}}=AuvSt) ->
    case {gb_trees:lookup(Id, Shs),gb_trees:lookup(Id, Orig)} of
	{{value,We},{value,We}} -> AuvSt;
	{{value,#we{es=Etab}},{value,#we{es=Etab}}} -> AuvSt;
	{{value,#we{es=Etab1}=We},{value,#we{es=Etab2}}} ->
	    case gb_trees:keys(Etab1) =:= gb_trees:keys(Etab2) of
		false -> topology_updated(We, AuvSt);
		true -> uvs_updated(We, AuvSt)
	    end
    end.

uvs_updated(We, AuvSt) ->
    topology_updated(We, AuvSt).

topology_updated(#we{fs=Ftab}=We, St) ->
    Charts = init_edit(gb_trees:keys(Ftab), We),
    St#st{shapes=Charts}.

%%%
%%% Draw routines.
%%%

draw_background(#st{mat=Mats,bb=#uvstate{matname=MatN}}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_view:load_matrices(false),

    %% Draw the background texture.

    gl:enable(?GL_DEPTH_TEST),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1, 1, 1),			%Clear
    case has_texture(MatN, Mats) of
	false -> ok;
	_ -> wings_material:apply_material(MatN, Mats)
    end,
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0, 0),    gl:vertex3f(0, 0, -1),
    gl:texCoord2f(1, 0),    gl:vertex3f(1, 0, -1),
    gl:texCoord2f(1, 1),    gl:vertex3f(1, 1, -1),
    gl:texCoord2f(0, 1),    gl:vertex3f(0, 1, -1),
    gl:'end'(), 

    %% Draw border around the UV space.
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:lineWidth(2),
    gl:color3f(0, 0, 1),
    gl:translatef(0, 0, -0.8),
    gl:recti(0, 0, 1, 1),

    gl:popAttrib().

redraw(St) ->
    wings_wm:set_prop(show_info_text, false),
    wings:redraw(St).

init_drawarea() ->
    {{X,TopY},{W0,TopH}} = wings_wm:win_rect(desktop),
    W = W0 div 2,
    {X+W,TopY+75,W,TopH-100}.
    
restore_wings_window() ->
    wings:unregister_postdraw_hook(wings_wm:this(), ?MODULE),
    wings_draw_util:delete_dlists().

%% Generate a checkerboard image of 4x4 squares 
%% with given side length in pixels.
checkerboard(Width, Height) ->
    White = [240,240,220],
    Black = [215,215,181],
    FourWhite = pattern_repeat(4, White),
    FourBlack = pattern_repeat(4, Black),
    R1 = pattern_repeat(Width div 8, [FourBlack|FourWhite]),
    R2 = pattern_repeat(Width div 8, [FourWhite|FourBlack]),
    R8 = [pattern_repeat(4, [R1])|pattern_repeat(4, [R2])],
    Pixels = list_to_binary(pattern_repeat(Height div 8, R8)),
    #e3d_image{width=Width,height=Height,image=Pixels,order=lower_left}.

pattern_repeat(0, _) -> [];
pattern_repeat(1, D) -> [D];
pattern_repeat(N, D) ->
    B = pattern_repeat(N div 2, D),
    case N rem 2 of
	0 -> [B|B];
	1 -> [D,B|B]
    end.

%%%
%%% Conversion routines.
%%%

auv2geom_faces(Fs0, #we{name=#ch{fm_a2g=A2G}}) ->
    Fs1 = sofs:from_external(Fs0, [atom]),
    Fs = sofs:image(A2G, Fs1),
    sofs:to_external(Fs).

geom2auv_faces(Fs0, #we{name=#ch{fm_a2g=A2G}}) ->
    Fs1 = sofs:from_external(Fs0, [atom]),
    Fs = sofs:inverse_image(A2G, Fs1),
    sofs:to_external(Fs).

auv2geom_vs(Vs, #we{name=#ch{vmap=Vmap}}) ->
    [auv_segment:map_vertex(V, Vmap) || V <- Vs].

geom2auv_vs(Vs, #we{name=#ch{vmap=Vmap},vp=Vtab}) ->
    geom2auv_vs_1(gb_trees:keys(Vtab), gb_sets:from_list(Vs), Vmap, []).

geom2auv_vs_1([V|Vs], VsSet, Vmap, Acc) ->
    case gb_sets:is_member(auv_segment:map_vertex(V, Vmap), VsSet) of
	true -> geom2auv_vs_1(Vs, VsSet, Vmap, [V|Acc]);
	false -> geom2auv_vs_1(Vs, VsSet, Vmap, Acc)
    end;
geom2auv_vs_1([], _, _, Acc) -> sort(Acc).

auv2geom_edges(Es, We) ->
    Es.

geom2auv_edges(Es, We) ->
    Es.
