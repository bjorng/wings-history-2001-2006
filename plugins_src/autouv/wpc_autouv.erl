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
%%     $Id: wpc_autouv.erl,v 1.217 2004/04/08 06:04:37 bjorng Exp $

-module(wpc_autouv).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
 
-export([init/0,menu/2,command/2,redraw/1]).

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
    ?DBG("Start shapes ~p~n",[gb_trees:keys(St#st.shapes)]), 
    start_uvmap(St);
command(_, _) -> next.

start_uvmap(#st{sel=Sel}=St) ->
    start_uvmap_1(Sel, St).

start_uvmap_1([{Id,_}|T], St) ->
    Name = {autouv,Id},
    case wings_wm:is_window(Name) of
	true -> wings_wm:raise(Name);
	false -> start_uvmap_2(Id, St)
    end,
    start_uvmap_1(T, St);
start_uvmap_1([], _) -> keep.

start_uvmap_2(Id, #st{shapes=Shs}=St) ->
    We = gb_trees:get(Id, Shs),
    case wings_vertex:isolated(We) of
	[] ->
	    start_uvmap_3(Id, We, St);
	[_|_] ->
	    wpa:error("The model has isolated vertices. (Use the Cleanup command.)")
    end.
    
start_uvmap_3(Id, #we{name=ObjName}=We, St) ->
    Op = {push,fun(Ev) -> auv_event(Ev, St) end},
    Name = {autouv,Id},
    Title = "AutoUV: " ++ ObjName,
    {X,Y,W,H} = init_drawarea(),
    Props = [{display_lists,Name}|wings_view:initial_properties()],
    CreateToolbar = fun(N, P, Wi) -> wings:create_toolbar(N, P, Wi) end,
    wings_wm:toplevel(Name, Title, {X,Y,highest}, {W,H},
		      [resizable,closable,menubar,{properties,Props},
		       {toolbar,CreateToolbar}], Op),
    wings_wm:send(Name, {init_uvmapping,We}).

auv_event({init_uvmapping,We}, St) ->
    wings:init_opengl(St),
    init_uvmapping(We, St);
auv_event({discard_uvs,Id,#st{shapes=Shs}=St}, #st{shapes=ShsOrig}) ->
    We = gb_trees:get(Id, Shs),
    OrigWe = gb_trees:get(Id, ShsOrig),
    auv_seg_ui:start(We, OrigWe, St);
auv_event({uv_edit,{MatName,Faces,We}}, St) ->
    do_edit(MatName, Faces, We, St);
auv_event({init_show_maps,Id,Map}, #st{shapes=Shs}=St) ->
    We = gb_trees:get(Id, Shs),
    init_show_maps(Map, We, St);
auv_event({callback,Cb}, _) ->
    Cb();
auv_event(redraw, _) ->
    wings_wm:clear_background(),
    keep;
auv_event(_Ev, _) -> keep.

%%%
%%% Start UV mapping. Always check if there are pre-existing UV coordinates.
%%%

init_uvmapping(We, St) ->
    case wings_we:uv_mapped_faces(We) of
	[] -> auv_seg_ui:start(We, We, St);
	_Faces -> start_edit(We, St)
    end.

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

do_edit(MatName0, Faces, We, St) ->
    {Areas,MatName} = init_edit(MatName0, Faces, We),
    create_uv_state(Areas, MatName, We, St).

%%%%%%

init_show_maps(Map0, We, St) ->
    Map1 = auv_placement:place_areas(Map0),
    Map = gb_trees:from_orddict(sort(Map1)),
    create_uv_state(Map, none, We, St).

create_uv_state(Charts0, MatName0, We, GeomSt0) ->
    Charts = restrict_ftab(Charts0),
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
		      gb_sets:from_list(lists:seq(1, gb_trees:size(Charts)))),
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

restrict_ftab(Charts0) ->
    Empty = gb_sets:empty(),
    Charts = [restrict_ftab_1(Ch, Empty) || Ch <- gb_trees:values(Charts0)],
    gb_trees:from_orddict(Charts).

restrict_ftab_1(#we{id=Id,name=#ch{fs=Fs0},fs=Ftab0}=We, Empty) ->
    Ftab1 = sofs:from_external(gb_trees:to_list(Ftab0), [{face,edge}]),
    Fs = sofs:set(Fs0, [face]),
    Ftab2 = sofs:restriction(Ftab1, Fs),
    Ftab3 = sofs:to_external(Ftab2),
    Ftab = gb_trees:from_orddict(Ftab3),
    {Id,We#we{fs=Ftab,he=Empty}}.

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
update_uvs(Cs, #we{es=Etab0}=We) ->
    update_uvs_1(Cs, We, Etab0).

update_uvs_1([#we{vp=Vpos0,name=#ch{fs=Fs,vmap=Vmap}}=ChartWe|Cs], We, Etab0) ->
    VFace0 = wings_face:fold_faces(
	       fun(Face, V, _, _, A) ->
		       [{V,Face}|A]
	       end, [], Fs, ChartWe),
    VFace1 = sofs:relation(VFace0),
    VFace2 = sofs:relation_to_family(VFace1),
    VFace = sofs:to_external(VFace2),
    Vpos = gb_trees:to_list(Vpos0),
    Etab = update_uvs_2(Vpos, VFace, Vmap, We, Etab0),
    update_uvs_1(Cs, We, Etab);
update_uvs_1([], We, Etab) ->  We#we{es=Etab}.

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
    Faces = lists:append([Fs || #we{name=#ch{fs=Fs}} <- gb_trees:values(Cs)]),
    wings_material:assign(MatName, Faces, We).

init_edit(MatName, Faces0, We0) ->
    Faces = [F || F <- Faces0, has_proper_uvs(F, We0)],
    FvUvMap = auv_segment:fv_to_uv_map(Faces, We0),
    {Charts1,Cuts} = auv_segment:uv_to_charts(Faces, FvUvMap, We0),
    Charts = auv_segment:cut_model(Charts1, Cuts, We0),
    Map1 = build_map(Charts, FvUvMap, 1, []),
    Map  = gb_trees:from_orddict(sort(Map1)),
    {Map,MatName}.

has_proper_uvs(Face, #we{mirror=Face}) -> false;
has_proper_uvs(Face, We) ->
    foldl(fun({_,_}, F) -> F;
	     (_, _) -> false
	  end, true, wings_face:vertex_info(Face, We)).

build_map([{Fs,Vmap,We0}|T], FvUvMap, No, Acc) ->
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
    Chart = #ch{fs=Fs,size=undefined,vmap=Vmap},
    We = We0#we{name=Chart,id=No,vp=gb_trees:from_orddict(UVs)},
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
	    {"Delete",delete,"Remove UV-coordinates for the selected charts"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(face, X, Y) ->
    Menu = [{basic,{"Face operations",ignore}},
	    {basic,separator},
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
	    {"Scale", {scale, Scale}, "Scale selected vertices"}
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
    case verify_state(GeomSt, AuvSt) of
	keep ->
	    update_selection(GeomSt, AuvSt);
	Other -> Other
    end;
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
	{save_state,St1} -> ok;
	#st{}=St1 -> ok
    end,
    St = filter_selection(St1),
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
    %%get_event(St#st{selmode=body,sh=false}).

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
    Vs = [V || V <- gb_trees:keys(Vtab), not_bordering(V, We)],
    wings_vertex_cmd:tighten(Vs, We, A).

not_bordering(V, #we{fs=Ftab}=We) ->
    wings_vertex:fold(fun(_, _, _, false) -> false;
			 (_, F, _, true) -> gb_trees:is_defined(F, Ftab)
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

%% filter_selection(St0) -> St
%%  Remove any items in the selection that are outside of a chart.
filter_selection(#st{sel=[]}=St) -> St;
filter_selection(#st{selmode=vertex}=St) ->
    Sel = wpa:sel_fold(fun(Vs0, #we{id=Id,vp=Vtab}, A) ->
			       Vs1 = gb_trees:keys(Vtab),
			       Vs2 = gb_sets:from_ordset(Vs1),
			       Vs = gb_sets:intersection(Vs0, Vs2),
			       [{Id,Vs}|A]
		       end, [], St),
    wpa:sel_set(vertex, Sel, St);
filter_selection(#st{selmode=edge}=St) ->
    Sel = wpa:sel_fold(fun(Es0, #we{id=Id,fs=Ftab}=We, A) ->
			       Fs = gb_trees:keys(Ftab),
			       Inner0 = wings_face:inner_edges(Fs, We),
			       Inner = gb_sets:from_ordset(Inner0),
			       Es = gb_sets:intersection(Es0, Inner),
			       [{Id,Es}|A]
		       end, [], St),
    wpa:sel_set(edge, Sel, St);
filter_selection(#st{selmode=face}=St) ->
    Sel = wpa:sel_fold(fun(Fs0, #we{id=Id,fs=Ftab}, A) ->
			       Fs1 = gb_trees:keys(Ftab),
			       Fs2 = gb_sets:from_ordset(Fs1),
			       Fs = gb_sets:intersection(Fs0, Fs2),
			       [{Id,Fs}|A]
		       end, [], St),
    wpa:sel_set(face, Sel, St);
filter_selection(#st{selmode=body}=St) -> St.

%% update_selection(GemoSt, AuvSt0) -> AuvSt
%%  Update the selection in the AutoUV window given a selection
%%  from a geometry window.
update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #st{bb=#uvstate{st=#st{selmode=Mode,sel=Sel}}=Uvs}=AuvSt) ->
    get_event_nodraw(AuvSt#st{bb=Uvs#uvstate{st=St}});
update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #st{selmode=AuvMode,bb=#uvstate{id=Id}=Uvs,
		     shapes=Charts0}=AuvSt0) ->
    case keysearch(Id, 1, Sel) of
	false ->
	    AuvSt = reset_sel(AuvSt0#st{bb=Uvs#uvstate{st=St}}),
	    get_event(AuvSt);
	{value,{Id,Elems0}} ->
	    Elems = gb_sets:to_list(Elems0),
	    Charts = gb_trees:to_list(Charts0),
	    Res = update_selection_1(AuvMode, Mode, Elems, Charts),
	    case Res of
		none ->
		    AuvSt = AuvSt0#st{sel=[],sh=false,bb=Uvs#uvstate{st=St}},
		    get_event_nodraw(AuvSt);
		NewSel ->
		    AuvSt = AuvSt0#st{sel=sort(NewSel),sh=false,
				      bb=Uvs#uvstate{st=St}},
		    get_event(AuvSt)
	    end
    end.

update_selection_1(vertex, _, _, _) ->
    io:format("~p\n", [?LINE]),
    none;
update_selection_1(edge, _, _, _) ->
    io:format("~p\n", [?LINE]),
    none;
update_selection_1(face, Mode, Elems, Charts) ->
    update_face_sel(Mode, Elems, Charts);
update_selection_1(body, Mode, Elems, Charts) ->
    update_body_sel(Mode, Elems, Charts).

%% update_face_sel(SelModeInGeom, Elems, Charts) -> Selection
%%  Convert the selection from the geoemetry window to
%%  a face selection in the AutoUV window.

update_face_sel(face, Faces, Charts) ->
    face_sel_to_face(Charts, Faces, []);
update_face_sel(body, _, Charts) ->
    body_sel_to_face(Charts, []);
update_face_sel(Mode, _, _) ->
    io:format("~p: ~p\n", [?LINE,Mode]),
    none.

face_sel_to_face([{K,#we{name=#ch{fs=Fs}}}|Cs], Faces, Sel) ->
    case ordsets:intersection(sort(Fs), Faces) of
 	[] ->
	    face_sel_to_face(Cs, Faces, Sel);
 	FaceSel0 ->
	    FaceSel = gb_sets:from_list(FaceSel0),
	    face_sel_to_face(Cs, Faces, [{K,FaceSel}|Sel])
    end;
face_sel_to_face([], _, Sel) -> Sel.

body_sel_to_face([{K,#we{fs=Ftab}}|Cs], Sel) ->
    FaceSel = gb_sets:from_ordset(gb_trees:keys(Ftab)),
    body_sel_to_face(Cs, [{K,FaceSel}|Sel]);
body_sel_to_face([], Sel) -> Sel.


%% update_body_sel(SelModeInGeom, Elems, Charts) -> Selection
%%  Convert the selection from the geoemetry window to
%%  a body selection in the AutoUV window.

update_body_sel(face, Elems, Charts) ->
    face_sel_to_body(Charts, Elems, []);
update_body_sel(body, _, Charts) ->
    body_sel_to_body(Charts, []);
update_body_sel(Mode, _, _) ->
    io:format("~p: ~p\n", [?LINE,Mode]),
    none.

face_sel_to_body([{K,#we{name=#ch{fs=Fs}}}|Cs], Faces, Sel) ->
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
    Fs0 = wpa:sel_fold(fun(_, #we{name=#ch{fs=Fs}}, A) ->
			       Fs++A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(face, [{Id,Fs}], GeomSt);
update_geom_selection(#st{selmode=face,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(Fs, _, A) ->
			       [Fs|A]
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:union(Fs0),
    wpa:sel_set(face, [{Id,Fs}], GeomSt);
update_geom_selection(#st{selmode=edge,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(_, #we{name=#ch{fs=Fs}}, A) ->
			       Fs++A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(face, [{Id,Fs}], GeomSt);
update_geom_selection(#st{selmode=vertex,sel=Sel,
			  bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(Vs, We, A) ->
			       update_geom_sel_vtx(Vs, We, A)
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(vertex, [{Id,Fs}], GeomSt).

update_geom_sel_vtx(Vs, #we{name=#ch{vmap=Vmap}}, A) ->
    [auv_segment:map_vertex(V, Vmap) || V <- gb_sets:to_list(Vs)] ++ A.

reset_sel(St0) ->
    wings_sel:reset(St0).
%     case wings_sel:reset(St0) of
% 	#st{selmode=body,sh=false}=St -> St;
% 	St -> St#st{selmode=body,sh=false}
%     end.

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

%%%
%%% Verify that the model in the geometry window hasn't changed its topology.
%%%
verify_state(WingsSt, AuvSt) ->
    case same_topology(WingsSt, AuvSt) of
	true -> keep;
	false ->
	    wings_wm:dirty(),
	    {seq,push,get_broken_event(AuvSt)}
    end.

same_topology(#st{shapes=Shs}, #st{bb=#uvstate{id=Id,st=#st{shapes=Orig}}}) ->
    case {gb_trees:lookup(Id, Shs),gb_trees:lookup(Id, Orig)} of
	{{value,We},{value,We}} -> true;
	{{value,#we{es=Etab1}},{value,#we{es=Etab2}}} ->
	    gb_trees:keys(Etab1) =:= gb_trees:keys(Etab2);
	_ ->
	    false
    end.

get_broken_event(AuvSt) ->
    {replace,fun(Ev) -> broken_event(Ev, AuvSt) end}.

broken_event(redraw, #st{bb=#uvstate{id=Id, st=#st{shapes=Shs}}}) ->
    {value, #we{name=Name}} = gb_trees:lookup(Id, Shs),
    {_,_,W,H} = wings_wm:viewport(),
    wings_io:ortho_setup(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1, 1, 1),
    gl:recti(0, H, W, 0),
    gl:color3f(0, 0, 0),
    wings_io:text_at(10, ?LINE_HEIGHT,
		     ["The topology of \"",Name,"\" has changed,"]),
    wings_io:text_at(10, 2*?LINE_HEIGHT,
		     "preventing UV coordinates to be applied to it."),
    wings_io:text_at(10, 4*?LINE_HEIGHT,
		     "Either quit AutoUV and start over, or Undo your changes."),
    wings_wm:message("[R] Show menu"),
    keep;
broken_event({current_state,geom_display_lists,St}, AuvSt) ->
    case same_topology(St, AuvSt) of
	false -> keep;
	true ->
	    wings_wm:dirty(),
	    pop
    end;
broken_event(close, _) ->
    restore_wings_window(),
    delete;
broken_event(#keyboard{}=Ev, _St) ->
    %% To accept Undo.
    wings_wm:send(geom, Ev),
    keep;
broken_event(Ev, _) ->
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} ->
	    Menu = [{"Cancel",cancel,"Cancel UV mapping"}],
	    wings_menu:popup_menu(X, Y, wings_wm:this(), Menu)
    end.

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
