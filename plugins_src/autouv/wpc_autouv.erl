%% File    : wpc_autouv.erl
%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%% Description : A semi-simple semi-automatic UV-mapping plugin
%%
%% Created : 24 Jan 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%-------------------------------------------------------------------
%%  Copyright (c) 2002-2004 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: wpc_autouv.erl,v 1.193 2004/03/09 07:24:25 bjorng Exp $

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
    {{X,Y,W,H},_Geom} = init_drawarea(),
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
	Faces -> start_edit(Faces, We, St)
    end.

start_edit(_Faces, We, St0) ->
    This = wings_wm:this(),
    Prompt = "The object already has UV coordinates.",
    Qs = {vframe,
	  [{label,Prompt,[{break,45}]},
	   {hframe,[{button,"Edit UVs",
		     fun(_) ->
			     start_edit_1(This, We, St0) end},
		    {button,"Discard UVs",
		     fun(_) ->
			     St = discard_uvmap(We, St0),
			     wings_wm:send(This, {discard_uvs,We#we.id,St}),
			     ignore
		     end},
		    {button,"Cancel",
		     fun(_) ->
			     wings_wm:delete(This),
			     ignore
		     end,[cancel]}]}]},
    wings_ask:dialog("", Qs, fun(_) ->
				     ignore
			     end).

start_edit_1(Win, #we{fs=Ftab}=We, St) ->
    MatNames0 = wings_material:get_all(We),
    MatNames1 = sofs:from_external(MatNames0, [{face,material}]),
    MatNames2 = sofs:converse(MatNames1),
    MatNames3 = sofs:relation_to_family(MatNames2),
    MatNames4 = sofs:to_external(MatNames3),
    MatNames = [Mat || {Name,_}=Mat <- MatNames4, has_texture(Name, St)],
    case MatNames of
	[] ->
	    Faces = gb_trees:keys(Ftab),
	    gen_edit_event(Win, none, Faces, We);
	[{MatName,Faces}] ->
	    gen_edit_event(Win, MatName, Faces, We);
	[{First,_}|_]=Ms ->
	    Ask = fun() -> start_edit_cb(Win, First, Ms, We) end,
	    wings_wm:send(Win, {callback,Ask}),
	    ignore
    end.

start_edit_cb(Win, First, Ms, We) ->
    Qs = [{vradio,[{"Material "++atom_to_list(M),M} || {M,_} <- Ms], First}],
    wings_ask:dialog("Choose Material to Edit",
		     Qs,
		     fun([Mat]) ->
			     {value,{_,Faces}} = keysearch(Mat, 1, Ms),
			     gen_edit_event(Win, Mat, Faces, We)
		     end).

gen_edit_event(Win, MatName, Faces, We) ->
    wings_wm:send(Win, {uv_edit,{MatName,Faces,We}}),
    keep.

discard_uvmap(#we{fs=Ftab}=We0, St) ->
    Faces = gb_trees:keys(Ftab),
    FvUvMap = auv_segment:fv_to_uv_map(Faces, We0),
    {Charts,Cuts} = auv_segment:uv_to_charts(Faces, FvUvMap, We0),
    We = We0#we{mode=material},
    auv_util:mark_segments(Charts, Cuts, We, St).

do_edit(MatName0, Faces, We, St) ->
    {Areas,MatName} = init_edit(MatName0, Faces, We),
    create_uv_state(Areas, MatName, We, St).

%%%%%%

init_show_maps(Map0, We, St) ->
    Map1 = auv_placement:place_areas(Map0),
    Map = gb_trees:from_orddict(sort(Map1)),
    create_uv_state(Map, none, We, St).

create_uv_state(Map, MatName0, We, GeomSt0) ->
    wings:mode_restriction([body]),
    wings_wm:current_state(#st{selmode=body,sel=[]}),
    {_,Geom} = init_drawarea(),
    {GeomSt1,MatName} = 
	case has_texture(MatName0, GeomSt0) of
	    true -> 
		{GeomSt0,MatName0};
	    false ->
		Tx = checkerboard(128, 128),
		add_material(Tx, We#we.name, MatName0, GeomSt0)
	end,
    GeomSt = insert_initial_uvcoords(Map, We#we.id, MatName, GeomSt1),
    wings_wm:send(geom, {new_state,GeomSt}),
    Uvs = #uvstate{st=wpa:sel_set(face, [], GeomSt),
		   geom=Geom,
		   id=We#we.id,
		   matname=MatName},
    St = GeomSt#st{selmode=body,sel=[],shapes=Map,bb=Uvs},
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
    wings_wm:set_prop(show_info_text, false),
    wings_wm:set_prop(orthogonal_view, true),
    wings_wm:set_prop(show_axes, false),
    wings_wm:set_prop(show_groundplane, false),
    wings_wm:set_prop(wireframed_objects,
		      lists:seq(1, gb_trees:size(Map))),
    wings_wm:set_prop(allow_rotation, false),

    get_event(St).

insert_initial_uvcoords(Charts, Id, MatName, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    UVpos = gen_uv_pos(gb_trees:values(Charts)),
    We1 = insert_coords(UVpos, We0),
    We2 = insert_material(Charts, MatName, We1),
    We = We2#we{mode=material},    
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{shapes=Shs}.
   
update_selected_uvcoords(#st{bb=Uvs}=St) ->
    Charts = wpa:sel_fold(fun(_, We, Acc) -> [We|Acc] end, [], St),
    #uvstate{st=#st{shapes=Shs0}=GeomSt0,id=Id} = Uvs,
    We0 = gb_trees:get(Id, Shs0),
    UVpos = gen_uv_pos(Charts),
    We = insert_coords(UVpos, We0),    
    Shs = gb_trees:update(Id, We, Shs0),
    GeomSt = GeomSt0#st{shapes=Shs},
    wings_wm:send(geom, {new_state,GeomSt}),
    St#st{bb=Uvs#uvstate{st=GeomSt}}.

gen_uv_pos(Cs) ->
    gen_uv_pos_1(Cs, []).

gen_uv_pos_1([#we{vp=Vpos0,name=#ch{fs=Fs,vmap=Vmap}}=We|T], Acc) ->
    Vpos1 = gb_trees:to_list(Vpos0),
    VFace0 = wings_face:fold_faces(
	       fun(Face, V, _, _, A) ->
		       [{V,Face}|A]
	       end, [], Fs, We),
    VFace = sofs:relation(VFace0, [{vertex,face}]),
    Vpos = sofs:relation(Vpos1, [{vertex,uvinfo}]),
    Comb0 = sofs:relative_product({VFace,Vpos}),
    Comb1 = sofs:to_external(Comb0),
    Comb = foldl(fun({V0,Data}, A) ->
			 V = auv_segment:map_vertex(V0, Vmap),
			 [{V,Data}|A]
		 end, Acc, Comb1),
    gen_uv_pos_1(T, Comb);
gen_uv_pos_1([], Acc) -> Acc.

insert_coords([{V,{Face,{S,T,_}}}|Rest], #we{es=Etab0}=We) ->
    Etab = wings_vertex:fold(
	     fun(Edge, _, Rec0, E0) ->
		     case Rec0 of
			 #edge{vs=V,lf=Face} ->
			     Rec = gb_trees:get(Edge, E0),
			     gb_trees:update(Edge, Rec#edge{a={S,T}}, E0);
			 #edge{ve=V,rf=Face} ->
			     Rec = gb_trees:get(Edge, E0),
			     gb_trees:update(Edge, Rec#edge{b={S,T}}, E0);
			 _ ->
			     E0
		     end
	     end, Etab0, V, We),
    insert_coords(Rest, We#we{es=Etab});
insert_coords([], #we{es=Etab0}=We0) -> 
    %% Ensure that we leave no vertex colors.
    Etab = map(fun({Id,E=#edge{a={_,_,_},b={_,_,_}}}) -> 
		       {Id,E#edge{a=none,b=none}};
		  ({Id,E=#edge{a={_,_,_}}}) -> 
		       {Id,E#edge{a=none}};
		  ({Id,E=#edge{b={_,_,_}}}) -> 
		       {Id,E#edge{b=none}};
		  (Rec) -> Rec
	       end, gb_trees:to_list(Etab0)),
    We0#we{es=gb_trees:from_orddict(Etab)}.

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

    Menu = [{"Face Group operations", ignore},
	    separator,
	    {"Move", move, "Move selected charts"},
	    {"Scale", {scale, Scale}, "Scale selected charts"},
	    {"Rotate", {rotate, Rotate}, "Rotate selected charts"}
% 	    separator,
% 	    {"Rescale All", rescale_all, "Pack the space in lower-left before rescaling"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(face, X, Y) ->
    Menu = [{"Face operations", ignore}, 
	    {"Email your ideas", ignore}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(edge, X, Y) ->
    Menu = [{"Edge operations", ignore},
	    {"Email your ideas", ignore}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(vertex, X, Y) ->
    Menu = [{"Vertex operations", ignore},
	    {"Email your ideas", ignore}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu).

option_menu() ->
    [separator,
     {"Create Texture",create_texture,"Make and Attach a texture to the model"}].

%%% Event handling

get_event(#st{}=St) ->
    wings_wm:dirty(),
    get_event_nodraw(St).

get_event_nodraw(#st{}=St) ->
    {replace,fun(Ev) -> handle_event(Ev, St) end}.

handle_event(redraw, St0) ->
    St = redraw(St0),
    get_event_nodraw(St);
handle_event(init_opengl, St) ->
    wings:init_opengl(St),
    {_,_,W,H} = wings_wm:viewport(wings_wm:this()),
    Geom = wingeom(W, H),
    get_event(update_geom(St, Geom));
handle_event(resized, St) ->
    {_,_,W,H} = wings_wm:viewport(wings_wm:this()),
    Geom = wingeom(W,H),
    get_event(update_geom(St, Geom));
handle_event({new_state,#st{selmode=Mode,sel=Sel,shapes=Shs}}, #st{bb=Uvs}=St) ->
    GeomSt = wings_select_faces(Sel, St),
    wings_wm:send(geom, {new_state,GeomSt}),
    get_event(St#st{selmode=Mode,sel=Sel,shapes=Shs,bb=Uvs#uvstate{st=GeomSt}});
handle_event({new_uv_state,St0}, _) ->
    St = update_selected_uvcoords(St0),
    wings_wm:dirty(),
    get_event(St);
handle_event(Ev, St) ->
    case wings_camera:event(Ev, fun() -> redraw(St) end) of
	next -> handle_event_0(Ev, St);
	Other -> Other
    end.

handle_event_0(Ev, St) ->
    case auv_pick:event(Ev, St) of
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
handle_event_1(#keyboard{state=?SDL_PRESSED,sym=?SDLK_SPACE}, #st{bb=#uvstate{st=St}}) ->
    wings_wm:send(geom, {new_state,wpa:sel_set(face, [], St)});
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

update_geom(#st{bb=Uvs}=St, Geom) ->
    St#st{bb=Uvs#uvstate{geom=Geom}}.

handle_command({rotate,free}, St) ->
    handle_command(rotate, St);
handle_command({scale,Scale}, St) ->
    handle_command(Scale, St);
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
handle_command(_, #st{sel=[]}) ->
    keep;
handle_command(Cmd, St) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    {seq,push,get_cmd_event(Cmd, X, Y, St)}.

% handle_command_1(rescale_all, Uvs0) ->
%     Uvs = clear_selection(Uvs0),
%     RscAreas = rescale_all(all_charts(Uvs)),
%     get_event(reset_dl(Uvs0#uvstate{areas=RscAreas}));

%%%
%%% Command handling.
%%%

-define(SS, 2.0).  % Scale mouse motion

get_cmd_event(Op, X, Y, #st{}=St) ->
    wings_wm:dirty(),
    get_cmd_event_noredraw(Op, X, Y, St).

get_cmd_event_noredraw(Op, X, Y, #st{}=St) ->
    {replace,fun(Ev) -> cmd_event(Ev, Op, X, Y, St) end}.

cmd_event(redraw, Op, X, Y, St0) ->
    St = redraw(St0),
    get_cmd_event_noredraw(Op, X, Y, St);

cmd_event(#mousemotion{x=MX0,y=MY0}, Op, X0, Y0, St0) ->
    #st{bb=#uvstate{geom={X0Y0,MW0,X0Y0,MH0}}} = St0,
    {_,_,W,H} = wings_wm:viewport(),
    DX = MX0 - X0,
    DY = MY0 - Y0,
    MW =  (MW0-X0Y0) * DX/W,
    MH = -(MH0-X0Y0) * DY/H,
%%    ?DBG("Viewp ~p ~p ~p ~p ~n", [MW,MH,DX,DY]), 
    St = case Op of
	     move ->
		 wpa:sel_map(fun(_, We) -> move_chart(4*MW, 4*MH, We) end, St0);
	     rotate ->
		 wpa:sel_map(fun(_, We) -> rotate_chart(MW*180, We) end, St0);
	     scale_uniform ->
		 wpa:sel_map(fun(_, We) -> scale_chart(1.0+MW*?SS, We) end, St0);
	     scale_x ->
		 wpa:sel_map(fun(_, We) -> scale_chart({1.0+MW*?SS,1.0,1.0}, We)end,St0);
	     scale_y ->
		 wpa:sel_map(fun(_, We) -> scale_chart({1.0,1.0+MH*?SS,1.0}, We)end,St0)
	 end,
    get_cmd_event(Op, MX0, MY0, St);
cmd_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_LEFT}, _, _, _, St) ->
    wings_wm:later({new_uv_state,St}),
    pop;
cmd_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_RIGHT}, _, _, _, _) ->
    wings_wm:dirty(),
    pop;
cmd_event(_, _, _, _, _) -> keep.

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

update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #st{bb = #uvstate{st=#st{selmode=Mode,sel=Sel}} = Uvs} = AuvSt) ->
    get_event_nodraw(AuvSt#st{bb = Uvs#uvstate{st=St}});
update_selection(#st{selmode=Mode,sel=Sel}=St, #st{bb=#uvstate{id=Id}=Uvs}=AuvSt0) ->
    AuvSt = AuvSt0#st{sel=[], bb=Uvs#uvstate{st=St}},
    case keysearch(Id, 1, Sel) of
	false ->
	    get_event(AuvSt);
	{value,{Id,Elems}} ->
	    update_selection_1(Mode, gb_sets:to_list(Elems), AuvSt)
    end.

update_selection_1(face, Faces, #st{shapes=Charts}=St) ->
    update_selection_2(gb_trees:to_list(Charts), Faces, St, []);
update_selection_1(_, _, St) ->
    get_event_nodraw(St).

update_selection_2([{K,#we{name=#ch{fs=Fs}}}|Cs], Faces, St, Sel) ->
    case ordsets:intersection(sort(Fs), Faces) of
	[] -> update_selection_2(Cs, Faces, St, Sel);
	_ -> update_selection_2(Cs, Faces, St, [{K,gb_sets:singleton(0)}|Sel])
    end;
update_selection_2([], _, St, Sel) ->
    get_event(St#st{sel=sort(Sel)}).
    
-define(OUT, 1.2/2). %% was 1/2 

wings_select_faces([], #st{bb=#uvstate{st=GeomSt}}) ->
    wpa:sel_set(face, [], GeomSt);
wings_select_faces(Sel, #st{bb=#uvstate{st=GeomSt,id=Id}}=St) ->
    Fs0 = wpa:sel_fold(fun(_, #we{name=#ch{fs=Fs}}, A) ->
			       Fs++A
		       end, [], St#st{sel=Sel}),
    Fs = gb_sets:from_list(Fs0),
    wpa:sel_set(face, [{Id,Fs}], GeomSt).

%%%% GUI Operations

move_chart(Dx, Dy, We) ->
    Translate = e3d_mat:translate(Dx, Dy, 0.0),
    wings_we:transform_vs(Translate, We).

rotate_chart(Angle, We) ->
    Center = wings_vertex:center(We),
    Rot0 = e3d_mat:translate(e3d_vec:neg(Center)),
    Rot1 = e3d_mat:mul(e3d_mat:rotate(float(trunc(Angle)), {0.0,0.0,1.0}), Rot0),
    Rot = e3d_mat:mul(e3d_mat:translate(Center), Rot1),
    wings_we:transform_vs(Rot, We).

scale_chart(Size, We) ->
    Center = wings_vertex:center(We),
    Scale0 = e3d_mat:translate(e3d_vec:neg(Center)),
    Scale1 = e3d_mat:mul(e3d_mat:scale(Size), Scale0),
    Scale  = e3d_mat:mul(e3d_mat:translate(Center), Scale1),
    wings_we:transform_vs(Scale, We).

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

% rescale_all(Charts0) ->
%     Charts = gb_trees:values(Charts0),
%     Find = fun(#we{name=#ch{center={CX,CY},size={W,H}}}, [MX,MY]) ->
% 		   TX = CX + W/2,
% 		   TY = CY + H/2,
% 		   NewMX = if TX > MX -> TX; true -> MX end,
% 		   NewMY = if TY > MY -> TY; true -> MY end,
% 		   [NewMX, NewMY]
% 	   end,
%     Max = max(foldl(Find, [0,0], Charts)),
%     Ns = 1.0 / Max,
%     rescale_all_1(Charts, Ns, []).

% rescale_all_1([#we{id=Id,name=#ch{center={CX0,CY0},size={W0,H0},scale=S0}=Ch}=We0|T],
% 	      Ns, Acc) ->
%     We = We0#we{name=Ch#ch{center={CX0*Ns,CY0*Ns},size={W0*Ns,H0*Ns},scale=S0*Ns}},
%     rescale_all_1(T, Ns, [{Id,We}|Acc]);
% rescale_all_1([], _, Acc) ->
%     gb_trees:from_orddict(reverse(Acc)).

%%%
%%% Verify that the model in the geometry window hasn't changed its topology.
%%%
verify_state(WingsSt, AuvSt) ->
    case same_topology(WingsSt,AuvSt) of
	true -> keep;
	false ->
	    wings_wm:dirty(),
	    {seq,push,get_broken_event(AuvSt)}
    end.

same_topology(#st{shapes=Shs}, #st{bb=#uvstate{id=Id,st=#st{shapes=Orig}}}) ->
    case {gb_trees:lookup(Id, Shs), gb_trees:lookup(Id, Orig)} of
	{{value,We},{value,We}} -> true;
	{value,#we{es=Etab1}, {value,#we{es=Etab2}}} -> 
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

redraw(#st{mat=Mats,bb=Uvs}=St) ->
    update_dlists(St#st{selmode=body}),

    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),

    %% Draw background of AutoUV window.

    {X,Y,W,H} = wings_wm:viewport(),
    gl:scissor(X, Y, W, H),
    gl:enable(?GL_SCISSOR_TEST),
    gl:clearColor(1, 1, 1, 1),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:disable(?GL_SCISSOR_TEST),

    #uvstate{matname=MatN} = Uvs,

    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),

    wings_view:load_matrices(false),

    %% Draw the background texture.

    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1, 1, 1),			%Clear
    case has_texture(MatN, Mats) of
	false -> ok;
	_ -> wings_material:apply_material(MatN, Mats)
    end,
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0, 0),    gl:vertex3f(0, 0, 0),
    gl:texCoord2f(1, 0),    gl:vertex3f(1, 0, 0),
    gl:texCoord2f(1, 1),    gl:vertex3f(1, 1, 0),
    gl:texCoord2f(0, 1),    gl:vertex3f(0, 1, 0),
    gl:'end'(), 
    gl:disable(?GL_TEXTURE_2D),

    %% Draw border around the UV space.
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:color3f(0, 0, 1),
    gl:recti(0, 0, 1, 1),

    %% Now draw charts and selection.

    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:shadeModel(?GL_FLAT),

    wings_draw_util:fold(fun(D, _) ->
				 draw_one_chart(D)
			 end, []),
    gl:popAttrib(),
    St.

init_drawarea() ->
    {{X,TopY},{W0,TopH}} = wings_wm:win_rect(desktop),
    W = W0 div 2,
    {{X+W,TopY+75,W,TopH-100}, wingeom(W, TopH)}.
    
wingeom(W,H) ->
    Border = 15,
    if 
	W > H ->
	    WF = Border / W,
	    {-WF,W/H+WF,-WF,1+2*WF};
	true ->
	    WF = Border / H,
	    {-WF,1+WF,-WF,H/W+WF}
    end.

draw_one_chart(#dlo{edges=Edges,sel=Sel}) ->
    wings_draw_util:call(Edges),
    wings_draw_util:call(Sel).

update_dlists(#st{}=St) ->
    wings_draw:invalidate_dlists(false, St),
    wings_draw_util:map(fun(D0, _) ->
				D = update_edges(D0),
				update_sel(D)
			end, []).

update_edges(#dlo{edges=none,src_we=#we{name=#ch{fs=Fs}}=We}=D) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:lineWidth(1),
    gl:color3f(0.1, 0.1, 0.1),
    draw_all_face_edges(Fs, We),
    gl:endList(),
    D#dlo{edges=List};
update_edges(#dlo{}=D) -> D.

draw_all_face_edges([F|Fs], We) ->
    draw_face_edges(F, We),
    draw_all_face_edges(Fs, We);
draw_all_face_edges([], _) -> ok.

draw_face_edges(Face, #we{vp=Vtab}=We) ->
    Vs = wings_face:vertices_cw(Face, We),
    gl:'begin'(?GL_LINE_LOOP),
    foreach(fun(V) -> gl:vertex3fv(gb_trees:get(V, Vtab)) end, Vs),
    gl:'end'().

draw_faces(Fs, We) ->
    Draw = fun(Face) -> wings_draw_util:plain_face(Face, We) end,
    wings_draw_util:begin_end(fun() -> foreach(Draw, Fs) end).

update_sel(#dlo{sel=none,src_sel={body,_},src_we=#we{name=#ch{fs=Fs}}=We}=D) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    {R,G,B} = wings_pref:get_value(selected_color),
    gl:pushMatrix(),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_BLEND),
    gl:translatef(0, 0, 0.1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color4f(R, G, B, 0.7),
    draw_faces(Fs, We#we{mode=material}),
    gl:disable(?GL_BLEND),
    gl:popMatrix(),
    gl:endList(),
    D#dlo{sel=List};
update_sel(#dlo{}=D) -> D.

restore_wings_window() ->
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
