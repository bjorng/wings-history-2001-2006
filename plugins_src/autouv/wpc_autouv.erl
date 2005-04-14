%%
%%  wpc_autouv.erl --
%%
%%     A semi-simple semi-automatic UV-mapping semi-plugin.
%%
%%  Copyright (c) 2002-2005 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_autouv.erl,v 1.309 2005/04/14 22:27:16 dgud Exp $
%%

-module(wpc_autouv).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
 
-export([init/0,menu/2,command/2,redraw/1]).
-export([handle_event/2,bg_image/0]). %% Debug
-import(lists, [sort/1,keysort/2,map/2,foldl/3,reverse/1,
		append/1,delete/2,usort/1,max/1,min/1,
		member/2,foreach/2,keysearch/3]).

%% Exports to auv_texture.
-export([has_texture/2]).

%% Exports to auv_seg_ui.
-export([init_show_maps/4]).

%% Background Image compressed.

init() ->
    true.

menu({body}, Menu) ->
    case wpc_snap:active() of
	true ->
	    Menu;
	false ->
	    Menu ++ [separator,auv_menu()]
    end;
menu({face}, Menu) ->
    case wpc_snap:active() of
	true ->
	    Menu;
	false ->
	    Menu ++ [separator,auv_menu()]
    end;
menu({window}, Menu) ->
    Menu ++ [separator,
	     {"UV Editor Window",uv_editor_window,
	      "Open a UV Editor window for each selected object"}];
menu(_Dbg, Menu) ->
    Menu.

auv_menu() ->
    {"UV Mapping", {?MODULE, fun auv_menu/2}}.
auv_menu(help,_) ->
    {"Generate UV mapping or texture",
     "",
     "Force to segmenting mode"};
auv_menu(1,_What) -> 
    case wings_pref:get_value(advanced_menus) of
	false -> 
	    [{"Direct", segment,
	      "Open UV-window directly if selection already contains uv-coords"},
	     {"Force Segment", force_seg,
	      "Delete old UV-coords and start over with segmenting"}];
	true -> 
	    {?MODULE, segment}
    end;
auv_menu(2,_) -> ignore;
auv_menu(3,_) -> {?MODULE, force_seg}.

command({body,{?MODULE, Op}} , St) ->
    start_uvmap(Op, St);
command({face,{?MODULE, Op}} , St) ->
    start_uvmap(Op, St);
command({?MODULE, Op}, St) ->
    start_uvmap(Op, St);
command({window,uv_editor_window}, St) ->
    window(St);
command(_Cmd, _) -> 
    next.

window(St) ->
    start_uvmap(edit, St).

start_uvmap(edit, #st{sel=[]}) -> wings_u:error("Nothing selected");
start_uvmap(Action, #st{sel=Sel}=St) ->
    start_uvmap_1(Sel, Action, St).

start_uvmap_1([{Id,_}|T], Action, St) ->
    EditWin   = {autouv,Id},
    EditExists = wings_wm:is_window(EditWin),
    SegWin    = {autouv,{segment,Id}},
    SegExists = wings_wm:is_window(SegWin),
    
    case segment_or_edit(Action,Id,St) of
	{edit,Fs} when EditExists ->
	    wings_wm:send(EditWin, {add_faces,Fs,St}),
	    wings_wm:raise(EditWin);
	{seg_ui,Fs} when SegExists ->
	    wings_wm:send(SegWin, {add_faces,Fs,St}),
	    wings_wm:raise(SegWin);
	Op when element(1,Op) == edit ->	    
	    create_window(Op, EditWin, Id, St);
	Op ->	    
	    create_window(Op, SegWin, Id, St)
    end,
    start_uvmap_1(T, Action, St);
start_uvmap_1([], _, _) -> keep.

segment_or_edit(edit, _Id, _St) -> edit;
segment_or_edit(segment,Id,#st{selmode=face,sel=Sel,shapes=Shs}) ->
    We = gb_trees:get(Id, Shs),
    UVFs = gb_sets:from_ordset(wings_we:uv_mapped_faces(We)),
    {value,{_,Fs}} = lists:keysearch(Id, 1, Sel),
    case gb_sets:is_subset(Fs,UVFs) of
	false -> {seg_ui, Fs};
	true ->  {edit, Fs}
    end;
segment_or_edit(segment,Id,#st{shapes=Shs}) ->
    We = gb_trees:get(Id, Shs),
    case wings_we:uv_mapped_faces(We) of    
	[] -> {seg_ui,object};
	_ ->  {edit,object}
    end;
segment_or_edit(force_seg,Id,#st{selmode=face,sel=Sel}) -> 
    {value,{_,Fs}} = lists:keysearch(Id, 1, Sel),
    {seg_ui,Fs};
segment_or_edit(force_seg,_Id,_) ->
    {seg_ui,object}.
	    
create_window(Action, Name, Id, #st{shapes=Shs}=St) ->
    #we{name=ObjName} = We = gb_trees:get(Id, Shs),
    Op = {replace,fun(Ev) -> auv_event(Ev, St) end},
    Segment = if element(1,Action) == edit -> ""; true -> "Segmenting" end,
    Title = "AutoUV "++ Segment ++": " ++ ObjName,
    {X,Y,W,H} = init_drawarea(),
    Props = [{display_lists,Name}|wings_view:initial_properties()],
    CreateToolbar = fun(N, P, Wi) -> wings_toolbar:create(N, P, Wi) end,
    wings_wm:toplevel(Name, Title, {X,Y,highest}, {W,H},
		      [resizable,closable,menubar,{properties,Props},
		       {toolbar,CreateToolbar}], Op),
    wings_wm:send(Name, {init,{Action,We}}).

auv_event({init,Op}, St) ->
    wings:init_opengl(St),
    case Op of
	{{edit,What},We} -> start_edit(What, We, St);
	{{seg_ui,_},We} ->  auv_seg_ui:start(We, We, St)
    end;
auv_event(redraw, _) ->
    wings_wm:clear_background(),
    keep;
auv_event({crash,Crash}, _) ->
    wings_u:win_crash(Crash),
    delete;
auv_event(_Ev, _) -> keep.

%%%
%%% Start the UV editor.
%%%

start_edit(Mode, We, St) ->
    MatNames0 = wings_facemat:all(We),
    MatNames1 = sofs:from_external(MatNames0, [{face,material}]),
    MatNames2 = sofs:converse(MatNames1),
    MatNames3 = sofs:relation_to_family(MatNames2),
    MatNames4 = sofs:to_external(MatNames3),
    MatNames = [Mat || {Name,_}=Mat <- MatNames4, has_texture(Name, St)],
    case MatNames of
	[{MatName,_}] ->
	    do_edit(MatName, Mode, We, St);
	_ ->
	    do_edit(none, Mode, We, St)
    end.

do_edit(MatName, Mode, #we{id=Id}=We, #st{shapes=Shs0}=GeomSt) ->
    Shs = gb_trees:update(Id, We#we{fs=undefined,es=gb_trees:empty()}, Shs0),
    FakeGeomSt = GeomSt#st{sel=[],shapes=Shs},
    AuvSt = create_uv_state(gb_trees:empty(), MatName, Mode, We, FakeGeomSt),
    new_geom_state(GeomSt, AuvSt).

init_show_maps(Charts0, Fs, #we{name=WeName,id=Id}, GeomSt0) ->
    Charts1 = auv_placement:place_areas(Charts0),
    Charts = gb_trees:from_orddict(keysort(1, Charts1)),
    Tx = bg_image(),
    {GeomSt1,MatName} = add_material(Tx, WeName, none, GeomSt0),
    GeomSt = insert_initial_uvcoords(Charts, Id, MatName, GeomSt1),
    EditWin   = {autouv,Id},
    case wings_wm:is_window(EditWin) of
	true -> 
	    wings_wm:send(EditWin, {add_faces,Fs,GeomSt}),
	    wings_wm:send(geom, {new_state,GeomSt});
	false ->
	    create_window({edit,Fs},EditWin,Id,GeomSt),
	    wings_wm:send(geom, {new_state,GeomSt})
    end,
    cleanup_before_exit(),
    delete.

create_uv_state(Charts, MatName, Fs, We, GeomSt) ->
    wings:mode_restriction([vertex,edge,face,body]),
    wings_wm:current_state(#st{selmode=body,sel=[]}),
    Uvs = #uvstate{st=wpa:sel_set(face, [], GeomSt),
		   id=We#we.id,
		   mode=Fs,
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
		 hither=0.0001,
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
    wings_wm:set_prop(select_backface, true),

    wings_wm:later(got_focus),

    Win = wings_wm:this(),
    wings:register_postdraw_hook(Win, ?MODULE,
				 fun draw_background/1),
    wings_wm:menubar(Win, menubar()),
    wings_wm:send({menubar,Win}, {current_state,St}),

    St.

menubar() ->
    [{"Edit",edit,
      fun(_) ->
	      [{"Undo/Redo",undo_toggle,"Undo or redo the last command"},
	       {"Redo",redo,"Redo the last command that was undone"},
	       {"Undo",undo,"Undo the last command"}]
      end},
     {"Select",select,
      fun(St) ->
	      Menu0 = wings_sel_cmd:menu(St),
	      Menu = [I || I <- Menu0,
			   keep_sel_item(I)],
	      redundant_separators(Menu)
      end}].

keep_sel_item(separator) -> true;
keep_sel_item({_,more,_}) -> true;
keep_sel_item({_,less,_}) -> true;
keep_sel_item({_,similar,_}) -> true;
keep_sel_item({_,inverse,_}) -> true;
keep_sel_item({_,all,_}) -> true;
keep_sel_item({_,deselect,_}) -> true;
keep_sel_item({_,{edge_loop,_}}) -> true;
keep_sel_item({_,{adjacent,_}}) -> true;
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
    clear_temp_sel(St#st{bb=Uvs#uvstate{st=GeomSt}}).

%% update_uvs(Charts, We0) -> We
%%  Update the UV coordinates for the original model.
update_uvs(Cs, #we{es=Etab0}=GeomWe) ->
    update_uvs_1(Cs, GeomWe, Etab0).

update_uvs_1([#we{vp=Vpos0,name=#ch{vmap=Vmap}}=ChartWe|Cs],
	     We, Etab0) ->
    VFace0 = wings_face:fold_faces(
	       fun(Face, V, _, _, A) ->
		       [{V,Face}|A]
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
    wings_facemat:assign(MatName, Faces, We).


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

add_material(#e3d_image{}=Tx, Name, none, St0) ->
    MatName0 = list_to_atom(Name++"_auv"),
    Mat = {MatName0,[{opengl,[]},{maps,[{diffuse,Tx}]}]},
    case wings_material:add_materials([Mat], St0) of
	{St,[]} ->
	    {St,MatName0};
	{St,[{MatName0,MatName}]} ->
	    {St,MatName}
    end;
add_material(Im = #e3d_image{}, _, MatName,St) ->
    catch wings_material:update_image(MatName, diffuse, Im, St),
    {St,MatName}.
   
%%%% Menus.

command_menu(body, X, Y) ->
    Menu = [{basic,{"Chart operations",ignore}},
	    {basic,separator},
	    {"Move", move, "Move selected charts"},
	    {"Scale", {scale, scale_directions()++ stretch_directions()}, 
	     "Scale selected charts"},
	    {"Rotate", rotate, "Rotate selected charts"},
	    separator,
	    {"Move to", 
	     {move_to, 
	      [{"Center", center, "Move to Center"},
	       {"Center X", center_x, "Move to horizontal center"},
	       {"Center Y", center_y, "Move to vertical center"},
	       {"Bottom", bottom, "Move to bottom border"},
	       {"Top", top, "Move to top border"},
	       {"Left", left, "Move to left border"},
	       {"Right", right, "Move to right border"}
	      ]}, "Move charts to position"},
	    {"Flip",{flip,
		     [{"Horizontal",horizontal,"Flip selection horizontally"},
		      {"Vertical",vertical,"Flip selection vertically"}]},
	     "Flip selected charts"},
	    separator,
	    {"Tighten",tighten,
	     "Move UV coordinates towards average midpoint"},
	    separator,
	    {"Hide",hide,"Hide selected charts but keep UV-coordinates"},
	    {"Delete",delete,"Remove UV-coordinates for the selected charts"},
	    separator,
	    {"ReMap UV", {remap, [{"Stretch optimization", stretch_opt, 
				   "Optimize the chart stretch"},
				  separator,
				  {"Unfold", lsqcm, "Unfold the chart"},
				  {"Project Normal", project, 
				   "Project UVs from chart normal"},
				  {"Spherical", sphere, 
				   "Spherical mapping"}
				 ]}, 
	     "Calculate new UVs with chosen algorithm"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(face, X, Y) ->
    Scale = scale_directions(),
    Menu = [{basic,{"Face operations",ignore}},
	    {basic,separator},
	    {"Move",move,"Move selected faces",[magnet]},
	    {"Scale",{scale,Scale},"Scale selected faces"},
	    {"Rotate",rotate,"Rotate selected faces"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(edge, X, Y) ->
    Scale = scale_directions(),
    Align = 	    
	[{"Free",free,"Rotate selection freely"},
	 {"Chart to X", align_x, "Rotate chart to align selected edge to X-axis"},
	 {"Chart to Y", align_y, "Rotate chart to align selected edge to Y-axis"}],
    Menu = [{basic,{"Edge operations",ignore}},
	    {basic,separator},
	    {"Move",move,"Move selected edges",[magnet]},
	    {"Scale",{scale,Scale},"Scale selected edges"},
	    {"Rotate",{rotate,Align},"Rotate commands"},
	    separator,
	    {"Stitch", stitch, "Stitch edges/charts"},
	    {"Cut", cut_edges, "Cut selected edges"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(vertex, X, Y) ->
    Scale = scale_directions(),
    Align = 	    
	[{"Free",free,"Rotate selection freely"},
	 {"Chart to X", align_x, 
	  "Rotate chart to align (imaginary) edge joining selected verts to X-axis"},
	 {"Chart to Y", align_y, 
	  "Rotate chart to align (imaginary) edge joining selected verts to Y-axis"}],

    Menu = [{basic,{"Vertex operations",ignore}},
	    {basic,separator},
	    {"Move",move,"Move selected vertices",[magnet]},
	    {"Scale",{scale,Scale},"Scale selected vertices"},
	    {"Rotate",{rotate,Align},"Rotation commands"},
	    separator,
	    {"Flatten",{flatten,
			[{"X", x, "Flatten horizontally"},
			 {"Y", y, "Flatten vertically"}]}, 
	     "Flatten selected vertices"},
	    {"Tighten",tighten,
	     "Move UV coordinates towards average midpoint",
	     [magnet]},
	    separator, 
	    {"Unfold",lsqcm,"Unfold the chart (without moving the selected vertices)"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);
command_menu(_, X, Y) ->
    [_|Menu] = option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu).

stretch_directions() ->
    [{"Max Uniform",    max_uniform, "Maximize either horizontally or vertically"},
     {"Max Horizontal", max_x, "Maximize horizontally (X dir)"},
     {"Max Vertical",   max_y, "Maximize vertically (Y dir)"}].

scale_directions() ->
    [{"Uniform",    scale_uniform, "Scale in both directions"},
     {"Horizontal", scale_x, "Scale horizontally (X dir)"},
     {"Vertical",   scale_y, "Scale vertically (Y dir)"}].

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
    {replace,fun(Ev) -> ?MODULE:handle_event(Ev, St) end}.

handle_event({crash,Crash}, _) ->
    wings_u:win_crash(Crash),
    delete;
handle_event({command_error,Error}, _) ->
    wings_u:message(Error);
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
handle_event(revert_state, St) ->
    get_event(St);
handle_event({current_state,geom_display_lists,GeomSt}, AuvSt) ->
    new_geom_state(GeomSt, AuvSt);
handle_event({do_tweak, Type, St =#st{sh=Sh,selmode=Mode}}, _) ->
    case Type of 
	temp_selection ->
	    handle_command(move,St#st{temp_sel={Mode,Sh}});
	_ -> 
	    handle_command(move,St)
    end;
handle_event({cancel_tweak,Ev}, St) ->
    handle_event_1(Ev,St,wings_msg:free_lmb_modifier());
handle_event({add_faces,Fs,GeomSt}, St0) ->
    AuvSt0 = add_faces(Fs,St0),
    case update_geom_state(GeomSt, AuvSt0) of
	{AuvSt,true} ->
	    wings_wm:send(geom, {new_state,GeomSt}),
	    new_state(AuvSt);
	{AuvSt,false} ->
	    get_event(AuvSt)
    end;
handle_event(Ev, St) ->
    case wings_camera:event(Ev, St, fun() -> redraw(St) end) of
	next ->
	    FreeLmbMod = wings_msg:free_lmb_modifier(),
%%	    io:format("Ev ~W~n",[Ev,3]),
	    handle_event_0(Ev, St, FreeLmbMod);
	Other -> 
	    Other
    end.

%% Short cut for tweak like move
handle_event_0(Ev=#mousebutton{state=?SDL_PRESSED,
			       x=X,y=Y,
			       button=?SDL_BUTTON_LEFT,
			       mod=Mod},
	       #st{sel=Sel}=St0, FreeLmbMod) 
  when (Mod band 16#0FFF) == 0 -> %% No modifiers
    case (Sel == []) and wings_pref:get_value(use_temp_sel) of
	true ->
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} -> 
		    start_tweak(temp_selection, Ev, St);
		_ -> 
		    handle_event_1(Ev, St0, FreeLmbMod)
	    end;
	false ->
	    case wings_pick:do_pick(X,Y,St0) of
		{delete,_,_} ->
		    start_tweak(selection, Ev, St0);
		_ -> 
		    handle_event_1(Ev, St0, FreeLmbMod)
	    end
    end;
handle_event_0(Ev, St, FreeLmbMod) ->
    handle_event_1(Ev, St, FreeLmbMod).

handle_event_1(Ev, St, _) ->
    case wings_pick:event(Ev, St) of
	next -> handle_event_2(Ev, St);
	Other -> Other
    end.

handle_event_2(Ev, St) ->
    case wings_hotkey:event(Ev, St) of
	next -> handle_event_3(Ev, St);
	Cmd -> wings_wm:later({action,Cmd})
    end.

handle_event_3(#mousebutton{button=?SDL_BUTTON_RIGHT}=Ev,
	       #st{selmode=Mode0,sel=Sel}) ->
    %% Note: Basic menus must be shown when the right mouse button
    %% is PRESSED; advanced menus when the button is RELEASED.
    %% wings_menu:is_popup_event/1 takes care of that.
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} ->
	    Mode = case Sel of 
		       [] -> undefined; 
		       _ -> Mode0 
		   end,
	    command_menu(Mode, X, Y)
    end;
handle_event_3({drop,_,DropData}, St) ->
    handle_drop(DropData, St);
handle_event_3({action,{auv,create_texture}},_St) ->
    auv_texture:draw_options();
handle_event_3({action,{auv,{draw_options,Opt}}}, #st{bb=Uvs}=St) ->
    #uvstate{st=GeomSt0,matname=MatName0} = Uvs,
    Tx = ?SLOW(auv_texture:get_texture(St, Opt)),
    case MatName0 of 
	none -> 
	    Id = wings_image:new("UVmap", Tx),
	    wings_image:window(Id),
	    get_event(St);
	_ ->
	    {GeomSt,MatName} = 
		add_material(Tx#e3d_image{name=atom_to_list(MatName0)}, 
			     undefined, MatName0, GeomSt0),
	    wings_wm:send(geom, {new_state,GeomSt}),
	    get_event(St#st{bb=Uvs#uvstate{st=GeomSt,matname=MatName}})
    end;
handle_event_3({action,{auv,{remap,Method}}}, St0) ->
    St = remap(Method, St0),
    get_event(St);
handle_event_3({action,{auv,lsqcm}}, St0) ->
    St = reunfold(St0),
    get_event(St);

%% Others
handle_event_3({vec_command,Command,_St}, _) when is_function(Command) ->
    %% Use to execute command with vector arguments (see wings_vec.erl).
    catch Command();
handle_event_3(close, _St) ->
    cleanup_before_exit(),
    delete;
handle_event_3({callback,Fun}, _) when is_function(Fun) ->
    Fun();
handle_event_3({action,{auv,quit}}, _St) ->
    cleanup_before_exit(),
    delete;
handle_event_3({action,{auv,Cmd}}, St) ->
%%    io:format("Cmd ~p ~n", [Cmd]),
    handle_command(Cmd, St);
handle_event_3({action,{select,Command}}, St0) ->
    case wings_sel_cmd:command(Command, St0) of
	{save_state,St} -> ok;
	#st{}=St -> ok
    end,
    new_state(St);
handle_event_3({action,{edit,undo_toggle}}=Act, _) ->
    wings_wm:send(geom, Act);
handle_event_3({action,{edit,undo}}=Act, _) ->
    wings_wm:send(geom, Act);
handle_event_3({action,{edit,redo}}=Act, _) ->
    wings_wm:send(geom, Act);
handle_event_3({action,Ev}, St) ->
    case Ev of
	{_, {move,_}} ->
	    handle_command(move,St);
	{_, {rotate,_}} ->
	    handle_command({rotate,free},St);
	{_, {scale,{x,_}}} ->
	    handle_command({scale,scale_x},St);
	{_, {scale,{y,_}}} ->
	    handle_command({scale,scale_y},St);
	{_, {scale,_}} ->
	    handle_command({scale,scale_uniform},St);
	{view,aim} ->
	    St1 = fake_selection(St),
	    wings_view:command(aim, St1),
	    get_event(St);
	{view,Cmd} when Cmd == frame ->
	    wings_view:command(Cmd,St),
	    get_event(St);
	_ ->
	    keep
    end;
handle_event_3(got_focus, _) ->
    Msg1 = wings_msg:button_format("Select"),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], "Show menu"),
    Message = wings_msg:join([Msg1,Msg2,Msg3]),
    wings_wm:message(Message, ""),
    wings_wm:dirty();
handle_event_3(_Event, _) ->
%%    io:format("MissEvent ~p~n", [_Event]),
    keep.

clear_temp_sel(#st{temp_sel=none}=St) -> St;
clear_temp_sel(#st{temp_sel={Mode,Sh}}=St) ->
    St#st{temp_sel=none,selmode=Mode,sh=Sh,sel=[]}.

-record(tweak, {type, st, pos, ev}).

start_tweak(Type, Ev = #mousebutton{x=X,y=Y}, St0) ->
%%    io:format("Start tweak~n", []),
    T = #tweak{type=Type,st=St0,pos={X,Y}, ev=Ev},
    {seq,push,get_tweak_event(T)}.

get_tweak_event(T) ->
    {replace,fun(Ev) -> tweak_event(Ev, T) end}.
tweak_event(#mousemotion{x=X,y=Y}, #tweak{pos={Sx,Sy},type=Type,st=St}) ->
    case (abs(X-Sx) > 2) orelse (abs(Y-Sy) > 2) of
	true -> 
	    if Type == temp_selection ->
		    wings_wm:later(clear_selection);
	       true -> ignore
	    end,
	    wings_wm:later({do_tweak,Type,St}),
	    pop;
	false ->
	    keep
    end;
tweak_event(Other, #tweak{ev=Ev}) ->
    wings_wm:later(Other),
    wings_wm:later({cancel_tweak,Ev}),
    pop.

new_state(#st{bb=#uvstate{}=Uvs}=St0) ->
    GeomSt = update_geom_selection(St0),
    St1 = St0#st{bb=Uvs#uvstate{st=GeomSt}},
    St = update_selected_uvcoords(St1),
    get_event(St).

handle_command(move, St) ->
    drag(wings_move:setup(free_2d, St));
handle_command({move,Magnet}, St) ->
    drag(wings_move:setup({free_2d,Magnet}, St));
handle_command({scale,scale_uniform}, St) ->
    drag(wings_scale:setup({uniform,center}, St));
handle_command({scale,scale_x}, St) ->
    drag(wings_scale:setup({x,center}, St));
handle_command({scale,scale_y}, St) ->
    drag(wings_scale:setup({y,center}, St));
handle_command({scale,Dir}, St0) -> %% Maximize chart
    St1 = wpa:sel_map(fun(_, We) -> stretch(Dir,We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command(rotate, St) ->
    drag(wings_rotate:setup({free,center}, St));
handle_command({rotate,free}, St) ->
    drag(wings_rotate:setup({free,center}, St));
handle_command({move_to,Dir}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> move_to(Dir,We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command({flip,horizontal}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> flip_horizontal(We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command({flip,vertical}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> flip_vertical(We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command({rotate,Dir}, St0) 
  when Dir == align_y; Dir == align_x; Dir == align_xy ->
    St1 = align_chart(Dir, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command({rotate,Deg}, St0) ->
    St1 = wpa:sel_map(fun(_, We) -> rotate_chart(Deg, We) end, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command(tighten, St) ->
    tighten(St);
handle_command({tighten,Magnet}, St) ->
    tighten(Magnet, St);
handle_command(delete, St) ->
    get_event(delete_charts(St));
handle_command(hide, St) ->
    get_event(hide_charts(St));
handle_command({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun handle_command/2);
handle_command({flatten, Plane}, St0 = #st{selmode=vertex}) ->
    {save_state, St1} = wings_vertex_cmd:flatten(Plane, St0),
    St = update_selected_uvcoords(St1),
    get_event(St);
handle_command(stitch, St0 = #st{selmode=edge}) ->
    St1 = stitch(St0),
    AuvSt = #st{bb=#uvstate{id=Id,st=Geom}} = update_selected_uvcoords(St1),
    %% Do something here, i.e. restart uvmapper.
    St = rebuild_charts(gb_trees:get(Id,Geom#st.shapes), AuvSt, []),
    get_event(St);
handle_command(cut_edges, St0 = #st{selmode=edge,bb=#uvstate{id=Id,st=Geom}}) ->
    Es = wpa:sel_fold(fun(Es,We=#we{name=#ch{emap=Emap},es=Etab},A) ->
			      Vis = gb_sets:from_list(wings_we:visible(We)),
			      A ++ [auv_segment:map_edge(E,Emap)
				    || E <-gb_sets:to_list(Es),
				       begin 
					   #edge{lf=LF,rf=RF} = gb_trees:get(E,Etab),
					   gb_sets:is_member(LF,Vis) and 
					       gb_sets:is_member(RF,Vis)
				       end]
		      end, [], St0),
    %% Do something here, i.e. restart uvmapper.
    St1 = rebuild_charts(gb_trees:get(Id,Geom#st.shapes), St0, Es),
    %% Displace charts some distance
    St2 = displace_cuts(Es, St1),
    St  = update_selected_uvcoords(St2),
    get_event(St);

%%    get_event(St);
handle_command(_, #st{sel=[]}) ->
    keep.

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

add_faces(NewFs,St0=#st{bb=ASt=#uvstate{id=Id,mode=Mode,st=GeomSt=#st{shapes=Shs0}}}) ->
    case {NewFs,Mode} of
	{_,object} -> St0;
	{object,_} -> %% Force a chart rebuild, we are switching object mode
	    We = gb_trees:get(Id,Shs0),
	    Shs = gb_trees:update(Id, We#we{fs=undefined,es=gb_trees:empty()}, Shs0),
	    Fake = GeomSt#st{sel=[],shapes=Shs},
	    St0#st{bb=ASt#uvstate{mode=object,st=Fake}};
	{NewFs,Fs0} ->
	    Fs = gb_sets:union(NewFs,Fs0),
	    case gb_sets:intersection(NewFs,Fs0) of
		NewFs -> 
		    St0#st{bb=ASt#uvstate{mode=Fs}};
		_ ->  %% Some new faces should be shown, force a chart rebuild
		    We = gb_trees:get(Id,Shs0),
		    Shs = gb_trees:update(Id, We#we{fs=undefined,es=gb_trees:empty()}, Shs0),
		    Fake = GeomSt#st{sel=[],shapes=Shs},
		    St0#st{bb=ASt#uvstate{st=Fake,mode=Fs}}
	    end
    end.

drag({drag,Drag}) ->
    wings:mode_restriction([vertex,edge,face,body]),
    wings_wm:set_prop(show_info_text, true),
    wings_drag:do_drag(Drag, none).

tighten(#st{selmode=vertex}=St) ->
    tighten_1(fun vertex_tighten/3, St);
tighten(#st{selmode=body}=St) ->
    tighten_1(fun(_, We, A) -> body_tighten(We, A) end, St).

tighten_1(Tighten, St) ->    
    Tvs = wings_sel:fold(Tighten, [], St),
    {drag,Drag} = wings_drag:setup(Tvs, [percent], St),
    wings_drag:do_drag(Drag, none).

vertex_tighten(Vs0, We, A) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    Vs = [V || V <- gb_sets:to_list(Vs0), not_bordering(V, Vis, We)],
    wings_vertex_cmd:tighten(Vs, We, A).

body_tighten(#we{vp=Vtab}=We, A) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    Vs = [V || V <- gb_trees:keys(Vtab), not_bordering(V, Vis, We)],
    wings_vertex_cmd:tighten(Vs, We, A).

tighten(Magnet, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, A) ->
				 mag_vertex_tighten(Vs, We, Magnet, A)
			 end, [], St),
    Flags = wings_magnet:flags(Magnet, []),
    {drag,Drag} = wings_drag:setup(Tvs, [percent,falloff], Flags, St),
    wings_drag:do_drag(Drag, none).

mag_vertex_tighten(Vs0, We, Magnet, A) ->
    Vis = gb_sets:from_ordset(wings_we:visible(We)),
    Vs = [V || V <- gb_sets:to_list(Vs0), not_bordering(V, Vis, We)],
    wings_vertex_cmd:tighten(Vs, We, Magnet, A).

not_bordering(V, Vis, We) ->
    wings_vertex:fold(fun(_, _, _, false) -> false;
			 (_, F, _, true) -> gb_sets:is_member(F, Vis)
		      end, true, V, We).

hide_charts(#st{shapes=Shs0,bb=UVs}=St) ->
    Shs = wpa:sel_fold(fun(_, #we{id=Id}, Shs) ->
			       gb_trees:delete(Id, Shs)
		       end, Shs0, St),
    Fs = foldl(fun(#we{fs=Ftab},Acc) ->
		       Fs = gb_sets:from_ordset(gb_trees:keys(Ftab)),
		       gb_sets:union(Fs,Acc)
	       end, gb_sets:empty(), gb_trees:values(Shs)),
    St#st{shapes=Shs,sel=[],bb=UVs#uvstate{mode=Fs}}.

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

stitch(St0) ->
    Map0 = wpa:sel_fold(fun(Es,#we{id=Id,name=#ch{emap=Emap}},A) ->
				[{auv_segment:map_edge(E,Emap),{Id,E}}
				 || E<-gb_sets:to_list(Es)] ++ A
			end, [], St0),
    Map1 = sofs:to_external(sofs:relation_to_family(sofs:relation(Map0))),
    foldl(fun stitch/2, St0, Map1).

stitch({_E, [_]}, St) -> St; %% Both edges not selected ignore
stitch({_E, [{Id,E1id},{Id,E2id}]},St0 = #st{shapes=Sh0}) -> %% Same We
    We = #we{name=#ch{vmap=Vmap},es=Etab,vp=Vpos0} = gb_trees:get(Id,Sh0),
    #edge{vs=Vs1,ve=Ve1} = gb_trees:get(E1id,Etab),
    #edge{vs=Vs2,ve=Ve2} = gb_trees:get(E2id,Etab),
    Vs1map = auv_segment:map_vertex(Vs1, Vmap),
    Same = case auv_segment:map_vertex(Vs2, Vmap) of
	       Vs1map -> [{Vs1,Vs2},{Ve1,Ve2}];
	       _ -> [{Vs1,Ve2},{Ve1,Vs2}]
	   end,
    Vpos = average_pos(Same, Vpos0),
    St0#st{shapes = gb_trees:update(Id, We#we{vp=Vpos}, Sh0)};

stitch({_E, [{Id1,E1id},{Id2,E2id}]}, St0 = #st{shapes=Sh0}) -> % Merge Charts
    We1 = #we{name=#ch{vmap=Vmap1},es=Etab1,vp=Vpos1} = gb_trees:get(Id1,Sh0),
    We2 = #we{name=#ch{vmap=Vmap2},es=Etab2,vp=Vpos2} = gb_trees:get(Id2,Sh0),
    #edge{vs=Vs1,ve=Ve1} = gb_trees:get(E1id,Etab1),
    #edge{vs=Vs2,ve=Ve2} = gb_trees:get(E2id,Etab2),
    Vs1map = auv_segment:map_vertex(Vs1, Vmap1),
    Same = case auv_segment:map_vertex(Vs2, Vmap2) of
	       Vs1map -> [{Vs1,Vs2},{Ve1,Ve2}];
	       _ -> [{Vs1,Ve2},{Ve1,Vs2}]
	   end,
    {Vp1,Vp2} = average_pos(Same, Vpos1, Vpos2),
    Sh1 = gb_trees:update(Id1, We1#we{vp=Vp1}, Sh0),
    Sh  = gb_trees:update(Id2, We2#we{vp=Vp2}, Sh1),
    St0#st{shapes = Sh}.

average_pos([{V1,V2}|R], Vpos0) ->
    Pos = e3d_vec:average(gb_trees:get(V1,Vpos0),gb_trees:get(V2,Vpos0)),
    Vpos1 = gb_trees:update(V1,Pos,Vpos0),
    Vpos  = gb_trees:update(V2,Pos,Vpos1),
    average_pos(R, Vpos);
average_pos([],Vpos) -> Vpos.

average_pos([{V1,V2}|R], Vpos1,Vpos2) ->
    Pos = e3d_vec:average(gb_trees:get(V1,Vpos1),gb_trees:get(V2,Vpos2)),
    Vp1 = gb_trees:update(V1,Pos,Vpos1),
    Vp2 = gb_trees:update(V2,Pos,Vpos2),
    average_pos(R, Vp1,Vp2);
average_pos([],Vpos1,Vpos2) -> {Vpos1,Vpos2}.

displace_cuts(SelEs,St=#st{shapes=Sh,bb=#uvstate{id=WeId,st=#st{shapes=GSh}}})->
    Elinks = wings_edge_loop:partition_edges(SelEs,gb_trees:get(WeId,GSh)),
    AuvEdsGroups = [edge_sel_to_edge(gb_trees:to_list(Sh),Es,[]) || Es <- Elinks],
    %% AuvEdsGroups = [[{id,auv_eds},..], [{id,auv_eds},..]]
    MapEds = fun({Id,Es},A) ->
		     #we{name=#ch{emap=Emap}} = gb_trees:get(Id,Sh),
		     [{auv_segment:map_edge(E,Emap),{Id,E}}
		      || E<-gb_sets:to_list(Es)] ++ A
	     end,
    MapAndClusterEds =
	fun(AuvEds) ->
		Mapped = foldl(MapEds, [], AuvEds),
		sofs:to_external(sofs:relation_to_family(sofs:relation(Mapped)))
	end,
    Remapped = [MapAndClusterEds(EdgeGroup) || EdgeGroup <- AuvEdsGroups],
    %% Remapped = [[{GeomEdge, [{AuvWeId,AuvEdge1},{AuvWeId2,AuvEdge2}]},..]
    Displaced = foldl(fun displace_cuts1/2, Sh, Remapped),
    %% Update selection to all new edges
    Sel0 = lists:append([WeEds || {_,WeEds} <- lists:append(Remapped)]),
    Sel1 = sofs:to_external(sofs:relation_to_family(
			      sofs:relation(Sel0))),
    Sel = [{Id,gb_sets:from_ordset(Eds)} || {Id,Eds} <- Sel1],
    St#st{sel=Sel,shapes=Displaced}.

displace_cuts1(Eds, Sh0) -> 
    Sh1 = displace_edges(Eds,Sh0),
    displace_charts(Eds,gb_sets:empty(),Sh1).

displace_edges([{_,[{Id,Edge1},{Id,Edge2}]}|Eds], Sh) ->
    We = #we{name=#ch{vmap=Vmap},es=Etab,vp=Vpos0} = gb_trees:get(Id,Sh),
    #edge{vs=Vs1,ve=Ve1} = gb_trees:get(Edge1,Etab),
    #edge{vs=Vs2,ve=Ve2} = gb_trees:get(Edge2,Etab),
    %% Get the vertices
    Vs1map = auv_segment:map_vertex(Vs1, Vmap),
    Same = case auv_segment:map_vertex(Vs2, Vmap) of
	       Vs1map -> [{Vs1,Vs2},{Ve1,Ve2}];
	       _ -> [{Vs1,Ve2},{Ve1,Vs2}]
	   end,   
    Vs = [{V1,V2} || {V1,V2} <- Same,
		     V1 /= V2,
		     gb_trees:get(V1,Vpos0) == gb_trees:get(V2,Vpos0)],
    case Vs of
	[] ->  %% Already displaced
	    displace_edges(Eds,Sh);
	_ ->
	    %% What Direction should we displace the verts?
	    [Move1,Move2] = displace_dirs(0.005,Edge1,We),
	    %% Make the move
	    Vpos = foldl(fun({V1,V2},VpIn) ->
				 Pos1 = e3d_vec:add(Move1,gb_trees:get(V1,Vpos0)),
				 Vpos1 = gb_trees:update(V1,Pos1,VpIn),
				 Pos2 = e3d_vec:add(Move2,gb_trees:get(V2,Vpos0)),
				 gb_trees:update(V2,Pos2,Vpos1)
			 end, Vpos0, Vs),	    
	    displace_edges(Eds,gb_trees:update(Id, We#we{vp=Vpos},Sh))
    end;
displace_edges([_Skip|Eds], Sh) ->
    displace_edges(Eds,Sh);
displace_edges([],Sh) -> Sh.

displace_charts([],_,Sh) -> Sh;
displace_charts([{_,[{Id,_},{Id,_}]}|Eds],Moved,Sh) ->
    displace_charts(Eds,Moved,Sh);
displace_charts([{_,[{Id1,_},{Id2,_}]}|Eds], Moved, Sh) ->
    case gb_sets:is_member(Id1,Moved) or gb_sets:is_member(Id2,Moved) of
	true -> displace_charts(Eds,Moved,Sh);
	false ->
	    We0 = #we{vp=Vpos0} = gb_trees:get(Id1,Sh),
	    C1 = wings_vertex:center(We0),
	    C2 = wings_vertex:center(gb_trees:get(Id2,Sh)),
	    Move = e3d_vec:mul(e3d_vec:norm(e3d_vec:sub(C1,C2)),0.01),
	    Vpos= [{V,e3d_vec:add(Pos,Move)} || 
		      {V,Pos} <- gb_trees:to_list(Vpos0)],
	    We = We0#we{vp=gb_trees:from_orddict(Vpos)},
	    displace_charts(Eds,gb_sets:add(Id1,Moved),
			    gb_trees:update(Id1,We,Sh))
    end.

displace_dirs(Dist,Edge1,We = #we{es=Etab,vp=Vpos}) ->
    #edge{vs=Vs1,ve=Ve1,lf=LF,rf=RF} = gb_trees:get(Edge1,Etab),
    Vp1 = gb_trees:get(Vs1,Vpos),
    Vp2 = gb_trees:get(Ve1,Vpos),
    {Dx,Dy,_} = e3d_vec:norm(e3d_vec:sub(Vp1,Vp2)),
    Dir = {Dy,-Dx,0.0},
    EdgeFace1 = if LF < 0 -> RF; true -> LF end,
    FaceCenter = wings_face:center(EdgeFace1, We),
    FaceDir = e3d_vec:sub(e3d_vec:average(Vp1,Vp2),FaceCenter),
    Moves = [e3d_vec:mul(Dir,-Dist),e3d_vec:mul(Dir,Dist)],
    case e3d_vec:dot(FaceDir,Dir) > 0.0 of
	true -> Moves;
	false -> reverse(Moves)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    keep.

is_power_of_two(X) ->
    (X band -X ) == X.

%%%
%%% Update charts from new state of Geometry window.
%%%

new_geom_state(GeomSt, AuvSt0) ->
    case update_geom_state(GeomSt, AuvSt0) of
	{AuvSt,true} -> get_event(AuvSt);
	{AuvSt,false} -> get_event_nodraw(AuvSt);
	delete ->
	    cleanup_before_exit(),
	    delete
    end.

update_geom_state(#st{mat=Mat,shapes=Shs}=GeomSt, AuvSt0) ->
    case new_geom_state_1(Shs, AuvSt0#st{mat=Mat}) of
	{AuvSt1,ForceRefresh0} ->
	    {AuvSt,ForceRefresh1} = update_selection(GeomSt, AuvSt1),
	    {AuvSt,ForceRefresh0 or ForceRefresh1};
	Other -> Other  %% delete
    end.

new_geom_state_1(Shs, #st{bb=#uvstate{id=Id,st=#st{shapes=Orig}}}=AuvSt) ->
    case {gb_trees:lookup(Id, Shs),gb_trees:lookup(Id, Orig)} of
	{none,_} -> delete;
	{{value,We},{value,We}} -> {AuvSt,false};
	{{value,#we{es=Etab}},{value,#we{es=Etab}}} -> {AuvSt,false};
	{{value,We},_} -> {rebuild_charts(We, AuvSt, []),true}
    end.

rebuild_charts(We, St = #st{bb=UVS=#uvstate{st=Old,mode=Mode}}, ExtraCuts) ->
    {Faces,FvUvMap} = auv_segment:fv_to_uv_map(Mode,We),
    {Charts0,Cuts0} = auv_segment:uv_to_charts(Faces, FvUvMap, We),
    {Charts1,Cuts} =
	case ExtraCuts of
	    [] -> {Charts0,Cuts0};
	    _ ->
		Cuts1 = gb_sets:union(Cuts0, gb_sets:from_list(ExtraCuts)),
		auv_segment:normalize_charts(Charts0, Cuts1, We)
	end,
    Charts2 = auv_segment:cut_model(Charts1, Cuts, We),
    Charts = update_uv_tab(Charts2, FvUvMap),
    wings_wm:set_prop(wireframed_objects,
		      gb_sets:from_ordset(lists:seq(1, length(Charts2)))),
    St#st{sel=[],bb=UVS#uvstate{mode=update_mode(Faces,We),st=Old#st{sel=[]}},
	  shapes=Charts}.

update_mode(Faces0, #we{fs=Ftab}) ->
    Fs = gb_sets:from_list(Faces0),
    case gb_sets:size(Fs) == gb_trees:size(Ftab) of 
	true -> object;
	false -> Fs
    end.
	    
update_uv_tab(Cs, FvUvMap) ->
    update_uv_tab_1(Cs, FvUvMap, []).

update_uv_tab_1([#we{id=Id,name=#ch{vmap=Vmap}}=We0|Cs], FvUvMap, Acc) ->
    Fs = wings_we:visible(We0),
    UVs0 = wings_face:fold_faces(
	     fun(F, V, _, _, A) ->
		     OrigV = auv_segment:map_vertex(V, Vmap),
		     [{V,[F|OrigV]}|A]
	     end, [], Fs, We0),
    case update_uv_tab_2(sort(UVs0), FvUvMap, 0.0, []) of
	error ->
	    %% No UV coordinate for at least some vertices (probably
	    %% all) in the chart. Throw away this chart.
	    update_uv_tab_1(Cs, FvUvMap, Acc);
	UVs1 ->
	    UVs = gb_trees:from_orddict(UVs1),
	    We = We0#we{vp=UVs},
	    update_uv_tab_1(Cs, FvUvMap, [{Id,We}|Acc])
    end;
update_uv_tab_1([], _, Acc) ->
    gb_trees:from_orddict(sort(Acc)).

update_uv_tab_2([{V,_}|T], FvUvMap, Z, [{V,_}|_]=Acc) ->
    update_uv_tab_2(T, FvUvMap, Z, Acc);
update_uv_tab_2([{V,Key}|T], FvUvMap, Z, Acc) ->
    case gb_trees:get(Key, FvUvMap) of
	{X,Y} ->
	    Pos = {X,Y,Z},
	    update_uv_tab_2(T, FvUvMap, Z, [{V,Pos}|Acc]);
	_ ->
	    %% No UV-coordinate for this vertex. Abandon the entire chart.
	    error
    end;
update_uv_tab_2([], _, _, Acc) -> reverse(Acc).

%% update_selection(GemoSt, AuvSt0) -> AuvSt
%%  Update the selection in the AutoUV window given a selection
%%  from a geometry window.
update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #st{bb=#uvstate{st=#st{selmode=Mode,sel=Sel}}=Uvs}=AuvSt) ->
    {AuvSt#st{bb=Uvs#uvstate{st=St}},false};
update_selection(#st{selmode=Mode,sel=Sel}=St0,
		 #st{selmode=AuvMode,bb=#uvstate{id=Id}=Uvs,
		     shapes=Charts0}=AuvSt0) ->
    Charts = gb_trees:to_list(Charts0),
    {case keysearch(Id, 1, Sel) of
	 false ->
	     %% No selection in any chart - clear selection.
	     AuvSt0#st{sel=[],bb=Uvs#uvstate{st=St0}};
	 {value,{Id,Elems0}} when AuvMode == body ->
	     %% Body selection in charts - must be specially handled.
	     Elems = gb_sets:to_list(Elems0),
	     NewSel = update_body_sel(Mode, Elems, Charts),
	     AuvSt0#st{sel=sort(NewSel),sh=false,bb=Uvs#uvstate{st=St0}};
	 {value,{Id,Elems0}} when AuvMode =:= Mode->
	     %% Same selection mode in Geometry and AutoUV.
	     Elems = gb_sets:to_list(Elems0),
	     NewSel = update_selection_1(AuvMode, Elems, Charts),
	     AuvSt0#st{sel=sort(NewSel),sh=false,bb=Uvs#uvstate{st=St0}};
	 {value,IdElems} ->
	     %% Different selection modes. Convert Geom selection to
	     %% the mode in AutoUV.
	     St = St0#st{sel=[IdElems]},
	     #st{sel=[{Id,Elems0}]} = wings_sel_conv:mode(AuvMode, St),
	     Elems = gb_sets:to_list(Elems0),
	     NewSel = update_selection_1(AuvMode, Elems, Charts),
	     AuvSt0#st{sel=sort(NewSel),sh=false,bb=Uvs#uvstate{st=St0}}
     end,true}.

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
update_body_sel(_Mode, _, _) ->
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

update_geom_selection(#st{temp_sel={_,_},bb=#uvstate{st=GeomSt}}) ->
    GeomSt;
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


%%%% GUI Operations

rotate_chart(Angle, We) ->
    Center = wings_vertex:center(We),
    rotate_chart(Angle, Center, We).

rotate_chart(Angle, Center, We) ->
    Rot0 = e3d_mat:translate(e3d_vec:neg(Center)),
    Rot1 = e3d_mat:mul(e3d_mat:rotate(float(Angle), {0.0,0.0,1.0}), Rot0),
    Rot = e3d_mat:mul(e3d_mat:translate(Center), Rot1),
    wings_we:transform_vs(Rot, We).

align_chart(Dir, St = #st{selmode=Mode}) ->
    wings_sel:map(
      fun(Sel, We = #we{vp=Vtab,es=Etab}) ->
	      case gb_sets:to_list(Sel) of
		  [V1,V2] when Mode == vertex -> 
		      align_chart(Dir,gb_trees:get(V1,Vtab),
				  gb_trees:get(V2,Vtab),
				  We);
		  [E] when Mode == edge -> 
		      #edge{vs=V1,ve=V2} = gb_trees:get(E, Etab),
		      align_chart(Dir,gb_trees:get(V1,Vtab),
				  gb_trees:get(V2,Vtab),
				  We);
		  _ -> align_error()
	      end
      end, St).

align_chart(Dir, V1={X1,Y1,_},V2={X2,Y2,_}, We) ->
    Deg0 = 180.0/math:pi() *
	case Dir of
	    align_x -> math:atan2(Y2-Y1,X2-X1);
	    align_y -> math:atan2(X1-X2,Y2-Y1);
	    align_xy -> math:atan2(Y2-Y1,X2-X1) -
			    45/180*math:pi()
	end,
    Deg = if abs(Deg0) < 90.0 -> Deg0;
	     true -> Deg0 + 180
	  end,
    Center = e3d_vec:average(V1,V2),
    rotate_chart(-Deg,Center,We).

align_error() ->
    wings_u:error("Select two vertices or one edge").

flip_horizontal(We) ->
    flip(e3d_mat:scale(-1.0, 1.0, 1.0), We).

flip_vertical(We) ->
    flip(e3d_mat:scale(1.0, -1.0, 1.0), We).

flip(Flip, We0) ->
    Center = wings_vertex:center(We0),
    T0 = e3d_mat:translate(e3d_vec:neg(Center)),
    T1 = e3d_mat:mul(Flip, T0),
    T = e3d_mat:mul(e3d_mat:translate(Center), T1),
    We = wings_we:transform_vs(T, We0),
    wings_we:invert_normals(We).

move_to(Dir,We) ->
    [V1={X1,Y1,_},V2={X2,Y2,_}] = wings_vertex:bounding_box(We),
    ChartCenter = {CCX,CCY,CCZ} = e3d_vec:average(V1,V2),
    Translate
	= case Dir of
	      center ->   e3d_vec:sub({0.5,0.5,CCZ}, ChartCenter);
	      center_x -> e3d_vec:sub({0.5,CCY,CCZ}, ChartCenter);
	      center_y -> e3d_vec:sub({CCX,0.5,CCZ}, ChartCenter);
	      bottom ->   {0.0,-Y1,0.0};
	      top ->      {0.0,1.0-Y2,0.0};   
	      left ->     {-X1,0.0,0.0};
	      right ->    {1.0-X2,0.0,0.0}
	  end,
    T = e3d_mat:translate(Translate),
    wings_we:transform_vs(T, We).

stretch(Dir,We) ->
    [{X1,Y1,_},{X2,Y2,_}] = wings_vertex:bounding_box(We),
    Center = {CX,CY,CZ} = {X1+(X2-X1)/2, Y1+(Y2-Y1)/2, 0.0},
    T0 = e3d_mat:translate(e3d_vec:neg(Center)),
    SX0 = 1.0/(X2-X1), SY0= 1.0/(Y2-Y1),
    {SX,SY} = case Dir of
		  max_x -> {SX0, 1.0};
		  max_y -> {1.0, SY0};
		  max_uniform when SX0 < SY0 -> 
		      {SX0, SX0};
		  _ ->
		      {SY0, SY0}
	      end,
    Stretch = e3d_mat:scale(SX, SY, 1.0),
    T1 = e3d_mat:mul(Stretch, T0),
    Pos = case Dir of
	      max_uniform -> {0.5,0.5,CZ};
	      max_x -> {0.5,CY,CZ};
	      max_y -> {CX,0.5,CZ}
	  end,    
    T = e3d_mat:mul(e3d_mat:translate(Pos), T1),
    wings_we:transform_vs(T, We).
    
reunfold(#st{sel=Sel,selmode=vertex}=St0) ->
    %% Check correct pinning.
    Ch = fun(Vs, _, _) ->
		 case gb_sets:size(Vs) of
		     N when N < 2 ->
			 E = "At least two vertices per chart must be pinned",
			 wpa:error(E);
		     _-> ok
		 end
	 end,
    wings_sel:fold(Ch, ok, St0),

    %% OK. Go ahead and re-unfold.
    wings_pb:start("remapping"),
    wings_pb:update(0.001),
    N = length(Sel),
    R = fun(Vs, #we{vp=Vtab}=We, I) ->
		Msg = "chart " ++ integer_to_list(I+1),
		wings_pb:update(I/N, Msg),
		Pinned = [begin
			      {S,T,_} = gb_trees:get(V, Vtab),
			      {V,{S,T}}
			  end || V <- gb_sets:to_list(Vs)],
		{remap(lsqcm, Pinned, We, St0),I+1}
	end,
    {St,_} = wings_sel:mapfold(R, 1, St0),
    wings_pb:done(update_selected_uvcoords(St)).

remap(Method, #st{sel=Sel}=St0) ->
    wings_pb:start("remapping"),
    wings_pb:update(0.001),
    N = length(Sel),
    Remap = fun(_, We, I) ->
		    Msg = "chart " ++ integer_to_list(I+1),
		    wings_pb:update(I/N, Msg),
		    {remap(Method, none, We, St0),I+1}
	    end,
    {St,_} = wings_sel:mapfold(Remap, 1, St0),
    wings_pb:done(update_selected_uvcoords(St)).

remap(stretch_opt, _, We, St) ->
    Vs3d = orig_pos(We, St),
    ?SLOW(auv_mapping:stretch_opt(We, Vs3d));
remap(Type, Pinned, #we{name=Ch}=We0, St) ->
    [Lower,Upper] = wings_vertex:bounding_box(We0),
    {W,H,_} = e3d_vec:sub(Upper, Lower),

    %% Get 3d positions (even for mapped vs).
    Vs3d = orig_pos(We0, St),
    case auv_mapping:map_chart(Type, We0#we{vp=Vs3d}, Pinned) of
	{error,Msg} -> 
	    wpa:error(Msg);
	Vs0 -> 
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
		   name=Ch#ch{size={Dx*Scale, Dy*Scale}}}
    end.

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
    
cleanup_before_exit() ->
    wings:unregister_postdraw_hook(wings_wm:this(), ?MODULE),
    wings_dl:delete_dlists().

%% Generate a checkerboard image of 4x4 squares 
%% with given side length in pixels.

compressed_bg() ->
<<131,80,0,0,12,5,120,156,181,211,193,106,19,81,20,6,224,60,128,111,225,131,248,
 12,130,224,170,43,209,141,138,96,187,201,74,208,165,136,139,212,214,82,187,105,
 153,133,40,184,42,177,138,132,80,170,196,90,104,67,179,154,42,169,138,88,81,132,
 136,1,67,146,241,159,254,230,207,157,51,167,205,184,72,249,25,78,134,225,59,247,
 158,123,123,189,84,58,83,186,176,116,197,205,167,185,190,155,229,155,127,86,230,
 250,249,36,107,103,221,80,123,253,254,221,73,254,239,221,33,243,125,117,144,250,
 179,61,104,31,118,135,100,85,140,205,246,122,154,172,159,36,137,235,147,69,240,
 1,106,249,248,73,252,213,234,32,227,67,174,95,75,51,106,33,31,91,8,119,65,31,
 239,213,133,181,124,224,236,146,241,33,171,75,214,191,247,114,9,81,139,208,231,
 100,140,143,149,35,254,124,70,120,126,62,42,66,63,172,195,249,100,10,131,159,48,
 31,108,33,244,177,114,115,190,139,55,126,105,62,206,252,57,249,220,124,138,223,
 79,247,114,158,114,63,239,60,232,187,169,156,187,236,230,197,252,140,155,100,
 230,188,27,106,173,120,40,153,53,181,184,182,173,132,254,215,184,33,153,245,216,
 108,108,165,201,250,152,167,240,167,207,7,242,241,190,122,235,33,19,250,120,63,
 94,249,113,61,198,43,119,211,140,90,200,135,44,60,244,221,249,208,252,183,242,
 208,199,95,182,144,15,89,187,40,232,103,158,147,124,60,209,66,7,49,209,199,226,
 247,170,21,60,139,251,97,139,137,62,112,20,120,102,124,78,158,167,16,248,211,
 187,159,157,242,186,155,103,107,143,221,204,239,189,117,115,123,225,146,27,106,
 189,230,23,197,248,159,219,31,17,227,199,63,127,152,66,96,235,96,27,49,62,102,
 216,141,118,152,208,135,220,168,111,33,106,65,13,223,155,66,248,147,141,69,68,
 45,228,187,243,193,123,118,97,49,209,135,204,2,239,141,31,14,199,248,220,130,
 241,49,25,196,248,74,222,15,135,99,124,110,193,248,213,118,140,20,247,221,243,
 229,228,121,10,69,230,163,243,53,254,244,238,103,173,118,213,205,197,104,193,77,
 167,94,118,83,142,58,110,168,29,29,237,152,130,218,155,195,3,38,239,247,190,53,
 17,227,55,15,123,140,241,49,46,83,80,195,207,251,155,27,136,90,8,239,182,34,68,
 45,168,225,251,104,179,139,168,133,88,172,28,201,251,166,160,150,254,191,28,23,
 104,97,124,83,200,223,223,127,132,228,125,206,7,91,112,125,51,159,83,252,255,
 154,143,252,252,124,92,31,43,231,124,80,20,89,63,39,207,83,48,62,207,23,71,16,
 250,211,187,159,127,1,245,246,60,42>>.

bg_image() ->
    Orig = binary_to_term(compressed_bg()),
    Pixels = repeat_image(Orig, []),
    Width = Height = 256,
    #e3d_image{width=Width,height=Height,image=Pixels,
	       order=lower_left,name="auvBG"}.
    
repeat_image(<<Row:(32*3)/binary,Rest/binary>>, Acc) ->
    repeat_image(Rest,[Row,Row,Row,Row,Row,Row,Row,Row|Acc]);
repeat_image(<<>>,Acc) -> 
    Im = lists:reverse(Acc),
    list_to_binary([Im,Im,Im,Im,Im,Im,Im,Im]).

%%%
%%% Conversion routines.
%%%

auv2geom_faces(Fs, _) ->
    Fs.
geom2auv_faces(Fs, _) ->
    Fs.

auv2geom_vs(Vs, #we{name=#ch{vmap=Vmap}}) ->
    sort([auv_segment:map_vertex(V, Vmap) || V <- Vs]).

geom2auv_vs(Vs, #we{name=#ch{vmap=Vmap},vp=Vtab}) ->
    geom2auv_vs_1(gb_trees:keys(Vtab), gb_sets:from_list(Vs), Vmap, []).

geom2auv_vs_1([V|Vs], VsSet, Vmap, Acc) ->
    case gb_sets:is_member(auv_segment:map_vertex(V, Vmap), VsSet) of
	true -> geom2auv_vs_1(Vs, VsSet, Vmap, [V|Acc]);
	false -> geom2auv_vs_1(Vs, VsSet, Vmap, Acc)
    end;
geom2auv_vs_1([], _, _, Acc) -> sort(Acc).

auv2geom_edges(Es, #we{name=#ch{emap=Emap}}) ->
    sort([auv_segment:map_edge(E, Emap) || E <- Es]).

geom2auv_edges(Es, #we{name=#ch{emap=Emap0}}) ->
    A2We = sofs:relation(gb_trees:to_list(Emap0)),
    W2Ae = sofs:relation_to_family(sofs:converse(A2We)),
    Tab = gb_trees:from_orddict(sofs:to_external(W2Ae)),
    foldl(fun(Edge, Acc) ->
		  case gb_trees:lookup(Edge, Tab) of
		      none -> [Edge|Acc];
		      {value,Hits} -> Hits ++ Acc
		  end
	  end, [], Es).
    
