%%% File    : wpu_autouv.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : A semi-simple semi-automatic UV-mapping plugin
%%%
%%% Created : 24 Jan 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: wpc_autouv.erl,v 1.61 2002/12/08 17:41:03 bjorng Exp $

-module(wpc_autouv).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
 
-export([init/0,menu/2,command/2]).

-import(lists, [sort/1, map/2, foldl/3, reverse/1, 
		append/1,delete/2, usort/1, max/1, min/1,
		member/2,foreach/2,keysearch/3]).

init() ->
    true.

add_as(AsAA, TreeAA) ->
    foldl(fun({[K|_],Area}, TreeBB) ->
		  gb_trees:insert(K, Area, TreeBB) 
	  end, TreeAA, AsAA).

add_areas(New, #areas{as=As}=Areas) ->
    NewAs = add_as(New, As),
    Areas#areas{as=NewAs}.

menu({body}, Menu) ->
    Menu ++ [separator,
	     {"UV Mapping (experimental)", ?MODULE,
	      "Generate or edit UV mapping or texture"}];
menu(_, Menu) -> Menu.

command({body,?MODULE}, St) ->
    start_uvmap(St);
command({body,{?MODULE,do_edit,{We,MatName,Faces}}}, St) ->
    do_edit(MatName, Faces, We, St);
command({body,{?MODULE,show_map,Info}}, St) ->
    {MappedCharts,We,Vmap,OrigWe} = Info,
    init_show_maps(MappedCharts, We, Vmap, OrigWe, St);
command({body,{?MODULE,uvmap_done,QuitOp,Uvs}}, _) ->
    #uvstate{st=St0,areas=Current,sel=Sel} = Uvs,
    Areas1 = add_areas(Sel, Current),
    St2 = case QuitOp of
	      quit_uv_tex ->
		  Tx = ?SLOW(get_texture(Uvs)),
		  {St1,Areas1} = add_material(edit, Tx, St0, Areas1),
		  St1;
	      quit_uv ->
		  St0
	  end,
    We = ?SLOW(insert_uvcoords(Areas1)),
    Shapes = gb_trees:update(We#we.id, We, St2#st.shapes),
    St = St2#st{shapes=Shapes},
    reset_view(),
    St;
command(_, _) -> next.

start_uvmap(#st{sel=[{Id,_}],shapes=Shs}=St) ->
    case gb_trees:get(Id, Shs) of
	#we{mode=uv}=We -> start_edit(Id, We, St);
	_ -> start_uvmap_1(St)
    end;
start_uvmap(_) ->
    wpa:error("Select only one object.").

%%%
%%% Segmentation interface.
%%%

-record(seg, {st,				%Current St.
	      selmodes,				%Legal selection modes.
	      we,				%Original We.
	      msg				%Message.
	     }).

start_uvmap_1(#st{sel=[{Id,_}],shapes=Shs}=St0) ->
    Modes = [vertex,edge,face],
    wings_io:icon_restriction(Modes),
    We0 = gb_trees:get(Id, Shs),
    We = We0#we{mode=material},
    check_for_defects(We),
    St1 = seg_create_materials(St0),
    St = St1#st{sel=[],selmode=face,shapes=gb_trees:from_orddict([{Id,We}])},
    Ss = seg_init_message(#seg{selmodes=Modes,st=St,we=We0}),
    wings_wm:callback(fun() ->
			      wings_util:menu_restriction(geom,[view,select])
		      end),
    {seq,push,get_seg_event(Ss)}.

seg_init_message(Ss) ->
    Msg = ["[L] Select  [R] Show menu  "|wings_camera:help()],
    Ss#seg{msg=Msg}.

get_seg_event(Ss) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> seg_event(Ev, Ss) end}.

seg_event(redraw, #seg{st=St,msg=Msg}) ->
    wings_wm:message(Msg, "Segmenting"),
    wings:redraw(St),
    keep;
seg_event(Ev, #seg{st=St}=Ss) ->
    case wings_camera:event(Ev, St) of
	next -> seg_event_2(Ev, Ss);
	Other -> Other
    end.

seg_event_2(Ev, #seg{st=St}=Ss) ->
    case wings_pick:event(Ev, St, fun() -> wings:redraw(St) end) of
	next -> seg_event_3(Ev, Ss);
	Other -> Other
    end.

seg_event_3(Ev, #seg{st=#st{selmode=Mode}}=Ss) ->
    case wings_menu:is_popup_event(Ev) of
	no -> seg_event_4(Ev, Ss);
	{yes,X,Y,_} -> 
	    Mappers = mappers(), 
	    Menu = [{"Continue",{continue, Mappers}},
		    separator,
		    {"Segment by",{segment,
				   [{"Projection",autouvmap},
				    {"Feature Detection",feature}]}}|
		    seg_mode_menu(Mode, Ss,
				  seg_debug([separator,
					     {"Cancel",cancel}]))],
	    wings_menu:popup_menu(X, Y, auv_segmentation, Menu)
    end.

-ifndef(DEBUG).
seg_debug(Tail) -> Tail.
mappers() ->
    [{"Unfolding",lsqcm},				  
     {"Projection",project}].
-else.
seg_debug(Tail) ->
    [separator,
     {"Debugging",
      {debug,
       [{"Select features",select_features},
	{"Select seeds",select_seeds},
        {"Select Pinned vertices", select_pinned}]}}|Tail].
mappers() ->
    [{"Unfolding",lsqcm}, 
     {"Two pass Unfolding",lsqcm2},
     {"Projection",project}].
-endif.

seg_mode_menu(vertex, _, Tail) -> Tail;
seg_mode_menu(edge, _, Tail) ->
    [separator,
     {"Mark Edges for Cut",cut_edges},
     {"Unmark Edges",no_cut_edges},
     separator,
     {"Select Marked Edges",select_hard_edges}|Tail];
seg_mode_menu(face, _, Tail) ->
    Menu0 = map(fun({Name,Color}) ->
			{atom_to_list(Name),Name,[],[{color,Color}]};
		   (Other) -> Other
		end, seg_materials()),
    Menu = Menu0 ++
	[separator,
	 {"Select",{select,Menu0}}|Tail],
    [separator|Menu].

seg_event_4(Ev, Ss) ->
    case translate_key(Ev) of
	next -> seg_event_5(Ev, Ss);
	Other -> Other
    end.

seg_event_5(Ev, #seg{st=St0}=Ss) ->
    case wings_hotkey:event(Ev, St0) of
	next -> seg_event_6(Ev, Ss);
	Action ->
	    wings_io:putback_event({action,Action}),
	    keep
    end.

seg_event_6({new_state,St}, Ss) ->
    get_seg_event(Ss#seg{st=St});
seg_event_6({action,{view,auto_rotate}}, _) -> keep;
seg_event_6({action,{view,smoothed_preview}}, _) -> keep;
seg_event_6({action,{view,Cmd}}, #seg{st=St0}=Ss) ->
    St = wings_view:command(Cmd, St0),
    get_seg_event(Ss#seg{st=St});
seg_event_6({action,{select,Cmd}}, #seg{st=St0}=Ss) ->
    case wings_sel_cmd:command(Cmd, St0) of
	St0 -> keep;
	{save_state,St} -> filter_sel_command(Ss, St);
	St -> filter_sel_command(Ss, St)
    end;
seg_event_6({action,{auv_segmentation,Cmd}}, Ss) ->
    seg_command(Cmd, Ss);
seg_event_6({callback, Fun}, _) when function(Fun) ->
    Fun();
seg_event_6({message,Message}, _) ->
    wings_util:message(Message);
seg_event_6(#mousemotion{}, _) -> keep;
seg_event_6(#mousebutton{}, _) -> keep;
seg_event_6(#keyboard{}, _) -> keep;
seg_event_6(_Ev, _) ->
    ?DBG("~w\n", [_Ev]),
    keep.

translate_key(#keyboard{keysym=KeySym}) ->
    translate_key_1(KeySym);
translate_key(_Event) -> next.

translate_key_1(#keysym{sym=27}) ->		%Escape
    seg_cancel();
translate_key_1(_) -> next.

filter_sel_command(#seg{selmodes=Modes}=Ss, #st{selmode=Mode}=St) ->
    case member(Mode, Modes) of
	false -> keep;
	true -> seg_event({new_state,St}, Ss)
    end.

seg_command({continue,Method}, Ss) ->
    seg_map_charts(Method, Ss);
seg_command(cancel, _) ->
    seg_cancel();
seg_command(cut_edges, #seg{st=St0}=Ss) ->
    St = wings_edge:hardness(hard, St0),
    get_seg_event(Ss#seg{st=St});
seg_command(no_cut_edges, #seg{st=St0}=Ss) ->
    St = wings_edge:hardness(soft, St0),
    get_seg_event(Ss#seg{st=St});
seg_command(select_hard_edges, _) ->
    wings_io:putback_event({action,{select,{by,hard_edges}}}),
    keep;
seg_command({select,Mat}, _) ->
    wings_io:putback_event({action,{select,{by,{material,Mat}}}}),
    keep;
seg_command({segment,Type}, #seg{st=St0}=Ss) ->
    St = segment(Type, St0),
    get_seg_event(Ss#seg{st=St});
seg_command({debug,select_features}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    Tot = gb_trees:size(We#we.es),
    {Es,_,_} = auv_segment:find_features(We, 60, Tot div 50),
    Sel = [{Id,gb_sets:from_list(Es)}],
    get_seg_event(Ss#seg{st=St#st{selmode=edge,sel=Sel}});
seg_command({debug,select_seeds}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    Tot = gb_trees:size(We#we.es),
    {Features,_,_} = auv_segment:find_features(We, 60, Tot div 50),
    {Seeds0,_} = auv_segment:build_seeds(Features, We),
    Seeds = [S || {_,S} <- Seeds0],
    Sel = [{Id,gb_sets:from_list(Seeds)}],
    get_seg_event(Ss#seg{st=St#st{selmode=face,sel=Sel}});
seg_command({debug,select_pinned}, #seg{we=#we{id=Id}=We,st=St}=Ss) ->
    [{Id,SetOfFaces}] = St#st.sel,
    case {St#st.selmode == face, gb_sets:to_list(SetOfFaces)} of
	{true,Fs} when Fs /= [] ->
	    {{V1,_UV1},{V2,_UV2}} = auv_mapping:find_pinned(Fs, We),
	    ?DBG("Pinned ~p ~n", [{{V1,_UV1},{V2,_UV2}}]),
	    Sel = [{Id,gb_sets:from_list([V1,V2])}],
	    get_seg_event(Ss#seg{st=St#st{selmode=vertex,sel=Sel}});
	_ -> 
	    ?DBG("Not in face mode~n", []),
	    keep
    end;

seg_command(Cmd, #seg{st=#st{mat=Mat}=St0}=Ss) ->
    case gb_trees:is_defined(Cmd, Mat) of
	false ->
	    io:format("Cmd: ~w\n", [Cmd]),
	    keep;
	true ->
	    St = wings_material:command({face,{material,Cmd}}, St0),
	    get_seg_event(Ss#seg{st=St})
    end.

seg_cancel() ->
    wings_io:clear_icon_restriction(),
    wings_wm:dirty(),
    pop.

seg_materials() ->
    [{'AuvChart1',{0.5,0.5,0.0}},
     {'AuvChart2',{0.5,0.0,0.5}},
     {'AuvChart3',{0.0,0.5,0.5}},
     separator,
     {'AuvChart4',{0.8,0.5,0.0}},
     {'AuvChart5',{0.8,0.0,0.5}},
     {'AuvChart6',{0.5,0.8,0.0}},
     separator,
     {'AuvChart7',{0.0,0.8,0.5}},
     {'AuvChart8',{0.5,0.0,0.8}},
     {'AuvChart9',{0.0,0.5,0.8}}].

seg_create_materials(St0) ->
    M0 = seg_materials(),
    M = [{Name,make_mat(Diff)} || {Name,Diff} <- M0],
    {St,[]} = wings_material:add_materials(M, St0),
    St.

seg_map_charts(Method, #seg{st=#st{shapes=Shs},we=OrigWe}=Ss) ->
    [{_,#we{he=Cuts0}=We0}] = gb_trees:to_list(Shs),
    Charts0 = auv_segment:segment_by_material(We0),
    {Charts,Cuts} = auv_segment:normalize_charts(Charts0, Cuts0, We0),
    {We,Vmap} = auv_segment:cut_model(Charts, Cuts, OrigWe),
    N = length(Charts),
    seg_map_charts_1(Charts, Method, We, {OrigWe,Vmap}, 1, N, [], Ss).

seg_map_charts_1(Cs, Type, We, Extra, I, N, Acc, Ss) when I =< N ->
    MapChart = fun() -> seg_map_chart(Cs, Type, We, Extra, I, N, Acc, Ss) end,
    wings_io:putback_event({callback,MapChart}),
    Msg = io_lib:format("Mapping chart ~w of ~w", [I,N]),
    get_seg_event(Ss#seg{msg=Msg});
seg_map_charts_1(_, _, We, {OrigWe,Vmap}, _, _, MappedCharts, _) ->
    wings_io:clear_icon_restriction(),
    Info = {MappedCharts,We,Vmap,OrigWe},
    wings_io:putback_event({action,{body,{?MODULE,show_map,Info}}}),
    pop.

seg_map_chart([C|Cs], Type, We, Extra, I, N, Acc0, Ss) ->
    case auv_mapping:map_chart(Type, C, We) of
	{error,Message} ->
	    seg_error(Message, Ss);
	Vs ->
	    Acc = [#ch{fs=C,vpos=Vs}|Acc0],
	    seg_map_charts_1(Cs, Type, We, Extra, I+1, N, Acc, Ss)
    end.

seg_error(Message, Ss) ->
    wings_io:putback_event({message,Message}),
    get_seg_event(seg_init_message(Ss)).

make_mat(Diff) ->
    [{opengl,[{diffuse,Diff},
	      {ambient,Diff},
	      {specular,{0.0,0.0,0.0}}]}].

check_for_defects(We) ->
    case wings_vertex:isolated(We) of
	[] ->
	    ok;
	[_|_] ->
	    wpa:error("The model has isolated vertices. (Use the Cleanup command.)")
    end.

%%%
%%% Edit interface.
%%%

start_edit(_Id, We, St) ->
    DefVar = {answer,edit},
    Qs = [{vframe,[{alt,DefVar,"Edit existing UV mapping",edit},
		   {alt,DefVar,"Discard existing UV mapping and start over",discard}],
	   [{title,"Model is already UV-mapped"}]}],
    wings_ask:dialog(Qs,
		     fun([Reply]) ->
			     case Reply of
				 edit ->
				     start_edit_1(We, St);
				 discard ->
				     Act = {action,{body,?MODULE}},
				     wings_io:putback_event(Act),
				     discard_uvmap(We, St)
			     end
		     end).

start_edit_1(#we{name=ObjName,fs=Ftab}=We, St) ->
    MatNames0 = foldl(fun({Face,#face{mat=Mat}}, A) ->
			      [{Mat,Face}|A]
		      end, [], gb_trees:to_list(Ftab)),
    MatNames1 = sofs:to_external(sofs:relation_to_family(sofs:relation(MatNames0))),
    MatNames = [Mat || {Name,_}=Mat <- MatNames1, has_texture(Name, St)],
    case MatNames of
	[] ->
	    Faces = gb_trees:keys(Ftab),
	    MatName = list_to_atom(ObjName ++ "_auv"),
	    gen_edit_event(MatName, Faces, We);
	[{MatName,Faces}] ->
	    gen_edit_event(MatName, Faces, We);
	[{First,_}|_]=Ms ->
	    Act = {callback,fun() -> start_edit_cb(First, Ms, We) end},
	    wings_io:putback_event(Act),
	    ignore
    end.

start_edit_cb(First, Ms, We) ->
    DefVar = {answer,First},
    Qs = [{vframe,
	   [{alt,DefVar,"Material "++atom_to_list(M),M} || {M,_} <- Ms],
	   [{title,"Choose Material"}]}],
    wings_ask:dialog(Qs,
		     fun([Mat]) ->
			     {value,{_,Faces}} = keysearch(Mat, 1, Ms),
			     gen_edit_event(Mat, Faces, We)
		     end).

gen_edit_event(MatName, Faces, We) ->
    Act = {action,{body,{?MODULE,do_edit,{We,MatName,Faces}}}},
    wings_io:putback_event(Act),
    ignore.

discard_uvmap(#we{fs=Ftab}=We0, St) ->
    Faces = gb_trees:keys(Ftab),
    FvUvMap = auv_segment:fv_to_uv_map(Faces, We0),
    {Charts,Cuts} = auv_segment:uv_to_charts(Faces, FvUvMap, We0),
    We = wings_we:uv_to_color(We0, St),
    mark_segments(Charts, Cuts, We, St).

do_edit(MatName, Faces, We, St) ->
    Areas = #areas{matname=MatName} = init_edit(MatName, Faces, We),
    {{X,Y,W,H},Geom} = init_drawarea(),
    TexSz = get_texture_size(MatName, St),
    Uvs = #uvstate{st=wings_select_faces([], Areas#areas.we, St),
		   origst=St,
		   areas=Areas,
		   geom=Geom, 
		   option=#setng{color=false,texbg=true,texsz=TexSz}},
    Op = {seq,push,get_event(Uvs)},
    wings_wm:new(autouv, {X,Y,2}, {W,H}, Op),
    wings_wm:callback(fun() -> wings_util:menu_restriction(autouv, []) end),
    keep.

segment(Mode, #st{shapes=Shs}=St) ->
    [{_,We}] = gb_trees:to_list(Shs),
    {Charts,Cuts} = auv_segment:create(Mode, We),
    mark_segments(Charts, Cuts, We, St).

mark_segments(Charts, Cuts, We0, St) ->
    We = We0#we{he=Cuts},			%Hard edges mark the cuts.

    %% Use materials to mark different charts.
    Template = list_to_tuple([make_mat(Diff) || {_,Diff} <- seg_materials()]),
    assign_materials(Charts, We, Template, 0, St).

assign_materials([Faces|T], #we{fs=Ftab0}=We0, Template, I0, #st{mat=Mat0}=St0) ->
    I = I0 + 1,
    MatName = list_to_atom("AuvChart" ++ integer_to_list(I)),
    Ftab = foldl(fun(F, A) ->
			 Rec = gb_trees:get(F, A),
			 gb_trees:update(F, Rec#face{mat=MatName}, A)
		 end, Ftab0, Faces),
    We = We0#we{fs=Ftab},
    case gb_trees:is_defined(MatName, Mat0) of
	true ->
	    assign_materials(T, We, Template, I, St0);
	false ->
	    MatDef = element(I0 rem size(Template) + 1, Template),
	    {St,[]} = wings_material:add_materials([{MatName,MatDef}], St0),
	    assign_materials(T, We, Template, I, St)
    end;
assign_materials([], #we{id=Id}=We, _, _, #st{shapes=Shs0}=St) ->
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{shapes=Shs}.
          

%%%%%%

replace_uvs(Map, We) when is_list(Map) ->
    foldl(fun({_,Chart}, W) -> replace_uv(Chart, W) end, We, Map);
replace_uvs(Map, We) ->
    replace_uvs(gb_trees:to_list(Map), We).

replace_uv(#ch{vpos=Vpos}, #we{vs=Vtab0}=We) ->
    Vtab = foldl(fun({V,Pos}, Vt) ->
			 Vtx = gb_trees:get(V, Vt),
			 gb_trees:update(V, Vtx#vtx{pos=Pos}, Vt)
		 end, Vtab0, Vpos),
    We#we{vs=Vtab}.

find_boundary_edges([{Id,#ch{fs=Fs}=C}|Cs], We, Acc) ->
    Be = auv_util:outer_edges(Fs, We),
    find_boundary_edges(Cs, We, [{Id,C#ch{be=Be}}|Acc]);
find_boundary_edges([], _, Acc) -> sort(Acc).

init_show_maps(Map0, #we{name=Name}=We0, Vmap, OrigWe, St0) ->
    Map1 = auv_placement:place_areas(Map0, We0),
    We = replace_uvs(Map1, We0),
    Map2 = find_boundary_edges(Map1, We, []),
    Map = gb_trees:from_orddict(Map2),
    Edges = gb_trees:keys(OrigWe#we.es),
    As0 = #areas{we=We,orig_we=OrigWe,edges=Edges,
		 as=Map,vmap=Vmap,
		 matname=list_to_atom(Name ++ "_auv")},
    {St1,Areas} = add_material(create_mat, none, St0, As0),
    {{X,Y,W,H},Geom} = init_drawarea(),
    Uvs = #uvstate{st=wings_select_faces([], Areas#areas.we, St1),
		   origst=St0,
		   areas=Areas, 
		   geom=Geom},
    Op = {seq,push,get_event(Uvs)},
    wings_wm:new(autouv, {X,Y,2}, {W,H}, Op),
    wings_wm:callback(fun() -> wings_util:menu_restriction(autouv, []) end),
    keep.
   
insert_uvcoords(#areas{orig_we=We0,we=WorkWe,as=Cs0,matname=MatName,vmap=Vmap}) ->
    Cs = gb_trees:values(Cs0),
    UVpos = gen_uv_pos(Cs, WorkWe, []),
    We1 = insert_coords(UVpos, Vmap, We0),
    We = insert_material(Cs, MatName, We1),
    We#we{mode=uv}.

insert_material(Cs, MatName, #we{fs=Ftab0}=We) ->
    Faces = lists:append([Fs || #ch{fs=Fs} <- Cs]),
    Ftab = foldl(fun(Face, A) ->
			 case gb_trees:get(Face, A) of
			     #face{mat=MatName} -> A;
			     Rec -> gb_trees:update(Face, Rec#face{mat=MatName}, A)
			 end
		 end, Ftab0, Faces),
    We#we{fs=Ftab}.

gen_uv_pos([#ch{fs=Fs,center={CX,CY},scale=Sc,vpos=Vs}|T], We, Acc) ->
    Vpos0 = auv_util:moveAndScale(Vs, CX, CY, Sc, []),
    VFace0 = wings_face:fold_faces(
	       fun(Face, V, _, _, A) ->
		       [{V,Face}|A]
	       end, [], Fs, We),
    VFace = sofs:relation(VFace0, [{vertex,face}]),
    Vpos = sofs:relation(Vpos0, [{vertex,uvinfo}]),
    Comb0 = sofs:relative_product({VFace,Vpos}),
    Comb = sofs:to_external(Comb0),
    gen_uv_pos(T, We, Comb++Acc);
gen_uv_pos([], _, Acc) -> Acc.

insert_coords([{V0,{Face,{S,T,_}}}|Rest], Vmap, #we{es=Etab0}=We) ->
    V = auv_segment:map_vertex(V0, Vmap),
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
    insert_coords(Rest, Vmap, We#we{es=Etab});
insert_coords([], _, We) -> We.

init_edit(MatName, Faces, We0) ->
    FvUvMap = auv_segment:fv_to_uv_map(Faces, We0),
    {Charts,Cuts} = auv_segment:uv_to_charts(Faces, FvUvMap, We0),
    {We1,Vmap} = auv_segment:cut_model(Charts, Cuts, We0),
    Map1 = auv_util:number(build_map(Charts, Vmap, FvUvMap, We1, []), 1),
    We = replace_uvs(Map1, We1),
    Map2 = find_boundary_edges(Map1, We, []),
    Map = gb_trees:from_orddict(Map2),
    Edges = gb_trees:keys(We0#we.es),
    #areas{we=We,orig_we=We0,edges=Edges,as=Map,vmap=Vmap,matname=MatName}.

build_map([Fs|T], Vmap, FvUvMap, We, Acc) ->
    %% XXX Because auv_segment:cut_model/3 distorts the UV coordinates
    %% (bug in wings_vertex_cmd), we must fetch the UV coordinates
    %% from the original object.
    UVs0 = wings_face:fold_faces(
	     fun(F, V, _, _, A) ->
		     OrigV = auv_segment:map_vertex(V, Vmap),
		     UV = gb_trees:get({F,OrigV}, FvUvMap),
		     [{V,UV}|A]
	     end, [], Fs, We),
    UVs1 = lists:usort(UVs0),
    %% Assertion.
    true = sofs:is_a_function(sofs:relation(UVs1, [{atom,atom}])),
    {{_,BX0},{_,BX1},{_,BY0},{_,BY1}} = auv_util:maxmin(UVs0),
    CX = BX0 + (BX1-BX0) / 2,
    CY = BY0 + (BY1-BY0) / 2,
    UVs = [{V,{X-CX,Y-CY,0.0}} || {V,{X,Y}} <- UVs1],
    Chart = #ch{fs=Fs,vpos=UVs,center={CX,CY},size={BX1-BX0,BY1-BY0}},
    build_map(T, Vmap, FvUvMap, We, [Chart|Acc]);
build_map([], _, _, _, Acc) -> Acc.

%%%%% Material handling

has_texture(MatName, #st{mat=Materials}) ->
    has_texture(MatName, Materials);
has_texture(MatName, Materials) ->
    Mat = gb_trees:get(MatName, Materials),
    Maps = proplists:get_value(maps,Mat, []),
    none /= proplists:get_value(diffuse, Maps, none).

get_texture_size(MatName, #st{mat=Materials}) ->
    Mat = gb_trees:get(MatName, Materials),
    Maps = proplists:get_value(maps, Mat, []),
    case proplists:get_value(diffuse, Maps, none) of
	none -> {512, 512};
	{W,H,_} -> {W,H}
    end.	     

get_material(Face, Materials, #we{fs=Ftab}) ->
    #face{mat=MatName} = gb_trees:get(Face, Ftab),
    Mat = gb_trees:get(MatName, Materials),
    proplists:get_value(diffuse, proplists:get_value(opengl, Mat)).

add_material(create_mat, none, St0, #areas{matname=MatName}=Areas) ->
    Mat = {MatName, [{opengl, []},{maps, []}]},
    case wings_material:add_materials([Mat], St0) of
	{St1, []} ->
	    {St1, Areas};
	{St1, [{MatName,NewName}]} ->
	    {St1, Areas#areas{matname = NewName}}
    end;
add_material(edit, Tx, St0, #areas{matname=MatName}=As) ->
    St = wings_material:replace_map(MatName, diffuse, Tx, St0),
    {St,As}.

%%% Opengl drawing routines

init_drawarea() ->
    {X,Y,W0,H} = wings_wm:viewport(geom),
    W = (W0 - 4) div 2,
    Border = 10,
    {_,TopH} = wings_wm:top_size(),
    {{X+W,TopH-Y-H,W,H},
     if 
	 W > H ->
	     WF = Border / W,
	     {-WF,W/H+WF,-WF,1+2*WF};
	 true ->
	     WF = Border / H,
	     {-WF,1+WF,-WF,H/W+WF}
     end}.

draw_texture(#uvstate{dl=undefined,option=Options}=Uvs) ->
    Materials = (Uvs#uvstate.origst)#st.mat,
    #areas{we=We,as=As0} = Uvs#uvstate.areas,
    DrawArea = fun({_,A}) ->
		       draw_area(A, We, Options, Materials)
	       end,
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    ?SLOW(foreach(DrawArea, gb_trees:to_list(As0))),
    gl:endList(),
    draw_texture(Uvs#uvstate{dl=Dl});
draw_texture(Uvs = #uvstate{dl=DL, sel=Sel, areas=#areas{we=We}}) ->
    gl:callList(DL),
    case Sel of 
	[] -> ignore;
	_ -> %% Draw selections slightly blended
	    {R,G,B} = wings_pref:get_value(selected_color),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:enable(?GL_BLEND),
	    gl:disable(?GL_DEPTH_TEST),
	    DrawArea = fun({_,A}) -> 
			       draw_area(A, We, #setng{color = {R,G,B,0.7}, 
						       edges = no_edges}, []) 
		       end,
	    lists:foreach(DrawArea, Sel),
	    gl:disable(?GL_BLEND)
    end,
    Uvs.

setup_view({Left,Right,Bottom,Top}, Uvs) ->
    #uvstate{st=#st{mat=Mats}, option=#setng{texbg=TexBg},
	     areas=#areas{matname = MatN}}=Uvs,
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_DEPTH_TEST),    

    {_,_,W,H} = wings_wm:viewport(),
    wings_io:ortho_setup(),
    gl:color3f(1, 1, 1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:recti(0, H, W, 0),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(Left, Right, Bottom, Top),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:color3b(0, 0, 0),
    gl:'begin'(?GL_LINE_LOOP),
    D = Left/10,
    gl:vertex2f(D, D),
    gl:vertex2f(1-D, D),
    gl:vertex2f(1-D, 1-D),
    gl:vertex2f(D, 1-D),
    gl:'end'(),    
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1.0, 1.0, 1.0),   %%Clear
    case TexBg of
	true ->
	    wings_material:apply_material(MatN, Mats);
	false ->
	    ok
    end,
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0,0),    gl:vertex3f(0,0,-0.9),
    gl:texCoord2f(1,0),    gl:vertex3f(1,0,-0.9),
    gl:texCoord2f(1,1),    gl:vertex3f(1,1,-0.9),
    gl:texCoord2f(0,1),    gl:vertex3f(0,1,-0.9),
    gl:'end'(), 
    gl:disable(?GL_TEXTURE_2D),
    gl:enable(?GL_DEPTH_TEST),
    gl:shadeModel(?GL_SMOOTH).

% wings_view(#uvstate{mode=Mode,geom={WingsPort,{X2,Y2,_,_,_,_,_}},st=St}=Uvs) ->
%     ModeL = atom_to_list(Mode),
%     Text = [ModeL] ++ [" Mode: [R] in texture window to access menu, "
% 		       "[L] to select face groups"],
%     wings_wm:message(Text),
%     {_,_,_,Oh} = OldViewport = wings_wm:viewport(),
%     set_viewport(WingsPort),
%     wings_draw:render(St),
%     set_viewport(OldViewport),
%     wings_io:update(Uvs#uvstate.st),
%     wings_io:ortho_setup(),
%     gl:color3fv(?PANE_COLOR),
%     {_,_,W,_} = WingsPort,
%     gl:recti(W, 0, X2, Oh - Y2),
%     ok.

reset_view() ->    
    gl:popAttrib().

%%% Texture Creation

calc_texsize(Vp, Tex) ->
    calc_texsize(Vp, Tex, Tex).

calc_texsize(Vp, Tex, Orig) when Tex < Vp ->
    {Tex,Orig div Tex};
calc_texsize(Vp, Tex, Orig) ->
    calc_texsize(Vp, Tex div 2, Orig).

get_texture(#uvstate{option=#setng{texsz={TexW,TexH}},sel=Sel,areas=As}=Uvs0) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:clearColor(1, 1, 1, 1),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),
    Current = wings_wm:viewport(),
    {_,_,W0,H0} = wings_wm:viewport(top),
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
    set_viewport({0,0,W,H}),
    Mem = sdl_util:malloc(W*H*3, ?GL_BYTE),
    Uvs = Uvs0#uvstate{sel=[],areas=add_areas(Sel, As),dl=undefined},
    ImageBins = get_texture(0, Wd, 0, Hd, {W,H,Mem}, Uvs, []),
    ImageBin = merge_texture(ImageBins, Wd, Hd, W*3, H, []),
    sdl_util:free(Mem),
    set_viewport(Current),
    gl:popAttrib(),

    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    case (TexW*TexH *3) == size(ImageBin) of
	true ->
	    {TexW,TexH,ImageBin};
	false ->
	    BinSzs = [size(Bin) || Bin <- ImageBins],
	    exit({texture_error,{TexW, TexH, size(ImageBin), W,Wd,H,Hd, BinSzs}})
    end.
		 
get_texture(Wc, Wd, Hc, Hd, {W,H,Mem}=Info, Uvs0, ImageAcc)
  when Wc < Wd, Hc < Hd ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    texture_view(Wc, Wd, Hc, Hd, Uvs0),
    Uvs = draw_texture(Uvs0),
    gl:flush(),
    gl:readBuffer(?GL_BACK),
    gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin = sdl_util:readBin(Mem, W*H*3),
    get_texture(Wc+1, Wd, Hc, Hd, Info, Uvs, [ImageBin|ImageAcc]);
get_texture(_Wc,Wd,Hc,Hd, Info, Uvs, ImageAcc) when Hc < Hd ->
    get_texture(0, Wd, Hc+1, Hd, Info, Uvs, ImageAcc);
get_texture(_, _, _, _, _, _, ImageAcc) -> reverse(ImageAcc).

texture_view(WC, WD, HC, HD, Uvs) ->
    #uvstate{st=#st{mat=Mats}, option=#setng{texbg=TexBg},
	     areas=#areas{matname = MatN}}=Uvs,
    gl:disable(?GL_DEPTH_TEST),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(WC/WD, (1+WC)/WD, HC/HD, (1+HC)/HD),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1.0, 1.0, 1.0),
    case TexBg of
	true -> wings_material:apply_material(MatN, Mats);
	false -> ok
    end,
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0,0),    gl:vertex3f(0, 0, -0.9),
    gl:texCoord2f(1,0),    gl:vertex3f(1, 0, -0.9),
    gl:texCoord2f(1,1),    gl:vertex3f(1, 1, -0.9),
    gl:texCoord2f(0,1),    gl:vertex3f(0, 1, -0.9),
    gl:'end'(),
    gl:disable(?GL_TEXTURE_2D),
    gl:enable(?GL_DEPTH_TEST).

merge_texture_cols(List, Wd, Wd, _W, _RowC, Acc) ->
    {list_to_binary(reverse(Acc)), List};
merge_texture_cols([H|R], Wc, Wd, W, RowC, Acc) ->
    SkipBytes = RowC*W,
    <<_:SkipBytes/binary, Row:W/binary,_/binary>> = H,
    merge_texture_cols(R, Wc + 1, Wd, W, RowC, [Row|Acc]).

merge_texture_rows(_ImageBins, H, H, _W, _Wd,Acc, Last) ->
    {list_to_binary(reverse(Acc)), Last};
merge_texture_rows(ImageBins, RowC, H, W, Wd, Acc, _) ->
    {Row, Rest} = merge_texture_cols(ImageBins, 0, Wd, W, RowC, []),
    merge_texture_rows(ImageBins, RowC + 1, H,W,Wd, [Row|Acc], Rest).

merge_texture([Bin],1,1,_,_,[]) ->   Bin;  %% No merge needed.
merge_texture(Bins, 1,_,_,_,[]) ->   list_to_binary(Bins);  %% No merge needed.
merge_texture([],_,_,_,_,Acc) -> 
    list_to_binary(reverse(Acc));
merge_texture(ImageBins,Wd,Hd,W,H,Acc) ->    
    {Col, Bins} = merge_texture_rows(ImageBins, 0, H, W, Wd, [], ImageBins),
    merge_texture(Bins,Wd,Hd,W,H,[Col|Acc]).

tga_prop() ->
    [{ext,".tga"},{ext_desc,"2D-Targa File"}].

%%%%%%% Events handling and window redrawing 
   
get_event(Uvs) ->
    wings_wm:dirty(),
    get_event_nodraw(Uvs).

get_event_nodraw(Uvs) ->
    {replace,fun(Ev) -> handle_event(Ev, Uvs) end}.

draw_windows(#uvstate{mode=Mode}=Uvs) ->
    Text = [atom_to_list(Mode)," mode: ",
	    "[L] Select [R] Show menu"],
    wings_wm:message(Text),
    setup_view(Uvs#uvstate.geom, Uvs),
    Uvs1 = draw_texture(Uvs),
    reset_view(),
    Uvs1.

command_menu(faceg, X,Y, _Uvs) ->
    Rotate = [{"Z    Free",  free, "Drag mouse to rotate free"},
	      {"Z   90 deg", 90, " "},
	      {"Z  -90 deg", -90, " "},
	      {"Z  180 deg", 180, " "},
	      separator,
	      {"X  180 deg", rot_x_180, "Flip Y coordinates"},
	      {"Y  180 deg", rot_y_180, "Flip X coordinates"}],

    Menu = [{"Face Group operations", ignore},
	    separator,
	    {"Move", move, "Move selected faces"},
	    {"Scale", scale, "Uniform Scale of selected faces"},
	    {"Rotate", {rotate, Rotate}, "Rotate selected faces"},
	    {"Rescale all", rescale_all, "Pack the space in lower-left before rescaling"}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);

command_menu(face, X,Y, _Uvs) ->
    Menu = [{"Face operations", ignore}, 
	    {"Email your ideas", ignore}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);

command_menu(edge, X,Y, _Uvs) ->
    Menu = [{"Edge operations", ignore},
	    {"Email your ideas", ignore}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu);

command_menu(vertex, X,Y, _Uvs) ->
    Menu = [{"Vertex operations", ignore},
	    {"Email your ideas", ignore}
	   ] ++ option_menu(),
    wings_menu:popup_menu(X,Y, auv, Menu).

option_menu() ->
    [separator,
     {"Draw Options", edge_options, "Edit draw options"},
     separator,
     {"Export", export, "Export texture"},
     {"Import", import, "Import texture"},
     {"Checkerboard", checkerboard, "Generate checkerboard texture"},
     separator,
     {"Apply texture", apply_texture, "Attach the current texture to the model"},
     separator,
     {"Quit", quit, "Quit AutoUv-mapper"}].

edge_option_menu(#uvstate{option = Option}) ->
    DefVar = {edge_mode, Option#setng.edges},
    DefTSz = {txsize, element(1, Option#setng.texsz)},
    MaxTxs = min([4096,gl:getIntegerv(?GL_MAX_TEXTURE_SIZE)]),
    TxSzs = genSizeOption(128, MaxTxs, DefTSz, []),    
    
    Qs = [{vframe,[{alt,DefVar,"Draw All Edges",    all_edges},
		   {alt,DefVar,"Draw Border Edges", border_edges},
		   {alt,DefVar,"Don't Draw Edges",  no_edges}],
	   [{title,"Edge Options"}]},
	  {vframe,[{"Use Face/Vertex Color on Border Edges", Option#setng.edge_color},
		   {label_column, [{"Border Edge width",  {text, Option#setng.edge_width}}]}],
	   [{title, "Overdraw options"}]},
	  {vframe,[{"Show Colors (or texture)",Option#setng.color},
		   {"Texture Background (if available)", Option#setng.texbg}],
	   [{title, "Display Color and texture?"}]},
	  {vframe, TxSzs, [{title,"Texture Size"}]}],
    wings_ask:dialog(Qs, %%fun() -> draw_windows(Uvs) end,
		     fun([Mode,BEC,BEW,Color,TexBg, TSz]) -> 
			     {auv, set_options, {Mode,BEC,BEW,Color,TexBg,TSz}}  end).

quit_menu(Uvs) ->
    #uvstate{st=St,areas=#areas{matname=MatN}} = Uvs,
    DefVar = {quit_mode, quit_uv_tex},
    A1 = {alt,DefVar, "Quit and save UV-coords and texture",quit_uv_tex},
    A2 = {alt,DefVar, "Quit and save only UV-coords (use old or imported texture)", quit_uv},
    A3 = {alt,DefVar, "Quit and cancel all changes", cancel},
    Alts = case has_texture(MatN, St) of
	       true ->
		   [A1,A2,A3];
	       false ->
		   [A1,A3]
	   end,
    Qs = [{vframe, Alts,[{title,"Quit"}]}],
    wings_ask:dialog(Qs, fun([Quit]) -> {auv,quit,Quit} end).

genSizeOption(V, MaxTxs, DefTSz, Acc) when V =< MaxTxs->
    Str = lists:flatten(io_lib:format("~px~p (~pkB)",[V,V,(V*V*3) div 1024])),
    genSizeOption(V*2, MaxTxs, DefTSz, [{alt, DefTSz, Str, V}|Acc]);
genSizeOption(_V, _MaxTxs, _DefTSz, Acc) ->
    reverse(Acc).

%%% Event handling

-record(op, {name, prev, add, undo}).

handle_event(redraw, Uvs0) ->
    %%    ?DBG("redraw event\n"),
    Uvs = draw_windows(Uvs0),
    get_event_nodraw(Uvs);
handle_event(#mousemotion{}=Ev, #uvstate{op=Op}=Uvs) when Op /= undefined ->	   
    handle_mousemotion(Ev, Uvs);
handle_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_RIGHT,x=X0,y=Y0}, 
	     #uvstate{op=undefined,mode=Mode}=Uvs) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    command_menu(Mode, X, Y, Uvs);
handle_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_RIGHT}, 
	     #uvstate{op=Op}) ->	   
    get_event(Op#op.undo);
handle_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_LEFT,x=MX,y=MY}, 
	     Uvs0 = #uvstate{geom=ViewP,
			     mode=Mode,
			     op=Op,
			     sel=Sel0,
			     areas=#areas{we=We,as=Curr0}=As})
  when Op == undefined; Op#op.name == fmove ->
    {_,_,_,OH} = wings_wm:viewport(),
    SX = MX,
    SY = OH-MY,
    case select(Mode, SX, SY, add_as(Sel0,Curr0), We, ViewP) of
	none when Op == undefined ->
	    keep;
	none -> 
	    get_event(Uvs0#uvstate{op = undefined});
	Hits ->
	    {Sel1, Curr1} = 
		case (sdl_keyboard:getModState() band ?KMOD_CTRL) /= 0 of
		    true -> 
			update_selection(Hits -- Sel0, Sel0, Curr0);
		    false ->
			update_selection([hd(Hits)], Sel0, Curr0)
		end,
	    get_event(Uvs0#uvstate{sel = Sel1,
				   st = wings_select_faces(Sel1, We, Uvs0#uvstate.st),
				   areas = As#areas{as=Curr1},
				   dl = undefined, op = undefined})
    end;
handle_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_LEFT,x=MX,y=MY}, 
	     #uvstate{geom=ViewP,
		      mode = Mode,
		      op = Op,
		      sel = Sel0,
		      areas=#areas{we=We0,as=Curr0}=As}=Uvs0) ->
    {_,_,_,OH} = wings_wm:viewport(),
    case Op#op.name of
	boxsel when Op#op.add == {MX,MY} -> %% No box
	    get_event(Uvs0#uvstate{op = undefined});
	boxsel ->
	    {OX,OY} = Op#op.add,
	    BW = abs(OX-MX),
	    BH = abs(OY-MY),
	    CX = if OX > MX -> MX + BW div 2; true -> MX - BW div 2 end,
	    CY = if OY > MY -> MY + BH div 2; true -> MY - BH div 2 end,
	    %%		    ?DBG("BW ~p BH ~p Center ~p \n",[BW,BH, {CX,CY}]),
	    case select(Mode, CX,(OH-CY), BW,BH, Curr0, We0, ViewP) of
		none -> 
		    get_event(Uvs0#uvstate{op = undefined});
		Hits -> 
		    %%			    ?DBG("Hit number ~p \n",[length(Hits)]),
		    {Sel1, Curr1} = update_selection(Hits, Sel0, Curr0),
		    get_event(Uvs0#uvstate{sel = Sel1,
					   st = wings_select_faces(Sel1, We0, Uvs0#uvstate.st),
					   areas = As#areas{as=Curr1},
					   dl = undefined, op = undefined})
	    end;
	rotate ->
	    Sel = [finish_rotate(A)|| A <- Sel0],
	    We = replace_uvs(Sel, We0),
	    get_event(Uvs0#uvstate{op=undefined,sel=Sel,areas=As#areas{we=We}});
	_ ->
	    get_event(Uvs0#uvstate{op = undefined})
    end;

handle_event(#mousebutton{state=?SDL_PRESSED,button=?SDL_BUTTON_LEFT,x=MX,y=MY}, 
	     #uvstate{geom=ViewP,
		      mode=Mode,
		      op=Op,
		      sel=Sel0,
		      areas=#areas{we=We,as=Curr0}}=Uvs0) 
  when Op == undefined ->
    {_,_,_,OH} = wings_wm:viewport(),
    case select(Mode, MX, (OH-MY), add_as(Sel0, Curr0), We, ViewP) of
	none -> 
	    get_event(Uvs0#uvstate{op=#op{name=boxsel, add={MX,MY}, 
					  prev={MX+1,MY+1},undo=Uvs0}});
	Hits ->
	    case Hits -- Sel0 of  
		Hits -> 
		    keep;
		_ -> %% Hit atleast one of the selected
		    get_event(Uvs0#uvstate{op=#op{name=fmove, prev={MX,MY}, undo=Uvs0}})
	    end
    end;
handle_event(#keyboard{state=?SDL_PRESSED,keysym=Sym}, 
	     #uvstate{sel=Sel0,areas=As=#areas{we=We}}=Uvs0) ->
    case Sym of
	#keysym{sym = ?SDLK_SPACE} ->
	    get_event(Uvs0#uvstate{sel = [],
				   st = wings_select_faces([], We, Uvs0#uvstate.st),
				   areas = add_areas(Sel0,As),
				   dl = undefined});
	#keysym{sym = ?SDLK_F5} ->
	    import_file(default, Uvs0); 
	_ ->  keep
    end;
handle_event({action,{auv,export}}, Uvs0) ->
    Ps = tga_prop(),
    case wpa:export_filename(Ps, #st{}) of
	aborted -> 
	    get_event(Uvs0);	
	FileName ->
	    {TW,TH,TexBin} = ?SLOW(get_texture(Uvs0)),
	    Image = #e3d_image{image=TexBin,width=TW,height=TH},
	    case ?SLOW((catch e3d_image:save(Image, FileName))) of
		ok -> 			   
		    get_event(Uvs0#uvstate{last_file = FileName});
		{_, Error0} ->
		    Error = FileName ++ ": " ++ file:format_error(Error0),
		    wings_util:message("Export failed: " ++ Error)
	    end
    end;
handle_event({action,{auv,import}}, Uvs0) ->
    Ps = [{extensions,wpa:image_formats()}],
    case wpa:import_filename(Ps) of
	aborted -> 
	    get_event(Uvs0);
	FileName ->
	    ?SLOW(import_file(FileName, Uvs0))
    end;
handle_event({action,{auv,checkerboard}}, #uvstate{option=Opt0}=Uvs) ->
    Sz = 512,
    Bin = checkerboard(Sz),
    Opt = Opt0#setng{texbg=true,color=false,edges=no_edges},
    add_texture_image(Sz, Sz, Bin, default, Uvs#uvstate{option=Opt});
handle_event({action,{auv,apply_texture}},
	     Uvs0=#uvstate{sel = Sel0,areas=As=#areas{we=We}}) ->
    Tx = ?SLOW(get_texture(Uvs0)),
    Areas1 = add_areas(Sel0,As),
    {St2, Areas1} = add_material(edit, Tx, Uvs0#uvstate.st, Areas1),
    We2 = insert_uvcoords(Areas1),
    Shapes1 = gb_trees:update(We#we.id, We2, St2#st.shapes),
    St3 = St2#st{shapes = Shapes1},
    get_event(Uvs0#uvstate{st = St3});
handle_event({action, {auv, edge_options}}, Uvs) ->
    edge_option_menu(Uvs);
handle_event({action,{auv,quit}}, Uvs) ->
    quit_menu(Uvs);
handle_event({action,{auv,quit,cancel}}, _) ->
    delete;
handle_event({action, {auv,quit,QuitOp}}, Uvs) ->
    wings_wm:send(geom, {action,{body,{?MODULE,uvmap_done,QuitOp,Uvs}}}),
    delete;
handle_event({action, {auv, set_options, {EMode,BEC,BEW,Color,TexBG,TexSz}}},
	     Uvs0) ->
    Uvs1 = Uvs0#uvstate{option = 
			#setng{edges = EMode, 
			       edge_color = BEC,
			       edge_width = BEW,
			       color = Color, 
			       texbg = TexBG,
			       texsz = {TexSz,TexSz}},
			dl = undefined},
    get_event(Uvs1);
handle_event({action, {auv, rescale_all}},
	     Uvs0=#uvstate{sel = Sel0,areas=As=#areas{as=Curr0}})->
    RscAreas = rescale_all(add_as(Sel0,Curr0)),
    get_event(Uvs0#uvstate{sel = [],
			   areas=As#areas{as=RscAreas},
			   dl = undefined});
handle_event({action, {auv, {rotate, free}}}, Uvs) ->
    handle_event({action, {auv, rotate}}, Uvs);
handle_event({action, {auv, {rotate, Deg}}},
	     Uvs0=#uvstate{mode=Mode,sel=Sel0, areas=#areas{we=We0}=Areas}) ->
    Uvs = case Deg of
	      rot_y_180 ->
		  Sel1 = [transpose_x(Mode, A) || A <- Sel0],
		  Uvs0#uvstate{sel = Sel1};
	      rot_x_180 ->
		  Sel1 = [transpose_y(Mode, A) || A <- Sel0],
		  Uvs0#uvstate{sel = Sel1};
	      Deg ->
		  Sel1 = [finish_rotate({Id,A#ch{rotate = Deg}})|| {Id,A} <- Sel0],
		  Uvs0#uvstate{op = undefined, sel = Sel1}
	  end,
    We = replace_uvs(Uvs#uvstate.sel, We0),
    get_event(Uvs#uvstate{areas=Areas#areas{we=We}});
handle_event({action, {auv, NewOp}},Uvs0=#uvstate{sel = Sel0}) ->
    case Sel0 of
	[] ->
	    get_event(Uvs0);
	_Else ->
	    %%      ?DBG("Got uv OP ~p ~n", [NewOp]),
	    get_event(Uvs0#uvstate{op=#op{name=NewOp,undo=Uvs0}})
    end;
handle_event({callback, Fun}, _) when function(Fun) ->
    Fun();
handle_event({resize,_,_},Uvs0) ->
    wings_draw_util:init(),
    St1 = wings_material:init(Uvs0#uvstate.st),	    
    {_,Geom} = init_drawarea(),
    get_event(Uvs0#uvstate{geom=Geom, st=St1, dl=undefined});
handle_event({action,_, {view,smoothed_preview}}, _Uvs0) ->
    keep; %% Bugbug didn't work crashes inside wings update_dlists
handle_event({action,wings,{view, Cmd}}, Uvs0) ->
    St = wings_view:command(Cmd, Uvs0#uvstate.st),
    get_event(Uvs0#uvstate{st=St});
handle_event({current_state,St}, Uvs) ->
    verify_state(St, Uvs);
handle_event(_Event, Uvs) ->
    ?DBG("Got unhandled Event ~p ~n", [_Event]),
    get_event(Uvs).

handle_mousemotion(#mousemotion{xrel = DX0, yrel = DY0, x=MX0,y=MY0}, Uvs0) ->
    #uvstate{geom={X0Y0,MW0,X0Y0,MH0},mode=Mode,op=Op,sel=Sel0}=Uvs0,
    {_,_,W,H} = wings_wm:viewport(),
    {DX,DY} = case Op#op.prev of 
		  undefined -> {DX0,DY0}; 
		  {MX1,MY1}->  %% Don't trust relative mouse event
		      {MX0-MX1, MY0-MY1}
	      end,
    MW =  (MW0-X0Y0) * DX/W,
    MH = -(MH0-X0Y0) * DY/H,
%%    ?DBG("Viewp ~p ~p ~p ~p ~p ~p~n", [MW,MH,DX,DY,MX,MY]),
    NewOp = Op#op{prev={MX0,MY0}},
    case Op#op.name of
	move ->
	    Sel1 = [move_area(Mode, A,MW,MH)|| A <- Sel0],
	    get_event(Uvs0#uvstate{sel = Sel1, op=NewOp});
	fmove ->
	    Sel1 = [move_area(Mode, A,MW,MH)|| A <- Sel0],
	    get_event(Uvs0#uvstate{sel = Sel1, op=NewOp});
	scale ->
	    Sel1 = [scale_area(Mode, A,MW,MH)|| A <- Sel0],
	    get_event(Uvs0#uvstate{sel = Sel1, op=NewOp});
	rotate ->
	    Sel1 = [rotate_area(Mode, A,MW,MH)|| A <- Sel0],
	    get_event(Uvs0#uvstate{sel = Sel1, op=NewOp});
	boxsel ->
	    gl:matrixMode(?GL_PROJECTION),
	    gl:pushMatrix(),
	    gl:loadIdentity(),
	    {_,_,WW,WH} = wings_wm:viewport(),
	    glu:ortho2D(0, WW, WH, 0),
	    gl:drawBuffer(?GL_FRONT),
	    gl:matrixMode(?GL_MODELVIEW),
	    gl:loadIdentity(),
	    gl:color3f(0,0,0),
	    draw_marquee(Op#op.add, Op#op.prev),
	    draw_marquee(Op#op.add, {MX0,MY0}),
	    gl:popMatrix(),
	    gl:flush(),
	    gl:drawBuffer(?GL_BACK),
	    get_event_nodraw(Uvs0#uvstate{op = NewOp});
	_ ->
	    keep
    end.

import_file(default, Uvs0) ->
    import_file(Uvs0#uvstate.last_file, Uvs0);
import_file(Filename, Uvs0) ->
    Ps = [{filename,Filename},{type,r8g8b8},{alignment,1},{order,lower_left}],
    case wpa:image_read(Ps) of
	{error,Error0} ->
	    Error = Filename ++ ": " ++ file:format_error(Error0),
	    wings_util:message("Import failed: " ++ Error);
	#e3d_image{width = TW, height = TH, image = TexBin} ->
	    case (TW == TH) andalso is_power_of_two(TW) of
		true ->
		    add_texture_image(TW, TH, TexBin, Filename, Uvs0);
		false ->
		    wings_util:message("Import failed: Can only import square," 
				       "power of 2 sized pictures", Uvs0#uvstate.st)	
	    end
    end.

add_texture_image(TW, TH, TexBin, FileName, #uvstate{option=Opt}=Uvs0) ->
    {St1,_As} = add_material(edit, {TW,TH,TexBin},
			     Uvs0#uvstate.st, Uvs0#uvstate.areas),
    get_event(Uvs0#uvstate{st=St1, 
			   option=Opt#setng{texbg = true}, 
			   last_file = FileName,
			   dl = undefined}).

is_power_of_two(X) ->
    (X band -X ) == X.

%%%%% Selection 
draw_marquee({X, Y}, {Ox,Oy}) ->
    gl:color3f(1.0, 1.0, 1.0),
    gl:enable(?GL_COLOR_LOGIC_OP),
    gl:logicOp(?GL_XOR),
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2i(X, Oy),
    gl:vertex2i(X, Y),
    gl:vertex2i(Ox, Y),
    gl:vertex2i(Ox, Oy),
    gl:'end'(),
    gl:flush(),
    gl:disable(?GL_COLOR_LOGIC_OP);
draw_marquee(_,_) -> ok.
    
update_selection(Areas, Sel0, Other0) -> 
    foldl(fun(Hit = {[Id|_],Area}, {Sel, Other}) ->
		  case gb_trees:lookup(Id, Other) of
		      {value, _} -> %% other 
			  {[Hit|Sel], gb_trees:delete(Id, Other)};
		      none ->
			  {lists:delete(Hit,Sel), gb_trees:insert(Id, Area,Other)}
		  end
	  end, {Sel0, Other0}, Areas).

select(Mode, X,Y, Objects, We, {XYS,XM,XYS,YM}=ViewP) ->
    {_,_,UVW,UVH} = wings_wm:viewport(),
    XT = (XM-XYS)*X/UVW+XYS,
    YT = (YM-XYS)*Y/UVH+XYS,
    case find_selectable(XT,YT, gb_trees:to_list(Objects), []) of
	[] -> 
	    none;
	Possible ->
	    select(Mode, X,Y, 3,3, gb_trees:from_orddict(Possible), We, ViewP)
    end.

select(Mode, X,Y, W, H, Objects0, We, {XYS,XM,XYS,YM}) ->
    {_,_,UVW,UVH} = wings_wm:viewport(),
    HitBuf = get(wings_hitbuf),
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    [_WX,_WY,WH,WW]= gl:getIntegerv(?GL_VIEWPORT),
    gl:viewport(0,0,UVW,UVH),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:pickMatrix(float(X), float(Y), W,H, {0,0,UVW,UVH}),
    glu:ortho2D(XYS,XM,XYS,YM),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    select_draw(gb_trees:to_list(Objects0), Mode, We),
    gl:flush(),
    gl:viewport(0,0,WH,WW),	    
    case gl:renderMode(?GL_RENDER) of
	0 -> 
	    none;
	NumHits ->
	    HitData = sdl_util:readBin(HitBuf, 5*NumHits),
%	    ?DBG("Hits ~p ", [NumHits]),
	    Hits = get_hits(NumHits, HitData, []),
%	    ?DBG("Hits ~p ~n", [lists:usort(Hits)]),
	    map(fun(Hit = [FG|_]) -> 
			HitArea = gb_trees:get(FG, Objects0),
			{Hit, HitArea}
		end, lists:usort(Hits))
    end.

select_draw([{Co,A}|R], Mode, We) ->
%%    ?DBG("Co ~p\n",[Co]),
    #ch{fs=Fs,center={CX,CY},scale=Scale,rotate=Rot} = A,
    gl:pushMatrix(),
    gl:pushName(0),
    gl:loadName(Co),
    gl:translatef(CX,CY,0.0),
    gl:scalef(Scale, Scale, 1.0),
    gl:rotatef(Rot,0,0,1),
    select_draw1(Mode, Fs, We#we{mode=material}),
    gl:popName(),
    gl:popMatrix(),
    select_draw(R, Mode, We);
select_draw([], _, _) ->
    ok.

select_draw1(faceg, Fs, We) ->
    draw_faces(Fs, We);
select_draw1(face, Fs, We) ->
    select_draw_faces(Fs,We, wings_pref:get_value(display_list_opt)),
    gl:edgeFlag(?GL_TRUE);
select_draw1(edge, Fs, We = #we{vs = Vtab}) ->
    DrawEdge = fun(_Face, _V, Edge, #edge{vs=Va,ve=Vb}, _) ->
		       gl:pushName(Edge),
		       gl:glBegin(?GL_LINES),
		       gl:vertex3fv(wings_vertex:pos(Va, Vtab)),
		       gl:vertex3fv(wings_vertex:pos(Vb, Vtab)),
		       gl:glEnd(),   
		       gl:popName(),
		       ok
	       end,
    wings_face:fold_faces(DrawEdge, ok, Fs, We);
select_draw1(vertex, Fs, We = #we{vs = Vtab}) ->
    DrawPoint = fun(_Face, V, _Edge, _Rec, _) ->
			gl:pushName(V),
			gl:glBegin(?GL_POINTS),
			gl:vertex3fv(wings_vertex:pos(V, Vtab)),
			gl:glEnd(),   
			gl:popName(),
			ok
		end,
    wings_face:fold_faces(DrawPoint, ok, Fs, We).

select_draw_faces([], _We, _) ->    ok;
select_draw_faces([H|R], We, false) ->
    gl:pushName(H),
    draw_face(H, We),
    gl:popName(),
    select_draw_faces(R,We,false);
select_draw_faces([H|R], We, true) ->
    gl:pushName(H),
    gl:'begin'(?GL_TRIANGLES),
    draw_face(H, We),
    gl:'end'(),
    gl:popName(),
    select_draw_faces(R,We,true).

get_hits(0, _, Acc) -> Acc;
get_hits(N, <<NumNames:32,_:32,_:32,Tail0/binary>>, Acc) ->
    <<Names:NumNames/binary-unit:32,Tail/binary>> = Tail0,
    Name = get_name(NumNames, Names, []),
    get_hits(N-1, Tail, [Name|Acc]).

get_name(0, _Tail, Acc) -> reverse(Acc);
get_name(N, <<Name:32,Names/binary>>, Acc) ->
    get_name(N-1, Names, [Name|Acc]).

-define(OUT, 1.2/2). %% was 1/2 

find_selectable(X,Y, [A={_, #ch{center={CX,CY},size={W,H}}}|Rest], Acc)
  when X > (CX-W*?OUT), X < (CX+W*?OUT), Y > (CY-H*?OUT), Y < (CY+H*?OUT) ->
    find_selectable(X,Y, Rest, [A|Acc]);
find_selectable(X,Y, [_H|R], Acc) ->
    find_selectable(X,Y, R, Acc);
find_selectable(_X,_Y, [], Acc) ->
    reverse(Acc).

wings_select_faces([], _, St) ->
    wpa:sel_set(face, [], St);
wings_select_faces(As, #we{id=Id}, St) ->
    Faces = [A#ch.fs || {_,A} <- As],
    wpa:sel_set(face, [{Id,gb_sets:from_list(lists:append(Faces))}], St).

%%%% GUI Operations

greatest(A,B) ->
    if abs(A) > abs(B) ->
	    A;
       true ->
	    B
    end.

move_area(faceg, {Id, A = #ch{center = {X0,Y0}}}, DX, DY) ->
%    ?DBG("Move ~p ~p ~p~n", [{X0,Y0}, S, {DX, DY}]),
%%    A#ch{center = {X0+DX/S, Y0+DY/S}}.
    {Id, A#ch{center = {X0+DX, Y0+DY}}}.
scale_area(faceg,{Id,A = #ch{scale = S, size = {W,H}}}, DX, DY) ->
    NS = greatest(DX,DY),
    {Id,A#ch{scale = S+NS, size = {W+W*NS, H+H*NS}}}.
rotate_area(faceg, {Id,A = #ch{rotate = R}}, DX, DY) ->
    NS = greatest(DX,DY),
    NewR = R + NS*180,
    {Id,A#ch{rotate = NewR}}.
transpose_x(faceg,{Id,A = #ch{vpos = Vpos}}) ->
    New = [{Nr, {-X,Y,Z}} || {Nr, {X,Y,Z}} <- Vpos],
    {Id,A#ch{vpos=New}}.
transpose_y(faceg,{Id,A = #ch{vpos = Vpos}}) ->
    New = [{Nr, {X,-Y,Z}} || {Nr, {X,Y,Z}} <- Vpos],
    {Id,A#ch{vpos=New}}.

rescale_all(Areas0) ->
    Areas1 = gb_trees:to_list(Areas0),
    Find = fun({_, #ch{center = {CX,CY}, size = {W,H}}}, [MX,MY]) ->
		   TX = CX + W/2,
		   TY = CY + H/2,
		   NewMX = if TX > MX -> TX; true -> MX end,
		   NewMY = if TY > MY -> TY; true -> MY end,
		   [NewMX, NewMY]
	   end,
    Max = max(foldl(Find, [0,0], Areas1)),
    NS = 1.0 / Max,
    Rescale = fun({Id,A = #ch{center = {CX0,CY0}, size = {W0,H0}, scale = S0}}) ->
		      {Id,A#ch{center = {CX0*NS,CY0*NS}, size = {W0*NS,H0*NS}, scale = S0*NS}}
	      end,
    gb_trees:from_orddict(map(Rescale, Areas1)).
finish_rotate({Id,Area = #ch{rotate = R, vpos = Vs0, scale = S}}) ->
    Rot = e3d_mat:rotate(float(trunc(R)), {0.0,0.0,1.0}),
    Vs1 = [{IdV, e3d_mat:mul_point(Rot, Vec)} || {IdV, Vec} <- Vs0],
    {{_,BX0},{_,BX1},{_,BY0},{_,BY1}} = auv_util:maxmin(Vs1),
    {Id,Area#ch{rotate=0.0, vpos = Vs1, size={(BX1-BX0)*S, (BY1-BY0)*S}}}.

%%%
%%% Verify that the model in the geometry window hasn't changed its topology.
%%%
verify_state(St, Uvs) ->
    case same_topology(St, Uvs) of
	true -> keep;
	false -> {seq,push,get_broken_event(Uvs)}
    end.

same_topology(#st{shapes=Shs},
	      #uvstate{areas=#areas{orig_we=#we{id=Id}=We,edges=Edges}}) ->
    case gb_trees:lookup(Id, Shs) of
	none -> false;
	{value,We} -> true;
	{value,#we{es=Etab}} -> gb_trees:keys(Etab) =:= Edges
    end.

get_broken_event(Uvs) ->
    {replace,fun(Ev) -> broken_event(Ev, Uvs) end}.

broken_event(redraw, #uvstate{areas=#areas{orig_we=#we{name=Name}}}) ->
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
broken_event({current_state,St}, Uvs) ->
    case same_topology(St, Uvs) of
	false -> keep;
	true ->
	    wings_wm:dirty(),
	    pop
    end;
broken_event({action,{autouv,cancel}}, _) ->
    delete;
broken_event(Ev, _) ->
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} ->
	    Menu = [{"Cancel",cancel,"Cancel UV mapping"}],
	    wings_menu:popup_menu(X, Y, autouv, Menu)
    end.

%%%
%%% Draw routines.
%%%
draw_area(#ch{fs=Fs,center={CX,CY},scale=Scale,rotate=R,be=Tbe}, 
	  We, Options = #setng{color = ColorMode, edges = EdgeMode}, Materials) -> 
    gl:pushMatrix(),
    gl:translatef(CX, CY, 0.0),
    gl:scalef(Scale, Scale, 1.0),
    gl:rotatef(float(trunc(R)),0,0,1),
    %% Draw Materials and Vertex Colors
    if
	EdgeMode == border_edges ->
	    %% Draw outer edges only
	    #we{es=Etab, vs=Vtab}=We,
	    gl:pushMatrix(),
	    gl:lineWidth(Options#setng.edge_width),
	    DrawEdge = 
		case We#we.mode of
		    material when Options#setng.edge_color == true -> 
			gl:translatef(0,0,-0.5),
			fun({Edge,Face}) ->
				#edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
				gl:color4fv(get_material(Face, Materials, We)),
				gl:vertex3fv(wings_vertex:pos(Va, Vtab)),
				gl:vertex3fv(wings_vertex:pos(Vb, Vtab))
			end;
		    vertex when Options#setng.edge_color == true -> 
			gl:translatef(0,0,-0.5),
			fun({Edge,_}) ->
				#edge{vs=Va, a=VaC, ve=Vb, b=VbC} =
				    gb_trees:get(Edge, Etab),
				gl:color3fv(VaC),
				gl:vertex3fv(wings_vertex:pos(Va, Vtab)),
				gl:color3fv(VbC),
				gl:vertex3fv(wings_vertex:pos(Vb, Vtab))
			end;
		    _ ->
			gl:translatef(0,0,0.5),
			fun({Edge, _}) ->
				#edge{vs = Va, ve = Vb} =
				    gb_trees:get(Edge, Etab),
				gl:vertex3fv(wings_vertex:pos(Va, Vtab)),
				gl:vertex3fv(wings_vertex:pos(Vb, Vtab))
			end
		end,
	    gl:glBegin(?GL_LINES),
	    gl:color3f(0.6, 0.6, 0.6),
	    lists:foreach(DrawEdge, Tbe),
	    gl:glEnd(),
	    gl:popMatrix();
	EdgeMode == all_edges ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:color3f(0.6, 0.6, 0.6),
	    draw_faces(Fs, We#we{mode=material});
	EdgeMode == no_edges ->
	    ok
    end,
    if
	ColorMode == true ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    MatName = (gb_trees:get(hd(Fs), We#we.fs))#face.mat,
	    wings_material:apply_material(MatName, Materials),
	    lists:foreach(fun(Face) ->
				  gl:color4fv(get_material(Face, Materials, We)),
				  draw_faces([Face], We)
			  end, Fs),
	    case has_texture(MatName, Materials) of
		true -> gl:disable(?GL_TEXTURE_2D);
		false -> ignore
	    end;
	is_tuple(ColorMode), size(ColorMode) == 4 ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:color4fv(ColorMode),
	    draw_faces(Fs, We#we{mode = material});
	true ->
	    ignore
    end,
    gl:popMatrix().

draw_faces(Fs, We) ->
    wings_draw_util:begin_end(fun() -> draw_faces2(Fs, We) end).

draw_faces2([], _We) -> ok;
draw_faces2([H|T], We) ->
    draw_face(H, We),
    draw_faces2(T, We).

draw_face(Face, #we{mode=material}=We) ->
    wings_draw_util:flat_face(Face, We);
draw_face(Face, #we{vs=Vtab}=We) ->
    Vs0 = wings_face:vinfo(Face, We),
    draw_face_1(Vs0, Vtab, [], []).

draw_face_1([[V|Col]|Vs], Vtab, Nacc, VsAcc) ->
    #vtx{pos=Pos} = gb_trees:get(V, Vtab),
    draw_face_1(Vs, Vtab, [Pos|Nacc], [[Pos|Col]|VsAcc]);
draw_face_1([], _, Nacc, Vs) ->
    N = e3d_vec:normal(reverse(Nacc)),
    gl:normal3fv(N),
    Tess = wings_draw_util:tess(),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    {X,Y,Z} = N,
    glu:tessNormal(Tess, X, Y, Z),
    tess_face_vtxcol(Tess, Vs).

tess_face_vtxcol(Tess, [[Pos|{_,_}=UV]|T]) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,UV}]),
    tess_face_vtxcol(Tess, T);
tess_face_vtxcol(Tess, [[Pos|{_,_,_}=Col]|T]) ->
    glu:tessVertex(Tess, Pos, [{color,Col}]),
    tess_face_vtxcol(Tess, T);
tess_face_vtxcol(Tess, []) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

set_viewport({X,Y,W,H}=Viewport) ->
    put(wm_viewport, Viewport),
    gl:viewport(X, Y, W, H).

%% Generate a checkerboard image.
checkerboard(Sz) ->
    White = [255,255,255],
    Black = [0,0,0],
    FourWhite = check_repeat(4, White),
    FourBlack = check_repeat(4, Black),
    R1 = check_repeat(Sz div 8, [FourBlack|FourWhite]),
    R2 = check_repeat(Sz div 8, [FourWhite|FourBlack]),
    R8 = [check_repeat(4, R1)|check_repeat(4, R2)],
    list_to_binary(check_repeat(Sz div 8, R8)).

check_repeat(0, _) -> [];
check_repeat(1, D) -> [D];
check_repeat(N, D) ->
    B = check_repeat(N div 2, D),
    case N rem 2 of
	0 -> [B|B];
	1 -> [D,B|B]
    end.
