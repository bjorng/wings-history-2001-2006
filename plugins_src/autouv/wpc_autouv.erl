%% File    : wpc_autouv.erl
%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%% Description : A semi-simple semi-automatic UV-mapping plugin
%%
%% Created : 24 Jan 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%-------------------------------------------------------------------
%%  Copyright (c) 2002-2003 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: wpc_autouv.erl,v 1.168 2003/11/28 15:35:03 raimo_niskanen Exp $

-module(wpc_autouv).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
 
-export([init/0,menu/2,command/2,redraw/1,update_dlists/1]).

-import(lists, [sort/1,map/2,foldl/3,reverse/1,
		append/1,delete/2,usort/1,max/1,min/1,
		member/2,foreach/2,keysearch/3]).

init() ->
    true.

menu({tools}, Menu) ->
    Menu ++ [separator,
	     {"Snap Image", {auv_snap, [{"Start Snap Mode", auv_snap_image, 
					 "Snap image to selected faces"}, 
					{"Quit Snap Mode", auv_cancel_snap, 
					 "Quit Snap Image Mode"}]}}];
menu({body}, Menu) ->
    case auv_snap:active() of
	true ->
	    Menu;
	false ->
	    Menu ++ [separator,
		     {"UV Mapping", ?MODULE,
		      "Generate or edit UV mapping or texture"}
		    ]
    end;

menu({face}, Menu) ->
    case auv_snap:active() of
	false ->
	    Menu;
	true ->
	    Menu ++ [separator,{"Snap Image", auv_complete_snap,
				"Put Image on select faces"}
		    ]
    end;

menu(_, Menu) -> Menu.

%% SNAP
command({face, auv_complete_snap}, St) ->
    auv_snap:complete(St);
command({tools, {auv_snap, auv_snap_image}}, St) ->
    auv_snap:select_image(St);
command({_, {auv_snap,auv_cancel_snap}}, St) ->
    auv_snap:cancel(St);
%%SNAP

command({body,?MODULE}, St) ->
    ?DBG("Start shapes ~p~n",[gb_trees:keys(St#st.shapes)]), 
    start_uvmap(St);
command({body,{?MODULE,uvmap_done,QuitOp,Uvs0}}, St0) ->
    Uvs = clear_selection(Uvs0),
    #uvstate{areas=Charts,matname=MatName0,orig_we=OrWe} = Uvs,
    {St,MatName} =
	case QuitOp of
	    quit_uv_tex ->
		Tx = ?SLOW(get_texture(Uvs)),
		add_material(Tx, OrWe#we.name, MatName0, St0);
	    quit_uv -> {St0,MatName0}
	end,
    ?SLOW(insert_uvcoords(Charts, OrWe#we.id, MatName, St));
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

start_edit_1(Win, #we{name=ObjName,fs=Ftab}=We, St) ->
    MatNames0 = wings_material:get_all(We),
    MatNames1 = sofs:from_external(MatNames0, [{face,material}]),
    MatNames2 = sofs:converse(MatNames1),
    MatNames3 = sofs:relation_to_family(MatNames2),
    MatNames4 = sofs:to_external(MatNames3),
    MatNames = [Mat || {Name,_}=Mat <- MatNames4, has_texture(Name, St)],
    case MatNames of
	[] ->
	    Faces = gb_trees:keys(Ftab),
	    MatName = list_to_atom(ObjName ++ "_auv"),
	    gen_edit_event(Win, MatName, Faces, We);
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
    {Edges,Areas,MatName} = init_edit(MatName0, Faces, We),
    TexSz = get_texture_size(MatName, St),
    Options = #setng{color=false,texbg=true,texsz=TexSz},
    create_uv_state(Edges, Areas, MatName, Options, We, St).

%%%%%%

init_show_maps(Map0, #we{es=Etab}=We, St) ->
    Map1  = auv_placement:place_areas(Map0),
    Map2  = find_boundary_edges(Map1, []),
    Map   = gb_trees:from_orddict(Map2),
    Edges = gb_trees:keys(Etab),
    create_uv_state(Edges, Map, none, #setng{}, We, St).

create_uv_state(Edges, Map, MatName, Options, We, St) ->
    wings:mode_restriction([body]),
    wings_wm:current_state(#st{selmode=body,sel=[]}),
    {_,Geom} = init_drawarea(),
    Uvs = #uvstate{st=wpa:sel_set(face, [], St),
		   origst=St,
		   areas=Map,
		   geom=Geom,
		   orig_we=We,
		   edges=Edges,
		   matname=MatName,
		   option=Options},
    Name = wings_wm:this(),
    wings_wm:set_prop(Name, drag_filter, fun drag_filter/1),
    get_event(Uvs).

find_boundary_edges([{Id,#we{name=#ch{fs=Fs}}=We}|Cs], Acc) ->
    Be = auv_util:outer_edges(Fs, We),
    find_boundary_edges(Cs, [{Id,We#we{he=Be}}|Acc]);
find_boundary_edges([], Acc) -> sort(Acc).
   
insert_uvcoords(Charts, Id, MatName, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    We = insert_uvcoords_1(We0, Charts, MatName),
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{shapes=Shs}.

insert_uvcoords_1(We0, Cs0, MatName) ->
    Cs = gb_trees:values(Cs0),
    UVpos = gen_uv_pos(Cs, []),
    We1 = insert_coords(UVpos, We0),
    We = insert_material(Cs, MatName, We1),
    We#we{mode=material}.

gen_uv_pos([#we{vp=Vpos0,name=#ch{fs=Fs,vmap=Vmap}}=We|T], Acc) ->
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
    gen_uv_pos(T, Comb);
gen_uv_pos([], Acc) -> Acc.

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
insert_coords([], We0 = #we{es=Etab0}) -> 
    %% Assure that no vertex colors pre-exist after insert coords 
    %% i.e. face marked with hole material
    Etab = lists:map(fun({Id,E=#edge{a={_,_,_},b={_,_,_}}}) -> 
			     {Id,E#edge{a=none,b=none}};
			({Id,E=#edge{a={_,_,_}}}) -> 
			     {Id,E#edge{a = none}};
			({Id,E=#edge{b={_,_,_}}}) -> 
			     {Id,E#edge{b = none}};
			(Rec) -> Rec
		     end, gb_trees:to_list(Etab0)),
    We0#we{es=gb_trees:from_orddict(Etab)}.

insert_material(Cs, MatName, We) ->
    Faces = lists:append([Fs || #we{name=#ch{fs=Fs}} <- Cs]),
    wings_material:assign(MatName, Faces, We).

init_edit(MatName, Faces, We0) ->
    FvUvMap = auv_segment:fv_to_uv_map(Faces, We0),
    {Charts1,Cuts} = auv_segment:uv_to_charts(Faces, FvUvMap, We0),
    Charts = auv_segment:cut_model(Charts1, Cuts, We0),
    Map1 = build_map(Charts, FvUvMap, 1, []),
    Map2 = find_boundary_edges(Map1, []),
    Map  = gb_trees:from_orddict(Map2),
    Edges= gb_trees:keys(We0#we.es),
    {Edges,Map,MatName}.

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
    UVs = [{V,{X,Y,0.0}} || {V,{X,Y}} <- UVs1],
    Chart = #ch{fs=Fs,size=undefined,vmap=Vmap},
    We = We0#we{name=Chart,id=No,vp=gb_trees:from_orddict(UVs)},
    build_map(T, FvUvMap, No+1, [{No,We}|Acc]);
build_map([], _, _, Acc) -> Acc.

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

get_texture_size(MatName, #st{mat=Materials}) ->
    Mat = gb_trees:get(MatName, Materials),
    Maps = proplists:get_value(maps, Mat, []),
    case proplists:get_value(diffuse, Maps, none) of
	none -> {512,512};
	ImageId ->
	    #e3d_image{width=W,height=H} = wings_image:info(ImageId),
	    {W,H}
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
    
%%% Opengl drawing routines

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

draw_texture(#uvstate{dl=undefined,option=Options,areas=As}=Uvs) ->
    Materials = (Uvs#uvstate.origst)#st.mat,
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    foreach(fun(#we{}=We) ->
		    draw_area(We, Options, Materials)
	    end, gb_trees:values(As)),
    gl:endList(),
    draw_texture(Uvs#uvstate{dl=Dl});
draw_texture(#uvstate{dl=DL}=Uvs) ->
    gl:enable(?GL_DEPTH_TEST),
    gl:callList(DL),
    draw_selection(Uvs),
    Uvs.

draw_selection(#uvstate{sel=[]}) -> ok;
draw_selection(Uvs) ->
    {R,G,B} = wings_pref:get_value(selected_color),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_BLEND),
    gl:translatef(0.0, 0.0, 0.1),
    Settings = #setng{color={R,G,B,0.7},edges=all_edges},
    sel_foreach(fun(We) -> draw_area(We, Settings, []) end, Uvs),
    gl:disable(?GL_BLEND).

setup_view(#uvstate{geom={Left,Right,Bottom,Top},st=#st{mat=Mats},
		    option=#setng{texbg=TexBg},matname=MatN}) ->
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    glu:ortho2D(Left, Right, Bottom, Top),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:color3f(1, 1, 1),

    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),

    gl:pushMatrix(),

    gl:translatef(0, 0, -0.99),
    gl:rectf(Left, Bottom, Right, Top),
    gl:translatef(0, 0, 0.01),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:color3b(0, 0, 0),
    gl:'begin'(?GL_LINE_LOOP),
    D = Left/10,
    gl:vertex2f(D, D),
    gl:vertex2f(1-D, D),
    gl:vertex2f(1-D, 1-D),
    gl:vertex2f(D, 1-D),
    gl:'end'(),    
    gl:popMatrix(),

    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1.0, 1.0, 1.0),   %%Clear
    case TexBg of
	true when MatN == none -> ok;
	true -> wings_material:apply_material(MatN, Mats);
	false -> ok
    end,
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0,0),    gl:vertex3f(0,0,-0.9),
    gl:texCoord2f(1,0),    gl:vertex3f(1,0,-0.9),
    gl:texCoord2f(1,1),    gl:vertex3f(1,1,-0.9),
    gl:texCoord2f(0,1),    gl:vertex3f(0,1,-0.9),
    gl:'end'(), 

    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:disable(?GL_TEXTURE_2D),
    gl:shadeModel(?GL_SMOOTH).

%%% Texture Creation

calc_texsize(Vp, Tex) ->
    calc_texsize(Vp, Tex, Tex).

calc_texsize(Vp, Tex, Orig) when Tex < Vp ->
    {Tex,Orig div Tex};
calc_texsize(Vp, Tex, Orig) ->
    calc_texsize(Vp, Tex div 2, Orig).

get_texture(#uvstate{option=#setng{texsz={TexW,TexH}}}=Uvs0) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    Current = wings_wm:viewport(),
    {W0,H0} = wings_wm:top_size(),
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
    ?DBG("Get texture sz ~p ~p ~n", [{W,Wd},{H,Hd}]),
    set_viewport({0,0,W,H}),
    Uvs = reset_dl(clear_selection(Uvs0)),
    ImageBins = get_texture(0, Wd, 0, Hd, {W,H}, Uvs, []),
    ImageBin = merge_texture(ImageBins, Wd, Hd, W*3, H, []),
    set_viewport(Current),
    gl:popAttrib(),

    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    case (TexW*TexH *3) == size(ImageBin) of
	true ->
	    #e3d_image{image=ImageBin,width=TexW,height=TexH};
	false ->
	    BinSzs = [size(Bin) || Bin <- ImageBins],
	    exit({texture_error,{TexW, TexH, size(ImageBin), 
				 W,Wd,H,Hd, BinSzs}})
    end.
		 
get_texture(Wc, Wd, Hc, Hd, {W,H}=Info, Uvs0, ImageAcc)
  when Wc < Wd, Hc < Hd ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:clearColor(1, 1, 1, 1),
    gl:shadeModel(?GL_SMOOTH),
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),
    texture_view(Wc, Wd, Hc, Hd, Uvs0),
    Uvs = draw_texture(Uvs0),
    gl:flush(),
    gl:readBuffer(?GL_BACK),
    Mem = sdl_util:alloc(W*H*3, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin = sdl_util:getBin(Mem),
    get_texture(Wc+1, Wd, Hc, Hd, Info, Uvs, [ImageBin|ImageAcc]);
get_texture(_Wc,Wd,Hc,Hd, Info, Uvs, ImageAcc) when Hc < Hd ->
    get_texture(0, Wd, Hc+1, Hd, Info, Uvs, ImageAcc);
get_texture(_, _, _, _, _, _, ImageAcc) -> reverse(ImageAcc).

texture_view(WC, WD, HC, HD, Uvs) ->
    #uvstate{st=#st{mat=Mats}, option=#setng{texbg=TexBg},
	     matname = MatN}=Uvs,
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

%%%%%%% Events handling and window redrawing 
   
get_event(Uvs) ->
    wings_wm:dirty(),
    get_event_nodraw(Uvs).

get_event_nodraw(Uvs) ->
    {replace,fun(Ev) -> handle_event(Ev, Uvs) end}.

redraw(#st{bb=Uvs}=St) ->
    update_dlists(St),
    redraw(Uvs);
redraw(#uvstate{areas=Shs,origst=#st{mat=Mat}}=Uvs0) ->
    St = #st{selmode=body,shapes=Shs,mat=Mat},
    update_dlists(St),
    wings_util:button_message("Select", [], "Show menu"),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    setup_view(Uvs0),
    Uvs = draw_texture(Uvs0),
    gl:popAttrib(),
    Uvs.

command_menu(body, X,Y, _Uvs) ->
    Rotate = [{"Free", free, "Rotate freely"},
	      {"90"++[?DEGREE]++" CW",-90,
	       "Rotate selection 90 degrees clockwise"},
	      {"90"++[?DEGREE]++" CCW",90,
	       "Rotate selection 90 degrees counter-clockwise"},
	      {"180"++[?DEGREE],180,"Rotate selection 180 degrees"},
	      separator,
	      {"Flip Horizontal",flip_horizontal,"Flip selection horizontally"},
	      {"Flip Vertical",flip_vertical,"Flip selection vertically"}],
    Menu = [{"Face Group operations", ignore},
	    separator,
	    {"Move", move, "Move selected faces"},
%%	    {"Scale", scale, "Uniformly scale selected faces"},
	    {"Rotate", {rotate, Rotate}, "Rotate selected faces"}
% 	    separator,
% 	    {"Rescale All", rescale_all, "Pack the space in lower-left before rescaling"}
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
     {"Apply Texture", apply_texture, "Attach the current texture to the model"}].

edge_option_menu(#uvstate{option = Option}) ->
    [MaxTxs0|_] = gl:getIntegerv(?GL_MAX_TEXTURE_SIZE),
    MaxTxs = min([4096,MaxTxs0]),
    
    Qs = [{vradio,[{"Draw All Edges",    all_edges},
		   {"Draw Border Edges", border_edges},
		   {"Don't Draw Edges",  no_edges}], 
	   Option#setng.edges, [{title,"Edge Options"}]},
	  {vframe,[{"Use Face/Vertex Color on Border Edges", Option#setng.edge_color},
		   {label_column, [{"Edge width",  {text, Option#setng.edge_width}}]}],
	   [{title, "Overdraw options"}]},
	  {vframe,[{"Show Colors (or texture)",Option#setng.color},
		   {"Texture Background (if available)", Option#setng.texbg}],
	   [{title, "Display Color and texture?"}]},
	  {vradio,gen_tx_sizes(MaxTxs, []),txsize,element(1, Option#setng.texsz),
	   [{title,"Texture Size"}]}],
    wings_ask:dialog("Draw Options", Qs,
		     fun([Mode,BEC,BEW,Color,TexBg, TSz]) -> 
			     {auv, set_options, {Mode,BEC,BEW,Color,TexBg,TSz}}  end).

gen_tx_sizes(Sz, Acc) when Sz < 128 -> Acc;
gen_tx_sizes(Sz, Acc) ->
    Bytes = Sz*Sz*3,
    Mb = 1024*1024,
    SzStr = if
		Bytes < 1024*1024 ->
		    io_lib:format("(~pKb)",[Bytes div 1024]);
		true ->
		    io_lib:format("(~pMb)",[Bytes div Mb])
	    end,
    Str0 = io_lib:format("~px~p ", [Sz,Sz]),
    Str = lists:flatten([Str0|SzStr]),
    gen_tx_sizes(Sz div 2, [{Str,Sz}|Acc]).

quit_menu(Uvs) ->
    #uvstate{st=St,matname=MatN} = Uvs,
    A1 = {"Save UV Coordinates and Texture",quit_uv_tex},
    A2 = {"Save Only UV Coordinates",quit_uv},
    A3 = {"Discard All Changes",cancel},
    Alts = case has_texture(MatN, St) of
	       true -> [A1,A2,A3];
	       false -> [A1,A3]
	   end,
    Qs = [{vradio,Alts,quit_mode,quit_uv_tex}],
    wings_ask:dialog("Exit Options",
		     Qs, fun([Quit]) -> {auv,quit,Quit} end).

%%% Event handling

handle_event(redraw, Uvs0) ->
    %%    ?DBG("redraw event\n"),
    Uvs = redraw(Uvs0),
    get_event_nodraw(Uvs);
handle_event(init_opengl, Uvs0) ->
    {_,_,W,H} = wings_wm:viewport(wings_wm:this()),
    Geom = wingeom(W, H),
    get_event(reset_dl(Uvs0#uvstate{geom=Geom}));
handle_event(resized, Uvs0) ->
    {_,_,W,H} = wings_wm:viewport(wings_wm:this()),
    Geom = wingeom(W,H),
    get_event(reset_dl(Uvs0#uvstate{geom=Geom}));
handle_event({current_state,geom_display_lists,St}, Uvs) ->
    case verify_state(St, Uvs) of
	keep -> update_selection(St, Uvs);
	Other -> Other
    end;
handle_event({new_uv_state,Uvs}, _) ->
    wings_wm:dirty(),
    get_event(reset_dl(Uvs));
handle_event({new_state,#st{selmode=Mode,sel=Sel,shapes=Shs}}, Uvs0) ->
    Uvs = Uvs0#uvstate{mode=Mode,sel=Sel,areas=Shs},
    GeomSt = wings_select_faces(Uvs),
    wings_wm:send(geom, {new_state,GeomSt}),
    get_event(reset_dl(Uvs#uvstate{st=GeomSt}));
handle_event(Ev, #uvstate{areas=Shs,sel=Sel,mode=Mode,origst=#st{mat=Mat}}=Uvs) ->
    St = #st{selmode=Mode,shapes=Shs,sel=Sel,mat=Mat,bb=Uvs},
    case auv_pick:event(Ev, St) of
	next -> handle_event_1(Ev, Uvs);
	Other -> Other
    end.

handle_event_1(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_RIGHT,x=X0,y=Y0}, 
	       #uvstate{mode=Mode}=Uvs) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    command_menu(Mode, X, Y, Uvs);
handle_event_1(#keyboard{state=?SDL_PRESSED,sym=?SDLK_SPACE}, #uvstate{st=St}) ->
    wings_wm:send(geom, {new_state,wpa:sel_set(face, [], St)});
handle_event_1({drop,_,DropData}, Uvs) ->
    handle_drop(DropData, Uvs);
handle_event_1({action,{auv,apply_texture}},
	       #uvstate{st=St0,orig_we=OWe,matname=MatName0}=Uvs) ->
    Tx = ?SLOW(get_texture(Uvs)),
    Charts = all_charts(Uvs),
    #we{name=Name,id=Id} = OWe,
    {St1,MatName} = add_material(Tx, Name, MatName0, St0),
    St = insert_uvcoords(Charts, Id, MatName, St1),
    wings_wm:send(geom, {new_state,St}),
    get_event(Uvs#uvstate{st=St,matname=MatName});
handle_event_1({action, {auv, edge_options}}, Uvs) ->
    edge_option_menu(Uvs);
handle_event_1({action,{auv,quit}}, Uvs) ->
    quit_menu(Uvs);
handle_event_1(close, Uvs) ->
    quit_menu(Uvs);
handle_event_1({action,{auv,quit,cancel}}, Uvs) ->
    restore_wings_window(Uvs),
    delete;
handle_event_1({action, {auv,quit,QuitOp}}, Uvs) ->
    restore_wings_window(Uvs),
    wings_wm:send(geom, {action,{body,{?MODULE,uvmap_done,QuitOp,Uvs}}}),
    delete;
handle_event_1({action, {auv, set_options, {EMode,BEC,BEW,Color,TexBG,TexSz}}},
	     Uvs0) ->
    Uvs1 = Uvs0#uvstate{option = 
			#setng{edges = EMode, 
			       edge_color = BEC,
			       edge_width = BEW,
			       color = Color, 
			       texbg = TexBG,
			       texsz = {TexSz,TexSz}}
		       },
    get_event(reset_dl(Uvs1));
handle_event_1({action,{auv,Command}}, Uvs) ->
    handle_command(Command, Uvs);
handle_event_1({callback, Fun}, _) when function(Fun) ->
    Fun();
handle_event_1(_Event, Uvs) ->
    ?DBG("Got unhandled Event ~p ~n", [_Event]),
    get_event(Uvs).

% handle_command(rescale_all, Uvs0) ->
%     Uvs = clear_selection(Uvs0),
%     RscAreas = rescale_all(all_charts(Uvs)),
%     get_event(reset_dl(Uvs0#uvstate{areas=RscAreas}));
handle_command({rotate,free}, Uvs) ->
    handle_command(rotate, Uvs);
handle_command({rotate,flip_horizontal}, Uvs0) ->
    Uvs = sel_map(fun(_, We) -> flip_horizontal(We) end, Uvs0),
    get_event(Uvs);
handle_command({rotate,flip_vertical}, Uvs0) ->
    Uvs = sel_map(fun(_, We) -> flip_vertical(We) end, Uvs0),
    get_event(Uvs);
handle_command({rotate,Deg}, Uvs0) ->
    Uvs = sel_map(fun(_, We) -> rotate_chart(Deg, We) end, Uvs0),
    get_event(Uvs);
handle_command(_, #uvstate{sel=[]}) ->
    keep;
handle_command(NewOp, Uvs) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    {seq,push,get_cmd_event(NewOp, X, Y, Uvs)}.

%%%
%%% Command handling (temporary version).
%%%

get_cmd_event(Op, X, Y, Uvs) ->
    wings_wm:dirty(),
    get_cmd_event_noredraw(Op, X, Y, Uvs).

get_cmd_event_noredraw(Op, X, Y, Uvs) ->
    {replace,fun(Ev) -> cmd_event(Ev, Op, X, Y, Uvs) end}.

cmd_event(redraw, Op, X, Y, Uvs0) ->
    Uvs = redraw(Uvs0),
    get_cmd_event_noredraw(Op, X, Y, Uvs);
cmd_event(#mousemotion{x=MX0,y=MY0}, Op, X0, Y0, Uvs0) ->
    #uvstate{geom={X0Y0,MW0,X0Y0,MH0}}=Uvs0,
    {_,_,W,H} = wings_wm:viewport(),
    DX = MX0 - X0,
    DY = MY0 - Y0,
    MW =  (MW0-X0Y0) * DX/W,
    MH = -(MH0-X0Y0) * DY/H,
%%    ?DBG("Viewp ~p ~p ~p ~p ~p ~p~n", [MW,MH,DX,DY,MX,MY]),
    Uvs = case Op of
	      move ->
		  sel_map(fun(_, We) -> move_chart(4*MW, 4*MH, We) end, Uvs0);
	      rotate ->
		  sel_map(fun(_, We) -> rotate_chart(MW*180, We) end, Uvs0)
	  end,
    get_cmd_event(Op, MX0, MY0, Uvs);
cmd_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_LEFT}, _, _, _, Uvs) ->
    wings_wm:later({new_uv_state,Uvs}),
    pop;
cmd_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_RIGHT}, _, _, _, _) ->
    wings_wm:dirty(),
    pop;
cmd_event(_, _, _, _, _) -> keep.

drag_filter({image,_,_}) ->
    {yes,"Drop: Change the texture image"};
drag_filter(_) -> no.

handle_drop({image,_,#e3d_image{width=W,height=H}=Im}, #uvstate{option=Opt0}=Uvs) ->
    case W =:= H andalso is_power_of_two(W) of
	false -> keep;
	true ->
	    Opt = Opt0#setng{color=false,edges=no_edges},    
	    add_texture_image(Im, Uvs#uvstate{option=Opt})
    end;
handle_drop(_DropData, _) ->
    %%io:format("~P\n", [_DropData,40]),
    keep.

add_texture_image(Im, #uvstate{st=St0,option=Opt,orig_we=OWe,matname=MatName0}=Uvs) ->
    Name = OWe#we.name,
    {St,MatName} = add_material(Im,Name,MatName0,St0),
    wings_wm:send(geom, {new_state,St}),
    get_event(reset_dl(Uvs#uvstate{st=St,matname=MatName,option=Opt#setng{texbg=true}})).

is_power_of_two(X) ->
    (X band -X ) == X.

update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #uvstate{st=#st{selmode=Mode,sel=Sel}}=Uvs) ->
    get_event_nodraw(Uvs#uvstate{st=St});
update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #uvstate{orig_we=#we{id=Id}}=Uvs0) ->
    Uvs = reset_dl(clear_selection(Uvs0)),
    case keysearch(Id, 1, Sel) of
	false ->
	    get_event(Uvs);
	{value,{Id,Elems}} ->
	    update_selection_1(Mode, gb_sets:to_list(Elems), Uvs#uvstate{st=St})
    end.

update_selection_1(face, Faces, #uvstate{areas=Charts}=Uvs) ->
    update_selection_2(gb_trees:to_list(Charts), Faces, Uvs, []);
update_selection_1(_, _, Uvs) ->
    get_event_nodraw(Uvs).

update_selection_2([{K,#we{name=#ch{fs=Fs}}}|Cs], Faces, Uvs, Sel) ->
    case ordsets:intersection(sort(Fs), Faces) of
	[] -> update_selection_2(Cs, Faces, Uvs, Sel);
	_ -> update_selection_2(Cs, Faces, Uvs, [{K,gb_sets:singleton(0)}|Sel])
    end;
update_selection_2([], _, Uvs, Sel) ->
    get_event(Uvs#uvstate{sel=sort(Sel)}).
    
-define(OUT, 1.2/2). %% was 1/2 

wings_select_faces(#uvstate{sel=[],st=GeomSt}) ->
    wpa:sel_set(face, [], GeomSt);
wings_select_faces(#uvstate{st=GeomSt,orig_we=#we{id=Id}}=Uvs) ->
    Fs0 = sel_fold(fun(_, #we{name=#ch{fs=Fs}}, A) ->
			   Fs++A
		   end, [], Uvs),
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
verify_state(St, Uvs) ->
    case same_topology(St, Uvs) of
	true -> keep;
	false ->
	    wings_wm:dirty(),
	    {seq,push,get_broken_event(Uvs)}
    end.

same_topology(#st{shapes=Shs}, #uvstate{orig_we=#we{id=Id}=We,edges=Edges}) ->
    case gb_trees:lookup(Id, Shs) of
	none -> false;
	{value,We} -> true;
	{value,#we{es=Etab}} -> gb_trees:keys(Etab) =:= Edges
    end.

get_broken_event(Uvs) ->
    {replace,fun(Ev) -> broken_event(Ev, Uvs) end}.

broken_event(redraw, #uvstate{orig_we=#we{name=Name}}) ->
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
broken_event({current_state,geom_display_lists,St}, Uvs) ->
    case same_topology(St, Uvs) of
	false -> keep;
	true ->
	    wings_wm:dirty(),
	    pop
    end;
broken_event({action,{autouv,cancel}}, Uvs) ->
    restore_wings_window(Uvs),
    delete;
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
draw_area(#we{name=#ch{fs=Fs},he=Tbe}=We,
	  #setng{color = ColorMode, edges = EdgeMode}=Options, Materials) -> 
    gl:pushMatrix(),
    gl:lineWidth(Options#setng.edge_width),
    %% Draw Materials and Vertex Colors
    if
	EdgeMode == border_edges ->
	    %% Draw outer edges only
	    #we{es=Etab,vp=Vtab}=We,
	    gl:pushMatrix(),
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
	    gl:pushMatrix(),
	    gl:translatef(0,0,0.9),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:color3f(0.6, 0.6, 0.6),
	    draw_faces(Fs, We#we{mode=material}),
	    gl:popMatrix();
	EdgeMode == no_edges ->
	    ok
    end,
    if
	ColorMode == true ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    MatName = wings_material:get(hd(Fs), We),
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
    Draw = fun(Face) -> face(Face, We) end,
    wings_draw_util:begin_end(fun() -> foreach(Draw, Fs) end).

%% XXX Wrong.
face(Face, #we{mode=material}=We) ->
    wings_draw_util:plain_face(Face, We);
face(Face, #we{mode=vertex}=We) ->
    wings_draw_util:vcol_face(Face, We).

reset_dl(Uvs = #uvstate{dl = undefined}) ->
    Uvs;
reset_dl(Uvs = #uvstate{dl = DL}) ->
    gl:deleteLists(DL, 1),
    Uvs#uvstate{dl = undefined}.

set_viewport({X,Y,W,H}=Viewport) ->
    put(wm_viewport, Viewport),
    gl:viewport(X, Y, W, H).

restore_wings_window(Uvs) ->
    wings_draw_util:delete_dlists(),
    reset_dl(Uvs).

%%%
%%% Most of this code will be rewritten as we slowly change the
%%% internal structures.
%%%

update_dlists(#st{}=St) ->
    wings_draw:invalidate_dlists(false, St).

clear_selection(Uvs) -> Uvs#uvstate{sel=[]}.

all_charts(#uvstate{areas=Charts}) -> Charts.

sel_map(F, #uvstate{areas=Shs0,sel=Sel}=Uvs) ->
    St = #st{shapes=Shs0,sel=Sel},
    #st{shapes=Shs} = wpa:sel_map(F, St),
    Uvs#uvstate{areas=Shs}.

sel_foreach(F, #uvstate{sel=Sel,areas=Shs}) ->
    foreach(fun({Id,_}) -> F(gb_trees:get(Id, Shs)) end, Sel).

sel_fold(F, Acc, #uvstate{sel=Sel,areas=Shs}) ->
    wpa:sel_fold(F, Acc, #st{sel=Sel,shapes=Shs}).
