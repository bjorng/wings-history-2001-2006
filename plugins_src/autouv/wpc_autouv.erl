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
%%     $Id: wpc_autouv.erl,v 1.127 2003/07/11 10:26:28 bjorng Exp $

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

add_areas(NewAreas, AreaTree) ->
    foldl(fun({[K|_],Area}, TreeBB) ->
 		  gb_trees:insert(K, Area, TreeBB) 
 	  end, AreaTree, NewAreas).

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
		     {"UV Mapping (experimental)", ?MODULE,
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
command({body,{?MODULE,uvmap_done,QuitOp,Uvs}}, St0) ->
    #uvstate{areas=Current,sel=Sel,matname=MatName0,orig_we=OrWe} = Uvs,
    Charts = add_areas(Sel, Current),
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
	false -> create_autouv_window(Id, St)
    end,
    start_uvmap_1(T, St);
start_uvmap_1([], _) -> keep.

create_autouv_window(Id, #st{shapes=Shs}=St) ->
    #we{name=ObjName} = We = gb_trees:get(Id, Shs),
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

auv_event({init_uvmapping,#we{mode=Mode}=We}, St) ->
    wings:init_opengl(St),
    case Mode of
	uv -> start_edit(We, St);
	_ -> auv_seg_ui:start(We, We, St)
    end;
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
%%% Edit interface.
%%%

start_edit(We, St0) ->
    DefVar = {answer,edit},
    Qs = [{vframe,[{alt,DefVar,"Edit existing UV mapping",edit},
		   {alt,DefVar,"Discard existing UV mapping and start over",discard}],
	   [{title,"Model is already UV-mapped"}]}],
    This = wings_wm:this(),
    Ask = fun([edit]) ->
		  start_edit_1(This, We, St0);
	     ([discard]) ->
		  St = discard_uvmap(We, St0),
		  wings_wm:send(This, {discard_uvs,We#we.id,St}),
		  ignore
	  end,
    wings_ask:dialog("Model Is Already UV-Mapped", Qs, Ask).

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
    DefVar = {answer,First},
    Qs = [{vframe,
	   [{alt,DefVar,"Material "++atom_to_list(M),M} || {M,_} <- Ms]}],
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

create_uv_state(Edges, Map, MatName, Options, #we{id=Id}=We, St) ->
    {_,Geom} = init_drawarea(),
    Uvs = #uvstate{st=wings_select_faces([], Id, St),
		   origst=St,
		   id=Id,
		   areas=Map,
		   geom=Geom,
		   orig_we=We,
		   edges=Edges,
		   matname=MatName,
		   option=Options},
    Name = wings_wm:this(),
    wings_wm:set_prop(Name, drag_filter, fun drag_filter/1),
    get_event(Uvs).

find_boundary_edges([{Id,#ch{fs=Fs,we=We}=C}|Cs], Acc) ->
    Be = auv_util:outer_edges(Fs, We),
    find_boundary_edges(Cs, [{Id,C#ch{be=Be}}|Acc]);
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
    We#we{mode=uv}.

gen_uv_pos([#ch{fs=Fs,center={CX,CY},scale=Sc,we=We,vmap=Vmap}|T], Acc) ->
    Vpos0 = auv_util:moveAndScale(gb_trees:to_list(We#we.vp), CX, CY, Sc, []),
    VFace0 = wings_face:fold_faces(
	       fun(Face, V, _, _, A) ->
		       [{V,Face}|A]
	       end, [], Fs, We),
    VFace = sofs:relation(VFace0, [{vertex,face}]),
    Vpos = sofs:relation(Vpos0, [{vertex,uvinfo}]),
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
    Etab = lists:map(fun(Rec = {_,#edge{a={_,_},b={_,_}}}) -> Rec;
		       ({Id,E=#edge{a={_,_,_},b={_,_}}}) -> {Id,E#edge{a ={0.0,0.0}}};
		       ({Id,E=#edge{a={_,_},b={_,_,_}}}) -> {Id,E#edge{b ={0.0,0.0}}};
		       ({Id,E=#edge{a={_,_,_},b={_,_,_}}}) -> 
			    {Id,E#edge{a={0.0,0.0},b={0.0,0.0}}}
		    end, gb_trees:to_list(Etab0)),
    We0#we{es=gb_trees:from_orddict(Etab)}.

insert_material(Cs, MatName, We) ->
    Faces = lists:append([Fs || #ch{fs=Fs} <- Cs]),
    wings_material:assign(MatName, Faces, We).

init_edit(MatName, Faces, We0) ->
    FvUvMap = auv_segment:fv_to_uv_map(Faces, We0),
    {Charts1,Cuts} = auv_segment:uv_to_charts(Faces, FvUvMap, We0),
    Charts = auv_segment:cut_model(Charts1, Cuts, We0),
    Map1 = build_map(Charts, FvUvMap, 1, []),
    Map3 = find_boundary_edges(Map1, []),
    Map  = gb_trees:from_orddict(Map3),
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
    UVs1 = lists:usort(UVs0),
    %% Assertion.
    true = sofs:is_a_function(sofs:relation(UVs1, [{atom,atom}])),
    {{_,BX0},{_,BX1},{_,BY0},{_,BY1}} = auv_util:maxmin(UVs0),
    CX = BX0 + (BX1-BX0) / 2,
    CY = BY0 + (BY1-BY0) / 2,
    UVs = [{V,{X-CX,Y-CY,0.0}} || {V,{X,Y}} <- UVs1],
    We = We0#we{id=No,vp=gb_trees:from_orddict(UVs)},
    Chart = #ch{we=We,fs=Fs,center={CX,CY},size={BX1-BX0,BY1-BY0},vmap=Vmap},
    build_map(T, FvUvMap, No+1, [{No,Chart}|Acc]);
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
    {X,Y,_,H} = wings_wm:viewport(geom),
    {W0,TopH} = wings_wm:top_size(),
    W = W0 div 2,
    {{X+W,TopH-Y-H,W,H}, wingeom(W,H)}.
    
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
    DrawArea = fun({_,A}) ->
		       draw_area(A, Options, Materials)
	       end,
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    ?SLOW(foreach(DrawArea, gb_trees:to_list(As))),
    gl:endList(),
    draw_texture(Uvs#uvstate{dl=Dl});
draw_texture(Uvs = #uvstate{dl=DL, sel=Sel}) ->
    gl:enable(?GL_DEPTH_TEST),
    gl:callList(DL),
    case Sel of 
	[] -> ignore;
	_ -> %% Draw selections slightly blended
	    {R,G,B} = wings_pref:get_value(selected_color),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:enable(?GL_BLEND),
	    gl:translatef(0.0,0.0,0.1),
	    DrawArea = fun({_,A}) -> 
			       draw_area(A, #setng{color = {R,G,B,0.7}, 
						   edges = all_edges}, []) 
		       end,
	    lists:foreach(DrawArea, Sel),
	    gl:disable(?GL_BLEND)
    end,
    Uvs.

setup_view({Left,Right,Bottom,Top}, Uvs) ->
    #uvstate{st=#st{mat=Mats},option=#setng{texbg=TexBg},
	     matname=MatN} = Uvs,
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
	true when MatN /= none ->
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

get_texture(#uvstate{option=#setng{texsz={TexW,TexH}},sel=Sel,areas=As}=Uvs0) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    Current = wings_wm:viewport(),
    {W0,H0} = wings_wm:top_size(),
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
    ?DBG("Get texture sz ~p ~p ~n", [{W,Wd},{H,Hd}]),
    set_viewport({0,0,W,H}),
    Uvs = reset_dl(Uvs0#uvstate{sel=[],areas=add_areas(Sel, As)}),
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

redraw(#uvstate{mode=Mode,geom=Geom}=Uvs0) ->
    Text = [atom_to_list(Mode)," mode: ",
	    "[L] Select [R] Show menu"],
    wings_wm:message(Text),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    setup_view(Geom, Uvs0),
    Uvs = draw_texture(Uvs0),
    gl:popAttrib(),
    Uvs.

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
	    separator,
	    {"Rescale All", rescale_all, "Pack the space in lower-left before rescaling"},
	    separator,
	    {"ReMap UV", {remap, [{"Project Normal", project, 
				   "Project UVs from chart normal"},
				  {"Unfold", lsqcm, " "},
				  separator,
				  {"Stretch optimization", stretch_opt, 
				   "Optimize the chart stretch"}
				 ]}, 
	     "Re-calculate new uv's with choosen algorithmen"}
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
     {"Apply Texture", apply_texture, "Attach the current texture to the model"},
     separator,
     {"Quit", quit, "Quit AutoUv-mapper"}].

edge_option_menu(#uvstate{option = Option}) ->
    DefVar = {edge_mode, Option#setng.edges},
    [MaxTxs0|_] = gl:getIntegerv(?GL_MAX_TEXTURE_SIZE),
    MaxTxs = min([4096,MaxTxs0]),
    
    Qs = [{vframe,[{alt,DefVar,"Draw All Edges",    all_edges},
		   {alt,DefVar,"Draw Border Edges", border_edges},
		   {alt,DefVar,"Don't Draw Edges",  no_edges}],
	   [{title,"Edge Options"}]},
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

-record(op, {name, prev, add, undo}).

handle_event(redraw, Uvs0) ->
    %%    ?DBG("redraw event\n"),
    Uvs = redraw(Uvs0),
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
handle_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_LEFT,
			  x=MX,y=MY}, 
	     #uvstate{op=#op{name=fmove,add={X,Y}}} = Uvs0) 
  when X /= MX; Y /= MY ->
    % fmove, not nowhere
    get_event(Uvs0#uvstate{op=undefined});
handle_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_LEFT,
			  x=MX,y=MY}, 
	     #uvstate{geom=ViewP,
		      mode=Mode,
		      op=Op,
		      id=Id,
		      sel=Sel0,
		      areas=Curr0} = Uvs0)
  when Op == undefined; 
       record(Op, op), Op#op.name == fmove ->
    % Deselection
    {_,_,_,OH} = wings_wm:viewport(),
    SX = MX,
    SY = OH-MY,
    case select(Mode, SX, SY, add_areas(Sel0,Curr0), ViewP) of
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
	    WingsSt = wings_select_faces(Sel1, Id, Uvs0#uvstate.st),
	    wings_wm:send(geom, {new_state,WingsSt}),
	    get_event(reset_dl(Uvs0#uvstate{sel = Sel1,
					    st=WingsSt,
					    areas=Curr1,
					    op = undefined}))
    end;
handle_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_LEFT,x=MX,y=MY}, 
	     #uvstate{geom=ViewP,
		      mode = Mode,
		      op = Op,
		      id = Id,
		      sel = Sel0,
		      areas=Curr0}=Uvs0) ->
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
	    case select_1(Mode, CX,(OH-CY), BW,BH, gb_trees:to_list(Curr0),
			Curr0, ViewP) of
		none -> 
		    get_event(Uvs0#uvstate{op = undefined});
		Hits -> 
		    {Sel1,Curr1} = update_selection(Hits, Sel0, Curr0),
		    WingsSt = wings_select_faces(Sel1, Id, Uvs0#uvstate.st),
		    wings_wm:send(geom, {new_state,WingsSt}),
		    Uvs = Uvs0#uvstate{op=undefined,
				       sel=Sel1,
				       st=WingsSt,
				       areas=Curr1},
		    get_event(reset_dl(Uvs))
	    end;
	rotate ->
	    Sel = [finish_rotate(A)|| A <- Sel0],
	    get_event(Uvs0#uvstate{op=undefined,sel=Sel});
	_ ->
	    get_event(Uvs0#uvstate{op = undefined})
    end;

handle_event(#mousebutton{state=?SDL_PRESSED,button=?SDL_BUTTON_LEFT,x=MX,y=MY}, 
	     #uvstate{geom=ViewP,
		      mode=Mode,
		      op=Op,
		      sel=Sel0,
		      areas=Curr0}=Uvs0) 
  when Op == undefined ->
    {_,_,_,OH} = wings_wm:viewport(),
    case select(Mode, MX, (OH-MY), add_areas(Sel0, Curr0), ViewP) of
	none -> 
	    get_event(Uvs0#uvstate{op=#op{name=boxsel, add={MX,MY}, 
					  prev={MX+1,MY+1},undo=Uvs0}});
	Hits ->
	    case Hits -- Sel0 of  
		Hits -> 
		    keep;
		_ -> %% Hit atleast one of the selected
		    get_event(Uvs0#uvstate{op=#op{name=fmove, add={MX,MY},
						  prev={MX,MY}, undo=Uvs0}})
	    end
    end;
handle_event(#keyboard{state=?SDL_PRESSED,sym=Sym},
	     #uvstate{st=St,id=Id}) ->
    case Sym of
	?SDLK_SPACE ->
	    wings_wm:send(geom, {new_state,wings_select_faces([],Id,St)}),
	    keep;
	_ -> keep
    end;
handle_event({drop,_,DropData}, Uvs) ->
    handle_drop(DropData, Uvs);
handle_event({action,{auv,apply_texture}},
	     #uvstate{st=St0,sel=Sel0,areas=As0,
		      orig_we=OWe,matname=MatName0}=Uvs) ->
    Tx = ?SLOW(get_texture(Uvs)),
    As = add_areas(Sel0, As0),
    #we{name = Name, id=Id} = OWe,
    {St1,MatName} = add_material(Tx,Name,MatName0,St0),
    St = insert_uvcoords(As, Id, MatName, St1),
    wings_wm:send(geom, {new_state,St}),
    get_event(Uvs#uvstate{st=St,matname=MatName});
handle_event({action, {auv, edge_options}}, Uvs) ->
    edge_option_menu(Uvs);
handle_event({action,{auv,quit}}, Uvs) ->
    quit_menu(Uvs);
handle_event(close, Uvs) ->
    restore_wings_window(Uvs),
    delete;
handle_event({action,{auv,quit,cancel}}, Uvs) ->
    restore_wings_window(Uvs),
    delete;
handle_event({action, {auv,quit,QuitOp}}, Uvs) ->
    restore_wings_window(Uvs),
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
			       texsz = {TexSz,TexSz}}
		       },
    get_event(reset_dl(Uvs1));

handle_event({action, {auv, {remap, Method}}}, 
	     Uvs0 = #uvstate{sel = Sel0, orig_we = We}) ->
    Sel = ?SLOW([remap(Chart, Method, We) || Chart <- Sel0]),
    get_event(Uvs0#uvstate{sel = Sel});

handle_event({action, {auv, rescale_all}},
	     Uvs0=#uvstate{sel = Sel0,areas=Curr0})->
    RscAreas = rescale_all(add_areas(Sel0,Curr0)),
    get_event(reset_dl(Uvs0#uvstate{sel = [],
				    areas=RscAreas}));
handle_event({action, {auv, {rotate, free}}}, Uvs) ->
    handle_event({action, {auv, rotate}}, Uvs);
handle_event({action, {auv, {rotate, Deg}}},
	     Uvs0=#uvstate{mode=Mode,sel=Sel0}) ->
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
    get_event(Uvs);
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
handle_event(init_opengl, Uvs0) ->
    {_,_,W,H} = wings_wm:viewport(wings_wm:this()),
    Geom = wingeom(W,H),
    get_event(reset_dl(Uvs0#uvstate{geom=Geom}));
handle_event(resized, Uvs0) ->
    {_,_,W,H} = wings_wm:viewport(wings_wm:this()),
    Geom = wingeom(W,H),
    get_event(reset_dl(Uvs0#uvstate{geom=Geom}));

handle_event({action,wings,{view, Cmd}}, Uvs0) ->
    St = wings_view:command(Cmd, Uvs0#uvstate.st),
    get_event(Uvs0#uvstate{st=St});
handle_event({current_state,geom_display_lists,St}, Uvs) ->
    case verify_state(St, Uvs) of
	keep -> update_selection(St, Uvs);
	Other -> Other
    end;
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

remap({Id, Ch0=#ch{we=We0,vmap=Vmap}}, stretch_opt, #we{vp=Orig}) ->
    Vs3d = map(fun({V0,_Pos}) ->
		       case gb_trees:lookup(V0, Vmap) of
			   none -> 
			       {V0, gb_trees:get(V0, Orig)};
			   {value,V} ->
			       {V0, gb_trees:get(V, Orig)}
		       end 
	       end, gb_trees:to_list(We0#we.vp)),
    Chart = auv_mapping:stretch_opt(Ch0, gb_trees:from_orddict(Vs3d)),
    {Id,Chart};

remap({Id, Chart0 = #ch{fs=Fs,we=We0,vmap=Vmap,size={W,H}}}, Type, #we{vp=Vs3d0}) ->
    %% Get 3d positions (even for mapped vs)
    Vs3d = map(fun({V0,_Pos}) ->
		       case gb_trees:lookup(V0, Vmap) of
			   none -> 
			       {V0, gb_trees:get(V0, Vs3d0)};
			   {value,V} ->
			       {V0, gb_trees:get(V, Vs3d0)}
		       end 
	       end, gb_trees:to_list(We0#we.vp)),
    Vs0 = auv_mapping:map_chart(Type, Fs, 
				We0#we{vp=gb_trees:from_orddict(Vs3d)}),
    We1 = We0#we{vp=gb_trees:from_orddict(sort(Vs0))},    
    {{Dx,Dy}, Vs} = auv_placement:center_rotate(Fs, We1),
    Scale = if Dx > Dy -> W / Dx;
	       true -> H / Dy
	    end,
    We = We0#we{vp=gb_trees:from_orddict(sort(Vs))},
    {Id, Chart0#ch{we=We, size={Dx*Scale, Dy*Scale}, scale=Scale}}.

drag_filter({image,_,_}) ->
    {yes,"Drop: Change the texture image"};
drag_filter(_) -> no.

handle_drop({image,_,#e3d_image{width=W,height=H}=Im}, #uvstate{option=Opt0}=Uvs) ->
    case W =:= H andalso is_power_of_two(W) of
	false -> keep;
	true ->
	    Opt = Opt0#setng{color=false,edges=no_edges},    
	    add_texture_image(Im, default, Uvs#uvstate{option=Opt})
    end;
handle_drop(_DropData, _) ->
    %%io:format("~P\n", [_DropData,40]),
    keep.

add_texture_image(Im, FileName,#uvstate{st=St0,option=Opt,
					orig_we=OWe,matname=MatName0}=Uvs) ->
    Name = OWe#we.name,
    {St,MatName} = add_material(Im,Name,MatName0,St0),
    wings_wm:send(geom, {new_state,St}),
    get_event(reset_dl(Uvs#uvstate{st=St,
				   matname=MatName,
				   option=Opt#setng{texbg=true},
				   last_file=FileName})).

is_power_of_two(X) ->
    (X band -X ) == X.

update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #uvstate{st=#st{selmode=Mode,sel=Sel}}=Uvs) ->
    get_event_nodraw(Uvs#uvstate{st=St});
update_selection(#st{selmode=Mode,sel=Sel}=St,
		 #uvstate{areas=As,orig_we=#we{id=Id},sel=ChSel}=Uvs) ->
    case keysearch(Id, 1, Sel) of
	false ->
	    get_event(reset_dl(Uvs#uvstate{st=St,sel=[],
					   areas=add_areas(ChSel, As)}));
	{value,{Id,Elems}} ->
	    update_selection_1(Mode, gb_sets:to_list(Elems), Uvs#uvstate{st=St})
    end.

update_selection_1(face, Faces, #uvstate{sel=Sel,areas=As0}=Uvs) ->
    As = gb_trees:to_list(add_areas(Sel, As0)),
    update_selection_2(As, Faces, Uvs, [], []);
update_selection_1(_, _, Uvs) ->
    get_event_nodraw(Uvs).

update_selection_2([{K,#ch{fs=Fs}=C}|Cs],Faces,Uvs,NonSel,Sel) ->
    case ordsets:intersection(sort(Fs), Faces) of
	[] -> update_selection_2(Cs, Faces, Uvs, [{K,C}|NonSel], Sel);
	_ -> update_selection_2(Cs, Faces, Uvs, NonSel, [{[K],C}|Sel])
    end;
update_selection_2([], _, Uvs0, NonSel, Sel) ->
    As = gb_trees:from_orddict(sort(NonSel)),
    Uvs = Uvs0#uvstate{sel=sort(Sel),areas=As},
    get_event(reset_dl(Uvs)).

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

select(Mode, X,Y, Objects, {XYS,XM,XYS,YM}=ViewP) ->
    {_,_,UVW,UVH} = wings_wm:viewport(),
    XT = (XM-XYS)*X/UVW+XYS,
    YT = (YM-XYS)*Y/UVH+XYS,
    case find_selectable(XT,YT, gb_trees:to_list(Objects), []) of
	[] -> none;
	Possible -> select_1(Mode, X,Y, 3,3, Possible, Objects, ViewP)
    end.

select_1(Mode, X,Y, W, H, Possible, All, {XYS,XM,XYS,YM}) ->
    {_,_,UVW,UVH} = wings_wm:viewport(),
    HitBuf = get(wings_hitbuf),
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    gl:viewport(0,0,UVW,UVH),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:pickMatrix(float(X), float(Y), W,H, {0,0,UVW,UVH}),
    glu:ortho2D(XYS, XM, XYS, YM),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    select_draw(Possible, Mode),
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> 
	    none;
	NumHits ->
	    HitData = sdl_util:read(HitBuf, 5*NumHits),
	    Hits = get_hits(NumHits, HitData),
	    map(fun(Hit) -> 
			HitArea = gb_trees:get(Hit, All),
			{[Hit], HitArea}
		end, lists:usort(Hits))
    end.

select_draw([{Co,A}|R], Mode) ->
%%    ?DBG("Co ~p\n",[Co]),
    #ch{fs=Fs,center={CX,CY},scale=Scale,rotate=Rot,we=We} = A,
    gl:pushMatrix(),
    gl:pushName(0),
    gl:loadName(Co),
    gl:translatef(CX,CY,0.0),
    gl:scalef(Scale, Scale, 1.0),
    gl:rotatef(Rot,0,0,1),
    select_draw1(Mode, Fs, We#we{mode=material}),
    gl:popName(),
    gl:popMatrix(),
    select_draw(R, Mode);
select_draw([], _) ->
    ok.

select_draw1(faceg, Fs, We) ->
    draw_faces(Fs, We);
select_draw1(face, Fs, We) ->
    select_draw_faces(Fs,We, wings_pref:get_value(display_list_opt)),
    gl:edgeFlag(?GL_TRUE);
select_draw1(edge, Fs, #we{vp=Vtab}=We) ->
    DrawEdge = fun(_Face, _V, Edge, #edge{vs=Va,ve=Vb}, _) ->
		       gl:pushName(Edge),
		       gl:glBegin(?GL_LINES),
		       gl:vertex3fv(gb_trees:get(Va, Vtab)),
		       gl:vertex3fv(gb_trees:get(Vb, Vtab)),
		       gl:glEnd(),   
		       gl:popName(),
		       ok
	       end,
    wings_face:fold_faces(DrawEdge, ok, Fs, We);
select_draw1(vertex, Fs, #we{vp=Vtab}=We) ->
    DrawPoint = fun(_Face, V, _Edge, _Rec, _) ->
			gl:pushName(V),
			gl:glBegin(?GL_POINTS),
			gl:vertex3fv(gb_trees:get(V, Vtab)),
			gl:glEnd(),   
			gl:popName(),
			ok
		end,
    wings_face:fold_faces(DrawPoint, ok, Fs, We).

select_draw_faces([], _We, _) ->    ok;
select_draw_faces([H|R], We, false) ->
    gl:pushName(H),
    wings_draw_util:face(H, We),
    gl:popName(),
    select_draw_faces(R,We,false);
select_draw_faces([H|R], We, true) ->
    gl:pushName(H),
    gl:'begin'(?GL_TRIANGLES),
    wings_draw_util:face(H, We),
    gl:'end'(),
    gl:popName(),
    select_draw_faces(R,We,true).

get_hits(N, Buf) ->
    get_hits_1(N, Buf, []).

get_hits_1(0, _, Acc) -> Acc;
get_hits_1(N, [1,_,_,A|T], Acc) ->
    get_hits_1(N-1, T, [A|Acc]).

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
wings_select_faces(As, Id, St) ->
    Faces0 = foldl(fun({_,#ch{fs=Fs}}, A) ->
			   [Fs|A]
		   end, [], As),
    Faces = gb_sets:from_list(lists:append(Faces0)),
    wpa:sel_set(face, [{Id,Faces}], St).

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
    {Id,A#ch{scale = S+NS, size = {W/S*(S+NS), H/S*(S+NS)}}}.
rotate_area(faceg, {Id,A = #ch{rotate = R}}, DX, DY) ->
    NS = greatest(DX,DY),
    NewR = R + NS*180,
    {Id,A#ch{rotate = NewR}}.
transpose_x(faceg,{Id,A = #ch{we=We=#we{vp=Vpos}}}) ->
    New = [{Nr, {-X,Y,Z}} || {Nr, {X,Y,Z}} <- gb_trees:to_list(Vpos)],
    {Id,A#ch{we=We#we{vp = gb_trees:from_orddict(New)}}}.
transpose_y(faceg,{Id,A = #ch{we=We=#we{vp=Vpos}}}) ->
    New = [{Nr, {X,-Y,Z}} || {Nr, {X,Y,Z}} <- gb_trees:to_list(Vpos)],
    {Id,A#ch{we=We#we{vp = gb_trees:from_orddict(New)}}}.

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
finish_rotate({Id,Area = #ch{rotate = R, we=We=#we{vp=Vpos}, scale = S}}) ->
    Rot = e3d_mat:rotate(float(trunc(R)), {0.0,0.0,1.0}),
    Vs = [{IdV, e3d_mat:mul_point(Rot, Vec)} || 
	     {IdV, Vec} <- gb_trees:to_list(Vpos)],
    {{_,BX0},{_,BX1},{_,BY0},{_,BY1}} = auv_util:maxmin(Vs),
    {Id,Area#ch{rotate=0.0, we=We#we{vp=gb_trees:from_orddict(Vs)}, 
		size={(BX1-BX0)*S, (BY1-BY0)*S}}}.

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

same_topology(#st{shapes=Shs},
	      #uvstate{orig_we=#we{id=Id}=We,edges=Edges}) ->
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
draw_area(#ch{fs=Fs,center={CX,CY},scale=Scale,rotate=R,be=Tbe, we=We}, 
	  Options = #setng{color = ColorMode, edges = EdgeMode}, Materials) -> 
    gl:pushMatrix(),
    gl:translatef(CX, CY, 0),
    gl:scalef(Scale, Scale, 1),
    gl:rotatef(trunc(R), 0, 0, 1),
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

draw_faces(Fs, #we{mode=Mode}=We) ->
    Draw = case Mode of
	       material ->
		   fun(Face) -> wings_draw_util:flat_face(Face, We) end;
	       _ ->
		   fun(Face) -> wings_draw_util:face(Face, We) end
	   end,
    wings_draw_util:begin_end(fun() -> foreach(Draw, Fs) end).

reset_dl(Uvs = #uvstate{dl = undefined}) ->
    Uvs;
reset_dl(Uvs = #uvstate{dl = DL}) ->
    gl:deleteLists(DL, 1),
    Uvs#uvstate{dl = undefined}.

set_viewport({X,Y,W,H}=Viewport) ->
    put(wm_viewport, Viewport),
    gl:viewport(X, Y, W, H).

restore_wings_window(Uvs) ->
    reset_dl(Uvs).
