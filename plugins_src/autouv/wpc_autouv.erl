%%% File    : wpu_autouv.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : A simple semi Automatic UV-mapping plugin
%%%
%%% Created : 24 Jan 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002 Dan Gudmundsson 
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: wpc_autouv.erl,v 1.8 2002/10/10 13:04:10 dgud Exp $

-module(wpc_autouv).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
 
-compile(export_all). %% debug
-export([menu/2,command/2, outer_edges/2]).

-import(lists, [sort/1, map/2, foldl/3, reverse/1, 
		append/1,delete/2, usort/1, max/1, min/1]).

add_areas(New, Areas) ->
    As = Areas#areas.as,
    NewAs = ?add_as(New, As),
    Areas#areas{as = NewAs}.

init() ->
    true.

menu({body}, Menu0) ->
    case get(auv_state) of
	undefined ->
	    SubMenu1 = 
		[{"Create UV map", create, 
		  "Create a new texture mapping (removing old)"},
		 {"Edit UV map", edit, 
		  "Edit a previously created UV-map"}],
	    Menu0 ++ [separator,
		      {"UV-Mapping", {uvmap, SubMenu1},
		       "Generate or edit a UV-map or texture"}];
	_UvState ->
	    parameterization_menu(Menu0)
    end;
menu({What}, Menu0) 
  when What == edge; What == face; 
       What == vertex; What == shape -> 
%%    ?DBG("menu ~p ~p ~n", [What, Menu0]),
    case get(auv_state) of
	undefined ->
	    Menu0;
	_UvState ->
	    Menu1 = 
		case What of
		    edge ->
			[separator,
			 {"Chart Boundry",{hardness,
					   [{"Delete Boundry",soft},
					    {"Set Boundry",hard}]}}];
		    face ->
			{value, Mat0} = %% HACK :-) 
			    lists:keysearch("Set Material",1,Menu0), 
			Mat1 = setelement(1, Mat0, "Add to Chart"),
			Mat2 = {"Get Charts", chart_names, ""},
			[Mat1, Mat2, separator];
		    _ ->
			[]
		end,
	    Menu1 ++ parameterization_menu(Menu0)
    end;
menu(_, Menu0) ->
    Menu0.

parameterization_menu(_Menu) ->
    [{"UV-Mapping Continue", continue_param, 
      "Continue with uv-mapping parameterization"},
     {"UV-Mapping Cancel", uvmap_cancel, "Cancel all changes"}].

command({body, {uvmap, create}}, St0) ->
    DefVar = {seg_type,autouvmap},
    Qs = [{vframe,[{alt,DefVar,"Advanced Cubic",autouvmap},
		   {alt,DefVar,"By Feature Detection",feature},
		   {alt,DefVar,"By Material",mat_uvmap},
		   {alt,DefVar,"I'll segment the model myself", one}
		  ],
	   [{title,"Segmentation type"}]}],
    Text = "Set charts, place faces into charts",
    wings_ask:dialog(Qs,
		     fun([Mode]) ->
			     St1 = segment(Mode, St0),
			     put(auv_state, [{oldst, St0}]),
			     wings_io:message(Text),
			     St1
		     end);
command({_, uvmap_cancel}, _St0) ->
    [{oldst,S1}] = get(auv_state),
    erase(auv_state),
    S1;
command({_State, continue_param}, _St) ->
    DefVar = {seg_type,project},
    Qs = [{vframe,[{alt,DefVar,"Projection",project},
		   {alt,DefVar,"Parametrization", lsqcm}
		  ],
	   [{title,"UV-mapping Type"}]}],
    wings_ask:dialog(Qs,
		     fun([Mode]) ->
			     {uvmap, {parametrization, Mode}}
		     end);

command({uvmap, {parametrization, Mode}}, St0) ->
    %% Now copy the old selections so we map the correct bodies.
    [{oldst,Old}] = get(auv_state),
    init_uvmap(St0#st{selmode=Old#st.selmode, 
		      sel=Old#st.sel}, 
	       Old, Mode);
    
command({body, {uvmap, edit}}, St0) ->
    AllAreas = ?SLOW(wings_sel:fold(fun(_Sel, We, A) ->
	init_edit(We, A, St0)
	end, [], St0)),
    if
	AllAreas == [] ->
	    St0;
	true ->
	    Geom = init_drawarea(),
	    [Areas = #areas{matname = MatName}|Remain] = AllAreas,
	    TexSz = get_texture_size(MatName, St0#st.mat),
	    Sel = [{(Areas#areas.we)#we.id, gb_sets:singleton(0)}],
	    Uvs = #uvstate{command = edit, 
			   st = wpa:sel_set(body, Sel, St0), 
			   origst = St0,
			   areas = Areas, geom = Geom, 
			   rest_objects = Remain,
			   option = #setng{color = false, texbg = true, 
					   texsz = TexSz}},
	    {seq,{push,dummy}, get_event(Uvs)}
    end;
command({body,{uvmap_done,QuitOp,
	       Uvs = #uvstate{st=St0,areas=Current,sel=Sel}}}, _) ->
    Areas1 = add_areas(Sel,Current),
    St2 = case QuitOp of
	      quit_uv_tex ->
		  Tx = ?SLOW(get_texture(Uvs)),
		  {St1, Areas1} = add_material(edit, Tx, St0, Areas1),
		  St1;
	      quit_uv ->
		  St0
	  end,
    We = ?SLOW(insert_uvcoords(Areas1)),
    Shapes1 = gb_trees:update(We#we.id, We, St2#st.shapes),
    St3 = St2#st{shapes = Shapes1},
    case Uvs#uvstate.rest_objects of
	[] ->
	    %% Last done, reset to original
	    reset_view(),
	    erase(auv_state),
	    St3;
	[Next|Objects] ->
	    WSel = [{(Next#areas.we)#we.id, gb_sets:singleton(0)}],
	    NewUvs = Uvs#uvstate{areas = Next, rest_objects = Objects, 
				 sel = [], 
				 st = wpa:sel_set(body, WSel, St3), 
				 dl = undefined},
	    {seq,{push,dummy}, get_event(NewUvs)}
    end;

command({face,chart_names}, St) ->
    Mats = wings_sel:fold(
	     fun(F0, We, A) ->
		     Faces = gb_sets:to_list(F0),
		     GetMat = 
			 fun(Face) ->
				 #face{mat=Mat} = 
				     gb_trees:get(Face, We#we.fs),
				 {Mat, Face} 
			 end,
		     Mats0 = lists:map(GetMat, Faces),
		     Mats0 ++ A
	     end, [], St),
    Msg = lists:flatten(io_lib:format("Charts: ~p", [Mats])),
    wings_util:message(Msg, St);

command(_Cmd, _St) -> 
%%    ?DBG("Unsupport command ~p~n",[_Cmd]),
    next.

segment(Mode, St0) ->
    ?SLOW(wings_sel:fold(
	fun(_Sel, We, A) ->
	   {Charts, Bounds} = auv_segment:create(Mode, We),	
   	   mark_segments(Charts, Bounds, We, A)
	end, St0, St0)).

mark_segments(Charts, Bounds, We0, St) ->
    %% Use HardEdges to mark Boundries
    We1 = We0#we{he = gb_sets:from_list(Bounds)},
    %% Use materials to mark different charts
    Max = length(Charts),
    ColorMe = [create_diffuse(This, Max) || This <- Charts],
    NewMat = wings_material:default(),
    create_materials(ColorMe, We1, St#st{mat=NewMat}).

%% Create temp Materials 
create_materials([{MatName, Diff, Faces}|Distances], We, St0) ->
    Black = {0.0,0.0,0.0,1.0},
    Mat = {MatName, [{opengl, [{diffuse, Diff}, 
			       {specular, Black}, 
			       {ambient, Diff},
			       {shininess,1.0}]}]},
    {St1, []} = wings_material:add_materials([Mat], St0),
%%    ?DBG("Created ~p ~p ~n", [MatName, _Prev]),
    We1 = lists:foldl(fun(Face, WeX) -> 
			      set_material(Face, MatName, WeX) 
		      end, We, Faces),		
    create_materials(Distances, We1, St1);
create_materials([], We, St0) ->
    Shapes = gb_trees:update(We#we.id, We, St0#st.shapes),
    St0#st{shapes = Shapes}.

set_material(Face, MatName, We= #we{fs = Fs}) ->
    F = gb_trees:get(Face, Fs),
    Fs1 = gb_trees:update(Face, F#face{mat=MatName}, Fs), 
    We#we{fs = Fs1}.

create_diffuse({Index0, Fs}, Max) ->
%    ?DBG("~p ~p ~n", [Index0, Max]),
    {ColorI, Diff} = 
	if Max =< 6 ->
		{Index0 div 6, 0.0};
	   true ->
		MaxPerColor = Max div 6,
		CI = Index0 div MaxPerColor,
		Col = (CI+1) / (MaxPerColor+2),
		{CI, Col}
	end,
%    ?DBG("~p of ~p => ~p = ~p in ~p ~n", 
%          [Index0, Max, ColorI, Diff, Index0 rem 6]),
    case Index0 rem 6 of
	0 ->
	    Color = {1.0, Diff, Diff, 1.0},	    	   
	    MatName = list_to_atom("red_" ++ integer_to_list(ColorI)),
	    {MatName, Color, Fs};
	1 ->
	    Color = {Diff, 1.0, Diff, 1.0},
	    MatName = list_to_atom("green_" ++ integer_to_list(ColorI)),
	    {MatName, Color, Fs};
	2 ->
	    Color = {Diff, Diff, 1.0, 1.0},	    	   
	    MatName = list_to_atom("blue_" ++ integer_to_list(ColorI)),
	    {MatName, Color, Fs};
	3 ->
	    Color = {1.0, 1.0, Diff, 1.0},	    	   
	    MatName = list_to_atom("yellow_" ++ integer_to_list(ColorI)),
	    {MatName, Color, Fs};
	4 ->
	    Color = {1.0, Diff, 1.0, 1.0},
	    MatName = list_to_atom("purple_" ++ integer_to_list(ColorI)),
	    {MatName, Color, Fs};
	5 ->
	    Color = {Diff, 1.0, 1.0, 1.0},
	    MatName =list_to_atom("turquoise_"++integer_to_list(ColorI)),
	     {MatName, Color, Fs}
    end.

%%%%%%

init_uvmap(St0, Old, Type) ->    
    {AllAreas, St1} = ?SLOW(wings_sel:fold(fun(_Sel, We, A) ->
	init_uvmap2(We, A, Type)
	end, {[], Old}, St0)),
    Geom = init_drawarea(),
    [Areas|Remain] = AllAreas,
    Sel = [{(Areas#areas.we)#we.id, gb_sets:singleton(0)}],
    Uvs = #uvstate{command = create_mat, st=wpa:sel_set(body, Sel, St1), 
		   origst = Old,
		   areas = Areas, 
		   rest_objects = Remain, geom = Geom},
    {seq,{push,dummy}, get_event(Uvs)}.

init_uvmap2(We0 = #we{id=Id,name = Name}, {A, St0}, Type) ->
    Clusters = auv_segment:segment_by_material(We0),
    We1 = gb_trees:get(Id, St0#st.shapes),
    ?DBG("Found ~p Charts~n", [length(Clusters)]),
    Areas = init_areas(Clusters, [], Type, We1),

    %% Place the cluster on the texturemap
    Map = auv_placement:place_areas(Areas),
    %%    ?DBG("AUV Maps ~p\n", [Map]),
    As0 = #areas{we = We1, as = Map, 
		 matname = list_to_atom(Name ++ "_auv")},
    {St1, As1} = add_material(create_mat, none, St0, As0),
    {[As1|A], St1}.

init_areas([Chart|R], A, Type, We) ->
    MappedVs = 
	case Type of 
	    project -> 
		auv_mapping:projectFromChartNormal(Chart, We);
	    lsqcm ->
		auv_mapping:lsqcm(Chart, We)
	end,
    New = create_area(Chart, MappedVs),
    init_areas(R, [New|A], Type, We);
init_areas([], A, _Type, _We) ->
    A.
   
create_area({_,Fs}, Vs0) ->
    [{_, {X,Y,_}} |RVs1] = Vs0,
    {BX0, BX1, BY0, BY1} =
	foldl(fun({_, Pos}, Ac) -> maxmin(Pos, Ac) end, {X,X,Y,Y}, RVs1),
    CX = BX0 + (BX1-BX0) / 2,
    CY = BY0 + (BY1-BY0) / 2,
    Vs3 = moveAndScale(Vs0, -CX, -CY, 1, []),
    #a{fs = Fs, vpos = Vs3, size = {BX1-BX0, BY1 -BY0}}.  

insert_uvcoords(#areas{we=We0, as = Map0, matname = MatName}) ->
    Map1 = [A#a{vpos = moveAndScale(Vtxs, CX, CY, S, [])} ||
	       {_, A = #a{center = {CX,CY}, scale = S, vpos = Vtxs}} 
		   <- gb_trees:to_list(Map0)],
    %% Insert UV coords to WE structure
    We1 = gen_coords(Map1, We0),
    Ftab1 = [{Face,Rec#face{mat=MatName}} ||
		{Face,Rec} <- gb_trees:to_list(We0#we.fs)],
    Ftab = gb_trees:from_orddict(Ftab1),
    We = We1#we{mode=uv,fs=Ftab},
    %% Done
    We.

init_edit(#we{name = Name, mode = Mode}, Acc, _St) when Mode /= uv ->
    wpa:error("Error: " ++ Name ++ " doesn't contain uv coords"),
    Acc;

init_edit(We = #we{fs = Ftab0}, Acc, St0) ->
    MatNames0 = find_mats(gb_trees:next(gb_trees:iterator(Ftab0)), []),
    MatNames1 = [Mat || Mat = {MatName,_,_} <- MatNames0,  %% Filter materials with textures
			has_texture(MatName, St0#st.mat)],    
    [{MatName, Faces, _}|_] = MatNames1,
    Ftab1 = foldl(fun(Face, FtabX) -> gb_trees:delete(Face, FtabX) end,
		  Ftab0, gb_trees:keys(Ftab0) -- Faces),
    Clusters = get_groups(Ftab1, We, []),
    BBox = fun({_, {U,V}}, MaxMin) ->
		   maxmin({U,V,0}, MaxMin)
	   end,
    Create = fun({FS, UVs},Count) ->
		     {_, {U0,V0}} = hd(UVs),
		     {BX0, BX1, BY0, BY1} = lists:foldl(BBox, {U0,U0,V0,V0}, UVs),
		     CX = BX0 + (BX1-BX0) / 2,
		     CY = BY0 + (BY1-BY0) / 2,
		     %%       ?DBG("Edit Data ~p ~p ~p\n", [BB, {CX,CY}, UVs]),
		     Center = fun({Id, {X,Y}}) -> {Id,{X - CX, Y - CY, 0.0}} end,
		     UVs1 = map(Center, UVs),
		     {{Count, #a{fs = FS, vpos = UVs1, center = {CX,CY},
				 size = {(BX1-BX0), (BY1 -BY0)}}}, Count+1}
	     end,
    {Map0,_Count} = lists:mapfoldl(Create, 1, Clusters),
    ?DBG("Edit UV ~p \n", [MatName]),
    Map = gb_trees:from_orddict(Map0),
    [#areas{we=We, as=Map, matname=MatName}|Acc].

%%%%% Material handling

has_texture(MatName, Materials) ->
    Mat = gb_trees:get(MatName, Materials),
    Maps = proplists:get_value(maps,Mat,[]),
    none /= proplists:get_value(diffuse, Maps, none).
get_texture_size(MatName, Materials) ->
    Mat = gb_trees:get(MatName, Materials),
    Maps = proplists:get_value(maps,Mat,[]),
    case proplists:get_value(diffuse, Maps, none) of
	none -> {512, 512};
	{W,H,_} -> {W,H}
    end.	     

textureId(MatName, _Materials) ->
    gb_trees:get(MatName, get(wings_material)).
get_material(Face, Materials, We) ->
    MatName = (gb_trees:get(Face, We#we.fs))#face.mat,
    Mat = gb_trees:get(MatName, Materials),
    proplists:get_value(diffuse, proplists:get_value(opengl, Mat)).

add_material(create_mat, none, St0, Areas = #areas{matname = MatName}) ->
    Mat = {MatName, [{opengl, []},{maps, []}]},
    case wings_material:add_materials([Mat], St0) of
	{St1, []} ->
	    {St1, Areas};
	{St1, [{MatName,NewName}]} ->
	    {St1, Areas#areas{matname = NewName}}
    end;
add_material(edit, Tx = {TxW,TxH,TxBin}, St0, As = #areas{matname = MatName}) ->
    Mats = St0#st.mat,
    Mat = gb_trees:get(MatName, Mats),
    Maps = proplists:get_value(maps, Mat),
    case proplists:get_value(diffuse, Maps, none) of
	none -> 
	    [TxId] = gl:genTextures(1),
	    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
	    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
	    gl:enable(?GL_TEXTURE_2D),
	    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
	    gl:bindTexture(?GL_TEXTURE_2D, TxId),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER,
			     ?GL_LINEAR),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER,
			     ?GL_LINEAR),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
	    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
			  TxW, TxH, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, TxBin),
	    gl:popAttrib(),
	    TxDict0 = get(wings_material),
	    TxDict = gb_trees:enter(MatName, TxId, TxDict0),
	    put(wings_material, TxDict);
	_Else ->
	    TxId = textureId(MatName, Mats),
	    gl:bindTexture(?GL_TEXTURE_2D, TxId),
	    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
			  TxW, TxH, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, TxBin)
    end,
    Maps0 = lists:keydelete(diffuse,1,Maps),
    NewMaps = [{diffuse, Tx}|Maps0],
    NewMat = lists:keyreplace(maps, 1, Mat, {maps, NewMaps}),
    NewMats = gb_trees:update(MatName, NewMat, Mats),
    
    {St0#st{mat = NewMats}, As}.

find_mats(none, Acc) ->
    S = sort(Acc),
    ClusterFunc = fun({CType, Face}, []) ->
			  [{CType, [Face], 1}];
		     ({CType, Face}, [{CType, List, C}|R]) ->
			  [{CType, [Face|List], C+1}|R];
		     ({CType, Face}, List) ->
			  [{CType, [Face], 1}|List]
		  end,
    Clustered = lists:foldl(ClusterFunc, [], S),
    reverse(lists:keysort(3, Clustered));
find_mats({Key, #face{mat = Mat}, Next}, Acc) ->
    find_mats(gb_trees:next(Next), [{Mat, Key}|Acc]).

get_groups(Ftab0, We, Acc) ->
    case gb_trees:is_empty(Ftab0) of
	true -> Acc;
	false ->
	    {F1, _FRec, Ftab1} = gb_trees:take_smallest(Ftab0),
	    {Faces, UVs, Ftab2} = get_group([F1], Ftab1, We, [], []),
	    get_groups(Ftab2, We, [{Faces, UVs}|Acc])
    end.

get_group([Face|Rest], Ftab1, We = #we{es = Es}, AF, AUV) ->    
    Get = fun(W,_E,ER,Acc) -> get_group2(W,ER,Acc,Es) end,
    {Fs, UVs, Ftab2} = wings_face:fold(Get, {[], AUV, Ftab1}, Face, We),
    get_group(Rest ++ Fs, Ftab2, We, Fs ++ [Face|AF], UVs);
get_group([], Ftab, _We, AF, AUV) ->
    {lists:usort(AF), lists:usort(AUV), Ftab}.

get_group2(VO, #edge{lf=F2,vs=VO,ve=V,b=F1Uv,ltpr=Eid2}, {Fs,Vs,Ftab0}, Etab) ->
    case gb_trees:is_defined(F2, Ftab0) of
	false ->
	    {Fs, [{V, F1Uv}|Vs], Ftab0};
	true ->
	    case gb_trees:get(Eid2, Etab) of
		#edge{rf=F2,ve=V,b=F1Uv} ->
		    {[F2|Fs], [{V, F1Uv}|Vs], gb_trees:delete(F2, Ftab0)};
		#edge{lf=F2,vs=V,a=F1Uv} ->
		    {[F2|Fs], [{V, F1Uv}|Vs], gb_trees:delete(F2, Ftab0)};
		_Edge ->
		    {Fs, [{V, F1Uv}|Vs], Ftab0}
	    end
    end;
get_group2(VO, #edge{rf=F2,vs=V,ve=VO,a=F1Uv,rtpr=Eid2}, {Fs,Vs,Ftab0},Etab) ->
    case gb_trees:is_defined(F2, Ftab0) of
	false ->
	    {Fs, [{V, F1Uv}|Vs], Ftab0};
	true ->
	    case gb_trees:get(Eid2, Etab) of
		#edge{rf=F2,ve=V,b=F1Uv} ->
		    {[F2|Fs], [{V, F1Uv}|Vs], gb_trees:delete(F2, Ftab0)};
		#edge{lf=F2,vs=V,a=F1Uv} ->
		    {[F2|Fs], [{V, F1Uv}|Vs], gb_trees:delete(F2, Ftab0)};
		_Edge ->
		    {Fs, [{V, F1Uv}|Vs], Ftab0}
	    end
    end.

moveAndScale([{Id, {X0, Y0,_}}|R], XD, YD, Scale, Acc) ->
    moveAndScale(R, XD,YD, Scale, 
		 [{Id, {X0*Scale+XD,Y0*Scale+YD,0.0}}|Acc]);
moveAndScale([],_,_,_,Acc) ->
    Acc.

maxmin({X,Y,_}, {XMin, XMax, YMin, YMax}) ->
    if 	X > XMax ->
	    if Y > YMax -> {XMin, X, YMin, Y};
	       Y < YMin -> {XMin, X, Y, YMax};
	       true -> {XMin, X, YMin, YMax}
	    end;
	X < XMin ->
	    if Y > YMax -> {X, XMax, YMin, Y};
	       Y < YMin -> {X, XMax, Y, YMax};
	       true -> {X, XMax, YMin, YMax}
	    end;
	Y > YMax ->
	    {XMin, XMax, YMin, Y};
	Y < YMin ->
	    {XMin, XMax, Y, YMax};
	true ->
	    {XMin, XMax, YMin, YMax}
    end.

%%% UV-coords map handling

gen_coords([#a{fs=Fs,vpos=Vpos0}|T], #we{es=Etab0}=We) ->
    VFace0 = wings_face:fold_faces(
	       fun(Face, V, _, _, A) ->
		       [{V,Face}|A]
	       end, [], Fs, We),
    VFace1 = sofs:relation(VFace0, [{vertex,face}]),
    Vpos1 = sofs:relation(Vpos0, [{vertex,uvinfo}]),
    Comb0 = sofs:relative_product({VFace1,Vpos1}),
    Comb = sofs:to_external(Comb0),
    Etab = insert_coords(Comb, We, Etab0),
    gen_coords(T, We#we{es=Etab});
gen_coords([], We) -> We.

insert_coords([{V,{Face,{S,T,_}}}|Rest], We, Etab0) ->
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
    insert_coords(Rest, We, Etab);
insert_coords([], _We, Etab) -> Etab.

%%% Opengl drawing routines

init_drawarea() ->
    [0,0,OW,OH] = gl:getIntegerv(?GL_VIEWPORT),    
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    HW = (OW - 4) div 2,
    {X2, W2} = {OW - HW, HW},
    %% Estimate Icon and message height
    %% Hard coded yes, but what do I do ??
    EH1 = 72,  %% Icons and message area height
    EH2 = 25,  %% Menu bar height
    {Y2, H2} = {EH1, OH - EH2 - EH1},
    Border = 10, %% Showed no pixels around the texturemap
    
    {X0Y0, XMax, YMax} =
 	if 
	    W2 > H2 -> 
		WF = Border / W2,
		{-WF, W2/H2+WF, 1+WF};
	    true -> 
		WF = Border / H2,
		{-WF, 1+WF, H2/W2+WF}
	    end,
    %%    {{0,0,HW,OH}, {X2,Y2,W3,H3}}.
    {{0,0,HW,OH}, {X2,Y2,W2,H2,X0Y0,XMax,YMax}}.
draw_texture(Uvs = #uvstate{dl = undefined, option = Options}) ->
    Materials = (Uvs#uvstate.origst)#st.mat,
    Areas = #areas{we = We, as = As0} = Uvs#uvstate.areas,
    DrawArea = fun({Id,A}) ->
		       {Id,draw_area(A, We, Options, Materials)}
	       end,
    %%    ?DBG("Rebuilding display list\n"),
    gl:newList(200, ?GL_COMPILE),
    As1 = ?SLOW(lists:map(DrawArea, gb_trees:to_list(As0))),
    gl:endList(),
    draw_texture(Uvs#uvstate{dl = 200, areas = Areas#areas{as = gb_trees:from_orddict(As1)}});
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

setup_view(Geom,Uvs) -> 
    setup_view(Geom, undefined,Uvs).

setup_view({X0,Y0,W,H,X0Y0,XM,YM}, Part, Uvs) ->
    #uvstate{st = #st{mat=Mats}, 
	     areas = #areas{matname = MatN}} = Uvs,
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),    
    gl:viewport(X0,Y0,W,H),
    gl:matrixMode(?GL_PROJECTION),    
    gl:loadIdentity(),
    case Part of
	undefined -> 
	    glu:ortho2D(X0Y0, XM, X0Y0, YM);
	{WD,HD,WC,HC} ->
	    glu:ortho2D(WC/WD, (1+WC)/WD, HC/HD, (1+HC)/HD)
    end,
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_DEPTH_TEST),    
    gl:color3f(1.0, 1.0, 1.0),   %%Clear
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:'begin'(?GL_QUADS),
    gl:vertex2f(X0Y0, X0Y0),
    gl:vertex2f(XM, X0Y0),
    gl:vertex2f(XM, YM),
    gl:vertex2f(X0Y0, YM),
    gl:'end'(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:color3b(0, 0, 0),   %%Clear   
    gl:'begin'(?GL_LINE_LOOP),
    D = X0Y0/10,
    gl:vertex2f(D, D),
    gl:vertex2f(1-D, D),
    gl:vertex2f(1-D, 1-D),
    gl:vertex2f(D, 1-D),
    gl:'end'(),    
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1.0, 1.0, 1.0),   %%Clear
    case has_texture(MatN, Mats) of
	false -> ignore;
	true when (Uvs#uvstate.option)#setng.texbg == false -> 
	    Id = textureId(MatN, Mats),
	    gl:bindTexture(?GL_TEXTURE_2D, Id);
	true ->
	    Id = textureId(MatN, Mats),
	    gl:bindTexture(?GL_TEXTURE_2D, Id),
	    gl:enable(?GL_TEXTURE_2D)
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

wings_view(Uvs = #uvstate{mode = Mode, geom = {{X1,Y1,W1,H1}, {X2,Y2,_,_,_,_,_}}, st = St}) ->
    ModeL = atom_to_list(Mode),
    Text = [ModeL] ++ [" Mode: [R] in texture window to access menu, "
		       "[L] to select face groups"],
    wings_io:message(Text),
    [X0=0,Y0=0,W0,H0] = gl:getIntegerv(?GL_VIEWPORT),
    gl:viewport(X1,Y1,W1,H1),
    put(wm_viewport, {X1,Y1,W1,H1}),
    wings_draw:render(St),
    put(wm_viewport, {X0,Y0,W0,H0}),
    gl:viewport(X0,Y0,W0,H0),
    %%    wings_io:info(info(St)),
    wings_io:update(Uvs#uvstate.st),
    wings_io:ortho_setup(),
    gl:color3fv(?PANE_COLOR),    
    gl:recti(W1, 25, X2, H0 - Y2),
    ok.

reset_view() ->    
    gl:popAttrib().

%%% Texture Creation

calc_texsize(Vp, Tex, Orig) when Vp >= Tex -> {Tex, Orig div Tex};
calc_texsize(Vp, Tex, Orig) -> 
    calc_texsize(Vp, Tex div 2, Orig).

get_texture(Uvs=#uvstate{option = Option, sel=Sel, areas=As}) ->
    {TexW,TexH} = Option#setng.texsz,
    [0, 0, W0, H0] = gl:getIntegerv(?GL_VIEWPORT),
    {W,Wd} = calc_texsize(W0, TexW, TexW),
    {H,Hd} = calc_texsize(H0, TexH, TexH),
    Mem = sdl_util:malloc(W*H*3, ?GL_BYTE),
    Uvs1 = Uvs#uvstate{sel=[], areas=add_areas(Sel, As), dl = undefined},
    GetSubTex = 
	fun(WC,HC,Uvs0) ->
		setup_view({0,0,W,H,0,W,H}, {Wd,Hd,WC,HC},Uvs0),
		Uvs2 = draw_texture(Uvs1),
		gl:flush(),
		gl:readBuffer(?GL_BACK),
		gl:readPixels(0,0,W,H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
		gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
		reset_view(),    
		ImageBin = sdl_util:readBin(Mem, W*H*3),
		{ImageBin, Uvs2}
	end,
    {ImageBins,_Uvs2} = get_texture(0,Wd,0,Hd, GetSubTex, [], Uvs),
    ImageBin = merge_texture(ImageBins,Wd,Hd,W*3,H,[]),
    sdl_util:free(Mem),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    case (TexW * TexH *3) == size(ImageBin) of	
	true ->
	    {TexW, TexH, ImageBin};
	false ->
	    BinSzs = [size(Bin) || Bin <- ImageBins],
	    exit({texture_error, {TexW, TexH, size(ImageBin), W,Wd,H,Hd, BinSzs}})
    end.
		 
get_texture(Wc,Wd,Hc,Hd, GetSubTex, Image, Uvs) when Wc<Wd,Hc<Hd ->
    {PI, Uvs1} = GetSubTex(Wc, Hc, Uvs),
    get_texture(Wc+1,Wd,Hc,Hd,GetSubTex, [PI|Image], Uvs1);
get_texture(_Wc,Wd,Hc,Hd, GetSubTex, Image, Uvs) when Hc < Hd ->
    get_texture(0,Wd,Hc+1,Hd,GetSubTex, Image, Uvs);
get_texture(_,_,_,_,_,Image, Uvs) ->
    {reverse(Image), Uvs}.

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

draw_windows(Uvs) ->
    wings_view(Uvs),
    setup_view(element(2,Uvs#uvstate.geom), Uvs),
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
     separator,
     {"Apply texture", apply_texture, "Test attach current texture to model"},
     separator,
     {"Quit", quit, "Quit AutoUv-mapper"}].

edge_option_menu(#uvstate{option = Option}) ->
    DefVar = {edge_mode, Option#setng.edges},
    DefTSz = {txsize, element(1, Option#setng.texsz)},
    MaxTxs = min([4096,gl:getIntegerv(?GL_MAX_TEXTURE_SIZE)]),
    TxSzs = genSizeOption(128, MaxTxs, DefTSz, []),    
    
    Qs = [{vframe,[{alt,DefVar,"Draw Border Edges", border_edges},
		   {alt,DefVar,"Draw All Edges",    all_edges},
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
    #uvstate{st = #st{mat=Mats}, 
	     areas = #areas{matname = MatN}} = Uvs,
    DefVar = {quit_mode, quit_uv_tex},
    A1 = {alt,DefVar, "Quit and save UV-coords and texture",quit_uv_tex},
    A2 = {alt,DefVar, "Quit and save only UV-coords (use old or imported texture)", quit_uv},
    A3 = {alt,DefVar, "Quit and cancel all changes", cancel},
    Alts = case has_texture(MatN, Mats) of
	       true ->
		   [A1,A2,A3];
	       false ->
		   [A1,A3]
	   end,
    Qs = [{vframe, Alts,[{title,"Quit"}]}],
    wings_ask:dialog(Qs, %%fun() -> draw_windows(Uvs) end,
		     fun([Quit]) -> {auv, quit, Quit} end).

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
handle_event(MouseM = #mousemotion{}, Uvs0 = #uvstate{op = Op}) when Op /= undefined ->	   
    handle_mousemotion(MouseM, Uvs0);
handle_event(#mousebutton{state = ?SDL_RELEASED, button = ?SDL_BUTTON_RIGHT,
			  x = MX, y = MY}, 
	     Uvs0 = #uvstate{geom = {{_,_,_,OH},ViewP={X0,Y0,W,H,_,_,_}}, 
			     op = Op, mode = Mode})
  when MX > X0, MX < X0 + W, (OH - MY) > Y0, (OH - MY) < Y0 + H, Op == undefined ->
    command_menu(Mode, MX,MY, Uvs0);
handle_event(#mousebutton{state = ?SDL_RELEASED, button = ?SDL_BUTTON_RIGHT}, 
	     Uvs0 = #uvstate{op = Op}) ->	   
    case Op of
	undefined ->
	    keep;
	_ ->
	    get_event(Op#op.undo)
    end;
handle_event(#mousebutton{state=?SDL_RELEASED,button=?SDL_BUTTON_LEFT,x = MX, y = MY}, 
	     Uvs0 = #uvstate{geom = {{_,_,_,OH},ViewP={X0,Y0,W,H,_,_,_}},
			     mode = Mode,
			     op = Op,
			     sel = Sel0,
			     areas = As = #areas{we=We,as=Curr0}})
  when Op == undefined; element(1,Op) == {fmove, MX,MY},
       MX > X0, MX < X0 + W, (OH - MY) > Y0, (OH - MY) < Y0 + H ->
    SX = (MX-X0),
    SY = ((OH-MY)-Y0),
    case select(Mode, SX, SY, ?add_as(Sel0,Curr0), We, ViewP) of
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
	     Uvs0 = #uvstate{geom = {{_,_,_,OH},ViewP={X0,Y0,W,H,_,_,_}},
			     mode = Mode,
			     op = Op,
			     sel = Sel0,
			     areas = As = #areas{we=We,as=Curr0}}) ->	   
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
	    case select(Mode, CX-X0,((OH-CY)-Y0), BW,BH, Curr0, We, ViewP) of
		none -> 
		    get_event(Uvs0#uvstate{op = undefined});
		Hits -> 
		    %%			    ?DBG("Hit number ~p \n",[length(Hits)]),
		    {Sel1, Curr1} = update_selection(Hits, Sel0, Curr0),
		    get_event(Uvs0#uvstate{sel = Sel1,
					   st = wings_select_faces(Sel1, We, Uvs0#uvstate.st),
					   areas = As#areas{as=Curr1},
					   dl = undefined, op = undefined})
	    end;
	rotate ->
	    Sel1 = [finish_rotate(A)|| A <- Sel0],
	    get_event(Uvs0#uvstate{op = undefined, sel = Sel1});
	_ ->
	    get_event(Uvs0#uvstate{op = undefined})
    end;

handle_event(#mousebutton{state=?SDL_PRESSED,button=?SDL_BUTTON_LEFT,x=MX,y=MY}, 
	     Uvs0 = #uvstate{geom = {{_,_,_,OH},ViewP={X0,Y0,W,H,_,_,_}},
			     mode = Mode,
			     op = Op,
			     sel = Sel0,
			     areas = As = #areas{we=We,as=Curr0}}) 
  when Op == undefined, MX > X0, MX < X0 + W, (OH - MY) > Y0, (OH - MY) < Y0 + H ->
    case ?TC(select(Mode, MX-X0, ((OH-MY)-Y0), ?add_as(Sel0,Curr0), We, ViewP)) of
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
%% #mousebutton{state = ?SDL_RELEASED, x = MX, y = MY} ->
%%     ?DBG("Untrapped Mouse event at ~p Y ~p~n", [{MX,MY}, {Y0, H}]),
%%     get_event(Uvs0);
handle_event(MB=#mousebutton{state=?SDL_PRESSED,button=Butt,x=MX}, 
	     Uvs0 = #uvstate{geom = {{_,_,_,OH},ViewP={X0,Y0,W,H,_,_,_}},
			     mode = Mode,
			     op = Op})
  when MX < X0, Op == undefined ->
    case Butt of 
	?SDL_BUTTON_MIDDLE ->
	    wings_camera:event(MB, fun() -> draw_windows(Uvs0) end);
	4 ->
	    wings_camera:event(MB, fun() -> draw_windows(Uvs0) end);
	5 -> 
	    wings_camera:event(MB, fun() -> draw_windows(Uvs0) end);
	_Else ->
	    case sdl_keyboard:getModState() of
		Mod when Mod band ?CTRL_BITS =/= 0 ->
		    wings_camera:event(MB, fun() -> draw_windows(Uvs0) end);
		_ ->
		    keep
	    end
    end;
handle_event(#mousebutton{}, _Uvs0) ->
    %%	    ?DBG("Got2 MB ~p\n", [MB]),
    keep;

handle_event(#keyboard{state = ?SDL_PRESSED, keysym = Sym}, 
	     Uvs0=#uvstate{sel = Sel0,areas=As=#areas{we=We,as=Curr0}}) ->
    case Sym of
	#keysym{sym = ?SDLK_SPACE} ->
	    get_event(Uvs0#uvstate{sel = [],
				   st = wings_select_faces([], We, Uvs0#uvstate.st),
				   areas = add_areas(Sel0,As),
				   dl = undefined});
	#keysym{sym = ?SDLK_F5} ->
	    import_file(default, Uvs0);
	#keysym{sym = $b} ->		    
	    get_event(Uvs0#uvstate{mode = faceg});
	#keysym{sym = $f} ->
	    get_event(Uvs0#uvstate{mode = face});
	#keysym{sym = $e} ->  %% Bugbug
	    Old = Uvs0#uvstate.option,
	    get_event(Uvs0#uvstate{mode = edge, dl=undefined, 
				   option = Old#setng{edges = all_edges}});
	#keysym{sym = $v} ->
	    get_event(Uvs0#uvstate{mode = vertex});		
	#keysym{sym = $p} ->
	    [?DBG("DBG ~p\n", [P]) || P <- ?add_as(Sel0,Curr0)],
	    keep;
	_Key ->
	    %%      ?DBG("Missed Key ~p ~p~n", [_Key, ?SDLK_SPACE]),
	    keep
    end;

handle_event({action, {auv, export}}, Uvs0) ->
    case wings_plugin:call_ui({file,export,tga_prop()}) of
	aborted -> 
	    get_event(Uvs0);	
	FileName0 ->
	    {TW,TH,TexBin} = ?SLOW(get_texture(Uvs0)),
	    Image = #e3d_image{image = TexBin, width = TW, height = TH},
	    FileName1 = ensure_ext(FileName0,".tga"),
	    case ?SLOW((catch e3d_image:save(Image, FileName1))) of
		ok -> 			   
		    get_event(Uvs0#uvstate{last_file = FileName1});	
		{_, Error0} ->
		    Error = FileName1 ++ ": " ++ file:format_error(Error0),
		    wings_util:message("Export failed: " ++ Error, Uvs0#uvstate.st)
	    end
    end;
handle_event({action, {auv, import}}, Uvs0) ->
    case wings_plugin:call_ui({file,import,tga_prop()}) of
	aborted -> 
	    get_event(Uvs0);
	FileName0 ->
	    FileName1 = ensure_ext(FileName0,".tga"),
	    ?SLOW(import_file(FileName1, Uvs0))
		end;

handle_event({action, {auv, apply_texture}},
	     Uvs0=#uvstate{sel = Sel0,areas=As=#areas{we=We,as=Curr0}}) ->
    Tx = ?SLOW(get_texture(Uvs0)),
    Areas1 = add_areas(Sel0,As),
    {St2, Areas1} = add_material(edit, Tx, Uvs0#uvstate.st, Areas1),
    We2 = insert_uvcoords(Areas1),
    Shapes1 = gb_trees:update(We#we.id, We2, St2#st.shapes),
    St3 = St2#st{shapes = Shapes1},
    get_event(Uvs0#uvstate{st = St3});

handle_event({action, {auv, edge_options}},Uvs0) ->
    edge_option_menu(Uvs0);
handle_event({action, {auv, quit}},Uvs0) ->
    quit_menu(Uvs0);
handle_event({action, {auv, quit, cancel}},_Uvs0) ->
    wings_io:putback_event({action,{body, uvmap_cancel}}),
    pop;
handle_event({action, {auv, quit, QuitOp}},Uvs0) ->
    wings_io:putback_event({action,{body, {uvmap_done, QuitOp, Uvs0}}}),
    pop;

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
	     Uvs0=#uvstate{sel = Sel0,areas=As=#areas{we=We,as=Curr0}})->
    RscAreas = rescale_all(?add_as(Sel0,Curr0)),
    get_event(Uvs0#uvstate{sel = [],
			   areas = As#areas{as=RscAreas},
			   dl = undefined});
handle_event({action, {auv, {rotate, Deg}}},
	     Uvs0=#uvstate{mode=Mode,sel = Sel0,areas=As=#areas{we=We,as=Curr0}}) ->
    case Deg of
	rot_y_180 ->
	    Sel1 = [transpose_x(Mode, A) || A <- Sel0],
	    get_event(Uvs0#uvstate{sel = Sel1});
	rot_x_180 ->
	    Sel1 = [transpose_y(Mode, A) || A <- Sel0],
	    get_event(Uvs0#uvstate{sel = Sel1});
	free ->
	    handle_event({action, {auv, rotate}}, Uvs0);
	Deg ->
	    Sel1 = [finish_rotate({Id,A#a{rotate = Deg}})|| {Id,A} <- Sel0],
	    get_event(Uvs0#uvstate{op = undefined, sel = Sel1})
    end;

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
handle_event({resize, NX,NY},Uvs0) ->
    wings_io:resize(NX, NY),
    wings_draw_util:init(),
    St1 = wings_material:init(Uvs0#uvstate.st),	    
    %% gl:viewport(0,0,NX,NY),
    Geom = init_drawarea(),
    get_event(Uvs0#uvstate{geom=Geom, st=St1, dl=undefined});
handle_event(_Event,Uvs0) ->
    %%	    ?DBG("Got unhandled Event ~p ~n", [_Event]),
    get_event(Uvs0).

handle_mousemotion(#mousemotion{xrel = DX0, yrel = DY0, x=MX0,y=MY0}, Uvs0) ->
    #uvstate{geom = {{_,_,_,_OH},{_X0,_Y0,W,H,X0Y0,MW0,MH0}},
	     mode = Mode, op = Op, sel = Sel0} = Uvs0,
    {DX,DY} = case Op#op.prev of 
		  undefined -> {DX0,DY0}; 
		  {MX1,MY1}->  %% Don't trust relativ mouse event
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
	boxsel -> %% , Orig = {_OX,_OY}, Last},Old}
	    gl:matrixMode(?GL_PROJECTION),
	    gl:pushMatrix(),
	    gl:loadIdentity(),
	    [_WX,_WY,WW,WH]= gl:getIntegerv(?GL_VIEWPORT),
	    glu:ortho2D(0,WW,WH, 0),
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
import_file(FileName1, Uvs0) ->
    Type = [{type, r8g8b8}, {alignment, 1}, {order, lower_left}],

    case catch e3d_image:load(FileName1, Type) of
	{_, Error0} ->
	    Error = FileName1 ++ ": " ++ format_error(Error0),
	    %%	 Error = lists:flatten(io_lib:format("~p", [Error0])),
	    wings_util:message("Import failed: " ++ Error, Uvs0#uvstate.st);
	#e3d_image{width = TW, height = TH, image = TexBin} ->
	    case (TW == TH) andalso is_power_of_two(TW) of
		true ->
		    {St1,_As} = add_material(edit, {TW,TH,TexBin},
					     Uvs0#uvstate.st, Uvs0#uvstate.areas),
		    Option = Uvs0#uvstate.option,
		    get_event(Uvs0#uvstate{st = St1, 
					   option=Option#setng{texbg = true}, 
					   last_file = FileName1,
					   dl = undefined});
		false ->
		    wings_util:message("Import failed: Can only import square," 
				       "power of 2 sized pictures", Uvs0#uvstate.st)	
	    end
    end.

is_power_of_two(X) ->
    (X band -X ) == X.

format_error(Err) when atom(Err) ->
    case atom_to_list(Err) of
	Str = ['e'|_] ->
	    Str ++ " " ++ file:format_error(Err);
	_ -> 
	    lists:flatten(io_lib:format("~p", [Err]))
    end;
format_error(Err) ->
    lists:flatten(io_lib:format("~p", [Err])).

%%%%% Filename stuff
lowercase([H|R]) when H >= $A, H =< $Z ->
    [H + $a - $A | lowercase(R)];
lowercase([H|R]) ->
    [H | lowercase(R)];
lowercase([]) ->
    [].
ensure_ext(FileName,Ext) ->
    case lowercase(filename:extension(FileName)) of
	Ext -> FileName;
	[] -> FileName ++ Ext;
	Else -> Else
    end.

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

select(Mode, X,Y, Objects, We, ViewP = {_UVX,_UVY,UVW,UVH,XYS,XM,YM}) ->         
    XT = (XM-XYS)*X/UVW+XYS,
    YT = (YM-XYS)*Y/UVH+XYS,
    case find_selectable(XT,YT, gb_trees:to_list(Objects), []) of
	[] -> 
	    none;
	Possible ->
	    select(Mode, X,Y, 3,3, gb_trees:from_orddict(Possible), We, ViewP)
    end.
select(Mode, X,Y, W, H, Objects0, We, {_UVX,_UVY,UVW,UVH,XYS,XM,YM}) ->
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
	    ?DBG("Hits ~p", [NumHits]),
	    Hits = get_hits(NumHits, HitData, []),
	    ?DBG(" ~p ~n", [lists:usort(Hits)]),
	    map(fun(Hit = [FG|_]) -> 
			HitArea = gb_trees:get(FG, Objects0),
			{Hit, HitArea}
		end, lists:usort(Hits))
    end.

select_draw([{Co, A}|R], Mode, We = #we{vs = TempVs}) ->
%%    ?DBG("Co ~p\n",[Co]),
    #a{fs=Fs,vpos=Vs,center={CX,CY},
       scale=Scale,rotate=Rot, twe = TempWe0} = A,
    TempWe = 
	case TempWe0 of 
	    undefined ->
		NewVs = foldl(fun({No, Pos}, Tree) ->
				      Vtx = gb_trees:get(No,Tree),
				      gb_trees:update(No, Vtx#vtx{pos=Pos}, Tree)
			      end, TempVs, Vs),
		We#we{vs = NewVs, mode=material}; %% We don't want verex colors
	    Else -> 
		Else#we{mode = material}
	end,
    gl:pushMatrix(),
    gl:pushName(0),
    gl:loadName(Co),
    gl:translatef(CX,CY,0.0),
    gl:scalef(Scale, Scale, 1.0),
    gl:rotatef(Rot,0,0,1),
    select_draw1(Mode, Fs, TempWe),
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

find_selectable(X,Y, [A={_, #a{center={CX,CY},size={W,H}}}|Rest], Acc)
  when X > (CX-W*?OUT), X < (CX+W*?OUT), Y > (CY-H*?OUT), Y < (CY+H*?OUT) ->
    find_selectable(X,Y, Rest, [A|Acc]);
find_selectable(X,Y, [_H|R], Acc) ->
    find_selectable(X,Y, R, Acc);
find_selectable(_X,_Y, [], Acc) ->
    reverse(Acc).

wings_select_faces(As, We, St) ->
    Faces = [A#a.fs || {_,A} <- As],
    wpa:sel_set(face, [{We#we.id, gb_sets:from_list(lists:append(Faces))}], St).

%%%% GUI Operations

greatest(A,B) ->
    if abs(A) > abs(B) ->
	    A;
       true ->
	    B
    end.

move_area(faceg, {Id, A = #a{center = {X0,Y0}}}, DX, DY) ->
%    ?DBG("Move ~p ~p ~p~n", [{X0,Y0}, S, {DX, DY}]),
%%    A#a{center = {X0+DX/S, Y0+DY/S}}.
    {Id, A#a{center = {X0+DX, Y0+DY}}}.
scale_area(faceg,{Id,A = #a{scale = S, size = {W,H}}}, DX, DY) ->
    NS = greatest(DX,DY),
    {Id,A#a{scale = S+NS, size = {W+W*NS, H+H*NS}}}.
rotate_area(faceg, {Id,A = #a{rotate = R}}, DX, DY) ->
    NS = greatest(DX,DY),
    NewR = R + NS*180,
    {Id,A#a{rotate = NewR}}.
transpose_x(faceg,{Id,A = #a{vpos = Vpos}}) ->
    New = [{Nr, {-X,Y,Z}} || {Nr, {X,Y,Z}} <- Vpos],
    {Id,A#a{vpos = New}}.
transpose_y(faceg,{Id,A = #a{vpos = Vpos}}) ->
    New = [{Nr, {X,-Y,Z}} || {Nr, {X,Y,Z}} <- Vpos],
    {Id,A#a{vpos = New}}.
rescale_all(Areas0) ->
    Areas1 = gb_trees:to_list(Areas0),
    Find = fun({_, #a{center = {CX,CY}, size = {W,H}}}, [MX,MY]) ->
		   TX = CX + W/2,
		   TY = CY + H/2,
		   NewMX = if TX > MX -> TX; true -> MX end,
		   NewMY = if TY > MY -> TY; true -> MY end,
		   [NewMX, NewMY]
	   end,
    Max = max(foldl(Find, [0,0], Areas1)),
    NS = 1.0 / Max,
    Rescale = fun({Id,A = #a{center = {CX0,CY0}, size = {W0,H0}, scale = S0}}) ->
		      {Id,A#a{center = {CX0*NS,CY0*NS}, size = {W0*NS,H0*NS}, scale = S0*NS}}
	      end,
    gb_trees:from_orddict(map(Rescale, Areas1)).
finish_rotate({Id,Area = #a{rotate = R, vpos = Vs0, scale = S}}) ->
    Rot = e3d_mat:rotate(float(trunc(R)), {0.0,0.0,1.0}),
    Vs1 = [{IdV, e3d_mat:mul_point(Rot, Vec)} || {IdV, Vec} <- Vs0],
    [{_, {X,Y,_}} |RVs1] = Vs1,
    {BX0, BX1, BY0, BY1} = 
	foldl(fun({_, Pos}, Ac) -> maxmin(Pos, Ac) end, {X,X,Y,Y}, RVs1),
    {Id,Area#a{rotate=0.0, vpos = Vs1, size={(BX1-BX0)*S, (BY1-BY0)*S}, twe = undefined}}.

%%%% Draw routines
outer_edges(Faces0, We) ->
    %% I use normals here to detect direction of face and remove 
    %% faces with wrong direction.
    Faces1 = foldl(fun(Face, Acc)-> 
			   case e3d_vec:dot(wings_face:normal(Face, We), {0.0,0.0,1.0}) >= 0.0 of
			       true -> [Face|Acc];
			       _ -> Acc
			   end
		   end, [], Faces0),    
    S = wings_face:fold_faces(fun(Face, _, E, _, A) -> [{E,Face}|A] end, [], Faces1, We),
    outer_edges_1(sort(S), []).
outer_edges_1([{E,_},{E,_}|T], Out) ->
    outer_edges_1(T, Out);
outer_edges_1([E|T], Out) ->
    outer_edges_1(T, [E|Out]);
outer_edges_1([], Out) -> reverse(Out).

draw_area(A = #a{fs=Fs, vpos=Vs, center={CX,CY}, scale=Scale, rotate = R, twe=Twe,tbe=Tbe}, 
	  We, Options = #setng{color = ColorMode, edges = EdgeMode}, Materials) -> 
    TempWe = case Twe of
		 undefined ->
		     %% Temporary patch the we structure so we can use wings functions
		     TempVs = We#we.vs,
		     NewVs = foldl(fun({No, Pos}, Tree) ->
					   Vtx = gb_trees:get(No,Tree),
					   gb_trees:update(No, Vtx#vtx{pos=Pos}, Tree)
				   end, TempVs, Vs),
		     We#we{vs = NewVs};
		 _ ->
		     Twe
	     end,
    TempBE = case Tbe of undefined -> outer_edges(Fs, TempWe); _ -> Tbe end,

    gl:pushMatrix(),
    gl:translatef(CX,CY,0.0),
    gl:scalef(Scale, Scale, 1.0),
    gl:rotatef(float(trunc(R)),0,0,1),
    %% Draw Materials and Vertex Colors
    if
	EdgeMode == border_edges ->
	    %% Draw outer edges only
	    #we{es=Etab, vs=Vtab}=TempWe,
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
	    lists:foreach(DrawEdge, TempBE),
	    gl:glEnd(),
	    gl:popMatrix();
	EdgeMode == all_edges ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    gl:color3f(0.6, 0.6, 0.6),
	    draw_faces(Fs, TempWe#we{mode = material});
	EdgeMode == no_edges ->
	    ok
    end,
    if
	ColorMode == true ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    MatName = (gb_trees:get(hd(Fs), We#we.fs))#face.mat,
	    case has_texture(MatName, Materials) of
		true -> gl:enable(?GL_TEXTURE_2D);
		false -> ignore
	    end,
	    lists:foreach(fun(Face) ->
				  gl:color4fv(get_material(Face, Materials, We)),
				  draw_faces([Face], TempWe)
			  end, Fs),
	    case has_texture(MatName, Materials) of
		true -> gl:disable(?GL_TEXTURE_2D);
		false -> ignore
	    end;
	is_tuple(ColorMode), size(ColorMode) == 4 ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:color4fv(ColorMode),
	    draw_faces(Fs, TempWe#we{mode = material});
	true ->
	    ignore
    end,
    gl:popMatrix(),
    A#a{twe = TempWe, tbe = TempBE}.

draw_faces(Fs, We) ->
    case wings_pref:get_value(display_list_opt) of
	false ->
	    draw_faces2(Fs,We);
	true ->
	    gl:'begin'(?GL_TRIANGLES),
	    draw_faces2(Fs,We),
	    gl:'end'()
    end,
    gl:edgeFlag(?GL_TRUE).

draw_faces2([], _We) ->    ok;
draw_faces2([H|R], We) ->
    draw_face(H, We),
    draw_faces2(R,We).

draw_face(Face, #we{mode=material,vs=Vtab, fs = Ftab}=We) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    Vs = wings_face:surrounding_vertices(Face, Edge, We),
    {X,Y,Z} = N = wings_face:face_normal(Vs, We),
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    Info = [{normal,N}],
    tess_face(Tess, Vs, Info, Vtab);
draw_face(Face, We = #we{fs = Ftab}) ->
    #face{edge=Edge} = gb_trees:get(Face, Ftab),
    Vs = wings_face:draw_info(Face, Edge, We),
    {X,Y,Z} = N = wings_face:draw_normal(Vs),
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    tess_face_vtxcol(Tess, Vs, [{normal,N}]).

tess_face_vtxcol(Tess, [{Pos,{_,_}=UV}|T], Normal) ->
    glu:tessVertex(Tess, Pos, [{texcoord2,UV}|Normal]),
    tess_face_vtxcol(Tess, T, Normal);
tess_face_vtxcol(Tess, [{Pos,{_,_,_}=Col}|T], Normal) ->
    glu:tessVertex(Tess, Pos, [{color,Col}|Normal]),
    tess_face_vtxcol(Tess, T, Normal);
tess_face_vtxcol(Tess, [], _Normal) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).

tess_face(Tess, [V|T], N, Vtab) ->
    glu:tessVertex(Tess, wings_vertex:pos(V, Vtab), N),
    tess_face(Tess, T, N, Vtab);
tess_face(Tess, [], _N, _Vtab) ->
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess).
