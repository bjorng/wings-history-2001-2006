%%
%%  wings_draw.erl --
%%
%%     This module draws objects using OpenGL.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw.erl,v 1.127 2003/06/09 08:56:20 bjorng Exp $
%%

-module(wings_draw).
-export([update_dlists/1,update_sel_dlist/0,
	 split/3,original_we/1,update_dynamic/2,
	 update_mirror/0,smooth_dlist/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,foldl/3,merge/1,sort/1,any/2]).

-record(split,
	{static_vs,
	 dyn_vs,
	 dyn_faces,
	 dyn_plan,				%Plan for drawing dynamic faces.
	 v2f=none,				%Vertex => face
	 f2v=none,				%Face => vertex
	 orig_we
	}).

%%%
%%% Update display lists.
%%%

update_dlists(#st{selmode=Mode,sel=Sel}=St) ->
    prepare_dlists(St),
    case wings_draw_util:changed_materials(St) of
	[] -> ok;
	ChangedMat -> invalidate_by_mat(ChangedMat)
    end,
    wings_draw_util:map(fun(D, Data) ->
				sel_fun(D, Data, Mode)
			end, Sel),
    do_update_dlists(St),
    update_sel_dlist(),
    update_mirror().

prepare_dlists(#st{shapes=Shs}) ->
    wings_draw_util:update(fun prepare_fun/2, gb_trees:values(Shs)).

prepare_fun(eol, [#we{perm=Perm}=We|Wes]) when ?IS_NOT_VISIBLE(Perm) ->
    {#dlo{src_we=empty_we(We)},Wes};
prepare_fun(eol, [We|Wes]) ->
    {#dlo{src_we=We,mirror=check_mirror(We)},Wes};
prepare_fun(eol, []) ->
    eol;
prepare_fun(#dlo{src_we=We,split=#split{}=Split}=D, [We|Wes]) ->
    {D#dlo{src_we=We,split=Split#split{orig_we=We}},Wes};
prepare_fun(#dlo{src_we=We}=D, [We|Wes]) ->
    {D#dlo{src_we=We},Wes};
prepare_fun(#dlo{src_we=#we{id=Id},proxy_data=Proxy}, [#we{id=Id,perm=Perm}=We|Wes]) ->
    if 
	?IS_VISIBLE(Perm) ->
	    {#dlo{src_we=We,mirror=check_mirror(We),proxy_data=Proxy},Wes};
	true ->
	    {#dlo{src_we=empty_we(We),proxy_data=Proxy},Wes}
    end;
prepare_fun(#dlo{}, Wes) ->
    {deleted,Wes}.

invalidate_by_mat(Changed0) ->
    Changed = gb_sets:from_list(Changed0),
    wings_draw_util:map(fun(D, _) -> invalidate_by_mat(D, Changed) end, []).

invalidate_by_mat(#dlo{work=none,vs=none,smooth=none,proxy_faces=none}=D, _) ->
    %% Nothing to do.
    D;
invalidate_by_mat(#dlo{src_we=We}=D, Changed) ->
    Used = wings_material:used_materials_we(We),
    case gb_sets:is_empty(gb_sets:intersection(Used, Changed)) of
	true -> D;
	false -> D#dlo{work=none,vs=none,smooth=none,proxy_faces=none}
    end.

empty_we(We) ->
    Et = gb_trees:empty(),
    We#we{es=Et,fs=Et,vc=Et,vp=Et,he=gb_sets:empty(),mat=default,
	  mirror=none,light=none}.

check_mirror(#we{mirror=none}) -> none;
check_mirror(#we{fs=Ftab,mirror=Face}) ->
    case gb_trees:is_defined(Face, Ftab) of
	false -> none;
	true -> {Face}
    end.

sel_fun(#dlo{src_we=#we{id=Id},src_sel=SrcSel}=D, [{Id,Items}|Sel], Mode) ->
    case SrcSel of
	{Mode,Items} -> {D,Sel};
	_ -> {D#dlo{sel=none,normals=none,src_sel={Mode,Items}},Sel}
    end;
sel_fun(D, Sel, _) ->
    {D#dlo{sel=none,src_sel=none},Sel}.

%%%
%%% Create all display lists that are currently needed.
%%% The work is done in two passes:
%%%
%%% 1. Find out which display lists are needed based on display modes.
%%%    (Does not check whether they exist or not.)
%%%
%%% 2. Create missing display lists.
%%%

%%
%% Pass 1 starts here.
%%

do_update_dlists(#st{selmode=vertex}=St) ->
    case wings_pref:get_value(vertex_size) of
	0.0 ->
	    do_update_dlists_1([], St);
	PointSize->
	    do_update_dlists_1([{vertex,PointSize}], St)
    end;
do_update_dlists(St) ->
    do_update_dlists_1([], St).

do_update_dlists_1(Need, St) ->
    case wings_pref:get_value(show_normals) of
	false -> do_update_dlists_2(Need, St);
	true -> do_update_dlists_2([normals|Need], St)
    end.

do_update_dlists_2(Need0, St) ->
    Need1 = foldl(fun(W, A) ->
			  case wings_wm:get_prop(W, workmode) of
			      false -> [smooth|A];
			      true -> [work|A]
			  end
		  end, Need0, wings_util:geom_windows()),
    Need = ordsets:from_list(Need1),
    do_update_dlists_3(Need, St).

do_update_dlists_3(CommonNeed, St) ->
    Need0 = wings_draw_util:fold(fun(D, A) ->
					 need_fun(D, CommonNeed, St, A)
				 end, []),
    Need = reverse(Need0),
    wings_draw_util:map(fun(D, N) ->
				update_fun(D, N, St)
			end, Need).

need_fun(#dlo{src_we=We}, _, _, Acc) when ?IS_LIGHT(We) ->
    [[light]|Acc];
need_fun(#dlo{src_we=#we{he=Htab,mode=Mode},proxy_data=Pd}, Need0, _St, Acc) ->
    Need1 = case gb_sets:is_empty(Htab) orelse not wings_pref:get_value(show_edges) of
		false -> [hard_edges|Need0];
		true -> Need0
	    end,
    Need2 = if
		Pd =:= none -> Need1;
		true -> [proxy|Need1]
	    end,
    Need = if
	       Mode =:= vertex -> Need2 ++ [vertex_mode_edges];
	       true -> Need2
	   end,
    [Need|Acc].

%%
%% Pass 2 starts here.
%%

update_fun(D0, [P|Ps], St) ->
    D = update_fun_1(D0, P, St),
    {D,Ps};
update_fun(D, [], _) -> D.

update_fun_1(D0, [H|T], St) ->
    D = update_fun_2(H, D0, St),
    update_fun_1(D, T, St);
update_fun_1(D, [], _) -> D.

update_fun_2(light, D, _) ->
    wings_light:update(D);
update_fun_2(work, #dlo{work=none,src_we=#we{fs=Ftab}=We}=D, St) ->
    Dl = draw_faces(gb_trees:to_list(Ftab), We, St),
    D#dlo{work=Dl};
update_fun_2(smooth, #dlo{smooth=none}=D, St) ->
    We = wings_subdiv:smooth_we(D),
    {List,Tr} = smooth_dlist(We, St),
    D#dlo{smooth=List,transparent=Tr};
update_fun_2({vertex,PtSize}, #dlo{vs=none,src_we=#we{vp=Vtab}}=D, _) ->
    UnselDlist = gl:genLists(1),
    gl:newList(UnselDlist, ?GL_COMPILE),
    gl:pointSize(PtSize),
    gl:color3f(0, 0, 0),
    gl:'begin'(?GL_POINTS),
    foreach(fun(Pos) -> gl:vertex3fv(Pos) end, gb_trees:values(Vtab)),
    gl:'end'(),
    gl:endList(),
    D#dlo{vs=UnselDlist};
update_fun_2(hard_edges, #dlo{hard=none,src_we=#we{he=Htab}=We}=D, _) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    #we{es=Etab,vp=Vtab} = We,
    foreach(fun(Edge) ->
		    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		    gl:vertex3fv(gb_trees:get(Va, Vtab)),
		    gl:vertex3fv(gb_trees:get(Vb, Vtab))
	    end, gb_sets:to_list(Htab)),
    gl:'end'(),
    gl:endList(),
    D#dlo{hard=List};
update_fun_2(vertex_mode_edges, #dlo{work={call,_,Faces}}=D, _) ->
    Dl = wings_draw_util:force_flat_color(Faces, wings_pref:get_value(edge_color)),
    D#dlo{edges=Dl};
update_fun_2(normals, D, _) ->
    make_normals_dlist(D);
update_fun_2(proxy, D, St) ->
    wings_subdiv:update(D, St);
update_fun_2(_, D, _) -> D.

%%%
%%% Update the selection display list.
%%%

update_sel_dlist() ->
    wings_draw_util:map(fun(D, _) ->
				update_sel(D)
			end, []).

update_sel(#dlo{src_we=We}=D) when ?IS_LIGHT(We) -> {D,[]};
update_sel(#dlo{sel=none,src_sel={body,_}}=D) ->
    update_sel_all(D);
update_sel(#dlo{sel=none,src_sel={face,Faces},src_we=#we{fs=Ftab}=We}=D) ->
    case gb_trees:size(Ftab) =:= gb_sets:size(Faces) of
	true ->
	    update_sel_all(D);
	false ->
	    Tess = wings_draw_util:tess(),
	    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
	    List = gl:genLists(1),
	    gl:newList(List, ?GL_COMPILE),
	    wings_draw_util:begin_end(
	      fun() ->
		      foreach(
			fun(Face) ->
				Edge = gb_trees:get(Face, Ftab),
				wings_draw_util:flat_face(Face, Edge, We)
			end, gb_sets:to_list(Faces))
	      end),
	    gl:endList(),
	    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
	    D#dlo{sel=List}
    end;
update_sel(#dlo{sel=none,src_sel={edge,Edges}}=D) ->
    #dlo{src_we=#we{es=Etab,vp=Vtab}} = D,
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    case gb_trees:size(Etab) =:= gb_sets:size(Edges) of
	true ->
	    foreach(fun(#edge{vs=Va,ve=Vb}) ->
			    gl:vertex3fv(gb_trees:get(Va, Vtab)),
			    gl:vertex3fv(gb_trees:get(Vb, Vtab))
		    end, gb_trees:values(Etab));
	false ->
	    foreach(fun(Edge) ->
			    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			    gl:vertex3fv(gb_trees:get(Va, Vtab)),
			    gl:vertex3fv(gb_trees:get(Vb, Vtab))
		    end, gb_sets:to_list(Edges))
    end,
    gl:'end'(),
    gl:endList(),
    D#dlo{sel=List};
update_sel(#dlo{sel=none,src_sel={vertex,Vs}}=D) ->
    #dlo{src_we=#we{vp=Vtab0}} = D,
    SelDlist = gl:genLists(1),
    case gb_trees:size(Vtab0) =:= gb_sets:size(Vs) of
	true ->
	    gl:newList(SelDlist, ?GL_COMPILE),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun(Pos) -> gl:vertex3fv(Pos) end, gb_trees:values(Vtab0)),
	    gl:'end'(),
	    gl:endList(),
	    D#dlo{sel=SelDlist};
	false ->
	    Vtab1 = gb_trees:to_list(Vtab0),
	    Vtab = sofs:from_external(Vtab1, [{vertex,data}]),
	    R = sofs:from_external(gb_sets:to_list(Vs), [vertex]),
	    Sel = sofs:to_external(sofs:restriction(Vtab, R)),

	    gl:newList(SelDlist, ?GL_COMPILE),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun({_,Pos}) -> gl:vertex3fv(Pos) end, Sel),
	    gl:'end'(),
	    gl:endList(),

	    D#dlo{sel=SelDlist}
    end;
update_sel(#dlo{}=D) -> D.

update_sel_all(#dlo{src_we=#we{mode=vertex},work={call,_,Faces}}=D) ->
    Dl = wings_draw_util:force_flat_color(Faces, wings_pref:get_value(selected_color)),
    D#dlo{sel=Dl};
update_sel_all(#dlo{work=Faces}=D) ->
    D#dlo{sel=Faces}.

update_mirror() ->
    wings_draw_util:map(fun update_mirror/2, []).

update_mirror(#dlo{mirror={Face},src_we=We}=D, _) when is_integer(Face) ->
    N = wings_face:normal(Face, We),
    Center = wings_face:center(Face, We),
    RotBack = e3d_mat:rotate_to_z(N),
    Rot = e3d_mat:transpose(RotBack),
    Mat0 = e3d_mat:mul(e3d_mat:translate(Center), Rot),
    Mat1 = e3d_mat:mul(Mat0, e3d_mat:scale(1.0, 1.0, -1.0)),
    Mat2 = e3d_mat:mul(Mat1, RotBack),
    Mat = e3d_mat:mul(Mat2, e3d_mat:translate(e3d_vec:neg(Center))),
    D#dlo{mirror=Mat};
update_mirror(D, _) -> D.

%%%
%%% Splitting of objects into two display lists.
%%%

split(#dlo{split=#split{orig_we=#we{}=We}=Split}=D, Vs, St) ->
    split(D#dlo{src_we=We}, Split, Vs, St);
split(D, Vs, St) ->
    split(D, #split{dyn_faces=sofs:set([], [face])}, Vs, St).

split(#dlo{src_we=#we{es=Etab}}=D, #split{v2f=none}=Split, Vs, St) ->
    V2F0 = foldl(fun(#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}, A) ->
			 [{Va,Lf},{Va,Rf},{Vb,Lf},{Vb,Rf}|A]
		 end, [], gb_trees:values(Etab)),
    V2F = sofs:relation(V2F0, [{vertex,face}]),
    F2V = sofs:converse(V2F),
    split(D, Split#split{v2f=V2F,f2v=F2V}, Vs, St);
split(#dlo{mirror=M,src_sel=Sel,src_we=#we{fs=Ftab0}=We,proxy_data=Pd}=D,
      #split{v2f=V2F,f2v=F2V,dyn_faces=Faces0}=Split0, Vs0, St) ->
    Vs = sofs:set(Vs0, [vertex]),
    Faces1 = sofs:image(V2F, Vs),
    Ftab = sofs:from_external(gb_trees:to_list(Ftab0), [{face,data}]),
    Faces = case sofs:is_subset(Faces1, Faces0) of
		true ->
		    case D#dlo.work of
			[List|_] -> Faces0;
			List when is_integer(List) -> Faces0
		    end;
		false ->
		    List = static_dlist(Faces1, Ftab, We, St),
		    Faces1
	    end,
    AllVs = sofs:image(F2V, Faces),
    
    {DynVs,VsDlist} = split_vs_dlist(AllVs, Sel, We),

    FtabDyn0 = sofs:restriction(Ftab, Faces),
    FtabDyn = sofs:to_external(FtabDyn0),
    WeDyn = wings_material:cleanup(We#we{fs=gb_trees:from_orddict(FtabDyn)}),

    StaticVs0 = sofs:to_external(sofs:difference(AllVs, Vs)),
    StaticVs = sort(insert_vtx_data(StaticVs0, We#we.vp, [])),
    DynPlan = wings_draw_util:prepare(FtabDyn, We, St),
    Split = Split0#split{static_vs=StaticVs,dyn_vs=DynVs,
			 dyn_faces=Faces,dyn_plan=DynPlan,
			 orig_we=We},
    #dlo{work=[List],mirror=M,vs=VsDlist,
	 src_sel=Sel,src_we=WeDyn,split=Split,proxy_data=Pd}.

original_we(#dlo{split=#split{orig_we=We}}) -> We;
original_we(#dlo{src_we=We}) -> We.

update_dynamic(#dlo{work=[_|_],src_we=We}=D, Vtab) when ?IS_LIGHT(We) ->
    wings_light:update_dynamic(D, Vtab);
update_dynamic(#dlo{work=[Work|_],vs=VsList0,
		    src_we=We0,split=Split}=D, Vtab0) ->
    #split{static_vs=StaticVs,dyn_vs=DynVs,dyn_plan=DynPlan} = Split,
    Vtab = gb_trees:from_orddict(merge([sort(Vtab0),StaticVs])),
    We = We0#we{vp=Vtab},
    Dl = draw_faces(DynPlan, We),
    VsList = update_dynamic_vs(VsList0, DynVs, We),
    D#dlo{work=[Work,Dl],vs=VsList,src_we=We}.

update_dynamic_vs(VsList, none, _) -> VsList;
update_dynamic_vs([Static|_], DynVs, #we{vp=Vtab}) ->
    UnselDlist = gl:genLists(1),
    gl:newList(UnselDlist, ?GL_COMPILE),
    case wings_pref:get_value(vertex_size) of
	0.0 -> ok;
	PtSize -> 
	    gl:pointSize(PtSize),
	    gl:color3f(0, 0, 0),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun(V) ->
			    gl:vertex3fv(gb_trees:get(V, Vtab))
		    end, DynVs),
	    gl:'end'()
    end,
    gl:endList(),
    [Static,UnselDlist].

insert_vtx_data([V|Vs], Vtab, Acc) ->
    insert_vtx_data(Vs, Vtab, [{V,gb_trees:get(V, Vtab)}|Acc]);
insert_vtx_data([], _, Acc) -> Acc.

split_vs_dlist(DynVs, {vertex,SelVs0}, #we{vp=Vtab}) ->
    SelVs = sofs:from_external(gb_sets:to_list(SelVs0), [vertex]),
    UnselDyn0 = sofs:difference(DynVs, SelVs),
    UnselDyn = sofs:to_external(UnselDyn0),
    UnselDlist = gl:genLists(1),
    gl:newList(UnselDlist, ?GL_COMPILE),
    case wings_pref:get_value(vertex_size) of
	0.0 -> ok;
	PtSize -> 
	    gl:pointSize(PtSize),
	    gl:color3f(0, 0, 0),
	    gl:'begin'(?GL_POINTS),
	    List0 = sofs:from_external(gb_trees:to_list(Vtab), [{vertex,info}]),
	    List1 = sofs:drestriction(List0, DynVs),
	    List = sofs:to_external(List1),
	    foreach(fun({_,Pos}) ->
			    gl:vertex3fv(Pos)
		    end, List),
	    gl:'end'()
    end,
    gl:endList(),
    {UnselDyn,[UnselDlist]};
split_vs_dlist(_, _, _) -> {none,none}.

static_dlist(Faces, Ftab, We, St) ->
    StaticFtab = sofs:to_external(sofs:drestriction(Ftab, Faces)),
    draw_faces(StaticFtab, We, St).

%%%
%%% Drawing routines for workmode.
%%%

draw_faces(Ftab, We, St) ->
    draw_faces(wings_draw_util:prepare(Ftab, We, St), We).

draw_faces({uv,MatFaces,St}, We) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    draw_uv_faces(MatFaces, We, St),
    gl:endList(),
    Dl;
draw_faces({material,MatFaces,St}, We) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    draw_mat_faces(MatFaces, We, St),
    gl:endList(),
    Dl;
draw_faces({color,Colors,#st{mat=Mtab}}, We) ->
    BasicFaces = gl:genLists(2),
    Dl = BasicFaces+1,
    gl:newList(BasicFaces, ?GL_COMPILE),
    draw_vtx_faces(Colors, We),
    gl:endList(),
    
    gl:newList(Dl, ?GL_COMPILE),
    wings_material:apply_material(default, Mtab),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    gl:callList(BasicFaces),
    gl:disable(?GL_COLOR_MATERIAL),
    gl:endList(),

    {call,Dl,BasicFaces}.

draw_vtx_faces({Same,Diff}, We) ->
    Draw = fun() ->
		   Tess = wings_draw_util:tess(),
		   glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
		   draw_vtx_faces_1(Same, We),
		   glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
		   draw_vtx_faces_3(Diff, We)
	   end,
    wings_draw_util:begin_end(Draw).

draw_vtx_faces_1([{Col,Faces}|Fs], We) ->
    gl:color3fv(Col),
    draw_vtx_faces_2(Faces, We),
    draw_vtx_faces_1(Fs, We);
draw_vtx_faces_1([], _) -> ok.

draw_vtx_faces_2([F|Fs], We) ->
    wings_draw_util:flat_face(F, We),
    draw_vtx_faces_2(Fs, We);
draw_vtx_faces_2([], _) -> ok.

draw_vtx_faces_3([F|Fs], We) ->
    wings_draw_util:face(F, We),
    draw_vtx_faces_3(Fs, We);
draw_vtx_faces_3([], _) -> ok.

draw_uv_faces(MatFaces, We, St) ->
    wings_draw_util:mat_faces(MatFaces, ?GL_TRIANGLES, We, St, fun draw_uv_faces_fun/2).

draw_uv_faces_fun([{Face,Edge}|Fs], We) ->
    wings_draw_util:face(Face, Edge, We),
    draw_uv_faces_fun(Fs, We);
draw_uv_faces_fun([], _We) -> ok.

draw_mat_faces(MatFaces, We, St) ->
    Tess = wings_draw_util:tess(),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
    wings_draw_util:mat_faces(MatFaces, ?GL_TRIANGLES, We, St, fun draw_mat_faces_fun/2),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA).

draw_mat_faces_fun([{Face,Edge}|Fs], We) ->
    wings_draw_util:flat_face(Face, Edge, We),
    draw_mat_faces_fun(Fs, We);
draw_mat_faces_fun([], _We) -> ok.

%%%
%%% Smooth drawing.
%%%

smooth_dlist(#we{he=Htab0}=We, St) ->
    case check_mirror(We) of
	none ->
	    Flist = wings_we:normals(We),
	    smooth_faces(Flist, We, St);
	{Face} ->
	    Edges = wings_face:outer_edges([Face], We),
	    Htab = gb_sets:union(Htab0, gb_sets:from_list(Edges)),
	    Flist = wings_we:normals(We#we{he=Htab}),
	    smooth_faces(Flist, We, St)
    end.

smooth_faces(Ftab, We, St) ->
    smooth_faces(wings_draw_util:prepare(Ftab, We, St)).

smooth_faces({uv,MatFaces,St}) ->
    draw_smooth_faces(fun draw_smooth_uv_face/2, MatFaces, St);
smooth_faces({material,MatFaces,St}) ->
    draw_smooth_faces(fun draw_smooth_mat_face/2, MatFaces, St);
smooth_faces({color,{Same,Diff},#st{mat=Mtab}}) ->
    ListOp = gl:genLists(1),
    gl:newList(ListOp, ?GL_COMPILE),
    wings_material:apply_material(default, Mtab),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    Draw = fun() ->
		   Tess = wings_draw_util:tess(),
		   draw_smooth_vtx_faces_1(Same, fun draw_smooth_mat_face/2, Tess),
		   do_draw_smooth_1(Diff, fun draw_smooth_color_face/2, Tess)
	   end,
    wings_draw_util:begin_end(Draw),
    gl:disable(?GL_COLOR_MATERIAL),
    gl:endList(),
    {[ListOp,none],false}.

draw_smooth_vtx_faces_1([{Col,Faces}|MatFaces], DrawFace, Tess) ->
    gl:color3fv(Col),
    do_draw_smooth_1(Faces, DrawFace, Tess),
    draw_smooth_vtx_faces_1(MatFaces, DrawFace, Tess);
draw_smooth_vtx_faces_1([], _, _) -> ok.

draw_smooth_faces(DrawFace, Flist, #st{mat=Mtab}) ->
    ListOp = gl:genLists(1),
    gl:newList(ListOp, ?GL_COMPILE),
    Trans = draw_smooth_opaque(Flist, DrawFace, Mtab, []),
    gl:endList(),
    if
	Trans =:= [] ->
	    {[ListOp,none],false};
	true ->
	    ListTr = gl:genLists(1),
	    gl:newList(ListTr, ?GL_COMPILE),
	    draw_smooth_tr(Flist, DrawFace, Mtab),
	    gl:endList(),
	    {[ListOp,ListTr],true}
    end.

draw_smooth_opaque([{M,Faces}=MatFaces|T], DrawFace, Mtab, Acc) ->
    case wings_material:is_transparent(M, Mtab) of
	true ->
	    draw_smooth_opaque(T, DrawFace, Mtab, [MatFaces|Acc]);
	false ->
	    do_draw_smooth(DrawFace, M, Faces, Mtab),
	    draw_smooth_opaque(T, DrawFace, Mtab, Acc)
    end;
draw_smooth_opaque([], _, _, Acc) -> Acc.

draw_smooth_tr([{M,Faces}|T], DrawFace, Mtab) ->
    do_draw_smooth(DrawFace, M, Faces, Mtab),
    draw_smooth_tr(T, DrawFace, Mtab);
draw_smooth_tr([], _, _) -> ok.

do_draw_smooth(DrawFace, Mat, Faces, Mtab) ->
    gl:pushAttrib(?GL_TEXTURE_BIT),
    wings_material:apply_material(Mat, Mtab),
    wings_draw_util:begin_end(
      fun() ->
	      Tess = wings_draw_util:tess(),
	      do_draw_smooth_1(Faces, DrawFace, Tess)
      end),
    gl:popAttrib().

do_draw_smooth_1([{_,{{X,Y,Z},Vs}}|Fs], DrawFace, Tess) ->
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    DrawFace(Vs, Tess),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    do_draw_smooth_1(Fs, DrawFace, Tess);
do_draw_smooth_1([], _, _) -> ok.

draw_smooth_uv_face([{P,{_,_}=UV,N}|Vs], Tess) ->
    glu:tessVertex(Tess, P, [{normal,N},{texcoord2,UV}]),
    draw_smooth_uv_face(Vs, Tess);
draw_smooth_uv_face([], _) -> ok.

draw_smooth_color_face([{P,{_,_,_}=Color,N}|Vs], Tess) ->
    glu:tessVertex(Tess, P, [{color,Color},{normal,N}]),
    draw_smooth_color_face(Vs, Tess);
draw_smooth_color_face([], _) -> ok.

draw_smooth_mat_face([{P,_,N}|Vs], Tess) ->
    glu:tessVertex(Tess, P, [{normal,N}]),
    draw_smooth_mat_face(Vs, Tess);
draw_smooth_mat_face([], _) -> ok.

%%
%% Draw normals for the selected elements.
%%

make_normals_dlist(#dlo{normals=none,src_we=We,src_sel={Mode,Elems}}=D) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    make_normals_dlist_1(Mode, Elems, We),
    gl:'end'(),
    gl:endList(),
    D#dlo{normals=List};
make_normals_dlist(#dlo{src_sel=none}=D) -> D#dlo{normals=none};
make_normals_dlist(D) -> D.

make_normals_dlist_1(vertex, Vs, #we{vp=Vtab}=We) ->
    foreach(fun(V) ->
		    Pos = gb_trees:get(V, Vtab),
		    gl:vertex3fv(Pos),
		    N = wings_vertex:normal(V, We),
		    gl:vertex3fv(e3d_vec:add(Pos, e3d_vec:mul(N, 0.3)))
	    end, gb_sets:to_list(Vs));
make_normals_dlist_1(edge, Edges, #we{es=Etab,vp=Vtab}=We) ->
    Et0 = sofs:relation(gb_trees:to_list(Etab), [{edge,data}]),
    Es = sofs:from_external(gb_sets:to_list(Edges), [edge]),
    Et1 = sofs:restriction(Et0, Es),
    Et = sofs:to_external(Et1),
    foreach(fun({_,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}}) ->
		    PosA = gb_trees:get(Va, Vtab),
		    PosB = gb_trees:get(Vb, Vtab),
		    Mid = e3d_vec:average([PosA,PosB]),
		    gl:vertex3fv(Mid),
		    N = e3d_vec:average([wings_face:normal(Lf, We),
					 wings_face:normal(Rf, We)]),
		    gl:vertex3fv(e3d_vec:add(Mid, e3d_vec:mul(N, 0.3)))
	    end, Et);
make_normals_dlist_1(face, Faces, We) ->
    foreach(fun(Face) ->
		    Vs = wings_face:vertices_cw(Face, We),
		    C = wings_vertex:center(Vs, We),
		    gl:vertex3fv(C),
		    N = wings_face:face_normal_cw(Vs, We),
		    gl:vertex3fv(e3d_vec:add(C, e3d_vec:mul(N, 0.3)))
	    end, gb_sets:to_list(Faces));
make_normals_dlist_1(body, _, #we{fs=Ftab}=We) ->
    make_normals_dlist_1(face, gb_sets:from_list(gb_trees:keys(Ftab)), We).
