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
%%     $Id: wings_draw.erl,v 1.145 2003/08/29 10:40:28 bjorng Exp $
%%

-module(wings_draw).
-export([invalidate_dlists/1,invalidate_dlists/2,update_dlists/1,update_sel_dlist/0,
	 changed_we/2,split/3,original_we/1,update_dynamic/2,join/1,
	 update_mirror/0,smooth_dlist/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,reverse/2,foldl/3,merge/1,sort/1,any/2]).

-record(split,
	{static_vs,
	 dyn_vs,
	 dyn_faces,
	 dyn_plan,				%Plan for drawing dynamic faces.
	 v2f=none,				%Vertex => face
	 f2v=none,				%Face => vertex
	 orig_ns,
	 orig_we
	}).

%%%
%%% Update display lists.
%%%

update_dlists(St) ->
    invalidate_dlists(St),
    do_update_dlists(St),
    update_sel_dlist(),
    update_mirror().

invalidate_dlists(St) ->
    invalidate_dlists(true, St).

invalidate_dlists(Update, #st{selmode=Mode,sel=Sel}=St) ->
    prepare_dlists(Update, St),
    case wings_draw_util:changed_materials(St) of
	[] -> ok;
	ChangedMat -> invalidate_by_mat(ChangedMat)
    end,
    wings_draw_util:map(fun(D, Data) ->
				sel_fun(D, Data, Mode)
			end, Sel).

prepare_dlists(Update, #st{shapes=Shs}) ->
    wings_draw_util:update(fun(D, A) -> prepare_fun(D, Update, A) end,
			   gb_trees:values(Shs)).

prepare_fun(eol, _, [#we{perm=Perm}=We|Wes]) when ?IS_NOT_VISIBLE(Perm) ->
    {new_we(#dlo{src_we=empty_we(We)}),Wes};
prepare_fun(eol, _, [We|Wes]) ->
    {new_we(#dlo{src_we=We}),Wes};
prepare_fun(eol, _, []) ->
    eol;
prepare_fun(#dlo{src_we=We,split=#split{}=Split}=D, _, [We|Wes]) ->
    {D#dlo{src_we=We,split=Split#split{orig_we=We}},Wes};
prepare_fun(#dlo{src_we=We}=D, _, [We|Wes]) ->
    {D#dlo{src_we=We},Wes};
prepare_fun(#dlo{src_we=#we{id=Id},proxy_data=Proxy}=D, Update,
	    [#we{id=Id,perm=Perm}=We|Wes]) ->
    if 
	?IS_VISIBLE(Perm) ->
	    {changed_we(D, Update, #dlo{src_we=We,mirror=none,proxy_data=Proxy}),Wes};
	true ->
	    {changed_we(D, Update, #dlo{src_we=empty_we(We),proxy_data=Proxy}),Wes}
    end;
prepare_fun(#dlo{}, _, Wes) ->
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

sel_fun(#dlo{src_we=#we{id=Id},src_sel=SrcSel}=D, [{Id,Items}|Sel], Mode) ->
    case SrcSel of
	{Mode,Items} -> {D,Sel};
	_ -> {D#dlo{sel=none,normals=none,src_sel={Mode,Items}},Sel}
    end;
sel_fun(D, Sel, _) ->
    {D#dlo{sel=none,src_sel=none},Sel}.

new_we(#dlo{src_we=We}=D) ->
    Ns = changed_we_1(none, We),
    D#dlo{ns=Ns}.
    
changed_we(D0, D1) ->
    changed_we(D0, true, D1).

changed_we(#dlo{ns=Ns0}, true, #dlo{src_we=We}=D) ->
    Ns = changed_we_1(Ns0, We),
    D#dlo{ns=Ns};
changed_we(#dlo{ns=Ns}, false, D) -> D#dlo{ns=Ns}.

changed_we_1(none, #we{fs=Ftab}=We) ->
    changed_we_2(gb_trees:to_list(Ftab), [], We, []);
changed_we_1(Ns, #we{fs=Ftab}=We) ->
    changed_we_2(gb_trees:to_list(Ftab), gb_trees:to_list(Ns), We, []).

changed_we_2([{Face,Edge}|Fs], [{Face,Data}=Pair|Ns], We, Acc) ->
    Ps = changed_we_pos(Face, Edge, We),
    case Data of
	[_|Ps] ->
	    changed_we_2(Fs, Ns, We, [Pair|Acc]);
	{_,Ps} ->
	    changed_we_2(Fs, Ns, We, [Pair|Acc]);
	_ ->
	    N = e3d_vec:normal(Ps),
	    case Ps of
		[_,_,_] ->
		    changed_we_2(Fs, Ns, We, [{Face,[N|Ps]}|Acc]);
		[A,B,C,D] ->
		    case wings_draw_util:good_triangulation(N, A, B, C, D) of
			false ->
			    changed_we_2(Fs, Ns, We, [{Face,{N,Ps}}|Acc]);
			true ->
			    changed_we_2(Fs, Ns, We, [{Face,[N|Ps]}|Acc])
		    end;
		_ ->
		    changed_we_2(Fs, Ns, We, [{Face,{N,Ps}}|Acc])
	    end
    end;
changed_we_2([{Fa,_}|_]=Fs, [{Fb,_}|Ns], We, Acc) when Fa > Fb ->
    changed_we_2(Fs, Ns, We, Acc);
changed_we_2([{Face,Edge}|Fs], Ns, We, Acc) ->
    Ps = changed_we_pos(Face, Edge, We),
    N = e3d_vec:normal(Ps),
    case Ps of
	[_,_,_] ->
	    changed_we_2(Fs, Ns, We, [{Face,[N|Ps]}|Acc]);
	[A,B,C,D] ->
	    case wings_draw_util:good_triangulation(N, A, B, C, D) of
		false ->
		    changed_we_2(Fs, Ns, We, [{Face,{N,Ps}}|Acc]);
		true ->
		    changed_we_2(Fs, Ns, We, [{Face,[N|Ps]}|Acc])
	    end;
	_ ->
	    changed_we_2(Fs, Ns, We, [{Face,{N,Ps}}|Acc])
    end;
changed_we_2([], _, _, Acc) -> gb_trees:from_orddict(reverse(Acc)).

changed_we_pos(Face, Edge, #we{vp=Vtab}=We) ->
    Vs = wings_face:vertices_cw(Face, Edge, We),
    changed_we_pos_1(Vs, Vtab, []).

changed_we_pos_1([V|Vs], Vtab, Acc) ->
    changed_we_pos_1(Vs, Vtab, [gb_trees:get(V, Vtab)|Acc]);
changed_we_pos_1([], _, VsPos) -> VsPos.

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
update_fun_2(work, #dlo{work=none,src_we=#we{fs=Ftab}}=D, St) ->
    Dl = draw_faces(gb_trees:to_list(Ftab), D, St),
    D#dlo{work=Dl};
update_fun_2(smooth, #dlo{smooth=none}=D, St) ->
    We = wings_subdiv:smooth_we(D),
    {List,Tr} = smooth_dlist(We, St),
    D#dlo{smooth=List,transparent=Tr};
update_fun_2({vertex,PtSize}, #dlo{vs=none,src_we=#we{vp=Vtab}}=D, _) ->
    UnselDlist = gl:genLists(1),
    gl:newList(UnselDlist, ?GL_COMPILE),
    gl:pointSize(PtSize),
    gl:color3b(0, 0, 0),
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
update_sel(#dlo{sel=none,src_sel={face,Faces},src_we=#we{fs=Ftab}}=D) ->
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
		      foreach(fun(Face) ->
				      wings_draw_util:unlit_face(Face, D)
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
update_sel_all(#dlo{src_we=#we{fs=Ftab}=We,work=none}=D) ->
    Tess = wings_draw_util:tess(),
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
    wings_draw_util:begin_end(
      fun() ->
	      foreach(fun({Face,Edge}) ->
			      wings_draw_util:unlit_face(Face, Edge, We)
		      end, gb_trees:to_list(Ftab))
      end),
    gl:endList(),
    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
    D#dlo{sel=List};
update_sel_all(#dlo{work=Faces}=D) ->
    D#dlo{sel=Faces}.

update_mirror() ->
    wings_draw_util:map(fun update_mirror/2, []).

update_mirror(#dlo{mirror=none,src_we=#we{mirror=none}}=D, _) -> D;
update_mirror(#dlo{mirror=none,src_we=#we{fs=Ftab,mirror=Face}=We}=D, _) ->
    case gb_trees:is_defined(Face, Ftab) of
	false ->
	    D#dlo{mirror=none};
	true ->
	    N = wings_face:normal(Face, We),
	    Center = wings_face:center(Face, We),
	    RotBack = e3d_mat:rotate_to_z(N),
	    Rot = e3d_mat:transpose(RotBack),
	    Mat0 = e3d_mat:mul(e3d_mat:translate(Center), Rot),
	    Mat1 = e3d_mat:mul(Mat0, e3d_mat:scale(1.0, 1.0, -1.0)),
	    Mat2 = e3d_mat:mul(Mat1, RotBack),
	    Mat = e3d_mat:mul(Mat2, e3d_mat:translate(e3d_vec:neg(Center))),
	    D#dlo{mirror=Mat}
    end;
update_mirror(D, _) -> D.

%%%
%%% Splitting of objects into two display lists.
%%%

split(#dlo{split=#split{orig_we=#we{}=We,orig_ns=Ns}=Split}=D, Vs, St) ->
    split(D#dlo{src_we=We,ns=Ns}, Split, Vs, St);
split(D, Vs, St) ->
    split(D, #split{dyn_faces=sofs:set([], [face])}, Vs, St).

split(#dlo{src_we=#we{fs=Ftab}=We}=D, #split{v2f=none}=Split, Vs, St) ->
    %% Efficiency note: Looping over the face table is slower than
    %% looping over the edge table, but sofs:relation/2 will be
    %% considerable faster, because an edge table loop will construct
    %% a list that is twice as long (with duplicates).
    F2V0 = wings_face:fold_faces(fun(F, V, _, _, A) ->
					 [{F,V}|A]
				 end, [], gb_trees:keys(Ftab), We),
    F2V = sofs:relation(F2V0, [{face,vertex}]),
    V2F = sofs:converse(F2V),
    split(D, Split#split{v2f=V2F,f2v=F2V}, Vs, St);
split(#dlo{mirror=M,src_sel=Sel,src_we=#we{fs=Ftab0}=We,proxy_data=Pd,ns=Ns0}=D,
      #split{v2f=V2F,f2v=F2V,dyn_faces=Faces0}=Split0, Vs0, St) ->
    Vs = sofs:set(Vs0, [vertex]),
    Faces1 = sofs:image(V2F, Vs),
    Ftab = sofs:from_external(gb_trees:to_list(Ftab0), [{face,data}]),
    Faces = case sofs:is_subset(Faces1, Faces0) of
		true ->
		    FtabDyn0 = sofs:restriction(Ftab, Faces0),
		    case D#dlo.work of
			[List|_] -> Faces0;
			List when is_integer(List) -> Faces0
		    end;
		false ->
		    {FtabDyn0,StaticFtab0} = sofs:partition(1, Ftab, Faces1),
		    StaticFtab = sofs:to_external(StaticFtab0),
		    List = draw_faces(StaticFtab, D, St),
		    Faces1
	    end,
    AllVs = sofs:image(F2V, Faces),
    
    {DynVs,VsDlist} = split_vs_dlist(AllVs, Sel, We),

    FtabDyn = sofs:to_external(FtabDyn0),
    WeDyn = wings_material:cleanup(We#we{fs=gb_trees:from_orddict(FtabDyn)}),

    StaticVs0 = sofs:to_external(sofs:difference(AllVs, Vs)),
    StaticVs = sort(insert_vtx_data(StaticVs0, We#we.vp, [])),
    DynPlan = wings_draw_util:prepare(FtabDyn, We, St),
    Split = Split0#split{static_vs=StaticVs,dyn_vs=DynVs,
			 dyn_faces=Faces,dyn_plan=DynPlan,
			 orig_ns=Ns0,orig_we=We},
    #dlo{work=[List],mirror=M,vs=VsDlist,
	 src_sel=Sel,src_we=WeDyn,split=Split,proxy_data=Pd}.

original_we(#dlo{split=#split{orig_we=We}}) -> We;
original_we(#dlo{src_we=We}) -> We.

update_dynamic(#dlo{work=[_|_],src_we=We}=D, Vtab) when ?IS_LIGHT(We) ->
    wings_light:update_dynamic(D, Vtab);
update_dynamic(#dlo{work=[Work|_],vs=VsList0,
		    src_we=We0,split=Split}=D0, Vtab0) ->
    #split{static_vs=StaticVs,dyn_vs=DynVs,dyn_plan=DynPlan} = Split,
    Vtab = gb_trees:from_orddict(merge([sort(Vtab0),StaticVs])),
    We = We0#we{vp=Vtab},
    D = changed_we(D0, D0),
    Dl = draw_faces(DynPlan, D),
    VsList = update_dynamic_vs(VsList0, DynVs, We),
    update_dynamic_1(D#dlo{work=[Work,Dl],vs=VsList,src_we=We}).

update_dynamic_1(#dlo{work=Faces,src_we=#we{mode=vertex}}=D) ->
    Dl = wings_draw_util:force_flat_color(Faces, wings_pref:get_value(edge_color)),
    D#dlo{edges=Dl};
update_dynamic_1(D) -> D.

update_dynamic_vs(VsList, none, _) -> VsList;
update_dynamic_vs([Static|_], DynVs, #we{vp=Vtab}) ->
    UnselDlist = gl:genLists(1),
    gl:newList(UnselDlist, ?GL_COMPILE),
    gl:pointSize(wings_pref:get_value(vertex_size)),
    gl:color3b(0, 0, 0),
    gl:'begin'(?GL_POINTS),
    foreach(fun(V) ->
		    gl:vertex3fv(gb_trees:get(V, Vtab))
	    end, DynVs),
    gl:'end'(),
    gl:endList(),
    [Static,UnselDlist].

insert_vtx_data([V|Vs], Vtab, Acc) ->
    insert_vtx_data(Vs, Vtab, [{V,gb_trees:get(V, Vtab)}|Acc]);
insert_vtx_data([], _, Acc) -> Acc.

split_vs_dlist(DynVs, {vertex,SelVs0}, #we{vp=Vtab}) ->
    case wings_pref:get_value(vertex_size) of
	0.0 -> {none,none};
	PtSize -> 
	    SelVs = sofs:from_external(gb_sets:to_list(SelVs0), [vertex]),
	    UnselDyn0 = sofs:difference(DynVs, SelVs),
	    UnselDyn = sofs:to_external(UnselDyn0),
	    UnselDlist = gl:genLists(1),
	    gl:newList(UnselDlist, ?GL_COMPILE),
	    gl:pointSize(PtSize),
	    gl:color3b(0, 0, 0),
	    gl:'begin'(?GL_POINTS),
	    List0 = sofs:from_external(gb_trees:to_list(Vtab), [{vertex,info}]),
	    List1 = sofs:drestriction(List0, DynVs),
	    List = sofs:to_external(List1),
	    foreach(fun({_,Pos}) ->
			    gl:vertex3fv(Pos)
		    end, List),
	    gl:'end'(),
	    gl:endList(),
	    {UnselDyn,[UnselDlist]}
    end;
split_vs_dlist(_, _, _) -> {none,none}.

%% Re-join display lists that have been split.
join(#dlo{src_we=#we{vp=Vtab0},ns=Ns1,split=#split{orig_we=We0,orig_ns=Ns0}}=D0) ->
    #we{vp=OldVtab} = We0,

    %% Heuristic for break-even. (Note that we don't know the exact number
    %% of vertices that will be updated.)
    Break = round(16*math:log(gb_trees:size(OldVtab)+1)/math:log(2)+0.5),
    Vtab = case gb_trees:size(Vtab0) of
	       Sz when Sz =< Break ->
		   %% Update the gb_tree to allow sharing with the undo list.
		   Vt = join_update(Vtab0, OldVtab),
%  		   io:format("cmp: ~p% \n", [round(100*cmp(Vt, OldVtab)/
%  						   gb_trees:size(OldVtab))]),
		   Vt;
	       _Sz ->
		   %% Too much updated - faster to rebuild the gb_tree.
		   %% (There would not have been much sharing anyway.)
		   join_rebuild(Vtab0, OldVtab)
	   end,
%     io:format("~p ~p\n", [erts_debug:size([OldVtab,Vtab]),
% 			   erts_debug:flat_size([OldVtab,Vtab])]),
    We = We0#we{vp=Vtab},
    Ns = join_ns(Ns0, Ns1),
    D = D0#dlo{vs=none,drag=none,sel=none,split=none,src_we=We,ns=Ns},
    changed_we(D, D).

join_ns(Ns0, Ns1) ->
    join_ns_1(gb_trees:to_list(Ns0), gb_trees:to_list(Ns1), []).

join_ns_1([{Face,_}|Fs0], [{Face,_}=El|Fs1], Acc) ->
    join_ns_1(Fs0, Fs1, [El|Acc]);
join_ns_1([El|Fs0], Fs1, Acc) ->
    join_ns_1(Fs0, Fs1, [El|Acc]);
join_ns_1([], Fs, Acc) ->
    gb_trees:from_orddict(reverse(Acc, Fs)).

join_update(New, Old) ->
    join_update(gb_trees:to_list(New), gb_trees:to_list(Old), Old).

join_update([Same|New], [Same|Old], Acc) ->
    join_update(New, Old, Acc);
join_update([{V,P0}|New], [{V,OldP}|Old], Acc) ->
    P = tricky_share(P0, OldP),
    join_update(New, Old, gb_trees:update(V, P, Acc));
join_update(New, [_|Old], Acc) ->
    join_update(New, Old, Acc);
join_update([], _, Acc) -> Acc.

join_rebuild(New, Old) ->
    join_rebuild(gb_trees:to_list(New), gb_trees:to_list(Old), []).

join_rebuild([N|New], [O|Old], Acc) when N =:= O ->
    join_rebuild(New, Old, [O|Acc]);
join_rebuild([{V,P0}|New], [{V,OldP}|Old], Acc) ->
    P = tricky_share(P0, OldP),
    join_rebuild(New, Old, [{V,P}|Acc]);
join_rebuild(New, [O|Old], Acc) ->
    join_rebuild(New, Old, [O|Acc]);
join_rebuild([], Old, Acc) ->
    gb_trees:from_orddict(reverse(Acc, Old)).

%% Too obvious to comment.
tricky_share({X,Y,Z}=New, {OldX,OldY,OldZ})
  when X =/= OldX, Y =/= OldY, Z =/= OldZ -> New;
tricky_share({X,Y,Z}, {X,Y,_}=Old) ->
    setelement(3, Old, Z);
tricky_share({X,Y,Z}, {X,_,Z}=Old) ->
    setelement(2, Old, Y);
tricky_share({X,Y,Z}, {_,Y,Z}=Old) ->
    setelement(1, Old, X);
tricky_share({X,Y,Z}, {X,_,_}=Old) ->
    {element(1, Old),Y,Z};
tricky_share({X,Y,Z}, {_,Y,_}=Old) ->
    {X,element(2, Old),Z};
tricky_share({X,Y,Z}, {_,_,Z}=Old) ->
    {X,Y,element(3, Old)}.

% cmp({S,New}, {S,Old}) ->
%     cmp(New, Old, 0).

% cmp({_,_,NewSmaller,NewBigger}=New, {_,_,OldSmaller,OldBigger}=Old, N0) ->
%     N1 = case erts_debug:same(New, Old) of
% 	     false -> N0;
% 	     true -> N0+1
% 	 end,
%     N = cmp(NewSmaller, OldSmaller, N1),
%     cmp(NewBigger, OldBigger, N);
% cmp(nil, nil, N) -> N.

%%%
%%% Drawing routines for workmode.
%%%

draw_faces(Ftab, D, St) ->
    draw_faces(wings_draw_util:prepare(Ftab, D, St), D).

draw_faces({material,MatFaces,St}, D) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    mat_faces(MatFaces, D, St),
    gl:endList(),
    Dl;
draw_faces({color,Colors,#st{mat=Mtab}}, D) ->
    BasicFaces = gl:genLists(2),
    Dl = BasicFaces+1,
    gl:newList(BasicFaces, ?GL_COMPILE),
    draw_vtx_faces(Colors, D),
    gl:endList(),
    
    gl:newList(Dl, ?GL_COMPILE),
    wings_material:apply_material(default, Mtab),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    gl:callList(BasicFaces),
    gl:disable(?GL_COLOR_MATERIAL),
    gl:endList(),

    {call,Dl,BasicFaces}.

draw_vtx_faces({Same,Diff}, D) ->
    Draw = fun() ->
		   Tess = wings_draw_util:tess(),
		   glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
		   draw_vtx_faces_1(Same, D),
		   glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA),
		   draw_vtx_faces_3(Diff, D)
	   end,
    wings_draw_util:begin_end(Draw).

draw_vtx_faces_1([{none,Faces}|Fs], D) ->
    gl:color3f(1.0, 1.0, 1.0),
    draw_vtx_faces_2(Faces, D),
    draw_vtx_faces_1(Fs, D);
draw_vtx_faces_1([{Col,Faces}|Fs], D) ->
    gl:color3fv(Col),
    draw_vtx_faces_2(Faces, D),
    draw_vtx_faces_1(Fs, D);
draw_vtx_faces_1([], _) -> ok.

draw_vtx_faces_2([F|Fs], D) ->
    wings_draw_util:plain_face(F, D),
    draw_vtx_faces_2(Fs, D);
draw_vtx_faces_2([], _) -> ok.

draw_vtx_faces_3([F|Fs], D) ->
    wings_draw_util:vcol_face(F, D),
    draw_vtx_faces_3(Fs, D);
draw_vtx_faces_3([], _) -> ok.

%%%
%%% Set material and draw faces.
%%%

mat_faces(List, We, #st{mat=Mtab}) ->
    mat_faces_1(List, We, Mtab);
mat_faces(List, We, Mtab) ->
    mat_faces_1(List, We, Mtab).
    
mat_faces_1([{Mat,Faces}|T], We, Mtab) ->
    gl:pushAttrib(?GL_TEXTURE_BIT),
    case wings_material:apply_material(Mat, Mtab) of
	false ->
	    Tess = wings_draw_util:tess(),
	    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_GLVERTEX),
	    wings_draw_util:begin_end(fun() -> draw_mat_faces(Faces, We) end),
	    glu:tessCallback(Tess, ?GLU_TESS_VERTEX, ?ESDL_TESSCB_VERTEX_DATA);
	true ->
	    wings_draw_util:begin_end(fun() -> draw_uv_faces(Faces, We) end)
    end,
    gl:edgeFlag(?GL_TRUE),
    gl:popAttrib(),
    mat_faces_1(T, We, Mtab);
mat_faces_1([], _, _) -> ok.

draw_mat_faces([{Face,Edge}|Fs], We) ->
    wings_draw_util:plain_face(Face, Edge, We),
    draw_mat_faces(Fs, We);
draw_mat_faces([], _) -> ok.

draw_uv_faces([{Face,Edge}|Fs], We) ->
    wings_draw_util:uv_face(Face, Edge, We),
    draw_uv_faces(Fs, We);
draw_uv_faces([], _) -> ok.

%%%
%%% Smooth drawing.
%%%

smooth_dlist(#we{he=Htab0,fs=Ftab,mirror=Face}=We, St) ->
    case gb_trees:is_defined(Face, Ftab) of
	false ->
	    Flist = wings_we:normals(We),
	    smooth_faces(Flist, We, St);
	true ->
	    Edges = wings_face:outer_edges([Face], We),
	    Htab = gb_sets:union(Htab0, gb_sets:from_list(Edges)),
	    Flist = wings_we:normals(We#we{he=Htab}),
	    smooth_faces(Flist, We, St)
    end.

smooth_faces(Ftab, We, St) ->
    smooth_faces(wings_draw_util:prepare(Ftab, We, St)).

smooth_faces({material,MatFaces,St}) ->
    draw_smooth_faces(fun draw_smooth_mat_faces/2, MatFaces, St);
smooth_faces({color,{Same,Diff},#st{mat=Mtab}}) ->
    ListOp = gl:genLists(1),
    gl:newList(ListOp, ?GL_COMPILE),
    wings_material:apply_material(default, Mtab),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    Draw = fun() ->
		   draw_smooth_vtx_faces_1(Same),
		   wings_draw_util:smooth_vcol_faces(Diff)
	   end,
    wings_draw_util:begin_end(Draw),
    gl:disable(?GL_COLOR_MATERIAL),
    gl:endList(),
    {[ListOp,none],false}.

draw_smooth_vtx_faces_1([{none,Faces}|MatFaces]) ->
    gl:color3f(1.0, 1.0, 1.0),
    wings_draw_util:smooth_mat_faces(Faces),
    draw_smooth_vtx_faces_1(MatFaces);
draw_smooth_vtx_faces_1([{Col,Faces}|MatFaces]) ->
    gl:color3fv(Col),
    wings_draw_util:smooth_mat_faces(Faces),
    draw_smooth_vtx_faces_1(MatFaces);
draw_smooth_vtx_faces_1([]) -> ok.

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

do_draw_smooth(DrawFaces, Mat, Faces, Mtab) ->
    gl:pushAttrib(?GL_TEXTURE_BIT),
    IsTxMaterial = wings_material:apply_material(Mat, Mtab),
    wings_draw_util:begin_end(
      fun() ->
	      DrawFaces(IsTxMaterial, Faces)
      end),
    gl:popAttrib().

draw_smooth_mat_faces(false, Fs) ->
    wings_draw_util:smooth_mat_faces(Fs);
draw_smooth_mat_faces(true, Fs) ->
    wings_draw_util:smooth_uv_faces(Fs).

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
