%%
%%  wings_draw.erl --
%%
%%     This module draws objects using OpenGL.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw.erl,v 1.89 2002/06/21 07:36:32 bjorng Exp $
%%

-module(wings_draw).
-export([update_dlists/1,update_sel_dlist/0,
	 split/3,original_we/1,update_dynamic/2,
	 update_mirror/0,
	 smooth_faces/2,
	 render/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1,foldl/3,merge/1,sort/1,any/2]).

-record(split,
	{static_vs,
	 dyn_vs,
	 dyn_faces,
	 dyn_ftab,
	 v2f=none,				%Vertex => face
	 f2v=none,				%Face => vertex
	 orig_we,
	 st}).

%%
%% Renders all shapes, including selections.
%%

render(St) ->
    update_dlists(St),
    wings_draw_util:render(St),
    St.

%%%
%%% Update display lists.
%%%

update_dlists(#st{selmode=Mode,sel=Sel}=St) ->
    prepare_dlists(St),
    wings_draw_util:map(fun(D, Data) ->
				sel_fun(D, Data, Mode)
			end, Sel),
    wings_draw_util:map(fun(D, _) ->
				update_fun(D, St)
			   end, []),
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
prepare_fun(#dlo{src_we=#we{id=Id},wire=W}, [#we{id=Id,perm=Perm}=We|Wes]) ->
    if 
	?IS_VISIBLE(Perm) ->
	    {#dlo{src_we=We,wire=W,mirror=check_mirror(We)},Wes};
	true ->
	    {#dlo{src_we=empty_we(We),wire=W},Wes}
    end;
prepare_fun(#dlo{}, Wes) ->
    {deleted,Wes}.

empty_we(We) ->
    Et = gb_trees:empty(),
    We#we{es=Et,fs=Et,vs=Et,he=gb_sets:empty()}.

check_mirror(#we{mirror=none}) -> none;
check_mirror(#we{fs=Ftab,mirror=Face}) ->
    case gb_trees:is_defined(Face, Ftab) of
	false -> none;
	true -> Face
    end.

sel_fun(#dlo{src_we=#we{id=Id},src_sel=SrcSel}=D, [{Id,Items}|Sel], Mode) ->
    case SrcSel of
	{Mode,Items} -> {D,Sel};
	_ -> {D#dlo{sel=none,normals=none,src_sel={Mode,Items}},Sel}
    end;
sel_fun(D, Sel, _) ->
    {D#dlo{sel=none,src_sel=none},Sel}.

update_fun(#dlo{work=none,src_we=#we{fs=Ftab}=We}=D, St) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    draw_faces(gb_trees:to_list(Ftab), We, St),
    gl:endList(),
    update_fun(D#dlo{work=List}, St);
update_fun(#dlo{vs=none,src_we=#we{vs=Vtab}}=D, #st{selmode=vertex}=St) ->
    UnselDlist = gl:genLists(1),
    gl:newList(UnselDlist, ?GL_COMPILE),
    case wings_pref:get_value(vertex_size) of
	0.0 -> ok;
	PtSize -> 
	    gl:pointSize(PtSize),
	    gl:color3f(0, 0, 0),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun(#vtx{pos=Pos}) -> gl:vertex3fv(Pos) end,
		    gb_trees:values(Vtab)),
	    gl:'end'()
    end,
    gl:endList(),
    update_fun(D#dlo{vs=UnselDlist}, St);
update_fun(D, St) ->
    update_fun_2(D, wings_pref:get_value(workmode), St).

update_fun_2(#dlo{smooth=none,src_we=We}=D, false, St) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    Tr = smooth_faces(We, St),
    gl:endList(),
    update_fun_3(D#dlo{smooth=List,transparent=Tr});
update_fun_2(#dlo{hard=none,src_we=#we{he=Htab}=We}=D, true, _) ->
    case gb_sets:is_empty(Htab) of
	true -> update_fun_3(D);
	false ->
	    List = gl:genLists(1),
	    gl:newList(List, ?GL_COMPILE),
	    gl:'begin'(?GL_LINES),
	    #we{es=Etab,vs=Vtab} = We,
	    foreach(fun(Edge) ->
			    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			    gl:vertex3fv(pos(Va, Vtab)),
			    gl:vertex3fv(pos(Vb, Vtab))
		    end, gb_sets:to_list(Htab)),
	    gl:'end'(),
	    gl:endList(),
	    update_fun_3(D#dlo{hard=List})
    end;
update_fun_2(D, _, _) -> update_fun_3(D).

update_fun_3(D) ->
    case wings_pref:get_value(show_normals) of
	false -> D;
	true -> make_normals_dlist(D)
    end.

update_sel_dlist() ->
    wings_draw_util:map(fun(D, _) ->
				update_sel(D)
			end, []).

update_sel(#dlo{work=Faces,sel=none,src_sel={body,_}}=D) ->
    {D#dlo{sel=Faces},[]};
update_sel(#dlo{sel=none,src_sel={face,Faces}}=D) ->
    #dlo{work=DlistFaces,src_we=#we{fs=Ftab}=We} = D,
    case gb_trees:size(Ftab) =:= gb_sets:size(Faces) of
	true ->
	    {D#dlo{sel=DlistFaces},[]};
	false ->
	    List = gl:genLists(1),
	    gl:newList(List, ?GL_COMPILE),
	    wings_draw_util:begin_end(
	      fun() ->
		      foreach(
			fun(Face) ->
				#face{edge=Edge} = gb_trees:get(Face, Ftab),
				wings_draw_util:flat_face(Face, Edge, We)
			end, gb_sets:to_list(Faces))
	      end),
	    gl:endList(),
	    {D#dlo{sel=List},[]}
    end;
update_sel(#dlo{sel=none,src_sel={edge,Edges}}=D) ->
    #dlo{src_we=#we{es=Etab,vs=Vtab}} = D,
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:'begin'(?GL_LINES),
    case gb_trees:size(Etab) =:= gb_sets:size(Edges) of
	true ->
	    foreach(fun(#edge{vs=Va,ve=Vb}) ->
			    gl:vertex3fv(pos(Va, Vtab)),
			    gl:vertex3fv(pos(Vb, Vtab))
		    end, gb_trees:values(Etab));
	false ->
	    foreach(fun(Edge) ->
			    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			    gl:vertex3fv(pos(Va, Vtab)),
			    gl:vertex3fv(pos(Vb, Vtab))
		    end, gb_sets:to_list(Edges))
    end,
    gl:'end'(),
    gl:endList(),
    {D#dlo{sel=List},[]};
update_sel(#dlo{sel=none,src_sel={vertex,Vs}}=D) ->
    #dlo{src_we=#we{vs=Vtab0}} = D,
    SelDlist = gl:genLists(1),
    case gb_trees:size(Vtab0) =:= gb_sets:size(Vs) of
	true ->
	    gl:newList(SelDlist, ?GL_COMPILE),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun(#vtx{pos=Pos}) -> gl:vertex3fv(Pos) end,
		    gb_trees:values(Vtab0)),
	    gl:'end'(),
	    gl:endList(),
	    {D#dlo{sel=SelDlist},[]};
	false ->
	    Vtab1 = gb_trees:to_list(Vtab0),
	    Vtab = sofs:from_external(Vtab1, [{vertex,data}]),
	    R = sofs:from_external(gb_sets:to_list(Vs), [vertex]),
	    Sel = sofs:to_external(sofs:restriction(Vtab, R)),
	    DrawFun = fun({_,#vtx{pos=Pos}}) -> gl:vertex3fv(Pos) end,

	    gl:newList(SelDlist, ?GL_COMPILE),
	    gl:'begin'(?GL_POINTS),
	    foreach(DrawFun, Sel),
	    gl:'end'(),
	    gl:endList(),

	    {D#dlo{sel=SelDlist},[]}
    end;
update_sel(#dlo{}=D) -> {D,[]}.

update_mirror() ->
    wings_draw_util:map(fun update_mirror/2, []).

update_mirror(#dlo{mirror=Face,src_we=We}=D, _) when is_integer(Face) ->
    N = wings_face:normal(Face, We),
    Vs = wings_face:surrounding_vertices(Face, We),
    Center = wings_vertex:center(Vs, We),
    RotBack = e3d_mat:rotate_to_z(N),
    Rot = e3d_mat:transpose(RotBack),
    Mat0 = e3d_mat:mul(e3d_mat:translate(Center), Rot),
    Mat1 = e3d_mat:mul(Mat0, e3d_mat:scale(1.0, 1.0, -1.0)),
    Mat2 = e3d_mat:mul(Mat1, RotBack),
    Mat = e3d_mat:mul(Mat2, e3d_mat:translate(e3d_vec:neg(Center))),
    D#dlo{mirror=e3d_mat:expand(Mat)};
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
split(#dlo{wire=W,mirror=M,src_sel=Sel,src_we=#we{fs=Ftab0}=We}=D,
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
    
    {DynVs,VsDlist} = split_vs_dlist(sofs:to_external(AllVs), Sel, We),

    FtabDyn0 = sofs:restriction(Ftab, Faces),
    FtabDyn = sofs:to_external(FtabDyn0),
    WeDyn = We#we{fs=gb_trees:from_orddict(FtabDyn)},

    StaticVs0 = sofs:to_external(sofs:difference(AllVs, Vs)),
    StaticVs = sort(insert_vtx_data(StaticVs0, We#we.vs, [])),
    Split = Split0#split{static_vs=StaticVs,dyn_vs=DynVs,
			 dyn_faces=Faces,dyn_ftab=FtabDyn,
			 orig_we=We,st=St},
    #dlo{work=[List],wire=W,mirror=M,vs=VsDlist,
	 src_sel=Sel,src_we=WeDyn,split=Split}.

original_we(#dlo{split=#split{orig_we=We}}) -> We.

update_dynamic(#dlo{work=[Work|_],vs=VsList0,
		    src_we=We0,split=Split}=D, Vtab0) ->
    #split{static_vs=StaticVs,dyn_vs=DynVs,dyn_ftab=Ftab,st=St} = Split,
    Vtab = gb_trees:from_orddict(merge([sort(Vtab0),StaticVs])),
    We = We0#we{vs=Vtab},
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    draw_faces(Ftab, We, St),
    gl:endList(),
    VsList = update_dynamic_vs(VsList0, DynVs, We),
    D#dlo{work=[Work,List],vs=VsList,src_we=We}.

update_dynamic_vs(VsList, none, _) -> VsList;
update_dynamic_vs([Static|_], DynVs, #we{vs=Vtab}) ->
    UnselDlist = gl:genLists(1),
    gl:newList(UnselDlist, ?GL_COMPILE),
    case wings_pref:get_value(vertex_size) of
	0.0 -> ok;
	PtSize -> 
	    gl:pointSize(PtSize),
	    gl:color3f(0, 0, 0),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun(V) ->
			    #vtx{pos=Pos} = gb_trees:get(V, Vtab),
			    gl:vertex3fv(Pos)
		    end, DynVs),
	    gl:'end'()
    end,
    gl:endList(),
    [Static,UnselDlist].

insert_vtx_data([V|Vs], Vtab, Acc) ->
    insert_vtx_data(Vs, Vtab, [{V,gb_trees:get(V, Vtab)}|Acc]);
insert_vtx_data([], _, Acc) -> Acc.

split_vs_dlist(DynVs0, {vertex,SelVs}, #we{vs=Vtab}) ->
    DynVs = gb_sets:from_ordset(DynVs0),
    UnselDyn = gb_sets:to_list(gb_sets:difference(DynVs, SelVs)),
    UnselDlist = gl:genLists(1),
    gl:newList(UnselDlist, ?GL_COMPILE),
    case wings_pref:get_value(vertex_size) of
	0.0 -> ok;
	PtSize -> 
	    gl:pointSize(PtSize),
	    gl:color3f(0, 0, 0),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun({V,#vtx{pos=Pos}}) ->
			    case gb_sets:is_member(V, DynVs) of
				true -> ok;
				false -> gl:vertex3fv(Pos)
			    end
		    end, gb_trees:to_list(Vtab)),
	    gl:'end'()
    end,
    gl:endList(),
    {UnselDyn,[UnselDlist]};
split_vs_dlist(_, _, _) -> {none,none}.

static_dlist(Faces, Ftab, We, St) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    StaticFtab = sofs:to_external(sofs:drestriction(Ftab, Faces)),
    draw_faces(StaticFtab, We, St),
    gl:endList(),
    List.

%%%
%%% Drawing routines.
%%%

draw_faces(Ftab, #we{mode=uv}=We, #st{mat=Mtab}) ->
    case wings_pref:get_value(show_textures) of
	true ->
	    MatFaces = mat_faces(Ftab),
	    draw_uv_faces(MatFaces, We, Mtab);
	false ->
	    draw_mat_faces([{default,Ftab}], We, Mtab)
    end;
draw_faces(Ftab, #we{mode=material}=We, #st{mat=Mtab}) ->
    MatFaces = case wings_pref:get_value(show_materials) of
		   true -> mat_faces(Ftab);
		   false -> [{default,Ftab}]
	       end,
    draw_mat_faces(MatFaces, We, Mtab);
draw_faces(Ftab, #we{mode=vertex}=We, #st{mat=Mtab}) ->
    MatFaces = [{default,Ftab}],
    case wings_pref:get_value(show_colors) of
	true -> draw_uv_faces(MatFaces, We, Mtab);
	false -> draw_mat_faces(MatFaces, We, Mtab)
    end.

mat_faces(Ftab) ->
    mat_faces(Ftab, []).

mat_faces([{_,#face{mat=Mat}}=Rec|Fs], Acc) ->
    mat_faces(Fs, [{Mat,Rec}|Acc]);
mat_faces([], Faces0) ->
    Faces1 = sofs:relation(Faces0),
    Faces = sofs:relation_to_family(Faces1),
    sofs:to_external(Faces).

draw_uv_faces([{Mat,Faces}|T], We, Mtab) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_material:apply_material(Mat, Mtab),
    wings_draw_util:begin_end(
      fun() ->
	      draw_attr_faces(Faces, We)
      end),
    gl:popAttrib(),
    draw_uv_faces(T, We, Mtab);
draw_uv_faces([], _We, _Mtab) -> ok.

draw_attr_faces([{Face,#face{edge=Edge}}|Fs], We) ->
    wings_draw_util:face(Face, Edge, We),
    draw_attr_faces(Fs, We);
draw_attr_faces([], _We) -> ok.

draw_mat_faces([{Mat,Ftab}|T], We, Mtab) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_material:apply_material(Mat, Mtab),
    wings_draw_util:begin_end(
      fun() ->
	      draw_plain_faces(Ftab, We)
      end),
    gl:popAttrib(),
    draw_mat_faces(T, We, Mtab);
draw_mat_faces([], _We, _Mtab) -> ok.

draw_plain_faces([{Face,#face{edge=Edge}}|Fs], We) ->
    wings_draw_util:flat_face(Face, Edge, We),
    draw_plain_faces(Fs, We);
draw_plain_faces([], _We) -> ok.

%%%
%%% Smooth drawing.
%%%

smooth_faces(#we{he=Htab0}=We, #st{mat=Mtab}) ->
    case check_mirror(We) of
	none ->
	    Flist = wings_we:normals(We),
	    smooth_faces(Flist, We, Mtab);
	Face ->
	    Edges = wings_face:outer_edges([Face], We),
	    Htab = gb_sets:union(Htab0, gb_sets:from_list(Edges)),
	    Flist = wings_we:normals(We#we{he=Htab}),
	    smooth_faces(Flist, We, Mtab)
    end.

smooth_faces(Faces, #we{mode=vertex}, _Mtab) ->
    case wings_pref:get_value(show_colors) of
	false -> draw_smooth_plain(Faces);
	true ->
	    gl:enable(?GL_COLOR_MATERIAL),
	    gl:colorMaterial(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE),
	    wings_draw_util:begin_end(fun() ->
					      draw_smooth_vcolor(Faces)
				      end),
	    gl:disable(?GL_COLOR_MATERIAL)
    end,
    false;
smooth_faces(Faces0, #we{mode=material}, Mtab) ->
    Faces1 = sofs:relation(Faces0),
    Faces = case wings_pref:get_value(show_materials) of
		false ->
		    [{default,sofs:to_external(sofs:range(Faces1))}];
		true ->
		    sofs:to_external(sofs:relation_to_family(Faces1))
	    end,
    draw_smooth_1(Faces, Mtab);
smooth_faces(Faces0, #we{mode=uv}, Mtab) ->
    Faces1 = sofs:relation(Faces0),
    Faces = case wings_pref:get_value(show_textures) of
		false ->
		    [{default,sofs:to_external(sofs:range(Faces1))}];
		true ->
		    sofs:to_external(sofs:relation_to_family(Faces1))
	    end,
    draw_smooth_1(Faces, Mtab).

draw_smooth_1(Faces, Mtab) ->
    IsTrans = any(fun({M,_}) ->
			  wings_material:is_transparent(M, Mtab)
		  end, Faces),
    case IsTrans of
	false ->
	    draw_smooth_2(Faces, Mtab),
	    false;
	true ->
	    {Opaq,Trans} = separate_mats(Faces, Mtab),
	    draw_smooth_2(Opaq, Mtab),
	    draw_smooth_2(Trans, Mtab),
	    true
    end.

draw_smooth_2([{Mat,Faces}|T], Mtab) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_material:apply_material(Mat, Mtab),
    Tess = wings_draw_util:tess(),
    wings_draw_util:begin_end(
      fun() ->
	      draw_smooth_3(Faces, Tess)
      end),
    gl:popAttrib(),
    draw_smooth_2(T, Mtab);
draw_smooth_2([], _) -> ok.

draw_smooth_3([{{X,Y,Z},Vs}|Fs], Tess) ->
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{{_,_}=UV,N}}) ->
		    glu:tessVertex(Tess, P, [{texcoord2,UV},{normal,N}]);
	       ({P,{_,N}}) ->
		    glu:tessVertex(Tess, P, [{normal,N}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_3(Fs, Tess);
draw_smooth_3([], _) -> ok.

separate_mats(Faces, Mtab) ->
    separate_mats(Faces, Mtab, [], []).

separate_mats([{M,_}=Mat|T], Mtab, Op, Tr) ->
    case wings_material:is_transparent(M, Mtab) of
	true -> separate_mats(T, Mtab, Op, [Mat|Tr]);
	false -> separate_mats(T, Mtab, [Mat|Op], Tr)
    end;
separate_mats([], _, Op, Tr) -> {Op,Tr}.

%% Smooth drawing for vertex colors.
draw_smooth_vcolor([{_,{{X,Y,Z},Vs}}|T]) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{{_,_,_}=Diff,N}}) ->
		    glu:tessVertex(Tess, P,
				   [{normal,N},
				    {color,Diff}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_vcolor(T);
draw_smooth_vcolor([]) -> ok.

%% Smooth drawing without any materials.
draw_smooth_plain(Faces) ->
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {1,1,1,1}),
    wings_draw_util:begin_end(
      fun() ->
	      draw_smooth_plain_1(Faces)
      end).

draw_smooth_plain_1([{_,{{X,Y,Z},Vs}}|T]) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, X, Y, Z),
    glu:tessBeginPolygon(Tess),
    glu:tessBeginContour(Tess),
    foreach(fun({P,{_,N}}) ->
		    glu:tessVertex(Tess, P, [{normal,N}])
	    end, Vs),
    glu:tessEndContour(Tess),
    glu:tessEndPolygon(Tess),
    draw_smooth_plain_1(T);
draw_smooth_plain_1([]) -> ok.

pos(Key, Tree) ->
    #vtx{pos=Pos} = gb_trees:get(Key, Tree),
    Pos.

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

make_normals_dlist_1(vertex, Vs, #we{vs=Vtab}=We) ->
    foreach(fun(V) ->
		    Pos = pos(V, Vtab),
		    gl:vertex3fv(Pos),
		    N = wings_vertex:normal(V, We),
		    gl:vertex3fv(e3d_vec:add(Pos, e3d_vec:mul(N, 0.3)))
	    end, gb_sets:to_list(Vs));
make_normals_dlist_1(edge, Edges, #we{es=Etab,vs=Vtab}=We) ->
    Et0 = sofs:relation(gb_trees:to_list(Etab), [{edge,data}]),
    Es = sofs:from_external(gb_sets:to_list(Edges), [edge]),
    Et1 = sofs:restriction(Et0, Es),
    Et = sofs:to_external(Et1),
    foreach(fun({_,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}}) ->
		    PosA = pos(Va, Vtab),
		    PosB = pos(Vb, Vtab),
		    Mid = e3d_vec:average([PosA,PosB]),
		    gl:vertex3fv(Mid),
		    N = e3d_vec:average([wings_face:normal(Lf, We),
					 wings_face:normal(Rf, We)]),
		    gl:vertex3fv(e3d_vec:add(Mid, e3d_vec:mul(N, 0.3)))
	    end, Et);
make_normals_dlist_1(face, Faces, We) ->
    foreach(fun(Face) ->
		    Vs = wings_face:surrounding_vertices(Face, We),
		    C = wings_vertex:center(Vs, We),
		    gl:vertex3fv(C),
		    N = wings_face:face_normal(Vs, We),
		    gl:vertex3fv(e3d_vec:add(C, e3d_vec:mul(N, 0.3)))
	    end, gb_sets:to_list(Faces));
make_normals_dlist_1(body, _, #we{fs=Ftab}=We) ->
    make_normals_dlist_1(face, gb_sets:from_list(gb_trees:keys(Ftab)), We).
