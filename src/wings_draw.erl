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
%%     $Id: wings_draw.erl,v 1.76 2002/05/11 08:47:50 bjorng Exp $
%%

-module(wings_draw).
-export([model_changed/1,
	 update_dlists/1,
	 update_sel_dlist/0,
	 draw_faces/2,
	 smooth_faces/2,
	 render/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,last/1,reverse/1]).

model_changed(St) ->
    St.

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
    wings_draw_util:update(fun(D, Data) ->
				   sel_fun(D, Data, Mode)
			   end, Sel),
    wings_draw_util:update(fun(D, _) ->
				   update_fun(D, St)
			   end, []),
    update_sel_dlist().

prepare_dlists(#st{shapes=Shs}) ->
    wings_draw_util:update(fun prepare_fun/2, gb_trees:values(Shs)).

prepare_fun(eol, [#we{perm=Perm}=We|Wes]) when ?IS_NOT_VISIBLE(Perm) ->
    {#dlo{src_we=empty_we(We)},Wes};
prepare_fun(eol, [We|Wes]) ->
    {#dlo{src_we=We},Wes};
prepare_fun(eol, []) ->
    eol;
prepare_fun(#dlo{src_we=We}=D, [We|Wes]) ->
    {D#dlo{src_we=We},Wes};
prepare_fun(#dlo{src_we=#we{id=Id},wire=W,mirror=M},
	    [#we{id=Id,perm=Perm}=We|Wes]) ->
    if 
	?IS_VISIBLE(Perm) ->
	    {#dlo{src_we=We,wire=W,mirror=M},Wes};
	true ->
	    {#dlo{src_we=empty_we(We),wire=W,mirror=M},Wes}
    end;
prepare_fun(#dlo{}, Wes) ->
    {deleted,Wes}.

empty_we(We) ->
    Et = gb_trees:empty(),
    We#we{es=Et,fs=Et,vs=Et,he=gb_sets:empty()}.

sel_fun(eol, _, _) -> eol;
sel_fun(#dlo{src_we=#we{id=Id},src_sel=SrcSel}=D, [{Id,Items}|Sel], Mode) ->
    case SrcSel of
	{Mode,Items} -> {D,Sel};
	_ -> {D#dlo{sel=none,normals=none,src_sel={Mode,Items}},Sel}
    end;
sel_fun(D, Sel, vertex) ->
    {D#dlo{sel=none,normals=none,src_sel={vertex,gb_sets:empty()}},Sel};
sel_fun(D, Sel, _) ->
    {D#dlo{sel=none,src_sel=none},Sel}.

update_fun(eol, _) -> eol;
update_fun(#dlo{work=none,src_we=We}=D, St) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    draw_faces(We, St),
    gl:endList(),
    update_fun(D#dlo{work=List}, St);
update_fun(D, St) ->
    update_fun_2(D, wings_pref:get_value(workmode), St).

update_fun_2(#dlo{smooth=none,src_we=We}=D, false, St) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    smooth_faces(We, St),
    gl:endList(),
    update_fun_3(D#dlo{smooth=List});
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
    wings_draw_util:update(fun(D, _) ->
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
				wings_draw_util:face(Face, Edge, We)
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
    SelDlist = gl:genLists(2),
    UnselDlist = SelDlist + 1,
    case gb_trees:size(Vtab0) =:= gb_sets:size(Vs) of
	true ->
	    gl:newList(SelDlist, ?GL_COMPILE),
	    gl:'begin'(?GL_POINTS),
	    foreach(fun(#vtx{pos=Pos}) -> gl:vertex3fv(Pos) end,
		    gb_trees:values(Vtab0)),
	    gl:'end'(),
	    gl:endList(),
	    gl:newList(UnselDlist, ?GL_COMPILE),
	    gl:endList(),
	    {D#dlo{sel=SelDlist,vs=UnselDlist},[]};
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

	    gl:newList(UnselDlist, ?GL_COMPILE),
	    case wings_pref:get_value(vertex_size) of
		0.0 -> ok;
		PtSize -> 
		    gl:pointSize(PtSize),
		    gl:'begin'(?GL_POINTS),
		    gl:color3f(0, 0, 0),
		    Unsel = sofs:to_external(sofs:drestriction(Vtab, R)),
		    foreach(DrawFun, Unsel),
	    	    gl:'end'()
	    end,
	    gl:endList(),
	    {D#dlo{sel=SelDlist,vs=UnselDlist},[]}
    end;
update_sel(#dlo{}=D) -> {D,[]};
update_sel(eol) -> eol.
    
draw_faces(#we{mode=uv}=We, #st{mat=Mtab}=St) ->
    case wings_pref:get_value(show_textures) of
	true ->
	    MatFaces = mat_faces(We),
	    draw_uv_faces(MatFaces, We, Mtab);
	false ->
	    draw_faces(We#we{mode=none}, St)
    end;
draw_faces(#we{fs=Ftab}=We, _St) ->
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {1,1,1,1}),
    wings_draw_util:begin_end(
      fun() ->
	      draw_faces_1(gb_trees:to_list(Ftab), We)
      end).

draw_faces_1([{Face,#face{edge=Edge}}|Fs], We) ->
    wings_draw_util:face(Face, Edge, We),
    draw_faces_1(Fs, We);
draw_faces_1([], _) -> ok.

draw_uv_faces([{Mat,Faces}|T], We, Mtab) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_material:apply_material(Mat, Mtab),
    wings_draw_util:begin_end(
      fun() ->
	      draw_uv_faces_1(Faces, We)
      end),
    gl:popAttrib(),
    draw_uv_faces(T, We, Mtab);
draw_uv_faces([], _We, _Mtab) -> ok.

draw_uv_faces_1([{Face,Edge}|Fs], We) ->
    wings_draw_util:face(Face, Edge, We),
    draw_uv_faces_1(Fs, We);
draw_uv_faces_1([], _We) -> ok.

mat_faces(#we{fs=Ftab}) ->
    mat_faces(gb_trees:to_list(Ftab), []).

mat_faces([{Face,#face{mat=Mat,edge=Edge}}|Fs], Acc) ->
    mat_faces(Fs, [{Mat,{Face,Edge}}|Acc]);
mat_faces([], Faces0) ->
    Faces1 = sofs:relation(Faces0),
    Faces = sofs:relation_to_family(Faces1),
    sofs:to_external(Faces).

smooth_faces(#we{mode=vertex}=We, _St) ->
    Faces = wings_we:normals(We),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE),
    wings_draw_util:begin_end(fun() -> draw_smooth_vcolor(Faces) end),
    gl:disable(?GL_COLOR_MATERIAL);
smooth_faces(#we{mode=Mode}=We, #st{mat=Mtab}) ->
    Faces0 = wings_we:normals(We),
    Faces1 = sofs:relation(Faces0),
    Faces2 = sofs:relation_to_family(Faces1),
    Faces = sofs:to_external(Faces2),
    case wings_pref:get_value(show_textures) of
	false when Mode == uv ->
	    draw_smooth_plain(Faces0);
	_Other ->
	    draw_smooth_1(Faces, Mtab)
    end.
    
draw_smooth_1([{Mat,Faces}|T], Mtab) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    wings_material:apply_material(Mat, Mtab),
    Tess = wings_draw_util:tess(),
    wings_draw_util:begin_end(
      fun() ->
	      draw_smooth_2(Faces, Tess)
      end),
    gl:popAttrib(),
    draw_smooth_1(T, Mtab);
draw_smooth_1([], _Mtab) -> ok.

draw_smooth_2([{{X,Y,Z},Vs}|Fs], Tess) ->
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
    draw_smooth_2(Fs, Tess);
draw_smooth_2([], _) -> ok.

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

draw_smooth_plain_1([{_,Vs}|T]) ->
    Tess = wings_draw_util:tess(),
    glu:tessNormal(Tess, 0, 0, 0),
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
