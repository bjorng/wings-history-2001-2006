%%
%%  wings_body.erl --
%%
%%     This module contains most of the command for entire Wings objects.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_body.erl,v 1.52 2003/03/17 20:41:28 bjorng Exp $
%%

-module(wings_body).
-export([menu/3,command/2,convert_selection/1]).
-export([auto_smooth/1]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,seq/2]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Menu = [{basic,{"Object operations",ignore}},
	    {basic,separator},
	    {"Move",{move,Dir}},
	    wings_menu_util:rotate(),
	    wings_menu_util:scale(),
	    separator,
	    {"Flip",
	     {flip,[{"X",x,"Flip the object around the X axis"},
		    {"Y",y,"Flip the object around the Y axis"},
		    {"Z",z,"Flip the object around the Z axis"}]}},
	    separator,
	    {"Invert",invert,
	     "Flip all normals, turning the object inside out"},
	    separator,
	    {"Tighten",tighten,
	     "Move vertices towards average midpoint"},
	    {"Smooth",smooth,
	     "Subdivide all faces to give the object a smoother apperance"},
	    {"Combine",combine,
	     "Combine multiple objects into a single object"},
	    {"Separate",separate,
	     "Separate a combined objects into its components"},
	    separator,
	    {"Weld",weld,"Merge pair of faces that are nearly coincident",
	     [option]},
	    separator,
	    {"Cleanup",cleanup,"Remove various defects",[option]},
	    {"Auto-Smooth",auto_smooth,
	     "Set edges hard or soft depending on the angle between faces",
	     [option]},
	    separator,
	    {"Duplicate",{duplicate,Dir}},
	    {"Delete",delete,"Delete the selected objects"},
	    {"Rename...",rename,"Rename selected objects"},
	    separator,
	    {"Mode",{mode,
		     [{"Vertex Color",vertex_color,
		       "Vertex colors will be shown"},
		      {"Material",material,
		       "Materials will be shown"}]},
	     "Change object mode to vertex colors or material"},
	    {"Strip Texture",strip_texture,
	     "Remove a texture, converting it to vertex colors"},
	    {"Colors to Materials",colors_to_materials,
	     "Convert vertex colors to materials"}],
    wings_menu:popup_menu(X, Y, body, Menu).

command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St);
command(invert, St) ->
    {save_state,invert_normals(St)};
command(duplicate, St) ->
    {save_state,duplicate(none, St)};
command({duplicate,Dir}, St) ->
    save_state,duplicate(Dir, St);
command({duplicate_object,Ids}, St) ->
    {save_state,duplicate_object(Ids, St)};
command(delete, St) ->
    {save_state,delete(St)};
command({delete_object,Ids}, St) ->
    {save_state,delete_object(Ids, St)};
command(tighten, St) ->
    tighten(St);
command(smooth, St) ->
    ?SLOW({save_state,smooth(St)});
command(combine, St) ->
    {save_state,combine(St)};
command(separate, St) ->
    {save_state,separate(St)};
command(auto_smooth, St) ->
    auto_smooth(St);
command({auto_smooth,Ask}, St) ->
    auto_smooth(Ask, St);
command({flip,Plane}, St) ->
    {save_state,flip(Plane, St)};
command(cleanup, St) ->
    cleanup(false, St);
command({cleanup,Ask}, St) ->
    cleanup(Ask, St);
command(collapse, St) ->
    {save_state,wings_collapse:collapse(St)};
command(rename, St) ->
    rename(St);
command({rename,Ids}, St) ->
    rename(Ids, St);
command(strip_texture, St) ->
    {save_state,strip_texture(St)};
command(colors_to_materials, St) ->
    {save_state,colors_to_materials(St)};
command({mode,Mode}, St) ->
    {save_state,set_mode(Mode, St)};
command({weld,Ask}, St) ->
    weld(Ask, St).

%%
%% Convert the current selection to a body selection.
%%
convert_selection(#st{sel=Sel0}=St) ->
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || {Id,_} <- Sel0],
    wings_sel:set(body, Sel, St).

%%%
%%% The Cleanup command.
%%%

cleanup(Ask, _) when is_atom(Ask) ->
    Qs = [{"Short Edges",true,[{key,short_edges}]},
	  {hframe,
	   [{label,"Length Tolerance"},{text,1.0E-3,[{range,{1.0E-5,10.0}}]}]},
	  {"Isolated Vertices",true,[{key,isolated_vs}]}],
    wings_ask:dialog(Ask, "Cleanup",
		     [{vframe,Qs}],
		     fun(Res) -> {body,{cleanup,Res}} end);
cleanup(Opts, St0) ->
    St = wings_sel:map(fun(_, We0) ->
			       We = cleanup_waists(We0),
			       cleanup_1(Opts, We)
		       end, St0),
    {save_state,St}.

cleanup_1([{short_edges,Flag},Tolerance|Opts], We0) ->
    We = case Flag of
	     true -> clean_short_edges(Tolerance, We0);
	     false -> We0
	 end,
    cleanup_1(Opts, We);
cleanup_1([{isolated_vs,true}|Opts], We) ->
    cleanup_1(Opts, clean_isolated_vertices(We));
cleanup_1([_|Opts], We) ->
    cleanup_1(Opts, We);
cleanup_1([], We) -> We.

clean_isolated_vertices(We) ->
    Isolated = wings_vertex:isolated(We),
    foldl(fun(V, W0) ->
		  case wings_vertex:dissolve(V, W0) of
		      error -> W0;
		      W -> W
		  end
	  end, We, Isolated).
		  
clean_short_edges(Tolerance, #we{es=Etab,vp=Vtab}=We) ->
    Short = foldl(
	      fun({Edge,#edge{vs=Va,ve=Vb}}, A) ->
		      VaPos = gb_trees:get(Va, Vtab),
		      VbPos = gb_trees:get(Vb, Vtab),
		      case abs(e3d_vec:dist(VaPos, VbPos)) of
			  Dist when Dist < Tolerance -> [Edge|A];
			  _Dist -> A
		      end
	      end, [], gb_trees:to_list(Etab)),
    foldl(fun(Edge, #we{es=Et}=W) ->
		  case gb_trees:is_defined(Edge, Et) of
		      true -> wings_collapse:collapse_edge(Edge, W);
		      false -> W
		  end
	  end, We, Short).

%%
%% A waist is a vertex shared by edges all of which cannot be
%% reached from the incident edge of the vertex.
%%
cleanup_waists(#we{es=Etab,vp=Vtab}=We) ->
    VsEs0 = foldl(fun({E,#edge{vs=Va,ve=Vb}}, A) ->
			  [{Va,E},{Vb,E}|A]
		  end, [], gb_trees:to_list(Etab)),
    VsEs = wings_util:rel2fam(VsEs0),
    cleanup_waists_1(gb_trees:keys(Vtab), VsEs, We).

cleanup_waists_1([V|Vs], [{V,AllEs}|VsEs], #we{es=Etab0,vp=Vtab0,vc=Vct0}=We0) ->
    Es0 = wings_vertex:fold(fun(E, _, _, A) -> [E|A] end, [], V, We0),
    case ordsets:subtract(AllEs, ordsets:from_list(Es0)) of
	[] ->					%Good.
	    cleanup_waists_1(Vs, VsEs, We0);
	[AnEdge|_]=Es ->
	    %% Some edges cannot be reached from the incident edge.
	    %% Repair by duplicating the original vertex.
	    {NewV,We1} = wings_we:new_id(We0),
	    Etab = patch_vtx_refs(Es, V, NewV, Etab0),
	    Vtab = gb_trees:insert(NewV, gb_trees:get(V, Vtab0), Vtab0),
	    Vct = gb_trees:insert(NewV, AnEdge, Vct0),
	    We = We1#we{es=Etab,vp=Vtab,vc=Vct},
	    io:format("Removed waist vertex: ~p\n", [V]),

	    %% Re-process the newly added vertex. (Some of the
	    %% edges may not be reachable from the incident of
	    %% the new vertex.)
	    cleanup_waists_1([NewV|Vs], [{NewV,Es}|VsEs], We)
    end;
cleanup_waists_1([], [], We) -> We.

patch_vtx_refs([E|Es], OldV, NewV, Etab0) ->
    Etab = case gb_trees:get(E, Etab0) of
	       #edge{vs=OldV}=Rec ->
		   gb_trees:update(E, Rec#edge{vs=NewV}, Etab0);
	       #edge{ve=OldV}=Rec ->
		   gb_trees:update(E, Rec#edge{ve=NewV}, Etab0)
	   end,
    patch_vtx_refs(Es, OldV, NewV, Etab);
patch_vtx_refs([], _, _, Etab) -> Etab.

%%%
%%% The Invert command.
%%%

invert_normals(St) ->
    wings_sel:map(fun(_, We) -> wings_we:invert_normals(We) end, St).

%%%
%%% The Duplicate command.
%%%

duplicate(Dir, #st{onext=Oid0}=St0) ->
    Copy = "copy",
    St1 = wings_sel:fold(fun(_, We, St) ->
				 wings_shape:insert(We, Copy, St)
			 end, St0, St0),
    %% Select the duplicate items, not the original items.
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || Id <- seq(Oid0, St1#st.onext-1)],
    St = wings_sel:set(Sel, St1),
    case Dir of
	none -> St;
	_ -> wings_move:setup(Dir, St)
    end.

%%%
%%% Duplicate called from the Outliner or Object window.
%%%

duplicate_object(Objects, #st{shapes=Shs}=St) ->
    Copy = "copy",
    foldl(fun(Id, S) ->
		  We = gb_trees:get(Id, Shs),
		  wings_shape:insert(We, Copy, S)
	  end, St, Objects).

%%%
%%% The Delete command.
%%%

delete(#st{shapes=Shapes0}=St) ->
    Shapes = wings_sel:fold(fun(_, #we{id=Id}, Shs) ->
				    gb_trees:delete(Id, Shs)
			    end, Shapes0, St),
    St#st{shapes=Shapes,sel=[]}.

%%%
%%% Delete called from the Outliner or Object window.
%%%

delete_object(Objects, #st{shapes=Shs0}=St) ->
    Shs = foldl(fun(Id, Shs) ->
			gb_trees:delete(Id, Shs)
		end, Shs0, Objects),
    wings_sel:valid_sel(St#st{shapes=Shs}).

%%%
%%% The Flip command
%%%

flip(Plane0, St) ->
    Plane = flip_scale(Plane0),
    wings_sel:map(fun(_, We) -> flip_body(Plane, We) end, St).

flip_body(Plane, We0) ->
    {Cx,Cy,Cz} = e3d_vec:average(wings_vertex:bounding_box(We0)),
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, Plane),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    We = wings_we:transform_vs(M, We0),
    wings_we:invert_normals(We).

flip_scale(x) -> e3d_mat:scale(-1.0, 1.0, 1.0);
flip_scale(y) -> e3d_mat:scale(1.0, -1.0, 1.0);
flip_scale(z) -> e3d_mat:scale(1.0, 1.0, -1.0).

%%%
%%% The Tighten command.
%%%

tighten(St) ->
    Tvs = wings_sel:fold(fun tighten/3, [], St),
    wings_drag:setup(Tvs, [percent], St).

tighten(_, #we{vp=Vtab}=We, A) ->
    Vs = gb_trees:keys(Vtab),
    wings_vertex_cmd:tighten(Vs, We, A).
    
%%%
%%% The Smooth command.
%%%
%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.
%%% 

smooth(St) ->
    wings_sel:map(fun(_, We) ->
			  wings_subdiv:smooth(We)
		  end, St).

%%%
%%% The Combine command.
%%%

combine(#st{sel=[]}=St) -> St;
combine(#st{shapes=Shs0,sel=[{Id,_}=S|_]=Sel0}=St) ->
    Shs1 = sofs:from_external(gb_trees:to_list(Shs0), [{id,object}]),
    Sel1 = sofs:from_external(Sel0, [{id,dummy}]),
    Sel2 = sofs:domain(Sel1),
    {Wes0,Shs2} = sofs:partition(1, Shs1, Sel2),
    Wes = sofs:to_external(sofs:range(Wes0)),
    Mode = unify_modes(Wes),
    We0 = wings_we:merge(Wes),
    We = We0#we{id=Id,mode=Mode},
    Shs = gb_trees:from_orddict(sort([{Id,We}|sofs:to_external(Shs2)])),
    St#st{shapes=Shs,sel=[S]}.

unify_modes([#we{mode=Mode}|Wes]) ->
    unify_modes(Wes, Mode).

unify_modes([#we{mode=Mode}|Wes], Mode) ->
    unify_modes(Wes, Mode);
unify_modes([#we{mode=Mode}|Wes], OldMode) ->
    NewMode = unify_modes_1(sort([OldMode,Mode])),
    unify_modes(Wes, NewMode);
unify_modes([], Mode) -> Mode.

unify_modes_1([_,vertex]) ->
    wings_util:error("Objects with vertex colors cannot be combined "
		     "with objects with materials and/or textures.");
unify_modes_1([material,uv]) -> uv.

%%%
%%% The Separate command.
%%%

separate(St) ->
    Sep = "sep",
    wings_sel:fold(
      fun(_, #we{id=Id}=We0, St0) ->
	      case wings_we:separate(We0) of
		  [_] -> St0;
		  [We|Wes] ->
		      St1 = foldl(fun(W, A) ->
					  wings_shape:insert(W, Sep, A)
				  end, St0, Wes),
		      wings_shape:replace(Id, We, St1)
	      end
      end, St, St).

%%%
%%% The Auto-Smooth command.
%%%

auto_smooth(St) ->
    do_auto_smooth(60, St).

auto_smooth(Ask, _) when is_atom(Ask) ->
    wings_ask:ask(Ask, "Auto Smooth Parameters",
		  [{"Crease Angle",60,[{range,{0,180}}]}],
		  fun(Res) -> {body,{auto_smooth,Res}} end);
auto_smooth([Angle], St) ->
    {save_state,do_auto_smooth(Angle, St)}.

do_auto_smooth(Angle, St) ->
    Cos = cos_degrees(Angle),
    wings_sel:map(fun(_, We) -> auto_smooth_1(Cos, We) end, St).

auto_smooth_1(Cos, #we{es=Etab,he=Htab0}=We) ->
    Htab = foldl(fun({E,R}, A) ->
			 auto_smooth(E, R, Cos, A, We)
		 end, Htab0, gb_trees:to_list(Etab)),
    We#we{he=Htab}.

auto_smooth(Edge, #edge{lf=Lf,rf=Rf}, Cos, H0, We) ->
    Ln = wings_face:normal(Lf, We),
    Lr = wings_face:normal(Rf, We),
    case e3d_vec:is_zero(Ln) orelse e3d_vec:is_zero(Lr) of
	true -> H0;				%Ignore this edge.
	false ->
	    case e3d_vec:dot(Ln, Lr) of
		P when P < Cos ->
		    wings_edge:hardness(Edge, hard, H0);
		_ ->				%angle =< 60
		    wings_edge:hardness(Edge, soft, H0)
	    end
    end.

cos_degrees(Angle) ->
    math:cos(Angle*math:pi()/180.0).


%%%
%%% Rename selected objects.
%%%

rename(St) ->
    Wes = wings_sel:fold(fun(_, We, A) -> [We|A] end, [], St),
    rename_1(Wes, St).

rename(Objects, #st{shapes=Shs}=St) ->
    Wes = foldl(fun(Id, A) -> [gb_trees:get(Id, Shs)|A] end, [], Objects),
    rename_1(Wes, St).

rename_1(Wes, St) ->
    Qs = rename_qs(Wes),
    wings_ask:dialog("Rename", Qs,
		     fun(NewNames) ->
			     rename_1(NewNames, Wes, St)
		     end).

rename_1(Names, Wes, #st{shapes=Shs}=St) ->
    rename_2(Names, Wes, Shs, St).

rename_2([N|Ns], [#we{id=Id}=We|Wes], Shs0, St) ->
    Shs = gb_trees:update(Id, We#we{name=N}, Shs0),
    rename_2(Ns, Wes, Shs, St);
rename_2([], [], Shs, St) -> St#st{shapes=Shs}.

rename_qs(Wes) ->
    OldNames = [{label,Name} || #we{name=Name} <- Wes],
    TextFields = [{text,Name,[]} || #we{name=Name} <- Wes],
    [{hframe,
      [{vframe,OldNames},
       {vframe,TextFields}]}].

%%%
%%% Set Mode.
%%%

set_mode(vertex_color, St) -> set_mode(vertex, St);
set_mode(Mode, St) ->
    wings_sel:map(
      fun(_, #we{mode=uv}) ->
	      Error ="Objects with UV coordinates cannot "
		  " have their mode changed.",
	      wings_util:error(Error);
	 (_, We) -> We#we{mode=Mode}
      end, St).

%%%
%%% Strip Texture.
%%%

strip_texture(St) ->
    wings_sel:map(fun(_, We) ->
			  wings_we:uv_to_color(We, St)
		  end, St).

%%%
%%% Convert vertex colors to materials.
%%%

colors_to_materials(St0) ->
    {St,#st{mat=Mat}} =
	wings_sel:mapfold(fun(_, We, S) ->
				  colors_to_materials_1(We, S)
			  end, St0, St0),
    St#st{mat=Mat}.

colors_to_materials_1(#we{mode=vertex,fs=Ftab}=We0, St) ->
    colors_to_materials_2(gb_trees:keys(Ftab), We0#we{mode=material}, St);
colors_to_materials_1(We, St) -> {We,St}.

colors_to_materials_2([F|Fs], #we{fs=Ftab0}=We, St0) ->
    Colors = [C || [_|C] <- wings_face:vinfo(F, We)],
    Color = e3d_vec:average(Colors),
    {Name,St} = color_material(Color, St0),
    FaceInfo = gb_trees:get(F, Ftab0),
    Ftab = gb_trees:update(F, FaceInfo#face{mat=Name}, Ftab0),
    colors_to_materials_2(Fs, We#we{fs=Ftab}, St);
colors_to_materials_2([], We, St) -> {We,St}.

color_material({R,G,B}=Color, #st{mat=Mat0}=St0) ->
    Name0 = "color_" ++ fmt_int(R) ++ "_" ++ fmt_int(G) ++ "_" ++ fmt_int(B),
    Name = list_to_atom(Name0),
    case gb_trees:is_defined(Name, Mat0) of
	true -> {Name,St0};
	false ->
	    Mat = [{opengl,[{diffuse,Color}]}],
	    case wings_material:add_materials([{Name,Mat}], St0) of
		{St,[]} -> {Name,St};
		{St,[{Name,New}]} -> {New,St}
	    end
    end.

fmt_int(I) ->
    L = integer_to_list(trunc(256*I)),
    fmt_int(length(L), L).

fmt_int(3, L) -> L;
fmt_int(N, L) -> fmt_int(N+1, [$0|L]).

%%%
%%% The Weld command.
%%%

weld(Ask, _) when is_atom(Ask) ->
    Qs = [{hframe,
	   [{label,"Distance Tolerance"},{text,1.0E-3,[{range,{1.0E-5,10.0}}]}]}],
    wings_ask:dialog(Ask, "Weld", Qs,
		     fun(Res) -> {body,{weld,Res}} end);
weld([Tolerance], St0) ->
    St1 = combine(St0),
    {St2,Sel} = wings_sel:mapfold(fun(_, We, Acc) ->
					  weld_1(Tolerance, We, Acc)
				  end, [], St1),
    St = wings_sel:set(vertex, Sel, St2),
    {save_state,wings_sel:valid_sel(St)}.

weld_1(Tol, #we{id=Id,fs=Fs0}=We0, Acc) ->
    Fs = weld_1_list(gb_trees:keys(Fs0), Tol, We0, []),
    R = sofs:relation(Fs, [{key,face}]),
    F = sofs:relation_to_family(R),
    Part0 = sofs:range(F),
    Part1 = sofs:specification({external,fun([_]) -> false;
					    (_) -> true end}, Part0),
    Part = sofs:to_external(Part1),
    case weld_2(Part, Tol, We0) of
	We0 ->
	    wings_util:error("Found no faces to weld.");
	We ->
	    {We,[{Id,weld_selection(lists:append(Part), We0, We)}|Acc]}
    end.

weld_1_list([F|Fs], Tol, We, Acc) ->
    Vs = wings_face:fold(
	   fun(V, _, _, Acc0) ->
		   [V|Acc0]
	   end, [], F, We),
    {X,Y,Z} = wings_vertex:center(Vs, We),
    Center = {granularize(X, Tol),granularize(Y, Tol),granularize(Z, Tol)},
    weld_1_list(Fs, Tol, We, [{{length(Vs),Center},F}|Acc]);
weld_1_list([], _, _, Acc) -> Acc.

granularize(F, Tol) -> Tol*round(F/Tol).

weld_2([P|Ps], Tol, We0) ->
    We = weld_part(P, Tol, We0),
    weld_2(Ps, Tol, We);
weld_2([], _, We) -> We.

weld_part([F|Fs], Tol, We) ->
    weld_part_1(F, Fs, Tol, We, []);
weld_part([], _, We) -> We.

weld_part_1(Fa, [Fb|Fs], Tol, We0, Acc) ->
    case try_weld(Fa, Fb, Tol, We0) of
	no -> weld_part_1(Fa, Fs, Tol, We0, [Fb|Acc]);	
	We -> weld_part(Fs++Acc, Tol, We)
    end;
weld_part_1(_, [], Tol, We, Acc) ->
    weld_part(Acc, Tol, We).

try_weld(Fa, Fb, Tol, We) ->
    case wings_face:are_neighbors(Fa, Fb, We) of
	true -> no;
	false ->
	    Na = wings_face:normal(Fa, We),
	    Nb = wings_face:normal(Fb, We),
	    case e3d_vec:dot(Na, Nb) of
		Dot when Dot < -0.99 ->
		    try_weld_1(Fa, Fb, Tol, We);
		_Dot -> no
	    end
    end.

try_weld_1(Fa, Fb, Tol, We0) ->
    N = wings_face:vertices(Fa, We0),
    IterA = wings_face:iterator(Fa, We0),
    {Va,_,_,_} = wings_face:next_cw(IterA),
    PosA = wings_vertex:pos(Va, We0),
    IterB0 = weld_synced_iterator(N, Fb, PosA, We0),
    case weld_same_positions(N, IterA, IterB0, Tol, We0) of
	false -> no;
	true ->
	    {Vb,_,_,_} = wings_face:next_ccw(IterB0),
	    We = wings_face_cmd:force_bridge(Fa, Va, Fb, Vb, We0),
	    Es = wings_we:new_items(edge, We0, We),
	    foldl(fun(E, W) -> wings_collapse:collapse_edge(E, W) end,
		  We, gb_sets:to_list(Es))
    end.

weld_synced_iterator(N, Face, Pos, We) ->
    Iter = wings_face:iterator(Face, We),
    weld_synced_iterator_1(N, Iter, Pos, We, []).

weld_synced_iterator_1(0, _, _, _, Acc) ->
    [{_,Iter}|_] = sort(Acc),
    Iter;
weld_synced_iterator_1(N, Iter0, Pos, We, Acc) ->
    {V,_,_,Iter} = wings_face:next_ccw(Iter0),
    D = e3d_vec:dist(Pos, wings_vertex:pos(V, We)),
    weld_synced_iterator_1(N-1, Iter, Pos, We, [{D,Iter0}|Acc]).

weld_same_positions(0, _, _, _, _) -> true;
weld_same_positions(N, IterA0, IterB0, Tol, We) ->
    {Va,_,_,IterA} = wings_face:next_cw(IterA0),
    {Vb,_,_,IterB} = wings_face:next_ccw(IterB0),
    PosA = wings_vertex:pos(Va, We),
    PosB = wings_vertex:pos(Vb, We),
    case e3d_vec:dist(PosA, PosB) of
	D when abs(D) < Tol -> weld_same_positions(N-1, IterA, IterB, Tol, We);
	_D -> false
    end.

weld_selection(Fs, OldWe, We) ->
    weld_selection(Fs, OldWe, We, []).

weld_selection([F|Fs], OldWe, #we{fs=Ftab}=We, Acc) ->
    case gb_trees:is_defined(F, Ftab) of
	true -> weld_selection(Fs, OldWe, We, Acc);
	false ->
	    Vs = wings_face:surrounding_vertices(F, OldWe),
	    weld_selection(Fs, OldWe, We, Vs++Acc)
    end;
weld_selection([], _, _, Acc) ->
    gb_sets:from_list(Acc).
