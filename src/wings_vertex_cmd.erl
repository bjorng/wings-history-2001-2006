%%
%%  wings_vertex_cmd.erl --
%%
%%     This module contains most of the commands for vertices.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vertex_cmd.erl,v 1.58 2006/01/12 17:03:19 giniu Exp $
%%

-module(wings_vertex_cmd).
-export([menu/3,command/2,tighten/3,tighten/4,
	 connect/2,bevel_vertex/2, flatten/2]).

-export([set_color/2]).

-include("wings.hrl").
-import(lists, [member/2,keymember/3,foldl/3,mapfoldl/3,
		reverse/1,last/1,sort/1]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Menu = [{basic,{?STR(menu,1,"Vertex operations"),ignore}},
	    {basic,separator},
	    {?STR(menu,2,"Move"),{move,Dir},[],[magnet]},
	    wings_menu_util:rotate(St),
	    wings_menu_util:scale(St),
	    separator,
	    {?STR(menu,17,"Positionize"),move_to,
	     ?STR(menu,18,"Move one vertex to exact position in absolute coordinates")},
	    separator,
	    {?STR(menu,3,"Extrude"),{extrude,Dir}},
	    separator,
	    wings_menu_util:flatten(),
	    separator,
	    {?STR(menu,4,"Connect"),connect,
	     ?STR(menu,5,"Create a new edge by connecting selected vertices")},
	    {?STR(menu,6,"Tighten"),tighten,
	     ?STR(menu,7,"Move selected vertices towards average midpoint"),[magnet]},
	    {?STR(menu,8,"Bevel"),bevel,?STR(menu,9,"Create faces of selected vertices")},
	    {?STR(menu,10,"Dissolve"),dissolve,
	     ?STR(menu,11,"Delete selected vertices (clearing selection)")},
	    {?STR(menu,12,"Collapse"),collapse,
	     ?STR(menu,13,"Delete selected vertices (creating a face selection)")},
	    {?STR(menu,19,"Weld"),weld,
	     ?STR(menu,20,"Weld selected vertex to other one")},
	    separator,
	    {?STR(menu,14,"Deform"),wings_deform:sub_menu(St)},
	    separator,
	    {?STR(menu,15,"Vertex Color"),vertex_color,
	     ?STR(menu,16,"Apply vertex colors to selected vertices")}],
    wings_menu:popup_menu(X, Y, vertex, Menu).

%% Vertex menu.
command({flatten,Plane}, St) ->
    flatten(Plane, St);
command(connect, St) ->
    {save_state,connect(St)};
command(tighten, St) ->
    tighten(St);
command({tighten,Magnet}, St) ->
    tighten(Magnet, St);
command(bevel, St) ->
    ?SLOW(bevel(St));
command({extrude,Type}, St) ->
    ?SLOW(extrude(Type, St));
command({deform,Deform}, St0) ->
    ?SLOW(wings_deform:command(Deform, St0));
command(auto_smooth, St) ->
    wings_body:auto_smooth(St);
command(dissolve, St) ->
    {save_state,dissolve(St)};
command(collapse, St) ->
    {save_state,wings_collapse:collapse(St)};
command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St);
command(vertex_color, St) ->
    wings_color:choose(fun(Color) ->
			       set_color(Color, St)
		       end);
command({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun command/2);
command(move_to, St) ->
    move_to(St);
command(weld, St) ->
    weld(St).
    
%%%
%%% The Flatten command.
%%%

flatten({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun flatten/2);
flatten({Plane,Center}, St) ->
    flatten(Plane, Center, St);
flatten(Plane, St) ->
    flatten(Plane, average, St).

flatten(Plane0, average, St) ->
    Plane = wings_util:make_vector(Plane0),
    {save_state,
     wings_sel:map(
       fun(Vs, We) ->
	       wings_vertex:flatten(Vs, Plane, We)
       end, St)};
flatten(Plane0, Center, St) ->
    Plane = wings_util:make_vector(Plane0),
    {save_state,
     wings_sel:map(
       fun(Vs, We) ->
	       wings_vertex:flatten(Vs, Plane, Center, We)
       end, St)}.
    
%%%
%%% The Extrude command.
%%%

extrude(Type, St0) ->
    {St,Tvs} = wings_sel:mapfold(
		 fun(Vs, We0, Acc) ->
			 extrude_vertices(Vs, We0, Acc)
		 end, [], St0),
    wings_move:plus_minus(Type, Tvs, St).

extrude_vertices(Vs, We0, Acc) ->
    We = foldl(fun(V, A) ->
		       ex_new_vertices(V, We0, A)
	       end, We0, gb_sets:to_list(Vs)),
    NewVs = wings_we:new_items_as_ordset(vertex, We0, We),
    {We,[{Vs,NewVs,gb_sets:empty(),We}|Acc]}.

ex_new_vertices(V, OrigWe, #we{vp=Vtab}=We0) ->
    Center = wings_vertex:pos(V, We0),
    {We,VsFaces} =
	wings_vertex:fold(
	  fun(Edge, Face, Rec, {W0,Vs}) ->
		  OtherV = wings_vertex:other(V, Rec),
		  R = edge_ratio(OtherV, OrigWe),
		  Pos0 = gb_trees:get(OtherV, Vtab),
		  Dir = e3d_vec:sub(Pos0, Center),
		  Pos = e3d_vec:add(Center, e3d_vec:mul(Dir, R)),
		  {W,NewV} = wings_edge:fast_cut(Edge, Pos, W0),
		  {W,[NewV,Face|Vs]}
	  end, {We0,[]}, V, We0),
    ex_connect(VsFaces, VsFaces, We).

edge_ratio(V, #we{vp=Vtab}) ->
    case gb_trees:is_defined(V, Vtab) of
	false -> 1/3;
	true -> 0.25
    end.

ex_connect([Va,Face|[Vb|_]=T], More, We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    ex_connect(T, More, We);
ex_connect([Va,Face], [Vb|_], We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    We.

%%%
%%% The Bevel command.
%%%

bevel(St0) ->
    {St,{Tvs0,FaceSel}} =
	wings_sel:mapfold(
	  fun(VsSet, We, A) ->
		  bevel_1(VsSet, We, A)
	  end, {[],[]}, St0),
    {Min,Tvs} = bevel_normalize(Tvs0),
    wings_drag:setup(Tvs, [{distance,{0.0,Min}}],
		     wings_sel:set(face, FaceSel, St)).

bevel_vertex(V, We0) ->
    Es = wings_vertex:fold(
	   fun(Edge, Face, Rec, Acc) ->
		   [{Edge,Face,Rec}|Acc]
	   end, [], V, We0),
    case length(Es) of
	2 -> We0;
	NumEdges ->
	    {We,_,_} = bevel_vertex_1(V, Es, NumEdges, [], We0, []),
	    We
    end.

bevel_1(VsSet, #we{id=Id}=We0, {Tvs,Fa}) ->
    Vs = gb_sets:to_list(VsSet),
    {We,Tv,Fs0} = bevel_vertices(Vs, VsSet, We0, We0, [], []),
    FaceSel = case Fs0 of
		  [] -> Fa;
		  _ -> [{Id,gb_sets:from_list(Fs0)}|Fa]
	      end,
    {We,{[{Id,Tv}|Tvs],FaceSel}}.

bevel_vertices([V|Vs], VsSet, WeOrig, We0, Acc0, Facc) ->
    Adj = adjacent(V, VsSet, WeOrig),
    Es = wings_vertex:fold(
	   fun(Edge, Face, Rec, Acc) ->
		   [{Edge,Face,Rec}|Acc]
	   end, [], V, We0),
    case length(Es) of
	2 ->					%Winged vertex - ignore.
	    bevel_vertices(Vs, VsSet, WeOrig, We0, Acc0, Facc);
	NumEdges ->
	    {We,Acc,Face} = bevel_vertex_1(V, Es, NumEdges, Adj, We0, Acc0),
	    bevel_vertices(Vs, VsSet, WeOrig, We, Acc, [Face|Facc])
    end;
bevel_vertices([], _, _, We, Acc, Facc) -> {We,Acc,Facc}.

bevel_vertex_1(V, Es, NumEdges, Adj, We0, Vec0) ->
    {InnerFace,We1} = wings_we:new_id(We0),
    {Ids,We2} = wings_we:new_wrap_range(NumEdges, 2, We1),
    #we{es=Etab0,vc=Vct0,vp=Vtab0,fs=Ftab0} = We1,
    {Vct,Vtab} = bevel_vertices_1(V, Ids, NumEdges, Vct0, Vtab0),
    {_,Etab,Vec} = foldl(
		     fun(E, {Ids0,Etab1,Vs0}) ->
			     {Etab,Vec} = bevel(V, E, InnerFace, Ids0,
						Adj, Vtab0, Etab0, Etab1),
			     {wings_we:bump_id(Ids0),Etab,[Vec|Vs0]}
		     end, {Ids,Etab0,Vec0}, Es),
    Mat = bevel_material(Es, We2),
    NewEdge = wings_we:id(1, Ids),
    Ftab = gb_trees:insert(InnerFace, NewEdge, Ftab0),
    We = wings_facemat:assign(Mat, [InnerFace], We2),
    {We#we{es=Etab,fs=Ftab,vc=Vct,vp=Vtab},Vec,InnerFace}.

bevel_material(Es, We) ->
    bevel_material(Es, We, []).

bevel_material([{_,Face,_}|Es], We, Acc) ->
    Mat = wings_facemat:face(Face, We),
    bevel_material(Es, We, [{Mat,Face}|Acc]);
bevel_material([], _, A0) ->
    A1 = sofs:relation(A0, [{mat,face}]),
    A2 = sofs:relation_to_family(A1),
    A = sofs:to_external(A2),
    [{_,Mat}|_] = sort([{-length(Fs),M} || {M,Fs} <- A]),
    Mat.

bevel(V, {Edge,Face,Rec0}, InnerFace, Ids, Adj, Vtab, OrigEtab, Etab0) ->
    Vprev = wings_we:id(0, Ids),
    Eprev = wings_we:id(1, Ids),
    Va = wings_we:id(2, Ids),
    Ecurr = wings_we:id(3, Ids),
    Vb = wings_we:id(4, Ids),
    Enext = wings_we:id(5, Ids),
    {Rec,Curr} =
	case Rec0 of
	    #edge{vs=V,ve=Vother,rf=Face,rtpr=ColEdge} ->
		Col = bevel_color(ColEdge, Face, OrigEtab),
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Eprev},
		 Rec0#edge{vs=Vb,ve=Va,a=Col,lf=InnerFace,
			   rtsu=Edge,ltpr=Eprev,ltsu=Enext}};
	    #edge{vs=V,ve=Vother,rf=Of,rtpr=ColEdge} ->
		Col = bevel_color(ColEdge, Of, OrigEtab),
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Enext},
		 Rec0#edge{vs=Vprev,ve=Va,a=Col,lf=InnerFace,
			   rtsu=Edge,ltpr=Enext,ltsu=Eprev}};
	    #edge{ve=V,vs=Vother,lf=Face,ltpr=ColEdge} ->
		Col = bevel_color(ColEdge, Face, OrigEtab),
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Eprev},
		 Rec0#edge{vs=Va,ve=Vb,b=Col,rf=InnerFace,
			   ltsu=Edge,rtpr=Eprev,rtsu=Enext}};
	    #edge{ve=V,vs=Vother,lf=Of,ltpr=ColEdge} ->
		Col = bevel_color(ColEdge, Of, OrigEtab),
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Enext},
		 Rec0#edge{vs=Va,ve=Vprev,b=Col,rf=InnerFace,
			   ltsu=Edge,rtpr=Enext,rtsu=Eprev}}
	end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    Vpos = gb_trees:get(V, Vtab),
    Vec = bevel_vec(Adj, Vother, Vpos, Vtab),
    {gb_trees:insert(Ecurr, Curr, Etab),{Vec,Va}}.

bevel_vec(Adj, Vother, Vpos, Vtab) ->
    Opos = gb_trees:get(Vother, Vtab),
    case member(Vother, Adj) of
	true ->
	    e3d_vec:sub(e3d_vec:average([Opos,Vpos]), Vpos);
	false ->
	    e3d_vec:sub(Opos, Vpos)
    end.

bevel_vertices_1(V, Ids, N, Vct0, Vtab0) ->
    Pos = gb_trees:get(V, Vtab0),
    Vct = gb_trees:delete(V, Vct0),
    Vtab = gb_trees:delete(V, Vtab0),
    bevel_new_vertices(Ids, N, Pos, Vct, Vtab).

bevel_new_vertices(Ids, N, Pos, Vct0, Vtab0) when N > 0 ->
    V = Id = wings_we:id(0, Ids),
    Edge = Id + 1,
    Vct = gb_trees:insert(V, Edge, Vct0),
    Vtab = gb_trees:insert(V, Pos, Vtab0),
    bevel_new_vertices(wings_we:bump_id(Ids), N-1, Pos, Vct, Vtab);
bevel_new_vertices(_, _, _, Vct, Vtab) -> {Vct,Vtab}.

bevel_normalize(Tvs) ->
    bevel_normalize(Tvs, 1.0E207, []).

bevel_normalize([{Id,VecVs0}|Tvs], Min0, Acc) ->
    {VecVs,Min} = bevel_normalize_1(VecVs0, Min0),
    bevel_normalize(Tvs, Min, [{Id,VecVs}|Acc]);
bevel_normalize([], Min, Tvs) -> {Min,Tvs}.

bevel_normalize_1(VecVs, Min0) ->
    mapfoldl(fun({Vec,V}, M0) ->
		     Min = case e3d_vec:len(Vec) of
			       Len when Len < M0 -> Len;
			       _Len -> M0
			   end,
		     {{e3d_vec:norm(Vec),[V]},Min}
	     end, Min0, VecVs).

bevel_color(ColEdge, Face, Etab0) ->
    case gb_trees:get(ColEdge, Etab0) of
	#edge{lf=Face,a=Col} -> Col;
	#edge{rf=Face,b=Col} -> Col
    end.

adjacent(V, Vs, We) ->
    wings_vertex:fold(
      fun(_, _, Rec, A) ->
	      OtherV = wings_vertex:other(V, Rec),
	      case gb_sets:is_member(OtherV, Vs) of
		  true -> [OtherV|A];
		  false -> A
	      end
      end, [], V, We).
    
%%%
%%% The Connect command.
%%%

connect(St) ->
    wings_sel:map(fun connect/2, St).

connect(Vs0, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
	     ({Face,Vs}, Acc) -> wings_vertex:connect(Face, Vs, Acc)
	  end, We, FaceVs).

%%%
%%% The Tighten command.
%%%

tighten(St) ->
    Tvs = wings_sel:fold(fun tighten/3, [], St),
    wings_drag:setup(Tvs, [percent], St).

tighten(Vs, #we{id=Id}=We, Acc) when is_list(Vs) ->
    Tv = foldl(
	   fun(V, A) ->
		   Vec = tighten_vec(V, We),
		   [{Vec,[V]}|A]
	   end, [], Vs),
    [{Id,Tv}|Acc];
tighten(Vs, We, Acc) -> 
    tighten(gb_sets:to_list(Vs), We, Acc).

tighten_vec(V, #we{vp=Vtab,mirror=MirrorFace}=We) ->
    Cs = wings_vertex:fold(
	   fun(_, Face, _, A) when Face =/= MirrorFace ->
		   FaceVs = wings_face:to_vertices([Face], We),
		   C = wings_vertex:center(FaceVs, We),
		   [C|A];
	      (_, _, _, A) -> A
	   end, [], V, We),
    Center = e3d_vec:average(Cs),
    e3d_vec:sub(Center, gb_trees:get(V, Vtab)).

%%%
%%% The magnetic version of Tighten.
%%%

tighten(Magnet, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 tighten(Vs, We, Magnet, Acc)
			 end, [], St),
    Flags = wings_magnet:flags(Magnet, []),
    wings_drag:setup(Tvs, [percent,falloff], Flags, St).

tighten(Vs, We, Magnet, Acc) when is_list(Vs) ->
    Tv = foldl(
	   fun(V, A) ->
		   Vec = tighten_vec(V, We),
		   [{Vec,[V]}|A]
	   end, [], Vs),
    magnet_move(Tv, Magnet, We, Acc);
tighten(Vs, We, Magnet, Acc) -> 
    tighten(gb_sets:to_list(Vs), We, Magnet, Acc).

magnet_move(Tv, Magnet0, #we{id=Id}=We, Acc) ->
    Vs = lists:append([Vs || {_,Vs} <- Tv]),
    {VsInf,Magnet,Affected} = wings_magnet:setup(Magnet0, Vs, We),
    Vec = magnet_tighten_vec(Affected, We, []),
    [{Id,{Affected,wings_move:magnet_move_fun(Vec, VsInf, Magnet)}}|Acc].

magnet_tighten_vec([V|Vs], We, Acc) ->
    Vec = tighten_vec(V, We),
    magnet_tighten_vec(Vs, We, [{V,Vec}|Acc]);
magnet_tighten_vec([], _, Acc) ->
    gb_trees:from_orddict(sort(Acc)).
    
%%%
%%% The Dissolve command. Like Collapse, but stays in vertex mode
%%% (without any selection).
%%%

dissolve(St0) ->
    St = wings_collapse:collapse(St0),
    St#st{selmode=vertex,sel=[]}.

%%%
%%% Set vertex color.
%%%

set_color(Color, St) ->
    wings_sel:map(fun(Vs, We) ->
			  set_color_1(gb_sets:to_list(Vs), Color,
				      We#we{mode=vertex})
		  end, St).

set_color_1([V|Vs], Color, #we{es=Etab0}=We) ->
    Etab = wings_vertex:fold(
	     fun(Edge, _Face, Rec0, Es) ->
		     Rec = case Rec0 of
			       #edge{vs=V} -> Rec0#edge{a=Color};
			       #edge{ve=V} -> Rec0#edge{b=Color}
			   end,
		     gb_trees:update(Edge, Rec, Es)
	     end, Etab0, V, We),
    set_color_1(Vs, Color, We#we{es=Etab});
set_color_1([], _, We) -> We.

%%
%% Move one vertex to absolute position
%%

move_to(#st{sel=[{Obj,{1,{Vert,_,_}}}],shapes=Shs}=St) ->
   We = gb_trees:get(Obj, Shs),
   Vtab = We#we.vp,
   {X,Y,Z}=gb_trees:get(Vert, Vtab),
   wings_ask:dialog(
      ?__(1,"Numeric Input"), 
      [{hframe,[{label,"X"},{text,X}]},
       {hframe,[{label,"Y"},{text,Y}]},
       {hframe,[{label,"Z"},{text,Z}]}],
      fun(Move) ->
         move_to1(Move,St)
      end);
move_to(St) ->
   wings_u:error(?__(2,"You can move only one vertex")),
   St.

move_to1([X,Y,Z],#st{sel=[{Obj,{1,{Vert,_,_}}}],shapes=Shs}=St) ->
   We = gb_trees:get(Obj, Shs),
   Vtab = We#we.vp,
   NewVtab = gb_trees:update(Vert,{X,Y,Z},Vtab),
   NewWe = We#we{vp=NewVtab},
   NewShs = gb_trees:update(Obj,NewWe,Shs),
   St#st{shapes=NewShs}.

%%
%% Weld one vertex to other
%%

weld(#st{sel=[{Obj,{1,{Vert,_,_}}}],shapes=Shs}=St) ->
   We = gb_trees:get(Obj, Shs),
   Vertices = gb_trees:size(We#we.fs),
   Mirror = We#we.mirror,
   if
      Mirror /= none -> 
         Fs = We#we.fs,
         Edge = gb_trees:get(Mirror,Fs),
         Verts = get_verts(Mirror, Edge, Edge, We, []),
         case member(Vert,Verts) of
            true ->
               wings_u:error(?__(1,"You cannot weld at mirrot plane")),
               St;
            _ -> ok
         end;
      true -> ok
   end,
   if
      Vertices < 4 -> 
         wings_u:error(?__(2,"Object must have at least 4 vertices")),
         St;
      true ->
         wings:ask(weld_select(St), St, fun weld/2)
   end;
weld(St) ->
   wings_u:error(?__(3,"You can weld only one vertex")),
   St.

weld_select(OrigSt) ->
    Desc = ?__(1,"Select target vertex you want weld to"),
    Fun = fun(check, St) -> weld_check_selection(St, OrigSt);
	     (exit, {_,_,#st{sel=Vert2}=St}) ->
		  case weld_check_selection(St, OrigSt) of
		      {_,[]} -> {[],[Vert2]};
		      {_,_} -> error
		  end
	  end,
    {[{Fun,Desc}],[],[],[vertex]}.

weld_check_selection(#st{sel=[{_Obj,{1,{Vert2,_,_}}}]},#st{sel=[{_Obj,{1,{Vert1,_,_}}}]}=St) ->
   if
      Vert1==Vert2 -> {none,?__(1,"You cannot weld vertex to itself")};
      true -> 
         St2=wings_sel_conv:mode(vertex,St),
         [{_,Sel2}]=St2#st.sel,
         CanDo = gb_sets:is_element(Vert2,Sel2),
         if
            CanDo -> {none,""};
            true -> {none,?__(2,"Vertices you want weld must share edge")}
         end
   end;
weld_check_selection(#st{sel=[{_Obj,_}]},#st{sel=[{_Obj,_}]}) ->
   {none,?__(3,"You can weld to only one point")};
weld_check_selection(#st{sel=[]},_) ->
   {none,?__(4,"Nothing selected")};
weld_check_selection(_,_) ->
   {none,?__(5,"You can weld only in same object")}.

weld([{_,{1,{Vert2,_,_}}}]=NewSel,#st{sel=[{Obj,{1,{Vert1,_,_}}}],shapes=Shs}=St) ->
   We = gb_trees:get(Obj, Shs),
   NewVp = gb_trees:delete(Vert1,We#we.vp),
   {NewEs,NewMat} = fix_edge(Vert1,Vert2,We),
   NewHe = fix_hardedge(NewEs, We#we.he),
   NewWe = wings_we:rebuild(We#we{vp=NewVp, es=NewEs, he=NewHe, vc=undefined, fs=undefined, mat=NewMat}),
   wings_we_util:validate(NewWe),
   NewShs = gb_trees:update(Obj,NewWe,Shs),
   St#st{shapes=NewShs,sel=NewSel}.

fix_edge(Vert1,Vert2,#we{mat=Mat}=Orig) ->
   Es = Orig#we.es,
   RemoveEdge = find_edge(Vert1,Vert2,Es),
   FixMe = needs_cleanup(RemoveEdge,Orig),
   Etab = fix_edge_1(RemoveEdge,Vert1,Vert2,Es,Orig),
   NewEs = fix_edge_2(FixMe,Etab),
   NewMat = fix_mat(FixMe,Orig),
   {NewEs,NewMat}.

fix_mat([Remove|FixRest],We) ->
   NewWe = wings_facemat:delete_face(Remove,We),
   fix_mat(FixRest,NewWe);
fix_mat([],We) ->
   We#we.mat.

fix_edge_1(RemoveEdge,Vert1,Vert2,Es,Orig) ->
   New = gb_trees:empty(),
   fix_edge_1(RemoveEdge,Vert1,Vert2,Es,Orig,New).

fix_edge_1(RemoveEdge,Vert1,Vert2,Es,Orig,Result) ->
   case gb_trees:size(Es) of
      0 -> Result;
      _ ->
         {Key,#edge{vs=V1,ve=V2,a=C1,b=C2,lf=LF,rf=RF,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS},Es2} = gb_trees:take_smallest(Es),
         if
            Key == RemoveEdge -> fix_edge_1(RemoveEdge,Vert1,Vert2,Es2,Orig,Result);
            true ->
               #edge{vs=OV1,a=OC1,b=OC2,lf=OLF,rf=ORF,ltpr=OLP,ltsu=OLS,rtpr=ORP,rtsu=ORS} = gb_trees:get(RemoveEdge,Orig#we.es),
               case V1 of
                  Vert1 -> NV1=Vert2,
                           case OV1 of
                              Vert2 -> NC1=OC1;
                              _ -> NC1=OC2
                           end;
                  _ -> NV1=V1,
                       NC1=C1
               end,
               case V2 of
                  Vert1 -> NV2=Vert2,
                           case OV1 of
                              Vert2 -> NC2=OC1;
                              _ -> NC2=OC2
                           end;
                  _ -> NV2=V2,
                       NC2=C2
               end,
               case LP of
                  RemoveEdge ->
                     if
                        LF == OLF -> NLP = OLP;
                        true -> NLP = ORP
                     end;
                  _ -> NLP = LP
               end,
               case LS of
                  RemoveEdge ->
                     if
                        LF == OLF -> NLS = OLS;
                        true -> NLS = ORS
                     end;
                  _ -> NLS = LS
               end,
               case RP of
                  RemoveEdge ->
                     if
                        RF == ORF -> NRP = ORP;
                        true -> NRP = OLP
                     end;
                  _ -> NRP = RP
               end,
               case RS of
                  RemoveEdge ->
                     if
                        RF == ORF -> NRS = ORS;
                        true -> NRS = OLS
                     end;
                  _ -> NRS = RS
               end,
               NewEdge = #edge{vs=NV1,ve=NV2,a=NC1,b=NC2,lf=LF,rf=RF,ltpr=NLP,ltsu=NLS,rtpr=NRP,rtsu=NRS},
               Result2 = gb_trees:insert(Key,NewEdge,Result),
               fix_edge_1(RemoveEdge,Vert1,Vert2,Es2,Orig,Result2)
         end
   end.

fix_edge_2([Remove|FixRest],Etab) ->
   NewEtab = remove_face(Remove,Etab),
   fix_edge_2(FixRest,NewEtab);
fix_edge_2([],Etab) ->
   Etab.

remove_face(Face,Etab) ->
   [Key1,Key2]=find_edge2(Face,Etab),
   OldEdge = gb_trees:get(Key1,Etab),
   NewEdge = gb_trees:get(Key2,Etab),
   K1 = find_edge_to_face(OldEdge,Face),
   K2 = find_edge_to_face(NewEdge,Face),
   if
      K1 == K2 ->
         if
            K1 == left ->
               NewEdge2=NewEdge#edge{lf=OldEdge#edge.rf, ltpr=OldEdge#edge.rtpr, ltsu=OldEdge#edge.rtsu};
            true ->
               NewEdge2=NewEdge#edge{rf=OldEdge#edge.lf, rtpr=OldEdge#edge.ltpr, rtsu=OldEdge#edge.ltsu}
         end;
      true ->
         if
            K1 == left ->
               NewEdge2=NewEdge#edge{rf=OldEdge#edge.rf, rtpr=OldEdge#edge.rtpr, rtsu=OldEdge#edge.rtsu};
            true ->
               NewEdge2=NewEdge#edge{lf=OldEdge#edge.lf, ltpr=OldEdge#edge.ltpr, ltsu=OldEdge#edge.ltsu}
         end
   end,
   Etab2 = gb_trees:update(Key2,NewEdge2,Etab),
   Etab3 = gb_trees:delete(Key1,Etab2),
   substitute(Key1,Key2,Etab3).

substitute(This,WithThis,Etab) ->
   New = gb_trees:empty(), 
   substitute(This,WithThis,Etab,New).

substitute(This,WithThis,Etab,New) ->
   case gb_trees:size(Etab) of 
      0 -> New;
      _ ->
         {Key,Edge,Etab2} = gb_trees:take_smallest(Etab),
         NewEdge = substitute_1(This,WithThis,Edge),
         New2 = gb_trees:insert(Key,NewEdge,New),
         substitute(This,WithThis,Etab2,New2)
   end.

substitute_1(This,WithThis,Edge) ->
   if
      Edge#edge.rtpr == This -> Rtpr = WithThis;
      true -> Rtpr = Edge#edge.rtpr
   end,
   if
      Edge#edge.rtsu == This -> Rtsu = WithThis;
      true -> Rtsu = Edge#edge.rtsu
   end,
   if
      Edge#edge.ltpr == This -> Ltpr = WithThis;
      true -> Ltpr = Edge#edge.ltpr
   end,
   if
      Edge#edge.ltsu == This -> Ltsu = WithThis;
      true -> Ltsu = Edge#edge.ltsu
   end,
   Edge#edge{ltsu=Ltsu, ltpr=Ltpr, rtsu=Rtsu, rtpr=Rtpr}.

fix_hardedge(Etab,He) ->
   New = gb_sets:empty(),
   fix_hardedge(Etab,He,New).

fix_hardedge(Etab,He,Result) ->
   case gb_sets:size(He) of
      0 -> Result;
      _ -> 
         {Edge,He2} = gb_sets:take_smallest(He),
         case gb_trees:is_defined(Edge, Etab) of
            true -> Result2 = gb_sets:add(Edge,Result);
            _ -> Result2 = Result
         end,
         fix_hardedge(Etab,He2,Result2)
   end.

needs_cleanup(RemoveEdge,Orig) ->
   #edge{lf=LF,rf=RF} = gb_trees:get(RemoveEdge,Orig#we.es),
   NLF = wings_face:vertices(LF, Orig),
   NRF = wings_face:vertices(RF, Orig),
   if
      NLF < 4 -> FixMe0 = [LF];
      true -> FixMe0 = []
   end,
   if
      NRF < 4 -> FixMe = [RF|FixMe0];
      true -> FixMe = FixMe0
   end,
   FixMe.

find_edge(Vert1,Vert2,Es) ->
   {Key,#edge{vs=V1,ve=V2},Es2} = gb_trees:take_smallest(Es),
   if
      ((Vert1 == V1) and (Vert2 == V2)) or ((Vert1 == V2) and (Vert2 == V1)) -> Key;
      true -> find_edge(Vert1,Vert2,Es2)
   end.

find_edge2(Face,Etab) ->
   find_edge2(Face,Etab,[]).

find_edge2(Face,Es,Result) ->
   case gb_trees:size(Es) of
      0 -> Result;
      _ ->
         {Key,#edge{lf=LF,rf=RF},Es2} = gb_trees:take_smallest(Es),
         if
            (LF == Face) or (RF == Face) -> Result2=[Key|Result];
            true -> Result2=Result
         end,
         find_edge2(Face,Es2,Result2)
   end.

find_edge_to_face(#edge{lf=LF,rf=RF},Face) ->
   case Face of
      LF -> left;
      RF -> right;
      _ -> none
   end.

get_verts(_Face, LastEdge, LastEdge, _We, [_|_]=Acc) -> Acc;
get_verts(Face, Edge, LastEdge, We, Acc) ->
   #we{es=Etab} = We,
   case catch gb_trees:get(Edge, Etab) of
      #edge{vs=V,lf=Face,ltsu=Next} ->
         get_verts(Face, Next, LastEdge, We, [V|Acc]);
      #edge{ve=V,rf=Face,rtsu=Next} ->
         get_verts(Face, Next, LastEdge, We, [V|Acc])
   end.
