%%
%%  wings_move.erl --
%%
%%     This module implements the Move command.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_move.erl,v 1.19 2001/12/23 17:48:06 bjorng Exp $
%%

-module(wings_move).
-export([setup/2]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,sort/1]).

setup(Type, #st{selmode=body,sel=Sel}=St) ->
    Vec = wings_util:make_vector(Type),
    Fun = translate_fun(Vec),
    Ids = [{Id,Fun} || {Id,_} <- Sel],
    wings_drag:init_drag({matrix,Ids}, constraint(Type), distance, St);
setup(Type, #st{selmode=Mode}=St) ->
    Vec = wings_util:make_vector(Type),
    Tvs0 = wings_sel:fold_shape(fun(Sh, Items, Acc) ->
					setup_1(Mode, Sh, Items, Vec, Acc)
				end, [], St),
    Tvs = case Mode of
	      body -> {matrix,Tvs0};
	      Other -> Tvs0
	  end,
    wings_drag:init_drag(Tvs, constraint(Type), distance, St).

setup_1(Mode, #shape{id=Id,sh=We}=Sh, Items, Vec, Acc) ->
    Tv = case Mode of
	     vertex -> vertices_to_vertices(gb_sets:to_list(Items), We, Vec);
	     edge -> edges_to_vertices(Items, We, Vec);
	     face -> faces_to_vertices(Items, We, Vec);
	     body -> body_to_vertices(Sh, Vec)
	 end,
    [{Id,Tv}|Acc].

constraint(free) -> view_dependent;
constraint(intrude) -> {0.025,1.0E200};
constraint(Other) -> none.
    
%%
%% Conversion of vertice selections to vertices. :-)
%% Not entirely pointless, as we'll need to add vectors for
%% the points (vertices).
%%

vertices_to_vertices(Vs, We, normal) -> vertex_normals(We, Vs);
vertices_to_vertices(Vs, We, Vec) -> make_tvs(Vs, Vec, We).

vertex_normals(We, Vs) ->
    foldl(fun(V, Acc) ->
		  Vec = wings_vertex:normal(V, We),
		  [{Vec,[V]}|Acc]
	  end, [], Vs).

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Es, We, normal) ->
    #we{es=Etab,vs=Vtab} = We,
    Vs = foldl(fun(Edge, D0) ->
		       #edge{vs=Va,ve=Vb,lf=FaceL,rf=FaceR} =
			   gb_trees:get(Edge, Etab),
		       VaPos = wings_vertex:pos(Va, Vtab),
		       VbPos = wings_vertex:pos(Vb, Vtab),
		       EdgeDir = e3d_vec:norm(e3d_vec:sub(VbPos, VaPos)),
 		       NL = wings_face:normal(FaceL, We),
 		       NR = wings_face:normal(FaceR, We),
		       Normal = e3d_vec:norm(e3d_vec:add(NL, NR)),
		       [{Va,{Normal,VaPos,EdgeDir}},
			{Vb,{Normal,VbPos,e3d_vec:neg(EdgeDir)}}|D0]
	       end, [], gb_sets:to_list(Es)),
    average(Vs);
edges_to_vertices(Es, We, Vec) ->
    make_tvs(wings_edge:to_vertices(Es, We), Vec, We).

average(Vs) ->
    R = sofs:relation(Vs),
    F = sofs:relation_to_family(R),
    foldl(fun average/2, [], sofs:to_external(F)).

average({V,Info}, Acc) ->
    Normal = average_normals(Info),
    [{Normal,[V]}|Acc].

average_normals([{Normal,_,_}]) -> Normal;
average_normals([{Na,Orig,Da}|[{Nb,_,Db}|_]=T]) ->
    %% This code is probably obvious. :-)
    Oa = e3d_vec:add(Orig, Na),
    Ob = e3d_vec:add(Orig, Nb),
    Diff = e3d_vec:sub(Oa, Ob),
    A = e3d_vec:dot(Da, Da),
    B = -e3d_vec:dot(Da, Db),
    C = e3d_vec:dot(Db, Db),
    D = e3d_vec:dot(Da, Diff),
    Det = A*C-B*B,
    if
	Det*Det >= 1.0E-9*abs(A*B) ->
	    E = -e3d_vec:dot(Db, Diff),
	    S = (B*E-C*D)/Det,
	    NewPos = e3d_vec:add(Oa, e3d_vec:mul(Da, S)),
	    e3d_vec:sub(NewPos, Orig);
	true ->					%Parallel edges
	    average_normals(T)
    end.

%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Faces, We, normal) ->
    #we{fs=Ftab,es=Etab,vs=Vtab} = We,
    Vs = foldl(fun(Face, Acc0) ->
		       Vs = wings_face:surrounding_vertices(Face, We),
		       face_normal(Face, Vs, Vtab, Acc0)
	       end, [], gb_sets:to_list(Faces)),
    face_average(Vs, Vtab);
faces_to_vertices(Faces, We, Vec) ->
    make_tvs(wings_face:to_vertices(Faces, We), Vec, We).

face_normal(Face, Vs, Vtab, Acc) ->
    Normal = wings_face:face_normal(Vs, Vtab),
    foldl(fun(V, A) -> [{V,Normal}|A] end, Acc, Vs).

face_average(Vs, Vtab) ->
    R = sofs:relation(Vs),
    F = sofs:relation_to_family(R),
    foldl(fun({V,Ns0}, Acc) ->
		  Ns = join_eq(Ns0),
		  case face_average_normals(V, Ns, Vtab) of
		      none -> Acc;
		      N -> [{N,[V]}|Acc]
		  end
	  end, [], sofs:to_external(F)).

face_average_normals(V, [], Vtab) -> none;
face_average_normals(V, [Normal], Vtab) -> Normal;
face_average_normals(V, [Na,Nb], Vtab) ->
    N = e3d_vec:norm(e3d_vec:add(Na, Nb)),
    Dot = e3d_vec:dot(N, Na),
    e3d_vec:divide(N, Dot); 
face_average_normals(V, [Na,Nb,Nc|T]=Ns, Vtab) ->
    Vpos = wings_vertex:pos(V, Vtab),
    Nao = e3d_vec:add(Vpos, Na),
    Nbo = e3d_vec:add(Vpos, Nb),
    Nco = e3d_vec:add(Vpos, Nc),
    {A,D,G} = Na,
    {B,E,H} = Nb,
    {C,F,I} = Nc,
    J = e3d_vec:dot(Nao, Na),
    K = e3d_vec:dot(Nbo, Nb),
    L = e3d_vec:dot(Nco, Nc),

    %% Calculate intersection of three planes using Cramer's rule.
    if
	is_float(A), is_float(B), is_float(C),
	is_float(D), is_float(E), is_float(F),
	is_float(G), is_float(H), is_float(I),
	is_float(J), is_float(K), is_float(L) ->
	    EiMinusHf = E*I - H*F,
	    GFMinusDI = G*F - D*I,
	    DHMinusEG = D*H - E*G,
	    JCMinusAL = J*C - A*L,
	    AKMinusJB = A*K - J*B,
	    BLMinusKC = B*L - K*C,
	    case A*EiMinusHf + B*GFMinusDI + C*DHMinusEG of
		M when abs(M) < 1.0E-3 ->
		    none;
		M ->
		    X = (J*EiMinusHf + K*GFMinusDI + L*DHMinusEG)/M,
		    Y = (I*AKMinusJB + H*JCMinusAL + G*BLMinusKC)/M,
		    Z = -(F*AKMinusJB + E*JCMinusAL + D*BLMinusKC)/M,
		    e3d_vec:sub({X,Y,Z}, Vpos)
	    end
    end.

join_eq(Ns0) ->
    Ns1 = [add_sort_key(N) || N <- Ns0, not e3d_vec:is_zero(N)],
    Ns = sofs:relation(Ns1),
    Fam = sofs:relation_to_family(Ns),
    [N || {_,[N|_]} <- sofs:to_external(Fam)].

add_sort_key({X,Y,Z}=N) ->
    S = 1000,
    {{trunc(X*S),trunc(Y*1000),trunc(Z*1000)},N}.

%%
%% Conversion of body selections (entire objects) to vertices.
%%

body_to_vertices(Sh, Vec) ->
    translate_fun(Vec).

translate_fun(free) ->
    fun(Matrix0, {Dx,Dy}) ->
	    wings_drag:message([Dx,Dy], distance),
	    #view{azimuth=Az,elevation=El} = wings_view:current(),
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
	    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
	    {Xt,Yt,Zt} = e3d_mat:mul_point(M1, {Dx,Dy,0.0}),
	    e3d_mat:translate(Xt, Yt, Zt)
    end;
translate_fun({Xt0,Yt0,Zt0}) ->
    fun(Matrix0, Dx) when float(Dx) ->
	    wings_drag:message([Dx], distance),
	    Xt = Xt0*Dx,
	    Yt = Yt0*Dx,
	    Zt = Zt0*Dx,
	    e3d_mat:translate(Xt, Yt, Zt)
    end.

%%%
%%% Utilities.
%%%

make_tvs(Vs, free, We) ->
    VsPos = wings_util:add_vpos(Vs, We),
    {Vs,move_fun(VsPos)};
make_tvs(Vs, Vec, We) -> [{Vec,Vs}].

move_fun(VsPos) ->
    fun(view_changed, NewWe) ->
	    move_fun(wings_util:update_vpos(VsPos, NewWe));
       ({Dx,Dy}, Acc) ->
	    #view{azimuth=Az,elevation=El} = wings_view:current(),
	    M0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
	    M = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
	    {Xt,Yt,Zt} = e3d_mat:mul_point(M, {Dx,Dy,0.0}),
	    foldl(fun({V,#vtx{pos={X,Y,Z}}=Rec}, A) -> 
			  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
			  [{V,Rec#vtx{pos=Pos}}|A]
		  end, Acc, VsPos)
    end.
