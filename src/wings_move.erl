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
%%     $Id: wings_move.erl,v 1.1 2001/08/14 18:16:38 bjorng Exp $
%%

-module(wings_move).
-export([setup/2]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3]).

setup(Type, #st{selmode=Mode}=St) ->
    Vec = make_vector(Type, St),
    Tvs = wings_sel:fold_shape(fun(Sh, Items, Acc) ->
				       setup_1(Mode, Sh, Items, Vec, Acc)
			       end, [], St),
    wings_drag:init_drag(Tvs, constraint(Type), St).

setup_1(Mode, #shape{id=Id,sh=We}=Sh, Items, Vec, Acc) ->
    Tv = case Mode of
	     vertex -> vertices_to_vertices(gb_sets:to_list(Items), We, Vec);
	     edge -> edges_to_vertices(gb_sets:to_list(Items), We, Vec);
	     face -> faces_to_vertices(Items, We, Vec);
	     body -> body_to_vertices(Sh, Vec)
	 end,
    [{Id,Tv}|Acc].

constraint(free) -> view_dependent;
constraint(intrude) -> {0.025,1.0E200};
constraint(Other) -> none.
    
make_vector(x, St) -> {?GROUND_GRID_SIZE,0.0,0.0};
make_vector(y, St) -> {0.0,?GROUND_GRID_SIZE,0.0};
make_vector(z, St) -> {0.0,0.0,?GROUND_GRID_SIZE};
make_vector(free, St) -> free;
make_vector(normal, St) -> normal;
make_vector(intrude, St) -> normal.

%%
%% Conversion of vertice selections to vertices. :-)
%% Not entirely pointless, as we'll need to add vectors for
%% the points (vertices).
%%

vertices_to_vertices(Vs, We, normal) -> vertex_normals(We, Vs);
vertices_to_vertices(Vs, We, Vec) -> make_tvs(Vs, Vec).

vertex_normals(We, Vs) ->
    foldl(fun(V, Acc) ->
		  Vec = wings_mat:mul(wings_vertex:normal(V, We),
				      float(?GROUND_GRID_SIZE)),
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
		       EdgeDir = wings_mat:norm(wings_mat:subtract(VbPos, VaPos)),
 		       NL = wings_face:normal(FaceL, We),
 		       NR = wings_face:normal(FaceR, We),
		       Normal = wings_mat:norm(wings_mat:add(NL, NR)),
		       [{Va,{Normal,VaPos,EdgeDir}},
			{Vb,{Normal,VbPos,wings_mat:negate(EdgeDir)}}|D0]
	       end, [], Es),
    average(Vs);
edges_to_vertices(Es, #we{es=Etab}, Vec) ->
    Vs0 = foldl(fun(Edge, Acc) ->
			#edge{vs=Vstart,ve=Vend} = gb_trees:get(Edge, Etab),
			[Vstart,Vend|Acc]
		end, [], Es),
    make_tvs(ordsets:from_list(Vs0), Vec).

average(Vs) ->
    R = sofs:relation(Vs),
    F = sofs:relation_to_family(R),
    foldl(fun average/2, [], sofs:to_external(F)).

average({V,Info}, Acc) ->
    Normal = wings_mat:mul(average_normals(Info), ?GROUND_GRID_SIZE),
    [{Normal,[V]}|Acc].

average_normals([{Normal,_,_}]) -> Normal;
average_normals([{Na,Orig,Da}|[{Nb,_,Db}|_]=T]) ->
    %% This code is probably obvious. :-)
    Oa = wings_mat:add(Orig, Na),
    Ob = wings_mat:add(Orig, Nb),
    Diff = wings_mat:subtract(Oa, Ob),
    A = wings_mat:dot_product(Da, Da),
    B = -wings_mat:dot_product(Da, Db),
    C = wings_mat:dot_product(Db, Db),
    D = wings_mat:dot_product(Da, Diff),
    Det = A*C-B*B,
    if
	Det*Det >= 1.0E-9*abs(A*B) ->
	    E = -wings_mat:dot_product(Db, Diff),
	    S = (B*E-C*D)/Det,
	    NewPos = wings_mat:add(Oa, wings_mat:mul(Da, S)),
	    wings_mat:subtract(NewPos, Orig);
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
    make_tvs(gb_sets:to_list(wings_face:to_vertices(Faces, We)), Vec).

face_normal(Face, Vs, Vtab, Acc) ->
    Normal = wings_face:face_normal(Vs, Vtab),
    foldl(fun(V, A) -> [{V,Normal}|A] end, Acc, Vs).

face_average(Vs, Vtab) ->
    R = sofs:relation(Vs),
    F = sofs:relation_to_family(R),
    foldl(fun({V,Ns}, Acc) ->
		  N0 = face_average_normals(V, Ns, Vtab),
		  N = wings_mat:mul(N0, ?GROUND_GRID_SIZE),
		  [{N,[V]}|Acc]
	  end, [], sofs:to_external(F)).

face_average_normals(V, [Normal], Vtab) -> Normal;
face_average_normals(V, [Na,Nb], Vtab) ->
    N = wings_mat:norm(wings_mat:add(Na, Nb)),
    Dot = wings_mat:dot_product(N, Na),
    wings_mat:divide(N, Dot); 
face_average_normals(V, [Na,Nb,Nc|T], Vtab) ->
    Vpos = wings_vertex:pos(V, Vtab),
    Nao = wings_mat:add(Vpos, Na),
    Nbo = wings_mat:add(Vpos, Nb),
    Nco = wings_mat:add(Vpos, Nc),
    {A,D,G} = Na,
    {B,E,H} = Nb,
    {C,F,I} = Nc,
    J = wings_mat:dot_product(Nao, Na),
    K = wings_mat:dot_product(Nbo, Nb),
    L = wings_mat:dot_product(Nco, Nc),

    %% Calculate intersection of three planes using Cramer's rule.
    if
	float(A), float(B), float(C),
	float(D), float(E), float(F),
	float(G), float(H), float(I),
	float(J), float(K), float(L) ->
	    EiMinusHf = E*I - H*F,
	    GFMinusDI = G*F - D*I,
	    DHMinusEG = D*H - E*G,
	    JCMinusAL = J*C - A*L,
	    AKMinusJB = A*K - J*B,
	    BLMinusKC = B*L - K*C,
	    case A*EiMinusHf + B*GFMinusDI + C*DHMinusEG of
		M when abs(M) < 1.0E-4 ->
		    remove_eq(V, Na, Nb, Nc, T, Vtab);
		M ->
		    X = (J*EiMinusHf + K*GFMinusDI + L*DHMinusEG)/M,
		    Y = (I*AKMinusJB + H*JCMinusAL + G*BLMinusKC)/M,
		    Z = -(F*AKMinusJB + E*JCMinusAL + D*BLMinusKC)/M,
		    wings_mat:subtract({X,Y,Z}, Vpos)
	    end
    end.

remove_eq(V, Na, Nb, Nc, T, Vtab) ->
    E = 1.0E-4,
    case aeq(Na, Nb, E) of
	true ->
	    face_average_normals(V, [Na,Nc|T], Vtab);
	false ->
	    case aeq(Na, Nc, E) of
		true ->
		    face_average_normals(V, [Na,Nb|T], Vtab);
		false ->
		    face_average_normals(V, [Nb,Nc|T], Vtab)
	    end
    end.

aeq({Xa,Ya,Za}, {Xb,Yb,Zb}, E) when abs(Xa-Xb) < E,
				    abs(Ya-Yb) < E,
				    abs(Za-Zb) < E ->
    true;
aeq(_, _, _) -> false.

%%
%% Conversion of body selections (entire objects) to vertices.
%%

body_to_vertices(Sh, Vec) ->
    translate_fun(Vec).

translate_fun(free) ->
    fun(#shape{matrix=Matrix0}, Dx, Dy, #st{azimuth=Az,elevation=El}) ->
	    wings_io:message(lists:flatten(io_lib:format("X:~10p Y:~10p",
							 [Dx,Dy]))),
	    G = ?GROUND_GRID_SIZE,
	    M0 = wings_mat:rotate_y(-Az),
	    M1 = wings_mat:mult(M0, wings_mat:rotate_x(-El)),
	    M2 = wings_mat:mult(M1, wings_mat:scale(G, G, G)),
	    {Xt,Yt,Zt,_} = wings_mat:mult(M2, {Dx,Dy,0.0,1.0}),
	    M3 = wings_mat:translate(Xt, Yt, Zt),
	    Matrix = wings_mat:mult(wings_mat:transpose(Matrix0), M3),
	    {shape_matrix,wings_mat:transpose(Matrix)}
    end;
translate_fun({Xt0,Yt0,Zt0}) ->
    fun(Sh, Dx, Dy, St) when float(Dx) ->
	    wings_io:message(lists:flatten(io_lib:format("X:~10p", [Dx]))),
	    Xt = Xt0*Dx,
	    Yt = Yt0*Dx,
	    Zt = Zt0*Dx,
	    {shape_matrix,wings_mat:transpose(wings_mat:translate(Xt, Yt, Zt))}
    end.

%%%
%%% Utilities.
%%%

make_tvs(Vs, free) ->
    fun(Sh, Dx, Dy, St) ->
	    Matrix = free_translation(Dx, Dy, St),
	    {tvs,[{{free,Matrix},Vs}]}
    end;
make_tvs(Vs, Vec) -> [{Vec,Vs}].

free_translation(Dx, Dy, #st{azimuth=Az,elevation=El}) ->
    G = ?GROUND_GRID_SIZE,
    M0 = wings_mat:rotate_y(-Az),
    M = wings_mat:mult(M0, wings_mat:rotate_x(-El)),
    wings_mat:mult(M, wings_mat:scale(G, G, G)).
