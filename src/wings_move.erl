%%
%%  wings_move.erl --
%%
%%     This module implements the Move command.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_move.erl,v 1.24 2002/01/26 11:26:51 bjorng Exp $
%%

-module(wings_move).
-export([setup/2,setup_we/4]).

-include("wings.hrl").
-import(lists, [map/2,foldr/3,foldl/3,sort/1]).

setup(Type, #st{selmode=body,sel=Sel}=St) ->
    Vec = vector(Type),
    Fun = translate_fun(Vec),
    Ids = [{Id,Fun} || {Id,_} <- Sel],
    wings_drag:init_drag({matrix,Ids}, constraint(Type), distance, St);
setup(Type, #st{selmode=Mode}=St) ->
    Vec = vector(Type),
    Tvs = wings_sel:fold(fun(Items, We, Acc) ->
				 setup_1(Mode, We, Items, Vec, Acc)
			 end, [], St),
    wings_drag:init_drag(Tvs, constraint(Type), distance, St).

setup_1(Mode, #we{id=Id}=We, Items, Vec, Acc) ->
    Tv = setup_we(Mode, Vec, Items, We),
    [{Id,Tv}|Acc].

setup_we(vertex, Vec, Items, We) ->
    vertices_to_vertices(gb_sets:to_list(Items), We, Vec);
setup_we(edge, Vec, Items, We) ->
    edges_to_vertices(Items, We, Vec);
setup_we(face, Vec, Items, We) ->
    faces_to_vertices(Items, We, Vec).

constraint(free) -> view_dependent;
constraint(intrude) -> {0.025,1.0E200};
constraint(Other) -> none.

vector({_,{_,_,_}=Vec}) -> Vec;
vector(Other) -> wings_util:make_vector(Other).

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
		  Ns = filter_normals(Ns0),
		  N = face_average_normals(V, Ns, Vtab),
		  [{N,[V]}|Acc]
	  end, [], sofs:to_external(F)).

face_average_normals(V, [Na], Vtab) -> Na;
face_average_normals(V, [Na,Nb], Vtab) ->
    N = e3d_vec:norm(e3d_vec:add(Na, Nb)),
    case e3d_vec:dot(N, Na) of
	Dot when abs(Dot) < 1.0E-6 ->
	    e3d_vec:add(Na, Nb);
	Dot ->
	    e3d_vec:divide(N, Dot)
    end;
face_average_normals(V, [Na,Nb,Nc], Vtab) ->
    %% The caller assures that the normals are not co-linear.
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
		M when abs(M) < 0.0001 ->
		    %% Should not happen, but just in case...
		    face_average_normals(V, [Na,Nb], Vtab);
		M ->
		    X = (J*EiMinusHf + K*GFMinusDI + L*DHMinusEG)/M,
		    Y = (I*AKMinusJB + H*JCMinusAL + G*BLMinusKC)/M,
		    Z = -(F*AKMinusJB + E*JCMinusAL + D*BLMinusKC)/M,
		    e3d_vec:sub({X,Y,Z}, Vpos)
	    end
    end.

%% Filter out normals that are too close to each other.
filter_normals([_]=Ns) -> Ns;
filter_normals([_,_]=Ns) -> Ns;
filter_normals([Na|[N0|_]=Ns]) ->
    %% Three or more normals. We want three normals that point
    %% in as different directions as possible, or just two normals
    %% if we can't find three different enough.
    Nb = largest_angle(Ns, Na),
    N1 = e3d_vec:cross(Na, Nb),
    case smallest_angle(Ns, N1) of
	{Nc,Dot} when Dot < 0.01 ->
	    %% The third normal is not usable. It is too close to
	    %% the plane of the other two.
	    [Na,Nb];
	{Nc,Dot} ->
	    %% The third normal is OK.
	    [Na,Nb,Nc]
    end.

%% Find the normal with the greatest angle from Na.
largest_angle([N|Ns], Na) ->
    Dot = abs(e3d_vec:dot(Na, N)),
    largest_angle(Ns, Na, Dot, N).
    
largest_angle([N|Ns], Na, OldDot, OldN) ->
    case abs(e3d_vec:dot(Na, N)) of
	Dot when Dot < OldDot ->
	    largest_angle(Ns, Na, Dot, N);
	Dot ->
	    largest_angle(Ns, Na, OldDot, OldN)
    end;
largest_angle([], Na, Dot, N) -> N.

%% Find the normal with the smallest angle from Na.
smallest_angle([N|Ns], Na) ->
    Dot = abs(e3d_vec:dot(Na, N)),
    smallest_angle(Ns, Na, Dot, N).
    
smallest_angle([N|Ns], Na, OldDot, OldN) ->
    case abs(e3d_vec:dot(Na, N)) of
	Dot when Dot > OldDot ->
	    smallest_angle(Ns, Na, Dot, N);
	Dot ->
	    smallest_angle(Ns, Na, OldDot, OldN)
    end;
smallest_angle([], Na, Dot, N) -> {N,Dot}.

%%
%% Conversion of body selections (entire objects) to vertices.
%%

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
