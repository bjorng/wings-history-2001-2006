%%%-------------------------------------------------------------------
%%% File    : auv_mapping.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Provides different uv-parametrisation algorihms
%%%
%%% Created :  4 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2002 Dan Gudmundsson, Raimo Niskanen, Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv_mapping.erl,v 1.43 2003/08/12 10:25:40 bjorng Exp $

%%%%%% Least Square Conformal Maps %%%%%%%%%%%%
%% Algorithms based on the paper, 
%% (now probably totally ruined by me or Raimo)
%% 'Least Square Conformal Maps for Automatic Texture Generation Atlas'
%% by Bruno Levy, Sylvain Petitjean, Nicolas Ray, Jerome Mailot
%% Presented on Siggraph 2002
%%
%%%% The Conjugate Gradient Method (trad)
%% Algorithms based on the paper:
%%       An Introduction to 
%%  the Conjugate Gradient Method
%%    Without the Agonizing Pain
%% by
%%  Jonathan Richard Shewchuk, March 7, 1994
%%
%% The Column Norm Preconditioning was stumbeled upon, just briefly
%% mentioned, in the paper:
%%      Incomplete Factorization Preconditioning
%%        for Linear Least Squares Problems
%% by
%%      Xiaoge Wang, 1994

%% All credits about the LSQCM implementation goes to Raimo, who
%% implemented the lot. 

-module(auv_mapping).

-export([lsq/2, lsq/3, find_pinned/2]). % Debug entry points
-export([stretch_opt/2, area2d2/3,area3d/3]).

-compile(export_all).

-ifdef(lsq_standalone).
-define(DBG(Fmt,Args), io:format(?MODULE_STRING++":~p: "++(Fmt), 
				       [?LINE | (Args)])).
-define(TC(Cmd), tc(?MODULE, ?LINE, fun () -> Cmd end)).
tc(Module, Line, Fun) ->
    {A1,A2,A3} = erlang:now(),
    Result = Fun(),
    {B1,B2,B3} = erlang:now(),
    Time = ((B1-A1)*1000000 + (B2-A2))*1000000 + B3-A3,
    io:format("~p:~p: ~.3f ms:~n", [Module, Line, Time/1000]),
    Result.
-else.

-export([map_chart/3, project2d/3]).

-include("wings.hrl").
-include("auv.hrl").
-include("e3d.hrl").

%%% From camera would look something like this!! 
%%% It actually worked once :-) 
%project -> %% Only one Area
%    Clustered = gb_trees:keys(Ftab0),
%    _CI = [_Aim, Azi, Elev, _Track] = 
%	wpa:camera_info([aim,azimuth, elevation, tracking]),
%    N0 = {0.0, 0.0, 1.0},
%    AziRot = e3d_mat:rotate(Azi, {0.0,1.0,0.0}),
%		N1 = e3d_mat:mul_vector(AziRot, N0),
%    ElevRot = e3d_mat:rotate(Elev, {1.0,0.0,0.0}),
%    N2 = e3d_mat:mul_vector(ElevRot, N1),
%    %%		?DBG("Projected by ~p using camera ~p ~n", [N2, _CI]),
%    [create_area(Clustered, N2, We0)];

map_chart(Type, Chart, We) ->
    case wpa:face_outer_edges(Chart, We) of
	[] ->
	    {error,"A closed surface cannot be mapped. "
	     "(Either divide it into into two or more charts, "
	     "or cut it along some edges.)"};
	[[_,_]] ->
	    {error,"A cut in a closed surface must consist of at least two edges."};
	[_] ->
	    map_chart_1(Type, Chart, We);
	[_,_|_] ->
	    map_chart_1(Type, Chart, We)
	    %% For the moment at least, allow holes.
            %%{error,"A chart is not allowed to have holes."}
    end.

map_chart_1(Type, Chart, We) ->
    case catch map_chart_2(Type, Chart, We) of
	{'EXIT',{badarith,_}} when Type == project ->
	    {error,"Numeric problem. (Probably impossible to calculate chart normal.)"};
	{'EXIT',{badarith,_}} ->
	    {error,"Numeric problem."};
	{'EXIT',Reason} ->
	    Msg = io_lib:format("Internal error: ~P", [Reason,10]),
	    {error,lists:flatten(Msg)};
	Other -> Other
    end.

map_chart_2(project, C, We) -> projectFromChartNormal(C, We);
map_chart_2(lsqcm, C, We) -> lsqcm(C, We);
map_chart_2(lsqcm2, C, We) -> lsqcm2(C, We).


projectFromChartNormal(Chart, We) ->
    CalcNormal = fun(Face, Sum) ->
			 Normal = wings_face:normal(Face, We),
			 Vs0 = wpa:face_vertices(Face, We),
			 Area = calc_area(Vs0,Normal, We),
			 %%         ?DBG("Area was ~p ~n", [Area]),
			 e3d_vec:add(Sum, e3d_vec:mul(Normal, Area))
		 end,
    N0 = lists:foldl(CalcNormal, e3d_vec:zero(), Chart),
    Normal = e3d_vec:norm(N0),
    Vs0 = wings_face:to_vertices(Chart, We),
    project2d(Vs0, Normal, We).

project2d(Vs, Normal, We) ->
    Rot = e3d_mat:rotate_s_to_t(Normal,{0.0,0.0,1.0}),
    [{V,e3d_mat:mul_point(Rot, wings_vertex:pos(V, We))} || V <- Vs].

%% Alg. found in comp.graphics.algorithms faq
%% To be correct it needs the polygons to flat but we
%% don't need to be 100% correct.
calc_area(Vs0, Normal, We) ->
    [V|Vs] = [wings_vertex:pos(V, We) || V <- Vs0],
    Sum = sum_crossp([V|Vs] ++ [V], e3d_vec:zero()),
    0.5 * abs(e3d_vec:dot(Normal, Sum)).

sum_crossp([V1,V2|Vs], Acc) ->
    Cross = e3d_vec:cross(V1,V2),
    sum_crossp([V2|Vs], e3d_vec:add(Acc, Cross));
sum_crossp([_Last], Acc) ->
    Acc.

project_and_triangulate([Face|Fs], We, I, Tris,Area) ->
    ?VALIDATE_MODEL(We),
    Normal = wings_face:normal(Face, We),
    Vs0 = wpa:face_vertices(Face, We),
    Vs2 = project2d(Vs0, Normal, We),
    NewArea = calc_area(Vs0, Normal, We) + Area,
    case length(Vs2) of 
	N when N > 3 ->
	    {Ids,Cds,All} = setup_tri_vs(Vs2,0,[],[],[]),
	    NewFs = e3d_mesh:triangulate_face(#e3d_face{vs=Ids}, Cds),
	    VT = gb_trees:from_orddict(All),
	    {Add, I1} = get_verts(NewFs, I, VT, []),
	    project_and_triangulate(Fs,We,I1,Add ++ Tris,NewArea);
	3 ->
	    Vs3 = [{Vid, {Vx, Vy}} || {Vid,{Vx,Vy,_}} <- Vs2],
	    project_and_triangulate(Fs,We,I,[{Face, Vs3}|Tris],NewArea);
	Else ->
	    io:format("Error: Can't triangulate face ~p with ~p vertices~n",
		      [Face, Else]),
	    project_and_triangulate(Fs,We,I,Tris,Area)
    end;
project_and_triangulate([],_,_,Tris,Area) -> 
    {Tris,Area}.

setup_tri_vs([{Old,Coord}|Vs],Id,Ids,Cds,All) ->
    setup_tri_vs(Vs,Id+1,[Id|Ids],[Coord|Cds],[{Id,{Old,Coord}}|All]);
setup_tri_vs([],_,Ids,Cds,All) ->
    {lists:reverse(Ids),lists:reverse(Cds),lists:reverse(All)}.

get_verts([#e3d_face{vs = Vs}|Fs],I,Coords,Acc) ->
    Get = fun(Id) ->
		  {RealId,{X,Y,_}} = gb_trees:get(Id, Coords),
		  {RealId,{X,Y}}
	  end,
    VsC = lists:map(Get, Vs),
    get_verts(Fs,I-1,Coords,[{I, VsC}|Acc]);
get_verts([],I,_,Acc) ->
    {Acc, I}.

lsqcm(Fs, We) ->
    ?DBG("Project and tri ~n", []),
    {Vs1,Area} = ?TC(project_and_triangulate(Fs,We,-1,[],0.0)),    
    {V1, V2} = ?TC(find_pinned(Fs, We)),
    ?DBG("LSQ ~p ~p~n", [V1,V2]),
%    Idstr = lists:flatten(io_lib:format("~p", [Id])),
%    {ok, Fd} = file:open("raimo_" ++ Idstr, [write]),
%    io:format(Fd, "{~w, ~w}.~n", [Vs1,[V1,V2]]),
%    file:close(Fd),
    case ?TC(lsq(Vs1,[V1,V2])) of
	{error, What} ->
	    ?DBG("TXMAP error ~p~n", [What]),
	    exit({txmap_error, What});
	{ok,Vs2} ->
	    %%?DBG("LSQ res ~p~n", [Vs2]),
	    Patch = fun({Idt, {Ut,Vt}}) -> {Idt,{Ut,Vt,0.0}} end,
	    Vs3 = lists:sort(lists:map(Patch, Vs2)),
	    TempVs = gb_trees:from_orddict(Vs3),
	    MappedArea = calc_2dface_area(Fs, We#we{vp=TempVs}, 0.0),
	    Scale = Area/MappedArea,
	    scaleVs(Vs3,math:sqrt(Scale),[])
    end.

%%
%%  We have extended the lsqcm idea with a separate pass to generate good
%%  borders, then we use the result of the first pass as input to the second
%%  pass when we generate the whole chart.
%%
lsqcm2(Fs, We) ->
    Vs = lists:append(wings_vertex:outer_partition(Fs,We)),
    OVs = gb_sets:from_list(Vs),    
    IEds = wings_face:inner_edges(Fs,We),
    case has_inner_vs(IEds, OVs, We#we.es) of
	false ->  %% We don't want the 2pass algo for one face charts..
	    lsqcm(Fs,We);  
	_ -> 
	    lsqcm2impl(Fs, We)
    end.

has_inner_vs([Edge|Rest], Ovs, Etab) ->
    #edge{vs = Va, ve = Vb} = gb_trees:get(Edge, Etab),
    case gb_sets:is_member(Va,Ovs) andalso gb_sets:is_member(Vb,Ovs) of
	true ->
	    has_inner_vs(Rest,Ovs, Etab);
	false ->
	    true
    end;
has_inner_vs([], _, _) ->
    false.

lsqcm2impl(Fs, We) ->
    ?DBG("LSQCM2 => Project and tri ~n", []),
    {CF, BorderEds} = find_border_edges(Fs,We),
    {P1,P2} = find_pinned_from_edges(BorderEds, CF, We),
 
    FsSet = gb_sets:from_list(Fs),
    {TempFs,BorderVs} = 
	lists:foldl(fun({Va,Vb,E,_}, {Faces,Verts}) ->
			    #edge{lf=LF,rf=RF} = gb_trees:get(E,We#we.es),
			    case gb_sets:is_member(LF, FsSet) of
				true ->
				    {[LF|Faces],[Va,Vb|Verts]};
				false ->
				    {[RF|Faces],[Va,Vb|Verts]}
			    end
		    end, {[],[]}, BorderEds),
    BorderVsSet = gb_sets:from_list(BorderVs),
    TempFsSet = gb_sets:from_list(TempFs),
    DelFs = gb_sets:difference(FsSet, TempFsSet),
    %% Create a new face, we are not allowed to have holes, right Bjorn :-)
    TempWe = wings_face_cmd:dissolve(DelFs, We),
    InnerFaces = gb_sets:to_list(wings_we:new_items(face, We, TempWe)),
    ?DBG("BordersFaces ~p ~p ~n", [TempFs, InnerFaces]),
    {Vs0,_} = ?TC(project_and_triangulate(InnerFaces ++ TempFs,TempWe,-1,[],0.0)), 

    {ok,PBVs}  = lsq(Vs0,[P1,P2]),
    PinnedBorder = lists:foldl(fun(PV = {Vid, _}, Acc) ->
				       case gb_sets:is_member(Vid,BorderVsSet) of
					   true -> [PV|Acc];
					   false -> Acc
				       end
			       end, [], PBVs),
    ?DBG("LSQ2 (2pass) ~p ~n", [PinnedBorder]),
    {Vs1,Area} = ?TC(project_and_triangulate(Fs,We,-1,[],0.0)),
    case ?TC(lsq(Vs1, PinnedBorder)) of
 	{error, What2} ->
 	    ?DBG("TXMAP (second pass) error ~p~n", [What2]),
 	    exit({txmap_error2, What2});
 	{ok,Vs2} ->
 %	    ?DBG("LSQ res ~p~n", [Vs2]),
 	    Patch = fun({Idt, {Ut,Vt}}) -> {Idt,{Ut,Vt,0.0}} end,
 	    Vs3 = lists:sort(lists:map(Patch, Vs2)),
 	    TempVs = gb_trees:from_orddict(Vs3),
 	    MappedArea = calc_2dface_area(Fs, We#we{vp=TempVs}, 0.0),
 	    Scale = Area/MappedArea,
 	    scaleVs(Vs3,math:sqrt(Scale),[])
    end.

% lsqcm2impl(Fs, We) ->
%     ?DBG("LSQCM2 => Project and tri ~n", []),
%     {CF, BorderEds} = find_border_edges(Fs,We),
%     ?DBG("BE ~p ~n", [BorderEds]),
% %     {BorderVs, Center0} = 
% % 	lists:mapfoldl(fun({V1,_V2,_E,Dist}, Acc) ->
% % 			      V1p = (gb_trees:get(V1, We#we.vs))#vtx.pos,
% % 			      T = e3d_vec:mul(V1p, Dist),
% % 			      Sum = e3d_vec:add(Acc, T),
% % 			      {{V1, V1p}, Sum}
% % 		      end, e3d_vec:zero(), BorderEds),
% %     Center = e3d_vec:divide(Center0, CF),        
%     BorderVs = [{V1,(gb_trees:get(V1, We#we.vs))#vtx.pos} || {V1,_,_,_} <-BorderEds],
%     Vs = [Vpos || {_,Vpos} <- BorderVs],
%     Center = e3d_vec:average(Vs),    
%     {V1, V2} = ?TC(find_pinned_from_edges(BorderEds, CF, We)),
%     TempFs = create_border_fs(BorderVs, BorderVs, {-1,Center}, 0, []),
%     ?DBG("LSQ2 (1pass) ~p ~p~n", [V1,V2]),
%     PinnedBorder =
% 	case ?TC(lsq(TempFs, [V1,V2])) of
% 	    {error, What} ->
% 		?DBG("TXMAP error (1st pass) ~p~n", [What]),
% 		exit({txmap_error1, What});
% 	    {ok,Pinned0} ->
% 		?DBG("LSQ2 (2pass) ~p ~n", [Pinned0]),
% 		lists:keydelete(-1, 1, Pinned0)
% 	end,
%     {Vs1,Area} = ?TC(project_and_triangulate(Fs,We,-1,[],0.0)),
%     case ?TC(lsq(Vs1, PinnedBorder)) of
%  	{error, What2} ->
%  	    ?DBG("TXMAP (second pass) error ~p~n", [What2]),
%  	    exit({txmap_error2, What2});
%  	{ok,Vs2} ->
%  %	    ?DBG("LSQ res ~p~n", [Vs2]),
%  	    Patch = fun({Idt, {Ut,Vt}}) -> {Idt,#vtx{pos={Ut,Vt,0.0}}} end,
%  	    Vs3 = lists:sort(lists:map(Patch, Vs2)),
%  	    TempVs = gb_trees:from_orddict(Vs3),
%  	    MappedArea = calc_2dface_area(Fs, We#we{vs=TempVs}, 0.0),	    
%  	    Scale = Area/MappedArea,
%  	    scaleVs(Vs3,math:sqrt(Scale),[])
%     end.

scaleVs([{Id, {X,Y,_}}|Rest],Scale,Acc) 
  when is_float(X), is_float(Y), is_float(Scale) ->
    scaleVs(Rest, Scale, [{Id, {X*Scale,Y*Scale,0.0}}|Acc]);
scaleVs([],_,Acc) ->
    lists:reverse(Acc).

calc_2dface_area([Face|Rest],We,Area) ->
    Vs0 = wpa:face_vertices(Face, We),
    NewArea = calc_area(Vs0, {0.0,0.0,1.0}, We) + Area,
    calc_2dface_area(Rest,We,NewArea);
calc_2dface_area([],_,Area) ->
    Area.

% -ifdef(DEBUG).
% create_border_fs([F,S|R], Orig, C, N, Acc) ->
%     New = {N,create_border_2d_face(S,F,C)},
%     create_border_fs([S|R], Orig, C, N+1, [New|Acc]);
% create_border_fs([F], [S|_], C, N, Acc) ->
%     New = {N,create_border_2d_face(S,F,C)},
%     [New|Acc].

% create_border_2d_face({I0, P0}, {I1,P1}, {I2,P2}) ->
%     Normal = e3d_vec:normal(P0,P1,P2),
%     Rot = e3d_mat:rotate_s_to_t(Normal,{0.0,0.0,1.0}),
%     {P0X,P0Y,_} = e3d_mat:mul_point(Rot,P0),
%     {P1X,P1Y,_} = e3d_mat:mul_point(Rot,P1),
%     {P2X,P2Y,_} = e3d_mat:mul_point(Rot,P2),
%     [{I0,{P0X,P0Y}},{I1,{P1X,P1Y}},{I2,{P2X,P2Y}}].
% -endif.

find_border_edges(Faces, We) ->
    case auv_placement:group_edge_loops(Faces, We) of
	[] -> 
	    exit({invalid_chart, {Faces, is_closed_surface}});
	[Best|_] ->
	    Best
    end.
find_pinned(Faces, We) ->
    {Circumference, BorderEdges} = find_border_edges(Faces,We),
    find_pinned_from_edges(BorderEdges, Circumference, We).
find_pinned_from_edges(BorderEdges, Circumference, We) ->
    Vs = [gb_trees:get(V1, We#we.vp) || {V1,_,_,_} <-BorderEdges],
    Center = e3d_vec:average(Vs),
    AllC = lists:map(fun({Id,_,_,_}) ->
			     Pos = gb_trees:get(Id, We#we.vp),
			     Dist = e3d_vec:dist(Pos, Center),
			     {Dist, Id, Pos}
		     end, BorderEdges),
    [{_,V0,_V1Pos}|_] = lists:reverse(lists:sort(AllC)),
    BE1 = reorder_edge_loop(V0, BorderEdges, []),
    HalfCC = Circumference/2, %% - Circumference/100,
    {V1, V2} = find_pinned(BE1, BE1, 0.0, HalfCC, HalfCC, undefined), 
    {{V1,{0.0,0.0}},{V2,{1.0,1.0}}}.
    
find_pinned(Curr=[{C1,_,_,Clen}|CR],Start=[{_,S2,_,Slen}|SR],Len,HCC,Best,BVs) ->    
    Dlen = HCC-(Clen+Len),
    ADlen = abs(Dlen),
%    ?DBG("Testing ~p ~p ~p ~p ~p~n", [{S2,C1},Dlen,{Len+Clen,HCC}, Best, BVs]),    
    if 
	Dlen >= 0.0 ->
	    if ADlen < Best ->
		    find_pinned(CR,Start,Clen+Len,HCC,ADlen,{S2,C1});
	       true ->
		    find_pinned(CR,Start,Clen+Len,HCC,Best,BVs)
	    end;
	Dlen < 0.0 ->
	    if ADlen < Best ->
		    find_pinned(Curr,SR,Len-Slen,HCC, ADlen,{S2,C1});
	       true ->
		    find_pinned(Curr,SR,Len-Slen,HCC,Best,BVs)
	    end
    end;
find_pinned([], _, _, _, _Best, Bvs) ->
%    ?DBG("Found ~p ~p~n", [_Best, Bvs]),
    Bvs.

reorder_edge_loop(V1, [Rec={V1,_,_,_}|Ordered], Acc) ->
    Ordered ++ lists:reverse([Rec|Acc]);
reorder_edge_loop(V1, [H|Tail], Acc) ->
    reorder_edge_loop(V1, Tail, [H|Acc]).

-endif. % -ifdef(lsq_standalone). -else.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Least Square Conformal Maps %%%%%%%%%%%%

lsq(L, Lpuv) when list(Lpuv) ->
    lsq(L, Lpuv, env);
lsq(Name, Method) when atom(Method) ->
    {ok, [{L, Lpuv}]} = file:consult(Name),
    lsq(L, Lpuv, Method).


lsq(L, Lpuv, Method) when list(L), list(Lpuv), atom(Method) ->
    Method_1 =
	case Method of 
	    env ->
		case os:getenv("WINGS_AUTOUV_SOLVER") of
		    "ge" -> ge;
		    "cg" -> cg;
		    "cg_jacobian" -> cg_jacobian;
		    "cg_colnorm" -> cg_colnorm;
		    _ -> cg_colnorm
		end;
	    M ->
		M
	end,
    case catch lsq_int(L, Lpuv, Method_1) of
	{'EXIT', Reason} ->
	    exit(Reason);
	badarg ->
	    erlang:fault(badarg, [L, Lpuv]);
	Result ->
	    Result
    end;
lsq(L, Lpuv, Method) ->
    erlang:fault(badarg, [L, Lpuv, Method]).

lsq_int(L, Lpuv, Method) ->
    %% Create a dictionary and a reverse dictionary for all points
    %% in the indata. Translate the point identities into a
    %% continous integer sequence.
    ?DBG("lsq_int - Lpuv = ~p~n", [Lpuv]),
    {M,Dict,Rdict} = lsq_points(L),
    {N,L1,L2} = lsq_triangles(L, Dict, M),
    Lquv = 
	lists:sort( % Must be sorted for pick() and insert() to work.
	  lists:map(
	    fun ({P,{U,V} = UV} = PUV) when number(U), number(V) ->
		    case dict:find(P, Dict) of
			{ok,Q} -> {Q,UV};
			error -> throw({error,{invalid_arguments,[PUV]}})
		    end;
		(PUV) ->
		    throw({error,{invalid_arguments,[PUV]}})
	    end,
	    Lpuv)),
    ?DBG("lsq_int - Lquv = ~p~n", [Lquv]),
    {Np,Usum,Vsum} =
	lists:foldl(
	  fun ({_,{U,V}}, {Np1,Usum1,Vsum1}) ->
		  {Np1+1,Usum1+U,Vsum1+V}
	  end,
	  {0,0,0}, Lquv),
    %% Build the basic submatrixes 
    %% M1 = Re(M), M2 = Im(M), M2n = -M2
    {M1,M2,M2n} = ?TC(build_basic(M,L1,L2)),
    %% Compile the basic submatrixes into the ones related to 
    %% free points (Mf*) i.e unknown, 
    %% and pinned points (Mp*).
    {Mfp1c,Mfp2c,Mfp2nc,LuLv} = ?TC(build_cols(M1,M2,M2n,Lquv)),
    ?DBG("lsq_int - LuLv = ~p~n", [LuLv]),
    %% Compose the matrix and vector to solve
    %% for a Least SQares solution.
    {Af,B} = ?TC(build_matrixes(N,Mfp1c,Mfp2c,Mfp2nc,LuLv)),
    ?DBG("Solving matrixes~n", []),
    X = case Method of
	    ge ->
		?TC(minimize(Af,B));
	    _ ->
		X0 = auv_matrix:vector(lists:duplicate(M-Np, Usum/Np)++
				       lists:duplicate(M-Np, Vsum/Np)),
		{_,X1} = ?TC(minimize_cg(Af, X0, B, Method)),
		X1
	end,
%%    ?DBG("X=~p~n", [X]),
    %% Extract the vector of previously unknown points,
    %% and insert the pinned points. Re-translate the
    %% original point identities.
    ?TC(lsq_result(X, Lquv, Rdict)).



build_basic(M,L1,L2) ->
    M1 = auv_matrix:rows(M, L1),
    M2 = auv_matrix:rows(M, L2),
    M2n = auv_matrix:rows(M, [auv_matrix:mult(-1, X) || X <- L2]),
    {M1,M2,M2n}.

build_cols(M1,M2,M2n,Lquv) ->
    %% Build column lists of the M matrixes
    M1c = auv_matrix:cols(M1),
    M2c = auv_matrix:cols(M2),
    M2nc = auv_matrix:cols(M2n),
    %% Split the column lists into free (Mf) and pinned (Mp)
    {Lq,Lu,Lv} = split_quv(Lquv), % Lquv is sorted
    {pick(M1c, Lq),pick(M2c, Lq),pick(M2nc, Lq), Lu++Lv}.

split_quv(Lquv) ->
    split_quv(Lquv, [], [], []).

split_quv([], Lq, Lu, Lv) ->
    {lists:reverse(Lq),lists:reverse(Lu),lists:reverse(Lv)};
split_quv([{Q,{U,V}} | Lquv], Lq, Lu, Lv) ->
    split_quv(Lquv, [Q | Lq], [U | Lu], [V | Lv]).

build_matrixes(N,{Mf1c,Mp1c},{Mf2c,Mp2c},{Mf2nc,Mp2nc},LuLv) ->
    %% Build the matrixes Af and Ap, and vector B
    %% A = [ M1 -M2 ],  B = Ap U, U is vector of pinned points
    %%     [ M2  M1 ]
    Afu = auv_matrix:cols(N, Mf1c++Mf2nc),
    Afl = auv_matrix:cols(N, Mf2c++Mf1c),
    Af = auv_matrix:cat_rows(Afu, Afl),
    Apu = auv_matrix:cols(N, Mp1c++Mp2nc),
    Apl = auv_matrix:cols(N, Mp2c++Mp1c),
    Ap = auv_matrix:cat_rows(Apu, Apl),
    {Np,K_LuLv} = keyseq_neg(LuLv),
    U = auv_matrix:vector(Np, K_LuLv),
    ?DBG("build_matrixes - U = ~p~n", [U]),
    B = auv_matrix:mult(Ap, U),
    {Af, B}.

keyseq_neg(L) ->
    keyseq(1, L, []).

keyseq(N, [], R) ->
    {N-1,lists:reverse(R)};
keyseq(N, [X | L], R) ->
    keyseq(N+1, L, [{N,-X} | R]).

%%               _   _    2
%% Minimize || A x - b ||  
%%
%%              t   _       t _
%% by solving A   A x  =  A   b
%%
%% using Gaussian Elimination and Back Substitution.
%%
minimize(A,B) ->
    AA = ?TC(mk_solve_matrix(A, B)),
    AAA = ?TC(auv_matrix:reduce(AA)),
%%    ?DBG("Reduced: ~p~n", [AAA]),
    X   = ?TC(auv_matrix:backsubst(AAA)),
    ?DBG("Solved~n",[]),
    X.    

mk_solve_matrix(Af,B) ->
    AfT = auv_matrix:trans(Af),
    AfTAf = auv_matrix:mult_trans(AfT, AfT),
    AfTB = auv_matrix:mult(-1, auv_matrix:mult(AfT, B)),
    auv_matrix:cat_cols(AfTAf, AfTB).

%%               _   _    2
%% Minimize || A x - b ||  
%%
%%             -1  t    _      -1  t  _
%% by solving M   A   A x  =  M   A   b
%%                                                         __
%% using the Preconditioned Coujugate Gradient method with x0 as 
%% iteration start vector.
%%
minimize_cg(A, X0, B, Method) ->
    ?DBG("minimize_cg - dim A=~p X0=~p B=~p~n",
	 [auv_matrix:dim(A), auv_matrix:dim(X0), auv_matrix:dim(B)]),
    {N,M} = auv_matrix:dim(A),
    {M,1} = auv_matrix:dim(X0),
    {N,1} = auv_matrix:dim(B),
    I = M,
    Epsilon = 1.0e-3,
    At = auv_matrix:trans(A),
    AtB = auv_matrix:mult(At, B),
    M_inv = 
	case Method of
	    cg_jacobian ->
		%% This preconditioning is not worth the effort.
		%% The time for convergence decreases, but that gain
		%% is lost on the AtA multiplication.
		AtA_u = ?TC(auv_matrix:square_right(At)),
		Diag = ?TC(auv_matrix:diag(AtA_u)),
		case catch [1/V || V <- Diag] of
		    {'EXIT', {badarith, _}} ->
			fun (R_new) ->
				auv_matrix:mult(1, R_new)
			end;
		    {'EXIT', Reason} ->
			exit(Reason);
		    Diag_inv ->
			M_i = ?TC(auv_matrix:diag(Diag_inv)),
			fun (R_new) ->
				auv_matrix:mult(M_i, R_new)
			end
		end;
	    cg_colnorm ->
		%% A very cheap preconditioning. The column norm
		%% takes no time to calculate compared to 
		%% AtA above. The iteration time impact is also 
		%% very low since it is a matrix multiplication
		%% with a diagonal (i.e very sparse) matrix.
		%% The preconditioning effect (on the required
		%% number of iterations) is modest, but 
		%% cost effective.
		Diag = ?TC(auv_matrix:row_norm(At)),
		case catch [1/V || V <- Diag] of
		    {'EXIT', {badarith, _}} ->
			fun (R_new) ->
				auv_matrix:mult(1, R_new)
			end;
		    {'EXIT', Reason} ->
			exit(Reason);
		    Diag_inv ->
			M_i = ?TC(auv_matrix:diag(Diag_inv)),
			fun (R_new) ->
				auv_matrix:mult(M_i, R_new)
			end
		end;
	    _ ->
		%% No (identity) preconditioning
		fun (R_new) ->
			auv_matrix:mult(1, R_new)
		end
	end,
    ?DBG("Preconditioning with ~p~n", [Method]),
    R = auv_matrix:sub(AtB, 
		       auv_matrix:mult(At, auv_matrix:mult(A, X0))),
    D = M_inv(R),
    Delta = auv_matrix:mult(auv_matrix:trans(R), D),
    Delta_max = Epsilon*Epsilon*Delta,
    minimize_cg(M_inv, At, A, AtB, Delta_max, 
		Delta, I, D, R, X0).

minimize_cg(_, _At, _A, _, _, 
	    _, 0, _D, _, X) ->
    ?DBG("minimize_cg() sizes were ~p ~p ~p~n", 
	 [auv_matrix:dim(_At), auv_matrix:dim(_A), auv_matrix:dim(_D)]),
    {stopped, X};
minimize_cg(_, _At, _A, _, Delta_max, 
	    Delta, _, _D, _, X) when Delta < Delta_max ->
    ?DBG("minimize_cg() sizes were ~p ~p ~p~n", 
	 [auv_matrix:dim(_At), auv_matrix:dim(_A), auv_matrix:dim(_D)]),
    {ok, X};
minimize_cg(M_inv, At, A, AtB, Delta_max, 
	    Delta, I, D, R, X) ->
%%    ?DBG("minimize_cg() step ~p Delta=~p~n", [I, Delta]),
    P = auv_matrix:mult(A, D),
    Alpha = Delta / auv_matrix:mult(auv_matrix:trans(P), P),
    X_new = auv_matrix:add(X, auv_matrix:mult(Alpha, D)),
    if (I + 5) rem 10 == 0 ->
	    minimize_cg_3(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, X_new);
       true ->
	    minimize_cg_2(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, R, X_new, Alpha, P)
    end.

minimize_cg_2(M_inv, At, A, AtB, Delta_max,
	      Delta, I, D, R, X_new, Alpha, P) ->
    R_new = auv_matrix:sub(R, auv_matrix:mult(Alpha, auv_matrix:mult(At, P))),
    S = M_inv(R_new),
    Delta_new = auv_matrix:mult(auv_matrix:trans(R_new), S),
    if Delta_new < Delta_max ->
	    minimize_cg_3(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, X_new);
       true ->
	    D_new = auv_matrix:add(S, auv_matrix:mult(Delta_new/Delta, D)),
	    minimize_cg(M_inv, At, A, AtB, Delta_max,
			Delta_new, I-1, D_new, R_new, X_new)
    end.

minimize_cg_3(M_inv, At, A, AtB, Delta_max,
	      Delta, I, D, X_new) ->
    ?DBG("minimize_cg() recalculating residual ~p~n", [Delta]),
    R_new = auv_matrix:sub
	      (AtB, auv_matrix:mult(At, auv_matrix:mult(A, X_new))),
    S = M_inv(R_new),
    Delta_new = auv_matrix:mult(auv_matrix:trans(R_new), S),
    D_new = auv_matrix:add(S, auv_matrix:mult(Delta_new/Delta, D)),
    minimize_cg(M_inv, At, A, AtB, Delta_max,
		Delta_new, I-1, D_new, R_new, X_new).



%% Extract all point identities from indata, and create
%% forwards and backwards dictionaries for translation
%% between point identity and point number.
%%
lsq_points(L) ->
    PL1 = 
	lists:foldl(
	  fun ({_,[{P1,_},{P2,_},{P3,_}]}, P) ->
		  [P1, P2, P3 | P];
	      (Invalid, _) ->
		  throw({error, {invalid_triangle, Invalid}})
	  end, [], L),
    PL2 = lists:usort(PL1),
    lists:foldl(
      fun (P, {N, D, DR}) ->
	      N1 = N+1,
	      {N1, dict:store(P, N1, D), dict:store(N1, P, DR)}
      end, {0, dict:new(), dict:new()}, PL2).



%% Create matrix M rows for all triangles in indata.
%% Return number of triangles, and lists for Re(M) and Im(M).
%%
lsq_triangles(L, Dict, M) ->
    {N, L1, L2} =
	lists:foldl(
	  fun ({_,[{P1,{X1,Y1}},{P2,{X2,Y2}},{P3,{X3,Y3}}]}, {N,Re,Im})
	      when number(X1), number(X2), number(X3),
		   number(Y1), number(Y2), number(Y3) ->
		  SqrtDT = math:sqrt(abs((X2-X1)*(Y3-Y1) - 
					 (Y2-Y1)*(X3-X1))),
		  W1re = X3-X2, W1im = Y3-Y2, 
		  W2re = X1-X3, W2im = Y1-Y3, 
		  W3re = X2-X1, W3im = Y2-Y1,
		  Q1 = dict:fetch(P1, Dict),
		  Q2 = dict:fetch(P2, Dict),
		  Q3 = dict:fetch(P3, Dict),
		  {N+1,
		   [auv_matrix:vector(M, [{Q1,W1re/SqrtDT}, 
					  {Q2,W2re/SqrtDT}, 
					  {Q3,W3re/SqrtDT}])
		    | Re],
		   [auv_matrix:vector(M, [{Q1,W1im/SqrtDT}, 
					  {Q2,W2im/SqrtDT}, 
					  {Q3,W3im/SqrtDT}])
		    | Im]};
	      (Invalid, _) ->
		  throw({error, {invalid_triangle, Invalid}})
	  end, {0, [],[]}, L),
    {N, lists:reverse(L1), lists:reverse(L2)}.



%% Extract the result from vector X and combine it with the 
%% pinned points. Re-translate the point identities.
%%
lsq_result(X, Lquv, Rdict) ->
    {MM,1} = auv_matrix:dim(X),
%     {_,UlistVlistR} =
% 	lists:foldl(
% 	  fun ({J,UV}, {J,R}) ->
% 		  {J+1, [UV | R]};
% 	      ({J2,_}, {J1,R}) ->
% 		  {J2+1, lists:duplicate(J2-J1, 0.0) ++ R};
% 	      (Other, State) ->
% 		  throw({error, {?FILE, ?LINE, [Other, State, X]}})
% 	  end, {1, []}, auv_matrix:vector(X)),
%     {Ulist, Vlist} = ?TC(split(lists:reverse(UlistVlistR), MM div 2)),
    {Ulist, Vlist} = ?TC(split(auv_matrix:vector(X), MM div 2)),
    {[],UVlistR} = 
	lists:foldl(
	  fun (U, {[], R}) ->
		  {[], [{U,0.0} | R]};
	      (U, {[V | L], R}) ->
		  {L, [{U,V} | R]};
	      (Other, State) ->
		  throw({error, {?FILE, ?LINE, [Other, State, X]}})
	  end, {Vlist, []}, Ulist),
    UVlist = insert(lists:reverse(UVlistR), Lquv),
    {_, TxMapR} =
	lists:foldl(
	  fun (UV, {Q,R}) ->
		  {Q+1,[{dict:fetch(Q, Rdict),UV} | R]}
	  end, {1,[]}, UVlist),
    TxMap = lists:reverse(TxMapR),
    ?DBG("lsq_result - TxMap = ~p~n", [TxMap]),
    {ok, TxMap}.



%% Picks terms with specified indexes from a list.
%%
%% L: list of terms
%% P: list of indexes in ascending order
%%
%% Return: {L_remaining, L_picked}
%%
pick(L, P) 
  when list(L), list(P) ->
    case pick(1, L, P, [], []) of
	{_, _} = Ok ->
	    Ok;
	Fault ->
	    erlang:fault(Fault, [L, P])
    end;
pick(L, P) ->
    erlang:fault(badarg, [L, P]).

pick(_, L, [], R, Q) ->
    {lists:reverse(R, L), lists:reverse(Q)};
pick(_, [], _, _, _) ->
    badarg;
pick(_, _, [I, J | _], _, _) when I >= J ->
    badarg;
pick(I, [V | L], [I | P], R, Q) ->
    pick(I+1, L, P, R, [V | Q]);
pick(I, [V | L], P, R, Q) ->
    pick(I+1, L, P, [V | R], Q);
pick(_, _, _, _, _) ->
    badarg.



%% Insert terms with specified indexes in a list
%%
%% L: List of terms
%% S: List of {Pos,Term} tuples with Term to be 
%%    inserted at position Pos in L
%%
insert(L, S)
  when list(L), list(S) ->
    case insert(1, L, S, []) of
	R when list(R) ->
	    R;
	Fault ->
	    erlang:fault(Fault, [L, S])
    end;
insert(L, S) ->
    erlang:fault(badarg, [L, S]).

insert(_, L, [], R) ->
    lists:reverse(R, L);
insert(_, _, [{I,_}, {J,_} | _], _) when I >= J ->
    badarg;
insert(I, L, [{I,E} | S], R) ->
    insert(I+1, L, S, [E | R]);
insert(_, [], _, _) ->
    badarg;
insert(I, [E | L], S, R) ->
    insert(I+1, L, S, [E | R]).



%% Split a list into two after N terms
%%
split(L, N) ->
    split(L, N, []).

split([], _, R) ->
    {lists:reverse(R), []};
split(L, 0, R) ->
    {lists:reverse(R), L};
split([E | L], N, R) ->
    split(L, N-1, [E | R]).

area2d2({S1,T1},{S2,T2},{S3,T3})
  when is_float(S1),is_float(S2),is_float(S3),
       is_float(T1),is_float(T2),is_float(T3) ->
    ((S2-S1)*(T3-T1)-(S3-S1)*(T2-T1)).

area3d(V1,V2,V3) ->
    abs(e3d_vec:len(e3d_vec:cross(e3d_vec:sub(V2,V1),e3d_vec:sub(V3,V1)))/2).


t() ->
    [P2,P3] = [{-1.0,0.0},{1.0,0.0}],
    [Q1,Q2,Q3] = [{0.0,1.0,0.0},{-1.0,0.0,0.0},{1.0,0.0,0.0}],
    [io:format("~p ~p~n", [Y, area2d2({0.0,0.5-Y/10},P2,P3)]) 
     || Y <- lists:seq(-10,10)],
    [io:format("~p ~p~n", [Y, catch l2({0.0,0.5-Y/10},P2,P3,Q1,Q2,Q3)]) 
     || Y <- lists:seq(-10,10)],
    [io:format("~p ~p~n", [Y, catch l8({0.0,0.5-Y/10},P2,P3,Q1,Q2,Q3)]) 
     || Y <- lists:seq(-10,10)].

    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Texture metric stretch 
%% From 'Texture Mapping Progressive Meshes' by
%% Pedro V. Sander, John Snyder Steven J. Gortler, Hugues Hoppe

stretch_opt(_, _) ->
    erlang:fault(nyi).
%stretch_opt(Ch=#ch{fs=Fs,we=We0}, OVs) ->
%     {_,R1,R2} = now(),
%     random:seed(R2,R1,128731),
%     Be = wings_face:outer_edges(Fs, We0),
%     Bv0 = lists:foldl(fun(Edge,Acc) -> #edge{vs=Vs,ve=Ve} = 
% 					   gb_trees:get(Edge,We0#we.es),
% 				       [Vs,Ve|Acc]
% 		      end, [], Be),
%     Bv = gb_sets:from_list(Bv0),
%     %% {FaceToStretchMean, FaceToStretchWorst,FaceToVerts,VertToFaces,VertToUvs}
%     {{F2S2,_F2S8,F2Vs,V2Fs,Uvs},S} = stretch_setup(Fs,We0,OVs),
%     ?DBG("F2S2 ~w~n",[gb_trees:to_list(F2S2)]),
% %     ?DBG("F2S8 ~w~n",[gb_trees:to_list(F2S8)]),
% %     ?DBG("F2Vs ~w~n",[gb_trees:to_list(F2Vs)]),
% %     ?DBG("UVs ~w~n", [gb_trees:to_list(Uvs)]),
%     S2V = stretch_per_vertex(model_l2,gb_trees:to_list(V2Fs),F2S2,F2Vs,OVs,Bv,
% 			     gb_trees:empty()),
%     MaxI = 100,   %% Max iterations
%     MinS = 0.001, %% Min Stretch
%     {SUvs0,_F2S2} = stretch_iter(S2V,1,MaxI,MinS,F2S2,F2Vs,V2Fs,Uvs,OVs,Bv),    
%     SUvs1 = gb_trees:to_list(SUvs0),
% %    ?DBG("SUvs ~p ~n", [SUvs1]),
%     Suvs = [{Id,{S0/S,T0/S,0.0}} || {Id,{S0,T0}} <- SUvs1],
%     Res = Ch#ch{we=We0#we{vp=gb_trees:from_orddict(Suvs)}},

%     _Mean2  = model_l2(gb_trees:keys(_F2S2), _F2S2, F2Vs, OVs,0.0, 0.0),
% %%    io:format("After Stretch sum (mean) ~p ~n",  [_Mean2]),
%     stretch_setup(Fs, Res#ch.we,OVs),
%     Res.

stretch_setup(Fs, We0, OVs) ->
    Tris0 = triangulate(Fs,-1,We0,[]),
    S = calc_scale(Tris0, OVs, 0.0, 0.0),
    Tris = [{Face,[{Id1,{S1*S,T1*S}},{Id2,{S2*S,T2*S}},{Id3,{S3*S,T3*S}}]} ||
	       {Face,[{Id1,{S1,T1}},{Id2,{S2,T2}},{Id3,{S3,T3}}]} <- Tris0],
    Init = {F2S2,F2S8,F2Vs,_,_} = init_stretch(Tris, OVs, [], [], [], [], []),
    Worst = model_l8(gb_trees:keys(F2S8), F2S8, 0.0), 
    Mean  = model_l2(gb_trees:keys(F2S2), F2S2, F2Vs, OVs,0.0, 0.0),
    io:format("Stretch sum (worst) ~p ~n", [Worst]),
    io:format("Stretch sum (mean) ~p ~n",  [Mean]),
    {Init,S}.

stretch_iter(S2V0={[{_,First}|_],_},I,MaxI,MinS,F2S20,F2Vs,V2Fs,Uvs0,Ovs,Bv) 
  when (First > MinS), I < MaxI ->
    io:format(".",[]),
    {S2V,F2S2,Uvs} = stretch_iter2(S2V0,I,MaxI,MinS,F2S20,F2Vs,V2Fs,Uvs0,Ovs,Bv,
				   gb_sets:empty()),
    stretch_iter(S2V,I+1,MaxI,MinS,F2S2,F2Vs,V2Fs,Uvs,Ovs,Bv);
stretch_iter(_,_,_,_,F2S2,_,_,Uvs,_,_) ->
    {Uvs,F2S2}.

stretch_iter2({[{V,Val}|R],V2S0},I,MaxI,MinS,F2S20,F2Vs,V2Fs,Uvs0,Ovs,Bv,Acc)
  when Val > MinS ->
    case gb_sets:is_member(V,Acc) of
	true -> 
	    stretch_iter2({R,V2S0},I,MaxI,MinS,F2S20,F2Vs,V2Fs,Uvs0,Ovs,Bv,Acc);
	false ->
	    Line = random_line(),
	    Fs   = gb_trees:get(V,V2Fs),
	    Max  = 1.0 - I/MaxI,
	    Step = Max/10.0,
	    %%    ?DBG("S ~.2f:",[Val]),
	    {PVal,Uvs,F2S2} = opt_v(Val,Step,Step,Max,V,Line,Fs,F2Vs,F2S20,Uvs0,Ovs),
	    case PVal == Val of 
		true -> 
%		    ?DBG("Unchanged value for ~p ~p ~p ~n",[{V,Val}, I, R]),
		    %%?DBG("~p ~p ~p~n",[V,gb_trees:get(V,Uvs0),gb_trees:get(V,Uvs)]),
		    stretch_iter2({R,V2S0},I,MaxI,MinS,F2S20,F2Vs,V2Fs,Uvs0,Ovs,
				  Bv, gb_sets:add(V,Acc));
		false ->
		    Vs0 = lists:usort(lists:append([gb_trees:get(F,F2Vs)|| F<-Fs])),
		    Upd0 = lists:foldl(fun(Vtx, New) ->
					       [{Vtx,gb_trees:get(Vtx, V2Fs)}|New]
				       end, [], Vs0),
		    Upd = stretch_per_vertex(model_l2,Upd0,F2S2,F2Vs,Ovs,
					     Bv,V2S0),
%		    ?DBG("Update value for ~p ~p~n ~w~n ~w ~n",
%			[{V,Val},PVal,gb_sets:to_list(Acc),element(1,Upd)]),
		    stretch_iter2(Upd,I,MaxI,MinS,F2S2,F2Vs,V2Fs,Uvs,Ovs,Bv,Acc)
	    end
    end;

stretch_iter2({_,S2V},_,_,_,F2S2,F2Vs,V2Fs,Uvs,Ovs,Bv,Acc0) ->
    Acc = lists:foldl(fun(V,Vals) ->
			      [{V,gb_trees:get(V,V2Fs)}|Vals]
		      end, [], gb_sets:to_list(Acc0)),
    Upd = stretch_per_vertex(model_l2,Acc,F2S2,F2Vs,Ovs,Bv,S2V),
    {Upd, F2S2, Uvs}.

random_line() ->
    X   = random:uniform()-0.5,
    Y   = random:uniform()-0.5,
    Len = math:sqrt(X*X+Y*Y),
    {X/Len,Y/Len}.

opt_v(PVal,I,Step,Max,V,L={X,Y},Fs,F2Vs,F2S0,Uvs0,Ovs) 
  when I =< Max, Step > 0.00001 ->
    %%    ?DBG("~w ~w ~w ~w~n",[V, I,Step,Max]),
    {S0,T0} = gb_trees:get(V, Uvs0),
    St = {S0+X*I,T0+Y*I},
    Uvs = gb_trees:update(V,St,Uvs0),
    F2S = lists:foldl(fun(Face,Acc) ->
			      Vs = gb_trees:get(Face, F2Vs),
			      S = l2(Vs,Uvs,Ovs),
			      gb_trees:update(Face, S, Acc)
		      end, F2S0, Fs),    
    Stretch = model_l2(Fs,F2S,F2Vs, Ovs, 0.0,0.0),
    
%%    io:format("~.2f ",[Stretch]),
    if 
	Stretch < PVal ->
%	    ?DBG("Found better ~p ~p ~p ~p ~n",[I,Step,Stretch,PVal]),
	    opt_v(Stretch,I+Step,Step,Max,V,L,Fs,F2Vs,F2S,Uvs,Ovs); 
	true ->
	    NewStep = Step/10,
	    opt_v(PVal,I-Step+NewStep,NewStep,Step,V,L,Fs,F2Vs,F2S0,Uvs0,Ovs)
%	    ?DBG("Miss ~p ~p ~p ~p ~n",[I,Step,Stretch,PVal]),
%%	    opt_v(PVal,I+Step,Step,Max,V,L,Fs,F2Vs,Uvs0,Ovs)
    end;
opt_v(PVal,_I,_Step,_Max,_V,_L,_Fs,_F2Vs,F2S,Uvs0,_Ovs) ->
    {PVal,Uvs0,F2S}.


%% Hmm, I just sum this up.. should this be calc'ed as model_XX ??
stretch_per_vertex(Method,[{V,Fs}|R],F2S,F2Vs,Ovs,Bv,Tree) ->
    case gb_sets:is_member(V,Bv) of
	false ->
	    Res = ?MODULE:Method(Fs,F2S,F2Vs,Ovs,0.0,0.0),
	    stretch_per_vertex(Method,R,F2S,F2Vs,Ovs,Bv,
			       gb_trees:enter(V,Res,Tree));
	true ->
	    stretch_per_vertex(Method,R,F2S,F2Vs,Ovs,Bv,Tree)
    end;
stretch_per_vertex(_,[], _, _,_,_,Acc) ->
    {lists:reverse(lists:keysort(2,gb_trees:to_list(Acc))),
     Acc}.

init_stretch([{Face,FUvs=[{Id1,P1},{Id2,P2},{Id3,P3}]}|R],
	     Ovs,F2S2,F2S8,F2Vs,V2Fs,UVs) ->
    Q1 = gb_trees:get(Id1,Ovs),
    Q2 = gb_trees:get(Id2,Ovs),
    Q3 = gb_trees:get(Id3,Ovs),
    S2 = l2(P1,P2,P3,Q1,Q2,Q3),
    S8 = l8(P1,P2,P3,Q1,Q2,Q3),
    init_stretch(R,Ovs,
		 [{Face,S2}|F2S2],[{Face,S8}|F2S8],
		 [{Face, [Id1,Id2,Id3]}|F2Vs],
		 [{Id1,Face},{Id2,Face},{Id3,Face}|V2Fs],
		 FUvs ++ UVs);
init_stretch([],_,F2S2,F2S8,F2Vs,V2Fs0,Uvs) ->
    V2Fs1 = sofs:relation(lists:sort(V2Fs0)),
    V2Fs2 = sofs:relation_to_family(V2Fs1),
    V2Fs = sofs:to_external(V2Fs2),
    { gb_trees:from_orddict(lists:sort(F2S2)),
      gb_trees:from_orddict(lists:sort(F2S8)),
      gb_trees:from_orddict(lists:sort(F2Vs)),
      gb_trees:from_orddict(V2Fs),
      gb_trees:from_orddict(lists:usort(Uvs))}.

calc_scale([{_,[{Id1,P1},{Id2,P2},{Id3,P3}]}|R], Ovs, A2D, A3D) ->
    A2 = abs(area2d2(P1,P2,P3)/2),
    Q1 = gb_trees:get(Id1,Ovs),
    Q2 = gb_trees:get(Id2,Ovs),
    Q3 = gb_trees:get(Id3,Ovs),    
    A3 = area3d(Q1,Q2,Q3),
    calc_scale(R,Ovs,A2+A2D,A3+A3D);
calc_scale([],_Ovs,A2D,A3D) ->
    math:sqrt(A3D/A2D).

model_l8([Face|R], F2S8, Worst) ->
    FVal = gb_trees:get(Face,F2S8),
    New  = if FVal > Worst -> 
%		   ?DBG("Face ~p has worst ~p~n", [Face,FVal]),
		   FVal;
	      true -> 
		   Worst
	   end,
    model_l8(R,F2S8,New);
model_l8([], _, Worst) -> Worst.

model_l2([Face|R], F2S2, F2Vs, Ovs, Mean, Area)  ->
    TriM = gb_trees:get(Face,F2S2),
    [Id1,Id2,Id3] = gb_trees:get(Face,F2Vs),
    Q1 = gb_trees:get(Id1,Ovs),
    Q2 = gb_trees:get(Id2,Ovs),
    Q3 = gb_trees:get(Id3,Ovs),    
    A = area3d(Q1,Q2,Q3),
    model_l2(R,F2S2,F2Vs,Ovs, TriM*TriM*A+Mean, Area+A);
model_l2([],_,_,_,Mean,Area) ->
    math:sqrt(Mean/Area).

% s(P,P1,P2,P3,Q1,Q2,Q3) ->
%     M1 = e3d_vec:mul(Q1, area2d(P,P2,P3)),
%     M2 = e3d_vec:mul(Q2, area2d(P,P3,P1)),
%     M3 = e3d_vec:mul(Q3, area2d(P,P1,P2)),
%     A = area2d(P1,P2,P3),
%     e3d_vec:divide(e3d_vec:add([M1,M2,M3]),A).

l2([V1,V2,V3], Uvs, Ovs) ->  
    l2(gb_trees:get(V1,Uvs),gb_trees:get(V2,Uvs),gb_trees:get(V3,Uvs),
       gb_trees:get(V1,Ovs),gb_trees:get(V2,Ovs),gb_trees:get(V3,Ovs)).

l2(P1,P2,P3,Q1,Q2,Q3) ->  %% Mean stretch value
    A2 = area2d2(P1,P2,P3),
    if A2 > 0.00000001 ->
	    SS0 = ss(P1,P2,P3,Q1,Q2,Q3),
	    ST0 = st(P1,P2,P3,Q1,Q2,Q3),
	    SS = e3d_vec:divide(SS0,A2),
	    ST = e3d_vec:divide(ST0,A2),
	    A = e3d_vec:dot(SS,SS),
	    C = e3d_vec:dot(ST,ST),
	    Temp = (A+C)/2.0,
	    if Temp < 1.0 ->
		    %% 1.0+1.0-math:sqrt(Temp);  %% This or 
		    1.0;  %% that
	       true ->
		    math:sqrt(Temp)
	    end;
       true -> 
	    9999999999.9
    end.
    
l8(P1,P2,P3,Q1,Q2,Q3) ->  %% Worst stretch value
    A2 = area2d2(P1,P2,P3),
    if A2 > 0.00000001 ->
	    SS0 = ss(P1,P2,P3,Q1,Q2,Q3),
	    ST0 = st(P1,P2,P3,Q1,Q2,Q3),
	    SS = e3d_vec:divide(SS0,A2),
	    ST = e3d_vec:divide(ST0,A2),
	    A = e3d_vec:dot(SS,SS),
	    B = e3d_vec:dot(SS,ST),
	    C = e3d_vec:dot(ST,ST),
	    math:sqrt(0.5*((A+C)+math:sqrt((A-C)*(A-C)+4*B*B)));
       true ->
	    9999999999.9
    end.

ss({_,T1},{_,T2},{_,T3},Q1,Q2,Q3) 
  when is_float(T1),is_float(T2),is_float(T3) ->
    M1 = e3d_vec:mul(Q1, T2-T3),
    M2 = e3d_vec:mul(Q2, T3-T1),
    M3 = e3d_vec:mul(Q3, T1-T2),
%%    ?DBG("~p ~p~n", [[M1,M2,M3],e3d_vec:add([M1,M2,M3])]),
    e3d_vec:add([M1,M2,M3]).
    
st({S1,_},{S2,_},{S3,_},Q1,Q2,Q3) 
  when is_float(S1),is_float(S2),is_float(S3) ->
    M1 = e3d_vec:mul(Q1, S3-S2),
    M2 = e3d_vec:mul(Q2, S1-S3),
    M3 = e3d_vec:mul(Q3, S2-S1),
    e3d_vec:add([M1,M2,M3]).

triangulate([Face|Fs], I, We,Tris) ->
    Vs0 = wpa:face_vertices(Face, We),
    Vs1 = [{V,wings_vertex:pos(V,We)} || V <- Vs0],
    if length(Vs0) > 3 ->
	    {Ids,Cds,All} = setup_tri_vs(Vs1,0,[],[],[]),
	    NewFs = e3d_mesh:triangulate_face(#e3d_face{vs=Ids}, Cds),
	    VT = gb_trees:from_orddict(All),
	    {Add, I1} = get_verts(NewFs, I, VT, []),
	    triangulate(Fs,I1,We,Add ++ Tris);
       true ->
	    Vs2 = [{Vid, {Vx, Vy}} || {Vid,{Vx,Vy,_}} <- Vs1],
	    triangulate(Fs,I,We,[{Face, Vs2}|Tris])
    end;
triangulate([], _, _, Tris) ->
    Tris.

