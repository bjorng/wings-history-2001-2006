%%%-------------------------------------------------------------------
%%% File    : auv_mapping.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Provides different uv-parametrisation algorihms
%%%
%%% Created :  4 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(auv_mapping).
-include("wings.hrl").

-export([projectFromChartNormal/2, project2d/3]).

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

projectFromChartNormal({Id,Chart},We) ->
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
    Vs2 = project2d(Vs0, Normal, We),
    Vs2.

project2d(Vs, Normal, We) ->
    Rot = e3d_mat:rotate_s_to_t(Normal,{0.0,0.0,1.0}),
    Res = [{V,e3d_mat:mul_point(Rot, wings_vertex:pos(V, We))} || 
	      V <- Vs],
    lists:reverse(Res).

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
