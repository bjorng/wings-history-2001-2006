%%
%% HLines -- Hidden Line Renderer for Wings 0.98.17a and higher.
%%
%% Based on "HLines" hidden line elimination program from "Programming
%% Principles in Computer Graphics" and "Computer Graphics for Java
%% Programmers" by Leen Ammeraal, http://home.planet.nl/~ammeraal/ .
%% The source code listed in the books can be found here:
%% ftp://ftp.expa.fnt.hvu.nl/pub/ammeraal/English/older/ppcgraph.zip
%% ftp://ftp.expa.fnt.hvu.nl/pub/ammeraal/English/grjava.zip
%%
%% Copyright (c) 2003 Dmitry Efremov <defremov@aha.ru>
%%
%% BUGS:
%%  Near clipping will not work correctly if occurs in the viewport
%%  Duplicate and zero length line segments are generated in some cases
%%
%% $Id: wpc_hlines.erl,v 1.1 2004/03/01 13:26:34 dgud Exp $
%%

-module(wpc_hlines).
-author('Dmitry Efremov <defremov@aha.ru>').

-export([init/0, menu/2, command/2]).
-import(lists, [foreach/2, foldl/3]).
-include("wings.hrl").
-include("e3d.hrl").

-define(EPS, 1.0e-6).
-define(BIG, 1.0e30).
-define(MAXDEPTH, 10).

-define(DEF_WIDTH, 288).
-define(DEF_HEIGHT, 216).
-define(DEF_EDGE_MODE, hard_edges).
-define(DEF_SMOOTH_ANGLE, 0).
-define(DEF_LINE_WIDTH, 1.0).
-define(DEF_LINE_CAP, 0).
-define(DEF_SUBDIVISIONS, 0).

init() ->
    true.

menu({file, export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

menu_entry(Menu) ->
    Menu ++ [{"Cartoon edges (.eps)...", eps, [option]}].

command({file, {export, {eps, Arg}}}, St) ->
    export(Arg, export, St);
command({file, {export_selected, {eps, Arg}}}, St) ->
    export(Arg, export_selected, St);
command(_, _) -> next.

export(Arg, Op, _) when is_atom(Arg) ->
    wpa:dialog(Arg, "Cartoon edges Render Options", dialog(),
	       fun(Res) -> {file, {Op, {eps, Res}}} end);
export(Arg, Op, St0) when is_list(Arg) ->
    set_pref(Arg),
    Camera_info = wpa:camera_info([aim, distance_to_aim,
				   azimuth, elevation, tracking, 
				   fov, hither, yon]),
    Props = [{title, "Export"},
	     {ext, ".eps"},
	     {ext_desc, "Encapsulated Postscript (EPS) File"},
	     {camera_info, Camera_info},
	     {subdivisions, get_pref(subdivisions, ?DEF_SUBDIVISIONS)},
	     {win_size, wings_wm:win_size()},
	     {ortho_view, wings_wm:get_prop(orthogonal_view)}],
    
    %% Freeze virtual mirrors.
    Shapes0 = gb_trees:to_list(St0#st.shapes),
    Shapes = [{Id, wpa:vm_freeze(We)} || {Id, We} <- Shapes0],
    St = St0#st{shapes = gb_trees:from_orddict(Shapes)},
    
    case Op of
        export ->
            ?SLOW(wpa:export(Props, fun_export(Props), St))
            ;
        export_selected ->
            ?SLOW(wpa:export_selected(Props, fun_export(Props), St))
    end.

dialog() ->
    BB_width = get_pref(bb_width, ?DEF_WIDTH),
    BB_height = get_pref(bb_height, ?DEF_HEIGHT),
    Edge_mode = get_pref(edge_mode, ?DEF_EDGE_MODE),
    Smooth_angle = get_pref(smooth_angle, ?DEF_SMOOTH_ANGLE),
    Line_width = get_pref(line_width, ?DEF_LINE_WIDTH),
    Line_cap = get_pref(line_cap, ?DEF_LINE_CAP),
    SubDiv = get_pref(subdivisions, ?DEF_SUBDIVISIONS),

    [
     {hframe, [
	       {label, "Width"},
	       {text, BB_width, [{key, bb_width}]},
	       {label, "Height"},
	       {text, BB_height, [{key, bb_height}]},
	       {label, "pt"}
	      ], [{title, "Bounding Box"}]},
     {hframe,[{label,"Sub-division Steps"},
	      {text,SubDiv,[{key,subdivisions},{range,0,4}]}],
      [{title,"Pre-rendering"}]},
     {hradio,
      [
       {"All", all_edges},
       {"Hard", hard_edges},
       {"None", no_edges}
      ],
      Edge_mode,
      [{key, edge_mode}, {title, "Show edges"}]
     },
     {hframe, [{label, "Smooth angle"},
	       {slider, {text, Smooth_angle, [{key, smooth_angle}, {range, {0, 180}}]}}]},
     {hradio,
      [
       {"Butt", 0},
       {"Round", 1},
       {"Square", 2}
      ],
      Line_cap,
      [{key, line_cap}, {title, "Line caps"}]
     },
     {hframe, [
	       {label, "Line width"},
	       {text, Line_width, [{key, line_width}, {range, {0.0, ?BIG}}]},
	       {label, "pt"}
	      ]}
    ].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

fun_export(Props) ->
    fun (File_name, Scene) -> do_export(Props, File_name, Scene) end.

do_export(Props, File_name, #e3d_file{objs=Objs, mat=Mats}) ->
    {ok,F} = file:open(File_name, [write]),

    Start_time = now(),

io:format("~n", []),

    [Aim, Distance, Azimuth, Elevation, {TrackX, TrackY}, Fov, Hither, Yonder] =
        proplists:get_value(camera_info, Props),

    Taim = e3d_mat:translate(Aim),
    Ry = e3d_mat:rotate(Azimuth, {0.0, 1.0, 0.0}),
    Rx = e3d_mat:rotate(Elevation, {1.0, 0.0, 0.0}),
    Tdist = e3d_mat:translate({TrackX, TrackY, -Distance}),
    Veye = e3d_mat:mul(Tdist, e3d_mat:mul(e3d_mat:mul(Rx, Ry), Taim)),

    Wbb = max(get_pref(bb_width, ?DEF_WIDTH), 1),
    Hbb = max(get_pref(bb_height, ?DEF_HEIGHT), 1),
    ARbb = Wbb / Hbb,
    
    {Wwin, Hwin} = proplists:get_value(win_size, Props),
    ARwin = Wwin / Hwin,

    if
        ARbb > ARwin ->
            ARw = ARbb,
            ARh = 1.0
            ;
        true ->
            ARw = ARwin,
            ARh = ARwin / ARbb
    end,

    Flen = 0.5 / math:tan(Fov * math:pi() / 180.0 / 2.0),

    VCs = lists:reverse(foldl(fun(#e3d_object{obj=Obj}, VCs_acc) ->
            appendl(Obj#e3d_mesh.vs, VCs_acc)
        end, [], Objs)),

    EyeMesh = e3d_mesh:transform(#e3d_mesh{vs = VCs}, Veye),

    VCt = list_to_tuple(EyeMesh#e3d_mesh.vs),

    case proplists:get_value(ortho_view, Props) of
        true ->
            Proj = fun ortho/2,
            Edge_norm = fun ortho_en/2,
            Front_face = fun ortho_ff/1,
            Side_face = fun ortho_sf/1,
            Wvp = Distance * ARw / Flen,
            Hvp = Distance * ARh / Flen,
            View_port = {{-Wvp / 2.0, -Hvp / 2.0, -Yonder},
                {Wvp / 2.0, Hvp / 2.0, -Hither}},
            Frustum =  View_port
            ;
        false ->
            Proj = fun persp/2,
            Edge_norm = fun persp_en/2,
            Front_face = fun persp_ff/1,
            Side_face = fun persp_sf/1,
            Wvp = ARw,
            Hvp = ARh,
            View_port = {{-Wvp / 2.0, -Hvp / 2.0, -Yonder},
                {Wvp / 2.0, Hvp / 2.0, -Hither}},
            Frustum = {Wvp / 2.0, Hvp / 2.0, -Yonder, -Hither, -Flen}
    end,


Mats_dict = materials(Mats, dict:new()),

Smooth_angle = get_pref(smooth_angle, ?DEF_SMOOTH_ANGLE),
Thresh_cos = math:cos((180.0 - Smooth_angle) * math:pi() / 180.0),

    {LinesSet, Trias_qtree, _, _}
        = foldl(fun(Obj, {Lines_acc, Trias_acc, VI_incr, TI_incr}) ->
io:format("Reading object \"~s\"...", [Obj#e3d_object.name]),
            R = add_mesh(Obj#e3d_object.obj, VCt, Mats_dict,
                Lines_acc, Trias_acc, VI_incr, TI_incr,
                Frustum, View_port, -Flen,
                Thresh_cos, Edge_norm, Front_face, Side_face, Proj),
io:format(" done~n", []),
            R
        end, {sets:new(), new_qtree(bbox_2d(View_port)), 0, 0}, Objs),

io:format("Processing...", []),

    Ls0 = foldl(fun({LI1, LI2}, Ls_acc) ->
        LCt = {LC1, LC2} = {coord(LI1, VCt), coord(LI2, VCt)},
        case project(Proj, -Flen, View_port, LCt) of
            {SC1, SC2} ->
                LN = Edge_norm(LC1, LC2),
                LD = dot(LN, LC1),
                LTrias = get_triangles({SC1, SC2}, Trias_qtree),
                appendl(line_segment({LI1, LI2}, LCt, {LN, LD}, LTrias, VCt),
                    Ls_acc)
                ;
            nil ->
                Ls_acc
        end
    end, [], sets:to_list(LinesSet)),

    Offset = divide(bbox_size(bbox_2d(View_port)), 2.0),

    Ls = project(Proj, -Flen, View_port, Offset, Wbb / Wvp, Ls0),

    Line_width = get_pref(line_width, ?DEF_LINE_WIDTH), 
    Line_cap = get_pref(line_cap, ?DEF_LINE_CAP), 

    write_eps(F, Ls, {{0.0, 0.0}, {Wbb, Hbb}}, Line_width, Line_cap),

    ok = file:close(F),

io:format(" finished in ~.1f sec~n", [timer:now_diff(now(), Start_time) / 1.0e6]).


persp({X, Y, Z}, Zf) ->
    Rz = zero_div(Zf, Z),
    {X * Rz, Y * Rz};
persp({{X1, Y1, Z1}, {X2, Y2, Z2}}, Zf) ->
    Rz_1 = zero_div(Zf, Z1),
    Rz_2 = zero_div(Zf, Z2),
    {{X1 * Rz_1, Y1 * Rz_1}, {X2 * Rz_2, Y2 * Rz_2}}.

ortho({X, Y, _Z}, _) -> {X, Y}.

appendl([], L)      when is_list(L) -> L;
appendl([E], L)     when is_list(L) -> [E | L];
appendl([E | T], L) when is_list(L) -> appendl(T, [E | L]).

incr(Incr, {A, B, C}) -> {A + Incr, B + Incr, C + Incr};
incr(Incr, {A, B}) -> {A + Incr, B + Incr};
incr(_, [])  -> [];
incr(Incr, [A | T]) -> [incr(Incr, A) | incr(Incr, T)].

normalize([])                -> [];
normalize([{P, Q} | T])      -> [normalize({P, Q}) | normalize(T)];
normalize({P, Q}) when P > Q -> {Q, P};
normalize({P, Q})            -> {P, Q}.

%
%  [9, 2, 5, 3] -> [{2, 9}, {2, 5}, {3, 5}, {3, 9}]
%
npair(L) when is_list(L) -> npair(hd(L), L).
npair(_, []) -> [];
npair(H, [E]) -> [normalize({E, H})];
npair(H, [E | T]) -> [normalize({E, hd(T)}) | npair(H, T)].

coord(I, VCt) when is_integer(I) ->
    element(I + 1, VCt).

cull({{X1, Y1, Z1}, {X2, Y2, Z2}}, Frustum) ->
    outcode({X1, Y1, Z1}, Frustum)
        band outcode({X2, Y2, Z2}, Frustum) == 0;
cull({{X1, Y1, Z1}, {X2, Y2, Z2}, {X3, Y3, Z3}}, Frustum) ->
    outcode({X1, Y1, Z1}, Frustum)
        band outcode({X2, Y2, Z2}, Frustum)
        band outcode({X3, Y3, Z3}, Frustum) == 0.

is_open(#e3d_mesh{fs = Fs}) ->
    EFs_dict = foldl(fun(#e3d_face{vs = FVIs}, ME_acc) ->
        foldl(fun(EVIt, FE_acc) ->
            case dict:find(EVIt, FE_acc) of
                {ok, Count} ->
                    dict:store(EVIt, Count + 1, FE_acc)
                    ;
                error ->
                    dict:store(EVIt, 1, FE_acc)
            end
        end, ME_acc, npair(FVIs))
    end, dict:new(), Fs),
    [] /= [C || {_, C} <- dict:to_list(EFs_dict), C < 2].

has_transp_faces(#e3d_mesh{fs = Fs}, Mats_dict) ->
    has_transp_faces(Fs, Mats_dict);
has_transp_faces([], _) -> false;
has_transp_faces([#e3d_face{mat = Mat} | T], Mats_dict) ->
    case dict:fetch(hd(Mat), Mats_dict) of
        true ->
            true
            ;
        false ->
            has_transp_faces(T, Mats_dict)
    end.

triangle(TIt, VI_incr, {TVC1, TVC2, TVC3}, TP, Edge_norm) ->
    EN1 = Edge_norm(TVC1, TVC2),
    EN2 = Edge_norm(TVC2, TVC3),
    EN3 = Edge_norm(TVC3, TVC1),
    EPt = {{EN1, dot(EN1, TVC1)},
        {EN2, dot(EN2, TVC2)}, {EN3, dot(EN3, TVC3)}},
    {incr(VI_incr, TIt), TP, EPt}.

add_edges(VIs, VI_incr, VCt, Frustum, TN, Is_FF, Mat, Edges_set,
    Lines_set, Ctrs_set) ->
    foldl(fun(EVIt, {Lines_acc, Ctrs_acc}) ->
        {EVI1, EVI2} = EVIt,
        ECt = {coord(EVI1 + VI_incr, VCt), coord(EVI2 + VI_incr, VCt)},
        case cull(ECt, Frustum) of
            true ->
                T_acc = case
                    Edges_set /= nil
                        andalso sets:is_element(EVIt, Edges_set)
                of
                    true ->
                        sets:add_element(incr(VI_incr, EVIt), Lines_acc);
                    false ->
                        Lines_acc
                end,
                CtrData = {Is_FF, TN, Mat},
                C_acc = dict:append(EVIt, CtrData, Ctrs_acc),
                {T_acc, C_acc}
                ;
            false ->
                {Lines_acc, Ctrs_acc}
        end
end, {Lines_set, Ctrs_set}, npair(VIs)).

edges(#e3d_face{vs = FVIs}) -> npair(FVIs);
edges([]) -> [];
edges([F | T]) -> appendl(edges(F), edges(T)).

add_mesh(#e3d_mesh{vs = VCs, fs = Faces, he = Hards} = Mesh, VCt,
    Mats_dict,        
    SceneLines_set, SceneTrias_qtree, VI_incr, Tri_incr,
    Frustum, View_port, Zf,
    Thresh_cos,
    Edge_norm, Front_face, Side_face, Proj) ->
    MeshIsOpen = is_open(Mesh) orelse has_transp_faces(Mesh, Mats_dict),
    TriMesh = e3d_mesh:triangulate(Mesh),

    Edge_mode = get_pref(edge_mode, ?DEF_EDGE_MODE),
    Edges_set = case Edge_mode of
        hard_edges -> sets:from_list(Hards);
        all_edges -> sets:from_list(edges(Faces));
        no_edges -> nil
    end,

    {Lines_set, Trias_qtree, TI, Ctrs_dict} =
        foldl(fun(#e3d_face{vs = Tria, mat=Mat},
            {Lines_acc, Trias_acc, TI, Ctrs_acc}) ->
        {TVI1, TVI2, TVI3} = TVIt = list_to_tuple(Tria),
        {TVC1, TVC2, TVC3} = TVCt = {coord(TVI1 + VI_incr, VCt),
            coord(TVI2 + VI_incr, VCt), coord(TVI3 + VI_incr, VCt)},
        case cull(TVCt, Frustum) of
            true ->
                TN = e3d_vec:normal(TVC1, TVC2, TVC3),
                TD = dot(TN, TVC1),
                TP = {TN, TD},
                Is_FF = Front_face(TP),
                case MeshIsOpen of
                    true ->
                        Transp = dict:fetch(hd(Mat), Mats_dict),
                        case Transp orelse Side_face(TP) of
                            true ->
                                T_qtree = Trias_acc
                                ;
                            false ->
                                T_data = case Is_FF of
                                    true ->
                                        triangle(TVIt, VI_incr,
                                            TVCt, {TN, TD}, Edge_norm)
                                        ;
                                    false ->
                                        triangle({TVI1, TVI3, TVI2},
                                            VI_incr,
                                            {TVC1, TVC3, TVC2},
                                            {neg(TN), -TD}, Edge_norm)
                                end,
                                T_qtree = add_triangle(Proj, Zf, View_port,
                                    TVCt, T_data, Trias_acc)
                        end,
                        {TriLines_set, TriCtrs_dict} =
                            add_edges(Tria, VI_incr, VCt, Frustum,
                                TN, Is_FF, Mat, Edges_set,
                                Lines_acc, Ctrs_acc),
                        {TriLines_set, T_qtree, TI + 1, TriCtrs_dict}
                        ;
                    false ->
                        case Is_FF of
                            true ->
                                T_data = triangle(TVIt, VI_incr, TVCt, TP,
                                    Edge_norm),
                                T_qtree = add_triangle(Proj, Zf, View_port,
                                    TVCt, T_data, Trias_acc),
                                {TriLines_set, TriCtrs_dict} =
                                    add_edges(Tria, VI_incr, VCt, Frustum,
                                        TN, Is_FF, Mat, Edges_set,
                                        Lines_acc, Ctrs_acc),
                                {TriLines_set, T_qtree, TI + 1, TriCtrs_dict}
                                ;
                            false ->
                                {Lines_acc, Trias_acc, TI, Ctrs_acc}
                        end
                end
                ;
            false ->
                {Lines_acc, Trias_acc, TI, Ctrs_acc}
        end
    end, {SceneLines_set, SceneTrias_qtree, Tri_incr, dict:new()},
        TriMesh#e3d_mesh.fs),
    Ctrs = incr(VI_incr, find_contours(Thresh_cos, dict:to_list(Ctrs_dict))),
    Ctrs_set = sets:from_list(Ctrs),
    Lines_set1 = sets:union(Lines_set, Ctrs_set),
    {Lines_set1, Trias_qtree, VI_incr + length(VCs), TI}.

contour({EVIt, Fs}, _) when list(Fs), length(Fs) < 2 -> [EVIt];
contour({EVIt, Fs}, _) when list(Fs), length(Fs) > 2 -> [EVIt];
contour({EVIt, Fs}, ThreshCosA) ->
    {{Is_FF1, TN1, Mat1}, {Is_FF2, TN2, Mat2}} = list_to_tuple(Fs),
    CosA = dot(TN1, TN2),
    case
        is_le(abs(CosA), ThreshCosA)
            orelse (Mat1 /= Mat2)
            orelse (Is_FF1 /= Is_FF2)
    of
        true ->
            [EVIt]
            ;
        false ->
            []
    end.

find_contours(_, []) -> [];
find_contours(ThreshCosA, [CtrEdge | T]) ->
    appendl(find_contours(ThreshCosA, T), contour(CtrEdge, ThreshCosA)).

materials([], Mats_dict) -> Mats_dict;
materials([{Name, Props} | T], Mats_dict) ->
    materials(T, dict:store(Name, is_transparent(Props), Mats_dict)).

%
% From src/wings_material.erl
%
is_transparent(MatProps) ->
    OpenGL = proplists:get_value(opengl, MatProps),
    foldl(fun(_, true) -> true;
        ({emission,_}, _) -> false;
        ({_,{_,_,_,1.0}}, _) -> false;
        ({_,{_,_,_,_}}, _) -> true;
        (_, _) -> false
    end, false, OpenGL).



eps(A) when is_float(A) -> ?EPS + ?EPS * abs(A).
% is_eq(A, B) when is_float(A), is_float(B) -> abs(A - B) =< eps(B).
% is_ne(A, B) when is_float(A), is_float(B) -> abs(A - B) >  eps(B).
is_lt(A, B) when is_float(A), is_float(B) -> A <  B - eps(B).
is_le(A, B) when is_float(A), is_float(B) -> A =< B + eps(B).
is_gt(A, B) when is_float(A), is_float(B) -> A >  B + eps(B).
is_ge(A, B) when is_float(A), is_float(B) -> A >= B - eps(B).
% is_eq0(A) when is_float(A) -> abs(A) =< ?EPS.
% is_ne0(A) when is_float(A) -> abs(A) >  ?EPS.
% is_lt0(A) when is_float(A) -> A <  -?EPS.
% is_le0(A) when is_float(A) -> A =<  ?EPS.
% is_gt0(A) when is_float(A) -> A >   ?EPS.
% is_ge0(A) when is_float(A) -> A >= -?EPS.
% eps(A, Eps) when is_float(A), is_float(Eps) -> Eps + Eps * abs(A).
% is_eq(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     abs(A - B) =< eps(B, Eps).
% is_ne(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     abs(A - B) >  eps(B, Eps).
% is_lt(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     A <  B - eps(B, Eps).
% is_le(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     A =< B + eps(B, Eps).
% is_gt(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     A >  B + eps(B, Eps).
% is_ge(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     A >= B - eps(B, Eps).
is_eq0(A, Eps) when is_float(A), is_float(Eps) -> abs(A) =< Eps.
% is_ne0(A, Eps) when is_float(A), is_float(Eps) -> abs(A) >  Eps.
is_lt0(A, Eps) when is_float(A), is_float(Eps) -> A <  -Eps.
% is_le0(A, Eps) when is_float(A), is_float(Eps) -> A =<  Eps.
% is_gt0(A, Eps) when is_float(A), is_float(Eps) -> A >   Eps.
% is_ge0(A, Eps) when is_float(A), is_float(Eps) -> A >= -Eps.

sign(A) when A > 0 -> 1;
sign(A) when A == 0 -> 0;
sign(A) when A < 0 -> -1.

min(A, B) when A =< B -> A;
min(A, B) when A > B -> B.

max(A, B) when A >= B -> A;
max(A, B) when A < B -> B.

add({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 + X2, Y1 + Y2, Z1 + Z2};
add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

sub({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 - X2, Y1 - Y2, Z1 - Z2};
sub({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.

neg({X, Y, Z}) -> {-X, -Y, -Z}.

mul({X, Y, Z}, S) -> {X * S, Y * S, Z * S};
mul({X, Y}, S) -> {X * S, Y * S}.

divide({X, Y, Z}, S) -> {X / S, Y / S, Z / S};
divide({X, Y}, S) -> {X / S, Y / S}.

zero_div(A, B) when is_float(A), is_float(B) ->
    case catch A / B of
        R when is_float(R) -> R;
        _ -> sign(A) * ?BIG
    end.

cross({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {Y1 * Z2 - Y2 * Z1, X2 * Z1 - X1 * Z2, X1 * Y2 - X2 * Y1}.

dot({X1, Y1, Z1}, {X2, Y2, Z2}) -> X1 * X2 + Y1 * Y2 + Z1 * Z2.

%normal(V1, V2, V3) -> cross(sub(V2, V1), sub(V3, V1)).

persp_en(V1, V2) -> cross(V1, V2).

ortho_en({X1, Y1, _}, {X2, Y2, _}) -> {Y2 - Y1, X1 - X2, 0.0}.

persp_ff({_, D}) -> is_lt0(D, ?EPS).

ortho_ff({{_, _, Z}, _}) -> is_gt(Z, 0.001).

persp_sf({_, D}) -> is_eq0(D, ?EPS).

ortho_sf({{_, _, Z}, _}) -> is_eq0(Z, 0.001).

%
% Cohen-Sutherland
%
outcode({X, Y, Z}, {{Xmin, Ymin, Zmin}, {Xmax, Ymax, Zmax}}) ->
    C0 = case X < Xmin of true ->  1; false -> 0 end,
    C1 = case X > Xmax of true ->  2; false -> 0 end,
    C2 = case Y < Ymin of true ->  4; false -> 0 end,
    C3 = case Y > Ymax of true ->  8; false -> 0 end,
    C4 = case Z < Zmin of true -> 16; false -> 0 end,
    C5 = case Z > Zmax of true -> 32; false -> 0 end,
    C0 bor C1 bor C2 bor C3 bor C4 bor C5;
outcode({X, Y}, {{Xmin, Ymin}, {Xmax, Ymax}}) ->
    C0 = case X < Xmin of true ->  1; false -> 0 end,
    C1 = case X > Xmax of true ->  2; false -> 0 end,
    C2 = case Y < Ymin of true ->  4; false -> 0 end,
    C3 = case Y > Ymax of true ->  8; false -> 0 end,
    C0 bor C1 bor C2 bor C3;
outcode({X, Y, Z}, {HSx, HSy, Zmin, Zmax, Zf}) ->
    R =  Z / Zf,
    Rx = HSx * R,
    Ry = HSy * R,
    if
        Z > 0.0 ->
            C0 = case X < -Rx of true ->  0; false -> 1 end,
            C1 = case X > Rx of true ->  0; false -> 2 end,
            C2 = case Y < -Ry of true ->  0; false -> 4 end,
            C3 = case Y > Ry of true ->  0; false -> 8 end
            ;
        true ->
            C0 = case X < -Rx of true ->  1; false -> 0 end,
            C1 = case X > Rx of true ->  2; false -> 0 end,
            C2 = case Y < -Ry of true ->  4; false -> 0 end,
            C3 = case Y > Ry of true ->  8; false -> 0 end
    end,
    C4 = case Z < Zmin of true -> 16; false -> 0 end,
    C5 = case Z > Zmax of true -> 32; false -> 0 end,
    C0 bor C1 bor C2 bor C3 bor C4 bor C5;
outcode({X, Y, Z}, {{{Xmin, Ymin}, {Xmax, Ymax}}, Zf}) ->
    R =  Z / Zf,
    Rx_min = Xmin * R,
    Ry_min = Ymin * R,
    Rx_max = Xmax * R,
    Ry_max = Ymax * R,
    if
        Z > 0.0 ->
            C0 = case X < Rx_min of true ->  0; false -> 1 end,
            C1 = case X > Rx_max of true ->  0; false -> 2 end,
            C2 = case Y < Ry_min of true ->  0; false -> 4 end,
            C3 = case Y > Ry_max of true ->  0; false -> 8 end
            ;
        true ->
            C0 = case X < Rx_min of true ->  1; false -> 0 end,
            C1 = case X > Rx_max of true ->  2; false -> 0 end,
            C2 = case Y < Ry_min of true ->  4; false -> 0 end,
            C3 = case Y > Ry_max of true ->  8; false -> 0 end
    end,
    C0 bor C1 bor C2 bor C3.

to_boundary({X, Y, Z}, {DX, DY, DZ}, {{Xmin, _, _}, _}) when X < Xmin ->
    {Xmin, Y + DY * (Xmin - X) / DX, Z + DZ * (Xmin - X) / DX};
to_boundary({X, Y, Z}, {DX, DY, DZ}, {_, {Xmax, _, _}}) when X > Xmax ->
    {Xmax, Y + DY * (Xmax - X) / DX, Z + DZ * (Xmax - X) / DX};
to_boundary({X, Y, Z}, {DX, DY, DZ}, {{_, Ymin, _}, _}) when Y < Ymin ->
    {X + DX * (Ymin - Y) / DY, Ymin, Z + DZ * (Ymin - Y) / DY};
to_boundary({X, Y, Z}, {DX, DY, DZ}, {_, {_, Ymax, _}}) when Y > Ymax ->
    {X + DX * (Ymax - Y) / DY, Ymax, Z + DZ * (Ymax - Y) / DY};
to_boundary({X, Y, Z}, {DX, DY, DZ}, {{_, _, Zmin}, _}) when Z < Zmin ->
    {X + DX * (Zmin - Z) / DZ, Y + DY * (Zmin - Z) / DZ, Zmin};
to_boundary({X, Y, Z}, {DX, DY, DZ}, {_, {_, _, Zmax}}) when Z > Zmax ->
    {X + DX * (Zmax - Z) / DZ, Y + DY * (Zmax - Z) / DZ, Zmax};
to_boundary({X, Y, Z}, {DX, DY, DZ}, Zlim) when is_float(Zlim) ->
    {X + DX * (Zlim - Z) / DZ, Y + DY * (Zlim - Z) / DZ, Zlim};
to_boundary({X, Y}, {DX, DY}, {{Xmin, _}, _}) when X < Xmin ->
    {Xmin, Y + DY * (Xmin - X) / DX};
to_boundary({X, Y}, {DX, DY}, {_, {Xmax, _}}) when X > Xmax ->
    {Xmax, Y + DY * (Xmax - X) / DX};
to_boundary({X, Y}, {DX, DY}, {{_, Ymin}, _}) when Y < Ymin ->
    {X + DX * (Ymin - Y) / DY, Ymin};
to_boundary({X, Y}, {DX, DY}, {_, {_, Ymax}}) when Y > Ymax ->
    {X + DX * (Ymax - Y) / DY, Ymax}.

clip({LC1, LC2}, Box) ->
    C1 = outcode(LC1, Box),
    C2 = outcode(LC2, Box),
    if
        C1 band C2 /= 0 ->
            nil
            ;
        C1 bor C2 /= 0 ->
            D = sub(LC2, LC1),
            case C1 /= 0 of
                true  ->
                    clip({to_boundary(LC1, D, Box), LC2}, Box)
                    ;
                false ->
                    clip({LC1, to_boundary(LC2, D, Box)}, Box)
            end
            ;
        true ->
            {LC1, LC2}
    end.

clip_z({LC1, LC2}, View_port) when size(View_port) == 2 ->
    {{_, _, Z1}, {_, _, Z2}} = {LC1, LC2},
    {{_, _, Zvp_min}, {_, _, Zvp_max}} = View_port,
    if
        Z1 >= Zvp_min, Z1 =< Zvp_max, Z2 >= Zvp_min, Z2 =< Zvp_max ->
            {LC1, LC2}
            ;
        ((Z1 < Zvp_min) and (Z2 < Zvp_min))
            or ((Z1 > Zvp_max) and (Z2 > Zvp_max)) ->
            nil
            ;
        true ->
            {if
                Z1 < Zvp_min ->
                    to_boundary(LC1, sub(LC2, LC1), Zvp_min)
                    ;
                Z1 > Zvp_max ->
                    to_boundary(LC1, sub(LC2, LC1), Zvp_max)
                    ;
                true ->
                    LC1
            end,
            if
                Z2 < Zvp_min ->
                    to_boundary(LC2, sub(LC2, LC1), Zvp_min)
                    ;
                Z2 > Zvp_max ->
                    to_boundary(LC2, sub(LC2, LC1), Zvp_max)
                    ;
                true ->
                    LC2
            end}
    end.



nearer({{_, _, LZ1}, {_, _, LZ2}},
    {{_, _, TZ1}, {_, _, TZ2}, {_, _, TZ3}}) ->
    is_gt(min(LZ1, LZ2), max(TZ1, max(TZ2, TZ3))).

%edge_of_tria(LIt, TIt) when LIt == nil -> false;
edge_of_tria(LIt, TIt) ->
    {TI1, TI2, TI3} = TIt,
    L = normalize(LIt),
           (L == normalize({TI1, TI2}))
    orelse (L == normalize({TI2, TI3}))
    orelse (L == normalize({TI3, TI1})).

outside_tria({D12, D23, D31}, {{_, ED12}, {_, ED23}, {_, ED31}}) ->
    is_gt(D12, ED12) orelse is_gt(D23, ED23) orelse is_gt(D31, ED31).

outside_edge(L1D, L2D, {_, ED}) ->
    (is_ge(L1D, ED) andalso is_gt(L2D, ED))
        orelse (is_gt(L1D, ED) andalso is_ge(L2D, ED)).

outside_tria({L1D12, L1D23, L1D31}, {L2D12, L2D23, L2D31},
    {EP12, EP23, EP31}) ->
    outside_edge(L1D12, L2D12, EP12)
        orelse outside_edge(L1D23, L2D23, EP23)
        orelse outside_edge(L1D31, L2D31, EP31).

orient(C, {N, D}) ->
    Dcn = dot(C, N),
    case is_lt(Dcn, D) of
        true -> -1;
        false ->
            case is_gt(Dcn, D) of
                true -> 1;
                false -> 0
            end
    end.

outside_seg({TVC1, TVC2, TVC3}, LP) ->
    P = orient(TVC1, LP) +  orient(TVC2, LP) + orient(TVC3, LP),
   (P > 1) orelse (P < -1).

line_segment(_, nil, _, _, _) -> [];
line_segment(_, LCt, _, [], _) -> [LCt];
line_segment(LIt, LCt, LP, [Triangle | T], VCt) ->
    {LC1, LC2} = LCt,
    {TIt, TP, EPt} = Triangle,
    {TI1, TI2, TI3} = TIt,
    {TN, TD} = TP,
    {EP12, EP23, EP31} = EPt,
    TCt = {coord(TI1, VCt), coord(TI2, VCt), coord(TI3, VCt)},
    {TVC1, TVC2, TVC3} = TCt,
    case
        nearer(LCt, TCt) orelse edge_of_tria(LIt, TIt)
    of
        true  ->
            line_segment(LIt, LCt, LP, T, VCt)
            ;
        false ->
            L1D = dot(LC1, TN),
            L2D = dot(LC2, TN),
            {{EN12, _}, {EN23, _}, {EN31, _}} = EPt,
            D1t = {dot(LC1, EN12), dot(LC1, EN23), dot(LC1, EN31)},
            D2t = {dot(LC2, EN12), dot(LC2, EN23), dot(LC2, EN31)},
            case
                (is_ge(L1D, TD) andalso is_ge(L2D, TD))
                    orelse outside_tria(D1t, D2t, EPt)
                    orelse outside_seg(TCt, LP)
            of
                true  ->
                    line_segment(LIt, LCt, LP, T, VCt)
                    ;
                false ->
                    L1_behind = is_le(L1D, TD),
                    L2_behind = is_le(L2D, TD),
                    case L1_behind orelse L2_behind of
                        true ->
                            Edges = [{TVC1, TVC2, EP12},
                                {TVC2, TVC3, EP23}, {TVC3, TVC1, EP31}],
                            L1_out = outside_tria(D1t, EPt),
                            L2_out = outside_tria(D2t, EPt),
                            case
                                L1_behind andalso L2_behind
                                    andalso not(L1_out orelse L2_out)
                            of
                                true ->
                                    []
                                    ;
                                false ->
                                    {Smin, Smax} = find_section(L1_out /= L2_out,
                                        LCt, LP, Edges),
                                    case
                                        divide_segment(LCt, TP, L1_behind,
                                            L2_behind, L1_out, L2_out, Smin, Smax)
                                    of
                                        {LA, LA} ->
                                            line_segment(LIt, LA, LP, T, VCt)
                                            ;
                                        {LA, LB} ->
                                            appendl(line_segment(LIt, LA, LP, T, VCt),
                                                line_segment(LIt, LB, LP, T, VCt))
                                    end
                            end
                            ;
                        false ->
                            line_segment(LIt, LCt, LP, T, VCt)
                    end
            end
    end.

divide_segment(LCt, TP, L1_behind, L2_behind, L1_out, L2_out,
    Smin, Smax) when L1_out xor L2_out ->
    {LC1, LC2} = LCt,
    {TN, TD} = TP,
    Lmin = param(LCt, Smin),
    Lmax = param(LCt, Smax),
    Lmin_behind = is_le(dot(Lmin, TN), TD),
    Lmax_behind = is_le(dot(Lmax, TN), TD),
    case Lmin_behind orelse Lmax_behind of
        true ->
            {case L1_out andalso (Smin > 0.0 + 0.001) of
                true ->
                    {LC1, Lmin}
                    ;
                false ->
                    case L1_behind of
                        true ->
                            nil
                            ;
                        false ->
                            {LC1, param(LCt, section(LCt, TP))}
                    end
            end,
            case L2_out andalso (Smax < 1.0 - 0.001) of
                true ->
                    {Lmax, LC2}
                    ;
                false ->
                    case L2_behind of
                        true ->
                            nil
                            ;
                        false ->
                            {param(LCt, section(LCt, TP)), LC2}
                    end
            end}
            ;
        false ->
            {case L1_out andalso (Smin > 0.0 + 0.001) of
                true ->
                    case L2_behind of
                        true ->
                            {LC1, param(LCt, section(LCt, TP))}
                            ;
                        false ->
                            {LC1, LC2}
                    end
                    ;
                false ->
                    nil
            end,
            case L2_out andalso (Smax < 1.0 - 0.001) of
                true ->
                    case L1_behind of
                        true ->
                            {param(LCt, section(LCt, TP)), LC2}
                            ;
                        false ->
                            {LC1, LC2}
                    end
                    ;
                false ->
                    nil
            end}
    end;
divide_segment(LCt, TP, _, _, L1_out, L2_out,
    Smin, Smax) when L1_out and L2_out ->
    {LC1, LC2} = LCt,
    {TN, TD} = TP,
    Lmin = param(LCt, Smin),
    Lmax = param(LCt, Smax),
    Lmin_behind = is_le(dot(Lmin, TN), TD),
    Lmax_behind = is_le(dot(Lmax, TN), TD),
    case Lmin_behind orelse Lmax_behind of
        true ->
            {case Lmin_behind of
                true ->
                    case L1_out andalso (Smin > 0.0 + 0.001) of
                        true ->
                            {LC1, Lmin}
                            ;
                        false ->
                            nil
                    end
                    ;
                false ->
                    {LC1, param(LCt, section(LCt, TP))}
            end,
            case Lmax_behind of
                true ->
                    case L2_out andalso (Smax < 1.0 - 0.001) of
                        true ->
                            {Lmax, LC2}
                            ;
                        false ->
                            nil
                    end
                    ;
                false ->
                    {param(LCt, section(LCt, TP)), LC2}
            end}
            ;
        false ->
            {LCt, LCt}
    end;
divide_segment(LCt, TP, L1_behind, L2_behind, _, _, _, _) ->
    {LC1, LC2} = LCt,
    {case L1_behind of
        true ->
            nil
            ;
        false ->
            {LC1, param(LCt, section(LCt, TP))}
    end,
    case L2_behind of
        true ->
            nil
            ;
        false ->
            {param(LCt, section(LCt, TP)), LC2}
    end}.

find_section(Find_one, LCt, LP, Edges) ->
    find_section(1.0, 0.0, Find_one, LCt, LP, Edges).

find_section(Smin, Smax, _, _, _, []) -> {Smin, Smax};
find_section(Smin, Smax, Find_one, LCt, LP, [{EC1, EC2, {EN, ED}} | T]) ->
    {LC1, LC2} = LCt,
    {LN, LD} = LP,
    R =  zero_div((LD - dot(LN, EC1)), dot(LN, sub(EC2, EC1))),
    if
        (R > 0.0 - 0.0001) and (R < 1.0 + 0.0001) ->
            S = zero_div((ED - dot(EN, LC1)), dot(EN, sub(LC2, LC1))),
            if
                (S > 0.0 - 0.0001) and (S < 1.0 + 0.0001) ->
                    case Find_one of
                        true ->
                            if
                                (S > 0.0 + 0.001) and (S < 1.0 - 0.001) ->
                                    {S, S}
                                    ;
                                true ->
                                    find_section(min(S, Smin), max(S, Smax),
                                        Find_one, LCt, LP, T)
                            end
                            ;
                        false ->
                            find_section(min(S, Smin), max(S, Smax),
                                Find_one, LCt, LP, T)
                    end
                    ;
                true ->
                    find_section(Smin, Smax, Find_one, LCt, LP, T)
            end
            ;
        true ->
            find_section(Smin, Smax, Find_one, LCt, LP, T)
    end.

section({LC1, LC2}, {TN, TD}) ->
    case catch (TD - dot(TN, LC1)) / dot(TN, sub(LC2, LC1)) of
        R when is_float(R)->
            R
            ;
        _ ->
            ?BIG
    end.

param({LC1, LC2}, U) ->
    e3d_vec:add(LC1, e3d_vec:mul(e3d_vec:sub(LC2, LC1), U)).




%%
%% See http://www.tulrich.com/geekstuff/partitioning.html
%%

new_qtree(Box) when size(Box) == 2 -> new_qtree(cons(Box));
new_qtree(Cons) -> {[], Cons, nil, nil, nil, nil}.

add_to_qtree(Obj, Q) -> add_to_qtree(Obj, Q, 0).

add_to_qtree(Fits, Obj, Cons, Q, Depth) ->
    case Fits of
        true ->
            add_to_qtree(Obj, if Q == nil -> new_qtree(Cons); true -> Q end, Depth + 1)
            ;
        false ->
            Q
    end.

add_to_qtree(Obj, {Ls, Cons, Q1, Q2, Q3, Q4}, Depth) ->
    C1 = cons(if Q1 == nil -> quad1(Cons); true -> Q1 end),
    C2 = cons(if Q2 == nil -> quad2(Cons); true -> Q2 end),
    C3 = cons(if Q3 == nil -> quad3(Cons); true -> Q3 end),
    C4 = cons(if Q4 == nil -> quad4(Cons); true -> Q4 end),
    F1 = fits(Obj, C1),
    F2 = fits(Obj, C2),
    F3 = fits(Obj, C3),
    F4 = fits(Obj, C4),
    case (Depth > ?MAXDEPTH - 1) orelse (not (F1 or F2 or F3 or F4)) of
        true ->
            {Data, Box, _} = Obj,
            {[{Data, Box} | Ls], Cons, Q1, Q2, Q3, Q4}
            ;
        false ->
            N1 = add_to_qtree(F1, Obj, C1, Q1, Depth), 
            N2 = add_to_qtree(F2, Obj, C2, Q2, Depth), 
            N3 = add_to_qtree(F3, Obj, C3, Q3, Depth), 
            N4 = add_to_qtree(F4, Obj, C4, Q4, Depth), 
            {Ls, Cons, N1, N2, N3, N4}
    end.

add_triangle(Proj, Zf, View_port, TVCt, Tria, Qtree) ->
    BB = bbox(Proj, Zf, View_port, TVCt),
    case BB of
        BB when is_tuple(BB) ->
            add_to_qtree({Tria, BB, mid(BB)}, Qtree)
                ;
        nil ->
            Qtree
    end.

cons({{Xmin, Ymin}, {Xmax, Ymax}}) ->
    Dx = (Xmax - Xmin) / 2.0,
    Dy = (Ymax - Ymin) / 2.0,
    {{{Xmin, Ymin}, {Xmax, Ymax}},
        {{Xmin - Dx, Ymin - Dy}, {Xmax + Dx, Ymax + Dy}},
        {Xmin + Dx, Ymin + Dy}};
cons({_, Cons, _, _, _, _}) -> Cons.

quad1({{_, {Xmax, Ymax}}, _, {Xmid, Ymid}}) ->
    {{Xmid, Ymid}, {Xmax, Ymax}}.
quad2({{{Xmin, _}, {_, Ymax}}, _, {Xmid, Ymid}}) ->
    {{Xmin, Ymid}, {Xmid, Ymax}}.
quad3({{{Xmin, Ymin}, _}, _, {Xmid, Ymid}}) ->
    {{Xmin, Ymin}, {Xmid, Ymid}}.
quad4({{{_, Ymin}, {Xmax, _}}, _, {Xmid, Ymid}}) ->
    {{Xmid, Ymin}, {Xmax, Ymid}}.

inside({X, Y}, {{Xmin, Ymin}, {Xmax, Ymax}}) ->
    (X >= Xmin) andalso (Y >= Ymin) andalso (X =< Xmax) andalso (Y =< Ymax).

fits({_, {Obj_min, Obj_max}, Obj_mid}, {Cons_box, Cons_lim, _}) ->
    inside(Obj_min, Cons_lim) andalso inside(Obj_max, Cons_lim)
        andalso inside(Obj_mid, Cons_box).

get_quad_trias(_, []) -> [];
get_quad_trias(Seg, [{Data, Box} | T]) ->
    case overlays(Seg, Box) of
        true ->
            [Data | get_quad_trias(Seg, T)]
            ;
        false ->
            get_quad_trias(Seg, T)
    end.

get_triangles(_,  nil) -> [];
get_triangles(Seg, {Ls, {_, Lim, _}, Q1, Q2, Q3, Q4}) ->
    case overlays(Seg, Lim) of
        true ->
            L1s = get_triangles(Seg, Q1),
            L2s = get_triangles(Seg, Q2),
            L3s = get_triangles(Seg, Q3),
            L4s = get_triangles(Seg, Q4),
            appendl(L4s, appendl(L3s, appendl(L2s,
                appendl(L1s, get_quad_trias(Seg, Ls)))))
            ;
        false ->
            []
    end.

intersects({{X1, Y1}, {X2, Y2}}, {{Xmin, Ymin}, {Xmax, Ymax}}) ->
    Dx = X2 - X1,
    Tx1 = zero_div(Xmin - X1, Dx),
    Tx2 = zero_div(Xmax - X1, Dx),
    Tx_near = max(-?BIG, min(Tx1, Tx2)),
    Tx_far = min(?BIG, max(Tx1, Tx2)),
    Dy = Y2 - Y1,
    Ty1 = zero_div(Ymin - Y1, Dy),
    Ty2 = zero_div(Ymax - Y1, Dy),
    Ty_near = max(Tx_near, min(Ty1, Ty2)),
    Ty_far = min(Tx_far, max(Ty1, Ty2)),
    (Ty_near =< Ty_far) andalso (Ty_near >= 0.0) andalso (Ty_far =< 1.0).

overlays({LC1, LC2}, Box) ->
    inside(LC1, Box) orelse inside(LC2, Box)
        orelse intersects({LC1, LC2}, Box).

mid({{Xmin, Ymin}, {Xmax, Ymax}}) -> {(Xmin + Xmax) / 2.0, (Ymin + Ymax) / 2.0}.

bbox_size({{Xmin, Ymin}, {Xmax, Ymax}}) -> {Xmax - Xmin, Ymax - Ymin}.

bbox({{X1, Y1}, {X2, Y2}}) ->
    Xmin = min(X1, X2),
    Ymin = min(Y1, Y2),
    Xmax = max(X1, X2),
    Ymax = max(Y1, Y2),
    {{Xmin, Ymin}, {Xmax, Ymax}}.

bbox({{Xmin_1, Ymin_1}, {Xmax_1, Ymax_1}},
    {{Xmin_2, Ymin_2}, {Xmax_2, Ymax_2}}) ->
    Xmin = min(Xmin_1, Xmin_2),
    Ymin = min(Ymin_1, Ymin_2),
    Xmax = max(Xmax_1, Xmax_2),
    Ymax = max(Ymax_1, Ymax_2),
    {{Xmin, Ymin}, {Xmax, Ymax}}.

bbox_2d({{Xmin, Ymin, _}, {Xmax, Ymax, _}}) -> {{Xmin, Ymin}, {Xmax, Ymax}}.

bbox(Proj, Zf, View_port, {TVC1, TVC2, TVC3}) ->
    TS1 = noclip_proj(Proj, Zf, View_port, {TVC1, TVC2}),
    TS2 = noclip_proj(Proj, Zf, View_port, {TVC2, TVC3}),
    TS3 = noclip_proj(Proj, Zf, View_port, {TVC3, TVC1}),
    B1 = case TS1 of nil -> nil ; Ct_1 -> bbox(Ct_1) end,
    B2 = case TS2 of
        nil ->
            B1
            ;
        Ct_2 ->
            case B1 of nil -> bbox(Ct_2) ; _ -> bbox(B1, bbox(Ct_2)) end
    end,
    B3 = case TS3 of
        nil ->
            B2
            ;
        Ct_3 ->
            case B2 of nil -> bbox(Ct_3) ; _ -> bbox(B2, bbox(Ct_3)) end
    end,
    intersection(B3, bbox_2d(View_port)).

intersection({{Xmin_1, Ymin_1}, {Xmax_1, Ymax_1}},
    {{Xmin_2, Ymin_2}, {Xmax_2, Ymax_2}}) ->
    Xmin = max(Xmin_1, Xmin_2),
    Ymin = max(Ymin_1, Ymin_2),
    Xmax = min(Xmax_1, Xmax_2),
    Ymax = min(Ymax_1, Ymax_2),
    if
        (Xmin < Xmax) and (Ymin < Ymax) ->
            {{Xmin, Ymin}, {Xmax, Ymax}}
            ;
        true ->
            nil
    end.


noclip_proj(Proj, Zf, View_port, LVCt)
    when size(View_port) == 2, size(LVCt) == 2 ->
    case clip_z(LVCt, View_port) of
        {LVC1, LVC2} ->
           {Proj(LVC1, Zf), Proj(LVC2, Zf)}
            ;
        nil ->
            nil
    end.

project(Proj, Zf, View_port, LVCt)
    when size(View_port) == 2, size(LVCt) == 2 ->
    case clip_z(LVCt, View_port) of
        {LVC1, LVC2} ->
            clip({Proj(LVC1, Zf), Proj(LVC2, Zf)}, bbox_2d(View_port))
            ;
        nil ->
            nil
    end.

project(Proj, Zf, View_port, Offset, Scale, LVCt)
    when size(View_port) == 2, size(LVCt) == 2 ->
    case project(Proj, Zf, View_port, LVCt) of
        {C1, C2} ->
            {mul(add(C1, Offset), Scale), mul(add(C2, Offset), Scale)}
            ;
        nil ->
            nil
    end;
project(_, _, _, _, _, []) -> [];
project(Proj, Zf, View_port, Offset, Scale, [LVCt | T])
    when size(View_port) == 2 ->
    case project(Proj, Zf, View_port, Offset, Scale, LVCt) of
        {LVC1, LVC2} ->
            [{LVC1, LVC2} | project(Proj, Zf, View_port, Offset, Scale, T)]
            ;
        nil ->
            project(Proj, Zf, View_port, Offset, Scale, T)
    end.

write_eps(F, Ls, {{Xbb_min, Ybb_min}, {Xbb_max, Ybb_max}}, Line_width0, Line_cap) ->
    io:put_chars(F, "%!PS-Adobe-2.0 EPSF-2.0\n"),
    io:fwrite(F, "%%BoundingBox: ~w ~w ~w ~w~n",
        [round(Xbb_min), round(Ybb_min), round(Xbb_max), round(Ybb_max)]),
    io:put_chars(F, "/l {lineto} bind def\n"),
    io:put_chars(F, "/m {moveto} bind def\n"),
    io:put_chars(F, "/n {newpath} bind def\n"),
    io:put_chars(F, "/s {stroke} bind def\n"),
    Line_width = max(Line_width0, 0.0),
    case Line_width of
        1.0 -> ok;
        _ -> io:fwrite(F, "~.1f setlinewidth~n", [Line_width])
    end,
    case Line_cap of
        0 -> ok;
        _ -> io:fwrite(F, "~w setlinecap~n", [Line_cap])
    end,
    foreach(fun({{X1, Y1}, {X2, Y2}}) ->
        io:fwrite(F, "n ~.1f ~.1f m ~.1f ~.1f l s~n", [X1, Y1, X2, Y2])
    end, Ls).
