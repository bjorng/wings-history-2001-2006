%%
%%  wpc_wrl.erl --
%%
%%     VRML export plugin.
%%
%%  Copyright (c) 2002 Sean Hinde
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_wrl.erl,v 1.6 2002/06/09 18:40:43 bjorng Exp $
%%

-module(wpc_wrl).
-author('Sean Hinde').

-export([init/0, menu/2, command/2]).
-import(lists, [foreach/2, foldl/3, map/2]).
-include("e3d.hrl").

init() ->
    true.

menu({file, export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file, {export, wrl}}, St) ->
    Props = props(),
    wpa:export(Props, fun export/2, St);
command({file, {export_selected, wrl}}, St) ->
    Props = props(),
    wpa:export_selected(Props, fun export/2, St);
command(_, _) -> next.

menu_entry(Menu) ->
    Menu ++ [{"VRML 2.0 (.wrl)", wrl}].

props() ->
    [{ext, ".wrl"},{ext_desc, "VRML 2.0 File"}].

%% The intent is to create each object from
%% a sequence of Shapes. Each "sub Shape" will consist of all the
%% faces and vertices for one material..
export(File_name, #e3d_file{objs=Objs,mat=Mat,creator=Creator}) ->
    %io:format("~p~n~p~n",[Objs, Mat]),
    {ok,F} = file:open(File_name, [write]),
    io:format(F, "#VRML V2.0 utf8\n", []),
    io:format(F, "#Exported from ~s\n",[Creator]),
    foldl(fun(#e3d_object{name = Name, obj=Obj}, Used_mats0) ->
		  io:format(F, "DEF ~s Transform {\n",[clean_id(Name)]),
		  io:format(F, "  children [\n",[]),
		  Used_mats = export_object(F, Obj, Mat, Used_mats0),
		  io:put_chars(F, "  ]\n"),
		  io:put_chars(F, "}\n\n"),
		  Used_mats
	  end, [], Objs),
    ok = file:close(F).



export_object(F, #e3d_mesh{fs=Fs,vs=Vs,vc=[]}, Mat_defs, Used_mats0) ->
    %% A first adventure into sofs. They seem extremely powerful if
    %% I could only make any sense of the documentation ;)

    Rel = map(fun(#e3d_face{mat=[Mat0], vs=Vs1}) ->
		      {Mat0, Vs1}
	      end, Fs),

    % Make a set of all vertices involved in 
    % any way with each material              e.g. of structure at each stage:
    R = sofs:relation(Rel, [{mat, [vertex]}]),	%[{r,[1,2]},{g,[1,3]},{g,[3,4,5]}]
    FR = sofs:relation_to_family(R),		%[{r,[[1,2]]},{g,[[1,3],[3,4,5]]}]
    Mats = sofs:family_union(FR),		%[{r,[1,2]},{g,[1,3,4,5]}]

    % Make a set of faces for each material
    R2 = sofs:from_term(Rel, [{mat, face}]),
    Faces = sofs:relation_to_family(R2),

    Combined0 = sofs:join(Mats, 1, Faces, 1),
    %% e.g. [{r,[1,2],[[1,2]]},{g,[1,3,4,5],[[1,3],[3,4,5]]}]
    Combined = sofs:to_external(Combined0),
    {_, Vs1} = to_gb_tree(Vs),
    foldl_except_last(fun({Mat, Vtxs, Fces}, Used_mats_in) ->
			      io:format(F, "    Shape {\n",[]),
			      Used_mats1 = material(F, Mat, Mat_defs, Used_mats_in),
			      coords(F, Vtxs, Vs1),
			      coord_index(F, Vtxs, Fces),
			      Used_mats1
		      end,
		      fun(_, Used_mats_in) ->
			      io:put_chars(F, "    ,\n"),
			      Used_mats_in
		      end, Used_mats0, Combined);
export_object(F, #e3d_mesh{fs=Fs,vs=Vtab,vc=ColTab}, Mat_defs, Used_mats0) ->
    %% Output vertex colors. We can use the indicies, vertex table, and
    %% color table directly.
    io:format(F, "    Shape {\n",[]),
    Used_mats = material(F, default, Mat_defs, Used_mats0),
    io:format(F, "      geometry IndexedFaceSet {\n",[]),
    io:format(F, "        colorPerVertex TRUE\n",[]),

    io:format(F, "        coord Coordinate { point [\n",[]),
    foreach_except_last(fun({X,Y,Z}) ->
				io:format(F, "          ~p ~p ~p", [X,Y,Z])
			end,
			fun(_) -> io:put_chars(F, ",\n") end,
			Vtab),
    io:format(F, "]\n        }\n",[]),

    io:put_chars(F, "        coordIndex [\n"),
    foreach_except_last(fun(#e3d_face{vs=Vs}) ->
				io:put_chars(F, "          "),
				print_face(F, Vs)
			end,
			fun(_) -> io:put_chars(F, ",\n") end,
			Fs),
    io:put_chars(F, "\n        ]\n"),

    io:format(F, "        color Color { color [\n",[]),
    foreach_except_last(fun({R,G,B}) ->
				io:format(F, "          ~p ~p ~p", [R,G,B])
			end,
			fun(_) -> io:put_chars(F, ",\n") end,
			ColTab),
    io:format(F, "]\n        }\n",[]),

    io:put_chars(F, "        colorIndex [\n"),
    foreach_except_last(fun(#e3d_face{vc=Vc}) ->
				io:put_chars(F, "          "),
				print_face(F, Vc)
			end,
			fun(_) -> io:put_chars(F, ",\n") end,
			Fs),
    io:put_chars(F, "\n        ]\n"),

    io:put_chars(F, "      }\n    }\n"),
    Used_mats.

material(F, Name, Mat_defs, Used) ->
    case lists:member(Name, Used) of
	true ->
	    use_material(F, Name),
	    Used;
	false ->
	    def_material(F, Name, lookup(Name, Mat_defs)),
	    [Name|Used]
    end.

% Note: vrml represents ambient colour as a proportion of 
% diffuse colour, not in its own right.
def_material(F, Name, Mat0) ->
    Mat = lookup(opengl, Mat0),
    io:format(F, "      appearance Appearance {\n",[]),
    io:format(F, "        material DEF ~s Material {\n",[clean_id(Name)]),
    {Ar, Ag, Ab, O} = lookup(ambient, Mat),
    {Dr, Dg, Db, _} = lookup(diffuse, Mat),
    io:format(F, "          diffuseColor ~p ~p ~p\n",[Dr, Dg, Db]),
    io:format(F, "          emissiveColor ~p ~p ~p\n",[0.0, 0.0, 0.0]),
    {Sr, Sg, Sb, _} = lookup(specular, Mat),
    io:format(F, "          specularColor ~p ~p ~p\n",[Sr, Sg, Sb]),
    Amb = (Ar+Ag+Ab)/3,
    io:format(F, "          ambientIntensity ~p\n",[Amb]),
    io:format(F, "          transparency ~p\n",[1.0-O]),
    S = lookup(shininess, Mat),
    io:format(F, "          shininess ~p\n",[S]),
    io:put_chars(F, "        }\n"),
    io:format(F, "      }\n", []).
    
use_material(F, Mat) ->
    io:format(F, "      appearance Appearance {\n",[]),
    io:format(F, "        material USE ~s\n",[clean_id(Mat)]),
    io:format(F, "      }\n", []).

coords(F, Vtxs, Vs) ->
    io:format(F, "      geometry IndexedFaceSet {\n",[]),
    io:format(F, "        coord Coordinate { point [\n",[]),
    foreach_except_last(fun(Vtx) ->
				{X,Y,Z} = gb_trees:'get'(Vtx, Vs),
				io:format(F, "          ~p ~p ~p", [X,Y,Z])
			end,
			fun(_) -> io:put_chars(F, ",\n") end, Vtxs),
    io:format(F, "]\n        }\n",[]).

coord_index(F, Vtxs, Faces) ->
    io:put_chars(F, "        coordIndex [\n"),
    {_,Mapping} =  foldl(fun(Vtx, {N, G}) ->
				 {N+1, gb_trees:insert(Vtx, N, G)}
			 end, {0, gb_trees:empty()}, Vtxs),
    foreach_except_last(fun(Face) ->
				io:put_chars(F, "          "),
				print_face(F, Face, Mapping)
			end,
			fun(_) -> io:put_chars(F, ",\n") end, Faces),
    io:put_chars(F, "\n        ]\n"),
    io:put_chars(F, "      }\n    }\n").

print_face(F, Vs) ->
    foreach(fun(V) -> io:format(F, "~p, ", [V]) end, Vs),
    io:put_chars(F, "-1").

print_face(F, Face, Mapping) ->
    foreach(fun(V) -> io:format(F, "~p, ", [gb_trees:'get'(V, Mapping)]) end, Face),
    io:put_chars(F, "-1").

% Useful helpers
to_gb_tree(A) ->	       
    foldl(fun(A1, {N, G}) ->
		  {N+1, gb_trees:insert(N, A1, G)}
	  end, {0, gb_trees:empty()}, A).

foreach_except_last(F, F_each, [H,H1|T]) ->
    F(H),
    F_each(H),
    foreach_except_last(F, F_each, [H1|T]);
foreach_except_last(F, _, [H]) ->
    F(H),
    ok.
   
foldl_except_last(F, F_each, AccIn, [H,H1|T]) ->
    Acc1 = F(H, AccIn),
    foldl_except_last(F, F_each, F_each(H,Acc1), [H1|T]);
foldl_except_last(F, _, AccIn, [H]) ->
    F(H, AccIn).

lookup(K, L) ->
    {value, {K, V}} =  lists:keysearch(K, 1, L),
    V.

% Fix to SF bug report 539951 - invalid ids
% If the first char is not allowed
% then prefix whole id with W. For rest of not allowed chars
% turn them into a safe 2 char representation.
clean_id(Id) when atom(Id) ->
    clean_id(atom_to_list(Id));
clean_id([First|T]) ->
    case is_not_allowed_first_char(First) of
	true ->
	    clean_id_rest([$W,First|T]);
	false ->
	    [First|clean_id_rest(T)]
    end.

clean_id_rest([]) ->
    [];
clean_id_rest([H|T]) ->
    case is_not_allowed_char(H) of
	true ->
	    fix_char(H)++clean_id_rest(T);
	false ->
	    [H|clean_id_rest(T)]
    end.

is_not_allowed_first_char(C) ->
    (is_not_allowed_char(C) or ((C >= 16#30) and (C =< 16#39))).

is_not_allowed_char(C) ->
    (C =< 16#20) or lists:member(C, [16#22,16#23,16#27,16#2b,16#2c,
				     16#2d,16#2e,16#5b,16#5c,16#5d,
				     16#7b,16#7d,16#7f]).
fix_char($ ) ->
    "_";
fix_char(C) ->
    fix1(<<C>>).

fix1(<<C1:4,C2:4>>) ->
    [C1+65,C2+65].
