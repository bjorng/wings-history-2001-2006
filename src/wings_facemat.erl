%%
%%  wings_facemat.erl --
%%
%%     This module keeps tracks of the mapping from a face number
%%     to its material name.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_facemat.erl,v 1.1 2004/12/27 16:40:27 bjorng Exp $
%%
%%
%%

-module(wings_facemat).
-export([all/1,used_materials/1,
	 hide_faces/1,show_faces/1,renumber/2,gc/1]).

-include("wings.hrl").
-import(lists, [reverse/1,sort/1]).

%%%
%%% API functions.
%%%

all(#we{mat=M}=We) when is_atom(M) ->
    Vis = visible_faces(We),
    make_tab(Vis, M);
all(#we{mat=L}) when is_list(L) ->
    remove_invisible(L).

used_materials(#we{mat=M}) when is_atom(M) -> [M];
used_materials(#we{mat=L}) when is_list(L) ->
    used_materials_1(L, []).

hide_faces(#we{mat=M}=We) when is_atom(M) -> We;
hide_faces(#we{mat=L0,fs=Ftab}=We) ->
    L = hide_faces_1(L0, Ftab, []),
    We#we{mat=L}.

show_faces(#we{mat=M}=We) when is_atom(M) -> We;
show_faces(#we{mat=L0}=We) ->
    L = show_faces_1(L0, []),
    We#we{mat=L}.

renumber(Mat, _) when is_atom(Mat) -> Mat;
renumber(L, Fmap) when is_list(L) -> renumber_1(L, Fmap, []).

gc(#we{mat=Mat}=We) when is_atom(Mat) -> We;
gc(#we{mat=Mat0,fs=Ftab}=We) ->
    Fs = sofs:from_external(gb_trees:keys(Ftab), [face]),
    Mat1 = sofs:from_external(Mat0, [{face,material}]),
    Mat2 = sofs:restriction(Mat1, Fs),
    Mat = sofs:to_external(Mat2),
    case mat_all_same(Mat) of
	true ->
	    [{_,M}|_] = Mat,
	    We#we{mat=M};
	false ->
	    We#we{mat=Mat}
    end.

%%%
%%% Local functions.
%%%
    
make_tab(Fs, M) ->
    make_tab_1(Fs, M, []).

make_tab_1([F|Fs], M, Acc) ->
    make_tab_1(Fs, M, [{F,M}|Acc]);
make_tab_1([], _, Acc) -> reverse(Acc).

visible_faces(#we{fs=Ftab}) ->
    visible_faces_1(gb_trees:keys(Ftab)).

visible_faces_1([F|Fs]) when F < 0 ->
    visible_faces_1(Fs);
visible_faces_1(Fs) -> Fs.
    
remove_invisible([{F,_}|Fs]) when F < 0 ->
    remove_invisible(Fs);
remove_invisible(Fs) -> Fs.

hide_faces_1([{F,_}=P|Fms], Ftab, Acc) when F < 0 ->
    hide_faces_1(Fms, Ftab, [P|Acc]);
hide_faces_1([{F,M}=P|Fms], Ftab, Acc) ->
    case gb_trees:is_defined(F, Ftab) of
	false -> hide_faces_1(Fms, Ftab, [{-F-1,M}|Acc]);
	true -> hide_faces_1(Fms, Ftab, [P|Acc])
    end;
hide_faces_1([], _, Acc) -> sort(Acc).

show_faces_1([{F,M}|Fms], Acc) when F < 0 ->
    show_faces_1(Fms, [{-F-1,M}|Acc]);
show_faces_1(Fs, Acc) -> sort(Acc++Fs).

renumber_1([{F,M}|T], Fmap, Acc) ->
    renumber_1(T, Fmap, [{gb_trees:get(F, Fmap),M}|Acc]);
renumber_1([], _, Acc) -> sort(Acc).

mat_all_same([]) -> false;
mat_all_same([{_,M}|T]) -> mat_all_same_1(T, M).

mat_all_same_1([{_,M}|T], M) -> mat_all_same_1(T, M);
mat_all_same_1([], _) -> true;
mat_all_same_1(_, _) -> false.

used_materials_1([{_,M}|T], [M|_]=Acc) ->
    used_materials_1(T, Acc);
used_materials_1([{_,M}|T], Acc) ->
    used_materials_1(T, [M|Acc]);
used_materials_1([], Acc) ->
    ordsets:from_list(Acc).
