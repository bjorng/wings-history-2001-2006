%%
%%  wings_color.erl --
%%
%%     Color utilites.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_color.erl,v 1.3 2001/12/10 07:28:16 bjorng Exp $
%%

-module(wings_color).
-export([init/0,default/0,share/1,store/1,
	 average/1,average/2,white/0]).

-include("wings.hrl").
-import(lists, [foreach/2]).

-define(BLACK, {0.0,0.0,0.0}).
-define(WHITE, {1.0,1.0,1.0}).

init() ->
    Black = ?BLACK,
    White = ?WHITE,
    Standard = [Black,White,average(Black, White)],
    foreach(fun(C0) ->
		    C = wings_util:share(C0),
		    put(C, C)
	    end, Standard),
    put(wings_default_color, get(White)).

default() ->
    get(wings_default_color).

share({_,_,_}=RGB) ->
    case get(RGB) of
	undefined -> wings_util:share(RGB);
	Other -> Other
    end.

store({_,_,_}=RGB) ->
    case get(RGB) of
	undefined ->
	    C = wings_util:share(RGB),
	    put(C, C),
	    C;
	Other -> Other
    end.

average(Colors) ->
    share(e3d_vec:average(Colors)).

average(Same, Same) -> Same;
average(C1, C2) -> share(e3d_vec:average([C1,C2])).

white() ->
    get(?WHITE).

    
    
