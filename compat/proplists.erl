-module(proplists).
-export([get_value/2,get_value/3,is_defined/2]).

%%%
%%% Compatability module to be used until R9 is released.
%%%

get_value(K, P) ->
    property_lists:get_value(K, P).

get_value(K, P, D) ->
    property_lists:get_value(K, P, D).

is_defined(K, P) ->
    property_lists:is_defined(K, P).
