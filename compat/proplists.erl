-module(proplists).
-export([get_value/2,get_value/3,is_defined/2]).

%% =====================================================================
%% Support functions for property lists
%%
%% Copyright (C) 2000-2002 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Author contact: richardc@csd.uu.se
%%
%% $Id: proplists.erl,v 1.2 2002/09/18 13:23:07 bjorng Exp $

%%%
%%% Compatability module to be used until R9 is released.
%%%

%% ---------------------------------------------------------------------

%% @spec is_defined(Key::term(), List::[term()]) -> bool()
%%
%% @doc Returns <code>true</code> if <code>List</code> contains at least
%% one entry associated with <code>Key</code>, otherwise
%% <code>false</code> is returned.

is_defined(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    true;
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    true;
       true ->
	    is_defined(Key, Ps)
    end;
is_defined(_Key, []) ->
    false.


%% ---------------------------------------------------------------------

%% @spec get_value(Key::term(), List::[term()]) -> term()
%% @equiv get_value(Key, List, undefined)

get_value(Key, List) ->
    get_value(Key, List, undefined).

%% @spec get_value(Key::term(), List::[term()], Default::term()) ->
%%         term()
%%
%% @doc Returns the value of a simple key/value property in
%% <code>List</code>. If <code>lookup(Key, List)</code> would yield
%% <code>{Key, Value}</code>, this function returns the corresponding
%% <code>Value</code>, otherwise <code>Default</code> is returned.
%%
%% @see lookup/2
%% @see get_value/1
%% @see get_all_values/2
%% @see get_bool/2

get_value(Key, [P | Ps], Default) ->
    if atom(P), P =:= Key ->
	    true;
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, Value} ->
		    Value;
		_ ->
		    %% Don</code>t continue the search!
		    Default
	    end;
       true ->
	    get_value(Key, Ps, Default)
    end;
get_value(_Key, [], Default) ->
    Default.
