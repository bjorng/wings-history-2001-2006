-module(proplists).
-export([get_value/2,get_value/3,get_bool/2,is_defined/2]).

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
%% $Id: proplists.erl,v 1.3 2002/09/20 12:24:34 bjorng Exp $

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

%% =====================================================================
%% get_bool(Key, List) -> bool()
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = [Name | {Key, ...} | term()]
%%
%%	If `lookup(Key, List)' would yield `{Key, true}', this function
%%	returns `true'; otherwise `false' is returned. Thus, the entry
%%	associated with `Key' is assumed to be a 2-tuple whose second
%%	element is a boolean value (`true' or `false'), and if it is
%%	not, or if there is no entry associated with `Key' in `List',
%%	the value `false' is used.

get_bool(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    true;
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, true} ->
		    true;
		_ ->
		    %% Don't continue the search!
		    false
	    end;
       true ->
	    get_bool(Key, Ps)
    end;
get_bool(Key, []) ->
    false.
