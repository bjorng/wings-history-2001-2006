%%
%%  wings_gl.erl --
%%
%%     A few OpenGL utilities.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_gl.erl,v 1.3 2005/08/18 09:29:11 dgud Exp $
%%

-module(wings_gl).
-export([init_extensions/0,is_ext/1,is_ext/2,
	 init_restrictions/0,is_restriction/1,
	 error_string/1]).

%% Debugging.
-export([check_error/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

%%%
%%% OpenGL extensions.
%%%
init_extensions() ->
    ets:new(wings_gl_ext, [named_table,public,ordered_set]),
    Exts0 = lists:sort(string:tokens(gl:getString(?GL_EXTENSIONS), " ")),
    Exts = [{list_to_atom(E)} || E <- Exts0],
    ets:insert(wings_gl_ext, Exts),
    Ver = case catch get_version() of
	      {_,_,_}=V -> V;
	      _ -> {1,1,0}
	  end,
    ets:insert(wings_gl_ext, {version,Ver}).

get_version() ->
    case string:tokens(gl:getString(?GL_VERSION), ". ") of
	[Major0,Minor0] ->
	    Patch = 0;
	[Major0,Minor0,Patch0|_] ->
	    case catch list_to_integer(Patch0) of
		{'EXIT',_} -> Patch = 0;
		Patch -> Patch
	    end
    end,
    Major = list_to_integer(Major0),
    Minor = list_to_integer(Minor0),
    {Major,Minor,Patch}.

%% Either check for a given version (or higher), or
%% for that all the given extensions are implemented.
is_ext(Wanted) when is_tuple(Wanted), size(Wanted) >= 2 ->
    [{_,Actual}] = ets:lookup(wings_gl_ext, version),
    version_match(Wanted, Actual);
is_ext(Ext) when is_atom(Ext); is_list(Ext) ->
    is_ext_1(Ext).

%% Must be Wanted version or higher, or the List of extensions must match.
is_ext(Wanted, []) ->
    is_ext(Wanted);
is_ext(Wanted, List) ->
    is_ext(Wanted) orelse is_ext_1(List).

is_ext_1([]) ->
    true;
is_ext_1([Name|R]) ->
    is_ext_1(Name) andalso is_ext(R);
is_ext_1(Name) ->
    ets:member(wings_gl_ext, Name).

version_match({Ma1,_}, {Ma2,_,_})
  when Ma1 < Ma2 -> 
    true;
version_match({Ma1,Mi1}, {Ma2,Mi2,_}) 
  when Ma1 =< Ma2, Mi1 =< Mi2 -> true;
version_match({Ma1,Mi1,P1}, {Ma2,Mi2,P2}) 
  when Ma1 =< Ma2, Mi1 =< Mi2, P1 =< P2 -> true;
version_match(_,_) ->
    false.

%%%
%%% OpenGL restrictions (bugs and limitations).
%%%
init_restrictions() ->
    ets:new(wings_gl_restriction, [named_table,public,ordered_set]),
    case os:type() of
	{unix,sunos} ->
	    %% Scissor does not work for clipping text.
	    ets:insert(wings_gl_restriction, [{broken_scissor}]);
	_ ->
	    ok
    end.

is_restriction(Name) ->
    ets:member(wings_gl_restriction, Name).

error_string(0) -> no_error;
error_string(?GL_INVALID_VALUE) -> "GL_INVALID_VALUE";
error_string(?GL_INVALID_ENUM) -> "GL_INVALID_ENUM";
error_string(?GL_INVALID_OPERATION) -> "GL_INVALID_OPERATION";
error_string(?GL_STACK_OVERFLOW) -> "GL_STACK_OVERFLOW";
error_string(?GL_STACK_UNDERFLOW) -> "GL_STACK_UNDERFLOW";
error_string(?GL_OUT_OF_MEMORY) -> "GL_OUT_OF_MEMORY";
error_string(Error) -> "Error: "++integer_to_list(Error).

%%%
%%% Error checking in debug builds.
%%%

-ifdef(DEBUG).
check_error(Mod, Line) ->
    case error_string(gl:getError()) of
	no_error ->
	    ok;
	Str ->
	    io:format("~p, line ~p: ~s\n", [Mod,Line,Str]),
	    erlang:error(gl_error, [Mod,Line])
    end.
-else.
check_error(_Mod, _Line) ->
    ok.
-endif.
