-module(make_compat).
-export([start/1]).

start([Ebin]) ->
    file:delete(filename:join(Ebin, "proplists.beam")),
    case code:which(proplists) of
	non_existing ->
	    compile:file(proplists, [{outdir,Ebin}]);
	Path when is_list(Path) ->
	    ok
    end.

    
