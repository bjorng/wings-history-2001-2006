%%
%%  wpa.erl --
%%
%%     Wings Plugin API.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpa.erl,v 1.1 2001/10/17 07:48:25 bjorng Exp $
%%
-module(wpa).
-export([ask/3]).

ask(Ask, Qs, Fun) ->
    wings_util:ask(Ask, Qs, Fun).
