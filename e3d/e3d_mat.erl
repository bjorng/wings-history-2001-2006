%%
%%  e3d_mat.erl --
%%
%%     Operations on matrices.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_mat.erl,v 1.1 2001/08/14 18:16:30 bjorng Exp $
%%

-module(e3d_mat).

-export([identity/0]).

identity() ->
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,Zero,
     Zero,One,Zero,Zero,
     Zero,Zero,One,Zero,
     Zero,Zero,Zero,One}.
