%%
%%  wpc_resize.erl --
%%
%%     Plug-in to resize main wings window (mainly for use under OS X).
%%
%%  Copyright (c) 2002 Sean Hinde
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_resize.erl,v 1.1 2002/07/22 16:08:42 bjorng Exp $
%%

-module(wpc_resize).

-export([init/0,menu/2,command/2]).

init() ->
    true.

menu({view}, Menu0) ->
    Menu0 ++ [separator,
	      {"Resize",resize,
	       "Resize the main Wings window."}];
menu(_, Menu) -> Menu.

command({view,resize}, St) ->
    {W0, H0} = wings_pref:get_value(window_size),
    wpa:ask([{"Width", W0},{"Height", H0}], St,
	    fun([W,H]) ->
		    wings_io:putback_event({resize,W,H}),
		    ignore
	    end);
command(Cmd, _) -> next.


