%%
%%  wpc_3ds.erl --
%%
%%     3ds max import/export.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_3ds.erl,v 1.1 2002/01/28 17:33:40 bjorng Exp $
%%

-module(wpc_3ds).

-export([init/0,menu/2,command/2]).

init() ->
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{import,tds}}, St) ->
    Props = props(),
    wpa:import(Props, fun import/1, St);
command({file,{export,tds}}, St) ->
    Props = props(),
    wpa:export(Props, fun export/2, St);
command({file,{export_selected,tds}}, St) ->
    Props = props(),
    wpa:export_selected(Props, fun export/2, St);
command(Cmd, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"3D Studio (.3ds)",tds}].

props() ->
    [{ext,".3ds"},{ext_desc,"3D Studio File"}].

import(Filename) ->
    case e3d_tds:import(Filename) of
	{ok,E3dFile} ->
	    {ok,E3dFile};
	{error,Error} ->
	    {error,Error}
    end.

export(Filename, Contents) ->
    case e3d_tds:export(Filename, Contents) of
	ok ->
	    ok;
	{error,Error} ->
	    {error,Error}
    end.

