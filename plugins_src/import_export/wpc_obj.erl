%%
%%  wpc_obj.erl --
%%
%%     Wavefront import/export.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_obj.erl,v 1.1 2002/01/28 17:33:40 bjorng Exp $
%%

-module(wpc_obj).

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

command({file,{import,obj}}, St) ->
    Props = props(),
    wpa:import(Props, fun import/1, St);
command({file,{export,obj}}, St) ->
    Props = props(),
    wpa:export(Props, fun export/2, St);
command({file,{export_selected,obj}}, St) ->
    Props = props(),
    wpa:export_selected(Props, fun export/2, St);
command(Cmd, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"Wavefront (.obj)",obj}].

props() ->
    [{ext,".obj"},{ext_desc,"Wavefront File"}].

import(Filename) ->
    case e3d_obj:import(Filename) of
	{ok,E3dFile} ->
	    {ok,E3dFile};
	{error,Error} ->
	    {error,Error}
    end.

export(Filename, Contents) ->
    case e3d_obj:export(Filename, Contents) of
	ok ->
	    ok;
	{error,Error} ->
	    {error,Error}
    end.
