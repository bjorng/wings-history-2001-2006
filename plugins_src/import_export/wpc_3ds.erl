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
%%     $Id: wpc_3ds.erl,v 1.4 2002/08/21 20:30:33 bjorng Exp $
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

command({file,{import,{tds,Ask}}}, St) ->
    do_import(Ask, St);
command({file,{export,{tds,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{tds,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"3D Studio (.3ds)",tds,[option]}].

props() ->
    [{ext,".3ds"},{ext_desc,"3D Studio File"}].

%%%
%%% Import.
%%%

do_import(Ask, St) when is_atom(Ask) ->
    wpa:dialog(Ask, dialog(import), St,
	       fun(Res) ->
		       {file,{import,{tds,Res}}}
	       end);
do_import(Attr, St) ->
    set_pref(Attr),
    wpa:import(props(), import_fun(Attr), St).

import_fun(Attr) ->
    fun(Filename) ->
	    case e3d_tds:import(Filename) of
		{ok,E3dFile0} ->
		    E3dFile = import_transform(E3dFile0, Attr),
		    {ok,E3dFile};
		{error,Error} ->
		    {error,Error}
	    end
    end.

%%%
%%% Export.
%%%

do_export(Ask, Op, _Exporter, St) when is_atom(Ask) ->
    wpa:dialog(Ask, dialog(export), St,
	       fun(Res) ->
		       {file,{Op,{tds,Res}}}
	       end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    Exporter(props(), export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr)
    end.

export_1(Filename, Contents0, Attr) ->
    Contents = export_transform(Contents0, Attr),
    case e3d_tds:export(Filename, Contents) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

dialog(import) ->
    [{label,"Import scale"},{text,get_pref(import_scale, 1.0),[{key,import_scale}]},
     {label,"(Export scale)"},{text,get_pref(export_scale, 1.0),[{key,export_scale}]}];
dialog(export) ->
    [{label,"(Import scale)"},{text,get_pref(import_scale, 1.0),[{key,import_scale}]},
     {label,"Export scale"},{text,get_pref(export_scale, 1.0),[{key,export_scale}]}].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(property_lists:get_value(export_scale, Attr, 1.0)),
    e3d_file:transform(Contents, Mat).

import_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(property_lists:get_value(import_scale, Attr, 1.0)),
    e3d_file:transform(Contents, Mat).
