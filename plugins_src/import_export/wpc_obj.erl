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
%%     $Id: wpc_obj.erl,v 1.2 2002/06/14 13:08:17 bjorng Exp $
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

command({file,{import,{obj,Ask}}}, St) ->
    do_import(Ask, St);
command({file,{export,{obj,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, Exporter, St);
command({file,{export_selected,{obj,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"Wavefront (.obj)",obj,[option]}].

props() ->
    [{ext,".obj"},{ext_desc,"Wavefront File"}].

%%%
%%% Import.
%%%

do_import(Ask, St) when is_atom(Ask) ->
    wpa:dialog(Ask, dialog(import), St,
	       fun(Res) ->
		       {file,{import,{obj,Res}}}
	       end);
do_import(Attr, St) ->
    set_pref(Attr),
    wpa:import(props(), import_fun(Attr), St).

import_fun(Attr) ->
    fun(Filename) ->
	    case e3d_obj:import(Filename) of
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

do_export(Ask, _Exporter, St) when is_atom(Ask) ->
    wpa:dialog(Ask, dialog(export), St,
	       fun(Res) ->
		       {file,{export,{obj,Res}}}
	       end);
do_export(Attr, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    Exporter(props(), export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr)
    end.

export_1(Filename, Contents0, Attr) ->
    Contents = export_transform(Contents0, Attr),
    case e3d_obj:export(Filename, Contents, Attr) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

dialog(import) ->
    common_dialog();
dialog(export) ->
    [{"One group per material",get_pref(group_per_material, true),
      [{key,group_per_material}]}|common_dialog()].

common_dialog()->
    [{label,"Scale"},{text,get_pref(scale, 1.0),[{key,scale}]}].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(property_lists:get_value(scale, Attr, 1.0)),
    e3d_file:transform(Contents, Mat).

import_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(1.0/property_lists:get_value(scale, Attr, 1.0)),
    e3d_file:transform(Contents, Mat).
