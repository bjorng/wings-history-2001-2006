%%
%%  wpc_rib.erl --
%%
%%     Renderman exporter.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rib.erl,v 1.2 2002/04/05 05:51:21 bjorng Exp $
%%

-module(wpc_rib).
-include_lib("e3d.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3,map/2,foreach/2]).

init() ->
    true.

menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{export,rib}}, St) ->
    Props = props(),
    wpa:export(Props, fun export/2, St);
command({file,{export_selected,rib}}, St) ->
    Props = props(),
    wpa:export_selected(Props, fun export/2, St);
command(Cmd, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"RenderMan (.rib)",rib}].

props() ->
    [{ext,".rib"},{ext_desc,"RenderMan File"}].

export(Filename, Contents) ->
    case export_1(Filename, Contents) of
	ok -> ok;
	{error,Error} -> {error,Error}
    end.

%% RenderMan export.

export_1(Name, #e3d_file{objs=Objs,mat=Mat,creator=Creator}=St) ->
    {ok,F} = file:open(Name, [write]),
    io:format(F, "# Exported from ~s\n", [Creator]),
    camera(F),
    io:put_chars(F, "WorldBegin\n"),
    foreach(fun(Obj) -> export_object(F, Obj) end, Objs),
    io:put_chars(F, "WorldEnd\n"),
    ok = file:close(F).

export_object(F, #e3d_object{name=Name,obj=Mesh}) ->
    io:format(F, "# Object: ~s\n", [Name]),
    #e3d_mesh{fs=Ftab,vs=Vtab,he=He} = Mesh,

    io:put_chars(F, "AttributeBegin\n"),
    io:put_chars(F, "SubdivisionMesh \"catmull-clark\"\n"),

    io:put_chars(F, "[ "),
    foreach(fun(#e3d_face{vs=Vs}) ->
		    io:format(F, "~p ", [length(Vs)])
	    end, Ftab),
    io:put_chars(F, "]\n"),

    io:put_chars(F, "[ "),
    foreach(fun(#e3d_face{vs=Vs}) ->
		    foreach(fun(V) ->
				    io:put_chars(F, integer_to_list(V)),
				    io:put_chars(F, " ")
			    end, Vs)
	    end, Ftab),
    io:put_chars(F, "]\n"),

    io:put_chars(F, "[\"interpolateboundary\"] [0 0] [] []\n"),

    io:put_chars(F, "\"P\"\n[\n"),
    foreach(fun({X,Y,Z}) ->
		    io:format(F, "~p ~p ~p\n", [X,Y,Z])
	    end, Vtab),
    io:put_chars(F, "]\n"),
    io:put_chars(F, "AttributeEnd\n").

camera(F) ->
    [{OX,OY,OZ},Dist,Az,El,{TrackX,TrackY},Fov] =
	wpa:camera_info([aim,distance_to_aim,azimuth,elevation,tracking,fov]),
    io:format(F, "Projection \"perspective\" \"fov\" ~p\n", [45]),
    io:format(F, "Scale ~p ~p ~p\n", [1,1,-1]),
    io:format(F, "Translate ~p ~p ~p\n", [TrackX,TrackY,-Dist]),
    io:format(F, "Rotate ~p ~p ~p ~p\n", [El,1,0,0]),
    io:format(F, "Rotate ~p ~p ~p ~p\n", [Az,0,1,0]),
    io:format(F, "Translate ~p ~p ~p\n", [OX,OY,OZ]).
