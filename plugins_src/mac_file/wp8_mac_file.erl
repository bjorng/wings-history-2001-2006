%%
%%  wp8_file.erl --
%%
%%     Native file dialog boxes for Mac OS X.
%%
%%  Copyright (c) 2001-2002 Patrik Nyblom, Bjorn Gustavsson.
%%
%%  Changes for OSX by Sean Hinde : 2002/2/18
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp8_mac_file.erl,v 1.11 2002/11/17 16:22:11 bjorng Exp $
%%

-module(wp8_mac_file).

-export([init/1]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

%% Operations supported by driver.
-define(OP_QUESTION, 0).
-define(OP_READ, 1).
-define(OP_WRITE, 2).
-define(OP_MESSAGE, 3).

init(Next) ->
    case os:type() of
	{unix,darwin} ->
	    Dir = filename:dirname(code:which(?MODULE)),
	    case erl_ddll:load_driver(Dir, "mac_wings_file_drv") of
		ok ->
		    case open_port({spawn,"mac_wings_file_drv"},[]) of
			Port when is_port(Port) ->
			    register(wp8_file_port, Port),
			    fun(What) ->
				    fileop(What,Next)
			    end;
			Other ->
			    Next
		    end;
		Else ->
		    Next
	    end;
	_ ->
	    Next
    end.

fileop({question,Question}, _Next) ->
    wait_for_modifiers_up(),
    list_to_atom(erlang:port_control(wp8_file_port, ?OP_QUESTION,
				     ["Wings 3D",0,Question,0]));
fileop({message,Message}, _Next) ->
    wait_for_modifiers_up(),
    Title = "Wings 3D",
    erlang:port_control(wp8_file_port, ?OP_MESSAGE, [Title,0,Message,0]);
fileop({file,open_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Open"),
    file_dialog(?OP_READ, Prop, Title);
fileop({file,save_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Save"),
    file_dialog(?OP_WRITE, Prop, Title);
fileop(What, Next) ->
    Next(What).

file_dialog(Type, Prop, Title) ->
    wait_for_modifiers_up(),
    Dir = wings_pref:get_value(current_directory),
    DefName = proplists:get_value(default_filename, Prop, ""),
    Filters = file_filters(Prop),
    Data = [Dir,0,Title,0,DefName,0|Filters],
    
    %% Disabling the key repeat here and then enable it again
    %% seems to get rid of the annoying problem with repeating
    %% dialog boxes.
    sdl_keyboard:enableKeyRepeat(0, 0),
    Res = case erlang:port_control(wp8_file_port, Type, Data) of
	      [] -> aborted;
	      Else -> filename:absname(Else)
    end,
    sdl_keyboard:enableKeyRepeat(?SDL_DEFAULT_REPEAT_DELAY,
				 ?SDL_DEFAULT_REPEAT_INTERVAL),
    Res.

file_filters(Prop) ->
    case proplists:get_value(extensions, Prop, none) of
	none ->
	    [$.|Ext] = proplists:get_value(ext, Prop, ".wings"),
	    [Ext,0,0];
	Exts ->
	    file_filters_1(Exts, [])
    end.

file_filters_1([{[$.|Ext],_Desc}|T], Acc0) ->
    Acc = [Acc0,Ext,0],
    file_filters_1(T, Acc);
file_filters_1([], Acc) -> [Acc,0].

wait_for_modifiers_up() ->
    case sdl_keyboard:getModState() of
	0 -> ok;
	Other ->
	    sdl_events:peepEvents(16, ?SDL_PEEKEVENT, ?SDL_ALLEVENTS),
	    wait_for_modifiers_up()
    end.
