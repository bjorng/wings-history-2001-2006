%%
%%  wp8_file.erl --
%%
%%     Native file dialog boxes for Mac OS X.
%%
%%  Copyright (c) 2001-2003 Patrik Nyblom, Bjorn Gustavsson.
%%
%%  Changes for OSX by Sean Hinde : 2002/2/18
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp8_mac_file.erl,v 1.15 2003/12/26 21:21:15 bjorng Exp $
%%

-module(wp8_mac_file).

-export([init/1]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

%% Operations supported by driver.
-define(OP_READ, 1).
-define(OP_WRITE, 2).

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

fileop({file,open_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, "Open"),
    Dir = proplists:get_value(directory, Prop),
    Res = file_dialog(?OP_READ, Dir, Prop, Title),
    Cont(Res);
fileop({file,save_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, "Save"),
    Dir = proplists:get_value(directory, Prop),
    Res = file_dialog(?OP_WRITE, Dir, Prop, Title),
    Cont(Res);
fileop({file,open_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Open"),
    old_file_dialog(?OP_READ, Prop, Title);
fileop({file,save_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Save"),
    old_file_dialog(?OP_WRITE, Prop, Title);
fileop(What, Next) ->
    Next(What).

old_file_dialog(Type, Prop, Title) ->
    Dir = wings_pref:get_value(current_directory),
    file_dialog(Type, Dir, Prop, Title).

file_dialog(Type, Dir, Prop, Title) ->
    wait_for_modifiers_up(),
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
    sdl_keyboard:setModState(0),
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
    case sdl_keyboard:getModState() == 0 andalso no_key_pressed() of
	true -> ok;
	false ->
	    receive after 10 -> ok end,
	    sdl_events:peepEvents(),
	    wait_for_modifiers_up()
    end.

no_key_pressed() ->
    no_key_pressed(1, sdl_keyboard:getKeyState()).

no_key_pressed(I, T) when element(I, T) =/= 0 -> false;
no_key_pressed(I, T) when I =< size(T) -> no_key_pressed(I+1, T);
no_key_pressed(_, _) -> true.
