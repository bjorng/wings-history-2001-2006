%%
%%  wp8_file.erl --
%%
%%     Native file dialog boxes for Win32.
%%
%%  Copyright (c) 2001 Patrik Nyblom
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp8_file.erl,v 1.5 2001/11/14 11:23:02 bjorng Exp $
%%

-module(wp8_file).
-author('patrik@ELLA').

-export([menus/0, init/1]).

menus() ->
    [].
init(Next) ->
    case os:type() of
	{win32,_} ->
	    Dir = filename:dirname(code:which(?MODULE)),
	    case erl_ddll:load_driver(Dir, "wings_file_drv") of
		ok ->
		    case open_port({spawn,"wings_file_drv"},[]) of
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

fileop({file,overwrite,Prop},_Next) ->
    yes; %Fixed in dialog instead

fileop({file,ask_save_changes,Prop},_Next) ->
    question("Save changes?","Do you want to save your current "
		             "changes?");

fileop({quit,ask_save_changes,Prop},_Next) ->
    question("Save changes?","Do you want to save your current "
		             "changes before quitting?");

fileop({file,open,Prop}, _Next) ->
    file_dialog(1,Prop,"Open Wings 3D file");

fileop({file,save,Prop}, _Next) ->
    file_dialog(2,Prop,"Save Wings 3D file");

fileop({file,import,Prop}, _Next) ->
    file_dialog(1,Prop,"Import file into Wings 3D");

fileop({file,export,Prop}, _Next) ->
    file_dialog(2,Prop,"Export file from Wings 3D");

fileop({file,merge,Prop}, _Next) ->
    file_dialog(1,Prop,"Merge Wings 3D file");

fileop(What,Next) ->
%     io:format("Default called for ~p~n",[What]),
%     Ret=Next(What),
%     io:format("Default returned ~p~n",[Ret]),
%     Ret.
    Next(What).

file_dialog(Type, Prop, Title) ->
    Ext = property_lists:get_value(ext, Prop, ".wings"),
    ExtDesc = property_lists:get_value(ext_desc, Prop, "Default type"),

    Dir = case get(wp8_file_defdir) of
	      undefined ->
		  [];
	      DefDir ->
		  filename:nativename(DefDir)
	  end,
    DefName = property_lists:get_value(default_filename, Prop, ""),
    Data = [Dir,0,Ext,0,ExtDesc,0,Title,0,DefName,0],
    case erlang:port_control(wp8_file_port, Type, Data) of
	[] ->
	    aborted;
	Else ->
	    put(wp8_file_defdir,filename:dirname(Else)),
	    filename:absname(Else) % Happens to turn windows slashes...
    end.

question(Title,Prompt) ->
    list_to_atom(erlang:port_control(wp8_file_port,0,
				     [Title,0,Prompt,0])).	

