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
%%     $Id: wp8_file.erl,v 1.3 2001/10/25 12:44:05 bjorng Exp $
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

fileop({file,{overwrite,FName}},_Next) ->
    question("Overwrite?",["Do you really want to overwrite ",
		           FName,"?"]);

fileop({file,ask_save_changes},_Next) ->
    question("Save changes?","Do you want to save your current "
		             "changes?");

fileop({quit,ask_save_changes},_Next) ->
    question("Save changes?","Do you want to save your current "
		             "changes?");

fileop({file,{open,Ext}}, _Next) ->
    file_dialog(1,Ext,"Open Wings 3D file");

fileop({file,{save,Ext}}, _Next) ->
    file_dialog(2,Ext,"Save Wings 3D file");

fileop({file,{import,Ext}}, _Next) ->
    file_dialog(1,Ext,"Import file into Wings 3D");

fileop({file,{export,Ext}}, _Next) ->
    file_dialog(2,Ext,"Export file from Wings 3D");

fileop({file,merge}, _Next) ->
    file_dialog(1,".wings","Merge Wings 3D file");

fileop(What,Next) ->
%     io:format("Default called for ~p~n",[What]),
%     Ret=Next(What),
%     io:format("Default returned ~p~n",[Ret]),
%     Ret.
    Next(What).

file_dialog(Type,Ext,Title) ->
    Dir = case get(wp8_file_defdir) of
	      undefined ->
		  [];
	      DefDir ->
		  filename:nativename(DefDir)
	  end,
    case erlang:port_control(wp8_file_port,Type,[Dir,0,Ext,0,
					         Title,0]) of
	[] ->
	    aborted;
	Else ->
	    put(wp8_file_defdir,filename:dirname(Else)),
	    filename:absname(Else) % Happens to turn windows slashes...
    end.

question(Title,Prompt) ->
    list_to_atom(erlang:port_control(wp8_file_port,0,
				     [Title,0,Prompt,0])).	

