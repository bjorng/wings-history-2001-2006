%%
%%  wp8_qt_file.erl --
%%
%%     Native file dialog boxes for QT/Unix.
%%
%%  Copyright (c) 2001 Patrik Nyblom
%%
%%  Changes for QT/Unix support by Chris Osgood : 2001/12/14
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wp8_qt_file).

-export([menus/0, init/1]).

%% Operations supported by driver.
-define(OP_QUESTION, 0).
-define(OP_READ, 1).
-define(OP_WRITE, 2).
-define(OP_MESSAGE, 3).
-define(OP_SERIOUS_QUESTION, 4).

menus() ->
    [].
init(Next) ->
    case os:type() of
	{unix,_} ->
	    Dir = filename:dirname(code:which(?MODULE)),
	    case erl_ddll:load_driver(Dir, "qt_wings_file_drv") of
		ok ->
		    case open_port({spawn,"qt_wings_file_drv"},[]) of
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
    list_to_atom(erlang:port_control(wp8_file_port, ?OP_QUESTION,
				     ["Wings 3D",0,Question,0]));

fileop({serious_question,Question}, _Next) ->
    list_to_atom(erlang:port_control(wp8_file_port, ?OP_SERIOUS_QUESTION,
				     ["Wings 3D",0,Question,0]));

fileop({message,Message}, _Next) ->
    Title = "Wings 3D",
    erlang:port_control(wp8_file_port, ?OP_MESSAGE, [Title,0,Message,0]);

fileop({file,open,Prop}, _Next) ->
    file_dialog(?OP_READ, Prop, "Open Wings 3D file");

fileop({file,save,Prop}, _Next) ->
    file_dialog(?OP_WRITE, Prop, "Save Wings 3D file");

fileop({file,import,Prop}, _Next) ->
    file_dialog(?OP_READ, Prop, "Import file into Wings 3D");

fileop({file,export,Prop}, _Next) ->
    file_dialog(?OP_WRITE, Prop, "Export file from Wings 3D");

fileop({file,merge,Prop}, _Next) ->
    file_dialog(?OP_READ, Prop, "Merge Wings 3D file");

fileop(What, Next) ->
%     io:format("Default called for ~p~n",[What]),
%     Ret=Next(What),
%     io:format("Default returned ~p~n",[Ret]),
%     Ret.
    Next(What).

file_dialog(Type, Prop, Title) ->
    Ext = proplists:get_value(ext, Prop, ".wings"),
    ExtDesc = proplists:get_value(ext_desc, Prop, "Default type"),

    Dir = case get(wp8_file_defdir) of
	      undefined ->
		  [];
	      DefDir ->
		  filename:nativename(DefDir)
	  end,
    DefName = proplists:get_value(default_filename, Prop, ""),
    Data = [Dir,0,Ext,0,ExtDesc,0,Title,0,DefName,0],
    case erlang:port_control(wp8_file_port, Type, Data) of
	[] ->
	    aborted;
	Else ->
	    put(wp8_file_defdir,filename:dirname(Else)),
	    filename:absname(Else) % Happens to turn windows slashes...
    end.
