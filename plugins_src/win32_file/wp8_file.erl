%%
%%  wp8_file.erl --
%%
%%     Native file dialog boxes for Win32.
%%
%%  Copyright (c) 2001 Patrik Nyblom
%%                2002-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp8_file.erl,v 1.13 2003/12/26 21:58:08 bjorng Exp $
%%

-module(wp8_file).

-export([menus/0, init/1]).

%% Operations supported by driver.
-define(OP_READ, 1).
-define(OP_WRITE, 2).

menus() -> [].

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

fileop({file,open_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, "Open"),
    Dir = proplists:get_value(directory, Prop),
    Cont(file_dialog(?OP_READ, Dir, Prop, Title));
fileop({file,save_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, "Save"),
    Dir = proplists:get_value(directory, Prop),
    Cont(file_dialog(?OP_WRITE, Dir, Prop, Title));
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

file_dialog(Type, Dir0, Prop, Title) ->
    DefName = proplists:get_value(default_filename, Prop, ""),
    {ok,Cwd} = file:get_cwd(),
    file:set_cwd(Dir0),
    Filters = file_filters(Prop),
    Dir = filename:nativename(Dir0),
    Data = [Dir,0,Title,0,DefName,0|Filters],
    case erlang:port_control(wp8_file_port, Type, Data) of
	[] ->
	    file:set_cwd(Cwd),
	    aborted;
	Else ->
	    file:set_cwd(Cwd),
	    filename:absname(Else) % Happens to turn windows slashes...
    end.

file_filters(Prop) ->
    Exts = case proplists:get_value(extensions, Prop, none) of
	       none ->
		   Ext = proplists:get_value(ext, Prop, ".wings"),
		   ExtDesc = proplists:get_value(ext_desc, Prop,
						 "Wings File"),
		   [{Ext,ExtDesc}];
	       Other -> Other
	   end,
    [file_add_all(Exts),file_filters_1(Exts++[{".*","All Files"}], [])].

file_filters_1([{Ext,Desc}|T], Acc0) ->
    Wildcard = "*" ++ Ext,
    Acc = [Acc0,Desc," (",Wildcard,")",0,Wildcard,0],
    file_filters_1(T, Acc);
file_filters_1([], Acc) -> [Acc,0].
    
file_add_all([_]) -> [];
file_add_all(Exts) ->
    All0 = ["*"++E || {E,_} <- Exts],
    All = file_add_semicolons(All0),
    ["All Formats (",All,")",0,All,0].

file_add_semicolons([E1|[E2|_]=T]) ->
    [E1,";"|file_add_semicolons(T)];
file_add_semicolons(Other) -> Other.
