%%
%%  wings_plugin.erl --
%%
%%     Experimental support of plugins.
%%
%%  Copyright (c) 2001 Jakob Cederlund, Bjorn Gustavsson
%%  Copyright (c) 2002-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_plugin.erl,v 1.26 2003/09/09 06:09:47 bjorng Exp $
%%
-module(wings_plugin).
-export([init/0,menu/2,dialog/2,dialog_result/2,command/2,call_ui/1]).
-export([install/1]).

-include("wings.hrl").
-include("e3d.hrl").
-import(lists, [append/1,flatmap/2,foreach/2,sort/1,reverse/1,foldl/3]).

%%%
%%% Currently, there can be a single directory for plugins, but
%%% sub-directories to any level will be searched.
%%% The plugin directory must be named 'plugins'. It must be located
%%% either in the same directory as the beam files, or in a directory
%%% parallell to the 'ebin' directory if the beam files are kept in
%%% a 'ebin' directory.
%%%
%%% To avoid name space clashing, plugins must be named according to
%%% the following convention:
%%%    wpT_*.beam
%%% where the T should be replaced with the type of the plugin.
%%%
%%% The types are defined as following:
%%%
%%% c   Command extension plugin-in.
%%% f   Font.
%%% 8   External user-interface plugin.
%%% 9   Default user-interface plugin.
%%%

init() ->
    put(wings_plugins, []),
    put(wings_ui, def_ui_plugin()),
    case try_dir(wings:root_dir(), "plugins") of
	none -> ok;
	PluginDir -> init(PluginDir)
    end.

call_ui(What) ->
    Ui = get(wings_ui),
    Ui(What).

menu(Name, Menu) ->
    menu_1(get(wings_plugins), Name, Menu).

menu_1([M|Ps], Name, Menu0) ->
    case catch M:menu(Name, Menu0) of
	Menu when is_list(Menu) ->
	    menu_1(Ps, Name, Menu);
	Other ->
	    io:format("~w:menu/2: bad return value: ~P\n", [M,Other,20]),
	    menu_1(Ps, Name, Menu0)
    end;
menu_1([], _Name, Menu) -> Menu.


dialog(Dialog, Ps) when is_list(Ps) ->
    dialog_1(Dialog, Ps, get(wings_plugins)).

dialog_1(Dialog, Ps, [M|Tail]) ->
    case catch M:dialog(Dialog, Ps) of
	{'EXIT',{undef,_}} ->
	    dialog_1(Dialog, Ps, Tail);
	{'EXIT',Reason} ->
	    io:format("~w:dialog/2: crashed: ~P\n", [M,Reason,20]),
	    wings_util:error("~w:dialog/2: crashed", [M]);
	NewPs when is_list(NewPs) ->
	    dialog_1(Dialog, NewPs, Tail);
	Other ->
	    io:format("~w:dialog/2: bad return value: ~P\n", [M,Other,20]),
	    wings_util:error("~w:dialog/2: bad return value", [M])
    end;
dialog_1(_Dialog, Ps, []) -> 
    Ps.

dialog_result(Dialog, Ps) when is_tuple(Dialog), is_list(Ps) ->
    dialog_result1(Dialog, Ps, get(wings_plugins)).

dialog_result1(Dialog, Ps, [M|Tail]) ->
    case catch M:dialog(Dialog, Ps) of
	{'EXIT',{undef,_}} ->
	    dialog_result1(Dialog, Ps, Tail);
	{'EXIT',Reason} ->
	    io:format("~w:dialog/2: crashed: ~P\n", [M,Reason,20]),
	    wings_util:error("~w:dialog/2: crashed", [M]);
	{Content,NewPs} when is_list(NewPs) ->
	    dialog_result1(setelement(size(Dialog), Dialog, Content), 
			   NewPs, Tail);
	Other ->
	    io:format("~w:dialog/2: bad return value: ~P\n", [M,Other,20]),
	    wings_util:error("~w:dialog/2: bad return value", [M])
    end;
dialog_result1(Dialog, Ps, []) -> 
    {element(size(Dialog), Dialog),Ps}.

command(Cmd, St) ->
    command(get(wings_plugins), Cmd, St).

command([M|Ps], Cmd, St) ->
    case catch M:command(Cmd, St) of
	next -> command(Ps, Cmd, St);
	Other ->
	    case check_result(M, Other, St) of
		next -> command(Ps, Cmd, St);
		Res -> Res
	    end
    end;
command([], _Cmd, _St) -> next.

%%%
%%% Local functions.
%%%

init(Dir) ->
    {Pas,Beams} = list_dir(Dir),
    foreach(fun(Pa) -> code:add_patha(Pa) end, Pas),
    TypeMods0 = to_modules(Beams),
    TypeMods = reverse(sort(load_modules(TypeMods0))),
    Plugins = init_plugins(TypeMods),
    put(wings_plugins, reverse(Plugins)).

init_plugins([{user_interface,M}|T]) ->
    Ui0 = get(wings_ui),
    case catch M:init(Ui0) of
	Ui when is_function(Ui) ->
	    put(wings_ui, Ui);
	Other ->
	    io:format("~w:init/1 bad return value: ~P\n", [M,Other,20])
    end,
    init_plugins(T);
init_plugins([{font,M}|T]) ->
    wings_text:font_module(M),
    init_plugins(T);
init_plugins([{_Type,M}|T]) ->
    case catch M:init() of
	true -> [M|init_plugins(T)];
	false -> init_plugins(T);
	Other ->
	    io:format("~w:init/0 bad return value: ~P\n", [M,Other,20]),
	    init_plugins(T)
    end;
init_plugins([]) -> [].
    
load_modules(TypeMods) ->
    foldl(fun({_,Mod}=TypeMod, A) ->
		  case c:l(Mod) of
		      {module,Mod} -> [TypeMod|A];
		      _Error -> A
		  end
	  end, [], TypeMods).

def_ui_plugin() ->
    fun(Missing) ->
	    Msg = io_lib:format("Reinstall Wings. "
				"Missing plugin for ~p.",
				[Missing]),
	    wings_wm:message(lists:flatten(Msg)),
	    aborted
    end.

try_dir(Base, Dir0) ->
    Dir = filename:join(Base, Dir0),
    case filelib:is_dir(Dir) of
	true -> Dir;
	false -> none
    end.

list_dir(Dir) ->
    list_dir([Dir], [], []).

list_dir([Dir|Dirs0], Pas, Beams0) ->
    case file:list_dir(Dir) of
	{ok,List} ->
	    case list_dir_1(List, Dir, Dirs0, Beams0) of
		{Dirs,Beams0} -> list_dir(Dirs, Pas, Beams0);
		{Dirs,Beams} -> list_dir(Dirs, [Dir|Pas], Beams)
	    end;
	{error,_} -> list_dir(Dirs0, Pas, Beams0)
    end;
list_dir([], Pas, Beams) -> {Pas,Beams}.

list_dir_1([[$~|_]|Ns], Dir0, Dirs, Beams) ->
    list_dir_1(Ns, Dir0, Dirs, Beams);
list_dir_1([[$w,$p,_,$_|_]=N|Ns], Dir0, Dirs, Beams) ->
    case filename:extension(N) of
	".beam" -> list_dir_1(Ns, Dir0, Dirs, [N|Beams]);
	_ -> list_dir_1(Ns, Dir0, Dirs, Beams)
    end;
list_dir_1([N|Ns], Dir0, Dirs, Beams) ->
    case try_dir(Dir0, N) of
	none -> list_dir_1(Ns, Dir0, Dirs, Beams);
	Dir -> list_dir_1(Ns, Dir0, [Dir|Dirs], Beams)
    end;
list_dir_1([], _Dir, Dirs, Beams) -> {Dirs,Beams}.
    
to_modules([[_,_,Type0|_]=Beam|T]) ->
    case convert_type(Type0) of
	undefined ->
	    to_modules(T);
	Type ->
	    Mod = list_to_atom(filename:rootname(Beam)),
	    [{Type,Mod}|to_modules(T)]
    end;
to_modules([]) -> [].

convert_type($c) -> command;
convert_type($f) -> font;
convert_type($8) -> user_interface;
convert_type($9) -> user_interface;
convert_type(_) -> undefined.

check_result(_M, {command_error,_}=Error, _St) -> throw(Error);
check_result(_M, {new_shape,Prefix,#e3d_object{}=Obj,Mat}, St) ->
    Name = object_name(Prefix, St),
    File = #e3d_file{objs=[Obj#e3d_object{name=Name,mat=Mat}]},
    wings_import:import(File, St);
check_result(_M, {new_shape,Prefix,Fs,Vs}, St) ->
    Name = object_name(Prefix, St),
    We = wings_we:build(Fs, Vs),
    wings_shape:new(Name, We, St);
check_result(_M, aborted, _St) -> aborted;
check_result(_M, {drag,_}=Drag, _) -> Drag;
check_result(_M, #st{}=St, _) -> St;
check_result(_M, {push,_}=Push, _) -> Push;
check_result(_M, {seq,_,_}=Seq, _) -> Seq;
check_result(_M, keep, _) -> keep;
check_result(M, Other, St) ->
    io:format("~w:command/3: bad return value: ~P\n", [M,Other,20]),
    St.

object_name(Prefix, #st{onext=Oid}) ->
    Prefix++integer_to_list(Oid).

%%%
%%% Installing a plug-in.
%%%

install(Name) ->
    Type = case install_file_type(Name) of
	       beam -> beam;
	       tar -> tar
	   end,
    io:format("~p: ~p\n", [Type,Name]),
    ok.

install_file_type(Name) ->
    case filename:extension(Name) of
	".tgz" -> tar;
	".beam" -> beam;
	".tar" -> tar;
	".gz" ->
	    case filename:extension(filename:rootname(Name, ".gz")) of
		".tar" -> tar;
		".beam" -> beam;
		_ ->
		    wings_util:error("File \"~s\": Unknown file type",
				     [filename:basename(Name)])
	    end
    end.

		    
	    

	    
