%%
%%  wings_file.erl --
%%
%%     This module contains the commands in the File menu.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_file.erl,v 1.150 2004/10/08 06:02:29 dgud Exp $
%%

-module(wings_file).
-export([init/0,init_autosave/0,menu/1,command/2]).

-include("wings.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").
-include_lib("kernel/include/file.hrl").

-import(lists, [sort/1,reverse/1,flatten/1,foldl/3,keymember/3,keydelete/3,foreach/2]).
-import(filename, [dirname/1]).

-define(WINGS, ".wings").

init() ->
    case wings_pref:get_value(current_directory) of
	undefined ->
	    case file:get_cwd() of
		{ok,Cwd} -> wings_pref:set_value(current_directory, Cwd);
		{error,_} -> wings_pref:set_value(current_directory, "/")
	    end;
	Cwd ->
	    case filelib:is_dir(Cwd) of
		false ->
		    wings_pref:delete_value(current_directory),
		    init();
		true -> ok
	    end
    end.

menu(_) ->
    ImpFormats = [{"Nendo (.ndo)...",ndo}],
    ExpFormats = [{"Nendo (.ndo)...",ndo}],
    [{?STR(menu,3,"New"),new,?STR(menu,4,"Create a new, empty scene")},
     {?STR(menu,5,"Open..."),open,?STR(menu,6,"Open a previously saved scene")},
     {?STR(menu,7,"Merge..."),merge,?STR(menu,8,"Merge a previously saved scene into the current scene")},
      separator,
     {?STR(menu,9,"Save"),save,?STR(menu,10,"Save the current scene")},
     {?STR(menu,11,"Save As..."),save_as,?STR(menu,12,"Save the current scene under a new name")},
     {?STR(menu,13,"Save Selected..."),save_selected,?STR(menu,14,"Save only the selected objects or faces")},
     {?STR(menu,15,"Save Incrementally"),save_incr},
      separator,
     {?STR(menu,16,"Revert"),revert,?STR(menu,17,"Revert current scene to the saved contents")},
      separator,
     {?STR(menu,18,"Import"),{import,ImpFormats}},
     {?STR(menu,19,"Export"),{export,ExpFormats}},
     {?STR(menu,20,"Export Selected"),{export_selected,ExpFormats}},
      separator,
     {?STR(menu,21,"Import Image..."),import_image,?STR(menu,22,"Import an image file")},
      separator,
     {?STR(menu,23,"Render"),{render,[]}},
      separator,
     {?STR(menu,24,"Install Plug-In"),install_plugin},
      separator|recent_files([{?STR(menu,25,"Exit"),quit}])].
												
    
command(new, St) ->
    new(St);
command(confirmed_new, St) ->
    confirmed_new(St);
command(open, St) ->
    open(St);
command(confirmed_open_dialog, _) ->
    confirmed_open_dialog();
command({confirmed_open,Filename}, St) ->
    confirmed_open(Filename, St);
command({confirmed_open,Next,Filename}, _) ->
    Next(Filename);
command(merge, _) ->
    merge();
command({merge,Filename}, St) ->
    merge(Filename, St);
command(save, St) ->
    save(ignore, St);
command({save,Next}, St) ->
    save(Next, St);
command(save_as, St) ->
    save_as(ignore, St);
command({save_as,{Filename,Next}}, St) ->
    save_now(Next, St#st{file=Filename});
command(save_selected, St) ->
    save_selected(St);
command({save_selected,Filename}, St) ->
    save_selected(Filename, St);
command(save_incr, St) -> 
    save_incr(St);
command(revert, St0) ->
    case revert(St0) of
	{error,Reason} ->
	 wings_util:error(?STR(command,1,"Revert failed: ") ++ Reason),
	    St0;
	#st{}=St -> {save_state,St}
    end;
command({import,ndo}, _St) ->
    import_ndo();
command({import,{ndo,Filename}}, St) ->
    import_ndo(Filename, St);
command(import_image, _St) ->
    import_image();
command({import_image,Name}, _) ->
    import_image(Name);
command({export,ndo}, St) ->
    export_ndo(export, ?STR(command,2,"Export"), St);
command({export_selected,ndo}, St) ->
    export_ndo(export_selected, ?STR(command,3,"Export Selected"), St);
command({export,{ndo,Filename}}, St) ->
    do_export_ndo(Filename, St);
command({export_selected,{ndo,Filename}}, St) ->
    Shs0 = wings_sel:fold(fun(_, #we{id=Id}=We, A) ->
				  [{Id,We}|A]
			  end, [], St),
    Shs = gb_trees:from_orddict(reverse(Shs0)),
    do_export_ndo(Filename, St#st{shapes=Shs});
command(install_plugin, _St) ->
    install_plugin();
command({install_plugin,Filename}, _St) ->
    wings_plugin:install(Filename);
command(quit, #st{saved=true}) ->
    quit;
command(quit, _) ->
    wings_util:yes_no_cancel(?STR(command,4,"Do you want to save your changes before quitting?"),
			     fun() -> {file,{save,{file,quit}}} end,
			     fun() -> {file,confirmed_quit} end);
command(confirmed_quit, _) ->
    quit;
command(Key, St) when is_integer(Key), 1 =< Key ->
    Recent0 = wings_pref:get_value(recent_files, []),
    {_,File} = lists:nth(Key, Recent0),
    case filelib:is_file(File) of
	true ->
	    named_open(File, St);
	false ->
	    Recent = delete_nth(Recent0, Key),
	    wings_pref:set_value(recent_files, Recent),
	    wings_util:error(?STR(command,5,"This file has been moved or deleted."))
    end.

delete_nth([_|T], 1) -> T;
delete_nth([H|T], N) -> [H|delete_nth(T, N-1)];
delete_nth([], _) -> [].

confirmed_new(#st{file=File}=St) ->
    %% Remove autosaved file; user has explicitly said so.
    catch file:delete(autosave_filename(File)),
    new(St#st{saved=true}).

new(#st{saved=true}=St0) ->
    St1 = clean_st(St0#st{file=undefined}),
    St = clean_images(wings_undo:init(St1)),
    wings:caption(St),
    {new,St#st{saved=true}};
new(St0) ->			     %File is not saved or autosaved.
    wings:caption(St0#st{saved=false}), 
    wings_util:yes_no_cancel(?STR(new,1,"Do you want to save your changes?"),
			     fun() -> {file,{save,{file,new}}} end,
			     fun() -> {file,confirmed_new} end).

open(#st{saved=true}) ->
    confirmed_open_dialog();
open(St) ->
    wings:caption(St#st{saved=false}),		%Clear any autosave flag.
    Confirmed = {file,confirmed_open_dialog},
    wings_util:yes_no_cancel(?STR(open,1,"Do you want to save your changes?"),
			     fun() -> {file,{save,Confirmed}} end,
			     fun() -> Confirmed end).

confirmed_open_dialog() ->
    %% All confirmation questions asked. The former contents has either
    %% been saved, or the user has accepted that it will be discarded.
    %% Go ahead and ask for the filename.

    Cont = fun(Filename) -> {file,{confirmed_open,Filename}} end,
    Dir = wings_pref:get_value(current_directory),
    Ps = [{directory,Dir},{title,?STR(confirmed_open_dialog,1,"Open")}|wings_prop()],
    wpa:import_filename(Ps, Cont).

confirmed_open(Name, St0) ->
    Fun = fun(File) ->
		  %% We now have:
		  %%   Name: Original name of file to be opened.
		  %%   File: Either original file or the autosave file
		  St1 = clean_st(St0#st{file=undefined}),
		  St2 = wings_undo:init(St1),
		  case ?SLOW(wings_ff_wings:import(File, St2)) of
		      #st{}=St3 ->
			  set_cwd(dirname(File)),
			  St = clean_images(St3),
			  add_recent(Name),
			  wings:caption(St#st{saved=true,file=Name});
		      {error,Reason} ->
			  clean_new_images(St2),
			  wings_util:error(?STR(confirmed_open,1,"Read failed: ") ++ Reason)
		  end
	  end,
    use_autosave(Name, Fun).

named_open(Name, #st{saved=true}=St) ->
    confirmed_open(Name, St);
named_open(Name, St) ->
    wings:caption(St#st{saved=false}),		%Clear any autosave flag.
    Confirmed = {file,{confirmed_open,Name}},
    wings_util:yes_no_cancel(?STR(named_open,1,"Do you want to save your changes?"),
			     fun() -> {file,{save,Confirmed}} end,
			     fun() -> Confirmed end).

merge() ->
    Cont = fun(Filename) -> {file,{merge,Filename}} end,
    Dir = wings_pref:get_value(current_directory),
    Ps = [{title,?STR(merge,1,"Merge")},{directory,Dir}|wings_prop()],
    wpa:import_filename(Ps, Cont).

merge(Name, St0) ->
    Fun = fun(File) ->
		  %% We now have:
		  %%   Name: Original name of file to be opened.
		  %%   File: Either original file or the autosave file
		  St1 = St0#st{saved=wings_image:next_id()},
		  case ?SLOW(wings_ff_wings:import(File, St0)) of
		      {error,Reason} ->
			  clean_new_images(St1),
			  wings_util:error(?STR(merge,2,"Read failed: ") ++ Reason);
		      #st{}=St ->
			  set_cwd(dirname(Name)),
			  St#st{saved=false}
		  end
	  end,
    use_autosave(Name, Fun).

save(Next, #st{saved=true}) ->
    maybe_send_action(Next);
save(Next, #st{file=undefined}=St) ->
    save_as(Next, St);
save(Next, St) ->
    save_now(Next, St).

save_as(Next, St) ->
    Cont = fun(Name) ->
		   set_cwd(dirname(Name)),
		   {file,{save_as,{Name,Next}}}
	   end,
    Ps = [{title,?STR(save_as,1,"Save")}|wings_prop()],
    wpa:export_filename(Ps, St, Cont).

save_now(Next, #st{file=Name}=St) ->
    Backup = backup_filename(Name),
    file:rename(Name, Backup),
    file:delete(autosave_filename(Name)),
    case ?SLOW(wings_ff_wings:export(Name, St)) of
	ok ->
	    set_cwd(dirname(Name)),
	    add_recent(Name),
	    maybe_send_action(Next),
	    {saved,wings:caption(St#st{saved=true})};
	{error,Reason} ->
	    wings_util:error(?STR(save_now,1,"Save failed: ") ++ Reason)
    end.

maybe_send_action(ignore) -> keep;
maybe_send_action(Action) -> wings_wm:later({action,Action}).
    
save_selected(#st{sel=[]}) ->
    wings_util:error(?STR(save_selected,1,"This command requires a selection."));
save_selected(St) ->
    Ps = [{title,?STR(save_selected,2,"Save Selected")}|wings_prop()],
    Cont = fun(Name) -> {file,{save_selected,Name}} end,
    wpa:export_filename(Ps, St, Cont).

save_selected(Name, #st{shapes=Shs0,sel=Sel}=St0) ->
    Shs = [Sh || {Id,_}=Sh <- gb_trees:to_list(Shs0),
		 keymember(Id, 1, Sel)],
    St = St0#st{shapes=gb_trees:from_orddict(Shs)},
    case ?SLOW(wings_ff_wings:export(Name, St)) of
	ok -> keep;
	{error,Reason} -> wings_util:error(Reason)
    end.

%%%
%%% Save incrementally. Original code submitted by Clacos.
%%%

save_incr(#st{saved=true}=St) -> St;
save_incr(#st{file=undefined}=St0) ->
    save_as(ignore, St0);
save_incr(#st{file=Name0}=St) -> 
    Name = increment_name(Name0),
    save_now(ignore, St#st{file=Name}).

increment_name(Name0) ->
    Name1 = reverse(filename:rootname(Name0)),
    Name = case find_digits(Name1)  of
	       {[],Base} ->
		   Base ++ "_01.wings";
	       {Digits0,Base} ->
		   Number = list_to_integer(Digits0),
		   Digits = integer_to_list(Number+1),
		   Zs = case length(Digits0)-length(Digits) of
			    Neg when Neg =< 0 -> [];
			    Nzs -> lists:duplicate(Nzs, $0)
			end,
		   Base ++ Zs ++ Digits ++ ".wings"
	   end,
    update_recent(Name0, Name),
    Name.

find_digits(List) -> 
    find_digits1(List, []).

find_digits1([H|T], Digits) when $0 =< H, H =< $9 ->
    find_digits1(T, [H|Digits]);
find_digits1([_|_]=Rest, Digits) ->
    {Digits,reverse(Rest)};
find_digits1([], Digits) ->
    {Digits,[]}.

wings_prop() ->
    %% Should we add autosaved wings files ??
    %% It's pretty nice to NOT see them in the file chooser /Dan
    [{ext,?WINGS},{ext_desc,"Wings File"}].    

use_autosave(File, Body) ->
    case file:read_file_info(File) of
	{ok,SaveInfo} ->
	    use_autosave_1(SaveInfo, File, Body);
	{error, _} ->			     % use autosaved file if it exists 
	    Auto = autosave_filename(File),
	    Body(case filelib:is_file(Auto) of
		     true -> Auto;
		     false -> File			%Let reader handle error.
		 end)
    end.

use_autosave_1(#file_info{mtime=SaveTime0}, File, Body) ->
    Auto = autosave_filename(File),
    case file:read_file_info(Auto) of
	{ok,#file_info{mtime=AutoInfo0}} ->
	    SaveTime = calendar:datetime_to_gregorian_seconds(SaveTime0),
	    AutoTime = calendar:datetime_to_gregorian_seconds(AutoInfo0),
	    if
		AutoTime > SaveTime ->
		    Msg = ?STR(use_autosave_1,1,"An autosaved file with a later time stamp exists; do you want to load the autosaved file instead?"),
		    wings_util:yes_no(Msg, autosave_fun(Body, Auto),
				      autosave_fun(Body, File));
		true ->
		    Body(File)
	    end;
	{error, _} ->				% No autosave file
	    Body(File)
    end.

autosave_fun(Next, Filename) ->
    fun() -> {file,{confirmed_open,Next,Filename}} end.

set_cwd(Cwd) ->
    wings_pref:set_value(current_directory, Cwd).

init_autosave() ->
    Name = autosaver,
    case wings_wm:is_window(Name) of
	true -> ok;
	false ->
	    Op = {seq,push,get_autosave_event(make_ref(), #st{saved=auto})},
	    wings_wm:new(Name, {0,0,1}, {0,0}, Op),
	    wings_wm:hide(Name),
	    wings_wm:set_prop(Name, display_lists, geom_display_lists)
    end,
    wings_wm:send(Name, start_timer).

get_autosave_event(Ref, St) ->
    {replace,fun(Ev) -> autosave_event(Ev, Ref, St) end}.
    
autosave_event(start_timer, OldTimer, St) ->
    wings_wm:cancel_timer(OldTimer),
    case {wings_pref:get_value(autosave),wings_pref:get_value(autosave_time)} of
	{false,_} -> delete;
	{true,0} ->
	    N = 24*60,
	    wings_pref:set_value(autosave_time, N),
	    Timer = wings_wm:set_timer(N*60000, autosave),
	    get_autosave_event(Timer, St);
	{true,N} ->
	    Timer = wings_wm:set_timer(N*60000, autosave),
	    get_autosave_event(Timer, St)
    end;
autosave_event(autosave, _, St) ->
    autosave(St),
    wings_wm:later(start_timer);
autosave_event({current_state,St}, Timer, _) ->
    get_autosave_event(Timer, St);
autosave_event(_, _, _) -> keep.

autosave(#st{file=undefined} = St) -> St;
autosave(#st{saved=true} = St) -> St;
autosave(#st{saved=auto} = St) -> St;
autosave(#st{file=Name}=St) ->
    Auto = autosave_filename(Name),
    %% Maybe this should be spawned to another process
    %% to let the autosaving be done in the background.
    %% But I don't want to copy a really big model either.
    case ?SLOW(wings_ff_wings:export(Auto, St)) of
	ok ->
	    wings:caption(St#st{saved=auto});
	{error,Reason} ->
	    Msg = lists:flatten(io_lib:format(?STR(autosave,1,"Autosaving \"~s\" failed: ~s"), [Auto,Reason])),
	    wings_util:message(Msg)
    end.

autosave_filename(File) ->
    Base = filename:basename(File),
    Dir = filename:dirname(File),
    filename:join(Dir, "#" ++ Base ++ "#").

backup_filename(File) ->
    File ++ "~".

add_recent(Name) ->
    Base = filename:basename(Name),
    case filename:extension(Base) of
	?WINGS ->
	    File = {Base,Name},
	    Recent0 = wings_pref:get_value(recent_files, []),
	    Recent1 = Recent0 -- [File],
	    Recent = add_recent(File, Recent1),
	    wings_pref:set_value(recent_files, Recent);
	_Other -> ok
    end.

update_recent(Old, New) ->
    OldFile = {filename:basename(Old),Old},
    NewFile = {filename:basename(New),New},
    Recent0 = wings_pref:get_value(recent_files, []),
    Recent1 = Recent0 -- [OldFile,NewFile],
    Recent = add_recent(NewFile, Recent1),
    wings_pref:set_value(recent_files, Recent).

add_recent(File, [A,B,C,D,E|_]) -> [File,A,B,C,D,E];
add_recent(File, Recent) -> [File|Recent].

recent_files(Rest) ->
    case wings_pref:get_value(recent_files, []) of
	[] -> Rest;
	Files -> number_files(Files, 1, [separator|Rest])
    end.

number_files([{Base,_}|T], I, Rest) ->
    [{Base,I}|number_files(T, I+1, Rest)];
number_files([], _I, Rest) -> Rest.

%%
%% The Revert command.
%%

revert(#st{file=undefined}=St) -> St;
revert(#st{file=File}=St0) ->
    St1 = clean_st(St0),
    case ?SLOW(wings_ff_wings:import(File, St1)) of
	#st{}=St -> clean_images(St);
	{error,_}=Error ->
	    Error
    end.

%%
%% Import.
%%

import_ndo() ->
    Ps = [{ext,".ndo"},{ext_desc,"Nendo File"}],
    Cont = fun(Name) -> {file,{import,{ndo,Name}}} end,
    wpa:import_filename(Ps, Cont).

import_ndo(Name, St0) ->
    case ?SLOW(wings_ff_ndo:import(Name, St0)) of
	#st{}=St ->
	    {save_state,St};
	{error,Reason} ->
	    wings_util:error(?STR(import_ndo,1,"Import failed: ") ++ Reason),
	    St0
    end.

import_image() ->
    Ps = [{extensions,wpa:image_formats()}],
    Cont = fun(Name) -> {file,{import_image,Name}} end,
    wpa:import_filename(Ps, Cont).

import_image(Name) ->
    case wings_image:from_file(Name) of
	Im when is_integer(Im) ->
	    keep;
	{error,Error} ->
	    wings_util:error(?STR(import_image,1,"Failed to load \"~s\": ~s\n"),
			     [Name,file:format_error(Error)])
    end.

%%
%% Export.
%%

export_ndo(Cmd, Title, St) ->
    Ps = [{title,Title},{ext,".ndo"},{ext_desc,"Nendo File"}],
    Cont = fun(Name) -> {file,{Cmd,{ndo,Name}}} end,
    wpa:export_filename(Ps, St, Cont).

do_export_ndo(Name, St) ->
    case wings_ff_ndo:export(Name, St) of
	ok -> keep;
	{error,Reason} -> wings_util:error(Reason)
    end.

%%%
%%% Install a plug-in.
%%%

install_plugin() ->
    Props = [{title,?STR(install_plugin,1,"Install Plug-In")},
	     {extensions,
	      [{".gz",?STR(install_plugin,2,"GZip Compressed File")},
	       {".tar",?STR(install_plugin,3,"Tar File")},
	       {".tgz",?STR(install_plugin,4,"Compressed Tar File")},
	       {".beam",?STR(install_plugin,5,"Beam File")}]}],
    Cont = fun(Name) -> {file,{install_plugin,Name}} end,
    wpa:import_filename(Props, Cont).

%%%    
%%% Utilities.
%%%

clean_st(St) ->
    foreach(fun(Win) ->
		    wings_wm:set_prop(Win, wireframed_objects, gb_sets:empty())
	    end, wings_util:geom_windows()),
    DefMat = wings_material:default(),
    Empty = gb_trees:empty(),
    Limit = wings_image:next_id(),
    wings_pref:delete_scene_value(),
    wings_view:delete_all(St#st{onext=1,shapes=Empty,mat=DefMat,
				sel=[],ssels=Empty,saved=Limit}).

clean_images(#st{saved=Limit}=St) when is_integer(Limit) ->
    wings_dl:map(fun(D, _) -> D#dlo{proxy_data=none} end, []),
    wings_image:delete_older(Limit),
    St#st{saved=false}.

clean_new_images(#st{saved=Limit}) when is_integer(Limit) ->
    wings_image:delete_from(Limit).
