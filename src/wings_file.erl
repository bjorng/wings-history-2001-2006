%%
%%  wings_file.erl --
%%
%%     This module contains the commands in the File menu.
%%
%%  Copyright (c) 2001-2000 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_file.erl,v 1.94 2002/12/26 09:47:08 bjorng Exp $
%%

-module(wings_file).
-export([init/0,finish/0,menu/1,command/2,
	 export/3,export_filename/2,
	 import/3,import_filename/1]).

-include("e3d.hrl").
-include("wings.hrl").
-include_lib("kernel/include/file.hrl").

-import(lists, [sort/1,reverse/1,flatten/1,foldl/3,keymember/3,keydelete/3]).
-import(filename, [dirname/1]).

-define(WINGS,    ".wings").
-define(AUTOSAVE, "_as").
-define(BACKUP,   "_bup").

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
    end,
    set_autosave_timer().

finish() ->
    ok.

menu(_) ->
    ImpFormats = [{"Nendo (.ndo)...",ndo}],
    ExpFormats = [{"Nendo (.ndo)...",ndo},
		  {"ExtremeUV [Experimental] (.xndo)...",xndo}],
    [{"New",new},
     {"Open...",open},
     {"Merge...",merge},
     separator,
     {"Save",save},
     {"Save As...",save_as},
     {"Save Selected...",save_selected},
     {"Save Incrementally",save_incr},
     separator,
     {"Revert",revert},
     separator,
     {"Import",{import,ImpFormats}},
     {"Export",{export,ExpFormats}},
     {"Export Selected",{export_selected,ExpFormats}},
     separator,
     {"Render",{render,[]}},
     separator|recent_files([{"Exit",quit}])].

command(new, St0) ->
    case new(St0) of
	aborted -> St0;
	St0 -> St0;
	St -> {new,St}
    end;
command(open, St0) ->
    case read(St0) of
	St0 -> St0;
	St -> {new,St}
    end;
command(merge, St0) ->
    case merge(St0) of
	St0 -> St0;
	St -> {save_state,St}
    end;
command(save, St) ->
    save(St);
command(save_as, St0) ->
    case save_as(St0) of
	aborted -> St0;
	#st{}=St -> {saved,St}
    end;
command(save_selected, St) ->
    save_selected(St),
    St;
command(save_incr, St0) -> 
    case save_incr(St0) of
	aborted -> St0;
	#st{}=St -> {saved,St}
    end;
command(autosave, St) ->
    autosave(St);
command(revert, St0) ->
    case revert(St0) of
	{error,Reason} ->
	    wings_util:error("Revert failed: " ++ Reason),
	    St0;
	#st{}=St -> {save_state,St}
    end;
command({import,ndo}, St0) ->
    case import_ndo(St0) of
	{warning,Warn,St} ->
	    wings_util:message(Warn, St),
	    {save_state,St};
	St -> {save_state,St}
    end;
command({export,ndo}, St) ->
    export_ndo(St),
    St;
command({export_selected,ndo}, St) ->
    Shs0 = wings_sel:fold(fun(_, #we{id=Id}=We, A) ->
				  [{Id,We}|A]
			  end, [], St),
    Shs = gb_trees:from_orddict(reverse(Shs0)),
    export_ndo(St#st{shapes=Shs}),
    St;
command({export,xndo}, St) ->
    export_xndo(St),
    St;
command({export_selected,xndo}, St) ->
    Shs0 = wings_sel:fold(fun(_, #we{id=Id}=We, A) ->
				  [{Id,We}|A]
			  end, [], St),
    Shs = gb_trees:from_orddict(reverse(Shs0)),
    export_xndo(St#st{shapes=Shs}),
    St;
command(quit, St) ->
    quit(St);
command(Key, St) when is_integer(Key) ->
    Recent = wings_pref:get_value(recent_files, []),
    {_,File} = lists:nth(Key, Recent),
    {new,named_open(File, St)}.

quit(#st{saved=true}) -> quit;
quit(St) ->
    case wings_util:yes_no("Do you want to save your changes before "
			   "quitting?") of
	no -> quit;
	yes ->
	    case save(St) of
		aborted -> St;
		_Other -> quit
	    end;
	aborted -> St
    end.

new(#st{saved=true}=St0) ->
    DefMat = wings_material:default(),
    Empty = gb_trees:empty(),
    St = St0#st{file=undefined,shapes=Empty,mat=DefMat,sel=[],ssels=Empty},
    wings:caption(St),
    wings_material:init(St);
new(St0) -> %% File is not saved or autosaved.
    wings:caption(St0#st{saved=false}), 
    case wings_util:yes_no("Do you want to save your changes?") of
	no -> %% Remove autosaved file, user has explicitly said so.
	    catch file:delete(autosave_filename(St0#st.file)),
	    new(St0#st{saved=true});
	yes ->
	    case save(St0) of
		aborted -> aborted;
		{saved,St} -> new(St);
		#st{}=St -> new(St)
	    end;
	aborted -> aborted
    end.

read(St0) ->
    case new(St0) of
	aborted -> St0;
	St1 ->
	    case wings_plugin:call_ui({file,open_dialog,wings_prop()}) of
		aborted ->
		    wings_material:init(St0),
		    St0;
		Name ->
		    set_cwd(dirname(Name)),
		    File = use_autosave(Name),
		    case ?SLOW(wings_ff_wings:import(File, St1)) of
			#st{}=St ->
			    add_recent(Name),
			    wings:caption(St#st{saved=true,file=Name});
			{error,Reason} ->
			    wings_util:error("Read failed: " ++ Reason)
		    end
	    end
    end.

named_open(Name, St0) ->
    case new(St0) of
	aborted -> St0;
	St1 ->
	    add_recent(Name),
	    File = use_autosave(Name),
	    case ?SLOW(wings_ff_wings:import(File, St1)) of
		#st{}=St ->
		    set_cwd(dirname(Name)),
		    wings:caption(St#st{saved=true,file=Name});
		{error,Reason} ->
		    wings_util:error("Read failed: " ++ Reason),
		    St0
	    end
    end.

merge(St0) ->
    case wings_plugin:call_ui({file,open_dialog,[{title,"Merge"}|wings_prop()]}) of
	aborted -> St0;
	Name ->
	    File = use_autosave(Name),
	    case ?SLOW(wings_ff_wings:import(File, St0)) of
		{error,Reason} ->
		    wings_util:error("Read failed: " ++ Reason),
		    St0;
		#st{}=St -> St
	    end
    end.

save(St0) ->
    case save_1(St0) of
	aborted -> St0;
	St0 -> St0;
	St  -> {saved,St}
    end.

save_1(#st{saved=true}=St) -> St;
save_1(#st{file=undefined}=St) ->
    save_as(St);
save_1(#st{file=Name}=St) ->
    Backup = backup_filename(Name),
    file:rename(Name, Backup),
    file:delete(autosave_filename(Name)),
    case ?SLOW(wings_ff_wings:export(Name, St)) of
	ok ->
	    set_cwd(dirname(Name)),
	    wings:caption(St#st{saved=true});
	{error,Reason} ->
	    wings_util:error("Save failed: " ++ Reason),
	    aborted
    end.

save_as(St) ->
    case output_file("Save", wings_prop()) of
	false -> St;
	aborted -> aborted;
	Name ->
	    add_recent(Name),
	    case ?SLOW(wings_ff_wings:export(Name, St)) of
		ok ->
		    wings:caption(St#st{saved=true,file=Name});
		{error,Reason} ->
		    wings_util:error("Save failed: " ++ Reason),
		    aborted
	    end
    end.

save_selected(#st{sel=[]}) ->
    wings_util:error("This command requires a selection.");
save_selected(#st{shapes=Shs0,sel=Sel}=St0) ->
    case output_file("Save", wings_prop()) of
	aborted -> ok;
	Name when is_list(Name) ->
	    Shs = [Sh || {Id,_}=Sh <- gb_trees:to_list(Shs0),
			 keymember(Id, 1, Sel)],
	    St = St0#st{shapes=gb_trees:from_orddict(Shs)},
	    case ?SLOW(wings_ff_wings:export(Name, St)) of
		ok -> ok;
		{error,Reason} ->
		    wings_util:error("Save failed: " ++ Reason)
	    end
    end.

%%%
%%% Save incrementally. Original code submitted by Clacos.
%%%

save_incr(#st{saved=true}=St) -> St;
save_incr(#st{file=undefined}=St0) ->
    save_as(St0);
save_incr(#st{file=Name0}=St) -> 
    Name = increment_name(Name0),
    save_1(St#st{file=Name}).

increment_name(Name0) ->
    Name = filename:rootname(Name0),
    incr(reverse(Name)).

incr(Name0) -> 
    case find_digits(Name0)  of
	{[],Base} ->
	    Base ++ "_01.wings";
	{Digits0,Base} ->
	    Number = list_to_integer(Digits0) + 1,
	    Digits = integer_to_list(Number),
	    Base ++ lists:duplicate(length(Digits0)-length(Digits), $0) ++
		Digits ++ ".wings"
    end.

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

use_autosave(File) ->
    case file:read_file_info(File) of
	{ok, SaveInfo} ->
	    Auto = autosave_filename(File),
	    case file:read_file_info(Auto) of
		{ok, AutoInfo} ->
		    SaveTime = calendar:datetime_to_gregorian_seconds(SaveInfo#file_info.mtime),
		    AutoTime = calendar:datetime_to_gregorian_seconds(AutoInfo#file_info.mtime),
		    if
			AutoTime > SaveTime ->
			    Msg = "An autosaved file with later time stamp exists, "
				"do you want to load the autosaved file instead?",
			    case wings_util:yes_no(Msg) of
				yes -> 
				    Auto;
				no -> 
				    File;
				aborted ->  %% ???
				    File
			    end;
			SaveTime >= AutoTime ->
			    File
		    end;
		{error, _} ->  %% No autosave file
		    File
	    end;
	{error, _} -> %% use autosave if exists 
	    Auto = autosave_filename(File),
	    case filelib:is_file(Auto) of
		true -> Auto;
		false -> File			%Let reader handle error.
	    end
    end.

set_cwd(Cwd) ->
    wings_pref:set_value(current_directory, Cwd).

set_autosave_timer() ->
    case wings_pref:get_value(autosave_time) of
	0 -> ok;
	N when is_number(N) ->
	    wings_io:set_timer(trunc(N*60000), {action,{file,autosave}})
    end.
    
autosave(#st{file=undefined} = St) -> 
    set_autosave_timer(),
    St;
autosave(#st{saved=true} = St) ->
    set_autosave_timer(),
    St;
autosave(#st{saved=auto} = St) ->
    set_autosave_timer(),
    St;
autosave(#st{file=Name}=St) ->
    Auto = autosave_filename(Name),
    %% Maybe this should be spawned to another process
    %% to let the autosaving be done in the background.
    %% But I don't want to copy a really big model either.
    case ?SLOW(wings_ff_wings:export(Auto, St)) of
	ok ->
	    set_autosave_timer(),
	    wings:caption(St#st{saved=auto});
	{error,Reason} ->
	    set_autosave_timer(),
	    wings_util:error("AutoSave failed: " ++ Reason),
            St
    end.

autosave_filename(File) ->
    File ++ ?AUTOSAVE.

backup_filename(File) ->
    File ++ ?BACKUP.

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

add_recent(File, [A,B,C|_]) -> [File,A,B,C];
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
    DefMat = wings_material:default(),
    St1 = wings_material:init(St0#st{shapes=gb_trees:empty(),sel=[],mat=DefMat}),
    case ?SLOW(wings_ff_wings:import(File, St1)) of
	#st{}=St -> St;
	{error,_}=Error ->
	    wings_material:init(St0),
	    Error
    end.

%%
%% Import.
%%

get_import_filename(Ps0) ->
    Ps = Ps0 ++ [{title,"Import"}],
    wings_plugin:call_ui({file,open_dialog,Ps}).

import_filename(Ps) ->
    case get_import_filename(Ps) of
	aborted -> aborted;
	Name ->
	    set_cwd(dirname(Name)),
	    Name
    end.

import(Ps, Importer, St0) ->
    case get_import_filename(Ps) of
	aborted -> St0;
	Name ->
	    set_cwd(dirname(Name)),
	    case ?SLOW(do_import(Importer, Name, St0)) of
		#st{}=St -> St;
		{warning,Warn,St} ->
		    wings_util:message(Warn, St),
		    St;
	    	{error,Reason} ->
		    wings_util:error("Import failed: " ++ Reason)
	    end
    end.

import_ndo(St0) ->
    Ps = [{ext,".ndo"},{ext_desc,"Nendo File"}],
    case get_import_filename(Ps) of
	aborted -> St0;
	Name ->
	    case ?SLOW(wings_ff_ndo:import(Name, St0)) of
		#st{}=St ->
		    set_cwd(dirname(Name)),
		    St;
	    	{error,Reason} ->
		    wings_util:error("Import failed: " ++ Reason),
		    St0
	    end
    end.

%%
%% Export.
%%

export_filename(Prop, St) ->
    case output_file("Export", export_file_prop(Prop, St)) of
	aborted -> aborted;
	Name ->
	    set_cwd(dirname(Name)),
	    Name
    end.

export(Props0, Exporter, St) ->
    SubDivs = proplists:get_value(subdivisions, Props0, 0),
    case export_file_prop(Props0, St) of
	none ->
	    ?SLOW(do_export(Exporter, none, SubDivs, St));
	Props ->
	    case output_file("Export", Props) of
		aborted -> ok;
		Name -> ?SLOW(do_export(Exporter, Name, SubDivs, St))
	    end
    end.

export_ndo(St) ->
    Prop = [{ext,".ndo"},{ext_desc,"Nendo File"}],
    case output_file("Export", export_file_prop(Prop, St)) of
	aborted -> St;
	Name -> wings_ff_ndo:export(Name, St)
    end.

export_xndo(St0) ->
    St = xndo_rewrite(St0),
    Prop = [{ext,".xndo"},{ext_desc,"ExtremeUV Nendo File"}],
    case output_file("Export", export_file_prop(Prop, St)) of
	aborted -> St;
	Name -> wings_ff_ndo:export(Name, St)
    end.

xndo_rewrite(#st{mat=Mat,shapes=Shs0}=St) ->
    MatTab = number_materials(gb_trees:keys(Mat), 0, []),
    Shs = xndo_rewrite_1(gb_trees:to_list(Shs0), MatTab),
    St#st{shapes=gb_trees:from_orddict(Shs)}.

xndo_rewrite_1([{Id,We0}|Shs], MatTab) ->
    #we{fs=Ftab0} = We = wings_we:renumber(We0, 0),
    Ftab = xndo_rewrite_ftab(gb_trees:to_list(Ftab0), MatTab, []),
    [{Id,We#we{fs=Ftab}}|xndo_rewrite_1(Shs, MatTab)];
xndo_rewrite_1([], _) -> [].

xndo_rewrite_ftab([{Face,#face{mat=M}=Rec}|T], MatTab, Acc0) ->
    Acc = [{Face,Rec#face{edge=gb_trees:get(M, MatTab)}}|Acc0],
    xndo_rewrite_ftab(T, MatTab, Acc);
xndo_rewrite_ftab([], _, Acc) ->
    gb_trees:from_orddict(reverse(Acc)).

number_materials([M|Ms], I, Acc) ->
    number_materials(Ms, I+1, [{M,I}|Acc]);
number_materials([], _, Acc) ->
    gb_trees:from_orddict(reverse(Acc)).
    
%%% Utilities.

export_file_prop(none, _) -> none;
export_file_prop(Prop, #st{file=undefined}) -> Prop;
export_file_prop(Prop, #st{file=File}) ->
    Ext = proplists:get_value(ext, Prop),
    Def = filename:rootname(filename:basename(File), ?WINGS) ++ Ext,
    [{default_filename,Def}|Prop].

output_file(Title, Prop) ->
    case wings_plugin:call_ui({file,save_dialog,[{title,Title}|Prop]}) of
	aborted -> aborted;
	Name ->
	    set_cwd(dirname(Name)),
	    Name
    end.

%%%
%%% Generic import code.
%%%

do_import(Importer, Name, St0) ->
    case Importer(Name) of
	{ok,#e3d_file{objs=Objs,mat=Mat}} ->
	    NumObjs = length(Objs),
	    Suffix = " of " ++ integer_to_list(NumObjs),
	    {UsedMat,St1} = translate_objects(Objs, gb_sets:empty(),
					     1, Suffix, St0),
	    {St2,NameMap} = wings_import:add_materials(UsedMat, Mat, St1),
	    St = rename_materials(NameMap, St0, St2),
	    case gb_trees:size(St#st.shapes)-gb_trees:size(St0#st.shapes) of
		NumObjs -> St;
		N ->
		    Warn = integer_to_list(NumObjs-N) ++
			" object(s) out of " ++
			integer_to_list(NumObjs) ++
			" object(s) could not be converted.",
		    {warning,Warn,St}
	    end;
	{error,Reason} ->
	    wings_util:error(Reason)
    end.

translate_objects([#e3d_object{name=Name}=Obj|Os], UsedMat0,
		  I, Suffix, St0) ->
    {St,UsedMat} = case wings_import:import(Obj, UsedMat0) of
		       error -> {St0,UsedMat0};
		       {We,Us0} -> {store_object(Name, We, St0),Us0}
		   end,
    translate_objects(Os, UsedMat, I+1, Suffix, St);
translate_objects([], UsedMat, _, _, St) -> {UsedMat,St}.

store_object(undefined, We, #st{onext=Oid}=St) ->
    Name = "unnamed_object" ++ integer_to_list(Oid),
    wings_shape:new(Name, We, St);
store_object(Name, We, St) ->
    wings_shape:new(Name, We, St).

rename_materials([], _, St) -> St;
rename_materials(NameMap0, #st{onext=FirstId}, #st{shapes=Shs0}=St) ->
    NameMap = gb_trees:from_orddict(sort(NameMap0)),
    Shs = rename_mat(gb_trees:to_list(Shs0), NameMap, FirstId, []),
    St#st{shapes=Shs}.

rename_mat([{Id,_}=Obj|Objs], NameMap, FirstId, Acc) when Id < FirstId ->
    rename_mat(Objs, NameMap, FirstId, [Obj|Acc]);
rename_mat([{Id,We0}|Objs], NameMap, FirstId, Acc) ->
    We = wings_import:rename_materials(NameMap, We0),
    rename_mat(Objs, NameMap, FirstId, [{Id,We}|Acc]);
rename_mat([], _, _, Acc) -> gb_trees:from_orddict(reverse(Acc)).

%%%
%%% Generic export code.
%%%

do_export(Exporter, Name, SubDivs, #st{shapes=Shs}=St) ->
    Objs = foldl(fun(W, A) ->
			 do_export(W, SubDivs, A)
		 end, [], gb_trees:values(Shs)),
    Creator = "Wings 3D " ++ ?WINGS_VERSION,
    Mat0 = wings_material:used_materials(St),
    Mat = keydelete('_hole_', 1, Mat0),
    Contents = #e3d_file{objs=Objs,mat=Mat,creator=Creator},
    case Exporter(Name, Contents) of
	ok -> ok;
	{error,Atom} when is_atom(Atom) ->
	    wings_util:error("Failed to export: " ++ file:format_error(Atom));
	{error,Reason} ->
	    wings_util:error(Reason)
    end.

do_export(#we{perm=Perm}, _, Acc) when ?IS_NOT_VISIBLE(Perm) -> Acc;
do_export(#we{name=Name,light=none}=We, SubDivs, Acc) ->
    Mesh = make_mesh(We, SubDivs),
    [#e3d_object{name=Name,obj=Mesh}|Acc];
do_export(_, _, Acc) -> Acc.

make_mesh(We0, SubDivs) ->
    We1 = sub_divide(SubDivs, We0),
    #we{fs=Ftab,vp=Vs0,es=Etab,he=He0} = We = wings_we:renumber(We1, 0),
    Vs = gb_trees:values(Vs0),
    {ColTab0,UvTab0} = make_tables(We),
    ColTab1 = gb_trees:from_orddict(ColTab0),
    UvTab1 = gb_trees:from_orddict(UvTab0),
    Fs0 = foldl(fun({_,#face{mat='_hole_'}}, A) ->
			A;
		   ({Face,#face{mat=Mat}}, A) ->
			[make_face(Face, Mat, ColTab1, UvTab1, We)|A]
		end, [], gb_trees:to_list(Ftab)),
    Fs = reverse(Fs0),
    He = hard_edges(gb_sets:to_list(He0), Etab, []),
    Matrix = e3d_mat:identity(),
    ColTab = strip_numbers(ColTab0),
    UvTab = strip_numbers(UvTab0),
    Mesh = #e3d_mesh{type=polygon,fs=Fs,vs=Vs,tx=UvTab,he=He,
		     vc=ColTab,matrix=Matrix},
    e3d_mesh:renumber(Mesh).

sub_divide(0, We) -> We;
sub_divide(N, We0) ->
    We = wings_subdiv:smooth(We0),
    sub_divide(N-1, We).

make_face(Face, Mat, _ColTab, UvTab, #we{mode=uv}=We) ->
    {Vs,UVs} = wings_face:fold_vinfo(
		 fun(V, {_,_}=UV, {VAcc,UVAcc}) ->
			 {[V|VAcc],[gb_trees:get(UV, UvTab)|UVAcc]};
		    (V, _, {VAcc,UVAcc}) ->
			 {[V|VAcc],UVAcc}
		 end, {[],[]}, Face, We),
    #e3d_face{vs=Vs,tx=UVs,mat=make_face_mat(Mat)};
make_face(Face, Mat, ColTab, _UvTab, #we{mode=vertex}=We) ->
    {Vs,Cols} = wings_face:fold_vinfo(
		  fun(V, {_,_,_}=Col, {VAcc,ColAcc}) ->
			  {[V|VAcc],[gb_trees:get(Col, ColTab)|ColAcc]};
		     (V, _Info, {VAcc,ColAcc}) ->
			  {[V|VAcc],ColAcc}
		  end, {[],[]}, Face, We),
    #e3d_face{vs=Vs,vc=Cols,mat=make_face_mat(Mat)};
make_face(Face, Mat, _, _, We) ->
    Vs = wings_face:surrounding_vertices(Face, We),
    #e3d_face{vs=Vs,mat=make_face_mat(Mat)}.

make_tables(#we{mode=vertex}=We) ->
    {make_table(We),[]};
make_tables(#we{mode=uv}=We) ->
    {[],make_table(We)};
make_tables(_) ->
    {[],[]}.

make_table(#we{es=Etab}) ->
    Cuvs = foldl(fun(#edge{a=A,b=B}, Acc) ->
			 [A,B|Acc]
		 end, [], gb_trees:values(Etab)),
    number(ordsets:from_list(Cuvs)).

number(L) ->
    number(L, 0, []).
number([H|T], I, Acc) ->
    number(T, I+1, [{H,I}|Acc]);
number([], _, Acc) -> reverse(Acc).

strip_numbers(L) ->
    strip_numbers(L, []).
strip_numbers([{H,_}|T], Acc) ->
    strip_numbers(T, [H|Acc]);
strip_numbers([], Acc) -> reverse(Acc).

make_face_mat([_|_]=Mat) -> Mat;
make_face_mat(Mat) -> [Mat].

hard_edges([E|Es], Etab, Acc) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
    hard_edges(Es, Etab, [hard(Va, Vb)|Acc]);
hard_edges([], _Etab, Acc) -> Acc.

hard(A, B) when A < B -> {A,B};
hard(A, B) -> {B,A}.

