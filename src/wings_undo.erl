%%
%%  wings_undo.erl --
%%
%%     This module handles the undo stack.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_undo.erl,v 1.1.1.1 2001/08/14 18:16:37 bjorng Exp $
%%

-module(wings_undo).
-export([new/1,save/2,undo_toggle/2,undo/2,redo/2]).
-import(lists, [reverse/1]).

-include("wings.hrl").

-record(undo,
	{max,					%Max levels of undo.
	 levels,				%Current number of levels.
	 top,					%Top of stack.
	 bottom,				%Bottom of stack.
	 next_is_undo,				%State of undo/redo toggle.
	 undone					%States that were undone.
	 }).

%% The essential part of the state record.
-record(est,
	{shapes,
	 selmode,
	 sel,
	 onext}).

new(MaxLevels) ->
    #undo{levels=0,max=MaxLevels,top=[],bottom=[],undone=[],next_is_undo=true}.

save(St, #undo{max=Max,levels=Levels}=Undo0) when Levels >= Max ->
    {_,Undo} = shift(Undo0),
    save(St, Undo);
save(St, Undo0) ->
    Undo = push(Undo0, St),
    Undo#undo{undone=[],next_is_undo=true}.

undo_toggle(St0, #undo{undone=Undone,next_is_undo=true}=Undo0) ->
    case pop(Undo0, St0) of
	empty ->
	    {St0,Undo0};
	{St,Undo} ->
	    {St,Undo#undo{undone=[St0|Undone],next_is_undo=false}}
    end;
undo_toggle(St0, #undo{undone=[StOld|Undone]}=Undo0) ->
    Undo = push(Undo0, St0),
    #st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext} = StOld,
    St = St0#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext},
    {St,Undo#undo{undone=Undone,next_is_undo=true}};
undo_toggle(St, Undo) -> {St,Undo}.

undo(St0, #undo{undone=Undone}=Undo0) ->
    case pop(Undo0, St0) of
	empty ->
	    {St0,Undo0};
	{St,Undo} ->
	    {St,Undo#undo{undone=[St0|Undone],next_is_undo=false}}
    end.

redo(St0, #undo{undone=[StOld|Undone]}=Undo0) ->
    Undo = push(Undo0, St0),
    #st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext} = StOld,
    St = St0#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext},
    {St,Undo#undo{undone=Undone,next_is_undo=true}};
redo(St, Undo) -> {St,Undo}.
    
%%
%% Low-level queue operations. Similar to Perl's array operators.
%%

push(#undo{}=Undo, #st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext}) ->
    Est = #est{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext},
    push(Undo, Est);
push(#undo{top=[],bottom=[_|_]=Bottom}=Undo, #est{}=Est) ->
    push(Undo#undo{top=reverse(Bottom),bottom=[]}, Est);
push(#undo{top=[],bottom=[],levels=Levels}=Undo, #est{}=Est) ->
    Undo#undo{top=[Est],levels=Levels+1};
push(#undo{top=[PrevEst|PrevTop]=Top,levels=Levels}=Undo, #est{}=Est) ->
    case compare_states(PrevEst, Est) of
	new ->
	    Undo#undo{top=[Est|Top],levels=Levels+1};
	new_sel ->
	    #est{sel=Sel} = Est,
	    Undo#undo{top=[PrevEst#est{sel=Sel}|PrevTop]}
    end.

compare_states(Old, New) ->
    #est{shapes=Osh,selmode=Omode,sel=Osel,onext=Oonext} = Old,
    #est{shapes=Nsh,selmode=Nmode,sel=Nsel,onext=Nonext} = New,
    if
	Omode =/= Nmode -> new;
	Oonext =/= Nonext -> new;
	Osel =/= Nsel ->
	    if
		Osh =:= Nsh -> new_sel;
		true -> new
	    end;
	true -> new
    end.

pop(#undo{top=[Est|Top],levels=Levels}=Undo, St0) ->
    #est{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext} = Est,
    St = St0#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext},
    {St,Undo#undo{top=Top,levels=Levels-1}};
pop(#undo{top=[],bottom=[_|_]=Bottom}=Undo, St) ->
    pop(Undo#undo{top=reverse(Bottom),bottom=[]}, St);
pop(_, St) -> empty.

shift(#undo{bottom=[St|Bottom],levels=Levels}=Undo) ->
    {St,Undo#undo{bottom=Bottom,levels=Levels-1}};
shift(#undo{bottom=[],top=[_|_]=Top}=Undo) ->
    shift(Undo#undo{bottom=reverse(Top),top=[]});
shift(Undo) -> empty.
