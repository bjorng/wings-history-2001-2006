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
%%     $Id: wings_undo.erl,v 1.3 2001/11/08 14:01:09 bjorng Exp $
%%

-module(wings_undo).
-export([init/1,save/2,undo_toggle/1,undo/1,redo/1]).
-import(lists, [reverse/1]).

-include("wings.hrl").

%% The essential part of the state record.
-record(est,
	{shapes,
	 selmode,
	 sel,
	 onext
	}).

init(St) ->
    St#st{top=[],bottom=[],undone=[],next_is_undo=true}.
    
save(OldState, St0) ->
    St1 = discard_old_states(St0),
    St = push(St1, OldState),
    St#st{undone=[],next_is_undo=true}.

undo_toggle(#st{next_is_undo=true}=St) -> undo(St);
undo_toggle(St) -> redo(St).

undo(#st{undone=Undone}=St0) ->
    case pop(St0) of
	empty -> St0;
	St -> St#st{undone=[St0|Undone],next_is_undo=false}
    end.

redo(#st{undone=[StOld|Undone]}=St0) ->
    St1 = push(St0, St0),
    #st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext} = StOld,
    St = St1#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext},
    St#st{undone=Undone,next_is_undo=true};
redo(St) -> St.

%%
%% Low-level queue operations.
%%

push(St, OldState) ->
    Est = save_essential(OldState),
    push_1(St, Est).

push_1(#st{top=[],bottom=[_|_]=Bottom}=St, #est{}=Est) ->
    push_1(St#st{top=reverse(Bottom),bottom=[]}, Est);
push_1(#st{top=[],bottom=[]}=St, #est{}=Est) ->
    St#st{top=[Est]};
push_1(#st{top=[PrevEst|PrevTop]=Top}=St, #est{}=Est) ->
    case compare_states(PrevEst, Est) of
	new -> St#st{top=[Est|Top]};
	new_sel ->
	    #est{sel=Sel} = Est,
	    St#st{top=[PrevEst#est{sel=Sel}|PrevTop]}
    end.

save_essential(#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext}) ->
    #est{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext}.
    
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

pop(#st{top=[Est|Top]}=St0) ->
    #est{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext} = Est,
    St = St0#st{shapes=Sh,selmode=Mode,sel=Sel,onext=Onext},
    St#st{top=Top};
pop(#st{top=[],bottom=[_|_]=Bottom}=St) ->
    pop(St#st{top=reverse(Bottom),bottom=[]});
pop(St) -> empty.

discard_old_states(#st{top=Top,bottom=Bot,undone=Undone}=St)
  when length(Top) + length(Bot) > ?UNDO_LEVELS ->
    discard_old_state(St);
discard_old_states(St) -> St.
    
discard_old_state(#st{bottom=[_|Bottom]}=St) ->
    St#st{bottom=Bottom};
discard_old_state(#st{bottom=[],top=[_|_]=Top}=St) ->
    discard_old_state(St#st{bottom=reverse(Top),top=[]}).
