%%
%%  wings_s.erl --
%%
%%     Common text strings.
%%
%%  Copyright (c) 2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_s.erl,v 1.2 2004/10/15 06:14:23 bjorng Exp $
%%

-module(wings_s).
-export([yes/0,no/0,cancel/0,
	 lmb/0,mmb/0,rmb/0,
	 modkey/1,shift/0,ctrl/0,alt/0,command/0,
	 key/1]).

-include("wings.hrl").

yes() -> ?STR(yes,1,"Yes").
no() -> ?STR(no,1,"No").
cancel() -> ?STR(cancel,1,"Cancel").

%% Mouse buttons.
lmb() -> ?STR(lmb,1,"L").
mmb() -> ?STR(mmb,1,"M").
rmb() -> ?STR(rmb,1,"R").

%% Modifier keys.
modkey(shift) -> shift();
modkey(ctrl) -> ctrl();
modkey(alt) -> alt();
modkey(command) -> command().
    
shift() -> ?STR(shift,1,"Shift").
ctrl() -> ?STR(ctrl,1,"Ctrl").
alt() -> ?STR(alt,1,"Alt").
command() -> ?STR(command,1,"Command").		%Command key on Mac.
    
%% Returns key name within square brackets.
key(Key) -> [$[,key_1(Key),$]].

key_1(shift) -> shift();
key_1(ctrl) -> ctrl();
key_1(alt) -> alt();
key_1(command) -> command();
key_1(Key) when is_atom(Key) -> atom_to_list(Key);
key_1(Key) when is_list(Key) -> Key.
