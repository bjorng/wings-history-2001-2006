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
%%     $Id: wings_s.erl,v 1.1 2004/10/14 10:22:08 bjorng Exp $
%%

-module(wings_s).
-export([yes/0,no/0,cancel/0,
	 lmb/0,mmb/0,rmb/0,
	 shift/0,ctrl/0,alt/0,command/0]).

-include("wings.hrl").

yes() -> ?STR(yes,1,"Yes").
no() -> ?STR(no,1,"No").
cancel() -> ?STR(cancel,1,"Cancel").

%% Mouse buttons.
lmb() -> ?STR(lmb,1,"L").
mmb() -> ?STR(mmb,1,"M").
rmb() -> ?STR(rmb,1,"R").

%% Modifier keys.
shift() -> ?STR(shift,1,"Shift").
ctrl() -> ?STR(ctrl,1,"Ctrl").
alt() -> ?STR(alt,1,"Alt").
command() -> ?STR(command,1,"Command").		%Command key on Mac.
    
