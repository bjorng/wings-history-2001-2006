%%
%%  wings_shape.erl --
%%
%%     Utilities for shape records.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_shape.erl,v 1.1 2001/08/20 07:33:40 bjorng Exp $
%%

-module(wings_shape).
-export([insert/3,update/2]).

-include("wings.hrl").
-import(lists, [reverse/1,reverse/2]).

insert(#shape{name=OldName}=Sh0, Suffix, #st{shapes=Shapes0,onext=Oid}=St) ->
    Name = new_name(OldName, Suffix, Oid),
    Sh = Sh0#shape{id=Oid,name=Name},
    Shapes = gb_trees:insert(Oid, Sh, Shapes0),
    St#st{shapes=Shapes,onext=Oid+1}.
    
new_name(OldName, Suffix, Id) ->
    Base = base(reverse(OldName)),
    Name = reverse(Base, "_" ++ Suffix ++ integer_to_list(Id)).

base(OldName) ->
    case base_1(OldName) of
	error -> OldName;
	Base -> Base
    end.

base_1([H|T]) when $0 =< H, H =< $9 -> base_1(T);
base_1("ypoc_"++Base) -> Base;			%"_copy"
base_1("tcartxe_"++Base) -> Base;		%"_extract"
base_1("pes_"++Base) -> Base;			%"_sep"
base_1("tuc_"++Base) -> Base;			%"_cut"
base_1(Base) -> error.

update(#shape{id=Id}=Sh, #st{shapes=Shapes0}=St) ->
    Shapes = gb_trees:update(Id, Sh, Shapes0),
    St#st{shapes=Shapes}.

