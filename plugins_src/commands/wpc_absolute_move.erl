%%
%%  wpc_absolute_move.erl --
%%
%%     Plug-in for absolute positioning
%%
%%  Copyright (c) 2006 Andrzej Giniewicz
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_absolute_move.erl,v 1.2 2006/02/07 12:42:50 giniu Exp $
%%
-module(wpc_absolute_move).

-include_lib("wings.hrl").

-export([init/0,menu/2,command/2]).

init() ->
   true.

menu({vertex,move}, Menu) ->
   Menu ++ [separator,
            {?__(1,"Absolute"),move_to,
             ?__(2,"Move vertex/vertices to exact position in absolute coordinates.")}];
menu({edge,move}, Menu) ->
   Menu ++ [separator,
            {?__(3,"Absolute"),move_to,
             ?__(4,"Move edges to exact position in absolute coordinates.")}];
menu({face,move}, Menu) ->
   Menu ++ [separator,
            {?__(5,"Absolute"),move_to,
             ?__(6,"Move faces to exact position in absolute coordinates.")}];
menu({body,move}, Menu) ->
   Menu ++ [separator,
            {?__(7,"Absolute"),move_to,
             ?__(8,"Move objects to exact position in absolute coordinates.")}];
menu(_, Menu) -> Menu.

command({_,{move,move_to}}, St) ->
   move_to(St);
command(_Cmd, _) -> next.

move_to(#st{selmode=vertex,sel=[{Obj,{1,{Vert,_,_}}}],shapes=Shs}=St) ->
   We = gb_trees:get(Obj, Shs),
   Vtab = We#we.vp,
   {X,Y,Z}=gb_trees:get(Vert, Vtab),
   wings_ask:dialog(
      ?__(1,"Numeric Input"), 
      [{hframe,[{label,"X"},{text,X}]},
       {hframe,[{label,"Y"},{text,Y}]},
       {hframe,[{label,"Z"},{text,Z}]}],
      fun(Move) ->
         move_to1(Move,St)
      end);
move_to(#st{selmode=SelMode,shapes=Shs}=St) ->
   VSt = case SelMode of
       vertex -> St;
       _ -> wings_sel_conv:mode(vertex,St)
   end,
   #st{sel=Sel} = VSt,
   Center = {XC,YC,ZC} = find_vert_center(Sel,Shs),
   wings_ask:dialog(
      ?__(2,"Numeric Input"), 
      [{hframe,
       [{vframe,
        [{hframe,[{label,?__(3,"Set position:")}]},
         {hframe,[{label,"X"},{text,XC}]},
         {hframe,[{label,"Y"},{text,YC}]},
         {hframe,[{label,"Z"},{text,ZC}]}]},
       {vframe,
        [{hframe,[{label,?__(4,"Flatten:")}]},
         {hframe,[{"",false,[{key,x}]}]},
         {hframe,[{"",false,[{key,y}]}]},
         {hframe,[{"",false,[{key,z}]}]}]}]}],
      fun(Move) ->
         move_to2(Move,Center,VSt,St)
      end).

move_to1([X,Y,Z],#st{sel=[{Obj,{1,{Vert,_,_}}}],shapes=Shs}=St) ->
   We = gb_trees:get(Obj, Shs),
   Vtab = We#we.vp,
   NewVtab = gb_trees:update(Vert,{X,Y,Z},Vtab),
   NewWe = We#we{vp=NewVtab},
   NewShs = gb_trees:update(Obj,NewWe,Shs),
   St#st{shapes=NewShs}.

move_to2([XN,YN,ZN,{x,PX},{y,PY},{z,PZ}],Center,#st{sel=Sel,shapes=Shapes},St) -> 
   NewShapes = do_move({XN,YN,ZN},{PX,PY,PZ},Center,Sel,Shapes),
   St#st{shapes=NewShapes}.

do_move(_,_,_,[],Shapes) ->
   Shapes;
do_move({XN,YN,ZN},{PX,PY,PZ},{XO,YO,ZO},[{Obj,Vset}|Rest],Shapes) ->
   We = gb_trees:get(Obj, Shapes),
   Vtab = We#we.vp,
   NewVtab = do_move2({XN,YN,ZN},{PX,PY,PZ},{XO,YO,ZO},Vset,Vtab),
   NewWe = We#we{vp=NewVtab},
   NewShapes = gb_trees:update(Obj,NewWe,Shapes),
   do_move({XN,YN,ZN},{PX,PY,PZ},{XO,YO,ZO},Rest,NewShapes).

do_move2({XN,YN,ZN},{PX,PY,PZ},{XO,YO,ZO},Vset,Vtab) ->
   case gb_sets:size(Vset) of
      0 -> Vtab;
      _ -> 
         {Vertex,Vset2} = gb_sets:take_smallest(Vset),
         {X,Y,Z} = gb_trees:get(Vertex, Vtab),
         X1 = case PX of
            true -> XN;
            _ -> X+XN-XO
         end,
         Y1 = case PY of
            true -> YN;
            _ -> Y+YN-YO
         end,
         Z1 = case PZ of
            true -> ZN;
            _ -> Z+ZN-ZO
         end,
         NewVtab = gb_trees:update(Vertex,{X1,Y1,Z1},Vtab),
         do_move2({XN,YN,ZN},{PX,PY,PZ},{XO,YO,ZO},Vset2,NewVtab)
     end.

find_vert_center(Sel,Shapes) ->
   find_vert_center(Sel,Shapes,{0,0,0},0).

find_vert_center([],_,_,0) ->
   none;
find_vert_center([],_,{X,Y,Z},N)->
   {X/N,Y/N,Z/N};
find_vert_center([{Obj,Vset}|Rest],Shapes,{X0,Y0,Z0},N0) ->
   We = gb_trees:get(Obj, Shapes),
   Vtab = We#we.vp,
   {{X,Y,Z},N} = find_center(Vset,Vtab,{X0,Y0,Z0},N0),
   find_vert_center(Rest,Shapes,{X,Y,Z},N).

find_center(Vset,Vtab,{X0,Y0,Z0},N) ->
   case gb_sets:size(Vset) of
      0 -> {{X0,Y0,Z0},N};
      _ -> 
         {Vertex,Vset2} = gb_sets:take_smallest(Vset),
         {X,Y,Z} = gb_trees:get(Vertex, Vtab),
         find_center(Vset2,Vtab,{X0+X,Y0+Y,Z0+Z},N+1)
     end.
