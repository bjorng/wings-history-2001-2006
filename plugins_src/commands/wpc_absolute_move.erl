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
%%     $Id: wpc_absolute_move.erl,v 1.6 2006/02/09 14:28:25 giniu Exp $
%%
-module(wpc_absolute_move).

-include("wings.hrl").

-export([init/0,menu/2,command/2]).

init() ->
   true.

menu({Mode,move}, Menu) ->
   Menu ++ [separator,
            {?__(1,"Absolute"),absolute_fun(Mode),{draw_menu(Mode),[],
             ?__(2,"Move object to exact position using center of selection as center of objecet")},[]}];
menu(_, Menu) -> Menu.

draw_menu(vertex) ->
    ?__(1,"Move vertex/vertices to exact position in absolute coordinates.");
draw_menu(edge) ->
    ?__(2,"Move edges to exact position in absolute coordinates.");
draw_menu(face) ->
    ?__(3,"Move faces to exact position in absolute coordinates.");
draw_menu(body) ->
    ?__(4,"Move objects to exact position in absolute coordinates.").

absolute_fun(Mode) ->
    fun(1, _Ns) ->
	    {Mode,{move,absolute_l}};
       (3, _Ns) ->
	    {Mode,{move,absolute_r}};
       (_, _) -> ignore
    end.

command({_,{move,absolute_l}}, St) ->
   move_to(St,false);
command({_,{move,absolute_r}}, St) ->
   move_to(St,true);
command(_Cmd, _) -> next.

move_to(#st{selmode=SelMode,shapes=Shs}=St,ObjectOn) ->
   #st{sel=Sel} = VSt = case SelMode of
       vertex -> St;
       _ -> wings_sel_conv:mode(vertex,St)
   end,
   SingleVert = case Sel of
      [{_,{1,{_,_,_}}}] -> true;
      _ -> false
   end,
   BSt = wings_sel_conv:mode(body,VSt),
   VSt2 = wings_sel_conv:mode(vertex,BSt),
   WholeObject = case VSt2 of
      VSt -> true;
      _ -> false
   end,
   NoFlatten = SingleVert or WholeObject,
   Center = {XC,YC,ZC} = find_vert_center(Sel,Shs),
   wings_ask:dialog(
      ?__(1,"Absolute move options"), 
      [{hframe,
         if
            WholeObject ->
                  if
                     NoFlatten ->
                        [draw_options({XC,YC,ZC})];
                     true ->
                        [draw_options({XC,YC,ZC}),
                         draw_options(flatten)]
                  end;
            true ->
                  if
                     NoFlatten ->
                        [{vframe,
                         [draw_options({XC,YC,ZC}),
                          draw_options(ObjectOn)]}];
                     true ->
                        [{vframe,
                         [draw_options({XC,YC,ZC}),
                          draw_options(ObjectOn)]},
                         draw_options(flatten)]
                  end
         end}],
      fun(Move) ->
         move_to1(Move,Center,VSt,VSt2,St)
      end).

draw_options({XC,YC,ZC}) ->
   {vframe,[
    {hframe,[{label,?__(1,"Set position") ++ ":"}]},
    {hframe,[{label,"X:"},{text,XC}]},
    {hframe,[{label,"Y:"},{text,YC}]},
    {hframe,[{label,"Z:"},{text,ZC}]}
   ]};
draw_options(flatten) ->
   {vframe,[
    {hframe,[{label,?__(2,"Flatten") ++ ":"}]},
    {hframe,[{"",false,[{key,x}]}]},
    {hframe,[{"",false,[{key,y}]}]},
    {hframe,[{"",false,[{key,z}]}]}
   ]};
draw_options(ObjectOn) when is_boolean(ObjectOn) ->
   {hframe,[
    {?__(3,"Move objects"),ObjectOn,[{key,all}]}
   ]}.

move_to1([X,Y,Z],Center,VSt,VSt2,St) -> 
   move_to1([X,Y,Z,{x,false},{y,false},{z,false},{all,false}],Center,VSt,VSt2,St);
move_to1([X,Y,Z,{x,PX},{y,PY},{z,PZ}],Center,VSt,VSt2,St) -> 
   move_to1([X,Y,Z,{x,PX},{y,PY},{z,PZ},{all,false}],Center,VSt,VSt2,St);
move_to1([X,Y,Z,{all,All}],Center,VSt,VSt2,St) -> 
   move_to1([X,Y,Z,{x,false},{y,false},{z,false},{all,All}],Center,VSt,VSt2,St);
move_to1([X,Y,Z,{x,PX},{y,PY},{z,PZ},{all,false}],Center,VSt,_,St) -> 
   move_to2([X,Y,Z,{x,PX},{y,PY},{z,PZ}],Center,VSt,St);
move_to1([X,Y,Z,{x,PX},{y,PY},{z,PZ},{all,true}],Center,VSt,VSt2,St) -> 
   St2 = move_to2([X,Y,Z,{x,false},{y,false},{z,false}],Center,VSt2,St),
   move_to2([X,Y,Z,{x,PX},{y,PY},{z,PZ}],{X,Y,Z},VSt,St2).

move_to2([X,Y,Z,{x,PX},{y,PY},{z,PZ}],Center,#st{sel=Sel},#st{shapes=Shapes}=St) -> 
   NewShapes = do_move({X,Y,Z},{PX,PY,PZ},Center,Sel,Shapes),
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
