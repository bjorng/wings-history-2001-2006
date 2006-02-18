%%
%%  wpc_absolute_move.erl --
%%
%%     Plug-in for move -> absolute
%%
%%  Copyright (c) 2006 Andrzej Giniewicz
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_absolute_move.erl,v 1.9 2006/02/18 00:16:23 giniu Exp $
%%
-module(wpc_absolute_move).

-include("wings.hrl").

-export([init/0,menu/2,command/2]).

%%%
%%% plugin interface
%%%

init() -> true.

menu({_,move},Menu) ->
    Menu ++ [separator,
             {?__(1,"Absolute"),abs_move,
              ?__(2,"Move to exact position in absolute coordinates.")}
            ];
menu(_,Menu) -> Menu.

command({_,{move,abs_move}},St) ->
    abs_move(St);
command(_,_) -> next.

abs_move(St) ->
    {DisplayOptions,Selection} = extract(St),
    draw_window(DisplayOptions,Selection,St).

%%
%% extract(State)
%%
%% functions that extracts all needed information from state
%%  it returns {Options,Selection}, where options is:
%%   {{move_obj,MoveO},{flatten,Flatten},{align,Align},{center,{CenterX,CenterY,CenterZ}}}
%%  (return values: {{atom,atom(no/one/many)},{atom,bool},{atom,bool},{atom,{float,float,float}}})
%%

extract(#st{shapes=Shapes}=St) ->
    Sel = get_selection(St),
    Center = get_center(Sel,Shapes),
    OneObject = check_single_obj(Sel),
    SinglePoints = check_single_vert(Sel),
    WholeObjects = if
                       SinglePoints ->
                           false;
                       true ->
                           check_whole_obj(St)
                   end,
    MoveObj = if
                  WholeObjects -> no;
                  OneObject -> one;
                  true -> many
              end,
    Flatten = if
                  SinglePoints or WholeObjects -> false;
                  true -> true
              end,
    Align = not OneObject,
    {{{move_obj,MoveObj},{flatten,Flatten},{align,Align},{center,Center}},Sel}.

get_selection(#st{selmode=SelMode}=St) ->
    #st{sel=Sel} = case SelMode of
        vertex -> St;
        _ -> wings_sel_conv:mode(vertex,St)
    end,
    Sel.

get_center(Sel,Shapes) ->
    get_center(Sel,Shapes,[]).

get_center([],_,Now) ->
    e3d_vec:average(Now);
get_center([{Obj,Vset}|Rest],Shapes,Now) ->
    We = gb_trees:get(Obj, Shapes),
    Positions = gb_sets:fold(fun(Vert, Acc) ->
                         [wings_vertex:pos(Vert, We)|Acc]
                      end, [], Vset),
    get_center(Rest,Shapes,Now++Positions).

check_single_obj([{_,_}]) -> true;
check_single_obj(_) -> false.

check_single_vert([]) -> true;
check_single_vert([{_,{1,_}}|Rest]) ->
    check_single_vert(Rest);
check_single_vert(_) -> false.

check_whole_obj(#st{selmode=SelMode}=St0) ->
    St1 = wings_sel_conv:mode(body,St0),
    St2 = wings_sel_conv:mode(SelMode,St1),
    St2 == St0.

%%
%% draw_window(Options,Selection,State)
%%
%% functions that draws interface and translates entered options for further processing
%%  and calls do_move(ProcessedOptions,Selection,State)
%%   

draw_window({{move_obj,MoveObj},{flatten,Flatten},{align,Align},{center,Center}},Sel,St) ->
    Frame1 = if
                 MoveObj =/= no ->
                     [{vframe,
                         [draw_window1(center,Center)] ++
                         [draw_window1(object,MoveObj)]}];
                 true ->
                     [draw_window1(center,Center)]
             end,
    Frame2 = if
                 Align ->
                     [draw_window1(align,default)];
                 true ->
                     []
             end,
    Frame3 = if
                 Flatten ->
                     [draw_window1(flatten,default)];
                 true ->
                     []
             end,
    Frame = [{hframe,Frame1++Frame2++Frame3}],
    Name = draw_window1(name,default),
    wings_ask:dialog(Name, Frame,
       fun(Move) ->
           translate(Move,Center,Sel,St)
       end).

draw_window1(name,_) ->
    ?__(1,"Absolute move options");
draw_window1(center,{XC,YC,ZC}) ->
    {vframe,[
        {hframe,[{label,?__(2,"Set position")++":"}]},
        {hframe,[{label,"X:"},{text,XC}]},
        {hframe,[{label,"Y:"},{text,YC}]},
        {hframe,[{label,"Z:"},{text,ZC}]}
    ]};
draw_window1(object,one) ->
    {hframe,[
        {?__(3,"Move object"),false,[{key,all}]}
    ]};
draw_window1(object,many) ->
    {hframe,[
        {?__(4,"Move objects"),false,[{key,all}]}
    ]};
draw_window1(align,_) ->
    {vframe,[
        {hframe,[{label,?__(5,"Align")++":"}]},
        {hframe,[{"",false,[{key,ax}]}]},
        {hframe,[{"",false,[{key,ay}]}]},
        {hframe,[{"",false,[{key,az}]}]}
    ]};
draw_window1(flatten,_) ->
    {vframe,[
        {hframe,[{label,?__(6,"Flatten")++":"}]},
        {hframe,[{"",false,[{key,fx}]}]},
        {hframe,[{"",false,[{key,fy}]}]},
        {hframe,[{"",false,[{key,fz}]}]}
    ]}.

translate([X,Y,Z],Center,Sel,St) ->
    do_move([Center,{X,Y,Z},true,{false,false,false},{false,false,false}],Sel,St);
translate([X,Y,Z,{all,Object}],Center,Sel,St) ->
    do_move([Center,{X,Y,Z},Object,{false,false,false},{false,false,false}],Sel,St);
translate([X,Y,Z,{all,Object},{fx,Fx},{fy,Fy},{fz,Fz}],Center,Sel,St) ->
    do_move([Center,{X,Y,Z},Object,{false,false,false},{Fx,Fy,Fz}],Sel,St);
translate([X,Y,Z,{ax,Ax},{ay,Ay},{az,Az}],Center,Sel,St) ->
    do_move([Center,{X,Y,Z},true,{Ax,Ay,Az},{false,false,false}],Sel,St);
translate([X,Y,Z,{all,Object},{ax,Ax},{ay,Ay},{az,Az}],Center,Sel,St) ->
    do_move([Center,{X,Y,Z},Object,{Ax,Ay,Az},{false,false,false}],Sel,St);
translate([X,Y,Z,{all,Object},{ax,Ax},{ay,Ay},{az,Az},{fx,Fx},{fy,Fy},{fz,Fz}],Center,Sel,St) ->
    do_move([Center,{X,Y,Z},Object,{Ax,Ay,Az},{Fx,Fy,Fz}],Sel,St).

%%
%% do_move(Options,Selection,State)
%%
%% this is main absolute move command, it returns new state.
%%

do_move(_,[],St) ->
    St;
do_move([{Cx,Cy,Cz},{X,Y,Z},Wo,{Ax,Ay,Az},{Fx,Fy,Fz}]=Move,[{Obj,Vset}|Rest],#st{shapes=Shapes}=St) ->
    Empty = gb_trees:empty(),
    We = gb_trees:get(Obj, Shapes),
    Vtab = We#we.vp,
    {Ox,Oy,Oz} = get_center([{Obj,Vset}],Shapes),
    Dx = if
             Ax -> X - Ox;
             true -> X - Cx
    end,
    Dy = if
             Ay -> Y - Oy;
             true -> Y - Cy
    end,
    Dz = if
             Az -> Z - Oz;
             true -> Z - Cz
    end,
    NewVtab = execute_move({Dx,Dy,Dz},{X,Y,Z},{Fx,Fy,Fz},Wo,Vset,Vtab,Empty),
    NewWe = We#we{vp=NewVtab},
    NewShapes = gb_trees:update(Obj,NewWe,Shapes),
    NewSt = St#st{shapes=NewShapes},
    do_move(Move,Rest,NewSt).

execute_move({Dx,Dy,Dz},{Nx,Ny,Nz},{Fx,Fy,Fz},Wo,Vset,Vtab,Now) ->
    case gb_trees:size(Vtab) of
        0 -> Now;
        _ ->
            {Vertex,{X,Y,Z},Vtab2} = gb_trees:take_smallest(Vtab),
            case gb_sets:is_element(Vertex,Vset) of
                true ->
                    X1 = case Fx of
                        true -> Nx;
                        _ -> X+Dx
                    end,
                    Y1 = case Fy of
                        true -> Ny;
                        _ -> Y+Dy
                    end,
                    Z1 = case Fz of
                        true -> Nz;
                        _ -> Z+Dz
                    end;
                _ ->
                    if
                        Wo ->
                            X1 = X+Dx,
                            Y1 = Y+Dy,
                            Z1 = Z+Dz;
                        true ->
                            X1 = X,
                            Y1 = Y,
                            Z1 = Z
                    end
            end,
            NewNow = gb_trees:insert(Vertex,{X1,Y1,Z1},Now),
            execute_move({Dx,Dy,Dz},{Nx,Ny,Nz},{Fx,Fy,Fz},Wo,Vset,Vtab2,NewNow)
    end.
