%%
%%  wings_pb.erl --
%%
%%     This module contains a progress bar
%%
%%  Copyright (c) 2004 Dan Gudmundsson and Bjorn Gustavsson 
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pb.erl,v 1.6 2004/02/08 15:29:35 bjorng Exp $
%%

-module(wings_pb).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-export([start/1,update/1,update/2,
	 done/0,done/1,done_stat/0,done_stat/1]).

-export([init/0,loop/1]).

-define(PB, progress_bar).

start(Msg) when is_list(Msg) ->
    WinInfo = wings_wm:viewport(message),
    cast({start,Msg,percent,WinInfo}).

update(Percent) when is_float(Percent) -> 
    cast({update,"",Percent}).

update(Percent, Msg) when is_list(Msg), is_float(Percent) -> 
    cast({update,Msg,Percent}).

done() ->
    call(done).

done(Ret) ->
    done(),
    Ret.

done_stat() ->
    Stat = done(),
    Stat().

done_stat(Ret) ->
    Stat = done(),
    Stat(),
    Ret.

%% Helpers

call(What) ->
    case whereis(?PB) of
	undefined ->
	    {error, not_running};
	Pid ->
	    Pid ! {self(), ?PB, What},
	    receive 
		{?PB, Res} ->
		    Res
	    end
    end.

cast(What) ->
    case whereis(?PB) of
	undefined ->
	    {error, not_running};
	Pid ->
	    Pid ! {?PB, What},
	    ok
    end.

reply(Pid, What) ->
    Pid ! {?PB,What},
    ok.
	   
%%%%%%%% Progress bar internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(REFRESH_T, 200).			%Refresh interval.

-record(state,
	{refresh=infinity,
	 level=0,
	 msg=[], 
	 pos=0.0,
	 next_pos=0.0,
	 t0,
	 stats=[]
	}).

%% Start progressbar process
init() ->
    Pid = spawn_link(?MODULE, loop, [#state{}]),
    register(?PB, Pid),
    ok.

loop(#state{refresh=After,level=Level}=S0) ->
    receive
	{?PB,{start,Msg,_Data,{X,Y,W,H}}} when Level =:= 0 ->
	    S = #state{refresh=?REFRESH_T,level=1,
		       msg=["",Msg],t0=now()},
	    put(wm_viewport, {X,Y,W-17,H}),
	    wings_text:choose_font(),
	    loop(draw_position(S));
	{?PB,{update,Msg,Time}} when Level =:= 1 ->
	    S1 = update(Msg, Time, S0),
	    S = calc_position(S1),
	    loop(draw_position(S));
	{Pid,?PB,done} when Level =:= 1 ->
	    S = update("done", 1.0, S0#state{next_pos=1.0,pos=1.0}),
	    draw_position(S),
	    reply(Pid, fun() -> print_stats(S) end),
	    loop(#state{});
	{?PB,{start,_Msg,_Data,_}} ->
	    S = S0#state{level=Level+1},
	    loop(S);
	{?PB,{update,_,_}} ->
	    loop(S0);
	{Pid,?PB,done} ->
	    reply(Pid, ok),
	    loop(S0#state{level=Level-1});
	Msg ->
	    io:format("~p: got unexpected msg ~p~n", [?MODULE, Msg]),
	    loop(S0)
    after After ->
	    S = calc_position(S0),
	    loop(draw_position(S))
    end.

update(Msg, Percent, #state{msg=[_|Msg0],stats=Stats0,t0=Time0}=S) ->
    NowDiff = now_diff(now(), Time0),
    Stats = [Percent,NowDiff|Stats0],
    S#state{msg=[Msg|Msg0],next_pos=Percent,stats=Stats}.

calc_position(#state{pos=Pos0,next_pos=NextPos}=S) when Pos0 < NextPos ->
    Pos = Pos0 + (NextPos - Pos0) / 5,
    S#state{pos=Pos}.

print_stats(#state{t0=Time0,stats=[_|Stats0]}) ->
    Total = now_diff(now(), Time0),
    [_|Stats] = lists:reverse(Stats0),
    io:nl(),
    print_stats_1(Stats, Total).

print_stats_1([Est,TimeDiff|T], Total) ->
    io:format("Est: ~f Real: ~f\n",
	      [Est,TimeDiff/Total]),
    print_stats_1(T, Total);
print_stats_1([], _) -> ok.

now_diff({A2, B2, C2}, {A1, B1, C1}) ->
    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.

%% Draw Progress Bar 

draw_position(#state{msg=Msg,pos=Pos}=S) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    {X,Y,W,H} = get(wm_viewport),
    gl:viewport(X, Y, W, H),
    gl:drawBuffer(?GL_FRONT),

    wings_io:ortho_setup(),
    wings_io:set_color(?PANE_COLOR),
    gl:recti(0, 0, W, H),

    BarLen = trunc((W-4)*Pos),
    double_gradient(4, 2, BarLen, W, ?LINE_HEIGHT),
    wings_io:set_color(wings_pref:get_value(info_color)),
    wings_io:text_at(6, ?CHAR_HEIGHT, build_msg(Msg)),

    gl:finish(),
    gl:drawBuffer(?GL_BACK),
    gl:popAttrib(),

    %%io:format("~p ~s: ~.3f ~w~n", [time(), Msg, Pos,S]),    
    S.

double_gradient(X, Y, BarW, W, H) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),

    gl:color3f(1, 1, 1),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+W, Y),
    gl:vertex2f(X+W, Y+H),
    gl:vertex2f(X, Y+H),

    gl:color3f(0.5, 0.73, 1),
    gl:vertex2f(X+BarW, Y+H div 2),
    gl:vertex2f(X, Y+H div 2),

    gl:color3f(0.66, 0.83, 1),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+BarW, Y),

    gl:color3f(0.62, 0.78, 1),
    gl:vertex2f(X+BarW, Y+H),
    gl:vertex2f(X, Y+H),

    gl:color3f(0.5, 0.73, 1),
    gl:vertex2f(X, Y+H div 2),
    gl:vertex2f(X+BarW, Y+H div 2),
    gl:'end'(),
    gl:shadeModel(?GL_FLAT).

build_msg([M]) -> M;
build_msg([[]|T]) -> build_msg(T);
build_msg([H|T]) -> build_msg(T) ++ ": " ++ H.
