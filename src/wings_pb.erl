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
%%     $Id: wings_pb.erl,v 1.4 2004/02/07 18:39:14 bjorng Exp $
%%

-module(wings_pb).

-define(NEED_OPENGL, 1).
%%%-define(NEED_ESDL, 1).
-include("wings.hrl").

-export([start/1,done/0,done/1,update/1,update/2,
	 init/0,loop/1]).

-define(PB, progress_bar).

%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Usage
%%
%% Start progress bar, you can't use opengl during start and stop
%% Two modes Percent or Parts both moves has a default time 20+ secs
%% the progress-speed slows down to zero when reaching the end
%%
%% Examples Mode1 (percentage): 
%% Basic progress bar takes 20-30 secs to completion
%% pb:start("Heavy calc1"), heavy_calc(xxx), pb:stop(),
%% additionaly you can change the msg with pb:update("New Msg") 
%% in between
%%
%% or in millisecs you specify the time
%% pb:start("Very Heavy", {time, 60000}),  heavy_calc(xxx), pb:stop(),
%% 
%% If you know the percentage of completion
%% you can call pb:update("New Msg", N/Max)  periodicly
%% to help compensate the speed
%% 
%% You can also help with speed compensation by giving the number of
%% parts/phases/stages that you have

%% Examples Mode2 (parts)
%% pb:start("Init", {parts, 3}), blaba,
%% pb:update("Second Phase"), blalba,
%% pb:update("Last Phase"), blalba,
%% pb:stop()
%%
%% or (not so well currently)
%% pb:start("Init", {parts, 238444}), loop(balal),
%% pb:update("Second Phase", 23434), baslsd,
%% pb:update("Second Phase", 43343), ...
%% pb:stop()

start(Msg) when is_list(Msg) ->
    WinInfo = wings_wm:viewport(message),
    call({start,Msg,percent,WinInfo}).

done() ->
    call(done).

done(Ret) ->
    done(),
    Ret.

% Next part is completed or in percent mode change help-msg
update(Msg) when is_list(Msg) -> 
    cast({update, Msg, undefined}).

%% Time in percent or in completed part number
update(Percent, Msg) when is_list(Msg),  is_number(Percent) -> 
    cast({update,Msg,Percent}).

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
	   
%% Progress bar internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%

-define(REFRESH_T, 200).    % Nice enough animation 10 fps?
-define(MAX_T, 20000).      % 20 sek is average completion time ?

-record(state,
	{refresh=infinity,
	 activity=false,
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

loop(#state{refresh=After,activity=Active}=S0) ->
    receive
	{Pid,?PB,{start,Msg,_Data,{X,Y,W,H}}} when Active == false ->
	    S = #state{refresh=?REFRESH_T,activity=percent,
		       msg=["",Msg],t0=now()},
	    put(wm_viewport, {X,Y,W-17,H}),
	    wings_text:choose_font(),
	    reply(Pid, ok),
	    loop(draw_position(S));
	{?PB,{update,Msg,Time}} ->
	    S1 = update(Msg, Time, S0),
	    S = calc_position(S1),
	    loop(draw_position(S));
	{Pid,?PB,done} ->
	    S = update("done", 1.0, S0#state{next_pos=1.0,pos=1.0}),
	    print_stats(S),
	    draw_position(S),
	    reply(Pid, ok),
	    loop(#state{});
	Msg ->
	    io:format("~p: got unexpected msg ~p~n", [?MODULE, Msg]),
	    loop(draw_position(S0))
    after After ->
	    S = calc_position(S0),
	    loop(draw_position(S))
    end.

update(Msg, Percent, #state{next_pos=OldNext,msg=[_|Msg0],
			    stats=Stats0,t0=Time0}=S) ->
    NowDiff = now_diff(now(), Time0),
    Stats = [Percent,NowDiff|Stats0],
    S#state{msg=[Msg|Msg0],pos=OldNext,next_pos=Percent,stats=Stats}.

calc_position(#state{pos=Pos0,next_pos=NextPos}=S) when Pos0 < NextPos ->
    Pos = Pos0 + (NextPos - Pos0) / 3,
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

draw_position(#state{activity=false}=S) -> S;
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
build_msg([H|T]) -> build_msg(T) ++ ": " ++ H.

    
