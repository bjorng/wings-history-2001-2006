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
%%     $Id: wings_pb.erl,v 1.1 2004/02/06 14:44:04 dgud Exp $
%%

-module(wings_pb).

-define(NEED_OPENGL, 1).
%%%-define(NEED_ESDL, 1).
-include("wings.hrl").

-export([start/1, start/2, stop/0, update/1, update/2,
	 init/0, loop/1]).
-compile(export_all). %% DBG

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

ti() ->
    put(wm_viewport, {0,0,935,488}),
    wings_pb:init().
    
t1() ->
    io:format("Test 1~n"),
    wings_pb:start("X"), 
    timer:sleep(1000), 
    wings_pb:update("ASD", 0.5), 
    timer:sleep(2000), 
    wings_pb:stop().
t2() ->
    io:format("Test 2~n"),
    wings_pb:start("X"), 
    timer:sleep(1000), 
    wings_pb:update("FAST", 0.5), 
    timer:sleep(3000), 
    wings_pb:update("SLOW", 0.55), 
    timer:sleep(5000), 
    wings_pb:stop().

t3() ->
    io:format("Test 3~n"),
    wings_pb:start("1", {parts, 3}), 
    timer:sleep(1000), 
    wings_pb:update("2"), 
    timer:sleep(5000), 
    wings_pb:update("3"), 
    timer:sleep(5000), 
    wings_pb:stop().

start(Msg) when list(Msg) ->
    WSz = wings_wm:win_size(),
    call({start, Msg, percent,WSz}).
start(Msg, percent) when list(Msg) ->
    WSz = wings_wm:win_size(),
    call({start, Msg, percent,WSz});
start(Msg, D = {time, _T}) when list(Msg) ->
    WSz = wings_wm:win_size(),
    call({start, Msg, D,WSz});
start(Msg, D = {parts, _}) when list(Msg) ->
    WSz = wings_wm:win_size(),
    call({start, Msg, D, WSz}).

stop() ->
    call(stop).

% Next part is completed or in percent mode change help-msg
update(Msg) when list(Msg) -> 
    cast({update, Msg, undefined}).
%% Time in percent or in completed part number
update(Msg, Time) when list(Msg),  number(Time) -> 
    cast({update, Msg, Time}).

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

-record(state, {refresh=infinity, activity=false, msg=[], 
		pos=0.0, wpos, wpart=0, mparts, max=?MAX_T,
		t0}).

%% Start progressbar process
init() ->
    Pid = spawn_link(?MODULE, loop, [#state{}]),
    register(?PB, Pid),
    ok.

loop(#state{refresh=After,activity=Active}=S0) ->
    receive 
	{Pid, ?PB, {start, Msg, Data,{W,H}}} when Active == false ->
	    S = start_pb(Msg, Data),
	    put(wm_viewport, {0,0,W,H}),
	    wings_text:choose_font(),
	    reply(Pid, ok),
	    loop(draw_position(S));
	{?PB, {update, Msg, Time}} ->
	    S1 = update(Msg, Time, S0),
	    S = calc_position(S1),
	    loop(draw_position(S));
	{Pid, ?PB, stop} ->
	    draw_position(S0#state{pos=1.0}),
	    reply(Pid, ok),
	    loop(#state{});
	Msg ->
	    io:format("~p: got unexpected msg ~p~n", [?MODULE, Msg]),
	    loop(draw_position(S0))
    after After ->
	    S = calc_position(S0),
	    loop(draw_position(S))
    end.

start_pb(Msg, percent) ->
    #state{refresh=?REFRESH_T, activity=percent, msg=Msg, t0=erlang:now()};
start_pb(Msg, {time, Max}) ->
    #state{refresh=?REFRESH_T, activity=percent, msg=Msg, max=Max, t0=erlang:now()};
start_pb(Msg, {parts, Max}) ->
    #state{refresh=?REFRESH_T, activity=parts, msg=Msg, mparts=Max, t0=erlang:now()}.

update(Msg, Percent, S0=#state{activity=percent,t0=T0,max=Max0}) 
  when Percent > 0.0, Percent < 1.0 ->
    Max = estimated_endtime(Percent,T0,Max0),
    S0#state{msg=Msg, wpos=Percent, max=Max};
update(Msg, Part, S0=#state{activity=parts,mparts=Mpart,t0=T0,max=Max0}) 
  when number(Part) ->
    Max = estimated_endtime(Part/Mpart,T0,Max0),
    S0#state{msg=Msg, wpart=Part, max=Max};
update(Msg, _, S0=#state{activity=parts,wpart=Old,mparts=Mpart,t0=T0,max=Max0}) ->
    Part = Old+1,
    Max = estimated_endtime(Part/Mpart,T0,Max0),
    S0#state{msg=Msg, wpart=Part, max=Max};
update(Msg, undefined, S0) ->
    S0#state{msg=Msg}.

estimated_endtime(Percent,T0,OldMax) ->
    Diff = (now_diff(erlang:now(), T0) div 1000),
    EstimatedMax = Diff/Percent,
    if EstimatedMax > OldMax ->
	    EstimatedMax;  % Grow Max fast 
       true ->             % Shrink slower
	    OldMax - (OldMax-EstimatedMax)*0.75
    end.

calc_position(S0 = #state{activity = percent, pos = Pos, max=Max, wpos=Wanted}) ->
    DefSpeed = default_speed(Pos,Max),
    if Wanted == undefined ->
	    S0#state{pos = Pos+DefSpeed};
       Wanted > Pos -> 
	    Speed = ((Wanted-Pos)/Wanted)/(1000/?REFRESH_T) + DefSpeed,
	    NewPos = Speed+Pos,
	    if NewPos > Wanted ->
		    S0#state{pos = NewPos, wpos=undefined};
	       true ->
		    S0#state{pos = NewPos}
	    end;
       true ->
	    Speed = Wanted/Pos*DefSpeed,
	    S0#state{pos = Pos+Speed}
    end;
calc_position(S0=#state{activity=parts,pos=Pos,max=Max,wpart=WP,mparts=MP}) ->
    Wanted = WP/MP,
    if Wanted =< Pos ->
	    DefSpeed = default_speed(MP*(Pos-Wanted),Max/MP),
	    S0#state{pos = DefSpeed+Pos};
       Wanted > Pos ->
	    DefSpeed = default_speed(Pos,Max),
	    Speed  = ((Wanted-Pos)/Wanted)/(1000/?REFRESH_T)+DefSpeed,
	    NewPos = Speed+Pos,
	    S0#state{pos = NewPos}
    end.
  
%% Math 
default_speed(Pos, Max) ->
    if (Pos == 0.0) or (Max == 0.0) ->
	    0.01;
       true -> 
	    (?REFRESH_T/Max) * (1-Pos*Pos) 
    end.

now_diff({A2, B2, C2}, {A1, B1, C1}) ->
    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.

%% Draw Progress Bar 

draw_position(S = #state{activity=false}) -> S;
draw_position(S = #state{msg=Msg, pos=Pos}) ->    
    {W,_} = wings_wm:win_size(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:drawBuffer(?GL_FRONT),
    wings_io:info(Msg),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:color4f(0.383,0.646,0.419, 0.61),    
    gl:recti(4, 1, trunc((W-8)*Pos), 1*?LINE_HEIGHT-1),
    gl:disable(?GL_BLEND),
    gl:finish(),
    gl:drawBuffer(?GL_BACK),
    gl:popAttrib(),

    io:format("~p ~s: ~.3f ~w~n", [time(), Msg, Pos,S]),    
    S.
