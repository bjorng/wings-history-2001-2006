%%
%%  wpc_test_ask.erl --
%%
%%     Default disabled test plugin for dialogs (wings_ask).
%%
%%  Copyright (c) 2003 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_test_ask.erl,v 1.18 2003/11/28 18:29:04 bjorng Exp $
%%

-module(wpc_test_ask).

-export([enable/0,disable/0]).
-export([load/1]).
-export([init/0,menu/2,dialog/2,command/2]).

-import(lists, [reverse/1,reverse/2]).

enable() -> wpa:pref_set(?MODULE, enabled, true).

disable() -> wpa:pref_delete(?MODULE, enabled).

enabled() -> wpa:pref_get(?MODULE, enabled).
    
load(File) ->
    load(wings_ask, File).

load(Mod, File) ->
    Ext = code:objfile_extension(),
    Filename = filename:rootname(File, Ext) ++ Ext,
    {ok,Code} = file:read_file(Filename),
    code:purge(Mod),
    code:load_binary(Mod, Filename, Code).



init() ->
    case enabled() of
	true -> true;
	_ -> false
    end.

menu({tools}, Menu) ->
    case enabled() of
	true -> 
	    Menu++
		[separator,
		 {"Test Ask",{?MODULE,[{"Minimal Dialog",minimal},
				       {"Large Dialog",large},
				       {"Overlay Dialog",overlay},
				       {"Dynamic Dialog",dynamic}]}}];
	_ -> Menu
    end;
menu(_, Menu) -> Menu.

command({tools,{?MODULE,minimal}}, St) ->
    maybe_dialog(fun minimal_dialog/1, St);
command({tools,{?MODULE,large}}, St) ->
    maybe_dialog(fun large_dialog/1, St);
command({tools,{?MODULE,overlay}}, St) ->
    maybe_dialog(fun overlay_dialog/1, St);
command({tools,{?MODULE,dynamic}}, St) ->
    maybe_dialog(fun dynamic_dialog/1, St);
command(_, _St) ->
    next.

maybe_dialog(Dialog, St) ->
    case enabled() of
	true -> Dialog(St);
	_ -> next
    end.
	    

dialog({material_editor_setup,_Name,_Mat}, Dialog) ->
    case enabled() of true -> Dialog++[{"Test Ask",true}];
	_ -> Dialog
    end;
dialog({material_editor_result,_Name,Mat}, [X|R]=Res) ->
    case enabled() of true -> 
	    erlang:display({?MODULE,?LINE,X}),
	    {Mat,R};
	_ -> {Mat,Res}
    end;
dialog({light_editor_setup,_Name,_Ps}, Dialog) ->
    case enabled() of true -> Dialog++[{"Foo",true}];
	_ -> Dialog
    end;
dialog({light_editor_result,_Name,Ps}, [X|R]=Res) ->
    case enabled() of true ->
	    erlang:display({?MODULE,?LINE,X}),
	    {Ps,R};
	_ -> {Ps,Res}
    end;
dialog(_X, Dialog) ->
    io:format("~p\n", [{_X,Dialog}]),
    Dialog.



minimal_dialog(_St) ->
    Fun = fun(Res) -> 
		  erlang:display({?MODULE,?LINE,Res}),
		  case Res of 
		      [{a,true}|_] -> 
			  wings_util:error("Uncheck the checkbox!");
		      _ -> ok
		  end, ignore
	  end,
    Dialog =
	[{hframe,[{label,"Label"},{"Checkbox",false,[{key,a}]}],
	 [{title,"Hframe"}]}],
    wings_ask:dialog("Test Ask Minimal", Dialog, Fun).



large_dialog(St) -> do_large_dialog(St, false, true, false, undefined).

do_large_dialog(St, MinimizedL, MinimizedC, MinimizedR, Pos) ->
    Dialog =
	[{hframe,
	  [large_dialog_l(MinimizedL, MinimizedC),
	   large_dialog_r(MinimizedR)]},
	 {position,Pos,[{key,position}]}],
    wings_ask:dialog("Test Ask Large", Dialog, large_result(St)).

large_result(St) ->
    fun ([MinimizedL|Res]) -> 
	    erlang:display({?MODULE,?LINE,Res}),
	    MinimizedC = proplists:get_value(minimized_c, Res),
	    MinimizedR = proplists:get_value(minimized_r, Res),
	    Pos = proplists:get_value(position, Res),
	    case proplists:get_value(reset, Res) of
		true -> 
		    do_large_dialog(St, MinimizedL, MinimizedC, MinimizedR, 
				    Pos),
		    ignore;
		false -> ignore
	    end
    end.

overlay_dialog(St) -> 
    do_overlay_dialog(St, buttons, 1, false, true, false, undefined).

do_overlay_dialog(St, Style, Active, MinimizedL, MinimizedC, MinimizedR, Pos) ->
    Dialog =
	[{hframe,[{label,"Style  "},
		  {hradio,[{"Buttons",buttons},{"Tabs",tabs},{"Menu",menu}],Style,
		  [{hook,fun (update, {Var,_I,Val,Sto}) ->
				 erlang:display({?MODULE,?LINE,
						 [update,{Var,_I,Val,sto}]}),
				{done,gb_trees:update(Var, Val, Sto)};
			     (_, _) -> void end}]}]},
	 {oframe,
	  [{"Left frame",large_dialog_l(MinimizedL, MinimizedC)},
	   {"Right frame",large_dialog_r(MinimizedR)}],
	  Active,
	  [{style,Style}]},
	 {position,Pos,[{key,position}]}],
    wings_ask:dialog("Test Ask Overlay", 
		     {hframe,[{vframe,Dialog},
			      {vframe,[{button,done,[ok]},
				       {button,cancel,[cancel]}]}]}, 
		     overlay_result(St)).

overlay_result(St) ->
    fun ([Style,Active,MinimizedL|Res]) -> 
	    erlang:display({?MODULE,?LINE,Res}),
	    MinimizedC = proplists:get_value(minimized_c, Res),
	    MinimizedR = proplists:get_value(minimized_r, Res),
	    Pos = proplists:get_value(position, Res),
	    case lists:last(Res) of
		false -> 
		    do_overlay_dialog(St, Style, Active,
				      MinimizedL, MinimizedC, MinimizedR, 
				      Pos),
		    ignore;
		true -> ignore
	    end
    end.

large_dialog_l(MinimizedL, MinimizedC) ->
    PaneColor = wings_pref:get_value(dialog_color),
    {vframe,
     [{label,"Label"},
      {key_alt,{d,1},"Alt 3",3,[{hook,disable_hook(c)}]},
      separator,
      {"Checkbox",false},
      {"Checkbox key",false,[{key,c},{hook,disable_hook(-1)}]},
      separator,
      {key_alt,{d,1},"Alt 1",1,[{hook,disable_hook(c)}]},
      {custom,40,10,fun (X, Y, W, H, Store) ->
			    Color = case gb_trees:get(c, Store) of
					true -> {1,1,0};
					false -> {0,1,1} end,
			    wings_io:blend(PaneColor,
					   fun(Col) ->
						   wings_io:sunken_rect(
						     X, Y, W, H, Color, Col)
					   end)
		    end},
      {slider,[{range,{1,3}},{key,d},{hook,disable_hook(c)}]},
      {key_alt,{d,1},"Alt 2",2,[{hook,disable_hook(c)}]},
      separator,
      {custom,40,10,fun (X, Y, W, H, Store) ->
			    R = gb_trees:get(red, Store),
			    G = gb_trees:get(green, Store),
			    B = gb_trees:get(blue, Store),
			    wings_io:blend(PaneColor,
					   fun(Col) ->
						   wings_io:sunken_rect(
						     X, Y, W, H, {R,G,B}, 
						     Col)
					   end)
		    end},
      {hframe,
       [{vframe,[{label,"R"},{label,"G"},{label,"B"},
		 {label,"H"},{label,"S"},{label,"V"}]},
	{vframe,
	 [{slider,[{color,{r,green,blue}},{key,red},
		   {value,0.5},{range,{0.0,1.0}},
		   {hook,
		    color_update(r, {green,blue}, {hue,sat,val})}]},
	  {slider,[{color,{g,red,blue}},{key,green},
		   {value,0.5},{range,{0.0,1.0}},
		   {hook,
		    color_update(g, {red,blue}, {hue,sat,val})}]},
	  {slider,[{color,{b,red,green}},{key,blue},
		   {value,0.5},{range,{0.0,1.0}},
		   {hook,
		    color_update(b, {red,green}, {hue,sat,val})}]},
	  {slider,[{color,{h,sat,val}},{key,hue},
		   {value,300},{range,{0,360}},
		   {hook,
		    color_update(h, {sat,val}, {red,green,blue})}]},
	  {slider,[{color,{s,hue,val}},{key,sat},
		   {value,0.0},{range,{0.0,1.0}},
		   {hook,
		    color_update(s, {hue,val}, {red,green,blue})}]},
	  {slider,[{color,{v,hue,sat}},{key,val},
		   {value,0.5},{range,{0.0,1.0}},
		   {hook,
		    color_update(v, {hue,sat}, {red,green,blue})}]}
	 ]}],[{title,"A Hframe"},{minimized,MinimizedC},{key,minimized_c}]}],
     [{title,"Left Vframe"},{minimized,MinimizedL}]}.

large_dialog_r(MinimizedR) ->
    {vframe,
     [{text,123,[{hook,disable_hook(c)}]},
      {slider,{text,0.5,[{range,{0.0,1.0}}]}},
      {color,{1.0,0.0,0.0},[{hook,disable_hook(c)}]},
      {color,{0.0,1.0,0.0,1.0}},
      {menu,[{"Alt 1",1},{"Alt 2",2},{"Alt 3",3}],3,
       [{key,d},{hook,disable_hook(c)},{info,info(c)}]},
      {hframe,[{color,{0.0,0.0,1.0}},
	       panel,
	       {"Hide next frame",true,
		[{key,1},
		 {hook,fun (update, {Var,_I,Val,Store}) ->
			       {layout,gb_trees:update(Var, Val, Store)};
			   (_, _) -> void end}]}]},
      {hframe,[{text,1.23},
	       {button,"Ok",ok,[{hook,disable_hook(c)}]}],
       [{minimized,true}]},
      {menu,[{"A",a},{"B",b},{"C",c}],a,
       [{key,m},
	{hook,fun (menu_disabled, {_Var,_I,Sto}) ->
		      case gb_trees:get(c, Sto) of
			  true -> [];
			  _ -> [{b,[{info,info(c)}]}]
		      end;
		  (_, _) -> void
	      end},
       {info,"Partly disabled menu"}]},
      {button,"Reset",done,[{key,reset}]}
     ],[{title,"Right vframe"},{minimized,MinimizedR},{key,minimized_r}]}.

info(c) -> "Requires \"Checkbox key\" checked".
    

color_update(T, {K1,K2}, {Ka,Kb,Kc}) ->
    fun (update, {Var,_I,Val,Store0}) ->
	    V1 = gb_trees:get(K1, Store0),
	    V2 = gb_trees:get(K2, Store0),
	    {Va,Vb,Vc} = color_update(T, Val, V1, V2),
	    Store1 = gb_trees:update(Var, Val, Store0),
	    Store2 = gb_trees:update(Ka, Va, Store1),
	    Store3 = gb_trees:update(Kb, Vb, Store2),
	    Store = gb_trees:update(Kc, Vc, Store3),
	    {store,Store};
	(_, _) ->
	    void
    end.

color_update(r, R, G, B) ->
    wings_ask:rgb_to_hsv(R, G, B);
color_update(g, G, R, B) ->
    wings_ask:rgb_to_hsv(R, G, B);
color_update(b, B, R, G) ->
    wings_ask:rgb_to_hsv(R, G, B);
color_update(h, H, S, V) ->
    wings_ask:hsv_to_rgb(H, S, V);
color_update(s, S, H, V) ->
    wings_ask:hsv_to_rgb(H, S, V);
color_update(v, V, H, S) ->
    wings_ask:hsv_to_rgb(H, S, V).

disable_hook(V) ->
    fun (is_disabled, {_Var,I,Store}) when integer(V) ->
	    not gb_trees:get(I+V, Store);
	(is_disabled, {_Var,_I,Store}) ->
	    not gb_trees:get(V, Store);
	(_, _) ->
	    void
    end.



dynamic_dialog(St) -> dynamic_dialog_1(St, [init,undefined]).

dynamic_dialog_1(St, Res) -> 
    dynamic_dialog_2(St, Res, []).

dynamic_dialog_2(_St, [false,_Pos], R0) -> 
    %% Dialog closed ok
    R = reverse(R0, [false]),
    erlang:display({?MODULE,?LINE,R}),
    R;
dynamic_dialog_2(St, [init,Pos], R) -> dynamic_dialog_3(St, [Pos,false|R]);
dynamic_dialog_2(St, [true,Pos], R) -> %% New frame
    Z = true, E = false, F = 0.5, D = false,
    dynamic_dialog_3(St, [Pos,false,D,F,E,Z|R]);
dynamic_dialog_2(St, [_Z,_E,_F,true|T], R) ->  %% Delete frame
    dynamic_dialog_3(St, reverse(T, R));
dynamic_dialog_2(St, [Z,E,F,false|T], R) -> 
    dynamic_dialog_2(St, T, [false,F,E,Z|R]).

dynamic_dialog_3(St, [Pos,_New|R]) ->
    dynamic_dialog_4(St, R, [{hframe,[{button,"New",done},panel]},
			     {position,Pos}]).

dynamic_dialog_4(St, [], Dialog) ->
    wings_ask:dialog("Test Ask Dynamic", Dialog, 
		     fun (R) -> dynamic_dialog_1(St, R), ignore end);
dynamic_dialog_4(St, [_D,F,E,Z|T], Dialog0) ->
    Dialog = 
	[{hframe,[{vframe,[{"Enable",E},
			   {text,F,[{range,{0.0,1.0}},
				    {hook,disable_hook(-1)}]}]},
		  {vframe,[{hframe,[{button,"Delete",done},panel]},
			  {slider,[{range,{0.0,1.0}},
				   {key,-5},
				   {hook,disable_hook(-6)}]}]}],
	  [{title,"Foo"},{minimized,Z}]}
	 |Dialog0],
    dynamic_dialog_4(St, T, Dialog).
