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
%%     $Id: wpc_test_ask.erl,v 1.3 2003/10/20 15:24:00 raimo_niskanen Exp $
%%

-module(wpc_test_ask).

-export([enable/0,disable/0]).
-export([load/1]).
-export([init/0,menu/2,dialog/2,command/2]).

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
	true -> Menu++[separator,{"Test Ask",?MODULE}];
	_ -> Menu
    end;
menu(_, Menu) -> Menu.

command({tools,?MODULE}, St) ->
    case enabled() of
	true -> command_dialog(St);
	_ -> next
    end;
command(_, _St) ->
    next.



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



command_dialog(_St) ->
    Fun = fun(Res) -> 
		  erlang:display({?MODULE,?LINE,Res}),
		  ignore
	  end,
    Dialog =
	[{hframe,
	  [{vframe,
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
				   wings_io:sunken_rect(X, Y, W, H, Color)
			   end},
	     {slider,[{range,{1,3}},{key,d},{hook,disable_hook(c)}]},
	     {key_alt,{d,1},"Alt 2",2,[{hook,disable_hook(c)}]},
	     separator,
	     {custom,40,10,fun (X, Y, W, H, Store) ->
				   R = gb_trees:get(red, Store),
				   G = gb_trees:get(green, Store),
				   B = gb_trees:get(blue, Store),
				   wings_io:sunken_rect(X, Y, W, H, {R,G,B})
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
		]}]}]},
	   {vframe,
	    [{text,123,[{hook,disable_hook(c)}]},
	     {slider,{text,0.5,[{range,{0.0,1.0}}]}},
	     {color,{1.0,0.0,0.0}},
	     {color,{0.0,1.0,0.0,1.0}},
	     {menu,[{"Alt 1",1},{"Alt 2",2},{"Alt 3",3}],{d,3},[{key,menu}]},
	     {color,{0.0,0.0,1.0}},
	     {text,1.23},
	     {menu,[{"A",a},{"B",b},{"C",c}],{m,a}}
	     ]}]}],
    wings_ask:dialog("Test Ask", Dialog, Fun).
%%%    keep

color_update(T, {K1,K2}, {Ka,Kb,Kc}) ->
    fun (update, {_Key,Val,Store0}) ->
	    V1 = gb_trees:get(K1, Store0),
	    V2 = gb_trees:get(K2, Store0),
	    {Va,Vb,Vc} = color_update(T, Val, V1, V2),
	    Store1 = gb_trees:update(Ka, Va, Store0),
	    Store2 = gb_trees:update(Kb, Vb, Store1),
	    Store = gb_trees:update(Kc, Vc, Store2),
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
	    gb_trees:get(I+V, Store);
	(is_disabled, {_Var,_I,Store}) ->
	    gb_trees:get(V, Store);
	(_, _) ->
	    void
    end.
