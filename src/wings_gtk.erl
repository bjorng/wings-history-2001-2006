%%% File    : wings_gtk.erl
%%% Author  :  <tony@orre.bluetail.com>
%%% Description : WINGS/Gtk
%%% Created : 15 Aug 2001 by  <tony@orre.bluetail.com>

-module(wings_gtk).

-export([start/0]).

-import(lists, [map/2]).

-include_lib("erlgtk/include/gdk.hrl").
-include_lib("erlgtk/include/gtk.hrl").
-include_lib("erlgtk/include/gdk_gl.hrl").
-include_lib("erlgtk/include/gtk_gl_area.hrl").
-include_lib("erlgtk/include/gl.hrl").

-include("wings.hrl").


start() ->
    gtk:start(),
    spawn_link(fun init/0).

init() ->
    register(wings, self()),
    main().

file_menu() ->
    {"File", [],
     [
      {"New",  [{accel,"<ctrl>N"}, {value,{file,new}}]},
      {"Open", [{accel,"<ctrl>O"}, {value,{file,open}}]},
      {"Merge",[{accel,"<ctrl>L"}, {value,{file,merge}}]},
      separator,
      { "Save",[{accel,"<ctrl>S"},{value,{file,save}}]},
      { "Save As...", [{value,{file,save_as}}]},
      separator,
      {"Import", [],
       [{"3D Studio (.3ds)", [{value,{file,{import,tds}}}]},
	{"Wawefront (.obj)", [{value,{file,{import,obj}}}]}
       ]},
      {"Export", [],
       [
	{"3D Studio (.3ds)", [{value,{file,{export,tds}}}]},
	{"Wawefront (.obj)", [{value,{file,{export,obj}}}]},
	{"RenderMan (.rib)", [{value,{file,{export,rib}}}]}
       ]},
      separator,
      {"Delete File", [{value,{file,delete}}]},
      separator,
      {"Exit", [{accel,"<ctrl>Q"}, {value,{file,quit}}]}
     ]}.

edit_menu(Material) ->
    {"Edit", [],
     [
      {"Undo/redo",[{accel,"<ctrl>Z"},{value,{edit,undo_toggle}}]},
      {"Redo",     [{accel,"<shft><ctrl>Z"},{value,{edit,redo}}]},
      {"Undo",     [{accel,"<alt><ctrl>Z"}, {value,{edit,undo}}]},
      separator,
      {"_name",  [{accel,"d"},{value,{edit,repeat}},
		  {parent_activate, 
		   fun(W) ->
			   menu_item_set_command_name(W)
		   end}]},
      separator,
      materials_menu(Material, edit)
     ]}.

materials_menu(Mat0, MenuName) ->
    {"Material", [],
     map(fun({Id,_}) ->
		 Name = case atom_to_list(Id) of
			    [H|T] when $a =< H, H =< $z ->
				[H-$\s|T];
			    Name0 -> Name0
			end,
		 {Name,[{value,{MenuName,{material,Id}}}]}
	 end, gb_trees:to_list(Mat0)) }.

view_menu() ->
    { "View", [],
      [
       %% #opt.ground
       { "_ground", 
	 [{value,{view, toggle_groundplane}},
	  {parent_activate, 
	   fun(W) ->
		   menu_item_toggle_opt(W, #opt.ground, 
					"Hide ground plane",
					"Show ground plane")
	   end}]},
       %% #opt.axes
       { "_axes",
	 [{value,{view, toggle_axes}},
	  {parent_activate, 
	   fun(W) ->
		   menu_item_toggle_opt(W, #opt.axes, 
					"Hide axes",
					"Show axes")
	   end}]},
       separator,
       %% #opt.wire
       {  "_wire",
	  [{accel, "w"}, 
	   {value,{view, toggle_wireframe}},
	   {parent_activate, 
	    fun(W) ->
		    menu_item_toggle_opt(W, #opt.wire, 
					 "Filled",
					 "Wireframe")
	    end}]},
       %% #opt.smooth
       { "_smooth", 
	 [{accel, "Tab"},
	  {value,{view, toggle_smooth}},
	  {parent_activate, fun(W) ->
				    menu_item_toggle_opt(W, #opt.smooth, 
							 "Flat Apperance",
							 "Smooth Preview")
			    end}]},
       separator,
       {"Reset View",[{accel,"r"}, {value,{view,reset}}]},
       {"Aim",       [{accel,"a"},{value,{view,aim}}]},
       %% #opt.ortho
       { "_ortho",
	 [{accel,"o"},
	  {value,{view,toggle_ortho}},
	  {parent_activate, fun(W) ->
				    menu_item_toggle_opt(W, #opt.ortho, 
							 "Perspective View",
							 "Ortographic View")
			    end}]},	  
       separator,
       {"View Along", [],
	[ 
	  {"+X",[{accel,"x"}, {value,{view,{along,x}}}]},
	  {"+Y",[{accel,"y"}, {value,{view,{along,y}}}]},
	  {"+Z",[{accel,"z"}, {value,{view,{along,z}}}]},
	  {"-X",[{accel,"<shft>X"},{value,{view,{along,neg_x}}}]},
	  {"-Y",[{accel,"<shft>Y"},{value,{view,{along,neg_y}}}]},
	  {"-Z",[{accel,"<shft>Z"},{value,{view,{along,neg_z}}}]}
	 ]}
      ]}.

select_menu(Material) ->
    { "Select", [],
      [
       {"Deselect",[{accel,"Space"},{value,{select,deselect}}]},
       separator,
       {"More",      [{accel,"+"}, {value,{select,more}}]},
       {"Less",      [{accel,"-"}, {value,{select,less}}]},
       {"Edge Loop", [{accel,"l"}, {value,{select,edge_loop}}]},
       {"Similar",   [{accel,"i"}, {value,{select,similar}}]},
       separator,
       {"Adjacent vertices",[{accel,"v"},{value,{select,vertex}}]},
       {"Adjacent edges",   [{accel,"e"},{value,{select,edge}}]},
       {"Adjacent faces",   [{accel,"f"},{value,{select,face}}]},
       separator,
       {"All", [],
	[{"Vertices", [{accel_if,fun() -> is_selection_mode(vertex) end, "<ctrl>a"},
		       {value,{select,{all,vertex}}}]},
	 {"Faces",    [{accel_if,fun() -> is_selection_mode(face) end, "<ctrl>a"},
		       {value,{select,{all,face}}}]},
	 {"Edges",    [{accel_if,fun() -> is_selection_mode(edge) end, "<ctrl>a"},
		       {value,{select,{all,edge}}}]},
	 {"Object",   [{accel_if,fun() -> is_selection_mode(body) end, "<ctrl>a"},
		       {value,{select,{all,body}}}]}
	]},
       separator,
       {"Hard edges", [{value,{select,hard_edges}}]},
       {"Vertices with", [],
	[{"2 edges", [{value,{select,{vertices_with,2}}}]},
	 {"3 edges", [{value,{select,{vertices_with,3}}}]},
	 {"4 edges", [{value,{select,{vertices_with,4}}}]},
	 {"5 edges", [{value,{select,{vertices_with,5}}}]}
	]},
       {"Faces with", [],
	[{"2 edges",  [{value,{select,{faces_with,2}}}]},
	 {"3 edges",  [{value,{select,{faces_with,3}}}]},
	 {"4 edges",  [{value,{select,{faces_with,4}}}]},
	 {"5 edges",  [{value,{select,{faces_with,5}}}]}
	]},
       materials_menu(Material, select),
       {"Random", [],
	map(fun(I) -> {integer_to_list(I)++"%", [{value,{select,{random,I}}}]} end,
	    [10,20,30,40,50,60,70,80,90])
       },
       separator,
       {"Inverse", [{value,{select,inverse}}]},
       separator,
       {"Save selection",[{value,{select,save}}]},
       {"Load selection",[{value,{select,load}}]},
       {"Exchange selection",[{value,{select,exchange}}]},
       separator,
       {"Union with saved",    [{value,{select,union}}]},
       {"Subtract with saved", [{value,{select,subtract}}]},
       {"Intersection with saved", [{value,{select,intersection}}]}
      ]}.
     

align_menu() ->
    {"Align", [],
     [{"Align", [],
       [{"XYZ", [{value, {align,all}}]},
	{"X",   [{value, {align,x}}]},
	{"Y",   [{value, {align,y}}]},
	{"Z",   [{value, {align,z}}]},
	{"Radial X (YZ)", [{value,{align,radial_x}}]},
	{"Radial Y (XZ)", [{value,{align,radial_y}}]},
	{"Radial Z (XY)", [{value,{align,radial_z}}]}]},
      {"Center", [],
       [{"XYZ", [{value, {center,all}}]},
	{"X",   [{value, {center,x}}]},
	{"Y",   [{value, {center,y}}]},
	{"Z",   [{value, {center,z}}]},
	{"Radial X (YZ)", [{value,{center,radial_x}}]},
	{"Radial Y (XZ)", [{value,{center,radial_y}}]},
	{"Radial Z (XY)", [{value,{center,radial_z}}]}]}
     ]}.

objects_menu() ->
    Menu0 = [],  %% make this dynamic some nice way 
    { "Objects", [],
      [
       {"Show All", [{value,{objects,show_all}}]},
       {"Hide All", [{value,{objects,hide_all}}]},
       {"Toggle Visibility",[{value,{objects,toggle_all}}]},
       separator,
       {"Hide Selected",  [{value,{objects,hide_selected}}]},
       {"Hide Unselected",[{value,{objects,hide_unselected}}]},
       separator | Menu0]}.
      
help_menu() ->
    { "Help", [],
      [
       {"About", [{value,{help,about}}]}
      ]}.

shape_menu() ->
    [].

vertex_menu() ->
    [].

edge_menu() ->
    [].

face_menu() ->
    [].

body_menu() ->
    [].



is_selection_mode(Mode) ->
    St = get('_state'),
    St#st.selmode == Mode.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Menus
%%  syntax:
%%    Item = {Name, Options [, Item* ]}
%%    Option = {type,  <normal|check|radio>}
%%             {accel, Accelerator} |
%%             {active, Function}
%%    Menu = [ Item* ]
%%
%%  menu_item: create a menu item (with submenu) from menu spec
%%    submenu: create a sub menu
%%       menu: insert menu items into a menu
%%    menubar: create a menu bar with all menus
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% toggle option text
menu_item_toggle_opt(MenuItem, OptPos, TrueText, FalseText) ->
    St = get('_state'),  %% Dirty trick!!!
    Opts = St#st.opts,
    Text = case element(OptPos, Opts) of
	       true  -> TrueText;
	       false -> FalseText
	   end,
    io:format("menu_item_toggle_opt: ~s\n", [Text]),
    AccelLabel = gtk:bin_get_child(?GTK_BIN(MenuItem)),
    gtk:label_set_text(?GTK_LABEL(AccelLabel), Text).

menu_item_set_command_name(MenuItem) ->
    St = get('_state'),  %% Dirty trick!!!
    Text = command_name(St),
    AccelLabel = gtk:bin_get_child(?GTK_BIN(MenuItem)),
    gtk:label_set_text(?GTK_LABEL(AccelLabel), Text).

command_name(#st{last_command=ignore}) ->
    "(Can't repeat)";
command_name(#st{last_command={_,Cmd}}=St) ->
    CmdStr = stringify(Cmd),
    command_name(CmdStr, St).

command_name(CmdStr, #st{sel=[]}) ->
    lists:flatten(["(Can't repeat \"",CmdStr,"\")"]);
command_name(CmdStr, #st{selmode=Mode,last_command={Mode,_}}) ->
    lists:flatten(["Repeat \"",CmdStr,"\""]);
command_name(CmdStr, St) ->
    lists:flatten(["(Can't repeat \"",CmdStr,"\")"]).

stringify({Atom,Other}) when atom(Atom) ->
    cap(atom_to_list(Atom)) ++ "->" ++ stringify(Other);
stringify(Atom) when atom(Atom) ->
    cap(atom_to_list(Atom));
stringify(Int) when integer(Int) ->
    integer_to_list(Int).

cap(Str) ->
    cap(Str, true).
cap([Lower|T], true) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|cap(T, false)];
cap([$_|T], Any) ->
    [$\s|cap(T, true)];
cap([H|T], Any) ->
    [H|cap(T, false)];
cap([], Flag) -> [].



%% Create menu items 
menu_item(Menu, Parent, AccelGroup, {Name, Properties, SubMenu}) ->
    io:format("menu = ~s\n", [Name]),
    Item = case lists:keysearch(type, 1, Properties) of
	       {value,{_,radio}} ->
		   gtk:radio_menu_item_new_with_label([], Name);
	       {value,{_,check}} ->
		   gtk:check_menu_item_new_with_label(Name);
	       _ ->
		   gtk:menu_item_new_with_label(Name)
	   end,
    case lists:keysearch(activate, 1, Properties) of
	false -> ok;
	{value,{_,F1}} ->
	    gtk:signal_connect(Item, 'activate', F1)
    end,
    case lists:keysearch(value, 1, Properties) of
	false -> ok;
	{value,{_,Val}} ->
	    gtk:signal_connect(Item, 'activate', fun() -> cast(Val) end)
    end,
    case lists:keysearch(parent_activate, 1, Properties) of
	false -> ok;
	{value,{_,F2}} ->
	    gtk:signal_connect(Parent, 'activate', fun() -> F2(Item) end)
    end,
    case lists:keysearch(accel, 1, Properties) of
	false -> ok;
	{value,{_,Accel}} ->
	    case gtk:accelerator_parse(Accel) of
		{ok,0,0} ->
		    io:format("unknown accelerator \"~s\" in menu ~s\n",
			      [Accel, Name]),
		    ignore;
		{ok,Key,Mod} ->
		    gtk:widget_add_accelerator(Item,
					       'activate', AccelGroup,
					       Key, Mod, ?GTK_ACCEL_VISIBLE)
	    end
    end,
    case lists:keysearch(accel_if, 1, Properties) of
	false -> ok;
	{value,{_,Cond,Accel1}} ->
	    case gtk:accelerator_parse(Accel1) of
		{ok,0,0} ->
		    io:format("unknown accelerator \"~s\" in menu ~s\n",
			      [Accel1, Name]),
		    ignore;
		{ok,Key1,Mod1} ->
		    gtk:signal_connect(Parent, 'activate', 
				       fun() ->
					       case Cond() of
						   true ->
						       gtk:widget_add_accelerator(
							 Item,
							 'activate', AccelGroup,
							 Key1, Mod1, ?GTK_ACCEL_VISIBLE);
						   false ->
						       false
					       end
				       end)
	    end
    end,
    if SubMenu == [] -> 
	    ok;
       true ->
	    submenu(Item, AccelGroup, SubMenu)
    end,
    Item;
menu_item(Menu, Parent, AccelGroup, {Name, Properties}) ->
    menu_item(Menu, Parent, AccelGroup, {Name, Properties, []});
menu_item(Menu, Parent, AccelGroup, {Name}) ->
    menu_item(Menu, Parent, AccelGroup, {Name, [], []});
menu_item(Menu, Parent,  AccelGroup, separator) ->
    gtk:menu_item_new().


%% Create submenu
submenu(Parent, AccelGroup, []) ->
    ?GTK_NULL;
submenu(Parent, AccelGroup, List) ->
    Menu = gtk:menu_new(),
    Items = menu(Menu, Parent, AccelGroup, List),
    gtk:menu_item_set_submenu(Parent, Menu),
    gtk:widget_show(Menu),
    Items.

%% Create menu return list of menu items
menu(Menu, Parent, AccelGroup, List) ->
    map(
      fun(Spec) ->
	      Item = menu_item(Menu, Parent, AccelGroup, Spec),
	      gtk:menu_append(Menu, Item),
	      gtk:widget_show(Item),
	      Item
      end, List).

		    
menubar(MenuItems) ->
    Menu = gtk:menu_bar_new(),
    AccelGroup = gtk:accel_group_new(),
    gtk:menu_set_accel_group(Menu, AccelGroup),
    gtk:menu_bar_set_shadow_type(Menu, 'GTK_SHADOW_OUT'),
    menu(Menu, ?GTK_NULL, AccelGroup, MenuItems),
    {Menu, AccelGroup}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Toolbar
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% create an pixmap widget - filled with xpm data
xpm_pixmap(XpmData) ->
    Colormap  = gdk:colormap_get_system(),
    TransparentColor = #gdk_color { },
    {Pixmap,Mask} = gdk:pixmap_colormap_create_from_xpm_d(?NULL,
							  Colormap,
							  TransparentColor,
							  XpmData),
    gtk:pixmap_new( Pixmap, Mask).

set_window_icon(Window, XpmData) ->
    WinGdk   = gtk:widget_get_window(Window),    
    Colormap  = gdk:colormap_get_system(),
    TransparentColor = #gdk_color { },
    {Pixmap,Mask} = gdk:pixmap_colormap_create_from_xpm_d(?NULL,
							  Colormap,
							  TransparentColor,
							  XpmData),    
    gdk:window_set_icon(WinGdk, ?NULL, Pixmap, Mask),
    gdk:window_set_icon_name(WinGdk, "Wings 3D").


xpm_toggle_button(Pixmap) ->
    Button = gtk:toggle_button_new(),
    gtk:container_add( Button, Pixmap ),    
    Button.

toolbar(Ts) ->
    Toolbar = gtk:toolbar_new('GTK_ORIENTATION_HORIZONTAL', 
			      'GTK_TOOLBAR_ICONS'),
    gtk:toolbar_set_space_size(Toolbar, 15),
    
    lists:foreach(fun(space) ->
			  gtk:toolbar_append_space(Toolbar);
		     ({Pixmap,Tooltip,Fun}) ->
			  Button = xpm_toggle_button(Pixmap),
			  gtk:widget_show( Pixmap ),
			  gtk:widget_show( Button ),
			  gtk:signal_connect(Button, 'toggled', Fun),
			  gtk:toolbar_append_widget(Toolbar,Button,Tooltip,"")
		  end, Ts),
    Toolbar.


wings_menu(Material) ->
    menubar(
      [file_menu(), edit_menu(Material), 
       view_menu(), select_menu(Material), align_menu(),
       objects_menu(), help_menu()]).


wings_toolbar() ->
    toolbar(
      [{xpm_pixmap(wings_xpms:vertex()),
	"Select vertices", fun(W) -> cast({select,vertex}) end},
       {xpm_pixmap(wings_xpms:edge()),  
	"Select edges",    fun(W) -> cast({select,edge}) end},
       {xpm_pixmap(wings_xpms:face()),  
	"Select faces",    fun(W) -> cast({select,face}) end},
       {xpm_pixmap(wings_xpms:body()),
	"Select objects",  fun(W) -> cast({select,body}) end},
       space,
       {xpm_pixmap(wings_xpms:groundplane()), 
	"Show/Hide ground plane", fun(W) -> cast({view,toggle_ground}) end},
       {xpm_pixmap(wings_xpms:axes()),        
	"Show/Hide axes", fun(W) -> cast({view,toggle_axes}) end},
       space,
       {xpm_pixmap(wings_xpms:wire()),
        "Render wireframe or filled object", fun(W) -> cast({view,toggle_wireframe}) end},
       {xpm_pixmap(wings_xpms:smooth()), 
	"Render Flat or Smooth objects", fun(W) -> cast({view,toggle_smooth}) end},
       {xpm_pixmap(wings_xpms:perspective()),
	"Render Persepective or Ortographic view",fun(W) -> cast({view,toggle_ortho}) end}
      ]).

cast(Event) ->
    wings ! {command, Event}.


handle_input(Message, St) ->
    io:format("hanle_input: ~p\n", [Message]),
    case Message of 
	{command, Cmd} ->
	    handle_command(Cmd, St);
	Other ->
	    {true, St}
    end.


do_select_file(St, DialogName, Default, Pattern, FileOps) ->
    Me = self(),
    Pid = 
	spawn(fun() ->
		      Fs = gtk:file_selection_new (DialogName),
		      gtk:window_set_position(Fs, 'GTK_WIN_POS_CENTER'),
		      gtk:window_set_modal(Fs, true),
		      gtk:signal_connect(?GTK_OBJECT (Fs), 'destroy',
					 fun() -> 
						 Me ! {self(),cancel},
						 gtk:main_quit() 
					 end),
		      if FileOps == true ->
			      gtk:file_selection_show_fileop_buttons(Fs);
			 true ->
			      gtk:file_selection_hide_fileop_buttons(Fs)
		      end,
		      if Pattern == [] ->
			      ok;
			 true ->
			      gtk:file_selection_complete(Fs, Pattern)
		      end,
		      gtk:signal_connect(
			gtk:file_selection_get_ok_button(Fs),
			'clicked', 
			fun () ->
				Name = gtk:file_selection_get_filename(Fs),
				Me ! {self(),{ok,Name}},
				gtk:main_quit()
			end),
		      gtk:signal_connect(
			gtk:file_selection_get_cancel_button(Fs),
			'clicked',
			fun() ->
				self() ! {self(),cancel},
				gtk:widget_destroy(Fs),
				gtk:main_quit()
			end),
		      gtk:file_selection_set_filename (?GTK_FILE_SELECTION(Fs),
						       Default),
		      gtk:widget_show(Fs),
		      gtk:main()
	      end),
    receive
	{Pid,Result} ->
	    Result
    end.


do_save_as(St) ->
    case do_select_file(St, "Save As", "foobar.wings", "", true) of
	cancel -> {true, St};
	{ok,File} -> {true, St}
    end.

do_save(St) ->
    if St#st.saved == true ->
	    {true, St};
       true ->
	    do_save_as(St)
    end.

do_open(St) ->
    case do_select_file(St, "Open", "", "*.wings", false) of
	cancel -> {true, St};
	{ok,File} -> {true, St}
    end.


handle_command({file,Op}, St) ->
    case Op of
	quit    -> gtk:main_quit(), {true,St};
	new     -> {true, St};
	save    -> do_save(St);
	save_as -> do_save_as(St); 
	open    -> do_open(St);
	Other   -> {true,St}
    end;
handle_command({view,Op}, St) ->
    case Op of
	toggle_groundplane ->
	    Opts0 = St#st.opts,
	    Opts1 = Opts0#opt { ground = not Opts0#opt.ground },
	    {true, St#st { opts = Opts1 }};
	toggle_axes ->
	    Opts0 = St#st.opts,
	    Opts1 = Opts0#opt { axes = not Opts0#opt.axes },
	    {true, St#st { opts = Opts1 }};
	toggle_wireframe ->
	    Opts0 = St#st.opts,
	    Opts1 = Opts0#opt { wire = not Opts0#opt.wire },
	    {true, St#st { opts = Opts1 }};
	toggle_smooth ->
	    Opts0 = St#st.opts,
	    Opts1 = Opts0#opt { smooth = not Opts0#opt.smooth },
	    {true, St#st { opts = Opts1 }};
	Other ->
	    {true, St}
    end;
handle_command({select,Op}, St) ->
    case Op of
	{all,Mode}   -> {true, St#st { selmode = Mode }};
	Other ->
	    {true, St}
    end;
handle_command({help,about}, St) ->
    Me = self(),
    Pid = spawn(fun () ->
			{Major,Minor} = ?WINGS_VERSION,
			Win = gtk:window_new('GTK_WINDOW_DIALOG'),
			gtk:window_set_policy(Win,false,false,false),
			gtk:window_set_position(Win, 'GTK_WIN_POS_CENTER'),
			gtk:window_set_modal(Win, true),
			Text = io_lib:format("Wings ~w.~w", [Major, Minor]),
			gtk:window_set_title(Win, lists:flatten(Text)),
			Vbox = gtk:vbox_new (false, 0),
			gtk:container_add (?GTK_CONTAINER (Win), Vbox),
			gtk:widget_show (Vbox),
			Wings = xpm_pixmap(wings_xpms:wings()),
			gtk:box_pack_start(?GTK_BOX(Vbox), Wings, false, false, 0),
			gtk:widget_show(Wings),
			Power = xpm_pixmap(wings_xpms:powered()),
			gtk:box_pack_start(?GTK_BOX(Vbox), Power, false, false, 0),
			gtk:widget_show(Power),
			gtk:widget_show(Win),
			gtk:signal_connect(Win, 'button_press_event',
					   fun() -> gtk:main_quit() end),
			gtk:signal_connect(Win, 'destroy', fun() -> gtk:main_quit() end),
			gtk:main(),
			Me ! {self(),done}
		end),
    receive
	{Pid,done} ->
	    {true, St}
    end;

handle_command(Cmd, St) ->
    {true, St}.


gl_init(Widget,_) ->
    %% OpenGL functions can be called only if make_current returns true 
    case gtk_gl_area:make_current(?GTK_GL_AREA(Widget)) of
	0 -> true;
	_ ->
	    Allocation = gtk:widget_get_allocation(Widget),
	    W = Allocation#gtk_allocation.width,
	    H = Allocation#gtk_allocation.height,
	    St = get('_state'),
	    gl:enable(?GL_DEPTH_TEST),
	    gl:clearColor(0.6, 0.6, 0.5, 1.0),
	    gl:clear(?GL_COLOR_BUFFER_BIT),
	    gl:viewport(0, 0, W, H),
	    gl:matrixMode(?GL_PROJECTION),
	    gl:loadIdentity(),
	    wings_view:perspective(St),	    
	    gl:matrixMode(?GL_MODELVIEW),
	    wings_draw:render(St),
	    true
	    %% gl:matrixMode(?GL_PROJECTION),
	    %% gl:loadIdentity(),
	    %% gl:ortho(0,100, 100,0, -1,1),
	    %% gl:matrixMode(?GL_MODELVIEW),
	    %% gl:loadIdentity(),
    end.

%% When widget is exposed it's contents are redrawn.
gl_draw(Widget, Event, _) ->
    %% Draw only last expose. 
    if Event#gdk_event_expose.count > 0 ->
	    true;
       true ->
	    %% OpenGL functions can be called only 
	    %% if make_current returns true 
	    case gtk_gl_area:make_current(?GTK_GL_AREA(Widget)) of
		0 -> 
		    true;
		_ ->
		    St = get('_state'),
		    wings_draw:render(St),
		    gtk_gl_area:swap_buffers(?GTK_GL_AREA(Widget)),
		    true
	    end
    end.

gl_reshape(Widget, Event, _) ->
    %% OpenGL functions can be called only if make_current returns true
    case gtk_gl_area:make_current(?GTK_GL_AREA(Widget)) of
	0 -> ignore;
	_ ->
	    Allocation = gtk:widget_get_allocation(Widget),
	    W = Allocation#gtk_allocation.width,
	    H = Allocation#gtk_allocation.height,	    
	    St = get('_state'),
	    gl:enable(?GL_DEPTH_TEST),
	    gl:clearColor(0.6, 0.6, 0.5, 1.0),
	    gl:viewport(0, 0, W, H),
	    gl:matrixMode(?GL_PROJECTION),
	    gl:loadIdentity(),
	    wings_view:perspective(St),	    
	    gl:matrixMode(?GL_MODELVIEW)
    end,
    true.

    
main() ->
    case gdk_gl:'query'() of
	0 ->
	    io:format("OpenGL not supported\n", []),
	    exit(normal);
	_ ->
	    ok
    end,

    Window = gtk:window_new ('GTK_WINDOW_TOPLEVEL'),

    gtk:input_add(fun handle_input/2),
    
    gtk:window_set_policy(Window, true, true, true),
    gtk:widget_set_name (Window, "wings"),
    {Major,Minor} = ?WINGS_VERSION,
    Text = io_lib:format("Wings 3D  ~w.~w", [Major, Minor]),
    gtk:window_set_title(Window, lists:flatten(Text)),
    
    Vbox = gtk:vbox_new (false, 0),    
    gtk:container_add (?GTK_CONTAINER (Window), Vbox),
    gtk:widget_show (Vbox),

    gtk:signal_connect (?GTK_OBJECT (Window), 'delete_event',
			fun() -> cast(quit) end),

    Material = wings_material:default(),
    Empty = gb_trees:empty(),
    Zero = 0.0,

    St0 = #st { shapes = Empty,
		hidden = Empty,
		selmode= face,
		sel    = [],
		hsel   = Empty,
		ssel   = {face,[]},
		mat    = Material,
		saved  = true,
		opts   = #opt{},
		onext  = 0,
		last_command=ignore,
		%%  hit_buf= sdl_util:malloc(?HIT_BUF_SIZE, ?GL_UNSIGNED_INT)
		hit_buf = 0  %% FIXME
	       },
    %% fake default view..
    St1 = St0#st { origo={Zero,Zero,Zero},
		   azimuth=-45.0,elevation=25.0,
		   distance=?CAMERA_DIST,
		   pan_x=Zero,pan_y=Zero},
	    
    put('_state', St1),

    {MBar,AccelGroup} = wings_menu(Material),
    gtk:box_pack_start(?GTK_BOX(Vbox), MBar, false, false, 0),
    gtk:widget_show(MBar),

    gtk:window_add_accel_group(Window, AccelGroup),

    %% gtk:widget_show (Window),

    TBar = wings_toolbar(),
    gtk:box_pack_start(?GTK_BOX(Vbox), TBar, false, false, 0),
    gtk:widget_show(TBar),

    %% Create the drawing area (will be gtkgl_area)
    GL_area = gtk_gl_area:new({?GDK_GL_RGBA,
			      ?GDK_GL_RED_SIZE,  4,
			      ?GDK_GL_GREEN_SIZE,4,
			      ?GDK_GL_BLUE_SIZE, 4,
			      ?GDK_GL_DOUBLEBUFFER,
			      ?GDK_GL_NONE}),

    gtk:widget_set_events(?GTK_WIDGET(GL_area),
			  ?GDK_EXPOSURE_MASK bor
			  ?GDK_BUTTON_PRESS_MASK),

    gtk:signal_connect(?GTK_OBJECT(GL_area), 'expose_event',
		       fun gl_draw/3, ?NULL),
    
    gtk:signal_connect(?GTK_OBJECT(GL_area), 'configure_event',
		       fun gl_reshape/3, ?NULL),

    %% Do initialization when widget has been realized. 
    gtk:signal_connect(?GTK_OBJECT(GL_area), 'realize',
		       fun gl_init/2, ?NULL),

    gtk:widget_set_usize(?GTK_WIDGET(GL_area), 800,600),

    gtk:box_pack_start (?GTK_BOX(Vbox), GL_area, true, true, 0),
    %% ?GTK_WIDGET_SET_FLAGS(Drawing_area, ?GTK_CAN_FOCUS),
    %% gtk:widget_grab_focus(Drawing_area),


    %% gtk:widget_add_events (Drawing_area, ?GDK_KEY_PRESS_MASK),    

    %% add a separator
    Sep = gtk:hseparator_new(),
    gtk:box_pack_start(?GTK_BOX(Vbox), Sep, false, true, 1),
    gtk:widget_show(Sep),

    %%
    gtk:widget_show (GL_area),

    gtk:widget_show (Window),

    set_window_icon(Window, wings_xpms:wings_icon()),

    gtk:main (St1).    

    
    
    

