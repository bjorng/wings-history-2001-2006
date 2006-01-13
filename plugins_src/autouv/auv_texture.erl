%%
%%  auv_texture.erl --
%%
%%     Render and capture a texture.
%%
%%  Copyright (c) 2002-2004 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: auv_texture.erl,v 1.14 2006/01/13 22:06:25 dgud Exp $
%%

-module(auv_texture).
-export([get_texture/1, get_texture/2, draw_options/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
-include("e3d.hrl").

-import(lists, [foreach/2, reverse/1, sort/1, foldl/3, min/1,max/1]).
-import(auv_segment, [map_vertex/2]).

-define(OPT_BG, [{type_sel,color},{undefined,ignore},{1.0,1.0,1.0}]).
-define(OPT_EDGES, [all_edges,{0.0,0.0,0.0}, 1.0, false]).
-define(OPT_FACES, [materials]).

-record(opt, {texsz = {512,512},   %% Texture size
	      no_renderers = 4,
	      renderers = [{auv_background, ?OPT_BG},
			   {auv_edges, [all_edges]}]
	     }).

-record(ts,         % What              Type
	{charts,    % #chart{}          (list)
         uv,        % UV postions       (binary)
	 pos,       % Real 3D position  (binary) 
	 n,         % Normal            (binary)
	 uvc,       % Previous uv or vertex color (binary)
	 uvc_mode   % material (uv) or vertex
	}).

-record(chart, 
	{id,        % Chart ID
	 fs,        % Faces see [{Face,#fs{}}]
	 oes=[],    % Outer vertices [[va,vb,face],[vc,vb,face]..]
	 mode       % Previous mode material/vertex-colored
	}).

-record(fs,
	{vs,        % triangulated vertex id in uv window [[Id1,Id2,Id3]]
	 vse,       % face vertex id's untriangulated for edge drawings
	 mat}).     % material

%% Menu

draw_options() ->
    [MaxTxs0|_] = gl:getIntegerv(?GL_MAX_TEXTURE_SIZE),
    MaxTxs = max([min([4096,MaxTxs0]),256]),
    Prefs = get_pref(tx_prefs, pref_to_list(#opt{})),
    TexSz = proplists:get_value(texsz, Prefs, 512),
    Qs = [{hframe,[{menu,gen_tx_sizes(MaxTxs,[]),TexSz,
		    [{key,texsz}]}],
	   [{title,"Size"}]},
	  {vframe, render_passes(Prefs), [{title,"Render"}]}],
    
    wings_ask:dialog("Draw Options", Qs,
		     fun(Options) ->
			     Opt = list_to_prefs(Options),
			     set_pref([{tx_prefs, pref_to_list(Opt)}]),
			     {auv,{draw_options,Opt}}
		     end).

render_passes(Prefs) ->
    NoOfPasses = lists:max([((length(Prefs) - 1) div 2)-1,4]),
    Menu = renderers(),
    Background = 
	{hframe, 
	 [{label, integer_to_list(0) ++ ": "},
	  {menu,[{"Background", auv_background}],auv_background,
	   [{key,{auv_pass,0}},layout]}, 
	  {value, get_def(Prefs,auv_opt,0), store_opt(0)},
	  {button,"Options",keep,[option_hook(0,background()),
				  drop_flags(0)]}],[]},
    Other = [{hframe, 
	      [{label, integer_to_list(Id) ++ ": "},
	       {menu,Menu,default_menu(Id,Prefs),
		[{key,{auv_pass,Id}},layout,pass_hook(Id)]}, 
	       {value, get_def(Prefs,auv_opt,Id), store_opt(Id)},
	       {button,"Options",keep,
		[option_hook(Id,Menu),
		 drop_flags(Id)]}],
	      []} 
	     || Id <- lists:seq(1,NoOfPasses)],
    [Background|Other].

default_menu(Pass,Prefs) -> 
    case get_def(Prefs, auv_pass, Pass) of
	ignore -> default_menu(Pass);
	Val -> Val
    end.
	    
default_menu(1) -> auv_edges; 
default_menu(_) -> ignore.

get_def(List, What, Id) ->
    case proplists:get_value({What,Id}, List) of
	undefined when What == auv_pass -> ignore;
	undefined when What == auv_opt  -> [];
	Val -> Val
    end.

background() ->
    [{"Background", auv_background}].
renderers() ->
    [{"None", ignore},
     {"Draw Edges",auv_edges},
     {"Draw Faces",auv_faces}     
    ].

options(auv_background, [{type_sel,Type},{Image,_},Color]) ->
    [{hradio,[{"Image",image},{"Color",color}],Type,[{key,type_sel},layout]},
     {hframe,[{label,"Image"},image_selector(0,Image)],[is_enabled(image)]},
     {hframe,[{label,"Color"},{color,fix(Color,have_fbo())}],[is_enabled(color)]}];
options(auv_background, _Bad) ->  
    options(auv_background, ?OPT_BG);
options(auv_edges,[Type,Color,Size,UseMat]) ->
    [{vradio,[{"Draw All Edges",all_edges},
	      {"Draw Border Edges", border_edges}], 
      Type, []},
     {hframe, [{label,"Edge Color:"}, {color, Color}]},
     {hframe, [{label,"Edge Width:"}, {text,Size,[{range, {0.0,100.0}}]}]},
     {"Use face material (on border edges)", UseMat}
    ];
options(auv_edges,_) -> options(auv_edges,?OPT_EDGES);
options(auv_faces,[Type]) ->
    [{vradio,[{"Use Material Colors",materials},
	      {"Use (previous) Texture/Vertex colors", texture}],
      Type, []}];
options(auv_faces,_) -> options(auv_faces,?OPT_FACES);
options(Command,Vals) ->
    io:format("~p: ~p~n",[Command, Vals]),
    exit(unknown_default).

image_selector(_Id,Default) ->
    Is = wings_image:images(),
    Menu = [{Name,{Name,TexId}} || {TexId, #e3d_image{name=Name}} <- Is],
    case lists:keysearch(Default,1,Menu) of 
	{value,{_,What}} -> Def = What;
	_ -> case Menu of 
		 [{_,Def}|_] -> Def;
		 _ -> Def = void
	     end
    end,
    {menu,Menu,Def,[layout]}.

is_enabled(Type) ->
    {hook, fun(is_minimized, {_Var,_I,Store}) -> 
		   case gb_trees:get(type_sel, Store) of
		       Type ->   false;
		       _Other -> true
		   end;
	      (_,_) -> void
	   end}.

option_hook(Id,Renderers) ->
    {hook, fun(is_disabled,{_Var,_I,Sto}) ->
		   gb_trees:get({auv_pass,Id}, Sto) == ignore;
	      (is_minimized, _) ->
		   false;
	      (update,{_Var,_I,_B,Sto}) ->
		   Name = gb_trees:get({auv_pass,Id},Sto),
		   render_option_dialog(Id,renderer(Name,Renderers),Sto);
	      (_,_) -> void
	   end}.

pass_hook(Id) ->
    {hook, fun(update,{Var,_I,B,Sto0}) ->
		   VarE = {auv_opt,Id},
		   Sto1 = gb_trees:enter(VarE,[],Sto0),
		   {store, gb_trees:enter(Var,B,Sto1)};
	      (_,_) -> void
	   end}.

renderer(Id,[Renderer={_,Id}|_R]) ->  Renderer;
renderer(Id,[_|R]) ->  renderer(Id,R).

render_option_dialog(Id,{StrName,Name},Sto) ->
    Fun = render_option_fun(wings_wm:this()),
    Opt = gb_trees:get({auv_opt,Id},Sto),
    wings_ask:dialog(StrName,options(Name,Opt),Fun).

render_option_fun(Parent) ->
    fun(What) -> wpa:drop(Parent, {render_opt, What}) end.

store_opt(Id) ->
    [{key,{auv_opt,Id}},
     {hook, fun(update,{Var,_I,_B={render_opt,Opt},Sto}) -> 
		    {store, gb_trees:enter(Var,Opt,Sto)};
	       (_,_) -> void
	    end}].
drop_flags(Id) ->
    {drop_flags, [{index,-1}|store_opt(Id)]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Texture Creation

get_texture(St) ->    
    Ops = list_to_prefs(get_pref(tx_prefs, list_to_prefs(#opt{}))),
    get_pref(St, Ops).
get_texture(St = #st{bb=#uvstate{}}, Options) ->
    Passes = get_passes(Options#opt.renderers),
    Ts = setup(St),
    render_image(Ts, Passes, Options).

get_passes(Passes) ->
    lists:map(fun(Pass) -> pass(Pass) end, Passes).

pass({auv_background,[{type_sel,color},_,Color]}) ->  
    fun(_Geom) ->
	    {R,G,B,A} = fix(Color,true),
	    gl:clearColor(R,G,B,A),
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT)
    end;
pass({auv_background,[{type_sel,image},{_Name,Id},Color]}) ->
    fun(_Geom) ->
	    {R,G,B,A} = fix(Color,true),
	    gl:clearColor(R,G,B,A),
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    case wings_image:txid(Id) of
		none -> ignore;
		TxId ->
 		    gl:enable(?GL_TEXTURE_2D),
		    gl:bindTexture(?GL_TEXTURE_2D, TxId),
		    gl:'begin'(?GL_QUADS),
		    gl:texCoord2f(0,0),    gl:vertex3f(0, 0, -0.99),
		    gl:texCoord2f(1,0),    gl:vertex3f(1, 0, -0.99),
		    gl:texCoord2f(1,1),    gl:vertex3f(1, 1, -0.99),
		    gl:texCoord2f(0,1),    gl:vertex3f(0, 1, -0.99),
		    gl:'end'(),
		    gl:disable(?GL_TEXTURE_2D)
	    end
    end;
pass({auv_background, _}) ->
    pass({auv_background, ?OPT_BG});
pass({auv_edges, [all_edges,Color,Width,_UseMat]}) ->
    R=fun(#chart{fs=Fs}) ->
	      Draw = fun(#fs{vse=Vs}) ->
			     Patched = vs_lines(Vs,hd(Vs)),
			     gl:drawElements(?GL_LINES,length(Patched),
					     ?GL_UNSIGNED_INT,Patched)
%%                     Doesn't work
%% 			     gl:drawElements(?GL_LINE_LOOP,length(Vs),
%% 					     ?GL_UNSIGNED_INT,Vs) 
			     end,
	      foreach(Draw,Fs)
      end,
    fun(#ts{charts=Charts}) ->  
	    gl:disable(?GL_DEPTH_TEST),	    
	    gl:color3fv(Color),
	    gl:lineWidth(Width),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    foreach(R, Charts),
	    gl:disableClientState(?GL_VERTEX_ARRAY)
    end;
pass({auv_edges, [border_edges,Color,Width,UseMat]}) -> 
    R= fun(#chart{oes=Es}) when UseMat ->
	       Draw = fun([A,B,Mat]) ->
			      gl:color4fv(get_diffuse(Mat)),
			      gl:drawElements(?GL_LINES,2,?GL_UNSIGNED_INT,[A,B])
		      end,
	       foreach(Draw,Es);
	  (#chart{oes=Es0}) ->
	       Es = foldl(fun([A,B,_],Acc) -> [A,B|Acc] end, [], Es0),
	       gl:drawElements(?GL_LINES,length(Es),?GL_UNSIGNED_INT,Es)
       end,
    fun(#ts{charts=Charts}) ->  
	    gl:color3fv(Color),
	    gl:lineWidth(Width),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:disable(?GL_DEPTH_TEST),
	    foreach(R, Charts),
	    gl:disableClientState(?GL_VERTEX_ARRAY)
    end;
pass({auv_edges, _}) ->
    pass({auv_edges, ?OPT_EDGES});

pass({auv_faces, [Type]}) ->
    fun(#ts{charts=Charts,uvc_mode=Mode}) ->  
	    gl:disable(?GL_DEPTH_TEST),
	    gl:disable(?GL_ALPHA_TEST),
	    gl:enable(?GL_BLEND),	   
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    R = case Type of
		    materials ->			
			fun(#fs{vs=Vs,mat=Mat}) ->
				gl:color4fv(get_diffuse(Mat)),
				gl:drawElements(?GL_TRIANGLES,length(Vs),
						?GL_UNSIGNED_INT,Vs)
			end;
		    _ when Mode == vertex -> 
			gl:enableClientState(?GL_COLOR_ARRAY),
			fun(#fs{vs=Vs}) ->
				gl:drawElements(?GL_TRIANGLES,length(Vs),
						?GL_UNSIGNED_INT,Vs)
			end;
		    _ -> 
			gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
			fun(#fs{vs=Vs,mat=Mat}) ->
				set_diffuse_tx(Mat),
				gl:drawElements(?GL_TRIANGLES,length(Vs),
						?GL_UNSIGNED_INT,Vs)
			end
		end,
	    erase({?MODULE,use_tx}),
	    gl:disable(?GL_TEXTURE_2D),
	    foreach(fun(#chart{fs=Fs}) -> foreach(R,Fs) end,Charts),
	    gl:disable(?GL_TEXTURE_2D),
	    gl:disableClientState(?GL_VERTEX_ARRAY),
	    gl:disableClientState(?GL_COLOR_ARRAY),
	    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY)
    end;
pass({auv_faces, _}) ->
    pass({auv_faces,?OPT_FACES});
pass({_R, _O}) ->    
    io:format("~p:~p: Unknown Render Pass (~p) or options (~p) ~n",
	      [?MODULE,?LINE,_R,_O]),
    fun(_) -> ok end.

get_diffuse(Mat) ->
    proplists:get_value(diffuse, proplists:get_value(opengl, Mat)).

set_diffuse_tx(Mat) ->
    case get({?MODULE,use_tx}) of
	Mat -> ok;
	_ ->
	    Maps = proplists:get_value(maps, Mat),
	    case proplists:get_value(diffuse, Maps, none) of
		none ->
		    gl:disable(?GL_TEXTURE_2D),
		    Ogl = proplists:get_value(opengl,Mat),
		    gl:color4fv(proplists:get_value(diffuse,Ogl)),
		    put({?MODULE,use_tx},Mat),
		    false;
		Diff0 ->
		    gl:enable(?GL_TEXTURE_2D),
		    Diff = wings_image:txid(Diff0),
		    gl:bindTexture(?GL_TEXTURE_2D,Diff),
		    put({?MODULE,use_tx},Mat),
		    true
	    end
    end.

render_image(Geom = #ts{uv=UVpos,n=Ns,uvc=Uvc,uvc_mode=Mode}, 
	     Passes,#opt{texsz={TexW,TexH}}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    Current = wings_wm:viewport(),
    UsingFbo = setup_fbo(TexW,TexH),
    {W0,H0} = case UsingFbo of
		  false -> wings_wm:top_size();
		  _ -> {TexW,TexH}
	      end,
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
%%    io:format("Get texture sz ~p ~p ~n", [{W,Wd},{H,Hd}]),
    set_viewport({0,0,W,H}),
    %% Load Pointers
    gl:vertexPointer(3, ?GL_FLOAT, 0, UVpos),
    gl:normalPointer(?GL_FLOAT, 0, Ns),
    case Mode of
	vertex -> gl:colorPointer(3,?GL_FLOAT,0,Uvc);
	_Other -> gl:texCoordPointer(2,?GL_FLOAT,0,Uvc)
    end,
    try 
	Dl = gl:genLists(1),
	gl:newList(Dl, ?GL_COMPILE),
	foreach(fun(Pass) -> Pass(Geom) end, Passes),
	gl:endList(),
	ImageBins = get_texture(0, Wd, 0, Hd, {W,H}, Dl, UsingFbo,[]),
	gl:deleteLists(Dl,1),
	ImageBin = merge_texture(ImageBins, Wd, Hd, W*3, H, []),
	set_viewport(Current),
	gl:popAttrib(),	
	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	case UsingFbo of
	    false ->
		#e3d_image{image=ImageBin,width=TexW,height=TexH};
	    FB ->
		gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, 0),
		gl:deleteFramebuffersEXT(1,[FB]),
 		#e3d_image{image=ImageBin,width=TexW,height=TexH,
 			   type=r8g8b8a8,bytes_pp=4}
	end
    catch _:What ->
	    Where = erlang:get_stacktrace(),
	    catch gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, 0),
	    catch gl:deleteFramebuffersEXT(1,[UsingFbo]),
	    gl:endList(),
	    gl:popAttrib(),
	    exit({What,Where})
    end.

have_fbo() -> wings_gl:is_ext('GL_EXT_framebuffer_object').

setup_fbo(W,H) ->
    case have_fbo() of
	false -> false;
	true ->
	    [FB] = gl:genFramebuffersEXT(1),
	    [Col] = gl:genTextures(1),
	    [Depth] = gl:genRenderbuffersEXT(1),
	    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, FB),
	    %% Init color texture
	    gl:bindTexture(?GL_TEXTURE_2D, Col),
	    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),
	    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA8, W, H, 0,
			  ?GL_RGBA, ?GL_UNSIGNED_BYTE, 0),
	    gl:framebufferTexture2DEXT(?GL_FRAMEBUFFER_EXT,
				       ?GL_COLOR_ATTACHMENT0_EXT,
				       ?GL_TEXTURE_2D, Col, 0),
	    %% Init depth texture
	    gl:bindRenderbufferEXT(?GL_RENDERBUFFER_EXT, Depth),
	    gl:renderbufferStorageEXT(?GL_RENDERBUFFER_EXT,
				      ?GL_DEPTH_COMPONENT24, W, H),
	    gl:framebufferRenderbufferEXT(?GL_FRAMEBUFFER_EXT,
					  ?GL_DEPTH_ATTACHMENT_EXT,
					  ?GL_RENDERBUFFER_EXT, Depth),
	    case check_fbo_status(FB) of
		false -> 
		    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, 0), 
		    false;
		_ ->
		    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, FB),
		    FB
	    end
    end.
	    
get_texture(Wc, Wd, Hc, Hd, {W,H}=Info, DL, UsingFbo, ImageAcc)
  when Wc < Wd, Hc < Hd ->
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:clearColor(1, 1, 1, 1),
    gl:shadeModel(?GL_SMOOTH),
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),
    texture_view(Wc, Wd, Hc, Hd),
    gl:callList(DL),
    gl:flush(),
    {Sz,Type} = 
	case UsingFbo of
	    false -> 
		gl:readBuffer(?GL_BACK),
		{3,?GL_RGB};
	    _ -> 
		gl:readBuffer(?GL_COLOR_ATTACHMENT0_EXT),
		{4,?GL_RGBA}
	end,
    Mem = sdl_util:alloc(W*H*Sz, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0,0,W,H,Type,?GL_UNSIGNED_BYTE,Mem),
    ImageBin = sdl_util:getBin(Mem),
    get_texture(Wc+1, Wd, Hc, Hd, Info, DL, UsingFbo, [ImageBin|ImageAcc]);
get_texture(_Wc,Wd,Hc,Hd, Info, Dl, UsingFbo, ImageAcc) when Hc < Hd ->
    get_texture(0, Wd, Hc+1, Hd, Info, Dl, UsingFbo, ImageAcc);
get_texture(_,_,_,_,_,_,_,ImageAcc) -> reverse(ImageAcc).

texture_view(WC, WD, HC, HD) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(WC/WD, (1+WC)/WD, HC/HD, (1+HC)/HD),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL).
    
merge_texture_cols(List, Wd, Wd, _W, _RowC, Acc) ->
    {list_to_binary(reverse(Acc)), List};
merge_texture_cols([H|R], Wc, Wd, W, RowC, Acc) ->
    SkipBytes = RowC*W,
    <<_:SkipBytes/binary, Row:W/binary,_/binary>> = H,
    merge_texture_cols(R, Wc + 1, Wd, W, RowC, [Row|Acc]).

merge_texture_rows(_ImageBins, H, H, _W, _Wd,Acc, Last) ->
    {list_to_binary(reverse(Acc)), Last};
merge_texture_rows(ImageBins, RowC, H, W, Wd, Acc, _) ->
    {Row, Rest} = merge_texture_cols(ImageBins, 0, Wd, W, RowC, []),
    merge_texture_rows(ImageBins, RowC + 1, H,W,Wd, [Row|Acc], Rest).

merge_texture([Bin],1,1,_,_,[]) ->   Bin;  %% No merge needed.
merge_texture(Bins, 1,_,_,_,[]) ->   list_to_binary(Bins);  %% No merge needed.
merge_texture([],_,_,_,_,Acc) -> 
    list_to_binary(reverse(Acc));
merge_texture(ImageBins,Wd,Hd,W,H,Acc) ->    
    {Col, Bins} = merge_texture_rows(ImageBins, 0, H, W, Wd, [], ImageBins),
    merge_texture(Bins,Wd,Hd,W,H,[Col|Acc]).

calc_texsize(Vp, Tex) ->
    calc_texsize(Vp, Tex, Tex).

calc_texsize(Vp, Tex, Orig) when Tex =< Vp ->
    {Tex,Orig div Tex};
calc_texsize(Vp, Tex, Orig) ->
    calc_texsize(Vp, Tex div 2, Orig).

check_fbo_status(FB) ->
    case gl:checkFramebufferStatusEXT(?GL_FRAMEBUFFER_EXT) of
	?GL_FRAMEBUFFER_COMPLETE_EXT ->
	    FB;
	?GL_FRAMEBUFFER_UNSUPPORTED_EXT ->
	    io:format("GL_FRAMEBUFFER_UNSUPPORTED_EXT~n",[]),
	    false;
	?GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT    ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT~n",[]),
	    false
    end.

get_pref(Key, Def) ->
    wpa:pref_get(autouv, Key, Def).
set_pref(KeyVals) ->
    wpa:pref_set(autouv, KeyVals).

pref_to_list(#opt{texsz={TexSz,_TexSz}, no_renderers=NoR, 
		  renderers=[{auv_background,Bg}|Rs]}) ->
    [{texsz, TexSz},{{auv_pass,0},auv_background},{{auv_opt,0},Bg}|
     r2list(Rs,1,NoR)].

r2list([{Type, Opts}|Rest], Id, Max) when Id < Max ->
    [{{auv_pass,Id}, Type},{{auv_opt,Id},Opts}|r2list(Rest,Id+1,Max)];
r2list([], Id, Max) when Id < Max ->
    [{{auv_pass,Id}, ignore},{{auv_opt,Id},[]}|r2list([],Id+1,Max)];
r2list([], _Id, _Max)  -> [].

list_to_prefs([{texsz, TexSz}|Rest]) ->
    {LR,Num} = listOfRenders(Rest,0, []),
    #opt{texsz={TexSz,TexSz},no_renderers=Num,
	 renderers=LR}.

listOfRenders([{{auv_pass,_},ignore},_|Rest],Id,Acc) ->
    if Id < 5 -> listOfRenders(Rest,Id+1,Acc);
       true ->   listOfRenders(Rest,Id,Acc)
    end;	    
listOfRenders([{{auv_pass,_},Type},{{auv_opt,_},Opts}|Rest],Id,Acc) ->
    listOfRenders(Rest,Id+1,[{Type,Opts}|Acc]);
listOfRenders([],Id,Acc) -> {reverse(Acc),Id}.
    
gen_tx_sizes(Sz, Acc) when Sz < 128 -> Acc;
gen_tx_sizes(Sz, Acc) ->
    Bytes = Sz*Sz*3,
    Mb = 1024*1024,
    SzStr = if
		Bytes < 1024*1024 ->
		    io_lib:format("(~pKb)",[Bytes div 1024]);
		true ->
		    io_lib:format("(~pMb)",[Bytes div Mb])
	    end,
    Str0 = io_lib:format("~px~p ", [Sz,Sz]),
    Str = lists:flatten([Str0|SzStr]),
    gen_tx_sizes(Sz div 2, [{Str,Sz}|Acc]).

set_viewport({X,Y,W,H}=Viewport) ->
    put(wm_viewport, Viewport),
    gl:viewport(X, Y, W, H).

setup(St = #st{bb=#uvstate{id=RId, st=#st{mat=Mat,shapes=Sh0},
			   orig_st=#st{mat=OrigMat,shapes=OrigSh}}}) ->
    We   = gb_trees:get(RId,Sh0),
    Orig = gb_trees:get(RId,OrigSh),
    Mats = merge_mats(gb_trees:to_list(OrigMat),Mat),
    {Charts,{_Cnt,UVpos,Vpos,Ns,Uvc}} = ?TC(setup_charts(St,We,Orig,Mats)),
    ?TC(#ts{charts=Charts,
	    uv=to_bin(UVpos,pos),
	    pos=to_bin(Vpos,pos),
	    n=to_bin(Ns,pos),
	    uvc=to_bin(Uvc,Orig#we.mode), uvc_mode=Orig#we.mode}).

setup_charts(#st{shapes=Cs0},We,OrigWe,Mats) ->
    Ns = setup_normals(We),
    Start = {0,[],[],[],[]}, %% {UvPos,3dPos,Normal,Uvc}
    Mat = fun(Face) -> get_material(Face,We,OrigWe,Mats) end,
    Setup = fun(Ch,Acc) -> setup_chart(Ch,Mat,Ns,We,OrigWe,Acc) end,
    lists:mapfoldl(Setup, Start, gb_trees:values(Cs0)).

setup_chart(Uv = #we{id=Id},Mat,Ns,WWe,OWe,State0) ->
    OEs0 = outer_verts(Uv), 
    {Fs,{OEs,State}}  = create_faces(Uv,WWe,OWe,Ns,Mat,{OEs0,State0}),
    {#chart{id=Id,fs=Fs,oes=OEs},State}.

create_faces(We = #we{vp=Vtab,name=#ch{vmap=Vmap}},
	     #we{vp=Vt3d},OWe=#we{vp=Vt3d,mode=OldMode},
	     NTab,GetMat,State) ->
    Fs = wings_we:visible(We),
    C=fun(Face,{OEs,{Cnt,UVpos,Vpos,Ns,Uvc}}) ->
	      Vs0 = wings_face:vertices_ccw(Face,We),
	      UVcoords = [gb_trees:get(V, Vtab) || V <- Vs0],
	      Coords = [gb_trees:get(map_vertex(V,Vmap),Vt3d) || V <- Vs0],
	      Normals = fix_normals(gb_trees:get(Face,NTab)),
	      OldUvc = fix_uvc(Vs0,wings_face:vinfo_ccw(Face,OWe),Vmap,OldMode,[]),
	      Len = length(Vs0),
	      FaceVs = lists:seq(0, Len-1),
	      Vs = case Len of
		       3 -> [FaceVs];
		       Len -> triangulate(FaceVs,UVcoords)
		   end,
	      Indx = fun(I) -> [V+Cnt || V <- I] end,
	      Mat = GetMat(Face),
	      {#fs{vs=Indx(Vs),vse=Indx(FaceVs),mat=Mat},
	       {map_oes(OEs,Vs0,Cnt,Face,Mat),
		{Cnt+Len,
		 lists:reverse(UVcoords) ++ UVpos,
		 lists:reverse(Coords) ++ Vpos,
		 Normals ++ Ns,
		 OldUvc ++ Uvc}}}
      end,
    lists:mapfoldl(C, State, Fs).

triangulate(FaceVs,Vcoords) ->
    E3dface = #e3d_face{vs=FaceVs},
    T3dfaces = e3d_mesh:triangulate_face(E3dface, Vcoords),
    lists:append([FVs || #e3d_face{vs=FVs} <- T3dfaces]).

map_oes([[A,B,Face]|OEs],Vs0,Cnt,Face,Mat) ->
    MA = member(A,Vs0,Cnt),
    MB = member(B,Vs0,Cnt),
    [[MA,MB,Mat]|map_oes(OEs,Vs0,Cnt,Face,Mat)];
map_oes([Other|OEs],Vs0,Cnt,Face,Mat) ->
    [Other|map_oes(OEs,Vs0,Cnt,Face,Mat)];
map_oes([],_,_,_,_) -> [].

member(Val,[Val|_],Pos) -> Pos;
member(Val,[_|R],Pos) -> member(Val,R,Pos+1).

fix_normals([H|Ns]) ->        %% Ugly rotate the order is wrong 
    fix_normals(Ns++[H],[]).  %% or different in wings_face:vinfo
fix_normals([[_|N]|R],Acc) ->
    fix_normals(R,[N|Acc]);
fix_normals([],Acc) -> Acc.

fix_uvc([V|Vs],Uvc,Vmap,Mode, Acc) ->
    Val = case find(map_vertex(V,Vmap),Uvc) of
	      Color = {_,_,_} when Mode == vertex ->  Color;
	      _ when Mode == vertex -> {1.0,1.0,1.0};
	      Uv = {_,_} -> Uv;
	      _ -> {0.0,0.0}
	  end,
    fix_uvc(Vs,Uvc,Vmap,Mode,[Val|Acc]);
fix_uvc([],_,_,_,Acc) -> Acc.

find(V, [[V|Info]|_R]) -> Info;
find(V, [_|R]) -> find(V,R);
find(_, []) -> none.

fix(OK = {_,_,_}, false) -> OK;
fix(OK = {_,_,_,_}, true) -> OK;
fix({R,G,B,_}, false) -> {R,G,B};
fix({R,G,B}, true) -> {R,G,B,1.0}.
    
setup_normals(We = #we{fs=Ftab}) ->
    FN0	= [{Face,wings_face:normal(Face, We)} || Face <- gb_trees:keys(Ftab)],
    Ns = wings_we:normals(FN0, We),  %% gb_tree of {Face, [VInfo|Normal]}    
    gb_trees:from_orddict(sort(Ns)).

get_material(Face, We, OrigWe, Materials) ->
    Mat1 = wings_facemat:face(Face,We),
    Mat = try
	      case reverse(atom_to_list(Mat1)) of
		  "vua_" ++ _ = Backup -> 
		      try wings_facemat:face(Face,OrigWe)
		      catch _:_ -> Backup
		      end;
		  _ ->
		      Mat1
	      end
	  catch _:_ ->
		  Mat1
	  end,
    gb_trees:get(Mat, Materials).

outer_verts(We = #we{es=Etab}) ->
    Fs = wings_we:visible(We),
    Outer = auv_util:outer_edges(Fs,We,false),
    Verts = fun({Edge,Face}) -> 
		    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		    [Va,Vb,Face]
	    end,
    lists:map(Verts, Outer).

%% Start with 64 bytes so that binary will be reference counted 
%% and not on the process heap spent hours debugging this.. :-(
to_bin(List, pos) -> to_bin3(List,[<<0:512>>]);
to_bin(List, vertex) -> to_bin3(List,[<<0:512>>]);  %% Vertex colors
to_bin(List, material) -> to_bin2(List,[<<0:512>>]).  %% UV coords.

to_bin3([{A,B,C}|R],Acc) -> 
    to_bin3(R,[<<A:32/native-float,B:32/native-float,C:32/native-float>>|Acc]);
to_bin3([],Acc) -> list_to_binary(Acc).
to_bin2([{A,B}|R],Acc) -> 
    to_bin2(R,[<<A:32/native-float,B:32/native-float>>|Acc]);
to_bin2([],Acc) -> list_to_binary(Acc).
%
% Workaround for ATI gl:'end' doesn't bite for line loop/strip..
vs_lines([A|R=[B|_]],Last) ->
    [A,B|vs_lines(R,Last)];
vs_lines([B],Last) ->
    [B,Last].

merge_mats([This={MatName,_Def}|R], Mats) ->
    case gb_trees:is_defined(MatName,Mats) of
	true ->
	    merge_mats(R,Mats);
	false ->
	    merge_mats(R,gb_trees:add(This, Mats))
    end;
merge_mats([],Mats) -> Mats.
