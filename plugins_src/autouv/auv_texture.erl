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
%%     $Id: auv_texture.erl,v 1.12 2006/01/12 12:50:25 dgud Exp $
%%

-module(auv_texture).
-export([get_texture/1, get_texture/2, draw_options/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").
-include("e3d.hrl").

-import(lists, [foreach/2, reverse/1, sort/1, min/1,max/1]).

-define(OPT_BG, [{type_sel,color},{undefined,ignore},{1.0,1.0,1.0}]).
-define(OPT_EDGES, [all_edges,{0.0,0.0,0.0}, 1.0, false]).
-define(OPT_FACES, [materials]).

-record(opt, {texsz = {512,512},   %% Texture size
	      no_renderers = 4,
	      renderers = [{auv_background, ?OPT_BG},
			   {auv_edges, [all_edges]}]
	     }).

-record(chart, 
	{id,        % Chart ID
	 fs,        % Faces see [{Face,#fs{}}]
	 uv,        % UV postions   (gb_tree) Id -> 2d pos
	 vmap,      % Vertex Map, from autouv to real vertex no.
	 pos,       % Real 3D position  (gb_tree) Vertex -> 3d pos
	 n,         % Normal            (gb_tree) {face,vertex} -> normal
	 uvc,       % Previous uv or vertex color (gb_tree) {face,vertex} -> uvc
	 oes=[],    % Outer vertices [[va,vb,face],[vc,vb,face]..]
	 mat,       % Material table (gb_tree)
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
     {hframe,[{label,"Color"},{color,Color}],[is_enabled(color)]}];
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

pass({auv_background,[{type_sel,color},_,{R,G,B}]}) ->  
    fun(_Geom) ->
	    gl:clearColor(R,G,B,1.0),
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT)
    end;
pass({auv_background,[{type_sel,image},{_Name,Id},{R,G,B}]}) ->
    fun(_Geom) ->
	    gl:clearColor(R,G,B,1.0),
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
    R=fun(#chart{fs=Fs,uv=UV}) ->
	      Vertex = fun(V) -> gl:vertex3fv(gb_trees:get(V, UV)) end,
	      gl:'begin'(?GL_LINES),
	      foreach(fun({_,#fs{vse=Vs=[H|_]}}) -> 
			      vs_all(Vertex,Vs,H)
		      end,Fs),
	      gl:'end'()
      end,
    fun(Charts) ->  
	    gl:disable(?GL_DEPTH_TEST),	    
	    gl:color3fv(Color),
	    gl:lineWidth(Width),
	    foreach(R, Charts) 
    end;
pass({auv_edges, [border_edges,Color,Width,UseMat]}) -> 
    R=fun(#chart{oes=Es,uv=UV,fs=Fs,mat=Mat}) ->
	      Vertex = fun(V) -> gl:vertex3fv(gb_trees:get(V, UV)) end,
	      Draw = 
		  if UseMat -> 
			  FT = gb_trees:from_orddict(sort(Fs)),
			  fun([A,B,FId]) -> 
				  Face = gb_trees:get(FId,FT),
				  gl:color4fv(get_diffuse(Face,Mat)),
				  Vertex(A),
				  Vertex(B)
			  end;
		     true ->
			  fun([A,B,_FId]) -> 
				  Vertex(A),
				  Vertex(B)
			  end
		  end,
	      gl:'begin'(?GL_LINES),
	      foreach(Draw,Es),
	      gl:'end'()
      end,
    fun(Charts) ->  
	    gl:color3fv(Color),
	    gl:lineWidth(Width),
	    gl:disable(?GL_DEPTH_TEST),
	    foreach(R, Charts) 
    end;
pass({auv_edges, _}) ->
    pass({auv_edges, ?OPT_EDGES});
pass({auv_faces, [Type]}) ->
    M = fun(#chart{uv=UV,fs=Fs,mat=Mtab}) ->
		D=fun({Id,Face=#fs{vs=Vs}}) ->
			  gl:color4fv(get_diffuse(Face,Mtab)),
			  draw(Id,Vs,material,UV,ignore,ignore)
		  end,
		gl:'begin'(?GL_TRIANGLES),
		foreach(D,Fs),
		gl:'end'()
	end,
    V = fun(#chart{uv=UV,fs=Fs,uvc=UVC,vmap=Vmap,mode=vertex}) ->
		D=fun({Id,#fs{vs=Vs}}) -> draw(Id,Vs,vertex,UV,UVC,Vmap) end,
		gl:'begin'(?GL_TRIANGLES),		
		foreach(D,Fs),
		gl:'end'();
	   (#chart{uv=UV,fs=Fs,uvc=UVC,vmap=Vmap,mat=Mtab}) ->
		D=fun({Id,Face=#fs{vs=Vs}}) ->
			  case set_diffuse_tx(Face,Mtab) of
			      false -> 
				  gl:'begin'(?GL_TRIANGLES),
				  draw(Id,Vs,material,UV,UVC,Vmap);
			      true ->  
				  gl:'begin'(?GL_TRIANGLES),
				  draw(Id,Vs,uv,UV,UVC,Vmap)
			  end,
			  gl:'end'()
		  end,
		foreach(D,Fs)
	end,
    R = case Type of
	    materials -> M;
	    texture -> V
	end,
    fun(Charts) ->  
	    gl:disable(?GL_DEPTH_TEST),
	    gl:disable(?GL_ALPHA_TEST),
	    gl:enable(?GL_BLEND),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    erase({?MODULE,use_tx}),
	    gl:disable(?GL_TEXTURE_2D),
	    foreach(R, Charts),
	    gl:disable(?GL_TEXTURE_2D)	    
    end;
pass({auv_faces, _}) ->
    pass({auv_faces,?OPT_FACES});
pass({_R, _O}) ->    
    io:format("~p:~p: Unknown Render Pass (~p) or options (~p) ~n",
	      [?MODULE,?LINE,_R,_O]),
    fun(_) -> ok end.

uvc(material,_Id,_V,_Uvc,_Vmap) -> ok;
uvc(vertex, Id, V, Uvc, Vmap) ->
    case gb_trees:get({Id,auv_segment:map_vertex(V, Vmap)},Uvc) of
	Col when size(Col) == 3 ->
	    gl:color3fv(Col);
	_ -> 
	    gl:color3fv({1.0,1.0,1.0})
    end;
uvc(_, Id,V,Uvc,Vmap) ->
    case gb_trees:get({Id,auv_segment:map_vertex(V,Vmap)},Uvc) of
	UV when size(UV) == 2 ->  
	    gl:texCoord2fv(UV);
	_ -> 
	    gl:texCoord2fv({0.0,0.0})
    end.

draw(Id,[[A,B,C]|Vs],Mode,Pos,UVC,Vmap) ->
    uvc(Mode,Id,A,UVC,Vmap),
    gl:vertex3fv(gb_trees:get(A, Pos)),
    uvc(Mode,Id,B,UVC,Vmap),
    gl:vertex3fv(gb_trees:get(B, Pos)),
    uvc(Mode,Id,C,UVC,Vmap),
    gl:vertex3fv(gb_trees:get(C, Pos)),
    draw(Id,Vs,Mode,Pos,UVC,Vmap);
draw(_,[],_,_,_,_) -> ok.

get_diffuse(#fs{mat=MatName}, Materials) ->
    Mat = gb_trees:get(MatName, Materials),
    proplists:get_value(diffuse, proplists:get_value(opengl, Mat)).

set_diffuse_tx(#fs{mat=MatName}, Materials) ->
    case get({?MODULE,use_tx}) of
	{MatName,Tx} -> Tx;
	_ ->
	    Mat = gb_trees:get(MatName, Materials),
	    Maps = proplists:get_value(maps, Mat),
	    case proplists:get_value(diffuse, Maps, none) of
		none ->
		    gl:disable(?GL_TEXTURE_2D),
		    Ogl = proplists:get_value(opengl,Mat),
		    gl:color4fv(proplists:get_value(diffuse,Ogl)),
		    put({?MODULE,use_tx},{MatName,false}),
		    false;
		Diff0 ->
		    gl:enable(?GL_TEXTURE_2D),
		    Diff = wings_image:txid(Diff0),
		    gl:bindTexture(?GL_TEXTURE_2D,Diff),
		    put({?MODULE,use_tx},{MatName,true}),
		    true
	    end
    end.

render_image(Geom,Passes,#opt{texsz={TexW,TexH}}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    Current = wings_wm:viewport(),
    {W0,H0} = wings_wm:top_size(),
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
    ?DBG("Get texture sz ~p ~p ~n", [{W,Wd},{H,Hd}]),
    set_viewport({0,0,W,H}),
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    foreach(fun(Pass) -> Pass(Geom) end, Passes),
    gl:endList(),    
    ImageBins = get_texture(0, Wd, 0, Hd, {W,H}, Dl, []),
    gl:deleteLists(Dl,1),
    ImageBin = merge_texture(ImageBins, Wd, Hd, W*3, H, []),
    set_viewport(Current),
    gl:popAttrib(),
    
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    case (TexW*TexH *3) == size(ImageBin) of
	true ->
	    #e3d_image{image=ImageBin,width=TexW,height=TexH};
	false ->
	    BinSzs = [size(Bin) || Bin <- ImageBins],
	    exit({texture_error,{TexW, TexH, size(ImageBin), 
				 W,Wd,H,Hd, BinSzs}})
    end.

get_texture(Wc, Wd, Hc, Hd, {W,H}=Info, DL, ImageAcc)
  when Wc < Wd, Hc < Hd ->
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:clearColor(1, 1, 1, 1),
    gl:shadeModel(?GL_SMOOTH),
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),
    texture_view(Wc, Wd, Hc, Hd),
    gl:callList(DL),
    gl:flush(),
    gl:readBuffer(?GL_BACK),
    Mem = sdl_util:alloc(W*H*3, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin = sdl_util:getBin(Mem),
    get_texture(Wc+1, Wd, Hc, Hd, Info, DL, [ImageBin|ImageAcc]);
get_texture(_Wc,Wd,Hc,Hd, Info, Dl, ImageAcc) when Hc < Hd ->
    get_texture(0, Wd, Hc+1, Hd, Info, Dl, ImageAcc);
get_texture(_,_,_,_,_,_,ImageAcc) -> reverse(ImageAcc).

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

calc_texsize(Vp, Tex, Orig) when Tex < Vp ->
    {Tex,Orig div Tex};
calc_texsize(Vp, Tex, Orig) ->
    calc_texsize(Vp, Tex div 2, Orig).

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
    setup_charts(St, We, Orig, Mats).
    
setup_charts(#st{shapes=Cs0},We=#we{vp=Pos},OrigWe, Mats) ->
    Ns = get_info(We,normal),
    Ex = get_info(OrigWe,uvOrCol),
    lists:map(fun(Uv = #we{id=Id, name=#ch{vmap=Vmap},vp=Uvs}) ->
		      Mat = fun(Face) -> get_material(Face,We,OrigWe) end,
		      Fs  = create_faces(Uv, Mat),
		      OEs = outer_verts(Uv), 
		      #chart{id=Id,fs=Fs,uv=Uvs,vmap=Vmap,oes=OEs,
			     pos=Pos,n=Ns,uvc=Ex,mat=Mats,mode=OrigWe#we.mode}
	      end, gb_trees:values(Cs0)).

create_faces(We, GetMat) ->
    Fs = wings_we:visible(We),
    lists:map(fun(Face) ->
		      Vs0 = wings_face:vertices_ccw(Face,We),
		      Vs = case length(Vs0) of
			       3 -> [Vs0];
			       Len -> triangulate(Vs0,Len,We)
			   end,
		      {Face,#fs{vs=Vs,vse=Vs0,mat=GetMat(Face)}}
	      end, Fs).

get_info(We = #we{fs=Ftab},What) ->
    FN0	= [{Face,wings_face:normal(Face, We)} || Face <- gb_trees:keys(Ftab)],
    FVN	= wings_we:normals(FN0, We),  %% gb_tree of {Face, [VInfo|Normal]}
    get_info(FVN, What, We, []).
get_info([{Face,[H|VsI]}|R],What,We,Acc0) ->
    Vis = wings_face:vertices_ccw(Face,We),
    Acc = zip(Vis,VsI++[H],What,Face,Acc0), %% Ugly rotate
    get_info(R,What,We,Acc);
get_info([],_,_,Acc) -> gb_trees:from_orddict(sort(Acc)).

get_material(Face, We, OrigWe) ->
    Mat1 = wings_facemat:face(Face,We),
    try
	case reverse(atom_to_list(Mat1)) of
	    "vua_" ++ _ -> 
		wings_facemat:face(Face,OrigWe);
	    _ ->
		Mat1
	end
    catch _:_ ->
	    Mat1
    end.

zip([V|Vs],[[_|N]|VsI], normal,Face,Acc) ->
    zip(Vs,VsI,normal,Face,[{{Face,V},N}|Acc]);
zip([V|Vs],[[Ex|_N]|VsI],W,Face,Acc) ->
    zip(Vs,VsI,W,Face,[{{Face,V},Ex}|Acc]);
zip([],[], _, _Face,Acc) -> Acc.

outer_verts(We = #we{es=Etab}) ->
    Fs = wings_we:visible(We),
    Outer = auv_util:outer_edges(Fs,We,false),
    Verts = fun({Edge,Face}) -> 
		    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
		    [Va,Vb,Face]
	    end,
    lists:map(Verts, Outer).

%% Workaround for ATI gl:'end' doesn't bite for line loop/strip..
vs_all(Draw,[A|R=[B|_]],Last) ->
    Draw(A),
    Draw(B),
    vs_all(Draw,R,Last);
vs_all(Draw,[B],Last) ->
    Draw(B),
    Draw(Last).

merge_mats([This={MatName,_Def}|R], Mats) ->
    case gb_trees:is_defined(MatName,Mats) of
	true ->
	    merge_mats(R,Mats);
	false ->
	    merge_mats(R,gb_trees:add(This, Mats))
    end;
merge_mats([],Mats) -> Mats.

triangulate(Vs,Len,#we{vp=Vtab}) ->
    FaceVs = lists:seq(0, Len-1),
    Vcoords = [gb_trees:get(V, Vtab) || V <- Vs],
    E3dface = #e3d_face{vs=FaceVs},
    T3dfaces = e3d_mesh:triangulate_face(E3dface, Vcoords),
    VsTuple = list_to_tuple(Vs),
    [renumber(FVs, VsTuple) || #e3d_face{vs=FVs} <- T3dfaces].

renumber(L, Vtab) ->
    renumber(L, Vtab, []).
renumber([V|Vs], Vtab, Acc) ->
    renumber(Vs, Vtab, [element(V+1, Vtab)|Acc]);
renumber([], _, Acc) -> reverse(Acc).
