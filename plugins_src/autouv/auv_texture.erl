%% File    : auv_texture.erl
%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%% Description : Renders and capture a texture
%%
%% Created : 24 Jan 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%-------------------------------------------------------------------
%%  Copyright (c) 2002-2004 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv_texture.erl,v 1.3 2004/02/13 08:02:41 dgud Exp $

-module(auv_texture).
-export([get_texture/1, get_texture/2, draw_options/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").
-include("auv.hrl").

-import(lists, [foreach/2, reverse/1, min/1,max/1]).

-record(opt, {texsz = {512, 512},   %% Texture size
		texbg = false,        %% Texture background
		color = true,         %% Texture drawing options
		edges = all_edges,    %% Draw edges ??
		edge_color = false,   %% Use vertex/face color on edges
		edge_width = 2.0      %% Edge Thickness (overdraw help)
	       }).

%% Menu

draw_options() ->
    [MaxTxs0|_] = gl:getIntegerv(?GL_MAX_TEXTURE_SIZE),
    MaxTxs = min([4096,MaxTxs0]),
    Option = list_to_prefs(get_pref(draw_prefs, pref_to_list(#opt{}))),
    
    Qs = [{vradio,[{"Draw All Edges",    all_edges},
		   {"Draw Border Edges", border_edges},
		   {"Don't Draw Edges",  no_edges}], 
	   Option#opt.edges, [{title,"Edge Options"}]},
	  {vframe,[{"Use Face/Vertex Color on Border Edges", Option#opt.edge_color},
		   {label_column, [{"Edge width",  {text, Option#opt.edge_width}}]}],
	   [{title, "Overdraw options"}]},
	  {vframe,[{"Show Colors (or texture)",Option#opt.color},
		   {"Texture Background (if available)", Option#opt.texbg}],
	   [{title, "Display Color and texture?"}]},
	  {vradio,gen_tx_sizes(MaxTxs, []),element(1, Option#opt.texsz),
	   [{title,"Texture Size"}]}],
    wings_ask:dialog("Draw Options", Qs,
		     fun(Options) ->
			     Opt = list_to_prefs(Options),
			     set_pref([{draw_prefs, pref_to_list(Opt)}]),
			     {auv,{draw_options,Opt}}
		     end).

get_pref(Key, Def) ->
    wpa:pref_get(autouv, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(autouv, KeyVals).

pref_to_list(#opt{edges=EMode,edge_color=BEC,
		  edge_width=BEW, color=Color, 
		  texbg=TexBg, texsz={TexSz,_TexSz}}) ->
    [{edges,EMode}, {edge_color,BEC},{edge_width,BEW}, 
     {color, Color},{texbg,TexBg}, {texsz,TexSz}].

list_to_prefs([{edges,EMode}, {edge_color,BEC},{edge_width,BEW}, 
	       {color, Color},{texbg,TexBg}, {texsz,TexSz}]) ->
    #opt{edges=EMode,edge_color=BEC,
	 edge_width=BEW, color=Color, 
	 texbg=TexBg, texsz={TexSz,TexSz}};
list_to_prefs([EMode,BEC,BEW,Color,TexBg,TexSz]) ->
    #opt{edges=EMode,edge_color=BEC,
	 edge_width=BEW, color=Color, 
	 texbg=TexBg, texsz={TexSz,TexSz}}.

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

%%% Texture Creation

get_texture(Uvs) ->    
    Ops = list_to_prefs(get_pref(draw_prefs, list_to_prefs(#opt{}))),
    get_pref(Uvs, Ops).
get_texture(#uvstate{st=#st{mat=Mats}, areas=As, matname=MatN}, Options) ->
    #opt{texsz={TexW,TexH}, texbg=TexBg} = Options,
    MI = {Mats,TexBg,MatN},
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    Current = wings_wm:viewport(),
    {W0,H0} = wings_wm:top_size(),
    {W,Wd} = calc_texsize(W0, TexW),
    {H,Hd} = calc_texsize(H0, TexH),
    ?DBG("Get texture sz ~p ~p ~n", [{W,Wd},{H,Hd}]),
    set_viewport({0,0,W,H}),
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    foreach(fun(#we{}=We) ->
		    draw_area(We, Options, Mats)
	    end, gb_trees:values(As)),
	   gl:endList(),    
    ImageBins = get_texture(0, Wd, 0, Hd, {W,H}, Dl, MI, []),
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
		 
get_texture(Wc, Wd, Hc, Hd, {W,H}=Info, DL, MI = {Mats,TexBg,MatN}, ImageAcc)
  when Wc < Wd, Hc < Hd ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    gl:clearColor(1, 1, 1, 1),
    gl:shadeModel(?GL_SMOOTH),
    gl:disable(?GL_CULL_FACE),
    gl:disable(?GL_LIGHTING),
    texture_view(Wc, Wd, Hc, Hd, TexBg, MatN,Mats),
    gl:callList(DL),
    gl:flush(),
    gl:readBuffer(?GL_BACK),
    Mem = sdl_util:alloc(W*H*3, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin = sdl_util:getBin(Mem),
    get_texture(Wc+1, Wd, Hc, Hd, Info, DL, MI, [ImageBin|ImageAcc]);
get_texture(_Wc,Wd,Hc,Hd, Info, Dl, Mi, ImageAcc) when Hc < Hd ->
    get_texture(0, Wd, Hc+1, Hd, Info, Dl, Mi, ImageAcc);
get_texture(_, _, _, _, _, _, _, ImageAcc) -> reverse(ImageAcc).

texture_view(WC, WD, HC, HD, TexBg, MatN, Mats) ->
    gl:disable(?GL_DEPTH_TEST),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(WC/WD, (1+WC)/WD, HC/HD, (1+HC)/HD),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:color3f(1.0, 1.0, 1.0),
    case TexBg of
	true -> wings_material:apply_material(MatN, Mats);
	false -> ok
    end,
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0,0),    gl:vertex3f(0, 0, -0.9),
    gl:texCoord2f(1,0),    gl:vertex3f(1, 0, -0.9),
    gl:texCoord2f(1,1),    gl:vertex3f(1, 1, -0.9),
    gl:texCoord2f(0,1),    gl:vertex3f(0, 1, -0.9),
    gl:'end'(),
    gl:disable(?GL_TEXTURE_2D),
    gl:enable(?GL_DEPTH_TEST).

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


%%%
draw_area(#we{name=#ch{fs=Fs}}=We,
	  #opt{color=ColorMode,edges=EdgeMode}=Options, Materials) -> 
    gl:pushMatrix(),
    gl:lineWidth(Options#opt.edge_width),
    Tbe = auv_util:outer_edges(Fs, We),
    %% Draw Materials and Vertex Colors
    case EdgeMode of
	border_edges ->
	    %% Draw outer edges only
	    #we{es=Etab,vp=Vtab}=We,
	    gl:pushMatrix(),
	    DrawEdge = 
		case We#we.mode of
		    material when Options#opt.edge_color == true -> 
			gl:translatef(0,0,-0.5),
			fun({Edge,Face}) ->
				#edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
				gl:color4fv(wpc_autouv:get_material(Face,Materials,We)),
				gl:vertex3fv(wings_vertex:pos(Va, Vtab)),
				gl:vertex3fv(wings_vertex:pos(Vb, Vtab))
			end;
		    vertex when Options#opt.edge_color == true -> 
			gl:translatef(0,0,-0.5),
			fun({Edge,_}) ->
				#edge{vs=Va, a=VaC, ve=Vb, b=VbC} =
				    gb_trees:get(Edge, Etab),
				gl:color3fv(VaC),
				gl:vertex3fv(wings_vertex:pos(Va, Vtab)),
				gl:color3fv(VbC),
				gl:vertex3fv(wings_vertex:pos(Vb, Vtab))
			end;
		    _ ->
			gl:translatef(0,0,0.5),
			fun({Edge, _}) ->
				#edge{vs = Va, ve = Vb} =
				    gb_trees:get(Edge, Etab),
				gl:vertex3fv(wings_vertex:pos(Va, Vtab)),
				gl:vertex3fv(wings_vertex:pos(Vb, Vtab))
			end
		end,
	    gl:glBegin(?GL_LINES),
	    gl:color3f(0.6, 0.6, 0.6),
	    lists:foreach(DrawEdge, Tbe),
	    gl:glEnd(),
	    gl:popMatrix();
	all_edges ->
	    gl:pushMatrix(),
	    gl:translatef(0, 0, 0.9),
	    gl:color3f(0.6, 0.6, 0.6),
	    draw_all_face_edges(Fs, We),
	    gl:popMatrix();
	no_edges ->
	    ok
    end,
    if
	ColorMode == true ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    MatName = wings_material:get(hd(Fs), We),
	    wings_material:apply_material(MatName, Materials),
	    lists:foreach(fun(Face) ->
				  gl:color4fv(wpc_autouv:get_material(Face, Materials, We)),
				  draw_faces([Face], We)
			  end, Fs),
	    case wpc_autouv:has_texture(MatName, Materials) of
		true -> gl:disable(?GL_TEXTURE_2D);
		false -> ignore
	    end;
	is_tuple(ColorMode), size(ColorMode) == 4 ->
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:color4fv(ColorMode),
	    draw_faces(Fs, We#we{mode = material});
	true ->
	    ignore
    end,
    gl:popMatrix().

draw_all_face_edges([F|Fs], We) ->
    draw_face_edges(F, We),
    draw_all_face_edges(Fs, We);
draw_all_face_edges([], _) -> ok.

draw_face_edges(Face, #we{vp=Vtab}=We) ->
    Vs = wings_face:vertices_cw(Face, We),
    draw_face_edges_1(Vs, Vtab, []).

draw_face_edges_1([V|Vs], Vtab, Acc) ->
    draw_face_edges_1(Vs, Vtab, [gb_trees:get(V, Vtab)|Acc]);
draw_face_edges_1([], _, VsPos) ->
    gl:'begin'(?GL_LINE_LOOP),
    foreach(fun(P) -> gl:vertex3fv(P) end, VsPos),
    gl:'end'().

draw_faces(Fs, We) ->
    Draw = fun(Face) -> face(Face, We) end,
    wings_draw_util:begin_end(fun() -> foreach(Draw, Fs) end).

%% XXX Wrong.
face(Face, #we{mode=material}=We) ->
    wings_draw_util:plain_face(Face, We);
face(Face, #we{mode=vertex}=We) ->
    wings_draw_util:vcol_face(Face, We).

set_viewport({X,Y,W,H}=Viewport) ->
    put(wm_viewport, Viewport),
    gl:viewport(X, Y, W, H).
