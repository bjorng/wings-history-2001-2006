%%
%%  wings_image.erl --
%%
%%     This module manages images.
%%
%%  Copyright (c) 2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_image.erl,v 1.30 2003/10/21 12:02:26 bjorng Exp $
%%

-module(wings_image).
-export([init/0,init_opengl/0,
	 from_file/1,new/2,new_temp/2,create/1,
	 rename/2,txid/1,info/1,images/0,
	 bumpid/1, normal_cubemapid/0,
	 next_id/0,delete_older/1,delete_from/1,delete/1,
	 update/2,update_filename/2,draw_preview/5,
	 window/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").
-import(lists, [reverse/1,foreach/2,flatten/1]).

init() ->
    spawn_opt(fun server/0, [link,{fullsweep_after,0}]).

init_opengl() ->
    req(init_opengl).

%%%
%%% Client API.
%%%

from_file(Filename) ->
    Props = [{filename,Filename},{alignment,1}],
    case wpa:image_read(Props) of
	#e3d_image{}=Image ->
	    Name = filename:rootname(filename:basename(Filename)),
	    req({new,Image#e3d_image{filename=Filename,name=Name},false});
	{error,_}=Error -> Error
    end.

new(Name, E3DImage) ->
    req({new,E3DImage#e3d_image{name=Name},false}).

new_temp(Name, E3DImage) ->
    req({new,E3DImage#e3d_image{name=Name},true}).

create(St) ->
    create_image(),
    St.

rename(Id, NewName) ->
    req({rename,Id,NewName}).

txid(Id) ->
    req({txid,Id}, false).

bumpid(Id) ->
    req({bumpid,Id}, false).

normal_cubemapid() ->
    req(normalCM, false).

info(Id) ->
    req({info,Id}, false).

images() ->
    req(images, false).

next_id() ->
    req(next_id, false).

delete_older(Id) ->
    req({delete_older,Id}).

delete_from(Id) ->
    req({delete_from,Id}).

delete(Id) ->
    req({delete,Id}).

update(Id, Image) ->
    req({update,Id,Image}).

update_filename(Id, Filename) ->
    req({update_filename,Id,Filename}).

draw_preview(X, Y, W, H, Id) ->
    req({draw_preview,X,Y,W,H,Id}, false).

req(Req) ->
    req(Req, true).

req(Req, Notify) ->
    Self = self(),
    Ref = make_ref(),
    wings_image ! {Self,Ref,Req},
    receive
	{Ref,Answer} ->
	    case Notify of
		false -> ok;
		true -> wings_wm:notify(image_change)
	    end,
	    Answer
    end.

%%%
%%% Server implementation.
%%%
%%% Reason for using a server: Convenient encapsulation of images.
%%% We'll get an entire process dictionary to use for texture ids.
%%% 

-record(ist,
	{next=0,				%Next image ID.
	 images					%All images (gb_trees).
	}).

server() ->
    register(wings_image, self()),
    loop(#ist{images=gb_trees:empty()}).

loop(S0) ->
    receive
	{Client,Ref,Req} ->
	    case handle(Req, S0) of
		#ist{}=S ->
		    Client ! {Ref,ok};
		{Resp,S} ->
		    Client ! {Ref,Resp}
	    end,
	    loop(S);
	Other ->
	    exit({bad_message_to_wings_image,Other})
    end.

handle(init_opengl, #ist{images=Images}=S) ->
    foreach(fun({Id,Image}) ->
		    make_texture(Id, Image)
	    end, gb_trees:to_list(Images)),
    init_background_tx(),
    S;
handle({new,#e3d_image{name=Name0}=Im0,false}, #ist{next=Id,images=Images0}=S) ->
    Name = make_unique(Name0, Images0),
    Im = maybe_convert(Im0#e3d_image{name=Name}),
    Images = gb_trees:insert(Id, Im, Images0),
    make_texture(Id, Im),
    {Id,S#ist{next=Id+1,images=Images}};
handle({new,#e3d_image{name=Name}=Im,true}, #ist{images=Images}=S0) ->
    Prev = [Id || {Id,#e3d_image{name=N}} <- gb_trees:to_list(Images),
		 N =:= Name],
    case Prev of
	[] ->
	    handle({new,Im,false}, S0);
	[Id] ->
	    S = handle({delete,Id}, S0),
	    handle({new,Im,false}, S)
    end;
handle({rename,Id,Name0}, #ist{images=Images0}=S) ->
    Name = make_unique(Name0, gb_trees:delete(Id, Images0)),
    Im0 = gb_trees:get(Id, Images0),
    Im = Im0#e3d_image{name=Name},
    Images = gb_trees:update(Id, Im, Images0),
    {Id,S#ist{images=Images}};
handle({txid,Id}, S) ->
    {case get(Id) of
	 undefined -> none;
	 TxId -> TxId
     end,S};
handle({bumpid,Id}, S) ->
    {case get({Id,bump}) of
	 undefined -> 
	     create_bump(Id, S);
	 TxId -> 
	     TxId
     end,S};
handle(normalCM, S) ->
    {case get(normalCM) of
	 undefined -> 
	     create_normal_cube_map();
	 TxId -> 
	     TxId
     end,S};
handle({info,Id}, #ist{images=Images}=S) ->
    case gb_trees:lookup(Id, Images) of
	{value,E3D} -> {E3D,S};
	none -> {none,S}
    end;
handle(images, #ist{images=Images}=S) ->
    {gb_trees:to_list(Images),S};
handle(next_id, #ist{next=Id}=S) ->
    {Id,S};
handle({delete,Id}, S) ->
    delete(Id, S);
handle({delete_older,Id}, S) ->
    delete_older(Id, S);
handle({delete_from,Id}, S) ->
    delete_from(Id, S);
handle({update,Id,Image}, S) ->
    do_update(Id, Image, S);
handle({update_filename,Id,NewName}, #ist{images=Images0}=S) ->
    Im0 = gb_trees:get(Id, Images0),
    Im = Im0#e3d_image{filename=NewName},
    Images = gb_trees:update(Id, Im, Images0),
    S#ist{images=Images};
handle({draw_preview,X,Y,W,H,Id}, S) ->
    {case get(Id) of
	 undefined -> error;
	 TxId -> draw_image(X, Y, W, H, TxId)
     end,S}.

create_bump(Id, #ist{images=Images0}) ->
    case gb_trees:lookup(Id, Images0) of
	{value, E3D} -> 
	    %% Scale ?? 4 is used in the only example I've seen.
	    Bump = e3d_image:height2normal(E3D, 4),
	    make_texture({Id,bump}, Bump);
	_ ->
	    none
    end.

create_normal_cube_map() ->
    case wings_util:is_gl_ext('GL_ARB_texture_cube_map') of
	true ->	
	    [CubeMap] = gl:genTextures(1),
	    gl:bindTexture(?GL_TEXTURE_CUBE_MAP, CubeMap),
	    make_normalize_vector_cubemap(32),
	    put(normalCM,CubeMap),
	    CubeMap;
	false ->
	    none
    end.

maybe_convert(#e3d_image{type=Type0,order=Order}=Im) ->
    case {img_type(Type0),Order} of
	{Type0,lower_left} -> Im;
	{Type,_} -> e3d_image:convert(Im, Type, 1, lower_left)
    end.

img_type(b8g8r8) -> r8g8b8;
img_type(b8g8r8a8) -> r8g8b8a8;
img_type(Type) -> Type.

init_background_tx() ->
    White = [255,255,255],
    Grey = [204,204,204],
    EightWhite = pattern_repeat(8, White),
    EightGrey = pattern_repeat(8, Grey),
    B0 = [pattern_repeat(8, [EightGrey|EightWhite])|
	  pattern_repeat(8, [EightWhite|EightGrey])],
    B = list_to_binary(B0),
    Im = #e3d_image{width=16,height=16,image=B},
    put(background, init_texture(Im)).

make_texture(Id, Image) ->
    TxId = init_texture(Image),
    put(Id, TxId),
    TxId.

init_texture(Image) ->
    [TxId] = gl:genTextures(1),
    init_texture(Image, TxId).

init_texture(Image0, TxId) ->
    Image = maybe_scale(Image0),
    #e3d_image{width=W,height=H,image=Bits} = Image,
    gl:pushAttrib(?GL_TEXTURE_BIT),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    Format = texture_format(Image),
    gl:texImage2D(?GL_TEXTURE_2D, 0, internal_format(Format),
		  W, H, 0, Format, ?GL_UNSIGNED_BYTE, Bits),
    gl:popAttrib(),
    TxId.

maybe_scale(#e3d_image{width=W0,height=H0,bytes_pp=BytesPerPixel,
		       image=Bits0}=Image) ->
    case {nearest_power_two(W0),nearest_power_two(H0)} of
	{W0,H0} -> Image;
	{W,H} ->
	    Out = sdl_util:alloc(BytesPerPixel*W*H, ?GL_UNSIGNED_BYTE),
	    Format = texture_format(Image),
	    glu:scaleImage(Format, W0, H0, ?GL_UNSIGNED_BYTE,
			   Bits0, W, H, ?GL_UNSIGNED_BYTE, Out),
	    Bits = sdl_util:getBin(Out),
	    Image#e3d_image{width=W,height=H,image=Bits}
    end.

nearest_power_two(N) when (N band -N) =:= N -> N;
nearest_power_two(N) -> nearest_power_two(N, 1).

nearest_power_two(N, B) when B > N -> B bsr 1;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).

texture_format(#e3d_image{type=r8g8b8}) -> ?GL_RGB;
texture_format(#e3d_image{type=r8g8b8a8}) -> ?GL_RGBA;
texture_format(#e3d_image{type=b8g8r8}) -> ?GL_BGR;
texture_format(#e3d_image{type=b8g8r8a8}) -> ?GL_BGRA;
texture_format(#e3d_image{type=g8}) -> ?GL_LUMINANCE;
texture_format(#e3d_image{type=a8}) -> ?GL_ALPHA.

internal_format(Type) ->
    Compress = wings_util:is_gl_ext({1,3}, 'GL_ARB_texture_compression'),
    internal_format(Type, Compress).

internal_format(?GL_BGR, false) -> ?GL_RGB;
internal_format(?GL_BGRA, false) -> ?GL_RGBA;
internal_format(Else, false) -> Else;
internal_format(?GL_BGR, true) -> ?GL_COMPRESSED_RGB;
internal_format(?GL_BGRA, true) -> ?GL_COMPRESSED_RGBA;
internal_format(?GL_ALPHA, true) -> ?GL_COMPRESSED_ALPHA;
internal_format(?GL_LUMINANCE, true) -> ?GL_COMPRESSED_LUMINANCE;
internal_format(?GL_LUMINANCE_ALPHA, true) -> ?GL_COMPRESSED_LUMINANCE_ALPHA;
internal_format(?GL_INTENSITY, true) -> ?GL_COMPRESSED_INTENSITY;
internal_format(?GL_RGB, true) ->  ?GL_COMPRESSED_RGB;
internal_format(?GL_RGBA, true) -> ?GL_COMPRESSED_RGBA;  
internal_format(Else, _) -> Else.

delete(Id, #ist{images=Images0}=S) ->
    gl:deleteTextures(1, [erase(Id)]),
    delete_bump(Id),
    Images = gb_trees:delete(Id, Images0),
    S#ist{images=Images}.

delete_older(Id, #ist{images=Images0}=S) ->
    Images1 = delete_older_1(gb_trees:to_list(Images0), Id),
    Images = gb_trees:from_orddict(Images1),
    S#ist{images=Images}.

delete_older_1([{Id,_}|T], Limit) when Id < Limit ->
    gl:deleteTextures(1, [erase(Id)]),
    delete_bump(Id),
    delete_older_1(T, Limit);
delete_older_1(Images, _) -> Images.

delete_from(Id, #ist{images=Images0}=S) ->
    Images1 = delete_from_1(gb_trees:to_list(Images0), Id, []),
    Images = gb_trees:from_orddict(Images1),
    S#ist{images=Images}.

delete_from_1([{Id,_}=Im|T], Limit, Acc) when Id < Limit ->
    delete_from_1(T, Limit, [Im|Acc]);
delete_from_1([{Id,_}|T], Limit, Acc) ->
    gl:deleteTextures(1, [erase(Id)]),
    delete_bump(Id),
    delete_from_1(T, Limit, Acc);
delete_from_1([], _, Acc) -> reverse(Acc).

delete_bump(Id) ->
    case erase({Id,bump}) of
	undefined ->
	    ok;
	Bid ->
	    gl:deleteTextures(1, [Bid])
    end.

do_update(Id, In = #e3d_image{width=W,height=H,type=Type}, 
	  #ist{images=Images0}=S) ->
    Im0 = #e3d_image{filename=File,name=Name} = gb_trees:get(Id, Images0),
    Im   = In#e3d_image{filename=File, name=Name},
    TxId = get(Id),
    Images = gb_trees:update(Id, Im, Images0),
    Size = {Im0#e3d_image.width, Im0#e3d_image.height, Im0#e3d_image.type},
    case Size of
	{W,H,Type} ->
	    gl:bindTexture(?GL_TEXTURE_2D, TxId),
	    gl:texSubImage2D(?GL_TEXTURE_2D, 0, 0, 0,
			     W, H, texture_format(Im), 
			     ?GL_UNSIGNED_BYTE, Im#e3d_image.image);
	_ ->	    
	    init_texture(Im, TxId)
    end,
    case get({Id,bump}) of
	undefined ->     
	    S#ist{images=Images};
	_Bid -> 
	    Bump = e3d_image:height2normal(Im, 4),
	    do_update({Id,bump}, Bump, S#ist{images=Images})
    end.

make_unique(Name, Images0) ->
    Images = [N || #e3d_image{name=N} <- gb_trees:values(Images0)],
    wings_util:unique_name(Name, Images).

%%%
%%% Window for image.
%%%

window(Id) ->
    Name = {image,Id},
    case wings_wm:is_window(Name) of
	true ->
	    wings_wm:raise(Name);
	false ->
	    {Size,Title} = window_params(Id),
	    Pos = {10,50,highest},
	    Op = {seq,push,window_fun(Id)},
	    wings_wm:toplevel(Name, Title, Pos, Size,
			      [resizable,closable], Op)
    end.

window_params(Id) ->
    #e3d_image{width=W0,height=H0,name=Name,bytes_pp=BytesPerPixel} = info(Id),
    Title = flatten(io_lib:format("Image: ~s [~wx~wx~w]",
				  [Name,W0,H0,8*BytesPerPixel])),
    {DeskW,DeskH} = wings_wm:win_size(desktop),
    W = if
	    W0 < 250 -> 250;
	    W0+50 < DeskW -> W0+2;
	    true -> DeskW - 50
	end,
    H = if
	    H0+70 < DeskH -> H0+2;
	    true -> DeskH - 70
	end,
    {{W,H},Title}.

window_fun(Id) ->
    fun(Ev) ->
	    event(Ev, Id)
    end.

event(redraw, Id) ->
    redraw(Id),
    keep;
event(close, _) -> delete;
event(_, _) -> keep.

redraw(Id) ->
    case info(Id) of
	none ->
	    wings_wm:later(close),
	    keep;
	Im -> redraw_1(Id, Im)
    end.

redraw_1(Id, #e3d_image{width=Iw,height=Ih}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    Aspect = Iw/Ih,
    {W0,H0} = wings_wm:win_size(),
    wings_io:ortho_setup(),
    wings_io:border(0, 0, W0-1, H0-1, ?PANE_COLOR),
    X = 1,
    Y = 1,
    W1 = W0-2,
    H1 = H0-2,
    W2 = round(Aspect*H1),
    H2 = round(W1/Aspect),
    {W,H} = if
		W2 =< W1 -> {W2,H1};
		true -> {W1,H2}
	    end,
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:disable(?GL_DEPTH_TEST),
    draw_background(X, Y, W, H),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    draw_image(X, Y, W, H, txid(Id)),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:popAttrib().

draw_background(X, Y, W, H) ->
    Ua = 0,
    Ub = 16*(W div 16)/16,
    Va = 0,
    Vb = 16*(H div 16)/16,
    gl:bindTexture(?GL_TEXTURE_2D, txid(background)),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(Ua, Va),
    gl:vertex2i(X, Y),
    gl:texCoord2f(Ua, Vb),
    gl:vertex2i(X, Y+H),
    gl:texCoord2f(Ub, Vb),
    gl:vertex2i(X+W, Y+H),
    gl:texCoord2f(Ub, Va),
    gl:vertex2i(X+W, Y),
    gl:'end'().

draw_image(X, Y, W, H, TxId) ->
    Ua = 0, Ub = 1,
    Va = 1, Vb = 0,
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2i(Ua, Va),
    gl:vertex2i(X, Y),
    gl:texCoord2i(Ua, Vb),
    gl:vertex2i(X, Y+H),
    gl:texCoord2i(Ub, Vb),
    gl:vertex2i(X+W, Y+H),
    gl:texCoord2i(Ub, Va),
    gl:vertex2i(X+W, Y),
    gl:'end'().

%%%
%%% Creating images with pre-defined patterns.
%%%

create_image() ->
    Qs = [{"Width",256,[{range,{8,1024}}]},
	  {"Height",256,[{range,{8,1024}}]},
	  {"Pattern",{menu,[{"Grid",grid},
			    {"Checkerboard",checkerboard},
			    {"Vertical Bars",vbars},
			    {"Horizontal Bars",hbars},
			    {"White",white},
			    {"Black",black}],
		      grid}}],
    wings_ask:ask("Create Image", Qs,
		  fun([W,H,Pattern]) ->
			  create_image_1(Pattern, W, H),
			  ignore
		  end).

create_image_1(Pattern, W, H) ->
    Pixels = pattern(Pattern, W, H),
    Im = #e3d_image{width=W,height=H,image=Pixels,order=upper_left},
    new(atom_to_list(Pattern), Im).

pattern(grid, W, H) ->
    grid(W, H);
pattern(checkerboard, W, H) ->
    checkerboard(W, H);
pattern(vbars, W, H) ->
    vertical_bars(W, H);
pattern(hbars, W, H) ->
    horizontal_bars(W, H);
pattern(white, W, H) ->
    all_white(W, H);
pattern(black, W, H) ->
    all_black(W, H).

%% Generate a grid image.
grid(Width, Height) ->
    White = [255,255,255],
    Black = [0,0,0],
    WhiteRow = pattern_repeat(Width, White),
    BlackRow = pattern_repeat(14, Black),
    R0 = pattern_repeat(14*(Width div 16), [White,BlackRow|White]),
    R = [WhiteRow,R0|WhiteRow],
    All = pattern_repeat(Height div 16, R),
    list_to_binary(All).

%% Generate a checkerboard image of 4x4 squares 
%% with given side length in pixels.
checkerboard(Width, Height) ->
    White = [255,255,255],
    Black = [0,0,0],
    FourWhite = pattern_repeat(4, White),
    FourBlack = pattern_repeat(4, Black),
    R1 = pattern_repeat(Width div 8, [FourBlack|FourWhite]),
    R2 = pattern_repeat(Width div 8, [FourWhite|FourBlack]),
    R8 = [pattern_repeat(4, [R1])|pattern_repeat(4, [R2])],
    list_to_binary(pattern_repeat(Height div 8, R8)).

%% Generate a vertical bars image of 4 pixels width 
%% with given side length in pixels.
vertical_bars(Width, Height) ->
    W = [255,255,255],
    B = [0,0,0],
    W4 = pattern_repeat(4, W),
    B4 = pattern_repeat(4, B),
    R = pattern_repeat(Width div 8, [B4|W4]),
    R8 = pattern_repeat(8, [R]),
    list_to_binary(pattern_repeat(Height div 8, [R8])).

%% Generate a horizontal bars image of 4 pixels width 
%% with given side length in pixels.
horizontal_bars(Width, Height) ->
    W = [255,255,255],
    B = [0,0,0],
    W8 = pattern_repeat(8, W),
    B8 = pattern_repeat(8, B),
    WR4 = pattern_repeat(4*(Width div 8), [W8]),
    BR4 = pattern_repeat(4*(Width div 8), [B8]),
    list_to_binary(pattern_repeat(Height div 8, [BR4|WR4])).

%% Generate an all white image
%% with given side length in pixels.
all_white(Width, Height) ->
    solid(Width, Height, [255,255,255]).

%% Generate an all white image
%% with given side length in pixels.
all_black(Width, Height) ->
    solid(Width, Height, [0,0,0]).

solid(Width, Height, Point) ->
    P8 = pattern_repeat(8, Point),
    R = pattern_repeat(Width div 8, P8),
    R8 = pattern_repeat(8, R),
    list_to_binary(pattern_repeat(Height div 8, R8)).

pattern_repeat(0, _) -> [];
pattern_repeat(1, D) -> [D];
pattern_repeat(N, D) ->
    B = pattern_repeat(N div 2, D),
    case N rem 2 of
	0 -> [B|B];
	1 -> [D,B|B]
    end.

%% Creating Normal-Cubemap

%% Initialize a cube map texture object that generates RGB values
%% that when expanded to a [-1,1] range in the texture-unit
%% form a normalized vector matching the per-pixel vector used to
%% access the cube map.

make_normalize_vector_cubemap(Size) ->
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_CUBE_MAP, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),

    Sides = [?GL_TEXTURE_CUBE_MAP_POSITIVE_X, ?GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
	     ?GL_TEXTURE_CUBE_MAP_POSITIVE_Y, ?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
	     ?GL_TEXTURE_CUBE_MAP_POSITIVE_Z, ?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z],
    lists:foreach(fun(Side) -> make_cube_map(Side,Size-1,Size-1,Size, []) end, Sides).

make_cube_map(Side,-1,-1,Size,Acc) ->
    Image = list_to_binary(Acc),
    gl:texImage2D(Side, 0, ?GL_RGB8, Size, Size, 0, 
		  ?GL_RGB, ?GL_UNSIGNED_BYTE, Image);
make_cube_map(Side,-1,Y,Size,Acc) ->
    make_cube_map(Side,Size-1,Y-1,Size,Acc);
make_cube_map(Side,X,Y,Size,Acc) ->
    Vec = get_cube_vec(Side,Size,X,Y),
    make_cube_map(Side,X-1,Y,Size,[Vec|Acc]).

%% Given a cube map face index, cube map size, and integer 2D face position,
%% return the cooresponding normalized vector.
get_cube_vec(Side, Size, X, Y) ->
    S = (X + 0.5) / Size,
    T = (Y + 0.5) / Size,    
    SC = S*2.0 - 1.0,    
    TC = T*2.0 - 1.0,
    Vec = 
	case Side of
	    ?GL_TEXTURE_CUBE_MAP_POSITIVE_X -> {1.0,  -TC, -SC};
	    ?GL_TEXTURE_CUBE_MAP_NEGATIVE_X -> {-1.0, -TC,  SC};
	    ?GL_TEXTURE_CUBE_MAP_POSITIVE_Y -> {SC,   1.0,  TC};
	    ?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y -> {SC,  -1.0, -TC};
	    ?GL_TEXTURE_CUBE_MAP_POSITIVE_Z -> {SC,   -TC, 1.0};
	    ?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z -> {-SC,  -TC,-1.0}
	end,
    {RX,RY,RZ} = e3d_vec:norm(Vec),
    [round(128+127*RX),round(128+127*RY),round(128+127*RZ)].
