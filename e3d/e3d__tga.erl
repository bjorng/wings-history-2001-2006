%%
%%  e3d__tga.erl --
%%
%%     Functions for reading and writing TGA files.
%%
%%  Copyright (c) 2001-2002 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d__tga.erl,v 1.9 2003/03/18 06:50:18 dgud Exp $
%%

-module(e3d__tga).
-export([load/2,save/3]).
-export([format_error/1]).
-include("e3d_image.hrl").

format_error(unsupported_format) ->
    "Unsupported format or bad TGA file";
format_error(bad_image_specifiction) ->
    "Bad image specification".

load(FileName, _Opts) ->
    case file:read_file(FileName) of
	%% Uncompressed image
	{ok, <<0,0,2,0,0,0,0,0,0,0,0,0,  Image/binary>>} ->
%	    io:format("Loading uncompressed ~n",[]),
	    load_uncomp(Image);
	%% Compressed image
	{ok, <<0,0,10,0,0,0,0,0,0,0,0,0, Image/binary>>} ->
%	    io:format("Loading compressed ~n",[]),
	    load_comp(Image);
	%% Black&White Image 
	{ok, <<0,0,3,0,0,0,0,0,0,0,0,0, Image/binary>>} ->
	    load_uncomp(Image);
	%% Black&White Image 
	{ok, <<0,0,11,0,0,0,0,0,0,0,0,0, Image/binary>>} ->
	    load_comp(Image);

	{ok, <<_Bin:12/binary, _Rest/binary>>} ->
	    io:format("~p~n", [_Bin]),
	    {error, {none,?MODULE,unsupported_format}};
	Error ->
	    Error
    end.

load_uncomp(<<W:16/little,H:16/little,BitsPP:8,0:1,0:1,Order:2, Alpha:4,Image/binary>>) ->
    BytesPerPixel = BitsPP div 8,
    SpecOK = (W > 0) and (H > 0) and ((BitsPP == 32) 
				      or (BitsPP == 24) 
				      or (BitsPP == 8)),
    if 
	SpecOK == false ->
	    io:format("Unsupported image spec W ~p H ~p BitsPP ~p Order ~p Alpha ~p ~n",
		      [W,H,BitsPP,Order,Alpha]),
	    {error, {none,?MODULE,bad_image_specifiction}};
	true ->
	    Type = case BytesPerPixel of
		       1 when Alpha == 8 -> a8;
		       1 -> g8;
		       3 -> b8g8r8;
		       4 -> b8g8r8a8
		   end,
	    Size = BytesPerPixel * W * H,
	    <<RealImage:Size/binary, _Rest/binary>> = Image,
	    #e3d_image{width = W, height = H, type = Type, 
		       order = get_order(Order),
		       bytes_pp = BytesPerPixel, alignment = 1,
		       image = RealImage}
    end.

load_comp(<<W:16/little,H:16/little,BitsPP:8,0:1,0:1,Order:2, Alpha:4,CImage/binary>>) ->
    BytesPerPixel = BitsPP div 8,
    SpecOK = (W > 0) and (H > 0) and ((BitsPP == 32) 
				      or (BitsPP == 24)
				      or (BitsPP == 8)),
    if 
	SpecOK == false ->
	    io:format("Unsupported image spec W ~p H ~p BitsPP ~p Order ~p Alpha ~p ~n",
		      [W,H,BitsPP,Order,Alpha]),

	    {error, {none,?MODULE,bad_image_specifiction}};
	true ->
	    Type = case BytesPerPixel of
		       1 when Alpha == 8 -> a8;
		       1 -> g8;
		       3 -> b8g8r8;
		       4 -> b8g8r8a8
		   end,	    
	    Size  = W * H,
	    Image = load_comp(CImage, Size, BytesPerPixel, []),
	    #e3d_image{width = W, height = H, type = Type, order = get_order(Order),
		       bytes_pp = BytesPerPixel, image = Image}
    end.

load_comp(_, PL, _ByPP, Acc) when PL =< 0 ->
    list_to_binary(lists:reverse(Acc));

load_comp(<<0:1, Len:7, Image/binary>>, PLeft, ByPP, Acc) ->
    Bytes = (Len+1) * ByPP,
    <<Pixels:Bytes/binary, RestImage/binary>> = Image,
    load_comp(RestImage, PLeft-(Len+1), ByPP, [Pixels| Acc]);
load_comp(<<1:1, Len:7, RestImage0/binary>>, PLeft, ByPP, Acc) ->
    <<Pixel:ByPP/binary, RestImage/binary>> = RestImage0,
    Pixels = lists:duplicate(Len+1, Pixel),
    load_comp(RestImage, PLeft-(Len+1), ByPP, [Pixels| Acc]).

save(Image0, FileName, _Opts) ->
    Order = get_order(Image0#e3d_image.order),
    {Image, BitsPP, Def} = 
	if 
	    Image0#e3d_image.bytes_pp == 3 ->
		{e3d_image:convert(Image0, b8g8r8, 1), 24,   <<Order:4, 0:4>>};
	    Image0#e3d_image.bytes_pp == 4 ->
		{e3d_image:convert(Image0, b8g8r8a8, 1), 32, <<Order:4, 8:4>>}
	end,
    Head0 = <<0,0,2,0,0,0,0,0,0,0,0,0>> ,    
    Head1 = <<(Image#e3d_image.width):16/little, (Image#e3d_image.height):16/little,
               BitsPP:8,(Def)/binary>> ,
    Bin =  <<Head0/binary, Head1/binary, (Image#e3d_image.image)/binary>> ,
    file:write_file(FileName, Bin).

get_order(lower_left) -> 0;
get_order(lower_right) ->1;
get_order(upper_left) -> 2;
get_order(upper_right) ->3;
get_order(0) -> lower_left;
get_order(1) -> lower_right;
get_order(2) -> upper_left;
get_order(3) -> upper_right.

     
