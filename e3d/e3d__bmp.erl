%%
%%  e3d__bmp.erl --
%%
%%     Functions for reading and writing DIB BMP files.
%%
%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d__bmp.erl,v 1.6 2002/08/12 08:50:32 dgud Exp $
%%

-module(e3d__bmp).
-export([load/2,save/3]).
-export([format_error/1]).

-include("e3d_image.hrl").

-define(BITMAPFILEHEADER,  
	$B:8,$M:8, FileSz:32/little, 0:16, 0:16, Offset:32/little).
-define(BITMAPINFOHEADER,  
	BiHeaderSz:32/little, BiW:32/little, BiH:32/little, 
	BiPlanes:16/little, BiBitCount:16/little, BiCompression:32/little, 
	BiSizeImage:32/little,
	BiXPelsPerMeter:32/little,BiYPelsPerMeter:32/little,
	BiClrUsed:32/little, BiClrImportant:32/little).

-define(BITMAPFILEHEADERSZ, 1+1+4+2+2+4).
-define(BITMAPINFOHEADERSZ, 4+4+4+2+2+4+4+4+4+4+4).

-define(DBGOUT(), io:format(" FileSz = ~p Offset = ~p BiHeaderSz = ~p ~n "
			    " BiW = ~p  BiH = ~p  BiPlanes = ~p ~n "
			    " BiBitCount = ~p  BiCompression = ~p ~n"
			    " BiSizeImage = ~p BiXPelsPerMeter = ~p , BiYPelsPerMeter = ~p~n"
			    " BiClrUsed = ~p , BiClrImportant = ~p ~n",
			    [FileSz, Offset,  BiHeaderSz, BiW, BiH, BiPlanes, BiBitCount,BiCompression,
			     BiSizeImage, BiXPelsPerMeter, BiYPelsPerMeter,
			     BiClrUsed, BiClrImportant])).	    

format_error(unsupported_format) ->
    "Unsupported format or bad BMP file".

load(FileName, Opts) ->
    %% Currently only supported format 
    BitCount = 24,
    Compression = 0,
    BiPlanes = 1,
    case file:read_file(FileName) of	
	{ok, <<?BITMAPFILEHEADER, ?BITMAPINFOHEADER, TmpImage/binary>>} when BiBitCount == 24 ->
%	    ?DBGOUT(),
%	    io:format("Size ~p ~n", [size(TmpImage)]),

	    %% Strip off last bytes on padded images so we get correct length
	    RowLength = BiW * 3 + e3d_image:pad_len(BiW * 3, 4),
	    Sz = BiH * RowLength,
	    <<Image:Sz/binary, _/binary>> = TmpImage,
	    #e3d_image{width = BiW, height = BiH, 
		       type = b8g8r8,
		       bytes_pp = 3, 
		       alignment = 4,
		       image = Image};
	{ok, Bin} ->
	    {error, {none,?MODULE,unsupported_format}};
	Error ->
	    Error
    end.

save(Image0, FileName, Opts) ->
    Image = e3d_image:convert(Image0, b8g8r8, 4, lower_left),
    FileSz = ?BITMAPFILEHEADERSZ + ?BITMAPINFOHEADERSZ + size(Image#e3d_image.image),
    Offset = ?BITMAPFILEHEADERSZ + ?BITMAPINFOHEADERSZ,
    BiHeaderSz = ?BITMAPINFOHEADERSZ,
    BiW = Image#e3d_image.width, 
    BiH = Image#e3d_image.height, 
    BiPlanes = 1, 
    BiBitCount = 24, 
    BiCompression = 0,
    BiSizeImage = 0, %% If RGB set to 0
    BiXPelsPerMeter = 0, BiYPelsPerMeter = 0,
    BiClrUsed = 0, BiClrImportant = 0,
%    ?DBGOUT(),
%    io:format("Size ~p ~n", [size(Image#e3d_image.image)]),
    Binary = <<?BITMAPFILEHEADER, ?BITMAPINFOHEADER, (Image#e3d_image.image)/binary>> ,
    file:write_file(FileName, Binary).
     
% debug(I1, I2) ->
%     debug(I1#e3d_image.image, I2#e3d_image.image, 0).
    
% debug(<<R:8,G:8,B:8, R1/binary>>, <<R:8,G:8,B:8, R2/binary>>, N) ->
%     debug(R1,R2, N+1);
% debug(I1, I2, N) ->
%     io:format("Diff: in Pixel ~p ~n~P~n~P~n", [N, I1, 10, I2, 10]).
