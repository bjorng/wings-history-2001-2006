%%
%%  wp8_jpeg_image.erl --
%%
%%     Plug-in for reading and writing JPEG files
%%     using libjpeg from IJG (Independent JPEG Group).
%%
%%  Copyright (c) 2004 Bjorn Gustavsson
%%
%%  libjpeg is copyright (C) 1991-1998, Thomas G. Lane.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp8_jpeg_image.erl,v 1.2 2004/01/18 11:25:51 bjorng Exp $
%%

-module(wp8_jpeg_image).

-export([init/1,format_error/1]).

-include("e3d_image.hrl").

%% Operations supported by driver.
-define(OP_IMAGE_READ, 0).
-define(OP_IMAGE_WRITE, 1).

init(Next) ->
    Dir = filename:dirname(code:which(?MODULE)),
    case erl_ddll:load_driver(Dir, "wings_jpeg_image_drv") of
	ok ->
	    case open_port({spawn,wings_jpeg_image_drv},[]) of
		Port when is_port(Port) ->
		    register(?MODULE, Port),
		    fun(What) ->
			    fileop(What,Next)
		    end;
		Other ->
		    Next
	    end;
	_ -> Next
    end.

format_error(format) -> "File format not recognized";
format_error(_) -> "Unknown error".

fileop({image,formats,Fs0}, Next) ->
    Fs = image_formats(Fs0),
    Next({image,formats,Fs});
fileop({image,read,Prop}=What, Next) ->
    Name = proplists:get_value(filename, Prop),
    case is_format_supported(Name) of
	true -> read_image(Name, Prop);
	false -> Next(What)
    end;
fileop({image,write,Prop}=What, Next) ->
    Name = proplists:get_value(filename, Prop),
    Image = proplists:get_value(image, Prop),
    Ext = lower(filename:extension(Name)),
    case is_format_supported(Name, Ext) of
	true -> write_image(Name, Ext, Image, Prop);
	false -> Next(What)
    end;
fileop(What, Next) ->
    Next(What).

read_image(Name, Prop) ->
    case file:read_file(Name) of
	{ok,Bin} ->
	    read_image_1(Bin, Prop);
	{error,_}=Error -> Error
    end.
	    
read_image_1(Bin, Prop) ->
    case erlang:port_control(?MODULE, ?OP_IMAGE_READ, Bin) of
	[] -> {error,{none,?MODULE,format}};
	Res -> read_image_2(Res, Prop)
    end.

read_image_2(<<W:32/native,H:32/native,SamplesPerPixel:32/native,
	      Bits/binary>>, Prop) ->
    Type = case SamplesPerPixel of
	       1 -> g8;
	       3 -> r8g8b8
	   end,
    Image = #e3d_image{type=Type,bytes_pp=SamplesPerPixel,
		       alignment=1,order=upper_left,
		       width=W,height=H,image=Bits},
    NeededType = proplists:get_value(type, Prop, Type),
    NeededAlignment = proplists:get_value(alignment, Prop, 1),
    NeededOrder = proplists:get_value(order, Prop, upper_left),
    e3d_image:convert(Image, NeededType, NeededAlignment, NeededOrder).

write_image(Name, Ext, Image, Prop) ->
    {ok,Tiff} = e3d_image:save_bin(Image, ".tiff"),
    Data = Tiff,
    case erlang:port_control(?MODULE, ?OP_IMAGE_WRITE, Data) of
	[] -> {error,{none,?MODULE,format}};
	Bin -> file:write_file(Name, Bin)
    end.

is_format_supported(Name) ->
    Ext = lower(filename:extension(Name)),
    is_format_supported(Name, Ext).

is_format_supported(Name, Ext) ->
    lists:keymember(Ext, 1, image_formats([])).

lower([Upper|T]) when $A =< Upper, Upper =< $Z ->
    [Upper-$A+$a|lower(T)];
lower([H|T]) ->
    [H|lower(T)];
lower([]) -> [].

image_formats(Fs) ->
    [{".jpg","JPEG File"}|Fs].
