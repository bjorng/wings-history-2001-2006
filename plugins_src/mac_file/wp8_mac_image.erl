%%
%%  wp8_mac_image.erl --
%%
%%     Image reading and writing for Mac OS X.
%%
%%  Copyright (c) 2002-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp8_mac_image.erl,v 1.4 2003/03/16 17:26:39 bjorng Exp $
%%

-module(wp8_mac_image).

-export([init/1,format_error/1]).

-include("e3d_image.hrl").

%% Operations supported by driver.
-define(OP_IMAGE_READ, 0).

init(Next) ->
    case os:type() of
	{unix,darwin} ->
	    Dir = filename:dirname(code:which(?MODULE)),
	    case erl_ddll:load_driver(Dir, "mac_wings_image_drv") of
		ok ->
		    case open_port({spawn,"mac_wings_image_drv"},[]) of
			Port when is_port(Port) ->
			    register(?MODULE, Port),
			    fun(What) ->
				    fileop(What,Next)
			    end;
			Other ->
			    Next
		    end;
		Else ->
		    Next
	    end;
	_ ->
	    Next
    end.

format_error(format) -> "Unknown or unsupported format.".

fileop({image,formats,Fs0}, Next) ->
    Fs = image_formats(Fs0),
    Next({image,formats,Fs});
fileop({image,read,Prop}=What, Next) ->
    Name = proplists:get_value(filename, Prop),
    case is_format_supported(Name) of
	true -> read_image(Name, Prop);
	false -> Next(What)
    end;
fileop(What, Next) ->
    Next(What).

read_image(Name, Prop) ->
    case file:open(Name, [read]) of
	{ok,Fd} ->
	    file:close(Fd),
	    read_image_1(Name, Prop);
	{error,_}=Error -> Error
    end.
	    
read_image_1(Name, Prop) ->
    Data = [Name,0],
    case erlang:port_control(?MODULE, ?OP_IMAGE_READ, Data) of
	[] -> {error,{none,?MODULE,format}};
	Res -> read_image_2(Res, Prop)
    end.

read_image_2(<<W:32/native,H:32/native,SamplesPerPixel0:32/native,BytesPerRow:32,
	      Bits/binary>>, Prop) ->
    SamplesPerPixel = case {BytesPerRow div W,SamplesPerPixel0} of
			  {4,3} -> 4;
			  _ -> SamplesPerPixel0
		      end,
    Type = case SamplesPerPixel of
	       3 -> r8g8b8;
	       4 -> r8g8b8a8
	   end,
    Al = case BytesPerRow - SamplesPerPixel*W of
	     0 -> 1;
	     1 -> 2;
	     2 -> 4;
	     3 -> 4
	 end,
    Image = #e3d_image{type=Type,bytes_pp=SamplesPerPixel,
		       alignment=Al,order=upper_left,
		       width=W,height=H,image=Bits},
    NeededType = proplists:get_value(type, Prop, Type),
    NeededAlignment = proplists:get_value(alignment, Prop, 1),
    NeededOrder = proplists:get_value(order, Prop, upper_left),
    e3d_image:convert(Image, NeededType, NeededAlignment, NeededOrder).

is_format_supported(Name) ->
    Ext = lower(filename:extension(Name)),
    lists:keymember(Ext, 1, image_formats([])).

lower([Upper|T]) when $A =< Upper, Upper =< $Z ->
    [Upper-$A+$a|lower(T)];
lower([H|T]) ->
    [H|lower(T)];
lower([]) -> [].

image_formats(Fs) ->
    [{".png","PNG File"},
     {".gif","Compuserve GIF"},
     {".jpg","JPEG File"}|Fs].
