%%
%%  e3d_image.erl --
%%
%%     Handle images (2D) and different file formats.
%%
%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_image.erl,v 1.2 2001/10/19 19:46:29 bjorng Exp $
%%

-module(e3d_image).
-author('dgud@erix.ericsson.se').

-include("e3d_image.hrl").

-export([load/1, load/2, 
	 convert/2, convert/3, convert/4, 
	 save/2, save/3,
	 bytes_pp/1]).

%% Func: load(FileName[, Options])  
%% Args: FileName = [Chars], Options = [Tagged Tuple]
%% Rets: #e3d_image | {error, Reason}
%% Desc: Loads a image file, currently BMP and TGA is supported
%%       Default loads image with type/alignment/order set to what is 
%%       used in loaded file format.
%%       Conversion between fileformats type/alignment/order can be done with 
%%       Options {type, Type} and/or {alignment, N} and/or {order, O} see e3d_image.hrl
load(FileName) ->
    load(FileName, []).
load(FileName, Opts) when list(FileName), list(Opts) ->
    Extension = file_extension(FileName),
    Res = 
	case Extension of 
	    ".tga" ->
		e3d__tga:load(FileName, Opts);
	    ".bmp" ->
		e3d__bmp:load(FileName, Opts);
	    _ ->
		{error, {not_supported, Extension}}
	end,
    fix_outtype(Res, Opts).

%% Func: save(#e3d_image, FileName [, Opts]
%% Rets: ok | {error, Reason}
%% Desc: Saves image to file using extension to know 
%%       of filename as file type.
save(Image, Filename) ->
    save(Image, Filename, []).
save(Image = #e3d_image{}, Filename, Opts) ->
    Extension = file_extension(Filename),
    case Extension of 
	".tga" ->
	    e3d__tga:save(Image, Filename, Opts);
	".bmp" ->
	    e3d__bmp:save(Image, Filename, Opts);
	_ ->
	    {error, {not_supported, Extension}}
    end.

%% Func: convert(#e3d_image, NewType [,NewAlignment [,NewOrder ]])
%% Rets: #e3d_image | {error, Reason}
%% Desc: Converts an image to new type optionally NewAlignment and NewOrder
convert(In, ToType) when atom(ToType) ->
    convert(In, ToType, In#e3d_image.alignment, In#e3d_image.order).
convert(In, ToType, NewAlignment) when atom(ToType) ->
    convert(In, ToType, NewAlignment, In#e3d_image.order).
convert(In = #e3d_image{type = TT, alignment = A, order = O}, TT, A, O) ->
    In;
convert(In = #e3d_image{type = FromType, image = Image, 
			alignment = FromAlm, order = FromOrder}, 
	ToType, ToAlm, ToOrder) ->
    OldRowLength  = In#e3d_image.width * In#e3d_image.bytes_pp,
    NewRowLength  = In#e3d_image.width * bytes_pp(ToType),
    OldPaddLength = (OldRowLength rem FromAlm),
    NewPaddLength = (NewRowLength rem ToAlm),
    NewPadd = lists:duplicate(NewPaddLength, 0),
    W = In#e3d_image.width,
    
    TypeConv  = type_conv(FromType, ToType),
    OrderConv = order_conv(FromOrder, ToOrder),
    
    New = swap(TypeConv, 0, W, Image, OldPaddLength, NewPadd, OrderConv, [[]]),    
    In#e3d_image{image = New, type = ToType}.

%% Func: bytes_pp(Type) 
%% Rets: integer()
%% Desc: Get the number of bytes per pixel for type Type
bytes_pp(r8g8b8) -> 3;
bytes_pp(b8g8r8) -> 3;
bytes_pp(r8g8b8a8) -> 4;
bytes_pp(b8g8r8a8) -> 4;
bytes_pp(#e3d_image{bytes_pp = Bpp}) ->
    Bpp.

%% Helpers 
file_extension(FileName) ->
    lowercase(lists:reverse(lists:sublist(lists:reverse(FileName), 4))).

lowercase([H|R]) when H >= $A, H =< $Z ->
    [H + $a - $A | lowercase(R)];
lowercase([H|R]) ->
    [H | lowercase(R)];
lowercase([]) ->
    [].

fix_outtype(Res = #e3d_image{}, Opts) ->
    Type = 
	case lists:keysearch(type, 1, Opts) of
	    {value, {type, T}} -> T;
	    false -> Res#e3d_image.type
	end,    
    Alignment = 
	case lists:keysearch(alignment, 1, Opts) of 
	    {value, {alignment, A}} -> A;
	    false -> Res#e3d_image.alignment
	end,
    Order = 
	case lists:keysearch(order, 1, Opts) of 
	    {value, {order, O}} -> O;
	    false -> Res#e3d_image.order
	end,
    convert(Res, Type, Alignment, Order);
fix_outtype(Res, _) ->  %% Propagate Error Case
    Res.

swap(Action, W, W, Bin, OPL, NP, {SC,SR}, [Row|Acc]) ->
    <<Skip:OPL/binary, Rest/binary>> = Bin,    
    NewRow = if 
		 SR == true ->  [NP|Row];
		 SR == false -> lists:reverse([NP|Row])
	     end,
    swap(Action, 0, W, Rest, OPL, NP, {SC,SR}, [[],NewRow|Acc]);
swap(_, _, _, <<>>, _, _, {SC,SR}, Acc) ->
    NewImage = 
	if 
	    SC == true -> Acc; 
	    SC == false -> lists:reverse(Acc)
	end,
    list_to_binary(NewImage);

swap(3, C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, OC, [Row|Acc]) ->
    swap(3, C+1, W, R, OPL, NP, OC,[[[B0,G0,R0]|Row]| Acc]);
swap(4, C, W, <<B0:8,G0:8,R0:8, A0:8, R/binary>>, OPL, NP, OC, [Row|Acc]) ->
    swap(4, C+1, W, R, OPL, NP, OC,[[[B0,G0,R0,A0]|Row]| Acc]);
swap(swap3, C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, OC, [Row|Acc]) ->
    swap(swap3, C+1, W, R, OPL, NP, OC,[[[R0,G0,B0]|Row]| Acc]);
swap(swap4, C, W, <<B0:8,G0:8,R0:8,A0:8, R/binary>>, OPL, NP, OC, [Row|Acc]) ->
    swap(swap4, C+1, W, R, OPL, NP, OC,[[[R0,G0,B0,A0]|Row]| Acc]);
swap(swap4to3,  C, W, <<B0:8,G0:8,R0:8,A0:8, R/binary>>, OPL, NP, OC,[Row|Acc]) ->
    swap(swap4to3, C+1, W, R, OPL, NP, OC,[[[R0,G0,B0]|Row]| Acc]);
swap(swap3to4, C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, OC, [Row|Acc]) ->
    swap(swap3to4, C+1, W, R, OPL, NP, OC,[[[R0,G0,B0,255]|Row]| Acc]).

%fix_alignment(<<>>, RL, OldP, NewP, Acc) ->
%    list_to_binary(lists:reverse(Acc));
%fix_alignment(Image, RL, OldP, NewP, Acc) ->
%    <<Row:RL/binary, Skip:OldP/binary, Rest/binary>> = Image,
%    fix_alignment(Rest, RL, OldP, NewP, [NewP, Row | Acc]).

type_conv(Type, Type) ->  %% No swap
    bytes_pp(Type);
type_conv(FromType, ToType) ->
    case {bytes_pp(FromType), bytes_pp(ToType)} of
	{3,3} -> swap3;
	{4,4} -> swap4;
	{3,4} -> swap3to4;
	{4,3} -> swap4to3
    end.

order_conv(Order, Order) -> {false, false};   %% {SwapColumns, SwapRows}
order_conv(lower_left, upper_right) -> {true,true};
order_conv(upper_right, lower_left) -> {true,true};
order_conv(lower_right, upper_left) -> {true,true};
order_conv(upper_left, lower_right) -> {true,true};

order_conv(lower_left, upper_left) ->  {true,false};
order_conv(upper_right, lower_right) ->{true,false};
order_conv(lower_right, upper_right) ->{true,false};
order_conv(upper_left, lower_left) ->  {true,false};

order_conv(lower_left,  lower_right) -> {false,true};
order_conv(upper_right, upper_left) ->  {false,true};
order_conv(lower_right, lower_left) ->  {false,true};
order_conv(upper_left,  upper_right) -> {false,true}.

