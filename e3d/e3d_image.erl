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
%%     $Id: e3d_image.erl,v 1.12 2003/04/23 12:38:26 bjorng Exp $
%%

-module(e3d_image).
-author('dgud@erix.ericsson.se').

-include("e3d_image.hrl").

-export([load/1, load/2, 
	 convert/2, convert/3, convert/4, 
	 save/2, save/3,
	 bytes_pp/1, pad_len/2, format_error/1]).

%% internal exports
-export([noswap1/8,noswap3/8,noswap4/8,
	 noswap1to3/8,noswap1ato4/8,noswap1gto4/8,
	 noswap3to4/8,noswap4to3/8, 
	 swap3/8,swap4/8,swap3to4/8,swap4to3/8]).

%% Func: load(FileName[, Options])  
%% Args: FileName = [Chars], Options = [Tagged Tuple]
%% Rets: #e3d_image | {error, Reason}
%% Desc: Loads an image file.
%%       Default loads image with type/alignment/order set to what is 
%%       stored in the file.
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
	    ".tif" -> 
		e3d__tif:load(FileName, Opts);
	    ".tiff" -> 
		e3d__tif:load(FileName, Opts);
	    _ ->
		return_error({not_supported, Extension})
	end,
    fix_outtype(Res, Opts).

%% Func: save(#e3d_image, FileName [, Opts]
%% Rets: ok | {error, Reason}
%% Desc: Saves image to file. Using extension to 
%%       know which fileformat to use. 
%%       Opts is a list of options. 
%%       Available options: compress 
%%        compress - compresses the file if it is possible/implemented (currently tif).
save(Image, Filename) ->
    save(Image, Filename, []).
save(Image = #e3d_image{}, Filename, Opts) ->
    Extension = file_extension(Filename),
    case Extension of 
	".tga" ->
	    e3d__tga:save(Image, Filename, Opts);
	".bmp" ->
	    e3d__bmp:save(Image, Filename, Opts);
	".tif" -> 
	    e3d__tif:save(Image, Filename, Opts);
	".tiff" -> 
	    e3d__tif:save(Image, Filename, Opts);
	_ ->
	    return_error({not_supported, Extension})
    end.

format_error({not_supported,Extension}) ->
    io_lib:format("Files of type ~s are not supported", [Extension]);
format_error(Other) ->
    file:format_error(Other).

%% Func: convert(#e3d_image, NewType [,NewAlignment [,NewOrder ]])
%% Rets: #e3d_image | {error, Reason}
%% Desc: Converts an image to new type optionally NewAlignment and NewOrder
convert(In, ToType) when atom(ToType) ->
    convert(In, ToType, In#e3d_image.alignment, In#e3d_image.order).

convert(In, ToType, NewAlignment) when atom(ToType) ->
    convert(In, ToType, NewAlignment, In#e3d_image.order).

convert(#e3d_image{type=Type, alignment=Al,order=O}=In, Type, Al, O) -> In;
convert(#e3d_image{type=FromType,image=Image,alignment=FromAlm,order=FromOrder}=In,
	ToType, ToAlm, ToOrder) ->
    OldRowLength  = In#e3d_image.width * In#e3d_image.bytes_pp,
    NewRowLength  = In#e3d_image.width * bytes_pp(ToType),

    OldPaddLength = pad_len(OldRowLength, FromAlm),
    NewPaddLength = pad_len(NewRowLength, ToAlm),
    NewPadd = lists:duplicate(NewPaddLength, 0),
    W = In#e3d_image.width,
    
    case type_conv(FromType, ToType) of
	{error, _Reason} = Err ->
	    Err;
	TypeConv ->
	    OrderConv = order_conv(FromOrder, ToOrder),
	    New = ?MODULE:TypeConv(0, W, Image, 
				   OldPaddLength, NewPadd, 
				   OrderConv, [], []),
	    In#e3d_image{image=New,type=ToType,
			 bytes_pp=bytes_pp(ToType), 
			 alignment=ToAlm,order=ToOrder}
    end.

%% Func: pad_len(RowLength (in bytes), Alignment) 
%% Rets: integer()
%% Desc: Get the number of bytes each row is padded with
pad_len(RL, Align) ->
    case RL rem Align of
	0 -> 0;
	Rem -> Align - Rem
    end.	     	   

%% Func: bytes_pp(Type) 
%% Rets: integer()
%% Desc: Get the number of bytes per pixel for type Type
bytes_pp(a8) -> 1;
bytes_pp(g8) -> 1;
bytes_pp(r8g8b8) -> 3;
bytes_pp(b8g8r8) -> 3;
bytes_pp(r8g8b8a8) -> 4;
bytes_pp(b8g8r8a8) -> 4;
bytes_pp(#e3d_image{bytes_pp = Bpp}) ->
    Bpp.

%% Helpers 
file_extension(FileName) ->
    lowercase(filename:extension(FileName)).

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


%-define(C3(A,B,C), A,B,C).
%-define(C4(A,B,C,D), A,B,C,D). 
-define(C3(A,B,C), [A,B,C]).
-define(C4(A,B,C,D), [A,B,C,D]).   %% Seems faster if I make a binary of each row!!
%-define(C3(A,B,C), <<A:8,B:8,C:8>>).
%-define(C4(A,B,C,D), <<A:8,B:8,C:8,D:8>>).

swap(Action, W, W, Bin, OPL, NP, {SC,SR}, Row, Acc) ->
    <<_Skip:OPL/binary, Rest/binary>> = Bin,    
    NewRow = if 
		 SR == true ->
		     list_to_binary([NP|Row]);
		 SR == false ->
		     list_to_binary(lists:reverse([NP|Row]))
	     end,
    ?MODULE:Action(0, W, Rest, OPL, NP, {SC,SR}, [], [NewRow|Acc]);
swap(_, _, _, <<>>, _, _, {SC,_SR}, Row, Acc) ->
    NewImage = 
	if 
	    SC == true -> [Row|Acc];
	    SC == false -> lists:reverse(Acc, [Row])
	end,
    list_to_binary(NewImage).

noswap1(0, W, <<>>, OPL,NP,OC,[],Acc) ->
    swap(noswap1, 0, W, <<>>, OPL,NP,OC,[],Acc);
noswap1(0, W, Bin, OPL, NP, {_,SR}=OC, [], Acc) ->
    <<Row:W/binary, R/binary>> = Bin,
    case SR of 
	false -> swap(noswap1,W,W,R,OPL, NP, OC, [Row], Acc);
	true ->
	    RowList = lists:reverse(binary_to_list(Row)),
	    swap(noswap1,W,W,R,OPL, NP, OC, RowList, Acc)
    end.
%%% RGB or BGR 
noswap3(0, W, Bin, OPL, NP, {_,false}=OC, RowAcc, Acc) ->
    case Bin of
	<<Row:W/binary-unit:24,R/binary>> ->
	    swap(noswap3, W, W, R, OPL, NP, OC, [Row], Acc);
	_ ->
	    noswap3_1(0, W, Bin, OPL, NP, OC, RowAcc, Acc)
    end;
noswap3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    noswap3_1(C, W, Bin, OPL, NP, OC, Row, Acc).

noswap3_1(C, W, <<RGB:3/binary,R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap3_1(C+1, W, R, OPL, NP, OC, [RGB|Row], Acc);
noswap3_1(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap3, C, W, Bin, OPL, NP, OC, Row, Acc).
%% Alpha8 or Grey8 to RGB
noswap1to3(C, W, <<G0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap1to3(C+1, W, R, OPL, NP, OC, [?C3(G0,G0,G0)|Row], Acc);
noswap1to3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap1to3, C, W, Bin, OPL, NP, OC, Row, Acc).
%% Alpha8 to RGBA
noswap1ato4(C, W, <<A0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap1ato4(C+1, W, R, OPL, NP, OC, [?C4(255,255,255,A0)|Row], Acc);
noswap1ato4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap1ato4, C, W, Bin, OPL, NP, OC, Row, Acc).
%% Grey8 to RGB
noswap1gto4(C, W, <<G0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap1gto4(C+1, W, R, OPL, NP, OC, [?C4(G0,G0,G0, 255)|Row], Acc);
noswap1gto4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap1gto4, C, W, Bin, OPL, NP, OC, Row, Acc).
%% RGB to RGBA 
noswap3to4(C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap3to4(C+1, W, R, OPL, NP, OC, [?C4(B0,G0,R0,255)|Row], Acc);
noswap3to4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap3to4, C, W, Bin, OPL, NP, OC, Row, Acc).
%% RGBA 
noswap4(C, W, <<Col:4/binary, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap4(C+1, W, R, OPL, NP, OC, [Col|Row], Acc);
noswap4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap4, C, W, Bin, OPL, NP, OC, Row, Acc).
%% RGBA to RGB
noswap4to3(C, W, <<RGB:3/binary,_:8,R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap4to3(C+1, W, R, OPL, NP, OC, [RGB|Row], Acc);
noswap4to3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap4to3, C, W, Bin, OPL, NP, OC, Row, Acc).

swap3(C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W->
    swap3(C+1, W, R, OPL, NP, OC, [?C3(R0,G0,B0)|Row], Acc);
swap3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(swap3, C, W, Bin, OPL, NP, OC, Row, Acc).

swap4(C, W, <<B0:8,G0:8,R0:8,A0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W->
    swap4(C+1, W, R, OPL, NP, OC, [?C4(R0,G0,B0,A0)|Row], Acc);
swap4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(swap4, C, W, Bin, OPL, NP, OC, Row, Acc).

swap4to3(C, W, <<B0:8,G0:8,R0:8,_:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W->
    swap4to3(C+1, W, R, OPL, NP, OC, [?C3(R0,G0,B0)|Row],  Acc);
swap4to3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(swap4to3, C, W, Bin, OPL, NP, OC, Row, Acc).

swap3to4(C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    swap3to4( C+1, W, R, OPL, NP, OC, [?C4(R0,G0,B0,255)|Row], Acc);
swap3to4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(swap3to4, C, W, Bin, OPL, NP, OC, Row, Acc).

%fix_alignment(<<>>, RL, OldP, NewP, Acc) ->
%    list_to_binary(lists:reverse(Acc));
%fix_alignment(Image, RL, OldP, NewP, Acc) ->
%    <<Row:RL/binary, Skip:OldP/binary, Rest/binary>> = Image,
%    fix_alignment(Rest, RL, OldP, NewP, [NewP, Row | Acc]).

type_conv(Type, Type) ->  %% No swap
    case bytes_pp(Type) of
	1 -> noswap1;
	3 -> noswap3;
	4 -> noswap4
    end;
type_conv(a8, r8g8b8) ->
    noswap1to3;
type_conv(g8, r8g8b8) ->
    noswap1to3;
type_conv(a8, r8g8b8a8) ->
    noswap1ato4;
type_conv(g8, r8g8b8a8) ->
    noswap1gto4;
type_conv(r8g8b8a8, r8g8b8) ->
    noswap4to3;
type_conv(b8g8r8a8, b8g8r8) ->
    noswap4to3;
type_conv(r8g8b8, r8g8b8a8) ->
    noswap3to4;
type_conv(b8g8r8, b8g8r8a8) ->
    noswap3to4;
type_conv(FromType, ToType) ->
    case {bytes_pp(FromType), bytes_pp(ToType)} of
	{3,3} -> swap3;
	{4,4} -> swap4;
	{3,4} -> swap3to4;
	{4,3} -> swap4to3;
	_ -> {error, {not_supported,conversion,FromType,ToType}}
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

return_error(Reason) ->
    {error, {none, ?MODULE, Reason}}.
