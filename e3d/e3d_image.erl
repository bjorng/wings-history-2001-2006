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
%%     $Id: e3d_image.erl,v 1.1 2001/10/18 16:06:18 bjorng Exp $
%%

-module(e3d_image).
-author('dgud@erix.ericsson.se').

-include("e3d_image.hrl").

-export([load/1, load/2, convert/3, save/2, save/3]).

%% Func: load(FileName[, Options])  
%% Args: FileName = [Chars], Options = [Tagged Tuple]
%% Rets: #e3d_image | {error, Reason}
%% Desc: Loads a image file, currently BMP and TGA is supported
%%       Default loads image with type and alignment set to what is 
%%       used in loaded file format.
%%       Conversion between fileformats and alignment can be done with 
%%       Option maybe {type, Type} and/or {alignment, N} see e3d_image.hrl
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
    OutType = lists:keysearch(type, 1, Opts),
    Alignment = lists:keysearch(alignment, 1, Opts),
    fix_outtype(Res, OutType, Alignment).

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

%% Func: convert(#e3d_image, NewType, NewAlignment)
%% Rets: #e3d_image | {error, Reason}
%% Desc: Converts an image to new type 
convert(In = #e3d_image{type = ToType, alignment = A}, ToType, A) ->
    In;
convert(In = #e3d_image{type = FromType, image = Image, alignment = FromAlm}, 
	ToType, ToAlm) ->
    OldRowLength  = In#e3d_image.width * In#e3d_image.bytes_pp,
    NewRowLength  = In#e3d_image.width * bytes_pp(ToType),
    OldPaddLength = (OldRowLength rem FromAlm),
    NewPaddLength = (NewRowLength rem ToAlm),
    NewPadd = lists:duplicate(NewPaddLength, 0),
    W = In#e3d_image.width,
    New = case {FromType, ToType} of	
	      {ToType, ToType} ->
		  fix_alignment(Image, OldRowLength, OldPaddLength, NewPadd, []);
	      {b8g8r8, r8g8b8} ->
		  swap(swap3, 0, W, Image, OldPaddLength, NewPadd, []);
	      {r8g8b8, b8g8r8} ->
		  swap(swap3, 0, W, Image, OldPaddLength, NewPadd, []);
	      {b8g8r8a8, r8g8b8a8} ->
		  swap(swap4, 0, W, Image, OldPaddLength, NewPadd, []);
	      {r8g8b8a8, b8g8r8a8} ->
		  swap(swap4, 0, W, Image, OldPaddLength, NewPadd, []);
	      {b8g8r8a8, r8g8b8} ->
		  swap(swap4to3, 0, W, Image, OldPaddLength, NewPadd, []);
	      {r8g8b8a8, b8g8r8} ->
		  swap(swap4to3, 0, W, Image, OldPaddLength, NewPadd, []);
	      {b8g8r8, r8g8b8a8} ->
		  swap(swap3to4, 0, W, Image, OldPaddLength, NewPadd, []);
	      {r8g8b8, b8g8r8a8} ->
		  swap(swap3to4, 0, W, Image, OldPaddLength, NewPadd, []);
	      Else ->
		  {error, {unsupported, {FromType, ToType}}}
	  end,
    In#e3d_image{image = New, type = ToType}.

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

fix_outtype(Res = #e3d_image{type = InT, alignment = InA}, OptType, AlignmentType) ->
    {Type, Alm} = 
	case {OptType, AlignmentType} of
	    {{value, {type, T}}, {value, {alignment, A}}} ->
		{T,A};
	    {{value, {type, T}}, false} ->
		{T,InA};
	    {false, {value, {alignment, A}}} ->
		{InT, A};
	    {false, false} ->
		{InT, InA}
	end,
    convert(Res, Type, Alm);
fix_outtype(Res, _, _) ->  %% Propagate Error Case
    Res.

swap(Action, W, W, Bin, OPL, NP, Acc) ->
    <<Skip:OPL/binary, Rest/binary>> = Bin,    
    swap(Action, 0, W, Rest, OPL, NP, [NP|Acc]);
swap(_, _, _, <<>>, _, _, Acc) ->
    list_to_binary(lists:reverse(Acc));
swap(swap3, C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, Acc) ->
    swap(swap3, C+1, W, R, OPL, NP, [<<R0:8,G0:8,B0:8>>| Acc]);
swap(swap4, C, W, <<B0:8,G0:8,R0:8,A0:8, R/binary>>, OPL, NP, Acc) ->
    swap(swap4, C+1, W, R, OPL, NP, [<<R0:8,G0:8,B0:8,A0:8>>| Acc]);
swap(swap4to3,  C, W, <<B0:8,G0:8,R0:8,A0:8, R/binary>>, OPL, NP, Acc) ->
    swap(swap4to3, C+1, W, R, OPL, NP, [<<R0:8,G0:8,B0:8>>| Acc]);
swap(swap3to4, C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, Acc) ->
    swap(swap3to4, C+1, W, R, OPL, NP, [<<R0:8,G0:8,B0:8,255:8>>| Acc]).

fix_alignment(<<>>, RL, OldP, NewP, Acc) ->
    list_to_binary(lists:reverse(Acc));
fix_alignment(Image, RL, OldP, NewP, Acc) ->
    <<Row:RL/binary, Skip:OldP/binary, Rest/binary>> = Image,
    fix_alignment(Rest, RL, OldP, NewP, [NewP, Row | Acc]).
