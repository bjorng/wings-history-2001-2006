%%
%%  wpc_image.erl --
%%
%%     Image plane plug-in
%%
%%  Copyright (c) 2002 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_image.erl,v 1.9 2002/12/10 13:26:58 raimo_niskanen Exp $
%%

-module(wpc_image).
-export([init/0,menu/2,command/2]).

-include_lib("esdl/include/gl.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").

-import(lists, [reverse/1]).

init() ->
    true.

menu({shape}, Menu) ->
    insert_before_more(Menu);
menu(_, Menu) -> Menu.

insert_before_more([H|_]=More) when element(1, element(2, H)) == more ->
    [image_menu(),separator|More];
insert_before_more([H|T]) ->
    [H|insert_before_more(T)];
insert_before_more([]) ->
    [image_menu()].

image_menu() ->
    {"Image...",image}.

command({shape,image}, _St) -> make_image();
command(_, _) -> next.

make_image() ->
    Ps = [{extensions,wpa:image_formats()}],
    case wpa:import_filename(Ps) of
	aborted -> keep;
	Name ->
	    Props = [{filename,Name},{type,r8g8b8},{alignment,1}],
	    case wpa:image_read(Props) of
		#e3d_image{}=Image ->
		    make_image_1(Image);
		{error,Error} ->
		    E = io_lib:format("Failed to load \"~s\": ~s\n",
				      [Name,file:format_error(Error)]),
		    wings_util:error(E)
	    end
    end.

make_image_1(Image0) ->
    Image1 = strip_any_alpha(Image0),
    #e3d_image{width=W0,height=H0,image=Pixels,order=Order} = Image1,
    {W,H,_} = Image = pad_image({W0,H0,Pixels}),
    case can_texture_be_loaded(Image) of
	false ->
	    wings_util:error("The image cannot be loaded as a texture "
			     "(it is probably too large).");
	true ->
	    MaxU = W0/W,
	    MaxV = H0/H,
	    M = [image],
	    Fs = [#e3d_face{vs=[0,3,2,1],tx=[1,0,3,2],mat=M},
		  #e3d_face{vs=[1,2,3,0],tx=[2,3,0,1],mat=M}],
	    Tx = case Order of
		     upper_left ->
			 [{0.0,MaxV},{MaxU,MaxV},{MaxU,0.0},{0.0,0.0}];
		     lower_left ->
			 [{0.0,0.0},{MaxU,0.0},{MaxU,MaxV},{0.0,MaxV}]
		 end,
	    {X,Y} = ratio(W0, H0),
	    Vs = [{0.0,-Y,-X},{0.0,Y,-X},{0.0,Y,X},{0.0,-Y,X}],
	    Mesh = #e3d_mesh{type=polygon,fs=Fs,vs=Vs,tx=Tx},
	    Obj = #e3d_object{obj=Mesh},
	    Mat = [{image,[{maps,[{diffuse,Image}]}]},
		   {default,[]}],
	    {new_shape,"image",Obj,Mat}
    end.

ratio(D, D) -> {1.0,1.0};
ratio(W, H) when W < H -> {1.0,H/W};
ratio(W, H) -> {W/H,1.0}.

strip_any_alpha(#e3d_image{type=r8g8b8}=Image) -> Image;
strip_any_alpha(#e3d_image{type=r8g8b8a8}=Image) -> e3d_image:convert(Image, r8g8b8).
    
can_texture_be_loaded({W,H,Pixels}) ->
    gl:texImage2D(?GL_PROXY_TEXTURE_2D, 0, ?GL_RGB,
		  W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Pixels),
    W == gl:getTexLevelParameteriv(?GL_PROXY_TEXTURE_2D, 0,
				   ?GL_TEXTURE_WIDTH).
pad_image({W0,H,Pixels0}=Image) ->
    case nearest_power_two(W0) of
	W0 ->
	    pad_image_1(Image);
	W ->
	    Pad = zeroes(3*(W-W0)),
	    Pixels = pad_rows(Pixels0, 3*W0, Pad, []),
	    pad_image_1({W,H,Pixels})
    end.

pad_image_1({W,H0,Pixels0}=Image) ->
    case nearest_power_two(H0) of
	H0 ->
	    pad_image_2(Image);
	H ->
	    Pad = zeroes(3*W*(H-H0)),
	    Pixels = [Pixels0|Pad],
	    pad_image_2({W,H,Pixels})
    end.

pad_image_2({W,H,Pixels}) when is_list(Pixels) ->
    {W,H,list_to_binary(Pixels)};
pad_image_2(Image) -> Image.

pad_rows(Bin, W, Pad, Acc) ->
    case Bin of
	<<>> -> reverse(Acc);
	<<Row:W/binary,T/binary>> ->
	    pad_rows(T, W, Pad, [[Row|Pad]|Acc])
    end.

zeroes(0) -> [];
zeroes(1) -> [0];
zeroes(N) when N rem 2 == 0 ->
    Z = zeroes(N div 2),
    [Z|Z];
zeroes(N) ->
    Z = zeroes(N div 2),
    [0,Z|Z].

nearest_power_two(N) ->
    nearest_power_two(N, 1).

nearest_power_two(N, B) when N =< B -> B;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).
