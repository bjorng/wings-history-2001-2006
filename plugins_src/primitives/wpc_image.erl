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
%%     $Id: wpc_image.erl,v 1.2 2002/08/03 06:56:34 bjorng Exp $
%%

-module(wpc_image).
-export([init/0,menu/2,command/2]).

-include_lib("gl.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

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
    {"Image",{image,[{"BMP",bmp},
		     {"TIFF",tif},
		     {"Targa",tga}]}}.

command({shape,{image,Format}}, _St) -> make_image(Format);
command(_, _) -> next.

make_image(Format) ->
    Ps = props(Format),
    case wpa:import_filename(Ps) of
	aborted -> keep;
	Name ->
	    case e3d_image:load(Name, [{type,r8g8b8},{order,lower_left}]) of
		#e3d_image{width=W,height=H,image=Pixels} ->
		    Image = {W,H,Pixels},
		    make_image_1(Image);
		{error,Error} ->
		    E = io_lib:format("Failed to load \"~s\": ~s\n",
				      [Name,file:format_error(Error)]),
		    wings_util:error(E)
	    end
    end.

make_image_1({W,H,Pixels}=Image) ->
    case can_texture_be_loaded(Image) of
	false ->
	    wings_util:error("The image cannot be loaded as a texture "
			     "(it is probably too large or "
			     " has wrong dimensions).");
	true ->
	    M = [image],
	    Fs = [#e3d_face{vs=[0,3,2,1],tx=[1,0,3,2],mat=M},
		  #e3d_face{vs=[1,2,3,0],tx=[2,3,0,1],mat=M}],
	    Tx = [{0.0,0.0},{1.0,0.0},{1.0,1.0},{0.0,1.0}],
	    {X,Y} = ratio(W, H),
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

can_texture_be_loaded({W,H,Pixels}) ->
    gl:texImage2D(?GL_PROXY_TEXTURE_2D, 0, ?GL_RGB,
		  W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Pixels),
    W == gl:getTexLevelParameteriv(?GL_PROXY_TEXTURE_2D, 0,
				   ?GL_TEXTURE_WIDTH).

props(bmp) -> [{ext,".bmp"},{ext_desc,"BMP Bitmap"}];
props(tif) -> [{ext,".tif"},{ext_desc,"Tiff Bitmap"}];
props(tga) -> [{ext,".tga"},{ext_desc,"Targa File"}].

    
