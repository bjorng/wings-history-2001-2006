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
%%     $Id: wpc_image.erl,v 1.1 2002/07/28 12:38:44 bjorng Exp $
%%

-module(wpc_image).
-export([init/0,menu/2,command/2]).

-include_lib("e3d.hrl").

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

command({shape,{image,Format}}, St) -> make_image(Format, St);
command(_, _) -> next.

make_image(Format, St) ->
    Ps = props(Format),
    case wpa:import_filename(Ps) of
	aborted -> keep;
	Name ->
	    M = [image],
	    Fs = [#e3d_face{vs=[0,3,2,1],tx=[1,0,3,2],mat=M},
		  #e3d_face{vs=[1,2,3,0],tx=[2,3,0,1],mat=M}],
	    Tx = [{0.0,0.0},{1.0,0.0},{1.0,1.0},{0.0,1.0}],
	    Vs = [{0.0,-1.0,-1.0},{0.0,1.0,-1.0},{0.0,1.0,1.0},{0.0,-1.0,1.0}],
	    Mesh = #e3d_mesh{type=polygon,fs=Fs,vs=Vs,tx=Tx},
	    Obj = #e3d_object{obj=Mesh},
	    Mat = [{image,[{maps,[{diffuse,Name}]}]},
		   {default,[]}],
	    {new_shape,"image",Obj,Mat}
    end.

props(bmp) -> [{ext,".bmp"},{ext_desc,"BMP Bitmap"}];
props(tif) -> [{ext,".tif"},{ext_desc,"Tiff Bitmap"}];
props(tga) -> [{ext,".tga"},{ext_desc,"Targa File"}].

    
