%%
%%  wings_image.erl --
%%
%%     This module manages images.
%%
%%  Copyright (c) 2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_image.erl,v 1.9 2003/01/30 09:53:55 bjorng Exp $
%%

-module(wings_image).
-export([init/0,init_opengl/0,
	 from_file/1,new/2,rename/2,txid/1,info/1,images/0,
	 next_id/0,delete_older/1,delete_from/1,
	 update/2,update_filename/2,
	 window/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").
-import(lists, [reverse/1,foreach/2,flatten/1]).

init() ->
    SdlWrapper = get(sdlwrapper),
    spawn_opt(fun() -> server(SdlWrapper) end, [link,{fullsweep_after,0}]).

init_opengl() ->
    req(init_opengl).

%%%
%%% Client API.
%%%

from_file(Filename) ->
    Props = [{filename,Filename},{alignment,1}],
    case wpa:image_read(Props) of
	#e3d_image{}=Image ->
	    Name = filename:rootname(filename:basename(Filename)),
	    req({new,Image#e3d_image{filename=Filename,name=Name}});
	{error,_}=Error -> Error
    end.

new(Name, E3DImage) ->
    req({new,E3DImage#e3d_image{name=Name}}).

rename(Id, NewName) ->
    req({rename,Id,NewName}).

txid(Id) ->
    req({txid,Id}, false).

info(Id) ->
    req({info,Id}, false).

images() ->
    req(images, false).

next_id() ->
    req(next_id, false).

delete_older(Id) ->
    req({delete_older,Id}).

delete_from(Id) ->
    req({delete_from,Id}).

update(Id, Image) ->
    req({update,Id,Image}).

update_filename(Id, Filename) ->
    req({update_filename,Id,Filename}).

req(Req) ->
    req(Req, true).

req(Req, Notify) ->
    Self = self(),
    Ref = make_ref(),
    wings_image ! {Self,Ref,Req},
    receive
	{Ref,Answer} ->
	    case Notify of
		false -> ok;
		true -> wings_wm:notify(image_change)
	    end,
	    Answer
    end.

%%%
%%% Server implementation.
%%%
%%% Reason for using a server: Convenient encapsulation of images.
%%% We'll get an entire process dictionary to use for texture ids.
%%% 

-record(ist,
	{next=0,				%Next image ID.
	 images					%All images (gb_trees).
	}).

server(SdlWrapper) ->
    put(sdlwrapper, SdlWrapper),
    register(wings_image, self()),
    loop(#ist{images=gb_trees:empty()}).

loop(S0) ->
    receive
	{Client,Ref,Req} ->
	    case handle(Req, S0) of
		#ist{}=S ->
		    Client ! {Ref,ok};
		{Resp,S} ->
		    Client ! {Ref,Resp}
	    end,
	    loop(S);
	Other ->
	    exit({bad_message_to_wings_image,Other})
    end.

handle(init_opengl, #ist{images=Images}=S) ->
    foreach(fun({Id,Image}) ->
		    make_texture(Id, Image)
	    end, gb_trees:to_list(Images)),
    S;
handle({new,#e3d_image{name=Name0}=Im0}, #ist{next=Id,images=Images0}=S) ->
    Name = make_unique(Name0, Images0),
    Im = Im0#e3d_image{name=Name},
    Images = gb_trees:insert(Id, Im, Images0),
    make_texture(Id, Im),
    {Id,S#ist{next=Id+1,images=Images}};
handle({rename,Id,Name0}, #ist{images=Images0}=S) ->
    Name = make_unique(Name0, gb_trees:delete(Id, Images0)),
    Im0 = gb_trees:get(Id, Images0),
    Im = Im0#e3d_image{name=Name},
    Images = gb_trees:update(Id, Im, Images0),
    {Id,S#ist{images=Images}};
handle({txid,Id}, S) ->
    {case get(Id) of
	 undefined -> none;
	 TxId -> TxId
     end,S};
handle({info,Id}, #ist{images=Images}=S) ->
    {gb_trees:get(Id, Images),S};
handle(images, #ist{images=Images}=S) ->
    {gb_trees:to_list(Images),S};
handle(next_id, #ist{next=Id}=S) ->
    {Id,S};
handle({delete_older,Id}, S) ->
    delete_older(Id, S);
handle({delete_from,Id}, S) ->
    delete_from(Id, S);
handle({update,Id,Image}, S) ->
    do_update(Id, Image, S);
handle({update_filename,Id,NewName}, #ist{images=Images0}=S) ->
    Im0 = gb_trees:get(Id, Images0),
    Im = Im0#e3d_image{filename=NewName},
    Images = gb_trees:update(Id, Im, Images0),
    S#ist{images=Images}.

make_texture(Id, Image) ->
    TxId = init_texture(Image),
    put(Id, TxId).

init_texture(Image0) ->
    Image = maybe_scale(Image0),
    #e3d_image{width=W,height=H,bytes_pp=BytesPerPixel,image=Bits} = Image,
    [TxId] = gl:genTextures(1),
    gl:pushAttrib(?GL_TEXTURE_BIT),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    Format = texture_format(Image),
    gl:texImage2D(?GL_TEXTURE_2D, 0, BytesPerPixel,
		  W, H, 0, Format, ?GL_UNSIGNED_BYTE, Bits),
    gl:popAttrib(),
    TxId.

maybe_scale(#e3d_image{width=W0,height=H0,bytes_pp=BytesPerPixel,
		       image=Bits0}=Image) ->
    case {nearest_power_two(W0),nearest_power_two(H0)} of
	{W0,H0} -> Image;
	{W,H} ->
	    Out = sdl_util:malloc(BytesPerPixel*W*H, ?GL_UNSIGNED_BYTE),
	    Format = texture_format(Image),
	    glu:scaleImage(Format, W0, H0, ?GL_UNSIGNED_BYTE,
			   Bits0, W, H, ?GL_UNSIGNED_BYTE, Out),
	    Bits = sdl_util:readBin(Out, BytesPerPixel*W*H),
	    sdl_util:free(Out),
	    Image#e3d_image{width=W,height=H,image=Bits}
    end.

nearest_power_two(N) when (N band -N) =:= N -> N;
nearest_power_two(N) -> nearest_power_two(N, 1).

nearest_power_two(N, B) when B > N -> B bsr 1;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).

texture_format(#e3d_image{type=r8g8b8}) -> ?GL_RGB;
texture_format(#e3d_image{type=r8g8b8a8}) -> ?GL_RGBA;
texture_format(#e3d_image{type=b8g8r8}) -> ?GL_BGR;
texture_format(#e3d_image{type=b8g8r8a8}) -> ?GL_BGRA;
texture_format(#e3d_image{type=g8}) -> ?GL_LUMINANCE.

delete_older(Id, #ist{images=Images0}=S) ->
    Images1 = delete_older_1(gb_trees:to_list(Images0), Id),
    Images = gb_trees:from_orddict(Images1),
    S#ist{images=Images}.

delete_older_1([{Id,_}|T], Limit) when Id < Limit ->
    gl:deleteTextures(1, [get(Id)]),
    erase(Id),
    delete_older_1(T, Limit);
delete_older_1(Images, _) -> Images.

delete_from(Id, #ist{images=Images0}=S) ->
    Images1 = delete_from_1(gb_trees:to_list(Images0), Id, []),
    Images = gb_trees:from_orddict(Images1),
    S#ist{images=Images}.

delete_from_1([{Id,_}=Im|T], Limit, Acc) when Id < Limit ->
    delete_from_1(T, Limit, [Im|Acc]);
delete_from_1([{Id,_}|T], Limit, Acc) ->
    gl:deleteTextures(1, [get(Id)]),
    erase(Id),
    delete_from_1(T, Limit, Acc);
delete_from_1([], _, Acc) ->
    reverse(Acc).

do_update(Id, #e3d_image{width=W,height=H,image=Bits}, #ist{images=Images0}=S) ->
    #e3d_image{width=W,height=H} = Im0 = gb_trees:get(Id, Images0),
    Im = Im0#e3d_image{image=Bits},
    Images = gb_trees:update(Id, Im, Images0),
    TxId = get(Id),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texSubImage2D(?GL_TEXTURE_2D, 0, 0, 0,
		     W, H, texture_format(Im), ?GL_UNSIGNED_BYTE, Bits),
    S#ist{images=Images}.

make_unique(Name, Images0) ->
    Images = [N || #e3d_image{name=N} <- gb_trees:values(Images0)],
    wings_util:unique_name(Name, Images).

%%%
%%% Window for image.
%%%

window(Id) ->
    Name = {image,Id},
    case wings_wm:is_window(Name) of
	true ->
	    wings_wm:delete(Name);
	false ->
	    {Size,Title} = window_params(Id),
	    Pos = {10,50,highest},
	    Op = {seq,push,window_fun(Id)},
	    wings_wm:toplevel(Name, Title, Pos, Size,
			      [resizable,closable], Op)
    end.

window_params(Id) ->
    #e3d_image{width=W0,height=H0,name=Name,bytes_pp=BytesPerPixel} = info(Id),
    Title = flatten(io_lib:format("Image: ~s [~wx~wx~w]",
				  [Name,W0,H0,8*BytesPerPixel])),
    {DeskW,DeskH} = wings_wm:win_size(desktop),
    W = if
	    W0 < 250 -> 250;
	    W0+50 < DeskW -> W0+2;
	    true -> DeskW - 50
	end,
    H = if
	    H0+70 < DeskH -> H0+2;
	    true -> DeskH - 70
	end,
    {{W,H},Title}.

window_fun(Id) ->
    fun(Ev) ->
	    event(Ev, Id)
    end.

event(redraw, Id) ->
    redraw(Id),
    keep;
event(_, _) -> keep.

redraw(Id) ->
    #e3d_image{width=Iw,height=Ih,order=Order} = info(Id),
    case Order of
	upper_left ->
	    Ua = 0, Ub = 1,
	    Va = 0, Vb = 1;
	lower_left ->
	    Ua = 0, Ub = 1,
	    Va = 1, Vb = 0
    end,
    Aspect = Iw/Ih,
    {W0,H0} = wings_wm:win_size(),
    wings_io:ortho_setup(),
    wings_io:border(0, 0, W0-0.5, H0-1, ?PANE_COLOR),
    X = 1,
    Y = 1,
    W1 = W0-2,
    H1 = H0-2,
    W2 = round(Aspect*H1),
    H2 = round(W1/Aspect),
    {W,H} = if
		W2 =< W1 -> {W2,H1};
		true -> {W1,H2}
	    end,
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:bindTexture(?GL_TEXTURE_2D, txid(Id)),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2i(Ua, Va),
    gl:vertex2i(X, Y),
    gl:texCoord2i(Ua, Vb),
    gl:vertex2i(X, Y+H),
    gl:texCoord2i(Ub, Vb),
    gl:vertex2i(X+W, Y+H),
    gl:texCoord2i(Ub, Va),
    gl:vertex2i(X+W, Y),
    gl:'end'(),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D).
