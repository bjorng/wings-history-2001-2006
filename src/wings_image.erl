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
%%     $Id: wings_image.erl,v 1.2 2003/01/21 13:40:36 bjorng Exp $
%%

-module(wings_image).
-export([init/0,init_opengl/0,
	 new/2,new/3,delete_all/0,txid/1,info/1,images/0,
	 next_id/0,delete_older/1,delete_from/1,
	 update/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").
-import(lists, [reverse/1,foreach/2]).

init() ->
    SdlWrapper = get(sdlwrapper),
    spawn_opt(fun() -> server(SdlWrapper) end, [link,{fullsweep_after,0}]).

init_opengl() ->
    req(init_opengl).

%%%
%%% Client API.
%%%

new(Name, E3DImage) ->
    req({new,Name,E3DImage,none}).

new(Name, E3DImage, Filename) ->
    req({new,Name,E3DImage,Filename}).

delete_all() ->
    req(delete_all).

txid(Id) ->
    req({txid,Id}).

info(Id) ->
    req({info,Id}).

images() ->
    req(images).

next_id() ->
    req(next_id).

delete_older(Id) ->
    req({delete_older,Id}).

delete_from(Id) ->
    req({delete_from,Id}).

update(Id, Image) ->
    req({update,Id,Image}).

req(Req) ->
    Self = self(),
    Ref = make_ref(),
    wings_image ! {Self,Ref,Req},
    receive
	{Ref,Answer} -> Answer
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

-record(im,
	{name,					%Name.
	 file,					%Filename|none
	 im					%e3d_image record.
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
    foreach(fun({Id,#im{im=Image}}) ->
		    make_texture(Id, Image)
	    end, gb_trees:to_list(Images)),
    S;
handle({new,Name0,E3D,Filename}, #ist{next=Id,images=Images0}=S) ->
    Name = make_unique(Name0, Images0),
    Im = #im{name=Name,file=Filename,im=E3D},
    Images = gb_trees:insert(Id, Im, Images0),
    make_texture(Id, E3D),
    {Id,S#ist{next=Id+1,images=Images}};
handle(delete_all, S) ->
    S;
handle({txid,Id}, S) ->
    {case get(Id) of
	 undefined -> none;
	 TxId -> TxId
     end,S};
handle({info,Id}, #ist{images=Images}=S) ->
    #im{im=Image} = gb_trees:get(Id, Images),
    {Image,S};
handle(images, #ist{images=Images}=S) ->
    {[{Id,Name} || {Id,#im{name=Name}} <- gb_trees:to_list(Images)],S};
handle(next_id, #ist{next=Id}=S) ->
    {Id,S};
handle({delete_older,Id}, S) ->
    delete_older(Id, S);
handle({delete_from,Id}, S) ->
    delete_from(Id, S);
handle({update,Id,Image}, S) ->
    do_update(Id, Image, S).

make_texture(Id, Image) ->
    TxId = init_texture(Image),
    put(Id, TxId).

init_texture(Image) ->
    #e3d_image{width=W,height=H,image=Bits} = maybe_scale(Image),
    [TxId] = gl:genTextures(1),
    gl:pushAttrib(?GL_TEXTURE_BIT),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
		  W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Bits),
    gl:popAttrib(),
    TxId.

maybe_scale(#e3d_image{width=W0,height=H0,image=Bits0}=Image) ->
    case {nearest_power_two(W0),nearest_power_two(H0)} of
	{W0,H0} -> Image;
	{W,H} ->
	    In = sdl_util:malloc(W0*H0*3, ?GL_UNSIGNED_BYTE),
	    sdl_util:write(In, Bits0),
	    Out = sdl_util:malloc(W*H*3, ?GL_UNSIGNED_BYTE),
	    glu:scaleImage(?GL_RGB, W0, H0, ?GL_UNSIGNED_BYTE,
			   In, W, H, ?GL_UNSIGNED_BYTE, Out),
	    sdl_util:free(In),
	    Bits = sdl_util:readBin(Out, W*H*3),
	    sdl_util:free(Out),
	    Image#e3d_image{width=W,height=H,image=Bits}
    end.

nearest_power_two(N) when (N band -N) =:= N -> N;
nearest_power_two(N) -> nearest_power_two(N, 1).

nearest_power_two(N, B) when B > N -> B bsr 1;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).

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
    #im{im=#e3d_image{width=W,height=H}=E3D} = Im0 = gb_trees:get(Id, Images0),
    Im = Im0#im{im=E3D#e3d_image{image=Bits}},
    Images = gb_trees:update(Id, Im, Images0),
    TxId = get(Id),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texSubImage2D(?GL_TEXTURE_2D, 0, 0, 0,
		     W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Bits),
    S#ist{images=Images}.

make_unique(Name, Images0) ->
    Images = [N || #im{name=N} <- gb_trees:values(Images0)],
    wings_util:unique_name(Name, Images).

