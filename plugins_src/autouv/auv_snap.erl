%%%-------------------------------------------------------------------
%%% File    : auv_snap.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Description : Snapp texture (image) to model
%%%
%%% Created : 28 May 2003 by  <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(auv_snap).

-define(NEED_OPENGL, 1).

-include("wings.hrl").
-include("e3d_image.hrl").

-export([active/0, 
	 select_image/1, 
	 cancel/1, 
	 complete/1]).

active() ->
    case get(?MODULE) of
	undefined -> false;	    
	_Else -> true
    end.

select_image(_St) ->
    Images = find_images(),
    case Images of
	[] -> 
	    wpa:error("No images present, import an image first.");
	_ ->
	    Qs = [{vframe, Images}],
	    Select = fun([Reply]) ->		     
			     TId = wings_image:txid(Reply),
			     Draw = fun(St) -> draw_image(TId,St) end,
			     wings:register_postdraw_hook(geom, ?MODULE,Draw),
			     #e3d_image{width=W, height=H} = wings_image:info(Reply),
			     put(?MODULE,{Reply,W,H}),
			     ignore
		     end,
	    wings_ask:dialog("Choose Image to snap",
			     Qs,Select)
    end.

find_images() ->
    case wings_image:images() of 
	[] -> [];
	Imgs = [{Def,_}|_] ->
	    DefVar = {answer,Def},
	    [{alt, DefVar, Name, Id} || 
		{Id, #e3d_image{name=Name}} <- Imgs]
    end.

draw_image(Image,_St) ->
    {_,_,W,H} = wings_wm:viewport(),

    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),        
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0.0, 1.0, 0.0, 1.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_DEPTH_TEST),    
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, Image),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),

    gl:'begin'(?GL_QUADS),
    gl:color4f(1.0, 1.0, 1.0, 0.3),   %%Semitransparant
    {X,Y} = scale(W,H),
    {Xs,Ys,Xe,Ye} = {0,0,1,1},
    gl:texCoord2f(0-X,0-Y),    gl:vertex2f(Xs,Ys),
    gl:texCoord2f(1+X,0-Y),    gl:vertex2f(Xe,Ys),
    gl:texCoord2f(1+X,1+Y),    gl:vertex2f(Xe,Ye),
    gl:texCoord2f(0-X,1+Y),    gl:vertex2f(Xs,Ye),
    gl:'end'(),
    gl:popAttrib().
    
scale(W,H) ->
    {_,IW,IH} = get(?MODULE),
    if 
	W == H ->
	    if 
		IW == IH -> 
		    {0,0};
		IW > IH ->
		    {0.0,(IW/IH-1)/2};
		true ->
		    {(IH/IW-1)/2, 0.0}
	    end;
	W > H ->
	    {(W/H-1)/2, 0.0};

% 	    if IW == IH ->
% 		    {(W/H-1)/2, 0.0};
% 	       IH > IW ->
% 		    {(IH/IW-1)*(W/H-1)/2, 0.0};
% 	       (IW/IH) > (W/H) ->
% 		    {0.0, (IW/IH-1)*(W/H-1)/2};
% 	       true ->
% 		    {(IW/IH-1)*(W/H-1)/2,0.0}
% 	    end;
	true -> 
	    {0.0, (H/W-1)/2}
% 	    if IW == IH ->
% 		    {0.0, (H/W-1)/2};
% 	       IW > IH ->
% 		    {0.0, (IW/IH-1)*(H/W-1)/2};
% 	       (IH/IW) > (H/W) ->
% 		    {(IH/IW-1)*(H/W-1)/2,0.0};
% 	       true -> 
% 		    {0.0,(IH/IW-1)*(H/W-1)/2}
% 	    end
    end.

cancel(St) ->
    wings:unregister_postdraw_hook(geom, ?MODULE),
    erase(?MODULE),
    St.

complete(St0) ->
    {Image, _,_} = get(?MODULE),
    St1 = set_materials(Image, St0),
    io:format("Completed ~p ~n", [St1#st.mat]),
    Res = insert_uvs(St1),
    wings:unregister_postdraw_hook(geom, ?MODULE),
    erase(?MODULE),
    Res.

insert_uvs(St0) ->
    {MM,PM,Viewport = {_,_,W,H}} = wings_util:get_matrices(0, original),
    {Xs,Ys} = scale(W,H),
    wings_sel:map(fun(Items, We=#we{vp=Vs,es=Etab0}) ->  
			  AddUv = fun(Face, _V, Edge, Rec0, E0) ->
					  case Rec0 of
					      #edge{vs=V,lf=Face} ->
						  Rec  = gb_trees:get(Edge, E0),
						  {X,Y,Z} = gb_trees:get(V, Vs),
						  {S,T, _} = glu:project(X,Y,Z,MM,PM,Viewport),
						  gb_trees:update(Edge, Rec#edge{a={S/W-Xs,T/W-Ys}}, 
								  E0);
					      #edge{ve=V,rf=Face} ->
						  Rec  = gb_trees:get(Edge, E0),
						  {X,Y,Z} = gb_trees:get(V, Vs),
						  {S,T, _} = glu:project(X,Y,Z,MM,PM,Viewport),
						  gb_trees:update(Edge, Rec#edge{b={S/W-Xs,T/W-Ys}}, 
								  E0)
					  end
				  end,
			  Etab = wings_face:fold_faces(AddUv, Etab0, Items, We),
			  We#we{es=Etab}
		  end, St0).

set_materials(Image,St0) ->     
    Fix = fun(Items,We0,NewMats0) ->
		  case We0#we.mode of
		      vertex -> 
			  wpa:error("Can't put on image when object has vertex colors");
		      _ -> 
			  continue
		  end,
		  Set = fun(Face,_,_,_,{We1,NMats0}) ->
				FaceM = wings_material:get(Face,We0),
				case lists:keysearch(FaceM,1,element(1,NMats0)) of
				    false ->
					{MatName,NMats} = dup_mat(FaceM, NMats0,Image),
					{wings_material:assign(MatName,[Face],We1),NMats};
				    {value, {_Old,MatName}}->
					{wings_material:assign(MatName,[Face],We1),NMats0}
				end
			end,
		  {We,NewMats} = 
		      wings_face:fold_faces(Set, {We0,NewMats0}, Items, We0),
		  {We#we{mode=uv},NewMats}
	  end,
    {St1, {_,NewMats}} = wings_sel:mapfold(Fix, {[],St0}, St0),
    St1#st{mat=NewMats#st.mat}.

dup_mat(MatName,{Used,St0},Image) ->
    Mat0 = gb_trees:get(MatName, St0#st.mat),
    Maps0 = proplists:get_value(maps, Mat0),    
    case proplists:get_value(diffuse, Maps0) of
	Image -> 
	    %% It already has the texture no need to create new material
	    {MatName,{[{MatName,MatName}|Used],St0}};
	Else ->
	    io:format("~p ~p~n", [?LINE, Else]),
	    NewMatName = list_to_atom(atom_to_list(MatName) ++ "_snap"),
	    Maps = case Else of
		       undefined -> [{diffuse,Image}|Maps0];
		       _ -> lists:keyreplace(diffuse,1,Maps0,{diffuse,Image})
		   end,
	    Mat   = {NewMatName,lists:keyreplace(maps,1,Mat0, {maps,Maps})},
	    case wings_material:add_materials([Mat], St0) of
		{St,[]} ->
		    {NewMatName, {[{MatName,NewMatName}|Used],St}};
		{St,[{NewMatName,ChangedMatName}]} ->
		    {ChangedMatName, {[{MatName,ChangedMatName}|Used],St}}
	    end
    end.
    
