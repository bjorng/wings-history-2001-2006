%%
%%  wings_body.erl --
%%
%%     This module contains most of the command for entire Wings objects.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_body.erl,v 1.8 2001/09/14 09:58:02 bjorng Exp $
%%

-module(wings_body).
-export([convert_selection/1,cleanup/1,
	 invert_normals/1,flip/2,duplicate/2,delete/1,
	 tighten/1,smooth/1,combine/1,separate/1,auto_smooth/1]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,seq/2]).

%%
%% Convert the current selection to a body selection.
%%
convert_selection(#st{sel=Sel0}=St) ->
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || {Id,_} <- Sel0],
    St#st{selmode=body,sel=Sel}.

%%%
%%% The Cleanup command.
%%%

cleanup(St) ->
    wings_sel:map(fun cleanup_1/1, St).

cleanup_1(#shape{sh=#we{}=We0}=Sh) ->
    We1 = clean_winged_vertices(We0),
    We = clean_short_edges(We1),
    Sh#shape{sh=We};
cleanup_1(Other) -> Other.

clean_winged_vertices(We) ->
    wings_util:fold_vertex(
      fun(V, _, W) ->
	      case wings_vertex:dissolve(V, W) of
		  error -> W;
		  Other -> Other
	      end
      end, We, We).

clean_short_edges(#we{es=Etab,vs=Vtab}=We) ->
    Short = foldl(
	      fun({Edge,#edge{vs=Va,ve=Vb}}, A) ->
		      VaPos = wings_vertex:pos(Va, Vtab),
		      VbPos = wings_vertex:pos(Vb, Vtab),
		      case abs(e3d_vec:dist(VaPos, VbPos)) of
			  Dist when Dist < 1.0E-3 -> [Edge|A];
			  Dist -> A
		      end
	      end, [], gb_trees:to_list(Etab)),
    foldl(fun(Edge, #we{es=Et}=W) ->
		  case gb_trees:is_defined(Edge, Et) of
		      true -> wings_collapse:collapse_edge(Edge, W);
		      false -> W
		  end
	  end, We, Short).

%%%
%%% The Invert command.
%%%

invert_normals(St0) ->
    wings_sel:map(fun invert_normals_1/1, St0).

invert_normals_1(#shape{sh=#we{}=We0}=Sh0) ->
    We = wings_we:invert_normals(We0),
    Sh0#shape{sh=We}.

%%%
%%% The Duplicate command.
%%%

duplicate(Dir, #st{onext=Oid0}=St0) ->
    Copy = "copy",
    St = wings_sel:fold(fun(Sh0, St) ->
				wings_shape:insert(Sh0, Copy, St)
			end, St0, St0),
    %% Select the duplicate items, not the original items.
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || Id <- seq(Oid0, St#st.onext-1)],
    wings_move:setup(Dir, St#st{sel=Sel}).

%%%
%%% The Delete command.
%%%

delete(#st{shapes=Shapes0}=St) ->
    Shapes = wings_sel:fold(fun(#shape{id=Id}, Shs) ->
				    gb_trees:delete(Id, Shs)
			    end, Shapes0, St),
    St#st{shapes=Shapes,sel=[]}.

%%%
%%% The Flip command
%%%

flip(Plane0, St) ->
    Plane = flip_scale(Plane0),
    wings_sel:map(fun(Sh) -> flip_body(Plane, Sh) end, St).

flip_body(Plane, #shape{sh=#we{}=We0}=Sh) ->
    {Cx,Cy,Cz} = e3d_vec:average(wings_vertex:bounding_box(We0)),
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, Plane),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    We1 = wings_we:transform_vs(M, We0),
    We = wings_we:invert_normals(We1),
    Sh#shape{sh=We}.

flip_scale(x) -> e3d_mat:scale(-1.0, 1.0, 1.0);
flip_scale(y) -> e3d_mat:scale(1.0, -1.0, 1.0);
flip_scale(z) -> e3d_mat:scale(1.0, 1.0, -1.0).

%%%
%%% The Tighten command.
%%%

tighten(St0) ->
    {St,Tvs} = wings_sel:mapfold(fun tighten/2, [], St0),
    wings_drag:init_drag(Tvs, none, St).

tighten(#shape{id=Id,sh=#we{vs=Vtab}=We0}=Sh, A) ->
    Vs = gb_trees:keys(Vtab),
    {We,Tvs} = wings_vertex_cmd:tighten(Id, Vs, We0, A),
    {Sh#shape{sh=We},Tvs}.
    
%%%
%%% The Smooth command.
%%%
%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.
%%% 

smooth(St) ->
    wings_sel:map(
      fun(#shape{sh=#we{fs=Ftab,he=Htab}=We0}=Sh0) ->
	      We = wings_subdiv:smooth(We0),
	      Sh = Sh0#shape{sh=We};
	 (Sh) -> Sh
      end, St).

%%%
%%% The Combine command.
%%%

combine(#st{sel=[]}=St) -> St;
combine(#st{shapes=Shapes0,sel=[{Id,_}=Sel|T]}=St) ->
    case gb_trees:get(Id, Shapes0) of
	#shape{sh=#we{}=We0}=Sh ->
	    {We,Shapes1} = combine(T, Shapes0, [We0]),
	    Shapes = gb_trees:update(Id, Sh#shape{sh=We}, Shapes1),
	    St#st{shapes=Shapes,sel=[Sel]};
	Other -> combine(St#st{sel=T})
    end.

combine([{Id,_}|T], Shapes0, Acc) ->
    case gb_trees:get(Id, Shapes0) of
	#shape{sh=#we{}=We} ->
	    Shapes = gb_trees:delete(Id, Shapes0),
	    combine(T, Shapes, [We|Acc]);
	Other -> combine(T, Shapes0, Acc)
    end;
combine([], Shapes, Acc) ->
    We = wings_we:merge(Acc),
    {We,Shapes}.

%%%
%%% The Separate command.
%%%

separate(St) ->
    Sep = "sep",
    wings_sel:fold(
      fun(#shape{sh=#we{}=We0}=Sh0, St0) ->
	      case wings_we:separate(We0) of
		  [_] -> St0;
		  [We|Wes] ->
		      St1 = foldl(fun(W, A) ->
					  Sh = Sh0#shape{sh=W},
					  wings_shape:insert(Sh, Sep, A)
				  end, St0, Wes),
		      Sh = Sh0#shape{sh=We},
		      wings_shape:update(Sh, St1)
	      end
      end, St, St).

%%%
%%% The Auto-Smooth command.
%%%

auto_smooth(St) ->
    wings_sel:map(
      fun(#shape{sh=#we{}=We0}=Sh0) ->
	      We = auto_smooth_1(We0),
	      Sh = Sh0#shape{sh=We};
	 (Sh) -> Sh
      end, St).

auto_smooth_1(#we{he=Htab0}=We) ->
    Htab = wings_util:fold_edge(fun(E, R, A) ->
					auto_smooth(E, R, A, We)
				end, Htab0, We),
    We#we{he=Htab}.

auto_smooth(Edge, #edge{lf=Lf,rf=Rf}, H0, We) ->
    Ln = wings_face:normal(Lf, We),
    Lr = wings_face:normal(Rf, We),
    case e3d_vec:is_zero(Ln) or e3d_vec:is_zero(Lr) of
	true -> H0;				%Ignore this edge.
	false ->
	    case e3d_vec:dot(Ln, Lr) of
		P when P < 0.5 ->		%cos(60), i.e. angle > 60
		    wings_edge:hardness(Edge, hard, H0);
		P ->				%angle =< 60
		    wings_edge:hardness(Edge, soft, H0)
	    end
    end.
