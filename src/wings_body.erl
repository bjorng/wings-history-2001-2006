%%
%%  wings_body.erl --
%%
%%     This module contains most of the command for entire Wings objects.
%%
%%  Copyright (c) 2001-2002 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_body.erl,v 1.19 2002/02/06 17:01:09 bjorng Exp $
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
    wings_sel:set(body, Sel, St).

%%%
%%% The Cleanup command.
%%%

cleanup(St) ->
    wings_sel:map(fun cleanup_1/2, St).

cleanup_1(_, We0) ->
    We = clean_winged_vertices(We0),
    clean_short_edges(We).

clean_winged_vertices(#we{vs=Vtab}=We) ->
    foldl(fun(V, W) ->
		  case wings_vertex:dissolve(V, W) of
		      error -> W;
		      Other -> Other
		  end
	  end, We, gb_trees:keys(Vtab)).

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

invert_normals(St) ->
    wings_sel:map(fun(_, We) -> wings_we:invert_normals(We) end, St).

%%%
%%% The Duplicate command.
%%%

duplicate(Dir, #st{onext=Oid0}=St0) ->
    Copy = "copy",
    St = wings_sel:fold(fun(_, We, St) ->
				wings_shape:insert(We, Copy, St)
			end, St0, St0),
    %% Select the duplicate items, not the original items.
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || Id <- seq(Oid0, St#st.onext-1)],
    wings_move:setup(Dir, wings_sel:set(Sel, St)).

%%%
%%% The Delete command.
%%%

delete(#st{shapes=Shapes0}=St) ->
    Shapes = wings_sel:fold(fun(_, #we{id=Id}, Shs) ->
				    gb_trees:delete(Id, Shs)
			    end, Shapes0, St),
    St#st{shapes=Shapes,sel=[]}.

%%%
%%% The Flip command
%%%

flip(Plane0, St) ->
    Plane = flip_scale(Plane0),
    wings_sel:map(fun(_, We) -> flip_body(Plane, We) end, St).

flip_body(Plane, We0) ->
    {Cx,Cy,Cz} = e3d_vec:average(wings_vertex:bounding_box(We0)),
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, Plane),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    We = wings_we:transform_vs(M, We0),
    wings_we:invert_normals(We).

flip_scale(x) -> e3d_mat:scale(-1.0, 1.0, 1.0);
flip_scale(y) -> e3d_mat:scale(1.0, -1.0, 1.0);
flip_scale(z) -> e3d_mat:scale(1.0, 1.0, -1.0).

%%%
%%% The Tighten command.
%%%

tighten(St0) ->
    {St,Tvs} = wings_sel:mapfold(fun tighten/3, [], St0),
    wings_drag:init_drag(Tvs, none, St).

tighten(_, #we{vs=Vtab}=We0, A) ->
    Vs = gb_trees:keys(Vtab),
    wings_vertex_cmd:tighten(Vs, We0, A).
    
%%%
%%% The Smooth command.
%%%
%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.
%%% 

smooth(St) ->
    wings_sel:map(
      fun(_, #we{name=Name,fs=Ftab,he=Htab}=We) ->
	      wings_io:progress("Smoothing \"" ++ Name ++ "\""),
	      wings_subdiv:smooth(We)
      end, St).

%%%
%%% The Combine command.
%%%

combine(#st{sel=[]}=St) -> St;
combine(#st{shapes=Shs0,sel=[{Id,_}=S|_]=Sel0}=St) ->
    Shs1 = sofs:from_external(gb_trees:to_list(Shs0), [{id,object}]),
    Sel1 = sofs:from_external(Sel0, [{id,dummy}]),
    Sel2 = sofs:domain(Sel1),
    Wes = sofs:to_external(sofs:range(sofs:restriction(Shs1, Sel2))),
    Mode = unify_modes(Wes),
    We0 = wings_we:merge(Wes),
    We = We0#we{id=Id,mode=Mode},
    Shs2 = sofs:drestriction(Shs1, Sel2),
    Shs = gb_trees:from_orddict(sort([{Id,We}|sofs:to_external(Shs2)])),
    St#st{shapes=Shs,sel=[S]}.

unify_modes([#we{mode=Mode}|Wes]) ->
    unify_modes(Wes, Mode).

unify_modes([#we{mode=Mode}|Wes], Mode) ->
    unify_modes(Wes, Mode);
unify_modes([#we{mode=Mode}|Wes], OldMode) ->
    NewMode = unify_modes_1(sort([OldMode,Mode])),
    unify_modes(Wes, NewMode);
unify_modes([], Mode) -> Mode.

unify_modes_1([_,vertex]) ->
    throw({command_error,"Objects with vertex colors cannot be combined "
	   "with objects with materials and/or textures."});
unify_modes_1([material,uv]) -> uv.

%%%
%%% The Separate command.
%%%

separate(St) ->
    Sep = "sep",
    wings_sel:fold(
      fun(_, #we{id=Id}=We0, St0) ->
	      case wings_we:separate(We0) of
		  [_] -> St0;
		  [We|Wes] ->
		      St1 = foldl(fun(W, A) ->
					  wings_shape:insert(W, Sep, A)
				  end, St0, Wes),
		      wings_shape:replace(Id, We, St1)
	      end
      end, St, St).

%%%
%%% The Auto-Smooth command.
%%%

auto_smooth(St) ->
    wings_sel:map(fun auto_smooth_1/2, St).

auto_smooth_1(_, #we{es=Etab,he=Htab0}=We) ->
    Htab = foldl(fun({E,R}, A) ->
			 auto_smooth(E, R, A, We)
		 end, Htab0, gb_trees:to_list(Etab)),
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
