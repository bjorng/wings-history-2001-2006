%%
%%  wings_ff_ndo.erl --
%%
%%     Import of Nendo .ndo files.
%%
%%  Copyright (c) 2001 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_ff_ndo.erl,v 1.7 2001/11/20 12:49:22 bjorng Exp $
%%

-module(wings_ff_ndo).
-export([import/2,export/2]).
-include("wings.hrl").
-import(lists, [sort/1,reverse/1,foldl/3,last/1]).

-define(NDO_HEADER10, "nendo 1.0").
-define(NDO_HEADER11, "nendo 1.1").

import(Name, St) ->
    case file:read_file(Name) of
	{ok,<<?NDO_HEADER10,Data/binary>>} ->
            {error,"Nendo 1.0 files not supported"};
	{ok,<<?NDO_HEADER11,Data/binary>>} ->
            import_1(Data, St);
	{ok,Bin} ->
	    {error,"not a Nendo file"};
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_1(<<_:8,NumObjs:16,_:8,Objs/binary>>, St0) ->
    io:format("~w object(s)\n", [NumObjs]),
    St = read_objects(NumObjs, Objs, St0).

read_objects(0, _, St) -> St;
read_objects(N, <<>>, St) ->
    io:format("  ~w empty object(s)\n", [N]),
    St;
read_objects(N, Bin, St0) ->
    case read_object(Bin) of
	bad ->
	    <<First:16/binary,_/binary>> = Bin,
	    io:format("  garbage: ~w\n", [First]),
	    St0;
	{Name,We,Rest} ->
	    St = wings_shape:new(Name, We, St0),
	    read_objects(N-1, Rest, St)
    end.

read_object(Bin) ->
    %%show_first(Bin),
    read_object_0(Bin).

read_object_0(<<0:8,_/binary>>=T) -> read_object_1(T);
read_object_0(<<_:8,T/binary>>) -> read_object_1(T).

read_object_1(<<0:16,T/binary>>) ->
    read_object(T);
read_object_1(<<1:16,C:8,_/binary>>=T0) when C < $\s; C > $~ ->
    <<_:16,T/binary>> = T0,
    read_object_1(T);
read_object_1(<<L:16,T0/binary>>) ->
    case get_name(L, T0) of
	bad -> bad;
	{Name,T1} ->
	    io:format("~w: ~s\n", [L,Name]),
	    <<Vis:8,Sensivity:8,_:8,_:8,_:72/binary,T2/binary>> = T1,
	    {Etab,Htab,T3} = read_edges(T2),
	    {Ftab,T4} = read_faces(T3),
	    {Vtab,T5} = read_vertices(T4),
	    T = skip_rest(T5),
	    We0 = #we{es=Etab,vs=Vtab,fs=Ftab,he=Htab},
	    We = set_next_id(We0),
	    {Name,We,T}
    end.

get_name(L, Bin) when size(Bin) < L -> bad;
get_name(L, Bin) ->
    <<Name0:L/binary,T/binary>> = Bin,
    Name = binary_to_list(Name0),
    foldl(fun(C, Val) when $\s =< C, C < 127 -> Val;
	     (C, _) -> bad
	  end, {Name,T}, Name).

skip_rest(<<Sz0:16,T0/binary>>) ->
    Sz = 2*Sz0,
    <<Skip:Sz/binary,T/binary>> = T0,
    %%io:format("  skipping ~w: ~w\n", [Szip,Sk]),
    skip_rest_1(T).

skip_rest_1(<<Sz0:16,T0/binary>>=Bin) ->
    Sz = 2*Sz0,
    <<Skip:Sz/binary,T/binary>> = T0,
%%    io:format("  skipping ~w: ~w\n", [Sz,Skip]),
%%    show_first(T),
    skip_rest_2(T).

skip_rest_2(<<0:8,T/binary>>) -> T;
skip_rest_2(<<2:8,Sz1:16,Sz2:16,T/binary>>=Bin) ->
    Sz = Sz1 * Sz2,
    skip_texture(Sz, T).

skip_texture(0, T) -> T;
skip_texture(N, <<Pixels:8,RGB:24,T/binary>>) when N > 0 ->
    skip_texture(N-Pixels, T).

read_edges(<<NumEdges:16,T/binary>>) ->
    io:format(" edges ~w\n", [NumEdges]),
    read_edges(0, NumEdges, T, [], []).
    
read_edges(N, N, T, Eacc, Hacc) ->
    Etab = gb_trees:from_orddict(reverse(Eacc)),
    Htab = gb_sets:from_ordset(reverse(Hacc)),
    {Etab,Htab,T};
read_edges(Edge, N, <<EdgeRec0:25/binary,T/binary>>, Eacc, Hacc0) ->
    <<Vb:16,Va:16,Lf:16,Rf:16,Ltsu:16,Rtsu:16,Rtpr:16,Ltpr:16,
     Hardness:8,BColor0:4/binary,AColor0:4/binary>> = EdgeRec0,
    AColor = convert_color(AColor0),
    BColor = convert_color(BColor0),
    EdgeRec = {Edge,#edge{vs=Va,ve=Vb,a=AColor,b=BColor,lf=Lf,rf=Rf,
			  ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu}},
    Hacc = if
	       Hardness == 0 -> Hacc0;
	       true -> [Edge|Hacc0]
	   end,
    read_edges(Edge+1, N, T, [EdgeRec|Eacc], Hacc).

read_faces(<<NumFaces:16,T/binary>>) ->
    io:format(" faces ~w\n", [NumFaces]),
    read_faces(0, NumFaces, T, []).

read_faces(N, N, T, Acc) ->
    {gb_trees:from_orddict(reverse(Acc)),T};
read_faces(Face, N, <<Edge:16,T/binary>>, Acc) ->
    FaceRec = {Face,#face{edge=Edge}},
    read_faces(Face+1, N, T, [FaceRec|Acc]).

read_vertices(<<NumVertices:16,T/binary>>) ->
    io:format(" vertices ~w\n", [NumVertices]),
    read_vertices(0, NumVertices, T, []).
    
read_vertices(N, N, T, Acc) ->
    {gb_trees:from_orddict(reverse(Acc)),T};
read_vertices(V, N,
	      <<Edge:16,X:32/float,Y:32/float,Z:32/float,T/binary>>, Acc) ->
    Pos = wings_util:share(X/10.0, Y/10.0, Z/10.0),
    Vtx = {V,#vtx{edge=Edge,pos=Pos}},
    read_vertices(V+1, N, T, [Vtx|Acc]).

set_next_id(#we{es=Etab,vs=Vtab,fs=Ftab}=We) ->
    NextId = last(sort([gb_trees:size(Etab),
			gb_trees:size(Ftab),
			gb_trees:size(Vtab)])),
    We#we{first_id=0,next_id=NextId}.

% show_first(<<First:32/binary,_/binary>>) ->
%     io:format("~w\n", [First]);
% show_first(Bin) ->
%     io:format("~w\n", [Bin]).


%%
%% Export.
%%

export(Name, #st{hidden=Hidden,shapes=Shapes0}=St) ->
    Shapes1 = gb_trees:values(Hidden) ++ gb_trees:values(Shapes0),
    Shapes2 = foldl(fun(Sh, A) ->
			shape(Sh, A)
		end, [], Shapes1),
    Shapes = reverse(Shapes2),
    write_file(Name, Shapes).

shape(#shape{name=Name,sh=We0}, Acc) ->
    NameChunk = [<<(length(Name)):16>>|Name],
    Vis = 1,
    Sense = 1,
    Shaded = 1,
    EnableColors = 1,
    Header = <<Vis:8,Sense:8,Shaded:8,EnableColors:8,0:72/unit:8>>,
    We = wings_we:renumber(We0, 0),
    #we{vs=Vs,es=Etab,fs=Ftab,he=Htab} = We,
    EdgeChunk = write_edges(gb_trees:to_list(Etab), Htab, []),
    FaceChunk = write_faces(gb_trees:values(Ftab), []),
    VertexChunk = write_vertices(gb_trees:values(Vs), []),
    FillChunk = [0,0,0,0,0,1],
    [[NameChunk,Header,EdgeChunk,FaceChunk,VertexChunk,FillChunk]|Acc].

write_edges([{Edge,Erec0}|Es], Htab, Acc) ->
    Hardness = case gb_sets:is_member(Edge, Htab) of
		   false -> 0;
		   true -> 1
	       end,
    #edge{vs=Va,ve=Vb,a=ACol,b=BCol,lf=Lf,rf=Rf,
	  ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu} = Erec0,
    Erec = [<<Vb:16,Va:16,Lf:16,Rf:16,Ltsu:16,Rtsu:16,Rtpr:16,Ltpr:16,
	     Hardness:8>>,convert_color(BCol)|convert_color(ACol)],
    write_edges(Es, Htab, [Erec|Acc]);
write_edges([], Htab, Acc) ->
    list_to_binary([<<(length(Acc)):16>>|reverse(Acc)]).

write_faces([#face{edge=Edge}|Fs], Acc) ->
    write_faces(Fs, [<<Edge:16>>|Acc]);
write_faces([], Acc) ->
    list_to_binary([<<(length(Acc)):16>>|reverse(Acc)]).

write_vertices([#vtx{pos={X,Y,Z},edge=Edge}|Fs], Acc) ->
    Vtx = <<Edge:16,(X*10):32/float,(Y*10):32/float,(Z*10):32/float>>,
    write_vertices(Fs, [Vtx|Acc]);
write_vertices([], Acc) ->
    list_to_binary([<<(length(Acc)):16>>|reverse(Acc)]).

write_file(Name, Objects) ->
    NumObjs = length(Objects),
    Data = [<<?NDO_HEADER11,0:8,NumObjs:16,1:8>>|Objects],
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

%%%
%%% Common utilities.
%%%

convert_color(<<R:8,G:8,B:8,A:8>>) ->
    wings_color:share({R/255,G/255,B/255});
convert_color({R,G,B}) ->
    [trunc(R*255),trunc(G*255),trunc(B*255),255].

