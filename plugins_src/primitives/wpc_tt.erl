%%
%%  wpc_tt.erl --
%%
%%     Functions for reading TrueType fonts (.tt)
%%
%%  Copyright (c) 2001 Howard Trickey
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(wpc_tt).
-export([init/0,menu/2,command/2,trygen/3,	% trygen is for debugging
	 findpolyareas/1,polyareas_to_faces/1,subdivide_pas/2]). % for ai

-import(lists, [reverse/1,sort/2,keysearch/3,duplicate/2,nthtail/2,
		mapfoldl/3,foldl/3,sublist/3,map/2,last/1,seq/2,seq/3,
		flatten/1,sum/1,append/1]).

-include("e3d.hrl").

-record(ttfont,
	{nglyph,			% number of glyphs
	 uperem,			% units per em
	 cmap,				% 256-element tuple, maps char -> glyph
	 loca,				% (nglyph+1)-element tuple, maps glyph -> offset in glyf
	 adv,				% nglyph-element tuple: maps glyph -> amount to advance x
	 glyf}).			% glyf table (binary)

-record(polyarea,
	{boundary,			%list of cedges (CCW oriented, closed)
	 islands=[]}).			%list of lists of cedges (CW, closed)

% a "possibly curved" edge, with explicit coords
% and optional cubic bezier control points
-record(cedge,
	{vs,cp1=nil,cp2=nil,ve}).	%all are {x,y} pairs

init() -> true.

menu({shape}, Menu) ->
    insert_before_more(Menu);
menu(_, Menu) -> Menu.

insert_before_more([H|_]=More) when element(1, element(2, H)) == more ->
    [{"Text",text,[option]},separator|More];
insert_before_more([H|T]) ->
    [H|insert_before_more(T)];
insert_before_more([]) ->
    [{"Text",text,[option]}].

command({shape,{text,Ask}}, St) -> make_text(Ask, St);
command(_, _) -> next.

make_text(Ask, St) when is_atom(Ask) ->
	DefFont = wpa:pref_get(wpc_tt, font, "Arial"),
	DefText = wpa:pref_get(wpc_tt, text, "A"),
	DefBisect = wpa:pref_get(wpc_tt, bisections, 0),
	wpa:ask(Ask, [ {"TrueType font",DefFont},
			{"Text", DefText},
			{"Number of edge bisections", DefBisect}],
		St, fun(Res) -> {shape,{text,Res}} end);
make_text([F,T,N], St) ->
	gen(F, T, N).

% Try to map a Name to a font file using registry
% (only works with Windows).
font_file(Name) ->
	case os:type() of
	{win32,nt} ->
		regtrans(Name, "Windows NT", "C:\\WINNT");
	{win32,windows} ->
		regtrans(Name, "Windows", "C:\\WINDOWS");
	_ -> Name
	end.

regtrans(Name, W, Sysroot) ->
	Key = "\\hklm\\SOFTWARE\\Microsoft\\"
		++ W ++ "\\CurrentVersion\\Fonts",
	Dir = Sysroot ++ "\\Fonts\\",
	case win32reg:open([read]) of
	{ok, RH} ->
		case win32reg:change_key(RH, Key) of
		ok ->
			Kname = Name ++ " (TrueType)",
			case win32reg:value(RH, Kname) of
			{ok, Fname} ->
				Ans = Dir ++ Fname;
			_ ->
				Ans = Name
			end;
		_ ->
			Ans = Name
		end,
		win32reg:close(RH);
	_ ->
		Ans = Name
	end,
	filename:absname(Ans).

gen(Font, Text, Nsubsteps) ->
	File = font_file(Font),
	case catch trygen(File, Text, Nsubsteps) of
	S = {new_shape,_,_,_} ->
		wpa:pref_set(wpc_tt, font, Font),
		wpa:pref_set(wpc_tt, text, Text),
		wpa:pref_set(wpc_tt, bisections, Nsubsteps),
		S;
	{error,Reason} ->
		wpa:error("Text failed: " ++ Reason);
	_ ->
		wpa:error("Text failed: internal error")
	end.

trygen(File, Text, Nsubsteps) ->
	case file:read_file(File) of
	    {ok,<<16#00010000:32,Rest/binary>>} ->
		Ttf = parsett(Rest),
		Pa = getpolyareas(Text, Ttf, Nsubsteps),
		{Vs,Fs} = polyareas_to_faces(Pa),
		{new_shape,"text",Fs,Vs};
	    {ok,_} ->
		{error,"Not a TrueType file version 1.0"};
	    {error,Reason} ->
		{error,file:format_error(Reason)}
	end.

% Return {Vs,Fs} corresponding to list of polyareas,
% where Vs is list of coords and Fs is list of list of
% coord indices, describing faces.
polyareas_to_faces(Pas) ->
	VFpairs = map(fun pa2object/1, Pas),
	concatvfs(VFpairs).
	

concatvfs(Vfp) -> concatvfs(Vfp, 0, [], []).

concatvfs([], Offset, Vsacc, Fsacc) ->
	{flatten(reverse(Vsacc)),Fsacc};
concatvfs([{Vs,Fs}|Rest], Offset, Vsacc, Fsacc) ->
	Fs1 = offsetfaces(Fs, Offset),
	Off1 = Offset + length(Vs),
	concatvfs(Rest, Off1,
		[Vs|Vsacc], Fsacc ++ Fs1).


% Parse binary arg, which should be a TrueType file after the version number,
% and return a ttfont.
% Throws {error,reason} or a badmatch if the file format is wrong.
parsett(<<Ntabs:16/unsigned,_Srchrng:16/unsigned,
	  _Esel:16/unsigned,_Rngshift:16/unsigned,B/binary>>) ->
	{Dirs,B1} = getdirs(Ntabs,B),
	Dirs1 = sort(fun({X,_,_},{Y,_,_}) -> X < Y end, Dirs),
	Offset = 12 + (Ntabs*16),
	Tabs = gettabs(Dirs1, B1, Offset),
	Nglyph = parsemaxptab(Tabs),
	Cmap = parsecmaptab(Tabs),
	{Uperem, ShortLoca} = parseheadtab(Tabs),
	Loca = parselocatab(Tabs, Nglyph, ShortLoca),
	Nhmetrics = parsehheatab(Tabs),
	Adv = parsehmtxtab(Tabs, Nglyph, Nhmetrics),
	Glyf = findtab("glyf", Tabs),
	#ttfont{nglyph=Nglyph, uperem=Uperem, cmap=Cmap, loca=Loca, adv=Adv, glyf=Glyf};
parsett(_) ->
	throw({error,"Bad offset table"}).

% returns list of table directory entries: {offset,length,name} tuples
getdirs(Ntabs,B) -> getdirs(Ntabs,B,[]).

getdirs(0, B, Acc) ->
	{reverse(Acc),B};
getdirs(Nleft,<<W,X,Y,Z,Csum:32,Off:32,Len:32,B/binary>>,Acc) ->
	getdirs(Nleft-1, B, [{Off,Len,[W,X,Y,Z]} | Acc]);
getdirs(_,_,_) ->
	throw({error,"Bad dir format"}).

% returns list of {tablename,table/binary} tuples
gettabs(Dirs,B,Offset) -> gettabs(Dirs,B,Offset,[]).

gettabs([],_,_,Acc) ->
	reverse(Acc);
gettabs([{Offnext,Len,Nam}|T]=Dirs,B,Off,Acc) ->
	if
	    Off == Offnext ->
		<<Tab:Len/binary,B1/binary>> = B,
		gettabs(T, B1, Off+Len, [{Nam,Tab} | Acc]);
	    Off < Offnext ->
		Padlen = Offnext - Off,
		<<C:Padlen/binary,B1/binary>> = B,
		gettabs(Dirs,B1,Offnext,Acc);
	    true ->
		throw({error,"Bad table offsets/sizes"})
	end.

% Find the table with the given name in Tabs and return it.
% Throw error if not found.
findtab(Name, Tabs) ->
	case keysearch(Name, 1, Tabs) of
	    {value, {_, Tab}} ->
		Tab;
	    false ->
		throw({error,"No " ++ Name ++ " table"})
	end.
	

% Parse the "maxp" (Maximum Profile) tab of Tabs and return numGlyphs
parsemaxptab(Tabs) ->
	Tab = findtab("maxp", Tabs),
	<<16#00010000:32,NumGlyphs:16/unsigned,_/binary>> = Tab,
	NumGlyphs.

% Parse the "cmap" (Character to Glyph Index) tab of Tabs.
% Return 256-long tuple where element (c+1) is glyph number for character c.
parsecmaptab(Tabs) ->
	Tab = findtab("cmap", Tabs),
	<<0:16,Nsubtabs:16,T1/binary>> = Tab,
	getcmap10(Nsubtabs, T1, Tab).

% Look for a subtable for Platform 1, Encoding 0, Format 0 (Apple, Roman)
% because it is the easiest one to deal with.
% If not found, throw an error.
getcmap10(0, _, _) ->
	throw({error,"No suitable character map"});
getcmap10(N, <<1:16,0:16,Off:32,_/binary>>, Tab) ->
	case list_to_tuple(binary_to_list(Tab,Off+1,Off+2)) of
	    {0,0} ->	% format 0 is easy: byte encoding table
		list_to_tuple(binary_to_list(Tab,Off+1+6,Off+255+6));
	    _ ->
		throw({error,"No suitable character map"})
	end;
getcmap10(N, <<_Pid:16,_Eid:16,_Off:32,T/binary>>, Tab) ->
	getcmap10(N-1, T, Tab).

% Parse the "head" (Font Header) tab of Tabs and return {units-per-em, shortloca}
% where shortloca is true if loca table uses "short" format
parseheadtab(Tabs) ->
	Tab = findtab("head", Tabs),
	<<16#00010000:32,_Frev:32,_Csuma:32,16#5F0F3CF5:32,
	  _Flags:16,Uperem:16,_Dcreat:64,_Dmod:64,
	  _Xmin:16,_Ymin:16,_Xmax:16,_Ymax:16,
	  _Macsty:16,_LRP:16,_Fdir:16,IndToLocFmt:16,0:16>> = Tab,
	{Uperem, case IndToLocFmt of 0 -> true; _ -> false end}.

% Parse the "loca" tab of Tabs and return an (Nglyph+1)-element tuple
% mapping a glyph index into an offset in the glyf table.
% ShortLoca is true for the "short" format, false otherwise.
parselocatab(Tabs, Nglyph, ShortLoca) ->
	Tab = findtab("loca", Tabs),
	case ShortLoca of
	    true ->
		locashort(Nglyph+1,Tab,[]);
	    false ->
		localong(Nglyph+1,Tab,[])
	end.

% short format: unsigned shorts divided by two are in table
locashort(0,_,Acc) ->
	list_to_tuple(reverse(Acc));
locashort(N,<<X:16/unsigned,T/binary>>,Acc) ->
	locashort(N-1, T, [2*X|Acc]).

localong(0,_,Acc) ->
	list_to_tuple(reverse(Acc));
localong(N,<<X:32,T/binary>>,Acc) ->
	localong(N-1, T, [X|Acc]).

% Parse the "hhea" (Horizontal Header) tab of Tabs and return numberOfHMetrics
parsehheatab(Tabs) ->
	Tab = findtab("hhea", Tabs),
	<<16#00010000:32,_Asc:16,_Desc:16,_Lgap:16,_Awmax:16,
	  _Minlsb:16,_Minrsb:16,_Xmaxext:16,_Cslrise:16,_Cslrun:16,
	  _Res:10/binary, 0:16, NumberOfHMetrics:16/unsigned>> = Tab,
	NumberOfHMetrics.

% Parse the "hmtx" (Horizontal Metrics) tab of Tabs and return an Nglyph-element tuple
% mapping a glyph index into the amound (in FUnits) to advance in the x-direction
% after "printing" the glyph.
parsehmtxtab(Tabs, Nglyph, Nhmetrics) ->
	Tab = findtab("hmtx", Tabs),
	hmtx(Nglyph, Nhmetrics, Tab, []).

% need to repeat last element if Nhmetrics goes to zero before Nglyph
hmtx(0, _, _, Acc) ->
	list_to_tuple(reverse(Acc));
hmtx(Nglyph, Nhmetrics, <<Aw:16/unsigned,_Lsb:16,T/binary>>, Acc) ->
	Acc1 = [Aw | Acc],
	Ng1 = Nglyph-1,
	Nh1 = Nhmetrics-1,
	if
	    Nh1 == 0, Ng1 > 0 ->
		list_to_tuple(reverse(Acc) ++ duplicate(Ng1, Aw));
	    true ->
		hmtx(Ng1, Nh1, T, Acc1)
	end.

getpolyareas(Text, Ttf, Nsubsteps) ->
	Pas = getpolyareas(Text, Ttf, 0, []),
	Pas1 = clean_pas(Pas),
	subdivide_pas(Pas1, Nsubsteps).

getpolyareas([], _, _, Acc) ->
	flatten(reverse(Acc));
getpolyareas([C|Rest], #ttfont{nglyph=Ng,adv=Adv,cmap=Cmap}=Ttf, X, Acc) ->
	{X1,Acc1} =
		if
		    C >= 0, C < 256 ->
			G = element(C+1, Cmap),
			if
			    G < Ng ->
				Xnew = X + element(G+1, Adv),
				case glyphpolyareas(G, Ttf, X) of
				    nil ->
					{Xnew, Acc};
				    Pa ->
					{Xnew, [Pa|Acc]}
				end;
			    true ->
				{X, Acc}
			end;
	   	 true ->
			{X, Acc}
		end,
	getpolyareas(Rest, Ttf, X1, Acc1).

% Get contours for glyph G (known to be in range 0..nglyph-1).
% Return nil if no data or no contours for glyph G.
glyphpolyareas(G, #ttfont{loca=Loca,glyf=Glyf,uperem=Uperem}, X) ->
	Off = element(G+1, Loca),
	Len = element(G+2, Loca) - Off,
	if
	    Len < 9 ->
		nil;
	    true ->
		Gdat = binary_to_list(Glyf, Off+1, Off+Len),
		[Nch,Ncl|T1] = Gdat,
		Ncont = toushort(Nch, Ncl),
		if
		    Ncont == 0 ->
			nil;
		    true ->
			% Calculate scale so Em box measures 2 by 2
			% (about the scale of wings primatives)
			Scale = 2.0/float(Uperem),
		    	gpa(nthtail(4*2, T1), Ncont, X, Scale)
		end
	end.

% continue glyphpolyareas, when there are > 0 contours
% (Gdat is now at start of endPtsOfContours array)
gpa(Gdat, Ncont, Xorg, Scale) ->
	{Eoc,T1} = takeushorts(Ncont,Gdat),
	Npt = element(Ncont, Eoc)+1,
	[Ninstrh,Ninstrl | T2] = T1,
	Ninstr = toushort(Ninstrh,Ninstrl),
	T3 = nthtail(Ninstr,T2),
	{Flags,T4} = gflags(Npt, T3),
	{X0,T5} = gcoords(Npt, T4, Flags, 2, 16),
	{Y0,T6} = gcoords(Npt, T5, Flags, 4, 32),
	X = makeabs(X0, Xorg, Scale),
	Y = makeabs(Y0, 0, Scale),
	Cntrs = contours(Ncont, Eoc, X, Y, Flags),
	Ccntrs = map(fun getcedges/1, Cntrs),
	findpolyareas(Ccntrs).

% Take N pairs of bytes off of L, convert each pair to ushort,
% return {tuple of the ushorts, remainder of L}.
takeushorts(N,L) -> takeushorts(N,L,[]).

takeushorts(0, L, Acc) ->
	{list_to_tuple(reverse(Acc)), L};
takeushorts(N, [B1,B2 | Rest], Acc) ->
	takeushorts(N-1, Rest, [toushort(B1,B2) | Acc]).

% Get N glyph flags from L and return {list of flags, rest of L}.
% Less than N flags might come off of L because if a flag has the
% repeat bit (8) set, the next byte is used as a repeat count.
gflags(N,L) -> gflags(N,L,[]).

gflags(0, L, Acc) ->
	{reverse(Acc), L};
gflags(N, [F|Rest], Acc) ->
	Acc1 = [F | Acc],
	if
	    (F band 8) == 8 ->	% repeat F next-byte more times
		[Rep|Rest2] = Rest,
		Acc2 = duplicate(Rep,F) ++ Acc1,
		gflags(N-1-Rep, Rest2, Acc2);
	    true ->
		gflags(N-1, Rest, Acc1)
	end.

% Get N glyph coords from L and return {list of coords, rest of L}.
% The coords are relative-to-previous at this point.
% The Flags list controls how next coord comes off of L:
% if Sbit is set, it's one byte (and Rbit is set if positive), else 2 bytes.
% if Sbit isn't set, Rbit set means value is same as previous (relative offset = 0)
gcoords(N,L,Flags,Sbit,Rbit) -> gcoords(N,L,Flags,Sbit,Rbit,[]).

gcoords(0,L,_,_,_,Acc) ->
	{reverse(Acc), L};
gcoords(N,L,[F|Tf]=Flags,Sbit,Rbit,Acc) ->
	SRbits = Sbit bor Rbit,
	case F band SRbits of
	    0 ->
		[B1,B2|Tl] = L,
		gcoords(N-1, Tl, Tf, Sbit, Rbit, [tosshort(B1,B2)|Acc]);
	    SRbits ->
		[B|Tl] = L,
		gcoords(N-1, Tl, Tf, Sbit, Rbit, [B|Acc]);
	    Sbit ->
		[B|Tl] = L,
		gcoords(N-1, Tl, Tf, Sbit, Rbit, [-B|Acc]);
	    Rbit ->
		gcoords(N-1, L, Tf, Sbit, Rbit, [0|Acc])
	end.

toushort(B1,B2) -> B1*256 + B2.

tosshort(B1,B2) ->
	<<A:16/signed>> = list_to_binary([B1,B2]),
	A.

% Change coords in L to be absolute (starting at V) rather than relative.
% Also, after translation, make into a float and scale by Scale
makeabs(L, V, Scale) ->
	{Labs, _} = mapfoldl(fun (Z,Pos) ->
				Znew = Z+Pos,
				{Scale*float(Znew),Znew}
			     end, V, L),
	Labs.

% Return list of Ncont {list of x-coords, list of y-coords, flags} tuples,
% where each is a sublist of X, Y, Flags, as directed by Eoc tuple.
contours(Ncont, Eoc, X, Y, Flags) -> contours(Ncont, 1, 1, Eoc, X, Y, Flags, []).

contours(0, _, _, _, _, _, _, Acc) ->
	reverse(Acc);
contours(Ncont, I, Start, Eoc, X, Y, Flags, Acc) ->
	End = element(I, Eoc) + 1,
	Len = End - Start + 1,
	X1 = sublist(X, Start, Len),
	Y1 = sublist(Y, Start, Len),
	F1 = sublist(Flags, Start, Len),
	contours(Ncont-1, I+1, End+1, Eoc, X, Y, Flags, [{X1,Y1,F1}|Acc]).

% Turn the parallel lists (X,Y,Flags), representing a TrueType glyph,
% into a list of cedges.
% We have to turn a quadratic B-spline into a list of cubic bezier curves.
getcedges({X,Y,Flags}) ->
	N = length(X),
	if
	    N >= 3 ->
		getcedges(X, Y, Flags, hd(X), hd(Y), []);
	    true ->
		[]
	end.

getcedges([], [], [], X0, Y0, [#cedge{ve={XL,YL}=VL}|_]=Acc) ->
	case (X0 == XL) and (Y0 == YL) of
	    true ->
		reverse(Acc);
	    _ ->
		% need straight line to close
		LastE = #cedge{vs=VL, ve={X0,Y0}},
		reverse([LastE | Acc])
	end;
getcedges([_|Xt]=X, [_|Yt]=Y, [_|Ft]=Flags, X0, Y0, Acc) ->
	{Cur,Ison} = nthptandison(1, X, Y, Flags, X0, Y0),
	{Next,Isnexton} = nthptandison(2, X, Y, Flags, X0, Y0),
	{Anext,Isanexton} = nthptandison(3, X, Y, Flags, X0, Y0),
	case (not(Ison) and Isnexton) of
	    true ->
		% this case generates no segment
		getcedges(Xt, Yt, Ft, X0, Y0, Acc);
	    _ ->
		Curon = case Ison of true -> Cur; _ -> avg(Cur,Next) end,
		Nexton = if
			    Isnexton -> Next;
			    Isanexton -> Anext;
			    true -> avg(Next,Anext)
			end,
		Ctl = if
			    Ison and Isnexton -> nil;
			    Isnexton -> avg(Curon, Next);
			    true -> Next
			end,
		% Ctl, if not nil, is quadratic Bezier control point.
		% Following uses degree-elevation theory to get cubic cps.
		{Cp1,Cp2} = case Ctl of
			    nil -> {nil, nil};
			    _ -> {lininterp(2.0/3.0, Curon, Ctl),
				  lininterp(2.0/3.0, Nexton, Ctl)}
			end,
		Edge = #cedge{vs=Curon, cp1=Cp1, cp2=Cp2, ve=Nexton},
		getcedges(Xt, Yt, Ft, X0, Y0, [Edge|Acc])
	end.

avg({X1,Y1},{X2,Y2}) -> {0.5*(X1+X2), 0.5*(Y1+Y2)}.

lininterp(F,{X1,Y1},{X2,Y2}) -> {(1.0-F)*X1 + F*X2, (1.0-F)*Y1 + F*Y2}.

% Return {Nth point, is-on-curve flag} based on args
% (use beginning (X0,Y0) when wrap).
nthptandison(1, [X|_], [Y|_], [F|_], X0, Y0) ->
	{{X,Y}, if (F band 1) == 1 -> true; true -> false end};
nthptandison(N, [], _, _, X0, Y0) ->
	{{X0,Y0}, true};
nthptandison(N, [_|Xt], [_|Yt], [_|Ft], X0, Y0) ->
	nthptandison(N-1, Xt, Yt, Ft, X0, Y0).

% Cconts is list of "curved contours".
% Each curved contour is a list of cedges, representing a closed contour.
% This routine analyzes the contours and partitions them into polyareas,
% where each polyarea has a boundary (CCW oriented) and an optional list
% of contained islands (each CW oriented).
findpolyareas(Cconts) ->
	Areas = map(fun ccarea/1, Cconts),
	{Cc,Ar} = orientccw(Cconts, Areas),
	Cct = list_to_tuple(Cc),
	N = size(Cct),
	Art = list_to_tuple(Areas),
	Lent = list_to_tuple(map(fun length/1,Cc)),
	Seqn = seq(1,N),
	Cls = [ {{I,J},classifyverts(element(I,Cct),element(J,Cct))}
		|| I <- Seqn, J <- Seqn],
	Clsd = gb_trees:from_orddict(Cls),
	Cont = [ {{I,J},contains(I,J,Cct,Art,Lent,Clsd)}
		|| I <- Seqn, J <- Seqn],
	Contd = gb_trees:from_orddict(Cont),
	Assigned = gb_sets:empty(),
	getpas(1,N,Contd,Cct,{[],Assigned}).

getpas(I,N,Contd,Cct,{Pas,Ass}) when I > N ->
	case length(gb_sets:to_list(Ass)) of
	N ->
		reverse(Pas);
	_ ->
		% not all assigned: loop again
		getpas(1,N,Contd,Cct,{Pas,Ass})
	end;
getpas(I,N,Contd,Cct,{Pas,Ass}=Acc) ->
	case gb_sets:is_member(I,Ass) of
	    true -> getpas(I+1,N,Contd,Cct,Acc);
	    _ ->
		case isboundary(I,N,Contd,Ass) of
		    true ->
			% have a new polyarea with boundary = contour I
			Ass1 = gb_sets:add(I,Ass),
			{Isls,Ass2} = getisls(I,N,N,Contd,Ass1,Ass1,[]),
			Cisls = map(fun (K) -> revccont(element(K,Cct)) end, Isls),
			Pa = #polyarea{boundary=element(I,Cct), islands=Cisls},
			getpas(I+1,N,Contd,Cct,{[Pa|Pas],Ass2});
		    _ -> getpas(I+1,N,Contd,Cct,Acc)
		end
	end.

% Return true if thre is no unassigned J <= second arg, J /= I,
% such that contour J contains contour I.
isboundary(I,0,Contd,Ass) -> true;
isboundary(I,I,Contd,Ass) -> isboundary(I,I-1,Contd,Ass);
isboundary(I,J,Contd,Ass) ->
	case gb_sets:is_member(J,Ass) of
	    true ->
		isboundary(I,J-1,Contd,Ass);
	    _ ->
		case gb_trees:get({J,I},Contd) of
		    true -> false;
		    _ -> isboundary(I,J-1,Contd,Ass)
		end
	end.

% Find islands for contour I : i.e., unassigned contours directly inside it.
% Only have to check J and less.
% Ass, Isls are (assigned-so-far, islands-so-far).
% Ass0 is assigned before we started adding islands.
% Return {list of island indices, Assigned array with those indices added}
getisls(_I,0,_N,_Contd,Ass0,Ass,Isls) -> {reverse(Isls),Ass};
getisls(I,J,N,Contd,Ass0,Ass,Isls) ->
	case gb_sets:is_member(J,Ass) of
	    true ->
		getisls(I,J-1,N,Contd,Ass0,Ass,Isls);
	    _ ->
		case directlycont(I,J,N,Contd,Ass0) of
		    true ->
			getisls(I,J-1,N,Contd,Ass0,gb_sets:add(J,Ass),[J|Isls]);
		    _ ->
			getisls(I,J-1,N,Contd,Ass0,Ass,Isls)
		end
	end.

directlycont(I,J,N,Contd,Ass) ->
	gb_trees:get({I,J},Contd) andalso
	foldl(fun (K,DC) ->
		DC andalso
		(K == J orelse gb_sets:is_member(K,Ass) orelse
		 not(gb_trees:get({K,J},Contd))) end,
	      true, seq(1,N)).

ccarea(Ccont) ->
	0.5 * foldl(fun (#cedge{vs={X1,Y1},ve={X2,Y2}},A) ->
			A + X1*Y2 - X2*Y1 end,
		    0.0, Ccont).

% Reverse contours if area is negative (meaning they were Clockwise),
% and return revised Cconts and Areas.
orientccw(Cconts, Areas) -> orientccw(Cconts, Areas, [], []).

orientccw([], [], Cacc, Aacc) ->
	{ reverse(Cacc), reverse(Aacc) };
orientccw([C|Ct], [A|At], Cacc, Aacc) ->
	if
	    A >= 0.0 ->
		orientccw(Ct, At, [C|Cacc], [A|Aacc]);
	    true ->
		orientccw(Ct, At, [revccont(C)|Cacc], [-A|Aacc])
	end.

revccont(C) -> reverse(map(fun revcedge/1, C)).

% reverse a cedge
revcedge(#cedge{vs=Vs,cp1=Cp1,cp2=Cp2,ve=Ve}) ->
	#cedge{vs=Ve,cp1=Cp2,cp2=Cp1,ve=Vs}.

% classify vertices of contour B with respect to contour A.
% return {# inside A, # on A}.
classifyverts(A,B) -> foldl(fun (#cedge{vs=Vb},Acc) -> cfv(A,Vb,Acc) end,
			    {0,0}, B).

% Subdivide (bisect each each) Nsubsteps times.
% When bezier edges are subdivided, the inserted point goes
% at the proper place on the curve.
subdivide_pas(Pas,0) -> Pas;
subdivide_pas(Pas,Nsubsteps) ->
	map(fun (Pa) -> subdivide_pa(Pa,Nsubsteps) end, Pas).

subdivide_pa(Pa=#polyarea{boundary=B,islands=Isls}, 0) ->
	Pa;
subdivide_pa(Pa=#polyarea{boundary=B,islands=Isls}, N) ->
	subdivide_pa(#polyarea{boundary=subdivide_contour(B),
				islands=map(fun subdivide_contour/1, Isls)}, N-1).

subdivide_contour(Cntr) ->
	flatten(map(fun (CE) -> subdivide_cedge(CE,0.5) end, Cntr)).

% subdivide CE at parameter Alpha, returning two new CE's in list.
subdivide_cedge(CE=#cedge{vs=Vs,cp1=nil,cp2=nil,ve=Ve},Alpha) ->
	Vm = lininterp(Alpha, Vs, Ve),
	[#cedge{vs=Vs,ve=Vm}, #cedge{vs=Vm,ve=Ve}];
subdivide_cedge(CE=#cedge{vs=Vs,cp1=C1,cp2=C2,ve=Ve},Alpha) ->
	B0 = {Vs,C1,C2,Ve},
	B1 = bezstep(B0,1,Alpha),
	B2 = bezstep(B1,2,Alpha),
	B3 = bezstep(B2,3,Alpha),
	[#cedge{vs=element(1,B0),cp1=element(1,B1),cp2=element(1,B2),ve=element(1,B3)},
	 #cedge{vs=element(1,B3),cp1=element(2,B2),cp2=element(3,B1),ve=element(4,B0)}].

bezstep(B,R,Alpha) ->
	list_to_tuple(bzss(B,0,3-R,Alpha)).

bzss(B,I,Ilim,Alpha) when I > Ilim -> [];
bzss(B,I,Ilim,Alpha) ->
	[lininterp(Alpha,element(I+1,B),element(I+2,B)) | bzss(B,I+1,Ilim,Alpha)].

% Clean up all the polygons in the polyarea list Pas.
% "Clean" means remove zero-length edges.
clean_pas(Pas) -> map(fun clean_pa/1, Pas).

clean_pa(Pa=#polyarea{boundary=B,islands=Isls}) ->
	#polyarea{boundary=clean_contour(B),
				islands=map(fun clean_contour/1, Isls)}.

clean_contour([]) -> [];
clean_contour([CE=#cedge{vs=Vs,ve=Ve} | T]) ->
	case Vs==Ve of
	true -> clean_contour(T);
	_ -> [CE | clean_contour(T)]
	end.

% Decide whether vertex P is inside or on (as a vertex) contour A,
% and return modified pair.  Assumes A is CCW oriented.
% CF Eric Haines ptinpoly.c in Graphics Gems IV
cfv(A,P,{Inside,On}=Acc) ->
	#cedge{vs=Va0} = last(A),
	if
	    Va0 == P ->
		{Inside, On+1};
	    true ->
		Yflag0 = (element(2,Va0) > element(2,P)),
		case vinside(A, Va0, P, false, Yflag0) of
		    true -> {Inside+1, On};
		    false -> {Inside, On};
		    on -> {Inside, On+1}
		end
	end.

vinside([], _V0, _P, Inside, _Yflag0) ->
	Inside;
vinside([#cedge{vs={X1,Y1}=V1}|Arest], V0={X0,Y0}, P={Xp,Yp}, Inside, Yflag0) ->
	if
	    V1 == P ->
		on;
	    true ->
		Yflag1 = (Y1 > Yp),
		Inside1 =
			if
			    Yflag0 == Yflag1 -> Inside;
			    true ->
				Xflag0 = (X0 >= Xp),
				Xflag1 = (X1 >= Xp),
				if
				    Xflag0 == Xflag1 ->
					case Xflag0 of
					    true -> not(Inside);
					    _ -> Inside
					end;
				    true ->
					Z = X1 - (Y1-Yp)*(X0-X1)/(Y0-Y1),
					if
					    Z >= Xp -> not(Inside);
					    true -> Inside
					end
				end
			end,
		vinside(Arest, V1, P, Inside1, Yflag1)
	end.

% I, J are indices into tuple Cct of curved contours.
% Clsd is gb_tree mapping {I,J} to [Inside,On,Outside].
% Return true if contour I contains at least 55% of contour J's vertices.
% (This low percentage is partly because we are dealing with polygonal approximations
% to curves, sometimes, and the containment relation may seem worse than it actually is.)
% Lengths (in Lent tuple) are used for calculating percentages.
% Areas (in Art tuple) are used for tie-breaking.
% Return false if contour I is different from contour J, and not contained in it.
% Return same if I == J or all vertices on I are on J (duplicate contour).
contains(I,I,_,_,_,_) ->
	same;
contains(I,J,Cct,Art,Lent,Clsd) ->
	LenI = element(I,Lent),
	LenJ = element(J,Lent),
	{JinsideI,On} = gb_trees:get({I,J},Clsd),
	if
	    JinsideI == 0 ->
		false;
	    On == LenJ, LenI == LenJ ->
		same;
	    true ->
		if
		    float(JinsideI) / float(LenJ) > 0.55 ->
			{IinsideJ,_} = gb_trees:get({J,I},Clsd),
			FIinJ = float(IinsideJ) / float(LenI),
			if
			    FIinJ > 0.55 ->
				element(I,Art) >= element(J,Art);
			    true ->
				true
			end;
		    true ->
			false
		end
	end.

% Return {Vs,Fs} where Vs is list of {X,Y,Z} for vertices 0, 1, ...
% and Fs is list of lists, each sublist is a face (CCW ordering of
% (zero-based) indices into Vs).
pa2object(#polyarea{boundary=B,islands=Isls}) ->
	Vslist = [cel2vec(B, 0.0) | map(fun (L) -> cel2vec(L, 0.0) end, Isls)],
	Vtop = flatten(Vslist),
	Vbot = map(fun ({X,Y,Z}) -> {X,Y,Z-0.2} end, Vtop),
	Vs = Vtop ++ Vbot,
	Nlist = [length(B) | map(fun (L) -> length(L) end, Isls)],
	Ntot = sum(Nlist),
	Ftop = [FBtop | Holestop] = faces(Nlist,0,top),
	Fbot = [FBbot | Holesbot] = faces(Nlist,Ntot,bot),
	Fsides = sidefaces(Nlist, Ntot),
	FtopQ = e3d__tri_quad:quadrangulate_face_with_holes(FBtop, Holestop, Vs),
	FbotQ = e3d__tri_quad:quadrangulate_face_with_holes(FBbot, Holesbot, Vs),
	Ft = [ F#e3d_face.vs || F <- FtopQ ],
	Fb = [ F#e3d_face.vs || F <- FbotQ ],
	Fs = Ft ++ Fb ++ Fsides,
	{Vs,Fs}.

cel2vec(Cel, Z) -> map(fun (#cedge{vs={X,Y}}) -> {X,Y,Z} end, Cel).

faces(Nlist,Org,Kind) -> faces(Nlist,Org,Kind,[]).

faces([],Org,Kind,Acc) -> reverse(Acc);
faces([N|T],Org,Kind,Acc) ->
	FI = case Kind of
		top -> #e3d_face{vs=seq(Org, Org+N-1)};
		bot -> #e3d_face{vs=seq(Org+N-1, Org, -1)}
		end,
	faces(T,Org+N,Kind,[FI|Acc]).

sidefaces(Nlist,Ntot) -> sidefaces(Nlist,0,Ntot,[]).

sidefaces([],Org,Ntot,Acc) -> append(reverse(Acc));
sidefaces([N|T],Org,Ntot,Acc) ->
	End = Org+N-1,
	Fs = [ [I, Ntot+I, wrap(Ntot+I+1,Ntot+Org,Ntot+End), wrap(I+1,Org,End)]
		|| I <- seq(Org, End) ],
	sidefaces(T,Org+N,Ntot,[Fs|Acc]).

% I should be in range (Start, Start+1, ..., End).  Make it so.
wrap(I,Start,End) -> Start + ((I-Start) rem (End+1-Start)).

offsetfaces(Fl, Offset) ->
	map(fun (F) -> offsetface(F,Offset) end, Fl).

offsetface(F, Offset) ->
	map(fun (V) -> V+Offset end, F).

