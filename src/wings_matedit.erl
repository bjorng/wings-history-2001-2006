%%
%%  wings_matedit.erl --
%%
%%     This module edits materials.
%%
%%  Copyright (c) 2001-2002 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_matedit.erl,v 1.15 2002/01/28 08:42:17 bjorng Exp $
%%

-module(wings_matedit).
-export([edit/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(E, 2.71828).
-define(LOG_BASE, 10000.0).

%% Material record.
-record(mat,
	{ambient={0.0,0.0,0.0}, %Ambient color
	 diffuse={0.0,0.0,0.0}, %Diffuse color
	 specular={0.0,0.0,0.0}, %Specular color
	 shininess=0.0, %Sinininess (0..1)
	 opacity=1.0, %Opacity (0..1)
	 twosided=false, %Twosided material.
	 diffuse_map=none, %Diffuse map.
	 diffuse_map_dl=none, %Diffuse map.
	 attr=[], %Uinterpreted attributes
	 setup %Fun for OpenGL drawing
	}).

-record(s, {ambient, diffuse, specular, shininess = 0.0, transp = 1.0,
	    prev, %% Prev color used in cancel
	    x,y,w,h, bgc,
	    orig_w, orig_h,
	    mouse, key
	   }). %% Windows stuff
-record(c, {name, x, y, rgb = {1,1,1}, cx = 0, cy = 0, lscale = 1.0}).

-define(COLORCIRCLE_RADIUS, 50).	 
-define(COLORCIRCLE_STEP, 5).
-define(COLORSELECT_HALFSZ, 2).
-define(LSCALEX, 10).
-define(LSCALEHALFW, ?LSCALEX div 2).

-define(BORDER_W, 10).
-define(BORDER_H, 20).

-define(ButtonSzX, 50).
-define(ButtonSzY, 25).

-define(Checker, <<16#F0,16#F0,16#F0,16#F0,16#F0,16#F0,16#F0,16#F0,
		  16#0F,16#0F,16#0F,16#0F,16#0F,16#0F,16#0F,16#0F>>).

edit(PC) when record(PC, mat) ->
    Amb  = convert_material(PC#mat.ambient),
    Diff = convert_material(PC#mat.diffuse),
    Spec = convert_material(PC#mat.specular),

    [X0,Y0,W,H] = gl:getIntegerv(?GL_VIEWPORT),
    WinW = 550,
    WinH = 200,
    X = X0 + W div 2 - WinW div 2,
    Y = Y0 + H div 2 - WinH div 2,

    S = #s{x = X, y = Y, w = WinW, h = WinH, orig_w=W, orig_h=H,
	   bgc = ?MENU_COLOR},
    gl:loadIdentity(),

    NS = S#s{ambient = Amb#c{name = "Ambient", 
			     x = ?BORDER_W+?COLORCIRCLE_RADIUS, 
			     y = S#s.h-?BORDER_H-?COLORCIRCLE_RADIUS},
	     diffuse = Diff#c{name = "Diffuse",
			      x = 4 * ?BORDER_W + 3 * ?COLORCIRCLE_RADIUS, 
			      y = S#s.h-?BORDER_H-?COLORCIRCLE_RADIUS},
	     specular = Spec#c{name = "Specular",
			       x = 7 * ?BORDER_W + 5 * ?COLORCIRCLE_RADIUS, 
			       y = S#s.h-?BORDER_H-?COLORCIRCLE_RADIUS},
	     transp = PC#mat.opacity,
	     shininess = PC#mat.shininess,
	     prev = PC
	    },	  
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    %% Standard blend function
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),

    gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_BLEND),
    gl:shadeModel(?GL_SMOOTH),

    gl:disable (?GL_LIGHTING),
    gl:disable(?GL_COLOR_MATERIAL),    
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0.5, 0.5, -2, 1}),

    gl:shadeModel (?GL_SMOOTH),
    Res = (catch color_picker_loop(NS)),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    case Res of
	{'EXIT',normal} ->
	    restore_state(NS),
	    NS#s.prev;
	{'EXIT', {normal, Material}} ->
	    restore_state(NS),
	    Material;
	{'EXIT', Reason} ->
	    io:format("Error Material Selection failed with Reason ~p ~n", [Reason]),
	    restore_state(NS),
	    NS#s.prev
    end.

restore_state(#s{orig_w=W,orig_h=H}) ->
    gl:viewport(0, 0, W, H),
    gl:popAttrib(),
    ok.

color_picker_loop(S) ->
    gl:viewport(S#s.x,S#s.y,S#s.w, S#s.h),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, S#s.w, 0, S#s.h),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),	  

    %%% Draw Window..
    gl:color3fv(S#s.bgc),  %% Get clear color from preferences
    draw_filled_box(0,S#s.w,0,S#s.h), 
    gl:color3fv(?BEVEL_HIGHLIGHT),
    draw_box(0,S#s.w,0,S#s.h), 
    
    Amb = S#s.ambient,	  
    Diff = S#s.diffuse,	   
    Spec = S#s.specular,
    
    %% Color Circles with darkness bars
    draw_color(Amb),
    draw_color(Diff),
    draw_color(Spec),	 

    %% Test Result
    {AR,AG,AB} = Amb#c.rgb,
    {DR,DG,DB} = Diff#c.rgb,
    {SR,SG,SB} = Spec#c.rgb,
    CFXStop = ?COLORCIRCLE_RADIUS + ?BORDER_W + ?LSCALEHALFW,    
    CFXLen = ?COLORCIRCLE_RADIUS + CFXStop,
    CFY1 = Amb#c.y - ?COLORCIRCLE_RADIUS - ?BORDER_W,
    CFY2 = CFY1 - ?ButtonSzY,
    CFCX = (CFXStop - ?COLORCIRCLE_RADIUS) / 2,
    CFCY = CFY2 + ?ButtonSzY / 2,

    gl:color3f(AR*Amb#c.lscale, AG*Amb#c.lscale, AB*Amb#c.lscale),
    draw_filled_box(Amb#c.x - ?COLORCIRCLE_RADIUS, Amb#c.x + CFXStop, CFY1, CFY2),
    draw_centered_box(Amb#c.x + CFCX, CFCY, CFXLen, ?ButtonSzY, box),    
    gl:color3f(DR*Diff#c.lscale, DG*Diff#c.lscale, DB*Diff#c.lscale),
    draw_filled_box(Diff#c.x - ?COLORCIRCLE_RADIUS, Diff#c.x + CFXStop, CFY1, CFY2),
    draw_centered_box(Diff#c.x + CFCX, CFCY, CFXLen, ?ButtonSzY, box),
    gl:color3f(SR*Spec#c.lscale, SG*Spec#c.lscale, SB*Spec#c.lscale),
    draw_filled_box(Spec#c.x - ?COLORCIRCLE_RADIUS, Spec#c.x + CFXStop, CFY1, CFY2),
    draw_centered_box(Spec#c.x + CFCX, CFCY, CFXLen, ?ButtonSzY, box),

    %% Transparancy and Shininess
    TandSXLen = (Spec#c.x + ?COLORCIRCLE_RADIUS + ?LSCALEX - 2 * ?BORDER_W) div 2, 
    TandSY    = ?BORDER_H div 2 + ?LSCALEX,
    TandSTextY = TandSY + ?BORDER_H div 2 - 3,

    TTextX = ?BORDER_W,
    STextX = TandSXLen + ?BORDER_W * 3,
    MaxX = STextX + TandSXLen + ?BORDER_W,
    MaxY = CFY2,
    gl:color3f(0,0,0),
    wings_io:text_at(TTextX, TandSTextY, "Transparancy:"),
    wings_io:text_at(STextX, TandSTextY, "Shininess:"),

    gl:color3f(1,1,1),
    gl:rasterPos2i(?BORDER_W, TandSY- ?LSCALEX + 1),
    [gl:bitmap(16, 8, -1, -1, 16, 0, ?Checker) || 
	_ <- lists:seq(1,TandSXLen div 16)],
    gl:enable(?GL_BLEND),
    gl:color3f(0,0,0),    
    draw_box(?BORDER_W, ?BORDER_W + TandSXLen, TandSY, TandSY - ?LSCALEX),
    gl:glBegin(?GL_QUADS),	%% Transparency Bar 
    gl:color4f(0,0,0,0),    
    gl:vertex2f(?BORDER_W, TandSY),
    gl:vertex2f(?BORDER_W, TandSY- ?LSCALEX),
    gl:color4f(0.3,0.3,0.3,1),    
    gl:vertex2f(?BORDER_W + TandSXLen, TandSY - ?LSCALEX),
    gl:vertex2f(?BORDER_W + TandSXLen, TandSY ),
    gl:glEnd(),
    gl:disable(?GL_BLEND),

    gl:color3f(0,0,0),      %% Shininess Bar

    gl:glBegin(?GL_LINE_LOOP),	
    draw_box(STextX, STextX + TandSXLen,TandSY, TandSY - ?LSCALEX),
    gl:glBegin(?GL_QUADS),	%% Shininess Bar filler
    gl:color3f(0.3,0.3,0.3),
    gl:vertex2f(STextX, TandSY),
    gl:vertex2f(STextX, TandSY - ?LSCALEX),
    gl:color3f(1,1,1),
    gl:vertex2f(STextX + TandSXLen, TandSY - ?LSCALEX),
    gl:vertex2f(STextX + TandSXLen, TandSY),
    gl:glEnd(),
 
    %% Transparency Indicator
    TiPosX = ?BORDER_W + S#s.transp * TandSXLen,
    SiPosX = STextX + inv_scale_pos_log(S#s.shininess) * TandSXLen,
    draw_centered_box(TiPosX, TandSY - ?LSCALEX/2, 4, ?LSCALEX + 6, black),
    %% Shininess Indicator
    draw_centered_box(SiPosX, TandSY - ?LSCALEX/2, 4, ?LSCALEX + 6, black),

    OkX  = S#s.w - ?BORDER_W,
    OkXC = S#s.w - ?BORDER_W - ?ButtonSzX div 2,
    OkYC = ?ButtonSzY div 2 + ?BORDER_W,
    center_text(OkXC,OkYC, "Ok"), 
    draw_centered_box(OkXC,OkYC, ?ButtonSzX, ?ButtonSzY, button),
    
    CancelX0 = OkX - 1.5 * ?ButtonSzX - 1*?BORDER_W, 
    CancelY0 = OkYC,
    {CancelX1, CancelX2, CancelY1, CancelY2} = 
	draw_centered_box(CancelX0, CancelY0, ?ButtonSzX, ?ButtonSzY, button),
    gl:color3f(0,0,0),
    center_text(CancelX0, CancelY0, "Cancel"),
    gl:color3f(0,0,0),
    wings_io:text_at(MaxX + 5, Amb#c.y + ?COLORCIRCLE_RADIUS + 4, "Material"),
    MVX1 = MaxX + 1,
    MVY1 = S#s.h - ?BORDER_H - 1,
    MVX2 = S#s.w - ?BORDER_W - 1,
    MVY2 = MaxY - 1,
    MVXSz = (MVX2 - MVX1),
    MVYSz = (MVY1 - MVY2),
    MVRatio = MVXSz/MVYSz,

    draw_centered_box(MVX1 + MVXSz /2, MVY1 - MVYSz /2, MVXSz + 2, MVYSz +2, box),

    %% Draw a 3d thing with lights so we can see the material..   
%%    io:format("~p ~n", [{MVX1, MVY1, MVXSz, MVYSz, MVRatio}]),
    gl:viewport(MVX1 + S#s.x, MVY2 + S#s.y, MVXSz, MVYSz),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    if MVXSz > MVYSz ->
	    gl:ortho(-1.5, 1.5, -1.5*MVRatio, 1.5*MVRatio, -10, 10);
       true  -> %% Else
	    gl:ortho(-1.5*MVRatio, 1.5*MVRatio, -1.5, 1.5, -10, 10)
    end,
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    AmbM =  {AR*Amb#c.lscale, AG*Amb#c.lscale, AB*Amb#c.lscale, S#s.transp},
    DiffM = {DR*Diff#c.lscale, DG*Diff#c.lscale, DB*Diff#c.lscale, S#s.transp},
    SpecM = {SR*Spec#c.lscale, SG*Spec#c.lscale, SB*Spec#c.lscale, S#s.transp},
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT, AmbM),
    gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, DiffM),
    gl:materialfv(?GL_FRONT, ?GL_SPECULAR, SpecM),
    gl:materialfv(?GL_FRONT, ?GL_SHININESS, (1.0-S#s.shininess)*128.0),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_BLEND),
    gl:enable(?GL_DEPTH_TEST),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, 0.9, 50, 50),
    glu:deleteQuadric(Obj),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_BLEND),
    gl:disable(?GL_DEPTH_TEST),
    gl:swapBuffers(),
    Ns = 
	case check_event(S) of
	    quit ->
		exit(normal); 
	    {select, {X0, Y0}} when %% Color Selection
		  Y0 >= Amb#c.y - ?COLORCIRCLE_RADIUS,
		  Y0 =< Amb#c.y + ?COLORCIRCLE_RADIUS -> 		
		Selected = select_color(X0, Y0, S),
		update_selected(X0, Y0, S#s{mouse = Selected, key = Selected});
	    {select, {X0, Y0}} when  %% Shininess or Transparency 
		  Y0 =< TandSY, Y0 >= TandSY - ?LSCALEX, X0 < MaxX ->
		Selected = select_st(X0, ?BORDER_W, ?BORDER_W * 3 + TandSXLen, TandSXLen),
		update_selected(X0, Y0, S#s{mouse = Selected, key = Selected});
	    {select, {X0, Y0}} when  %% Buttons Ok or Cancel
		  Y0 =< ?ButtonSzY + ?BORDER_H div 2, Y0 >= ?BORDER_H div 2 ->
		Selected = select_butt(X0, OkX- ?ButtonSzX - ?BORDER_W, CancelX1, ?ButtonSzX, S),
		update_selected(X0, Y0, S#s{mouse = Selected, key = Selected});
	    {motion, {X0,Y0}} ->
		update_selected(X0, Y0, S);
	    {release, {X0,Y0}} ->
		Ns0 = update_selected(X0, Y0, S),
		Ns0#s{mouse = undefined};
	    _ ->
		S	
	end,
    timer:sleep(10),
    color_picker_loop(Ns).

update_selected(X0, Y0, S) ->
    case S#s.mouse of
	undefined -> 
	    S;
	ambient ->
	    Color = S#s.ambient,
	    {X,Y} = scale_pos(X0,Y0,Color#c.x,Color#c.y,?COLORCIRCLE_RADIUS, 1),
	    NewColor = do_update_color(Color, X, Y),
	    S#s{ambient = NewColor};
	{ambient, darkness} ->
	    Color = S#s.ambient,
	    X = scale_pos(Y0, Color#c.y - ?COLORCIRCLE_RADIUS, 2*?COLORCIRCLE_RADIUS, 1),
	    NewColor = do_update_color_darkness(Color, X),
	    S#s{ambient = NewColor};
	diffuse ->
	    Color = S#s.diffuse,	   
	    {X,Y} = scale_pos(X0,Y0,Color#c.x,Color#c.y,?COLORCIRCLE_RADIUS, 1),
	    NewColor = do_update_color(Color, X, Y),
	    S#s{diffuse = NewColor};
	{diffuse, darkness} ->
	    Color = S#s.diffuse,
	    X = scale_pos(Y0, Color#c.y - ?COLORCIRCLE_RADIUS, 2*?COLORCIRCLE_RADIUS, 1),
	    NewColor = do_update_color_darkness(Color, X),
	    S#s{diffuse = NewColor};
	specular ->
	    Color = S#s.specular,	   
	    {X,Y} = scale_pos(X0,Y0,Color#c.x,Color#c.y,?COLORCIRCLE_RADIUS, 1),
	    NewColor = do_update_color(Color, X, Y),
	    S#s{specular = NewColor};
	{specular, darkness} ->
	    Color = S#s.specular,
	    X = scale_pos(Y0, Color#c.y - ?COLORCIRCLE_RADIUS, 2*?COLORCIRCLE_RADIUS, 1),
	    NewColor = do_update_color_darkness(Color, X),
	    S#s{specular = NewColor};
	{transp, Xs, Size} ->
	    X = scale_pos(X0, Xs, Size, 1),
	    S#s{transp = X};
	{shininess, Xs, Size} ->
	    X = scale_pos_log(X0, Xs, Size, 1),
	    S#s{shininess = X};	    
	Else ->
	    io:format("~p ~p: internal error ~p ~n", [?MODULE, ?LINE, Else]),
	    S
    end.

select_color(X0, Y0, S) ->
    Amb = S#s.ambient,	  
    Diff = S#s.diffuse,	   
    Spec = S#s.specular,
    
    if 
	%% Ambient color selection 
	X0 >= Amb#c.x - ?COLORCIRCLE_RADIUS,
	X0 =< Amb#c.x + ?COLORCIRCLE_RADIUS ->	
	    check_circle(X0,Y0,Amb#c.x,Amb#c.y, ?COLORCIRCLE_RADIUS, ambient);
	%% Ambient darkness selection
	X0 >= Amb#c.x + ?COLORCIRCLE_RADIUS + ?LSCALEX - ?LSCALEHALFW,
	X0 =< Amb#c.x + ?COLORCIRCLE_RADIUS + ?LSCALEX + ?LSCALEHALFW ->
	    {ambient, darkness};
	%% Diffuse color selection
	X0 >= Diff#c.x - ?COLORCIRCLE_RADIUS,
	X0 =< Diff#c.x + ?COLORCIRCLE_RADIUS ->
	    check_circle(X0,Y0,Diff#c.x,Diff#c.y,?COLORCIRCLE_RADIUS, diffuse);
	%% Diffuse darkness selection
	X0 >= Diff#c.x + ?COLORCIRCLE_RADIUS + ?LSCALEX - ?LSCALEHALFW,
	X0 =< Diff#c.x + ?COLORCIRCLE_RADIUS + ?LSCALEX + ?LSCALEHALFW ->
	    {diffuse, darkness};
	%% Specular color selection
	X0 >= Spec#c.x - ?COLORCIRCLE_RADIUS,
	X0 =< Spec#c.x + ?COLORCIRCLE_RADIUS ->
	    check_circle(X0,Y0,Spec#c.x,Spec#c.y,?COLORCIRCLE_RADIUS, specular);
	%% Specular darkness selection
	X0 >= Spec#c.x + ?COLORCIRCLE_RADIUS + ?LSCALEX - ?LSCALEHALFW,
	X0 =< Spec#c.x + ?COLORCIRCLE_RADIUS + ?LSCALEX + ?LSCALEHALFW ->
	    {specular, darkness};
	true ->
	    undefined
    end.

select_st(X0, TS, SS, Size) ->
    if
	X0 >= TS, X0 =< TS + Size->
	    {transp, TS, Size};
	X0 >= SS, X0 =< SS + Size->
	    {shininess, SS, Size};
	true ->
	    undefined
    end.
	
select_butt(X0, OkS, CancelS, Size, S) ->
    if
	X0 >= OkS, X0 =< OkS + Size ->
	    Amb = S#s.ambient,	  
	    Diff = S#s.diffuse,	   
	    Spec = S#s.specular,
	    {AR,AG,AB} = Amb#c.rgb,
	    {DR,DG,DB} = Diff#c.rgb,
	    {SR,SG,SB} = Spec#c.rgb,
	    
	    AmbR  = {AR*Amb#c.lscale, AG*Amb#c.lscale, AB*Amb#c.lscale},
	    DiffR = {DR*Diff#c.lscale, DG*Diff#c.lscale, DB*Diff#c.lscale},
	    SpecR = {SR*Spec#c.lscale, SG*Spec#c.lscale, SB*Spec#c.lscale},	   
	    exit({normal, (S#s.prev)#mat{ambient = AmbR, 
					 diffuse = DiffR, 
					 specular = SpecR,
					 shininess = S#s.shininess,
					 opacity = S#s.transp
					}});
	X0 >= CancelS, X0 =< CancelS + Size ->
	    exit(normal);
	true ->
	    undefined
    end.
 	    
scale_pos(X0, X1, Scale, Constraint) ->
    Len = (X0 - X1) / Scale,
    if Len < 0 -> 0.0;
       Len > Constraint -> Constraint;
       true -> Len
    end.

scale_pos_log(X0, X1, Scale, Constraint) ->
    Len = ((X0 - X1) / Scale)*(?LOG_BASE-1)+1.0,
    if Len < 1.0 -> 0.0;
       Len > ?LOG_BASE -> 1.0;
       true -> log(Len, ?LOG_BASE)
    end.

log(X, Base) ->
    math:log(X)/math:log(Base).

inv_scale_pos_log(X) ->
    (math:pow(?LOG_BASE, X)-1.0) / (?LOG_BASE-1).

scale_pos(X0,Y0,X1,Y1, Scale, Constraint) ->
    X = (X0 - X1) / Scale,
    Y = (Y0 - Y1) / Scale,
    Length = math:sqrt(X*X+Y*Y),
    ConstraintMatch = 
	(Length > Constraint) or (abs(X) > Constraint) or (abs(Y) > Constraint),
    if 
	Length == 0.0 ->
	    {X, Y};
	ConstraintMatch ->
	    Div = Constraint / Length,
	    {chop(X * Div, Constraint), chop(Y * Div, Constraint)};
	true ->
	    {X, Y}
    end.

chop(X, Constraint) when abs(X) > Constraint ->
    if X > 0 ->
	    Constraint;
       X < 0 ->
	    -Constraint
    end;
chop(X, Constraint) ->
    X.

check_circle(X0, Y0, X1, Y1, Scale, Ret) -> 
    X = (X0 - X1) / Scale,
    Y = (Y0 - Y1) / Scale,
    Length = math:sqrt(X*X+Y*Y),
    if 
	Length =< 1 ->
	    Ret;
	true ->
	    undefined
    end.

do_update_color(CType, X, Y) ->
    Color = calc_color(X,Y),
    %%io:format("Ambient ~p ~p ~p~n", [X,Y,Color]),
    CType#c{rgb=Color, lscale=1.0, cx=X, cy=Y}.
do_update_color_darkness(CType, X) ->
    CType#c{lscale = X}.

draw_filled_box(X1,X2,Y1,Y2) ->
    gl:glBegin(?GL_QUADS),
    gl:vertex2f(X1,  Y1),
    gl:vertex2f(X1,  Y2),
    gl:vertex2f(X2,  Y2),
    gl:vertex2f(X2,  Y1),
    gl:glEnd().

draw_box(X1,X2,Y1,Y2) ->
    gl:glBegin(?GL_LINE_LOOP),
    gl:vertex2f(X1,  Y1),
    gl:vertex2f(X1,  Y2),
    gl:vertex2f(X2,  Y2),
    gl:vertex2f(X2,  Y1),
    gl:glEnd().

draw_centered_box(X,Y, XSz, YSz, BorB) ->
    X1 = X - XSz / 2,
    X2 = X + XSz / 2,
    Y1 = Y - YSz / 2,
    Y2 = Y + YSz / 2,
    {HighC,LowC} = case BorB of
		       button -> {?BEVEL_HIGHLIGHT, ?BEVEL_LOWLIGHT};
		       box    -> {?BEVEL_LOWLIGHT, ?BEVEL_HIGHLIGHT};
		       white  -> {?BEVEL_HIGHLIGHT, ?BEVEL_HIGHLIGHT};
		       black  -> {{0,0,0}, {0,0,0}}
		   end,
    gl:glBegin(?GL_LINES), 
    gl:color3fv(HighC),
    gl:vertex2f(X1,Y1), 
    gl:vertex2f(X1,Y2), 
    gl:vertex2f(X1,Y2), 
    gl:vertex2f(X2,Y2),
    gl:color3fv(LowC),
    gl:vertex2f(X2,Y2),
    gl:vertex2f(X2,Y1),
    gl:vertex2f(X2,Y1),
    gl:vertex2f(X1,Y1),
    gl:glEnd(),
    {X1,X2,Y1,Y2}.
    
center_text(X,Y,Text) ->
    W = length(Text) * 7,  H = 13, 
    wings_io:text_at(round(X - W / 2), round(Y - H / 2), Text).

draw_color(CType) ->
    gl:pushMatrix(),
    %% Draw color circle
    gl:translatef(CType#c.x, CType#c.y, 0.0),
    draw_color_selection(?COLORCIRCLE_RADIUS),	 
    gl:pushMatrix(),

    %% Draw Selected color Indicator
    CSX = CType#c.cx * ?COLORCIRCLE_RADIUS,
    CSY = CType#c.cy * ?COLORCIRCLE_RADIUS, 
    gl:color3f(0,0,0),
    gl:translatef(CSX, CSY, 0),

    gl:glBegin(?GL_LINE_LOOP),	
    gl:vertex2i(-?COLORSELECT_HALFSZ, ?COLORSELECT_HALFSZ),
    gl:vertex2i(?COLORSELECT_HALFSZ,  ?COLORSELECT_HALFSZ),
    gl:vertex2i(?COLORSELECT_HALFSZ, -?COLORSELECT_HALFSZ),
    gl:vertex2i(-?COLORSELECT_HALFSZ,-?COLORSELECT_HALFSZ),
    gl:glEnd(),
    gl:popMatrix(),

    %% Draw Light Scale selector
    gl:translatef(?COLORCIRCLE_RADIUS + ?LSCALEX, +?COLORCIRCLE_RADIUS, 0),
    gl:color3fv(CType#c.rgb),
    gl:glBegin(?GL_QUADS),
    gl:vertex2i(-?LSCALEHALFW, 0),
    gl:vertex2i(?LSCALEHALFW,  0),
    gl:color3f(0,0,0),
    gl:vertex2i(?LSCALEHALFW,  -?COLORCIRCLE_RADIUS*2),
    gl:vertex2i(-?LSCALEHALFW, -?COLORCIRCLE_RADIUS*2),	    
    gl:glEnd(),
    %% Draw Selected LightScale Indicator
    gl:translatef(0, -2*?COLORCIRCLE_RADIUS, 0),
    gl:glBegin(?GL_LINE_LOOP),
    LscalePosY = CType#c.lscale * ?COLORCIRCLE_RADIUS*2,
    LscalePosYU = LscalePosY + 2,
    LscalePosYD = LscalePosY - 2,
    LscaleXL = -?LSCALEHALFW-3,
    LscaleXR = ?LSCALEHALFW+3,
    gl:color3f(1,1,1),
    gl:vertex2f(LscaleXL, LscalePosYU),
    gl:vertex2f(LscaleXR, LscalePosYU),
    gl:vertex2f(LscaleXR, LscalePosYD),
    gl:vertex2f(LscaleXL, LscalePosYD),
    gl:glEnd(),
    gl:popMatrix(),
    gl:color3f(0,0,0),
    wings_io:text_at(CType#c.x - ?COLORCIRCLE_RADIUS div 2, 
     CType#c.y + ?COLORCIRCLE_RADIUS + 4, 
     CType#c.name).

draw_color_selection(Radie) ->
    gl:glBegin(?GL_TRIANGLE_FAN),
    gl:color3fv({1,1,1}),
    gl:vertex2f(0,0), %% First last 
    draw_color_selection(0, Radie),
    gl:glEnd().

draw_color_selection(Deg, Radie) when Deg < 360 ->
    XD = math:sin( deg2rad(Deg)),
    YD = math:cos( deg2rad(Deg)),
    Color = calc_color(XD, YD),
    gl:color3fv(Color),	   
    gl:vertex2f((XD*Radie),(Radie*YD)),
    draw_color_selection(Deg + ?COLORCIRCLE_STEP, Radie);
draw_color_selection(Deg, Radie) ->
    XD = math:sin( deg2rad(0)),
    YD = math:cos( deg2rad(0)),
    Color = calc_color(XD, YD),
    gl:color3fv(Color),	   
    gl:vertex2f((XD*Radie),(Radie*YD)).

rad2deg(R) ->
    R * 180 / math:pi().

deg2rad(R) ->
    R * math:pi() / 180.
  
calc_color(X, Y) when X =< 1.0, Y =< 1.0 ->  
    Saturation = math:sqrt(X*X+Y*Y),	
    %%    io:format("X ~p Y~p ~p ~p ~p ", [X,Y,Hue, Saturation, MinCol]),
    if 
	Saturation < 0.01 ->
	    {1.0, 1.0, 1.0};
	true -> 
	    Where = rad2deg(math:atan2(Y, X)), 
	    Hue = 
		if 
		    Where < 0 -> 360 + Where;
		    true      -> Where
		end, 
	    Yellow  = (Hue < 90) or (Hue >= 330),
	    Magenta = (Hue >= 90) and (Hue < 210),
	    Cyan    = (Hue >= 210) and (Hue < 330),
	    MinCol = 1.0 - Saturation,	    
	    if 
		Yellow ->
		    HueT = if 
			       Hue < 90 -> Hue + 30; %% Want Hue to be between 0 - 120.
			       true -> Hue + 30 - 360
			   end,
		    Blue = MinCol, 
		    if 
			HueT < 60 -> 
			    Green = 1.0,
			    Red = MinCol + HueT * (1.0 - MinCol)/ (120 - HueT),
			    %%     io:format("YellowGreen ~p ~p ~n", [{Red,Green, Blue}, HueT]),
			    {Red,Green,Blue};
			true -> 
			    Red = 1.0,
			    Green = MinCol + (120 - HueT) * (1.0 - MinCol)/ HueT,
			    %%     io:format("YellowRed ~p ~p ~n", [{Red,Green, Blue}, HueT]),
			    {Red,Green,Blue}
		    end;

		Magenta -> 
		    HueT = Hue - 90, %% Want Hue to be between 0 - 120.
		    Green = MinCol, 
		    if 
			HueT < 60 -> 
			    Red = 1.0,
			    Blue = MinCol + HueT * (1.0 - MinCol)/ (120 - HueT),
			    %%     io:format("Magenta ~p ~n", [{Red,Green, Blue}]),
			    {Red,Green,Blue};
			true -> 
			    Blue = 1.0,
			    Red = MinCol + (120 - HueT) * (1.0 - MinCol)/ HueT,
			    %%     io:format("Magenta ~p ~n", [{Red,Green, Blue}]),
			    {Red,Green,Blue}
		    end;

		Cyan -> 
		    HueT = Hue - 210, %% Want Hue to be between 0 - 120.
		    Red = MinCol, 
		    if 
			HueT < 60 -> 
			    Blue = 1.0,
			    Green = MinCol + HueT * (1.0 - MinCol)/ (120 - HueT),
			    %%     io:format("Cyan ~p ~n", [{Red,Green, Blue}]),
			    {Red,Green,Blue};
			true -> 
			    Green = 1.0,
			    Blue = MinCol + (120 - HueT) * (1.0 - MinCol)/ HueT,
			    %%     io:format("Cyan ~p ~n", [{Red,Green, Blue}]),
			    {Red,Green,Blue}
		    end;
		true ->
		    io:format("Error ~p  X ~p Y ~p Where ~p(~p) ~n", 
			      [{?MODULE,?LINE}, X,Y, Where, Hue]),
		    {0.0, 0.0, 0.0}
	    end
    end.

convert_material(Mat) ->
    case rgb_to_hsv(Mat) of
	{undefined, 0, Value} ->
	    #c{lscale = Value};
	{Hue, Sat, Value} ->
	    Rad = deg2rad(Hue),
	    {R,G,B} = Mat,
	    #c{rgb = {R / Value, G / Value, B / Value},
	       cx = Sat * math:cos(Rad), cy = Sat * math:sin(Rad), 
	       lscale = Value}
    end.

rgb_to_hsv({R,G,B}) ->
    rgb_to_hsv(R,G,B).
rgb_to_hsv(R,G,B) ->
    MaxCol = lists:max([R,G,B]),
    MinCol = lists:min([R,G,B]),
    Value  = MaxCol,
    if 
	MaxCol == MinCol ->
	    {undefined, 0, Value};
        MinCol == B ->
	    Saturation = (MaxCol - MinCol) / MaxCol,
	    Hue = 120 * (R - MinCol)/(R + G - 2 * MinCol) - 30,
	    {fixdeg(Hue), Saturation, Value};
	MinCol == G ->
	    Saturation = (MaxCol - MinCol) / MaxCol,
	    Hue = 120 * (1 + (B - MinCol)/(R + B - 2 * MinCol)) - 30,
	    {fixdeg(Hue), Saturation, Value};
	MinCol == R ->
	    Saturation = (MaxCol - MinCol) / MaxCol,
	    Hue = 120 * (2 + (G - MinCol)/(B + G - 2 * MinCol)) - 30,
	    {fixdeg(Hue), Saturation, Value}
    end.

fixdeg(Hue) when Hue < 0 ->
    Hue + 360;
fixdeg(Hue) ->
    Hue.

check_event(S) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    Event = wings_io:get_event(),
    case Event of
	quit ->
	    quit;
	[] -> 
	    ok;
	no_event ->
	    ok;
	Quit when record(Quit, keyboard) -> 
	    if 
		(Quit#keyboard.keysym)#keysym.sym == ?SDLK_ESCAPE ->
		    quit;
		(Quit#keyboard.keysym)#keysym.sym == ?SDLK_q ->
		    quit;
		true -> 
		    %%io:format("Got event ~p~n", [Quit]),
		    ok
	    end;		    
	{mousebutton,0,1,0, X,Y} ->
	    {release, translate_position(X, Y, S)};	
	{mousebutton,0,1,1, X,Y} ->
	    {select, translate_position(X, Y, S)};	
	{0, []} -> 
	    ok; 
	{'EXIT', Why} ->
	    io:format("Exit ~p ~n", [Why]);
	{NE, Evs} ->
	    io:format("Got ~p events: ~p~n", [NE, Evs]),
	    ok;
	#mousemotion{x=X,y=Y,state=State} when State band 1 =:= 1 ->
	    {motion, translate_position(X, Y, S)};
	#mousemotion{} ->
	    ok;
	Event -> 
	    io:format("Got event ~p~n", [Event]),
	    ok	  
    end.

translate_position(X, Y, S) ->
    WindowsSizeH = S#s.orig_h,
    %% X 0 Y 0 Upper Left 
    {X - S#s.x, (WindowsSizeH - Y) - S#s.y}.
