#
#  Makefile --
#
#     Makefile for building icons for Wings 3D.
#
#  Copyright (c) 2001 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.1 2002/09/18 13:16:06 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop
.PHONY: all

ESRC=.
EBIN=../ebin
ERLC=erlc


TARGET_FILES=all

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

all:
	erl -compile make_compat
	erl -noshell -run make_compat start $(EBIN) -s erlang halt
