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
#     $Id: Makefile,v 1.3 2005/06/08 05:59:19 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../ebin
ERLC=erlc

TARGET_FILES=$(EBIN)/wings_icon.bundle

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

$(EBIN)/wings_icon.bundle: *.bmp *.tga collect_bmp.erl
	erl -compile collect_bmp
	erl -noinput -run collect_bmp start . $(EBIN)/wings_icon.bundle -s erlang halt

clean:
	rm -f $(TARGET_FILES)
	rm -f core
