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
#     $Id: Makefile,v 1.4 2006/07/28 18:54:27 antoneos Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../ebin
ERLC=erlc

TARGET_FILES=$(EBIN)/wings_icon.bundle

ICONSETS= \
	classic \
	bluecube

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

$(EBIN)/wings_icon.bundle: collect_bmp.erl
	erl -compile collect_bmp
	@for ICONSET in $(ICONSETS); do \
		erl -noinput -run collect_bmp start $$ICONSET $(EBIN)/wings_icon_$$ICONSET.bundle -s erlang halt; \
	done

clean:
	@for ICONSET in $(ICONSETS); do \
		rm -f $(EBIN)/wings_icon_$$ICONSET.bundle; \
	done
	rm -f collect_bmp.beam
