#
#  Makefile --
#
#     Makefile for fonts.
#
#  Copyright (c) 2001-2005 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.1 2005/04/04 13:59:25 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

include ../vsn.mk

ESRC=.
EBIN=.
FONT_DIR=../fonts
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

FONTS= \
	6x11

MODULES= \
	bdf2wingsfont

TARGET_FILES=  $(MODULES:%=$(EBIN)/%.beam) $(FONTS:%=$(FONT_DIR)/%.wingsfont)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) +debug_info '-Dwings_version="$(WINGS_VSN)"'

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES)


$(FONT_DIR)/6x11.wingsfont: 6x11_latin1.bdf 6x11_latin2.bdf
	erl -noshell -run bdf2wingsfont convert $@ $?

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
