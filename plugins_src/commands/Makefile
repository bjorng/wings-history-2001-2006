#
#  Makefile --
#
#     Makefile for building the plug-ins.
#
#  Copyright (c) 2001-2002 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.1 2002/01/28 17:33:40 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../../plugins/commands
ERLC=erlc
WINGS_SRC=../../src

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG +debug_info
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_region

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -I $(WINGS_SRC)
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
