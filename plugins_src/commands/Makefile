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
#     $Id: Makefile,v 1.7 2002/10/15 14:47:12 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../../plugins/commands
ERLC=erlc
WINGS_SRC=../../src
WINGS_E3D=../../e3d

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_region \
	wpc_triquad_cmd \
	wpc_turnedge \
	wpc_tweak

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -I $(WINGS_SRC) -I $(WINGS_E3D) -pa $(ESDL_PATH)/ebin
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) +warn_unused_vars +debug_info

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
