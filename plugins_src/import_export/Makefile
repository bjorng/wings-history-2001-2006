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
#     $Id: Makefile,v 1.6 2002/11/04 14:34:40 dgud Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../../plugins/import_export
ERLC=erlc
WINGS_SRC=../../src
WINGS_E3D=../../e3d
ESDL=$(ESDL_PATH)

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_ai \
	wpc_3ds \
	wpc_obj \
	wpc_opengl \
	wpc_rib \
	wpc_ply \
	wpc_wrl

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -I $(WINGS_SRC) -I $(WINGS_E3D) -I $(ESDL_PATH)/include
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) -pa $(ESDL_PATH)/ebin +warn_unused_vars +debug_info

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
