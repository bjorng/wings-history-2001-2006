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
#     $Id: Makefile,v 1.3 2002/07/12 04:55:18 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../plugins/default
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG +debug_info
else
TYPE_FLAGS=
endif

MODULES= \
	wp9_dialogs

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += +warn_unused_vars
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES) subdirs

subdirs:
	(cd import_export; $(MAKE))
	(cd primitives; $(MAKE))
	(cd commands; $(MAKE))

clean: subdirs_clean
	rm -f $(TARGET_FILES)
	rm -f core

subdirs_clean:
	(cd import_export; $(MAKE) clean)
	(cd primitives; $(MAKE) clean)
	(cd commands; $(MAKE) clean)

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
