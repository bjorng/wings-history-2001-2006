#
#  Makefile --
#
#     Makefile for building the E3D modules.
#
#  Copyright (c) 2001 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.2 2001/09/03 11:01:39 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../ebin
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	e3d_mesh \
	e3d_obj \
	e3d_tds \
	e3d_mat \
	e3d_vec \
	e3d__meshclean

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -W +debug_info $(TYPE_FLAGS)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
