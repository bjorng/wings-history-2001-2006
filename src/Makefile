#
#  Makefile --
#
#     Makefile for building Wings 3D.
#
#  Copyright (c) 2001-2002 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.23 2002/04/11 08:20:39 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

include ../vsn.mk

ESRC=.
EBIN=../ebin
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG +debug_info
else
TYPE_FLAGS=+warn_unused_vars
endif

MODULES= \
	user_default \
	wings \
	wings_align \
	wings_ask \
	wings_body \
	wings_camera \
	wings_collapse \
	wings_color \
	wings_deform \
	wings_drag \
	wings_draw \
	wings_draw_util \
	wings_edge \
	wings_edge_loop \
	wings_extrude_edge \
	wings_extrude_face \
	wings_face \
	wings_face_cmd \
	wings_file \
	wings_ff_ndo \
	wings_ff_wings \
	wings_getline \
	wings_help \
	wings_hotkey \
	wings_io \
	wings_magnet \
	wings_material \
	wings_menu \
	wings_menu_util \
	wings_move \
	wings_pick \
	wings_plugin \
	wings_pref \
	wings_rotate \
	wings_sel \
	wings_sel_cmd \
	wings_scale \
	wings_shape \
	wings_shapes \
	wings_start \
	wings_subdiv \
	wings_text \
	wings_undo \
	wings_util \
	wings_vec \
	wings_vertex \
	wings_vertex_cmd \
	wings_we \
	wings_view \
	wpa

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(ESDL_PATH)/ebin
ERL_COMPILE_FLAGS += -I$(ESDL_PATH)/include -I ../e3d -W $(TYPE_FLAGS) \
 '-Dwings_version="$(WINGS_VSN)"'

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


$(TARGET_FILES): wings.hrl

