#
#  Makefile --
#
#     Makefile for building file dialog plug-in for Mac.
#
#  Copyright (c) 2002 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.2 2002/07/11 18:19:14 bjorng Exp $
#
LIBDIR = ../../plugins/mac_file

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)

BEAM = $(LIBDIR)/wp8_mac_file.beam
ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR)

DRV = $(LIBDIR)/mac_wings_file_drv.so

all: $(DRV) $(BEAM)

$(LIBDIR)/wp8_mac_file.beam: wp8_mac_file.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) wp8_mac_file.erl

$(DRV): mac_wings_file_drv.c
	install -d $(LIBDIR)
	cc -ObjC -I $(ERL_DIR)/usr/include -bundle -flat_namespace -undefined suppress \
          -framework Cocoa -o $(LIBDIR)/mac_wings_file_drv.so mac_wings_file_drv.c
