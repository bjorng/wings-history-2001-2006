#
#  Makefile --
#
#     Makefile for building file dialog plug-in for Windows.
#
#  Copyright (c) 2003-2004 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.14 2004/10/29 16:28:17 bjorng Exp $
#
LIBDIR = ../../plugins/win32_file

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERL_INC = $(ERL_DIR)/usr/include
BEAM = $(LIBDIR)/wp8_file.beam
LIBS = -shared -lcomdlg32

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR) -I ../../e3d -I ../../src -I $(ESDL_PATH)/include -pa $(ESDL_PATH) -W +warn_unused_vars

DRV = $(LIBDIR)/wings_file_drv.dll

all: $(BEAM) $(DRV)

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(LIBDIR)/wings_file_drv.dll: wings_file_drv.c
	install -d $(LIBDIR)
	mingw32-gcc -I$(ERL_INC) -o $(LIBDIR)/wings_file_drv.dll wings_file_drv.c $(LIBS)
