#
#  Makefile --
#
#     Makefile for building file dialog plug-in for Windows.
#
#  Copyright (c) 2003 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.11 2003/09/21 19:38:30 bjorng Exp $
#
LIBDIR = ../../plugins/win32_file

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)
ERL_INC = $(ERL_DIR)/usr/include
BEAM = $(LIBDIR)/wp8_file.beam
LIBS = -shared -lcomdlg32

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR) -I ../../e3d -I ../../src -I $(ESDL_PATH)/include -pa $(ESDL_PATH)

DRV = $(LIBDIR)/wings_file_drv.dll

all: $(BEAM) $(DRV)

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(LIBDIR)/wings_file_drv.dll: wings_file_drv.c \
  erl_driver.h erl_win_dyn_driver.h
	install -d $(LIBDIR)
	mingw32-gcc -o $(LIBDIR)/wings_file_drv.dll wings_file_drv.c $(LIBS)

erl_driver.h: $(ERL_INC)/erl_driver.h
	cp $(ERL_INC)/erl_driver.h .

erl_win_dyn_driver.h: $(ERL_INC)/erl_win_dyn_driver.h
	sed 's,##Params, Params,g' "$(ERL_INC)/erl_win_dyn_driver.h" > erl_win_dyn_driver.h
