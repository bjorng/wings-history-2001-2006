#
#  Makefile --
#
#     Makefile for building Wings "accelerator" helpers.
#
#  Copyright (c) 2004 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.1 2004/04/19 04:33:59 bjorng Exp $
#
OS := $(shell uname -s)

ifeq ($(findstring CYGWIN,$(OS)),CYGWIN)
  CYGWIN := true
endif

LIBDIR = ../../plugins/accel

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERL_INC = $(ERL_DIR)/usr/include
BEAM = $(LIBDIR)/wpc_ogla.beam

ifeq ($(OS),Darwin)
   LIBS = -bundle -flat_namespace -undefined suppress
else
   LIBS = -shared -lopengl32
endif

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR)

ifdef CYGWIN
SO_EXT = dll
else
SO_EXT = so
endif

DRV = $(LIBDIR)/wings_ogla_drv.$(SO_EXT)

all: $(BEAM) $(DRV)

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $<

ifdef CYGWIN
$(LIBDIR)/wings_ogla_drv.dll: wings_ogla_drv.c \
  erl_driver.h erl_win_dyn_driver.h
	install -d $(LIBDIR)
	mingw32-gcc -o $(LIBDIR)/wings_ogla_drv.dll \
  wings_ogla_drv.c $(LIBS)

erl_driver.h: $(ERL_INC)/erl_driver.h
	cp $(ERL_INC)/erl_driver.h .

erl_win_dyn_driver.h: $(ERL_INC)/erl_win_dyn_driver.h
	sed 's,##Params, Params,g' "$(ERL_INC)/erl_win_dyn_driver.h" > erl_win_dyn_driver.h
else

$(LIBDIR)/wings_ogla_drv.so: wings_ogla_drv.c
	install -d $(LIBDIR)
	gcc -o $(LIBDIR)/wings_ogla_drv.so -I$(ERL_INC) \
  wings_ogla_drv.c $(LIBS)
endif

