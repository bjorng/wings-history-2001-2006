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
#     $Id: Makefile,v 1.9 2004/11/27 19:27:16 bjorng Exp $
#
OS := $(shell uname -s)

ifeq ($(findstring CYGWIN,$(OS)),CYGWIN)
  CYGWIN := true
endif

LIBDIR = ../../plugins/accel

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERL_INC = $(ERL_DIR)/usr/include
BEAM = $(LIBDIR)/wpc_ogla.beam

ifdef CYGWIN
 SO_EXT = dll
 LIBS = -shared
 GL_LIBS = -lopengl32 -lglu32
else
 SO_EXT = so
 ifeq ($(OS),Darwin)
   LIBS = -bundle -flat_namespace -undefined suppress
   GL_LIBS =
 else
   LIBS = -shared -fpic
   GL_LIBS = -lGLU -lGL
 endif
endif

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR)

DRV = $(LIBDIR)/wings_ogla_drv.$(SO_EXT)

TARGET_FILES=$(BEAM) $(DRV)

all: $(TARGET_FILES)

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $<

ifdef CYGWIN
$(LIBDIR)/wings_ogla_drv.dll: wings_ogla_drv.c
	install -d $(LIBDIR)
	mingw32-gcc -I$(ERL_INC) -o $(LIBDIR)/wings_ogla_drv.dll \
  wings_ogla_drv.c $(LIBS) $(GL_LIBS)
else
$(LIBDIR)/wings_ogla_drv.so: wings_ogla_drv.c
	install -d $(LIBDIR)
	gcc -o $(LIBDIR)/wings_ogla_drv.so -I$(ERL_INC) \
  wings_ogla_drv.c $(LIBS) $(GL_LIBS)
endif

clean:
	rm -f $(TARGET_FILES)
	rm -f core
