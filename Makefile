#
#  Makefile --
#
#     Top-level Makefile for building Wings 3D.
#
#  Copyright (c) 2001-2004 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.17 2005/03/12 06:37:35 bjorng Exp $
#
include vsn.mk

all:
	(cd src; $(MAKE))
	(cd e3d; $(MAKE))
	(cd plugins_src; $(MAKE))
	(cd icons; $(MAKE))

debug:
	(cd src; $(MAKE) debug)
	(cd e3d; $(MAKE) debug)
	(cd plugins_src; $(MAKE) debug)
	(cd icons; $(MAKE) debug)

clean:
	(cd src; $(MAKE) clean)
	(cd e3d; $(MAKE) clean)
	(cd plugins_src; $(MAKE) clean)
	(cd icons; $(MAKE) clean)

lang:
	(cd src; $(MAKE) lang)

#
# Build installer for Windows.
#
win32: all lang
	(cd plugins_src/win32_file; $(MAKE))
	(cd plugins_src/jpeg; $(MAKE))
	(cd win32; $(MAKE))
	win32/make_installer

#
# Build a package for MacOS X.
#
macosx: all lang
	(cd plugins_src/mac_file; $(MAKE))
	(cd plugins_src/fbx; $(MAKE))
	(cd macosx; xcodebuild)
	sh tools/mac_make_dmg $(WINGS_VSN)

#
# Build package for Unix.
#
unix: all lang
	(cd plugins_src/jpeg; $(MAKE))
	unix/make_installer

