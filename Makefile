#
#  Makefile --
#
#     Top-level Makefile for building Wings 3D.
#
#  Copyright (c) 2001 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.1.1.1 2001/08/14 18:16:30 bjorng Exp $
#

all:
	(cd src; $(MAKE))
	(cd e3d; $(MAKE))
	(cd icons; $(MAKE))

debug:
	(cd src; $(MAKE) debug)
	(cd e3d; $(MAKE) debug)
	(cd icons; $(MAKE) debug)
