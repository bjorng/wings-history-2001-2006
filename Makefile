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
#     $Id: Makefile,v 1.4 2002/09/18 13:16:05 bjorng Exp $
#

all:
	(cd src; $(MAKE))
	(cd e3d; $(MAKE))
	(cd plugins_src; $(MAKE))
	(cd icons; $(MAKE))
	(cd compat; $(MAKE))

debug:
	(cd src; $(MAKE) debug)
	(cd e3d; $(MAKE) debug)
	(cd plugins_src; $(MAKE) debug)
	(cd icons; $(MAKE) debug)
	(cd compat; $(MAKE) debug)

clean:
	(cd src; $(MAKE) clean)
	(cd e3d; $(MAKE) clean)
	(cd plugins_src; $(MAKE) clean)
	(cd icons; $(MAKE) clean)

