/*
 *
 *  mac_wings_file_drv.c --
 *
 *     Erlang driver for native file dialog boxes for Mac OS X.
 *
 *  Copyright (c) 2001 Patrik Nyblom
 *
 *  Modified to support OSX by Sean Hinde
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: mac_wings_file_drv.c,v 1.4 2002/07/19 10:04:15 bjorng Exp $
 */

/*cc -ObjC -I ~/local/lib/erlang/usr/include -bundle -flat_namespace -undefined suppress -framework Cocoa -o ../../plugins/mac_file/mac_wings_file_drv.so mac_wings_file_drv.c */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "erl_driver.h"

#import <Cocoa/Cocoa.h>

#define PATH_MAX 1024

/*
** Interface routines
*/
static ErlDrvData mac_wings_file_start(ErlDrvPort port, char *buff);
static void mac_wings_file_stop(ErlDrvData handle);
static int mac_wings_file_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size);

/*
** Internal routines
*/


@interface ComboView : NSView {
    NSPoint center;
    NSColor *color;
    float radius;
}

// Standard view create/free methods
- (id)initWithFrame:(NSRect)frame;
- (void)dealloc;

// Drawing
- (void)drawRect:(NSRect)rect;
- (BOOL)isOpaque;
@end

/*
** The driver struct
*/
ErlDrvEntry mac_wings_file_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    mac_wings_file_start,      /* L_PTR start, called when port is opened */
    mac_wings_file_stop,       /* F_PTR stop, called when port is closed */
    NULL,                  /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "mac_wings_file_drv",      /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    mac_wings_file_control,    /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    NULL                   /* F_PTR outputv, reserved */
};

/*
** Driver initialization routine
*/
DRIVER_INIT(mac_wings_file_drv)
{
    return &mac_wings_file_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static ErlDrvData mac_wings_file_start(ErlDrvPort port, char *buff)
{
    return (ErlDrvData) 0;
}


/*
** Close a port
*/
static void mac_wings_file_stop(ErlDrvData handle)
{
    return;
}

/*
** Control message from erlang, syncronous operations which hang the
** emulator. This is not a place where you normally do blocking
** operations, but as the wings application is single threaded
** it doesn't matter.
*/
static int mac_wings_file_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size)
{
  int result;
  char *rbuff;
  char *defdir;
    char *filter;
    char *filter_desc;
    char *title;
    char *text;
    char *defname;
    switch (command) {
    case 0: /* Yes/No/Cancel question */
    case 4: /* Yes/No/Cancel question */
      {
	NSString *title = [NSString stringWithCString:buff]; /* Title of window */
	NSString *text = [NSString stringWithCString:buff + [title length] + 1]; /* Prompt text */

        switch (NSRunAlertPanel(title, text, @"Yes", @"No", @"Cancel")) {
	case NSAlertDefaultReturn:
	  strcpy(*res,"yes");
	  return 3;
	case NSAlertAlternateReturn:
	  strcpy(*res,"no");
	  return 2;
	default:
	  strcpy(*res,"aborted");
	  return 7;
	}
      }
    case 1: /* Open (or import) file */
    case 2: /* Save (or export) file */
      {
	NSArray *fileTypes;
	NSString* defdir1;
	NSString* filter1;
	NSString* title1;
	NSString* defname1;
          
	defdir = buff; /* Default directory */
	filter = defdir + strlen(defdir) + 1; /* Filter expression (.wings) */
	filter_desc = filter + strlen(filter) + 1;      /* Desc. of filter */
	title = filter_desc + strlen(filter_desc) + 1;  /* Title of dialog */
	defname = title + strlen(title) + 1; /* Default name for file */

        defdir1 = [NSString stringWithCString:defdir];
	/* The description of the filter is ignored for Mac */
        filter1 = [NSString stringWithCString:filter + 1]; // . not needed for mac
        title1 = [NSString stringWithCString:title];
        defname1 = [NSString stringWithCString:defname];
	
	rbuff=driver_alloc(PATH_MAX+1);
	strcpy(rbuff, defname);
	fileTypes = [NSArray arrayWithObject:filter1];
	
	if (command == 1) {
	  NSOpenPanel *oPanel = [NSOpenPanel openPanel];
	  [oPanel setAllowsMultipleSelection:NO];
	  result = [oPanel runModalForDirectory:defdir1 file:nil types:fileTypes];
	  if (result == NSOKButton) {
	    NSString *aFile = [oPanel filename];
	    [aFile getCString:rbuff];
	    *res = rbuff;
	    return strlen(rbuff);
	  }
	  driver_free(rbuff);
	  return 0;
	} else {
	  NSSavePanel *sPanel = [NSSavePanel savePanel];
	  ComboView *comboView = [ComboView alloc];
	  [sPanel setRequiredFileType:filter1];
	  [sPanel setAccessoryView:comboView];
	  result = [sPanel runModalForDirectory:defdir1 file:defname1];
	  if (result == NSOKButton) {
	    NSString *aFile = [sPanel filename];
	    [aFile getCString:rbuff];
	    *res = rbuff;
	    return strlen(rbuff);
	  }
	  driver_free(rbuff);
	  return 0;
	}
	return 0;
      }
    case 3: /* Message box */
    {
      NSString *text = [NSString stringWithCString:buff];
      NSString *title = [NSString stringWithCString:buff + strlen(buff) + 1];
      NSRunAlertPanel(title, text, nil, nil, nil);	
      return 0;
    }
    default:
        return -1; /* Error return, throws exception in erlang */
    }
}


@implementation ComboView

// initWithFrame: is NSView's designated initializer (meaning it should be
// overridden in the subclassers if needed, and it should call super, that is
// NSView's implementation).  In DotView we do just that, and also set the
// instance variables.
//
// Note that we initialize the instance variables here in the same way they are
// initialized in the nib file. This is adequate, but a better solution is to make
// sure the two places are initialized from the same place. Slightly more
// sophisticated apps which load nibs for each document or window would initialize
// UI elements at the time they're loaded from values in the program.

- (id)initWithFrame:(NSRect)frame {
    [super initWithFrame:frame];
    center.x = 50.0;
    center.y = 50.0;
    radius = 10.0;
    color = [[NSColor redColor] retain];
    return self;
}

// dealloc is the method called when objects are being freed. (Note that "release"
// is called to release objects; when the number of release calls reduce the
// total reference count on an object to zero, dealloc is called to free
// the object.  dealloc should free any memory allocated by the subclass
// and then call super to get the superclass to do additional cleanup.

- (void)dealloc {
    [color release];
    [super dealloc];
}

// drawRect: should be overridden in subclassers of NSView to do necessary
// drawing in order to recreate the the look of the view. It will be called
// to draw the whole view or parts of it (pay attention the rect argument);
// it will also be called during printing if your app is set up to print.
// In DotView we first clear the view to white, then draw the dot at its
// current location and size.

- (void)drawRect:(NSRect)rect {
    NSRect dotRect;

    [[NSColor whiteColor] set];
    NSRectFill([self bounds]);   // Equiv to [[NSBezierPath bezierPathWithRect:[self bounds]] fill]

    dotRect.origin.x = center.x - radius;
    dotRect.origin.y = center.y - radius;
    dotRect.size.width  = 2 * radius;
    dotRect.size.height = 2 * radius;
    
    [color set];
    [[NSBezierPath bezierPathWithOvalInRect:dotRect] fill];
}

// Views which totally redraw their whole bounds without needing any of the
// views behind it should override isOpaque to return YES. This is a performance
// optimization hint for the display subsystem. This applies to DotView, whose
// drawRect: does fill the whole rect its given with a solid, opaque color.

- (BOOL)isOpaque {
    return YES;
}

@end
