/*
 *
 *  mac_image_drv.c --
 *
 *     Erlang driver for image reading and writing for Mac
 *
 *  Copyright (c) 2002 Bjorn Gustavsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: mac_wings_image_drv.m,v 1.2 2002/11/17 10:29:34 bjorng Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_driver.h"

#import <Cocoa/Cocoa.h>

#define PATH_MAX 1024

/*
 * Interface routines
 */
static ErlDrvData mac_image_start(ErlDrvPort port, char *buff);
static void mac_image_stop(ErlDrvData handle);
static int mac_image_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size);

/*
 * Internal routines
 */

/*
 * The driver struct
 */
ErlDrvEntry mac_image_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    mac_image_start,      /* L_PTR start, called when port is opened */
    mac_image_stop,       /* F_PTR stop, called when port is closed */
    NULL,                  /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "mac_wings_image_drv", /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    mac_image_control,    /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    NULL                   /* F_PTR outputv, reserved */
};

/*
 * Driver initialization routine
 */
DRIVER_INIT(mac_image_drv)
{
    return &mac_image_driver_entry;
}

/*
 * Driver interface routines
 */

/*
 * Open a port.
 */
static ErlDrvData mac_image_start(ErlDrvPort port, char *buff)
{
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) 0;
}

/*
 * Close a port.
 */
static void mac_image_stop(ErlDrvData handle)
{
}

static int mac_image_control(ErlDrvData handle, unsigned int command, 
			     char* buff, int count, 
			     char** res, int res_size)
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  switch (command) {
  case 0: {			/* Read */
    NSString* name = [NSString stringWithCString:buff];
    NSBitmapImageRep* bitmap = [NSBitmapImageRep imageRepWithContentsOfFile:name];

    if (bitmap == nil) {
      *res = NULL;
    } else {
      ErlDrvBinary* bin;
      size_t size;
      unsigned char* rbuf;

      size = [bitmap bytesPerPlane];
      bin = driver_alloc_binary(size+16);
      rbuf = bin->orig_bytes;
      ((unsigned *)rbuf)[0] = [bitmap pixelsWide];
      ((unsigned *)rbuf)[1] = [bitmap pixelsHigh];
      ((unsigned *)rbuf)[2] = [bitmap samplesPerPixel];
      ((unsigned *)rbuf)[3] = [bitmap bytesPerRow];
      memcpy(rbuf+16, [bitmap bitmapData], size);
      *res = (void *) bin;
    }
    [pool release];
    return 0;
  }
  default:
    [pool release];
    return -1; /* Error return, throws exception in erlang */
  }
}
