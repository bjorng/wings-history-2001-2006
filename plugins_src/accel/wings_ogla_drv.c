/*
 *  wings_ogla.c --
 *
 *     Erlang driver for OpenGL acceleration.
 *
 *  Copyright (c) 2004 Bjorn Gustavsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings_ogla_drv.c,v 1.1 2004/04/19 04:33:59 bjorng Exp $
 */

#include "erl_driver.h"

#ifdef __WIN32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#if defined(__APPLE__) && defined(__MACH__)
# include <OpenGL/gl.h>	/* Header File For The OpenGL Library */
#else
# include <GL/gl.h>	/* Header File For The OpenGL Library */
#endif

/*
 * Interface routines.
 */
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff);
static void wings_file_stop(ErlDrvData handle);
static int control(ErlDrvData handle, unsigned int command, 
                   char* buff, int count, 
                   char** res, int res_size);

int fbx_control(unsigned int command, 
                char* buff, int count, 
                char** res, int res_size);

/*
 * Internal routines
 */

/*
 * The driver struct
 */
ErlDrvEntry wings_file_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    wings_file_start,      /* L_PTR start, called when port is opened */
    wings_file_stop,       /* F_PTR stop, called when port is closed */
    NULL,                  /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "wings_ogla_drv",   /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    control,               /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    NULL                   /* F_PTR outputv, reserved */
};

/*
 * Driver initialization routine
 */
DRIVER_INIT(wings_file_drv)
{
  return &wings_file_driver_entry;
}

/*
 * Driver interface routines
 */

/*
 * Open a port
 */
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff)
{
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) 0;
}

/*
 * Close a port
 */
static void wings_file_stop(ErlDrvData handle)
{
}

static int
control(ErlDrvData handle, unsigned int command,
        char* buff, int count, 
        char** res, int res_size)
{
  switch (command) {
  case 0: {
    GLfloat* f = (GLfloat *) buff;
    glVertex3fv(f);
    glVertex3fv(f+3);
    glVertex3fv(f+6);
    *res = 0;
    return 0;
  }
  case 1: {
    GLfloat* f = (GLfloat *) buff;
    glVertex3fv(f);
    glVertex3fv(f+3);
    glVertex3fv(f+6);
    glVertex3fv(f+6);
    glVertex3fv(f+9);
    glVertex3fv(f);
    *res = 0;
    return 0;
  }
  case 2: {
    GLfloat* f = (GLfloat *) buff;
    glVertex3fv(f);
    glVertex3fv(f+3);
    glVertex3fv(f+6);
    glVertex3fv(f+9);
    *res = 0;
    return 0;
  }
  case 3: {
    GLfloat* f = (GLfloat *) buff;
    glVertex3fv(f);
    glVertex3fv(f+3);
    *res = 0;
    return 0;
  }
  default:
    return -1;
  }
}

void
send_response(char** res, char* s, int len)
{
   ErlDrvBinary* bin;

   bin = driver_alloc_binary(len); 
   memcpy(bin->orig_bytes, s, len);
   *res = (char *) bin;
}
