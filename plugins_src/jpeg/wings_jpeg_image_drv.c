/*
 *
 *  wings_jpg_drv.c --
 *
 *     Erlang driver for reading and writing JPEG files
 *     using libjpeg from IJG (Independent JPEG Group).
 *
 *  Copyright (c) 2004 Bjorn Gustavsson
 *
 *  libjpeg is copyright (C) 1991-1998, Thomas G. Lane.
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings_jpeg_image_drv.c,v 1.1 2004/01/06 11:28:20 bjorng Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include "erl_driver.h"

#include "jpeglib.h"
#include "jerror.h"

/*
 * Interface routines
 */
static ErlDrvData jpeg_image_start(ErlDrvPort port, char *buff);
static void jpeg_image_stop(ErlDrvData handle);
static int jpeg_image_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size);

/*
 * Internal functions.
 */
static void jpeg_buffer_src(j_decompress_ptr cinfo, char* buf, int count);


/*
 * The driver struct
 */

ErlDrvEntry jpeg_image_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    jpeg_image_start,      /* L_PTR start, called when port is opened */
    jpeg_image_stop,       /* F_PTR stop, called when port is closed */
    NULL,                  /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "wings_jpeg_image_drv", /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    jpeg_image_control,    /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    NULL                   /* F_PTR outputv, reserved */
};

/*
 * Driver initialization routine
 */
DRIVER_INIT(jpeg_image_drv)
{
    return &jpeg_image_driver_entry;
}

/*
 * Open a port.
 */
static ErlDrvData
jpeg_image_start(ErlDrvPort port, char *buff)
{
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) 0;
}

/*
 * Close a port.
 */
static void
jpeg_image_stop(ErlDrvData handle)
{
}

static int
jpeg_image_control(ErlDrvData handle, unsigned int command, 
		   char* buf, int count, 
		   char** res, int res_size)
{
  switch (command) {
  case 0: {			/* Read */
    struct jpeg_decompress_struct cinfo;
    int row_stride;		/* physical row width in output buffer */
    JSAMPROW row;
    ErlDrvBinary* bin = 0;
    unsigned char* rbuf;
    struct jpeg_error_mgr jerr;

    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);
    jpeg_buffer_src(&cinfo, buf, count);
    (void) jpeg_read_header(&cinfo, TRUE);
    (void) jpeg_start_decompress(&cinfo);

    row_stride = cinfo.output_width * cinfo.output_components;
    res_size = row_stride * cinfo.output_height;
    bin = driver_alloc_binary(res_size+12);
    rbuf = bin->orig_bytes;
    ((unsigned *)rbuf)[0] = cinfo.output_width;
    ((unsigned *)rbuf)[1] = cinfo.output_height;
    ((unsigned *)rbuf)[2] = cinfo.output_components;
    rbuf += 12;
    while (cinfo.output_scanline < cinfo.output_height) {
      row = (JSAMPROW) rbuf;
      (void) jpeg_read_scanlines(&cinfo, &row, 1);
      rbuf += row_stride;
    }
    (void) jpeg_finish_decompress(&cinfo);
    *res = (void *) bin;
    return 0;
  }
  case 1: {			/* Write */
    return 0;
  }
  default:
    return -1;			/* Error return, throws exception in erlang */
  }
}

/*
 * Being slightly paranoid :), I don't want to use the
 * stdio-based data sources and destinations, as stdio is
 * not used anywhere else in the Erlang virtual machine.
 *
 * Here is a memory based source.
 */

/* Expanded data source object for memory */

typedef struct {
  struct jpeg_source_mgr pub;	/* public fields */
  JOCTET* current;		/* current pointer into buffer */
  size_t left;			/* number of bytes left in buffer */
} MemSourceMgr;

/*
 * Initialize source --- called by jpeg_read_header
 * before any data is actually read.
 */

METHODDEF(void)
init_source (j_decompress_ptr cinfo)
{
  /* Nothing to do here. */
}

METHODDEF(boolean)
fill_input_buffer(j_decompress_ptr cinfo)
{
  MemSourceMgr* src = (MemSourceMgr *) cinfo->src;

  if (src->left == 0) {
    ERREXIT(cinfo, JERR_INPUT_EMPTY);
  }

  src->pub.next_input_byte = src->current;
  src->pub.bytes_in_buffer = src->left;
  src->left = 0;
  return TRUE;
}


METHODDEF(void)
skip_input_data (j_decompress_ptr cinfo, long num_bytes)
{
  /* We assume that this function will never get called. */
}

METHODDEF(void)
term_source (j_decompress_ptr cinfo)
{
  /* no work necessary here */
}

/*
 * Prepare for input from a memory buffer.
 */

static void
jpeg_buffer_src(j_decompress_ptr cinfo, char* buf, int count)
{
  MemSourceMgr* src;

  if (cinfo->src == NULL) {	/* first time for this JPEG object? */
    cinfo->src = (struct jpeg_source_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  sizeof(MemSourceMgr));
    src = (MemSourceMgr *) cinfo->src;
  }

  src = (MemSourceMgr *) cinfo->src;
  src->pub.init_source = init_source;
  src->pub.fill_input_buffer = fill_input_buffer;
  src->pub.skip_input_data = skip_input_data;
  src->pub.resync_to_restart = jpeg_resync_to_restart; /* use default method */
  src->pub.term_source = term_source;

  src->current = (JOCTET *) buf;
  src->left = (size_t) count;
  src->pub.bytes_in_buffer = 0; /* forces fill_input_buffer on first read */
  src->pub.next_input_byte = NULL; /* until buffer loaded */
}
