/*
 *  perlin_noise_drv.c --
 *
 *     Erlang driver for generating simplex perlin noise
 *
 *  Copyright (c) 2005 Dan Gudmundsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: perlin_noise_drv.c,v 1.1 2006/01/14 09:02:38 dgud Exp $
 */

#include <stdio.h>
#include "erl_driver.h"

#ifdef __WIN32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include <math.h>

#define PNOISE3 3

#define PNOISE_MAP1 11
#define PNOISE_MAP2 12
#define PNOISE_MAP3 13

/* Function declarations */
double   fade(double t);
double   lerp(double t, double a, double b);
double   grad(int hash, double x, double y, double z); 
void     init();
double   pnoise(double x, double y, double z);

/*
 * Interface routines.
 */
static ErlDrvData perlin_noise_start(ErlDrvPort port, char *buff);
static void perlin_noise_stop(ErlDrvData handle);
static int control(ErlDrvData handle, unsigned int command, 
                   char* buff, int count, 
                   char** res, int res_size);

/*
 * Internal routines
 */

/*
 * The driver struct
 */
ErlDrvEntry perlin_file_driver_entry = {
   NULL,		   /* F_PTR init, N/A */
   perlin_noise_start,      /* L_PTR start, called when port is opened */
   perlin_noise_stop,       /* F_PTR stop, called when port is closed */
   NULL,                  /* F_PTR output, called when erlang has sent */
   NULL,                  /* F_PTR ready_input, called when input descriptor 
			     ready */
   NULL,                  /* F_PTR ready_output, called when output 
			     descriptor ready */
   "perlin_noise_drv",     /* char *driver_name, the argument to open_port */
   NULL,                  /* F_PTR finish, called when unloaded */
   NULL,                  /* void * that is not used (BC) */
   control,               /* F_PTR control, port_control callback */
   NULL,                  /* F_PTR timeout, driver_set_timer callback */
   NULL                   /* F_PTR outputv, reserved */
};

/*
 * Driver initialization routine
 */
DRIVER_INIT(perlin_file_drv)
{
   return &perlin_file_driver_entry;
}

/*
 * Driver interface routines
 */

/*
 * Open a port
 */
static ErlDrvData perlin_noise_start(ErlDrvPort port, char *buff)
{
   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
   return (ErlDrvData) 0;
}

/*
 * Close a port
 */
static void perlin_noise_stop(ErlDrvData handle)
{
   
}

static int control(ErlDrvData handle, unsigned int command,
		   char* buff, int count, 
		   char** res, int res_size)
{
   ErlDrvBinary* bin;

   switch (command) {

   case PNOISE3: {
      double f[3];
      memcpy(f, buff, sizeof(double)*3);
      bin = driver_alloc_binary(sizeof(double)); 
      * (double *) bin->orig_bytes = pnoise(f[0], f[1], f[2]);
      *res = (char *) bin;
      return sizeof(double);
   }

   case PNOISE_MAP1: {
      int i;
      int sz = * ((unsigned int*) buff);
      unsigned char *noise;
      bin = driver_alloc_binary(sz); 
      noise = bin->orig_bytes;
      for(i=0; i < sz ; i++) {
	 double where = (double)i/(sz-1);
	 double temp = pnoise(where,where,where);
	 *noise++ = (unsigned char) floor((temp*255.0)+127.5);
      }
      
      *res = (char *) bin;
      return sz;
   }
   case PNOISE_MAP2: {
      int sz = * ((unsigned int*) buff);
      int i,j;
      unsigned char *noise;
      bin = driver_alloc_binary(sz*sz); 
      noise = bin->orig_bytes;
      
      for(i=0; i < sz ; i++) {
	 for(j=0; j < sz ; j++) {
	    double where = (double)i/(sz-1);
	    double temp = pnoise(where,(double)j/(sz-1),where);
	    *noise++ = (unsigned char) floor((temp*255.0)+127.5);
	 }
      }
      
      *res = (char *) bin;
      return sz*sz;
   }

   case PNOISE_MAP3: {
      int sz = * ((unsigned int*) buff);
      int i,j,k;
      unsigned char *noise;
      bin = driver_alloc_binary(sz*sz*sz); 
      noise = bin->orig_bytes;
      
      for(i=0; i < sz ; i++) {
	 for(j=0; j < sz ; j++) {
	    for(k=0; k < sz ; k++) {
	       double temp = pnoise((double)i/(sz-1),
				    (double)j/(sz-1),
				    (double)k/(sz-1));
	       * noise++ = (unsigned char) floor((temp*255.0)+127.5);
	    }
	 } 
      }	 
      *res = (char *) bin;
      return sz*sz;
   }
      
   default:
      fprintf(stderr, "ARRG whats happening\r\n");
      *res = 0;
      return -1;
   }
}

static int p[512] = 
 { 151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,
   21,10,23,190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
   35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168, 68,175,
   74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,
   230,220,105,92,41,55,46,245,40,244,102,143,54, 65,25,63,161, 1,216,
   80,73,209,76,132,187,208, 89,18,169,200,196,135,130,116,188,159,86,
   164,100,109,198,173,186, 3,64,52,217,226,250,124,123,5,202,38,147,
   118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,
   183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,
   172,9,129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,
   218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,241, 81,51,
   145,235,249,14,239,107,49,192,214,31,181,199,106,157,184, 84,204,176,
   115,121,50,45,127, 4,150,254,138,236,205,93,222,114,67,29,24,72,243,
   141,128,195,78,66,215,61,156,180,
   151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,
   21,10,23,190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
   35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168, 68,175,
   74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,
   230,220,105,92,41,55,46,245,40,244,102,143,54, 65,25,63,161, 1,216,
   80,73,209,76,132,187,208, 89,18,169,200,196,135,130,116,188,159,86,
   164,100,109,198,173,186, 3,64,52,217,226,250,124,123,5,202,38,147,
   118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,
   183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,
   172,9,129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,
   218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,241, 81,51,
   145,235,249,14,239,107,49,192,214,31,181,199,106,157,184, 84,204,176,
   115,121,50,45,127, 4,150,254,138,236,205,93,222,114,67,29,24,72,243,
   141,128,195,78,66,215,61,156,180
 };

double pnoise(double x, double y, double z) 
{
   int   X,Y,Z;
   double u,v,w;
   int A,AA,AB,B,BA,BB;

   X = (int)floor(x) & 255;             /* FIND UNIT CUBE THAT */
   Y = (int)floor(y) & 255;             /* CONTAINS POINT.     */
   Z = (int)floor(z) & 255;
   x -= floor(x);                       /* FIND RELATIVE X,Y,Z */
   y -= floor(y);                       /* OF POINT IN CUBE.   */
   z -= floor(z);
   u = fade(x);                         /* COMPUTE FADE CURVES */
   v = fade(y);                         /* FOR EACH OF X,Y,Z.  */
   w = fade(z);

   A  = p[X]+Y;
   AA = p[A]+Z;
   AB = p[A+1]+Z; /* HASH COORDINATES OF */
   B  = p[X+1]+Y;
   BA = p[B]+Z;
   BB = p[B+1]+Z; /* THE 8 CUBE CORNERS, */

   return lerp(w,lerp(v,lerp(u, grad(p[AA  ], x, y, z),    /* AND ADD */
			     grad(p[BA  ], x-1, y, z)),    /* BLENDED */
		      lerp(u, grad(p[AB  ], x, y-1, z),    /* RESULTS */
			   grad(p[BB  ], x-1, y-1, z))),   /* FROM  8 */
	       lerp(v, lerp(u, grad(p[AA+1], x, y, z-1 ),  /* CORNERS */
			    grad(p[BA+1], x-1, y, z-1)),   /* OF CUBE */
		    lerp(u, grad(p[AB+1], x, y-1, z-1),
			 grad(p[BB+1], x-1, y-1, z-1))));
}

double fade(double t){ return t * t * t * (t * (t * 6 - 15) + 10); }
double lerp(double t, double a, double b){ return a + t * (b - a); }
double grad(int hash, double x, double y, double z) 
{
   int     h = hash & 15;       /* CONVERT LO 4 BITS OF HASH CODE */
   double  u = h < 8 ? x : y,   /* INTO 12 GRADIENT DIRECTIONS.   */
      v = h < 4 ? y : h==12||h==14 ? x : z;
   return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
}
