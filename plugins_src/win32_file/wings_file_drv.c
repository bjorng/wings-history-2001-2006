#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef DEBUG
#ifndef __WIN32__
#define ASSERT(X) do {if (!(X)) {erl_exit(1,"%s",#X);} } while(0)
#else
#include <assert.h>
#define ASSERT(X) assert(X)
#endif
#else
#define ASSERT(X)
#endif



#include "erl_driver.h"



/*
** Interface routines
*/
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff);
static void wings_file_stop(ErlDrvData handle);
static int wings_file_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size);

/*
** Internal routines
*/

/*
** The driver struct
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
    "wings_file_drv",      /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    wings_file_control,    /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    NULL                   /* F_PTR outputv, reserved */
};

/*
** Driver initialization routine
*/
DRIVER_INIT(wings_file_drv)
{
    return &wings_file_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff)
{
    return (ErlDrvData) 0;
}


/*
** Close a port
*/
static void wings_file_stop(ErlDrvData handle)
{
    return;
}

static void fill_ofn(OPENFILENAME *pofn)
{
    pofn->lStructSize = sizeof(OPENFILENAME);
    pofn->hwndOwner = GetActiveWindow();
    pofn->hInstance = NULL; 
    pofn->lpstrFilter = NULL;     /* For caller to fill in */ 
    pofn->lpstrCustomFilter = NULL; 
    pofn->nMaxCustFilter = 0; 
    pofn->nFilterIndex = 0; 
    pofn->lpstrFile = NULL;       /* For caller to fill in */ 
    pofn->nMaxFile = 0;           /* For caller to fill in */ 
    pofn->lpstrFileTitle = NULL; 
    pofn->nMaxFileTitle = 0; 
    pofn->lpstrInitialDir = NULL; /* For caller to fill in */
    pofn->lpstrTitle = NULL;      /* For caller to fill in */
    pofn->Flags = 0;              /* For caller to fill in */
    pofn->nFileOffset = 0; 
    pofn->nFileExtension = 0; 
    pofn->lpstrDefExt = NULL;     /* For caller to fill in */
    pofn->lCustData = 0; 
    pofn->lpfnHook = NULL; 
    pofn->lpTemplateName = NULL; 
}

/*
** Control message from erlang, syncronous operations which hang the
** emulator. This is not a place where you normally do blocking
** operations, but as the wings application is single threaded
** it doesn't matter.
*/
static int wings_file_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size)
{
    OPENFILENAME ofn;
    char *rbuff;
    char filterbuff[1024];
    char *ptr;
    char *defdir;
    char *filter;
    char *title;
    char *text;
    int ret;
    
    switch (command) {
    case 0: /* Yes/No/Cancel question */
        title = buff; /* Title of window */
	text = title + strlen(title) + 1; /* Prompt text */
	/* I can copy the answers directly into the supplied buffer, 
	   it will be large enough (64 bytes at least) */
	switch (MessageBox(GetActiveWindow(),text,title,MB_YESNOCANCEL)) {
	case IDYES:
	    strcpy(*res,"yes");
	    return 3;
	case IDNO:
	    strcpy(*res,"no");
	    return 2;
	default:
	   strcpy(*res,"aborted");
	   return 7;
	}
    case 1: /* Open (or import) file */
    case 2: /* Save (or export) file */
        defdir = buff; /* Default directory */
	filter = defdir + strlen(defdir) + 1; /* Filter expression (.wings) */
	title = filter + strlen(filter) + 1;  /* Title of dialog box */
	sprintf(filterbuff,"Requested type (*%s)",filter);
	ptr = filterbuff + strlen(filterbuff) +1;
	sprintf(ptr,"*%s",filter);
	ptr += strlen(ptr)+1;
	strcpy(ptr, "All files (*.*)");
	ptr += strlen(ptr)+1;
	strcpy(ptr,"*.*");
	ptr[strlen(ptr)+1]='\0';
	rbuff=driver_alloc(_MAX_PATH+1);
	*rbuff = '\0';
	fill_ofn(&ofn);
	ofn.lpstrFilter = filterbuff; 
        ofn.lpstrFile = rbuff; 
	ofn.nMaxFile = _MAX_PATH+1; 
	ofn.lpstrInitialDir = strlen(defdir) ? defdir : NULL; 
	ofn.lpstrTitle = title; 
	ofn.lpstrDefExt = strlen(filter) ? filter+1 : NULL; 
	if (command == 1) {
	  ofn.Flags = OFN_FILEMUSTEXIST | OFN_HIDEREADONLY; 
	  ret = GetOpenFileName(&ofn);
	} else {
	  ofn.Flags = OFN_HIDEREADONLY; 
	  ret = GetSaveFileName(&ofn);
	}
	if (ret) {
	    /* Use rbuff instead of default buffer, emulator will free it */
	    *res = rbuff;
	    return strlen(rbuff);
	}
	driver_free(rbuff); /* As it isn't passed to emulator, we have to
			       free it ourselves */
	return 0;
    default:
        return -1; /* Error return, throws exception in erlang */
    }
}



