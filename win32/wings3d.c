/*
 *  wings3d.c --
 *
 *     Wrapper to start Wings3D on Windows.
 *
 *  Copyright (c) 2002 Bjorn Gustavsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings3d.c,v 1.1 2002/11/21 08:34:49 bjorng Exp $
 *
 */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <winreg.h>

#include <stdlib.h>

int
WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR szCmdLine, int sw)
{
    PROCESS_INFORMATION piProcInfo;
    STARTUPINFO siStartInfo = {0};
    int argc = __argc;
    char** argv = __argv;
    char install_dir[1024];
    DWORD install_dir_size = sizeof(install_dir);
    char werl_path[1024];
    DWORD werl_path_size = sizeof(install_dir);
    char cmd_line[4096];
    int ok;
    int err;
    HKEY hkey;
    DWORD type;

    err = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software\\Wings 3D", 0,
		      KEY_QUERY_VALUE, &hkey);
    if (err) {
	MessageBox(NULL, "No \"HKLM\\Software\\Wings 3D\" key in registry",
		   NULL, MB_OK);
	exit(0);
    }
    err = RegQueryValueEx(hkey, "", NULL, &type,
			  install_dir, &install_dir_size);
    if (err) {
	MessageBox(NULL, "Failed to find install directory in registry",
		   NULL, MB_OK);
	exit(0);
    }
    err = RegQueryValueEx(hkey, "WerlPath", NULL, &type,
			  werl_path, &werl_path_size);
    if (err) {
	MessageBox(NULL, "Failed to find werl path in registry",
		   NULL, MB_OK);
	exit(0);
    }
    RegCloseKey(hkey);

    sprintf(cmd_line, "\"%s\" -regkey Wings3D -pa \"%s\\ebin\" "
	    "-run wings_start start_halt",
	    werl_path, install_dir);
    if (argc > 1) {
	sprintf(cmd_line+strlen(cmd_line), " \"%s\"", argv[1]);
    }
    
    siStartInfo.cb = sizeof(STARTUPINFO); 
    siStartInfo.wShowWindow = SW_MINIMIZE;
    siStartInfo.dwFlags = STARTF_USESHOWWINDOW;

    ok = CreateProcess(NULL, 
		       cmd_line, 
		       NULL, 
		       NULL, 
		       FALSE,
		       0,
		       NULL,
		       NULL,
		       &siStartInfo,
		       &piProcInfo);
    if (!ok) {
	MessageBox(NULL, "Failed to start Wings 3D", NULL, MB_OK);
    }
    exit(0);
}
