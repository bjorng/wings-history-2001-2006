#
#  wings2.nsi --
#
#     Install script for NSIS installer.
#
#  Copyright (c) 2002 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: wings2.nsi,v 1.2 2002/08/14 06:15:21 bjorng Exp $
#

Name "Wings 3D"
OutFile "../wings-${WINGS_VERSION}.exe"

; Some default compiler settings (uncomment and change at will):
; SetCompress auto ; (can be off or force)
; SetDatablockOptimize on ; (can be off)
; CRCCheck on ; (can be off)
; AutoCloseWindow false ; (can be true for the window go away automatically at end)
; ShowInstDetails hide ; (can be show to have them shown, or nevershow to disable)
; SetDateSave off ; (can be on to have files restored to their orginal date)

InstallDir "$PROGRAMFILES\wings3d"
InstallDirRegKey HKLM "SOFTWARE\Wings 3D" ""
DirShow show ; (make this hide to not let the user change it)
DirText "Select the directory to install Wings 3D in:"

Function GetStartMenu
  Push $0
  ReadRegStr $0 HKLM "Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" \
   "Common Programs"
  StrCmp $0 "" 0 get_start_menu_done
  StrCpy $0 "$SMPROGRAMS"

get_start_menu_done:
  Exch $0
FunctionEnd

Function un.GetStartMenu
  Push $0
  ReadRegStr $0 HKLM "Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" \
   "Common Programs"
  StrCmp $0 "" 0 get_start_menu_done
  StrCpy $0 "$SMPROGRAMS"

get_start_menu_done:
  Exch $0
FunctionEnd

Section "ThisNameIsIgnoredSoWhyBother?"
  SetOutPath "$INSTDIR"
  File /r AUTHORS license.terms
  SetOutPath "$INSTDIR\plugins"
  File /r plugins\*.*
  SetOutPath "$INSTDIR\ebin"
  File /r ebin\*.*
  SetOutPath "$INSTDIR\priv"
  File /r priv\*.*
  SetOutPath "$INSTDIR\erlang"
  File /r erlang\*.*
  SetOutPath "$INSTDIR"

  WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\Wings 3D" "" "$INSTDIR"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D" "DisplayName" "Wings 3D (remove only)"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D" "UninstallString" '"$INSTDIR\uninst.exe"'

  WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\Ericsson\Erlang\Wings3D" "BinDir" "$INSTDIR\erlang\bin"
  WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\Ericsson\Erlang\Wings3D" "Rootdir" "$INSTDIR\erlang"
  WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\Ericsson\Erlang\Wings3D" "Progname" "erl"

  ; Write out uninstaller.
  WriteUninstaller "$INSTDIR\uninst.exe"

  ; Delete old garbage.
  Delete "$INSTDIR\unwise.exe"
  Delete "$INSTDIR\install.log"
  Delete "$INSTDIR\wings_crasch.dump" ; Old spelling
  RMDir "$INSTDIR\icons"
  Delete "$INSTDIR\Makefile"
  Delete "$INSTDIR\Makefile.win32"
  Delete "$INSTDIR\README"
  Delete "$INSTDIR\plugins\primitives\wpc_plane.beam"

  ; create shortcuts
  Call GetStartMenu
  Pop $3

  CreateDirectory "$3\Wings 3D"
  GetFullPathName /short $1 $INSTDIR
  ClearErrors
  CreateShortCut "$3\Wings 3D\Wings 3D.lnk" "$INSTDIR\erlang\bin\werl.exe" \
    "-regkey Wings3D -pa $1\ebin -run wings_start start_halt"  "$INSTDIR\ebin\wings.icon" \
    0 SW_SHOWMINIMIZED
  CreateShortCut "$DESKTOP\Wings 3D.lnk" "$INSTDIR\erlang\bin\werl.exe" \
    "-regkey Wings3D -pa $1\ebin -run wings_start start_halt"  "$INSTDIR\ebin\wings.icon" \
    0 SW_SHOWMINIMIZED
  IfErrors "" shortcut_created

  CreateDirectory "$STARTMENU\Programs\Wings 3D"
  CreateShortCut "$STARTMENU\Programs\Wings 3D\Wings 3D.lnk" "$0\bin\werl.exe" \
    "-pa $1\ebin -run wings_start start_halt"  "$INSTDIR\ebin\wings.icon" \
    0 SW_SHOWMINIMIZED

shortcut_created:

  ; Delete any installed patches. Create empty patches directory.
  Delete "$INSTDIR\patches\*.*"
  CreateDirectory "$INSTDIR\patches"

  ; Delete beam files in $INSTDIR.
  Delete "$INSTDIR\*.beam"
  
  ; Write shell extensions
  WriteRegStr HKCR ".wings" "" "Wings3DFile"
  WriteRegStr HKCR "Wings3DFile" "" "Wings 3D File"
  WriteRegStr HKCR "Wings3DFile\shell" "" "open"
  WriteRegStr HKCR "Wings3DFile\DefaultIcon" "" $INSTDIR\ebin\wings.icon,0
  WriteRegStr HKCR "Wings3DFile\shell\open\command" "" '$1\wings_start.bat "%1"'

  ; Write batch file to start Wings
  FileOpen $4 "$INSTDIR\wings_start.bat" w
  FileWrite $4 '@start /min $0\bin\werl.exe -pa $1\ebin -run wings_start start_halt %1'
  FileClose $4
  
SectionEnd ; end of default section


; begin uninstall settings/section
UninstallText "This will uninstall Wings 3D from your system"

Section Uninstall
  Delete "$INSTDIR\AUTHORS"
  Delete "$INSTDIR\vsn.mk"
  Delete "$INSTDIR\license.terms"
  RMDir /r "$INSTDIR\e3d"
  RMDir /r "$INSTDIR\src"
  RMDir /r "$INSTDIR\plugins_src"
  RMDir /r "$INSTDIR\plugins"
  RMDir /r "$INSTDIR\ebin"
  RMDir /r "$INSTDIR\priv"
  RMDir /r "$INSTDIR\erlang"

  Delete "$INSTDIR\uninst.exe"

  Call un.GetStartMenu
  Pop $0

Delete "$0\Wings 3D\Wings 3D.lnk"
Delete "$0\Wings 3D\Wings 3D Uninstall.lnk"
RMDir "$0\Wings 3D"

Delete "$STARTMENU\Programs\Wings 3D\Wings 3D.lnk"
RMDir "$STARTMENU\Programs\Wings 3D"

DeleteRegKey HKLM "SOFTWARE\Wings 3D"
DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D"
DeleteRegKey HKLM "SOFTWARE\Ericsson\Erlang\Wings3D"

Delete "$INSTDIR\wings_start.bat"
Delete "$INSTDIR\patches\*.*"
RMDir /r "$INSTDIR\patches"
RMDir "$INSTDIR"

DeleteRegKey HKCR ".wings"
DeleteRegKey HKCR "Wings3DFile"
SectionEnd ; end of uninstall section

; eof
