#
#  wings.nsi --
#
#     Install script for NSIS installer.
#
#  Copyright (c) 2002 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: wings.nsi,v 1.6 2002/05/13 10:48:19 bjorng Exp $
#

Name "Wings 3D"
OutFile "wings-${WINGS_VERSION}.exe"

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

; $9 = counter
; $8 = DLL
; $7 = ini
Function .onInit
  StrCpy $9 0
  GetTempFileName $8
  GetTempFileName $7
  File /oname=$8 InstallOptions.dll
  File /oname=$7 "otp.ini"
FunctionEnd

; cleanup on exit.
Function .onInstSuccess
Call Cleanup
FunctionEnd

Function .onInstFailed
Call Cleanup
FunctionEnd

Function .onUserAbort
Call Cleanup
FunctionEnd

Function Cleanup
  Delete $8
  Delete $7
FunctionEnd

Function RunDialog
  Push $7
  CallInstDLL $8 dialog
FunctionEnd

;------------------------------------------------------------------------------
; GetErlangInstPath
; 
; takes no parameters
; returns with the Erlang/OTP install directory on the stack (it will be
; an empty string if Erlang/OTP is not detected).
;
; modifies no other variables
;
; Usage:
;   Call GetErlangInstPath
;   Pop $0
;   MessageBox MB_OK "Erlang installed at: $0"

Function GetErlangInstPath
  Push $0
  Push $1
  Push $2
  ReadRegStr $0 HKLM \
     "Software\Microsoft\Windows\CurrentVersion\Uninstall\Erlang OTP R8B" \ 
     "UninstallString"
  StrCmp $0 "" fin

    StrCpy $1 $0 1 0 ; get firstchar
    StrCmp $1 '"' "" find_space
      ; if first char is ", let's remove "'s first.
      StrCpy $0 $0 "" 1
      StrCpy $1 0
      rqloop:
        StrCpy $2 $0 1 $1
        StrCmp $2 '"' rqdone
        StrCmp $2 "" rqdone
        IntOp $1 $1 + 1
        Goto rqloop
      rqdone:
      StrCpy $0 $0 $1
      goto getparent

    find_space:
      StrCpy $1 0
      fsloop:
        StrCpy $2 $0 1 $1
        StrCmp $2 ' ' fsdone
        StrCmp $2 "" fsdone
        IntOp $1 $1 + 1
        Goto fsloop
      fsdone:
      StrCpy $0 $0 $1

    getparent:
    ; the uninstall string goes to an EXE, let's get the directory.
    StrCpy $1 -1
    gploop:
      StrCpy $2 $0 1 $1
      StrCmp $2 "" gpexit
      StrCmp $2 "\" gpexit
      IntOp $1 $1 - 1
      Goto gploop
    gpexit:
    StrCpy $0 $0 $1

    StrCmp $0 "" fin
    IfFileExists $0\bin\werl.exe fin
      StrCpy $0 ""
  fin:
  Pop $2
  Pop $1
  Exch $0
FunctionEnd

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
  Call GetErlangInstPath
  Pop $0
  StrCmp $0 "" 0 continue_1

  MessageBox MB_OK "Unable to automatically find install Erlang/OTP R8B."

dialog:
  call RunDialog
  Pop $0
  StrCmp $0 "cancel" "" nocancel
    Call Cleanup
    Quit

  nocancel:

  ReadINIStr $0 $7 "Field 2" State
  IfFileExists $0\bin\werl.exe continue_1

  MessageBox MB_OK "The chosen directory doesn't seem to contain an Erlang/OTP installation."

  goto dialog

continue_1:
  SetOutPath "$INSTDIR"
  File /r AUTHORS vsn.mk license.terms
  SetOutPath "$INSTDIR\e3d"
  File /r e3d\*.*
  SetOutPath "$INSTDIR\src"
  File /r src\*.*
  SetOutPath "$INSTDIR\plugins_src"
  File /r plugins_src\*.*
  SetOutPath "$INSTDIR\plugins"
  File /r plugins\*.*
  SetOutPath "$INSTDIR\ebin"
  File /r ebin\*.*
  File /r ..\esdl\ebin\*.*
  SetOutPath "$INSTDIR\priv"
  File ..\esdl\priv\SDL.dll ..\esdl\priv\sdl_driver.dll
  SetOutPath "$INSTDIR"

  WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\Wings 3D" "" "$INSTDIR"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D" "DisplayName" "Wings 3D (remove only)"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D" "UninstallString" '"$INSTDIR\uninst.exe"'

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
  CreateShortCut "$3\Wings 3D\Wings 3D.lnk" "$0\bin\werl.exe" \
    "-pa $1\ebin -run wings_start start_halt"  "$INSTDIR\ebin\wings.icon" \
    0 SW_SHOWMINIMIZED

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


  Delete "$INSTDIR\uninst.exe"

  Call un.GetStartMenu
  Pop $0

Delete "$0\Wings 3D\Wings 3D.lnk"
Delete "$0\Wings 3D\Wings 3D Uninstall.lnk"
RMDir "$0\Wings 3D"
DeleteRegKey HKLM "SOFTWARE\Wings 3D"
DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D"
Delete "$INSTDIR\wings_start.bat"
Delete "$INSTDIR\patches\*.*"
RMDir /r "$INSTDIR\patches"
RMDir "$INSTDIR"

DeleteRegKey HKCR ".wings"
DeleteRegKey HKCR "Wings3DFile"
SectionEnd ; end of uninstall section

; eof
