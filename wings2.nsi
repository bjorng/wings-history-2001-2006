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
#     $Id: wings2.nsi,v 1.6 2002/11/21 08:34:49 bjorng Exp $
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

Section "ThisNameIsIgnoredSoWhyBother?"
  SetOutPath "$INSTDIR"
  File /r AUTHORS license.terms vsn.mk Wings3D.exe
  SetOutPath "$INSTDIR\plugins"
  File /r plugins\*.*
  SetOutPath "$INSTDIR\ebin"
  File /r ebin\*.*
  SetOutPath "$INSTDIR\priv"
  File /r priv\*.*
  SetOutPath "$INSTDIR\erlang"
  File /r erlang\*.*
  SetOutPath "$INSTDIR"

  WriteRegStr HKLM "SOFTWARE\Wings 3D" "" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D" "DisplayName" "Wings 3D (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D" "UninstallString" '"$INSTDIR\uninst.exe"'

  WriteRegStr HKLM "SOFTWARE\Ericsson\Erlang\Wings3D" "BinDir" "$INSTDIR\erlang\bin"
  WriteRegStr HKLM "SOFTWARE\Ericsson\Erlang\Wings3D" "Rootdir" "$INSTDIR\erlang"
  WriteRegStr HKLM "SOFTWARE\Ericsson\Erlang\Wings3D" "Progname" "erl"

  WriteRegStr HKLM "SOFTWARE\Wings 3D" "WerlPath" "$INSTDIR\erlang\bin\werl.exe"

  ; Write out uninstaller.
  WriteUninstaller "$INSTDIR\uninst.exe"

  ; Delete old garbage.
  Delete "$INSTDIR\Wings 3D.lnk"
  Delete "$INSTDIR\wings_start.bat"
  Delete "$INSTDIR\unwise.exe"
  Delete "$INSTDIR\install.log"
  Delete "$INSTDIR\wings_crasch.dump" ; Old spelling
  RMDir "$INSTDIR\icons"
  Delete "$INSTDIR\Makefile"
  Delete "$INSTDIR\Makefile.win32"
  Delete "$INSTDIR\README"
  Delete "$INSTDIR\plugins\primitives\wpc_plane.beam"
  DeleteRegKey HKLM "SOFTWARE\Wings3D"

  ; Some releases have all command plug-ins in the import_export directory.
  Delete "$INSTDIR\plugins\import_export\wpc_region.beam"
  Delete "$INSTDIR\plugins\import_export\wpc_triquad_cmd.beam"
  Delete "$INSTDIR\plugins\import_export\wpc_turnedge.beam"
  Delete "$INSTDIR\plugins\import_export\wpc_tweak.beam"

  ; create shortcuts

  CreateShortCut "$DESKTOP\Wings 3D.lnk" "$INSTDIR\Wings3D.exe" "" \
	"$INSTDIR\ebin\wings.icon"
  CreateShortCut "$QUICKLAUNCH\Wings 3D.lnk" "$INSTDIR\Wings3D.exe" "" \
	"$INSTDIR\ebin\wings.icon"

  SetShellVarContext all
  CreateDirectory "$SMPROGRAMS\Wings 3D"
  GetFullPathName /short $1 $INSTDIR
  ClearErrors
  CreateShortCut "$SMPROGRAMS\Wings 3D\Wings 3D.lnk" \
           "$INSTDIR\Wings3D.exe" "" "$INSTDIR\ebin\wings.icon"
  IfErrors "" shortcut_created

  SetShellVarContext current
  CreateDirectory "$SMPROGRAMS\Wings 3D"
  CreateShortCut "$SMPROGRAMS\Wings 3D\Wings 3D.lnk" \
	"$INSTDIR\Wings3D.exe" "" "$INSTDIR\ebin\wings.icon"

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
  WriteRegStr HKCR "Wings3DFile\shell\open\command" "" '$1\Wings3D.exe "%1"'
  
SectionEnd ; end of default section


; begin uninstall settings/section
UninstallText "This will uninstall Wings 3D from your system"

Section Uninstall
  Delete "$INSTDIR\AUTHORS"
  Delete "$INSTDIR\vsn.mk"
  Delete "$INSTDIR\license.terms"
  Delete "$INSTDIR\Wings3D.exe"
  Delete "$DESKTOP\Wings 3D.lnk"
  Delete "$QUICKLAUNCH\Wings 3D.lnk"
  RMDir /r "$INSTDIR\e3d"
  RMDir /r "$INSTDIR\src"
  RMDir /r "$INSTDIR\plugins_src"
  RMDir /r "$INSTDIR\plugins"
  RMDir /r "$INSTDIR\ebin"
  RMDir /r "$INSTDIR\priv"
  RMDir /r "$INSTDIR\erlang"

  Delete "$INSTDIR\uninst.exe"
  DeleteRegValue HKLM "SOFTWARE\Wings 3D" "WerlPath"

  SetShellVarContext all

  Delete "$SMPROGRAMS\Wings 3D\Wings 3D.lnk"
  RMDir "$SMPROGRAMS\Wings 3D"

  SetShellVarContext current

  Delete "$SMPROGRAMS\Wings 3D\Wings 3D.lnk"
  RMDir "$SMPROGRAMS\Wings 3D"

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
