#
#  wings.nsi --
#
#     Install script for NSIS installer.
#
#  Copyright (c) 2002-2003 Bjorn Gustavsson
#		      2003 Patrik Nyblom
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: wings.nsi,v 1.3 2003/09/12 15:05:22 bjorng Exp $
#

	!define MUI_PRODUCT "Wings 3D"
	!define MUI_VERSION ${WINGS_VERSION}

	!include "MUI.nsh"

; General
	OutFile "../wings-${WINGS_VERSION}.exe"

; Folder selection page
	InstallDir "$PROGRAMFILES\wings3d_${WINGS_VERSION}"
; Remember install folder
	InstallDirRegKey HKLM "SOFTWARE\Wings 3D\${WINGS_VERSION}" ""

; Set the default start menu folder

	!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${MUI_PRODUCT}"

; Registry keys where start menu folder is stored
  	!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM" 
  	!define MUI_STARTMENUPAGE_REGISTRY_KEY \
		"SOFTWARE\Wings 3D\${WINGS_VERSION}"
  	!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

; Temporary variable used here and there...
  	!define TEMP $R0
  
;--------------------------------
;Modern UI Configuration
        !define MUI_ICON "install.ico"
        !define MUI_UNICON "${NSISDIR}\Contrib\Icons\normal-uninstall.ico"
	!define MUI_WELCOMEPAGE
  	!define MUI_COMPONENTSPAGE
  	!define MUI_DIRECTORYPAGE
  	!define MUI_STARTMENUPAGE
  
  	!define MUI_ABORTWARNING
  
  	!define MUI_UNINSTALLER
  	!define MUI_UNCONFIRMPAGE
	
;--------------------------------
;Languages
 
  	!insertmacro MUI_LANGUAGE "English"
;--------------------------------
;Language Strings

;Description
  	LangString DESC_SecWings ${LANG_ENGLISH} "The Wings 3D modeler"
  	LangString DESC_SecWingsBase ${LANG_ENGLISH} \
		"Basic Wings components"
  	LangString DESC_SecWingsMakeDefault ${LANG_ENGLISH} \
"Make this installation of Wings 3D the one that will be started when you double-click on a .wings file."
  	LangString DESC_SecWingsClutter ${LANG_ENGLISH} \
		"Clutter your computer with Wings 3D shortcuts."
  	LangString DESC_SecWingsClutterDesktop ${LANG_ENGLISH} \
		"Create a shortcut to Wings3D on the Desktop."
  	LangString DESC_SecWingsClutterQuicklaunch ${LANG_ENGLISH} \
       		"Create a shortcut to Wings3D in the task bar."
 
;--------------------------------
;Installer Sections

SubSection /e "Wings 3D" SecWings
Section "Base" SecWingsBase
SectionIn 1 2 3 RO

  	StrCmp ${MUI_STARTMENUPAGE_VARIABLE} "" 0 skip_silent_mode
	StrCpy ${MUI_STARTMENUPAGE_VARIABLE} \
		"${MUI_STARTMENUPAGE_DEFAULTFOLDER}"

skip_silent_mode:
  	SetOutPath "$INSTDIR"
  	File /r AUTHORS license.terms Wings3D.exe
  	SetOutPath "$INSTDIR\lib"
  	File /r lib\*.*
  	SetOutPath "$INSTDIR\bin"
  	File /r bin\*.*
  	SetOutPath "$INSTDIR"

  	WriteRegStr HKLM "SOFTWARE\Wings 3D\${WINGS_VERSION}" "" $INSTDIR

; Install Erlang/OTP.
  	ExecWait "Wings3D.exe --install"

; Create uninstaller before shortcuts
  	WriteUninstaller "$INSTDIR\Uninstall.exe"
	
; The startmenu stuff
  	!insertmacro MUI_STARTMENU_WRITE_BEGIN

; Try to use the Common startmenu...
  	SetShellVarContext All
  	ClearErrors
  	CreateDirectory "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}"
  	IfErrors 0 continue_create
    	;MessageBox MB_OK "Error creating file"
    	SetShellVarContext current
    	CreateDirectory "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}"
continue_create:
  	CreateShortCut "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}\Wings 3D ${MUI_VERSION}.lnk" \
		"$INSTDIR\Wings3D.exe"
  
  	!insertmacro MUI_STARTMENU_WRITE_END

  	WriteRegStr HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}" \
		"DisplayName" "Wings 3D ${MUI_VERSION}"
  	WriteRegStr HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}" \
		"UninstallString" "$INSTDIR\Uninstall.exe"
  	WriteRegDWORD HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}" \
		"NoModify" 1
  	WriteRegDWORD HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}" \
		"NoRepair" 1

; Check that the registry could be written, we only check one key,
; but it should be sufficient...
  	ReadRegStr ${TEMP} "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" \
		"${MUI_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"

  	StrCmp ${TEMP} "" 0 done

; Now we're done if we are a superuser. If the registry stuff failed, we 
; do the things below...

  	WriteRegStr HKCU "${MUI_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}" \
		"${MUI_STARTMENUPAGE_VARIABLE}"
  	WriteRegStr HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}" \
		"DisplayName" "Wings 3D ${MUI_VERSION}"
  	WriteRegStr HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}" \
		"UninstallString" "$INSTDIR\Uninstall.exe"
  	WriteRegDWORD HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}" \
		"NoModify" 1
  	WriteRegDWORD HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}" \
		"NoRepair" 1

done:
        CreateShortCut "$INSTDIR\plugins.lnk" "$INSTDIR\lib\wings-${WINGS_VERSION}\plugins"

  	; Delete beam files in $INSTDIR (should not be any).
  	Delete "$INSTDIR\*.beam"

  	; Delete any installed patches. Create empty patches directory.
  	Delete "$INSTDIR\lib\wings-${WINGS_VERSION}\patches\*.*"
        CreateDirectory "$INSTDIR\lib\wings-${WINGS_VERSION}\patches"
SectionEnd ; SecWingsBase

Section "Make Default" SecWingsMakeDefault
  ; Write shell extensions
  WriteRegStr HKCR ".wings" "" "Wings3DFile"
  WriteRegStr HKCR "Wings3DFile" "" "Wings 3D File"
  WriteRegStr HKCR "Wings3DFile\shell" "" "open"
  WriteRegStr HKCR "Wings3DFile\DefaultIcon" "" $INSTDIR\Wings3D.exe,1
  WriteRegStr HKCR "Wings3DFile\shell\open\command" "" '$INSTDIR\Wings3D.exe "%1"'
  WriteRegStr HKLM "SOFTWARE\Wings 3D\DefaultVersion" "" ${WINGS_VERSION}
SectionEnd  

SubSection "Shortcuts" SecWingsClutter
Section "Desktop shortcut" SecWingsClutterDesktop
  SetShellVarContext All
  ClearErrors
  CreateShortCut "$DESKTOP\Wings 3D ${WINGS_VERSION}.lnk" "$INSTDIR\Wings3D.exe"
  IfErrors 0 continue_create
  SetShellVarContext current
  CreateShortCut "$DESKTOP\Wings 3D ${WINGS_VERSION}.lnk" "$INSTDIR\Wings3D.exe"
continue_create:
SectionEnd

Section "QuickLaunch shortcut" SecWingsClutterQuickLaunch
  SetShellVarContext All
  ClearErrors
  CreateShortCut "$QUICKLAUNCH\Wings 3D ${WINGS_VERSION}.lnk" "$INSTDIR\Wings3D.exe"
  IfErrors 0 continue_create
  SetShellVarContext current
  CreateShortCut "$QUICKLAUNCH\Wings 3D ${WINGS_VERSION}.lnk" "$INSTDIR\Wings3D.exe"
continue_create:
SectionEnd
SubSectionEnd
SubSectionEnd
;Display the Finish header
	!insertmacro MUI_SECTIONS_FINISHHEADER

;--------------------------------
;Descriptions
	!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWings} $(DESC_SecWings)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsBase} $(DESC_SecWingsBase)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsMakeDefault} $(DESC_SecWingsMakeDefault)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsClutter} \
		$(DESC_SecWingsClutter)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsClutterDesktop} \
		$(DESC_SecWingsClutterDesktop)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsClutterQuicklaunch} \
		$(DESC_SecWingsClutterQuicklaunch)
	!insertmacro MUI_FUNCTIONS_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section



; begin uninstall settings/section
;UninstallText "This will uninstall Wings 3D from your system"

Section Uninstall
  Delete "$INSTDIR\AUTHORS"
  Delete "$INSTDIR\license.terms"
  Delete "$INSTDIR\Wings3D.exe"
  SetShellVarContext All
  ;MessageBox MB_OK "$DESKTOP\Wings 3D ${WINGS_VERSION}.lnk"
  Delete "$DESKTOP\Wings 3D ${WINGS_VERSION}.lnk"
  Delete "$QUICKLAUNCH\Wings 3D ${WINGS_VERSION}.lnk"
  SetShellVarContext current
  Delete "$DESKTOP\Wings 3D ${WINGS_VERSION}.lnk"
  Delete "$QUICKLAUNCH\Wings 3D ${WINGS_VERSION}.lnk"
  RMDir /r "$INSTDIR\lib"
  RMDir /r "$INSTDIR\bin"
  Delete "$INSTDIR\plugins.lnk"
  Delete "$INSTDIR\Uninstall.exe"

;Remove shortcut
  	ReadRegStr ${TEMP} "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" \
		"${MUI_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"
	StrCmp ${TEMP} "" 0 end_try
; Try HKCU instead...
  	ReadRegStr ${TEMP} HKCU \
		"${MUI_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"
; If this failed to, we have no shortcuts (eh?)
  	StrCmp ${TEMP} "" noshortcuts
end_try:
  	SetShellVarContext All
  	ClearErrors
; If we cannot find the shortcut, switch to current user context
  	GetFileTime "$SMPROGRAMS\${TEMP}\Wings 3D ${MUI_VERSION}.lnk" $R1 $R2
  	IfErrors 0 continue_delete
    	;MessageBox MB_OK "Error removing file"
    	SetShellVarContext current
continue_delete:
  	Delete "$SMPROGRAMS\${TEMP}\Wings 3D ${MUI_VERSION}.lnk"
  	RMDir "$SMPROGRAMS\${TEMP}" ;Only if empty

noshortcuts:
; We delete both in HKCU and HKLM, we don't really know were they might be...
  	DeleteRegKey /ifempty HKLM "SOFTWARE\Wings 3D\${WINGS_VERSION}"
  	DeleteRegKey /ifempty HKCU "SOFTWARE\Wings 3D\${WINGS_VERSION}"
  	DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}"
  	DeleteRegKey HKCU "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${MUI_VERSION}"

  	RMDir "$INSTDIR"

  	ReadRegStr ${TEMP} HKLM "SOFTWARE\Wings 3D\DefaultVersion" ""

  	StrCmp ${TEMP} "${WINGS_VERSION}" 0 done
	;MessageBox MB_OK ${TEMP}
  	DeleteRegKey HKCR ".wings"
  	DeleteRegKey HKCR "Wings3DFile"
	DeleteRegKey HKLM "SOFTWARE\Wings 3D\DefaultVersion"

done:
	;MessageBox MB_OK ${TEMP}

;Display the Finish header
  	!insertmacro MUI_UNFINISHHEADER

SectionEnd ; end of uninstall section

Function .onInit
;; Turn off all clutter options by default.

SectionGetFlags ${SecWingsClutterQuickLaunch} $0
IntOp $0 $0 & ~1
SectionSetFlags ${SecWingsClutterQuickLaunch} $0

SectionGetFlags ${SecWingsClutterDesktop} $0
IntOp $0 $0 & ~1
SectionSetFlags ${SecWingsClutterDesktop} $0

;ReadRegStr ${TEMP} HKLM "SOFTWARE\Wings 3D\DefaultVersion" ""
;StrCmp ${TEMP} "" 0 disable

;SectionGetFlags ${SecWingsMakeDefault} $0
;IntOp $0 $0 | 16
;SectionSetFlags ${SecWingsMakeDefault} $0
;Goto done

;disable:
;SectionGetFlags ${SecWingsMakeDefault} $0
;IntOp $0 $0 & ~1
;SectionSetFlags ${SecWingsMakeDefault} $0

;done:
FunctionEnd 
; eof
