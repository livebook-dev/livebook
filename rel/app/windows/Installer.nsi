!include "MUI2.nsh"
!include "WinVer.nsh"

Name "Livebook"
ManifestDPIAware true
OutFile "bin\LivebookInstall.exe"
Unicode True

; Install to user home so we have permission to write COOKIE, crash dumps, etc
InstallDir "$LOCALAPPDATA\Livebook"

; Need admin for registering URL scheme
RequestExecutionLevel user

!define MUI_ABORTWARNING
!define MUI_ICON "Resources\AppIcon.ico"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Function .onInit
${IfNot} ${AtLeastWin10}
  MessageBox mb_iconStop "It is recommended to run Livebook on Windows 10+"
${EndIf}
FunctionEnd

Section "Install"
  SetOutPath "$INSTDIR"

  File "bin\vc_redist.x64.exe"

  ; Check if the redistributable is installed
  ReadRegDWORD $0 HKLM "SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64" "Installed"
  ${If} $0 == 1
      DetailPrint "Visual C++ Redistributable is already installed. Skipping installation."
  ${Else}
      DetailPrint "Visual C++ Redistributable is not installed. Installing now..."
      ExecWait '"$INSTDIR\vc_redist.x64.exe" /install /quiet /norestart'
  ${EndIf}
  Delete "$INSTDIR\vc_redist.x64.exe"

  DetailPrint "Stopping epmd.exe from previous installation, if any"
  ExecWait "taskkill /f /t /im epmd.exe"

  File /a /r "bin\Livebook-Release\"

  CreateDirectory "$LOCALAPPDATA\Livebook\Logs"
  WriteUninstaller "$INSTDIR\LivebookUninstall.exe"

  WriteRegStr   HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Livebook" "DisplayName" "Livebook"
  WriteRegStr   HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Livebook" "DisplayVersion" "${LIVEBOOK_VERSION}"
  WriteRegStr   HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Livebook" "DisplayIcon" "$INSTDIR\Livebook.exe"
  WriteRegStr   HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Livebook" "Publisher" "Dashbit"
  WriteRegStr   HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Livebook" "UninstallString" '"$INSTDIR\LivebookUninstall.exe"'
  WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Livebook" "NoModify" 1
  WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Livebook" "NoRepair" 1

  WriteRegStr   HKCU "Software\Dashbit\Livebook" "InstallRoot" "$INSTDIR"
SectionEnd

Section "Desktop Shortcut"
  CreateShortCut "$DESKTOP\Livebook.lnk" "$INSTDIR\Livebook.exe" ""
SectionEnd

Section "Check"
  DetailPrint "Checking Erlang..."
  ; we use otp\erts-:vsn\bin\erl.exe instead of otp\bin\erl.exe because the latter for some reason
  ; hardcoded the path: c:\otp\erts-:vsn\bin\erlexec.dll. The Elixir releases uses the former
  ; anyway.
  nsExec::ExecToLog '"$INSTDIR\rel\vendor\otp\erts-${ERTS_VERSION}\bin\erl.exe" -noinput -eval "erlang:display(ok), halt()."'
  Pop $0
  ${If} $0 != 0
    MessageBox mb_iconStop "Checking Erlang failed: $0. Please click 'Show details' and report an issue."
    Abort
  ${EndIf}

  DetailPrint "Checking Distributed Erlang..."
  nsExec::ExecToLog '"$INSTDIR\rel\vendor\otp\erts-${ERTS_VERSION}\bin\erl.exe" -sname "livebook-install-test" -noinput -eval "erlang:display(ok), halt()."'
  Pop $0
  ${If} $0 != 0
    MessageBox mb_iconStop "Checking Distributed Erlang failed: $0. Please click 'Show details' and report an issue."
    Abort
  ${EndIf}
SectionEnd

Section "Install Handlers"
  DetailPrint "Registering .livemd File Handler"
  DeleteRegKey HKCU "Software\Classes\.livemd"
  WriteRegStr  HKCU "Software\Classes\.livemd" "" "Livebook.LiveMarkdown"
  DeleteRegKey HKCU "Software\Classes\Livebook.LiveMarkdown"
  WriteRegStr  HKCU "Software\Classes\Livebook.LiveMarkdown" "" "LiveMarkdown"
  WriteRegStr  HKCU "Software\Classes\Livebook.LiveMarkdown\DefaultIcon" "" "$INSTDIR\Livebook.exe,1"
  WriteRegStr  HKCU "Software\Classes\Livebook.LiveMarkdown\shell\open\command" "" '"$INSTDIR\Livebook.exe" "open:%1"'

  DetailPrint "Registering livebook URL Handler"
  DeleteRegKey HKCU "Software\Classes\livebook"
  WriteRegStr  HKCU "Software\Classes\livebook" "" "Livebook URL Protocol"
  WriteRegStr  HKCU "Software\Classes\livebook" "URL Protocol" ""
  WriteRegStr  HKCU "Software\Classes\livebook\shell" "" ""
  WriteRegStr  HKCU "Software\Classes\livebook\shell\open" "" ""
  WriteRegStr  HKCU "Software\Classes\livebook\shell\open\command" "" '"$INSTDIR\Livebook.exe" "open:%1"'
SectionEnd

Section "Uninstall"
  DeleteRegKey HKCU "Software\Classes\.livemd"
  DeleteRegKey HKCU "Software\Classes\Livebook.LiveMarkdown"
  DeleteRegKey HKCU "Software\Classes\livebook"
  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Livebook"
  DeleteRegKey HKCU "Software\Dashbit\Livebook"
  DeleteRegKey /ifempty HKCU "Software\Dashbit"

  DetailPrint "Terminating Livebook..."
  ExecWait "taskkill /f /t /im Livebook.exe"
  ExecWait "taskkill /f /t /im epmd.exe"
  Sleep 1000

  Delete "$DESKTOP\Livebook.lnk"
  RMDir /r "$INSTDIR"
SectionEnd
