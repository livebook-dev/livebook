!include "MUI2.nsh"
!include "WinVer.nsh"

Name "Livebook"
ManifestDPIAware true
OutFile "bin\LivebookInstall.exe"
Unicode True

; Install to user home so we have permission to write COOKIE, crash dumps, etc
InstallDir "$LOCALAPPDATA\Livebook"

; Need admin for registering URL scheme
RequestExecutionLevel admin

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
  ExecWait '"$INSTDIR\vc_redist.x64.exe" /install /quiet /norestart'
  Delete "$INSTDIR\vc_redist.x64.exe"

  File /a /r "bin\Livebook-Release\"

  CreateDirectory "$INSTDIR\Logs"
  WriteUninstaller "$INSTDIR\LivebookUninstall.exe"
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
  DeleteRegKey HKCR ".livemd"
  WriteRegStr  HKCR ".livemd" "" "Livebook.LiveMarkdown"
  DeleteRegKey HKCR "Livebook.LiveMarkdown"
  WriteRegStr  HKCR "Livebook.LiveMarkdown" "" "LiveMarkdown"
  WriteRegStr  HKCR "Livebook.LiveMarkdown\DefaultIcon" "" "$INSTDIR\Livebook.exe,1"
  WriteRegStr  HKCR "Livebook.LiveMarkdown\shell\open\command" "" '"$INSTDIR\Livebook.exe" "open:%1"'

  DetailPrint "Registering livebook URL Handler"
  DeleteRegKey HKCR "livebook"
  WriteRegStr  HKCR "livebook" "" "Livebook URL Protocol"
  WriteRegStr  HKCR "livebook" "URL Protocol" ""
  WriteRegStr  HKCR "livebook\shell" "" ""
  WriteRegStr  HKCR "livebook\shell\open" "" ""
  WriteRegStr  HKCR "livebook\shell\open\command" "" '"$INSTDIR\Livebook.exe" "open:%1"'
SectionEnd

Section "Uninstall"
  DeleteRegKey HKCR ".livemd"
  DeleteRegKey HKCR "Livebook.LiveMarkdown"
  DeleteRegKey HKCR "livebook"

  DetailPrint "Terminating Livebook..."
  ExecWait "taskkill /f /t /im Livebook.exe"
  ExecWait "taskkill /f /t /im epmd.exe"
  Sleep 1000

  Delete "$DESKTOP\Livebook.lnk"
  RMDir /r "$INSTDIR"
SectionEnd
