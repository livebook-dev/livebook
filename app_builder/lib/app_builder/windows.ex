defmodule AppBuilder.Windows do
  @moduledoc false

  import AppBuilder.Utils
  require EEx

  @doc """
  Creates a Windows installer.
  """
  def build_windows_installer(release, options) do
    tmp_dir = release.path <> "_tmp"
    File.rm_rf(tmp_dir)
    File.mkdir_p!(tmp_dir)

    File.cp_r!(release.path, Path.join(tmp_dir, "rel"))

    options =
      Keyword.validate!(options, [
        :name,
        :version,
        :url_schemes,
        :document_types,
        :logo_path,
        :module
      ])

    app_name = Keyword.fetch!(options, :name)

    logo_path = options[:logo_path] || Application.app_dir(:wx, "examples/demo/erlang.png")
    app_icon_path = Path.join(tmp_dir, "app_icon.ico")
    copy_image(logo_path, app_icon_path)

    erts_dir = Path.join([tmp_dir, "rel", "erts-#{:erlang.system_info(:version)}"])
    rcedit_path = ensure_rcedit()
    cmd!(rcedit_path, ["--set-icon", app_icon_path, Path.join([erts_dir, "bin", "erl.exe"])])

    File.write!(Path.join(tmp_dir, "#{app_name}.vbs"), launcher_vbs(release, options))
    nsi_path = Path.join(tmp_dir, "#{app_name}.nsi")
    File.write!(nsi_path, nsi(options))
    makensis_path = ensure_makensis()
    cmd!(makensis_path, [nsi_path])

    File.rename!(
      Path.join(tmp_dir, "#{app_name}Install.exe"),
      Path.join([Mix.Project.build_path(), "rel", "#{app_name}Install.exe"])
    )

    release
  end

  code = """
  <%
  app_name = Keyword.fetch!(options, :name)
  url_schemes = Keyword.get(options, :url_schemes, [])
  %>
  !include "MUI2.nsh"

  ;--------------------------------
  ;General

  Name "<%= app_name %>"
  OutFile "<%= app_name %>Install.exe"
  Unicode True
  InstallDir "$LOCALAPPDATA\\<%= app_name %>"

  ; Need admin for registering URL scheme
  RequestExecutionLevel admin

  ;--------------------------------
  ;Interface Settings

  !define MUI_ABORTWARNING

  ;--------------------------------
  ;Pages

  ;!insertmacro MUI_PAGE_COMPONENTS
  !define MUI_ICON "app_icon.ico"
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

  ;--------------------------------
  ;Languages

  !insertmacro MUI_LANGUAGE "English"

  ;--------------------------------
  ;Installer Sections

  Section "Install"
    SetOutPath "$INSTDIR"
    File /r rel rel
    File "<%= app_name %>.vbs"
    File "app_icon.ico"
    CreateDirectory "$INSTDIR\\Logs"
    WriteUninstaller "$INSTDIR\\<%= app_name %>Uninstall.exe"

  <%= for type <- Keyword.get(options, :document_types, []) do %>
  <%= for ext <- type.extensions do %>
    WriteRegStr HKCR ".<%= ext %>" "" "<%= app_name %>.<%= type.name %>"
  <% end %>
    WriteRegStr HKCR "<%= app_name %>.<%= type.name %>" "" "<%= type.name %>"
    WriteRegStr HKCR "<%= app_name %>.<%= type.name %>\\DefaultIcon" "" "$INSTDIR\\app_icon.ico"
    WriteRegStr HKCR "<%= app_name %>.<%= type.name %>\\shell\\open\\command" "" '$WINDIR\\system32\\wscript.exe "$INSTDIR\\<%= app_name %>.vbs" "%1"'
  <% end %>

  <%= for url_scheme <- url_schemes do %>
    DetailPrint "Register <%= url_scheme %> URL Handler"
    DeleteRegKey HKCR "<%= url_scheme %>"
    WriteRegStr  HKCR "<%= url_scheme %>" "" "<%= url_scheme %> Protocol"
    WriteRegStr  HKCR "<%= url_scheme %>" "URL Protocol" ""
    WriteRegStr  HKCR "<%= url_scheme %>\\shell" "" ""
    WriteRegStr  HKCR "<%= url_scheme %>\\shell\\open" "" ""
    WriteRegStr  HKCR "<%= url_scheme %>\\shell\\open\\command" "" '$WINDIR\\system32\\wscript.exe "$INSTDIR\\<%= app_name %>.vbs" "%1"'
  <% end %>
  SectionEnd

  Section "Desktop Shortcut"
    CreateShortCut "$DESKTOP\\<%= app_name %>.lnk" "$INSTDIR\\<%= app_name %>.vbs" "" "$INSTDIR\\app_icon.ico"
  SectionEnd

  Section "Uninstall"
    Delete "$DESKTOP\\<%= app_name %>.lnk"
    ; TODO: stop epmd if it was started
    RMDir /r "$INSTDIR"
  SectionEnd
  """

  EEx.function_from_string(:defp, :nsi, code, [:options], trim: true)

  code = ~S"""
  <%
  app_name = Keyword.fetch!(options, :name)
  module = Keyword.fetch!(options, :module)
  %>
  ' This vbs script avoids a flashing cmd window when launching the release bat file

  path = Left(Wscript.ScriptFullName, Len(Wscript.ScriptFullName) - Len(Wscript.ScriptName)) & "rel\bin\<%= release.name %>.bat"

  If WScript.Arguments.Count > 0 Then
    url = WScript.Arguments(0)
  Else
    url = ""
  End If

  Set shell = CreateObject("WScript.Shell")

  ' Below we run two commands:
  '
  '   1. bin/release rpc
  '   2. bin/release start
  '
  ' The first one will only succeed when the app is already running. The second one when it is not.
  ' It's ok for either to fail because we run them asynchronously.

  Set env = shell.Environment("Process")
  env("PATH") = ".\rel\erts-<%= :erlang.system_info(:version) %>\bin;" & env("PATH")

  ' > bin/release rpc "mod.windows_connected(url)"
  '
  ' We send the URL through IO, as opposed through the rpc expression, to avoid RCE.
  cmd = "echo """ & url & """ | """ & path & """ rpc <%= inspect(module) %>.windows_connected(IO.read(:line))"
  code = shell.Run("cmd /c " & cmd, 0)

  ' > bin/release start
  cmd = """" & path & """ start"
  env("<%= String.upcase(app_name) <> "_URL" %>") = url
  code = shell.Run("cmd /c " & cmd & " >> .\Logs\<%= app_name %>.log 2>&1", 0)
  """

  EEx.function_from_string(:defp, :launcher_vbs, code, [:release, :options], trim: true)

  defp ensure_makensis do
    url = "https://downloads.sourceforge.net/project/nsis/NSIS%203/3.08/nsis-3.08.zip"
    sha256 = "1bb9fc85ee5b220d3869325dbb9d191dfe6537070f641c30fbb275c97051fd0c"
    AppBuilder.Utils.ensure_executable(url, sha256, "nsis-3.08/makensis.exe")
  end

  defp ensure_rcedit do
    url = "https://github.com/electron/rcedit/releases/download/v1.1.1/rcedit-x64.exe"
    sha256 = "02e8e8c5d430d8b768980f517b62d7792d690982b9ba0f7e04163cbc1a6e7915"
    AppBuilder.Utils.ensure_executable(url, sha256)
  end

  defp ensure_magick do
    url =
      "https://download.imagemagick.org/ImageMagick/download/binaries/ImageMagick-7.1.0-portable-Q16-x64.zip"

    sha256 = "6c0a104527d93c73da2e93c050795c94d711620b9ca396d7ace1ed1adc94f4b3"
    AppBuilder.Utils.ensure_executable(url, sha256, "magick.exe")
  end

  defp copy_image(src_path, dest_path) do
    if Path.extname(src_path) == ".ico" do
      File.cp!(src_path, dest_path)
    else
      magick_path = ensure_magick()
      sizes = [16, 32, 48, 64, 128]

      for i <- sizes do
        cmd!(magick_path, [src_path, "-resize", "#{i}x#{i}", sized_path(dest_path, i)])
      end

      sized_paths = Enum.map(sizes, &sized_path(dest_path, &1))
      cmd!(magick_path, sized_paths ++ [dest_path])
    end
  end

  defp sized_path(path, size) do
    String.replace_trailing(path, ".ico", ".#{size}.ico")
  end
end
