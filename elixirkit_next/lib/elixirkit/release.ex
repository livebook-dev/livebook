defmodule ElixirKit.Release do
  @moduledoc """
  Functions for working with Elixir releases.
  """

  @doc ~S"""
  Signs the release for distribution.

  Currently, this function is only executed on macOS.

  It expects the following environment variables:

    * `APPLE_SIGNING_IDENTITY` - identity to use for code signing.

  It can be configured with the following release options:

    * `:entitlements` - path to entitlements file.

  ## Examples

  In `mix.exs`:

  ```elixir
  def project do
    [
      app: :example,
      version: "0.1.0",
      deps: deps(),
      releases: releases()
    ]
  end

  defp releases do
    [
      app: [
        steps: [:assemble, &ElixirKit.Release.codesign/1],
        entitlements: "#{__DIR__}/src-tauri/App.entitlements"
      ]
    ]
  end
  ```

  In `App.entitlements`:

  ```xml
  <?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
    <dict>
    <key>com.apple.security.cs.allow-jit</key>
    <true/>
    <key>com.apple.security.cs.allow-unsigned-executable-memory</key>
    <true/>
    <key>com.apple.security.cs.allow-dyld-environment-variables</key>
    <true/>
    <key>com.apple.security.cs.disable-library-validation</key>
    <true/>
  </dict>
  </plist>
  ```
  """
  def codesign(release) do
    if :os.type() == {:unix, :darwin} do
      identity = System.fetch_env!("APPLE_SIGNING_IDENTITY")
      entitlements = release.options[:entitlements]
      macos_codesign(release.path, identity, entitlements)
    end

    release
  end

  defp macos_codesign(release_path, identity, entitlements) do
    entitlements_flag =
      if entitlements, do: ~s[--entitlements "#{entitlements}"], else: ""

    {_, 0} =
      System.cmd("bash", [
        "-c",
        ~s"""
        files=$(find "#{release_path}" -perm +111 -type f -exec sh -c \
          'file "$1" | grep --silent Mach-O && echo "$1"' _ {} \\;)
        count=$(echo "$files" | wc -l | tr -d ' ')
        echo "Signing $count files with identity: #{identity}"
        echo "$files" | xargs -n 1 -I {} \
          codesign --force --options runtime \
            #{entitlements_flag} \
            --sign "#{identity}" \
            --timestamp "{}"
        """
      ], into: IO.stream(), stderr_to_stdout: true)
  end
end
