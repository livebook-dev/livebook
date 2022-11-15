defmodule Mix.Tasks.App.Build do
  use Mix.Task

  @impl true
  def run([]) do
    ElixirKit.Bundler.bootstrap_otp()
    path = "#{ElixirKit.Bundler.otp_dir()}/bin" <> ":" <> System.fetch_env!("PATH")

    # build the release in a new shell using bootstrapped OTP
    {_, 0} =
      System.shell("mix release app --overwrite",
        env: %{
          "PATH" => path,
          "MIX_ENV" => to_string(Mix.env()),
          "MIX_TARGET" => to_string(Mix.target())
        },
        into: IO.stream()
      )
  end
end
