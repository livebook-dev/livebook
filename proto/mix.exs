defmodule LivebookProto.MixProject do
  use Mix.Project

  def project do
    [
      app: :livebook_proto,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [{:protobuf, "~> 0.8.0"}]
  end

  defp aliases do
    [
      "protobuf.generate": [
        "cmd protoc --elixir_out=one_file_per_module=true:lib --elixir_opt=include_docs=true --elixir_opt=gen_struct=true --elixir_opt=package_prefix=livebook_proto messages.proto",
        "format lib/livebook_proto/*.pb.ex",
        "deps.get",
        "compile"
      ]
    ]
  end
end
