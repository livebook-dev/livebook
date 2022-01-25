defmodule Livebook.Config.FileBackend do
  defstruct loaded?: false, config: %{}

  @block_size 16

  def default_path() do
    dir =
      if Mix.env() == :test do
        System.tmp_dir!()
      else
        :filename.basedir(:user_cache, "livebook")
      end

    Path.join([dir, "livebook_#{System.get_env("LIVEBOOK_PORT")}.conf"])
  end

  def encrypt(payload, secret) do
    payload = pad(payload, @block_size)

    iv = :crypto.strong_rand_bytes(16)
    <<secret::binary-32, _rest::binary>> = secret
    ct = :crypto.crypto_one_time(:aes_256_cbc, secret, iv, payload, true)

    Base.encode16(iv <> ct)
  end

  def decrypt(payload, secret) do
    <<iv::binary-16, payload::binary>> = Base.decode16!(payload)

    <<secret::binary-32, _rest::binary>> = secret
    payload = :crypto.crypto_one_time(:aes_256_cbc, secret, iv, payload, false)

    unpad(payload)
  end

  def unpad(data) do
    to_remove = :binary.last(data)
    :binary.part(data, 0, byte_size(data) - to_remove)
  end

  # PKCS5Padding
  def pad(data, block_size) do
    to_add = block_size - rem(byte_size(data), block_size)
    data <> :binary.copy(<<to_add>>, to_add)
  end
end

defimpl Livebook.ConfigBackend, for: Livebook.Config.FileBackend do
  alias Livebook.Config.FileBackend

  def get(%FileBackend{loaded?: false}, _key), do: {:error, :not_loaded}

  def get(%FileBackend{config: config}, key),
    do: Map.get(config, key) || Application.fetch_env!(:livebook, key)

  @secret Base.decode64!("XMtOuJDRTDiltaZqRpcPI/e6Jm8OTFNozh6+cjLqM2tGkbXlQdv9bx3H90AP6FkV")
  def put(%FileBackend{config: config} = backend, key, value) do
    backend = %{backend | config: Map.put(config, key, value)}

    payload = :erlang.term_to_binary(backend.config)

    secret = Livebook.Config.secret!("LIVEBOOK_SECRET_KEY_BASE") || @secret
    payload = FileBackend.encrypt(payload, secret)

    File.write!(FileBackend.default_path(), payload)

    backend
  end

  def load(%FileBackend{loaded?: true} = backend), do: backend

  def load(%FileBackend{} = backend) do
    secret = Livebook.Config.secret!("LIVEBOOK_SECRET_KEY_BASE") || @secret

    filename = FileBackend.default_path()

    config =
      with true <- File.exists?(filename),
           payload when is_binary(payload) and payload != "" <- File.read!(filename) do
        payload
        |> FileBackend.decrypt(secret)
        |> :erlang.binary_to_term()
      else
        _ ->
          %{}
      end

    %FileBackend{backend | config: config, loaded?: true}
  end
end
