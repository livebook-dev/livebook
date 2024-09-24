defmodule Livebook.Stamping do
  # Cryptographic functions used to implement notebook stamping.

  @doc """
  Performs authenticated encryption with associated data (AEAD) [1].

  Uses XChaCha20-Poly1305 [2]. Returns a single token which carries
  encrypted `payload` and signature for both `payload` and
  `additional_data`.

  [1]: https://en.wikipedia.org/wiki/Authenticated_encryption#Authenticated_encryption_with_associated_data_(AEAD)
  [2]: https://en.wikipedia.org/wiki/XChaCha20-Poly1305
  """
  @spec chapoly_encrypt(term(), String.t(), String.t()) :: String.t()
  def chapoly_encrypt(payload, additional_data, secret_key) do
    secret = derive_key(secret_key)
    payload = :erlang.term_to_binary(payload)
    Plug.Crypto.MessageEncryptor.encrypt(payload, additional_data, secret, "unused")
  end

  @doc """
  Decrypts and verifies data obtained from `chapoly_encrypt/3`.
  """
  @spec chapoly_decrypt(String.t(), String.t(), String.t()) :: {:ok, term()} | {:error, :invalid}
  def chapoly_decrypt(encrypted, additional_data, secret_key) do
    secret = derive_key(secret_key)

    case Plug.Crypto.MessageEncryptor.decrypt(encrypted, additional_data, secret, "unused") do
      {:ok, payload} ->
        {:ok, Plug.Crypto.non_executable_binary_to_term(payload)}

      :error ->
        {:error, :invalid}
    end
  end

  @doc """
  Decrypts and verifies data obtained from AEAD using AES-GCM-128 [1].

  Earlier Livebook versions implemented AEAD using AES-GCM-128 and
  this function can be used to decrypt that data.

  [1]: https://www.rfc-editor.org/rfc/rfc5116#section-5
  """
  @spec aead_decrypt(String.t(), String.t(), String.t()) :: {:ok, term()} | {:error, :invalid}
  def aead_decrypt(encrypted, additional_data, secret_key) do
    <<secret::16-bytes, sign_secret::16-bytes>> = derive_key(secret_key)

    case Plug.Crypto.MessageEncryptor.decrypt(encrypted, additional_data, secret, sign_secret) do
      {:ok, payload} ->
        {:ok, Plug.Crypto.non_executable_binary_to_term(payload)}

      :error ->
        {:error, :invalid}
    end
  end

  defp derive_key(secret_key) do
    binary_key = Base.url_decode64!(secret_key, padding: false)
    Plug.Crypto.KeyGenerator.generate(binary_key, "notebook signing", cache: Plug.Crypto.Keys)
  end

  @doc """
  Verifies RSA `signature` of the given `payload` using the public key.
  """
  @spec rsa_verify?(String.t(), String.t(), String.t()) :: boolean()
  def rsa_verify?(signature, payload, public_key) do
    der_key = Base.url_decode64!(public_key, padding: false)

    with {:ok, raw_key} <- safe_der_decode(der_key),
         {:ok, raw_signature} <- Base.url_decode64(signature, padding: false) do
      :public_key.verify(payload, :sha256, raw_signature, raw_key)
    else
      _ -> false
    end
  end

  defp safe_der_decode(der_key) do
    try do
      {:ok, :public_key.der_decode(:RSAPublicKey, der_key)}
    rescue
      _ -> :error
    end
  end
end
