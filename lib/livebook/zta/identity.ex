defmodule Livebook.ZTIIdentity do
  def get() do
    # identity = Application.fetch_env!(:livebook, :zti)
    # key = Application.fetch_env!(:livebook, :zti_key)
    {identity, key} =
      Application.fetch_env!(:livebook, :identity_provider)
      |> String.split(":")
      |> List.to_tuple()

    identity(identity, key)
  end

  def identity("cloudflare", key) do
    %{
      key: "domain",
      iss: "https://#{key}.cloudflareaccess.com",
      certs: "https://#{key}.cloudflareaccess.com/cdn-cgi/access/certs",
      assertion: "cf-access-jwt-assertion"
    }
  end

  def identity("googleiap", _key) do
    %{
      key: "aud",
      iss: "https://cloud.google.com/iap",
      certs: "https://www.gstatic.com/iap/verify/public_key",
      assertion: "x-goog-iap-jwt-assertion"
    }
  end

  def identity(_, _), do: nil
end
