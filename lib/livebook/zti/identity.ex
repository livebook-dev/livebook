defmodule Livebook.ZTIIdentity do
  def get("cloudflare", key) do
    %{
      key: "domain",
      iss: "https://#{key}.cloudflareaccess.com",
      certs: "https://#{key}.cloudflareaccess.com/cdn-cgi/access/certs",
      assertion: "cf-access-jwt-assertion"
    }
  end

  def get("googleiap", _key) do
    %{
      key: "aud",
      iss: "https://cloud.google.com/iap",
      certs: "https://www.gstatic.com/iap/verify/public_key",
      assertion: "x-goog-iap-jwt-assertion"
    }
  end
end
