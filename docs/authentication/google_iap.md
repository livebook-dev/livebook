# Authentication with Google IAP

To integrate your Google Identity-Aware Proxy (IAP) authentication with Livebook,
set the `LIVEBOOK_IDENTITY_PROVIDER` environment variable to `google_iap:<your-jwt-audience>`.

For more information about Google IAP, see https://cloud.google.com/iap/docs/concepts-overview.

Only access with Google accounts is supported. See https://cloud.google.com/iap/docs/authenticate-users-google-accounts.

For more details about how to find your JWT audience, see: https://cloud.google.com/iap/docs/signed-headers-howto
and look for "Signed Header JWT Audience."