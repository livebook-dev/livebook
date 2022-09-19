## How to build & run

1. Install xcode from the app store.
1. Install brew, git, carthage, npm

    `brew install carthage git openssl@1.1 npm`

1. Install Erlang-OTP (with openssl) in the same version 25.0.4 as the bundled runtime edition:

    ```
    export DED_LDFLAGS_CONFTEST="-bundle"
    export KERL_CONFIGURE_OPTIONS="--without-javac --with-ssl=$(brew --prefix openssl@1.1)"
    asdf install erlang 25.0.4 --with-ssl=$(brew --prefix openssl@1.1)`
    asdf install elixir 1.13.4-otp-25
    ```

carthage update --use-xcframeworks

1. Open the ios-example-app project with xcode
1. Start the App

