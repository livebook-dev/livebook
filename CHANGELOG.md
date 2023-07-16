# Changelog for Livebook v0.10

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.10.0](https://github.com/livebook-dev/livebook/tree/v0.10.0) (2023-07-14)

### Added

- Added @home CLI argument for opening homepage ([#1868](https://github.com/livebook-dev/livebook/pull/1868))
- Notebook presentation view ([#1852](https://github.com/livebook-dev/livebook/pull/1852))
- (Desktop) Show clear error when app cannot start because hostname contains whitespace ([#1894](https://github.com/livebook-dev/livebook/pull/1894))
- Form snippet under "+ Block" ([#1750](https://github.com/livebook-dev/livebook/pull/1750))
- Apps setting for showing either all outputs or rich outputs ([#1905](https://github.com/livebook-dev/livebook/pull/1905))
- Support for multi-instance apps and automatic shutdown ([#1913](https://github.com/livebook-dev/livebook/pull/1913))
- (Desktop) Support for setting `LIVEBOOK_PORT` in `~/.livebookdesktop.sh` ([#1920](https://github.com/livebook-dev/livebook/pull/1920))
- Added decorations showing doctest results directly in the editor ([#1911](https://github.com/livebook-dev/livebook/pull/1911), [#1936](https://github.com/livebook-dev/livebook/pull/1936) and [#1944](https://github.com/livebook-dev/livebook/pull/1944))
- Support for "workflow apps" via automatic reevaluation and explicit interrupts ([#1928](https://github.com/livebook-dev/livebook/pull/1928))
- Decorations showing multiple compiler errors and warnings in the editor on Elixir v1.15 ([#1918](https://github.com/livebook-dev/livebook/pull/1918))
- Autocompletion for map/struct fields on update ([#1918](https://github.com/livebook-dev/livebook/pull/1918))
- Debug button to app menu for authenticated users ([#1940](https://github.com/livebook-dev/livebook/pull/1940))
- Support for Erlang code cells ([#1892](https://github.com/livebook-dev/livebook/pull/1892), [#1960](https://github.com/livebook-dev/livebook/pull/1960))
- Notifications on an app page when a new version is deployed ([#1955](https://github.com/livebook-dev/livebook/pull/1955))
- Showing import warnings on the apps page for apps deployed via `LIVEBOOK_APPS_PATH` ([#1955](https://github.com/livebook-dev/livebook/pull/1955))
- Fallback that loads package assets from CDN to improve behaviour when Livebook runs behind an auth proxy ([#1958](https://github.com/livebook-dev/livebook/pull/1958))
- More intellisense details for types ([#1974](https://github.com/livebook-dev/livebook/pull/1974) and [#1975](https://github.com/livebook-dev/livebook/pull/1975))
- Support for Zero Trust authentication ([#1938](https://github.com/livebook-dev/livebook/pull/1938))
- Improved the session page on mobile ([#2013](https://github.com/livebook-dev/livebook/pull/2013))
- Support for datetime, time and date inputs ([#2002](https://github.com/livebook-dev/livebook/pull/2002))

### Changed

- Moved app settings to a modal and added explanations for all options ([#1914](https://github.com/livebook-dev/livebook/pull/1914))
- Disabled double-click entering edit mode on Markdown cells ([#1937](https://github.com/livebook-dev/livebook/pull/1937))
- Connecting an "Attached node" runtime now requires that the remote node use the same or later Elixir and OTP version as Livebook when running Livebook Desktop or using Docker
  - For instance, this means that Livebook Desktop v0.10.0 can only start attached nodes running Elixir 1.15.2+ and Erlang/OTP 26+
  - Livebook CLI installed with earlier versions of Elixir and Erlang/OTP can still connect to remote nodes running those versions or later

### Fixed

- Improve experience when Livebook is embedded in an iframe and has no cookie access ([#1888](https://github.com/livebook-dev/livebook/pull/1888) and [#1889](https://github.com/livebook-dev/livebook/pull/1889))
- Fixed intellisense crash when hovering over bitstring modifiers ([#1917](https://github.com/livebook-dev/livebook/pull/1917))
- Fixed file input crash when using unknown MIME type as filter ([#1922](https://github.com/livebook-dev/livebook/pull/1922))
- (Desktop) Fixed the app not showing up in Spotlight search on macOS ([#1930](https://github.com/livebook-dev/livebook/pull/1930) and [#1942](https://github.com/livebook-dev/livebook/pull/1942))
- Fixed intellisense crash when hovering over Erlang modules with type names in signature ([#1957](https://github.com/livebook-dev/livebook/pull/1957))
- Started ignoring generated functions when running doctests ([#1966](https://github.com/livebook-dev/livebook/pull/1966))
- Fixed card images not showing in Learn section when `LIVEBOOK_BASE_URL_PATH` is set ([#1988](https://github.com/livebook-dev/livebook/pull/1988))

## v0.9

The CHANGELOG for v0.9 releases can be found in the [v0.9](https://github.com/livebook-dev/livebook/tree/v0.9/CHANGELOG.md) branch.
