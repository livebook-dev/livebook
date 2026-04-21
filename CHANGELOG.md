# Changelog for Livebook v0.19

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## HEAD

### Added

- Added configurable output size for cells, also applicable to apps (#3160)
- Exposed query params from a multi-session permalink via `Kino.Workspace.app_info/0` (#3169)

### Fixed

- App server to be locked while Livebook Teams connection is pending (#3168)
- Icons not loading when using `LIVEBOOK_BASE_URL_PATH`
- (Desktop) Missing macOS .livemd icons
- (Desktop) Opening .livebookdesktop.sh,.bat
- (Desktop) Missing Windows code signing
- (Desktop) Missing Windows installer icon

## [v0.19.6](https://github.com/livebook-dev/livebook/tree/v0.19.6) (2026-03-27)

### Fixed

- Some smart cells getting cleared on notebook `/dev/sync` ([#3161](https://github.com/livebook-dev/livebook/pull/3161))
- Reauthentication failures when accessing Livebook Teams apps ([#3164](https://github.com/livebook-dev/livebook/pull/3164))

## [v0.19.5](https://github.com/livebook-dev/livebook/tree/v0.19.5) (2026-03-23)

### Changed

- Cache app users authentication within a time window if Livebook Teams is down ([#3156](https://github.com/livebook-dev/livebook/pull/3156))

## [v0.19.4](https://github.com/livebook-dev/livebook/tree/v0.19.4) (2026-03-18)

### Added

- Dev endpoint for syncing changes from file ([#3154](https://github.com/livebook-dev/livebook/pull/3154), [#3155](https://github.com/livebook-dev/livebook/pull/3155))
- Dev endpoint for restamping notebooks ([#3154](https://github.com/livebook-dev/livebook/pull/3154))

### Fixed

- Fixed a few UI styling regressions

## [v0.19.3](https://github.com/livebook-dev/livebook/tree/v0.19.3) (2026-03-09)

### Added

- Link for to the current GitHub release on the settings page ([#3147](https://github.com/livebook-dev/livebook/pull/3147))
- Learn section notebook about Python integrations ([#3150](https://github.com/livebook-dev/livebook/pull/3150))

### Fixed

- (Desktop) Fixed "View logs" ([#3148](https://github.com/livebook-dev/livebook/pull/3148))
- (Desktop) Registering .livemd file associations at runtime on AppImage on Linux ([#3148](https://github.com/livebook-dev/livebook/pull/3148))
- (Desktop) Fixed xdg-open breaking on Linux inside AppImage due to injected env vars ([#3148](https://github.com/livebook-dev/livebook/pull/3148))
- (Desktop) Console window appearning when starting app on Windows ([#3148](https://github.com/livebook-dev/livebook/pull/3148))
- (Desktop) Fixed macOS x86 app expecting arm64
- Cell stale indicator missing color

## [v0.19.2](https://github.com/livebook-dev/livebook/tree/v0.19.2) (2026-03-04)

### Fixed

- Fix compatibility with OTP 27 and earlier

## [v0.19.1](https://github.com/livebook-dev/livebook/tree/v0.19.1) (2026-03-04)

### Fixed

- Fly runtime failing to start with proxy auth error on latest fly CLI ([#3145](https://github.com/livebook-dev/livebook/pull/3145))
- Compatibility of form number input with prior kino versions

## [v0.19.0](https://github.com/livebook-dev/livebook/tree/v0.19.0) (2026-03-03)

As part of this release, we reworked the desktop app to use Tauri, and we now have a Linux version of the app. The Linux support is considered in to be in beta mode, if you notice any unexpected behaviour, please let us know.

### Added

- Python intellisense ([#3133](https://github.com/livebook-dev/livebook/pull/3133))
- (Desktop) Linux build of the desktop app ([#3112](https://github.com/livebook-dev/livebook/pull/3112))
- Support for min, max and step in number inputs ([#3139](https://github.com/livebook-dev/livebook/pull/3139))
- Added `LIVEBOOK_APPS_BANNER` for configuring Livebook apps banner ([#3144](https://github.com/livebook-dev/livebook/pull/3144))

### Changed

- Extract Livebook.ZTA into its own library ([#3103](https://github.com/livebook-dev/livebook/pull/3103))
- (Desktop) Changed the underlying desktop app implementation to use Tauri ([#3112](https://github.com/livebook-dev/livebook/pull/3112))
- Formatting shortcut to `ctrl + shift + f` on Linux/Windows and `cmd + shift + f` on macOS ([#3141](https://github.com/livebook-dev/livebook/pull/3141))

### Fixed

- Documentation links to Erlang functions and types
- Pressing Home/End/PageUp/PageDown from moving the page while editing

## v0.18

The CHANGELOG for v0.18 releases can be found in the [v0.18](https://github.com/livebook-dev/livebook/tree/v0.18/CHANGELOG.md) branch.
