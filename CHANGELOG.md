# Changelog for Livebook v0.19

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

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
