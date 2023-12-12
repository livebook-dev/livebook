# Changelog for Livebook v0.12

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased](https://github.com/livebook-dev/livebook/tree/main)

### Added

* Support for custom identity providers ([#2301](https://github.com/livebook-dev/livebook/pull/2301))
* Zero Trust authentication strategy for Teleport ([#2296](https://github.com/livebook-dev/livebook/pull/2296))
* Docs for shared Teams secrets and file storages ([#2317](https://github.com/livebook-dev/livebook/pull/2317), [#2318](https://github.com/livebook-dev/livebook/pull/2318))
* Support for sourcing S3 file system credentials from environment variables, AWS credentials, or Amazon EC2/ECS metadata ([#2347](https://github.com/livebook-dev/livebook/pull/2347), [#2358](https://github.com/livebook-dev/livebook/pull/2358))
* (Desktop) Started registering Livebook in Add/Remove Programs on Windows ([#2398](https://github.com/livebook-dev/livebook/pull/2398))

### Changed

* Required OTP version to >= 25 ([#2333](https://github.com/livebook-dev/livebook/pull/2333))
* Converting from Smart cell to Code cell to skip outputs ([#2348](https://github.com/livebook-dev/livebook/pull/2348))
* Smart cell attributes serialization in Live Markdown ([#2364](https://github.com/livebook-dev/livebook/pull/2364))

### Removed

* The deprecated `warmup_apps.sh` script from Docker release in favour of `warmup_apps`

### Fixed

* Session crashes when deleting a broken Smart cell ([#2314](https://github.com/livebook-dev/livebook/pull/2314))
* Session page crash when restoring cell with input from bin ([#2312](https://github.com/livebook-dev/livebook/pull/2312))
* Rare `Kino.JS.Live` output crashes on socket reconnection ([#2363](https://github.com/livebook-dev/livebook/pull/2363))
* Persisting Smart cells with `-->` sequence in the attributes ([#2364](https://github.com/livebook-dev/livebook/pull/2364))
* Downloading files in notebook export
* Secrets list not updating when deleting Teams hub secrets ([#2371](https://github.com/livebook-dev/livebook/pull/2371))
* File system form in Teams hub to show errors when creation fails because of duplicated bucket ([#2396](https://github.com/livebook-dev/livebook/pull/2396))

## v0.11

The CHANGELOG for v0.11 releases can be found in the [v0.11](https://github.com/livebook-dev/livebook/tree/v0.11/CHANGELOG.md) branch.
