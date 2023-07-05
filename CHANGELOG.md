# Changelog for Livebook v0.9

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.9.3](https://github.com/livebook-dev/livebook/tree/v0.9.3) (2023-06-20)

### Fixed

- Do not browser open arbitrary URLs

## [v0.9.2](https://github.com/livebook-dev/livebook/tree/v0.9.2) (2023-04-14)

### Added

- Made the error status clickable on failed deployments ([#1854](https://github.com/livebook-dev/livebook/pull/1854))

### Changed

- Made app settings form hidden in app session ([#1860](https://github.com/livebook-dev/livebook/pull/1860))

### Fixed

- Fix smart cell indicator when source changes on start ([#1851](https://github.com/livebook-dev/livebook/pull/1851))
- Persist show-source app option ([#1859](https://github.com/livebook-dev/livebook/pull/1859))
- Added a link to navigate out of iframe when Livebook has no access to cookies ([#1863](https://github.com/livebook-dev/livebook/pull/1863))

## [v0.9.1](https://github.com/livebook-dev/livebook/tree/v0.9.1) (2023-04-06)

### Added

- Support for clients-only outputs ([#1810](https://github.com/livebook-dev/livebook/pull/1810))
- Option to unstar notebooks directly from cards ([#1818](https://github.com/livebook-dev/livebook/pull/1818))
- Option to hide notebooks from recent list ([#1830](https://github.com/livebook-dev/livebook/pull/1830))
- Purging cookies once they exceed 24kB ([#1833](https://github.com/livebook-dev/livebook/pull/1833))
- Preview for booting apps on the auth screen ([#1836](https://github.com/livebook-dev/livebook/pull/1836))

### Changed

- Moved "Livebook" to the end of page title ([#1828](https://github.com/livebook-dev/livebook/pull/1828))

### Fixed

- Image and audio inputs to update preview on change for other users ([#1807](https://github.com/livebook-dev/livebook/pull/1807))
- Notebooks on Kino and on MapLibre ([#1806](https://github.com/livebook-dev/livebook/pull/1806), [#1811](https://github.com/livebook-dev/livebook/pull/1811))
- Booting if apps path is unavailable ([#1812](https://github.com/livebook-dev/livebook/pull/1812))
- Rendering of error pages
- (Desktop) Fixed app URLs ([#1835](https://github.com/livebook-dev/livebook/pull/1835))
- Respect whitespace in plain text output ([#1841](https://github.com/livebook-dev/livebook/pull/1841))
- (Desktop) Fixed displaying error dialog and logging errors ([#1848](https://github.com/livebook-dev/livebook/pull/1848))

## [v0.9.0](https://github.com/livebook-dev/livebook/tree/v0.9.0) (2023-03-20)

We migrated Docker images from Docker Hub to GitHub Container Registry (see [#1792](https://github.com/livebook-dev/livebook/pull/1792)). Make sure to upgrade image references from `livebook/livebook` to `ghcr.io/livebook-dev/livebook`.

### Added

- (Desktop) "Copy URL", "View Logs" and "Settings" buttons ([#1650](https://github.com/livebook-dev/livebook/pull/1650))
- Support for sending frame updates to a specific client ([#1662](https://github.com/livebook-dev/livebook/pull/1662))
- Support for setting node and distribution using environment vars ([#1672](https://github.com/livebook-dev/livebook/pull/1672))
- (Desktop) Support for configuring env vars ([#1676](https://github.com/livebook-dev/livebook/pull/1676))
- Configuration for allowed hyperlink schemes in Markdown content ([#1702](https://github.com/livebook-dev/livebook/pull/1702))
- Support for deploying notebooks as apps locally ([#1709](https://github.com/livebook-dev/livebook/pull/1709), [#1715](https://github.com/livebook-dev/livebook/pull/1715), [#1728](https://github.com/livebook-dev/livebook/pull/1728), [#1733](https://github.com/livebook-dev/livebook/pull/1733))
- Support for deploying apps from directory ([#1741](https://github.com/livebook-dev/livebook/pull/1741), [#1757](https://github.com/livebook-dev/livebook/pull/1757))
- History of recently open notebooks ([#1639](https://github.com/livebook-dev/livebook/pull/1639))
- Ability to star notebooks for quicker access ([#1639](https://github.com/livebook-dev/livebook/pull/1639), [#1753](https://github.com/livebook-dev/livebook/pull/1753))
- Added "New notebook" action to the file explorer ([#1754](https://github.com/livebook-dev/livebook/pull/1754))
- Light editor theme ([#1755](https://github.com/livebook-dev/livebook/pull/1755))
- Added Hubs page and Personal Hub for secret management, much more to come ([#1756](https://github.com/livebook-dev/livebook/pull/1756), [#1744](https://github.com/livebook-dev/livebook/pull/1744), [#1763](https://github.com/livebook-dev/livebook/pull/1763))
- Added "Data transform cell" to predefined Smart cells ([#1758](https://github.com/livebook-dev/livebook/pull/1758))
- Added stamp to notebook source to securely persist allowed secret names ([#1768](https://github.com/livebook-dev/livebook/pull/1768))
- Support for collapsing sections ([#1772](https://github.com/livebook-dev/livebook/pull/1772), [#1779](https://github.com/livebook-dev/livebook/pull/1779))
- Data transform guide ([#1773](https://github.com/livebook-dev/livebook/pull/1773))
- Triggering completion suggestions on dot ([#1784](https://github.com/livebook-dev/livebook/pull/1784))
- Reorganized Kino guides
- Support for plain text output ([#1790](https://github.com/livebook-dev/livebook/pull/1790))

### Changed

- Evaluation error to cancel further evaluation and show distinct status ([#1688](https://github.com/livebook-dev/livebook/pull/1688))
- Setup cell to stay open on error ([#1718](https://github.com/livebook-dev/livebook/pull/1718))
- Unified opening and importing notebooks under a single Open page ([#1639](https://github.com/livebook-dev/livebook/pull/1639))
- `MIX_ENV` from Livebook startup is no longer propagated to the runtime
- Default directory when saving a forked notebook ([#1767](https://github.com/livebook-dev/livebook/pull/1767))
- Migrated Docker images to GitHub Container Registry ([#1792](https://github.com/livebook-dev/livebook/pull/1792), [#1794](https://github.com/livebook-dev/livebook/pull/1794))

### Removed

- Removed most CLI flags in favour of env vars ([#1704](https://github.com/livebook-dev/livebook/pull/1704))
- High contrast editor theme in favour of light theme ([#1755](https://github.com/livebook-dev/livebook/pull/1755))

### Fixed

- Reworked file input to work inside form with multiple clients ([#1673](https://github.com/livebook-dev/livebook/pull/1673))
- Resetting autosave interval when changing file system ([#1689](https://github.com/livebook-dev/livebook/pull/1689))
- Erlang module docs in intellisense ([#1719](https://github.com/livebook-dev/livebook/pull/1719))
- Assertion errors in doctests ([#1752](https://github.com/livebook-dev/livebook/pull/1752))
- Error when converting Smart cell with input to Code ([#1781](https://github.com/livebook-dev/livebook/pull/1781))
- Improved iframe repositioning when moving sections ([#1796](https://github.com/livebook-dev/livebook/pull/1796))

## v0.8

The CHANGELOG for v0.8 releases can be found in the [v0.8](https://github.com/livebook-dev/livebook/tree/v0.8/CHANGELOG.md) branch.
