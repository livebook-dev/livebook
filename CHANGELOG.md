# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased](https://github.com/livebook-dev/livebook)

### Added

- Introduced file system abstraction and an S3 support ([#492](https://github.com/livebook-dev/livebook/pull/492))
- Added support for configuring file systems using env variables ([#498](https://github.com/livebook-dev/livebook/pull/498))
- Added a keyboard shortcut for triggering on-hover docs ([#508](https://github.com/livebook-dev/livebook/pull/508))
- Added `--open-new` CLI flag to `livebook server` ([#529](https://github.com/livebook-dev/livebook/pull/529))
- Nx introductory notebook ([#528](https://github.com/livebook-dev/livebook/pull/528))
- Display creation date of a session in home ([#593](https://github.com/livebook-dev/livebook/pull/593))
- Dynamic favicon reflecting session state ([#594](https://github.com/livebook-dev/livebook/pull/594))

### Changed

- Improved intellisense to handle structs and sigils ([#513](https://github.com/livebook-dev/livebook/pull/513))
- Create new notebooks with an already focused code cell ([#527](https://github.com/livebook-dev/livebook/pull/527))
- Switched base Docker image from alpine to debian-slim ([#552](https://github.com/livebook-dev/livebook/pull/552))
- Update matching brackets style ([#595](https://github.com/livebook-dev/livebook/pull/595))

### Fixed

- Improved Markdown and math integration by migrating to remark ([#495](https://github.com/livebook-dev/livebook/pull/495))
- Improved the evaluator process to not consume user-submitted messages from inbox ([#502](https://github.com/livebook-dev/livebook/pull/502))
- Improved sections panel UI to better handle numerous sections or long section names ([#534](https://github.com/livebook-dev/livebook/pull/534) and [#537](https://github.com/livebook-dev/livebook/pull/537))
- Fixed branching section evaluation when the parent section is empty ([#560](https://github.com/livebook-dev/livebook/pull/560)
- Fixed ANSI support to handle multi-code escape sequences ([#569](https://github.com/livebook-dev/livebook/pull/569)

## [v0.2.3](https://github.com/livebook-dev/livebook/tree/v0.2.3) (2021-08-12)

### Added

- Added UI for directory creation ([#424](https://github.com/livebook-dev/livebook/pull/424))
- Added UI for file deletion and renaming ([#426](https://github.com/livebook-dev/livebook/pull/426))
- Listing Livebook version on dashboard home
- Support for relative navigation between notebooks ([#441](https://github.com/livebook-dev/livebook/pull/441) and [#445](https://github.com/livebook-dev/livebook/pull/445))
- Introduced branching sections ([#449](https://github.com/livebook-dev/livebook/pull/449))
- Range (slider) input ([#435](https://github.com/livebook-dev/livebook/pull/435) and [#440](https://github.com/livebook-dev/livebook/pull/440))
- Select input ([#448](https://github.com/livebook-dev/livebook/pull/448))
- Checkbox input ([#461](https://github.com/livebook-dev/livebook/pull/461))
- Showing full documentation when hovering over an identifier ([#453](https://github.com/livebook-dev/livebook/pull/453))
- Added notebook source preview and export ([#457](https://github.com/livebook-dev/livebook/pull/457))
- Added Elixir source export ([#476](https://github.com/livebook-dev/livebook/pull/476))
- Added option to export and persist Live Markdown with output ([#483](https://github.com/livebook-dev/livebook/pull/483) and [#485](https://github.com/livebook-dev/livebook/pull/485))

### Changed

- Merge undo stack for collaborative editing ([#433](https://github.com/livebook-dev/livebook/pull/433))
- Restructured remote node processes to allow for multiple connections ([#434](https://github.com/livebook-dev/livebook/pull/434))

### Fixed

- Fixed editor font size on MacOS

## [v0.2.2](https://github.com/livebook-dev/livebook/tree/v0.2.2) (2021-07-01)

### Added

- Support for configuring Attached as the default runtime ([#397](https://github.com/livebook-dev/livebook/pull/397))
- Textarea input ([#382](https://github.com/livebook-dev/livebook/pull/382))
- Color input ([#410](https://github.com/livebook-dev/livebook/pull/410))
- Support for markdown output ([#404](https://github.com/livebook-dev/livebook/pull/404))
- Introduced cells bin for restoring deleted cells ([#414](https://github.com/livebook-dev/livebook/pull/414))
- Added code formatting integration to Elixir cells ([#416](https://github.com/livebook-dev/livebook/pull/416))
- Added `--open` CLI flag to `livebook server` ([#417](https://github.com/livebook-dev/livebook/pull/417))
- Suggest restarting runtime on Mix.install error and add restart shortcut ([#418](https://github.com/livebook-dev/livebook/pull/418))
- Documented editor shortcuts and added basic view ([#419](https://github.com/livebook-dev/livebook/pull/419))

### Changed

- Improved section management ([#411](https://github.com/livebook-dev/livebook/pull/411))
- Highlight matching brackets only in insert mode ([#421](https://github.com/livebook-dev/livebook/pull/421))

## [v0.2.1](https://github.com/livebook-dev/livebook/tree/v0.2.1) (2021-06-24)

### Added

- Showing the evaluation time next to the cell indicator ([#366](https://github.com/livebook-dev/livebook/pull/366))
- Showing a ticking timer while cell is evaluating ([#374](https://github.com/livebook-dev/livebook/pull/374))
- Added a section on evaluation vs compilation to the Elixir notebook ([#376](https://github.com/livebook-dev/livebook/pull/376))
- Support for image output ([#380](https://github.com/livebook-dev/livebook/pull/380))
- Introduced reactive inputs ([#389](https://github.com/livebook-dev/livebook/pull/389))
- Added copy to clipboard button to virtualized output ([#393](https://github.com/livebook-dev/livebook/pull/393))

## [v0.2.0](https://github.com/livebook-dev/livebook/tree/v0.2.0) (2021-06-17)

Next milestone, see [New in Livebook v0.2 by José Valim](https://www.youtube.com/watch?v=MOTEgF-wIEI).

### Added

- Introduced an Explore section ([#310](https://github.com/livebook-dev/livebook/pull/310))
- Notebook about the unique features behind Elixir and Livebook ([#314](https://github.com/livebook-dev/livebook/pull/314))
- Portal guide ([#328](https://github.com/livebook-dev/livebook/pull/318))
- VegaLite introductory notebook ([#335](https://github.com/livebook-dev/livebook/pull/335))
- Kino introductory notebook ([#364](https://github.com/livebook-dev/livebook/pull/364))
- Notebook on VM introspection ([#346](https://github.com/livebook-dev/livebook/pull/346))
- Show an informative message on completion when there is no runtime ([#316](https://github.com/livebook-dev/livebook/pull/316))
- Support for inputs ([#328](https://github.com/livebook-dev/livebook/pull/328))
- Numeric input ([#352](https://github.com/livebook-dev/livebook/pull/352))
- Password input ([#357](https://github.com/livebook-dev/livebook/pull/357))
- Support for Mix runtime as the default one ([#334](https://github.com/livebook-dev/livebook/pull/334))
- Dynamic table output ([#356](https://github.com/livebook-dev/livebook/pull/356))

## [v0.1.2](https://github.com/livebook-dev/livebook/tree/v0.1.2) (2021-06-01)

### Added

- Capture and show logger output ([#298](https://github.com/livebook-dev/livebook/pull/298))
- Support for dynamic Vega-Lite graphics ([#306](https://github.com/livebook-dev/livebook/pull/306) and [#309](https://github.com/livebook-dev/livebook/pull/309))

## [v0.1.1](https://github.com/livebook-dev/livebook/tree/v0.1.1) (2021-05-24)

### Added

- Support for plots rendering via [`VegaLite`](https://github.com/elixir-nx/vega_lite) ([#287](https://github.com/livebook-dev/livebook/pull/287))

### Changed

- Improved path selector to properly handle text editing in the middle ([#283](https://github.com/livebook-dev/livebook/pull/283))
- Made section anchor links deterministic ([#288](https://github.com/livebook-dev/livebook/pull/288))

## [v0.1.0](https://github.com/livebook-dev/livebook/tree/v0.1.0) (2021-05-19)

Initial release, see [Livebook's announcement by José Valim](https://www.youtube.com/watch?v=RKvqc-UEe34).
