# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.5.2](https://github.com/livebook-dev/livebook/tree/v0.5.1) (2022-01-27)

### Added

- Allowed Livebook port to be set to 0 for a random port ([#906](https://github.com/livebook-dev/livebook/pull/906))
- Added memory usage information to runtime panel and sessions list ([#898](https://github.com/livebook-dev/livebook/pull/898), [#917](https://github.com/livebook-dev/livebook/pull/917) and [#918](https://github.com/livebook-dev/livebook/pull/918))
- Added support for font-awesome in mermaid.js diagrams ([#913](https://github.com/livebook-dev/livebook/pull/913))
- Support for reopening the desktop app ([#928](https://github.com/livebook-dev/livebook/pull/928))
- Added a warning when copying to clipboard fails ([#922](https://github.com/livebook-dev/livebook/pull/922))

### Changed

- App icon on macOS to look more native ([#924](https://github.com/livebook-dev/livebook/pull/924))
- Improved errors formatting ([#926](https://github.com/livebook-dev/livebook/pull/926))
- Improved intellisense to work while code is evaluating ([#941](https://github.com/livebook-dev/livebook/pull/941))
- Updated the release to run in interactive mode, hence using less memory ([#944](https://github.com/livebook-dev/livebook/pull/944))
- Updated the release to use a random cookie on every startup ([#944](https://github.com/livebook-dev/livebook/pull/944))

### Fixed

- Favicon rendering in Safari ([#920](https://github.com/livebook-dev/livebook/pull/920))
- Fixed code evaluation in the desktop app to work without Elixir installed globally ([929](https://github.com/livebook-dev/livebook/pull/929))
- Fixed line break support in Mermaid diagram definition ([932](https://github.com/livebook-dev/livebook/pull/932))
- Improved error handling in case of erroneous implementations of the `Inspect` protocol ([934](https://github.com/livebook-dev/livebook/pull/934))
- Fixed image insertion in Markdown cells when the image name includes special characters ([945](https://github.com/livebook-dev/livebook/pull/945))

## [v0.5.1](https://github.com/livebook-dev/livebook/tree/v0.5.1) (2022-01-20)

### Changed

- Changed the file system root path to $HOME in the desktop app ([#887](https://github.com/livebook-dev/livebook/pull/887))

### Fixed

- Loading JavaScript widgets when running behind a domain ([#902](https://github.com/livebook-dev/livebook/pull/902))

## [v0.5.0](https://github.com/livebook-dev/livebook/tree/v0.5.0) (2022-01-19)

### Added

- Support for input forms ([#790](https://github.com/livebook-dev/livebook/pull/790))
- Completion for struct keys ([#793](https://github.com/livebook-dev/livebook/pull/793))
- Support for custom JavaScript widgets ([#826](https://github.com/livebook-dev/livebook/pull/826))
- Introduced a dedicated Explore subsection for Kino guides ([#830](https://github.com/livebook-dev/livebook/pull/830))
- Added notebook name to page title ([#844](https://github.com/livebook-dev/livebook/pull/844))
- Added `@deprecated` and `@since` documentation metadata on mouse over ([#852](https://github.com/livebook-dev/livebook/pull/852))
- Added Livebook and Elixir version information to the Settings page ([#851](https://github.com/livebook-dev/livebook/pull/851))
- Added shutdown button to the sidebar ([#862](https://github.com/livebook-dev/livebook/pull/862))
- An option to increase the font size in the editor ([#860](https://github.com/livebook-dev/livebook/pull/860))
- An option to use a high-contrast editor theme ([#868](https://github.com/livebook-dev/livebook/pull/868) and [#871](https://github.com/livebook-dev/livebook/pull/871))
- Support for rendering mermaid.js graphs in Markdown cells ([#816](https://github.com/livebook-dev/livebook/pull/816))
- Introduced desktop app packaging for macOS ([#865](https://github.com/livebook-dev/livebook/pull/865))

### Changed

- Removed confirmation step when deleting an empty section ([#829](https://github.com/livebook-dev/livebook/pull/829))
- Applied several design improvements ([#858](https://github.com/livebook-dev/livebook/pull/858), [#859](https://github.com/livebook-dev/livebook/pull/859) and [#882](https://github.com/livebook-dev/livebook/pull/882))
- Changed the color scheme in text outputs to improve contrast ([#864](https://github.com/livebook-dev/livebook/pull/858) and [#859](https://github.com/livebook-dev/livebook/pull/864))
- Reworked Kino guides in the Explore section ([#879](https://github.com/livebook-dev/livebook/pull/858) and [#859](https://github.com/livebook-dev/livebook/pull/879))

### Fixed

- Disallowed saving notebooks with empty file name ([#823](https://github.com/livebook-dev/livebook/pull/823))
- Fixed unexpected focus behaviour in large editors ([#831](https://github.com/livebook-dev/livebook/pull/831))
- Fixed Escape key to exit multi-cursor mode ([#833](https://github.com/livebook-dev/livebook/pull/833))
- Improved error reports when reconnecting Mix runtime fails ([#837](https://github.com/livebook-dev/livebook/pull/837))
- Fixed code blocks in Markdown to use monospaced font for all characters ([#855](https://github.com/livebook-dev/livebook/pull/855))

## [v0.4.1](https://github.com/livebook-dev/livebook/tree/v0.4.1) (2021-12-09)

### Added

- Added <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Enter</kbd> (<kbd>⌘</kbd> + <kbd>⇧</kbd> + <kbd>↵</kbd>) for evaluating current and all outdated cells ([#766](https://github.com/livebook-dev/livebook/pull/766))

### Changed

- Disabled word suggestions on Elixir cells ([#763](https://github.com/livebook-dev/livebook/pull/763))

### Fixed

- Fixed error on back navigation after closing a session ([#769](https://github.com/livebook-dev/livebook/pull/769))
- Fixed disappearing indentation when pasting code into the editor ([#779](https://github.com/livebook-dev/livebook/pull/779))

## [v0.4.0](https://github.com/livebook-dev/livebook/tree/v0.4.0) (2021-12-05)

### Added

- Support for file scheme when importing notebook from URL ([#706](https://github.com/livebook-dev/livebook/pull/706))
- Support for rendering UI controls, such as buttons and keyboard ([#710](https://github.com/livebook-dev/livebook/pull/710))
- Added Pong notebook to the explore section to showcase controls ([#729](https://github.com/livebook-dev/livebook/pull/729))
- Improved function completion to insert parentheses ([#693](https://github.com/livebook-dev/livebook/pull/693))
- Added user-specific intellisense configuration ([#693](https://github.com/livebook-dev/livebook/pull/693))
- Added signature completion ([#640](https://github.com/livebook-dev/livebook/pull/640))
- Added automatic persistence for unsaved notebooks ([#736](https://github.com/livebook-dev/livebook/pull/736))
- Support XML and JSON highlighting in Markdown cells ([#743](https://github.com/livebook-dev/livebook/pull/743))

### Changed

- Restructured j/k navigation to support headlines ([#707](https://github.com/livebook-dev/livebook/pull/707))
- Migrated inputs to Kino ([#714](https://github.com/livebook-dev/livebook/pull/714))

### Removed

- Removed input cells in favour of `Kino.Input`, see [#714](https://github.com/livebook-dev/livebook/pull/714) for more details

### Fixed

- Fixed evaluation timer reset on page refresh ([#732](https://github.com/livebook-dev/livebook/pull/732))
- Fixed alignment of long names in the sections panel ([#734](https://github.com/livebook-dev/livebook/pull/734))
- Fixed timeout when importing a notebook with Vega-Lite output ([#741](https://github.com/livebook-dev/livebook/issues/741))

## [v0.3.2](https://github.com/livebook-dev/livebook/tree/v0.3.2) (2021-11-10)

### Added

- An option to clear evaluation outputs ([#661](https://github.com/livebook-dev/livebook/pull/661))
- Evaluation indicators to the sections side panel ([#657](https://github.com/livebook-dev/livebook/pull/657))
- Support for importing a notebook via file upload ([#665](https://github.com/livebook-dev/livebook/pull/665))
- Show/hide button to password inputs ([#664](https://github.com/livebook-dev/livebook/pull/664))
- Improved new directory creation under high latency ([#674](https://github.com/livebook-dev/livebook/pull/674))
- Enabled persisting static Vega-Lite plot to Live Markdown ([#676](https://github.com/livebook-dev/livebook/pull/676))
- Support for animable frame output ([#688](https://github.com/livebook-dev/livebook/pull/688))
- An option to amplify cell outputs ([#689](https://github.com/livebook-dev/livebook/pull/689))
- Included CMake in the Docker image ([#694](https://github.com/livebook-dev/livebook/pull/694))
- Environment variable for disabling token auth ([#696](https://github.com/livebook-dev/livebook/pull/696))

### Changed

- Redesigned save to file modal ([#663](https://github.com/livebook-dev/livebook/pull/663))
- Moved current runtime information into a new side panel ([#692](https://github.com/livebook-dev/livebook/pull/692))

### Fixed

- Rendering math with KaTeX that uses SVGs ([#684](https://github.com/livebook-dev/livebook/pull/684))

## [v0.3.1](https://github.com/livebook-dev/livebook/tree/v0.3.1) (2021-10-27)

### Added

- Introduced automatically reevaluating cells ([#637](https://github.com/livebook-dev/livebook/pull/637))

### Changed

- Changed color for aborted and queued cell status ([#620](https://github.com/livebook-dev/livebook/pull/620))
- Improved Markdown rendering of task and nested lists ([#623](https://github.com/livebook-dev/livebook/pull/623) and [#623](https://github.com/livebook-dev/livebook/pull/631))

### Fixed

- Connecting to an empty S3 bucket ([#646](https://github.com/livebook-dev/livebook/pull/623) and [#623](https://github.com/livebook-dev/livebook/pull/646))
- Importing notebooks served with `application/octet-stream` content type ([#650](https://github.com/livebook-dev/livebook/pull/623) and [#623](https://github.com/livebook-dev/livebook/pull/650))

### Removed

- Removed the keyboard shortcut for "Evaluate cells below" ([#621](https://github.com/livebook-dev/livebook/pull/621))
- Removed reactive inputs in favour of automatically reevaluating cells ([#649](https://github.com/livebook-dev/livebook/pull/649))

## [v0.3.0](https://github.com/livebook-dev/livebook/tree/v0.3.0) (2021-10-19)

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
- Fixed branching section evaluation when the parent section is empty ([#560](https://github.com/livebook-dev/livebook/pull/560))
- Fixed ANSI support to handle multi-code escape sequences ([#569](https://github.com/livebook-dev/livebook/pull/569))

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
