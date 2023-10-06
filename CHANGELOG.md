# Changelog for Livebook v0.11

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased](https://github.com/livebook-dev/livebook/tree/main)

This release introduces file management, you can now tell Livebook what files the notebook depends on, be it on your disk or on the web. In relation to this, the `images/` directory has been deprecated in favour of notebook attachments (files living in the `files/` directory). One way to migrate is to rename the `images/` directory to `files/`, open the files panel (in the session sidebar), then "Add file" and choose "From unlisted". If you want to automatically do this for a large repository of notebooks, you can try out [this script](https://gist.github.com/jonatanklosko/20e28aa772a888a25a829337a4b805e1).

This release introduces a **breaking change** to audio and image inputs. Previously input values (when read or received as an event) included a `:data` attribute with a binary. This attribute has been removed in favour of `:file_ref`, this way the binary is not stored in the memory upfront and it is up to the user to read it or stream it depending on the use case. For more information on the API, see [Kino v0.11.0 changelog](https://github.com/livebook-dev/kino/blob/main/CHANGELOG.md#user-content-v0110-2023-10-06).

### Added

* Introduced file management to track and access files used by a notebook ([#2022](https://github.com/livebook-dev/livebook/pull/2022), [#2044](https://github.com/livebook-dev/livebook/pull/2044), [#2072](https://github.com/livebook-dev/livebook/pull/2072), [#2083](https://github.com/livebook-dev/livebook/pull/2083), [#2085](https://github.com/livebook-dev/livebook/pull/2085), [#2111](https://github.com/livebook-dev/livebook/pull/2111), [#2112](https://github.com/livebook-dev/livebook/pull/2112), [#2113](https://github.com/livebook-dev/livebook/pull/2113), [#2131](https://github.com/livebook-dev/livebook/pull/2131), [#2167](https://github.com/livebook-dev/livebook/pull/2167))
* Improved debugging discoverability on app errors ([#2061](https://github.com/livebook-dev/livebook/pull/2061))
* Option to mark a directory as the default file selector location ([#2046](https://github.com/livebook-dev/livebook/pull/2046))
* Button for reevaluating apps on change ([#2066](https://github.com/livebook-dev/livebook/pull/2066))
* Button for retrying on app errors ([#2066](https://github.com/livebook-dev/livebook/pull/2066))
* Drag and drop actions for notebook files ([#2096](https://github.com/livebook-dev/livebook/pull/2096), [#2103](https://github.com/livebook-dev/livebook/pull/2103), [#2192](https://github.com/livebook-dev/livebook/pull/2192))
* Support for dropping external files into the notebook ([#2097](https://github.com/livebook-dev/livebook/pull/2097))
* Notebook custom view ([#2101](https://github.com/livebook-dev/livebook/pull/2101))
* Automatic runtime setup when creating a new notebook to streamline autocompletion ([#2102](https://github.com/livebook-dev/livebook/pull/2102))
* Manual warmup mode for apps to cache notebook dependencies during a Docker build ([#2115](https://github.com/livebook-dev/livebook/pull/2115))
* Warning when Livebook runs out of memory during notebook setup ([#2128](https://github.com/livebook-dev/livebook/pull/2128))
* Shortcut to toggle keyboard control in the focused cell ([#2145](https://github.com/livebook-dev/livebook/pull/2145))
* Option to enable Vim/Emacs mode in the editor ([#2173](https://github.com/livebook-dev/livebook/pull/2173))
* Support for chunked text and markdown outputs ([#2174](https://github.com/livebook-dev/livebook/pull/2174))
* (Desktop) Allowed the application to ask for microphone and camera access ([#2177](https://github.com/livebook-dev/livebook/pull/2177))
* Added Remote execution cell ([#2197](https://github.com/livebook-dev/livebook/pull/2197), [#2217](https://github.com/livebook-dev/livebook/pull/2217))
* Support for getting temporary directory tied to runtime lifetime ([#2204](https://github.com/livebook-dev/livebook/pull/2204))
* Button for inserting a branching section ([#2205](https://github.com/livebook-dev/livebook/pull/2205))
* Zero Trust authentication strategy for Tailscale ([#1938](https://github.com/livebook-dev/livebook/pull/1938))
* (Desktop) "Open .livebookdesktop.sh|bat" option to the system tray menu ([#2225](https://github.com/livebook-dev/livebook/pull/2225))
* Support for customizing debounce behaviour on inputs ([#2224](https://github.com/livebook-dev/livebook/pull/2224))
* (Desktop) "New Notebook" option to the system tray menu ([#2247](https://github.com/livebook-dev/livebook/pull/2247))
* Improved audio and image inputs to handle large files by streaming ([#2249](https://github.com/livebook-dev/livebook/pull/2249))
* Docker image with CUDA 12 ([#2255](https://github.com/livebook-dev/livebook/pull/2255))

### Changed

* Changed apps to not reevaluate automatically and require an action on change ([#2066](https://github.com/livebook-dev/livebook/pull/2066))
* Changed attached runtime to establish a hidden connection to avoid clustering errors on OTP25 ([#2110](https://github.com/livebook-dev/livebook/pull/2110))
* Clicking links within iframes to trigger navigation for the whole page ([#2160](https://github.com/livebook-dev/livebook/pull/2160))
* Setup cell output to never be exported ([#2184](https://github.com/livebook-dev/livebook/pull/2184))
* Moved file storage configuration form Settings to Hub page ([#2212](https://github.com/livebook-dev/livebook/pull/2212))
* Bumped Elixir and Erlang versions in the Docker image ([#2254](https://github.com/livebook-dev/livebook/pull/2254))
* **(Breaking)** Changed audio and image input values to include file rather than inline binary data
* Changed the algorithm used by Personal hub for stamping ([#2252](https://github.com/livebook-dev/livebook/pull/2252))

### Fixed

* App menu to fit in the viewport on mobile ([#2092](https://github.com/livebook-dev/livebook/pull/2092))
* Smart cell editor initialization when no language is specified
* Race conditions during apps setup when deploying from a directory ([#2115](https://github.com/livebook-dev/livebook/pull/2115))
* Fixed `require` not propagating across cells in certain cases ([#2134](https://github.com/livebook-dev/livebook/pull/2134))
* Frame updates containing inputs ([#2219](https://github.com/livebook-dev/livebook/pull/2219))

## v0.10

The CHANGELOG for v0.10 releases can be found in the [v0.10](https://github.com/livebook-dev/livebook/tree/v0.10/CHANGELOG.md) branch.
