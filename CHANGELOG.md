## 0.11.0 - 2021-04-14

- Alert user when disconnected from host runtime

### Viewer Updates

- Preview generic objects like strings
  - Allow users to expand / collapse the pr-str version of the object with `e`
- Allow users to render viewers from hiccup (experimental)
  - Checkout the [pwa](https://djblue.github.io/portal/) hiccup example for a
    demo
- Sticky keys / values in inspector map view
  - Align keys / values at the top of a map entry pair instead of the middle
- Include the key in path if the key is selected when calling
  `portal.command/copy-path`
- Add bigint support

### Command Palette Updates

- Add placeholder help to command-palette
- Add tab to command palette shortcuts
  - This prevents tabbing / shift+tabbing focus of selected elements when
    command palette is open.
- Put docs in command palette when an item is active
- Add select-viewer command

### Implementation Updates

- Switch to custom serialization format `cson`
  - Fixes transit error when encoding symbol keys with metadata
    [#11](https://github.com/djblue/portal/issues/11)
  - Fixes issues with older version of transit-clj (0.8.309) brought in with
    ClojureScript

## 0.10.0 - 2021-03-15

- Add command button to improve discoverability (bottom right)
- Expand items without nav-ing into them
  - Allow string expansion
  - Middle click to expand items
- Add preview type to each collection as header

### Item Selection

- Add the concept of a selection
  - Single click to select an item
  - Double click to select + nav into an item
  - Use arrow keys to move selection around
    - Limited to `:portal.viewers/inspector` for now
  - Change the viewer for a selected item

### Vega Viewer

- Initial Vega Viewer
  - Added by [@BrianChevalier](https://github.com/BrianChevalier)
  - In PR [#44](https://github.com/djblue/portal/pull/44)
  - Thanks!

### Material UI Theme

- Added material-ui theme
  - Added by [@rzwiefel](https://github.com/rzwiefel)
  - In PR [#45](https://github.com/djblue/portal/pull/45)
    - Also fixed hex color regex
  - Thanks!

### Window Title

- Adds an option to set a custom window title when opening portal
  - Added by [@coyotesqrl](https://github.com/coyotesqrl)
  - In PR [#42](https://github.com/djblue/portal/pull/42)
  - Thanks!

## 0.9.0 - 2021-01-22

- PWA startup for linux with >1 chrome profile (thanks [@brdloush](https://github.com/brdloush) [#41](https://github.com/djblue/portal/pull/41))
- Adds portal.api/submit target function for add-tap and remove-tap. (thanks [@coyotesqrl](https://github.com/coyotesqrl) [#39](https://github.com/djblue/portal/pull/39))
  - Now users can `(clojure.core/remove-tap #'portal.api/submit)` to stop sending `tap>`  values to portal.
- jvm.launcher: use system prop for path separator (thanks [@bennyandresen](https://github.com/bennyandresen) [#34](https://github.com/djblue/portal/pull/34))
  - fallback to previous value ":"
  - This improves Windows support. Unix uses ":" and Windows uses ";"

## 0.8.0 - 2021-01-06

- Support portal parameter in portal.api/open (thanks [@rfhayashi](https://github.com/rfhayashi) [#32](https://github.com/djblue/portal/pull/32))
  - This makes it possible to "reopen" a closed portal session
- Initial vega-lite & portal charts viewers (thanks [@BrianChevalier](https://github.com/BrianChevalier) [#31](https://github.com/djblue/portal/pull/31))
- Remove shift+c shortcut
- Improve hiccup styles
- Improve copy-as-edn by improving printing
  - Add *print-length* and *print-level*
- Add ratio support (thanks [@pangloss](https://github.com/pangloss) [#28](https://github.com/djblue/portal/issues/28))
- Initial cut of reverse search for commands
  - ctrl + r - Allows for executing previous command
- Fix filter-data for infinite seqs

## 0.7.0 - 2020-12-12

- Bump min babashka version from 0.2.2 to 0.2.4

### New

- Add YAML support (thanks [@yvendruscolo](https://github.com/yvendruscolo) [#27](https://github.com/djblue/portal/pull/27))
- Add a default viewer via the metadata key `:portal.viewer/default` (thanks [@JJ-Atkinson](https://github.com/JJ-Atkinson) [#24](https://github.com/djblue/portal/pull/24))
- Add more data manipulation commands
  - type, deref, datafy, bean
  - slurp for files, URL and URI
  - first, rest, count
  - get, get-in
- Update scrollbar style

### Fixed

- Prevent weird scrolling issue on osx
- Fix command palette css for safari
- Close portal on process shutdown for portal.main
- Find chrome binary from within wsl

### Changed

- Remove auto-datafy of objects pulled by portal
  - You can now run datafy manually via the command palette.

## 0.6.4 - 2020-11-15

- Chunk seqs into groups of 100 for serialization
  - Thanks [@l0st3d](https://github.com/l0st3d) [#17](https://github.com/djblue/portal/issues/17) for the bug report!

## 0.6.3 - 2020-11-02

- Print url when open fails (thanks, [@djomphe-elyada](https://github.com/djomphe-elyada) [#21](https://github.com/djblue/portal/issues/21))

## 0.6.2 - 2020-10-26

- Try to sort maps before rendering
- Switch viewer from command palette
- Add options for specifying port and host of launched UI (thanks, [@justone](https://github.com/justone) [#19](https://github.com/djblue/portal/issues/19))

## 0.6.1 - 2020-10-12

- Ensure active element is always visible
- Add data transformation commands
  - keys
  - vals
  - select-keys
  - select-columns
  - transpose-map
- Add new enumerated selection input

## 0.6.0 - 2020-10-11

- Initial release of command palette and shortcuts
  - `cmd+shift+p` on osx and `ctrl+shift+p` everywhere else
  - Other shortcuts are listed in the command palette next to the command
- Add edn, json, transit and csv viewers
- Only pull the last 25 tap items
- Include version and runtime type in app title
- Switch rpc to web-sockets
  - Switch to http-kit for jvm and bb runtimes
    - Require a minimum version of 0.2.2 for babashka
  - Bundle `ws` for node clojurescript runtime
- Fix nav issue with diff viewer

## 0.5.1 - 2020-09-16

- Fix portal.main

## 0.5.0 - 2020-09-10

- Remove accidentally included example data in release jar
- Table viewer updates
  - prevent text wrapping
  - fix coll of map indexing
  - sticky row and column headers
  - reduce padding to increase info density
- Allow window refresh and zoom in / out for portal.web
- Add pseudo portal atom

For jvm and web:

- `portal.api/open` will return an atom like thing
  - Use `deref` to get the current value viewed in portal
  - Use `reset!` to push a value into portal's history
  - Use `swap!` to apply a fn to the current value then `reset!` the
    result

For node and bb:

- `portal.api/open` will continue to return nil
  - bb support will be added when interfaces can be implemented
  - node support is hard since it can't block synchronously

## 0.4.1 - 2020-08-31

- Fix windows issues (thanks, [@MrGung](https://github.com/MrGung) [#12](https://github.com/djblue/portal/issues/12))
  - Explicitly set string encoding for Windows
- Fix markdown viewer code style
- Fix support for binary data
  - Stop excluding commons-codec/commons-codec

## 0.4.0 - 2020-08-17

- Add support for next release of babashka
  - Requires changes only presently available in master
  - Switch to cheshire for json
- Table viewer updates for map of maps (thanks, [@BrianChevalier](https://github.com/BrianChevalier) [#9](https://github.com/djblue/portal/pull/9))
- Slim down dependencies
- Expose theme setting via `portal.api/open`

## 0.3.1 - 2020-08-07

- Prevent re-initialization of web portal on page reload
- Fix portal on node when it can't find chrome
- Explicitly return nil from api functions

## 0.3.0 - 2020-08-05

- First cut of `portal.web`, the browser specific api
  - Mirrors portal.api for jvm and node
  - Exported for use in js dev console as well
    - `portal.web.open()`
    - `portal.web.close()`
    - `portal.web.clear()`
    - `portal.web.tap()`
- Fix nav to nil exception (thanks, [@rzwiefel](https://github.com/rzwiefel) [#8](https://github.com/djblue/portal/pull/8))
- Spawn chrome app in future to prevent blocking
- Initial keyboard support
  - Tab to move focus to next nav target
  - Confirm to nav into currently focused item
- Add lazy rendering to map, coll and table viewer

## 0.2.1 - 2020-08-02

- Fix issue with finding chrome in windows
- Fix issue with transit, tagged values and metadata

## 0.2.0 - 2020-08-02

- Add drag and drop feature to portal
- Add lines numbers to text viewer
- Filter lines in text viewer
- Lazy render text viewer
- Add forward navigation
- Preserve viewer selection on navigation
- Preserve filter text on navigation
- Disable navigation buttons when not possible

## 0.1.1 - 2020-07-26

- Special case rendering urls encoded as strings
- Simplify filtering
- Improve tree viewer complex key handling

## 0.1.0 - 2020-07-24

- Initial release

