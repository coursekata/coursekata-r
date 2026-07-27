# coursekata (development version)

- `library(coursekata)` no longer attaches `fivethirtyeightdata`, and the drat repository that existed only to serve it is gone. No CourseKata teaching content uses any of its 19 exclusive datasets, and it accounted for roughly 63 MiB of the browser-based Playground bundle -- 93% of the cost of shipping every suggested data package. `fivethirtyeight` itself is unaffected; if you use a dataset that lived only in `fivethirtyeightdata`, install it with `install.packages("fivethirtyeightdata", repos = "https://fivethirtyeightdata.github.io/drat/")`.
- Relicensed from AGPL-3 to GPL-3-or-later. The Affero clause obliges anyone who runs modified code as a network service to publish their changes, which is aimed at hosted applications rather than at a package people install and teach with. GPL keeps the copyleft that matters here -- modifications stay open -- without attaching a condition that has no bearing on how `coursekata` is actually used.
- Require `mosaic` 1.10.2 or later, which raises the minimum R version to 4.1. Earlier `mosaic` releases are broken by `rlang` 1.2.0: `do(1000) * b1(shuffle(...))` returns the same value a thousand times instead of a sampling distribution, with no error to warn you. Since `coursekata` attaches `mosaic` for you, an old copy meant silently wrong sampling distributions in your own work.
- New documentation site at <https://coursekata.github.io/coursekata-r/>, with a curated function reference and the JupyterLite demo moved to <https://coursekata.github.io/coursekata-r/lite/>, which now opens on the getting-started notebook.
- Fix `gf_resid()`, `gf_square_resid()`, `gf_resid_fun()`, and `gf_square_resid_fun()` overlays misaligning with jittered points: jitter positions are now pinned to the plot itself, so they are stable across repeated builds and chained calls, and the user's random seed is no longer reset.
- Fix `gf_square_resid()` drawing rectangles instead of squares on jittered plots. The vertical side was measured from the displayed (jittered) position, but the horizontal side was measured against the model's unjittered response, so the two sides disagreed by the size of the jitter. The whole point of the plot is to show squared error as area, so a shape that was not actually square undermined it.
- Fix jitter pinning leaking out of the plot being drawn. A layer written as `geom_jitter()` or `position = "jitter"` shares one position object with every other such layer in the session, so pinning it in place fixed the jitter for unrelated plots too.
- `gf_sd_ruler()` now works on histograms, drawing a horizontal ruler from the mean to mean + SD along the baseline.
- `gf_sd_ruler()` now reports which variable it could not use, instead of failing inside `sd()` on a categorical outcome or silently drawing nothing when handed a name that is not in the data.
- Fix variable names in `gf_sd_ruler()`'s `y` and `x` arguments. Bare (unquoted) names previously errored despite being documented, and a variable holding a column name stopped resolving when that was fixed; all three spellings now work.
- Rebuild the hex sticker from the official brand artwork so it renders identically everywhere. The previous one set its letters as live text in a licensed font it could not embed, which fell back to a different typeface on most machines and to missing-glyph boxes in the favicons.
- `gf_squaresid()` is no longer deprecated: it remains a fully supported alias of `gf_square_resid()`. With our appreciation to Tyler Haslam (@TH4SL4M), the Utah high school teacher whose efforts shaped the residual and squared-residual visualizations -- including working out how `gf_resid()` and `gf_square_resid()` handle jitter plots, and the insight to emphasize the area of the squares rather than their outline -- and who requested the function by this name.
- `gf_resid()` and `gf_square_resid()` now say when the model was fit on fewer observations than the plot draws, naming both counts. `lm()` drops rows with missing values, so a model fit on a variable with any gaps does not line up with the plot's points; that mismatch used to surface as "arguments imply differing number of rows", which says nothing about what went wrong or how to fix it.
- `gf_model()` now errors when an aesthetic is mapped to a variable that is not one of the model's predictors, instead of silently dropping the mapping. A mapping the model could not honor used to just vanish from the plot without a word, which is a hard thing to debug in a notebook.
- Expand reference examples for the model visualization and distribution functions with textbook-style, pedagogy-focused examples.

# coursekata 0.19.2

- Add experimental visualization functions: `gf_resid_fun()`, `gf_square_resid_fun()`, `gf_sd_ruler()`, `gf_squareplot()`, `show_cutoffs()`, and `outer()`.
- Rename `gf_squaresid()` to `gf_square_resid()` with deprecation warning for the old name.
- Add `coursekata.check_missing` option to control the missing-package install prompt, with automatic suppression on Emscripten/WASM environments.
- Add tidyverse-style conflict reporting on startup, showing which coursekata exports mask objects from other packages.
- Relax ggplot2 version constraint to `>= 3.5.2` to support Emscripten/WASM environments.
- Improve performance of estimate extraction functions by bypassing `supernova` when using default arguments.

# coursekata 0.19.1

- Upgrade to ggplot2 4.0.0 compatibility.

# coursekata 0.19.0

- Add `gf_resid()` and `gf_squaresid()` functions for residual and squared residual plots that layer onto `ggformula::gf_point()` plots.

# coursekata 0.18.1

- Add alternate name `Fingers$Gender` to `Fingers` dataset to prevent awkward naming in exercises.

# coursekata 0.18.0

- Remove dependency on `pak`. `pak` was initially used to manage and parse dependencies, but itself depends on `curl`. `curl` is not available on all platforms (e.g. WASM), so we have removed the dependency on `pak` and opted for pure R where possible (or `remotes` which has a pure R fallback).
- Add `FoodQuality` to `TipExperiment` dataset.
- Various CI improvements: update for compatibility with more `rhub` platforms, don't run `vdiffr` tests on CI, allow tests to run in parallel.
- Fix CRAN note by adding missing package anchors to link targets

# coursekata 0.17.0

- Make CRAN compatible by removing `Remotes` field from DESCRIPTION
- Update visual snapshot tests
- Improve handling of non-CRAN package installs

# coursekata 0.16.1

- Ignore some tests on CI where `vdiffr` gave erroneous results

# coursekata 0.16.0

- Make fivethirtyeight and Lock5withR required packages using the Remotes field (this is non-standard, but we aren't on CRAN anyway)
- Update to ggplot 3.5.0, which required some small changes to the theme parameters

# coursekata 0.15.0

- Change name of dataset `Fingers.messy` to `FingersMessy`

# coursekata 0.14.1

- Reduce calls to `pak::pkg_status()` to improve startup time
- Address issue where `require(lib.loc = ...)` was sometimes being passed `NA`
- Appropriately skip actions that require the user when running in non-interactive mode (and add related tests)

# coursekata 0.14.0

- Remove deprecated `gf_model_old()` function

# coursekata 0.13.1

- Fix issue where startup message was not being displayed
- Make dependent startup package messages visible by default
- Add `coursekata.quiet` option to suppress startup messages
- Document `coursekata.quickstart` and `coursekata.quiet` options in README
- Trim unused packages in preparation for CRAN submission

# coursekata 0.13.0

- Add `coursekata.quickstart` option, which can reduce load times significantly.
- Reduce expensive lookups when attaching packages, further reducing load times.
- Re-introduce `gf_model` tests for density plots now that upstream is fixed.
- Add `test_fit()` simple model stats to help teachers evaluate student models.

# coursekata 0.12.0

- Remove `sse()`, `ssm()`, `ssr()`, `SSE()`, `SSM()`, `SSR()` functions: they conflict with `Metrics` package.
- Reverse package load order (load most important last so that they mask others)

# coursekata 0.11.0

- Add `palmerpenguins` and `World`

# coursekata 0.10.0

- Add [`Metrics` package](https://CRAN.R-project.org/package=Metrics)
- Remove `zargle`

# coursekata 0.9.4

- Double digit `fevdata$AGE` values were truncated by first character. This has been fixed.

# coursekata 0.9.3

- Fix issue installing missing packages where package could not be found.

# coursekata 0.9.2

- Use `pak` for package management.

# coursekata 0.8.0

- Add `game_data` dataset.

# coursekata 0.7.1

- Add argument forwarding for installs.

# coursekata 0.6.3

Patch release to resolve `R CMD CHECK` failures.

- Mainly migrates use of `size` for line widths to `linewidth`.
