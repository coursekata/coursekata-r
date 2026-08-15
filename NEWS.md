# coursekata (development version)

- `gf_sd_ruler()` is now a real ggformula layer. It takes a formula rather than
  `y` and `x` arguments -- `gf_sd_ruler(Thumb ~ Height)` where you used to write
  `gf_sd_ruler(y = Thumb, x = Height)` -- and with that comes everything the other
  `gf_` functions already had: `y ~ x | group` faceting, data-first piping,
  `title=`/`xlab=`/`ylab=`, and a plot whose aesthetics live on a layer rather
  than on the plot. `y` and `x` now say to use the formula instead of being
  quietly ignored, and `size` still works and now says to write `linewidth`.
- Fix `gf_sd_ruler()` placing the ruler where no observations are drawn. On a
  categorical x it derived positions in order of first appearance while the axis
  orders them alphabetically, so on unbalanced data `where = "median"` put the
  ruler over one group and the median over another.
- `gf_sd_ruler()` on a faceted plot now measures each panel's own data. It used
  to compute one ruler from the pooled data and stamp the same segment into every
  panel, which is the one thing a facet exists to avoid.
- `gf_sd_ruler()` measures the values the plot draws. A transformed axis or a
  computed mapping such as `~log(Thumb)` used to be refused outright; both are now
  measured in the space they are drawn in.
- `gf_sd_ruler()` draws one ruler per panel, so an aesthetic mapped on the call --
  `gf_sd_ruler(color = ~Sex)` -- is refused and points at `y ~ x | Sex`. It used to
  be discarded without a word.
- New `StatSdRuler` export. `gf_sd_ruler()` now draws through a real ggplot2 stat
  instead of computing the ruler by hand, and is exported so you can put a
  standard deviation ruler into a plot you are building yourself.
- `gf_model()` is now built the same way every other `gf_` layer is, so it behaves like one. Calling it with no arguments prints its own help instead of reporting a missing argument, and calling it on a plot with no model says which model it needs and names both ways of giving one -- a fitted `lm()` or `aov()`, or the formula for one -- rather than surfacing R's own missing-argument error. `show.help = TRUE` now prints that same help on a plot that already has a model to check, rather than running the check anyway and reporting its result instead of the help you asked for. Everything it draws is unchanged.
- `gf_model()` draws a model whose predictor is transformed. `gf_model(lm(Thumb ~ log(Height)))` over a plot of `Thumb ~ Height`, and the same claim written in place as `gf_model(Thumb ~ log(Height))`, were both refused as using variables the plot does not have, because `log(Height)` was compared against the plot's columns as though it were the name of one. The prediction grid is now built over the columns a term is made of, which is what `predict()` needs, and drawn against the plot's own mapping. A transformed *outcome* is still refused, and now says so in those terms rather than failing while computing aesthetics.
- `gf_model()` refuses a one-sided formula by name. `gf_model(~flipper_length_m)` used to fail inside `lm.fit()` with `incompatible dimensions`; it now says the model has no outcome and shows where to write one.
- `gf_model()` says at the call, rather than while drawing, that a fit line or a group mark needs its outcome mapped by the plot rather than by a layer underneath it. Those two shapes leave the outcome's axis free and inherit it, which is what lets a flipped plot draw correctly without any orientation logic; when there is nothing to inherit, ggplot2 used to report a missing `y` from deep inside the build.
- `gf_model()` draws a group model as a plain mark at each group mean rather than as an errorbar. An errorbar glyph reads as uncertainty -- a standard error, a confidence interval -- to students who are weeks away from meeting interval estimates, while a group model claims a single value per group and says nothing about how sure of it you should be. The mark is drawn at the same place, the same width and the same colour as before; what goes away is the eight-point path with two identical caps around a zero-length stem, and the `ymin`, `ymax` and `flipped_aes` columns that described an interval that was never there. A group mark drawn with `alpha` is now as translucent as you asked for, instead of nearly twice as dark where the two identical caps overlapped.
- `gf_model()` now says a model's outcome has to be numeric, instead of letting `lm()` coerce it and report `NA/NaN/Inf in 'y'`. The carefully worded refusal was already written; it just sat after the fit, where nothing with a categorical outcome could ever reach it. On the one path that did reach it -- a logical outcome, which `lm()` accepts -- it reported the type as "character", because it read the class of the variable's name rather than of the variable.
- `gf_model(size = )` no longer trips ggplot2's `size`-is-now-`linewidth` deprecation warning, which told the reader that coursekata had done something wrong and asked them to file an issue. The value was correctly folded into `linewidth` and then also passed along under its old name, where the geom accepted and discarded it. A `size` mapped by the plot underneath also used to overwrite a `linewidth` given explicitly to `gf_model()`; the explicit one now wins.
- `show_cutoffs()` now draws its markers on a plot whose count axis has been transformed.
  The triangles hang below the axis, which means a negative count, and `scale_y_sqrt()` or
  `scale_y_log10()` has no such value to offer: both markers came out at `NA` and both
  dashed uprights lost their lower end, with eight warnings, and the labels, which did
  survive, were placed by treating a square root as a count and landed a third of the way up
  the panel where they belong two thirds of the way up. None of those heights was ever a
  count -- they are fractions of the panel -- so they are now measured in the space the axis
  is drawn in and handed over as positions no scale is asked to represent.
- Adding cutoff markers no longer changes the histogram they are added to. The markers were
  placed as data, so the count axis stretched to enclose a triangle that was meant to sit
  outside the panel: the bars were redrawn almost 6% shorter, the triangle ended up inside
  the panel after all, and calling `show_cutoffs()` on a plot that already had markers moved
  them, because the second call measured the axis the first one had stretched. On a
  `coord_flip()` histogram it was worse -- the heights were measured against the range of
  the variable on the vertical axis rather than the counts, so the labels were placed off
  the end of the count axis and the bars were redrawn at under two thirds of their length.
  The reference figure for `middle(Thumb, .95)` changes accordingly: the markers are in the
  same place relative to the bars, and the empty band beneath the axis is gone.
- Fix `middle()`, `tails()`, and `outer()` dropping one value from each tail when the tail's exact size is a whole number. `middle(x, .90)` on 20 values highlighted all twenty instead of eighteen. The tail proportion is computed as `(1 - prop) / 2`, which cannot represent .05 exactly, so the count landed a fraction below the whole number and rounded down. `prop = .95` was unaffected, which is why this survived.
- `library(coursekata)` no longer attaches `fivethirtyeightdata`, and the drat repository that existed only to serve it is gone. No CourseKata teaching content uses any of its 19 exclusive datasets, and it accounted for roughly 63 MiB of the browser-based Playground bundle -- 93% of the cost of shipping every suggested data package. `fivethirtyeight` itself is unaffected; if you use a dataset that lived only in `fivethirtyeightdata`, install it with `install.packages("fivethirtyeightdata", repos = "https://fivethirtyeightdata.github.io/drat/")`.
- Relicensed from AGPL-3 to GPL-3-or-later. The Affero clause obliges anyone who runs modified code as a network service to publish their changes, which is aimed at hosted applications rather than at a package people install and teach with. GPL keeps the copyleft that matters here -- modifications stay open -- without attaching a condition that has no bearing on how `coursekata` is actually used.
- Require `mosaic` 1.10.2 or later, which raises the minimum R version to 4.1. Earlier `mosaic` releases are broken by `rlang` 1.2.0: `do(1000) * b1(shuffle(...))` returns the same value a thousand times instead of a sampling distribution, with no error to warn you. Since `coursekata` attaches `mosaic` for you, an old copy meant silently wrong sampling distributions in your own work.
- Require `ggformula` 0.12.0 or later. Two separate reasons, both measured rather than assumed. On R 4.4 and later, `ggformula` 0.10.1's own `create_extras_and_dots()` fails every `gf_*` call, `coursekata`'s included, with `unique() applies only to vectors` -- a bug in `ggformula` itself, fixed in 0.10.2. Beyond that, `coursekata` now builds several layers with `ggformula::layer_factory()`, handing it this package's own `Stat` and `Geom` objects; only 0.12.0 and later capture those unevaluated, and earlier versions resolve them while `coursekata`'s namespace is still being built, so the package cannot be installed at all.
- Reference examples no longer build their data with `do()` and `shuffle()`. They construct the same distributions with base R, so a reference page cannot break because an attached package changed the shape of what it returns. The full shuffle-and-estimate workflow is now taught in the [sampling distributions guide](https://coursekata.github.io/coursekata-r/articles/sampling-distributions.html), alongside a new [model visualization guide](https://coursekata.github.io/coursekata-r/articles/model-visualization.html).
- New documentation site at <https://coursekata.github.io/coursekata-r/>, with a curated function reference and the JupyterLite demo moved to <https://coursekata.github.io/coursekata-r/lite/>, which now opens on the getting-started notebook.
- Fix `gf_resid()`, `gf_square_resid()`, `gf_resid_fun()`, and `gf_square_resid_fun()` overlays misaligning with jittered points: jitter positions are now pinned to the plot itself, so they are stable across repeated builds and chained calls, and the user's random seed is no longer reset.
- Fix `gf_square_resid()` drawing rectangles instead of squares on jittered plots. The vertical side was measured from the displayed (jittered) position, but the horizontal side was measured against the model's unjittered response, so the two sides disagreed by the size of the jitter. The whole point of the plot is to show squared error as area, so a shape that was not actually square undermined it.
- Fix jitter pinning leaking out of the plot being drawn. A layer written as `geom_jitter()` or `position = "jitter"` shares one position object with every other such layer in the session, so pinning it in place fixed the jitter for unrelated plots too.
- `gf_sd_ruler()` now works on histograms, drawing a horizontal ruler from the mean to mean + SD along the baseline.
- `gf_sd_ruler()` now reports which variable it could not use, instead of failing inside `sd()` on a categorical outcome or silently drawing nothing when handed a name that is not in the data.
- Fix variable names in `gf_sd_ruler()`'s `y` and `x` arguments. Bare (unquoted) names previously errored despite being documented, and a variable holding a column name stopped resolving when that was fixed; all three spellings now work.
- Rebuild the hex sticker from the official brand artwork so it renders identically everywhere. The previous one set its letters as live text in a licensed font it could not embed, which fell back to a different typeface on most machines and to missing-glyph boxes in the favicons.
- `gf_squaresid()` is no longer deprecated: it remains a fully supported alias of `gf_square_resid()`. With our appreciation to Tyler Haslam (@TH4SL4M), the Utah high school teacher whose efforts shaped the residual and squared-residual visualizations -- including working out how `gf_resid()` and `gf_square_resid()` handle jitter plots, and the insight to emphasize the area of the squares rather than their outline -- and who requested the function by this name.
- `gf_model()` now errors when an aesthetic is mapped to a variable that is not one of the model's predictors, instead of silently dropping the mapping. A mapping the model could not honor used to just vanish from the plot without a word, which is a hard thing to debug in a notebook.
- Expand reference examples for the model visualization and distribution functions with textbook-style, pedagogy-focused examples.
- `show_cutoffs()` now reads its fill aesthetic the way R reads any call. `fill = ~middle(Thumb)` was told it needed at least two arguments even though `prop` is documented to default to .95; named arguments in any other order, such as `~middle(prop = .9, x = Thumb)` or `~middle(Thumb, greedy = FALSE, prop = .9)`, were still read by position and failed on whatever landed in the third slot; and `~coursekata::middle(Thumb, .95)` crashed on a length-3 coercion rather than being recognised as `middle()`. The call is matched against the real function's formals now, so naming arguments, reordering them, leaving them at their documented defaults, and qualifying the call with `coursekata::` all behave as they do everywhere else in R. `greedy` is honoured too, having previously been ignored.
- `show_cutoffs()` now puts its markers on values the fill actually shades. The marker positions reimplemented the arithmetic that decides how many observations fall in a tail rather than asking for it, and the two disagreed whenever the tail's exact size was a whole number: with `upper(x, .05)` on 200 observations the triangle sat on the 190th value while the shading started at the 191st. They come from the same count now, so they cannot drift apart. The labels also compared floating-point values against literals and never matched, so the flagship example annotated its cutoffs `0.025` where a textbook writes `.025`.
- `show_cutoffs()` now refuses a plot it cannot mark instead of guessing at it. An `x` aesthetic holding an expression rather than a bare variable surfaced a raw `rlang` coercion error, and a plot without cartesian axes -- `coord_polar()`, say -- fell back on an invented y axis running to 30 and drew the markers at heights that meant nothing. Both are named errors now, raised before any position is computed.
- `gf_model()` now documents the model you can write in place. Alongside a model already fit by `lm()` or `aov()`, it takes the formula for one -- `gf_model(body_mass_kg ~ species)` -- and fits it against the data the plot was built from, with `body_mass_kg ~ NULL` for the empty model. That has always worked, and the help page promised `lm()` or `aov()` only, so the shortest way to draw a claim was also the least discoverable one. The outcome still has to be named: `~species` would describe predictors and no claim, and the only way to draw it would be to guess the outcome off the axes, which is what `gf_lm()` already does.

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
