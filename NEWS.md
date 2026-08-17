# coursekata (development version)

- A residual drawn over a jittered plot lands on the jittered points, on a plot with
  any number of layers. The residual now declares the same jittered position the points
  carry -- two layers sharing a seed compute the same offsets independently -- rather
  than reaching back into the plot to seed and replay the points layer's own position.
  The old approach quietly stopped working under ggplot2 4, where the copy it made lost
  its width and offset nothing, so `gf_jitter() %>% gf_model() %>% gf_resid()` drew every
  point on its group's centre. Adding a residual to an unseeded jitter now returns a plot
  whose jitter is fixed, so a second overlay lands on the same dots; the plot you passed
  in is left as it was.
- A named argument that is not a parameter of the layer is now discarded without a
  word, at the call and at the draw. That is how every `gf_` function behaves --
  `ggformula` builds the layer with parameter checking off -- and `gf_model()`,
  `gf_sd_ruler()`, `gf_squareplot()` and the five residual functions are ordinary
  `gf_` functions now, so they behave that way too. That includes the names that
  moved out of their own signatures: a name this package used to take is not
  special, and is ignored like any other name the layer does not recognize. Where
  each one went is on the function's reference page, which is where a reader looks
  for it. A genuine misspelling is the same story: `gf_squareplot(~Thumb, data = Fingers,
  binwidht = 5)` draws a plot that ignores it, while a correctly spelled
  neighbour on the same call is applied. If a parameter appears to do nothing,
  check its spelling first; `ggplot2::ggplot_build(p)$data[[1]]` shows what the
  layer actually received.
- `gf_squareplot()` is now a real ggformula layer. It carries the data it was given
  and maps `x`, so it facets with `~ x | group`, accepts a data frame piped into it,
  takes a mapped `fill`, can be added to a plot you already have, and can be read by
  `gf_model()` and `gf_sd_ruler()` -- none of which worked before, because the
  function built an empty plot and handed the layer a renamed one-column frame.
  `~log(Thumb)` and any other expression now plots what it says.
- `gf_squareplot()` bins the values that are actually drawn. It used to choose the
  binwidth and the origin from the raw data when the call was made, then hand them to
  a stat that runs after the scales have transformed everything, so `scale_x_log10()`
  drew four values spanning three orders of magnitude as a single column on an axis
  running to 10^33. The stat decides now.
- `gf_squareplot()`'s scale and annotation arguments have moved to the scales and
  annotations that own them: `xrange` is `gf_lims(x = )`, `xbreaks` is
  `scale_x_continuous(breaks = )`, `mincount` is `expand_limits(y = )`, and `show_mean`
  and `show_dgp` are `%>% show_mean()` and `%>% show_dgp()`. `auto_subdivide` is gone
  because what it did is now what happens anyway: it was the opt-in that split a bin of
  more than 75 observations into sub-columns so the squares stayed countable, and squares
  now stay countable at any size without being asked. Passing one of the old names now
  does nothing, the same as any other name the layer does not recognize.
- A squareplot can be drawn on a transformed count axis, with
  `%>% gf_refine(coord_transform(y = "sqrt"))`. Each square still spans exactly one
  count, so the squares are drawn shorter the higher up the stack they sit --
  which is the point: the distortion is what shows that the unit changes as the
  scale climbs. The borders between squares are fitted per square rather than once
  for the layer, so the compressed ones at the top are not swallowed by their own
  outlines. A `scale_y_*()` transformation is still refused, because a scale
  transforms the counts before the squares are built and the squares would be
  drawn in one space and labelled in another; the refusal now names the
  `coord_transform()` spelling instead of saying a transformed axis is impossible.
  A discrete y scale is still refused, and now says why: there is no count for a
  square to be one of.
- A squareplot's bins are a histogram's bins, argument for argument: `bins`,
  `binwidth`, `center`, `boundary`, `closed`, `breaks` and `pad` all mean exactly
  what they mean on `gf_histogram()`, because a squareplot now bins through the same
  code a histogram does. The default grid moved as a result -- bins are centered on
  round numbers rather than starting on them -- and a maximum sitting on a bin edge
  no longer gets a column of its own past the end of the data.
- `bins`, `center`, `boundary`, `closed` and `breaks` are now named parameters of
  `gf_squareplot()`, not merely accepted through `...`, so they appear in its usage,
  in autocomplete, and in the help sheet a bare `gf_squareplot()` call prints.
- A discrete x on a squareplot is counted, not binned: a factor, character or logical
  x now gets one column per level, centered on its own tick, the way `gf_bar()`
  positions its bars, instead of a column of level numbers sitting half a step past
  the labels they belong to. A factor with more than 51 levels no longer merges
  neighbouring levels into shared columns. A binning argument passed alongside a
  discrete x -- `binwidth`, `bins`, `center`, `boundary`, `closed`, `breaks`, `pad` --
  now warns that it has no effect, rather than being silently discarded.
- `color` colors the bar on a squareplot, which is the only thing it ever affected,
  and an explicit `"black"` is now drawn black rather than silently redrawn as the
  default grey.
- The undocumented `print.gf_squareplot()` method is gone. It existed to swallow a
  warning the old constructor caused by building a plot with no complete layer of its
  own; a real ggformula layer never triggers that warning, so `gf_squareplot()` now
  prints like any other plot.
- A factor keeps its levels on a squareplot's x axis, so a level nothing landed in
  still holds its place instead of being closed over by its neighbours -- which also
  moved the columns that were drawn, not just the ticks.
- When squares are added to an existing plot, its x scale and any identity-continuous
  y scale are preserved. A transformed or discrete y scale is refused because a square
  cannot stay exactly one count tall on it; the check works whether that scale is added
  before or after `gf_squareplot()`.
- The squareplot no longer imposes its own theme. It uses the package theme like every
  other plot, so its panel and gridlines now match the plots beside it, and adding
  squares to a plot you have themed yourself leaves your theme alone.
- `GeomSquareplot` can draw a bin as the bar its squares add up to, through a `bars`
  parameter taking `"none"`, `"outline"` or `"solid"`, with `bar_color` and
  `bar_linewidth` to style it. All three are one layer on one set of bins, and the bar
  is derived from the squares rather than binned again, so it cannot land anywhere the
  squares did not. A solid bar shows its bin's group composition too: a mapped `fill`
  stacks in the bar the way it stacks in the squares.
- New `show_mean()` and `show_dgp()`: pipe a plot of one distribution through them to mark
  its mean, or to frame it with the process that generated it -- the population model on a
  top axis, the sample estimate below the plot, and a marker at the null hypothesis. They
  describe a distribution rather than one particular way of drawing one, so the same frame
  now goes on a `gf_histogram()` or a `gf_dotplot()` of the same data, and each layer they
  add carries a name so it can be found and changed afterwards. On a faceted plot
  `show_mean()` draws each panel's own mean. `show_dgp()` raises the count axis to make
  room for the population band rather than hanging it in the margin: countable squares size
  the separator between them from the fraction of the panel they fill, so a band drawn
  outside the panel silently redraws every square 39% taller.
- `gf_sd_ruler()` is now a real ggformula layer. It takes a formula rather than
  `y` and `x` arguments -- `gf_sd_ruler(Thumb ~ Height)` where you used to write
  `gf_sd_ruler(y = Thumb, x = Height)` -- and with that comes everything the other
  `gf_` functions already had: `y ~ x | group` faceting, data-first piping,
  `title=`/`xlab=`/`ylab=`, and a plot whose aesthetics live on a layer rather
  than on the plot. `y` and `x` are names the layer no longer recognizes and are
  ignored like any other; `size` still works and says to write `linewidth`.
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
- `gf_model()` draws a group model as a plain mark at each group mean rather than as an errorbar. An errorbar glyph reads as uncertainty -- a standard error, a confidence interval -- to students who are weeks away from meeting interval estimates, while a group model claims a single value per group and says nothing about how sure of it you should be. The mark is drawn at the same place, the same width and the same color as before; what goes away is the eight-point path with two identical caps around a zero-length stem, and the `ymin`, `ymax` and `flipped_aes` columns that described an interval that was never there. A group mark drawn with `alpha` is now as translucent as you asked for, instead of nearly twice as dark where the two identical caps overlapped.
- `gf_model()` now says a model's outcome has to be numeric, instead of letting `lm()` coerce it and report `NA/NaN/Inf in 'y'`. The carefully worded refusal was already written; it just sat after the fit, where nothing with a categorical outcome could ever reach it. On the one path that did reach it -- a logical outcome, which `lm()` accepts -- it reported the type as "character", because it read the class of the variable's name rather than of the variable.
- `gf_model(size = )` no longer trips ggplot2's `size`-is-now-`linewidth` deprecation warning, which told the reader that coursekata had done something wrong and asked them to file an issue. The value was correctly folded into `linewidth` and then also passed along under its old name, where the geom accepted and discarded it. A `size` mapped by the plot underneath also used to overwrite a `linewidth` given explicitly to `gf_model()`; the explicit one now wins.
- `gf_model()`, `show_cutoffs()` and the residual overlays now read a plot through one reader, which finds a variable whether it was mapped on the plot or on the plot's first layer, and finds the data there too. Plots built with ggformula always map at plot level, so this changes nothing about the documented pipelines. For `show_cutoffs()` and the residual overlays it changes how they read rather than what they can draw: a plot written as `ggplot(data) + geom_point(aes(x, y))` already worked. What it buys is `gf_model()`, which used to refuse that plot outright and now draws an intercept model on it. A fit line or a group mark still needs its outcome mapped on the plot rather than on a layer beneath it -- those two shapes leave the outcome's axis free and inherit it, so there is nothing for them to inherit from -- and that is now refused by name, at the call. The residual overlays' refusal, for a plot that really has no x or y anywhere, names the axis that is absent.
- The five residual functions -- `gf_resid()`, `gf_square_resid()`, `gf_squaresid()`,
  `gf_resid_fun()` and `gf_square_resid_fun()` -- are now built the same way every other
  `gf_` layer is, which closes the last place this package had two ways of making the
  same kind of thing. Nothing they draw has changed. What arrives with it is the rest of
  the family's behavior: a bare call prints its own help, `title=`/`xlab=`/`ylab=` reach
  the plot, and a first argument that is not a plot says so rather than failing with
  `attempt to apply non-function`. A call with no model, or no function, names what it
  needs instead of reporting R's own missing-argument error.
- The residual functions take the plot as `object`, the name every other `gf_` function
  uses for it, and their remaining arguments have to be named. The released signatures
  put `linewidth` third on `gf_resid()`, and `aspect` and `alpha` third and fourth on
  `gf_square_resid()`; those positions belong to the arguments every generated layer
  carries there, so write `gf_resid(p, model, linewidth = 0.5)` and
  `gf_square_resid(p, model, aspect = 1)`. A value left in the third position is refused
  by ggformula, in the words it uses for any other `gf_` function.
- `gf_resid()` and `gf_square_resid()` now measure the residual along whichever axis the plot puts the model's outcome on. A plot of `Thumb ~ Height` with `lm(Height ~ Thumb)` over it drew every segment vertically, to a predicted *height* read off as though it were a thumb length, with nothing to say so; the squares squared that same wrong distance. The residual now runs across x for a model of the x variable, its square turns with it, and the fitted end lands on the line `gf_model()` draws for the same model.
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
- `gf_resid()` and `gf_square_resid()` no longer refuse a model that was fit on fewer rows than the data holds. `lm()` drops rows with missing values, and those are the same rows the plot cannot draw a point for, so the two always agreed about what is on screen -- but the check counted rows before the plot dropped them and reported 157 points against 128 fitted values for a plot that draws 128 of each. The residual is now drawn for every point that is drawn, and the error that remains is the one worth raising: the model uses a variable the plot's data does not have, named.
- Fix `gf_square_resid()` and `gf_square_resid_fun()` drawing every square in every facet panel. The squares were built as a standalone table of coordinates that carried none of the data's other columns, so ggplot2 had nothing to split them by and repeated all of them in each panel; their side was also scaled against the first panel's ranges wherever they landed. Each square is now drawn where its observation is.
- New `StatResid`, `GeomResid` and `GeomSquareResid` exports. The residual overlays are real ggplot2 layers now rather than a pre-rendered table of coordinates, so they follow the plot they are added to -- through facets, through a jitter, and through rows the model dropped -- and you can use them in a plot you are assembling yourself.
- Fix `gf_model()` failing with `'from' must be a finite number` when a predictor on an axis has any missing values. The grid of values the model is drawn over was spanned between the predictor's smallest and largest value without excluding the gaps, so a single missing observation made both ends undefined and nothing was drawn at all -- including for a model `lm()` had fit perfectly well on the rows that were complete.
- `gf_model()` now errors when an aesthetic is mapped to a variable that is not one of the model's predictors, instead of silently dropping the mapping. A mapping the model could not honor used to just vanish from the plot without a word, which is a hard thing to debug in a notebook.
- Expand reference examples for the model visualization and distribution functions with textbook-style, pedagogy-focused examples.
- `gf_squareplot()` now names the variable it could not use. A misspelled column, a data-first pipe, a character column and a date column all reported the same ``` `x` must be numeric. ```, and a formula holding an expression such as `~log(Thumb)` silently plotted the untransformed variable and labelled the axis with it. `na.rm = FALSE` now says it is unsupported instead of failing inside `range()` or shipping a rectangle at `NA`. A two-sided formula such as `gf_squareplot(y ~ x)` used to silently plot `x` and discard `y` with no error; it now says the formula must be one-sided.
- `gf_squareplot()` keeps drawing countable squares on large samples. Above 75 observations in a bin it used to replace every square with a solid bar, so the 2000-observation example in its own documentation drew 27 bars and not one countable square. The separator between squares is now fitted to the squares -- at 2000 observations a square is about 1.2 pt tall while the old separator was 1.4 pt wide, so each square erased itself. A `linewidth` set explicitly on the layer is still honored, now with a warning when the border is wide enough to hide the observations behind it.
- New `StatSquareplot` and `GeomSquareplot` exports. `gf_squareplot()` now draws through a real ggplot2 stat and geom instead of assembling rectangles itself, and both are exported so you can put countable squares into a plot you are building yourself -- including with a mapped `fill`, which stacks its groups within each bin rather than drawing them on top of one another.
- `show_cutoffs()` now reads its fill aesthetic the way R reads any call. `fill = ~middle(Thumb)` was told it needed at least two arguments even though `prop` is documented to default to .95; named arguments in any other order, such as `~middle(prop = .9, x = Thumb)` or `~middle(Thumb, greedy = FALSE, prop = .9)`, were still read by position and failed on whatever landed in the third slot; and `~coursekata::middle(Thumb, .95)` crashed on a length-3 coercion rather than being recognized as `middle()`. The call is matched against the real function's formals now, so naming arguments, reordering them, leaving them at their documented defaults, and qualifying the call with `coursekata::` all behave as they do everywhere else in R. `greedy` is honored too, having previously been ignored.
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
