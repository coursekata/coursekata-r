# `coursekata` change log

## coursekata 0.17.0

- Make CRAN compatible by removing `Remotes` field from DESCRIPTION
- Update visual snapshot tests
- Improve handling of non-CRAN package installs

## coursekata 0.16.1

- Ignore some tests on CI where `vdiffr` gave erroneous results

## coursekata 0.16.0

- Make fivethirtyeight and Lock5withR required packages using the Remotes field (this is non-standard, but we aren't on CRAN anyway)
- Update to ggplot 3.5.0, which required some small changes to the theme parameters

## coursekata 0.15.0

- Change name of dataset `Fingers.messy` to `FingersMessy`

## coursekata 0.14.1

- Reduce calls to `pak::pkg_status()` to improve startup time
- Address issue where `require(lib.loc = ...)` was sometimes being passed `NA`
- Appropriately skip actions that require the user when running in non-interactive mode (and add related tests)

## coursekata 0.14.0

- Remove deprecated `gf_model_old()` function

## coursekata 0.13.1

- Fix issue where startup message was not being displayed
- Make dependent startup package messages visible by default
- Add `coursekata.quiet` option to suppress startup messages
- Document `coursekata.quickstart` and `coursekata.quiet` options in README
- Trim unused packages in preparation for CRAN submission

## coursekata 0.13.0

- Add `coursekata.quickstart` option, which can reduce load times significantly.
- Reduce expensive lookups when attaching packages, further reducing load times.
- Re-introduce `gf_model` tests for density plots now that upstream is fixed.
- Add `test_fit()` simple model stats to help teachers evaluate student models.

## coursekata 0.12.0

- Remove `sse()`, `ssm()`, `ssr()`, `SSE()`, `SSM()`, `SSR()` functions: they conflict with `Metrics` package.
- Reverse package load order (load most important last so that they mask others)

## coursekata 0.11.0

- Add `palmerpenguins` and `World`

## coursekata 0.10.0

- Add [`Metrics` package](https://CRAN.R-project.org/package=Metrics)
- Remove `zargle`

## coursekata 0.9.4

- Double digit `fevdata$AGE` values were truncated by first character. This has been fixed.

## coursekata 0.9.3

- Fix issue installing missing packages where package could not be found.

## coursekata 0.9.2

- Use `pak` for package management.

## coursekata 0.8.0

- Add `game_data` dataset.

## coursekata 0.7.1

- Add argument forwarding for installs.

## coursekata 0.6.3

Patch release to resolve `R CMD CHECK` failures.

- Mainly migrates use of `size` for line widths to `linewidth`.
