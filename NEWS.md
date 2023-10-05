# `coursekata` change log

## coursekata 0.12.0

- Remove `sse()`, `ssm()`, `ssr()`, `SSE()`, `SSM()`, `SSR()` functions: they conflict with `Metrics` package.
- Reverse package load order (load most important last so that they mask others)

## coursekata 0.11.0

- Add `palmerpenguins` and `World`

## coursekata 0.10.0

- Add [`Metrics` package](https://cran.r-project.org/web/packages/Metrics/index.html)
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
