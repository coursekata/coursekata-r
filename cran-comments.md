# CRAN Comments

## 0.20.1 patch

Version 0.20.1 documents the internal `.conflicts.OK` marker installed in the
attached package environment. This resolves the only warning reported when the
0.20.0 source tarball was checked by win-builder R-devel. The marker remains an
internal implementation detail; this patch does not change package behavior.

## Return from the CRAN archive

This release returns coursekata to CRAN after version 0.19.2 was archived on
2026-05-11. It addresses both failures shown on the archived CRAN check page:

- The examples for the distribution-part functions no longer build their data
  with `mosaic::do()`, whose changed result-column naming caused the examples to
  pass a function instead of a numeric column to `middle()`. They now use base R
  and package-owned functions, and run successfully during `R CMD check`.
- The `gf-squareplot` vignette that failed under updated dependencies has been
  replaced by site-only articles. The articles are excluded from the source
  tarball; all examples that ship in the package run during `R CMD check`.

## Notable changes

- Add model-visualization layers for coefficients, model reductions, inferred
  models, distribution means, and data-generating processes.
- Rebuild the squareplot, residual, model, cutoff, and standard-deviation-ruler
  layers on ggplot2/ggformula extension interfaces, with regression coverage for
  ggplot2 3.5.2 and the current ggplot2 release.
- Correct exact-proportion tail selection in `middle()`, `tails()`, and
  `outer()`.
- Stop attaching the optional `fivethirtyeightdata` package.
- Raise the minimum R version from 3.6 to 4.1 and declare measured dependency
  floors for ggformula and mosaic.
- Change the package license from AGPL (>= 3) to GPL (>= 3).

## Test environments

- Local: macOS Tahoe 26.5.2 (arm64), R 4.6.1, including CRAN/win-builder's
  strict check for undocumented dot-prefixed objects
- Win-builder: Windows Server 2022, R-devel r90424
- GitHub Actions:
  - macOS: R release
  - Windows: R devel, release, and oldrel-1
  - Ubuntu: R devel, release, oldrel-1, and the package's R 4.1 floor
  - Ubuntu: R release with ggplot2 3.5.2 and ggformula 0.12.2 dependency floors
- R-hub:
  - Linux, Windows, and macOS arm64: R-devel
  - Clang 22, GCC 16, Clang ASan/UBSan, GCC ASan, and macOS M1 sanitizers
  - All nine package checks completed with `Status: OK`. An additional Intel
    macOS job did not reach the package check: R-hub's dependency setup stalled
    after its repository probe encountered a missing Bioconductor R-devel Intel
    binary index. The macOS arm64 and M1 sanitizer checks were unaffected.

## R CMD check results

0 errors | 0 warnings | 1 note

- CRAN incoming feasibility reports "New submission" and "Package was
  archived on CRAN". This is expected; the archived failures and their fixes
  are described above.
- The exact source tarball completed win-builder R-devel with this same single
  note; all remaining checks, including examples, tests, and both manuals,
  passed.

The package has no current CRAN reverse dependencies because version 0.19.2 is
archived.
