# CRAN Comments

## Release summary

- Add `gf_resid()` and `gf_squaresid()` functions for residual and squared residual plots that layer onto `ggformula::gf_point()` plots.

## Test environments

- Local install on macOS Sequoia 15.5 (ARM); R 4.5.1
- GitHub Actions
  - macOS: 14.7.6; R: R 4.5.1
  - Microsoft Windows Server 2022: 10.0.20348; R: 4.5.1, 4.4.3
  - Ubuntu: 22.04.2; R: devel, 4.5.1, 4.4.3
- rhub::rhub_check(platforms = rhub::rhub_platforms()$name[c(1, 2, 3, 4, 8, 9, 11, 16, 17, 18)])
  - R-devel on linux, macos, macos-arm64, windows, clang17, clang18, gcc13, ubuntu-clang, ubuntu-gcc12, ubuntu-next, ubuntu-release

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded
