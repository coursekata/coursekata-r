# CRAN Comments

## Release summary

- Add experimental visualization functions (`gf_resid_fun()`, `gf_square_resid_fun()`, `gf_sd_ruler()`, `gf_squareplot()`, `show_cutoffs()`, `outer()`).
- Rename `gf_squaresid()` to `gf_square_resid()` with deprecation warning.
- Add `coursekata.check_missing` option to control the missing-package install prompt.
- Improve performance of estimate extraction functions.

## Test environments

- Local install on macOS Tahoe 26.3 (ARM); R 4.5.1
- GitHub Actions
  - macOS: latest; R: 4.5.1
  - Microsoft Windows Server 2022: latest; R: 4.5.1, 4.4.3
  - Ubuntu: 22.04; R: devel, 4.5.1, 4.4.3
- rhub::rhub_check(platforms = rhub::rhub_platforms()$name[c(1, 2, 4, 8, 9, 11, 16, 17, 18)])
  - R-devel on linux, m1-san, macos-arm64, clang-asan, clang-ubsan, clang17, gcc-asan, gcc13, gcc14

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded
