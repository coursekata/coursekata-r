# CRAN Comments

## Release summary

Version 0.21.0 moves coursekata's plotting tools onto shared ggplot2 4
extension interfaces. The native `geom_*()` and `stat_*()` constructors now
use the same model planning, calculations, and drawing code as the `gf_*()`
interface. This release raises the minimum versions to ggplot2 4.0.2 and
ggformula 1.0.0 and removes compatibility code for the older graphics stack.

## Test environments

- Local: macOS Tahoe 26.5.2 (arm64), R 4.6.1
- Win-builder: Windows Server 2022 x64, R-devel r90572
- R-hub:
  - Linux and Windows: R-devel
  - macOS Intel and Apple silicon: R-devel
  - M1 sanitizer, Clang 22, GCC 16, Clang ASan/UBSan, and GCC ASan
  - All ten package checks completed successfully.

## R CMD check results

0 errors | 0 warnings | 1 note

- The local and win-builder checks report `Version contains large components
  (0.20.1.9000)`. This is caused by the development-version suffix and will not
  apply to the 0.21.0 release tarball. All remaining checks passed.

## Reverse dependencies

coursekata has no current CRAN reverse dependencies.
