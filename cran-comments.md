## Release summary

- Add alternate name `Fingers$Gender` to `Fingers` dataset to prevent awkward naming in exercises.

## Test environments

- Local install on macOS Sonoma 14.5 (ARM); R 4.4.1
- GitHub Actions
  - macOS: 14.4.1; R: R 4.4.1
  - Microsoft Windows Server 2022: 10.0.20348; R: 4.4.1, 4.3.3
  - Ubuntu: 22.04.4; R: devel, 4.4.1, 4.3.3
- rhub::rhub_check(platforms = rhub::rhub_platforms()$name[c(1, 2, 3, 4, 8, 9, 11, 16, 17, 18)])
  - R-devel on linux, macos, macos-arm64, windows, clang17, clang18, gcc13, ubuntu-clang, ubuntu-gcc12, ubuntu-next, ubuntu-release

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded
