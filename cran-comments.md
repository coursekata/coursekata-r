## Release summary

- Change name of dataset `Fingers.messy` to `FingersMessy`

## Test environments

- Local install on macOS Sonoma 14.1.2 (ARM); R 4.3.2
- GitHub Actions
  - macOS: 12.6.9; R: 4.3.2
  - Microsoft Windows Server 2022: 10.0.20348; R: 4.3.2, 3.6.3
  - Ubuntu: 22.04.3; R: devel, 4.3.2, 4.2.3
- devtools::check_rhub(platforms = rhub::platforms()$name)
- devtools::check_win_devel()

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded
