## Release summary

Package was failing on CRAN checks and asking for user input in non-interactive mode. The check is for a missing package that should be installed for full functionality, but is not required. The fix was to add a check for interactive mode before asking for user input.

Additional comments from Uwe Ligges were that the user prompt was confusing. Instead of using `yesno` to generate the prompt, we are now using `rlang::check_installed()` to handle the prompt and subsequent installation.

Other changes:

- Reduce calls to `pak::pkg_status()` to improve startup time
- Address issue where `require(lib.loc = ...)` was sometimes being passed `NA` instead of `NULL`

## Test environments

- Local install on macOS Sonoma 14.0 (ARM); R 4.3.1
- GitHub Actions
  - macOS: 12.6.9; R: 4.3.1
  - Microsoft Windows Server 2022: 10.0.20348; R: 4.3.1, 3.6.3
  - Ubuntu: 22.04.3; R: devel, 4.3.1, 4.2.3

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded
