## Release summary

- This is a new package. This is the second time submitting this package to CRAN, and it has been
  updated to address the feedback from the first submission.

- Requested changes:

  - The Title and Description now use single quotes around 'CourseKata'
  - All exported functions have been checked to ensure that their return values
    are documented in the `@return` section of the function documentation.
    - coursekata_load_theme
    - coursekata_unload_theme
    - gf_model

- Additional changes:
  - Removed internal docs and opted for @noRd tags as suggested by tidyverse

## Test environments

- Local install on macOS Sonoma 14.0 (ARM); R 4.3.1
- GitHub Actions
  - macOS: 12.7; R: 4.3.1
  - Microsoft Windows Server 2022: 10.0.20348; R: 4.3.1, 3.6.3
  - Ubuntu: 22.04.3; R: devel, 4.3.1, 4.2.3

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded
