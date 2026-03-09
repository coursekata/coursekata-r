#' coursekata: CourseKata Statistics and Data Science
#'
#' @section Package Options:
#' The following options control startup behavior when `library(coursekata)` is called:
#'
#' \describe{
#'   \item{`coursekata.quickstart`}{If `TRUE`, skips dependency checks and
#'     suppresses all startup messages. Default: `FALSE`.}
#'   \item{`coursekata.quiet`}{If `TRUE`, suppresses startup messages but still
#'     checks for missing packages. Default: `FALSE`.}
#'   \item{`coursekata.check_missing`}{Controls the missing-package installation
#'     prompt. Accepts a tri-state value:
#'     \itemize{
#'       \item `NULL` (default, unset): Auto-detect. Skips the prompt when R is
#'         running under Emscripten (e.g., JupyterLite/WASM); prompts otherwise.
#'       \item `TRUE`: Always prompt for missing packages, even in Emscripten.
#'       \item `FALSE`: Never prompt for missing packages.
#'     }
#'     Non-logical values are treated as `NULL` (auto-detect). Note that
#'     `coursekata.quickstart = TRUE` takes precedence and suppresses the prompt
#'     regardless of this option.}
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
## usethis namespace: end
NULL

# These packages are in Imports to ensure installation but are not imported
# into the NAMESPACE. Loading their namespaces at import time would generate
# S3 method overwrite notes before any hook can suppress them. Instead they
# are attached for the user by coursekata_attach(). The :: references here
# satisfy R CMD check's "Imports not imported from" note.
ignore_unused_imports <- function() {
  dslabs::take_poll
  lsr::cohensD
  mosaic::qdist
  palmerpenguins::path_to_file
}
