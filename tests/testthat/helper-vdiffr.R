# if vdiffr is not installed, all visual tests are skipped
# if it is installed, change the argument order to make it easier to pipe
if (requireNamespace("vdiffr", quietly = TRUE) && utils::packageVersion("testthat") >= "3.0.3") {
  expect_doppelganger <- function(fig, title, ...) {
    vdiffr::expect_doppelganger(title, fig, ...)
  }
} else {
  # If vdiffr is not available and visual tests are explicitly required, raise error.
  if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {
    abort("vdiffr is not installed")
  }

  # Otherwise, assign a dummy function
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}
