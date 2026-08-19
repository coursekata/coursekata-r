# if vdiffr is not installed, all visual tests are skipped
# if it is installed, change the argument order to make it easier to pipe
if (requireNamespace("vdiffr", quietly = TRUE) && utils::packageVersion("testthat") >= "3.0.3") {
  expect_doppelganger <- function(fig, title, ...) {
    # Snapshots are SVG rendered by one ggplot2 ON one R. Below either version
    # they were written with, the same correct plot draws slightly different
    # markup, so a diff reports a version difference rather than a regression.
    # The behavior these plots stand for is asserted directly elsewhere; this
    # only guards appearance, and appearance is only comparable within a
    # toolchain.
    #
    # WHEN THE FLOOR MOVES TO 4.0: delete the ggplot2 skip.
    if (utils::packageVersion("ggplot2") < "4.0.0") {
      skip("visual snapshots are written against ggplot2 4; see R/compat-ggplot2.R")
    }
    # The R skip is about the graphics engine, not about ggplot2, and it stays
    # until the R floor moves. Measured on the matrix: identical package
    # versions throughout -- vdiffr 1.0.9, systemfonts 1.3.2, fontquiver 0.2.1
    # -- and R 4.1 still redraws every text-bearing plot differently, while
    # macOS and Windows match Linux exactly. So this is not platform drift, and
    # the floor row is there to prove the package WORKS at its declared R
    # floor; how it looks there is not part of that claim.
    if (getRversion() < "4.2") {
      skip("visual snapshots are written against a newer R graphics engine")
    }
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
