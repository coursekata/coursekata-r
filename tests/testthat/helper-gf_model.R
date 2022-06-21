load_before <- function(x) {
  .GlobalEnv$.load_dev()
  x
}

expect_doppelganger <- function(plot, name) {
  vdiffr::expect_doppelganger(name, plot)
}
