detacher <- function(pkg) {
  try(detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE), silent = TRUE)
}

attacher <- function(pkg) {
  suppressMessages(library(pkg, character.only = TRUE))
}

load_before <- function(x) {
  .GlobalEnv$.load_dev()
  x
}

expect_doppelganger <- function(plot, name) {
  vdiffr::expect_doppelganger(name, plot)
}
