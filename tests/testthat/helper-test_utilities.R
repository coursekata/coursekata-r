detacher <- function(pkgs) {
  detach_ <- function(pkg) {
    try(detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE), silent = TRUE)
  }
  lapply(pkgs, detach_)
}

attacher <- function(pkgs) {
  attach_ <- function(pkg) {
    suppressMessages(library(pkg, character.only = TRUE))
  }
  lapply(pkgs, attach_)
}

expect_doppelganger <- function(plot, name) {
  vdiffr::expect_doppelganger(name, plot)
}
