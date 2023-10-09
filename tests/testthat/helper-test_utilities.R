detacher <- function(pkgs) {
  lapply(pkgs, function(pkg) {
    try(detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE), silent = TRUE)
  })
}

attacher <- function(pkgs) {
  lapply(pkgs, function(pkg) {
    suppressPackageStartupMessages(suppressWarnings(require(
      pkg, character.only = TRUE, quietly = TRUE
    )))
  })
}

expect_doppelganger <- function(plot, name) {
  vdiffr::expect_doppelganger(name, plot)
}
