#' @keywords internal
.onLoad <- function(...) {
  # this is needed because of the way mosaic loads packages
  # gets rid of "Registered S3 method overwritten by 'mosaic'" message
  suppressMessages(pkg_is_installed('mosaic'))
}


#' @keywords internal
.onAttach <- function(...) {
  crayon::num_colors(TRUE)
  coursekata_attach(TRUE)
  load_coursekata_themes()
}
