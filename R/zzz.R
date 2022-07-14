#' @keywords internal
.onLoad <- function(...) {
  # this is needed because of the way mosaic loads packages
  # gets rid of "Registered S3 method overwritten by 'mosaic'" message
  suppressMessages(pkg_is_installed("mosaic"))
}


#' @keywords internal
.onAttach <- function(...) {
  # prevents double message in devtools::test()
  needed <- coursekata_pkg_list[!pkg_is_attached(coursekata_pkg_list)]
  if (length(needed) == 0) {
    return()
  }

  crayon::num_colors(TRUE)
  coursekata_attach(TRUE)
  load_coursekata_themes()
}
