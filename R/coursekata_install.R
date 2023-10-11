#' Install or update all CourseKata packages.
#'
#' @param ... Arguments passed on to [`pak::pkg_install`].
#'
#' @return The state of all the packages after any updates have been performed.
#'
#' @export
#' @rdname coursekata_install
coursekata_install <- function(...) {
  pkgs <- coursekata_packages(TRUE)
  behind <- pkgs[!pkgs$installed | (!is.na(pkgs$behind) & pkgs$behind), ]

  if (nrow(behind) == 0) {
    cli::cat_line()
    cli::cat_line("All packages up-to-date!")
    cli::cat_line()
    return(invisible(pkgs))
  }

  cli::cat_line()
  pkg_install(behind$package, ...)
  invisible(coursekata_packages(TRUE))
}


#' @rdname coursekata_install
#' @export
coursekata_update <- function(...) {
  coursekata_install(upgrade = TRUE)
}
