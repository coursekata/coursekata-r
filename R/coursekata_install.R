#' Install or update all CourseKata packages.
#'
#' @return The state of all the packages after any updates have been performed.
#'
#' @export
#' @rdname coursekata_install
coursekata_install <- function() {
  pkgs <- coursekata_packages(TRUE)
  behind <- pkgs[!pkgs$installed | (!is.na(pkgs$behind) & pkgs$behind), ]

  if (nrow(behind) == 0) {
    cli::cat_line()
    cli::cat_line("All packages up-to-date!")
    cli::cat_line()
    return(invisible(pkgs))
  }

  if (any(behind$attached)) {
    cli::cat_line("The following packages are out of date:")
    cli::cat_line()
    cli::cat_bullet(
      format(behind$package),
      " (", behind$version, " -> ", behind$remote_version, ")"
    )

    cli::cat_line()
    cli::cat_line("Start a clean R session then run:")

    pkg_str <- paste0(deparse(behind$package), collapse = "\n")
    cli::cat_line("install.packages(", pkg_str, ", repos = coursekata::coursekata_repos())")
  } else {
    cli::cat_line()
    purrr::walk(behind$package, pkg_install)
  }

  invisible(coursekata_packages(TRUE))
}


#' @rdname coursekata_install
#' @export
coursekata_update <- coursekata_install
