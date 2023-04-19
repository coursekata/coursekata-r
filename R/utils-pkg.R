#' Check if packages are attached
#'
#' @param pkgs Character vector of the names of the packages to check.
#'
#' @return Logical vector indicating whether the packages are attached or not.
#' @keywords internal
pkg_is_attached <- function(pkgs) {
  paste0("package:", pkgs) %in% search()
}


#' Check if packages are installed
#'
#' Note: this function differs from [`rlang::is_installed()`] in two regards: it is quieter and will
#' show no messages, and it returns a vector indicating which packages are installed or not (rather
#' than a single Boolean value regarding the packages as a set).
#'
#' @param pkgs Character vector of the names of the packages to check.
#'
#' @return Logical vector indicating whether the packages are installed.
#' @keywords internal
pkg_is_installed <- function(pkgs) {
  pkgs %in% pak::pkg_status(pkgs)$package
}


#' Determine which libraries packages were loaded from
#'
#' @param pkgs A character vector of packages to check.
#'
#' @return A character vector of library directory paths the packages were loaded from, the default
#'   location if the package is not loaded but is installed, or NA if the package is not installed.
#' @keywords internal
pkg_library_location <- function(pkgs) {
  possibly_pkg_status(pkgs, "library")
}


#' Get the local package version numbers.
#'
#' @param pkgs A character vector of packages to look up.
#'
#' @return A character vector of the package versions. If the package is already loaded, this is
#'   pulled from the library the package was loaded from, else the default library location.
#' @keywords internal
pkg_version <- function(pkgs) {
  possibly_pkg_status(pkgs, "version")
}

#' Attempt to get package information using `pak`, but fail with NA
#'
#' @param pkgs A character vector of packages to look up.
#' @param status_key The column to get from the [`pak::pkg_status()`] output.
#'
#' @return A character vector with each package's status information.
#' @keywords internal
possibly_pkg_status <- function(pkgs, status_key) {
  purrr::map_chr(pkgs, function(pkg) {
    x <- pak::pkg_status(pkg)[[status_key]]
    if (length(x)) x else NA_character_
  })
}


#' Get the remote package version numbers.
#'
#' @param pkgs A character vector of packages to look up.
#'
#' @return A character vector of the package versions, or NA if the package cannot be found.
#' @keywords internal
pkg_remote_version <- function(pkgs) {
  info <- utils::available.packages(repos = coursekata_repos())
  get_remote_version <- purrr::possibly(function(pkg) info[pkg, ]["Version"], NA_character_)
  purrr::map_chr(pkgs, get_remote_version)
}


#' Load the package, making sure to load from the correct location. Sometimes people have different
#' versions of packages installed, so if the package is currently loaded, make sure to load from the
#' place it was already loaded from.
#'
#' @param pkgs A character vector of packages to load.
#' @param do_not_ask Prevent asking the user to install missing packages (they are skipped).
#' @param quietly Prevent package startup messages.
#'
#' @keywords internal
pkg_require <- function(pkgs, do_not_ask = FALSE) {
  loader <- function(pkg) {
    suppressPackageStartupMessages(suppressWarnings(require(
      pkg,
      lib.loc = pkg_library_location(pkg),
      character.only = TRUE,
      warn.conflicts = FALSE,
      quietly = TRUE
    )))
  }

  purrr::map_lgl(pkgs, function(pkg) {
    if (pkg_is_installed(pkg)) {
      loader(pkg)
    } else if (!do_not_ask && ask_to_install(pkg)) {
      pkg_install(pkg)
      loader(pkg)
    } else {
      FALSE
    }
  })
}


#' Ask the user if they want to install packages
#'
#' @param pkgs A character vector of packages to ask about.
#'
#' @return A logical indicating whether the user answered yes or no.
#' @keywords internal
ask_to_install <- function(pkgs) {
  if (!interactive()) {
    return(FALSE)
  }

  line <- function(x = "") {
    sprintf("%s\n", x)
  }
  yesno::yesno(crayon::red(paste0(
    line(),
    line("The following packages could not be found:"),
    line(paste0("  - ", pkgs, "\n", collapse = "")),
    "Install missing packages?"
  )))
}


#' Install packages using appropriate repositories.
#'
#' @param pkgs A character vector of the packages to install.
#' @param ... Arguments passed on to [`pak::pkg_install()`].
#'
#' @keywords internal
pkg_install <- function(pkgs, ...) {
  is_538 <- pkgs %in% "fivethirtyeight"
  if (any(is_538)) pak::pkg_install("fivethirtyeightdata/fivethirtyeightdata")
  pak::pkg_install(pkgs[!is_538], ...)
}
