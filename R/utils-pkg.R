#' Check if packages are attached
#'
#' @param pkgs Character vector of the names of the packages to check.
#'
#' @return Logical vector indicating whether the packages are attached or not.
#'
#' @noRd
pkg_is_attached <- function(pkgs) {
  paste0("package:", pkgs) %in% search()
}


#' Check if packages are installed
#'
#' Note: this function differs from [`rlang::is_installed()`] in two regards: it is quieter and will
#' show no messages, and it returns a vector indicating which packages are installed or not (rather
#' than a single Boolean value regarding the packages as a set). It is similar to
#'
#' @param pkgs Character vector of the names of the packages to check.
#' @param statuses The output of [`pak::pkg_status()`] (computed if not supplied).
#'
#' @return Named logical vector indicating whether the packages are installed.
#'
#' @noRd
pkg_is_installed <- function(pkgs, statuses = NULL) {
  statuses <- if (is.null(statuses)) pak::pkg_status(pkgs) else statuses
  checker <- function(pkg) pkg %in% statuses$package
  vapply(pkgs, checker, logical(1))
}


#' Determine which libraries packages were loaded from
#'
#' @param pkgs A character vector of packages to check.
#' @param statuses The output of [`pak::pkg_status()`] (computed if not supplied).
#'
#' @return A character vector of library directory paths the packages were loaded from, the default
#'   location if the package is not loaded but is installed, or NA if the package is not installed.
#'
#' @noRd
pkg_library_location <- function(pkgs, statuses = NULL) {
  possibly_pkg_status(pkgs, "library", statuses = statuses)
}


#' Get the local package version numbers.
#'
#' @param pkgs A character vector of packages to look up.
#' @param statuses The output of [`pak::pkg_status()`] (computed if not supplied).
#'
#' @return A character vector of the package versions. If the package is already loaded, this is
#'   pulled from the library the package was loaded from, else the default library location.
#'
#' @noRd
pkg_version <- function(pkgs, statuses = NULL) {
  possibly_pkg_status(pkgs, "version", statuses = statuses)
}

#' Attempt to get package information using `pak`, but fail with NA
#'
#' @param pkgs A character vector of packages to look up.
#' @param status_key The column to get from the [`pak::pkg_status()`] output.
#' @param statuses The output of [`pak::pkg_status()`] (computed if not supplied).
#'
#' @return A character vector with each package's status information.
#'
#' @noRd
possibly_pkg_status <- function(pkgs, status_key, statuses = NULL) {
  statuses <- if (is.null(statuses)) pak::pkg_status(pkgs) else statuses
  get_key <- function(pkg) {
    if (pkg %in% statuses$package) {
      statuses[statuses$package == pkg, status_key]
    } else {
      NA_character_
    }
  }
  vapply(pkgs, get_key, character(1))
}


#' Get the remote package version numbers.
#'
#' @param pkgs A character vector of packages to look up.
#'
#' @return A character vector of the package versions, or NA if the package cannot be found.
#'
#' @noRd
pkg_remote_version <- function(pkgs) {
  info <- utils::available.packages(repos = coursekata_repos())
  get_remote_version <- purrr::possibly(function(pkg) info[pkg, ]["Version"], NA_character_)
  vapply(pkgs, get_remote_version, character(1))
}


#' Load the package, making sure to load from the correct location. Sometimes people have different
#' versions of packages installed, so if the package is currently loaded, make sure to load from the
#' place it was already loaded from.
#'
#' @param pkgs A character vector of packages to load.
#' @param quietly Prevent package startup messages.
#'
#' @return A named logical indicating whether each package was loaded.
#'
#' @noRd
pkg_require <- function(pkgs, quietly = TRUE) {
  quiet_wrap <- function(fn) {
    function(...) suppressPackageStartupMessages(suppressWarnings(fn(...)))
  }

  pkg_load <- function(pkg) {
    lib_loc <- if (quickstart()) NULL else pkg_library_location(pkg)
    lib_loc <- if (!is.null(lib_loc) && is.na(lib_loc)) NULL else lib_loc
    require(pkg, lib.loc = lib_loc, character.only = TRUE, quietly = quietly)
  }

  loader <- if (quietly) quiet_wrap(pkg_load) else pkg_load
  vapply(pkgs, quiet_wrap(pkg_load), logical(1))
}


#' Ask the user if they want to install packages then install them.
#'
#' @param pkgs A character vector of packages to ask about.
#'
#' @return Whether the packages were installed or not.
#'
#' @noRd
pkg_check_installed <- function(pkgs) {
  if (!interactive()) {
    return(FALSE)
  }

  withRestarts(
    is.null(rlang::check_installed(pkgs, action = function(pkgs, ...) {
      pkgs <- pkg_fix_remote_names(pkgs)
      pak::pkg_install(pkgs, ask = FALSE, ...)
    })),
    abort = function(e) FALSE
  )
}

#' Replace package names with their remote names (i.e. qualify with repo name).
#'
#' @param pkgs A character vector of package names.
#'
#' @return A character vector of package names with remote names.
#'
#' @noRd
pkg_fix_remote_names <- function(pkgs) {
  pkgs[pkgs == "fivethirtyeightdata"] <- "fivethirtyeightdata/fivethirtyeightdata"
  pkgs
}
