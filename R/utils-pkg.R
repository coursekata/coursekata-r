#' Install packages, fixing remote names if necessary.
#'
#' @param pkgs A character vector of package names.
#' @param ... Arguments passed on to [`remotes::install_cran`] or [`remotes::install_github`]
#' depending on whether the package appears to be from CRAN or GitHub.
#'
#' @noRd
pkg_install <- function(pkgs, ...) {
  pkgs <- pkg_fix_remote_names(pkgs)
  is_remote <- grepl("/", pkgs)
  # install from CRAN first because it's faster than building from source from GitHub
  remotes::install_cran(pkgs[!is_remote], ...)
  remotes::install_github(pkgs[is_remote], ...)
}

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


#' Check if packages are installed.
#'
#' @param pkgs Character vector of the names of the packages to check.
#'
#' @return Named logical vector indicating whether the packages are installed.
#'
#' @noRd
pkg_is_installed <- function(pkgs) {
  vapply(pkgs, rlang::is_installed, logical(1))
}


#' Get the local package version numbers.
#'
#' @param pkgs A character vector of packages to look up.
#'
#' @return A character vector of the package versions. If the package is already loaded, this is
#'   pulled from the library the package was loaded from, else the default library location.
#'
#' @noRd
pkg_version <- function(pkgs) {
  get_version <- purrr::possibly(utils::packageVersion, NA_character_)
  vapply(pkgs, function(x) as.character(get_version(x)), character(1))
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
    lib_loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path")) else NULL
    require(pkg, lib.loc = lib_loc, character.only = TRUE, quietly = quietly)
  }

  loader <- if (quietly) quiet_wrap(pkg_load) else pkg_load
  vapply(pkgs, loader, logical(1))
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
      pkg_install(pkgs, upgrade = "always", ...)
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
  pkgs[pkgs == "Lock5withR"] <- "rpruim/Lock5withR"
  pkgs
}
