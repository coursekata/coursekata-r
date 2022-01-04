#' Check if packages are attached
#'
#' @param pkgs Character vector of the names of the packages to check.
#'
#' @return Logical vector indicating whether the packages are attached or not.
#' @keywords internal
pkg_is_attached <- function(pkgs) {
  paste0("package:", pkgs) %in% search()
}


#' check if packages are installed.
#'
#' @param pkgs Character vector of the names of the packages to check.
#'
#' @return Logical vector indicating whether the packages are attached or not.
#' @keywords internal
pkg_is_installed <- function(pkgs) {
  purrr::map_lgl(pkgs, ~ identical(TRUE, requireNamespace(.x, quietly = TRUE)))
}


#' Determine which libraries packages were loaded from
#'
#' @param pkgs A character vector of packages to check.
#'
#' @return A character vector of library directory paths the packages were loaded from, the default
#'   location if the package is not loaded but is installed, or NA if the package is not installed.
#' @keywords internal
pkg_library_location <- function(pkgs) {
  get_namespace_directory <- purrr::possibly(
    function(pkg) dirname(getNamespaceInfo(pkg, "path")),
    NA_character_
  )
  purrr::map_chr(pkgs, get_namespace_directory)
}


#' Get the local package version numbers.
#'
#' @param pkgs A character vector of packages to look up.
#'
#' @return A character vector of the package versions. If the package is already loaded, this is
#'   pulled from the library the package was loaded from, else the default library location.
#' @keywords internal
pkg_version <- function(pkgs) {
  purrr::map_chr(pkgs, function(pkg) {
    if (!pkg_is_installed(pkg)) {
      NA_character_
    } else {
      location <- pkg_library_location(pkg)
      version <- utils::packageVersion(pkg, lib.loc = if (is.na(location)) NULL else location)
      as.character(version)
    }
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
  purrr::map_chr(pkgs, ~ tryCatch(info[.x, ]['Version'], error = function(cond) { NA_character_ }))
}


#' Load the package, making sure to load from the correct location. Sometimes people have different
#' versions of packages installed, so if the package is currently loaded, make sure to load from the
#' place it was already loaded from.
#'
#' @param pkgs A character vector of packages to load.
#' @param do_not_ask Prevent asking the user to install missing packages (they are skipped).
#'
#' @keywords internal
pkg_require <- function(pkgs, do_not_ask = FALSE) {
  loader <- function(pkg) {
    location <- pkg_library_location(pkg)
    suppressWarnings(require(
      pkg,
      lib.loc = if (is.na(location)) NULL else location,
      character.only = TRUE,
      warn.conflicts = FALSE,
      quietly = TRUE
    ))
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
  if (!interactive()) return(FALSE)

  line <- function(x = '') { sprintf("%s\n", x) }
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
#'
#' @keywords internal
pkg_install <- function(pkgs) {
  utils::install.packages(pkgs, repos = coursekata_repos())
}
