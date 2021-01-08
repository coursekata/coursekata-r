#' Check if packages are attached
#'
#' @param package Character vector of the names of the packages to check.
#'
#' @return Logical vector indicating whether the packages are attached or not.
#' @keywords internal
pkg_is_attached <- function(package) {
  paste0("package:", package) %in% search()
}


#' Determine which libraries packages were loaded from
#'
#' @param pkg A character vector of packages to check.
#'
#' @return A character vector of library directory paths, or NA if the package is not loaded.
#' @keywords internal
pkg_library_location <- function(pkg) {
  get_namespace_directory <- purrr::possibly(
    function(pkg) dirname(getNamespaceInfo(pkg, "path")),
    NA_character_
  )
  purrr::map_chr(pkg, get_namespace_directory)
}


#' Get package versions.
#'
#' @param pkg A character vector of packages to look up.
#'
#' @return A character vector of the package versions. If the package is already loaded, this is
#'   pulled from the library the package was loaded from, else the default library location.
#' @keywords internal
pkg_version <- function(pkg) {
  params <- list(pkg = pkg, location = pkg_library_location(pkg))
  purrr::pmap_chr(params, function(pkg, location) {
    location <- if (is.na(location)) NULL else location
    tryCatch(
      utils::packageVersion(pkg, lib.loc = location) %>% as.character(),
      error = function(condition) NA_character_
    )
  })
}


#' Load the package, making sure to load from the correct location. Sometimes people have different
#' versions of packages installed, so if the package is currently loaded, make sure to load from the
#' place it was already loaded from.
#'
#' @keywords internal
pkg_require <- function(pkg, do_not_ask = FALSE) {
  params <- list(pkg = pkg, location = pkg_library_location(pkg))
  loaded <- purrr::pmap_lgl(params, function(pkg, location) {
    location <- if (is.na(location)) NULL else location
    suppressWarnings(require(
      pkg,
      lib.loc = location,
      character.only = TRUE,
      warn.conflicts = FALSE,
      quietly = TRUE
    ))
  })

  missing <- pkg[!loaded]
  if (length(missing) == 0) return(loaded)
  if (!do_not_ask && ask_to_install(missing)) {
    pkg_install(missing)
    pkg_require(missing)
  }
}


#' Ask the user if they want to install packages
#'
#' @param missing Packages to ask about.
#'
#' @return A logical indicating whether the use answered yes or no.
#' @keywords internal
ask_to_install <- function(missing) {
  if (!interactive()) return(FALSE)
  yesno::yesno(crayon::red(paste0(
    "\n", "The following packages could not be found:", "\n",
    paste0("  - ", missing, "\n", collapse = ""), "\n",
    "Install missing packages?"
  )))
}


#' Install packages, but intercept for fivethirtyeightdata
#'
#' fivethirtyeight data needs to be loaded from an alternate repo.
#'
#' @param pkg A character vector of the packages to install.
#'
#' @keywords internal
pkg_install <- function(pkg) {
  special_package <- match('fivethirtyeightdata', pkg)
  if (!is.na(special_package)) {
    special_repo <- 'https://fivethirtyeightdata.github.io/drat/'
    utils::install.packages('fivethirtyeightdata', repos = special_repo, type = 'source')
    pkg <- pkg[-special_package]
  }

  utils::install.packages(pkg)
}
