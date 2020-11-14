#' @keywords internal
.onAttach <- function(...) {
  crayon::num_colors(TRUE)
  coursekata_attach()
}


#' Attach the CourseKata course packages
#'
#' @keywords internal
coursekata_attach <- function() {
  versions <- purrr::map_chr(coursekata_packages(), package_version)
  packages <- paste(
    crayon::blue(format(coursekata_packages())),
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  packageStartupMessage(
    cli::rule(left = crayon::magenta(crayon::bold("CourseKata course packages"))), "\n",
    to_cols(packages, 2), "\n"
  )

  to_attach <- coursekata_detached()
  if (length(to_attach)) {
    invisible(suppressPackageStartupMessages(
      purrr::map(to_attach, load_package)
    ))
  }
}


#' Load the package, making sure to load from the correct location. Sometimes people have different
#' versions of packages installed, so if the package is currently loaded, make sure to load from the
#' place it was already loaded from.
#' @keywords internal
load_package <- function(package) {
  location <- library_location(package)
  library(package, lib.loc = location, character.only = TRUE, warn.conflicts = FALSE)
}


#' Get a package's version
#'
#' @param package The package name string.
#'
#' @return The version of the package as a character string. If the package is already loaded, this
#'   is pulled from the library the package was loaded from, else the default library location.
#' @keywords internal
package_version <- function(package) {
  location <- library_location(package)
  as.character(utils::packageVersion(package, lib.loc = location))
}


#' Determine which library a package was loaded from
#'
#' @param package The package name string.
#'
#' @return If the package is loaded, the location it was loaded from, else NULL
#' @keywords internal
library_location <- function(package) {
  if (package %in% loadedNamespaces()) dirname(getNamespaceInfo(package, "path"))
}


#' Split a character into columns for terminal output
#'
#' Split the string into \code{n} columns, then glue the columns together row-wise with
#' \code{space_between}, then glue the rows together with new line characters.
#'
#' @param strings The strings to divide into columns.
#' @param n_cols The number of columns.
#' @param space_between What to put between the columns.
#'
#' @return A string that prints to the terminal as columns.
#' @keywords internal
to_cols <- function(strings, n_cols = 2, space_between = '       ') {
  items_per_col <- ceiling(length(strings) / n_cols)
  spacers <- rep("", items_per_col * n_cols - length(strings))
  strings <- append(strings, spacers)
  cols <- purrr::map(seq_len(n_cols), ~ strings[seq_len(items_per_col) + items_per_col * (.x - 1)])
  paste(purrr::reduce(cols, ~ paste0(.x, space_between, .y)), collapse = "\n")
}
