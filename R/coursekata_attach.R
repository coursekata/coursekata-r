#' @keywords internal
.onLoad <- function(...) {
  # this is needed because of the way mosaic loads packages
  # gets rid of "Registered S3 method overwritten by 'mosaic'" message
  suppressMessages(pkg_version('mosaic'))
}


#' @keywords internal
.onAttach <- function(...) {
  crayon::num_colors(TRUE)
  coursekata_attach(TRUE)
  load_coursekata_themes()
}


#' Attach the CourseKata course packages
#'
#' @param startup Is this being run at start-up?
#'
#' @return A coursekata attachments object with info about which course packages are installed and
#'   attached.
#' @export
#'
#' @examples
#' coursekata_attach()
coursekata_attach <- function(startup = FALSE) {
  to_attach <- coursekata_detached()
  if (length(to_attach)) {
    suppressPackageStartupMessages(purrr::walk(to_attach, pkg_require))
  }

  coursekata_attachments(TRUE)
}


#' Information about CourseKata packages.
#'
#' @param startup Is this being run at start-up?
#'
#' @return A coursekata_attachments object, also of class data.frame with a row for each course
#'   package and a column for each of the \code{package} name, \code{version}, and whether it is
#'   currently \code{attached}.
#' @keywords internal
coursekata_attachments <- function(startup = FALSE) {
  info <- coursekata_packages()
  msg <- coursekata_attachments_message(info)
  if (startup) packageStartupMessage(msg)
  else message(msg)

  invisible(info)
}


#' Pretty console printing when attaching packages
#'
#' @param pkg_info The package information to print.
#'
#' @return A string that has been formatted to print to console.
#' @keywords internal
coursekata_attachments_message <- function(pkg_info) {
  pkg_info$version <- ifelse(is.na(pkg_info$version), "", pkg_info$version)

  packages <- crayon::blue(paste(
    ifelse(pkg_info$attached, crayon::green(cli::symbol$tick), crayon::red('x')),
    format(pkg_info$package),
    crayon::col_align(pkg_info$version, max(crayon::col_nchar(pkg_info$version)))
  ))

  paste0(
    "\n",
    cli::rule(left = crayon::magenta(crayon::bold("CourseKata course packages"))), "\n",
    to_cols(packages, 2), "\n"
  )
}
