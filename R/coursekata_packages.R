# reversed so that most important packages are loaded last (and mask earlier ones)
coursekata_pkgs <- rev(c(
  "supernova", "mosaic", "lsr", "Metrics",
  "fivethirtyeight", "fivethirtyeightdata", "Lock5withR", "dslabs"
))


#' List all CourseKata course packages
#'
#' @param check_remote_version Should the remote version number be checked? Requires internet, and
#'   will take longer.
#'
#' @return A data frame with three variables: the name of the package `package`, the `version`, and
#'   whether it is currently `attached`.
#'
#' @export
#' @examples
#' coursekata_packages()
coursekata_packages <- function(check_remote_version = FALSE) {
  pkgs <- coursekata_pkgs
  statuses <- pak::pkg_status(pkgs)
  info <- data.frame(
    package = pkgs,
    installed = pkg_is_installed(pkgs, statuses),
    attached = pkg_is_attached(pkgs),
    version = pkg_version(pkgs, statuses),
    stringsAsFactors = FALSE
  )

  if (check_remote_version) {
    info$remote_version <- pkg_remote_version(pkgs)
    info$behind <- info$version < info$remote_version
  }

  info
}


#' List all currently attached CourseKata course packages
#'
#' @return A character vector of the course packages that have been attached.
#'
#' @noRd
coursekata_attached <- function() {
  coursekata_pkgs[pkg_is_attached(coursekata_pkgs)]
}


#' List all currently NOT attached CourseKata course packages
#'
#' @return A character vector of the course packages that are not attached.
#'
#' @noRd
coursekata_detached <- function() {
  coursekata_pkgs[!pkg_is_attached(coursekata_pkgs)]
}
