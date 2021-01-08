#' List all CourseKata course packages
#'
#' @return A data frame with three variables: the name of the package \code{package}, the
#'   \code{version}, and whether it is currently \code{attached}.
#' @export
#'
#' @examples
#' coursekata_packages()
coursekata_packages <- function() {
  pkgs <- c(
    'supernova', 'mosaic', 'lsr',
    'fivethirtyeight', 'fivethirtyeightdata', 'Lock5withR', 'okcupiddata', 'dslabs'
  )

  data.frame(
    package = pkgs,
    version = pkg_version(pkgs),
    attached = pkg_is_attached(pkgs),
    stringsAsFactors = FALSE
  )
}


#' List all currently attached CourseKata course packages
#'
#' @return A character vector of the course packages that have been attached.
#' @keywords internal
coursekata_attached <- function() {
  info <- coursekata_packages()
  info$package[info$attached]
}


#' List all currently NOT attached CourseKata course packages
#'
#' @return A character vector of the course packages that are not attached.
#' @keywords internal
coursekata_detached <- function() {
  info <- coursekata_packages()
  info$package[!info$attached]
}
