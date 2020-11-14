#' List all CourseKata course packages
#'
#' @param core_only Optionally suppress the data packages.
#'
#' @return A character vector of the names of the course packages.
#' @export
#'
#' @examples
#' coursekata_packages()
coursekata_packages <- function(core_only = FALSE) {
  core_packages <- c('supernova', 'mosaic', 'lsr')
  data_packages <- c('fivethirtyeight', 'fivethirtyeightdata', 'Lock5withR', 'okcupiddata', 'dslabs')
  if (core_only) core_packages else c(core_packages, data_packages)
}


#' List all currently attached CourseKata course packages
#'
#' @param core_only Optionally suppress the data packages.
#'
#' @return A character vector of the course packages that have been attached.
#' @keywords internal
coursekata_attached <- function(core_only = FALSE) {
  packages <- coursekata_packages(core_only)
  packages[is_attached(packages)]
}


#' List all currently NOT attached CourseKata course packages
#'
#' @param core_only Optionally suppress the data packages.
#'
#' @return A character vector of the course packages that are not attached.
#' @keywords internal
coursekata_detached <- function(core_only = FALSE) {
  packages <- coursekata_packages(core_only)
  packages[!is_attached(packages)]
}


#' Check if packages are attached
#'
#' @param package Character vector of the names of the packages to check.
#'
#' @return Logical vector indicating whether the packages are attached or not.
#' @keywords internal
is_attached <- function(package) {
  paste0("package:", package) %in% search()
}
