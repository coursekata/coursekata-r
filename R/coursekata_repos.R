#' Get repositories for the packages.
#'
#' Ensures a default CRAN is set if one is not already set, and adds the repository for
#' fivethirtyeightdata.
#'
#' @param repos Optionally set a repository character vector to augment.
#'
#' @return A set of repositories that can be used to install or update the CourseKata packages.
#'
#' @export
#' @examples
#' coursekata_repos()
coursekata_repos <- function(repos = getOption("repos")) {
  if (is.na(repos["CRAN"])) repos["CRAN"] <- "https://cloud.r-project.org"
  c(repos, fivethirtyeight = "https://fivethirtyeightdata.github.io/drat/")
}
