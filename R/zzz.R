#' @keywords internal
.onAttach <- function(...) {
  attached <- coursekata_attach(quietly = getOption("coursekata.quiet", FALSE))
  coursekata_load_theme()
  if (!quickstart()) {
    rlang::inform(coursekata_attach_message(attached), class = "packageStartupMessage")
  }
}

quickstart <- function() {
  getOption("coursekata.quickstart", FALSE) ||
    !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "coursekata")
}
