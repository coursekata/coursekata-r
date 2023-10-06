#' @keywords internal
.onAttach <- function(...) {
  attached <- coursekata_attach()
  coursekata_load_theme()
  if (!quickstart()) {
    coursekata_attach_message(attached)
  }
}

quickstart <- function() {
  getOption("coursekata.quickstart", FALSE) ||
    !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "coursekata")
}
