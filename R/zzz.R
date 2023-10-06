#' @keywords internal
.onAttach <- function(...) {
  attached <- coursekata_attach()
  coursekata_load_theme()
  if (!is_loading_for_tests()) {
    coursekata_attach_message(attached)
  }
}

is_loading_for_tests <- function() {
  !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "tidyverse")
}
