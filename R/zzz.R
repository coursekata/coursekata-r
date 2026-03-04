.onAttach <- function(...) {
  attached <- coursekata_attach(
    do_not_ask = !interactive() || interactive() && quickstart() || !check_missing(),
    quietly = getOption("coursekata.quiet", FALSE) || quickstart()
  )

  coursekata_load_theme()
  if (!quickstart()) {
    rlang::inform(coursekata_attach_message(attached), class = "packageStartupMessage")
  }
}

quickstart <- function() {
  getOption("coursekata.quickstart", FALSE) ||
    !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "coursekata")
}

is_emscripten <- function() {
  identical(R.version$os, "emscripten")
}

check_missing <- function() {
  opt <- getOption("coursekata.check_missing")
  if (isTRUE(opt)) return(TRUE)
  if (isFALSE(opt)) return(FALSE)
  !is_emscripten()
}
