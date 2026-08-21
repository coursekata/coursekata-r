#' Suppress conflict warnings
#'
#' Set to `TRUE` in the package environment during `.onAttach()` so that
#' `base::library()` skips its default "masked objects" messages.
#'
#' @keywords internal
".conflicts.OK" <- TRUE

.onLoad <- function(libname, pkgname) {
  # Suppress "Registered S3 method overwritten by ..." notes from sub-packages
  # loaded later (e.g. by coursekata_attach). This must be set before those
  # namespaces are loaded; .onLoad is the earliest available hook.
  s3_note_env_var <- "_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_"
  .s3_note_state[["old"]] <- Sys.getenv(s3_note_env_var, unset = NA)
  Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
}

.onAttach <- function(...) {
  on.exit({
    old <- .s3_note_state[["old"]]
    if (is.na(old)) {
      Sys.unsetenv("_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_")
    } else {
      Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = old)
    }
  })

  attached <- coursekata_attach(
    do_not_ask = !interactive() || quickstart() || !check_missing(),
    quietly = TRUE
  )

  coursekata_load_theme()
  quietly <- getOption("coursekata.quiet", FALSE) || quickstart()
  if (!quietly) {
    msg <- paste(
      c(coursekata_attach_message(attached), coursekata_conflict_message()),
      collapse = "\n"
    )
    rlang::inform(msg, class = "packageStartupMessage")
  }

  # Suppress R's default "The following objects are masked from ..." messages.
  # checkConflicts() in base::library() runs after .onAttach returns and skips
  # its warnings when .conflicts.OK exists in the attached package environment.
  pkg_env <- as.environment("package:coursekata")
  assign(".conflicts.OK", TRUE, envir = pkg_env)
}

.s3_note_state <- new.env(parent = emptyenv())

quickstart <- function() {
  getOption("coursekata.quickstart", FALSE) ||
    !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "coursekata")
}

is_emscripten <- function() {
  identical(R.version$os, "emscripten")
}

check_missing <- function() {
  opt <- getOption("coursekata.check_missing")
  if (isTRUE(opt)) {
    return(TRUE)
  }
  if (isFALSE(opt)) {
    return(FALSE)
  }
  !is_emscripten()
}
