#!/usr/bin/env Rscript

Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
options(coursekata.quiet = TRUE, coursekata.quickstart = FALSE, cli.width = 80)

# ensure this package is uninstalled
suppressMessages(try(remove.packages("fivethirtyeightdata"), silent = TRUE))

# function to compare output to a snapshot
test_output_snapshot <- function(expr, snapshot) {
  output <- capture.output(expr, type = "message")
  output <- trimws(output, "right")
  output <- paste(output, collapse = "\n")
  comp <- waldo::compare(output, snapshot)
  if (length(comp) > 0) stop(paste0(comp, collapse = "\n\n"), call. = FALSE)
}

# individual package start messages should not be printed (only the CourseKata message)
test_output_snapshot(library(coursekata), trimws("
── CourseKata packages ──────────────────────────────────── coursekata 0.14.1 ──
✔ dslabs              0.7.6         ✔ Metrics             0.1.4
✔ Lock5withR          1.2.2         ✔ lsr                 0.5.2
x fivethirtyeightdata               ✔ mosaic              1.8.4.2
✔ fivethirtyeight     0.6.2         ✔ supernova           2.5.7
"))
