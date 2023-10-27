#!/usr/bin/env Rscript

Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
options(coursekata.quiet = FALSE, coursekata.quickstart = TRUE, cli.width = 80)

# ensure this package is uninstalled
suppressMessages(try(remove.packages("fivethirtyeightdata"), silent = TRUE))

# function to compare output to a snapshot
test_output_snapshot <- function(expr, snapshot) {
  replace_smart_quotes <- function(x) {
    x <- gsub("’", "'", x)
    x <- gsub("‘", "'", x)
    x <- gsub("“", '"', x)
    x <- gsub("”", '"', x)
    x
  }

  output <- capture.output(expr, type = "message")
  output <- trimws(output, "right")
  output <- paste(output, collapse = "\n")
  comp <- waldo::compare(replace_smart_quotes(output), replace_smart_quotes(snapshot))
  if (length(comp) > 0) stop(paste0(comp, collapse = "\n\n"), call. = FALSE)
}

# loading should not trigger the install prompt or show any messages
test_output_snapshot(library(coursekata), "")
