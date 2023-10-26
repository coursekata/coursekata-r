#!/usr/bin/env Rscript

options(
  coursekata.quiet = TRUE,
  coursekata.quickstart = FALSE
)

# ensure this package is uninstalled
suppressMessages(try(remove.packages("fivethirtyeightdata"), silent = TRUE))

# loading should trigger the install prompt if interactive, and not if not
# individual package start messages should not be printed (only the CourseKata message)
library(coursekata)
