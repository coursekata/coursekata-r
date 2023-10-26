#!/usr/bin/env Rscript

options(
  coursekata.quiet = FALSE,
  coursekata.quickstart = FALSE
)

# ensure this package is uninstalled
suppressMessages(try(remove.packages("fivethirtyeightdata"), silent = TRUE))

# loading should trigger the install prompt if interactive, and not if not
library(coursekata)
