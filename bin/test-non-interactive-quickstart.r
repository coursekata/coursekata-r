#!/usr/bin/env Rscript

options(
  coursekata.quiet = FALSE,
  coursekata.quickstart = TRUE
)

# ensure this package is uninstalled
suppressMessages(try(remove.packages("fivethirtyeightdata"), silent = TRUE))

# loading should not trigger the install prompt or show any messages
library(coursekata)
