#!/usr/bin/env Rscript

Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
options(coursekata.quiet = FALSE, coursekata.quickstart = FALSE, cli.width = 80)

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

test_output_snapshot(library(coursekata), trimws("
Loading required package: dslabs
Loading required package: Lock5withR
Loading required package: fivethirtyeight
Some larger datasets need to be installed separately, like senators and
house_district_forecast. To install these, we recommend you install the
fivethirtyeightdata package by running:
install.packages('fivethirtyeightdata', repos =
'https://fivethirtyeightdata.github.io/drat/', type = 'source')
Loading required package: Metrics
Loading required package: lsr
Loading required package: mosaic

The 'mosaic' package masks several functions from core packages in order to add
additional features.  The original behavior of these functions should not be affected by this.

Attaching package: ‘mosaic’

The following objects are masked from ‘package:dplyr’:

    count, do, tally

The following object is masked from ‘package:Matrix’:

    mean

The following object is masked from ‘package:ggplot2’:

    stat

The following objects are masked from ‘package:stats’:

    binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
    quantile, sd, t.test, var

The following objects are masked from ‘package:base’:

    max, mean, min, prod, range, sample, sum

Loading required package: supernova
── CourseKata packages ──────────────────────────────────── coursekata 0.14.1 ──
✔ dslabs              0.7.6         ✔ Metrics             0.1.4
✔ Lock5withR          1.2.2         ✔ lsr                 0.5.2
x fivethirtyeightdata               ✔ mosaic              1.8.4.2
✔ fivethirtyeight     0.6.2         ✔ supernova           2.5.7
"))
