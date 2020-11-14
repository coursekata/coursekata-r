
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coursekata <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->
<!-- badges: end -->

## Overview

*CourseKata Statistics and Data Science*, is an innovative interactive
online textbook for teaching introductory statistics and data science in
colleges, universities, and high schools. Part of CourseKata’s *Better
Book* Project, we are leveraging research and student data to guide
continuous improvement of online learning resources. The **coursekata**
package is designed to make it easy to install and load the packages,
functions, and data used in the book and supplementary materials.

Learn more about CourseKata and its free services and materials at
[CourseKata.org](https://coursekata.org/).

This package was inspired by the
[tidyverse](https://tidyverse.tidyverse.org) meta-package.

## Installation

``` r
# Install from CRAN
install.packages("coursekata")

# Or the development version from GitHub
# install.packages("remotes")
remotes::install_github("UCLATALL/coursekata")
```

## Usage

`library(coursekata)` will load these core packages:

``` r
library(coursekata)
#> -- CourseKata course packages -------------------------------------------------------------------------------------------------------
#> supernova           2.3.0       fivethirtyeightdata 0.1.0
#> mosaic              1.8.2       Lock5withR          1.2.2
#> lsr                 0.5         okcupiddata         0.1.0
#> fivethirtyeight     0.6.1       dslabs              0.7.3
```

-   [coursekata\_core](https://github.com/UCLATALL/coursekata_core), for
    various helpers like
    -   `middle()`, `upper()`, and `lower()` to facilitate shading
        proportions of plots
    -   an augmented `print.lm()` which prints the fitted equation as
        well
    -   tools for extracting information from fitted models (`b0()`,
        `b1()`, `PRE()`, `fVal()`)
    -   … and more!
-   [supernova](https://github.com/UCLATALL/supernova), for creating
    ANOVA tables.
-   [mosaic](https://projectmosaic.github.io/mosaic/), for a unified
    interface to most statistical tools.
-   [ggformula](https://projectmosaic.github.io/ggformula/), for a
    formula interface to ggplot2.
-   [dplyr](https://dplyr.tidyverse.org), for data manipulation.

In addition to useful functions, a great deal of data sets are used by
instructors who teach the course. This package installs these:

-   [fivethirtyeight and
    fivethirtyeightdata](https://cran.r-project.org/web/packages/fivethirtyeight/vignettes/fivethirtyeight.html)
-   [Lock5withR](https://github.com/rpruim/Lock5withR)
-   [dslabs](https://github.com/rafalab/dslabs)
-   [okcupiddata](https://github.com/rudeboybert/okcupiddata)

# Contributing

If you see an issue, problem, or improvement that you think we should
know about, or you think would fit with this package, please let us know
on our [issues page](https://github.com/UCLATALL/supernova/issues).
Alternatively, if you are up for a little coding of your own, submit a
pull request:

1.  Fork it!
2.  Create your feature branch: `git checkout -b my-new-feature`
3.  Commit your changes: `git commit -am 'Add some feature'`
4.  Push to the branch: `git push origin my-new-feature`
5.  Submit a [pull request](https://github.com/UCLATALL/supernova/pulls)
    :D
