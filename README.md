
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coursekata <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/coursekata)](https://CRAN.R-project.org/package=coursekata)
[![R build
status](https://github.com/coursekata/coursekata-r/workflows/R-CMD-check/badge.svg)](https://github.com/coursekata/coursekata-r/actions)
[![codecov](https://codecov.io/gh/coursekata/coursekata-r/branch/main/graph/badge.svg?token=HEenoYyHcn)](https://app.codecov.io/gh/coursekata/coursekata-r)
[![lite-badge](https://jupyterlite.rtfd.io/en/latest/_static/badge.svg)](https://coursekata.github.io/coursekata-r/lite/lab/index.html?path=getting-started.ipynb)
<!-- badges: end -->

Documentation, including a reference for every function and data set, is
at <https://coursekata.github.io/coursekata-r/>.

## Overview

*CourseKata Statistics and Data Science*, is an innovative interactive
online textbook for teaching introductory statistics and data science in
colleges, universities, and high schools. Part of CourseKata’s *Better
Book* Project, we are leveraging research and student data to guide
continuous improvement of online learning resources. The **coursekata**
package is designed to make it easy to install and load the packages,
functions, and data used in the book and supplementary materials.

Learn more about CourseKata and its services and materials at
[CourseKata.org](https://www.coursekata.org/).

This package makes it easy to install and load all packages and
functions used in CourseKata courses. It additionally provides a handful
of helper functions and augments some generic functions to provide
cohesion between the network of packages. This package was inspired by
the [tidyverse](https://tidyverse.tidyverse.org) meta-package.

## Try it in your browser

You can try coursekata before installing anything: the [live
demo](https://coursekata.github.io/coursekata-r/lite/lab/index.html?path=getting-started.ipynb)
runs R and the coursekata package entirely in your browser (no
installation required, powered by
[JupyterLite](https://jupyterlite.readthedocs.io/)). It opens a
getting-started notebook that walks through loading the package,
exploring course data sets, and fitting and visualizing models.

## Installation

``` r
install.packages("coursekata")
```

After installing the core packages, you might want to install the
supplementary data packages used in the course. These are not required
for the package to work, but they are used in the course materials. You
can install them with the following command:

``` r
coursekata::coursekata_install()
```

If you don’t install these packages, you will be prompted to install
them each time you load the package. If you want to disable that prompt,
you can set `options(coursekata.quickstart = TRUE)`.

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of `coursekata` from GitHub.

``` r
# install.packages("pak")
pak::pak("coursekata/coursekata-r")
```

## Loading Packages Used in CourseKata Courses

`library(coursekata)` will load the following core packages in addition
to the [functions and theme](#functions-and-theme) included in the
`coursekata` package:

``` r
library(coursekata)
```

- [supernova](https://cran.r-project.org/package=supernova), for ANOVA
  tables, tools for extracting information from fitted models (`b0()`,
  `b1()`, `PRE()`, `fVal()`), and an augmented `print.lm()` that prints
  the fitted equation as well.
- [mosaic](https://cran.r-project.org/package=mosaic), for a unified
  interface to most statistical tools.
- [ggformula](https://cran.r-project.org/package=ggformula), for a
  formula interface to ggplot2.
- [dplyr](https://cran.r-project.org/package=dplyr), for data
  manipulation.
- [Metrics](https://cran.r-project.org/package=Metrics), for model
  evaluation.

Instructors who teach the course also use a number of data packages,
which this package installs:
[fivethirtyeight](https://cran.r-project.org/package=fivethirtyeight),
[fivethirtyeightdata](https://fivethirtyeightdata.github.io/fivethirtyeightdata/index.html),
[Lock5withR](https://cran.r-project.org/package=Lock5withR), and
[dslabs](https://cran.r-project.org/package=dslabs).

### Startup options

- `coursekata.quickstart`: Each time the package is loaded (e.g. via
  `library(coursekata)`) a check is run to ensure that all the
  dependencies are installed and reasonably up-to-date. If they are not,
  you will be prompted to install missing packages. This can be disabled
  by setting `options(coursekata.quickstart = TRUE)`.

- `coursekata.quiet`: By default, the package will show all startup
  messages from the dependent packages. To quiet these (like in the
  output above), you can set `options(coursekata.quiet = TRUE)`

## Functions and Theme

This package also comes with a variety of functions useful for teaching
statistics and data science: tools for layering models and residuals
onto plots (e.g. `gf_model()`, `gf_resid()`), extracting estimates from
fitted models for bootstrapping (e.g. `b0()`, `b1()`, `pre()`),
sectioning distributions (e.g. `middle()`, `tails()`), and quantifying
model fit. It also automatically sets a `ggplot2` theme complete with
colorblind-friendly palettes and other improvements to aid perception
and clarity of plots.

Browse all of the functions and data sets, organized by what they are
for, in the [package
reference](https://coursekata.github.io/coursekata-r/reference/).

# Contributing

If you see an issue, problem, or improvement that you think we should
know about, or you think would fit with this package, please let us know
on our [issues page](https://github.com/coursekata/coursekata-r/issues).
Alternatively, if you are up for a little coding of your own, submit a
pull request:

1.  Fork it!
2.  Create your feature branch: `git checkout -b my-new-feature`
3.  Commit your changes: `git commit -am 'Add some feature'`
4.  Push to the branch: `git push origin my-new-feature`
5.  Submit a [pull
    request](https://github.com/coursekata/coursekata-r/pulls) :D
