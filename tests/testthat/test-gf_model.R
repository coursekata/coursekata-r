# Constraints ---------------------------------------------------------------------------------

test_that("it needs to be layered onto a plot", {
  gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_error()
})

test_that("the model variables must be in the underlying plot", {
  wrong_model <- lm(Thumb ~ NULL, data = Fingers)
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(wrong_model) %>%
    expect_error(".*missing in plot: Thumb.*")
})

test_that("the model outcome has to be one of the axes", {
  gf_point(base_anxiety ~ condition, color = ~later_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_error(".*model outcome.*one of the axes.*")
})


# No predictor -------------------------------------------------------------------------------

test_that("it plots the empty model as a horizontal line when outcome is on Y, two axis plots", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger("[gf_point] Empty model, outcome on Y")
})

test_that("it plots the empty model as a vertical line when outcome is on Y, one axis plot", {
  # I know that the plot has two axes, but I only specify one, that's why "one" axis plot
  snap_name <- function(plot_name, suffix = "") {
    glue("[{plot_name}] Empty model, outcome on Y")
  }

  plot_args <- list(gformula = ~later_anxiety, color = ~condition, data = er)

  # skip these funs because their related stat can't be found when running in testthat
  # but these stat funs are in ggstance, so idk... if you're here and you're worried, just
  # test by hand because it usually only doesn't work in testthat
  # bin_plots <- c("gf_histogramh", "gf_dhistogramh")
  other_plots <- c("gf_rugy")
  purrr::walk(other_plots, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ NULL, data = er), color = "brown") %>%
      expect_doppelganger(snap_name(plot))
  })

  # box/violin plots have different formulae
  gf_boxplot(later_anxiety ~ 1, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger(snap_name("gf_boxplot", " -- 2"))

  testthat::skip_if(Sys.getenv("CI") == "true", "Skipping these tests on GitHub Actions.")

  gf_violin(later_anxiety ~ 1, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger(snap_name("gf_violin"))
})

test_that("it plots the empty model as a horizontal line when outcome is on X, two axis plot", {
  gf_point(base_anxiety ~ later_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger("[gf_point] Empty model, outcome on X")
})

test_that("it plots the empty model as a vertical line when outcome is on X, one axis plot", {
  # I know that the plot has two axes, but I only specify one, that's why "one" axis plot
  snap_name <- function(plot_name, suffix = "") {
    glue("[{plot_name}] Empty model, outcome on X{suffix}")
  }

  plot_args <- list(gformula = ~later_anxiety, color = ~condition, data = er)

  bin_plots <- c("gf_histogram", "gf_dhistogram", "gf_freqpoly")
  purrr::walk(bin_plots, function(plot) {
    do.call(plot, append(plot_args, list(bins = 30))) %>%
      gf_model(lm(later_anxiety ~ NULL, data = er), color = "brown") %>%
      expect_doppelganger(snap_name(plot))
  })

  # box/violin plots have different formulae
  gf_boxplot(~later_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger(snap_name("gf_boxplot"))


  testthat::skip_if(Sys.getenv("CI") == "true", "Skipping these tests on GitHub Actions.")

  gf_violin(1 ~ later_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger(snap_name("gf_violin", " -- 2"))

  gf_violin(later_anxiety ~ 1, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger(snap_name("gf_violin horizontal"))

  # TODO: "gf_dens2": Can't find geom called "density_line"
  other_plots <- c("gf_rug", "gf_rugx", "gf_density", "gf_dens")
  purrr::walk(other_plots, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ NULL, data = er), color = "brown") %>%
      expect_doppelganger(snap_name(plot))
  })
})


# Single predictor, on axis, categorical ------------------------------------------------------

test_that("it plots 1 predictor (on axis, categorical) models as lines at means, outcome on Y", {
  testthat::skip_if(Sys.getenv("CI") == "true", "Skipping these tests on GitHub Actions.")

  snap_name <- function(plot_name, suffix = "") {
    glue("[{plot_name}] cond. mod., outcome on Y{suffix}")
  }

  plot_args <- list(gformula = later_anxiety ~ condition, color = ~condition, data = er)
  plot_types <- c("gf_point", "gf_boxplot", "gf_violin")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er), color = "brown") %>%
      expect_doppelganger(snap_name(plot))
  })
})

test_that("it plots 1 predictor (on axis, categorical) models as lines at means, outcome on X", {
  snap_name <- function(plot_name, suffix = "") {
    glue("[{plot_name}] cond. mod., outcome on X{suffix}")
  }

  plot_args <- list(gformula = condition ~ later_anxiety, color = ~condition, data = er)
  plot_types <- c("gf_point")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er), color = "brown") %>%
      expect_doppelganger(snap_name(plot))
  })
})


# Single predictor, on aesthetic, categorical -------------------------------------------------

test_that("it plots 1 predictor (on aesthetic, cat.) models as lines at means, outcome on Y", {
  testthat::skip_if(Sys.getenv("CI") == "true", "Skipping these tests on GitHub Actions.")

  snap_name <- function(plot_name, suffix = "") {
    glue("[{plot_name}] cond. mod., outcome on Y, pred. on color")
  }

  # plots where both axes are specified
  plot_args <- list(gformula = later_anxiety ~ provider, color = ~condition, data = er)
  plot_types <- c("gf_point", "gf_boxplot", "gf_violin")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })

  # plot where one axis is calculated
  plot_args <- list(gformula = ~later_anxiety, color = ~condition, data = er)
  plot_types <- c("gf_rugy")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })
})

test_that("it plots 1 predictor (on aesthetic, cat.) models as lines at means, outcome on X", {
  testthat::skip_if(Sys.getenv("CI") == "true", "Skipping these tests on GitHub Actions.")

  snap_name <- function(plot_name, suffix = "") {
    glue("[{plot_name}] cond. mod., outcome on X, pred. on color")
  }

  # plots where both axes are specified
  plot_args <- list(gformula = provider ~ later_anxiety, color = ~condition, data = er)
  plot_types <- c("gf_point", "gf_boxplot", "gf_violin")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })

  # plots where one axis is calculated
  plot_args <- list(gformula = ~later_anxiety, color = ~condition, data = er)
  plot_types <- c("gf_histogram", "gf_dhistogram", "gf_rug", "gf_rugx")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })
})


# Single predictor, on facet, categorical -----------------------------------------------------

test_that("it plots 1 predictor (on facet, compact cat.) models as lines at means, outcome on Y", {
  testthat::skip_if(Sys.getenv("CI") == "true", "Skipping these tests on GitHub Actions.")

  snap_name <- function(plot_name, suffix = "") {
    glue("[{plot_name}] cond. mod., outcome on Y, pred. on facet")
  }

  # plots where both axes are specified
  plot_args <- list(gformula = later_anxiety ~ provider | condition, data = er)
  plot_types <- c("gf_point", "gf_boxplot", "gf_violin")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })

  # alternative specification
  plot_args <- list(gformula = later_anxiety ~ provider, data = er)
  plot_types <- c("gf_point", "gf_boxplot", "gf_violin")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_facet_wrap(~condition) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })

  # plots where one axis is calculated
  # skip "gf_histogramh", "gf_dhistogramh" funs because their related stat funs can't be found
  plot_args <- list(gformula = ~ later_anxiety | condition, data = er)
  plot_types <- c("gf_rugy")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })
})

test_that("it plots 1 predictor (on facet, compact cat.) models as lines at means, outcome on X", {
  testthat::skip_if(Sys.getenv("CI") == "true", "Skipping these tests on GitHub Actions.")

  snap_name <- function(plot_name, suffix = "") {
    glue("[{plot_name}] cond. mod., outcome on X, pred. on facet")
  }

  # plots where both axes are specified
  plot_args <- list(gformula = provider ~ later_anxiety | condition, data = er)
  plot_types <- c("gf_point", "gf_boxplot", "gf_violin")
  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })

  # plots where one axis is calculated
  plot_args <- list(gformula = ~ later_anxiety | condition, data = er)
  plot_types <- c("gf_histogram", "gf_dhistogram", "gf_rug", "gf_rugx")

  purrr::walk(plot_types, function(plot) {
    do.call(plot, plot_args) %>%
      gf_model(lm(later_anxiety ~ condition, data = er)) %>%
      expect_doppelganger(snap_name(plot))
  })
})


# Single predictor, on axis, continuous -------------------------------------------------------

test_that("it plots 1 predictor (on axis, cont.) models as a fit line", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_doppelganger("[gf_point] anx. mod., outcome on Y")

  gf_point(base_anxiety ~ later_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_doppelganger("[gf_point] anx. mod., outcome on X")
})


# Single predictor, on aesthetic, continuous --------------------------------------------------

test_that("it splits continuous aesthetic predictors at -+1 SD and mean", {
  gf_point(later_anxiety ~ condition, color = ~base_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_doppelganger("[gf_point] anx. mod., pred. on color, outcome on Y")

  gf_point(condition ~ later_anxiety, color = ~base_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_doppelganger("[gf_point] anx. mod., pred. on color, outcome on X")
})


# Two predictors, on axis and aesthetic -------------------------------------------------------

test_that("it plots main effects models (cat. + cat.)", {
  gf_point(later_anxiety ~ provider, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ provider + condition, data = er)) %>%
    expect_doppelganger("[gf_point] floating 'parallel' hashes in two colors")
})

test_that("it plots main effects models (quant. + cat.)", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety + condition, data = er)) %>%
    expect_doppelganger("[gf_point] parallel lines in two colors")
})

test_that("it plots main effects models (cat. + quant.)", {
  gf_point(later_anxiety ~ condition, color = ~base_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ condition + base_anxiety, data = er)) %>%
    expect_doppelganger("[gf_point] parallel hashes in three colors (at M, +-SD)")
})

test_that("it plots main effect models (quant. + quant.)", {
  gf_point(later_anxiety ~ base_anxiety, color = ~base_depression, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety + base_depression, data = er)) %>%
    expect_doppelganger("[gf_point] parallel lines in three colors (at M, +-SD)")
})

test_that("it plots interactive models (cat. * cat.)", {
  gf_point(later_anxiety ~ provider, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ provider * condition, data = er)) %>%
    expect_doppelganger("[gf_point] hashes in two colors at varying distances")
})

test_that("it plots interactive models (quant. * cat.)", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety * condition, data = er)) %>%
    expect_doppelganger("[gf_point] diverging lines in two colors")

  gf_point(base_anxiety ~ later_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety * condition, data = er)) %>%
    expect_doppelganger("[gf_point] diverging lines in two colors, flipped")
})

test_that("it plots interactive models (cat. * quant.)", {
  gf_point(later_anxiety ~ condition, color = ~base_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ condition * base_anxiety, data = er)) %>%
    expect_doppelganger("[gf_point] non-parallel hashes in three colors (at M, +-SD)")
})

test_that("it plots interactive models (quant. * quant.)", {
  gf_point(later_anxiety ~ base_anxiety, color = ~base_depression, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety * base_depression, data = er)) %>%
    expect_doppelganger("[gf_point] crossing lines in two colors")
})


# Two predictors, on axis and facet -----------------------------------------------------------

test_that("it plots main effect models across facets (cat. + cat.)", {
  gf_point(later_anxiety ~ provider | condition, data = er) %>%
    gf_model(lm(later_anxiety ~ provider + condition, data = er)) %>%
    expect_doppelganger("[gf_point] hashes at an offset across facets")
})

test_that("it plots main effect models across facets (quant. + cat.)", {
  gf_point(later_anxiety ~ base_anxiety | condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety + condition, data = er)) %>%
    expect_doppelganger("[gf_point] parallel lines in different facets")
})

test_that("it plots interactive models across facets (cat. * cat.)", {
  gf_point(later_anxiety ~ provider | condition, data = er) %>%
    gf_model(lm(later_anxiety ~ provider * condition, data = er)) %>%
    expect_doppelganger("[gf_point] hashes with different patterns across facets")
})

test_that("it plots interactive models across facets (quant. * cat.)", {
  gf_point(later_anxiety ~ base_anxiety | condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety * condition, data = er)) %>%
    expect_doppelganger("[gf_point] diverging lines in different facets")

  gf_point(base_anxiety ~ later_anxiety | condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety + condition, data = er)) %>%
    expect_doppelganger("[gf_point] diverging lines in different facets, flipped")
})

# faceting on a quantitative variable isn't advisable -- maybe just show a warning for this?
# it plots main effect models across facets (cat. + quant.)
# it plots main effect models across facets (quant. + quant.)
# it plots interactive models across facets (cat. * quant.)
# it plots interactive models across facets (quant. * quant.)


# Mappings ------------------------------------------------------------------------------------

test_that("it respects static aesthetic choices", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er), color = "blue") %>%
    expect_doppelganger("[gf_point] model line is blue")
})

test_that("it un-maps dynamic aesthetics from underlying layers that are not in the model", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, shape = ~provider, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_doppelganger("[gf_point] anx. mod., outcome on Y, with color & shape")
})

test_that("it will translate color arguments if applicable (e.g. fill to color)", {
  gf_boxplot(later_anxiety ~ provider, fill = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er)) %>%
    expect_doppelganger("[gf_point] cond. mod., outcome on Y, with color")
})

test_that("it can use aesthetics other than color... just checking", {
  gf_point(later_anxiety ~ base_anxiety, shape = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er)) %>%
    expect_doppelganger("[gf_point] cond. mod., outcome on Y, pred. on shape")
})

test_that("it allows mapping new aesthetics", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er), linetype = ~condition) %>%
    expect_doppelganger("[gf_point] cond. mod., outcome on Y, pred. on color, linetype")
})



# Alternate specification ---------------------------------------------------------------------

test_that("you can pass it a formula instead of an `lm()` object", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(later_anxiety ~ condition) %>%
    expect_doppelganger("should look the same as if you did the lm() version")
})


# Other tests ---------------------------------------------------------------------------------

test_that("it treats boolean and character predictors like factors", {
  new_er <- er %>%
    mutate(base_anxiety_high = base_anxiety > 5)

  gf_point(later_anxiety ~ base_anxiety_high, data = new_er) %>%
    gf_model(later_anxiety ~ base_anxiety_high) %>%
    expect_doppelganger("[gf_point] hashes TRUE higher than FALSE")
})
