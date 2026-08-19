# These assert against ABSOLUTE bounds rather than against the installed version,
# because a test that recomputes packageVersion("ggplot2") only restates the
# function it is checking. A predicate hardcoded to TRUE fails the upper bound; one
# hardcoded to FALSE fails the lower. Both mutations have happened.

test_that("the version predicate answers, rather than always agreeing or refusing", {
  # no ggplot2 that can load this package is below 0.0.1 or above 999
  expect_true(ggplot2_at_least("0.0.1"))
  expect_false(ggplot2_at_least("999.0.0"))
})

test_that("the version predicate reads the installed ggplot2, not a constant", {
  # its own version is the one boundary where TRUE and FALSE must both be reachable
  # from neighboring inputs; a constant cannot satisfy both
  here <- as.character(utils::packageVersion("ggplot2"))
  expect_true(ggplot2_at_least(here))
  expect_false(ggplot2_at_least(paste0(as.integer(sub("[.].*", "", here)) + 1L, ".0.0")))
})

test_that("the coord is named something this ggplot2 actually exports", {
  # the whole point of the helper: a refusal that names a function the reader does
  # not have sends them nowhere. Checking membership in the namespace catches a
  # wrong name without repeating the branch that chose it.
  name <- coord_transform_name()
  expect_true(name %in% getNamespaceExports("ggplot2"))
  expect_true(is.function(getExportedValue("ggplot2", name)))
})

test_that("the coord constructor builds a transforming coord, not a cartesian one", {
  # a fallback that quietly returned coord_cartesian() would leave squares
  # undistorted and every version-skew test still green
  coord <- coord_transform_compat(y = "sqrt")
  expect_s3_class(coord, "Coord")
  expect_false(inherits(coord, "CoordCartesian") && !inherits(coord, "CoordTrans"))
  expect_true(any(grepl("Trans|Transform", class(coord))))
})

test_that("the refusal quotes the coord this ggplot2 exports", {
  # the user-visible payoff, asserted through the public path rather than the helper
  d <- data.frame(x = c(1, 1, 2, 2, 2, 3))
  p <- suppressMessages(gf_squareplot(~x, data = d) %>% gf_refine(ggplot2::scale_y_sqrt()))
  # the check runs in the stat, so the refusal arrives at build rather than at the call
  err <- expect_error(ggplot2::ggplot_build(p))
  expect_true(grepl(coord_transform_name(), conditionMessage(err), fixed = TRUE))
})

test_that("the residual's jitter offers a hook for either release, and picks one", {
  # MUTATION: implementing only one of the two hooks. `PositionResidJitter` has
  # to land on the same offsets as the points layer, and ggplot2 moved where
  # that work happens: 3.5.2 implements `compute_layer` and jitters the whole
  # layer in one sequence, 4.0 implements `compute_panel` and re-seeds per
  # panel. Defining only one leaves whichever release wants the other falling
  # through to `Position`, which jitters nothing at all.
  #
  # What each hook actually computes is asserted by the faceted alignment test
  # in test-gf_resid_fun.R, which runs on both releases in CI. This one only
  # holds the shape, because only one release is installed at a time.
  expect_true(all(c("compute_layer", "compute_panel") %in% names(PositionResidJitter)))

  # and the choice tracks the object rather than a version number, so a release
  # that moves it again is followed without an edit
  expect_identical(
    jitter_is_per_panel(),
    "compute_panel" %in% names(ggplot2::PositionJitter)
  )
  expect_length(jitter_is_per_panel(), 1L)
})
