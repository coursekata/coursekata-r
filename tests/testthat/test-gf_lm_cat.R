test_that("gf_lm_cat adds group-mean segments", {
  p <- gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1)
  result <- suppressMessages(gf_lm_cat(p))

  expect_s3_class(result, "ggplot")
  geoms <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% geoms)
})

test_that("gf_lm_cat segments sit at the group means", {
  p <- gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1)
  result <- suppressMessages(gf_lm_cat(p))

  seg <- result$layers[[length(result$layers)]]$data
  means <- tapply(TipExperiment$Tip, TipExperiment$Condition, mean, na.rm = TRUE)
  expect_equal(sort(unique(seg$y)), sort(as.numeric(means)), ignore_attr = TRUE)
})

test_that("gf_lm_cat rejects a continuous x", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  expect_error(suppressMessages(gf_lm_cat(p)), "categorical")
})

test_that("gf_lm_cat snapshot", {
  skip_if_not_installed("vdiffr")
  # gf_point (not gf_jitter) keeps dot positions deterministic for the snapshot
  suppressMessages(
    gf_point(Tip ~ Condition, data = TipExperiment) %>%
      gf_lm_cat()
  ) %>%
    expect_doppelganger("gf_lm_cat-basic")
})
