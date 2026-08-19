# Visual snapshots of gf_b(): whether the layout reads correctly -- do the
# labels overlap the arrows, is the run label on the correct side of the
# rise. Everything numeric is asserted in test-gf_b.R; these two exist only
# for what an assertion cannot state.

test_that("the continuous rise-over-run triangle renders", {
  model <- lm(Thumb ~ Height, data = Fingers)
  gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
    gf_b(model) %>%
    expect_doppelganger("gf_b continuous triangle")
})

test_that("the 3-group categorical arrows render", {
  set.seed(41)
  df <- data.frame(y = rnorm(60, 10, 3), g = factor(rep(c("a", "b", "c"), each = 20)))
  model <- lm(y ~ g, data = df)
  gf_jitter(y ~ g, data = df, width = .1, seed = 41) %>%
    gf_b(model) %>%
    expect_doppelganger("gf_b categorical arrows")
})
