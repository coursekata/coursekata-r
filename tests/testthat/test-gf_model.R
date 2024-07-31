# Constraints ---------------------------------------------------------------------------------

# TODO: why? shouldn't we be able to determine a default representation?
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
