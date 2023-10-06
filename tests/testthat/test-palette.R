test_that("it provides hand-picked colors where available", {
  max_colors <- length(coursekata_palette())
  provider <- coursekata_palette_provider()
  expect_equal(provider(max_colors), as.character(coursekata_palette()))
})

test_that("it uses viridisLite when more colors are needed", {
  provider <- coursekata_palette_provider()
  expect_equal(provider(20), viridisLite::viridis(20))
})
