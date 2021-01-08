pkgs <- c(
  'supernova', 'mosaic', 'lsr',
  'fivethirtyeight', 'fivethirtyeightdata', 'Lock5withR', 'okcupiddata', 'dslabs'
)


test_that('all course packages are listed with version and whether attached', {
  packages <- suppressMessages(coursekata_packages())
  expect_identical(packages$package, pkgs)
  expect_identical(packages$version, pkg_version(pkgs))
  expect_identical(packages$attached, pkg_is_attached(pkgs))
})


test_that('detached packages are listed as not attached', {
  try(detach("package:supernova", unload = TRUE, character.only = TRUE), silent = TRUE)
  withr::defer(library(supernova))

  packages <- suppressMessages(coursekata_packages())
  expect_false(packages[match('supernova', packages$package), 'attached'])
})
