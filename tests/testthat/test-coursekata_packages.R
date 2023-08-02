pkgs <- c(
  "supernova", "mosaic", "lsr", "Metrics",
  "fivethirtyeight", "fivethirtyeightdata", "Lock5withR", "dslabs"
)


test_that("all course packages are listed with version and whether attached", {
  packages <- suppressMessages(coursekata_packages())
  expect_identical(packages$package, pkgs)
  expect_identical(packages$version, pkg_version(pkgs))
  expect_identical(packages$attached, pkg_is_attached(pkgs))
})


test_that("detached packages are listed as not attached", {
  detacher("fivethirtyeight")
  withr::defer(attacher("fivethirtyeight"))

  packages <- suppressMessages(coursekata_packages())
  expect_false(packages[match("fivethirtyeight", packages$package), "attached"])
})
