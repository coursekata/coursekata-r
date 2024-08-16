test_that("all course packages are listed with version and whether attached", {
  packages <- suppressMessages(coursekata_packages())
  expect_identical(packages$package, coursekata_pkgs)
  expect_identical(packages$version, unname(pkg_version(coursekata_pkgs)))
  expect_identical(packages$attached, unname(pkg_is_attached(coursekata_pkgs)))
})


test_that("detached packages are listed as not attached", {
  detachable <- c("fivethirtyeightdata", "Lock5withR")
  detacher(detachable)
  withr::defer(attacher(detachable))

  packages <- suppressMessages(coursekata_packages())
  were_detached <- packages[match(detachable, packages$package), "attached"]
  expect_identical(rep_along(were_detached, FALSE), were_detached)
})
