test_that("all course packages are listed with version and whether attached", {
  packages <- suppressMessages(coursekata_packages())
  expect_identical(packages$package, coursekata_pkgs)
  expect_identical(packages$version, unname(pkg_version(coursekata_pkgs)))
  expect_identical(packages$attached, unname(pkg_is_attached(coursekata_pkgs)))
})


test_that("detached packages are listed as not attached", {
  detacher("fivethirtyeight")
  withr::defer(attacher("fivethirtyeight"))

  packages <- suppressMessages(coursekata_packages())
  expect_false(packages[match("fivethirtyeight", packages$package), "attached"])
})
