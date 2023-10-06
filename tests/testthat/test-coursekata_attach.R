test_that("all course packages are listed with version and whether attached", {
  pkgs <- "fivethirtyeight" # use this package because it is not imported
  detacher(pkgs)
  withr::defer(attacher(pkgs))

  attachments <- coursekata_attach()
  expect_identical(attachments, pkgs)
})


test_that("a nicely formatted message is displayed when attaching the packages", {
  msg <- coursekata_attach_message(coursekata_pkgs)
  purrr::walk(coursekata_pkgs, function(package) {
    expect_match(msg, sprintf(".*[vx] %s +.*", package))
  })
})
