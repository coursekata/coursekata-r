test_that("all course packages are listed with version and whether attached", {
  # use these packages because they are not imported
  # they need to be in the order they appear in the coursekata_pkgs object
  pkgs <- c("fivethirtyeightdata", "fivethirtyeight")
  # fivethirtyeightdata is not always installed, and if so, it won't get attached
  # detach/re-attach only if it is installed
  installed <- pkg_is_installed(pkgs)
  detacher(pkgs[installed])
  withr::defer(attacher(pkgs[installed]))

  # only the installed package will be attached
  attachments <- coursekata_attach(quietly = TRUE)
  names(installed) <- pkgs
  expect_identical(attachments, installed)
})


test_that("a nicely formatted message is displayed when attaching the packages", {
  msg <- coursekata_attach_message(coursekata_pkgs)
  purrr::walk(coursekata_pkgs, function(package) {
    expect_match(msg, sprintf(".*[vx] %s +.*", package))
  })
})
