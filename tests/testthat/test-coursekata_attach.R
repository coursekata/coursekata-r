test_that("all course packages are listed with version and whether attached", {
  # these packages are not imported, but are still in the load out
  # (important because they can be detached and re-attached during testing)
  pkgs <- c("fivethirtyeightdata", "fivethirtyeight", "Lock5withR")

  # they need to be in the order they appear in the coursekata_pkgs object
  pkgs <- pkgs[order(match(pkgs, coursekata_pkgs))]

  # detach/re-attach only installed packages
  installed <- pkg_is_installed(pkgs)
  names(installed) <- pkgs
  detacher(pkgs[installed])
  withr::defer(attacher(pkgs[installed]))

  # only the installed package will be attached
  attachments <- coursekata_attach(quietly = TRUE)
  expect_identical(attachments, installed)
})


test_that("a nicely formatted message is displayed when attaching the packages", {
  msg <- coursekata_attach_message(coursekata_pkgs)
  purrr::walk(coursekata_pkgs, function(package) {
    expect_match(msg, sprintf(".*[vx] %s +.*", package))
  })
})
