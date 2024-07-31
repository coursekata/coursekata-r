test_that("attaching packages reports which packages we attempted to attach and their success", {
  installed <- pkg_is_installed(coursekata_pkgs)
  not_attached_but_installed <- installed[!pkg_is_attached(coursekata_pkgs)]
  expect_identical(coursekata_attach(quietly = TRUE), not_attached_but_installed)
})


test_that("a nicely formatted message is displayed when attaching the packages", {
  msg <- coursekata_attach_message(coursekata_pkgs)
  purrr::walk(coursekata_pkgs, function(package) {
    expect_match(msg, sprintf(".*[vx] %s +.*", package))
  })
})
