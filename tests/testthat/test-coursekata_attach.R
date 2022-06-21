test_that("all course packages are listed with version and whether attached", {
  attachments <- suppressMessages(coursekata_attach())
  expect_identical(attachments, coursekata_packages())
})


test_that("a nicely formatted message is displayed when attaching the packages", {
  info <- coursekata_packages()
  purrr::walk(info$package, function(package) {
    expect_message(coursekata_attach(), sprintf(".*[vx] %s +.*", package))
  })
})
