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


# --- coursekata_conflicts() ---

test_that("coursekata_conflicts detects exports that mask base packages", {
  conflicts <- coursekata_conflicts()
  # coursekata exports `outer` which masks `base::outer`
  expect_true("outer" %in% names(conflicts))
  expect_equal(conflicts$outer$masked_from, "base")
  expect_true(conflicts$outer$is_function)
})

test_that("coursekata_conflicts ignores internal objects starting with dot", {
  conflicts <- coursekata_conflicts()
  dot_names <- grep("^\\.", names(conflicts), value = TRUE)
  expect_length(dot_names, 0)
})

test_that("coursekata_conflicts identifies functions vs non-functions correctly", {
  conflicts <- coursekata_conflicts()
  ck_env <- as.environment("package:coursekata")
  for (name in names(conflicts)) {
    expect_equal(
      conflicts[[name]]$is_function,
      is.function(get(name, envir = ck_env)),
      info = paste("checking:", name)
    )
  }
})

test_that("coursekata_conflicts reports the closest masked package", {
  conflicts <- coursekata_conflicts()
  sp <- search()
  ck_pos <- match("package:coursekata", sp)
  other_pkgs <- sp[seq(ck_pos + 1L, length(sp))]

  for (name in names(conflicts)) {
    # the reported package should be the first one after coursekata that has this name
    expected_pkg <- NULL
    for (pkg in other_pkgs) {
      if (name %in% ls(pkg)) {
        expected_pkg <- sub("^package:", "", pkg)
        break
      }
    }
    expect_equal(
      conflicts[[name]]$masked_from, expected_pkg,
      info = paste("checking closest package for:", name)
    )
  }
})


# --- coursekata_conflict_message() ---

test_that("coursekata_conflict_message returns NULL when there are no conflicts", {
  local_mocked_bindings(coursekata_conflicts = function() list())
  expect_null(coursekata_conflict_message())
})

test_that("coursekata_conflict_message includes masked package names", {
  local_mocked_bindings(coursekata_conflicts = function() {
    list(outer = list(masked_from = "base", is_function = TRUE))
  })
  msg <- coursekata_conflict_message()
  expect_match(msg, "coursekata")
  expect_match(msg, "base")
  expect_match(msg, "outer")
})

test_that("coursekata_conflict_message appends parens to function conflicts", {
  local_mocked_bindings(coursekata_conflicts = function() {
    list(
      myfun = list(masked_from = "pkg", is_function = TRUE),
      mydata = list(masked_from = "pkg", is_function = FALSE)
    )
  })
  msg <- coursekata_conflict_message()
  expect_match(msg, "myfun\\(\\)")
  expect_false(grepl("mydata\\(\\)", msg))
  expect_match(msg, "mydata")
})
