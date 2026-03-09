# .onAttach() assigns .conflicts.OK into the package environment. After the
# initial library() call the namespace is sealed and bindings are locked, so
# subsequent .onAttach() calls (from tests) would fail. This helper unlocks
# the binding so the assign can succeed.
unlock_conflicts_ok <- function() {
  pkg_env <- as.environment("package:coursekata")
  if (exists(".conflicts.OK", envir = pkg_env) && bindingIsLocked(".conflicts.OK", pkg_env)) {
    unlockBinding(".conflicts.OK", pkg_env)
  }
}

# --- US1: Auto-Detection in JupyterLite ---

test_that("is_emscripten() returns TRUE when R.version$os is 'emscripten'", {
  local_mocked_bindings(is_emscripten = function() TRUE)
  expect_true(is_emscripten())
})

test_that("is_emscripten() returns FALSE for standard platforms", {
  # On standard test runners, R.version$os is never "emscripten"
  expect_false(is_emscripten())
})

test_that("check_missing() returns FALSE when option is unset and Emscripten is detected", {
  local_mocked_bindings(is_emscripten = function() TRUE)
  withr::with_options(list(coursekata.check_missing = NULL), {
    expect_false(check_missing())
  })
})

test_that("auto-detection works for any Emscripten-based WASM environment", {
  local_mocked_bindings(is_emscripten = function() TRUE)
  withr::with_options(list(coursekata.check_missing = NULL), {
    # Future non-JupyterLite WASM environments also use Emscripten
    expect_false(check_missing())
  })
})

# --- US2: Manual Override via Option ---

test_that("check_missing() returns FALSE when option is FALSE on non-Emscripten", {
  local_mocked_bindings(is_emscripten = function() FALSE)
  withr::with_options(list(coursekata.check_missing = FALSE), {
    expect_false(check_missing())
  })
})

test_that("check_missing() returns TRUE when option is TRUE on Emscripten", {
  local_mocked_bindings(is_emscripten = function() TRUE)
  withr::with_options(list(coursekata.check_missing = TRUE), {
    expect_true(check_missing())
  })
})

test_that("non-logical option values are treated as NULL (auto-detect)", {
  local_mocked_bindings(is_emscripten = function() FALSE)
  for (val in list("yes", 1, NA, NA_real_)) {
    withr::with_options(list(coursekata.check_missing = val), {
      # Non-logical -> treated as NULL -> auto-detect -> not Emscripten -> TRUE
      expect_true(check_missing(), info = paste("value:", deparse(val)))
    })
  }
})

# --- US3: Default Behavior Unchanged ---

test_that("check_missing() returns TRUE when option is unset and not Emscripten", {
  local_mocked_bindings(is_emscripten = function() FALSE)
  withr::with_options(list(coursekata.check_missing = NULL), {
    expect_true(check_missing())
  })
})

test_that("check_missing() returns TRUE when option is explicitly TRUE", {
  local_mocked_bindings(is_emscripten = function() FALSE)
  withr::with_options(list(coursekata.check_missing = TRUE), {
    expect_true(check_missing())
  })
})

# --- US4: Interaction with Quickstart Mode ---

test_that("quickstart=TRUE skips check regardless of check_missing value", {
  withr::with_options(list(coursekata.quickstart = TRUE, coursekata.check_missing = TRUE), {
    # quickstart() returns TRUE, so do_not_ask is TRUE regardless
    expect_true(quickstart())
  })
})

test_that("both quickstart=TRUE and check_missing=TRUE: quickstart wins", {
  withr::with_options(list(coursekata.quickstart = TRUE, coursekata.check_missing = TRUE), {
    # In .onAttach, do_not_ask = ... || quickstart() || ...
    # quickstart() is TRUE, so do_not_ask is TRUE
    expect_true(quickstart())
    expect_true(check_missing())
    # But quickstart wins because of OR short-circuit in do_not_ask
  })
})

test_that("check_missing=FALSE with quickstart=FALSE skips prompt", {
  local_mocked_bindings(is_emscripten = function() FALSE)
  withr::with_options(list(coursekata.quickstart = FALSE, coursekata.check_missing = FALSE), {
    expect_false(quickstart())
    expect_false(check_missing())
    # !check_missing() is TRUE, so do_not_ask becomes TRUE
  })
})


# --- Startup Message Behavior ---

test_that("startup messages are shown by default when coursekata.quiet is unset", {
  unlock_conflicts_ok()
  local_mocked_bindings(
    coursekata_attach = function(...) c(pkg = TRUE),
    coursekata_load_theme = function() NULL,
    coursekata_attach_message = function(pkgs) "attach info",
    coursekata_conflict_message = function() NULL,
    quickstart = function() FALSE
  )
  withr::with_options(list(coursekata.quiet = NULL), {
    expect_message(coursekata:::.onAttach(), "attach info")
  })
})

test_that("startup messages are suppressed when coursekata.quiet is TRUE", {
  unlock_conflicts_ok()
  local_mocked_bindings(
    coursekata_attach = function(...) c(pkg = TRUE),
    coursekata_load_theme = function() NULL,
    coursekata_attach_message = function(pkgs) "should not see",
    coursekata_conflict_message = function() NULL,
    quickstart = function() FALSE
  )
  withr::with_options(list(coursekata.quiet = TRUE), {
    expect_message(coursekata:::.onAttach(), NA)
  })
})

test_that("startup messages are suppressed in quickstart mode", {
  unlock_conflicts_ok()
  local_mocked_bindings(
    coursekata_attach = function(...) c(pkg = TRUE),
    coursekata_load_theme = function() NULL,
    coursekata_attach_message = function(pkgs) "should not see",
    coursekata_conflict_message = function() NULL,
    quickstart = function() TRUE
  )
  withr::with_options(list(coursekata.quiet = FALSE), {
    expect_message(coursekata:::.onAttach(), NA)
  })
})

test_that("conflict messages are included in startup output", {
  unlock_conflicts_ok()
  local_mocked_bindings(
    coursekata_attach = function(...) c(pkg = TRUE),
    coursekata_load_theme = function() NULL,
    coursekata_attach_message = function(pkgs) "attach info",
    coursekata_conflict_message = function() "conflict info",
    quickstart = function() FALSE
  )
  withr::with_options(list(coursekata.quiet = FALSE), {
    expect_message(coursekata:::.onAttach(), "conflict info")
  })
})

test_that(".conflicts.OK is set in the package environment after attach", {
  unlock_conflicts_ok()
  local_mocked_bindings(
    coursekata_attach = function(...) c(pkg = TRUE),
    coursekata_load_theme = function() NULL,
    coursekata_attach_message = function(pkgs) NULL,
    coursekata_conflict_message = function() NULL,
    quickstart = function() FALSE
  )
  pkg_env <- as.environment("package:coursekata")
  assign(".conflicts.OK", FALSE, envir = pkg_env)
  withr::defer(assign(".conflicts.OK", TRUE, envir = pkg_env))

  withr::with_options(list(coursekata.quiet = TRUE), {
    coursekata:::.onAttach()
  })
  expect_true(get(".conflicts.OK", envir = pkg_env))
})
