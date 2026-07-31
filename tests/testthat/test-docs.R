# Three rules about the documentation, each of which a person can only break by
# accident. A `[topic()]` link inside an `@noRd` block points at a page that does
# not exist and never will, so it is prose, not a link -- roxygen says so on every
# regeneration, where a real problem then has to compete with it. And a reference
# page is read by someone who was never shown the old behaviour: arguing against a
# premise there teaches it and then withdraws it. That story belongs in NEWS.md,
# whose reader arrives already holding the premise, because they wrote code on it.
#
# roxygen accepts a topic link in two shapes: `[topic()]` for a function and the
# bare `[Topic]` for anything else -- a class, a data object, a `@format` tag. A
# link immediately followed by `(url)` is a markdown link, not either shape, so it
# is excluded by requiring the bracket not be followed by `(`. A bracket wrapped
# in backticks, like `` [`lm`] ``, is roxygen's syntax for a link resolved against
# every loaded namespace rather than this package's own `man/`, so it is excluded
# the same way the original scanner excluded `[pkg::topic()]` -- by refusing a
# character the local-alias syntax never contains.

pkg_dir <- function(name) {
  path <- testthat::test_path("..", "..", name)
  skip_if_not(dir.exists(path), paste0(name, "/ is only present in the source tree"))
  path
}

pkg_file <- function(name) {
  path <- testthat::test_path("..", "..", name)
  skip_if_not(file.exists(path), paste0(name, " is only present in the source tree"))
  path
}

documented_topics <- function() {
  rd <- list.files(pkg_dir("man"), pattern = "[.]Rd$", full.names = TRUE)
  unlist(lapply(rd, function(f) {
    sub("^\\\\alias\\{(.*)\\}$", "\\1",
        grep("^\\\\alias\\{", readLines(f, warn = FALSE), value = TRUE))
  }))
}

test_that("an internal help block does not link to a topic that has no page", {
  topics <- documented_topics()
  sources <- list.files(pkg_dir("R"), pattern = "[.][Rr]$", full.names = TRUE)
  unresolved <- unlist(lapply(sources, function(f) {
    lines <- readLines(f, warn = FALSE)
    unlist(lapply(grep("^\\s*#'", lines), function(i) {
      links <- regmatches(
        lines[i],
        gregexpr("\\[[^]|:`[ ]+(\\(\\))?\\](?!\\()", lines[i], perl = TRUE)
      )[[1]]
      topic_of <- sub("\\(\\)$", "", sub("^\\[(.*)\\]$", "\\1", links))
      bad <- links[!topic_of %in% topics]
      if (length(bad)) sprintf("%s:%d: %s", basename(f), i, bad)
    }))
  }))
  expect_equal(unresolved, NULL)
})

test_that("a reference page does not argue against a premise the reader never had", {
  # `deprecated` is deliberately absent: an argument that still works but is on its
  # way out IS a premise the reader has, and saying so on its help page is right.
  banned <- paste0(
    "(?i)\\b(no longer|accepted but ignored|it used to|which used to|used to be|",
    "has been replaced|as it did before|in earlier versions)\\b"
  )
  pages <- list.files(pkg_dir("man"), pattern = "[.]Rd$", full.names = TRUE)
  hits <- unlist(lapply(pages, function(f) {
    lines <- readLines(f, warn = FALSE)
    k <- grep(banned, lines, perl = TRUE)
    if (length(k)) sprintf("%s:%d: %s", basename(f), k, trimws(lines[k]))
  }))
  expect_equal(hits, NULL)
})

# The `ggformula` floor is an install-time claim, and no test can check it.
# `layer_factory()` bakes the `Stat` and `Geom` it captured into the lazy-load
# database while the package is being installed, so an installed copy either
# survived that or was never built: by the time a test session starts, the question
# has been answered. What a test can do is keep the floor in one place, so that the
# procedure a maintainer follows verifies the version the package declares. The
# procedure, and the reason each floor is where it is, are in
# `.github/CONTRIBUTING.md`.

declared_floors <- function() {
  entries <- trimws(strsplit(read.dcf(pkg_file("DESCRIPTION"))[1, "Imports"], ",")[[1]])
  found <- Filter(length, regmatches(
    entries,
    regexec("^([^[:space:](]+)[[:space:]]*\\(>=[[:space:]]*([^)]+)\\)$", entries)
  ))
  setNames(
    trimws(vapply(found, `[[`, character(1), 3L)),
    vapply(found, `[[`, character(1), 2L)
  )
}

verified_floors <- function() {
  lines <- readLines(pkg_file(".github/CONTRIBUTING.md"), warn = FALSE)
  sections <- cumsum(grepl("^## ", lines))
  rows <- lines[sections %in% sections[grepl("^## Dependency floors", lines)]]
  found <- Filter(length, regmatches(
    rows,
    regexec("^\\|[[:space:]]*`([^`]+)`[[:space:]]*\\|[[:space:]]*([^|[:space:]]+)", rows)
  ))
  setNames(
    vapply(found, `[[`, character(1), 3L),
    vapply(found, `[[`, character(1), 2L)
  )
}

test_that("DESCRIPTION and the floor-check procedure name the same minimum versions", {
  declared <- declared_floors()
  verified <- verified_floors()
  expect_setequal(names(verified), c("ggformula", "ggplot2"))
  expect_identical(declared["ggformula"], verified["ggformula"])
  expect_identical(declared["ggplot2"], verified["ggplot2"])
})
