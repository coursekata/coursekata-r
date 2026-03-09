#' Attach the CourseKata course packages
#'
#' @param do_not_ask Prevent asking the user to install missing packages (they are skipped).
#' @param quietly Whether to suppress messages.
#'
#' @return A named logical vector indicating which packages were attached.
#'
#' @export
#' @examples
#' coursekata_attach()
coursekata_attach <- function(do_not_ask = FALSE, quietly = FALSE) {
  !do_not_ask && pkg_check_installed(coursekata_pkgs)
  detached <- coursekata_detached()
  installed <- coursekata_pkgs[pkg_is_installed(coursekata_pkgs)]
  attached <- pkg_require(detached[detached %in% installed], quietly = quietly)

  result <- rep_named(detached, FALSE)
  result[names(attached)] <- TRUE
  invisible(result)
}


#' Information about CourseKata packages.
#'
#' @param pkgs A character vector of packages being loaded.
#'
#' @return A coursekata_attachments object, also of class data.frame with a row for each course
#'   package and a column for each of the `package` name, `version`, and whether it is currently
#'   `attached`.
#'
#' @noRd
coursekata_attach_message <- function(pkgs) {
  if (length(pkgs) == 0) {
    return(NULL)
  }

  info <- coursekata_packages()
  version <- ifelse(is.na(info$version), "", info$version)
  pkgs <- paste(
    ifelse(info$attached, cli::col_green(cli::symbol$tick), cli::col_red("x")),
    cli::col_green(format(info$package)),
    cli::ansi_align(version, max(cli::ansi_nchar(version)))
  )

  paste(
    cli::rule(
      left = cli::style_bold("CourseKata packages"),
      right = cli::style_bold("coursekata ", utils::packageVersion("coursekata"))
    ),
    to_cols(pkgs, 2),
    sep = "\n"
  )
}


#' Build a conflict message for coursekata exports that mask other packages.
#'
#' Detects coursekata exports that shadow objects in previously attached packages
#' (e.g. base, datasets, stats) and formats them in the tidyverse style.
#'
#' @return A formatted string, or NULL if there are no conflicts.
#'
#' @noRd
coursekata_conflict_message <- function() {
  conflicts <- coursekata_conflicts()
  if (length(conflicts) == 0) {
    return(NULL)
  }

  winner_pkg <- "coursekata"
  lines <- vapply(names(conflicts), function(name) {
    loser_pkg <- conflicts[[name]]$masked_from
    suffix <- if (conflicts[[name]]$is_function) "()" else ""
    paste0(
      cli::col_red(cli::symbol$cross), " ",
      cli::col_blue(winner_pkg), "::", cli::col_green(paste0(name, suffix)),
      " masks ",
      cli::col_blue(loser_pkg), "::", paste0(name, suffix)
    )
  }, character(1))

  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = cli::style_bold("coursekata conflicts")
  )
  paste(c(header, lines), collapse = "\n")
}


#' Detect coursekata exports that conflict with other attached packages.
#'
#' @return A named list. Each element is named after the conflicting object and
#'   contains `masked_from` (the package being masked) and `is_function`.
#'
#' @noRd
coursekata_conflicts <- function() {
  ck_env <- as.environment("package:coursekata")
  ck_exports <- ls(ck_env)
  # ignore internal objects
  ck_exports <- ck_exports[!startsWith(ck_exports, ".")]

  # packages on the search path that come after coursekata (i.e. were loaded before it)
  sp <- search()
  ck_pos <- match("package:coursekata", sp)
  if (is.na(ck_pos)) {
    return(list())
  }
  other_pkgs <- sp[seq(ck_pos + 1L, length(sp))]

  conflicts <- list()
  for (pkg in other_pkgs) {
    pkg_objs <- ls(pkg)
    masked <- intersect(ck_exports, pkg_objs)
    for (name in masked) {
      if (is.null(conflicts[[name]])) {
        pkg_name <- sub("^package:", "", pkg)
        conflicts[[name]] <- list(
          masked_from = pkg_name,
          is_function = is.function(get(name, envir = ck_env))
        )
      }
    }
  }
  conflicts
}


#' List all currently NOT attached CourseKata course packages
#'
#' @return A character vector of the course packages that are not attached.
#'
#' @noRd
coursekata_detached <- function() {
  coursekata_pkgs[!pkg_is_attached(coursekata_pkgs)]
}
