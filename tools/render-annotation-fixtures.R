#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !nzchar(args[[1L]])) {
  stop("Usage: render-annotation-fixtures.R OUTPUT_DIR", call. = FALSE)
}
output_dir <- args[[1L]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

candidate_library <- Sys.getenv("COURSEKATA_FIXTURE_LIBRARY")
revision <- Sys.getenv("COURSEKATA_FIXTURE_REVISION")
if (!nzchar(candidate_library) || !nzchar(revision)) {
  stop(
    "Set COURSEKATA_FIXTURE_LIBRARY and COURSEKATA_FIXTURE_REVISION ",
    "to identify the installed candidate being rendered.",
    call. = FALSE
  )
}
.libPaths(c(normalizePath(candidate_library, mustWork = TRUE), .libPaths()))

suppressPackageStartupMessages({
  library(coursekata)
  library(ggplot2)
})

loaded_library <- dirname(normalizePath(find.package("coursekata")))
if (!identical(loaded_library, normalizePath(candidate_library))) {
  stop("The requested candidate library was not loaded.", call. = FALSE)
}

manifest <- list()
manifest_common <- list(
  role = "candidate",
  revision = revision,
  coursekata = as.character(packageVersion("coursekata")),
  ggplot2 = as.character(packageVersion("ggplot2")),
  ggformula = as.character(packageVersion("ggformula")),
  R = R.version.string,
  platform = R.version$platform,
  library = loaded_library,
  locale = Sys.getlocale("LC_CTYPE"),
  font_family = if (is.null(ggplot2::theme_get()$text$family)) {
    ""
  } else {
    ggplot2::theme_get()$text$family
  },
  seed = "42"
)

set.seed(42)
base <- gf_histogram(
  ~Thumb, data = Fingers, bins = 30, fill = ~middle(Thumb, .95)
)

cutoff_one <- suppressMessages(show_cutoffs(base, show_labels = TRUE))
cutoff_three <- suppressMessages(
  base |>
    show_cutoffs(middle(Thumb, .999), show_labels = TRUE) |>
    show_cutoffs(
      middle(Thumb, .95), color = "firebrick", show_labels = TRUE
    ) |>
    show_cutoffs(
      middle(Thumb, .80), color = "darkgreen", show_labels = TRUE
    )
)
dgp <- suppressMessages(show_dgp(base))
faceted <- gf_histogram(
  ~Thumb | Sex, data = Fingers, bins = 20, fill = ~middle(Thumb, .95)
)

fixtures <- list(
  show_dgp = list(plot = dgp, sizes = list(c(4, 3), c(6, 4), c(8, 6))),
  cutoff_one = list(
    plot = cutoff_one, sizes = list(c(4, 3), c(6, 4), c(8, 6))
  ),
  cutoff_three = list(
    plot = cutoff_three, sizes = list(c(4, 3), c(6, 4), c(8, 6))
  ),
  cutoff_three_flip = list(
    plot = cutoff_three + coord_flip(),
    sizes = list(c(4, 3), c(6, 4), c(8, 6))
  ),
  dgp_cutoff_one = list(
    plot = suppressMessages(show_cutoffs(dgp, show_labels = TRUE)),
    sizes = list(c(6, 4), c(8, 6))
  ),
  dgp_cutoff_three = list(
    plot = suppressMessages(
      dgp |>
        show_cutoffs(middle(Thumb, .999), show_labels = TRUE) |>
        show_cutoffs(
          middle(Thumb, .95), color = "firebrick", show_labels = TRUE
        ) |>
        show_cutoffs(
          middle(Thumb, .80), color = "darkgreen", show_labels = TRUE
        )
    ),
    sizes = list(c(8, 6))
  ),
  cutoff_flip = list(
    plot = suppressMessages(show_cutoffs(
      base + coord_flip(), show_labels = TRUE
    )),
    sizes = list(c(4, 3), c(6, 4), c(8, 6))
  ),
  cutoff_faceted = list(
    plot = suppressMessages(show_cutoffs(faceted, show_labels = TRUE)),
    sizes = list(c(8, 6))
  )
)

for (fixture_name in names(fixtures)) {
  fixture <- fixtures[[fixture_name]]
  for (size in fixture$sizes) {
    suffix <- paste0(size[[1L]], "x", size[[2L]])
    for (extension in c("png", "svg")) {
      path <- file.path(
        output_dir, paste0(fixture_name, "-", suffix, ".", extension)
      )
      ggsave(
        path, fixture$plot, width = size[[1L]], height = size[[2L]],
        units = "in", dpi = 144, bg = "white",
        device = if (extension == "svg") grDevices::svg else grDevices::png
      )
      manifest[[length(manifest) + 1L]] <- c(
        list(
          artifact = basename(path), fixture = fixture_name,
          width_in = as.character(size[[1L]]),
          height_in = as.character(size[[2L]]), dpi = "144",
          format = extension,
          device = if (extension == "svg") "grDevices::svg" else "grDevices::png"
        ),
        manifest_common
      )
    }
  }
}

write.dcf(
  do.call(rbind.data.frame, c(manifest, list(stringsAsFactors = FALSE))),
  file.path(output_dir, "manifest.dcf")
)
