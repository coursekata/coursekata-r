#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !nzchar(args[[1L]])) {
  stop("Usage: render-coefficient-fixtures.R OUTPUT_DIR", call. = FALSE)
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
  seed = "41"
)

positive_fit <- lm(Thumb ~ Height, data = Fingers)
positive <- gf_point(Thumb ~ Height, data = Fingers, alpha = 0.3) |>
  gf_b(positive_fit)

negative <- data.frame(x = seq(-20, 20, length.out = 81))
negative$y <- 50 - 2.25 * negative$x + sin(negative$x)
negative_fit <- lm(y ~ x, data = negative)
negative_plot <- gf_point(y ~ x, data = negative, alpha = 0.3) |>
  gf_b(negative_fit)

flat <- data.frame(x = seq(-20, 20, length.out = 81), y = 50)
flat_fit <- lm(y ~ x, data = flat)

set.seed(41)
categorical <- data.frame(
  g = factor(rep(c("a", "b", "c"), each = 24)),
  y = c(rnorm(24, 10, 1.3), rnorm(24, 15, 1.3), rnorm(24, 6, 1.3))
)
categorical_fit <- lm(y ~ g, data = categorical)

faceted <- transform(
  Fingers,
  panel = factor(
    rep(c("one", "two", "three", "four", "five"), length.out = nrow(Fingers))
  )
)
facet_fit <- lm(Thumb ~ Height, data = faceted)

fixtures <- list(
  continuous_positive = positive,
  continuous_negative = negative_plot,
  continuous_negative_run = gf_point(
    Thumb ~ Height, data = Fingers, alpha = 0.3
  ) |>
    gf_b(positive_fit, run = -5, run_x = 65),
  continuous_zero_slope = gf_point(y ~ x, data = flat, alpha = 0.3) |>
    gf_b(flat_fit, run = 10, run_x = -10),
  continuous_transposed = gf_point(
    Height ~ Thumb, data = Fingers, alpha = 0.3
  ) |>
    gf_b(positive_fit),
  continuous_x_reverse = positive + scale_x_reverse(),
  continuous_y_reverse = positive + scale_y_reverse(),
  continuous_flip = positive + coord_flip(),
  continuous_flip_x_reverse = positive + scale_x_reverse() + coord_flip(),
  categorical = gf_jitter(
    y ~ g, data = categorical, width = 0.1, seed = 41
  ) |>
    gf_b(categorical_fit),
  categorical_transposed = gf_jitter(
    g ~ y, data = categorical, height = 0.1, seed = 41
  ) |>
    gf_b(categorical_fit),
  facet_continuous = gf_point(
    Thumb ~ Height, data = faceted, alpha = 0.25
  ) |>
    gf_facet_wrap(~panel, nrow = 1) |>
    gf_b(facet_fit)
)

for (fixture_name in names(fixtures)) {
  for (size in list(c(4, 3), c(6, 4), c(8, 6))) {
    suffix <- paste0(size[[1L]], "x", size[[2L]])
    for (extension in c("png", "svg")) {
      path <- file.path(
        output_dir, paste0(fixture_name, "-", suffix, ".", extension)
      )
      ggsave(
        path, fixtures[[fixture_name]], width = size[[1L]],
        height = size[[2L]], units = "in", dpi = 144, bg = "white",
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
