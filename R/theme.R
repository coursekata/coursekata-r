#' A simple theme built on top of [`ggplot2::theme_bw`]
#'
#' The `coursekata` package automatically loads this theme when the package is loaded. This is in
#' addition to a number of other plot tweaks and option settings. To just restore the theme to the
#' default, you can run `set_theme(theme_grey)`. If you want to restore all plot related settings
#' and/or prevent them when loading the package, see [`coursekata_unload_theme`].
#'
#' @return A gg theme object
#'
#' @export
#' @examples
#' gf_boxplot(Thumb ~ RaceEthnic, data = Fingers, fill = ~RaceEthnic)
theme_coursekata <- function() {
  ggplot2::theme_bw() + ggplot2::theme(
    # Fonts
    plot.title = ggplot2::element_text(face = "bold", size = 13),
    axis.title = ggplot2::element_text(size = 11),

    # Legend
    legend.background = ggplot2::element_rect(fill = "white", linewidth = 4, colour = "white"),
    legend.position = "right",

    # Grid
    axis.ticks = ggplot2::element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.major = ggplot2::element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.minor = ggplot2::element_blank(),

    # Facet labels
    strip.background = ggplot2::element_rect(fill = "#EEEEEE"),
    strip.text = ggplot2::element_text(size = 11)
  )
}


# The theme helper changes process-wide state owned by ggplot2 and base R. Keep
# the state from the first load in an active load/unload cycle so calling
# coursekata_load_theme() twice does not replace the caller's restoration point.
.coursekata_theme_state <- new.env(parent = emptyenv())
.coursekata_theme_state$active <- FALSE
.coursekata_theme_state$snapshot <- NULL

coursekata_theme_option_names <- function() {
  c(
    "repr.plot.width", "repr.plot.height",
    "ggplot2.discrete.fill", "ggplot2.discrete.colour",
    "ggplot2.continuous.fill", "ggplot2.continuous.colour"
  )
}

coursekata_theme_option_state <- function() {
  current <- options()
  option_names <- coursekata_theme_option_names()
  current[intersect(option_names, names(current))]
}

restore_coursekata_options <- function(saved) {
  option_names <- coursekata_theme_option_names()
  options(stats::setNames(
    rep(list(NULL), length(option_names)), option_names
  ))
  if (length(saved) > 0L) options(saved)
  invisible()
}

coursekata_theme_geom_defaults <- function() {
  list(
    bar = ggplot2::aes(
      colour = "black", fill = coursekata_palette(1), linewidth = 0.1,
      alpha = 0.7
    ),
    boxplot = ggplot2::aes(
      colour = "black", fill = coursekata_palette(1), alpha = 0.6
    ),
    hline = ggplot2::aes(
      colour = coursekata_palette("blue80"), linewidth = 1
    ),
    line = ggplot2::aes(colour = "black", linewidth = 1),
    lm = ggplot2::aes(
      colour = coursekata_palette("blue80"), linewidth = 1
    ),
    point = ggplot2::aes(colour = "black", size = 2, alpha = 0.6),
    segment = ggplot2::aes(
      colour = coursekata_palette("blue80"), linewidth = 1
    ),
    smooth = ggplot2::aes(
      colour = coursekata_palette("blue80"), linewidth = 1
    ),
    violin = ggplot2::aes(
      colour = "black", fill = coursekata_palette(1), alpha = 0.6
    ),
    vline = ggplot2::aes(
      colour = coursekata_palette("blue80"), linewidth = 1
    )
  )
}


#' The color palettes used in our theme system
#'
#' @param indices The indices of the colors to pull (or all colors if no indices are given).
#'
#' @return A named list of the requested colors in the palette.
#'
#' @export
#' @examples
#' coursekata_palette()
#' coursekata_palette(c(1, 3, 5))
coursekata_palette <- function(indices = integer(0)) {
  # original order (carbon palette)
  # palette <- list(
  #   purple70 = "#6929c4",
  #   cyan50 = "#1192e8",
  #   teal70 = "#005d5d",
  #   magenta70 = "#9f1853",
  #   red50 = "#fa4d56",
  #   red90 = "#570408",
  #   green60 = "#198038",
  #   blue80 = "#002d9c",
  #   magenta50 = "#ee538b",
  #   yellow50 = "#b28600",
  #   teal50 = "#009d9a",
  #   cyan90 = "#012749",
  #   orange70 = "#8a3800",
  #   purple50 = "#a56eff",
  #   gray30 = "gray30",
  # )

  # Ji's order
  palette <- list(
    teal50 = "#009d9a",
    purple70 = "#6929c4",
    orange70 = "#8a3800",
    red50 = "#fa4d56",
    green60 = "#198038",
    cyan90 = "#012749",
    yellow50 = "#b28600",
    red90 = "#570408",
    cyan50 = "#1192e8",
    teal70 = "#005d5d",
    magenta70 = "#9f1853",
    blue80 = "#002d9c",
    purple50 = "#a56eff",
    magenta50 = "#ee538b",
    gray30 = "gray30"
  )

  if (length(indices) == 0) {
    indices <- seq_along(palette)
  }

  out_of_range <- purrr::map_lgl(palette[indices], is.null)
  if (any(out_of_range)) {
    oor_idx <- paste(which(out_of_range), sep = ", ")
    rlang::abort(paste(
      glue::glue("Palette only has {length(palette)} colors."),
      glue::glue("There is no color at index {oor_idx}")
    ))
  }

  palette[indices]
}

#' Create a function that provides a colorblind palette.
#'
#' @return A function that accepts one argument `n`, which is the number of colors you want to use
#'   in the plot. This function is used by scales like `scale_color_discrete` to provide colorblind-
#'   safe palettes. Where possible, the function will use the hand-picked colors from
#' [`coursekata_palette()`], and when more colors are needed than are available, it will use the
#' [`viridisLite::viridis()`] palette.
#'
#' @seealso scale_discrete_coursekata
#' @export
#' @examples
#' palette <- coursekata_palette_provider()
#' palette(3)
coursekata_palette_provider <- function() {
  unwrap <- function(x) unlist(unname(x))

  palette <- unwrap(coursekata_palette())
  max_values <- length(palette)

  provider <- function(n) {
    if (n > max_values) {
      viridisLite::viridis(n)
    } else {
      palette[seq_len(n)]
    }
  }

  structure(provider, max_n = max_values)
}


#' A discrete color scale constructor with colorblind-safe palettes.
#'
#' See [`coursekata_palette()`] for more information.
#'
#' @param ... Additional parameters passed on to the scale type.
#'
#' @return A discrete color scale.
#'
#' @seealso coursekata_palette
#' @export
#' @examples
#' gf_point(Thumb ~ Height, data = Fingers, color = ~RaceEthnic) +
#'   scale_discrete_coursekata()
scale_discrete_coursekata <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = c("colour", "fill"),
    palette = coursekata_palette_provider(),
    ...
  )
}


#' Utility function for loading all themes.
#'
#' This function is called at package start-up and should rarely be needed by the user. The
#' exception is when the user has called [`coursekata_unload_theme()`] and wants to go back to the
#' CourseKata look and feel. When run, this function sets the CourseKata color palettes
#' [`coursekata_palette()`], sets the default theme to [`theme_coursekata()`], and tweaks some
#' default settings for specific plots. To restore the plotting settings that
#' were active before this function first changed them, run [`coursekata_unload_theme()`].
#'
#' @return No return value, called to adjust the global state of `ggplot2`.
#'
#' @seealso coursekata_palette theme_coursekata scale_discrete_coursekata coursekata_unload_theme
#' @export
coursekata_load_theme <- function() {
  first_load <- !isTRUE(.coursekata_theme_state$active)
  if (first_load) {
    .coursekata_theme_state$snapshot <- list(
      theme = ggplot2::theme_get(),
      options = coursekata_theme_option_state(),
      geom_defaults = list()
    )
    .coursekata_theme_state$active <- TRUE
  }

  defaults <- coursekata_theme_geom_defaults()
  for (geom in names(defaults)) {
    previous <- ggplot2::update_geom_defaults(geom, defaults[[geom]])
    if (first_load) {
      saved <- .coursekata_theme_state$snapshot
      saved$geom_defaults[[geom]] <- previous
      .coursekata_theme_state$snapshot <- saved
    }
  }

  ggplot2::theme_set(theme_coursekata())

  options(
    repr.plot.width = 6,
    repr.plot.height = 4,
    ggplot2.discrete.fill = scale_discrete_coursekata,
    ggplot2.discrete.colour = scale_discrete_coursekata,
    ggplot2.continuous.fill = "viridis",
    ggplot2.continuous.colour = "viridis"
  )

  invisible()
}


#' Restore the caller's plotting settings
#'
#' This function restores the theme, options, and geom defaults that were active
#' before [`coursekata_load_theme()`] first changed them. Calling it again before
#' another load has no effect. To go back to the CourseKata look and feel, run
#' [`coursekata_load_theme()`].
#'
#' @return No return value, called to restore the global state of `ggplot2`.
#'
#' @seealso coursekata_load_theme
#' @export
coursekata_unload_theme <- function() {
  if (!isTRUE(.coursekata_theme_state$active)) return(invisible())

  saved <- .coursekata_theme_state$snapshot
  for (geom in names(saved$geom_defaults)) {
    ggplot2::update_geom_defaults(geom, saved$geom_defaults[[geom]])
  }
  ggplot2::theme_set(saved$theme)
  restore_coursekata_options(saved$options)

  .coursekata_theme_state$active <- FALSE
  .coursekata_theme_state$snapshot <- NULL

  invisible()
}
