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


#' The color palettes used in our theme system
#'
#' @param indices The indices of the colors to pull (or all colors if no indices are given).
#'
#' @return A named list of the requested colors in the palette.
#'
#' @export
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
coursekata_palette_provider <- function() {
  unwrap <- function(x) {
    x %>%
      unname() %>%
      unlist() %>%
      force()
  }

  palette <- coursekata_palette() %>% unwrap()
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
#' default settings for specific plots. To restore the original `ggplot2` settings, run
#' [`coursekata_unload_theme()`].
#'
#' @return No return value, called to adjust the global state of `ggplot2`.
#'
#' @seealso coursekata_palette theme_coursekata scale_discrete_coursekata coursekata_unload_theme
#' @export
coursekata_load_theme <- function() {
  ggplot2::update_geom_defaults("bar", ggplot2::aes(
    `colour` = "black",
    `fill` = coursekata_palette(1),
    `linewidth` = 0.1,
    `alpha` = 0.7
  ))

  ggplot2::update_geom_defaults("boxplot", ggplot2::aes(
    `colour` = "black",
    `fill` = coursekata_palette(1),
    `alpha` = .6
  ))

  ggplot2::update_geom_defaults("hline", ggplot2::aes(
    `colour` = coursekata_palette("blue80"),
    `linewidth` = 1
  ))

  ggplot2::update_geom_defaults("line", ggplot2::aes(
    `colour` = "black",
    `linewidth` = 1
  ))

  ggplot2::update_geom_defaults("lm", ggplot2::aes(
    `colour` = coursekata_palette("blue80"),
    `linewidth` = 1
  ))

  ggplot2::update_geom_defaults("point", ggplot2::aes(
    `colour` = "black",
    `size` = 2,
    `alpha` = 0.6
  ))

  ggplot2::update_geom_defaults("segment", ggplot2::aes(
    `colour` = coursekata_palette("blue80"),
    `linewidth` = 1
  ))

  ggplot2::update_geom_defaults("smooth", ggplot2::aes(
    `colour` = coursekata_palette("blue80"),
    `linewidth` = 1
  ))

  ggplot2::update_geom_defaults("violin", ggplot2::aes(
    `colour` = "black",
    `fill` = coursekata_palette(1),
    `alpha` = .6
  ))

  ggplot2::update_geom_defaults("vline", ggplot2::aes(
    `colour` = coursekata_palette("blue80"),
    `linewidth` = 1
  ))

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


#' Restore `ggplot2` default settings
#'
#' This function will restore all of the tweaks to themes and plotting to the original `ggplot2`
#' defaults. If you want to go back to the CourseKata look and feel, run
#' [`coursekata_load_theme()`].
#'
#' @return No return value, called to restore the global state of `ggplot2`.
#'
#' @seealso coursekata_load_theme
#' @export
coursekata_unload_theme <- function() {
  # find these values by creating a plot, storing it to a variable, and, e.g.
  # p$layers[[1]]$geom$default_aes
  ggplot2::update_geom_defaults("bar", ggplot2::aes(
    `colour` = NA,
    `fill` = "grey35",
    `linewidth` = 0.5,
    `linetype` = 1,
    `alpha` = NA,
  ))

  ggplot2::update_geom_defaults("boxplot", ggplot2::aes(
    `weight` = 1,
    `colour` = "grey20",
    `fill` = "white",
    `linewidth` = 0.5,
    `alpha` = NA,
    `shape` = 19,
    `linetype` = "solid"
  ))

  ggplot2::update_geom_defaults("hline", ggplot2::aes(
    `colour` = "black",
    `linewidth` = 0.5,
    `linetype` = 1,
    `alpha` = NA
  ))

  ggplot2::update_geom_defaults("line", ggplot2::aes(
    `colour` = "black",
    `linewidth` = 0.5,
    `linetype` = 1,
    `alpha` = NA
  ))

  ggplot2::update_geom_defaults("point", ggplot2::aes(
    `shape` = 19,
    `colour` = "black",
    `size` = 1.5,
    `fill` = NA,
    `alpha` = NA,
    `stroke` = 0.5,
  ))

  ggplot2::update_geom_defaults("segment", ggplot2::aes(
    `colour` = "black",
    `linewidth` = 0.5,
    `linetype` = 1,
    `alpha` = NA
  ))

  ggplot2::update_geom_defaults("smooth", ggplot2::aes(
    `colour` = "#3366FF",
    `fill` = "grey60",
    `linewidth` = 1,
    `linetype` = 1,
    `weight` = 1,
    `alpha` = 0.4
  ))

  ggplot2::update_geom_defaults("violin", ggplot2::aes(
    `weight` = 1,
    `colour` = "grey20",
    `fill` = "white",
    `linewidth` = 0.5,
    `alpha` = NA,
    `linetype` = "solid"
  ))

  ggplot2::update_geom_defaults("vline", ggplot2::aes(
    `colour` = "black",
    `linewidth` = 0.5,
    `linetype` = 1,
    `alpha` = NA
  ))

  ggplot2::theme_set(theme_grey())

  options(
    repr.plot.width = NULL,
    repr.plot.height = NULL,
    ggplot2.discrete.fill = NULL,
    ggplot2.discrete.colour = NULL,
    ggplot2.continuous.fill = NULL,
    ggplot2.continuous.colour = NULL
  )

  invisible()
}
