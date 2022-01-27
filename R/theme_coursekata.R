#' A simple theme built on top of [`theme_bw()`]
#'
#' The `coursekata` package automatically loads this theme when the package is loaded. This is in
#' addition to a number of other plot tweaks and option settings. To just restore the theme to the
#' default, you can run `set_theme(theme_grey)`. If you want to restore all plot related settings
#' and/or prevent them when loading the package, see [`restore_default_themes`].
#'
#' @return A gg theme object
#' @export
#'
#' @examples
#' gf_boxplot(Thumb ~ RaceEthnic, data = Fingers, fill = ~ RaceEthnic)
theme_coursekata <- function() {
  ggplot2::theme_bw() + ggplot2::theme(
    # Fonts
    plot.title = ggplot2::element_text(face = "bold", size = 13),
    axis.title = ggplot2::element_text(size = 11),

    # Legend
    legend.background = ggplot2::element_rect(fill = "white", size = 4, color = "white"),
    legend.position = "right",

    # Grid
    axis.ticks = ggplot2::element_line(color = "grey70", size = 0.2),
    panel.grid.major = ggplot2::element_line(color = "grey70", size = 0.2),
    panel.grid.minor = ggplot2::element_blank(),

    # Facet labels
    strip.background = ggplot2::element_rect(fill = "#EEEEEE"),
    strip.text = ggplot2::element_text(size = 11)
  )
}


#' The color palettes used in our theme system
#'
#' There are four main color palettes used in our theme system: - **single-color**: for plots with
#' one color, the CourseKata blue from the website is used - **eight-color**: for plots with up to
#' eight colors, the colors from `ggtheme::scale_color_colorblind` are used, but the orders of some
#' of the colors are swapped - **fifteen-color**: when a broader palette of up to 15 colors is
#' needed, a different colorblind-friendly palette is used, from [this academic
#' poster](http://mkweb.bcgsc.ca/biovis2012/). - **continuous**: for plots with continuous
#' colorspaces, the [`viridisLite::viridis()`] palette is used
#'
#' @return A named list of the different color palettes used is returned with the exception of the
#'   continuous palette. The continuous palette can be accessed by calling `viridisLite::viridis(n)`
#'   where `n` is the number of colors to retrieve.
#' @export
coursekata_palettes <- function() {
  list(
    single = ggsci::pal_locuszoom()(2)[[2]],
    small = ggsci::pal_locuszoom()(7),
    large = list(
      black = "#000000",
      dark_green = "#004949",
      green = "#009292",
      pink = "#FF6DB6",
      light_pink = "#FFB6DB",
      indigo = "#490092",
      blue = "#006DDB",
      lavender = "#B66DFF",
      light_blue = "#6DB6FF",
      pale_blue = "#B6DBFF",
      red = "#920000",
      brown = "#924900",
      orange = "#DB6D00",
      light_green = "#24FF24",
      yellow = "#FFFF6D"
    )
  )
}

#' Create a function that provides a colorblind palette.
#'
#' @return A function that accepts one argument `n`, which is the number of colors you want to use
#'   in the plot. This function is used by scales like `scale_color_discrete` to provide colorblind-
#'   safe palettes. See [`scale_discrete_coursekata`] for more information.
#' @export
coursekata_palette_provider <- function() {
  unwrap <- function(x) x %>% unname() %>% unlist() %>% force()
  palettes <- coursekata_palettes()
  small_palette <- unwrap(palettes$small)
  large_palette <- unwrap(palettes$large)
  max_values <- length(large_palette)

  provider <- function(n) {
    if (n > max_values) {
      warning("This manual palette can handle a maximum of ",
              max_values, " values. You have supplied ", n, ".",
              call. = FALSE)
    }

    if (n > length(small_palette)) {
      large_palette[seq_len(n)]
    } else {
      small_palette[seq_len(n)]
    }
  }

  structure(provider, max_n = max_values)
}


#' A discrete color scale constructor with colorblind-safe palettes.
#'
#' See [`coursekata_palettes()`] for more information.
#'
#' @param ... Additional parameters passed on to the scale type.
#'
#' @return A discrete color scale.
#' @seealso coursekata_palettes
#' @export
scale_discrete_coursekata <- function(...) {
  ggplot2::discrete_scale(
    c("color", "fill"),
    "coursekata",
    coursekata_palette_provider(),
    ...
  )
}


#' Utility function for loading all themes.
#'
#' This function is called at package start-up and should rarely be needed by the user. The
#' exception is when the user has called [`restore_default_themes()`] and wants to go back to the
#' CourseKata look and feel. When run, this function sets the CourseKata color palettes
#' [`coursekata_palettes`], sets the default theme to [`theme_coursekata`], and tweaks some default
#' settings for specific plots. To restore the original `ggplot2` settings, run
#' [`restore_default_themes()`].
#'
#' @seealso coursekata_palettes theme_coursekata scale_discrete_coursekata restore_default_themes
#' @export
load_coursekata_themes <- function() {
  ggplot2::update_geom_defaults("bar", list(
    color = "black",
    fill = coursekata_palettes()$single,
    alpha = .8,
    size = .1
  ))

  ggplot2::update_geom_defaults("barh", list(
    color = "black",
    fill = coursekata_palettes()$single,
    alpha = .8,
    size = .1
  ))

  ggplot2::update_geom_defaults("boxplot", list(
    color = "black",
    fill = coursekata_palettes()$single,
    alpha = .6
  ))

  ggplot2::update_geom_defaults("boxploth", list(
    color = "black",
    fill = coursekata_palettes()$single,
    alpha = .6
  ))

  ggplot2::update_geom_defaults("lm", list(
    color = coursekata_palettes()$small[[5]],
    size = .8
  ))

  ggplot2::update_geom_defaults("hline", list(
    color = coursekata_palettes()$small[[5]],
    size = .8
  ))

  ggplot2::update_geom_defaults("vline", list(
    color = coursekata_palettes()$small[[5]],
    size = .8
  ))

  ggplot2::update_geom_defaults("segment", list(
    color = coursekata_palettes()$small[[5]],
    size = .8
  ))

  ggplot2::theme_set(theme_coursekata())

  options(
    repr.plot.width = 6,
    repr.plot.height = 4,
    ggplot2.discrete.fill = scale_discrete_coursekata,
    ggplot2.discrete.color = scale_discrete_coursekata,
    ggplot2.continuous.fill = 'viridis',
    ggplot2.continuous.color = 'viridis'
  )
}


#' Restore `ggplot2` default settings
#'
#' This function will restore all of the tweaks to themes and plotting to the original `ggplot2`
#' defaults. If you want to go back to the CourseKata look and feel, run
#' [`load_coursekata_themes()`].
#'
#' @seealso load_coursekata_themes
#' @export
restore_default_themes <- function() {
  # find these values by creating a plot, storing it to a variable, and, e.g.
  # p$layers[[1]]$geom$default_aes
  ggplot2::update_geom_defaults("bar", list(
    color = NA,
    fill = 'grey35',
    size = .5
  ))

  ggplot2::update_geom_defaults("barh", list(
    color = NA,
    fill = 'grey35',
    size = .5
  ))

  ggplot2::update_geom_defaults("boxplot", list(
    color = "grey20",
    fill = "white",
    alpha = NA
  ))

  ggplot2::update_geom_defaults("boxploth", list(
    color = "grey20",
    fill = "white",
    alpha = NA
  ))

  ggplot2::update_geom_defaults("lm", list(
    color = "#3366FF"
  ))

  ggplot2::update_geom_defaults("hline", list(
    color = "black"
  ))

  ggplot2::update_geom_defaults("vline", list(
    color = "black"
  ))

  ggplot2::update_geom_defaults("segment", list(
    color = "black"
  ))

  ggplot2::theme_set(theme_grey())

  options(
    repr.plot.width = NULL,
    repr.plot.height = NULL,
    ggplot2.discrete.fill = NULL,
    ggplot2.discrete.color = NULL,
    ggplot2.continuous.fill = NULL,
    ggplot2.continuous.color = NULL
  )
}
