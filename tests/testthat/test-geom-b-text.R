axis_basis <- function(plot, aesthetic) {
  built <- ggplot2::ggplot_build(plot)
  vector <- b_panel_axis_vector(
    built$layout$panel_params[[1]], plot$coordinates, aesthetic
  )
  physical <- if (abs(vector[["x"]]) >= abs(vector[["y"]])) "x" else "y"
  c(physical = physical, direction = as.character(sign(vector[[physical]])))
}

test_that("coefficient text resolves the original axes after scales and coordinates", {
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_blank()
  cases <- list(
    ordinary = list(plot = base, x = c("x", "1"), y = c("y", "1")),
    x_reverse = list(
      plot = base + ggplot2::scale_x_reverse(),
      x = c("x", "-1"), y = c("y", "1")
    ),
    y_reverse = list(
      plot = base + ggplot2::scale_y_reverse(),
      x = c("x", "1"), y = c("y", "-1")
    ),
    both_reverse = list(
      plot = base + ggplot2::scale_x_reverse() + ggplot2::scale_y_reverse(),
      x = c("x", "-1"), y = c("y", "-1")
    ),
    flip = list(
      plot = base + ggplot2::coord_flip(),
      x = c("y", "1"), y = c("x", "1")
    ),
    flip_x_reverse = list(
      plot = base + ggplot2::scale_x_reverse() + ggplot2::coord_flip(),
      x = c("y", "-1"), y = c("x", "1")
    ),
    flip_y_reverse = list(
      plot = base + ggplot2::scale_y_reverse() + ggplot2::coord_flip(),
      x = c("y", "1"), y = c("x", "-1")
    ),
    coord_reverse = list(
      plot = base + ggplot2::coord_cartesian(reverse = "x"),
      x = c("x", "-1"), y = c("y", "1")
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    expect_equal(axis_basis(case$plot, "x"),
                 c(physical = case$x[[1]], direction = case$x[[2]]),
                 info = paste(name, "x"))
    expect_equal(axis_basis(case$plot, "y"),
                 c(physical = case$y[[1]], direction = case$y[[2]]),
                 info = paste(name, "y"))
  }
})

test_that("semantic text justification follows the trained screen axes", {
  expect_equal(
    b_physical_justification(-0.35, 1.2, c(x = 1, y = 0), c(x = 0, y = 1)),
    c(hjust = -0.35, vjust = 1.2)
  )
  expect_equal(
    b_physical_justification(-0.35, 1.2, c(x = -1, y = 0), c(x = 0, y = -1)),
    c(hjust = 1.35, vjust = -0.2)
  )
  expect_equal(
    b_physical_justification(-0.35, 1.2, c(x = 0, y = 1), c(x = 1, y = 0)),
    c(hjust = 1.2, vjust = -0.35)
  )
})

test_that("coefficient marks refuse non-Cartesian coordinates before or after gf_b", {
  model <- lm(Thumb ~ Height, data = Fingers)
  base <- gf_point(Thumb ~ Height, data = Fingers)
  polar_before <- base + ggplot2::coord_polar()
  polar_after <- base |>
    gf_b(model) +
    ggplot2::coord_polar()

  expect_error(
    gf_b(polar_before, model),
    "needs a plot with cartesian x and y axes",
    class = "coursekata_gf_b_coord"
  )
  expect_error(
    ggplot2::ggplotGrob(polar_after),
    "coefficient labels require Cartesian coordinates",
    class = "coursekata_gf_b_coord"
  )
})
