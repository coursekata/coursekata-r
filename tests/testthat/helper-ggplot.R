# R 4.1 cannot dispatch `plot + data` past `Ops.data.frame`. Calling ggplot2's
# public double-dispatch generic exercises the same data replacement at the R
# floor and on current R.
replace_plot_data <- function(plot, data) {
  ggplot2::update_ggplot(data, plot)
}
