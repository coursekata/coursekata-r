## code to prepare `TipExperiment` dataset goes here

TipExperiment <- structure(
  list(
    TableID = 1:44,
    Tip = structure(
      c(39, 36, 34, 34, 33, 31, 31, 30, 30, 28, 28, 28, 27, 27, 25, 23, 22, 21, 21, 20, 18, 8, 72,
        65, 47, 44, 41, 40, 34, 33, 33, 30, 29, 28, 27, 27, 25, 24, 24, 23, 22, 21, 21, 17),
      format.spss = "F8.0"
    ),
    Condition = structure(
      c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
        2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
      levels = c("Control", "Smiley Face"),
      class = "factor")
    ),
  row.names = c(NA, -44L),
  class = "data.frame"
)

set.seed(22)
x <- round(rnorm(1000, mean=15, sd=10), digits=1)
y <- x[x > 5 & x < 30]
TipPct <- sample(y, 44)
TipExperiment$Check <- (TipExperiment$Tip / TipPct) * 100

usethis::use_data(TipExperiment, overwrite = TRUE)
