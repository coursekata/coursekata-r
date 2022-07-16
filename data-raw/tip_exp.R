library(purrr)
library(dplyr)
library(readr)
library(usethis)

set.seed(123)

params <- list(
  list(
    gender = "male",
    condition = "control",
    n = 21,
    mean = 21.41,
    sd = 12.64
  ),
  list(
    gender = "female",
    condition = "control",
    n = 23,
    mean = 27.78,
    sd = 7.77
  ),
  list(
    gender = "male",
    condition = "smiley face",
    n = 23,
    mean = 17.78,
    sd = 5.57
  ),
  list(
    gender = "female",
    condition = "smiley face",
    n = 22,
    mean = 33.04,
    sd = 14.02
  )
)

tip_exp <- purrr::map_df(params, function(x) {
  tibble(
    gender = x$gender,
    condition = x$condition,
    tip_percent = rnorm(x$n, x$mean, x$sd) %>% round(2),
  )
}) %>%
  mutate(
    gender = factor(gender),
    condition = factor(condition)
  )

tip_exp$tip_percent[[18]] <- 0

use_data(tip_exp, overwrite = TRUE, compress = "xz")
