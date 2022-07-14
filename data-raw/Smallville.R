library(simstudy)
library(dplyr)

def <- defData(varname = "seed", dist = "normal", formula = 1.5536, variance = .17) |>
  defData(varname = "Neighborhood", dist = "binary", formula = .5) |>
  defData(varname = "HomeSizeK", formula = "seed + Neighborhood*-.3", variance = .4) |>
  defData(varname = "PriceK", formula = "69 - 57.77*Neighborhood + 87*HomeSizeK", variance = 5000)

set.seed(14)

Smallville <- genData(32, def) |>
  mutate(
    Neighborhood = factor(Neighborhood, levels = 0:1, labels = c("Downtown", "Eastside")),
    HomeSizeK = HomeSizeK + .25,
    HasFireplace = factor(ifelse(HomeSizeK > 2.15, 1, 0)),
    PriceK = 135 + .77 * PriceK
  ) |>
  select(PriceK, Neighborhood, HomeSizeK, HasFireplace)

usethis::use_data(Smallville, overwrite = TRUE)
