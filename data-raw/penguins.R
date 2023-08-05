library(readr)
library(dplyr)
library(usethis)

# Data sent by Ji
penguins_raw <-
  read_csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5jslWc4CXMRu60zBEBJ2ZNrC7iiBlM-uKUUFzBPQvLUGaCfX9JCUMPCgsffdozxe6qkh6S8S1b_v/pub?gid=2056245125&single=true&output=csv",
    show_col_types = FALSE,
    col_select = 2:8
  ) |>
  write_csv("data-raw/penguins_raw.csv")

# Modifications sent by Ji
penguins <- penguins_raw |>
  mutate(
    gentoo = factor(gentoo),
    species = factor(species),
    island = factor(island),
    female = factor(female),
    flipper_length_m = flipper_length_cm / 100,
    body_mass_kg = body_mass_g / 1000
  ) |>
  select(1, 2, 9, 8, 4, 7, 3)

use_data(penguins, overwrite = TRUE, compress = "xz")
