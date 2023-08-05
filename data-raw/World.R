library(readr)
library(dplyr)
library(usethis)

# Data sent by Ji
World_raw <-
  read_csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vQhIVfGa0h3qqdi-RlEBtpLhREoURgo0tK_efokXYEJJHe9KjLl-R2oXYKTbjXrWd2exwv1NWHPD2Uk/pub?gid=1108300079&single=true&output=csv",
    show_col_types = FALSE,
  ) |>
  write_csv("data-raw/World_raw.csv")

World <- World_raw |>
  select(
    Country, Region, Code, LifeExpectancy, GirlsH1900, GirlsH1980, Happiness, GDPperCapita,
    FertRate, PeopleVacc, PeopleVacc_per100, Population2010, Population2020, WineServ
  ) |>
  na.omit()

use_data(World, overwrite = TRUE, compress = "xz")
