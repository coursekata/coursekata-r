library(readr)
library(dplyr)
library(usethis)

# Citation:
# Kahn,M. (2003), "Data Sleuth," STATS, 37, 24.

# Data site:
# http://jse.amstat.org/datasets/fev.dat.txt

# Information:
# http://jse.amstat.org/datasets/fev.txt

raw_path <- "data-raw/fevdata.dat"
if (!file.exists(raw_path)) {
  download.file(
    "http://jse.amstat.org/datasets/fev.dat.txt",
    raw_path
  )
}

fevdata <- raw_path |>
  read_fwf(
    fwf_empty(raw_path, col_names = c("AGE", "FEV", "HEIGHT", "SEX", "SMOKE")),
    col_types = "iddii"
  ) |>
  mutate(
    SEX = factor(SEX, 0:1, c("Female", "Male")),
    SMOKE = factor(SMOKE, 0:1, c("Non-smoker", "Smoker"))
  )

write_csv(fevdata, "data-raw/fevdata.csv")
use_data(fevdata, overwrite = TRUE, compress = "xz")
