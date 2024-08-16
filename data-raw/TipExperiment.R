## code to prepare `TipExperiment` dataset goes here

TipExperiment <- structure(
  list(
    TableID = 1:44,
    Tip = structure(
      c(
        39, 36, 34, 34, 33, 31, 31, 30, 30, 28, 28, 28, 27, 27, 25, 23, 22, 21, 21, 20, 18, 8, 72,
        65, 47, 44, 41, 40, 34, 33, 33, 30, 29, 28, 27, 27, 25, 24, 24, 23, 22, 21, 21, 17
      ),
      format.spss = "F8.0"
    ),
    Condition = structure(
      c(
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
        2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L
      ),
      levels = c("Control", "Smiley Face"),
      class = "factor"
    )
  ),
  row.names = c(NA, -44L),
  class = "data.frame"
)


# Add Check ###################################

set.seed(22)
x <- round(rnorm(1000, mean = 15, sd = 10), digits = 1)
y <- x[x > 5 & x < 30]
TipPct <- sample(y, 44)
TipExperiment$Check <- round((TipExperiment$Tip / TipPct) * 100, 2)


# Add FoodQuality #############################

set.seed(10)

# Generate a correlated variable food_quality_raw based on existing Tip values
n <- nrow(TipExperiment)
desired_correlation <- 0.6
mean_tip <- mean(TipExperiment$Tip)
sd_tip <- sd(TipExperiment$Tip)

# Set up the covariance matrix based on desired correlation
cov_matrix <- matrix(
  c(
    sd_tip^2, desired_correlation * sd_tip * 10,
    desired_correlation * sd_tip * 10, 100
  ),
  nrow = 2
)
colnames(cov_matrix) <- rownames(cov_matrix) <- c("Tip", "food_quality_raw")

# Generate data including a simulated Tip to match the structure, then replace with actual Tip
sim_data <- MASS::mvrnorm(n, mu = c(mean_tip, 50), Sigma = cov_matrix)
sim_tip <- sim_data[, "Tip"]
food_quality_raw <- sim_data[, "food_quality_raw"]

# Adjust food_quality_raw to simulate the correlation structure
# Scale to 0-100 using a logistic transformation, simplified here as clipping
food_quality <- pmin(100, pmax(0, food_quality_raw))

# Add food_quality to your existing dataframe
TipExperiment$FoodQuality <- round(food_quality, 1)


# Save the dataset ############################

usethis::use_data(TipExperiment, overwrite = TRUE)
