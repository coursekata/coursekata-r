library(readr)
library(dplyr)
library(stringr)
library(usethis)


# This is the original source cited in De Cock (2011), but the link does not work
# Data source: http://www.amstat.org/v19n3/decock/AmesHousing.xls

# The data is also hosted on Kaggle:
# https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/data

# All data from Kaggle
ames_raw <- bind_rows(
  read_csv("data-raw/ames_kaggle_test.csv", show_col_types = FALSE),
  read_csv("data-raw/ames_kaggle_train.csv", show_col_types = FALSE)
) |>
  write_csv("data-raw/ames_raw.csv")

# Kaggle short descriptions of full data set
# SalePrice - the property's sale price in dollars
# MSSubClass: The building class
# MSZoning: The general zoning classification
# LotFrontage: Linear feet of street connected to property
# LotArea: Lot size in square feet
# Street: Type of road access
# Alley: Type of alley access
# LotShape: General shape of property
# LandContour: Flatness of the property
# Utilities: Type of utilities available
# LotConfig: Lot configuration
# LandSlope: Slope of property
# Neighborhood: Physical locations within Ames city limits
# Condition1: Proximity to main road or railroad
# Condition2: Proximity to main road or railroad (if a second is present)
# BldgType: Type of dwelling
# HouseStyle: Style of dwelling
# OverallQual: Overall material and finish quality
# OverallCond: Overall condition rating
# YearBuilt: Original construction date
# YearRemodAdd: Remodel date
# RoofStyle: Type of roof
# RoofMatl: Roof material
# Exterior1st: Exterior covering on house
# Exterior2nd: Exterior covering on house (if more than one material)
# MasVnrType: Masonry veneer type
# MasVnrArea: Masonry veneer area in square feet
# ExterQual: Exterior material quality
# ExterCond: Present condition of the material on the exterior
# Foundation: Type of foundation
# BsmtQual: Height of the basement
# BsmtCond: General condition of the basement
# BsmtExposure: Walkout or garden level basement walls
# BsmtFinType1: Quality of basement finished area
# BsmtFinSF1: Type 1 finished square feet
# BsmtFinType2: Quality of second finished area (if present)
# BsmtFinSF2: Type 2 finished square feet
# BsmtUnfSF: Unfinished square feet of basement area
# TotalBsmtSF: Total square feet of basement area
# Heating: Type of heating
# HeatingQC: Heating quality and condition
# CentralAir: Central air conditioning
# Electrical: Electrical system
# 1stFlrSF: First Floor square feet
# 2ndFlrSF: Second floor square feet
# LowQualFinSF: Low quality finished square feet (all floors)
# GrLivArea: Above grade (ground) living area square feet
# BsmtFullBath: Basement full bathrooms
# BsmtHalfBath: Basement half bathrooms
# FullBath: Full bathrooms above grade
# HalfBath: Half baths above grade
# Bedroom: Number of bedrooms above basement level
# Kitchen: Number of kitchens
# KitchenQual: Kitchen quality
# TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
# Functional: Home functionality rating
# Fireplaces: Number of fireplaces
# FireplaceQu: Fireplace quality
# GarageType: Garage location
# GarageYrBlt: Year garage was built
# GarageFinish: Interior finish of the garage
# GarageCars: Size of garage in car capacity
# GarageArea: Size of garage in square feet
# GarageQual: Garage quality
# GarageCond: Garage condition
# PavedDrive: Paved driveway
# WoodDeckSF: Wood deck area in square feet
# OpenPorchSF: Open porch area in square feet
# EnclosedPorch: Enclosed porch area in square feet
# 3SsnPorch: Three season porch area in square feet
# ScreenPorch: Screen porch area in square feet
# PoolArea: Pool area in square feet
# PoolQC: Pool quality
# Fence: Fence quality
# MiscFeature: Miscellaneous feature not covered in other categories
# MiscVal: $Value of miscellaneous feature
# MoSold: Month Sold
# YrSold: Year Sold
# SaleType: Type of sale
# SaleCondition: Condition of sale

# According to the data description, this is how the data were filtered
# However, doing so does not yield the same number of rows
# Keeping the code here for general idea, but committing actual file used
# ames_raw %>%
#   filter(
#     Neighborhood %in% c("CollegeCreek", "OldTown"),
#     # kitchen average (TA) or better
#     KitchenQual %in% c("Ex", "Gd", "TA"),
#     # only single family homes
#     BldgType == "1Fam",
#     # residental zoning
#     str_detect(MSZoning, "^R|FV"),
#     # homes with brick, cinderblock, or concrete foundations
#     Foundation %in% c("PConc", "CBlock", "BrkTil"),
#     # 1-2 story
#     HouseStyle %in% c("1Story", "2Story", "1.5Fin", "1.5Unf")
#   )

Ames <- read_csv("data-raw/ames_full.csv") |>
  select(-1) |>
  mutate(Neighborhood = factor(Neighborhood))

use_data(Ames, overwrite = TRUE, compress = "xz")
