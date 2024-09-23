# Created: 2024-09-23
# Updated: 2024-09-23

# Purpose: Examine distributions.

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

culms.raw <- read_xlsx("data/raw/2024-09_LO_Buffelgrass-culm-demography_master.xlsx", sheet = "Demography")
monitor.prism <- read_csv("data/cleaned/02.1_monitoring-info-with-PRISM-data_clean.csv")

# Data wrangling ----------------------------------------------------------

dat <- culms.raw |> 
  left_join(monitor.prism)

# Check for NAs
apply(dat, 2, anyNA)

# Separate NA culm counts
na.culm <- dat |> 
  filter(is.na(Vegetative_culms))

# Remove NA culm count - plants could not be located and weren't sampled
dat <- dat |> 
  filter(!is.na(Vegetative_culms))

# Add Perc_dev, Perc_dev_abs, and Deviation_mm cols
dat <- dat |> 
  mutate(Perc_dev = (Prev_year_precip - MAP) / MAP,
         Deviation_mm = Prev_year_precip - MAP) |> 
  mutate(Perc_dev_abs = abs(Perc_dev))

# Separate out continuous explanatory variables
exp.cont <- dat |> 
  select(PlotSlope, Elevation_ft, Prev_year_precip, MAT, MAP, Perc_dev, Deviation_mm)

# Separate out response variables
res.cont <- dat |> 
  select(BGCover, BGDensity, ShrubCover, ForbCover, NGCover, Vegetative_culms, Reproductive_culms, Total_Live_Culms,
         Longestleaflength_cm)


# Response variable: Culm count -------------------------------------------

## Histogram --------------------------------------------------------------

# All, Reproductive
hist(dat$Reproductive_culms)

# By site, Reproductive
hist(filter(dat, Site == "TumamocHill")$Reproductive_culms)
hist(filter(dat, Site == "LomaVerde")$Reproductive_culms)
hist(filter(dat, Site == "KinneyHill")$Reproductive_culms)
hist(filter(dat, Site == "ApachePeak")$Reproductive_culms)

# All, Total
hist(dat$Total_Live_Culms)

# All, Vegetative
hist(dat$Vegetative_culms)


## Boxplot ----------------------------------------------------------------

# All, by site
#   Reproductive
dat |> 
  ggplot(aes(x = Site, y = Reproductive_culms)) +
  geom_boxplot()

#   Total
dat |> 
  ggplot(aes(x = Site, y = Total_Live_Culms)) +
  geom_boxplot()

#   Vegetative
dat |> 
  ggplot(aes(x = Site, y = Vegetative_culms)) +
  geom_boxplot()


# By aspect
dat |> 
  ggplot(aes(x = Aspect, y = Reproductive_culms)) +
  geom_boxplot()
