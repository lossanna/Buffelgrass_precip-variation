# Created: 2024-09-23
# Updated: 2024-09-23

# Purpose: Run generalized linear models with Reproductive_culms as response variable.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")

# Data wrangling ----------------------------------------------------------

# Examine character cols
str(dat)


# GLM ---------------------------------------------------------------------

pos.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + (1 | Site / Plot),
                     data = dat,
                     family = genpois)
summary(pos.repro)
