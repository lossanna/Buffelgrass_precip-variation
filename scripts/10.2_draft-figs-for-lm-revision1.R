# Created: 2026-01-28
# Updated: 2026-01-29

# Purpose: Graph data to accompany 09.2.R of revision1 linear models (uses year-to-year-change
#   as response variable).


# https://easystats.github.io/modelbased/articles/visualisation_matrix.html#visualising-an-interaction-between-two-numeric-variables-three-way-interaction

# Datagrids are needed because otherwise would have to generate prediction for each observation (1248 for culm).
#   Two datagrids are needed so there can be multiple x & y values under the same column name; same col
#   name is needed to be able to graph prediction on the same axes as original data.

# To graph back-transformed/unscaled continuous explanatory variables with model prediction:
#   1. Make datagrid of with prediction and CI with scaled variable using get_predicted() and get_predicted_CI().
#        Name the prediction column the same as the y-axis of the graph (response variable).
#   2. Make datagrid of unscaled variable that is the same length as the datagrid with predictions & gg.
#        Arrange this datagrid with unscaled variable by that variable so the order will be the same as first datagrid. 
#   3. Add the unscaled variable column to datagrid with predictions & gg.

# Note: For some reason the modelbased version of the Aspect plots don't work anymore (I think it
#   has something to do with the nested random effect), so I am just going with the ggeffects version.

library(tidyverse)
library(insight)
library(modelbased)
library(ggeffects)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/09.2_data-and-version2-models-revision1.RData")
dat.survival.raw <- dat.survival


# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm change - total & repro
dat.culm <- culm.change.flat.rm %>% 
  select(Change_Total_Live_Culms, Change_Reproductive_culms, 
         Prev_year_precip, Aspect, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Prev_year_precip_scaled, PlotSlope_scaled, Change_BGDensity_scaled,
         Change_ShrubCover_scaled, Change_HerbCover_scaled)

#   Plot change - density & cover
dat.plot <- plot.change %>% 
  select(Change_BGDensity, Change_BGCover, 
         Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover)

#   Survival  
dat.survival <- dat.survival.raw %>% 
  select(survival_transf, 
         Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover,
         Prev_year_precip_scaled, PlotSlope_scaled, BGDensity_scaled,
         ShrubCover_scaled, HerbCover_scaled)


# 2. Dataset for constructing datagrid with prediction & CI (scaled explanatory variables only)
#   Culm change - total & repro 
dat.culm.ex <- culm.change.flat.rm %>% 
  select(Prev_year_precip_scaled, Aspect, PlotSlope_scaled, 
         Change_BGDensity_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled)

#   Plot change - density & cover
dat.plot.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled)

#   Survival  
dat.survival.ex <- dat.survival.raw %>% 
  select(Prev_year_precip_scaled,
         PlotSlope_scaled, ShrubCover_scaled, HerbCover_scaled,
         BGDensity_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm change - total & repro
dat.culm.unscaled <- culm.change.flat.rm %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover)

#   Plot change - density & cover
dat.plot.unscaled <- plot.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover)

#   Survival 
dat.survival.unscaled <- dat.survival.raw %>% 
  select(Prev_year_precip, PlotSlope, BGDensity, ShrubCover, HerbCover)



# Test different precip levels --------------------------------------------

# Am trying to generate a datagrid with a precip level that is closest to 0 (the mean),
#   so I can then have the highest, mean, and lowest graphed. Change_HerbCover is just used as
#   an example here, but the numbers will be the same for any two continuous variables.
# I can then just remove the other unwanted levels to still just have three lines on the graph.
# (There is probably a smarter way to go about this than checking every level, but that's what
#   I'm going to do lol.)

# Precip range for culm
range(dat.culm.ex$Prev_year_precip_scaled) # -0.952, 1.641

# Precip range for plot
range(dat.plot.ex$Prev_year_precip_scaled) # -0.957, 1.609


## Culm models ------------------------------------------------------------

# Results: I checked 3-30 precip levels, and the closest that came to 0 was 0.003,
#   which was generated at 20 precip levels.

# 3 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.345

# 4 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 4, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.088

# 5 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.304

# 6 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 6, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.085

# 7 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 7, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.088

# 8 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 8, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.159

# 9 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.02

# 10 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.088

# 11 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 11, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.085

# 12 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.009

# 13 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 13, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.088

# 14 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 14, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.045

# 15 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 15, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.026

# 16 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 16, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.085

# 17 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 17, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.02

# 18 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 18, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.037

# 19 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 19, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.056

# 20 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.003 (8th)

# 21 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 21, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) #-0.044

# 22 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 22, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.036

# 23 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 23, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.009

# 24 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 24, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.05

# 25 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 25, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.02

# 26 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 26, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled)# -0.019

# 27 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 27, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.045

# 28 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 28, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.008

# 29 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 29, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.026

# 30 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 30, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.032


## Density & cover models -------------------------------------------------

# Results: I checked 3-30 precip levels, and the closest that came to 0 was 0.005,
#   which was generated first at 9 precip levels.

# 3 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.326

# 4 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 4, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.102

# 5 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.316

# 6 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 6, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.069

# 7 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 7, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.102

# 8 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 8, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.143

# 9 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.005 (4th)

# 10 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.102

# 11 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 11, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.069

# 12 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.024

# 13 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 13, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.102

# 14 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 14, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.03

# 15 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 15, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.041

# 16 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 16, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.069

# 17 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 17, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.005

# 18 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 18, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.051

# 19 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 19, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.041

# 20 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.012

# 21 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 21, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) #-0.059

# 22 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 22, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.021

# 23 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 23, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.024

# 24 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 24, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.047

# 25 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 25, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.005

# 26 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 26, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled)# -0.033

# 27 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 27, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.03

# 28 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 28, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.007

# 29 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 29, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.041

# 30 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 30, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.016



# Total culm change -------------------------------------------------------

## Total: Precip ----------------------------------------------------------

# Construct datagrid with scaled variable 
insight.total.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
# Add prediction column (must be must be named the same as y-axis in graph)
insight.total.precip$Change_Total_Live_Culms <- get_predicted(total2, insight.total.precip) 
# Add SE and CI columns
insight.total.precip$SE <- get_predicted_ci(total2, data = insight.total.precip)$SE
insight.total.precip$CI <- insight.total.precip$SE * 1.96
# Construct datagrid of same length with unscaled variable
unscaled.precip <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
# Add unscaled variable to datagrid with prediction & CI
insight.total.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

# Graph
total.precip <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.precip,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.precip,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI), 
              alpha = 0.2) +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in total culm count vs. precip") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.precip


## Total: Aspect ----------------------------------------------------------

# Generate prediction & CI
mb.total.aspect <- estimate_means(total2, terms = "Aspect") %>% 
  rename(Change_Total_Live_Culms = Mean) # idk why this doesn't work

# ggeffects version
#   Generate prediction & CI
gg.total.aspect <- predict_response(total2, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_Total_Live_Culms = predicted)

# Graph (ggeffects version)
total.aspect <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_Total_Live_Culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_pointrange(data = gg.total.aspect,
                  aes(ymin = conf.low, ymax = conf.high),
                  color = "purple3",
                  linewidth = 1.3) +
  geom_point(data = gg.total.aspect,
             aes(x = Aspect, y = Change_Total_Live_Culms),
             color = "purple3",
             size = 3,
             shape = 15) +
  theme_bw() +
  labs(title = "Change in total culm count by aspect",
       y = expression(Delta ~ "Total culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
total.aspect


## Total: BG density ------------------------------------------------------

# Generate prediction and add unscaled variable
insight.total.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                    length = 100)
insight.total.bgden$Change_Total_Live_Culms <- get_predicted(total2, insight.total.bgden)
insight.total.bgden$SE <- get_predicted_ci(total2, data = insight.total.bgden)$SE
insight.total.bgden$CI <- insight.total.bgden$SE * 1.96
unscaled.bgden <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                  length = 100) %>% 
  arrange(Change_BGDensity)
insight.total.bgden$Change_BGDensity <- unscaled.bgden$Change_BGDensity

# Graph
total.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.bgden,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.bgden,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. plot density change") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.bgden


## Total: Shrub -----------------------------------------------------------

# Generate prediction with unscaled variable added
insight.total.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                    length = 100)
insight.total.shrub$Change_Total_Live_Culms <- get_predicted(total2, insight.total.shrub)
insight.total.shrub$SE <- get_predicted_ci(total2, data = insight.total.shrub)$SE
insight.total.shrub$CI <- insight.total.shrub$SE * 1.96
unscaled.shrub <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
insight.total.shrub$Change_ShrubCover <- unscaled.shrub$Change_ShrubCover

# Graph
total.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.shrub,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.shrub,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. shrub cover change") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.shrub


## Total: Precip * shrub --------------------------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get mean
insight.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.total.shrub.precip$Predicted <- get_predicted(total2, insight.total.shrub.precip)
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.total.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.total.shrub.precip$Prev_year_precip_scaled)
insight.total.shrub.precip <- insight.total.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph
total.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. shrub cover change")
total.shrub.precip


## Total: Precip * herb ---------------------------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get mean
insight.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.total.herb.precip$Predicted <- get_predicted(total2, insight.total.herb.precip)
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.total.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.total.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.total.herb.precip$Prev_year_precip_scaled)
insight.total.herb.precip <- insight.total.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph
total.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. herb cover change")
total.herb.precip


## Total: Slope (NS) ------------------------------------------------------

# Generate prediction and add unscaled variable
insight.total.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.total.slope$Change_Total_Live_Culms <- get_predicted(total2, insight.total.slope)
insight.total.slope$SE <- get_predicted_ci(total2, data = insight.total.slope)$SE
insight.total.slope$CI <- insight.total.slope$SE * 1.96
unscaled.slope <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.total.slope$PlotSlope <- unscaled.slope$PlotSlope

#   Graph
total.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.slope,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.slope,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count vs. slope") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.slope


## Total: Herb (NS) -------------------------------------------------------

# Generate prediction and add unscaled variable
insight.total.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                                   length = 100)
insight.total.herb$Change_Total_Live_Culms <- get_predicted(total2, insight.total.herb)
insight.total.herb$SE <- get_predicted_ci(total2, data = insight.total.herb)$SE
insight.total.herb$CI = insight.total.herb$SE * 1.96
unscaled.herb <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
insight.total.herb$Change_HerbCover <- unscaled.herb$Change_HerbCover

# Graph
total.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.herb,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.herb,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. herb cover change") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.herb


## Total: Precip * density (NS) -------------------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get mean
insight.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
insight.total.bgden.precip$Predicted <- get_predicted(total2, insight.total.bgden.precip)
unscaled.bgden.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity)
insight.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip$Change_BGDensity
insight.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip$Prev_year_precip
unique(insight.total.bgden.precip$Prev_year_precip_scaled)
insight.total.bgden.precip <- insight.total.bgden.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph
total.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.bgden.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in total culm count vs. plot density change")
total.bgden.precip



# Reproductive culm change ------------------------------------------------

## Repro: Aspect ----------------------------------------------------------

# Generate prediction & CI
gg.repro.aspect <- predict_response(repro2, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_Reproductive_culms = predicted)

# Graph
repro.aspect <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_Reproductive_culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_pointrange(data = gg.repro.aspect,
                  aes(ymin = conf.low, ymax = conf.high),
                  color = "purple3",
                  linewidth = 1.3) +
  geom_point(data = gg.repro.aspect,
             aes(x = Aspect, y = Change_Reproductive_culms),
             color = "purple3",
             size = 3,
             shape = 15) +
  theme_bw() +
  labs(title = "Change in repro culm count by aspect",
       y = expression(Delta ~ "Reproductive culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
repro.aspect


## Repro: BG density -------------------------------------------------------

# Generate prediction and add unscaled variable
insight.repro.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                    length = 100)
insight.repro.bgden$Change_Reproductive_culms <- get_predicted(repro2, insight.repro.bgden)
insight.repro.bgden$SE <- get_predicted_ci(repro2, data = insight.repro.bgden)$SE
insight.repro.bgden$CI <- insight.repro.bgden$SE * 1.96
unscaled.bgden <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                  length = 100) %>% 
  arrange(Change_BGDensity)
insight.repro.bgden$Change_BGDensity <- unscaled.bgden$Change_BGDensity

# Graph
repro.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.bgden,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.bgden,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. plot density change") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.bgden


## Repro: Shrub -----------------------------------------------------------

# Generate prediction with unscaled variable added
insight.repro.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                    length = 100)
insight.repro.shrub$Change_Reproductive_culms <- get_predicted(repro2, insight.repro.shrub)
insight.repro.shrub$SE <- get_predicted_ci(repro2, data = insight.repro.shrub)$SE
insight.repro.shrub$CI <- insight.repro.shrub$SE * 1.96
unscaled.shrub <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
insight.repro.shrub$Change_ShrubCover <- unscaled.shrub$Change_ShrubCover

# Graph
repro.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.shrub,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.shrub,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. shrub cover change") +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.shrub


## Repro: Herb ------------------------------------------------------------

# Generate prediction and add unscaled variable
insight.repro.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                                   length = 100)
insight.repro.herb$Change_Reproductive_culms <- get_predicted(repro2, insight.repro.herb)
insight.repro.herb$SE <- get_predicted_ci(repro2, data = insight.repro.herb)$SE
insight.repro.herb$CI <- insight.repro.herb$SE * 1.96
unscaled.herb <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
insight.repro.herb$Change_HerbCover <- unscaled.herb$Change_HerbCover

# Graph
repro.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.herb,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.herb,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. herb cover change") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.herb


## Repro: Precip (NS) -----------------------------------------------------

# Generate prediction and add unscaled variable
insight.repro.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.repro.precip$Change_Reproductive_culms <- get_predicted(repro2, insight.repro.precip)
insight.repro.precip$SE <- get_predicted_ci(repro2, data = insight.repro.precip)$SE
insight.repro.precip$CI <- insight.repro.precip$SE * 1.96
unscaled.precip <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
insight.repro.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

#   Graph
repro.precip <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.precip,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.precip,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in repro culm count vs. precip") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.precip


## Repro: Slope (NS) -----------------------------------------------------

# Generate prediction and add unscaled variable
insight.repro.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.repro.slope$Change_Reproductive_culms <- get_predicted(repro2, insight.repro.slope)
insight.repro.slope$SE <- get_predicted_ci(repro2, data = insight.repro.slope)$SE
insight.repro.slope$CI <- insight.repro.slope$SE * 1.96
unscaled.slope <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.repro.slope$PlotSlope <- unscaled.slope$PlotSlope

# Graph
repro.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.slope,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.slope,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in repro culm count vs. slope") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.slope


## Repro: Precip * density (NS) -------------------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get mean
insight.repro.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
insight.repro.bgden.precip$Predicted <- get_predicted(repro2, insight.repro.bgden.precip)
unscaled.bgden.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity)
insight.repro.bgden.precip$Change_BGDensity <- unscaled.bgden.precip$Change_BGDensity
insight.repro.bgden.precip$Prev_year_precip <- unscaled.bgden.precip$Prev_year_precip
unique(insight.repro.bgden.precip$Prev_year_precip_scaled)
insight.repro.bgden.precip <- insight.repro.bgden.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph
repro.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.repro.bgden.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in repro culm count vs. plot density change")
repro.bgden.precip


## Repro: Precip * shrub (NS) ---------------------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get mean
insight.repro.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.repro.shrub.precip$Predicted <- get_predicted(repro2, insight.repro.shrub.precip)
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.repro.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.repro.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.repro.shrub.precip$Prev_year_precip_scaled)
insight.repro.shrub.precip <- insight.repro.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph
repro.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.repro.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in repro culm count vs. shrub cover change")
repro.shrub.precip


## Repro: Precip * herb (NS) ----------------------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get mean
insight.repro.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.repro.herb.precip$Predicted <- get_predicted(repro2, insight.repro.herb.precip)
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.repro.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.repro.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.repro.herb.precip$Prev_year_precip_scaled)
insight.repro.herb.precip <- insight.repro.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph
repro.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.repro.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in repro culm count vs. herb cover change")
repro.herb.precip



# Buffelgrass density change ----------------------------------------------

## BG density: Precip -----------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgden.precip$Change_BGDensity <- get_predicted(bgden2, insight.bgden.precip)
insight.bgden.precip$SE <- get_predicted_ci(bgden2, data = insight.bgden.precip)$SE
insight.bgden.precip$CI <- insight.bgden.precip$SE * 1.96
unscaled.precip <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgden.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

# Graph 
bgden.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = insight.bgden.precip,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.precip,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip")
bgden.precip


## BG density: Aspect -----------------------------------------------------

# Generate prediction & CI
gg.bgden.aspect <- predict_response(bgden2, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_BGDensity = predicted)

# Graph 
bgden.aspect <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGDensity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_pointrange(data = gg.bgden.aspect,
                  aes(ymin = conf.low, ymax = conf.high),
                  color = "purple3",
                  linewidth = 1.3) +
  geom_point(data = gg.bgden.aspect,
             aes(x = Aspect, y = Change_BGDensity),
             color = "purple3",
             size = 3,
             shape = 15) +
  theme_bw() +
  labs(title = "Change in plot density by aspect",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgden.aspect


## BG density: Precip * shrub ---------------------------------------------

# Generate prediction and add unscaled variable - 9 levels to get mean
insight.bgden.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.bgden.shrub.precip$Predicted <- get_predicted(bgden2, insight.bgden.shrub.precip)
unscaled.shrub.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.bgden.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.bgden.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.bgden.shrub.precip$Prev_year_precip_scaled) 
insight.bgden.shrub.precip <- insight.bgden.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.957, 0.005, 1.609))

# Graph
bgden.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgden.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. shrub cover change")
bgden.shrub.precip


## BG density: Slope (NS) -------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.slope <- get_datagrid(dat.plot.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.bgden.slope$Change_BGDensity <- get_predicted(bgden2, insight.bgden.slope)
insight.bgden.slope$SE <- get_predicted_ci(bgden2, data = insight.bgden.slope)$SE
insight.bgden.slope$CI <- insight.bgden.slope$SE * 1.96
unscaled.slope <- get_datagrid(dat.plot.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.bgden.slope$PlotSlope <- unscaled.slope$PlotSlope

# Graph
bgden.slope <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = insight.bgden.slope,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.slope,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in buffelgrass density vs. slope") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgden.slope


## BG density: Shrub (NS) -------------------------------------------------

# Generate prediction and add unscaled variable
insight.bgden.shrub <- get_datagrid(dat.plot.ex, by = c("Change_ShrubCover_scaled"),
                                    length = 100)
insight.bgden.shrub$Change_BGDensity <- get_predicted(bgden2, insight.bgden.shrub)
insight.bgden.shrub$SE <- get_predicted_ci(bgden2, data = insight.bgden.shrub)$SE
insight.bgden.shrub$CI <- insight.bgden.shrub$SE * 1.96
unscaled.shrub <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
insight.bgden.shrub$Change_ShrubCover <- unscaled.shrub$Change_ShrubCover

# Graph
bgden.shrub <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = insight.bgden.shrub,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.shrub,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. shrub cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.shrub


## BG density: Herb (NS) --------------------------------------------------

# Generate prediction and add unscaled variable (use model2, which includes herb)
insight.bgden.herb <- get_datagrid(dat.plot.ex, by = c("Change_HerbCover_scaled"),
                                   length = 100)
insight.bgden.herb$Change_BGDensity <- get_predicted(bgden2, insight.bgden.herb)
insight.bgden.herb$SE <- get_predicted_ci(bgden2, data = insight.bgden.herb)$SE
insight.bgden.herb$CI <- insight.bgden.herb$SE * 1.96
unscaled.herb <- get_datagrid(dat.plot.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
insight.bgden.herb$Change_HerbCover <- unscaled.herb$Change_HerbCover

# Graph
bgden.herb <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = insight.bgden.herb,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.herb,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. herb cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.herb


## BG density: Precip * herb (NS) -----------------------------------------

# Generate prediction and add unscaled variable - 9 levels to get mean
insight.bgden.herb.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.bgden.herb.precip$Predicted <- get_predicted(bgden2, insight.bgden.herb.precip)
unscaled.herb.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.bgden.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.bgden.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.bgden.herb.precip$Prev_year_precip_scaled) 
insight.bgden.herb.precip <- insight.bgden.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.957, 0.005, 1.609))

# Graph
bgden.herb.precip <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgden.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. herb cover change")
bgden.herb.precip



# Buffelgrass cover change ------------------------------------------------

## BG cover: Precip -------------------------------------------------------

# Generate prediction and add scaled variable 
insight.bgcov.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgcov.precip$Change_BGCover <- get_predicted(bgcov2, insight.bgcov.precip)
insight.bgcov.precip$SE <- get_predicted_ci(bgcov2, data = insight.bgcov.precip)$SE
insight.bgcov.precip$CI <- insight.bgcov.precip$SE * 1.96
unscaled.precip <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgcov.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

# Graph
bgcov.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = insight.bgcov.precip,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.precip,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. precip")
bgcov.precip


## BG cover: Aspect -------------------------------------------------------

# Generate prediction & CI
gg.bgcov.aspect <- predict_response(bgcov2, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_BGCover = predicted)

# Graph 
bgcov.aspect <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGCover)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_pointrange(data = gg.bgcov.aspect,
                  aes(ymin = conf.low, ymax = conf.high),
                  color = "purple3",
                  linewidth = 1.3) +
  geom_point(data = gg.bgcov.aspect,
             aes(x = Aspect, y = Change_BGCover),
             color = "purple3",
             size = 3,
             shape = 15) +
  theme_bw() +
  labs(title = "Change in plot cover by aspect",
       y = expression(Delta ~ "Cover (%)"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgcov.aspect


## BG cover: Shrub --------------------------------------------------------

# Generate prediction and add scaled variable 
insight.bgcov.shrub <- get_datagrid(dat.plot.ex, by = c("Change_ShrubCover_scaled"),
                                    length = 100)
insight.bgcov.shrub$Change_BGCover <- get_predicted(bgcov2, insight.bgcov.shrub)
insight.bgcov.shrub$SE <- get_predicted_ci(bgcov2, data = insight.bgcov.shrub)$SE
insight.bgcov.shrub$CI <- insight.bgcov.shrub$SE * 1.96
unscaled.shrub <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
insight.bgcov.shrub$Change_ShrubCover <- unscaled.shrub$Change_ShrubCover

# Graph
bgcov.shrub <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = insight.bgcov.shrub,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.shrub,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. shrub cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.shrub


## BG cover: Slope (NS) ---------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgcov.slope <- get_datagrid(dat.plot.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.bgcov.slope$Change_BGCover <- get_predicted(bgcov2, insight.bgcov.slope)
insight.bgcov.slope$SE <- get_predicted_ci(bgcov2, data = insight.bgcov.slope)$SE
insight.bgcov.slope$CI <- insight.bgcov.slope$SE * 1.96
unscaled.slope <- get_datagrid(dat.plot.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.bgcov.slope$PlotSlope <- unscaled.slope$PlotSlope

# Graph
bgcov.slope <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = insight.bgcov.slope,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.slope,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in buffelgrass cover vs. slope") +
  labs(y = expression(Delta ~ "Cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgcov.slope


## BG cover: Herb (NS) ----------------------------------------------------

# Generate prediction and add scaled variable 
insight.bgcov.herb <- get_datagrid(dat.plot.ex, by = c("Change_HerbCover_scaled"),
                                   length = 100)
insight.bgcov.herb$Change_BGCover <- get_predicted(bgcov2, insight.bgcov.herb)
insight.bgcov.herb$SE <- get_predicted_ci(bgcov2, data = insight.bgcov.herb)$SE
insight.bgcov.herb$CI <- insight.bgcov.herb$SE * 1.96
unscaled.herb <- get_datagrid(dat.plot.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
insight.bgcov.herb$Change_HerbCover <- unscaled.herb$Change_HerbCover

# Graph
bgcov.herb <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = insight.bgcov.herb,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.herb,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native herb cover (%)"),
       title = "Change in buffelgrass cover vs. herb cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.herb


## BG cover: Precip * shrub (NS) ------------------------------------------

# Generate prediction and add unscaled variable - 9 levels to get mean
insight.bgcov.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.bgcov.shrub.precip$Predicted <- get_predicted(bgcov2, insight.bgcov.shrub.precip)
unscaled.shrub.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.bgcov.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.bgcov.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.bgcov.shrub.precip$Prev_year_precip_scaled) 
insight.bgcov.shrub.precip <- insight.bgcov.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.957, 0.005, 1.609))

# Graph
bgcov.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgcov.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. shrub cover change")
bgcov.shrub.precip


## BG cover: Precip * herb (NS) -------------------------------------------

# Generate prediction and add unscaled variable - 9 levels to get mean
insight.bgcov.herb.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.bgcov.herb.precip$Predicted <- get_predicted(bgcov2, insight.bgcov.herb.precip)
unscaled.herb.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.bgcov.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.bgcov.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.bgcov.herb.precip$Prev_year_precip_scaled) 
insight.bgcov.herb.precip <- insight.bgcov.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.957, 0.005, 1.609))

# Graph
bgcov.herb.precip <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgcov.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass cover vs. herb cover change")
bgcov.herb.precip



# Survival ----------------------------------------------------------------

## Survival: Precip -------------------------------------------------------

#   Construct datagrid with scaled variable 
insight.survival.precip <- get_datagrid(dat.survival.ex, by = c("Prev_year_precip_scaled"),                                   
                                        length = 50)
#   Attempt to add prediction and CI (something is wrong with CI)
insight.survival.precip <- insight.survival.precip %>% 
  bind_cols(as.data.frame(get_predicted(survival2, insight.survival.precip))) %>% 
  rename(survival_transf = Predicted) 
#   Give up on CI and make unscaled datagrid
unscaled.precip <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                  length = 50) %>% 
  arrange(Prev_year_precip)
insight.survival.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

#   Graph, no CI (insight version)
survival.precip <- dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.precip,
            aes(y = survival_transf), linewidth = 1.3,
            color = "purple3") +
  geom_ribbon(data = insight.survival.precip,
              aes(ymin = CI_low, ymax = CI_high)) + # no ribbon because CI values are same as predicted
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") 
survival.precip 


# ggeffects version
#   Generate CI and add unscaled variable
gg.survival.precip <- predict_response(survival2, terms = "Prev_year_precip_scaled")
unscaled.precip11 <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                  length = 11) %>% 
  arrange(Prev_year_precip)
gg.survival.precip$Prev_year_precip <- unscaled.precip11$Prev_year_precip
gg.survival.precip$survival_transf <- gg.survival.precip$predicted 

# Graph (ggeffects version)
survival.precip.gg <- dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") 
survival.precip.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.survival.precip,
            aes(y = survival_transf), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") # insight above ggeffects (but pretty close)



## Survival: Slope (NS) ---------------------------------------------------

# (don't bother with CI, has same issue as precip did) 
#   Generate prediction and add scaled variable (use model5, which includes slope)
insight.survival.slope <- get_datagrid(dat.survival.ex, by = c("PlotSlope_scaled"),
                                       length = 50)
insight.survival.slope$survival_transf <- get_predicted(survival_model5, insight.survival.slope) 
unscaled.slope50 <- get_datagrid(dat.survival.unscaled, by = "PlotSlope",
                                 length = 50) %>% 
  arrange(PlotSlope)
insight.survival.slope$PlotSlope <- unscaled.slope50$PlotSlope

#   Graph
survival.slope <- dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.slope,
            aes(y = survival_transf), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") 
survival.slope 


# ggeffects version
#   Generate CI and add unscaled variable
gg.survival.slope <- predict_response(survival_model5, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.survival.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
gg.survival.slope$PlotSlope <- unscaled.slope12$PlotSlope
gg.survival.slope$survival_transf <- gg.survival.slope$predicted 

#   Graph (ggeffects version)
survival.slope.gg <- dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") 
survival.slope.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.survival.slope,
            aes(y = survival_transf), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") # basically the same



## Survival: BG density (NS) ----------------------------------------------

# Generate prediction and add scaled variable (don't bother with CI, has same issue as precip did) 
insight.survival.bgden <- get_datagrid(dat.survival.ex, by = c("BGDensity_scaled"),
                                       length = 50)
insight.survival.bgden$survival_transf <- get_predicted(survival2, insight.survival.bgden) 
unscaled.bgden50 <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                                 length = 50) %>% 
  arrange(BGDensity)
insight.survival.bgden$BGDensity <- unscaled.bgden50$BGDensity

# Graph, no CI (insight version)
survival.bgden <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.bgden,
            aes(y = survival_transf), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") 
survival.bgden


# ggeffects version
#   Generate CI and add unscaled variable
gg.survival.bgden <- predict_response(survival2, terms = "BGDensity_scaled")
unscaled.bgden9 <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                                length = 9) %>% 
  arrange(BGDensity)
gg.survival.bgden$BGDensity <- unscaled.bgden9$BGDensity
gg.survival.bgden$survival_transf <- gg.survival.bgden$predicted 

#   Graph (ggeffects version)
survival.bgden.gg <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") 
survival.bgden.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.survival.bgden,
            aes(y = survival_transf), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") # basically the same



## Survival: Shrub (NS) ---------------------------------------------------

# Generate prediction and add scaled variable (CI still has problems)
insight.survival.shrub <- get_datagrid(dat.survival.ex, by = c("ShrubCover_scaled"),
                                       length = 50)
insight.survival.shrub$survival_transf <- get_predicted(survival2, insight.survival.shrub) 
unscaled.shrub50 <- get_datagrid(dat.survival.unscaled, by = "ShrubCover",
                                 length = 50) %>% 
  arrange(ShrubCover)
insight.survival.shrub$ShrubCover <- unscaled.shrub50$ShrubCover

# Graph, no CI (insight version)
survival.shrub <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.shrub,
            aes(y = survival_transf), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native shrub cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. shrub cover") 
survival.shrub 



## Survival: Herb (NS) ----------------------------------------------------

# Generate prediction and add scaled variable 
insight.survival.herb <- get_datagrid(dat.survival.ex, by = c("HerbCover_scaled"),
                                      length = 50)
insight.survival.herb$survival_transf <- get_predicted(survival2, insight.survival.herb)
unscaled.herb50 <- get_datagrid(dat.survival.unscaled, by = "HerbCover",
                                length = 50) %>% 
  arrange(HerbCover)
insight.survival.herb$HerbCover <- unscaled.herb50$HerbCover

# Graph, no CI
survival.herb <- dat.survival %>% 
  ggplot(aes(x =HerbCover, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.herb,
            aes(y = survival_transf), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native forb & grass cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. herb cover") 
survival.herb 


## Survival: Precip * density (NS) ----------------------------------------

# Generate prediction and add unscaled variable (use model6, which includes precip*density interaction)
insight.survival.bgden.precip <- dat.survival.ex %>% 
  get_datagrid(c("BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(BGDensity_scaled)
insight.survival.bgden.precip$Predicted <- get_predicted(survival_model6, insight.survival.bgden.precip) 
unscaled.bgden.precip43 <- dat.survival.unscaled %>% 
  get_datagrid(c("BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(BGDensity)
insight.survival.bgden.precip$BGDensity <- unscaled.bgden.precip43$BGDensity
insight.survival.bgden.precip$Prev_year_precip <- unscaled.bgden.precip43$Prev_year_precip
unique(insight.survival.bgden.precip$Prev_year_precip_scaled) # -1.036, 1.240, 3.515

# Graph, no CI (insight version)
survival.bgden.precip <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.survival.bgden.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = expression(paste("Density (individuals / ", m^2, ")")),
       title = "Buffelgrass seedling survival vs. density") +
  scale_y_continuous(labels = scales::percent)
survival.bgden.precip # lines are kind of weird and not smooth


# Generate CI with scaled explanatory variable
survival.pred.bgden.precip <- predict_response(survival_model6, 
                                               terms = c("BGDensity_scaled", "Prev_year_precip_scaled"))
survival.pred.bgden.precip$group <- factor(survival.pred.bgden.precip$group,
                                           levels = c("1", "0", "-1"))
survival.pred.bgden.precip <- as.data.frame(survival.pred.bgden.precip)

# Graph with CI, scaled prediction only (ggeffects version)  
survival.bgden.precip.ci <- survival.pred.bgden.precip %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  labs(y = "Seedling survival (%)",
       x = "Density (scaled)",
       title = "Buffelgrass seedling survival vs. density (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(labels = scales::percent) 
survival.bgden.precip.ci # insight higher than ggeffects (looks pretty different, especially for wettest)



## Survival: Precip * shrub (NS) ------------------------------------------

# Generate prediction and add unscaled variable
insight.survival.shrub.precip <- dat.survival.ex %>% 
  get_datagrid(c("ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(ShrubCover_scaled)
insight.survival.shrub.precip$Predicted <- get_predicted(survival2, insight.survival.shrub.precip) # Warning: Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.shrub.precip43 <- dat.survival.unscaled %>% 
  get_datagrid(c("ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(ShrubCover)
insight.survival.shrub.precip$ShrubCover <- unscaled.shrub.precip43$ShrubCover
insight.survival.shrub.precip$Prev_year_precip <- unscaled.shrub.precip43$Prev_year_precip
unique(insight.survival.shrub.precip$Prev_year_precip_scaled) # -1.036, 1.240, 3.515

# Graph, no CI (insight version)
survival.shrub.precip <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.survival.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = "Native shrub cover (%)",
       title = "Buffelgrass seedling survival vs. shrub cover") +
  scale_y_continuous(labels = scales::percent)
survival.shrub.precip 


## Survival: Precip * herb (NS) -------------------------------------------

# Generate prediction and add unscaled variable
insight.survival.herb.precip <- dat.survival.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(HerbCover_scaled)
insight.survival.herb.precip$Predicted <- get_predicted(survival2, insight.survival.herb.precip) # Warning: Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.herb.precip43 <- dat.survival.unscaled %>% 
  get_datagrid(c("HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(HerbCover)
insight.survival.herb.precip$HerbCover <- unscaled.herb.precip43$HerbCover
insight.survival.herb.precip$Prev_year_precip <- unscaled.herb.precip43$Prev_year_precip
unique(insight.survival.herb.precip$Prev_year_precip_scaled) # -1.036, 1.240, 3.515

# Graph, CI
survival.herb.precip <- dat.survival %>% 
  ggplot(aes(x = HerbCover, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.survival.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = "Native grass & forb cover (%)",
       title = "Buffelgrass seedling survival vs. herb cover") +
  scale_y_continuous(labels = scales::percent)
survival.herb.precip 



# Shrub and herb vs. precip (linear regression) ---------------------------

# Shrub vs. precip
dat %>% 
  ggplot(aes(x = Prev_year_precip, y = ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# Shrub cover change vs. precip
shrub.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native shrub cover (%)"),
       x = "Previous year precipitation (mm)",
       title = "Change in shrub cover vs. precip")
shrub.change.precip


# Herb vs. precip
dat %>% 
  ggplot(aes(x = Prev_year_precip, y = HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# Herb cover change vs. precip
herb.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native grass & forb cover (%)"),
       x = "Previous year precipitation (mm)",
       title = "Change in herb cover vs. precip")
herb.change.precip



# Cover simple linear regression ------------------------------------------

# Total vs. repro
total.repro <- culm.change.flat.rm %>% 
  ggplot(aes(x = Change_Reproductive_culms, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Reproductive culm count"),
       title = "Change in total culm count vs. change in repro culm count")
total.repro

# Total vs. cover
total.cover <- culm.change.flat.rm %>% 
  ggplot(aes(x = Change_BGCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Cover (%)"),
       title = "Change in total culm count vs. change in plot cover")
total.cover

# Density vs. cover
density.cover <- plot.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(x = expression(Delta ~ "Cover (%)"),
       y = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in plot density vs. change in plot cover")
density.cover



# Write out draft figures -------------------------------------------------

## Total change -----------------------------------------------------------

# Significant
# Total change vs. Prev_year_precip
tiff("figures/2026-01_draft-figures/Total-change_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.precip
dev.off()

# Total change by Aspect
tiff("figures/2026-01_draft-figures/Total-change_aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.aspect
dev.off()

# Total change vs. Change_BGDensity
tiff("figures/2026-01_draft-figures/Total-change_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.bgden
dev.off()

# Total change vs. Change_ShrubCover
tiff("figures/2026-01_draft-figures/Total-change_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.shrub
dev.off()

# Total change interaction of precip*shrub
tiff("figures/2026-01_draft-figures/Total-change_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.shrub.precip
dev.off()

# Total change interaction of precip*herb
tiff("figures/2026-01_draft-figures/Total-change_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.herb.precip
dev.off()


# Not significant
# Total change vs. PlotSlope
tiff("figures/2026-01_draft-figures/Total-change_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.slope
dev.off()

# Total change vs. Change_HerbCover 
tiff("figures/2026-01_draft-figures/Total-change_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.herb
dev.off()

# Total change interaction of precip*BG density
tiff("figures/2026-01_draft-figures/Total-change_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.bgden.precip
dev.off()



## Repro change -----------------------------------------------------------

# Significant
# Repro change by Aspect
tiff("figures/2026-01_draft-figures/Repro-change_aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.aspect
dev.off()

# Repro change vs. Change_BGDensity
tiff("figures/2026-01_draft-figures/Repro-change_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.bgden
dev.off()

# Repro change vs. Change_ShrubCover
tiff("figures/2026-01_draft-figures/Repro-change_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.shrub
dev.off()

# Repro change vs. Change_HerbCover
tiff("figures/2026-01_draft-figures/Repro-change_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.herb
dev.off()


# Not significant
# Repro change vs. Prev_year_precip
tiff("figures/2026-01_draft-figures/Repro-change_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.precip
dev.off()

# Repro change vs. PlotSlope
tiff("figures/2026-01_draft-figures/Repro-change_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.slope
dev.off()

# Repro change interaction of precip*BG density
tiff("figures/2026-01_draft-figures/Repro-change_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.bgden.precip
dev.off()

# Repro change interaction of precip*shrub
tiff("figures/2026-01_draft-figures/Repro-change_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.shrub.precip
dev.off()

# Repro change interaction of precip*herb
tiff("figures/2026-01_draft-figures/Repro-change_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.herb.precip
dev.off()


## BG density change ------------------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2026-01_draft-figures/BG-density-change_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.precip
dev.off()

# BG density change by Aspect 
tiff("figures/2026-01_draft-figures/BG-density-change_aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.aspect
dev.off()

# BG density change interaction of precip*shrub 
tiff("figures/2026-01_draft-figures/BG-density-change_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.shrub.precip
dev.off()


# Not significant
# BG density change vs. PlotSlope
tiff("figures/2026-01_draft-figures/BG-density-change_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.slope
dev.off()

# BG density change vs. Change_ShrubCover
tiff("figures/2026-01_draft-figures/BG-density-change_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.shrub
dev.off()

# BG density change vs. Change_HerbCover
tiff("figures/2026-01_draft-figures/BG-density-change_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.herb
dev.off()

# BG density change interaction of precip*herb
tiff("figures/2026-01_draft-figures/BG-density-change_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.herb.precip
dev.off()


## BG cover change --------------------------------------------------------

# Significant
# BG cover change vs. Prev_year_precip
tiff("figures/2026-01_draft-figures/BG-cover-change_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.precip
dev.off()

# BG cover change by Aspect 
tiff("figures/2026-01_draft-figures/BG-cover-change_aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.aspect
dev.off()

# BG cover change vs. Change_ShrubCover
tiff("figures/2026-01_draft-figures/BG-cover-change_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.shrub
dev.off()


# Not significant
# BG cover change vs. PlotSlope
tiff("figures/2026-01_draft-figures/BG-cover-change_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.slope
dev.off()

# BG cover change vs. Change_HerbCover
tiff("figures/2026-01_draft-figures/BG-cover-change_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.herb
dev.off()

# BG cover change interaction of precip*shrub
tiff("figures/2026-01_draft-figures/BG-cover-change_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.shrub.precip
dev.off()

# BG cover change interaction of precip*herb
tiff("figures/2026-01_draft-figures/BG-cover-change_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.herb.precip
dev.off()



## Survival ---------------------------------------------------------------

# Significant
# Survival vs. Prev_year_precip
tiff("figures/2026-01_draft-figures/Survival_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip
dev.off()
# Survival vs. Prev_year_precip (with CI)
tiff("figures/2026-01_draft-figures/Survival_prev-year-precip_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip.gg
dev.off()


# Not significant
# Survival vs. PlotSlope
tiff("figures/2026-01_draft-figures/Survival_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.slope
dev.off()
# Survival vs. PlotSlope (with CI)
tiff("figures/2026-01_draft-figures/Survival_plot-slope_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.slope.gg
dev.off()

# Survival vs. BGDensity
tiff("figures/2026-01_draft-figures/Survival_BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden
dev.off()
# Survival vs. BGDensity (with CI)
tiff("figures/2026-01_draft-figures/Survival_BG-density_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden.gg
dev.off()

# Survival vs. ShrubCover
tiff("figures/2026-01_draft-figures/Survival_shrub-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.shrub
dev.off()
# Survival vs. ShrubCover (with CI)
tiff("figures/2026-01_draft-figures/Survival_shrub-cover_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.shrub.gg
dev.off()

# Survival vs. HerbCover
tiff("figures/2026-01_draft-figures/Survival_herb-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.herb
dev.off()
# Survival vs. HerbCover (with CI)
tiff("figures/2026-01_draft-figures/Survival_herb-cover_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.herb.gg
dev.off()

# Survival interaction of precip*density
tiff("figures/2026-01_draft-figures/Survival_BG-density-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.bgden.precip
dev.off()
# Survival interaction of precip*density (with CI, scaled)
tiff("figures/2026-01_draft-figures/Survival_BG-density-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.bgden.precip.ci
dev.off()

# Survival interaction of precip*shrub
tiff("figures/2026-01_draft-figures/Survival_shrub-cover-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.shrub.precip
dev.off()
# Survival interaction of precip*shrub (with CI, scaled)
tiff("figures/2026-01_draft-figures/Survival_shrub-cover-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.shrub.precip.ci
dev.off()

# Survival interaction of precip*herb
tiff("figures/2026-01_draft-figures/Survival_herb-cover-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.precip
dev.off()
# Survival interaction of precip*herb (with CI, scaled)
tiff("figures/2026-01_draft-figures/Survival_herb-cover-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.precip.ci
dev.off()



## Precip plot ------------------------------------------------------------

# Combined precip plot for density, cover, survival, no CI
tiff("figures/2026-01_draft-figures/Precip-combined_density-cover-survival.tiff",
     units = "in", height = 7, width = 9, res = 150)
ggarrange(bgden.precip.noci, bgcov.precip.noci, survival.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))
dev.off()

# Combined precip plot for density, cover, survival with CI
tiff("figures/2026-01_draft-figures/Precip-combined_density-cover-survival_CI.tiff",
     units = "in", height = 7, width = 9, res = 150)
ggarrange(bgden.precip, bgcov.precip, survival.precip.gg,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))
dev.off()



## Shrub & herb cover change vs. precip -----------------------------------

# Shrub
tiff("figures/2026-01_draft-figures/Shrub-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
shrub.change.precip
dev.off()

# Herb
tiff("figures/2026-01_draft-figures/Herb-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
herb.change.precip
dev.off()


## Relationships with cover -----------------------------------------------

# Total culm change vs. cover change
tiff("figures/2026-01_draft-figures/Total-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.cover
dev.off()

# Density change vs. cover change
tiff("figures/2026-01_draft-figures/Density-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
density.cover
dev.off()


save.image("RData/10.2_draft-figs-for-lm-revision1.RData")


