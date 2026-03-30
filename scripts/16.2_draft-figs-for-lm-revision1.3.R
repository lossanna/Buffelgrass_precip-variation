# Created: 2026-03-24
# Updated: 2026-03-24

# Purpose: Graph data to accompany 15.2.R of revision1.3 linear models (includes
#   initial BG density/shrub/herb conditions, as well as slope*shrub/herb interactions; aspect dropped).

# https://easystats.github.io/modelbased/articles/visualisation_matrix.html#visualising-an-interaction-between-two-numeric-variables-three-way-interaction


library(tidyverse)
library(insight)
library(modelbased)
library(ggeffects)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/15.2_data-and-models-revision1.3.RData")
dat.survival.raw <- dat.survival
dat <- read_csv("data/cleaned/11.1_demography-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm change - total & repro
dat.culm <- culm.change %>% 
  select(Change_Total_Live_Culms, Change_Reproductive_culms, 
         Prev_year_precip, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Plot change - density & cover
dat.plot <- plot.change %>% 
  select(Change_BGDensity, Change_BGCover, 
         Prev_year_precip, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover) 

#   Survival  
dat.survival <- dat.survival.raw %>%
  select(remaining_toothpicks, seedlings_surviving,
         Prev_year_precip, PlotSlope, BGDensity, ShrubCover, HerbCover,
         Prev_year_precip_scaled, PlotSlope_scaled, BGDensity_scaled,
         ShrubCover_scaled, HerbCover_scaled) %>% 
  mutate(survival_prop = seedlings_surviving / remaining_toothpicks)


# 2. Dataset for constructing datagrid with prediction & CI (scaled explanatory variables only)
#   Culm change - total & repro 
dat.culm.ex <- culm.change %>% 
  select(Prev_year_precip_scaled, PlotSlope_scaled, 
         Change_BGDensity_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Init_BGDensity_scaled, Init_ShrubCover_scaled, Init_HerbCover_scaled)

#   Plot change - density & cover
dat.plot.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Init_BGDensity_scaled, Init_ShrubCover_scaled, Init_HerbCover_scaled)

#   Survival  
dat.survival.ex <- dat.survival.raw %>%
  select(Prev_year_precip_scaled, Aspect,
         PlotSlope_scaled, BGDensity_scaled, ShrubCover_scaled, HerbCover_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm change - total & repro
dat.culm.unscaled <- culm.change %>% 
  select(Prev_year_precip, PlotSlope, 
         Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Plot change - density & cover
dat.plot.unscaled <- plot.change %>% 
  select(Prev_year_precip, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

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
range(dat.culm.ex$Prev_year_precip_scaled) # -0.943, 1.659

# Precip range for plot
range(dat.plot.ex$Prev_year_precip_scaled) # -0.941, 1.657


## Culm models ------------------------------------------------------------

# Results: I checked 3-12 precip levels, and they were basically the same as R1.2;
#   will just go with 9 precip levels, which is 0.033 (more than that and it takes too long
#   to run).

# 3 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # 0.358

# 4 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 4, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.076

# 5 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.292

# 6 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 6, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # 0.098

# 7 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 7, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.076

# 8 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 8, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # 0.172

# 9 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # 0.033

# 10 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.076



## Density & cover models -------------------------------------------------

# Results: 12 precip levels is 0.004, which is the closest to 0 that I can get with
#   reasonable computation time.

# 3 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.358

# 4 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 4, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.075

# 5 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.292

# 6 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 6, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # 0.098

# 7 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 7, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.075

# 8 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 8, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.172

# 9 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # 0.033

# 10 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.075

# 11 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 11, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # 0.098

# 12 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.004

# 13 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 13, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # -0.075

# 14 precip levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 14, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$Prev_year_precip_scaled) # 0.058


## Survival model ---------------------------------------------------------

# Results: I checked 3-10 precip levels, and the closest that came to 0 was -0.005,
#   which was generated first at 5 precip levels.

# 3 precip levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 1.013

# 4 precip levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 4, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.334

# 5 precip levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.005

# 6 precip levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 6, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.606

# 7 precip levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 7, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.334

# 8 precip levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 8, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.140

# 9 precip levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.005

# 10 precip levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 10, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.118




# Test different slope levels ---------------------------------------------

# Once again trying to find something close to 0 with reasonable computation time.

# Slope range for culm
range(dat.culm.ex$PlotSlope_scaled) # -2.268, 2.713

# Slope range for plot
range(dat.plot.ex$PlotSlope_scaled) # -2.245, 2.673


## Culm models ------------------------------------------------------------

# Results: I checked 3-11 slope levels, and the closest that came to 0 was -0.054,
#   which was generated first at 10 slope levels.

# 3 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.223

# 4 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 4, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.608

# 5 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 5, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.223

# 6 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 6, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.276

# 7 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 7, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.223

# 8 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 8, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.133

# 9 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 5) %>% 
  get_datagrid("PlotSlope_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.223

# 10 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.054

# 11 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 11, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.223


## Density & cover models -------------------------------------------------

# Results: 12 slope levels is -0.010, which is the closest to 0 that I can get with
#   reasonable computation time.

# 3 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$PlotSlope_scaled) # 0.214

# 4 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 4, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.606

# 5 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 5, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.214

# 6 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 6, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.278

# 7 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 7, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.606

# 8 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 8, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$PlotSlope_scaled) # -0.137

# 9 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.214

# 10 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.059

# 11 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 11, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.214

# 12 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
unique(x$PlotSlope_scaled) # -0.010

# 13 slope levels
x <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>% 
  get_datagrid("PlotSlope_scaled", length = 13, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.196


## Survival model ---------------------------------------------------------

# Results: 12 slope levels is 0.026.

# 3 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 3, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # -0.181

# 4 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 4, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # 0.580

# 5 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 5, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # -0.181

# 6 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 6, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # 0.275

# 7 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 7, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # 0.580

# 8 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 8, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # 0.145

# 9 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 9, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # -0.181

# 10 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # 0.072

# 11 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 11, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # -0.181

# 12 slope levels
x <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 10) %>%
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>%
  arrange(HerbCover_scaled)
unique(x$PlotSlope_scaled) # 0.026



# Precip ------------------------------------------------------------------

# Create df of dat for joining
dat.precip.join <- dat %>% 
  select(Date, Year, Site, Transect, Plot, Prev_year_precip, Perc_dev, MAP) %>% 
  distinct(.keep_all = TRUE) 

# Create df of average for precip by site
precip.avg.site <- dat.precip.join %>% 
  group_by(Year, Site) %>% 
  summarise(Perc_dev_avg = mean(Perc_dev),
            Prev_year_precip_avg = mean(Prev_year_precip),
            .groups = "keep")

map.avg.all <- dat.precip.join %>% 
  select(Site, Transect, MAP) %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(Site) %>% 
  summarise(MAP_avg = mean(MAP))
map.avg.all

# Line graph of averages across sites
precip.site.all <- precip.avg.site %>% 
  ggplot(aes(x = Year, y = Prev_year_precip_avg)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw() +
  labs(x = NULL,
       y = "Previous year precip (mm)",
       title = "Precipitation conditions") +
  theme(axis.text.x = element_text(color = "black")) +
  geom_hline(data = map.avg.all, 
             aes(yintercept = MAP_avg), 
             linetype = "dashed", color = "red")
precip.site.all

#   Percent deviation
precip.site.dev.all <- precip.avg.site %>% 
  ggplot(aes(x = Year, y = Perc_dev_avg)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "Percent deviation from normals (%)",
       title = "Precipitation conditions") +
  theme(axis.text.x = element_text(color = "black")) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "red") 
precip.site.dev.all


## Culm, density, cover change --------------------------------------------

# Join with plot.change 
plot.change.join <- plot.change %>% 
  left_join(dat.precip.join)

summary(plot.change.join$Prev_year_precip)

# Create df of average for precip by site
plot.avg.site <- plot.change.join %>% 
  group_by(Year, Site) %>% 
  summarise(Perc_dev_avg = mean(Perc_dev),
            Prev_year_precip_avg = mean(Prev_year_precip),
            .groups = "keep")

map.avg.change <- plot.change.join %>% 
  select(Site, Transect, MAP) %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(Site) %>% 
  summarise(MAP_avg = mean(MAP))
map.avg.change

# Line graph of averages across sites
precip.site.change <- plot.avg.site %>% 
  ggplot(aes(x = Year, y = Prev_year_precip_avg)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw() +
  labs(x = NULL,
       y = "Previous year precip (mm)",
       title = "Precipitation conditions") +
  theme(axis.text.x = element_text(color = "black")) +
  geom_hline(data = map.avg.change, 
             aes(yintercept = MAP_avg), 
             linetype = "dashed", color = "red")
precip.site.change

#   Percent deviation
plot.avg.site %>% 
  ggplot(aes(x = Year, y = Perc_dev_avg)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "Percent deviation from normals (%)",
       title = "Precipitation conditions") +
  theme(axis.text.x = element_text(color = "black")) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "red") 



# Total culm change -------------------------------------------------------

## Total: Precip ----------------------------------------------------------

# Construct datagrid with scaled variable 
insight.total.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
# Add prediction column (must be must be named the same as y-axis in graph)
insight.total.precip$Change_Total_Live_Culms <- get_predicted(total1, insight.total.precip) 
# Add SE and CI columns
insight.total.precip$SE <- get_predicted_ci(total1, data = insight.total.precip)$SE
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
  geom_point(alpha = 0.35) +
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


## Total: BG density ------------------------------------------------------

# Generate prediction and add unscaled variable
insight.total.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                    length = 100)
insight.total.bgden$Change_Total_Live_Culms <- get_predicted(total1, insight.total.bgden)
insight.total.bgden$SE <- get_predicted_ci(total1, data = insight.total.bgden)$SE
insight.total.bgden$CI <- insight.total.bgden$SE * 1.96
unscaled.bgden <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                               length = 100) %>% 
  arrange(Change_BGDensity)
insight.total.bgden$Change_BGDensity <- unscaled.bgden$Change_BGDensity

# Graph
total.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.total.bgden,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.bgden,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. change in density") +
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
insight.total.shrub$Change_Total_Live_Culms <- get_predicted(total1, insight.total.shrub)
insight.total.shrub$SE <- get_predicted_ci(total1, data = insight.total.shrub)$SE
insight.total.shrub$CI <- insight.total.shrub$SE * 1.96
unscaled.shrub <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                               length = 100) %>% 
  arrange(Change_ShrubCover)
insight.total.shrub$Change_ShrubCover <- unscaled.shrub$Change_ShrubCover

# Graph
total.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.total.shrub,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.shrub,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. change in shrub cover") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.shrub


## Total: Initial BG density ----------------------------------------------

# Generate prediction and add unscaled variable
insight.total.inbgden <- get_datagrid(dat.culm.ex, by = c("Init_BGDensity_scaled"),
                                      length = 100)
insight.total.inbgden$Change_Total_Live_Culms <- get_predicted(total1, insight.total.inbgden)
insight.total.inbgden$SE <- get_predicted_ci(total1, data = insight.total.inbgden)$SE
insight.total.inbgden$CI <- insight.total.inbgden$SE * 1.96
unscaled.inbgden <- get_datagrid(dat.culm.unscaled, by = "Init_BGDensity",
                                 length = 100) %>% 
  arrange(Init_BGDensity)
insight.total.inbgden$Init_BGDensity <- unscaled.inbgden$Init_BGDensity

# Graph
total.inbgden <- dat.culm %>% 
  ggplot(aes(x = Init_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.25) +
  geom_line(data = insight.total.inbgden,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.inbgden,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. initial density") +
  labs(x = expression(paste("Initial density (individuals / ", m^2, ")")),
       y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.inbgden


## Total: Precip * shrub --------------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.precip$Predicted <- get_predicted(total1, insight.total.shrub.precip)
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 3) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.total.shrub.precip$Prev_year_precip_scaled)
insight.total.shrub.precip <- insight.total.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.943, 0.033, 1.659))

# Graph
total.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
       title = "Change in total culm count vs. change in shrub cover")
total.shrub.precip


# Generate CI with scaled explanatory variable 
total.pred.shrub.precip <- predict_response(total1, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
total.pred.shrub.precip$group <- factor(total.pred.shrub.precip$group,
                                        levels = c("1", "0", "-1"))

# Graph with CI, scaled (ggeffects version)
total.shrub.precip.ci <- total.pred.shrub.precip %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Shrub cover (scaled)"),
       title = "Change in total culm count vs. change in shrub cover (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-120, 220))
total.shrub.precip.ci


## Total: Precip * herb ---------------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.herb.precip$Predicted <- get_predicted(total1, insight.total.herb.precip)
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 3) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.total.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.total.herb.precip$Prev_year_precip_scaled)
insight.total.herb.precip <- insight.total.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.943, 0.033, 1.659))

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
       title = "Change in total culm count vs. change in herb cover")
total.herb.precip


# Generate CI with scaled explanatory variable 
total.pred.herb.precip <- predict_response(total1, 
                                           terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
total.pred.herb.precip$group <- factor(total.pred.herb.precip$group,
                                       levels = c("1", "0", "-1"))

# Graph with CI, scaled (ggeffects version)
total.herb.precip.ci <- total.pred.herb.precip %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Herb cover (scaled)"),
       title = "Change in total culm count vs. change in herb cover (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-120, 220))
total.herb.precip.ci


## Total: Slope * shrub ---------------------------------------------------

# Generate prediction and add unscaled variable - 10 slope levels to get mean
insight.total.shrub.slope <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.slope$Predicted <- get_predicted(total1, insight.total.shrub.slope)
unscaled.shrub.slope <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 10, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.slope$Change_ShrubCover <- unscaled.shrub.slope$Change_ShrubCover
insight.total.shrub.slope$PlotSlope <- unscaled.shrub.slope$PlotSlope
unique(insight.total.shrub.slope$PlotSlope_scaled)
insight.total.shrub.slope <- insight.total.shrub.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.268, -0.054, 2.713))

# Graph
total.shrub.slope <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.shrub.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. change in shrub cover")
total.shrub.slope


# Generate CI with scaled explanatory variable 
total.pred.shrub.slope <- predict_response(total1, 
                                           terms = c("Change_ShrubCover_scaled", "PlotSlope_scaled"))
total.pred.shrub.slope$group <- factor(total.pred.shrub.slope$group,
                                       levels = c("1", "0", "-1"))

# Graph with CI, scaled (ggeffects version)
total.shrub.slope.ci <- total.pred.shrub.slope %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#0D0887FF", "#CC4678FF", "#F0F921FF")) +
  scale_fill_manual(values = c("#0D0887FF", "#CC4678FF", "#F0F921FF")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Shrub cover (scaled)"),
       title = "Change in total culm count vs. change in shrub cover (scaled)",
       color = "Plot slope (\u00B0)",
       fill = "Plot slope (\u00B0)") +
  scale_y_continuous(limits = c(-120, 220))
total.shrub.slope.ci


## Total: Slope (NS) ------------------------------------------------------

# Generate prediction and add unscaled variable
insight.total.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.total.slope$Change_Total_Live_Culms <- get_predicted(total1, insight.total.slope)
insight.total.slope$SE <- get_predicted_ci(total1, data = insight.total.slope)$SE
insight.total.slope$CI <- insight.total.slope$SE * 1.96
unscaled.slope <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                               length = 100) %>% 
  arrange(PlotSlope)
insight.total.slope$PlotSlope <- unscaled.slope$PlotSlope

# Graph
total.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.total.slope,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.slope,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.3) +
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
insight.total.herb$Change_Total_Live_Culms <- get_predicted(total1, insight.total.herb)
insight.total.herb$SE <- get_predicted_ci(total1, data = insight.total.herb)$SE
insight.total.herb$CI = insight.total.herb$SE * 1.96
unscaled.herb <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                              length = 100) %>% 
  arrange(Change_HerbCover)
insight.total.herb$Change_HerbCover <- unscaled.herb$Change_HerbCover

# Graph
total.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.total.herb,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.herb,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. change in herb cover") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.herb


## Total: Initial shrub cover (NS) -----------------------------------------

# Generate prediction and add unscaled variable
insight.total.inshrub <- get_datagrid(dat.culm.ex, by = c("Init_ShrubCover_scaled"),
                                      length = 10)
insight.total.inshrub$Change_Total_Live_Culms <- get_predicted(total1, insight.total.inshrub)
insight.total.inshrub$SE <- get_predicted_ci(total1, data = insight.total.inshrub)$SE
insight.total.inshrub$CI <- insight.total.inshrub$SE * 1.96
unscaled.inshrub <- get_datagrid(dat.culm.unscaled, by = "Init_ShrubCover",
                                 length = 10) %>% 
  arrange(Init_ShrubCover) # idk why but this only produces 23 rows when asked for 100, so I just have to go with 10, which it can do
insight.total.inshrub$Init_ShrubCover <- unscaled.inshrub$Init_ShrubCover

# Graph
total.inshrub <- dat.culm %>%
  ggplot(aes(x = Init_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.total.inshrub,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.inshrub,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. initial shrub cover") +
  labs(x = "Initial shrub cover (%)",
       y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.inshrub


## Total: Initial herb cover (NS) -----------------------------------------

# Generate prediction and add unscaled variable
insight.total.inherb <- get_datagrid(dat.culm.ex, by = c("Init_HerbCover_scaled"),
                                     length = 100)
insight.total.inherb$Change_Total_Live_Culms <- get_predicted(total1, insight.total.inherb)
insight.total.inherb$SE <- get_predicted_ci(total1, data = insight.total.inherb)$SE
insight.total.inherb$CI <- insight.total.inherb$SE * 1.96
unscaled.inherb <- get_datagrid(dat.culm.unscaled, by = "Init_HerbCover",
                                length = 100) %>% 
  arrange(Init_HerbCover)
insight.total.inherb$Init_HerbCover <- unscaled.inherb$Init_HerbCover

# Graph
total.inherb <- dat.culm %>%
  ggplot(aes(x = Init_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.total.inherb,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.inherb,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in total culm count vs. initial herb cover") +
  labs(x = "Initial grass & forb cover (%)",
       y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.inherb


## Total: Precip * density (NS) -------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.bgden.precip$Predicted <- get_predicted(total1, insight.total.bgden.precip)
unscaled.bgden.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 3) %>% 
  get_datagrid("Prev_year_precip", length =  9, numerics = "all") %>% 
  arrange(Change_BGDensity) %>% 
  distinct(.keep_all = TRUE)
insight.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip$Change_BGDensity
insight.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip$Prev_year_precip
unique(insight.total.bgden.precip$Prev_year_precip_scaled)
insight.total.bgden.precip <- insight.total.bgden.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.943, 0.033, 1.659))

# Graph
total.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
       title = "Change in total culm count vs. change in density")
total.bgden.precip


## Total: Slope * herb (NS) -----------------------------------------------

# Generate prediction and add unscaled variable - 10 slope levels to get mean
insight.total.herb.slope <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.herb.slope$Predicted <- get_predicted(total1, insight.total.herb.slope)
unscaled.herb.slope <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.herb.slope$Change_HerbCover <- unscaled.herb.slope$Change_HerbCover
insight.total.herb.slope$PlotSlope <- unscaled.herb.slope$PlotSlope
unique(insight.total.herb.slope$PlotSlope_scaled)
insight.total.herb.slope <- insight.total.herb.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.268, -0.054, 2.713))

# Graph
total.herb.slope <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.herb.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. change in herb cover")
total.herb.slope



# Reproductive culm change ------------------------------------------------

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
  geom_point(alpha = 0.35) +
  geom_line(data = insight.repro.herb,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.herb,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. change in herb cover") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.herb


## Repro: Initial BG density ----------------------------------------------

# Generate prediction and add unscaled variable
insight.repro.inbgden <- get_datagrid(dat.culm.ex, by = c("Init_BGDensity_scaled"),
                                      length = 100)
insight.repro.inbgden$Change_Reproductive_culms <- get_predicted(repro2, insight.repro.inbgden)
insight.repro.inbgden$SE <- get_predicted_ci(repro2, data = insight.repro.inbgden)$SE
insight.repro.inbgden$CI <- insight.repro.inbgden$SE * 1.96
unscaled.inbgden <- get_datagrid(dat.culm.unscaled, by = "Init_BGDensity",
                                 length = 100) %>% 
  arrange(Init_BGDensity)
insight.repro.inbgden$Init_BGDensity <- unscaled.inbgden$Init_BGDensity

# Graph
repro.inbgden <- dat.culm %>% 
  ggplot(aes(x = Init_BGDensity, y = Change_Reproductive_culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.repro.inbgden,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.inbgden,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. initial density") +
  labs(x = expression(paste("Initial density (individuals / ", m^2, ")")),
       y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.inbgden


## Repro: Precip * density ------------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.repro.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.bgden.precip$Predicted <- get_predicted(repro2, insight.repro.bgden.precip)
unscaled.bgden.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 3) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_BGDensity) %>% 
  distinct(.keep_all = TRUE)
insight.repro.bgden.precip$Change_BGDensity <- unscaled.bgden.precip$Change_BGDensity
insight.repro.bgden.precip$Prev_year_precip <- unscaled.bgden.precip$Prev_year_precip
unique(insight.repro.bgden.precip$Prev_year_precip_scaled)
insight.repro.bgden.precip <- insight.repro.bgden.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.943, 0.033, 1.659))

# Graph
repro.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
       title = "Change in repro culm count vs. change in density")
repro.bgden.precip


## Repro: Slope * shrub ---------------------------------------------------

# Generate prediction and add unscaled variable - 10 slope levels to get mean
insight.repro.shrub.slope <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.slope$Predicted <- get_predicted(repro2, insight.repro.shrub.slope)
unscaled.shrub.slope <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 10, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.slope$Change_ShrubCover <- unscaled.shrub.slope$Change_ShrubCover
insight.repro.shrub.slope$PlotSlope <- unscaled.shrub.slope$PlotSlope
unique(insight.repro.shrub.slope$PlotSlope_scaled)
insight.repro.shrub.slope <- insight.repro.shrub.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.268, -0.054, 2.713))

# Graph
repro.shrub.slope <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.repro.shrub.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in repro culm count vs. change in shrub cover")
repro.shrub.slope


# Generate CI with scaled explanatory variable 
repro.pred.shrub.slope <- predict_response(repro2, 
                                           terms = c("Change_ShrubCover_scaled", "PlotSlope_scaled"))
repro.pred.shrub.slope$group <- factor(repro.pred.shrub.slope$group,
                                       levels = c("1", "0", "-1"))

# Graph with CI, scaled (ggeffects version)
repro.shrub.slope.ci <- repro.pred.shrub.slope %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#0D0887FF", "#CC4678FF", "#F0F921FF")) +
  scale_fill_manual(values = c("#0D0887FF", "#CC4678FF", "#F0F921FF")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Shrub cover (scaled)"),
       title = "Change in repro culm count vs. change in shrub cover (scaled)",
       color = "Plot slope (\u00B0)",
       fill = "Plot slope (\u00B0)") +
  scale_y_continuous(limits = c(-85, 110))
repro.shrub.slope.ci


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

# Graph
repro.precip <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point(alpha = 0.35) +
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
  geom_point(alpha = 0.35) +
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


## Repro: BG density (NS) --------------------------------------------------

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
  geom_point(alpha = 0.35) +
  geom_line(data = insight.repro.bgden,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.bgden,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. change in density") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.bgden


## Repro: Shrub (NS) ------------------------------------------------------

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
  geom_point(alpha = 0.35) +
  geom_line(data = insight.repro.shrub,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.shrub,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. change in shrub cover") +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.shrub


## Repro: Initial shrub cover (NS) ----------------------------------------

# Generate prediction and add unscaled variable
insight.repro.inshrub <- get_datagrid(dat.culm.ex, by = c("Init_ShrubCover_scaled"),
                                      length = 10)
insight.repro.inshrub$Change_Reproductive_culms <- get_predicted(repro2, insight.repro.inshrub)
insight.repro.inshrub$SE <- get_predicted_ci(repro2, data = insight.repro.inshrub)$SE
insight.repro.inshrub$CI <- insight.repro.inshrub$SE * 1.96
unscaled.inshrub <- get_datagrid(dat.culm.unscaled, by = "Init_ShrubCover") %>% 
  arrange(Init_ShrubCover)
insight.repro.inshrub$Init_ShrubCover <- unscaled.inshrub$Init_ShrubCover

# Graph
repro.inshrub <- dat.culm %>%
  ggplot(aes(x = Init_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.repro.inshrub,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.inshrub,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. initial shrub cover") +
  labs(x = "Initial shrub cover (%)",
       y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.inshrub


## Repro: Initial herb cover (NS) -----------------------------------------

# Generate prediction and add unscaled variable
insight.repro.inherb <- get_datagrid(dat.culm.ex, by = c("Init_HerbCover_scaled"),
                                     length = 100)
insight.repro.inherb$Change_Reproductive_culms <- get_predicted(repro2, insight.repro.inherb)
insight.repro.inherb$SE <- get_predicted_ci(repro2, data = insight.repro.inherb)$SE
insight.repro.inherb$CI <- insight.repro.inherb$SE * 1.96
unscaled.inherb <- get_datagrid(dat.culm.unscaled, by = "Init_HerbCover",
                                length = 100) %>% 
  arrange(Init_HerbCover)
insight.repro.inherb$Init_HerbCover <- unscaled.inherb$Init_HerbCover

# Graph
repro.inherb <- dat.culm %>%
  ggplot(aes(x = Init_HerbCover, y = Change_Reproductive_culms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.repro.inherb,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.inherb,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  ggtitle("Change in repro culm count vs. initial herb cover") +
  labs(x = "Initial grass & forb cover (%)",
       y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.inherb


## Repro: Precip * shrub (NS) ---------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.repro.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.precip$Predicted <- get_predicted(repro2, insight.repro.shrub.precip)
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 3) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.repro.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.repro.shrub.precip$Prev_year_precip_scaled)
insight.repro.shrub.precip <- insight.repro.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.943, 0.033, 1.659))

# Graph
repro.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
       title = "Change in repro culm count vs. change in shrub cover")
repro.shrub.precip


## Repro: Precip * herb (NS) ----------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.repro.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.herb.precip$Predicted <- get_predicted(repro2, insight.repro.herb.precip)
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 3) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.repro.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.repro.herb.precip$Prev_year_precip_scaled)
insight.repro.herb.precip <- insight.repro.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.943, 0.033, 1.659))

# Graph
repro.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
       title = "Change in repro culm count vs. change in herb cover")
repro.herb.precip


## Repro: Slope * herb (NS) -----------------------------------------------

# Generate prediction and add unscaled variable - 10 slope levels to get mean
insight.repro.herb.slope <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.herb.slope$Predicted <- get_predicted(repro2, insight.repro.herb.slope)
unscaled.herb.slope <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.herb.slope$Change_HerbCover <- unscaled.herb.slope$Change_HerbCover
insight.repro.herb.slope$PlotSlope <- unscaled.herb.slope$PlotSlope
unique(insight.repro.herb.slope$PlotSlope_scaled)
insight.repro.herb.slope <- insight.repro.herb.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.268, -0.054, 2.713))

# Graph
repro.herb.slope <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.repro.herb.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in repro culm count vs. change in herb cover")
repro.herb.slope



# Buffelgrass density change ----------------------------------------------

## BG density: Precip -----------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgden.precip$Change_BGDensity <- get_predicted(bgden3, insight.bgden.precip)
insight.bgden.precip$SE <- get_predicted_ci(bgden3, data = insight.bgden.precip)$SE
insight.bgden.precip$CI <- insight.bgden.precip$SE * 1.96
unscaled.precip <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgden.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

# Graph 
bgden.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
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
       y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip")
bgden.precip


## BG density: Precip * shrub ---------------------------------------------

# Generate prediction and add unscaled variable - 12 levels to get mean
insight.bgden.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.shrub.precip$Predicted <- get_predicted(bgden3, insight.bgden.shrub.precip)
unscaled.shrub.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.bgden.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.bgden.shrub.precip$Prev_year_precip_scaled) 
insight.bgden.shrub.precip <- insight.bgden.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.941, 0.004, 1.657))

# Graph
bgden.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
  labs(y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. change in shrub cover")
bgden.shrub.precip


## BG density: Slope (NS) -------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.slope <- get_datagrid(dat.plot.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.bgden.slope$Change_BGDensity <- get_predicted(bgden3, insight.bgden.slope)
insight.bgden.slope$SE <- get_predicted_ci(bgden3, data = insight.bgden.slope)$SE
insight.bgden.slope$CI <- insight.bgden.slope$SE * 1.96
unscaled.slope <- get_datagrid(dat.plot.unscaled, by = "PlotSlope",
                               length = 100) %>% 
  arrange(PlotSlope)
insight.bgden.slope$PlotSlope <- unscaled.slope$PlotSlope

# Graph
bgden.slope <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
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
insight.bgden.shrub$Change_BGDensity <- get_predicted(bgden3, insight.bgden.shrub)
insight.bgden.shrub$SE <- get_predicted_ci(bgden3, data = insight.bgden.shrub)$SE
insight.bgden.shrub$CI <- insight.bgden.shrub$SE * 1.96
unscaled.shrub <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                               length = 100) %>% 
  arrange(Change_ShrubCover)
insight.bgden.shrub$Change_ShrubCover <- unscaled.shrub$Change_ShrubCover

# Graph
bgden.shrub <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.shrub,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.shrub,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. change in shrub cover") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.shrub


## BG density: Herb (NS) --------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.herb <- get_datagrid(dat.plot.ex, by = c("Change_HerbCover_scaled"),
                                   length = 100)
insight.bgden.herb$Change_BGDensity <- get_predicted(bgden3, insight.bgden.herb)
insight.bgden.herb$SE <- get_predicted_ci(bgden3, data = insight.bgden.herb)$SE
insight.bgden.herb$CI <- insight.bgden.herb$SE * 1.96
unscaled.herb <- get_datagrid(dat.plot.unscaled, by = "Change_HerbCover",
                              length = 100) %>% 
  arrange(Change_HerbCover)
insight.bgden.herb$Change_HerbCover <- unscaled.herb$Change_HerbCover

# Graph
bgden.herb <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.herb,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.herb,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. change in herb cover") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.herb


## BG density: Initial density (NS) ---------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.inbgden <- get_datagrid(dat.plot.ex, by = c("Init_BGDensity_scaled"),
                                      length = 100)
insight.bgden.inbgden$Change_BGDensity <- get_predicted(bgden3, insight.bgden.inbgden)
insight.bgden.inbgden$SE <- get_predicted_ci(bgden3, data = insight.bgden.inbgden)$SE
insight.bgden.inbgden$CI <- insight.bgden.inbgden$SE * 1.96
unscaled.inbgden <- get_datagrid(dat.plot.unscaled, by = "Init_BGDensity",
                                 length = 100) %>% 
  arrange(Init_BGDensity)
insight.bgden.inbgden$Init_BGDensity <- unscaled.inbgden$Init_BGDensity

# Graph
bgden.inbgden <- dat.plot %>% 
  ggplot(aes(x = Init_BGDensity, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.inbgden,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.inbgden,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       x = expression(paste("Initial density (individuals / ", m^2, ")")),
       title = "Change in buffelgrass density vs. initial density") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.inbgden


## BG density: Initial shrub cover (NS) -----------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.inshrub <- get_datagrid(dat.plot.ex, by = c("Init_ShrubCover_scaled"),
                                      length = 10)
insight.bgden.inshrub$Change_BGDensity <- get_predicted(bgden3, insight.bgden.inshrub)
insight.bgden.inshrub$SE <- get_predicted_ci(bgden3, data = insight.bgden.inshrub)$SE
insight.bgden.inshrub$CI <- insight.bgden.inshrub$SE * 1.96
unscaled.inshrub <- get_datagrid(dat.plot.unscaled, by = "Init_ShrubCover") %>% 
  arrange(Init_ShrubCover)
insight.bgden.inshrub$Init_ShrubCover <- unscaled.inshrub$Init_ShrubCover

# Graph
bgden.inshrub <- dat.plot %>% 
  ggplot(aes(x = Init_ShrubCover, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.inshrub,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.inshrub,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       x = "Initial shrub cover (%)",
       title = "Change in buffelgrass density vs. initial shrub cover") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.inshrub


## BG density: Initial herb cover (NS) ------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.inherb <- get_datagrid(dat.plot.ex, by = c("Init_HerbCover_scaled"),
                                     length = 100)
insight.bgden.inherb$Change_BGDensity <- get_predicted(bgden3, insight.bgden.inherb)
insight.bgden.inherb$SE <- get_predicted_ci(bgden3, data = insight.bgden.inherb)$SE
insight.bgden.inherb$CI <- insight.bgden.inherb$SE * 1.96
unscaled.inherb <- get_datagrid(dat.plot.unscaled, by = "Init_HerbCover",
                                length = 100) %>% 
  arrange(Init_HerbCover)
insight.bgden.inherb$Init_HerbCover <- unscaled.inherb$Init_HerbCover

# Graph
bgden.inherb <- dat.plot %>% 
  ggplot(aes(x = Init_HerbCover, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.inherb,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.inherb,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       x = "Initial grass & forb cover (%)",
       title = "Change in buffelgrass density vs. initial herb cover") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.inherb


## BG density: Precip * herb (NS) -----------------------------------------

# Generate prediction and add unscaled variable - 12 levels to get mean
insight.bgden.herb.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.herb.precip$Predicted <- get_predicted(bgden3, insight.bgden.herb.precip)
unscaled.herb.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.bgden.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.bgden.herb.precip$Prev_year_precip_scaled) 
insight.bgden.herb.precip <- insight.bgden.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.941, 0.004, 1.657))

# Graph
bgden.herb.precip <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
  labs(y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. change in herb cover")
bgden.herb.precip


## BG density: Slope * shrub (NS) -----------------------------------------

# Generate prediction and add unscaled variable - 12 slope levels to get mean
insight.bgden.shrub.slope <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.shrub.slope$Predicted <- get_predicted(bgden3, insight.bgden.shrub.slope)
unscaled.shrub.slope <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.shrub.slope$Change_ShrubCover <- unscaled.shrub.slope$Change_ShrubCover
insight.bgden.shrub.slope$PlotSlope <- unscaled.shrub.slope$PlotSlope
unique(insight.bgden.shrub.slope$PlotSlope_scaled)
insight.bgden.shrub.slope <- insight.bgden.shrub.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.245, -0.010, 2.673))

# Graph
bgden.shrub.slope <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.shrub.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. change in shrub cover")
bgden.shrub.slope


## BG density: Slope * herb (NS) ------------------------------------------

# Generate prediction and add unscaled variable - 12 slope levels to get mean
insight.bgden.herb.slope <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.herb.slope$Predicted <- get_predicted(bgden3, insight.bgden.herb.slope)
unscaled.herb.slope <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.herb.slope$Change_HerbCover <- unscaled.herb.slope$Change_HerbCover
insight.bgden.herb.slope$PlotSlope <- unscaled.herb.slope$PlotSlope
unique(insight.bgden.herb.slope$PlotSlope_scaled)
insight.bgden.herb.slope <- insight.bgden.herb.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.245, -0.010, 2.673))

# Graph
bgden.herb.slope <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.herb.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. change in herb cover")
bgden.herb.slope



# Buffelgrass cover change ------------------------------------------------

## BG cover: Precip -------------------------------------------------------

# Generate prediction and add scaled variable 
insight.bgcov.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgcov.precip$Change_BGCover <- get_predicted(bgcov4, insight.bgcov.precip)
insight.bgcov.precip$SE <- get_predicted_ci(bgcov4, data = insight.bgcov.precip)$SE
insight.bgcov.precip$CI <- insight.bgcov.precip$SE * 1.96
unscaled.precip <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgcov.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

# Graph
bgcov.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
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


## BG cover: Slope (NS) ---------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgcov.slope <- get_datagrid(dat.plot.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.bgcov.slope$Change_BGCover <- get_predicted(bgcov4, insight.bgcov.slope)
insight.bgcov.slope$SE <- get_predicted_ci(bgcov4, data = insight.bgcov.slope)$SE
insight.bgcov.slope$CI <- insight.bgcov.slope$SE * 1.96
unscaled.slope <- get_datagrid(dat.plot.unscaled, by = "PlotSlope",
                               length = 100) %>% 
  arrange(PlotSlope)
insight.bgcov.slope$PlotSlope <- unscaled.slope$PlotSlope

# Graph
bgcov.slope <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
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


## BG cover: Shrub (NS) ---------------------------------------------------

# Generate prediction and add scaled variable 
insight.bgcov.shrub <- get_datagrid(dat.plot.ex, by = c("Change_ShrubCover_scaled"),
                                    length = 100)
insight.bgcov.shrub$Change_BGCover <- get_predicted(bgcov4, insight.bgcov.shrub)
insight.bgcov.shrub$SE <- get_predicted_ci(bgcov4, data = insight.bgcov.shrub)$SE
insight.bgcov.shrub$CI <- insight.bgcov.shrub$SE * 1.96
unscaled.shrub <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                               length = 100) %>% 
  arrange(Change_ShrubCover)
insight.bgcov.shrub$Change_ShrubCover <- unscaled.shrub$Change_ShrubCover

# Graph
bgcov.shrub <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.shrub,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.shrub,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. change in shrub cover") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.shrub


## BG cover: Herb (NS) ----------------------------------------------------

# Generate prediction and add scaled variable 
insight.bgcov.herb <- get_datagrid(dat.plot.ex, by = c("Change_HerbCover_scaled"),
                                   length = 100)
insight.bgcov.herb$Change_BGCover <- get_predicted(bgcov4, insight.bgcov.herb)
insight.bgcov.herb$SE <- get_predicted_ci(bgcov4, data = insight.bgcov.herb)$SE
insight.bgcov.herb$CI <- insight.bgcov.herb$SE * 1.96
unscaled.herb <- get_datagrid(dat.plot.unscaled, by = "Change_HerbCover",
                              length = 100) %>% 
  arrange(Change_HerbCover)
insight.bgcov.herb$Change_HerbCover <- unscaled.herb$Change_HerbCover

# Graph
bgcov.herb <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.herb,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.herb,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native herb cover (%)"),
       title = "Change in buffelgrass cover vs. change in herb cover") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.herb


## BG cover: Initial density (NS) -----------------------------------------

# Generate prediction and add unscaled variable 
insight.bgcov.inbgden <- get_datagrid(dat.plot.ex, by = c("Init_BGDensity_scaled"),
                                      length = 100)
insight.bgcov.inbgden$Change_BGCover <- get_predicted(bgcov4, insight.bgcov.inbgden)
insight.bgcov.inbgden$SE <- get_predicted_ci(bgcov4, data = insight.bgcov.inbgden)$SE
insight.bgcov.inbgden$CI <- insight.bgcov.inbgden$SE * 1.96
unscaled.inbgden <- get_datagrid(dat.plot.unscaled, by = "Init_BGDensity",
                                 length = 100) %>% 
  arrange(Init_BGDensity)
insight.bgcov.inbgden$Init_BGDensity <- unscaled.inbgden$Init_BGDensity

# Graph
bgcov.inbgden <- dat.plot %>% 
  ggplot(aes(x = Init_BGDensity, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.inbgden,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.inbgden,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(paste("Initial density (individuals / ", m^2, ")")),
       title = "Change in buffelgrass cover vs. initial density") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.inbgden


## BG cover: Initial shrub cover (NS) -------------------------------------

# Generate prediction and add unscaled variable 
insight.bgcov.inshrub <- get_datagrid(dat.plot.ex, by = c("Init_ShrubCover_scaled"),
                                      length = 10)
insight.bgcov.inshrub$Change_BGCover <- get_predicted(bgcov4, insight.bgcov.inshrub)
insight.bgcov.inshrub$SE <- get_predicted_ci(bgcov4, data = insight.bgcov.inshrub)$SE
insight.bgcov.inshrub$CI <- insight.bgcov.inshrub$SE * 1.96
unscaled.inshrub <- get_datagrid(dat.plot.unscaled, by = "Init_ShrubCover") %>% 
  arrange(Init_ShrubCover)
insight.bgcov.inshrub$Init_ShrubCover <- unscaled.inshrub$Init_ShrubCover

# Graph
bgcov.inshrub <- dat.plot %>% 
  ggplot(aes(x = Init_ShrubCover, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.inshrub,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.inshrub,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = "Initial shrub cover (%)",
       title = "Change in buffelgrass cover vs. initial shrub cover") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.inshrub


## BG cover: Initial herb cover (NS) --------------------------------------

# Generate prediction and add unscaled variable 
insight.bgcov.inherb <- get_datagrid(dat.plot.ex, by = c("Init_HerbCover_scaled"),
                                     length = 100)
insight.bgcov.inherb$Change_BGCover <- get_predicted(bgcov4, insight.bgcov.inherb)
insight.bgcov.inherb$SE <- get_predicted_ci(bgcov4, data = insight.bgcov.inherb)$SE
insight.bgcov.inherb$CI <- insight.bgcov.inherb$SE * 1.96
unscaled.inherb <- get_datagrid(dat.plot.unscaled, by = "Init_HerbCover",
                                length = 100) %>% 
  arrange(Init_HerbCover)
insight.bgcov.inherb$Init_HerbCover <- unscaled.inherb$Init_HerbCover

# Graph
bgcov.inherb <- dat.plot %>% 
  ggplot(aes(x = Init_HerbCover, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.inherb,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgcov.inherb,
              aes(ymin = Change_BGCover - CI, ymax = Change_BGCover + CI), alpha = 0.2) +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = "Initial grass & forb cover (%)",
       title = "Change in buffelgrass cover vs. initial herb cover") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.inherb


## BG cover: Precip * shrub (NS) ------------------------------------------

# Generate prediction and add unscaled variable - 12 levels to get mean
insight.bgcov.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.shrub.precip$Predicted <- get_predicted(bgcov4, insight.bgcov.shrub.precip)
unscaled.shrub.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.bgcov.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.bgcov.shrub.precip$Prev_year_precip_scaled) 
insight.bgcov.shrub.precip <- insight.bgcov.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.941, 0.004, 1.657))

# Graph
bgcov.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
       title = "Change in buffelgrass cover vs. change in shrub cover")
bgcov.shrub.precip


## BG cover: Precip * herb (NS) -------------------------------------------

# Generate prediction and add unscaled variable - 12 levels to get mean
insight.bgcov.herb.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.herb.precip$Predicted <- get_predicted(bgcov4, insight.bgcov.herb.precip)
unscaled.herb.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.bgcov.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.bgcov.herb.precip$Prev_year_precip_scaled) 
insight.bgcov.herb.precip <- insight.bgcov.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.941, 0.004, 1.657))

# Graph
bgcov.herb.precip <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
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
       title = "Change in buffelgrass cover vs. change in herb cover")
bgcov.herb.precip


## BG cover: Slope * shrub (NS) -------------------------------------------

# Generate prediction and add unscaled variable - 12 slope levels to get mean
insight.bgcov.shrub.slope <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.shrub.slope$Predicted <- get_predicted(bgcov4, insight.bgcov.shrub.slope)
unscaled.shrub.slope <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.shrub.slope$Change_ShrubCover <- unscaled.shrub.slope$Change_ShrubCover
insight.bgcov.shrub.slope$PlotSlope <- unscaled.shrub.slope$PlotSlope
unique(insight.bgcov.shrub.slope$PlotSlope_scaled)
insight.bgcov.shrub.slope <- insight.bgcov.shrub.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.245, -0.010, 2.673))

# Graph
bgcov.shrub.slope <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.shrub.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. change in shrub cover")
bgcov.shrub.slope


## BG cover: Slope * herb (NS) --------------------------------------------

# Generate prediction and add unscaled variable - 12 slope levels to get mean
insight.bgcov.herb.slope <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.herb.slope$Predicted <- get_predicted(bgcov4, insight.bgcov.herb.slope)
unscaled.herb.slope <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.herb.slope$Change_HerbCover <- unscaled.herb.slope$Change_HerbCover
insight.bgcov.herb.slope$PlotSlope <- unscaled.herb.slope$PlotSlope
unique(insight.bgcov.herb.slope$PlotSlope_scaled)
insight.bgcov.herb.slope <- insight.bgcov.herb.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.245, -0.010, 2.673))

# Graph
bgcov.herb.slope <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.herb.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass cover vs. change in herb cover")
bgcov.herb.slope



# Survival ----------------------------------------------------------------

## Survival: Precip -------------------------------------------------------

# Generate prediction and add scaled variable
insight.survival.precip <- get_datagrid(dat.survival.ex, by = c("Prev_year_precip_scaled"),
                                        length = 50)
insight.survival.precip$survival_prop <- get_predicted(survival2, insight.survival.precip)
insight.survival.precip$SE <- get_predicted_ci(survival2, data = insight.survival.precip)$SE
insight.survival.precip$CI <- insight.survival.precip$SE * 1.96
unscaled.precip <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                length = 50) %>%
  arrange(Prev_year_precip)
insight.survival.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

# Graph
survival.precip <- dat.survival %>%
  ggplot(aes(x = Prev_year_precip, y = survival_prop)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.precip,
            aes(y = survival_prop), linewidth = 1.3,
            color = "purple3") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = "Proportion of surviving seedlings",
       title = "Buffelgrass seedling survival vs. precip")
survival.precip

# Graph, with CI
dat.survival %>%
  ggplot(aes(x = Prev_year_precip, y = survival_prop)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.precip,
            aes(y = survival_prop), linewidth = 1.3,
            color = "purple3") +
  geom_ribbon(data = insight.survival.precip,
              aes(ymin = survival_prop - CI, ymax = survival_prop + CI), alpha = 0.2) +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = "Proportion of surviving seedlings",
       title = "Buffelgrass seedling survival vs. precip")


## Survival: BG density ---------------------------------------------------

# Generate prediction and add scaled variable
insight.survival.bgden <- get_datagrid(dat.survival.ex, by = c("BGDensity_scaled"),
                                       length = 50)
insight.survival.bgden$survival_prop <- get_predicted(survival2, insight.survival.bgden)
unscaled.bgden <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                               length = 50) %>%
  arrange(BGDensity)
insight.survival.bgden$BGDensity <- unscaled.bgden$BGDensity

# Graph
survival.bgden <- dat.survival %>%
  ggplot(aes(x = BGDensity, y = survival_prop)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.bgden,
            aes(y = survival_prop), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Proportion of surviving seedlings",
       title = "Buffelgrass seedling survival vs. plot density")
survival.bgden


## Survival: Shrub --------------------------------------------------------

# Generate prediction and add scaled variable
insight.survival.shrub <- get_datagrid(dat.survival.ex, by = c("ShrubCover_scaled"),
                                       length = 50)
insight.survival.shrub$survival_prop <- get_predicted(survival2, insight.survival.shrub) 
unscaled.shrub <- get_datagrid(dat.survival.unscaled, by = "ShrubCover",
                               length = 50) %>%
  arrange(ShrubCover)
insight.survival.shrub$ShrubCover <- unscaled.shrub$ShrubCover

# Graph
survival.shrub <- dat.survival %>%
  ggplot(aes(x = ShrubCover, y = survival_prop)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.shrub,
            aes(y = survival_prop), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Native shrub cover (%)",
       y = "Proportion of surviving seedlings",
       title = "Buffelgrass seedling survival vs. shrub cover")
survival.shrub


## Survival: Slope (NS) ---------------------------------------------------

# Generate prediction and add scaled variable
insight.survival.slope <- get_datagrid(dat.survival.ex, by = c("PlotSlope_scaled"),
                                       length = 50)
insight.survival.slope$survival_prop <- get_predicted(survival2, insight.survival.slope)
unscaled.slope <- get_datagrid(dat.survival.unscaled, by = "PlotSlope",
                               length = 50) %>%
  arrange(PlotSlope)
insight.survival.slope$PlotSlope <- unscaled.slope$PlotSlope

# Graph
survival.slope <- dat.survival %>%
  ggplot(aes(x = PlotSlope, y = survival_prop)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.slope,
            aes(y = survival_prop), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Plot slope (\u00B0)",
       y = "Proportion of surviving seedlings",
       title = "Buffelgrass seedling survival vs. plot slope")
survival.slope


## Survival: Herb (NS) ----------------------------------------------------

# Generate prediction and add scaled variable
insight.survival.herb <- get_datagrid(dat.survival.ex, by = c("HerbCover_scaled"),
                                      length = 50)
insight.survival.herb$survival_prop <- get_predicted(survival2, insight.survival.herb) 
unscaled.herb <- get_datagrid(dat.survival.unscaled, by = "HerbCover",
                              length = 50) %>%
  arrange(HerbCover)
insight.survival.herb$HerbCover <- unscaled.herb$HerbCover

# Graph
survival.herb <- dat.survival %>%
  ggplot(aes(x = HerbCover, y = survival_prop)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.herb,
            aes(y = survival_prop), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Native forb & grass cover (%)",
       y = "Proportion of surviving seedlings",
       title = "Buffelgrass seedling survival vs. herb cover")
survival.herb


## Survival: Precip * density (NS) ----------------------------------------

# Generate prediction and add unscaled variable - 5 levels to get mean
insight.survival.bgden.precip <- dat.survival.ex %>%
  get_datagrid(c("BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>%
  arrange(BGDensity_scaled) %>%
  distinct(.keep_all = TRUE)
insight.survival.bgden.precip$Predicted <- get_predicted(survival2, insight.survival.bgden.precip)
unscaled.bgden.precip <- dat.survival.unscaled %>%
  get_datagrid(c("BGDensity", "Prev_year_precip"), length = 10) %>%
  get_datagrid("Prev_year_precip", length = 5, numerics = "all") %>%
  arrange(BGDensity) %>%
  distinct(.keep_all = TRUE)
insight.survival.bgden.precip$BGDensity <- unscaled.bgden.precip$BGDensity
insight.survival.bgden.precip$Prev_year_precip <- unscaled.bgden.precip$Prev_year_precip
unique(insight.survival.bgden.precip$Prev_year_precip_scaled)
insight.survival.bgden.precip <- insight.survival.bgden.precip %>%
  filter(Prev_year_precip_scaled %in% c(-1.023, -0.005, 3.049))

# Graph
survival.bgden.precip <- dat.survival %>%
  ggplot(aes(x = BGDensity, y = survival_prop,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.bgden.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Proportion of surviving seedlings",
       x = expression(paste("Density (individuals / ", m^2, ")")),
       title = "Buffelgrass seedling survival vs. density")
survival.bgden.precip


## Survival: Precip * shrub (NS) ------------------------------------------

# Generate prediction and add unscaled variable - 5 levels to get mean
insight.survival.shrub.precip <- dat.survival.ex %>%
  get_datagrid(c("ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>%
  arrange(ShrubCover_scaled) %>%
  distinct(.keep_all = TRUE)
insight.survival.shrub.precip$Predicted <- get_predicted(survival2, insight.survival.shrub.precip) 
unscaled.shrub.precip <- dat.survival.unscaled %>%
  get_datagrid(c("ShrubCover", "Prev_year_precip"), length = 10) %>%
  get_datagrid("Prev_year_precip", length = 5, numerics = "all") %>%
  arrange(ShrubCover) %>%
  distinct(.keep_all = TRUE)
insight.survival.shrub.precip$ShrubCover <- unscaled.shrub.precip$ShrubCover
insight.survival.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.survival.shrub.precip$Prev_year_precip_scaled)
insight.survival.shrub.precip <- insight.survival.shrub.precip %>%
  filter(Prev_year_precip_scaled %in% c(-1.023, -0.005, 3.049))

# Graph
survival.shrub.precip <- dat.survival %>%
  ggplot(aes(x = ShrubCover, y = survival_prop,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Proportion of surviving seedlings",
       x = "Native shrub cover (%)",
       title = "Buffelgrass seedling survival vs. shrub cover")
survival.shrub.precip


## Survival: Precip * herb (NS) -------------------------------------------

# Generate prediction and add unscaled variable - 5 levels to get mean
insight.survival.herb.precip <- dat.survival.ex %>%
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>%
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>%
  arrange(HerbCover_scaled) %>%
  distinct(.keep_all = TRUE)
insight.survival.herb.precip$Predicted <- get_predicted(survival2, insight.survival.herb.precip)
unscaled.herb.precip <- dat.survival.unscaled %>%
  get_datagrid(c("HerbCover", "Prev_year_precip"), length = 10) %>%
  get_datagrid("Prev_year_precip", length = 5, numerics = "all") %>%
  arrange(HerbCover) %>%
  distinct(.keep_all = TRUE)
insight.survival.herb.precip$HerbCover <- unscaled.herb.precip$HerbCover
insight.survival.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.survival.herb.precip$Prev_year_precip_scaled)
insight.survival.herb.precip <- insight.survival.herb.precip %>%
  filter(Prev_year_precip_scaled %in% c(-1.023, -0.005, 3.049))

# Graph
survival.herb.precip <- dat.survival %>%
  ggplot(aes(x = HerbCover, y = survival_prop,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Proportion of surviving seedlings",
       x = "Native grass & forb cover (%)",
       title = "Buffelgrass seedling survival vs. herb cover")
survival.herb.precip



## Survival: Slope * shrub (NS) -------------------------------------------

# Generate prediction and add unscaled variable - 12 slope levels to get mean
insight.survival.shrub.slope <- dat.survival.ex %>% 
  get_datagrid(c("ShrubCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>% 
  arrange(ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.survival.shrub.slope$Predicted <- get_predicted(survival2, insight.survival.shrub.slope)
unscaled.shrub.slope <- dat.survival.unscaled %>% 
  get_datagrid(c("ShrubCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 12, numerics = "all") %>% 
  arrange(ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.survival.shrub.slope$ShrubCover <- unscaled.shrub.slope$ShrubCover
insight.survival.shrub.slope$PlotSlope <- unscaled.shrub.slope$PlotSlope
unique(insight.survival.shrub.slope$PlotSlope_scaled)
insight.survival.shrub.slope <- insight.survival.shrub.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.465, 0.026, 2.102))

# Graph
survival.shrub.slope <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_prop,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.shrub.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  labs(y = "Proportion of surviving seedlings",
       x = "Native shrub cover (%)",
       title = "Buffelgrass seedling survival vs. shrub cover")
survival.shrub.slope


## Survival: Slope * herb (NS) --------------------------------------------

# Generate prediction and add unscaled variable - 12 slope levels to get mean
insight.survival.herb.slope <- dat.survival.ex %>% 
  get_datagrid(c("HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>% 
  arrange(HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.survival.herb.slope$Predicted <- get_predicted(survival2, insight.survival.herb.slope)
unscaled.herb.slope <- dat.survival.unscaled %>% 
  get_datagrid(c("HerbCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 12, numerics = "all") %>% 
  arrange(HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.survival.herb.slope$HerbCover <- unscaled.herb.slope$HerbCover
insight.survival.herb.slope$PlotSlope <- unscaled.herb.slope$PlotSlope
unique(insight.survival.herb.slope$PlotSlope_scaled)
insight.survival.herb.slope <- insight.survival.herb.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.465, 0.026, 2.102))

# Graph
survival.herb.slope <- dat.survival %>% 
  ggplot(aes(x = HerbCover, y = survival_prop,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.herb.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  labs(y = "Proportion of surviving seedlings",
       x = "Native grass & forb cover (%)",
       title = "Buffelgrass seedling survival vs. herb cover")
survival.herb.slope


# Simple linear regression ------------------------------------------------

# Shrub cover change vs. precip
shrub.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native shrub cover (%)"),
       x = "Previous year precipitation (mm)",
       title = "Change in shrub cover vs. precip")
shrub.change.precip

# Herb cover change vs. precip
herb.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_HerbCover)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native grass & forb cover (%)"),
       x = "Previous year precipitation (mm)",
       title = "Change in herb cover vs. precip")
herb.change.precip

# Total vs. repro
total.repro <- culm.change %>% 
  ggplot(aes(x = Change_Reproductive_culms, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Reproductive culm count"),
       title = "Change in total culm count vs. change in repro culm count")
total.repro

# Total vs. cover
total.cover <- culm.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_Total_Live_Culms)) +
  geom_point(alpha = 0.35) +
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
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(x = expression(Delta ~ "Cover (%)"),
       y = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in plot density vs. change in plot cover")
density.cover


# Change in BG density vs. initial density
inbgden.bgden <- plot.change %>% 
  ggplot(aes(x = Init_BGDensity, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = expression(paste("Initial density (individuals / ", m^2, ")")),
       y = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in BG density vs. initial density")
inbgden.bgden



# Write out draft figures -------------------------------------------------

## Total change -----------------------------------------------------------

# Significant
# Total change vs. Prev_year_precip
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.precip
dev.off()

# Total change vs. Change_BGDensity
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.bgden
dev.off()

# Total change vs. Change_ShrubCover
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.shrub
dev.off()

# Total change vs. Init_BGDensity 
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_initial-BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.inbgden
dev.off()

# Total change interaction of precip*shrub
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.shrub.precip
dev.off()

# Total change interaction of precip*herb
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.herb.precip
dev.off()

# Total change interaction of slope*shrub
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_shrub-cover-change-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.shrub.slope
dev.off()


# Not significant
# Total change vs. PlotSlope
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.slope
dev.off()

# Total change vs. Change_HerbCover 
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.herb
dev.off()

# Total change vs. Init_ShrubCover 
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_initial-shrub-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.inshrub
dev.off()

# Total change vs. Init_HerbCover 
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_initial-herb-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.inherb
dev.off()

# Total change interaction of precip*BG density
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.bgden.precip
dev.off()

# Total change interaction of slope*herb
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_herb-cover-change-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.herb.slope
dev.off()


## Repro change -----------------------------------------------------------

# Significant
# Repro change vs. Change_HerbCover
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.herb
dev.off()

# Rerpo change vs. Init_BGDensity 
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_initial-BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.inbgden
dev.off()

# Repro change interaction of precip*BG density
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.bgden.precip
dev.off()

# Repro change interaction of slope*shrub
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_shrub-cover-change-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.shrub.slope
dev.off()


# Not significant
# Repro change vs. Prev_year_precip
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.precip
dev.off()

# Repro change vs. PlotSlope
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.slope
dev.off()

# Repro change vs. Change_BGDensity
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.bgden
dev.off()

# Repro change vs. Change_ShrubCover
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.shrub
dev.off()

# Repro change vs. Init_ShrubCover 
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_initial-shrub-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.inshrub
dev.off()

# Repro change vs. Init_HerbCover 
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_initial-herb-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.inherb
dev.off()

# Repro change interaction of precip*shrub
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.shrub.precip
dev.off()

# Repro change interaction of precip*herb
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.herb.precip
dev.off()

# Repro change interaction of slope*herb
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_herb-cover-change-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.herb.slope
dev.off()


## BG density change ------------------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.precip
dev.off()

# BG density change interaction of precip*shrub 
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.shrub.precip
dev.off()


# Not significant
# BG density change vs. PlotSlope
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.slope
dev.off()

# BG density change vs. Change_ShrubCover
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.shrub
dev.off()

# BG density change vs. Change_HerbCover
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.herb
dev.off()

# BG density change vs. Init_BGDensity
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_initial-BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.inbgden
dev.off()

# BG density change vs. Init_ShrubCover
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_initial-shrub-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.inshrub
dev.off()

# BG density change vs. Init_HerbCover
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_initial-herb-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.inherb
dev.off()

# BG density change interaction of precip*herb
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.herb.precip
dev.off()

# BG density change interaction of slope*shrub 
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_shrub-cover-change-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.shrub.slope
dev.off()

# BG density change interaction of slope*herb
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_herb-cover-change-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.herb.slope
dev.off()


## BG cover change --------------------------------------------------------

# Significant
# BG cover change vs. Prev_year_precip
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.precip
dev.off()


# Not significant
# BG cover change vs. PlotSlope
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.slope
dev.off()

# BG cover change vs. Change_ShrubCover
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.shrub
dev.off()

# BG cover change vs. Change_HerbCover
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.herb
dev.off()

# BG cover change vs. Init_BGDensity
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_initial-BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.inbgden
dev.off()

# BG cover change vs. Init_ShrubCover
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_initial-shrub-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.inshrub
dev.off()

# BG cover change vs. Init_HerbCover
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_initial-herb-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.inherb
dev.off()

# BG cover change interaction of precip*shrub
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.shrub.precip
dev.off()

# BG cover change interaction of precip*herb
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.herb.precip
dev.off()

# BG cover change interaction of slope*shrub
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_shrub-cover-change-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.shrub.slope
dev.off()

# BG cover change interaction of slope*herb
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_herb-cover-change-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.herb.slope
dev.off()


## Survival ---------------------------------------------------------------

# Significant
# Survival vs. Prev_year_precip
tiff("figures/2026-03_draft-figures-revision1.3/Survival_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip
dev.off()

# Survival vs. BGDensity
tiff("figures/2026-03_draft-figures-revision1.3/Survival_BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden
dev.off()

# Survival vs. ShrubCover
tiff("figures/2026-03_draft-figures-revision1.3/Survival_shrub-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.shrub
dev.off()


# Not significant
# Survival vs. PlotSlope
tiff("figures/2026-03_draft-figures-revision1.3/Survival_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.slope
dev.off()

# Survival vs. HerbCover
tiff("figures/2026-03_draft-figures-revision1.3/Survival_herb-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.herb
dev.off()

# Survival interaction of precip*density
tiff("figures/2026-03_draft-figures-revision1.3/Survival_BG-density-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.bgden.precip
dev.off()

# Survival interaction of precip*shrub
tiff("figures/2026-03_draft-figures-revision1.3/Survival_shrub-cover-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.shrub.precip
dev.off()

# Survival interaction of precip*herb
tiff("figures/2026-03_draft-figures-revision1.3/Survival_herb-cover-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.precip
dev.off()

# Survival interaction of slope*shrub
tiff("figures/2026-03_draft-figures-revision1.3/Survival_shrub-cover-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.shrub.slope
dev.off()

# Survival interaction of slope*herb
tiff("figures/2026-03_draft-figures-revision1.3/Survival_herb-cover-and-slope-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.slope
dev.off()



## Precip -----------------------------------------------------------------

# Combined precip plot for density, cover, survival, no CI
tiff("figures/2026-03_draft-figures-revision1.3/Precip-combined_density-cover-survival.tiff",
     units = "in", height = 7, width = 9, res = 150)
ggarrange(bgden.precip, bgcov.precip, survival.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))
dev.off()


# Averages by site
tiff("figures/2026-03_draft-figures-revision1.3/Precip-by-site.tiff",
     units = "in", height = 4, width = 6, res = 150)
precip.site.all
dev.off()

# Percent deviation by site
tiff("figures/2026-03_draft-figures-revision1.3/Precip-deviation-by-site.tiff",
     units = "in", height = 4, width = 6, res = 150)
precip.site.dev.all
dev.off()



## Simple linear regression -----------------------------------------------

# Shrub vs. precip
tiff("figures/2026-03_draft-figures-revision1.3/Shrub-change_precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
shrub.change.precip
dev.off()

# Herb vs. precip
tiff("figures/2026-03_draft-figures-revision1.3/Herb-change_precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
herb.change.precip
dev.off()

# Total culm vs. cover
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_BG-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.cover
dev.off()

# Density vs. cover
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_BG-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
density.cover
dev.off()


save.image("RData/16.2_draft-figs-for-lm-revision1.3.RData")
