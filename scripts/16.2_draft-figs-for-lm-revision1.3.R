# Created: 2026-03-24
# Updated: 2026-03-24

# Purpose: Graph data to accompany 15.2.R of revision1.3 linear models (includes
#   initial BG density/shrub/herb conditions, as well as slope*shrub/herb interactions; aspect dropped).

# https://easystats.github.io/modelbased/articles/visualisation_matrix.html#visualising-an-interaction-between-two-numeric-variables-three-way-interaction


library(tidyverse)
library(insight)
library(modelbased)
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

# Results: I checked 3-11 precip levels, and the closest that came to 0 was -0.054,
#   which was generated first at 10 precip levels.

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
  geom_point(alpha = 0.25) +
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
